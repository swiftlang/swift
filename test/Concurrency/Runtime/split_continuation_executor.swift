// RUN: %target-run-simple-swift( -target %target-swift-5.1-abi-triple %import-libdispatch -parse-as-library -disable-availability-checking -swift-version 6 -enable-experimental-feature SplitContinuations)
// RUN: %target-run-simple-swift( -target %target-swift-5.1-abi-triple %import-libdispatch -parse-as-library -disable-availability-checking -swift-version 6 -O -enable-experimental-feature SplitContinuations)

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime
// REQUIRES: libdispatch
// REQUIRES: swift_feature_SplitContinuations
// UNSUPPORTED: back_deployment_runtime

// Naming the executors on `resume` lets an executor that also resumes the
// continuation run the task on its own thread, with no enqueue and no hop.
// Also covers the cases where the offered thread can't be used and the task
// falls back to an enqueue instead of trapping.

@_spi(Concurrency) import _Concurrency
@preconcurrency import Dispatch
import StdlibUnittest

// A serial executor with a thread of its own, so donation is distinguishable
// from an enqueue.
final class QueueExecutor: SerialExecutor {
  let queue: DispatchQueue
  private let key = DispatchSpecificKey<Void>()

  init(label: String = "split-continuation-executor") {
    self.queue = DispatchQueue(label: label)
    queue.setSpecific(key: key, value: ())
  }

  func enqueue(_ job: UnownedJob) {
    queue.async { unsafe job.runSynchronously(on: self.asUnownedSerialExecutor()) }
  }

  func asUnownedSerialExecutor() -> UnownedSerialExecutor {
    UnownedSerialExecutor(ordinary: self)
  }

  // Delayed so the awaiting task has certainly suspended first.
  func onOwnThreadOnceSuspended(_ body: @escaping @Sendable () -> Void) {
    queue.asyncAfter(deadline: .now() + .milliseconds(100), execute: body)
  }

  // Whether the current execution is still nested inside this executor's
  // queue. Donation never leaves that queue's dispatch context (no new
  // dispatch happens); an enqueue runs on a different queue. Unlike append
  // order in a shared log, this isn't racy under contention.
  var isCurrentQueue: Bool {
    DispatchQueue.getSpecific(key: key) != nil
  }
}

// A task executor, to give the awaiting task a preference the resumer does
// not know about.
final class SimpleTaskExecutor: TaskExecutor {
  let queue = DispatchQueue(label: "split-continuation-task-executor")
  private let key = DispatchSpecificKey<Void>()

  init() {
    queue.setSpecific(key: key, value: ())
  }

  func enqueue(_ job: UnownedJob) {
    queue.async { unsafe job.runSynchronously(on: self.asUnownedTaskExecutor()) }
  }

  func asUnownedTaskExecutor() -> UnownedTaskExecutor {
    UnownedTaskExecutor(ordinary: self)
  }

  func onOwnThreadOnceSuspended(_ body: @escaping @Sendable () -> Void) {
    queue.asyncAfter(deadline: .now() + .milliseconds(100), execute: body)
  }

  var isCurrentQueue: Bool {
    DispatchQueue.getSpecific(key: key) != nil
  }
}

// Thread-safe ordering log. The enqueue-fallback cases post the resumed task
// onto a different queue than the one running the resume, so this really
// does race.
final class Recorder: @unchecked Sendable {
  private let lock = DispatchQueue(label: "split-continuation-executor-recorder")
  private var items: [String] = []

  func record(_ item: String) { lock.sync { items.append(item) } }
  var all: [String] { lock.sync { items } }
}

// Holds the resume half of a split continuation so another thread can
// resume it.
final class ContinuationHolder<Success: ~Copyable, Failure: Error>: @unchecked Sendable {
  private var continuation: Continuation<Success, Failure>?

  init(_ continuation: consuming Continuation<Success, Failure>) {
    self.continuation = consume continuation
  }

  func take() -> Continuation<Success, Failure> {
    continuation.take()!
  }
}

// A separate isolation domain, to move the await off the executor the
// continuation was created on.
actor Elsewhere {
  func awaitIt(_ awaiter: consuming ContinuationAwaiter<Int, Never>) async -> Int {
    await awaiter.wait()
  }
}

actor Service {
  private let _executor: QueueExecutor

  nonisolated var unownedExecutor: UnownedSerialExecutor {
    _executor.asUnownedSerialExecutor()
  }

  init(executor: QueueExecutor) { self._executor = executor }

  // Resumes from the executor's own thread, naming that executor. `order`
  // records who observed the resumption first: donation runs the resumed
  // task before the resume returns, so the awaiting side is recorded first;
  // an enqueue records the resumer first.
  func run(order: Recorder) async -> Int {
    let value = await withContinuation(of: Int.self, throwing: Never.self) {
      (continuation: consuming Continuation<Int, Never>,
       awaiter: consuming ContinuationAwaiter<Int, Never>) in
      let holder = ContinuationHolder(continuation)
      let executor = self._executor
      executor.onOwnThreadOnceSuspended {
        holder.take().resume(
          returning: 99,
          isolatedTo: executor.asUnownedSerialExecutor())
        order.record("resumer returned")
      }
      let value = await awaiter.wait()
      order.record("task resumed")
      return value
    }
    return value
  }

  // Resumed from the escalation/cancellation-free path with handlers
  // installed, to show the two compose.
  func runWithHandlers() async -> Int {
    await withContinuation(of: Int.self, throwing: Never.self) {
      (continuation: consuming Continuation<Int, Never>,
       awaiter: consuming ContinuationAwaiter<Int, Never>) in
      let holder = ContinuationHolder(continuation)
      let executor = self._executor
      executor.onOwnThreadOnceSuspended {
        holder.take().resume(
          with: .success(100),
          isolatedTo: executor.asUnownedSerialExecutor())
      }
      return await awaiter.wait(onCancel: {}, onEscalate: { _ in })
    }
  }

  // Resumes from `wrongExecutor`'s thread while naming it, even though this
  // task resumes on `_executor`. The offer can't be taken, so the task is
  // enqueued instead. `wrongExecutor` and `_executor` are different queues,
  // so whether the resumed code still ran on `wrongExecutor`'s queue is a
  // direct, non-racy check -- unlike comparing append order across queues.
  func runResumedFromAnotherExecutor(
    wrongExecutor: QueueExecutor
  ) async -> (value: Int, ranOnWrongExecutor: Bool) {
    var ranOnWrongExecutor = false
    let value = await withContinuation(of: Int.self, throwing: Never.self) {
      (continuation: consuming Continuation<Int, Never>,
       awaiter: consuming ContinuationAwaiter<Int, Never>) in
      let holder = ContinuationHolder(continuation)
      wrongExecutor.onOwnThreadOnceSuspended {
        holder.take().resume(
          returning: 1,
          isolatedTo: wrongExecutor.asUnownedSerialExecutor())
      }
      let value = await awaiter.wait()
      ranOnWrongExecutor = wrongExecutor.isCurrentQueue
      return value
    }
    return (value, ranOnWrongExecutor)
  }

  // Creates the continuation on this actor's executor -- the only one the
  // resumer can name -- but awaits from another isolation domain, so the
  // task resumes on that executor instead. Must not trap.
  func runAwaitedElsewhere(_ other: Elsewhere) async -> Int {
    await withContinuation(of: Int.self, throwing: Never.self) {
      (continuation: consuming Continuation<Int, Never>,
       awaiter: consuming ContinuationAwaiter<Int, Never>) in
      let holder = ContinuationHolder(continuation)
      let executor = self._executor
      executor.onOwnThreadOnceSuspended {
        holder.take().resume(
          returning: 42,
          isolatedTo: executor.asUnownedSerialExecutor())
      }
      return await other.awaitIt(awaiter)
    }
  }

  // An ordinary continuation, from the synchronous-body form of
  // withContinuation -- no ContinuationAwaiter here at all. It has no way to
  // defer its resume executor to await time, so naming one is not a misuse:
  // it's ignored and this enqueues as usual.
  func runOrdinaryContinuation(
    wrongExecutor: QueueExecutor, order: Recorder
  ) async -> Int {
    await withContinuation(of: Int.self, throwing: Never.self) {
      (continuation: consuming Continuation<Int, Never>) in
      let holder = ContinuationHolder(continuation)
      wrongExecutor.onOwnThreadOnceSuspended {
        holder.take().resume(
          returning: 7,
          isolatedTo: wrongExecutor.asUnownedSerialExecutor())
        order.record("resumer returned")
      }
    }
  }

  // Creates the continuation under `preference`, then resumes naming only
  // that task executor -- but this method is isolated to `self`'s actor, so
  // the task resumes with real serial isolation the offer can't match.
  // Falls back to an enqueue exactly as a mismatched `isolatedTo:` would.
  // `preference` and `_executor` are different queues, so checking whether
  // the resumed code is still nested in `preference`'s queue is a direct,
  // non-racy way to prove the offer wasn't taken.
  func runTaskExecutorOnlyButActorIsolated(
    preference: SimpleTaskExecutor
  ) async -> (value: Int, ranOnPreferenceExecutor: Bool) {
    var ranOnPreferenceExecutor = false
    let value = await withContinuation(of: Int.self, throwing: Never.self) {
      (continuation: consuming Continuation<Int, Never>,
       awaiter: consuming ContinuationAwaiter<Int, Never>) in
      let holder = ContinuationHolder(continuation)
      preference.onOwnThreadOnceSuspended {
        holder.take().resume(returning: 55, on: preference.asUnownedTaskExecutor())
      }
      let value = await awaiter.wait()
      ranOnPreferenceExecutor = preference.isCurrentQueue
      return value
    }
    return (value, ranOnPreferenceExecutor)
  }
}

// Creates the continuation under `preference` and resumes naming only that
// same task executor, from a nonisolated caller so the task also resumes
// with no serial isolation. Both sides agree on the task executor, so the
// offer is taken.
func runTaskExecutorOnlyMatching(
  preference: SimpleTaskExecutor, order: Recorder
) async -> Int {
  await withTaskExecutorPreference(preference) {
    await withContinuation(of: Int.self, throwing: Never.self) {
      (continuation: consuming Continuation<Int, Never>,
       awaiter: consuming ContinuationAwaiter<Int, Never>) in
      let holder = ContinuationHolder(continuation)
      preference.onOwnThreadOnceSuspended {
        holder.take().resume(returning: 33, on: preference.asUnownedTaskExecutor())
        order.record("resumer returned")
      }
      let value = await awaiter.wait()
      order.record("task resumed")
      return value
    }
  }
}

// Resumes naming an executor the resumer happens to be on, while the task
// itself has no isolation and no task executor preference. That's not
// enough: the offer is taken only if it's the task's real executor, its
// preferred task executor, or the default. `executor` is none of those, and
// the real target is the default pool, a different queue -- so checking
// queue identity (not append order) is what's non-racy here.
func runWithNoIsolationAtAll(
  naming executor: QueueExecutor
) async -> (value: Int, ranOnOfferedExecutor: Bool) {
  var ranOnOfferedExecutor = false
  let value = await withContinuation(of: Int.self, throwing: Never.self) {
    (continuation: consuming Continuation<Int, Never>,
     awaiter: consuming ContinuationAwaiter<Int, Never>) in
    let holder = ContinuationHolder(continuation)
    executor.onOwnThreadOnceSuspended {
      holder.take().resume(
        returning: 66, isolatedTo: executor.asUnownedSerialExecutor())
    }
    let value = await awaiter.wait()
    ranOnOfferedExecutor = executor.isCurrentQueue
    return value
  }
  return (value, ranOnOfferedExecutor)
}

// Same as above, but the resumer offers the actual default executor, which
// genuinely is where the task resumes -- so the offer is taken.
func runWithDefaultTaskExecutorOffered(order: Recorder) async -> Int {
  await withContinuation(of: Int.self, throwing: Never.self) {
    (continuation: consuming Continuation<Int, Never>,
     awaiter: consuming ContinuationAwaiter<Int, Never>) in
    let holder = ContinuationHolder(continuation)
    Task.detached {
      try? await Task.sleep(for: .milliseconds(100))
      holder.take().resume(returning: 77, on: globalConcurrentExecutor.asUnownedTaskExecutor())
      order.record("resumer returned")
    }
    let value = await awaiter.wait()
    order.record("task resumed")
    return value
  }
}

@main struct Main {
  static func main() async {
    let tests = TestSuite("Continuation: resuming on the resumer's own thread")

    tests.test("naming the awaiting task's executor donates the thread") {
      let executor = QueueExecutor()
      let service = Service(executor: executor)
      let order = Recorder()

      expectEqual(99, await service.run(order: order))
      expectEqual(["task resumed", "resumer returned"], order.all)
    }

    tests.test("donating the thread with handlers installed") {
      let executor = QueueExecutor()
      let service = Service(executor: executor)
      expectEqual(100, await service.runWithHandlers())
    }

    tests.test("naming another executor enqueues instead of trapping") {
      let service = Service(executor: QueueExecutor(label: "right"))

      let result = await service.runResumedFromAnotherExecutor(
        wrongExecutor: QueueExecutor(label: "wrong"))
      expectEqual(1, result.value)
      expectFalse(result.ranOnWrongExecutor)
    }

    tests.test("awaiting from another isolation domain enqueues instead of trapping") {
      let service = Service(executor: QueueExecutor(label: "creating"))
      expectEqual(42, await service.runAwaitedElsewhere(Elsewhere()))
    }

    tests.test("a task executor preference enqueues instead of trapping") {
      let service = Service(executor: QueueExecutor(label: "preferring"))
      let order = Recorder()

      await withTaskExecutorPreference(SimpleTaskExecutor()) {
        expectEqual(99, await service.run(order: order))
      }

      expectEqual(["resumer returned", "task resumed"], order.all)
    }

    tests.test("naming an executor on an ordinary continuation just enqueues") {
      let service = Service(executor: QueueExecutor(label: "ordinary"))
      let order = Recorder()

      expectEqual(
        7,
        await service.runOrdinaryContinuation(
          wrongExecutor: QueueExecutor(label: "unrelated"), order: order))
      expectEqual(["resumer returned"], order.all)
    }

    tests.test("naming only a task executor donates when the task matches it") {
      let preference = SimpleTaskExecutor()
      let order = Recorder()

      expectEqual(33, await runTaskExecutorOnlyMatching(
        preference: preference, order: order))
      expectEqual(["task resumed", "resumer returned"], order.all)
    }

    tests.test("naming only a task executor enqueues when the task is actor-isolated") {
      let preference = SimpleTaskExecutor()
      let service = Service(executor: QueueExecutor(label: "task-executor-only"))

      // Put `preference` on the task too, so the enqueue is provably caused
      // by the actor isolation alone.
      let result = await withTaskExecutorPreference(preference) {
        await service.runTaskExecutorOnlyButActorIsolated(preference: preference)
      }
      expectEqual(55, result.value)
      expectFalse(result.ranOnPreferenceExecutor)
    }

    tests.test("naming an unrelated executor enqueues when the task has no isolation at all") {
      let executor = QueueExecutor(label: "unrelated-to-generic")

      let result = await runWithNoIsolationAtAll(naming: executor)
      expectEqual(66, result.value)
      expectFalse(result.ranOnOfferedExecutor)
    }

    tests.test("naming the actual default task executor donates when the task has no isolation") {
      let order = Recorder()

      expectEqual(77, await runWithDefaultTaskExecutorOffered(order: order))
      expectEqual(["task resumed", "resumer returned"], order.all)
    }

    await runAllTestsAsync()
  }
}
