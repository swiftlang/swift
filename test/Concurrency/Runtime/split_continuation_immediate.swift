// RUN: %target-run-simple-swift( -target %target-swift-5.1-abi-triple %import-libdispatch -parse-as-library -disable-availability-checking -swift-version 6 -enable-experimental-feature SplitContinuations)
// RUN: %target-run-simple-swift( -target %target-swift-5.1-abi-triple %import-libdispatch -parse-as-library -disable-availability-checking -swift-version 6 -O -enable-experimental-feature SplitContinuations)

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime
// REQUIRES: libdispatch
// REQUIRES: swift_feature_SplitContinuations
// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: back_deploy_concurrency
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: freestanding

// A split continuation created by a task started with `Task.immediate`, which
// runs on the caller's thread until its first suspension.
//
// From an isolated caller, `Task.immediate` inherits that isolation and
// records a real executor. From a nonisolated caller, it records
// `SerialExecutorRef::forSynchronousStart()`, a marker meaning "running
// synchronously on the caller's thread". That marker's identity is null,
// same as the plain generic executor, so a thread-offering resume treats it
// the same way: the offer is taken only if it's actually the default
// executor, not merely because there's no isolation to violate.

@_spi(Concurrency) import _Concurrency
@preconcurrency import Dispatch
import StdlibUnittest

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

// Thread-safe ordering log.
final class Recorder: @unchecked Sendable {
  private let lock = DispatchQueue(label: "split-continuation-immediate-recorder")
  private var items: [String] = []

  func record(_ item: String) { lock.sync { items.append(item) } }
  var all: [String] { lock.sync { items } }
}

// A serial executor that only needs to be nameable on a resume.
final class UnrelatedExecutor: SerialExecutor {
  let queue = DispatchQueue(label: "split-continuation-immediate-unrelated")
  private let key = DispatchSpecificKey<Void>()

  init() {
    queue.setSpecific(key: key, value: ())
  }

  func enqueue(_ job: UnownedJob) {
    queue.async { unsafe job.runSynchronously(on: self.asUnownedSerialExecutor()) }
  }

  func asUnownedSerialExecutor() -> UnownedSerialExecutor {
    UnownedSerialExecutor(ordinary: self)
  }

  // Whether the current execution is still nested inside this executor's
  // queue. Donation never leaves that queue's dispatch context; an enqueue
  // runs on a different queue. Unlike append order in a shared log under
  // cross-queue contention, this isn't racy.
  var isCurrentQueue: Bool {
    DispatchQueue.getSpecific(key: key) != nil
  }
}

let resumeQueue = DispatchQueue(label: "split-continuation-immediate-resumer")

// Resume from another thread, delayed until the awaiting task has certainly
// suspended.
func resumeOnceSuspended(_ body: @escaping @Sendable () -> Void) {
  resumeQueue.asyncAfter(deadline: .now() + .milliseconds(100), execute: body)
}

// A nonisolated caller: `Task.immediate` starts on this thread with the
// synchronous-start marker rather than inheriting an executor. The real
// target here is the default pool, a different queue than `executor`, so
// checking queue identity (not append order) is what's non-racy.
func immediateFromNonisolatedCaller(
  naming executor: UnrelatedExecutor
) async -> (value: Int, ranOnOfferedExecutor: Bool) {
  let task = Task.immediate { () -> (Int, Bool) in
    let value = await withContinuation(of: Int.self, throwing: Never.self) {
      (continuation: consuming Continuation<Int, Never>,
       awaiter: consuming ContinuationAwaiter<Int, Never>) in
      let holder = ContinuationHolder(continuation)
      resumeOnceSuspended {
        holder.take().resume(
          returning: 20, isolatedTo: executor.asUnownedSerialExecutor())
      }
      return await awaiter.wait()
    }
    return (value, executor.isCurrentQueue)
  }
  return await task.value
}

// Same as above, but the resumer offers the actual default task executor,
// which genuinely is where the marker resumes -- so the offer is taken.
func immediateFromNonisolatedCallerOfferedDefault(order: Recorder) async -> Int {
  let task = Task.immediate {
    await withContinuation(of: Int.self, throwing: Never.self) {
      (continuation: consuming Continuation<Int, Never>,
       awaiter: consuming ContinuationAwaiter<Int, Never>) in
      let holder = ContinuationHolder(continuation)
      Task.detached {
        try? await Task.sleep(for: .milliseconds(100))
        holder.take().resume(returning: 22, on: globalConcurrentExecutor.asUnownedTaskExecutor())
        order.record("resumer returned")
      }
      let value = await awaiter.wait()
      order.record("task resumed")
      return value
    }
  }
  return await task.value
}

@main struct Main {
  static func main() async {
    let tests = TestSuite("Continuation: split continuation under Task.immediate")

    tests.test("immediate task, resumed after it suspends") {
      let task = Task.immediate {
        await withContinuation(of: Int.self, throwing: Never.self) {
          (continuation: consuming Continuation<Int, Never>,
           awaiter: consuming ContinuationAwaiter<Int, Never>) in
          let holder = ContinuationHolder(continuation)
          resumeOnceSuspended { holder.take().resume(returning: 17) }
          return await awaiter.wait()
        }
      }
      expectEqual(17, await task.value)
    }

    tests.test("immediate task, resumed before the await") {
      let task = Task.immediate {
        await withContinuation(of: Int.self, throwing: Never.self) {
          (continuation: consuming Continuation<Int, Never>,
           awaiter: consuming ContinuationAwaiter<Int, Never>) in
          continuation.resume(returning: 18)
          return await awaiter.wait()
        }
      }
      expectEqual(18, await task.value)
    }

    tests.test("immediate task, with handlers installed") {
      let task = Task.immediate {
        await withContinuation(of: Int.self, throwing: Never.self) {
          (continuation: consuming Continuation<Int, Never>,
           awaiter: consuming ContinuationAwaiter<Int, Never>) in
          let holder = ContinuationHolder(continuation)
          resumeOnceSuspended { holder.take().resume(returning: 19) }
          return await awaiter.wait(onCancel: {}, onEscalate: { _ in })
        }
      }
      expectEqual(19, await task.value)
    }

    tests.test("immediate task from a nonisolated caller enqueues when the offer isn't the default") {
      let result = await immediateFromNonisolatedCaller(naming: UnrelatedExecutor())
      expectEqual(20, result.value)
      expectFalse(result.ranOnOfferedExecutor)
    }

    tests.test("immediate task from a nonisolated caller donates when the offer is the default") {
      let order = Recorder()
      let value = await immediateFromNonisolatedCallerOfferedDefault(order: order)
      expectEqual(22, value)
      expectEqual(["task resumed", "resumer returned"], order.all)
    }

    tests.test("immediate task from an isolated caller enqueues") {
      let unrelated = UnrelatedExecutor()
      var ranOnUnrelatedExecutor = false
      let value = await MainActor.run {
        Task.immediate { @MainActor in
          await withContinuation(of: Int.self, throwing: Never.self) {
            (continuation: consuming Continuation<Int, Never>,
             awaiter: consuming ContinuationAwaiter<Int, Never>) in
            let holder = ContinuationHolder(continuation)
            resumeOnceSuspended {
              holder.take().resume(
                returning: 21, isolatedTo: unrelated.asUnownedSerialExecutor())
            }
            let value = await awaiter.wait()
            ranOnUnrelatedExecutor = unrelated.isCurrentQueue
            return value
          }
        }
      }.value
      expectEqual(21, value)
      expectFalse(ranOnUnrelatedExecutor)
    }

    await runAllTestsAsync()
  }
}
