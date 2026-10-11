// RUN: %target-run-simple-swift( -target %target-swift-5.1-abi-triple -parse-as-library -disable-availability-checking -swift-version 6 -enable-experimental-feature SplitContinuations)
// RUN: %target-run-simple-swift( -target %target-swift-5.1-abi-triple -parse-as-library -disable-availability-checking -swift-version 6 -O -enable-experimental-feature SplitContinuations)

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime
// REQUIRES: swift_feature_SplitContinuations
// UNSUPPORTED: back_deployment_runtime

// A pre-existing OptimizeHopToExecutor bug.
// Tracked by https://github.com/swiftlang/swift/issues/92356
// XFAIL: swift_test_mode_optimize_none_with_opaque_values

@_spi(Concurrency) import _Concurrency
@_spi(ExperimentalCustomExecutors) import _Concurrency
@_spi(SplitContinuation) import _Concurrency
import StdlibUnittest
import SwiftPrivateThreadExtras
import Synchronization

// MARK: - Executors

// A serial executor that queues jobs instead of running them, so tests can step
// through suspend/resume/donation interleavings deterministically. Actors in
// these tests use it as their executor.
final class ManualExecutor: SerialExecutor, TaskExecutor, Sendable {
  private let jobs = Mutex<[UnownedJob]>([])
  var pendingJobCount: Int { jobs.withLock { $0.count } }

  func enqueue(_ job: UnownedJob) {
    jobs.withLock { $0.append(job) }
  }

  func asUnownedSerialExecutor() -> UnownedSerialExecutor {
    UnownedSerialExecutor(ordinary: self)
  }

  func asUnownedTaskExecutor() -> UnownedTaskExecutor {
    UnownedTaskExecutor(ordinary: self)
  }

  private func popJob() -> UnownedJob? {
    jobs.withLock { $0.isEmpty ? nil : $0.removeFirst() }
  }

  // Runs exactly one enqueued job on this serial executor, trapping if none is
  // enqueued.
  func runNextJob() {
    guard let job = popJob() else {
      preconditionFailure("runNextJob() called with no job enqueued")
    }
    unsafe job.runSynchronously(on: asUnownedSerialExecutor())
  }

  // Runs exactly one enqueued job on this serial executor and the given task
  // executor, trapping if none is enqueued.
  func runNextJob(taskExecutor: UnownedTaskExecutor) {
    guard let job = popJob() else {
      preconditionFailure("runNextJob(taskExecutor:) called with no job enqueued")
    }
    unsafe job.runSynchronously(
      isolatedTo: asUnownedSerialExecutor(), taskExecutor: taskExecutor)
  }
}

// A task executor that queues jobs instead of running them.
final class ManualTaskExecutor: TaskExecutor, Sendable {
  private let jobs = Mutex<[UnownedJob]>([])
  var pendingJobCount: Int { jobs.withLock { $0.count } }

  func enqueue(_ job: UnownedJob) {
    jobs.withLock { $0.append(job) }
  }

  func asUnownedTaskExecutor() -> UnownedTaskExecutor {
    UnownedTaskExecutor(ordinary: self)
  }

  // Runs exactly one enqueued job, trapping if none is enqueued.
  func runNextJob() {
    guard let job = jobs.withLock({ $0.isEmpty ? nil : $0.removeFirst() }) else {
      preconditionFailure("runNextJob() called with no job enqueued")
    }
    unsafe job.runSynchronously(on: asUnownedTaskExecutor())
  }
}

// The default executor. It runs jobs as soon as they are enqueued, unless held,
// in which case it queues them until they are run explicitly.
final class ManualDefaultExecutor: TaskExecutor, Sendable {
  private let jobs = Mutex<[UnownedJob]>([])
  private let held = Mutex<Bool>(false)
  var pendingJobCount: Int { jobs.withLock { $0.count } }

  func enqueue(_ job: UnownedJob) {
    if held.withLock({ $0 }) {
      jobs.withLock { $0.append(job) }
    } else {
      unsafe job.runSynchronously(on: asUnownedTaskExecutor())
    }
  }

  func asUnownedTaskExecutor() -> UnownedTaskExecutor {
    UnownedTaskExecutor(ordinary: self)
  }

  func hold() { held.withLock { $0 = true } }

  // Runs all queued jobs, then stops holding.
  func drainAndRelease() {
    while let job = jobs.withLock({ $0.isEmpty ? nil : $0.removeFirst() }) {
      unsafe job.runSynchronously(on: asUnownedTaskExecutor())
    }
    held.withLock { $0 = false }
  }

  // Runs exactly one queued job, trapping if none is queued.
  func runNextJob() {
    guard let job = jobs.withLock({ $0.isEmpty ? nil : $0.removeFirst() }) else {
      preconditionFailure("runNextJob() called with no job enqueued")
    }
    unsafe job.runSynchronously(on: asUnownedTaskExecutor())
  }
}

final class InlineMainExecutor: MainExecutor, @unchecked Sendable {
  var isRunning: Bool = false
  func enqueue(_ job: consuming ExecutorJob) {
    unsafe UnownedJob(job).runSynchronously(on: self.asUnownedSerialExecutor())
  }
  func checkIsolated() {}
  func run() throws {}
  func stop() {}
}

struct TestFactory: ExecutorFactory {
  static var mainExecutor: any MainExecutor { InlineMainExecutor() }
  static var defaultExecutor: any TaskExecutor { ManualDefaultExecutor() }
}

// MARK: - Continuations

enum ContinuationKind: CaseIterable, CustomStringConvertible, Sendable {
  case split
  case regular

  var description: String {
    switch self {
    case .split: "split continuation"
    case .regular: "regular continuation"
    }
  }
}

// Suspends on a continuation of the given kind, handing it to `handOff` to be
// resumed.
nonisolated(nonsending) func suspend<Success: ~Copyable, Failure: Error>(
  _ kind: ContinuationKind,
  of: Success.Type,
  throwing: Failure.Type,
  handOff: (consuming Continuation<Success, Failure>) -> Void
) async throws(Failure) -> Success {
  switch kind {
  case .split:
    return try await withContinuation(of: Success.self, throwing: Failure.self) {
      (continuation: consuming Continuation<Success, Failure>,
       awaiter: consuming ContinuationAwaiter<Success, Failure>)
        async throws(Failure) -> Success in
      handOff(continuation)
      return try await awaiter.wait()
    }
  case .regular:
    return try await withContinuation(of: Success.self, throwing: Failure.self) {
      (continuation: consuming Continuation<Success, Failure>) in
      handOff(continuation)
    }
  }
}

struct TestError: Error, Equatable {
  let code: Int
}

// The families of `resume` overloads.
enum ResumeForm: CaseIterable, Sendable {
  case returning
  case throwing
  case resultSuccess
  case resultFailure

  var code: Int {
    switch self {
    case .returning: 1
    case .throwing: 2
    case .resultSuccess: 3
    case .resultFailure: 4
    }
  }
}

extension Continuation where Success == Int, Failure == TestError {
  consuming func resume(_ form: ResumeForm, on taskExecutor: UnownedTaskExecutor) {
    switch form {
    case .returning:
      resume(returning: form.code, on: taskExecutor)
    case .throwing:
      resume(throwing: TestError(code: form.code), on: taskExecutor)
    case .resultSuccess:
      resume(with: .success(form.code), on: taskExecutor)
    case .resultFailure:
      resume(with: .failure(TestError(code: form.code)), on: taskExecutor)
    }
  }

  consuming func resume(_ form: ResumeForm, isolatedTo serialExecutor: UnownedSerialExecutor) {
    switch form {
    case .returning:
      resume(returning: form.code, isolatedTo: serialExecutor)
    case .throwing:
      resume(throwing: TestError(code: form.code), isolatedTo: serialExecutor)
    case .resultSuccess:
      resume(with: .success(form.code), isolatedTo: serialExecutor)
    case .resultFailure:
      resume(with: .failure(TestError(code: form.code)), isolatedTo: serialExecutor)
    }
  }

  consuming func resume(
    _ form: ResumeForm,
    isolatedTo serialExecutor: UnownedSerialExecutor,
    taskExecutor: UnownedTaskExecutor
  ) {
    switch form {
    case .returning:
      resume(returning: form.code, isolatedTo: serialExecutor, taskExecutor: taskExecutor)
    case .throwing:
      resume(
        throwing: TestError(code: form.code), isolatedTo: serialExecutor,
        taskExecutor: taskExecutor)
    case .resultSuccess:
      resume(with: .success(form.code), isolatedTo: serialExecutor, taskExecutor: taskExecutor)
    case .resultFailure:
      resume(
        with: .failure(TestError(code: form.code)), isolatedTo: serialExecutor,
        taskExecutor: taskExecutor)
    }
  }
}

// Suspends on a continuation of the given kind and returns the code it was
// resumed with, whether returned or thrown.
nonisolated(nonsending) func suspendForCode(
  _ kind: ContinuationKind,
  handOff: (consuming Continuation<Int, TestError>) -> Void
) async -> Int {
  do throws(TestError) {
    return try await suspend(kind, of: Int.self, throwing: TestError.self, handOff: handOff)
  } catch {
    return error.code
  }
}

// Holds a continuation so that a resumer elsewhere can take it.
final class ContinuationHolder<Success: ~Copyable, Failure: Error>: Sendable {
  private let storage: Mutex<Continuation<Success, Failure>?>

  init() {
    storage = Mutex(nil)
  }

  init(_ continuation: consuming Continuation<Success, Failure>) {
    storage = Mutex(continuation)
  }

  func store(_ continuation: consuming Continuation<Success, Failure>) {
    var continuation = Optional(continuation)
    storage.withLock { $0 = continuation.take() }
  }

  func take() -> Continuation<Success, Failure> {
    storage.withLock { $0.take()! }
  }
}

// MARK: - Values

final class DropCounter: Sendable {
  private let count = Atomic<Int>(0)
  var value: Int { count.load(ordering: .relaxed) }

  func increment() { count.add(1, ordering: .relaxed) }
}

struct Token: ~Copyable, Sendable {
  let value: Int
  let drops: DropCounter

  deinit { drops.increment() }
}

// MARK: - Event log

// Records the order in which the awaiting and the resuming side get to run.
final class EventLog: Sendable {
  private let events = Mutex<[String]>([])
  var recorded: [String] { events.withLock { $0 } }

  func record(_ event: String) {
    events.withLock { $0.append(event) }
  }
}

// MARK: - Actors

// A default actor, which a thread on the generic executor can take over to run
// a task on it inline.
actor Counter {
  private var count = 0

  func increment() -> Int {
    count += 1
    return count
  }
}

// An actor isolated to a `ManualExecutor`.
actor Worker {
  private let _executor: ManualExecutor
  nonisolated var unownedExecutor: UnownedSerialExecutor { _executor.asUnownedSerialExecutor() }
  init(_ executor: ManualExecutor) { self._executor = executor }

  func run<T: Sendable>(_ body: sending nonisolated(nonsending) () async -> T) async -> T {
    await body()
  }

  // Awaits a split continuation created elsewhere, on this actor, calling
  // `beforeWaiting` right before suspending.
  func awaitIt(
    _ awaiter: consuming ContinuationAwaiter<Int, Never>,
    log: EventLog,
    beforeWaiting: @Sendable () -> Void
  ) async -> Int {
    beforeWaiting()
    let value = await awaiter.wait()
    log.record("awaiter resumed")
    return value
  }

  func touch() {}
}

// Starts a nonisolated `Task.immediate` that awaits a continuation of the given
// kind during its synchronous start, has it resumed from the default executor
// with an offer of that executor, and then calls into an idle default actor.
nonisolated func startImmediatelyAndCallActor(
  _ kind: ContinuationKind, _ counter: Counter, _ log: EventLog
) -> Task<Int, Never> {
  Task.immediate {
    let value = await suspend(kind, of: Int.self, throwing: Never.self) { continuation in
      let holder = ContinuationHolder(continuation)
      Task.detached {
        holder.take().resume(
          returning: 19, on: globalConcurrentExecutor.asUnownedTaskExecutor())
        log.record("resume returned")
      }
    }
    log.record("awaiter resumed")
    _ = await counter.increment()
    log.record("actor call returned")
    return value
  }
}

// MARK: - Tests

@main
struct Main {
  typealias DefaultExecutorFactory = TestFactory

  static func main() async {
    let generic = Task.defaultExecutor as! ManualDefaultExecutor

    let tests = TestSuite("Continuation: resuming on the resumer's executors")

    // MARK: Both kinds of continuation

    for kind in ContinuationKind.allCases {
      tests.test("\(kind): offering a nonisolated task's task executor donates the thread, for every form of resume") {
        let pool = ManualTaskExecutor()
        let log = EventLog()
        let task = Task.detached(executorPreference: pool) { () -> [Int] in
          var codes: [Int] = []
          for form in ResumeForm.allCases {
            codes.append(await suspendForCode(kind) { continuation in
              let holder = ContinuationHolder(continuation)
              Task.detached(executorPreference: pool) {
                holder.take().resume(form, on: pool.asUnownedTaskExecutor())
                log.record("resume returned")
              }
            })
            log.record("awaiter resumed")
          }
          return codes
        }
        // The awaiting task up to its first suspension, then one resumer per
        // form, each of which runs the awaiting task on to its next one.
        pool.runNextJob()
        for _ in ResumeForm.allCases { pool.runNextJob() }
        // One donated resume per form.
        expectEqual(
          ["awaiter resumed", "resume returned",
           "awaiter resumed", "resume returned",
           "awaiter resumed", "resume returned",
           "awaiter resumed", "resume returned"],
          log.recorded)
        expectEqual(0, pool.pendingJobCount)
        expectEqual(ResumeForm.allCases.map(\.code), await task.value)
      }

      tests.test("\(kind): offering an unrelated task executor enqueues") {
        let pool = ManualTaskExecutor()
        let unrelated = ManualTaskExecutor()
        let log = EventLog()
        let task = Task.detached(executorPreference: pool) {
          let value = await suspend(kind, of: Int.self, throwing: Never.self) { continuation in
            let holder = ContinuationHolder(continuation)
            Task.detached(executorPreference: unrelated) {
              holder.take().resume(returning: 5, on: unrelated.asUnownedTaskExecutor())
              log.record("resume returned")
            }
          }
          log.record("awaiter resumed")
          return value
        }
        pool.runNextJob()
        unrelated.runNextJob()
        // Enqueued back onto the task's own task executor.
        expectEqual(["resume returned"], log.recorded)
        expectEqual(1, pool.pendingJobCount)
        pool.runNextJob()
        expectEqual(["resume returned", "awaiter resumed"], log.recorded)
        expectEqual(5, await task.value)
      }

      tests.test("\(kind): offering the default executor donates the thread when the task has no isolation") {
        let log = EventLog()
        generic.hold()
        let task = Task.detached {
          let value = await suspend(kind, of: Int.self, throwing: Never.self) { continuation in
            let holder = ContinuationHolder(continuation)
            Task.detached {
              holder.take().resume(
                returning: 6, on: globalConcurrentExecutor.asUnownedTaskExecutor())
              log.record("resume returned")
            }
          }
          log.record("awaiter resumed")
          return value
        }
        // The awaiting task up to its suspension, then the resumer.
        generic.runNextJob()
        generic.runNextJob()
        expectEqual(["awaiter resumed", "resume returned"], log.recorded)
        expectEqual(0, generic.pendingJobCount)
        generic.drainAndRelease()
        expectEqual(6, await task.value)
      }

      tests.test("\(kind): offering a serial executor enqueues when the task has no isolation") {
        let executor = ManualExecutor()
        let log = EventLog()
        generic.hold()
        let task = Task.detached {
          let value = await suspend(kind, of: Int.self, throwing: Never.self) { continuation in
            let holder = ContinuationHolder(continuation)
            Task.detached(executorPreference: executor) {
              holder.take().resume(
                returning: 7, isolatedTo: executor.asUnownedSerialExecutor())
              log.record("resume returned")
            }
          }
          log.record("awaiter resumed")
          return value
        }
        generic.runNextJob()
        executor.runNextJob()
        // Enqueued on the default executor.
        expectEqual(["resume returned"], log.recorded)
        expectEqual(1, generic.pendingJobCount)
        generic.runNextJob()
        expectEqual(["resume returned", "awaiter resumed"], log.recorded)
        generic.drainAndRelease()
        expectEqual(7, await task.value)
      }

      tests.test("\(kind): offering an actor-isolated task's actor executor donates the thread, for every form of resume") {
        let executor = ManualExecutor()
        let isolated = Worker(executor)
        let log = EventLog()
        let task = Task.detached {
          await isolated.run { () -> [Int] in
            var codes: [Int] = []
            for form in ResumeForm.allCases {
              codes.append(await suspendForCode(kind) { continuation in
                let holder = ContinuationHolder(continuation)
                Task.detached(executorPreference: executor) {
                  holder.take().resume(form, isolatedTo: executor.asUnownedSerialExecutor())
                  log.record("resume returned")
                }
              })
              log.record("awaiter resumed")
            }
            return codes
          }
        }
        // The hop onto the actor, up to the first suspension, then one resumer
        // per form.
        executor.runNextJob()
        for _ in ResumeForm.allCases { executor.runNextJob() }
        // One donated resume per form.
        expectEqual(
          ["awaiter resumed", "resume returned",
           "awaiter resumed", "resume returned",
           "awaiter resumed", "resume returned",
           "awaiter resumed", "resume returned"],
          log.recorded)
        expectEqual(0, executor.pendingJobCount)
        expectEqual(ResumeForm.allCases.map(\.code), await task.value)
      }

      tests.test("\(kind): offering another serial executor enqueues on the actor's executor") {
        let executor = ManualExecutor()
        let other = ManualExecutor()
        let isolated = Worker(executor)
        let log = EventLog()
        generic.hold()
        let task = Task.detached {
          await isolated.run {
            let value = await suspend(kind, of: Int.self, throwing: Never.self) { continuation in
              let holder = ContinuationHolder(continuation)
              Task.detached(executorPreference: other) {
                holder.take().resume(returning: 8, isolatedTo: other.asUnownedSerialExecutor())
                log.record("resume returned")
              }
            }
            log.record("awaiter resumed")
            return value
          }
        }
        // The task starts, hops onto the actor and suspends; then the resumer.
        generic.runNextJob()
        executor.runNextJob()
        other.runNextJob()
        // Enqueued directly on the actor's executor, without a detour through
        // the default executor.
        expectEqual(["resume returned"], log.recorded)
        expectEqual(1, executor.pendingJobCount)
        expectEqual(0, generic.pendingJobCount)
        executor.runNextJob()
        expectEqual(["resume returned", "awaiter resumed"], log.recorded)
        generic.drainAndRelease()
        expectEqual(8, await task.value)
      }

      tests.test("\(kind): offering only a task executor enqueues when the task is actor-isolated") {
        let executor = ManualExecutor()
        let preference = ManualTaskExecutor()
        let isolated = Worker(executor)
        let log = EventLog()
        let task = Task.detached(executorPreference: preference) {
          let value = await isolated.run {
            let value = await suspend(kind, of: Int.self, throwing: Never.self) { continuation in
              let holder = ContinuationHolder(continuation)
              Task.detached(executorPreference: preference) {
                holder.take().resume(returning: 9, on: preference.asUnownedTaskExecutor())
                log.record("resume returned")
              }
            }
            log.record("awaiter resumed on the actor")
            return value
          }
          log.record("awaiter left the actor")
          return value
        }
        // The task starts and hops onto the actor, suspends there, then the
        // resumer runs. The task resumes on the actor's executor, which the
        // offer doesn't name.
        preference.runNextJob()
        executor.runNextJob(taskExecutor: preference.asUnownedTaskExecutor())
        preference.runNextJob()
        expectEqual(["resume returned"], log.recorded)
        expectEqual(1, executor.pendingJobCount)
        executor.runNextJob(taskExecutor: preference.asUnownedTaskExecutor())
        // Leaving the actor enqueues the task back onto its task executor.
        expectEqual(["resume returned", "awaiter resumed on the actor"], log.recorded)
        expectEqual(1, preference.pendingJobCount)
        preference.runNextJob()
        expectEqual(
          ["resume returned", "awaiter resumed on the actor", "awaiter left the actor"],
          log.recorded)
        expectEqual(9, await task.value)
      }

      tests.test("\(kind): offering an actor's executor and the task's task executor donates the thread, for every form of resume") {
        let executor = ManualExecutor()
        let preference = ManualTaskExecutor()
        let isolated = Worker(executor)
        let log = EventLog()
        let task = Task.detached(executorPreference: preference) {
          let codes = await isolated.run { () -> [Int] in
            var codes: [Int] = []
            for form in ResumeForm.allCases {
              codes.append(await suspendForCode(kind) { continuation in
                let holder = ContinuationHolder(continuation)
                Task.detached(executorPreference: executor) {
                  holder.take().resume(
                    form,
                    isolatedTo: executor.asUnownedSerialExecutor(),
                    taskExecutor: preference.asUnownedTaskExecutor())
                  log.record("resume returned")
                }
              })
              log.record("awaiter resumed on the actor")
              await isolated.touch()
              log.record("same-actor call returned")
            }
            return codes
          }
          log.record("awaiter left the actor")
          return codes
        }
        // The task starts and hops onto the actor, up to its first suspension,
        // then one resumer per form.
        preference.runNextJob()
        executor.runNextJob(taskExecutor: preference.asUnownedTaskExecutor())
        for _ in ResumeForm.allCases {
          executor.runNextJob(taskExecutor: preference.asUnownedTaskExecutor())
        }
        // Each donated thread is set up on both executors, so it keeps running
        // the task on the actor: through the hop back onto the actor after the
        // await and through the same-actor call. Only leaving the actor
        // enqueues the task back onto its task executor.
        let onTheActor = [
          "awaiter resumed on the actor", "same-actor call returned", "resume returned",
          "awaiter resumed on the actor", "same-actor call returned", "resume returned",
          "awaiter resumed on the actor", "same-actor call returned", "resume returned",
          "awaiter resumed on the actor", "same-actor call returned", "resume returned",
        ]
        expectEqual(onTheActor, log.recorded)
        expectEqual(0, executor.pendingJobCount)
        expectEqual(1, preference.pendingJobCount)
        preference.runNextJob()
        expectEqual(onTheActor + ["awaiter left the actor"], log.recorded)
        expectEqual(ResumeForm.allCases.map(\.code), await task.value)
      }

      tests.test("\(kind): offering only an actor's executor enqueues when the task has a task executor preference") {
        let executor = ManualExecutor()
        let preference = ManualTaskExecutor()
        let isolated = Worker(executor)
        let log = EventLog()
        let task = Task.detached(executorPreference: preference) {
          let value = await isolated.run {
            let value = await suspend(kind, of: Int.self, throwing: Never.self) { continuation in
              let holder = ContinuationHolder(continuation)
              Task.detached(executorPreference: executor) {
                holder.take().resume(returning: 10, isolatedTo: executor.asUnownedSerialExecutor())
                log.record("resume returned")
              }
            }
            log.record("awaiter resumed on the actor")
            return value
          }
          log.record("awaiter left the actor")
          return value
        }
        // The task starts and hops onto the actor, suspends there, then the
        // resumer runs. It runs on both executors but names only the serial
        // one.
        preference.runNextJob()
        executor.runNextJob(taskExecutor: preference.asUnownedTaskExecutor())
        executor.runNextJob(taskExecutor: preference.asUnownedTaskExecutor())
        // Unexpected: code isolated to an actor with a custom executor runs on
        // that executor whatever the task's task executor preference (see
        // `withTaskExecutorPreference`), so offering just the actor's executor
        // should donate the thread.
        // Instead the task is enqueued, because `mustSwitchToRun` compares task
        // executors even when the serial executors match and are an actor's
        // custom executor, and the offer has none while the task prefers one.
        //
        // This isn't specific to continuations: the same comparison makes a
        // task with a task executor preference enqueue itself again at every
        // hop onto such an actor whose executor runs its jobs with only the
        // serial executor (e.g. `runSynchronously(on:)`).
        expectEqual(["resume returned"], log.recorded)
        expectEqual(1, executor.pendingJobCount)
        executor.runNextJob(taskExecutor: preference.asUnownedTaskExecutor())
        expectEqual(["resume returned", "awaiter resumed on the actor"], log.recorded)
        expectEqual(1, preference.pendingJobCount)
        preference.runNextJob()
        expectEqual(
          ["resume returned", "awaiter resumed on the actor", "awaiter left the actor"],
          log.recorded)
        expectEqual(10, await task.value)
      }

      tests.test("\(kind): a donated resume moves a reference-counted value") {
        let pool = ManualTaskExecutor()
        let log = EventLog()
        let task = Task.detached(executorPreference: pool) {
          let tracked = await suspend(kind, of: LifetimeTracked.self, throwing: Never.self) { continuation in
            let holder = ContinuationHolder(continuation)
            Task.detached(executorPreference: pool) {
              holder.take().resume(returning: LifetimeTracked(11), on: pool.asUnownedTaskExecutor())
              log.record("resume returned")
            }
          }
          log.record("awaiter resumed")
          return tracked.value
        }
        pool.runNextJob()
        pool.runNextJob()
        expectEqual(["awaiter resumed", "resume returned"], log.recorded)
        expectEqual(0, pool.pendingJobCount)
        expectEqual(11, await task.value)
      }

      tests.test("\(kind): a donated resume moves a noncopyable value") {
        let pool = ManualTaskExecutor()
        let log = EventLog()
        let drops = DropCounter()
        let task = Task.detached(executorPreference: pool) {
          let token = await suspend(kind, of: Token.self, throwing: Never.self) { continuation in
            let holder = ContinuationHolder(continuation)
            Task.detached(executorPreference: pool) {
              holder.take().resume(
                returning: Token(value: 12, drops: drops), on: pool.asUnownedTaskExecutor())
              log.record("resume returned")
            }
          }
          log.record("awaiter resumed")
          return token.value
        }
        pool.runNextJob()
        pool.runNextJob()
        expectEqual(["awaiter resumed", "resume returned"], log.recorded)
        expectEqual(0, pool.pendingJobCount)
        expectEqual(12, await task.value)
        expectEqual(1, drops.value)
      }

      tests.test("\(kind): resuming with an offer before the await continues inline") {
        let pool = ManualTaskExecutor()
        let task = Task.detached(executorPreference: pool) {
          await suspend(kind, of: Int.self, throwing: Never.self) { continuation in
            continuation.resume(returning: 13, on: pool.asUnownedTaskExecutor())
          }
        }
        // The awaiting task finds the continuation already resumed.
        pool.runNextJob()
        expectEqual(0, pool.pendingJobCount)
        expectEqual(13, await task.value)
      }

      tests.test("\(kind): a resumer that isn't a task donates its thread") {
        let pool = ManualTaskExecutor()
        let log = EventLog()
        let handOff = ContinuationHolder<Int, Never>()
        let task = Task.detached(executorPreference: pool) {
          let value = await suspend(kind, of: Int.self, throwing: Never.self) { continuation in
            handOff.store(continuation)
          }
          log.record("awaiter resumed")
          return value
        }
        // Drive the executor from a thread that runs no task.
        let (_, thread) = _stdlib_thread_create_block({ _ in
          pool.runNextJob()
          handOff.take().resume(returning: 14, on: pool.asUnownedTaskExecutor())
          log.record("resume returned")
        }, ())
        _ = _stdlib_thread_join(thread!, Void.self)
        expectEqual(["awaiter resumed", "resume returned"], log.recorded)
        expectEqual(0, pool.pendingJobCount)
        expectEqual(14, await task.value)
      }

      tests.test("\(kind): a task cancelled while suspended is still resumed by a donated resume") {
        let pool = ManualTaskExecutor()
        let log = EventLog()
        let task = Task.detached(executorPreference: pool) {
          let value = await suspend(kind, of: Int.self, throwing: Never.self) { continuation in
            let holder = ContinuationHolder(continuation)
            Task.detached(executorPreference: pool) {
              holder.take().resume(returning: 15, on: pool.asUnownedTaskExecutor())
              log.record("resume returned")
            }
          }
          log.record(Task.isCancelled ? "awaiter resumed, cancelled" : "awaiter resumed")
          return value
        }
        pool.runNextJob()
        // Cancelling doesn't resume the continuation, nor enqueue anything.
        task.cancel()
        expectEqual([], log.recorded)
        expectEqual(1, pool.pendingJobCount)
        pool.runNextJob()
        expectEqual(["awaiter resumed, cancelled", "resume returned"], log.recorded)
        expectEqual(0, pool.pendingJobCount)
        expectEqual(15, await task.value)
      }

      tests.test("\(kind): a donated resume after Task.immediate's synchronous start can take over an idle actor") {
        let counter = Counter()
        let log = EventLog()
        generic.hold()
        // Runs synchronously up to the await, queueing the resumer.
        let task = startImmediatelyAndCallActor(kind, counter, log)
        generic.runNextJob()
        // The donated thread runs the task on, taking over the idle actor for
        // the actor call, all before the resume returns.
        expectEqual(["awaiter resumed", "actor call returned", "resume returned"], log.recorded)
        generic.drainAndRelease()
        expectEqual(19, await task.value)
      }
    }

    // MARK: Split continuations only

    tests.test("split continuation: a donated resume with handlers installed, after a cancellation ran the cancellation handler") {
      let pool = ManualTaskExecutor()
      let log = EventLog()
      let task = Task.detached(executorPreference: pool) {
        await withContinuation(of: Int.self, throwing: Never.self) {
          (continuation: consuming Continuation<Int, Never>,
           awaiter: consuming ContinuationAwaiter<Int, Never>) in
          let holder = ContinuationHolder(continuation)
          Task.detached(executorPreference: pool) {
            holder.take().resume(returning: 16, on: pool.asUnownedTaskExecutor())
            log.record("resume returned")
          }
          let value = await awaiter.wait(
            onCancel: { log.record("cancellation handler") },
            onEscalate: { _ in log.record("escalation handler") })
          log.record("awaiter resumed")
          return value
        }
      }
      // The awaiting task suspends with its handlers installed.
      pool.runNextJob()
      // Cancelling runs the cancellation handler without resuming.
      task.cancel()
      expectEqual(["cancellation handler"], log.recorded)
      expectEqual(1, pool.pendingJobCount)
      pool.runNextJob()
      expectEqual(["cancellation handler", "awaiter resumed", "resume returned"], log.recorded)
      expectEqual(0, pool.pendingJobCount)
      expectEqual(16, await task.value)
    }

    // A split continuation can be awaited somewhere other than where it was
    // created, and an offer is checked against where it was awaited: offering
    // where it was created enqueues the task where it was awaited instead.

    tests.test("split continuation: offering the executor of the actor it is awaited on donates the thread") {
      let creatingExecutor = ManualExecutor()
      let awaitingExecutor = ManualExecutor()
      let creating = Worker(creatingExecutor)
      let awaiting = Worker(awaitingExecutor)
      let log = EventLog()
      let task = Task.detached {
        await creating.run {
          await withContinuation(of: Int.self, throwing: Never.self) {
            (continuation: consuming Continuation<Int, Never>,
             awaiter: consuming ContinuationAwaiter<Int, Never>) in
            let holder = ContinuationHolder(continuation)
            return await awaiting.awaitIt(awaiter, log: log) {
              // Start the resumer only now, so that it runs after the task has
              // suspended rather than resuming ahead of the await.
              Task.detached(executorPreference: awaitingExecutor) {
                holder.take().resume(
                  returning: 17, isolatedTo: awaitingExecutor.asUnownedSerialExecutor())
                log.record("resume returned")
              }
            }
          }
        }
      }
      // The task creates the continuation on the creating actor, then hops
      // onto the awaiting actor, where it starts the resumer and awaits.
      creatingExecutor.runNextJob()
      awaitingExecutor.runNextJob()
      // The resumer, offering the awaiting actor's executor.
      awaitingExecutor.runNextJob()
      expectEqual(["awaiter resumed", "resume returned"], log.recorded)
      // Returning from the awaiting actor takes the task back to the creating
      // one.
      expectEqual(1, creatingExecutor.pendingJobCount)
      creatingExecutor.runNextJob()
      expectEqual(17, await task.value)
    }

    tests.test("split continuation: offering the executor of the actor it was created on enqueues on the one it is awaited on") {
      let creatingExecutor = ManualExecutor()
      let awaitingExecutor = ManualExecutor()
      let creating = Worker(creatingExecutor)
      let awaiting = Worker(awaitingExecutor)
      let log = EventLog()
      let task = Task.detached {
        await creating.run {
          await withContinuation(of: Int.self, throwing: Never.self) {
            (continuation: consuming Continuation<Int, Never>,
             awaiter: consuming ContinuationAwaiter<Int, Never>) in
            let holder = ContinuationHolder(continuation)
            return await awaiting.awaitIt(awaiter, log: log) {
              // Start the resumer only now, so that it runs after the task has
              // suspended rather than resuming ahead of the await.
              Task.detached(executorPreference: creatingExecutor) {
                holder.take().resume(
                  returning: 20, isolatedTo: creatingExecutor.asUnownedSerialExecutor())
                log.record("resume returned")
              }
            }
          }
        }
      }
      // The task creates the continuation on the creating actor, then hops
      // onto the awaiting actor, where it starts the resumer and awaits.
      creatingExecutor.runNextJob()
      awaitingExecutor.runNextJob()
      // The resumer, offering the creating actor's executor.
      creatingExecutor.runNextJob()
      expectEqual(["resume returned"], log.recorded)
      expectEqual(1, awaitingExecutor.pendingJobCount)
      awaitingExecutor.runNextJob()
      expectEqual(["resume returned", "awaiter resumed"], log.recorded)
      // Returning from the awaiting actor takes the task back to the creating
      // one.
      expectEqual(1, creatingExecutor.pendingJobCount)
      creatingExecutor.runNextJob()
      expectEqual(20, await task.value)
    }

    // A split continuation can likewise be awaited under a different task
    // executor preference than the one it was created under.

    tests.test("split continuation: offering the preference taken before the await donates the thread") {
      let atCreation = ManualTaskExecutor()
      let atAwait = ManualTaskExecutor()
      let log = EventLog()
      let task = Task.detached(executorPreference: atCreation) {
        await withContinuation(of: Int.self, throwing: Never.self) {
          (continuation: consuming Continuation<Int, Never>,
           awaiter: consuming ContinuationAwaiter<Int, Never>) in
          let holder = ContinuationHolder(continuation)
          var slot: ContinuationAwaiter<Int, Never>? = consume awaiter
          return await withTaskExecutorPreference(atAwait) {
            // Start the resumer only now, so that it runs after the task has
            // suspended rather than resuming ahead of the await.
            Task.detached(executorPreference: atAwait) {
              holder.take().resume(returning: 18, on: atAwait.asUnownedTaskExecutor())
              log.record("resume returned")
            }
            let value = await slot.take()!.wait()
            log.record("awaiter resumed")
            return value
          }
        }
      }
      // The task creates the continuation and hops onto its new preference,
      // where it starts the resumer and awaits.
      atCreation.runNextJob()
      atAwait.runNextJob()
      // The resumer, offering the preference at the await.
      atAwait.runNextJob()
      expectEqual(["awaiter resumed", "resume returned"], log.recorded)
      // Leaving the preference's scope takes the task back to the one it was
      // created under.
      expectEqual(1, atCreation.pendingJobCount)
      atCreation.runNextJob()
      expectEqual(18, await task.value)
    }

    tests.test("split continuation: offering the preference from creation enqueues on the one at the await") {
      let atCreation = ManualTaskExecutor()
      let atAwait = ManualTaskExecutor()
      let log = EventLog()
      let task = Task.detached(executorPreference: atCreation) {
        await withContinuation(of: Int.self, throwing: Never.self) {
          (continuation: consuming Continuation<Int, Never>,
           awaiter: consuming ContinuationAwaiter<Int, Never>) in
          let holder = ContinuationHolder(continuation)
          var slot: ContinuationAwaiter<Int, Never>? = consume awaiter
          return await withTaskExecutorPreference(atAwait) {
            // Start the resumer only now, so that it runs after the task has
            // suspended rather than resuming ahead of the await.
            Task.detached(executorPreference: atCreation) {
              holder.take().resume(returning: 21, on: atCreation.asUnownedTaskExecutor())
              log.record("resume returned")
            }
            let value = await slot.take()!.wait()
            log.record("awaiter resumed")
            return value
          }
        }
      }
      // The task creates the continuation and hops onto its new preference,
      // where it starts the resumer and awaits.
      atCreation.runNextJob()
      atAwait.runNextJob()
      // The resumer, offering the preference at creation.
      atCreation.runNextJob()
      expectEqual(["resume returned"], log.recorded)
      expectEqual(1, atAwait.pendingJobCount)
      atAwait.runNextJob()
      expectEqual(["resume returned", "awaiter resumed"], log.recorded)
      // Leaving the preference's scope takes the task back to the one it was
      // created under.
      expectEqual(1, atCreation.pendingJobCount)
      atCreation.runNextJob()
      expectEqual(21, await task.value)
    }

    await runAllTestsAsync()
  }
}
