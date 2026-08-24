// RUN: %target-run-simple-swift( -target %target-swift-5.1-abi-triple -parse-as-library -disable-availability-checking -swift-version 6 -enable-experimental-feature SplitContinuations)
// RUN: %target-run-simple-swift( -target %target-swift-5.1-abi-triple -parse-as-library -disable-availability-checking -swift-version 6 -O -enable-experimental-feature SplitContinuations)

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime
// REQUIRES: swift_feature_SplitContinuations
// UNSUPPORTED: back_deployment_runtime

// XFAIL: swift_test_mode_optimize_none_with_opaque_values

@_spi(Concurrency) import _Concurrency
@_spi(ExperimentalCustomExecutors) import _Concurrency
import StdlibUnittest
import Synchronization

// An executor that queues jobs instead of running them, so tests can step
// through suspend/resume/donation interleavings deterministically.
final class ManualExecutor: SerialExecutor, TaskExecutor, Sendable {
  private let jobs = Mutex<[UnownedJob]>([])

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

  // Runs exactly one enqueued job, trapping if none is enqueued.
  func runNextJob() {
    guard let job = popJob() else {
      preconditionFailure("runNextJob() called with no job enqueued")
    }
    unsafe job.runSynchronously(on: asUnownedSerialExecutor())
  }

  // Runs exactly `count` enqueued jobs.
  func runNextJobs(count: Int) {
    for _ in 0..<count { runNextJob() }
  }
}


final class ManualDefaultExecutor: TaskExecutor, Sendable {
  private let jobs = Mutex<[UnownedJob]>([])
  private let held = Mutex<Bool>(false)

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

  private func popJob() -> UnownedJob? {
    jobs.withLock { $0.isEmpty ? nil : $0.removeFirst() }
  }

  func hold() { held.withLock { $0 = true } }
  func release() { held.withLock { $0 = false } }

  func runNextJob() {
    guard let job = popJob() else {
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

final class ContinuationHolder<Success: ~Copyable, Failure: Error>: Sendable {
  private let storage: Mutex<Continuation<Success, Failure>?>

  init(_ continuation: consuming Continuation<Success, Failure>) {
    storage = Mutex(continuation)
  }

  func take() -> Continuation<Success, Failure> {
    storage.withLock { $0.take()! }
  }
}

actor IsolatedActor {
  private let _executor: ManualExecutor
  nonisolated var unownedExecutor: UnownedSerialExecutor { _executor.asUnownedSerialExecutor() }
  init(_ executor: ManualExecutor) { self._executor = executor }

  func run<T: Sendable>(_ body: sending nonisolated(nonsending) () async -> T) async -> T {
    await body()
  }

  func awaitIt(_ awaiter: consuming ContinuationAwaiter<Int, Never>) async -> Int {
    await awaiter.wait()
  }
}

@main struct Main {
  typealias DefaultExecutorFactory = TestFactory

  static func main() async {
    let generic = Task.defaultExecutor as! ManualDefaultExecutor

    let tests = TestSuite("Continuation: resuming on the resumer's own thread")

    tests.test("passing the awaiting task's executor donates the thread") {
      let executor = ManualExecutor()
      let isolated = IsolatedActor(executor)
      let task = Task.immediate {
        await isolated.run {
          await withContinuation(of: Int.self, throwing: Never.self) {
            (continuation: consuming Continuation<Int, Never>,
             awaiter: consuming ContinuationAwaiter<Int, Never>) in
            let holder = ContinuationHolder(continuation)
            Task.detached(executorPreference: executor) {
              holder.take().resume(returning: 99, isolatedTo: executor.asUnownedSerialExecutor())
            }
            return await awaiter.wait()
          }
        }
      }
      // 2 jobs: the actor hop, then the resumer which completes the whole chain
      // inline.
      executor.runNextJobs(count: 2)
      expectEqual(99, await task.value)
    }

    tests.test("donating the thread with handlers installed") {
      let executor = ManualExecutor()
      let isolated = IsolatedActor(executor)
      let task = Task.immediate {
        await isolated.run {
          await withContinuation(of: Int.self, throwing: Never.self) {
            (continuation: consuming Continuation<Int, Never>,
             awaiter: consuming ContinuationAwaiter<Int, Never>) in
            let holder = ContinuationHolder(continuation)
            Task.detached(executorPreference: executor) {
              holder.take().resume(
                with: .success(100), isolatedTo: executor.asUnownedSerialExecutor())
            }
            return await awaiter.wait(onCancel: {}, onEscalate: { _ in })
          }
        }
      }
      executor.runNextJobs(count: 2)
      expectEqual(100, await task.value)
    }

    tests.test("passing another executor enqueues") {
      let rightExecutor = ManualExecutor()
      let wrongExecutor = ManualExecutor()
      let isolated = IsolatedActor(rightExecutor)
      let task = Task.immediate {
        await isolated.run {
          await withContinuation(of: Int.self, throwing: Never.self) {
            (continuation: consuming Continuation<Int, Never>,
             awaiter: consuming ContinuationAwaiter<Int, Never>) in
            let holder = ContinuationHolder(continuation)
            Task.detached(executorPreference: wrongExecutor) {
              holder.take().resume(
                returning: 1, isolatedTo: wrongExecutor.asUnownedSerialExecutor())
            }
            return await awaiter.wait()
          }
        }
      }
      // The actor hop (rightExecutor), the resumer (wrongExecutor, whose
      // offer can't be taken since the task really resumes on
      // rightExecutor), and the resulting fallback enqueue back onto
      // rightExecutor to actually complete it.
      rightExecutor.runNextJob()
      wrongExecutor.runNextJob()
      rightExecutor.runNextJob()
      expectEqual(1, await task.value)
    }

    tests.test("awaiting from another isolation domain resumes across the hop") {
      // Creates the continuation on this actor's executor but awaits from
      // another isolation domain (a second `IsolatedActor`, isolated to its own
      // executor), so the task resumes there instead.
      let executor = ManualExecutor()
      let elsewhereExecutor = ManualExecutor()
      let isolated = IsolatedActor(executor)
      let other = IsolatedActor(elsewhereExecutor)
      let task = Task.immediate {
        await isolated.run {
          await withContinuation(of: Int.self, throwing: Never.self) {
            (continuation: consuming Continuation<Int, Never>,
             awaiter: consuming ContinuationAwaiter<Int, Never>) in
            let holder = ContinuationHolder(continuation)
            Task.detached(executorPreference: executor) {
              holder.take().resume(returning: 42, isolatedTo: executor.asUnownedSerialExecutor())
            }
            return await other.awaitIt(awaiter)
          }
        }
      }
      executor.runNextJob()
      executor.runNextJob()
      elsewhereExecutor.runNextJob()
      executor.runNextJob()
      expectEqual(42, await task.value)
    }

    tests.test("a task executor preference enqueues") {
      let executor = ManualExecutor()
      let preference = ManualExecutor()
      let isolated = IsolatedActor(executor)
      let task = Task(executorPreference: preference) {
        await isolated.run {
          await withContinuation(of: Int.self, throwing: Never.self) {
            (continuation: consuming Continuation<Int, Never>,
             awaiter: consuming ContinuationAwaiter<Int, Never>) in
            let holder = ContinuationHolder(continuation)
            Task.detached(executorPreference: executor) {
              holder.take().resume(
                returning: 99, isolatedTo: executor.asUnownedSerialExecutor())
            }
            return await awaiter.wait()
          }
        }
      }
      preference.runNextJob()
      executor.runNextJobs(count: 4)
      expectEqual(99, await task.value)
    }

    tests.test("passing an executor on an ordinary continuation just enqueues") {
      let executor = ManualExecutor()
      let unrelated = ManualExecutor()
      let isolated = IsolatedActor(executor)
      generic.hold()
      let task = Task.immediate {
        await isolated.run {
          await withContinuation(of: Int.self, throwing: Never.self) {
            (continuation: consuming Continuation<Int, Never>) in
            let holder = ContinuationHolder(continuation)
            Task.detached(executorPreference: unrelated) {
              holder.take().resume(
                returning: 7, isolatedTo: unrelated.asUnownedSerialExecutor())
            }
          }
        }
      }
      executor.runNextJob()
      unrelated.runNextJob()
      generic.runNextJob()
      executor.runNextJob()
      generic.release()
      expectEqual(7, await task.value)
    }

    tests.test("passing only a task executor donates when the task matches it") {
      let preference = ManualExecutor()
      let task = Task(executorPreference: preference) {
        await withContinuation(of: Int.self, throwing: Never.self) {
          (continuation: consuming Continuation<Int, Never>,
           awaiter: consuming ContinuationAwaiter<Int, Never>) in
          let holder = ContinuationHolder(continuation)
          Task.detached(executorPreference: preference) {
            holder.take().resume(returning: 33, on: preference.asUnownedTaskExecutor())
          }
          return await awaiter.wait()
        }
      }
      preference.runNextJobs(count: 3)
      expectEqual(33, await task.value)
    }

    tests.test("passing only a task executor enqueues when the task is actor-isolated") {
      let executor = ManualExecutor()
      let preference = ManualExecutor()
      let isolated = IsolatedActor(executor)
      let task = Task(executorPreference: preference) {
        await isolated.run {
          await withContinuation(of: Int.self, throwing: Never.self) {
            (continuation: consuming Continuation<Int, Never>,
             awaiter: consuming ContinuationAwaiter<Int, Never>) in
            let holder = ContinuationHolder(continuation)
            Task.detached(executorPreference: preference) {
              holder.take().resume(returning: 55, on: preference.asUnownedTaskExecutor())
            }
            return await awaiter.wait()
          }
        }
      }
      preference.runNextJob()
      executor.runNextJob()
      preference.runNextJob()
      executor.runNextJobs(count: 2)
      expectEqual(55, await task.value)
    }

    tests.test("passing an unrelated executor enqueues when the task has no isolation at all") {
      let executor = ManualExecutor()
      generic.hold()
      let task = Task.detached {
        await withContinuation(of: Int.self, throwing: Never.self) {
          (continuation: consuming Continuation<Int, Never>,
           awaiter: consuming ContinuationAwaiter<Int, Never>) in
          let holder = ContinuationHolder(continuation)
          Task.detached(executorPreference: executor) {
            holder.take().resume(
              returning: 66, isolatedTo: executor.asUnownedSerialExecutor())
          }
          return await awaiter.wait()
        }
      }
      generic.runNextJob()
      executor.runNextJob()
      generic.runNextJob()
      generic.release()
      expectEqual(66, await task.value)
    }

    tests.test("passing the actual default task executor donates when the task has no isolation") {
      let executor = ManualExecutor()
      generic.hold()
      let task = Task.detached {
        await withContinuation(of: Int.self, throwing: Never.self) {
          (continuation: consuming Continuation<Int, Never>,
           awaiter: consuming ContinuationAwaiter<Int, Never>) in
          let holder = ContinuationHolder(continuation)
          Task.detached(executorPreference: executor) {
            holder.take().resume(returning: 77, on: globalConcurrentExecutor.asUnownedTaskExecutor())
          }
          return await awaiter.wait()
        }
      }
      generic.runNextJob()
      executor.runNextJob()
      generic.release()
      expectEqual(77, await task.value)
    }

    await runAllTestsAsync()
  }
}
