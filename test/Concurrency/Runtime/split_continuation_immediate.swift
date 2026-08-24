// RUN: %target-run-simple-swift( -target %target-swift-5.1-abi-triple -parse-as-library -disable-availability-checking -swift-version 6 -enable-experimental-feature SplitContinuations)
// RUN: %target-run-simple-swift( -target %target-swift-5.1-abi-triple -parse-as-library -disable-availability-checking -swift-version 6 -O -enable-experimental-feature SplitContinuations)

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime
// REQUIRES: swift_feature_SplitContinuations
// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: back_deploy_concurrency
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: freestanding

@_spi(Concurrency) import _Concurrency
@_spi(ExperimentalCustomExecutors) import _Concurrency
import StdlibUnittest
import Synchronization

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
}

@main struct Main {
  typealias DefaultExecutorFactory = TestFactory

  static func main() async {
    let tests = TestSuite("Continuation: split continuation under Task.immediate")

    tests.test("immediate task, resumed after it suspends") {
      let executor = ManualExecutor()
      let task = Task.immediate {
        await withContinuation(of: Int.self, throwing: Never.self) {
          (continuation: consuming Continuation<Int, Never>,
           awaiter: consuming ContinuationAwaiter<Int, Never>) in
          let holder = ContinuationHolder(continuation)
          Task.detached(executorPreference: executor) { holder.take().resume(returning: 17) }
          return await awaiter.wait()
        }
      }
      executor.runNextJob()
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
      let executor = ManualExecutor()
      let task = Task.immediate {
        await withContinuation(of: Int.self, throwing: Never.self) {
          (continuation: consuming Continuation<Int, Never>,
           awaiter: consuming ContinuationAwaiter<Int, Never>) in
          let holder = ContinuationHolder(continuation)
          Task.detached(executorPreference: executor) { holder.take().resume(returning: 19) }
          return await awaiter.wait(onCancel: {}, onEscalate: { _ in })
        }
      }
      executor.runNextJob()
      expectEqual(19, await task.value)
    }

    tests.test("immediate task from a nonisolated caller enqueues when the offer isn't the default") {
      let executor = ManualExecutor()
      let task = Task.immediate {
        await withContinuation(of: Int.self, throwing: Never.self) {
          (continuation: consuming Continuation<Int, Never>,
           awaiter: consuming ContinuationAwaiter<Int, Never>) in
          let holder = ContinuationHolder(continuation)
          Task.detached(executorPreference: executor) {
            holder.take().resume(returning: 20, isolatedTo: executor.asUnownedSerialExecutor())
          }
          return await awaiter.wait()
        }
      }
      executor.runNextJob()
      expectEqual(20, await task.value)
    }

    tests.test("immediate task from a nonisolated caller donates when the offer is the default") {
      let executor = ManualExecutor()
      let task = Task.immediate {
        await withContinuation(of: Int.self, throwing: Never.self) {
          (continuation: consuming Continuation<Int, Never>,
           awaiter: consuming ContinuationAwaiter<Int, Never>) in
          let holder = ContinuationHolder(continuation)
          Task.detached(executorPreference: executor) {
            holder.take().resume(returning: 22, on: globalConcurrentExecutor.asUnownedTaskExecutor())
          }
          return await awaiter.wait()
        }
      }
      executor.runNextJob()
      expectEqual(22, await task.value)
    }

    tests.test("immediate task from an isolated caller enqueues") {
      let executor = ManualExecutor()
      let unrelated = ManualExecutor()
      let isolated = IsolatedActor(executor)
      let outer = Task.immediate {
        await isolated.run {
          let task = Task.immediate {
            await withContinuation(of: Int.self, throwing: Never.self) {
              (continuation: consuming Continuation<Int, Never>,
               awaiter: consuming ContinuationAwaiter<Int, Never>) in
              let holder = ContinuationHolder(continuation)
              Task.detached(executorPreference: unrelated) {
                holder.take().resume(
                  returning: 21, isolatedTo: unrelated.asUnownedSerialExecutor())
              }
              return await awaiter.wait()
            }
          }
          return await task.value
        }
      }
      executor.runNextJob()
      unrelated.runNextJob()
      executor.runNextJob()
      expectEqual(21, await outer.value)
    }

    await runAllTestsAsync()
  }
}
