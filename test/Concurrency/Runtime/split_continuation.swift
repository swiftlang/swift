// RUN: %target-run-simple-swift( -target %target-swift-5.1-abi-triple -parse-as-library -disable-availability-checking -swift-version 6) 2>&1 | %FileCheck %s
// RUN: %target-run-simple-swift( -target %target-swift-5.1-abi-triple -parse-as-library -disable-availability-checking -swift-version 6 -O) 2>&1 | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime

@_spi(Concurrency) import _Concurrency
import StdlibUnittest
import Synchronization

struct TestError: Error {}
struct DifferentError: Error {}

struct UniqueResource: ~Copyable {
  let value: Int
  init(_ value: Int) { self.value = value }
  deinit { print("UniqueResource(\(value)).deinit") }
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

// Handlers run holding the status-record lock, so they can't resume inline.
extension ContinuationHolder where Success: Sendable {
  func resumeFromDetachedTask(
    returning value: Success, executorPreference executor: (any TaskExecutor)?
  ) {
    Task.detached(executorPreference: executor) { [self] in
      self.take().resume(returning: value)
    }
  }
}

// A task executor that queues jobs instead of running them, so tests can
// step through suspend/resume interleavings deterministically.
final class ManualExecutor: TaskExecutor, Sendable {
  private let jobs = Mutex<[UnownedJob]>([])

  func enqueue(_ job: UnownedJob) {
    jobs.withLock { $0.append(job) }
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
    unsafe job.runSynchronously(on: asUnownedTaskExecutor())
  }

  // Runs exactly `count` enqueued jobs.
  func runNextJobs(count: Int) {
    for _ in 0..<count { runNextJob() }
  }
}

final class Box<Value: Sendable>: Sendable {
  private let storage: Mutex<Value>

  init(_ value: Value) {
    storage = Mutex(value)
  }

  var value: Value {
    get { storage.withLock { $0 } }
    set { storage.withLock { $0 = newValue } }
  }

  func modify<Result: Sendable>(_ body: (inout sending Value) -> sending Result) -> Result {
    storage.withLock(body)
  }
}

func one() async -> Int { 1 }

// Raised alignment; 16 is the max a Swift type can request.
@_alignment(16)
struct OverAligned {
  var a: Int
  var b: Int
}

struct Large {
  var head: Int
  var padding = (0, 0, 0, 0, 0, 0, 0, 0)
  var tail: Int
}

// Reached generically, so the result is address-only.
struct Boxed<Payload> {
  var payload: Payload
}

func roundTripGenerically<T>(_ value: T) async -> T where T: Sendable {
  await withContinuation(of: T.self, throwing: Never.self) {
    (continuation: consuming Continuation<T, Never>,
     waiter: consuming ContinuationWaiter<T, Never>) in
    continuation.resume(returning: value)
    return await waiter.wait()
  }
}

// A separate isolation domain.
actor Elsewhere {
  func awaitIt(_ waiter: consuming ContinuationWaiter<Int, Never>) async -> Int {
    await waiter.wait()
  }
}

@main struct Main {
  static func main() async {
    let tests = TestSuite("Continuation: split continuation")

    // MARK: - Resume before await.

    tests.test("resume before await, returning") {
      let value = await withContinuation(of: Int.self, throwing: Never.self) {
        (continuation: consuming Continuation<Int, Never>,
         waiter: consuming ContinuationWaiter<Int, Never>) in
        continuation.resume(returning: 42)
        return await waiter.wait()
      }
      expectEqual(42, value)
    }

    tests.test("resume before await, Void") {
      await withContinuation(of: Void.self, throwing: Never.self) {
        (continuation: consuming Continuation<Void, Never>,
         waiter: consuming ContinuationWaiter<Void, Never>) in
        continuation.resume(returning: ())
        return await waiter.wait()
      }
    }

    tests.test("resume before await, throwing continuation returning") {
      do {
        let value = try await withContinuation(of: Int.self, throwing: (any Error).self) {
          (continuation: consuming Continuation<Int, any Error>,
           waiter: consuming ContinuationWaiter<Int, any Error>) in
          continuation.resume(returning: 17)
          return try await waiter.wait()
        }
        expectEqual(17, value)
      } catch {
        expectUnreachable("unexpected error: \(error)")
      }
    }

    tests.test("resume before await, throwing continuation throwing") {
      do {
        _ = try await withContinuation(of: Int.self, throwing: (any Error).self) {
          (continuation: consuming Continuation<Int, any Error>,
           waiter: consuming ContinuationWaiter<Int, any Error>) in
          continuation.resume(throwing: TestError())
          return try await waiter.wait()
        }
        expectUnreachable("wait() should have thrown")
      } catch {
        expectTrue(error is TestError)
      }
    }

    tests.test("resume before await, with Result") {
      let value = await withContinuation(of: Int.self, throwing: Never.self) {
        (continuation: consuming Continuation<Int, Never>,
         waiter: consuming ContinuationWaiter<Int, Never>) in
        continuation.resume(with: .success(7))
        return await waiter.wait()
      }
      expectEqual(7, value)
    }

    tests.test("resume before await, ~Copyable Success round-trips") {
      let resource = await withContinuation(of: UniqueResource.self, throwing: Never.self) {
        (continuation: consuming Continuation<UniqueResource, Never>,
         waiter: consuming ContinuationWaiter<UniqueResource, Never>) in
        continuation.resume(returning: UniqueResource(99))
        return await waiter.wait()
      }
      expectEqual(99, resource.value)
      _ = consume resource
      // CHECK: UniqueResource(99).deinit
    }

    // MARK: - Resume after await.

    tests.test("resume after await, from another task") {
      let executor = ManualExecutor()
      let task = Task(executorPreference: executor) {
        await withContinuation(of: Int.self, throwing: Never.self) {
          (continuation: consuming Continuation<Int, Never>,
           waiter: consuming ContinuationWaiter<Int, Never>) in
          let holder = ContinuationHolder(continuation)
          // Enqueued on the same executor as the awaiting task, so nothing
          // runs this until the task above has genuinely suspended.
          Task.detached(executorPreference: executor) {
            holder.take().resume(returning: 314)
          }
          return await waiter.wait()
        }
      }
      executor.runNextJobs(count: 3)
      expectEqual(314, await task.value)
    }

    tests.test("resume after await, throwing from another task") {
      let executor = ManualExecutor()
      let task = Task(executorPreference: executor) {
        try await withContinuation(of: Int.self, throwing: (any Error).self) {
          (continuation: consuming Continuation<Int, any Error>,
           waiter: consuming ContinuationWaiter<Int, any Error>) in
          let holder = ContinuationHolder(continuation)
          Task.detached(executorPreference: executor) {
            holder.take().resume(throwing: TestError())
          }
          return try await waiter.wait()
        }
      }
      executor.runNextJobs(count: 3)
      do {
        _ = try await task.value
        expectUnreachable("wait() should have thrown")
      } catch {
        expectTrue(error is TestError)
      }
    }

    // MARK: - Handlers installed on the await.

    tests.test("handlers are not run when nothing happens") {
      let cancelled = Box(false)
      let escalated = Box(false)
      let value = await withContinuation(of: Int.self, throwing: Never.self) {
        (continuation: consuming Continuation<Int, Never>,
         waiter: consuming ContinuationWaiter<Int, Never>) in
        continuation.resume(returning: 5)
        return await waiter.wait(
          onCancel: { cancelled.value = true },
          onEscalate: { _ in escalated.value = true })
      }
      expectEqual(5, value)
      expectFalse(cancelled.value)
      expectFalse(escalated.value)
    }

    tests.test("cancellation handler runs once while suspended and can resume") {
      // Only possible because the handler is installed on the await.
      let runs = Box(0)
      let executor = ManualExecutor()
      let task = Task(executorPreference: executor) { () -> (Int, Bool) in
        let value = await withContinuation(of: Int.self, throwing: Never.self) {
          (continuation: consuming Continuation<Int, Never>,
           waiter: consuming ContinuationWaiter<Int, Never>) in
          let holder = ContinuationHolder(continuation)
          return await waiter.wait(
            onCancel: {
              runs.modify { $0 += 1 }
              holder.resumeFromDetachedTask(returning: -1, executorPreference: executor)
            },
            onEscalate: { _ in })
        }
        return (value, Task.isCancelled)
      }
      executor.runNextJob() // The one job: runs the task to its suspension.
      task.cancel()
      task.cancel()
      executor.runNextJobs(count: 2) // Runs the cancellation handler's resume.
      let (value, isCancelled) = await task.value
      expectEqual(-1, value)
      expectEqual(1, runs.value)
      // The awaiting task observes its own cancellation.
      expectTrue(isCancelled)
    }

    tests.test("cancellation handler runs when the task is already cancelled") {
      // Cancelling before draining the executor guarantees the task is
      // already cancelled before it ever runs.
      let executor = ManualExecutor()
      let task = Task(executorPreference: executor) {
        expectTrue(Task.isCancelled)
        return await withContinuation(of: Int.self, throwing: Never.self) {
          (continuation: consuming Continuation<Int, Never>,
           waiter: consuming ContinuationWaiter<Int, Never>) in
          let holder = ContinuationHolder(continuation)
          return await waiter.wait(
            onCancel: { holder.resumeFromDetachedTask(returning: -2, executorPreference: executor) },
            onEscalate: { _ in })
        }
      }
      task.cancel()
      executor.runNextJobs(count: 3)
      expectEqual(-2, await task.value)
    }

    tests.test("cancellation-only handler runs while suspended and can resume") {
      let executor = ManualExecutor()
      let task = Task(executorPreference: executor) {
        await withContinuation(of: Int.self, throwing: Never.self) {
          (continuation: consuming Continuation<Int, Never>,
           waiter: consuming ContinuationWaiter<Int, Never>) in
          let holder = ContinuationHolder(continuation)
          return await waiter.wait(
            onCancel: { holder.resumeFromDetachedTask(returning: -20, executorPreference: executor) })
        }
      }
      executor.runNextJob() // The one job: runs the task to its suspension.
      task.cancel()
      executor.runNextJobs(count: 2) // Runs the cancellation handler's resume.
      expectEqual(-20, await task.value)
    }

    tests.test("handlers are uninstalled once the await resolves") {
      // Cancelling after wait() returns must not run the handler.
      let runs = Box(0)
      let executor = ManualExecutor()
      let task = Task(executorPreference: executor) {
        await withContinuation(of: Int.self, throwing: Never.self) {
          (continuation: consuming Continuation<Int, Never>,
           waiter: consuming ContinuationWaiter<Int, Never>) in
          continuation.resume(returning: 1)
          let value = await waiter.wait(
            onCancel: { runs.modify { $0 += 1 } },
            onEscalate: { _ in })
          await withTaskCancellationHandler {
            try? await Task.sleep(for: .seconds(10))
          } onCancel: {}
          return value
        }
      }
      executor.runNextJob() // The one job: runs the task into the long sleep.
      task.cancel()
      executor.runNextJob() // The one job: the cancelled sleep resuming.
      expectEqual(1, await task.value)
      expectEqual(0, runs.value)
    }

    // MARK: - Task-allocated continuation storage.

    tests.test("the body may suspend and task-allocate before awaiting") {
      let value = await withContinuation(of: Int.self, throwing: Never.self) {
        (continuation: consuming Continuation<Int, Never>,
         waiter: consuming ContinuationWaiter<Int, Never>) in
        let holder = ContinuationHolder(continuation)

        await Task.yield()

        async let nested = one()
        _ = await nested

        await withTaskGroup(of: Int.self) { group in
          group.addTask { 1 }
          for await _ in group {}
        }

        let inner = await withContinuation(of: Int.self, throwing: Never.self) {
          (c: consuming Continuation<Int, Never>,
           a: consuming ContinuationWaiter<Int, Never>) in
          c.resume(returning: 6)
          return await a.wait()
        }

        Task.detached { holder.take().resume(returning: inner + 1) }
        return await waiter.wait()
      }
      expectEqual(7, value)
    }

    // MARK: - Interaction with the rest of the Continuation API.

    tests.test("the synchronous-body withContinuation still resolves") {
      // The split form is an overload; the existing form must be unaffected.
      let value = await withContinuation(of: Int.self, throwing: Never.self) {
        (continuation: consuming Continuation<Int, Never>) in
        continuation.resume(returning: 11)
      }
      expectEqual(11, value)
    }

    tests.test("a split continuation converts to an UnsafeContinuation") {
      // Same Continuation type, so existing conversions still work.
      let value = await withContinuation(of: Int.self, throwing: Never.self) {
        (continuation: consuming Continuation<Int, Never>,
         waiter: consuming ContinuationWaiter<Int, Never>) in
        let unsafeContinuation = UnsafeContinuation(continuation)
        unsafeContinuation.resume(returning: 13)
        return await waiter.wait()
      }
      expectEqual(13, value)
    }

    tests.test("a split continuation converts to a CheckedContinuation") {
      let value = await withContinuation(of: Int.self, throwing: Never.self) {
        (continuation: consuming Continuation<Int, Never>,
         waiter: consuming ContinuationWaiter<Int, Never>) in
        let checked = CheckedContinuation(continuation)
        checked.resume(returning: 14)
        return await waiter.wait()
      }
      expectEqual(14, value)
    }

    // MARK: - The body's own result and error type are independent.

    tests.test("the body can return a different type than Success") {
      let value = await withContinuation(of: Int.self, throwing: Never.self) {
        (continuation: consuming Continuation<Int, Never>,
         waiter: consuming ContinuationWaiter<Int, Never>) in
        continuation.resume(returning: 42)
        let n = await waiter.wait()
        return "value: \(n)"
      }
      expectEqual("value: 42", value)
    }

    tests.test("the body can throw a different type than Failure") {
      do {
        _ = try await withContinuation(of: Int.self, throwing: TestError.self) {
          (continuation: consuming Continuation<Int, TestError>,
           waiter: consuming ContinuationWaiter<Int, TestError>) in
          continuation.resume(throwing: TestError())
          do {
            return try await waiter.wait()
          } catch {
            throw DifferentError()
          }
        }
        expectUnreachable("the body should have thrown")
      } catch {
        expectTrue(error is DifferentError)
      }
    }

    // MARK: - Cancellation scopes.

    tests.test("cancellation while suspended inside a scope runs the handler") {
      // Cancellation walks past the scope's record to reach the handlers.
      let executor = ManualExecutor()
      let task = Task(executorPreference: executor) {
        await __withTaskCancellationScope { _ in
          await withContinuation(of: Int.self, throwing: Never.self) {
            (continuation: consuming Continuation<Int, Never>,
             waiter: consuming ContinuationWaiter<Int, Never>) in
            let holder = ContinuationHolder(continuation)
            return await waiter.wait(
              onCancel: { holder.resumeFromDetachedTask(returning: -10, executorPreference: executor) },
              onEscalate: { _ in })
          }
        }
      }
      executor.runNextJob()
      task.cancel()
      executor.runNextJobs(count: 2)
      expectEqual(-10, await task.value)
    }

    tests.test("installing inside an already-cancelled scope runs the handler") {
      let task = Task {
        await __withTaskCancellationScope { scope in
          scope.cancel()
          expectTrue(Task.isCancelled)
          return await withContinuation(of: Int.self, throwing: Never.self) {
            (continuation: consuming Continuation<Int, Never>,
             waiter: consuming ContinuationWaiter<Int, Never>) in
            let holder = ContinuationHolder(continuation)
            return await waiter.wait(
              onCancel: { holder.resumeFromDetachedTask(returning: -11, executorPreference: nil) },
              onEscalate: { _ in })
          }
        }
      }
      expectEqual(-11, await task.value)
      expectFalse(task.isCancelled)
    }

    tests.test("a cancellation shield suppresses the handler while suspended") {
      let executor = ManualExecutor()
      let task = Task(executorPreference: executor) {
        await withTaskCancellationShield {
          await withContinuation(of: Int.self, throwing: Never.self) {
            (continuation: consuming Continuation<Int, Never>,
             waiter: consuming ContinuationWaiter<Int, Never>) in
            let holder = ContinuationHolder(continuation)
            holder.resumeFromDetachedTask(returning: 42, executorPreference: executor)
            return await waiter.wait(
              onCancel: { expectUnreachable("shielded onCancel must not run") },
              onEscalate: { _ in })
          }
        }
      }
      executor.runNextJob() // Runs the task to its suspension.
      task.cancel()
      executor.runNextJobs(count: 2) // Runs the detached resumer, then the resume itself.
      expectEqual(42, await task.value)
    }

    // MARK: - Errors thrown out of the body.

    tests.test("an error thrown by the body propagates and cleans up") {
      // Storage and handler records must still be released when the body
      // throws.
      do {
        _ = try await withContinuation(of: Int.self, throwing: (any Error).self) {
          (continuation: consuming Continuation<Int, any Error>,
           waiter: consuming ContinuationWaiter<Int, any Error>) in
          continuation.resume(returning: 1)
          _ = try await waiter.wait(
            onCancel: {}, onEscalate: { _ in })
          throw TestError()
        }
        expectUnreachable("the body should have thrown")
      } catch {
        expectTrue(error is TestError)
      }

      // A second one must still work: shows the allocator was left
      // consistent.
      let value = await withContinuation(of: Int.self, throwing: Never.self) {
        (continuation: consuming Continuation<Int, Never>,
         waiter: consuming ContinuationWaiter<Int, Never>) in
        continuation.resume(returning: 2)
        return await waiter.wait()
      }
      expectEqual(2, value)
    }

    // MARK: - Result types other than the trivial ones.

    tests.test("an over-aligned result round-trips") {
      let value = await withContinuation(of: OverAligned.self, throwing: Never.self) {
        (continuation: consuming Continuation<OverAligned, Never>,
         waiter: consuming ContinuationWaiter<OverAligned, Never>) in
        continuation.resume(returning: OverAligned(a: 1, b: 2))
        return await waiter.wait()
      }
      expectEqual(1, value.a)
      expectEqual(2, value.b)
    }

    tests.test("an address-only result round-trips") {
      let value = await roundTripGenerically(Boxed(payload: "hello"))
      expectEqual("hello", value.payload)
    }

    tests.test("a large result round-trips") {
      let value = await withContinuation(of: Large.self, throwing: Never.self) {
        (continuation: consuming Continuation<Large, Never>,
         waiter: consuming ContinuationWaiter<Large, Never>) in
        continuation.resume(returning: Large(head: 7, tail: 8))
        return await waiter.wait()
      }
      expectEqual(7, value.head)
      expectEqual(8, value.tail)
    }

    // MARK: - Escalation before the handlers exist.

    tests.test("escalation before the await is not reported to the handler") {
      let escalations = Box(0)
      let executor = ManualExecutor()
      let task = Task(executorPreference: executor, priority: .background) { () -> Int in
        await withContinuation(of: Int.self, throwing: Never.self) {
          (continuation: consuming Continuation<Int, Never>,
           waiter: consuming ContinuationWaiter<Int, Never>) in
          continuation.resume(returning: 1)
          return await waiter.wait(
            onCancel: {},
            onEscalate: { _ in escalations.modify { $0 += 1 } })
        }
      }
      task.escalatePriority(to: .medium)
      executor.runNextJob()
      expectEqual(1, await task.value)
      expectEqual(0, escalations.value)
    }

    // MARK: - Handing the await half to another isolation domain.

    tests.test("the await half can be awaited from another isolation domain") {
      let executor = ManualExecutor()
      let other = Elsewhere()
      let task = Task(executorPreference: executor) {
        await withContinuation(of: Int.self, throwing: Never.self) {
          (continuation: consuming Continuation<Int, Never>,
           waiter: consuming ContinuationWaiter<Int, Never>) in
          let holder = ContinuationHolder(continuation)
          Task.detached(executorPreference: executor) {
            holder.take().resume(returning: 21)
          }
          return await other.awaitIt(waiter)
        }
      }
      executor.runNextJobs(count: 4)
      expectEqual(21, await task.value)
    }

    await runAllTestsAsync()
  }
}
