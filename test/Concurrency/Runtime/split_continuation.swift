// RUN: %target-run-simple-swift( -target %target-swift-5.1-abi-triple -parse-as-library -disable-availability-checking -swift-version 6) 2>&1 | %FileCheck %s
// RUN: %target-run-simple-swift( -target %target-swift-5.1-abi-triple -parse-as-library -disable-availability-checking -swift-version 6 -O) 2>&1 | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime

@_spi(Concurrency) import _Concurrency
import StdlibUnittest

struct TestError: Error {}

struct UniqueResource: ~Copyable {
  let value: Int
  init(_ value: Int) { self.value = value }
  deinit { print("UniqueResource(\(value)).deinit") }
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

// Handlers run holding the status-record lock, so they can't resume inline.
extension ContinuationHolder where Success: Sendable {
  func resumeFromDetachedTask(returning value: Success) {
    Task.detached { [self] in
      self.take().resume(returning: value)
    }
  }
}

final class Box<Value>: @unchecked Sendable {
  var value: Value
  init(_ value: Value) { self.value = value }
}

func one() async -> Int { 1 }

// Sendable value handlers can capture; handlers may run on any thread.
final class Shared: Sendable {
  let id: Int
  init(id: Int) { self.id = id }
  func cancel() {}
  func escalate(to priority: TaskPriority) {}
}

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
     awaiter: consuming ContinuationAwaiter<T, Never>) in
    continuation.resume(returning: value)
    return await awaiter.wait()
  }
}

// A separate isolation domain.
actor Elsewhere {
  func awaitIt(_ awaiter: consuming ContinuationAwaiter<Int, Never>) async -> Int {
    await awaiter.wait()
  }
}

@main struct Main {
  static func main() async {
    let tests = TestSuite("Continuation: split continuation")

    // MARK: - Resume before await.

    tests.test("resume before await, returning") {
      let value = await withContinuation(of: Int.self, throwing: Never.self) {
        (continuation: consuming Continuation<Int, Never>,
         awaiter: consuming ContinuationAwaiter<Int, Never>) in
        continuation.resume(returning: 42)
        return await awaiter.wait()
      }
      expectEqual(42, value)
    }

    tests.test("resume before await, Void") {
      await withContinuation(of: Void.self, throwing: Never.self) {
        (continuation: consuming Continuation<Void, Never>,
         awaiter: consuming ContinuationAwaiter<Void, Never>) in
        continuation.resume(returning: ())
        return await awaiter.wait()
      }
    }

    tests.test("resume before await, throwing continuation returning") {
      do {
        let value = try await withContinuation(of: Int.self, throwing: (any Error).self) {
          (continuation: consuming Continuation<Int, any Error>,
           awaiter: consuming ContinuationAwaiter<Int, any Error>) in
          continuation.resume(returning: 17)
          return try await awaiter.wait()
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
           awaiter: consuming ContinuationAwaiter<Int, any Error>) in
          continuation.resume(throwing: TestError())
          return try await awaiter.wait()
        }
        expectUnreachable("wait() should have thrown")
      } catch {
        expectTrue(error is TestError)
      }
    }

    tests.test("resume before await, with Result") {
      let value = await withContinuation(of: Int.self, throwing: Never.self) {
        (continuation: consuming Continuation<Int, Never>,
         awaiter: consuming ContinuationAwaiter<Int, Never>) in
        continuation.resume(with: .success(7))
        return await awaiter.wait()
      }
      expectEqual(7, value)
    }

    tests.test("resume before await, ~Copyable Success round-trips") {
      let resource = await withContinuation(of: UniqueResource.self, throwing: Never.self) {
        (continuation: consuming Continuation<UniqueResource, Never>,
         awaiter: consuming ContinuationAwaiter<UniqueResource, Never>) in
        continuation.resume(returning: UniqueResource(99))
        return await awaiter.wait()
      }
      expectEqual(99, resource.value)
      _ = consume resource
      // CHECK: UniqueResource(99).deinit
    }

    // MARK: - Resume after await.

    tests.test("resume after await, from another task") {
      let value = await withContinuation(of: Int.self, throwing: Never.self) {
        (continuation: consuming Continuation<Int, Never>,
         awaiter: consuming ContinuationAwaiter<Int, Never>) in
        let holder = ContinuationHolder(continuation)
        Task.detached {
          // Delay so the awaiter suspends before the resume.
          try? await Task.sleep(for: .milliseconds(100))
          holder.take().resume(returning: 314)
        }
        return await awaiter.wait()
      }
      expectEqual(314, value)
    }

    tests.test("resume after await, throwing from another task") {
      do {
        _ = try await withContinuation(of: Int.self, throwing: (any Error).self) {
          (continuation: consuming Continuation<Int, any Error>,
           awaiter: consuming ContinuationAwaiter<Int, any Error>) in
          let holder = ContinuationHolder(continuation)
          Task.detached {
            try? await Task.sleep(for: .milliseconds(100))
            holder.take().resume(throwing: TestError())
          }
          return try await awaiter.wait()
        }
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
         awaiter: consuming ContinuationAwaiter<Int, Never>) in
        continuation.resume(returning: 5)
        return await awaiter.wait(
          onCancel: { cancelled.value = true },
          onEscalate: { _ in escalated.value = true })
      }
      expectEqual(5, value)
      expectFalse(cancelled.value)
      expectFalse(escalated.value)
    }

    tests.test("cancellation handler runs while suspended and can resume") {
      // Only possible because the handler is installed on the await.
      let task = Task {
        await withContinuation(of: Int.self, throwing: Never.self) {
          (continuation: consuming Continuation<Int, Never>,
           awaiter: consuming ContinuationAwaiter<Int, Never>) in
          let holder = ContinuationHolder(continuation)
          return await awaiter.wait(
            onCancel: { holder.resumeFromDetachedTask(returning: -1) },
            onEscalate: { _ in })
        }
      }
      // Let the task reach the suspension before cancelling it.
      try? await Task.sleep(for: .milliseconds(100))
      task.cancel()
      expectEqual(-1, await task.value)
    }

    tests.test("cancellation handler runs when the task is already cancelled") {
      let task = Task {
        // Make sure the task is cancelled before it ever reaches the await.
        try? await Task.sleep(for: .milliseconds(100))
        expectTrue(Task.isCancelled)
        return await withContinuation(of: Int.self, throwing: Never.self) {
          (continuation: consuming Continuation<Int, Never>,
           awaiter: consuming ContinuationAwaiter<Int, Never>) in
          let holder = ContinuationHolder(continuation)
          return await awaiter.wait(
            onCancel: { holder.resumeFromDetachedTask(returning: -2) },
            onEscalate: { _ in })
        }
      }
      task.cancel()
      expectEqual(-2, await task.value)
    }

    tests.test("cancellation handler runs at most once") {
      let runs = Box(0)
      let task = Task {
        await withContinuation(of: Int.self, throwing: Never.self) {
          (continuation: consuming Continuation<Int, Never>,
           awaiter: consuming ContinuationAwaiter<Int, Never>) in
          let holder = ContinuationHolder(continuation)
          return await awaiter.wait(
            onCancel: {
              runs.value += 1
              holder.resumeFromDetachedTask(returning: -3)
            },
            onEscalate: { _ in })
        }
      }
      try? await Task.sleep(for: .milliseconds(100))
      task.cancel()
      task.cancel()
      expectEqual(-3, await task.value)
      expectEqual(1, runs.value)
    }

    tests.test("the awaiting task observes its own cancellation") {
      // The handler runs on the cancelling thread; Task.isCancelled is
      // observed by the resumed task.
      let task = Task { () -> (Int, Bool) in
        let value = await withContinuation(of: Int.self, throwing: Never.self) {
          (continuation: consuming Continuation<Int, Never>,
           awaiter: consuming ContinuationAwaiter<Int, Never>) in
          let holder = ContinuationHolder(continuation)
          return await awaiter.wait(
            onCancel: { holder.resumeFromDetachedTask(returning: -4) },
            onEscalate: { _ in })
        }
        return (value, Task.isCancelled)
      }
      try? await Task.sleep(for: .milliseconds(100))
      task.cancel()
      let (value, isCancelled) = await task.value
      expectEqual(-4, value)
      expectTrue(isCancelled)
    }

    tests.test("handlers are uninstalled once the await resolves") {
      // Cancelling after wait() returns must not run the handler: the
      // records were popped.
      let runs = Box(0)
      let task = Task {
        await withContinuation(of: Int.self, throwing: Never.self) {
          (continuation: consuming Continuation<Int, Never>,
           awaiter: consuming ContinuationAwaiter<Int, Never>) in
          continuation.resume(returning: 1)
          let value = await awaiter.wait(
            onCancel: { runs.value += 1 },
            onEscalate: { _ in })
          await withTaskCancellationHandler {
            try? await Task.sleep(for: .seconds(10))
          } onCancel: {}
          return value
        }
      }
      try? await Task.sleep(for: .milliseconds(100))
      task.cancel()
      expectEqual(1, await task.value)
      expectEqual(0, runs.value)
    }

    // MARK: - Task-allocated continuation storage.

    tests.test("the body may suspend and task-allocate before awaiting") {
      // Storage is task-allocated; create/destroy must bracket everything
      // the body allocates in between.
      let value = await withContinuation(of: Int.self, throwing: Never.self) {
        (continuation: consuming Continuation<Int, Never>,
         awaiter: consuming ContinuationAwaiter<Int, Never>) in
        let holder = ContinuationHolder(continuation)

        try? await Task.sleep(for: .milliseconds(10))

        async let nested = one()
        _ = await nested

        await withTaskGroup(of: Int.self) { group in
          group.addTask { 1 }
          for await _ in group {}
        }

        let inner = await withContinuation(of: Int.self, throwing: Never.self) {
          (c: consuming Continuation<Int, Never>,
           a: consuming ContinuationAwaiter<Int, Never>) in
          c.resume(returning: 6)
          return await a.wait()
        }

        Task.detached { holder.take().resume(returning: inner + 1) }
        return await awaiter.wait()
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
         awaiter: consuming ContinuationAwaiter<Int, Never>) in
        let unsafeContinuation = UnsafeContinuation(continuation)
        unsafeContinuation.resume(returning: 13)
        return await awaiter.wait()
      }
      expectEqual(13, value)
    }

    tests.test("a split continuation converts to a CheckedContinuation") {
      let value = await withContinuation(of: Int.self, throwing: Never.self) {
        (continuation: consuming Continuation<Int, Never>,
         awaiter: consuming ContinuationAwaiter<Int, Never>) in
        let checked = CheckedContinuation(continuation)
        checked.resume(returning: 14)
        return await awaiter.wait()
      }
      expectEqual(14, value)
    }

    tests.test("an ordinary Continuation still converts") {
      let value = await withContinuation(of: Int.self, throwing: Never.self) {
        (continuation: consuming Continuation<Int, Never>) in
        let unsafeContinuation = UnsafeContinuation(continuation)
        unsafeContinuation.resume(returning: 15)
      }
      expectEqual(15, value)
    }

    tests.test("handlers may capture a shared value") {
      // sending is more permissive than @Sendable: both handlers may still
      // share one Sendable value.
      let value = await withContinuation(of: Int.self, throwing: Never.self) {
        (continuation: consuming Continuation<Int, Never>,
         awaiter: consuming ContinuationAwaiter<Int, Never>) in
        let shared = Shared(id: 4)
        continuation.resume(returning: shared.id)
        return await awaiter.wait(
          onCancel: { shared.cancel() },
          onEscalate: { shared.escalate(to: $0) })
      }
      expectEqual(4, value)
    }

    // MARK: - Cancellation scopes.

    tests.test("cancellation while suspended inside a scope runs the handler") {
      // Cancellation walks past the scope's record to reach the handlers.
      let task = Task {
        await __withTaskCancellationScope { _ in
          await withContinuation(of: Int.self, throwing: Never.self) {
            (continuation: consuming Continuation<Int, Never>,
             awaiter: consuming ContinuationAwaiter<Int, Never>) in
            let holder = ContinuationHolder(continuation)
            return await awaiter.wait(
              onCancel: { holder.resumeFromDetachedTask(returning: -10) },
              onEscalate: { _ in })
          }
        }
      }
      try? await Task.sleep(for: .milliseconds(100))
      task.cancel()
      expectEqual(-10, await task.value)
    }

    tests.test("installing inside an already-cancelled scope runs the handler") {
      let task = Task {
        await __withTaskCancellationScope { scope in
          scope.cancel()
          expectTrue(Task.isCancelled)
          return await withContinuation(of: Int.self, throwing: Never.self) {
            (continuation: consuming Continuation<Int, Never>,
             awaiter: consuming ContinuationAwaiter<Int, Never>) in
            let holder = ContinuationHolder(continuation)
            return await awaiter.wait(
              onCancel: { holder.resumeFromDetachedTask(returning: -11) },
              onEscalate: { _ in })
          }
        }
      }
      expectEqual(-11, await task.value)
      expectFalse(task.isCancelled)
    }

    // MARK: - Errors thrown out of the body.

    tests.test("an error thrown by the body propagates and cleans up") {
      // Storage and handler records must still be released when the body
      // throws.
      do {
        _ = try await withContinuation(of: Int.self, throwing: (any Error).self) {
          (continuation: consuming Continuation<Int, any Error>,
           awaiter: consuming ContinuationAwaiter<Int, any Error>) in
          continuation.resume(returning: 1)
          _ = try await awaiter.wait(
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
         awaiter: consuming ContinuationAwaiter<Int, Never>) in
        continuation.resume(returning: 2)
        return await awaiter.wait()
      }
      expectEqual(2, value)
    }

    // MARK: - Result types other than the trivial ones.

    tests.test("an over-aligned result round-trips") {
      let value = await withContinuation(of: OverAligned.self, throwing: Never.self) {
        (continuation: consuming Continuation<OverAligned, Never>,
         awaiter: consuming ContinuationAwaiter<OverAligned, Never>) in
        continuation.resume(returning: OverAligned(a: 1, b: 2))
        return await awaiter.wait()
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
         awaiter: consuming ContinuationAwaiter<Large, Never>) in
        continuation.resume(returning: Large(head: 7, tail: 8))
        return await awaiter.wait()
      }
      expectEqual(7, value.head)
      expectEqual(8, value.tail)
    }

    // MARK: - Escalation before the handlers exist.

    tests.test("escalation before the await is not reported to the handler") {
      // Handlers are published at suspension; an escalation that already
      // happened isn't replayed into them.
      let escalations = Box(0)
      let task = Task(priority: .background) { () -> Int in
        // Let the escalation land before the continuation is even created.
        try? await Task.sleep(for: .milliseconds(100))
        return await withContinuation(of: Int.self, throwing: Never.self) {
          (continuation: consuming Continuation<Int, Never>,
           awaiter: consuming ContinuationAwaiter<Int, Never>) in
          continuation.resume(returning: 1)
          return await awaiter.wait(
            onCancel: {},
            onEscalate: { _ in escalations.value += 1 })
        }
      }
      task.escalatePriority(to: .medium)
      expectEqual(1, await task.value)
      expectEqual(0, escalations.value)
    }

    // MARK: - Handing the await half to another isolation domain.

    tests.test("the await half can be awaited from another isolation domain") {
      // ContinuationAwaiter is Sendable: non-copyable plus consuming wait()
      // means only one exists and it's awaited at most once.
      let other = Elsewhere()
      let value = await withContinuation(of: Int.self, throwing: Never.self) {
        (continuation: consuming Continuation<Int, Never>,
         awaiter: consuming ContinuationAwaiter<Int, Never>) in
        let holder = ContinuationHolder(continuation)
        Task.detached {
          try? await Task.sleep(for: .milliseconds(100))
          holder.take().resume(returning: 21)
        }
        return await other.awaitIt(awaiter)
      }
      expectEqual(21, value)
    }

    // MARK: - Nesting.

    tests.test("split continuations nest") {
      let value = await withContinuation(of: Int.self, throwing: Never.self) {
        (outerContinuation: consuming Continuation<Int, Never>,
         outerAwaiter: consuming ContinuationAwaiter<Int, Never>) in
        let inner = await withContinuation(of: Int.self, throwing: Never.self) {
          (continuation: consuming Continuation<Int, Never>,
           awaiter: consuming ContinuationAwaiter<Int, Never>) in
          continuation.resume(returning: 20)
          return await awaiter.wait()
        }
        outerContinuation.resume(returning: inner + 2)
        return await outerAwaiter.wait()
      }
      expectEqual(22, value)
    }

    await runAllTestsAsync()
  }
}
