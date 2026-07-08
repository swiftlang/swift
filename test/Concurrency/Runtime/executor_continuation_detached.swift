// RUN: %target-run-simple-swift( -target %target-swift-5.1-abi-triple -parse-as-library -disable-availability-checking -enable-experimental-feature Lifetimes) 2>&1 | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime
// REQUIRES: swift_feature_Lifetimes
// UNSUPPORTED: back_deployment_runtime

@_spi(ExperimentalScheduling) import _Concurrency
import StdlibUnittest

struct TestError: Error {}

struct UniqueResource: ~Copyable {
  let value: Int
  init(_ value: Int) { self.value = value }
  deinit { print("UniqueResource(\(value)).deinit") }
}

// A Sendable box that lets us move the `~Copyable` resume half into an escaping
// closure (a detached task) by `take()`-ing it back out on the other side.
final class ContinuationBox<Success: ~Copyable, Failure: Error>: @unchecked Sendable {
  var storage: ExecutorContinuation<Success, Failure>?
  init(_ c: consuming ExecutorContinuation<Success, Failure>) {
    self.storage = consume c
  }
}

@main struct Main {
  static func main() async {
    let tests = TestSuite("ExecutorContinuation: detached")

    // MARK: - Sync-complete (resume-before-await) path.
    // The producer is resumed synchronously before the awaiter suspends, so
    // awaitDetached observes `Resumed` and continues inline.

    tests.test("sync-complete resume returning") {
      let value = await withExecutorContinuation(of: Int.self, throwing: Never.self) {
        (producer: consuming ExecutorContinuation<Int, Never>,
         awaiter: consuming ExecutorContinuationAwaiter<Int, Never>) in
        producer.resumeSynchronously(returning: 42)
        return await awaiter.wait()
      }
      expectEqual(42, value)
    }

    tests.test("sync-complete resume void") {
      await withExecutorContinuation(of: Void.self, throwing: Never.self) {
        (producer: consuming ExecutorContinuation<Void, Never>,
         awaiter: consuming ExecutorContinuationAwaiter<Void, Never>) in
        producer.resumeSynchronously(returning: ())
        return await awaiter.wait()
      }
    }

    tests.test("sync-complete throwing resume returning") {
      do {
        let value = try await withExecutorContinuation(of: Int.self, throwing: (any Error).self) {
          (producer: consuming ExecutorContinuation<Int, any Error>,
           awaiter: consuming ExecutorContinuationAwaiter<Int, any Error>) in
          producer.resumeSynchronously(returning: 17)
          return try await awaiter.wait()
        }
        expectEqual(17, value)
      } catch {
        expectUnreachable()
      }
    }

    tests.test("sync-complete throwing resume throwing") {
      do {
        let _ = try await withExecutorContinuation(of: Int.self, throwing: (any Error).self) {
          (producer: consuming ExecutorContinuation<Int, any Error>,
           awaiter: consuming ExecutorContinuationAwaiter<Int, any Error>) in
          producer.resumeSynchronously(throwing: TestError())
          return try await awaiter.wait()
        }
        expectUnreachable()
      } catch {
        // Expected.
      }
    }

    tests.test("sync-complete ~Copyable Success round-trips") {
      let resource = await withExecutorContinuation(of: UniqueResource.self, throwing: Never.self) {
        (producer: consuming ExecutorContinuation<UniqueResource, Never>,
         awaiter: consuming ExecutorContinuationAwaiter<UniqueResource, Never>) in
        producer.resumeSynchronously(returning: UniqueResource(99))
        return await awaiter.wait()
      }
      expectEqual(99, resource.value)
      _ = consume resource
      // CHECK: UniqueResource(99).deinit
    }

    // NOTE: The resume-after-await (Pending -> Awaited, then resume enqueues the
    // suspended task) path requires the `~Copyable` resume half to cross to
    // another thread. It cannot be captured directly in an escaping closure, but
    // it can be moved through a Sendable box and `take()`-ed back out.

    tests.test("cross-task resume after await") {
      let value = await withExecutorContinuation(of: Int.self, throwing: Never.self) {
        (producer: consuming ExecutorContinuation<Int, Never>,
         awaiter: consuming ExecutorContinuationAwaiter<Int, Never>) in
        let box = ContinuationBox(producer)
        Task.detached {
          // Delay so the awaiter suspends (Pending -> Awaited) before the
          // resume, exercising the enqueue-the-suspended-task path.
          try? await Task.sleep(for: .milliseconds(50))
          guard let producer = box.storage.take() else { return }
          producer.resumeSynchronously(returning: 314)
        }
        return await awaiter.wait()
      }
      expectEqual(314, value)
    }

    await runAllTestsAsync()
  }
}
