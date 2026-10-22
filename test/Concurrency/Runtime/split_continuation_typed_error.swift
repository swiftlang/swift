// RUN: %target-run-simple-swift( -target %target-swift-5.1-abi-triple -parse-as-library -disable-availability-checking -swift-version 6)
// RUN: %target-run-simple-swift( -target %target-swift-5.1-abi-triple -parse-as-library -disable-availability-checking -swift-version 6 -O)

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: back_deploy_concurrency
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: freestanding

// The storage of a split continuation comes from the task allocator, which is a
// stack; `withContinuation` has to destroy it after everything that was
// allocated after it, including the (typed) error and result buffers of its
// body.  With a non-`Never` failure type whose size isn't known statically
// (`CancellationError` is a resilient type) those buffers used to be freed after
// the continuation, which aborted with "freed pointer was not the last
// allocation" in optimized builds.

@_spi(Concurrency) import _Concurrency
import StdlibUnittest

func succeed() async throws(CancellationError) {
  try await withContinuation(of: Void.self, throwing: CancellationError.self) {
    (continuation: consuming Continuation<Void, CancellationError>,
     waiter: consuming ContinuationWaiter<Void, CancellationError>)
      async throws(CancellationError) -> Void in
    continuation.resume()
    try await waiter.wait()
  }
}

func fail() async throws(CancellationError) {
  try await withContinuation(of: Void.self, throwing: CancellationError.self) {
    (continuation: consuming Continuation<Void, CancellationError>,
     waiter: consuming ContinuationWaiter<Void, CancellationError>)
      async throws(CancellationError) -> Void in
    continuation.resume(throwing: CancellationError())
    try await waiter.wait()
  }
}

func result() async throws(CancellationError) -> Int {
  try await withContinuation(of: Int.self, throwing: CancellationError.self) {
    (continuation: consuming Continuation<Int, CancellationError>,
     waiter: consuming ContinuationWaiter<Int, CancellationError>)
      async throws(CancellationError) -> Int in
    continuation.resume(returning: 7)
    return try await waiter.wait() + 1
  }
}

@main struct Main {
  static func main() async {
    let tests = TestSuite("split_continuation_typed_error")

    tests.test("typed failure type, resumed normally") {
      do throws(CancellationError) {
        try await succeed()
      } catch {
        expectUnreachable("unexpected error \(error)")
      }
    }

    tests.test("typed failure type, resumed with an error") {
      do throws(CancellationError) {
        try await fail()
        expectUnreachable("expected an error")
      } catch {
        _ = error  // CancellationError
      }
    }

    tests.test("typed failure type, with a result") {
      do throws(CancellationError) {
        expectEqual(8, try await result())
      } catch {
        expectUnreachable("unexpected error \(error)")
      }
    }

    await runAllTestsAsync()
  }
}
