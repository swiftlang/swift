// RUN: %target-run-simple-swift( -target %target-swift-5.1-abi-triple -parse-as-library -disable-availability-checking -swift-version 6)
// RUN: %target-run-simple-swift( -target %target-swift-5.1-abi-triple -parse-as-library -disable-availability-checking -swift-version 6 -O)

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime
// REQUIRES: libdispatch
// UNSUPPORTED: back_deployment_runtime

@_spi(Concurrency) @_spi(ExperimentalScheduling) @_spi(ExperimentalCustomExecutors) import _Concurrency
import StdlibUnittest

@main struct Main {
  static func main() async {
    let tests = TestSuite("Continuation: split continuation misuse")

    tests.test("dropping the continuation without resuming traps") {
      expectCrashLater()
      _ = await withContinuation(of: Int.self, throwing: Never.self) {
        (continuation: consuming Continuation<Int, Never>,
         waiter: consuming ContinuationWaiter<Int, Never>) in
        _ = consume continuation
        return await waiter.wait()
      }
    }

    tests.test("dropping the waiter without awaiting traps") {
      expectCrashLater()
      _ = await withContinuation(of: Int.self, throwing: Never.self) {
        (continuation: consuming Continuation<Int, Never>,
         waiter: consuming ContinuationWaiter<Int, Never>) in
        continuation.resume(returning: 1)
        _ = consume waiter
        return 1
      }
    }

    tests.test("waiting for another continuation's timer traps") {
      expectCrashLater()
      let scheduler = Task.defaultExecutor as! any SchedulingExecutor
      try? await withContinuation(
        of: Void.self, throwing: CancellationError.self
      ) { (outer: consuming Continuation<Void, CancellationError>,
           outerWaiter: consuming ContinuationWaiter<Void, CancellationError>)
        async throws(CancellationError) -> Void in
        var scheduled = Optional(
          scheduler.schedule(outer, at: .after(.milliseconds(1)),
                             tolerance: nil, clock: ContinuousClock()))
        try await withContinuation(
          of: Void.self, throwing: CancellationError.self
        ) { (inner: consuming Continuation<Void, CancellationError>,
             innerWaiter: consuming ContinuationWaiter<Void, CancellationError>)
          async throws(CancellationError) -> Void in
          inner.resume()
          try await innerWaiter.wait(for: scheduled.take()!)
        }
        try await outerWaiter.wait()
      }
    }

    await runAllTestsAsync()
  }
}
