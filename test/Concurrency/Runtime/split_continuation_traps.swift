// RUN: %target-run-simple-swift( -target %target-swift-5.1-abi-triple -parse-as-library -disable-availability-checking -swift-version 6)
// RUN: %target-run-simple-swift( -target %target-swift-5.1-abi-triple -parse-as-library -disable-availability-checking -swift-version 6 -O)

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime
// REQUIRES: libdispatch
// UNSUPPORTED: back_deployment_runtime

@_spi(Concurrency) import _Concurrency
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

    await runAllTestsAsync()
  }
}
