// RUN: %target-run-simple-swift( -target %target-swift-5.1-abi-triple -parse-as-library -disable-availability-checking -swift-version 6 -enable-experimental-feature SplitContinuations)
// RUN: %target-run-simple-swift( -target %target-swift-5.1-abi-triple -parse-as-library -disable-availability-checking -swift-version 6 -O -enable-experimental-feature SplitContinuations)

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime
// REQUIRES: libdispatch
// REQUIRES: swift_feature_SplitContinuations
// UNSUPPORTED: back_deployment_runtime

// Misuses a split continuation traps on: dropping either half unused.
//
// Naming a wrong executor is not a misuse; see split_continuation_executor.swift.

@_spi(Concurrency) import _Concurrency
import StdlibUnittest

@main struct Main {
  static func main() async {
    let tests = TestSuite("Continuation: split continuation misuse")

    tests.test("dropping the continuation without resuming traps") {
      expectCrashLater()
      _ = await withContinuation(of: Int.self, throwing: Never.self) {
        (continuation: consuming Continuation<Int, Never>,
         awaiter: consuming ContinuationAwaiter<Int, Never>) in
        _ = consume continuation
        return await awaiter.wait()
      }
    }

    tests.test("dropping the awaiter without awaiting traps") {
      expectCrashLater()
      _ = await withContinuation(of: Int.self, throwing: Never.self) {
        (continuation: consuming Continuation<Int, Never>,
         awaiter: consuming ContinuationAwaiter<Int, Never>) in
        continuation.resume(returning: 1)
        _ = consume awaiter
        return 1
      }
    }

    await runAllTestsAsync()
  }
}
