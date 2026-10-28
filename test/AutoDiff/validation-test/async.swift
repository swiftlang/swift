// RUN: %target-run-simple-swift(-Xfrontend -disable-availability-checking)
// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime

import _Concurrency
import StdlibUnittest
import DifferentiationUnittest

var AsyncTests = TestSuite("Async")

@_semantics("autodiff.opaque")
func iidentity(_ x: Float) async -> Float { x }

@derivative(of: iidentity)
func vjpIdentity(_ x: Float) async -> (value: Float, pullback: (Float) -> Float) {
    (x, { $0 })
}

AsyncTests.test("CustomVJP") {
  let (value, pullback) = await valueWithPullback(at: Float(42.0), of: iidentity)

  expectEqual(42.0, value)
  expectEqual(2.0, pullback(2.0))
}


await runAllTestsAsync()
