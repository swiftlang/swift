// RUN: %target-run-simple-swift
// REQUIRES: executable_test

// A test that we can properly differentiate types that require refcounting.

import StdlibUnittest
import DifferentiationUnittest

var LeakCheckingTests = TestSuite("LeakChecking")

struct ExampleLeakModel : Differentiable {
  var bias: Tracked<Float> = 2.0
  func applied(to input: Tracked<Float>) -> Tracked<Float> {
    var v = input + bias
    return v
  }
}

LeakCheckingTests.test("BasicVarLeakChecking") {
  do {
    var model = ExampleLeakModel()
    let x: Tracked<Float> = 1.0
    let _ = model.gradient(at: x) { m, x in m.applied(to: x) }
  }
  expectEqual(0, _GlobalLeakCount.count, "Leak Detected.")
}

runAllTests()
