// RUN: %target-run-simple-swift-control-flow-differentiation
// REQUIRES: executable_test

// A test that we can properly differentiate types that require refcounting.

import StdlibUnittest
import DifferentiationUnittest

var LeakCheckingTests = TestSuite("LeakChecking")

/// Execute body, check expect leak count, and reset global leak count.
func testWithLeakChecking(expectedLeakCount: Int = 0, _ body: () -> Void) {
  body()
  expectEqual(expectedLeakCount, _GlobalLeakCount.count, "Leak detected.")
  _GlobalLeakCount.count = 0
}

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
  expectEqual(0, _GlobalLeakCount.count, "Leak detected.")
}

LeakCheckingTests.test("ControlFlow") {
  testWithLeakChecking(expectedLeakCount: 0) {
    func cond_tuple_var(_ x: Float) -> Float {
      var y: (Float, Float) = (x, x)
      var z: (Float, Float) = (x + x, x - x)
      if x > 0 {
        y.0 = x
        y.1 = x
        z.0 = z.0 - y.0
        z.1 = z.1 + y.0
      } else {
        z = (x, x)
      }
      return y.0 + y.1 - z.0 + z.1
    }
    _ = gradient(at: 4, in: cond_tuple_var)
  }

  // FIXME: Fix control flow AD memory leaks.
  // See related FIXME comments in adjoint value/buffer propagation in
  // lib/SILOptimizer/Mandatory/Differentiation.cpp.
  testWithLeakChecking(expectedLeakCount: 9) {
    var model = ExampleLeakModel()
    let x: Tracked<Float> = 1.0
    let _ = model.gradient(at: x) { m, x in
      let result: Tracked<Float>
      if x > 0 {
        result = m.applied(to: x)
      } else {
        result = x
      }
      return result
    }
  }

  // FIXME: Fix control flow AD memory leaks.
  // See related FIXME comments in adjoint value/buffer propagation in
  // lib/SILOptimizer/Mandatory/Differentiation.cpp.
  testWithLeakChecking(expectedLeakCount: 14) {
    var model = ExampleLeakModel()
    let x: Tracked<Float> = 1.0
    let _ = model.gradient(at: x) { m, x in
      var result: Tracked<Float> = x
      if x > 0 {
        result = result + m.applied(to: x)
      }
      return result
    }
  }
}

runAllTests()
