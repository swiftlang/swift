// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import DifferentiationUnittest
import StdlibUnittest

var DerivativeCustomizationTests = TestSuite("DerivativeCustomization")

DerivativeCustomizationTests.testWithLeakChecking("withDerivative") {
  do {
    var counter = 0
    func callback(_ x: inout Tracked<Float>) { counter += 1 }
    _ = gradient(at: 4) { (x: Tracked<Float>) -> Tracked<Float> in
      // Non-active value should not be differentiated, so `callback` should
      // not be called.
      _ = x.withDerivative(callback)
      return x.withDerivative(callback) + x.withDerivative(callback)
    }
    expectEqual(2, counter)
  }

  expectEqual(
    30,
    gradient(at: 4) { (x: Tracked<Float>) in
      x.withDerivative { $0 = 10 } + x.withDerivative { $0 = 20 }
    })
}

DerivativeCustomizationTests.testWithLeakChecking("withoutDerivative") {
  expectEqual(
    0,
    gradient(at: Tracked<Float>(4)) { x -> Tracked<Float> in
      withoutDerivative(at: x) { x in
        x * x * x
      }
    })

  expectEqual(
    0,
    gradient(at: Tracked<Float>(4)) { x -> Tracked<Float> in
      let y = withoutDerivative(at: x)
      return y * y * y
    })

  expectEqual(
    2,
    gradient(at: Tracked<Float>(4)) { x -> Tracked<Float> in
      let y = withoutDerivative(at: x)
      return x + y * y * y + x
    })
}

runAllTests()
