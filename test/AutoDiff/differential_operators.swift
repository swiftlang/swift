// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import Swift
import StdlibUnittest

var BuiltinDifferentialOperatorTests = TestSuite("DifferentialOperators")

BuiltinDifferentialOperatorTests.test("Simple") {
  let t: Float = 1.0
  do {
    let (value: y, pullback: pb) = valueWithPullback(at: 4.0, 5.0) { x0, x1 in
      x0 * x1 * t
    }
    expectEqual(20, y)
    expectEqual((5, 4), pb(1))
    expectEqual((0, 0), pb(0))
  }

  do {
    let pb = pullback(at: 4.0, 5.0) { x0, x1 in
      x0 * x1 * t
    }
    expectEqual((5, 4), pb(1))
    expectEqual((0, 0), pb(0))
  }

  do {
    let (value: y, gradient: grad) = valueWithGradient(at: 4.0, 5.0) { x0, x1 in
      x0 * x1 * t
    }
    expectEqual(20, y)
    expectEqual((5, 4), grad)
  }

  do {
    let grad = gradient(at: 4.0, 5.0) { x0, x1 in
      x0 * x1 * t
    }
    expectEqual((5, 4), grad)
  }

  do {
    let grad = gradient { x0, x1 in
      x0 * x1 * t
    }
    expectEqual((5, 4), grad(4, 5))
  }
}

runAllTests()
