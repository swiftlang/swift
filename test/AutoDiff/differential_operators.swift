// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import Swift
import StdlibUnittest

var BuiltinDifferentialOperatorTests = TestSuite("BuiltinDifferentialOperators")

BuiltinDifferentialOperatorTests.test("Trivial") {
  let t = 1.0
  do {
    let (value: y, pullback: pb) = valueWithPullback(at: 4.0) { x in
      x * x * t
    }
    expectEqual(16, y)
    expectEqual(8, pb(1))
    expectEqual(0, pb(0))
  }

  do {
    let pb = pullback(at: 4.0) { x in
      x * x * t
    }
    expectEqual(8, pb(1))
    expectEqual(0, pb(0))
  }

  do {
    let (value: y, gradient: grad) = valueWithGradient(at: 4.0) { x in
      x * x * t
    }
    expectEqual(16, y)
    expectEqual(8, grad)
  }

  do {
    let grad = gradient(at: 4.0) { x in
      x * x * t
    }
    expectEqual(8, grad)
  }
}

runAllTests()

