// RUN: %target-run-simple-parse-stdlib-swift
// REQUIRES: executable_test

import Swift
import StdlibUnittest

var BuiltinDifferentialOperatorTests = TestSuite("BuiltinDifferentialOperators")

BuiltinDifferentialOperatorTests.test("Trivial") {
  let t = 1.0
  let (value: y, pullback: pb) = valueWithPullback(at: 4.0) { x in
    x * x * t
  }
  expectEqual(16, y)
  expectEqual(8, pb(1))

  let pb2 = pullback(at: 4.0) { x in
    x * x * t
  }
  expectEqual(8, pb2(1))

  let grad = gradient(at: 4.0) { x in
    x * x * t
  }
  expectEqual(8, grad)
}

runAllTests()

