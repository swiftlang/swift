// RUN: %target-run-simple-parse-stdlib-swift
// REQUIRES: executable_test

import Swift
import StdlibUnittest

var BuiltinDifferentialOperatorTests = TestSuite("BuiltinDifferentialOperators")

BuiltinDifferentialOperatorTests.test("Unary") {
  func func_to_diff(x: Float) -> Float {
    return x * x
  }
  let (y, pullback) = Builtin.autodiffApply_vjp(func_to_diff, 4)
  expectEqual(16, y)
  expectEqual(8, pullback(1))
}

BuiltinDifferentialOperatorTests.test("Binary") {
  func func_to_diff(x: Float, y: Float) -> Float {
    return x * y
  }
  let (y, pullback) = Builtin.autodiffApply_vjp_arity2(func_to_diff, 4, 5)
  expectEqual(20, y)
  expectEqual((5, 4), pullback(1))
}

runAllTests()
