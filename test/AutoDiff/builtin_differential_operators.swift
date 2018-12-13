// RUN: %target-run-simple-parse-stdlib-swift
// REQUIRES: executable_test

import Swift
import StdlibUnittest

var BuiltinDifferentialOperatorTests = TestSuite("BuiltinDifferentialOperators")

BuiltinDifferentialOperatorTests.test("Trivial") {
  func func_to_diff(x: Float) -> Float {
    return x * x
  }
  let (y, pullback) = Builtin.autodiffApplyVJP(func_to_diff, 4)
  expectEqual(8, pullback(1))
}

runAllTests()
