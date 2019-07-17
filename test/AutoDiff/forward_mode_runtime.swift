// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -Xllvm -run-jvp-generation -parse-stdlib -o %t/Builtins
// RUN: %target-codesign %t/Builtins
// RUN: %target-run %t/Builtins
// REQUIRES: executable_test

import Swift
import StdlibUnittest

var ForwardModeTests = TestSuite("ForwardMode")

ForwardModeTests.test("Unary") {
  func func_to_diff(x: Float) -> Float {
    return x * x
  }
  let (y, differential) = Builtin.autodiffApply_jvp(func_to_diff, 4)
  expectEqual(16, y)
  expectEqual(8, differential(1))
}

ForwardModeTests.test("Binary") {
  func func_to_diff(x: Float, y: Float) -> Float {
    return x * y
  }
  let (y, pullback) = Builtin.autodiffApply_jvp_arity2(func_to_diff, 4, 5)
  expectEqual(20, y)
  expectEqual(9, pullback(1, 1))
}

runAllTests()
