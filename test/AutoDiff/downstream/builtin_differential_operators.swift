// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -parse-stdlib -o %t/Builtins
// RUN: %target-codesign %t/Builtins
// RUN: %target-run %t/Builtins
// REQUIRES: executable_test

import Swift
import StdlibUnittest

var BuiltinDifferentialOperatorTests = TestSuite("BuiltinDifferentialOperators")

BuiltinDifferentialOperatorTests.test("UnaryDifferentiable") {
  func func_to_diff(x: Float) -> Float {
    return x * x
  }
  let (y, pullback) = Builtin.applyDerivative_vjp(func_to_diff, 4)
  expectEqual(16, y)
  expectEqual(8, pullback(1))
}

BuiltinDifferentialOperatorTests.test("BinaryDifferentiable") {
  func func_to_diff(x: Float, y: Float) -> Float {
    return x * y
  }
  let (y, pullback) = Builtin.applyDerivative_vjp_arity2(func_to_diff, 4, 5)
  expectEqual(20, y)
  expectEqual((5, 4), pullback(1))
}

BuiltinDifferentialOperatorTests.test("UnaryTranspose") {
  func foo(_ x: Float) -> Float {
    return 2 * x
  }
  func foo_transpose(_ v: Float) -> Float {
    return v
  }
  let lin_foo = Builtin.linearFunction_arity1(foo, foo_transpose)
  expectEqual(0, Builtin.applyTranspose_arity1(lin_foo, 0))
  expectEqual(1, Builtin.applyTranspose_arity1(lin_foo, 1))
}

BuiltinDifferentialOperatorTests.test("BinaryTranspose") {
  func foo(_ x: Float, _ y: Float) -> Float {
    return x + y
  }
  func foo_transpose(_ v: Float) -> (Float, Float) {
    return (v, v)
  }
  let lin_foo = Builtin.linearFunction_arity2(foo, foo_transpose)
  expectEqual((0, 0), Builtin.applyTranspose_arity2(lin_foo, 0))
  expectEqual((1, 1), Builtin.applyTranspose_arity2(lin_foo, 1))
}

runAllTests()
