// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -parse-stdlib -o %t/Builtins
// RUN: %target-codesign %t/Builtins
// RUN: %target-run %t/Builtins
// REQUIRES: executable_test

import Swift
import StdlibUnittest

var BuiltinDifferentiableFunctionConstructorTests =
  TestSuite("BuiltinDifferentiableFunctionConstructor")

BuiltinDifferentiableFunctionConstructorTests.test("UnaryDifferentiable") {
  func foo(_ x: Float) -> Float {
    return x * x
  }
  func foo_jvp(_ x: Float) -> (value: Float, differential: (Float) -> Float) {
    return (foo(x), { v in 2 * x * v })
  }
  func foo_vjp(_ x: Float) -> (value: Float, pullback: (Float) -> Float) {
    return (foo(x), { v in 2 * x * v })
  }
  let diff_foo = Builtin.differentiableFunction_arity1(foo, foo_jvp, foo_vjp)
  let (y, pb) = valueWithPullback(at: 4, in: diff_foo)
  expectEqual(16, y)
  expectEqual(0, pb(0))
  expectEqual(8, pb(1))
}

BuiltinDifferentiableFunctionConstructorTests.test("BinaryDifferentiable") {
  func foo(_ x: Float, _ y: Float) -> Float {
    return x * y
  }
  func foo_jvp(_ x: Float, _ y: Float) -> (value: Float, differential: (Float, Float) -> Float) {
    return (foo(x, y), { v1, v2 in y * v1 + x * v2 })
  }
  func foo_vjp(_ x: Float, _ y: Float) -> (value: Float, pullback: (Float) -> (Float, Float)) {
    return (foo(x, y), { v in (y * v, x * v) })
  }
  let diff_foo = Builtin.differentiableFunction_arity2(foo, foo_jvp, foo_vjp)
  let (y, pb) = valueWithPullback(at: 4, 6, in: diff_foo)
  expectEqual(24, y)
  expectEqual((0, 0), pb(0))
  expectEqual((6, 4), pb(1))
}

BuiltinDifferentiableFunctionConstructorTests.test("UnaryLinear") {
  func foo(_ x: Float) -> Float {
    return 2 * x
  }
  func foo_transpose(_ v: Float) -> Float {
    return v
  }
  let lin_foo = Builtin.linearFunction_arity1(foo, foo_transpose)
  // TODO(SR-11845): Uncomment the following once `transpose(of:)` is available.
  // let trans_foo = transpose(of: lin_foo)
  // expectEqual(2, trans_foo(2))
}

BuiltinDifferentiableFunctionConstructorTests.test("BinaryLinear") {
  func foo(_ x: Float, _ y: Float) -> Float {
    return x + y
  }
  func foo_transpose(_ v: Float) -> (Float, Float) {
    return (v, v)
  }
  let lin_foo = Builtin.linearFunction_arity2(foo, foo_transpose)
  // TODO(SR-11845): Uncomment the following once `transpose(of:)` is available.
  // let trans_foo = transpose(of: lin_foo)
  // expectEqual((1, 1), trans_foo(1))
}

runAllTests()
