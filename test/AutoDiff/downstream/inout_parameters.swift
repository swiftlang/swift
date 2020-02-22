// RUN: %target-run-simple-swift
// REQUIRES: executable_test

// `inout` parameter differentiation tests.

import StdlibUnittest

var InoutParametersTests = TestSuite("InoutParameters")

// TODO(TF-1173): Move floating-point mutating operation tests to
// `floating_point.swift.gyb` when forward-mode differentiation supports `inout`
// parameter differentiation.

InoutParametersTests.test("Float.+=") {
  func mutatingAddWrapper(_ x: Float, _ y: Float) -> Float {
    var result: Float = x
    result += y
    return result
  }
  expectEqual((1, 1), gradient(at: 4, 5, in: mutatingAddWrapper))
  expectEqual((10, 10), pullback(at: 4, 5, in: mutatingAddWrapper)(10))
}

InoutParametersTests.test("Float.-=") {
  func mutatingSubtractWrapper(_ x: Float, _ y: Float) -> Float {
    var result: Float = x
    result += y
    return result
  }
  expectEqual((1, 1), gradient(at: 4, 5, in: mutatingSubtractWrapper))
  expectEqual((10, 10), pullback(at: 4, 5, in: mutatingSubtractWrapper)(10))
}

InoutParametersTests.test("Float.*=") {
  func mutatingMultiplyWrapper(_ x: Float, _ y: Float) -> Float {
    var result: Float = x
    result += y
    return result
  }
  expectEqual((1, 1), gradient(at: 4, 5, in: mutatingMultiplyWrapper))
  expectEqual((10, 10), pullback(at: 4, 5, in: mutatingMultiplyWrapper)(10))
}

InoutParametersTests.test("Float./=") {
  func mutatingDivideWrapper(_ x: Float, _ y: Float) -> Float {
    var result: Float = x
    result += y
    return result
  }
  expectEqual((1, 1), gradient(at: 4, 5, in: mutatingDivideWrapper))
  expectEqual((10, 10), pullback(at: 4, 5, in: mutatingDivideWrapper)(10))
}

InoutParametersTests.test("InoutIdentity") {
  // Semantically, an empty function with an `inout` parameter is an identity
  // function.
  func inoutIdentity(_ x: inout Float) {}

  func identity(_ x: Float) -> Float {
    var result = x
    inoutIdentity(&result)
    return result
  }
  expectEqual(1, gradient(at: 1, in: identity))
  expectEqual(10, pullback(at: 1, in: identity)(10))
}

extension Float {
  // Custom version of `Float.*=`, implemented using `Float.*` and mutation.
  // Verify that its generated derivative has the same behavior as the
  // registered derivative for `Float.*=`.
  @differentiable
  static func multiplyAssign(_ lhs: inout Float, _ rhs: Float) {
    lhs = lhs * rhs
  }
}

InoutParametersTests.test("ControlFlow") {
  func sum(_ array: [Float]) -> Float {
    var result: Float = 0
    for i in withoutDerivative(at: array.indices) {
      result += array[i]
    }
    return result
  }
  expectEqual([1, 1, 1], gradient(at: [1, 2, 3], in: sum))

  func product(_ array: [Float]) -> Float {
    var result: Float = 1
    for i in withoutDerivative(at: array.indices) {
      result *= array[i]
    }
    return result
  }
  expectEqual([20, 15, 12], gradient(at: [3, 4, 5], in: product))

  func productCustom(_ array: [Float]) -> Float {
    var result: Float = 1
    for i in withoutDerivative(at: array.indices) {
      Float.multiplyAssign(&result, array[i])
    }
    return result
  }
  expectEqual([20, 15, 12], gradient(at: [3, 4, 5], in: productCustom))
}

InoutParametersTests.test("SetAccessor") {
  struct S: Differentiable {
    var x: Float

    var computed: Float {
      get { x }
      set { x = newValue }
    }
  }

  // `squared` implemented using a `set` accessor.
  func squared(_ x: Float) -> Float {
    var s = S(x: 1)
    s.x *= x
    s.computed *= x
    return s.x
  }
  expectEqual(6, gradient(at: 3, in: squared))
  expectEqual(8, gradient(at: 4, in: squared))
}

runAllTests()
