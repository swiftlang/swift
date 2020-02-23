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

// Simplest possible `inout` parameter differentiation.
InoutParametersTests.test("InoutIdentity") {
  // Semantically, an empty function with an `inout` parameter is an identity
  // function.
  func inoutIdentity(_ x: inout Float) {}

  func identity(_ x: Float) -> Float {
    var result = x
    inoutIdentity(&result)
    return result
  }
  expectEqual(1, gradient(at: 10, in: identity))
  expectEqual(10, pullback(at: 10, in: identity)(10))
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

// Test differentiation wrt `inout` parameters that have a class type.
InoutParametersTests.test("InoutClassParameter") {
  class Class: Differentiable {
    @differentiable
    var x: Float

    init(_ x: Float) {
      self.x = x
    }
  }

  do {
    func squaredViaMutation(_ c: inout Class) {
      c = Class(c.x * c.x)
    }
    func squared(_ x: Float) -> Float {
      var c = Class(x)
      squaredViaMutation(&c)
      return c.x
    }
    expectEqual((100, 20), valueWithGradient(at: 10, in: squared))
    expectEqual(200, pullback(at: 10, in: squared)(10))
  }

  do {
    func squaredViaModifyAccessor(_ c: inout Class) {
      // The line below calls `Class.x.modify`.
      c.x *= c.x
    }
    func squared(_ x: Float) -> Float {
      var c = Class(x)
      squaredViaModifyAccessor(&c)
      return c.x
    }
    // FIXME(TF-1080): Fix incorrect class property `modify` accessor derivative values.
    // expectEqual((100, 20), valueWithGradient(at: 10, in: squared))
    // expectEqual(200, pullback(at: 10, in: squared)(10))
    expectEqual((100, 1), valueWithGradient(at: 10, in: squared))
    expectEqual(10, pullback(at: 10, in: squared)(10))
  }
}

runAllTests()
