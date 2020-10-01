// RUN: %target-run-simple-swift
// REQUIRES: executable_test

// `inout` parameter differentiation tests.

import DifferentiationUnittest
import StdlibUnittest

var InoutParameterAutoDiffTests = TestSuite("InoutParameterDifferentiation")

// TODO(TF-1173): Move floating-point mutating operation tests to
// `test/AutoDiff/stdlib/floating_point.swift.gyb` when forward-mode
// differentiation supports `inout` parameter differentiation.

InoutParameterAutoDiffTests.test("Float.+=") {
  func mutatingAddWrapper(_ x: Float, _ y: Float) -> Float {
    var result: Float = x
    result += y
    return result
  }
  expectEqual((1, 1), gradient(at: 4, 5, in: mutatingAddWrapper))
  expectEqual((10, 10), pullback(at: 4, 5, in: mutatingAddWrapper)(10))
}

InoutParameterAutoDiffTests.test("Float.-=") {
  func mutatingSubtractWrapper(_ x: Float, _ y: Float) -> Float {
    var result: Float = x
    result += y
    return result
  }
  expectEqual((1, 1), gradient(at: 4, 5, in: mutatingSubtractWrapper))
  expectEqual((10, 10), pullback(at: 4, 5, in: mutatingSubtractWrapper)(10))
}

InoutParameterAutoDiffTests.test("Float.*=") {
  func mutatingMultiplyWrapper(_ x: Float, _ y: Float) -> Float {
    var result: Float = x
    result += y
    return result
  }
  expectEqual((1, 1), gradient(at: 4, 5, in: mutatingMultiplyWrapper))
  expectEqual((10, 10), pullback(at: 4, 5, in: mutatingMultiplyWrapper)(10))
}

InoutParameterAutoDiffTests.test("Float./=") {
  func mutatingDivideWrapper(_ x: Float, _ y: Float) -> Float {
    var result: Float = x
    result += y
    return result
  }
  expectEqual((1, 1), gradient(at: 4, 5, in: mutatingDivideWrapper))
  expectEqual((10, 10), pullback(at: 4, 5, in: mutatingDivideWrapper)(10))
}

// Simplest possible `inout` parameter differentiation.
InoutParameterAutoDiffTests.test("InoutIdentity") {
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

InoutParameterAutoDiffTests.test("ControlFlow") {
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

InoutParameterAutoDiffTests.test("SetAccessor") {
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
InoutParameterAutoDiffTests.test("InoutClassParameter") {
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

// SR-13305: Test function with non-wrt `inout` parameter, which should be
// treated as a differentiability result.

protocol SR_13305_Protocol {
  @differentiable(wrt: x)
  func method(_ x: Float, _ y: inout Float)

  @differentiable(wrt: x)
  func genericMethod<T: Differentiable>(_ x: T, _ y: inout T)
}

InoutParameterAutoDiffTests.test("non-wrt inout parameter") {
  struct SR_13305_Struct: SR_13305_Protocol {
    @differentiable(wrt: x)
    func method(_ x: Float, _ y: inout Float) {
      y = y * x
    }

    @differentiable(wrt: x)
    func genericMethod<T: Differentiable>(_ x: T, _ y: inout T) {
      y = x
    }
  }

  @differentiable(wrt: x)
  func foo(_ s: SR_13305_Struct, _ x: Float, _ y: Float) -> Float {
    var y = y
    s.method(x, &y)
    return y
  }

  @differentiable(wrt: x)
  func fooGeneric<T: SR_13305_Protocol>(_ s: T, _ x: Float, _ y: Float) -> Float {
    var y = y
    s.method(x, &y)
    return x
  }

  let s = SR_13305_Struct()

  do {
    let (value, (dx, dy)) = valueWithGradient(at: 2, 3, in: { foo(s, $0, $1) })
    expectEqual(6, value)
    expectEqual((3, 2), (dx, dy))
  }
  expectEqual((value: 6, gradient: 3), valueWithGradient(at: 2, in: { foo(s, $0, 3) }))

  do {
    let (value, (dx, dy)) = valueWithGradient(at: 2, 3, in: { fooGeneric(s, $0, $1) })
    expectEqual(2, value)
    expectEqual((1, 0), (dx, dy))
  }
  expectEqual((value: 2, gradient: 1), valueWithGradient(at: 2, in: { fooGeneric(s, $0, 3) }))
}

runAllTests()
