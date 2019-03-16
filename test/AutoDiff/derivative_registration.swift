// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest

var DerivativeRegistrationTests = TestSuite("DerivativeRegistration")

@_semantics("autodiff.opaque")
func unary(x: Float) -> Float {
  return x
}
@differentiating(unary)
func _vjpUnary(x: Float) -> (value: Float, pullback: (Float) -> Float) {
  return (value: x, pullback: { v in v })
}
DerivativeRegistrationTests.test("UnaryFreeFunction") {
  expectEqual(1, gradient(at: 3.0, in: unary))
}

@_semantics("autodiff.opaque")
func multiply(_ x: Float, _ y: Float) -> Float {
  return x * y
}
@differentiating(multiply)
func _vjpMultiply(_ x: Float, _ y: Float)
  -> (value: Float, pullback: (Float) -> (Float, Float)) {
  return (x * y, { v in (v * y, v * x) })
}
DerivativeRegistrationTests.test("BinaryFreeFunction") {
  expectEqual((3.0, 2.0), gradient(at: 2.0, 3.0, in: { x, y in multiply(x, y) }))
}

struct Wrapper : Differentiable {
  var float: Float
}

extension Wrapper {
  @_semantics("autodiff.opaque")
  static func multiply(_ x: Float, _ y: Float) -> Float {
    return x * y
  }

  @differentiating(multiply)
  static func _vjpMultiply(_ x: Float, _ y: Float)
    -> (value: Float, pullback: (Float) -> (Float, Float)) {
    return (x * y, { v in (v * y, v * x) })
  }
}
DerivativeRegistrationTests.test("StaticMethod") {
  expectEqual((3.0, 2.0), gradient(at: 2.0, 3.0, in: { x, y in Wrapper.multiply(x, y) }))
}

extension Wrapper {
  @_semantics("autodiff.opaque")
  func multiply(_ x: Float) -> Float {
    return float * x
  }

  @differentiating(multiply)
  func _vjpMultiply(_ x: Float)
    -> (value: Float, pullback: (Float) -> (Wrapper.CotangentVector, Float)) {
    return (float * x, { v in
      (Wrapper.CotangentVector(float: v * x), v * self.float)
    })
  }
}
DerivativeRegistrationTests.test("InstanceMethod") {
  let x: Float = 2
  let wrapper = Wrapper(float: 3)
  let (𝛁wrapper, 𝛁x) = wrapper.gradient(at: x) { wrapper, x in wrapper.multiply(x) }
  expectEqual(Wrapper.CotangentVector(float: 2), 𝛁wrapper)
  expectEqual(3, 𝛁x)
}

runAllTests()
