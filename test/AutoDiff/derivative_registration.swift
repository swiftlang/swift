// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest
import DifferentiationUnittest

var DerivativeRegistrationTests = TestSuite("DerivativeRegistration")

@_semantics("autodiff.opaque")
func unary(x: Tracked<Float>) -> Tracked<Float> {
  return x
}
@differentiating(unary)
func _vjpUnary(x: Tracked<Float>) -> (value: Tracked<Float>, pullback: (Tracked<Float>) -> Tracked<Float>) {
  return (value: x, pullback: { v in v })
}
DerivativeRegistrationTests.testWithLeakChecking("UnaryFreeFunction") {
  expectEqual(1, gradient(at: 3.0, in: unary))
}

@_semantics("autodiff.opaque")
func multiply(_ x: Tracked<Float>, _ y: Tracked<Float>) -> Tracked<Float> {
  return x * y
}
@differentiating(multiply)
func _vjpMultiply(_ x: Tracked<Float>, _ y: Tracked<Float>)
  -> (value: Tracked<Float>, pullback: (Tracked<Float>) -> (Tracked<Float>, Tracked<Float>)) {
  return (x * y, { v in (v * y, v * x) })
}
DerivativeRegistrationTests.testWithLeakChecking("BinaryFreeFunction") {
  expectEqual((3.0, 2.0), gradient(at: 2.0, 3.0, in: { x, y in multiply(x, y) }))
}

struct Wrapper : Differentiable {
  var float: Tracked<Float>
}

extension Wrapper {
  @_semantics("autodiff.opaque")
  static func multiply(_ x: Tracked<Float>, _ y: Tracked<Float>) -> Tracked<Float> {
    return x * y
  }

  @differentiating(multiply)
  static func _vjpMultiply(_ x: Tracked<Float>, _ y: Tracked<Float>)
    -> (value: Tracked<Float>, pullback: (Tracked<Float>) -> (Tracked<Float>, Tracked<Float>)) {
    return (x * y, { v in (v * y, v * x) })
  }
}
DerivativeRegistrationTests.testWithLeakChecking("StaticMethod") {
  expectEqual((3.0, 2.0), gradient(at: 2.0, 3.0, in: { x, y in Wrapper.multiply(x, y) }))
}

extension Wrapper {
  @_semantics("autodiff.opaque")
  func multiply(_ x: Tracked<Float>) -> Tracked<Float> {
    return float * x
  }

  @differentiating(multiply)
  func _vjpMultiply(_ x: Tracked<Float>)
    -> (value: Tracked<Float>, pullback: (Tracked<Float>) -> (Wrapper.TangentVector, Tracked<Float>)) {
    return (float * x, { v in
      (Wrapper.TangentVector(float: v * x), v * self.float)
    })
  }
}
DerivativeRegistrationTests.testWithLeakChecking("InstanceMethod") {
  let x: Tracked<Float> = 2
  let wrapper = Wrapper(float: 3)
  let (𝛁wrapper, 𝛁x) = wrapper.gradient(at: x) { wrapper, x in wrapper.multiply(x) }
  expectEqual(Wrapper.TangentVector(float: 2), 𝛁wrapper)
  expectEqual(3, 𝛁x)
}

runAllTests()
