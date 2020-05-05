// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest
import DifferentiationUnittest

var DerivativeRegistrationTests = TestSuite("DerivativeRegistration")

@_semantics("autodiff.opaque")
func unary(x: Tracked<Float>) -> Tracked<Float> {
  return x
}
@derivative(of: unary)
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
@derivative(of: multiply)
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
  init(_ x: Tracked<Float>, _ y: Tracked<Float>) {
    self.float = x * y
  }

  @derivative(of: init(_:_:))
  static func _vjpInit(_ x: Tracked<Float>, _ y: Tracked<Float>)
    -> (value: Self, pullback: (TangentVector) -> (Tracked<Float>, Tracked<Float>)) {
    return (.init(x, y), { v in (v.float * y, v.float * x) })
  }
}
DerivativeRegistrationTests.testWithLeakChecking("Initializer") {
  let v = Wrapper.TangentVector(float: 1)
  let (𝛁x, 𝛁y) = pullback(at: 3, 4, in: { x, y in Wrapper(x, y) })(v)
  expectEqual(4, 𝛁x)
  expectEqual(3, 𝛁y)
}

extension Wrapper {
  @_semantics("autodiff.opaque")
  static func multiply(_ x: Tracked<Float>, _ y: Tracked<Float>) -> Tracked<Float> {
    return x * y
  }

  @derivative(of: multiply)
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

  @derivative(of: multiply)
  func _vjpMultiply(_ x: Tracked<Float>)
    -> (value: Tracked<Float>, pullback: (Tracked<Float>) -> (Wrapper.TangentVector, Tracked<Float>)) {
    return (float * x, { v in
      (TangentVector(float: v * x), v * self.float)
    })
  }
}
DerivativeRegistrationTests.testWithLeakChecking("InstanceMethod") {
  let x: Tracked<Float> = 2
  let wrapper = Wrapper(float: 3)
  let (𝛁wrapper, 𝛁x) = gradient(at: wrapper, x) { wrapper, x in wrapper.multiply(x) }
  expectEqual(Wrapper.TangentVector(float: 2), 𝛁wrapper)
  expectEqual(3, 𝛁x)
}

extension Wrapper {
  subscript(_ x: Tracked<Float>) -> Tracked<Float> {
    @_semantics("autodiff.opaque")
    get { float * x }
    set {}
  }

  @derivative(of: subscript(_:))
  func _vjpSubscript(_ x: Tracked<Float>)
    -> (value: Tracked<Float>, pullback: (Tracked<Float>) -> (Wrapper.TangentVector, Tracked<Float>)) {
    return (self[x], { v in
      (TangentVector(float: v * x), v * self.float)
    })
  }
}
DerivativeRegistrationTests.testWithLeakChecking("Subscript") {
  let x: Tracked<Float> = 2
  let wrapper = Wrapper(float: 3)
  let (𝛁wrapper, 𝛁x) = gradient(at: wrapper, x) { wrapper, x in wrapper[x] }
  expectEqual(Wrapper.TangentVector(float: 2), 𝛁wrapper)
  expectEqual(3, 𝛁x)
}

extension Wrapper {
  var computedProperty: Tracked<Float> {
    @_semantics("autodiff.opaque")
    get { float * float }
    set {}
  }

  @derivative(of: computedProperty)
  func _vjpComputedProperty()
    -> (value: Tracked<Float>, pullback: (Tracked<Float>) -> Wrapper.TangentVector) {
    return (computedProperty, { [f = self.float] v in
      TangentVector(float: v * (f + f))
    })
  }
}
DerivativeRegistrationTests.testWithLeakChecking("ComputedProperty") {
  let wrapper = Wrapper(float: 3)
  let 𝛁wrapper = gradient(at: wrapper) { wrapper in wrapper.computedProperty }
  expectEqual(Wrapper.TangentVector(float: 6), 𝛁wrapper)
}

struct Generic<T> {
  @differentiable // derivative generic signature: none
  func instanceMethod(_ x: Tracked<Float>) -> Tracked<Float> {
    x
  }
}
extension Generic {
  @derivative(of: instanceMethod) // derivative generic signature: <T>
  func vjpInstanceMethod(_ x: Tracked<Float>)
    -> (value: Tracked<Float>, pullback: (Tracked<Float>) -> Tracked<Float>) {
    (x, { v in 1000 })
  }
}
DerivativeRegistrationTests.testWithLeakChecking("DerivativeGenericSignature") {
  let generic = Generic<Float>()
  let x: Tracked<Float> = 3
  let dx = gradient(at: x) { x in generic.instanceMethod(x) }
  expectEqual(1000, dx)
}

// When non-canonicalized generic signatures are used to compare derivative configurations, the
// `@differentiable` and `@derivative` attributes create separate derivatives, and we get a
// duplicate symbol error in TBDGen.
public protocol RefinesDifferentiable: Differentiable {}
extension Float: RefinesDifferentiable {}
@differentiable(where T: Differentiable, T: RefinesDifferentiable)
public func nonCanonicalizedGenSigComparison<T>(_ t: T) -> T { t }
@derivative(of: nonCanonicalizedGenSigComparison)
public func dNonCanonicalizedGenSigComparison<T: RefinesDifferentiable>(_ t: T)
  -> (value: T, pullback: (T.TangentVector) -> T.TangentVector)
{
  (t, { _ in T.TangentVector.zero })
}
DerivativeRegistrationTests.testWithLeakChecking("NonCanonicalizedGenericSignatureComparison") {
  let dx = gradient(at: Float(0), in: nonCanonicalizedGenSigComparison)
  // Expect that we use the custom registered derivative, not a generated derivative (which would
  // give a gradient of 1).
  expectEqual(0, dx)
}

// Test derivatives of default implementations.
protocol HasADefaultImplementation {
  func req(_ x: Tracked<Float>) -> Tracked<Float>
}
extension HasADefaultImplementation {
  func req(_ x: Tracked<Float>) -> Tracked<Float> { x }
  @derivative(of: req)
  func req(_ x: Tracked<Float>) -> (value: Tracked<Float>, pullback: (Tracked<Float>) -> Tracked<Float>) {
    (x, { 10 * $0 })
  }
}
struct StructConformingToHasADefaultImplementation : HasADefaultImplementation {}
DerivativeRegistrationTests.testWithLeakChecking("DerivativeOfDefaultImplementation") {
  let dx = gradient(at: Tracked<Float>(0)) { StructConformingToHasADefaultImplementation().req($0) }
  expectEqual(Tracked<Float>(10), dx)
}

runAllTests()
