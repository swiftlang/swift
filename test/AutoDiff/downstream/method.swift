// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest
import DifferentiationUnittest

var MethodTests = TestSuite("Method")

// ==== Tests with generated adjoint ====

struct Parameter : Equatable {
  private let storedX: Tracked<Float>
  @differentiable(wrt: (self))
  var x: Tracked<Float> {
      return storedX
  }

  init(x: Tracked<Float>) {
    storedX = x
  }

  @derivative(of: x)
  func vjpX() -> (value: Tracked<Float>, pullback: (Tracked<Float>) -> Parameter) {
    return (x, { dx in Parameter(x: dx) } )
  }

  @derivative(of: x)
  func jvpX() -> (value: Tracked<Float>, differential: (Parameter) -> Tracked<Float>) {
    return (x, { $0.x })
  }
}

extension Parameter {
  func squared() -> Tracked<Float> {
    return x * x
  }

  static func squared(p: Parameter) -> Tracked<Float> {
    return p.x * p.x
  }

  func multiplied(with other: Tracked<Float>) -> Tracked<Float> {
    return x * other
  }

  static func * (_ a: Parameter, _ b: Parameter) -> Tracked<Float> {
    return a.x * b.x
  }
}

extension Parameter : Differentiable, AdditiveArithmetic {
  typealias TangentVector = Parameter
  typealias Scalar = Tracked<Float>
  typealias Shape = ()
  init(repeating repeatedValue: Tracked<Float>, shape: ()) {
    self.init(x: repeatedValue)
  }
  static func + (lhs: Parameter, rhs: Parameter) -> Parameter {
    return Parameter(x: lhs.x + rhs.x)
  }
  static func - (lhs: Parameter, rhs: Parameter) -> Parameter {
    return Parameter(x: lhs.x - rhs.x)
  }
  static func * (lhs: Scalar, rhs: Parameter) -> Parameter {
    return Parameter(x: lhs * rhs.x)
  }
  static var zero: Parameter { return Parameter(x: 0) }
}

MethodTests.testWithLeakChecking(
  "instance method with generated adjoint, called from differentated func"
) {
  func f(_ p: Parameter) -> Tracked<Float> {
    return 100 * p.squared()
  }
  expectEqual(Parameter(x: 4 * 100), gradient(at: Parameter(x: 2), in: f))
  expectEqual(Parameter(x: 40 * 100), gradient(at: Parameter(x: 20), in: f))
}

MethodTests.testWithLeakChecking(
  "instance method with generated adjoint, differentiated directly"
) {
  // This is our current syntax for taking gradients of instance methods
  // directly. If/when we develop nicer syntax for this, change this test.
  func g(p: Parameter) -> Tracked<Float> { p.squared() }
  expectEqual(Parameter(x: 4), gradient(at: Parameter(x: 2), in: g))
  expectEqual(Parameter(x: 40), gradient(at: Parameter(x: 20), in: g))
}

MethodTests.testWithLeakChecking("instance method with generated adjoint, wrt only self") {
  func f(_ p: Parameter) -> Tracked<Float> {
    return 100 * p.multiplied(with: 200)
  }
  expectEqual(Parameter(x: 100 * 200), gradient(at: Parameter(x: 1), in: f))
  expectEqual(Parameter(x: 100 * 200), gradient(at: Parameter(x: 2), in: f))
}

MethodTests.testWithLeakChecking("instance method with generated adjoint, wrt only non-self") {
  func f(_ other: Tracked<Float>) -> Tracked<Float> {
    return 100 * Parameter(x: 200).multiplied(with: other)
  }
  expectEqual(100 * 200, gradient(at: 1, in: f))
  expectEqual(100 * 200, gradient(at: 2, in: f))
}

// FIXME: Add a binary differential operator.
//
// MethodTests.testWithLeakChecking(
//   "instance method with generated adjoint, wrt self and non-self"
// ) {
//   let g = #gradient({ (p: Parameter, o: Tracked<Float>) in p.multiplied(with: o) })
//   expectEqual((Parameter(x: 100), 200), g(Parameter(x: 200), 100))
//   expectEqual((Parameter(x: 200), 100), g(Parameter(x: 100), 200))
// }

MethodTests.testWithLeakChecking(
  "static method with generated adjoint, called from differentiated func"
) {
  func f(_ p: Parameter) -> Tracked<Float> {
    return 100 * Parameter.squared(p: p)
  }
  expectEqual(Parameter(x: 4 * 100), gradient(at: Parameter(x: 2), in: f))
  expectEqual(Parameter(x: 40 * 100), gradient(at: Parameter(x: 20), in: f))
}

// TODO(SR-8699): Fix this test.
// MethodTests.testWithLeakChecking(
//   "static method with generated adjoint, differentiated directly"
// ) {
//   let grad = #gradient(Parameter.squared(p:))
//   expectEqual(Parameter(x: 4), grad(Parameter(x: 2)))
//   expectEqual(Parameter(x: 40), grad(Parameter(x: 20)))
// }

MethodTests.testWithLeakChecking("static method with generated adjoint, wrt only first param") {
  func f(_ p: Parameter) -> Tracked<Float> {
    return 100 * (p * Parameter(x: 200))
  }
  expectEqual(Parameter(x: 100 * 200), gradient(at: Parameter(x: 1), in: f))
  expectEqual(Parameter(x: 100 * 200), gradient(at: Parameter(x: 2), in: f))
}

MethodTests.testWithLeakChecking("static method with generated adjoint, wrt only second param") {
  func f(_ p: Parameter) -> Tracked<Float> {
    return 100 * (Parameter(x: 200) * p)
  }
  expectEqual(Parameter(x: 100 * 200), gradient(at: Parameter(x: 1), in: f))
  expectEqual(Parameter(x: 100 * 200), gradient(at: Parameter(x: 2), in: f))
}

MethodTests.testWithLeakChecking("static method with generated adjoint, wrt all params") {
  func g(a: Parameter, b: Parameter) -> Tracked<Float> { a * b }
  expectEqual((Parameter(x: 100), Parameter(x: 200)),
              gradient(at: Parameter(x: 200), Parameter(x: 100), in: g))
  expectEqual((Parameter(x: 200), Parameter(x: 100)),
              gradient(at: Parameter(x: 100), Parameter(x: 200), in: g))
}

// ==== Tests with custom adjoint ====

// Test self-reordering thunk for jvp/vjp methods.
struct DiffWrtSelf : Differentiable {
  @differentiable(wrt: (self, x, y))
  func call<T : Differentiable, U : Differentiable>(_ x: T, _ y: U) -> T {
    return x
  }
  @derivative(of: call)
  func _jvpCall<T : Differentiable, U : Differentiable>(_ x: T, _ y: U)
    -> (value: T, differential: (DiffWrtSelf.TangentVector, T.TangentVector, U.TangentVector) -> T.TangentVector) {
    return (x, { (dself, dx, dy) in dx })
  }
  @derivative(of: call)
  func _vjpCall<T : Differentiable, U : Differentiable>(_ x: T, _ y: U)
    -> (value: T, pullback: (T.TangentVector) -> (DiffWrtSelf.TangentVector, T.TangentVector, U.TangentVector)) {
    return (x, { (.zero, $0, .zero) })
  }
}

struct CustomParameter : Equatable {
  let storedX: Tracked<Float>
  @differentiable(wrt: (self))
  var x: Tracked<Float> {
      return storedX
  }

  init(x: Tracked<Float>) {
    storedX = x
  }

  @derivative(of: x)
  func vjpX() -> (value: Tracked<Float>, pullback: (Tracked<Float>) -> CustomParameter) {
    return (x, { dx in CustomParameter(x: dx) })
  }
}

extension CustomParameter : Differentiable, AdditiveArithmetic {
  typealias TangentVector = CustomParameter
  typealias Scalar = Tracked<Float>
  typealias Shape = ()
  init(repeating repeatedValue: Tracked<Float>, shape: ()) {
    self.init(x: repeatedValue)
  }
  static func + (lhs: CustomParameter, rhs: CustomParameter) -> CustomParameter {
    return CustomParameter(x: lhs.x + rhs.x)
  }
  static func - (lhs: CustomParameter, rhs: CustomParameter) -> CustomParameter {
    return CustomParameter(x: lhs.x - rhs.x)
  }
  static func * (lhs: Scalar, rhs: CustomParameter) -> CustomParameter {
    return CustomParameter(x: lhs * rhs.x)
  }
  static var zero: CustomParameter { return CustomParameter(x: 0) }
}

extension Tracked where T : FloatingPoint {
  func clamped(to limits: ClosedRange<Tracked<T>>) -> Tracked<T> {
    return min(max(self, limits.lowerBound), limits.upperBound)
  }
}

extension CustomParameter {
  @differentiable(wrt: (self))
  func squared() -> Tracked<Float> {
    return x * x
  }

  @derivative(of: squared)
  func dSquared() -> (value: Tracked<Float>, pullback: (Tracked<Float>) -> CustomParameter) {
    return (squared(), { [x] v in CustomParameter(x: (2 * x).clamped(to: -10.0...10.0) * v) })
  }

  @differentiable
  static func squared(p: CustomParameter) -> Tracked<Float> {
    return p.x * p.x
  }

  @derivative(of: squared)
  static func dSquared(
    _ p: CustomParameter
  ) -> (value: Tracked<Float>, pullback: (Tracked<Float>) -> CustomParameter) {
    return (p.x * p.x, { v in CustomParameter(x: (2 * p.x).clamped(to: -10.0...10.0) * v) })
  }

  // There is currently no way to define multiple custom VJPs wrt different
  // parameters on the same func, so we define a copy of this func per adjoint.

  @differentiable(wrt: (self, other))
  func multiplied(with other: Tracked<Float>) -> Tracked<Float> {
    return x * other
  }

  @differentiable(wrt: (other))
  func multiplied_constSelf(with other: Tracked<Float>) -> Tracked<Float> {
    return x * other
  }

  @differentiable(wrt: (self))
  func multiplied_constOther(with other: Tracked<Float>) -> Tracked<Float> {
    return x * other
  }

  @derivative(of: multiplied)
  func dMultiplied_wrtAll(
    with other: Tracked<Float>
  ) -> (value: Tracked<Float>, pullback: (Tracked<Float>) -> (CustomParameter, Tracked<Float>)) {
    return (multiplied(with: other),
      { [x] v in (CustomParameter(x: other.clamped(to: -10.0...10.0) * v),
                  x.clamped(to: -10.0...10.0) * v) })
  }

  @derivative(of: multiplied_constSelf, wrt: other)
  func dMultiplied_wrtOther(
    with other: Tracked<Float>
  ) -> (value: Tracked<Float>, pullback: (Tracked<Float>) -> Tracked<Float>) {
    let (r, pb) = dMultiplied_wrtAll(with: other)
    return (r, { v in pb(v).1 })
  }

  @derivative(of: multiplied_constOther, wrt: self)
  func dMultiplied_wrtSelf(
    with other: Tracked<Float>
  ) -> (value: Tracked<Float>, pullback: (Tracked<Float>) -> CustomParameter) {
    let (r, pb) = dMultiplied_wrtAll(with: other)
    return (r, { v in pb(v).0 })
  }

  @differentiable
  static func multiply(_ lhs: CustomParameter, _ rhs: CustomParameter)
      -> Tracked<Float> {
    return lhs.x * rhs.x
  }

  @differentiable(wrt: (rhs))
  static func multiply_constLhs(_ lhs: CustomParameter, _ rhs: CustomParameter) -> Tracked<Float> {
    return lhs.x * rhs.x
  }

  @derivative(of: multiply)
  static func dMultiply_wrtAll(_ lhs: CustomParameter,_ rhs: CustomParameter)
      -> (value: Tracked<Float>, pullback: (Tracked<Float>) -> (CustomParameter, CustomParameter)) {
    let result = multiply(lhs, rhs)
    return (result, { v in (CustomParameter(x: rhs.x.clamped(to: -10.0...10.0) * v),
                            CustomParameter(x: lhs.x.clamped(to: -10.0...10.0) * v)) })
  }

  @derivative(of: multiply_constLhs, wrt: rhs)
  static func dMultiply_wrtRhs(_ lhs: CustomParameter, _ rhs: CustomParameter)
      -> (value: Tracked<Float>, pullback: (Tracked<Float>) -> CustomParameter) {
    let (r, pb) = dMultiply_wrtAll(lhs, rhs)
    return (r, { v in pb(v).1 })
  }
}

MethodTests.testWithLeakChecking(
  "instance method with custom adjoint, called from differentated func"
) {
  func f(_ p: CustomParameter) -> Tracked<Float> {
    return 100 * p.squared()
  }
  expectEqual(CustomParameter(x: 4 * 100), gradient(at: CustomParameter(x: 2), in: f))
  expectEqual(CustomParameter(x: 10 * 100), gradient(at: CustomParameter(x: 20), in: f))
}

MethodTests.testWithLeakChecking("instance method with generated adjoint, differentated directly") {
  // This is our current syntax for taking gradients of instance methods
  // directly. If/when we develop nicer syntax for this, change this test.
  func g(p: CustomParameter) -> Tracked<Float> { p.squared() }
  expectEqual(CustomParameter(x: 4), gradient(at: CustomParameter(x: 2), in: g))
  expectEqual(CustomParameter(x: 10), gradient(at: CustomParameter(x: 20), in: g))
}

MethodTests.testWithLeakChecking("static method with custom adjoint, called from differentated func") {
  func f(_ p: CustomParameter) -> Tracked<Float> {
    return 100 * CustomParameter.squared(p: p)
  }
  expectEqual(CustomParameter(x: 4 * 100), gradient(at: CustomParameter(x: 2), in: f))
  expectEqual(CustomParameter(x: 10 * 100), gradient(at: CustomParameter(x: 20), in: f))
}

// TODO(SR-8699): Fix this test.
// MethodTests.testWithLeakChecking("static method with custom adjoint, differentiated directly") {
//   let grad = #gradient(CustomParameter.squared(p:))
//   expectEqual(CustomParameter(x: 4), grad(CustomParameter(x: 2)))
//   expectEqual(CustomParameter(x: 10), grad(CustomParameter(x: 20)))
// }

MethodTests.testWithLeakChecking("instance method with custom adjoint, wrt only self") {
  func f(_ p: CustomParameter) -> Tracked<Float> {
    return 100 * p.multiplied_constOther(with: 200)
  }
  expectEqual(CustomParameter(x: 100 * 10), gradient(at: CustomParameter(x: 1), in: f))
  expectEqual(CustomParameter(x: 100 * 10), gradient(at: CustomParameter(x: 2), in: f))
}

MethodTests.testWithLeakChecking("instance method with custom adjoint, wrt only non-self") {
  func f(_ other: Tracked<Float>) -> Tracked<Float> {
    return 100 * CustomParameter(x: 200).multiplied_constSelf(with: other)
  }
  expectEqual(100 * 10, gradient(at: 1, in: f))
  expectEqual(100 * 10, gradient(at: 2, in: f))
}

MethodTests.testWithLeakChecking("instance method with custom adjoint, wrt self and non-self") {
  func g(p: CustomParameter, o: Tracked<Float>) -> Tracked<Float> { p.multiplied(with: o) }
  expectEqual((CustomParameter(x: 5), 10), gradient(at: CustomParameter(x: 100), 5, in: g))
  expectEqual((CustomParameter(x: 10), 5), gradient(at: CustomParameter(x: 5), 100, in: g))
}

MethodTests.testWithLeakChecking("static method with custom adjoint, wrt only lhs") {
  func f(_ p: CustomParameter) -> Tracked<Float> {
    return 100 * CustomParameter.multiply_constLhs(CustomParameter(x: 200), p)
  }
  expectEqual(CustomParameter(x: 100 * 10), gradient(at: CustomParameter(x: 1), in: f))
  expectEqual(CustomParameter(x: 100 * 10), gradient(at: CustomParameter(x: 2), in: f))
}

MethodTests.testWithLeakChecking("static method with custom adjoint, wrt only rhs") {
  func f(_ p: CustomParameter) -> Tracked<Float> {
    return 100 * CustomParameter.multiply_constLhs(CustomParameter(x: 200), p)
  }
  expectEqual(CustomParameter(x: 100 * 10), gradient(at: CustomParameter(x: 1), in: f))
  expectEqual(CustomParameter(x: 100 * 10), gradient(at: CustomParameter(x: 2), in: f))
}

MethodTests.testWithLeakChecking("static method with custom adjoint, wrt all") {
  func f(_ a: CustomParameter, _ b: CustomParameter) -> Tracked<Float> {
    return CustomParameter.multiply(a, b)
  }
  expectEqual((CustomParameter(x: 5), CustomParameter(x: 10)),
              gradient(at: CustomParameter(x: 100), CustomParameter(x: 5), in: f))
  expectEqual((CustomParameter(x: 10), CustomParameter(x: 5)),
              gradient(at: CustomParameter(x: 5), CustomParameter(x: 100), in: f))
}

runAllTests()
