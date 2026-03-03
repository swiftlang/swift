// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest
import DifferentiationUnittest

var MethodTests = TestSuite("Method")

// ==== Tests with generated adjoint ====

struct Parameter : Equatable {
  private let storedX: Float
  @differentiable(reverse, wrt: (self))
  var x: Float {
      return storedX
  }

  init(x: Float) {
    storedX = x
  }

  @derivative(of: x)
  func vjpX() -> (value: Float, pullback: (Float) -> Parameter) {
    return (x, { dx in Parameter(x: dx) } )
  }

  @derivative(of: x)
  func jvpX() -> (value: Float, differential: (Parameter) -> Float) {
    return (x, { $0.x })
  }
}

extension Parameter {
  func squared() -> Float {
    return x * x
  }

  static func squared(p: Parameter) -> Float {
    return p.x * p.x
  }

  func multiplied(with other: Float) -> Float {
    return x * other
  }

  static func * (_ a: Parameter, _ b: Parameter) -> Float {
    return a.x * b.x
  }
}

extension Parameter : Differentiable, AdditiveArithmetic {
  typealias TangentVector = Parameter
  typealias Scalar = Float
  typealias Shape = ()
  init(repeating repeatedValue: Float, shape: ()) {
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

MethodTests.test(
  "instance method with generated adjoint, called from differentiated func"
) {
  func f(_ p: Parameter) -> Float {
    return 100 * p.squared()
  }
  expectEqual(Parameter(x: 4 * 100), gradient(at: Parameter(x: 2), of: f))
  expectEqual(Parameter(x: 40 * 100), gradient(at: Parameter(x: 20), of: f))
}

MethodTests.test(
  "instance method with generated adjoint, differentiated directly"
) {
  // This is our current syntax for taking gradients of instance methods
  // directly. If/when we develop nicer syntax for this, change this test.
  func g(p: Parameter) -> Float { p.squared() }
  expectEqual(Parameter(x: 4), gradient(at: Parameter(x: 2), of: g))
  expectEqual(Parameter(x: 40), gradient(at: Parameter(x: 20), of: g))
}

MethodTests.test("instance method with generated adjoint, wrt only self") {
  func f(_ p: Parameter) -> Float {
    return 100 * p.multiplied(with: 200)
  }
  expectEqual(Parameter(x: 100 * 200), gradient(at: Parameter(x: 1), of: f))
  expectEqual(Parameter(x: 100 * 200), gradient(at: Parameter(x: 2), of: f))
}

MethodTests.test("instance method with generated adjoint, wrt only non-self") {
  func f(_ other: Float) -> Float {
    return 100 * Parameter(x: 200).multiplied(with: other)
  }
  expectEqual(100 * 200, gradient(at: 1, of: f))
  expectEqual(100 * 200, gradient(at: 2, of: f))
}

MethodTests.test(
  "instance method with generated adjoint, wrt self and non-self"
) {
  expectEqual(
    (Parameter(x: 100), 200), gradient(at: Parameter(x: 200), 100) { $0.multiplied(with: $1) })
  expectEqual(
    (Parameter(x: 200), 100), gradient(at: Parameter(x: 100), 200) { $0.multiplied(with: $1) })
}

MethodTests.test(
  "static method with generated adjoint, called from differentiated func"
) {
  func f(_ p: Parameter) -> Float {
    return 100 * Parameter.squared(p: p)
  }
  expectEqual(Parameter(x: 4 * 100), gradient(at: Parameter(x: 2), of: f))
  expectEqual(Parameter(x: 40 * 100), gradient(at: Parameter(x: 20), of: f))
}

MethodTests.test(
  "static method with generated adjoint, differentiated directly"
) {
  expectEqual(Parameter(x: 4), gradient(at: Parameter(x: 2), of: Parameter.squared))
  expectEqual(Parameter(x: 40), gradient(at: Parameter(x: 20), of: Parameter.squared))
}

MethodTests.test("static method with generated adjoint, wrt only first param") {
  func f(_ p: Parameter) -> Float {
    return 100 * (p * Parameter(x: 200))
  }
  expectEqual(Parameter(x: 100 * 200), gradient(at: Parameter(x: 1), of: f))
  expectEqual(Parameter(x: 100 * 200), gradient(at: Parameter(x: 2), of: f))
}

MethodTests.test("static method with generated adjoint, wrt only second param") {
  func f(_ p: Parameter) -> Float {
    return 100 * (Parameter(x: 200) * p)
  }
  expectEqual(Parameter(x: 100 * 200), gradient(at: Parameter(x: 1), of: f))
  expectEqual(Parameter(x: 100 * 200), gradient(at: Parameter(x: 2), of: f))
}

MethodTests.test("static method with generated adjoint, wrt all params") {
  func g(a: Parameter, b: Parameter) -> Float { a * b }
  expectEqual((Parameter(x: 100), Parameter(x: 200)),
              gradient(at: Parameter(x: 200), Parameter(x: 100), of: g))
  expectEqual((Parameter(x: 200), Parameter(x: 100)),
              gradient(at: Parameter(x: 100), Parameter(x: 200), of: g))
}


/* Temporary disabled until https://github.com/swiftlang/swift/issues/84840 is fixed
   We cannot use `Tracked<T>` :(
struct ParameterTracked : Equatable {
  private let storedX: Tracked<Float>
  @differentiable(reverse, wrt: (self))
  var x: Tracked<Float> {
      return storedX
  }

  init(x: Tracked<Float>) {
    storedX = x
  }

  @derivative(of: x)
  func vjpX() -> (value: Tracked<Float>, pullback: (Tracked<Float>) -> ParameterTracked) {
    return (x, { dx in ParameterTracked(x: dx) } )
  }

  @derivative(of: x)
  func jvpX() -> (value: Tracked<Float>, differential: (ParameterTracked) -> Tracked<Float>) {
    return (x, { $0.x })
  }
}

extension ParameterTracked {
  func squared() -> Tracked<Float> {
    return x * x
  }

  static func squared(p: ParameterTracked) -> Tracked<Float> {
    return p.x * p.x
  }

  func multiplied(with other: Tracked<Float>) -> Tracked<Float> {
    return x * other
  }

  static func * (_ a: ParameterTracked, _ b: ParameterTracked) -> Tracked<Float> {
    return a.x * b.x
  }
}

extension ParameterTracked : Differentiable, AdditiveArithmetic {
  typealias TangentVector = ParameterTracked
  typealias Scalar = Tracked<Float>
  typealias Shape = ()
  init(repeating repeatedValue: Tracked<Float>, shape: ()) {
    self.init(x: repeatedValue)
  }
  static func + (lhs: ParameterTracked, rhs: ParameterTracked) -> ParameterTracked {
    return ParameterTracked(x: lhs.x + rhs.x)
  }
  static func - (lhs: ParameterTracked, rhs: ParameterTracked) -> ParameterTracked {
    return ParameterTracked(x: lhs.x - rhs.x)
  }
  static func * (lhs: Scalar, rhs: ParameterTracked) -> ParameterTracked {
    return ParameterTracked(x: lhs * rhs.x)
  }
  static var zero: ParameterTracked { return ParameterTracked(x: 0) }
}

MethodTests.testWithLeakChecking(
  "instance method with generated adjoint, called from differentiated func"
) {
  func f(_ p: ParameterTracked) -> Tracked<Float> {
    return 100 * p.squared()
  }
  expectEqual(ParameterTracked(x: 4 * 100), gradient(at: ParameterTracked(x: 2), of: f))
  expectEqual(ParameterTracked(x: 40 * 100), gradient(at: ParameterTracked(x: 20), of: f))
}

MethodTests.testWithLeakChecking(
  "instance method with generated adjoint, differentiated directly"
) {
  // This is our current syntax for taking gradients of instance methods
  // directly. If/when we develop nicer syntax for this, change this test.
  func g(p: ParameterTracked) -> Tracked<Float> { p.squared() }
  expectEqual(ParameterTracked(x: 4), gradient(at: ParameterTracked(x: 2), of: g))
  expectEqual(ParameterTracked(x: 40), gradient(at: ParameterTracked(x: 20), of: g))
}

MethodTests.testWithLeakChecking("instance method with generated adjoint, wrt only self") {
  func f(_ p: ParameterTracked) -> Tracked<Float> {
    return 100 * p.multiplied(with: 200)
  }
  expectEqual(ParameterTracked(x: 100 * 200), gradient(at: ParameterTracked(x: 1), of: f))
  expectEqual(ParameterTracked(x: 100 * 200), gradient(at: ParameterTracked(x: 2), of: f))
}

MethodTests.testWithLeakChecking("instance method with generated adjoint, wrt only non-self") {
  func f(_ other: Tracked<Float>) -> Tracked<Float> {
    return 100 * ParameterTracked(x: 200).multiplied(with: other)
  }
  expectEqual(100 * 200, gradient(at: 1, of: f))
  expectEqual(100 * 200, gradient(at: 2, of: f))
}

MethodTests.testWithLeakChecking(
  "instance method with generated adjoint, wrt self and non-self"
) {
  expectEqual(
    (ParameterTracked(x: 100), 200), gradient(at: ParameterTracked(x: 200), 100) { $0.multiplied(with: $1) })
  expectEqual(
    (ParameterTracked(x: 200), 100), gradient(at: ParameterTracked(x: 100), 200) { $0.multiplied(with: $1) })
}

MethodTests.testWithLeakChecking(
  "static method with generated adjoint, called from differentiated func"
) {
  func f(_ p: ParameterTracked) -> Tracked<Float> {
    return 100 * ParameterTracked.squared(p: p)
  }
  expectEqual(ParameterTracked(x: 4 * 100), gradient(at: ParameterTracked(x: 2), of: f))
  expectEqual(ParameterTracked(x: 40 * 100), gradient(at: ParameterTracked(x: 20), of: f))
}

MethodTests.testWithLeakChecking(
  "static method with generated adjoint, differentiated directly"
) {
  expectEqual(ParameterTracked(x: 4), gradient(at: ParameterTracked(x: 2), of: ParameterTracked.squared))
  expectEqual(ParameterTracked(x: 40), gradient(at: ParameterTracked(x: 20), of: ParameterTracked.squared))
}

MethodTests.testWithLeakChecking("static method with generated adjoint, wrt only first param") {
  func f(_ p: ParameterTracked) -> Tracked<Float> {
    return 100 * (p * ParameterTracked(x: 200))
  }
  expectEqual(ParameterTracked(x: 100 * 200), gradient(at: ParameterTracked(x: 1), of: f))
  expectEqual(ParameterTracked(x: 100 * 200), gradient(at: ParameterTracked(x: 2), of: f))
}

MethodTests.testWithLeakChecking("static method with generated adjoint, wrt only second param") {
  func f(_ p: ParameterTracked) -> Tracked<Float> {
    return 100 * (ParameterTracked(x: 200) * p)
  }
  expectEqual(ParameterTracked(x: 100 * 200), gradient(at: ParameterTracked(x: 1), of: f))
  expectEqual(ParameterTracked(x: 100 * 200), gradient(at: ParameterTracked(x: 2), of: f))
}

MethodTests.testWithLeakChecking("static method with generated adjoint, wrt all params") {
  func g(a: ParameterTracked, b: ParameterTracked) -> Tracked<Float> { a * b }
  expectEqual((ParameterTracked(x: 100), ParameterTracked(x: 200)),
              gradient(at: ParameterTracked(x: 200), ParameterTracked(x: 100), of: g))
  expectEqual((ParameterTracked(x: 200), ParameterTracked(x: 100)),
              gradient(at: ParameterTracked(x: 100), ParameterTracked(x: 200), of: g))
}
*/

// ==== Tests with custom adjoint ====

// Test self-reordering thunk for jvp/vjp methods.
struct DiffWrtSelf : Differentiable {
  @differentiable(reverse, wrt: (self, x, y))
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
  let storedX: Float
  @differentiable(reverse, wrt: (self))
  var x: Float {
      return storedX
  }

  init(x: Float) {
    storedX = x
  }

  @derivative(of: x)
  func vjpX() -> (value: Float, pullback: (Float) -> CustomParameter) {
    return (x, { dx in CustomParameter(x: dx) })
  }
}

extension CustomParameter : Differentiable, AdditiveArithmetic {
  typealias TangentVector = CustomParameter
  typealias Scalar = Float
  typealias Shape = ()
  init(repeating repeatedValue: Float, shape: ()) {
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

extension Float {
  func clamped(to limits: ClosedRange<Float>) -> Float {
    return min(max(self, limits.lowerBound), limits.upperBound)
  }
}

extension CustomParameter {
  @differentiable(reverse, wrt: (self))
  func squared() -> Float {
    return x * x
  }

  @derivative(of: squared)
  func dSquared() -> (value: Float, pullback: (Float) -> CustomParameter) {
    return (squared(), { [x] v in CustomParameter(x: (2 * x).clamped(to: -10.0...10.0) * v) })
  }

  @differentiable(reverse)
  static func squared(p: CustomParameter) -> Float {
    return p.x * p.x
  }

  @derivative(of: squared)
  static func dSquared(
    _ p: CustomParameter
  ) -> (value: Float, pullback: (Float) -> CustomParameter) {
    return (p.x * p.x, { v in CustomParameter(x: (2 * p.x).clamped(to: -10.0...10.0) * v) })
  }

  // There is currently no way to define multiple custom VJPs wrt different
  // parameters on the same func, so we define a copy of this func per adjoint.

  @differentiable(reverse, wrt: (self, other))
  func multiplied(with other: Float) -> Float {
    return x * other
  }

  @differentiable(reverse, wrt: (other))
  func multiplied_constSelf(with other: Float) -> Float {
    return x * other
  }

  @differentiable(reverse, wrt: (self))
  func multiplied_constOther(with other: Float) -> Float {
    return x * other
  }

  @derivative(of: multiplied)
  func dMultiplied_wrtAll(
    with other: Float
  ) -> (value: Float, pullback: (Float) -> (CustomParameter, Float)) {
    return (multiplied(with: other),
      { [x] v in (CustomParameter(x: other.clamped(to: -10.0...10.0) * v),
                  x.clamped(to: -10.0...10.0) * v) })
  }

  @derivative(of: multiplied_constSelf, wrt: other)
  func dMultiplied_wrtOther(
    with other: Float
  ) -> (value: Float, pullback: (Float) -> Float) {
    let (r, pb) = dMultiplied_wrtAll(with: other)
    return (r, { v in pb(v).1 })
  }

  @derivative(of: multiplied_constOther, wrt: self)
  func dMultiplied_wrtSelf(
    with other: Float
  ) -> (value: Float, pullback: (Float) -> CustomParameter) {
    let (r, pb) = dMultiplied_wrtAll(with: other)
    return (r, { v in pb(v).0 })
  }

  @differentiable(reverse)
  static func multiply(_ lhs: CustomParameter, _ rhs: CustomParameter)
      -> Float {
    return lhs.x * rhs.x
  }

  @differentiable(reverse, wrt: (rhs))
  static func multiply_constLhs(_ lhs: CustomParameter, _ rhs: CustomParameter) -> Float {
    return lhs.x * rhs.x
  }

  @derivative(of: multiply)
  static func dMultiply_wrtAll(_ lhs: CustomParameter,_ rhs: CustomParameter)
      -> (value: Float, pullback: (Float) -> (CustomParameter, CustomParameter)) {
    let result = multiply(lhs, rhs)
    return (result, { v in (CustomParameter(x: rhs.x.clamped(to: -10.0...10.0) * v),
                            CustomParameter(x: lhs.x.clamped(to: -10.0...10.0) * v)) })
  }

  @derivative(of: multiply_constLhs, wrt: rhs)
  static func dMultiply_wrtRhs(_ lhs: CustomParameter, _ rhs: CustomParameter)
      -> (value: Float, pullback: (Float) -> CustomParameter) {
    let (r, pb) = dMultiply_wrtAll(lhs, rhs)
    return (r, { v in pb(v).1 })
  }
}

MethodTests.test(
  "instance method with custom adjoint, called from differentiated func"
) {
  func f(_ p: CustomParameter) -> Float {
    return 100 * p.squared()
  }
  expectEqual(CustomParameter(x: 4 * 100), gradient(at: CustomParameter(x: 2), of: f))
  expectEqual(CustomParameter(x: 10 * 100), gradient(at: CustomParameter(x: 20), of: f))
}

MethodTests.test("instance method with generated adjoint, differentiated directly") {
  // This is our current syntax for taking gradients of instance methods
  // directly. If/when we develop nicer syntax for this, change this test.
  func g(p: CustomParameter) -> Float { p.squared() }
  expectEqual(CustomParameter(x: 4), gradient(at: CustomParameter(x: 2), of: g))
  expectEqual(CustomParameter(x: 10), gradient(at: CustomParameter(x: 20), of: g))
}

MethodTests.test("static method with custom adjoint, called from differentiated func") {
  func f(_ p: CustomParameter) -> Float {
    return 100 * CustomParameter.squared(p: p)
  }
  expectEqual(CustomParameter(x: 4 * 100), gradient(at: CustomParameter(x: 2), of: f))
  expectEqual(CustomParameter(x: 10 * 100), gradient(at: CustomParameter(x: 20), of: f))
}

MethodTests.test("static method with custom adjoint, differentiated directly") {
  expectEqual(
    CustomParameter(x: 4), gradient(at: CustomParameter(x: 2), of: CustomParameter.squared))
  expectEqual(
    CustomParameter(x: 10), gradient(at: CustomParameter(x: 20), of: CustomParameter.squared))
}

MethodTests.test("instance method with custom adjoint, wrt only self") {
  func f(_ p: CustomParameter) -> Float {
    return 100 * p.multiplied_constOther(with: 200)
  }
  expectEqual(CustomParameter(x: 100 * 10), gradient(at: CustomParameter(x: 1), of: f))
  expectEqual(CustomParameter(x: 100 * 10), gradient(at: CustomParameter(x: 2), of: f))
}

MethodTests.test("instance method with custom adjoint, wrt only non-self") {
  func f(_ other: Float) -> Float {
    return 100 * CustomParameter(x: 200).multiplied_constSelf(with: other)
  }
  expectEqual(100 * 10, gradient(at: 1, of: f))
  expectEqual(100 * 10, gradient(at: 2, of: f))
}

MethodTests.test("instance method with custom adjoint, wrt self and non-self") {
  func g(p: CustomParameter, o: Float) -> Float { p.multiplied(with: o) }
  expectEqual((CustomParameter(x: 5), 10), gradient(at: CustomParameter(x: 100), 5, of: g))
  expectEqual((CustomParameter(x: 10), 5), gradient(at: CustomParameter(x: 5), 100, of: g))
}

MethodTests.test("static method with custom adjoint, wrt only lhs") {
  func f(_ p: CustomParameter) -> Float {
    return 100 * CustomParameter.multiply_constLhs(CustomParameter(x: 200), p)
  }
  expectEqual(CustomParameter(x: 100 * 10), gradient(at: CustomParameter(x: 1), of: f))
  expectEqual(CustomParameter(x: 100 * 10), gradient(at: CustomParameter(x: 2), of: f))
}

MethodTests.test("static method with custom adjoint, wrt only rhs") {
  func f(_ p: CustomParameter) -> Float {
    return 100 * CustomParameter.multiply_constLhs(CustomParameter(x: 200), p)
  }
  expectEqual(CustomParameter(x: 100 * 10), gradient(at: CustomParameter(x: 1), of: f))
  expectEqual(CustomParameter(x: 100 * 10), gradient(at: CustomParameter(x: 2), of: f))
}

MethodTests.test("static method with custom adjoint, wrt all") {
  func f(_ a: CustomParameter, _ b: CustomParameter) -> Float {
    return CustomParameter.multiply(a, b)
  }
  expectEqual((CustomParameter(x: 5), CustomParameter(x: 10)),
              gradient(at: CustomParameter(x: 100), CustomParameter(x: 5), of: f))
  expectEqual((CustomParameter(x: 10), CustomParameter(x: 5)),
              gradient(at: CustomParameter(x: 5), CustomParameter(x: 100), of: f))
}

/* Temporary disabled until https://github.com/swiftlang/swift/issues/84840 is fixed
   We cannot use `Tracked<T>` :(
struct CustomParameterTracked : Equatable {
  let storedX: Tracked<Float>
  @differentiable(reverse, wrt: (self))
  var x: Tracked<Float> {
      return storedX
  }

  init(x: Tracked<Float>) {
    storedX = x
  }

  @derivative(of: x)
  func vjpX() -> (value: Tracked<Float>, pullback: (Tracked<Float>) -> CustomParameterTracked) {
    return (x, { dx in CustomParameterTracked(x: dx) })
  }
}

extension CustomParameterTracked : Differentiable, AdditiveArithmetic {
  typealias TangentVector = CustomParameterTracked
  typealias Scalar = Tracked<Float>
  typealias Shape = ()
  init(repeating repeatedValue: Tracked<Float>, shape: ()) {
    self.init(x: repeatedValue)
  }
  static func + (lhs: CustomParameterTracked, rhs: CustomParameterTracked) -> CustomParameterTracked {
    return CustomParameterTracked(x: lhs.x + rhs.x)
  }
  static func - (lhs: CustomParameterTracked, rhs: CustomParameterTracked) -> CustomParameterTracked {
    return CustomParameterTracked(x: lhs.x - rhs.x)
  }
  static func * (lhs: Scalar, rhs: CustomParameterTracked) -> CustomParameterTracked {
    return CustomParameterTracked(x: lhs * rhs.x)
  }
  static var zero: CustomParameterTracked { return CustomParameterTracked(x: 0) }
}

extension Tracked where T : FloatingPoint {
  func clamped(to limits: ClosedRange<Tracked<T>>) -> Tracked<T> {
    return min(max(self, limits.lowerBound), limits.upperBound)
  }
}

extension CustomParameterTracked {
  @differentiable(reverse, wrt: (self))
  func squared() -> Tracked<Float> {
    return x * x
  }

  @derivative(of: squared)
  func dSquared() -> (value: Tracked<Float>, pullback: (Tracked<Float>) -> CustomParameterTracked) {
    return (squared(), { [x] v in CustomParameterTracked(x: (2 * x).clamped(to: -10.0...10.0) * v) })
  }

  @differentiable(reverse)
  static func squared(p: CustomParameterTracked) -> Tracked<Float> {
    return p.x * p.x
  }

  @derivative(of: squared)
  static func dSquared(
    _ p: CustomParameterTracked
  ) -> (value: Tracked<Float>, pullback: (Tracked<Float>) -> CustomParameterTracked) {
    return (p.x * p.x, { v in CustomParameterTracked(x: (2 * p.x).clamped(to: -10.0...10.0) * v) })
  }

  // There is currently no way to define multiple custom VJPs wrt different
  // parameters on the same func, so we define a copy of this func per adjoint.

  @differentiable(reverse, wrt: (self, other))
  func multiplied(with other: Tracked<Float>) -> Tracked<Float> {
    return x * other
  }

  @differentiable(reverse, wrt: (other))
  func multiplied_constSelf(with other: Tracked<Float>) -> Tracked<Float> {
    return x * other
  }

  @differentiable(reverse, wrt: (self))
  func multiplied_constOther(with other: Tracked<Float>) -> Tracked<Float> {
    return x * other
  }

  @derivative(of: multiplied)
  func dMultiplied_wrtAll(
    with other: Tracked<Float>
  ) -> (value: Tracked<Float>, pullback: (Tracked<Float>) -> (CustomParameterTracked, Tracked<Float>)) {
    return (multiplied(with: other),
      { [x] v in (CustomParameterTracked(x: other.clamped(to: -10.0...10.0) * v),
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
  ) -> (value: Tracked<Float>, pullback: (Tracked<Float>) -> CustomParameterTracked) {
    let (r, pb) = dMultiplied_wrtAll(with: other)
    return (r, { v in pb(v).0 })
  }

  @differentiable(reverse)
  static func multiply(_ lhs: CustomParameterTracked, _ rhs: CustomParameterTracked)
      -> Tracked<Float> {
    return lhs.x * rhs.x
  }

  @differentiable(reverse, wrt: (rhs))
  static func multiply_constLhs(_ lhs: CustomParameterTracked, _ rhs: CustomParameterTracked) -> Tracked<Float> {
    return lhs.x * rhs.x
  }

  @derivative(of: multiply)
  static func dMultiply_wrtAll(_ lhs: CustomParameterTracked,_ rhs: CustomParameterTracked)
      -> (value: Tracked<Float>, pullback: (Tracked<Float>) -> (CustomParameterTracked, CustomParameterTracked)) {
    let result = multiply(lhs, rhs)
    return (result, { v in (CustomParameterTracked(x: rhs.x.clamped(to: -10.0...10.0) * v),
                            CustomParameterTracked(x: lhs.x.clamped(to: -10.0...10.0) * v)) })
  }

  @derivative(of: multiply_constLhs, wrt: rhs)
  static func dMultiply_wrtRhs(_ lhs: CustomParameterTracked, _ rhs: CustomParameterTracked)
      -> (value: Tracked<Float>, pullback: (Tracked<Float>) -> CustomParameterTracked) {
    let (r, pb) = dMultiply_wrtAll(lhs, rhs)
    return (r, { v in pb(v).1 })
  }
}

MethodTests.testWithLeakChecking(
  "instance method with custom adjoint, called from differentiated func"
) {
  func f(_ p: CustomParameterTracked) -> Tracked<Float> {
    return 100 * p.squared()
  }
  expectEqual(CustomParameterTracked(x: 4 * 100), gradient(at: CustomParameterTracked(x: 2), of: f))
  expectEqual(CustomParameterTracked(x: 10 * 100), gradient(at: CustomParameterTracked(x: 20), of: f))
}

MethodTests.testWithLeakChecking("instance method with generated adjoint, differentiated directly") {
  // This is our current syntax for taking gradients of instance methods
  // directly. If/when we develop nicer syntax for this, change this test.
  func g(p: CustomParameterTracked) -> Tracked<Float> { p.squared() }
  expectEqual(CustomParameterTracked(x: 4), gradient(at: CustomParameterTracked(x: 2), of: g))
  expectEqual(CustomParameterTracked(x: 10), gradient(at: CustomParameterTracked(x: 20), of: g))
}

MethodTests.testWithLeakChecking("static method with custom adjoint, called from differentiated func") {
  func f(_ p: CustomParameterTracked) -> Tracked<Float> {
    return 100 * CustomParameterTracked.squared(p: p)
  }
  expectEqual(CustomParameterTracked(x: 4 * 100), gradient(at: CustomParameterTracked(x: 2), of: f))
  expectEqual(CustomParameterTracked(x: 10 * 100), gradient(at: CustomParameterTracked(x: 20), of: f))
}

MethodTests.testWithLeakChecking("static method with custom adjoint, differentiated directly") {
  expectEqual(
    CustomParameterTracked(x: 4), gradient(at: CustomParameterTracked(x: 2), of: CustomParameterTracked.squared))
  expectEqual(
    CustomParameterTracked(x: 10), gradient(at: CustomParameterTracked(x: 20), of: CustomParameterTracked.squared))
}

MethodTests.testWithLeakChecking("instance method with custom adjoint, wrt only self") {
  func f(_ p: CustomParameterTracked) -> Tracked<Float> {
    return 100 * p.multiplied_constOther(with: 200)
  }
  expectEqual(CustomParameterTracked(x: 100 * 10), gradient(at: CustomParameterTracked(x: 1), of: f))
  expectEqual(CustomParameterTracked(x: 100 * 10), gradient(at: CustomParameterTracked(x: 2), of: f))
}

MethodTests.testWithLeakChecking("instance method with custom adjoint, wrt only non-self") {
  func f(_ other: Tracked<Float>) -> Tracked<Float> {
    return 100 * CustomParameterTracked(x: 200).multiplied_constSelf(with: other)
  }
  expectEqual(100 * 10, gradient(at: 1, of: f))
  expectEqual(100 * 10, gradient(at: 2, of: f))
}

MethodTests.testWithLeakChecking("instance method with custom adjoint, wrt self and non-self") {
  func g(p: CustomParameterTracked, o: Tracked<Float>) -> Tracked<Float> { p.multiplied(with: o) }
  expectEqual((CustomParameterTracked(x: 5), 10), gradient(at: CustomParameterTracked(x: 100), 5, of: g))
  expectEqual((CustomParameterTracked(x: 10), 5), gradient(at: CustomParameterTracked(x: 5), 100, of: g))
}

MethodTests.testWithLeakChecking("static method with custom adjoint, wrt only lhs") {
  func f(_ p: CustomParameterTracked) -> Tracked<Float> {
    return 100 * CustomParameterTracked.multiply_constLhs(CustomParameterTracked(x: 200), p)
  }
  expectEqual(CustomParameterTracked(x: 100 * 10), gradient(at: CustomParameterTracked(x: 1), of: f))
  expectEqual(CustomParameterTracked(x: 100 * 10), gradient(at: CustomParameterTracked(x: 2), of: f))
}

MethodTests.testWithLeakChecking("static method with custom adjoint, wrt only rhs") {
  func f(_ p: CustomParameterTracked) -> Tracked<Float> {
    return 100 * CustomParameterTracked.multiply_constLhs(CustomParameterTracked(x: 200), p)
  }
  expectEqual(CustomParameterTracked(x: 100 * 10), gradient(at: CustomParameterTracked(x: 1), of: f))
  expectEqual(CustomParameterTracked(x: 100 * 10), gradient(at: CustomParameterTracked(x: 2), of: f))
}

MethodTests.testWithLeakChecking("static method with custom adjoint, wrt all") {
  func f(_ a: CustomParameterTracked, _ b: CustomParameterTracked) -> Tracked<Float> {
    return CustomParameterTracked.multiply(a, b)
  }
  expectEqual((CustomParameterTracked(x: 5), CustomParameterTracked(x: 10)),
              gradient(at: CustomParameterTracked(x: 100), CustomParameterTracked(x: 5), of: f))
  expectEqual((CustomParameterTracked(x: 10), CustomParameterTracked(x: 5)),
              gradient(at: CustomParameterTracked(x: 5), CustomParameterTracked(x: 100), of: f))
}
*/

runAllTests()
