// RUN: %target-swift-frontend -typecheck -verify %s

// Test top-level functions.

// expected-note @+1 {{'sin' defined here}}
func sin(_ x: Float) -> Float {
  return x // dummy implementation
}
@derivative(of: sin) // ok
func jvpSin(x: @noDerivative Float) -> (value: Float, differential: (Float) -> (Float)) {
  return (x, { $0 })
}
// expected-note @+1 {{other attribute declared here}}
@derivative(of: sin, wrt: x) // ok
func vjpSinExplicitWrt(x: Float) -> (value: Float, pullback: (Float) -> Float) {
  return (x, { $0 })
}

// expected-error @+1 {{a derivative already exists for 'sin'}}
@derivative(of: sin)
func vjpDuplicate(x: Float) -> (value: Float, pullback: (Float) -> Float) {
  return (x, { $0 })
}
// expected-error @+1 {{'@derivative(of:)' attribute requires function to return a two-element tuple of type '(value: T..., pullback: (U.TangentVector) -> T.TangentVector...)' or '(value: T..., differential: (T.TangentVector...) -> U.TangentVector)'}}
@derivative(of: sin)
func jvpSinResultInvalid(x: @noDerivative Float) -> Float {
  return x
}
// expected-error @+1 {{'@derivative(of:)' attribute requires function to return a two-element tuple (second element must have label 'pullback:' or 'differential:')}}
@derivative(of: sin)
func vjpSinResultWrongLabel(x: Float) -> (value: Float, (Float) -> Float) {
  return (x, { $0 })
}
// expected-error @+1 {{could not find function 'sin' with expected type '(Int) -> Int'}}
@derivative(of: sin)
func vjpSinResultNotDifferentiable(x: Int) -> (value: Int, pullback: (Int) -> Int) {
  return (x, { $0 })
}
// expected-error @+2 {{function result's 'pullback' type does not match 'sin'}}
// expected-note @+2 {{'pullback' does not have expected type '(Float.TangentVector) -> Float.TangentVector' (aka '(Float) -> Float')}}
@derivative(of: sin)
func vjpSinResultInvalidSeedType(x: Float) -> (value: Float, pullback: (Double) -> Double) {
  return (x, { $0 })
}

func generic<T : Differentiable>(_ x: T, _ y: T) -> T {
  return x
}
@derivative(of: generic) // ok
func jvpGeneric<T : Differentiable>(x: T, y: T) -> (value: T, differential: (T.TangentVector, T.TangentVector) -> T.TangentVector) {
  return (x, { $0 + $1 })
}
// expected-error @+1 {{'@derivative(of:)' attribute requires function to return a two-element tuple (second element must have label 'pullback:' or 'differential:')}}
@derivative(of: generic)
func vjpGenericWrongLabel<T : Differentiable>(x: T, y: T) -> (value: T, (T) -> (T, T)) {
  return (x, { ($0, $0) })
}
// expected-error @+1 {{could not find function 'generic' with expected type '<T where T : Differentiable, T == T.TangentVector> (x: T) -> T'}}
@derivative(of: generic)
func vjpGenericDiffParamMismatch<T : Differentiable>(x: T) -> (value: T, pullback: (T) -> (T, T)) where T == T.TangentVector {
  return (x, { ($0, $0) })
}
@derivative(of: generic) // ok
func vjpGenericExtraGenericRequirements<T : Differentiable & FloatingPoint>(x: T, y: T) -> (value: T, pullback: (T) -> (T, T)) where T == T.TangentVector {
  return (x, { ($0, $0) })
}

// Test ambiguous original declaration.

protocol P1 {}
protocol P2 {}
func ambiguous<T: P1>(_ x: T) -> T { x }
func ambiguous<T: P2>(_ x: T) -> T { x }

// expected-error @+1 {{ambiguous reference to 'ambiguous' in '@derivative' attribute}}
@derivative(of: ambiguous)
func jvpAmbiguous<T: P1 & P2 & Differentiable>(x: T)
  -> (value: T, differential: (T.TangentVector) -> (T.TangentVector)) {
  return (x, { $0 })
}

// Test no valid original declaration.
// Original declarations are invalid because they have extra generic
// requirements unsatisfied by the `@derivative` function.

func invalid<T: BinaryFloatingPoint>(x: T) -> T { x }
func invalid<T: CustomStringConvertible>(x: T) -> T { x }
func invalid<T: FloatingPoint>(x: T) -> T { x }

// expected-error @+1 {{could not find function 'invalid' with expected type '<T where T : Differentiable> (x: T) -> T'}}
@derivative(of: invalid)
func jvpInvalid<T: Differentiable>(x: T) -> (value: T, differential: (T.TangentVector) -> T.TangentVector) {
  return (x, { $0 })
}

// Test `wrt` clauses.

func add(x: Float, y: Float) -> Float {
  return x + y
}
@derivative(of: add, wrt: x) // ok
func vjpAddWrtX(x: Float, y: Float) -> (value: Float, pullback: (Float) -> (Float)) {
  return (x + y, { $0 })
}
@derivative(of: add, wrt: (x, y)) // ok
func vjpAddWrtXY(x: Float, y: Float) -> (value: Float, pullback: (Float) -> (Float, Float)) {
  return (x + y, { ($0, $0) })
}
// expected-error @+1 {{unknown parameter name 'z'}}
@derivative(of: add, wrt: z)
func vjpUnknownParam(x: Float, y: Float) -> (value: Float, pullback: (Float) -> (Float)) {
  return (x + y, { $0 })
}
// expected-error @+1 {{parameters must be specified in original order}}
@derivative(of: add, wrt: (y, x))
func vjpParamOrderNotIncreasing(x: Float, y: Float) -> (value: Float, pullback: (Float) -> (Float, Float)) {
  return (x + y, { ($0, $0) })
}
// expected-error @+1 {{'self' parameter is only applicable to instance methods}}
@derivative(of: add, wrt: self)
func vjpInvalidSelfParam(x: Float, y: Float) -> (value: Float, pullback: (Float) -> (Float, Float)) {
  return (x + y, { ($0, $0) })
}

func noParams() -> Float {
  return 1
}
// expected-error @+1 {{'vjpNoParams()' has no parameters to differentiate with respect to}}
@derivative(of: noParams)
func vjpNoParams() -> (value: Float, pullback: (Float) -> Float) {
  return (1, { $0 })
}

func noDiffParams(x: Int) -> Float {
  return 1
}
// expected-error @+1 {{no differentiation parameters could be inferred; must differentiate with respect to at least one parameter conforming to 'Differentiable'}}
@derivative(of: noDiffParams)
func vjpNoDiffParams(x: Int) -> (value: Float, pullback: (Float) -> Int) {
  return (1, { _ in 0 })
}

func foo<T : FloatingPoint & Differentiable>(_ x: T) -> T { return x }
// expected-error @+1 {{could not find function 'foo' with expected type '<T where T : AdditiveArithmetic, T : Differentiable> (T) -> T'}}
@derivative(of: foo)
func vjpFoo<T : AdditiveArithmetic & Differentiable>(_ x: T) -> (value: T, pullback: (T.TangentVector) -> (T.TangentVector)) {
  return (x, { $0 })
}
@derivative(of: foo)
func vjpFooExtraGenericRequirements<T : FloatingPoint & Differentiable & BinaryInteger>(_ x: T) -> (value: T, pullback: (T) -> (T)) where T == T.TangentVector {
  return (x, { $0 })
}

// Test cross-file derivative registration. Currently unsupported.
// TODO(TF-1021): Lift this restriction.
extension FloatingPoint where Self: Differentiable {
  // expected-error @+1 {{derivative not in the same file as the original function}}
  @derivative(of: rounded)
  func vjpRounded() -> (value: Self, pullback: (TangentVector) -> TangentVector) {
    fatalError()
  }
}

// Test static methods.

extension Differentiable where Self : AdditiveArithmetic {
  // expected-error @+1 {{'+' is not defined in the current type context}}
  @derivative(of: +)
  static func vjpPlus(x: Self, y: Self) -> (value: Self, pullback: (Self.TangentVector) -> (Self.TangentVector, Self.TangentVector)) {
    return (x + y, { v in (v, v) })
  }
}

extension AdditiveArithmetic where Self : Differentiable, Self == Self.TangentVector {
  // expected-error @+1 {{could not find function '+' with expected type '<Self where Self : Differentiable, Self == Self.TangentVector> (Self) -> (Self, Self) -> Self'}}
  @derivative(of: +)
  func vjpPlusInstanceMethod(x: Self, y: Self) -> (value: Self, pullback: (Self) -> (Self, Self)) {
    return (x + y, { v in (v, v) })
  }
}

// Test instance methods.

protocol InstanceMethod : Differentiable {
  func foo(_ x: Self) -> Self
  func foo2(_ x: Self) -> Self
  func bar<T : Differentiable>(_ x: T) -> Self
  func bar2<T : Differentiable>(_ x: T) -> Self
}

extension InstanceMethod {
  // expected-note @+1 {{'foo' defined here}}
  func foo(_ x: Self) -> Self { self }
  func foo2(_ x: Self) -> Self { self }
  // expected-note @+1 {{'bar' defined here}}
  func bar<T : Differentiable>(_ x: T) -> Self { self }
  func bar2<T : Differentiable>(_ x: T) -> Self { self}
}

extension InstanceMethod {
  // If `Self` conforms to `Differentiable`, then `Self` is currently always inferred to be a differentiation parameter.
  // expected-error @+2 {{function result's 'pullback' type does not match 'foo'}}
  // expected-note @+2 {{'pullback' does not have expected type '(Self.TangentVector) -> (Self.TangentVector, Self.TangentVector)'}}
  @derivative(of: foo)
  func vjpFoo(x: Self) -> (value: Self, pullback: (TangentVector) -> TangentVector) {
    return (x, { $0 })
  }

  @derivative(of: foo)
  func jvpFoo(x: Self) -> (value: Self, differential: (TangentVector, TangentVector) -> (TangentVector)) {
    return (x, { $0 + $1 })
  }

  @derivative(of: foo, wrt: (self, x))
  func vjpFooWrt(x: Self) -> (value: Self, pullback: (TangentVector) -> (TangentVector, TangentVector)) {
    return (x, { ($0, $0) })
  }
}

extension InstanceMethod {
  // expected-error @+2 {{function result's 'pullback' type does not match 'bar'}}
  // expected-note @+2 {{'pullback' does not have expected type '(Self.TangentVector) -> (Self.TangentVector, T.TangentVector)'}}
  @derivative(of: bar)
  func vjpBar<T : Differentiable>(_ x: T) -> (value: Self, pullback: (TangentVector) -> T.TangentVector) {
    return (self, { _ in .zero })
  }

  @derivative(of: bar)
  func vjpBar<T : Differentiable>(_ x: T) -> (value: Self, pullback: (TangentVector) -> (TangentVector, T.TangentVector)) {
    return (self, { ($0, .zero) })
  }

  @derivative(of: bar, wrt: (self, x))
  func jvpBarWrt<T : Differentiable>(_ x: T) -> (value: Self, differential: (TangentVector, T.TangentVector) -> TangentVector) {
    return (self, { dself, dx in dself })
  }
}

extension InstanceMethod where Self == Self.TangentVector {
  @derivative(of: foo2)
  func vjpFooExtraRequirements(x: Self) -> (value: Self, pullback: (Self) -> (Self, Self)) {
    return (x, { ($0, $0) })
  }

  @derivative(of: foo2)
  func jvpFooExtraRequirements(x: Self) -> (value: Self, differential: (Self, Self) -> (Self)) {
    return (x, { $0 + $1 })
  }

  @derivative(of: bar2)
  func vjpBarExtraRequirements<T : Differentiable>(x: T) -> (value: Self, pullback: (Self) -> (Self, T.TangentVector)) {
    return (self, { ($0, .zero) })
  }

  @derivative(of: bar2)
  func jvpBarExtraRequirements<T : Differentiable>(_ x: T) -> (value: Self, differential: (Self, T.TangentVector) -> Self) {
    return (self, { dself, dx in dself })
  }
}

protocol GenericInstanceMethod : Differentiable where Self == Self.TangentVector {
  func instanceMethod<T : Differentiable>(_ x: T) -> T
}

extension GenericInstanceMethod {
  func instanceMethod<T : Differentiable>(_ x: T) -> T { x }
}

extension GenericInstanceMethod {
  @derivative(of: instanceMethod)
  func jvpInstanceMethod<T : Differentiable>(_ x: T) -> (value: T, differential: (Self, T.TangentVector) -> (T.TangentVector)) {
    return (x, { (dself, dx) in dx })
  }

  @derivative(of: instanceMethod)
  func vjpInstanceMethod<T : Differentiable>(_ x: T) -> (value: T, pullback: (T.TangentVector) -> (Self, T.TangentVector)) {
    return (x, { v in (self, v) })
  }
}

// Test extra generic constraints.

func bar<T>(_ x: T) -> T {
  return x
}
@derivative(of: bar)
func vjpBar<T : Differentiable>(_ x: T) -> (value: T, pullback: (T.TangentVector) -> T.TangentVector) {
  return (x, { $0 })
}

func baz<T, U>(_ x: T, _ y: U) -> T {
  return x
}
@derivative(of: baz)
func vjpBaz<T : Differentiable, U : Differentiable>(_ x: T, _ y: U)
    -> (value: T, pullback: (T) -> (T, U))
  where T == T.TangentVector, U == U.TangentVector
{
  return (x, { ($0, .zero) })
}

protocol InstanceMethodProto {
  func bar() -> Float
}
extension InstanceMethodProto {
  func bar() -> Float { 0 }
}
extension InstanceMethodProto where Self : Differentiable {
  @derivative(of: bar)
  func vjpBar() -> (value: Float, pullback: (Float) -> TangentVector) {
    return (bar(), { _ in .zero })
  }
}

//===----------------------------------------------------------------------===//
// `@differentiable` and `@derivative` interactions
//===----------------------------------------------------------------------===//

// Test "overlapping" usages of `@differentiable` and `@derivative`, where both
// have the same original declaration and parameter indices.

protocol Protocol: Differentiable {
  func requirementOnlyDerivativeAttr() -> Self

  @differentiable
  func requirementOverlapping() -> Self
}
extension Protocol {
  @differentiable
  func requirementOverlapping() -> Self { self }

  func nonRequirementOnlyDerivativeAttr() -> Self { self }

  @differentiable
  func nonRequirementOverlapping() -> Self { self }

  @differentiable
  func nonRequirementOverlappingDerivativeGenSig() -> Self { self }
}
extension Protocol {
  @derivative(of: nonRequirementOnlyDerivativeAttr)
  func vjpRequirementOnlyDerivativeAttr()
    -> (value: Self, pullback: (TangentVector) -> (TangentVector)) {
    return (self, { $0 })
  }
}

// Test overlapping attributes for protocol requirement.
// Valid: `@differentiable` attribute on protocol requirement umambiguously
// means "require all implementations to have same `@differentiable`
// attribute".
extension Protocol {
  @derivative(of: requirementOverlapping)
  func vjpRequirementOverlapping()
    -> (value: Self, pullback: (TangentVector) -> (TangentVector)) {
    return (self, { $0 })
  }
}
// Test overlapping `@derivative` attribute with same derivative generic
// signature as the `@differentiable` attribute.
extension Protocol {
  @derivative(of: nonRequirementOverlapping)
  func vjpNonRequirementOverlapping()
    -> (value: Self, pullback: (TangentVector) -> (TangentVector)) {
    return (self, { $0 })
  }
}
// Test overlapping `@derivative` attribute with different derivative generic
// signature from the `@differentiable` attribute.
extension Protocol where Self: AdditiveArithmetic {
  @derivative(of: nonRequirementOverlappingDerivativeGenSig)
  func vjpNonRequirementOverlappingDerivativeGenSig()
    -> (value: Self, pullback: (TangentVector) -> (TangentVector)) {
    return (self, { $0 })
  }
}

// Test class methods.
class Class: Differentiable {
  func classMethodOnlyDerivativeAttr() -> Float { 1 }

  @differentiable
  func classMethodOverlapping() -> Float { 1 }
}
extension Class {
  @derivative(of: classMethodOnlyDerivativeAttr)
  func vjpClassMethodOnlyDerivativeAttr()
     -> (value: Float, pullback: (Float) -> (TangentVector)) {
    return (1, { _ in .init() })
  }

  @derivative(of: classMethodOverlapping)
  func vjpClassMethodOverlapping()
    -> (value: Float, pullback: (Float) -> (TangentVector)) {
    return (1, { _ in .init() })
  }
}

// Old: test consistent usages of `@differentiable` and `@derivative` where
// both attributes register the same derivatives. This was previously valid
// but is now rejected.

// expected-warning @+1 2 {{'jvp:' and 'vjp:' arguments in '@differentiable' attribute are deprecated; use '@derivative' attribute for derivative registration instead}}
@differentiable(jvp: jvpConsistent, vjp: vjpConsistent)
func consistentSpecifiedDerivatives(_ x: Float) -> Float {
  return x
}
@derivative(of: consistentSpecifiedDerivatives)
func jvpConsistent(_ x: Float) -> (value: Float, differential: (Float) -> Float) {
  return (x, { $0 })
}
@derivative(of: consistentSpecifiedDerivatives(_:))
func vjpConsistent(_ x: Float) -> (value: Float, pullback: (Float) -> Float) {
  return (x, { $0 })
}

// Index based 'wrt:'

func add2(x: Float, y: Float) -> Float {
  return x + y
}

@derivative(of: add2, wrt: (0, y)) // ok
func two3(x: Float, y: Float) -> (value: Float, pullback: (Float) -> (Float, Float)) {
  return (x + y, { ($0, $0) })
}

@derivative(of: add2, wrt: (1)) // ok
func two4(x: Float, y: Float) -> (value: Float, pullback: (Float) -> Float) {
  return (x + y, { $0 })
}


@derivative(of: add2, wrt: 2) // expected-error {{parameter index is larger than total number of parameters}}
func two5(x: Float, y: Float) -> (value: Float, pullback: (Float) -> (Float, Float)) {
  return (x + y, { ($0, $0) })
}

@derivative(of: add2, wrt: (1, x)) // expected-error {{parameters must be specified in original order}}
func two6(x: Float, y: Float) -> (value: Float, pullback: (Float) -> (Float, Float)) {
  return (x + y, { ($0, $0) })
}

@derivative(of: add2, wrt: (1, 0)) // expected-error {{parameters must be specified in original order}}
func two7(x: Float, y: Float) -> (value: Float, pullback: (Float) -> (Float, Float)) {
  return (x + y, { ($0, $0) })
}

// Test class methods.

class Super: Differentiable {
  var float: Float

  init(_ float: Float) {
    self.float = float
  }

  // expected-error @+1 {{cannot register derivative for 'init' in a non-final class; consider making 'Super' final}}
  @derivative(of: init)
  static func vjpInit(_ float: Float) -> (value: Super, pullback: (TangentVector) -> Float) {
    return (Super(float), { v in v.float })
  }

  @differentiable
  func foo(_ x: Float) -> Float {
    return x
  }

  @derivative(of: foo, wrt: x)
  func vjpFoo(_ x: Float) -> (value: Float, pullback: (Float) -> Float) {
    return (foo(x), { v in v })
  }
}

final class Sub : Super {
  override init(_ float: Float) {
    self.float = float
  }

  @derivative(of: init)
  static func vjpSubInit(_ float: Float) -> (value: Sub, pullback: (TangentVector) -> Float) {
    return (Sub(float), { v in v.float })
  }

  // TODO(TF-649): Enable `@derivative` to override original functions from superclass.
  // expected-error @+1 {{'foo' is not defined in the current type context}}
  @derivative(of: foo, wrt: x)
  override func vjpFoo(_ x: Float) -> (value: Float, pullback: (Float) -> Float) {
    return (foo(x), { v in v })
  }
}

// Test non-`func` original declarations.

struct Struct<T> {
  var x: T
}
extension Struct: Equatable where T: Equatable {}
extension Struct: Differentiable & AdditiveArithmetic
where T: Differentiable & AdditiveArithmetic {}

// Test computed properties.
extension Struct {
  var computedProperty: T { x }
}
extension Struct where T: Differentiable & AdditiveArithmetic {
  @derivative(of: computedProperty)
  func vjpProperty() -> (value: T, pullback: (T.TangentVector) -> TangentVector) {
    (x, { v in .init(x: v) })
  }
}

// Test initializers.
extension Struct {
  init(_ x: Float) {}
  init(_ x: T, y: Float) {}
}
extension Struct where T: Differentiable & AdditiveArithmetic {
  @derivative(of: init)
  static func vjpInit(_ x: Float) -> (value: Struct, pullback: (TangentVector) -> Float) {
    (.init(x), { _ in .zero })
  }

  @derivative(of: init(_:y:))
  static func vjpInit2(_ x: T, _ y: Float) -> (value: Struct, pullback: (TangentVector) -> (T.TangentVector, Float)) {
    (.init(x, y: y), { _ in (.zero, .zero) })
  }
}

// Test subscripts.
extension Struct {
  subscript() -> Float {
    get { 1 }
    set {}
  }
  subscript(float float: Float) -> Float { 1 }
  subscript<T: Differentiable>(x: T) -> T { x }
}

extension Struct where T: Differentiable & AdditiveArithmetic {
  @derivative(of: subscript)
  func vjpSubscript() -> (value: Float, pullback: (Float) -> TangentVector) {
    (1, { _ in .zero })
  }

  @derivative(of: subscript(float:), wrt: self)
  func vjpSubscriptLabelled(float: Float) -> (value: Float, pullback: (Float) -> TangentVector) {
    (1, { _ in .zero })
  }

  @derivative(of: subscript(_:), wrt: self)
  func vjpSubscriptGeneric<T: Differentiable>(x: T) -> (value: T, pullback: (T.TangentVector) -> TangentVector) {
    (x, { _ in .zero })
  }
}

// Check that `@derivative` attribute rejects stored property original declarations.

struct StoredProperty: Differentiable {
  // expected-note @+1 {{'stored' declared here}}
  var stored: Float
  // expected-error @+1 {{cannot register derivative for stored property 'stored'}}
  @derivative(of: stored)
  func vjpStored() -> (value: Float, pullback: (Float) -> TangentVector) {
    (stored, { _ in .zero })
  }
}

// When the generic signature was not considered while calculating the actual pullback type, the
// typechecker did not realize that `T.TangentVector == Float`, and therefore it complained that
// "'pullback' does not have expected type '(Float) -> (Float)'".
// Users were able to work around this by setting the pullback type to `(Float) -> Float`.
func genericSignatureConsidered<T>(_ x: T) -> T { fatalError() }
@derivative(of: genericSignatureConsidered)
func dGenericSignatureConsidered<T>(_ x: T)
  -> (value: T, pullback: (T.TangentVector) -> T.TangentVector)
  where T: Differentiable, T.TangentVector == Float
{
  fatalError()
}

// When the generic signature was not considered while calculating the actual pullback type,
// the typechecker complained that the pullback type was not correct, and there was no pullback type
// that users could specify to satisfy the typechecker.
struct Wrapper<T: AdditiveArithmetic & Equatable>: AdditiveArithmetic, Equatable {
  var t: T
  init(_ t: T) { self.t = t }
}
extension Wrapper: Differentiable where T: Differentiable, T == T.TangentVector {
  typealias TangentVector = Wrapper<T.TangentVector>
}
extension Wrapper where T: Differentiable, T == T.TangentVector {
  @derivative(of: init(_:))
  static func dInit(_ t: T) -> (value: Self, pullback: (Wrapper<T>.TangentVector) -> (T)) {
    fatalError()
  }
}
