// RUN: %target-swift-frontend -typecheck -verify %s

// Test top-level functions.

// expected-note @+1 {{'sin' defined here}}
func sin(_ x: Float) -> Float {
  return x // dummy implementation
}
@differentiating(sin) // ok
func jvpSin(x: @nondiff Float) -> (value: Float, differential: (Float) -> (Float)) {
  return (x, { $0 })
}
@differentiating(sin, wrt: x) // ok
func vjpSinExplicitWrt(x: Float) -> (value: Float, pullback: (Float) -> Float) {
  return (x, { $0 })
}

// expected-error @+1 {{a derivative already exists for 'sin'}}
@differentiating(sin)
func vjpDuplicate(x: Float) -> (value: Float, pullback: (Float) -> Float) {
  return (x, { $0 })
}
// expected-error @+1 {{'@differentiating' attribute requires function to return a two-element tuple of type '(value: T..., pullback: (U.CotangentVector) -> T.CotangentVector...)' or '(value: T..., differential: (T.TangentVector...) -> U.TangentVector)'}}
@differentiating(sin)
func jvpSinResultInvalid(x: @nondiff Float) -> Float {
  return x
}
// expected-error @+1 {{'@differentiating' attribute requires function to return a two-element tuple (second element must have label 'pullback:' or 'differential:')}}
@differentiating(sin)
func vjpSinResultWrongLabel(x: Float) -> (value: Float, (Float) -> Float) {
  return (x, { $0 })
}
// expected-error @+1 {{'@differentiating' attribute requires function to return a two-element tuple (first element type 'Int' must conform to 'Differentiable')}}
@differentiating(sin)
func vjpSinResultNotDifferentiable(x: Int) -> (value: Int, pullback: (Int) -> Int) {
  return (x, { $0 })
}
// expected-error @+2 {{function result's 'pullback' type does not match 'sin'}}
// expected-note @+2 {{'pullback' does not have expected type '(Float.CotangentVector) -> (Float.CotangentVector)' (aka '(Float) -> Float')}}
@differentiating(sin)
func vjpSinResultInvalidSeedType(x: Float) -> (value: Float, pullback: (Double) -> Double) {
  return (x, { $0 })
}

func generic<T : Differentiable>(_ x: T, _ y: T) -> T {
  return x
}
@differentiating(generic) // ok
func jvpGeneric<T : Differentiable>(x: T, y: T) -> (value: T, differential: (T.TangentVector, T.TangentVector) -> T.TangentVector) {
  return (x, { $0 + $1 })
}
// expected-error @+1 {{'@differentiating' attribute requires function to return a two-element tuple (second element must have label 'pullback:' or 'differential:')}}
@differentiating(generic)
func vjpGenericWrongLabel<T : Differentiable>(x: T, y: T) -> (value: T, (T) -> (T, T)) {
  return (x, { ($0, $0) })
}
// expected-error @+1 {{could not find function 'generic' with expected type '<T where T : Differentiable, T == T.CotangentVector> (x: T) -> T'}}
@differentiating(generic)
func vjpGenericDiffParamMismatch<T : Differentiable>(x: T) -> (value: T, pullback: (T) -> (T, T)) where T == T.CotangentVector {
  return (x, { ($0, $0) })
}
@differentiating(generic) // ok
func vjpGenericExtraGenericRequirements<T : Differentiable & FloatingPoint>(x: T, y: T) -> (value: T, pullback: (T) -> (T, T)) where T == T.CotangentVector {
  return (x, { ($0, $0) })
}

func foo<T : FloatingPoint & Differentiable>(_ x: T) -> T { return x }

// Test `wrt` clauses.

func add(x: Float, y: Float) -> Float {
  return x + y
}
@differentiating(add, wrt: x) // ok
func vjpAddWrtX(x: Float, y: Float) -> (value: Float, pullback: (Float) -> (Float)) {
  return (x + y, { $0 })
}
@differentiating(add, wrt: (x, y)) // ok
func vjpAddWrtXY(x: Float, y: Float) -> (value: Float, pullback: (Float) -> (Float, Float)) {
  return (x + y, { ($0, $0) })
}
// expected-error @+1 {{unknown parameter name 'z'}}
@differentiating(add, wrt: z)
func vjpUnknownParam(x: Float, y: Float) -> (value: Float, pullback: (Float) -> (Float)) {
  return (x + y, { $0 })
}
// expected-error @+1 {{parameter names must be specified in original order}}
@differentiating(add, wrt: (y, x))
func vjpParamOrderNotIncreasing(x: Float, y: Float) -> (value: Float, pullback: (Float) -> (Float, Float)) {
  return (x + y, { ($0, $0) })
}
// expected-error @+1 {{'self' parameter is only applicable to instance methods}}
@differentiating(add, wrt: self)
func vjpInvalidSelfParam(x: Float, y: Float) -> (value: Float, pullback: (Float) -> (Float, Float)) {
  return (x + y, { ($0, $0) })
}

func noParams() -> Float {
  return 1
}
// expected-error @+1 {{'vjpNoParams()' has no parameters to differentiate with respect to}}
@differentiating(noParams)
func vjpNoParams() -> (value: Float, pullback: (Float) -> Float) {
  return (1, { $0 })
}

func noDiffParams(x: Int) -> Float {
  return 1
}
// expected-error @+1 {{no differentiation parameters could be inferred; must differentiate with respect to at least one parameter conforming to 'Differentiable'}}
@differentiating(noDiffParams)
func vjpNoDiffParams(x: Int) -> (value: Float, pullback: (Float) -> Int) {
  return (1, { _ in 0 })
}

// expected-error @+1 {{functions ('@differentiable (Float) -> Float') cannot be differentiated with respect to}}
@differentiable(wrt: fn)
func invalidDiffWrtFunction(_ fn: @differentiable(Float) -> Float) -> Float {
  return fn(.pi)
}

// expected-error @+2 {{type 'T' does not conform to protocol 'FloatingPoint'}}
// expected-error @+1 {{could not find function 'foo' with expected type '<T where T : AdditiveArithmetic, T : Differentiable> (T) -> T'}}
@differentiating(foo)
func vjpFoo<T : AdditiveArithmetic & Differentiable>(_ x: T) -> (value: T, pullback: (T.CotangentVector) -> (T.CotangentVector)) {
  return (x, { $0 })
}
@differentiating(foo)
func vjpFooExtraGenericRequirements<T : FloatingPoint & Differentiable & BinaryInteger>(_ x: T) -> (value: T, pullback: (T) -> (T)) where T == T.CotangentVector {
  return (x, { $0 })
}

// Test static methods.

extension AdditiveArithmetic where Self : Differentiable {
  // expected-error @+1 {{derivative not in the same file as the original function}}
  @differentiating(+)
  static func vjpPlus(x: Self, y: Self) -> (value: Self, pullback: (Self.CotangentVector) -> (Self.CotangentVector, Self.CotangentVector)) {
    return (x + y, { v in (v, v) })
  }
}

extension FloatingPoint where Self : Differentiable, Self == Self.CotangentVector {
  // expected-error @+1 {{derivative not in the same file as the original function}}
  @differentiating(+)
  static func vjpPlus(x: Self, y: Self) -> (value: Self, pullback: (Self) -> (Self, Self)) {
    return (x + y, { v in (v, v) })
  }
}

extension Differentiable where Self : AdditiveArithmetic {
  // expected-error @+1 {{'+' is not defined in the current type context}}
  @differentiating(+)
  static func vjpPlus(x: Self, y: Self) -> (value: Self, pullback: (Self.CotangentVector) -> (Self.CotangentVector, Self.CotangentVector)) {
    return (x + y, { v in (v, v) })
  }
}

extension AdditiveArithmetic where Self : Differentiable, Self == Self.CotangentVector {
  // expected-error @+1 {{could not find function '+' with expected type '<Self where Self : Differentiable, Self == Self.CotangentVector> (Self) -> (Self, Self) -> Self'}}
  @differentiating(+)
  func vjpPlusInstanceMethod(x: Self, y: Self) -> (value: Self, pullback: (Self) -> (Self, Self)) {
    return (x + y, { v in (v, v) })
  }
}

// Test instance methods.

protocol InstanceMethod : Differentiable {
  // expected-note @+1 {{'foo' defined here}}
  func foo(_ x: Self) -> Self
  func foo2(_ x: Self) -> Self
  // expected-note @+1 {{'bar' defined here}}
  func bar<T : Differentiable>(_ x: T) -> Self
  func bar2<T : Differentiable>(_ x: T) -> Self
}

extension InstanceMethod {
  // If `Self` conforms to `Differentiable`, then `Self` is currently always inferred to be a differentiation parameter.
  // expected-error @+2 {{function result's 'pullback' type does not match 'foo'}}
  // expected-note @+2 {{'pullback' does not have expected type '(Self.CotangentVector) -> (Self.CotangentVector, Self.CotangentVector)'}}
  @differentiating(foo)
  func vjpFoo(x: Self) -> (value: Self, pullback: (CotangentVector) -> CotangentVector) {
    return (x, { $0 })
  }

  @differentiating(foo)
  func jvpFoo(x: Self) -> (value: Self, differential: (TangentVector, TangentVector) -> (TangentVector)) {
    return (x, { $0 + $1 })
  }

  @differentiating(foo, wrt: (self, x))
  func vjpFooWrt(x: Self) -> (value: Self, pullback: (CotangentVector) -> (CotangentVector, CotangentVector)) {
    return (x, { ($0, $0) })
  }
}

extension InstanceMethod {
  // expected-error @+2 {{function result's 'pullback' type does not match 'bar'}}
  // expected-note @+2 {{'pullback' does not have expected type '(Self.CotangentVector) -> (Self.CotangentVector, T.CotangentVector)'}}
  @differentiating(bar)
  func vjpBar<T : Differentiable>(_ x: T) -> (value: Self, pullback: (CotangentVector) -> T.CotangentVector) {
    return (self, { _ in .zero })
  }

  @differentiating(bar)
  func vjpBar<T : Differentiable>(_ x: T) -> (value: Self, pullback: (CotangentVector) -> (CotangentVector, T.CotangentVector)) {
    return (self, { ($0, .zero) })
  }

  @differentiating(bar, wrt: (self, x))
  func jvpBarWrt<T : Differentiable>(_ x: T) -> (value: Self, differential: (TangentVector, T.TangentVector) -> TangentVector) {
    return (self, { dself, dx in dself })
  }
}

extension InstanceMethod where Self == Self.TangentVector, Self == Self.CotangentVector {
  @differentiating(foo2)
  func vjpFooExtraRequirements(x: Self) -> (value: Self, pullback: (Self) -> (Self, Self)) {
    return (x, { ($0, $0) })
  }

  @differentiating(foo2)
  func jvpFooExtraRequirements(x: Self) -> (value: Self, differential: (Self, Self) -> (Self)) {
    return (x, { $0 + $1 })
  }

  @differentiating(bar2)
  func vjpBarExtraRequirements<T : Differentiable>(x: T) -> (value: Self, pullback: (Self) -> (Self, T.CotangentVector)) {
    return (self, { ($0, .zero) })
  }

  @differentiating(bar2)
  func jvpBarExtraRequirements<T : Differentiable>(_ x: T) -> (value: Self, differential: (Self, T.TangentVector) -> Self) {
    return (self, { dself, dx in dself })
  }
}

protocol GenericInstanceMethod : Differentiable where Self == Self.TangentVector, Self == Self.CotangentVector {
  func instanceMethod<T : Differentiable>(_ x: T) -> T
}

extension GenericInstanceMethod {
  func jvpInstanceMethod<T : Differentiable>(_ x: T) -> (T, (T.TangentVector) -> (TangentVector, T.TangentVector)) {
    return (x, { v in (self, v) })
  }

  func vjpInstanceMethod<T : Differentiable>(_ x: T) -> (T, (T.CotangentVector) -> (CotangentVector, T.CotangentVector)) {
    return (x, { v in (self, v) })
  }
}

// Test extra generic constraints.

func bar<T>(_ x: T) -> T {
  return x
}
@differentiating(bar)
func vjpBar<T : Differentiable & VectorNumeric>(_ x: T) -> (value: T, pullback: (T.CotangentVector) -> T.CotangentVector) {
  return (x, { $0 })
}

func baz<T, U>(_ x: T, _ y: U) -> T {
  return x
}
@differentiating(baz)
func vjpBaz<T : Differentiable & VectorNumeric, U : Differentiable>(_ x: T, _ y: U)
    -> (value: T, pullback: (T) -> (T, U))
  where T == T.CotangentVector, U == U.CotangentVector
{
  return (x, { ($0, .zero) })
}

protocol InstanceMethodProto {
  func bar() -> Float
}
extension InstanceMethodProto where Self : Differentiable {
  @differentiating(bar)
  func vjpBar() -> (value: Float, pullback: (Float) -> CotangentVector) {
    return (bar(), { _ in .zero })
  }
}
