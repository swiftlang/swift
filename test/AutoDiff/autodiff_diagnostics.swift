// RUN: %target-swift-frontend -emit-sil -verify %s

//===----------------------------------------------------------------------===//
// Basic function
//===----------------------------------------------------------------------===//

func one_to_one_0(_ x: Float) -> Float {
  return x + 2
}

_ = gradient(at: 0, in: one_to_one_0) // okay!

//===----------------------------------------------------------------------===//
// Non-differentiable stored properties
//===----------------------------------------------------------------------===//

struct S {
  var p: Float
}
extension S : Differentiable, VectorProtocol {
  // Test custom `TangentVector` type with non-matching stored property name.
  struct TangentVector: Differentiable, VectorProtocol {
    var dp: Float
  }
  typealias AllDifferentiableVariables = S
  static var zero: S { return S(p: 0) }
  typealias Scalar = Float
  static func + (lhs: S, rhs: S) -> S { return S(p: lhs.p + rhs.p) }
  static func - (lhs: S, rhs: S) -> S { return S(p: lhs.p - rhs.p) }
  static func * (lhs: Float, rhs: S) -> S { return S(p: lhs * rhs.p) }

  mutating func move(along direction: TangentVector) {
    p.move(along: direction.dp)
  }
}

// expected-error @+2 {{function is not differentiable}}
// expected-note @+1 {{property cannot be differentiated because 'S.TangentVector' does not have a member named 'p'}}
_ = gradient(at: S(p: 0)) { s in 2 * s.p }

struct NoDerivativeProperty : Differentiable {
  var x: Float
  @noDerivative var y: Float
}
// expected-error @+1 {{function is not differentiable}}
_ = gradient(at: NoDerivativeProperty(x: 1, y: 1)) { s -> Float in
  var tmp = s
  // expected-note @+1 {{cannot differentiate through a '@noDerivative' stored property; do you want to use 'withoutDerivative(at:)'?}}
  tmp.y = tmp.x
  return tmp.x
}
_ = gradient(at: NoDerivativeProperty(x: 1, y: 1)) { s in
  // expected-warning @+1 {{result does not depend on differentiation arguments and will always have a zero derivative; do you want to use 'withoutDerivative(at:)'?}} {{10-10=withoutDerivative(at:}} {{13-13=)}}
  return s.y
}
_ = gradient(at: NoDerivativeProperty(x: 1, y: 1)) {
  // expected-warning @+1 {{result does not depend on differentiation arguments and will always have a zero derivative; do you want to use 'withoutDerivative(at:)'?}} {{3-3=withoutDerivative(at:}} {{7-7=)}}
  $0.y
}

//===----------------------------------------------------------------------===//
// Function composition
//===----------------------------------------------------------------------===//

func uses_optionals(_ x: Float) -> Float {
  var maybe: Float? = 10
  maybe = x
  // expected-note @+1 {{differentiating enum values is not yet supported}}
  return maybe!
}

// expected-error @+1 {{function is not differentiable}}
_ = gradient(at: 0, in: uses_optionals)

func base(_ x: Float) -> Float {
  // expected-error @+2 {{expression is not differentiable}}
  // expected-note @+1 {{cannot differentiate through a non-differentiable result; do you want to use 'withoutDerivative(at:)'?}}
  return Float(Int(x))
}

// TODO: Fix nested differentiation diagnostics. Need to fix indirect differentiation invokers.
func nested(_ x: Float) -> Float {
  // xpected-note @+1 {{when differentiating this function call}}
  return base(x)
}

func middle(_ x: Float) -> Float {
  // xpected-note @+1 {{when differentiating this function call}}
  return nested(x)
}

func middle2(_ x: Float) -> Float {
  // xpected-note @+1 {{when differentiating this function call}}
  return middle(x)
}

func func_to_diff(_ x: Float) -> Float {
  // xpected-note @+1 {{expression is not differentiable}}
  return middle2(x)
}

func calls_grad_of_nested(_ x: Float) -> Float {
  // xpected-error @+1 {{function is not differentiable}}
  return gradient(at: x, in: func_to_diff)
}

//===----------------------------------------------------------------------===//
// Control flow
//===----------------------------------------------------------------------===//

func if_else(_ x: Float, _ flag: Bool) -> Float {
  let y: Float
  if flag {
    y = x + 1
  } else {
    y = x
  }
  return y
}

_ = gradient(at: 0) { x in if_else(x, true) }

//===----------------------------------------------------------------------===//
// @differentiable attributes
//===----------------------------------------------------------------------===//

var a: Float = 3.0
protocol P {
  @differentiable(wrt: x)
  func foo(x: Float) -> Float
}

enum T : P {
  // expected-note @+2 {{when differentiating this function definition}}
  // expected-error @+1 {{function is not differentiable}}
  @differentiable(wrt: x) func foo(x: Float) -> Float {
    // expected-note @+1 {{cannot differentiate writes to global variables}}
    a = a + x
    return a
  }
}

// expected-note @+2 {{when differentiating this function definition}}
// expected-error @+1 {{function is not differentiable}}
@differentiable(wrt: x) func foo(x: Float) -> Float {
  // expected-note @+1 {{cannot differentiate writes to global variables}}
  a = a + x
  return a
}

// Test `@differentiable` on initializer with assignments.
struct TF_305 : Differentiable {
  var filter: Float
  var bias: Float
  typealias Activation = @differentiable (Float) -> Float
  @noDerivative let activation: Activation
  @noDerivative let strides: (Int, Int)

  @differentiable
  public init(
    filter: Float,
    bias: Float,
    activation: @escaping Activation,
    strides: (Int, Int)
  ) {
    self.filter = filter
    self.bias = bias
    self.activation = activation
    self.strides = strides
  }
}

//===----------------------------------------------------------------------===//
// Classes and existentials (not yet supported)
//===----------------------------------------------------------------------===//

class Foo {
  // FIXME: Figure out why diagnostics for direct references end up here, and
  // why they are duplicated.
  // expected-error @+2 2 {{expression is not differentiable}}
  // expected-note @+1 2 {{differentiating class members is not yet supported}}
  func class_method(_ x: Float) -> Float {
    return x
  }
}

// Nested call case.
@differentiable
func triesToDifferentiateClassMethod(x: Float) -> Float {
  // expected-error @+2 {{expression is not differentiable}}
  // expected-note @+1 {{differentiating class members is not yet supported}}
  return Foo().class_method(x)
}

let _: @differentiable (Float) -> Float = Foo().class_method

//===----------------------------------------------------------------------===//
// Unreachable
//===----------------------------------------------------------------------===//

// expected-error @+1 {{function is not differentiable}}
let no_return: @differentiable (Float) -> Float = { x in
  let _ = x + 1
// expected-error @+2 {{missing return in a closure expected to return 'Float'}}
// expected-note @+1 {{missing return for differentiation}}
}

// expected-error @+1 {{function is not differentiable}}
@differentiable
// expected-note @+1 {{when differentiating this function definition}}
func roundingGivesError(x: Float) -> Float {
  // expected-note @+1 {{cannot differentiate through a non-differentiable result; do you want to use 'withoutDerivative(at:)'?}}
  return Float(Int(x))
}

//===----------------------------------------------------------------------===//
// Inout arguments
//===----------------------------------------------------------------------===//

func activeInoutArg(_ x: Float) -> Float {
  var a = x
  // expected-note @+1 {{cannot differentiate through 'inout' arguments}}
  a += x
  return a
}
// expected-error @+1 {{function is not differentiable}}
_ = pullback(at: .zero, in: activeInoutArg(_:))


func activeInoutArgTuple(_ x: Float) -> Float {
  var tuple = (x, x)
  // expected-note @+1 {{cannot differentiate through 'inout' arguments}}
  tuple.0 *= x
  return x * tuple.0
}
// expected-error @+1 {{function is not differentiable}}
_ = pullback(at: .zero, in: activeInoutArgTuple(_:))

//===----------------------------------------------------------------------===//
// Non-varied results
//===----------------------------------------------------------------------===//

func one() -> Float {
  return 1
}
@differentiable
func nonVariedResult(_ x: Float) -> Float {
  // expected-warning @+1 {{result does not depend on differentiation arguments and will always have a zero derivative; do you want to use 'withoutDerivative(at:)'?}} {{10-10=withoutDerivative(at:}} {{15-15=)}}
  return one()
}

//===----------------------------------------------------------------------===//
// Subset parameters
//===----------------------------------------------------------------------===//

func nondiff(_ f: @differentiable (Float, @nondiff Float) -> Float) -> Float {
  // expected-note @+2 {{cannot differentiate with respect to a '@nondiff' parameter}}
  // expected-error @+1 {{function is not differentiable}}
  return gradient(at: 2) { x in f(x * x, x) }
}
