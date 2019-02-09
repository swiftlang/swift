// RUN: %target-swift-frontend -emit-sil -verify %s

//===----------------------------------------------------------------------===//
// Top-level (before primal/adjoint synthesis)
//===----------------------------------------------------------------------===//

// expected-note @+1 {{opaque non-'@differentiable' function is not differentiable}}
func foo(_ f: (Float) -> Float) -> Float {
  // expected-error @+1 {{function is not differentiable}}
  return gradient(at: 0, in: f)
}

//===----------------------------------------------------------------------===//
// Basic function
//===----------------------------------------------------------------------===//

func one_to_one_0(_ x: Float) -> Float {
  return x + 2
}

_ = gradient(at: 0, in: one_to_one_0) // okay!

//===----------------------------------------------------------------------===//
// Indirect parameters/results (generics)
//===----------------------------------------------------------------------===//

// expected-note @+3 {{differentiating functions with parameters or result of unknown size is not supported yet}}
// expected-error @+2 {{function is not differentiable}}
@differentiable()
func generic<T: Differentiable & FloatingPoint>(_ x: T) -> T {
  return x + 1
}

struct Tensor<Scalar> {
  static func + (_ lhs: Tensor, rhs: Scalar) -> Tensor { return lhs }
}
extension Tensor : Differentiable where Scalar : Differentiable & FloatingPoint {}
extension Tensor where Scalar : BinaryFloatingPoint {
  // expected-note @+3 {{differentiating functions with parameters or result of unknown size is not supported yet}}
  // expected-error @+2 {{function is not differentiable}}
  @differentiable(wrt: (self) where Scalar : Differentiable)
  func TF_6(_ x: Float) -> Tensor {
    return self + Scalar(x)
  }
}

// expected-error @+2 {{function is not differentiable}}
// expected-note @+1 {{differentiating functions with parameters or result of unknown size is not supported yet}}
_ = gradient(at: 1.0, in: { x in x.squareRoot() })

// FIXME(TF-159): Diagnose functions with inout parameters.
// _ = Float(5).gradient { x -> Float in
//   var a = x
//   a += x
//   return a
// }

//===----------------------------------------------------------------------===//
// Non-differentiable stored properties
//===----------------------------------------------------------------------===//

struct S {
  let p: Float
}

extension S : Differentiable, VectorNumeric {
  static var zero: S { return S(p: 0) }
  typealias Scalar = Float
  static func + (lhs: S, rhs: S) -> S { return S(p: lhs.p + rhs.p) }
  static func - (lhs: S, rhs: S) -> S { return S(p: lhs.p - rhs.p) }
  static func * (lhs: Float, rhs: S) -> S { return S(p: lhs * rhs.p) }

  typealias TangentVector = S
  typealias CotangentVector = S
}

// expected-error @+2 {{function is not differentiable}}
// expected-note @+1 {{property is not differentiable}}
_ = gradient(at: S(p: 0)) { s in 2 * s.p }

//===----------------------------------------------------------------------===//
// Function composition
//===----------------------------------------------------------------------===//

// FIXME: Figure out why diagnostics no longer accumulate after we removed
// gradient synthesis. When it's fixed, replace "xpected" with "expected" below.
#if false

func uses_optionals(_ x: Float) -> Float {
  var maybe: Float? = 10
  maybe = x
  // xpected-note @+1 {{differentiating control flow is not supported yet}}
  return maybe!
}

_ = gradient(at: 0, in: uses_optionals) // xpected-error {{function is not differentiable}}

func f0(_ x: Float) -> Float {
  return x // okay!
}

func nested(_ x: Float) -> Float {
  return gradient(at: x, in: f0) // xpected-note {{nested differentiation is not supported yet}}
}

func middle(_ x: Float) -> Float {
  let y = uses_optionals(x)
  return nested(y) // xpected-note {{when differentiating this function call}}
}

func middle2(_ x: Float) -> Float {
  return middle(x) // xpected-note {{when differentiating this function call}}
}

func func_to_diff(_ x: Float) -> Float {
  return middle2(x) // xpected-note {{expression is not differentiable}}
}

func calls_grad_of_nested(_ x: Float) -> Float {
  return gradient(at: x, in: func_to_diff) // xpected-error {{function is not differentiable}}
}

//===----------------------------------------------------------------------===//
// Control flow
//===----------------------------------------------------------------------===//

func if_else(_ x: Float, _ flag: Bool) -> Float {
  let y: Float
  // xpected-note @+1 {{differentiating control flow is not supported yet}}
  if flag {
    y = x + 1
  } else {
    y = x
  }
  return y
}

// xpected-error @+1 {{function is not differentiable}}
_ = gradient(at: 0) { x in if_else(0, true) }

#endif

//===----------------------------------------------------------------------===//
// @differentiable attributes
//===----------------------------------------------------------------------===//
var a: Float = 3.0
protocol P {
  @differentiable
  func foo(x: Float) -> Float
}

enum T : P {
  // expected-note @+2 {{when differentiating this function definition}}
  // expected-error @+1 {{function is not differentiable}}
  @differentiable func foo(x: Float) -> Float {
    // expected-note @+1 {{cannot differentiate writes to global variables}}
    a = a + x
    return a
  }
}

// expected-note @+2 {{when differentiating this function definition}}
// expected-error @+1 {{function is not differentiable}}
@differentiable func foo(x: Float) -> Float {
  // expected-note @+1 {{cannot differentiate writes to global variables}}
  a = a + x
  return a
}

//===----------------------------------------------------------------------===//
// Crasher TF-8
//===----------------------------------------------------------------------===//

protocol TF8Proto : Differentiable {
  associatedtype Scalar
  @differentiable(wrt: (self, input))
  func applied(to input: Float) -> Float
}

struct TF8Struct<Scalar> : TF8Proto where Scalar : FloatingPoint & Differentiable {
  @noDerivative let bar: Scalar

  @differentiable(wrt: (self, input))
  // expected-note @+2 {{differentiating functions with parameters or result of unknown size is not supported yet}}
  // expected-error @+1 {{function is not differentiable}}
  func applied(to input: Float) -> Float {
    return input
  }
}
