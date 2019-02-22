// RUN: %target-swift-frontend -emit-sil -verify -verify-ignore-unknown %s

// FIXME(TF-201): Remove `-verify-ignore-unknown`. This is currently necessary
// due to direct differentiation of reabstraction thunks, which emits errors
// with unknown location.

// expected-error @+1 2 {{function is not differentiable}}
@differentiable()
// expected-note @+2 {{when differentiating this function definition}}
// expected-note @+1 {{when differentiating this function definition}}
func generic<T: Differentiable & FloatingPoint>(_ x: T) -> T {
  // expected-note @+2 {{member is not differentiable because the corresponding protocol requirement is not '@differentiable'}}
  // expected-note @+1 {{expression is not differentiable}}
  return x + 1
}
_ = gradient(at: 1.0, in: generic) // expected-error {{function is not differentiable}}

// okay!
@differentiable
func directMissingConformance<T : Differentiable>(_ x: T) -> T {
  return x
}

@differentiable
func direct<T : Differentiable>(_ x: T) -> T {
  return x
}

struct Tensor<Scalar> {
  static func + (_ lhs: Tensor, rhs: Scalar) -> Tensor { return lhs }
}
extension Tensor : Differentiable where Scalar : Differentiable & FloatingPoint {}
extension Tensor where Scalar : BinaryFloatingPoint {
  @differentiable(wrt: (self) where Scalar : Differentiable)
  func TF_6(_ x: Float) -> Tensor {
    return self + Scalar(x)
  }
}

protocol TF8Proto : Differentiable {
  associatedtype Scalar
  @differentiable(wrt: (self, input))
  func applied(to input: Float) -> Float
}

struct TF8Struct<Scalar> : TF8Proto where Scalar : FloatingPoint & Differentiable {
  @noDerivative let bar: Scalar

  @differentiable(wrt: (self, input))
  func applied(to input: Float) -> Float {
    return input
  }
}

_ = gradient(at: 1.0, in: { x in x.squareRoot() })
