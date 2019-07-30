// RUN: %target-swift-frontend -emit-sil -verify -verify-ignore-unknown %s

// FIXME(TF-201): Remove `-verify-ignore-unknown`. This is currently necessary
// due to direct differentiation of reabstraction thunks, which emits errors
// with unknown location.

@differentiable
func generic<T: Differentiable & FloatingPoint>(_ x: T) -> T {
  // expected-error @+2 {{expression is not differentiable}}
  // expected-note @+1 {{member is not differentiable because the corresponding protocol requirement is not '@differentiable'}}
  return x + 1
}
_ = gradient(at: 1.0, in: generic)

// Test unmet generic requirements.

@differentiable(
  vjp: vjpWeirdExtraRequirements
  where T : Differentiable & CaseIterable, T.AllCases : ExpressibleByStringLiteral
)
func weird<T>(_ x: T) -> T {
  return x
}
func vjpWeirdExtraRequirements<
  T : Differentiable & CaseIterable
>(_ x: T) -> (T, (T.TangentVector) -> T.TangentVector)
  where T.AllCases : ExpressibleByStringLiteral
{
  return (x, { $0 })
}
func weirdWrapper<T : Differentiable>(_ x: T) -> T {
  // expected-error @+2 {{expression is not differentiable}}
  // expected-note @+1 {{function call is not differentiable because generic requirements are not met: 'T : CaseIterable, T.AllCases : ExpressibleByStringLiteral'}}
  return weird(x)
}
_ = gradient(at: Float(1), in: { x in weirdWrapper(x) })

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
