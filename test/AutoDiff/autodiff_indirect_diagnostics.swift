// RUN: %target-swift-emit-sil -verify %s

//===----------------------------------------------------------------------===//
// Non-differentiable arguments and results
//===----------------------------------------------------------------------===//

@differentiable
func generic<T: Differentiable & FloatingPoint>(_ x: T) -> T {
  // expected-error @+2 {{expression is not differentiable}}
  // expected-note @+1 {{member is not differentiable because the corresponding protocol requirement is not '@differentiable'}}
  return x + 1
}
_ = gradient(at: 1.0, in: generic)

// Test unmet generic requirements.

func weird<T>(_ x: T) -> T {
  return x
}
@derivative(of: weird)
func vjpWeirdExtraRequirements<T : Differentiable & CaseIterable>(_ x: T) -> (
  value: T, pullback: (T.TangentVector) -> T.TangentVector
) where T.AllCases : ExpressibleByStringLiteral {
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
  @differentiable(wrt: self where Scalar : Differentiable)
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

//===----------------------------------------------------------------------===//
// Non-differentiable arguments and results
//===----------------------------------------------------------------------===//

struct TF_687<T> : Differentiable {
  @noDerivative var indirectDummy: T
  var base: Float

  init(_ base: Float, dummy: T) {
    self.base = base
    self.indirectDummy = dummy
  }
}
// expected-error @+2 {{function is not differentiable}}
// expected-note @+1 {{cannot differentiate through a non-differentiable argument; do you want to use 'withoutDerivative(at:)'?}}
let _: @differentiable (Float) -> TF_687<Any> = { x in TF_687<Any>(x, dummy: x) }

//===----------------------------------------------------------------------===//
// Add `Differentiable` conformance for generic wrt parameters
//===----------------------------------------------------------------------===//

func id<T>(_ x: T) -> T { x }
let _: @differentiable (Float) -> Float = { x in id(x) }

struct TF_691<Scalar> {
  var x: Scalar
  init(_ x: Scalar) {
    self.x = x
  }
}
extension TF_691: Differentiable where Scalar: Differentiable {}

func identity<T>(_ x: TF_691<T>) -> TF_691<T> { x }
let _: @differentiable (Float) -> TF_691<Float> = { x in identity(TF_691(x)) }
let _: @differentiable (Float) -> TF_691<Float> = { x in id(TF_691(x)) }
