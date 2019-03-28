// RUN: %target-swift-frontend -emit-sil -verify %s

struct Tensor<Scalar : FloatingPoint & Differentiable> : VectorNumeric, Differentiable {
  // NOTE: `value` must have type with known size (e.g. `Float`, not `Scalar`)
  // until differentiation has indirect passing support.
  var value: Float
  init(_ value: Float) { self.value = value }
}

func generic<T : FloatingPoint & Differentiable>(_ x: Tensor<T>) -> Float {
  return x.value + x.value
}
_ = gradient(at: Tensor<Float>(1), in: generic)

// Test case where associated derivative function's requirements are unmet.

@differentiable(vjp: vjpWeirdExtraRequirements where T : CaseIterable, T.AllCases : ExpressibleByStringLiteral)
func weird<T : FloatingPoint & Differentiable>(_ x: Tensor<T>) -> Tensor<T> {
  return x
}
func vjpWeirdExtraRequirements<T : FloatingPoint & Differentiable>(_ x: Tensor<T>) -> (Tensor<T>, (Tensor<T>) -> Tensor<T>) where T : CaseIterable, T.AllCases : ExpressibleByStringLiteral {
  return (x, { $0 })
}
func weirdWrapper<T : FloatingPoint & Differentiable>(_ x: Tensor<T>) -> Tensor<T> {
  return weird(x) // expected-note {{function call is not differentiable because generic requirements are not met}}
}
_ = pullback(at: Tensor<Float>(1), in: weirdWrapper) // expected-error {{function is not differentiable}}
_ = pullback(at: Tensor<Float>(3), in: weirdWrapper)

// Test case where associated derivative function's requirements are met.
extension Tensor where Scalar : Numeric {
  @differentiable(wrt: self where Scalar : Differentiable & FloatingPoint)
  func mean() -> Tensor {
    return self
  }

  @differentiable(wrt: self where Scalar : Differentiable & FloatingPoint)
  func variance() -> Tensor {
    return mean() // ok
  }
}
_ = pullback(at: Tensor<Float>(1), in: { $0.variance() })

// Tests TF-277.
protocol Layer : Differentiable {
  associatedtype Output : Differentiable
}
struct SupervisedTrainer<Model : Layer> {
  var model: Model
  var lossFunction: @differentiable (Model.Output, Model.Output) -> Float
  func fit(y: Model.Output) {
    // expected-warning @+1 {{result does not depend on differentiation arguments and will always have a zero derivative; do you want to add '.withoutDerivative()'?}} {{64-64=.withoutDerivative()}}
    _ = gradient(at: Float(1)) { _ in return lossFunction(y, y) }
  }
}

// TODO: add more tests.
