// RUN: %target-swift-frontend -typecheck -verify %s

// expected-error @+1 {{'@differentiable' used without importing module '_Differentiation'}}
let _: @differentiable(reverse) (Float) -> Float

// expected-error @+2 {{'@differentiable' used without importing module '_Differentiation'}}
// expected-error @+1 {{'@noDerivative' used without importing module '_Differentiation'}}
let _: @differentiable(reverse) (Float, @noDerivative Float) -> Float

// expected-error @+1 {{'@noDerivative' used without importing module '_Differentiation'}}
let _: (Float, @noDerivative Float) -> Float

// expected-error @+1 {{'@noDerivative' used without importing module '_Differentiation'}}
let _: @noDerivative Float

func id(_ x: Float) -> Float {
  return x
}
// expected-error @+1 {{@derivative attribute used without importing module '_Differentiation'}}
@derivative(of: id)
func jvpId(x: Float) -> (value: Float, differential: (Float) -> (Float)) {
  return (x, { $0 })
}
