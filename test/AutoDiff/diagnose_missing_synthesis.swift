// RUN: %target-swift-frontend -emit-sil -Xllvm -differentiation-use-vjp=false -verify %s

@differentiable(vjp: vjpMultiply)
func multiply(_ x: Float) -> Float {
  return x * 10
}

func vjpMultiply(_ x: Float) -> (Float, (Float) -> Float) {
  return (x * 10, { v in v * 10 })
}

func wrapper(_ x: Float) -> Float {
  // expected-note @+1 {{function does not have primal or adjoint; define an adjoint (and optionally, a primal) using a '@differentiable' attribute, or enable '-differentiation-use-vjp' mode}}
  return multiply(x)
}

// expected-error @+1 {{function is not differentiable}}
print(gradient(at: 0, in: wrapper))
