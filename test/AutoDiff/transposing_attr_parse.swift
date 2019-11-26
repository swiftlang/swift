// RUN: %target-swift-frontend -parse -verify %s

/// Good

@transpose(of: linearFunc) // ok
func jvpLinearFunc(x: @nondiff Float) -> Float {
  return (x, { 2 * $0 })
}

@transpose(of: linearFunc, wrt: 0) // ok
func jvpLinearFunc(t: @nondiff Float) -> Float {
  return 2 * t
}

@transpose(of: add, wrt: (0, 1)) // ok
func vjpAdd(t: Float) -> (Float, Float) {
  return (t, t)
}

extension AdditiveArithmetic where Self : Differentiable {
  @transpose(of: +) // ok
  static func addTranspose(t: Self) -> (TangentVector, TangentVector) {
    return (t, t)
  }
}

/// Bad

// expected-error @+1 {{expected label 'wrt:' in '@transpose' attribute}}
@transpose(of: linearFunc, linear)
func tfoo(t: Float) -> Float {
  return t
}

// expected-error @+2 {{expected an original function name}}
// expected-error @+1 {{expected declaration}}
@transpose(of: 3)
func tfoo(t: Float) -> Float {
  return t
}

// expected-error @+1 {{unexpected ',' separator}}
@transpose(of: foo,)
func tfoo(t: Float) -> Float {
  return t
}

// expected-error @+2 {{expected ')' in 'transpose' attribute}}
// expected-error @+1 {{expected declaration}}
@transpose(of: foo, wrt: 0,)
func tfoo(t: Float) -> Float {
  return t
}

// expected-error @+1 {{expected a parameter, which can be a 'unsigned int' parameter number or 'self'}}
@transpose(of: foo, wrt: x)
func tfoo(t: Float) -> Float {
  return t
}

// expected-error @+1 {{expected a parameter, which can be a 'unsigned int' parameter number or 'self'}}
@transpose(of: foo, wrt: (0, x))
func tfoo(t: Float) -> Float {
  return t
}

func localTransposeRegistration() {
  // Not okay. Transpose registration can only be non-local.
  // expected-error @+1 {{attribute '@transpose' can only be used in a non-local scope}}
  @transpose(of: +)
  func foo(_ x: Float) -> (Float, Float)
}
