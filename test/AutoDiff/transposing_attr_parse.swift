// RUN: %target-swift-frontend -parse -verify %s

/// Good

@transposing(linearFunc) // ok
func jvpLinearFunc(x: @nondiff Float) -> Float {
  return (x, { 2 * $0 })
}

@transposing(linearFunc, wrt: 0) // ok
func jvpLinearFunc(t: @nondiff Float) -> Float {
  return 2 * t
}

@transposing(add, wrt: (0, 1)) // ok
func vjpAdd(t: Float) -> (Float, Float) {
  return (t, t)
}

extension AdditiveArithmetic where Self : Differentiable {
  @transposing(+) // ok
  static func transposingPlus(t: Self) 
  -> (Self.TangentVector, Self.TangentVector) {
    return (t, t)
  }
}

/// Bad

// expected-error @+1 {{expected label 'wrt:' in '@transposing' attribute}}
@transposing(linearFunc, linear)
func tfoo(t: Float) -> Float {
  return t
}

// expected-error @+2 {{expected an original function name}}
// expected-error @+1 {{expected declaration}}
@transposing(3)
func tfoo(t: Float) -> Float {
  return t
}

// expected-error @+1 {{unexpected ',' separator}}
@transposing(foo,)
func tfoo(t: Float) -> Float {
  return t
}

// expected-error @+2 {{expected ')' in 'transposing' attribute}}
// expected-error @+1 {{expected declaration}}
@transposing(foo, wrt: 0,)
func tfoo(t: Float) -> Float {
  return t
}

// expected-error @+1 {{expected a parameter, which can be a 'unsigned int' parameter number or 'self'}}
@transposing(foo, wrt: x)
func tfoo(t: Float) -> Float {
  return t
}

// expected-error @+1 {{expected a parameter, which can be a 'unsigned int' parameter number or 'self'}}
@transposing(foo, wrt: (0, x))
func tfoo(t: Float) -> Float {
  return t
}

func localTransposeRegistration() {
  // Not okay. Transpose registration can only be non-local.
  // expected-error @+1 {{attribute '@transposing' can only be used in a non-local scope}}
  @transposing(+)
  func foo(_ x: Float) -> (Float, Float)
}
