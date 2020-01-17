// RUN: %target-swift-frontend -parse -verify %s

/// Good

@derivative(of: sin, wrt: x) // ok
func vjpSin(x: Float) -> (value: Float, pullback: (Float) -> Float) {
  return (x, { $0 })
}

@derivative(of: add, wrt: (x, y)) // ok
func vjpAdd(x: Float, y: Float)
  -> (value: Float, pullback: (Float) -> (Float, Float)) {
  return (x + y, { ($0, $0) })
}

extension AdditiveArithmetic where Self: Differentiable {
  @derivative(of: +) // ok
  static func vjpAdd(x: Self, y: Self)
    -> (value: Self, pullback: (TangentVector) -> (TangentVector, TangentVector)) {
    return (x + y, { v in (v, v) })
  }
}

@derivative(of: foo) // ok
func dfoo(x: Float) -> (value: Float, differential: (Float) -> (Float)) {
  return (x, { $0 })
}

/// Bad

// expected-error @+2 {{expected an original function name}}
// expected-error @+1 {{expected declaration}}
@derivative(of: 3)
func dfoo(x: Float) -> (value: Float, differential: (Float) -> (Float)) {
  return (x, { $0 })
}

// expected-error @+1 {{expected label 'wrt:' in '@derivative' attribute}}
@derivative(of: wrt, foo)
func dfoo(x: Float) -> (value: Float, differential: (Float) -> (Float)) {
  return (x, { $0 })
}

// expected-error @+1 {{expected a colon ':' after 'wrt'}}
@derivative(of: foo, wrt)
func dfoo(x: Float) -> (value: Float, differential: (Float) -> (Float)) {
  return (x, { $0 })
}

// expected-error @+1 {{expected label 'wrt:' in '@derivative' attribute}}
@derivative(of: foo, blah, wrt: x)
func dfoo(x: Float) -> (value: Float, differential: (Float) -> (Float)) {
  return (x, { $0 })
}

// expected-error @+2 {{expected ')' in 'derivative' attribute}}
// expected-error @+1 {{expected declaration}}
@derivative(of: foo, wrt: x, blah)
func dfoo(x: Float) -> (value: Float, differential: (Float) -> (Float)) {
    return (x, { $0 })
}

// expected-error @+1 {{unexpected ',' separator}}
@derivative(of: foo,)
func dfoo(x: Float) -> (value: Float, differential: (Float) -> (Float)) {
  return (x, { $0 })
}

// expected-error @+2 {{expected ')' in 'derivative' attribute}}
// expected-error @+1 {{expected declaration}}
@derivative(of: foo, wrt: x,)
func dfoo(x: Float) -> (value: Float, differential: (Float) -> (Float)) {
  return (x, { $0 })
}

// expected-error @+1 {{expected label 'wrt:' in '@derivative' attribute}}
@derivative(of: foo, foo,)
func dfoo(x: Float) -> (value: Float, differential: (Float) -> (Float)) {
  return (x, { $0 })
}

// expected-error @+1 {{unexpected ',' separator}}
@derivative(of: foo,)
func dfoo(x: Float) -> (value: Float, differential: (Float) -> (Float)) {
  return (x, { $0 })
}

func localDerivativeRegistration() {
  // expected-error @+1 {{attribute '@derivative' can only be used in a non-local scope}}
  @derivative(of: sin)
  func dsin()
}
