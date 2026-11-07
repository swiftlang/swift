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

@derivative(of: property.get) // ok
func dPropertyGetter() -> ()

@derivative(of: subscript.get) // ok
func dSubscriptGetter() -> ()

@derivative(of: subscript(_:label:).get) // ok
func dLabeledSubscriptGetter() -> ()

@derivative(of: property.set) // ok
func dPropertySetter() -> ()

@derivative(of: subscript.set) // ok
func dSubscriptSetter() -> ()

@derivative(of: subscript(_:label:).set) // ok
func dLabeledSubscriptSetter() -> ()

@derivative(of: nestedType.name) // ok
func dNestedTypeFunc() -> ()

/// Bad

// expected-cparser-error @+2 {{expected an original function name}} expected-astgen-error@+2 {{unexpected code '3' in attribute}}
// expected-cparser-error @+1 {{expected declaration}} expected-astgen-error@+1 {{expected identifier in attribute arguments}}
@derivative(of: 3)
func dfoo(x: Float) -> (value: Float, differential: (Float) -> (Float)) {
  return (x, { $0 })
}

// expected-cparser-error @+1 {{expected label 'wrt:' in '@derivative' attribute}} expected-astgen-error@+1 {{expected 'wrt:' in differentiability argument}}
@derivative(of: wrt, foo)
func dfoo(x: Float) -> (value: Float, differential: (Float) -> (Float)) {
  return (x, { $0 })
}

// expected-cparser-error @+1 {{expected a colon ':' after 'wrt'}} expected-astgen-error@+1 {{expected ':' and arguments in differentiability argument}}
@derivative(of: foo, wrt)
func dfoo(x: Float) -> (value: Float, differential: (Float) -> (Float)) {
  return (x, { $0 })
}

// expected-cparser-error @+1 {{expected label 'wrt:' in '@derivative' attribute}} expected-astgen-error@+1 {{expected 'wrt:' in differentiability argument}}
@derivative(of: foo, blah, wrt: x) // expected-astgen-error {{unexpected code 'wrt: x' in attribute}} expected-astgen-error {{expected ')' in 'derivative' attribute}} expected-astgen-error {{expected declaration}}
func dfoo(x: Float) -> (value: Float, differential: (Float) -> (Float)) {
  return (x, { $0 })
}

// expected-error @+2 {{expected ')' in 'derivative' attribute}}
// expected-error @+1 {{expected declaration}}
@derivative(of: foo, wrt: x, blah) // expected-astgen-error {{unexpected code 'blah' in attribute}}
func dfoo(x: Float) -> (value: Float, differential: (Float) -> (Float)) {
    return (x, { $0 })
}

// expected-cparser-error @+1 {{unexpected ',' separator}} {{20-21=}} expected-astgen-error@+1 {{expected 'wrt:' and arguments in differentiability argument}}
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

// expected-cparser-error @+1 {{expected label 'wrt:' in '@derivative' attribute}} expected-astgen-error@+1 {{expected 'wrt:' in differentiability argument}}
@derivative(of: foo, foo,) // expected-astgen-error {{expected ')' in 'derivative' attribute}} expected-astgen-error {{expected declaration}}
func dfoo(x: Float) -> (value: Float, differential: (Float) -> (Float)) {
  return (x, { $0 })
}

// expected-cparser-error @+1 {{unexpected ',' separator}} {{20-21=}} expected-astgen-error@+1 {{expected 'wrt:' and arguments in differentiability argument}}
@derivative(of: foo,)
func dfoo(x: Float) -> (value: Float, differential: (Float) -> (Float)) {
  return (x, { $0 })
}

// TF-1168: missing comma before `wrt:`.
// expected-cparser-error @+2 {{expected ',' separator}} {{20-20=,}}
// expected-cparser-error @+1 {{expected declaration}} expected-astgen-error@+1 {{unexpected code 'wrt: x' in attribute}}
@derivative(of: foo wrt: x)
func dfoo(x: Float) -> (value: Float, differential: (Float) -> (Float)) {
  return (x, { $0 })
}

func testLocalDerivativeRegistration() {
  // expected-error @+1 {{'@derivative' can only be used in a non-local scope}}
  @derivative(of: sin)
  func dsin()
}


func testLocalDerivativeRegistration() {
  // expected-error @+1 {{'@derivative' can only be used in a non-local scope}}
  @derivative(of: sin)
  func dsin()
}

// expected-cparser-error @+2 {{expected ',' separator}}
// expected-cparser-error @+1 {{expected declaration}}
@derivative(of: nestedType.name.set)
func dNestedTypePropertySetter() -> ()

