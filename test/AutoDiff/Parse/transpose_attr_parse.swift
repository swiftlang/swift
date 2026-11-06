// RUN: %target-swift-frontend -parse -verify %s

/// Good

@transpose(of: foo)
func transpose(v: Float) -> Float

@transpose(of: foo(_:_:))
func transpose(v: Float) -> Float

@transpose(of: wrt, wrt: 0)
func transpose(v: Float) -> Float

@transpose(of: foo, wrt: 0)
func transpose(v: Float) -> Float

@transpose(of: foo, wrt: (0, 1))
func transpose(v: Float) -> (Float, Float)

@transpose(of: foo, wrt: (self, 0, 1, 2))
func transpose(v: Float) -> (Float, Float, Float, Float)

// Qualified declaration.
@transpose(of: A.B.C.foo(x:y:_:z:))
func transpose(v: Float) -> Float

// Qualified declaration with specialized generic type.
@transpose(of: A<T>.B<U, V>.C.foo(x:y:_:z:))
func transpose(v: Float) -> Float

// Qualified operator.
// TODO(TF-1065): Consider disallowing qualified operators.
@transpose(of: Swift.Float.+)
func transpose(v: Float) -> Float

// Qualified leading-period operator (confusing).
// TODO(TF-1065): Consider disallowing qualified operators.
@transpose(of: Swift.Float..<)
func transpose(v: Float) -> Float

// `init` keyword edge case.
@transpose(of: Swift.Float.init(_:))
func transpose(v: Float) -> Float

// `subscript` keyword edge case.
@transpose(of: Swift.Array.subscript(_:))
func transpose(v: Float) -> Float

/// Bad

// expected-cparser-error @+2 {{expected an original function name}} expected-astgen-error@+2 {{unexpected code '3' in attribute}}
// expected-cparser-error @+1 {{expected declaration}} expected-astgen-error@+1 {{expected identifier in attribute arguments}}
@transpose(of: 3)
func transpose(v: Float) -> Float

// expected-cparser-error @+1 {{expected label 'wrt:' in '@transpose' attribute}} expected-astgen-error@+1 {{expected 'wrt:' in '@differentiable' argument}}
@transpose(of: foo, blah) // expected-astgen-error {{expected a parameter, which can be a function parameter index or 'self'}}
func transpose(v: Float) -> Float

// expected-cparser-error @+1 {{expected a colon ':' after 'wrt'}} expected-astgen-error@+1 {{expected ':' and arguments in '@differentiable' argument}}
@transpose(of: foo, wrt) // expected-astgen-error {{expected a parameter, which can be a function parameter index or 'self'}}
func transpose(v: Float) -> Float

// expected-cparser-error @+1 {{unexpected ',' separator}} {{19-20=}} expected-astgen-error@+1 {{expected 'wrt:' and arguments in '@differentiable' argument}}
@transpose(of: foo,) // expected-astgen-error {{expected a parameter, which can be a function parameter index or 'self'}}
func transpose(v: Float) -> Float

// expected-cparser-error @+2 {{expected ')' in 'transpose' attribute}}
// expected-cparser-error @+1 {{expected declaration}}
@transpose(of: foo, wrt: 0,) // expected-astgen-error {{expected ')' in 'transpose' attribute}} expected-astgen-error {{expected declaration}}
func transpose(v: Float) -> Float

// expected-cparser-error @+1 {{expected a parameter, which can be a function parameter index or 'self'}}
@transpose(of: foo, wrt: v) // expected-astgen-error {{expected a parameter, which can be a function parameter index or 'self'}}
func transpose(v: Float) -> Float

// expected-cparser-error @+1 {{expected a parameter, which can be a function parameter index or 'self'}}
@transpose(of: foo, wrt: (0, v)) // expected-astgen-error {{expected a parameter, which can be a function parameter index or 'self'}}
func transpose(v: Float) -> Float

// NOTE: The "expected ',' separator" diagnostic is not ideal.
// Ideally, the diagnostic should point out that `Swift.Float.+(_:_)` is
// not a valid declaration name (missing colon after second argument label).
// expected-cparser-error @+2 {{expected ',' separator}}
// expected-cparser-error @+1 {{expected declaration}} expected-astgen-error@+1 {{unexpected code '(_:_)' in attribute}}
@transpose(of: Swift.Float.+(_:_))
func transpose(v: Float) -> Float

// NOTE: The "expected ',' separator" diagnostic is not ideal.
// Ideally, the diagnostic should point out that `Swift.Float.+.a` is
// not a valid declaration name.
// expected-cparser-error @+2 {{expected ',' separator}} expected-astgen-error@+2 {{unexpected code 'a' in attribute}}
// expected-cparser-error @+1 {{expected declaration}} expected-astgen-error@+1 {{expected 'get' in attribute arguments}}
@transpose(of: Swift.Float.+.a)
func transpose(v: Float) -> Float

// TF-1168: missing comma before `wrt:`.
// expected-cparser-error @+2 {{expected ',' separator}} {{19-19=,}}
// expected-cparser-error @+1 {{expected declaration}} expected-astgen-error@+1 {{unexpected code 'wrt: x' in attribute}}
@transpose(of: foo wrt: x)
func transpose(v: Float) -> Float

func testLocalTransposeRegistration() {
  // Transpose registration can only be non-local.
  // expected-error @+1 {{'@transpose' can only be used in a non-local scope}}
  @transpose(of: +)
  func transpose(_ x: Float) -> (Float, Float)
}
