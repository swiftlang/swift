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

// expected-error @+2 {{expected an original function name}}
// expected-error @+1 {{expected declaration}}
@transpose(of: 3)
func transpose(v: Float) -> Float

// expected-error @+1 {{expected label 'wrt:' in '@transpose' attribute}}
@transpose(of: foo, blah)
func transpose(v: Float) -> Float

// expected-error @+1 {{expected a colon ':' after 'wrt'}}
@transpose(of: foo, wrt)
func transpose(v: Float) -> Float

// expected-error @+1 {{unexpected ',' separator}}
@transpose(of: foo,)
func transpose(v: Float) -> Float

// expected-error @+2 {{expected ')' in 'transpose' attribute}}
// expected-error @+1 {{expected declaration}}
@transpose(of: foo, wrt: 0,)
func transpose(v: Float) -> Float

// expected-error @+1 {{expected a parameter, which can be a function parameter index or 'self'}}
@transpose(of: foo, wrt: v)
func transpose(v: Float) -> Float

// expected-error @+1 {{expected a parameter, which can be a function parameter index or 'self'}}
@transpose(of: foo, wrt: (0, v))
func transpose(v: Float) -> Float

// expected-error @+2 {{expected ')' in 'transpose' attribute}}
// expected-error @+1 {{expected declaration}}
@transpose(of: Swift.Float.+(_:_))
func transpose(v: Float) -> Float

// expected-error @+2 {{expected ')' in 'transpose' attribute}}
// expected-error @+1 {{expected declaration}}
@transpose(of: Swift.Float.+.a)
func transpose(v: Float) -> Float

func testLocalTransposeRegistration() {
  // Transpose registration can only be non-local.
  // expected-error @+1 {{attribute '@transpose' can only be used in a non-local scope}}
  @transpose(of: +)
  func transpose(_ x: Float) -> (Float, Float)
}
