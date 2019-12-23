// RUN: %target-swift-frontend -parse -verify %s

/// Good

struct Foo {
  @differentiable
  var x: Float
}

// expected-warning @+1 {{'jvp:' and 'vjp:' arguments in '@differentiable' attribute are deprecated}}
@differentiable(vjp: foo(_:_:)) // okay
func bar(_ x: Float, _: Float) -> Float {
  return 1 + x
}

// expected-warning @+1 {{'jvp:' and 'vjp:' arguments in '@differentiable' attribute are deprecated}}
@differentiable(vjp: foo(_:_:) where T : FloatingPoint) // okay
func bar<T : Numeric>(_ x: T, _: T) -> T {
    return 1 + x
}

// expected-warning @+1 {{'jvp:' and 'vjp:' arguments in '@differentiable' attribute are deprecated}}
@differentiable(wrt: (self, x, y), vjp: foo(_:_:)) // okay
func bar(_ x: Float, _ y: Float) -> Float {
  return 1 + x
}

// expected-warning @+1 2 {{'jvp:' and 'vjp:' arguments in '@differentiable' attribute are deprecated}}
@differentiable(wrt: (self, x, y), jvp: bar, vjp: foo(_:_:)) // okay
func bar(_ x: Float, _ y: Float) -> Float {
  return 1 + x
}

@differentiable // okay
func bar(_ x: Float, _: Float) -> Float {
  return 1 + x
}

@differentiable(wrt: x) // okay
func bar(_ x: Float, _: Float) -> Float {
  return 1 + x
}

@differentiable(wrt: (x)) // okay
func bar(_ x: Float, _: Float) -> Float {
  return 1 + x
}

@differentiable(wrt: self) // okay
func bar(_ x: Float, _: Float) -> Float {
  return 1 + x
}

@_transparent
@differentiable // okay
@inlinable
func playWellWithOtherAttrs(_ x: Float, _: Float) -> Float {
  return 1 + x
}

@_transparent
// expected-warning @+1 {{'jvp:' and 'vjp:' arguments in '@differentiable' attribute are deprecated}}
@differentiable(wrt: (self), vjp: _vjpSquareRoot) // okay
public func squareRoot() -> Self {
  var lhs = self
  lhs.formSquareRoot()
  return lhs
}

@differentiable(linear) // okay
func identity(_ x: Float) -> Float {
  return x
}

@differentiable(linear, wrt: x) // okay
func slope2(_ x: Float) -> Float {
  return 2 * x
}

@differentiable(wrt: y) // ok
func two(x: Float, y: Float) -> Float {
  return x + y
}

@differentiable(wrt: (x, y)) // ok
func two(x: Float, y: Float) -> Float {
  return x + y
}

@differentiable(wrt: (0, y)) // ok
func two(x: Float, y: Float) -> Float {
  return x + y
}

@differentiable(wrt: (x, 1)) // ok
func two(x: Float, y: Float) -> Float {
  return x + y
}

@differentiable(wrt: (0, 1)) // ok
func two(x: Float, y: Float) -> Float {
  return x + y
}

/// Bad

@differentiable(3) // expected-error {{expected either 'wrt:' or a function specifier label, e.g. 'jvp:', or 'vjp:'}}
func bar(_ x: Float, _: Float) -> Float {
  return 1 + x
}

@differentiable(foo(_:_:)) // expected-error {{expected either 'wrt:' or a function specifier label, e.g. 'jvp:', or 'vjp:'}}
func bar(_ x: Float, _: Float) -> Float {
  return 1 + x
}

// expected-warning @+1 {{'jvp:' and 'vjp:' arguments in '@differentiable' attribute are deprecated}}
@differentiable(vjp: foo(_:_:), 3) // expected-error {{expected either 'wrt:' or a function specifier label, e.g. 'jvp:', or 'vjp:'}}
func bar(_ x: Float, _: Float) -> Float {
  return 1 + x
}

@differentiable(wrt: (x), foo(_:_:)) // expected-error {{expected either 'wrt:' or a function specifier label, e.g. 'jvp:', or 'vjp:'}}
func bar(_ x: Float, _: Float) -> Float {
  return 1 + x
}

@differentiable(wrt: x, y) // expected-error {{expected either 'wrt:' or a function specifier label, e.g. 'jvp:', or 'vjp:'}}
func bar(_ x: Float, _ y: Float) -> Float {
  return 1 + x
}

@differentiable(wrt: 0, 1) // expected-error {{expected either 'wrt:' or a function specifier label, e.g. 'jvp:', or 'vjp:'}}
func two(x: Float, y: Float) -> Float {
  return x + y
}

@differentiable(wrt: 0, y) // expected-error {{expected either 'wrt:' or a function specifier label, e.g. 'jvp:', or 'vjp:'}}
func two(x: Float, y: Float) -> Float {
  return x + y
}

@differentiable(wrt: 0,) // expected-error {{unexpected ',' separator}}
func two(x: Float, y: Float) -> Float {
  return x + y
}

// expected-warning @+1 {{'jvp:' and 'vjp:' arguments in '@differentiable' attribute are deprecated}}
@differentiable(vjp: foo(_:_:) // expected-error {{expected ')' in 'differentiable' attribute}}
func bar(_ x: Float, _: Float) -> Float {
  return 1 + x
}

// expected-warning @+1 {{'jvp:' and 'vjp:' arguments in '@differentiable' attribute are deprecated}}
@differentiable(vjp: foo(_:_:) where T) // expected-error {{expected ':' or '==' to indicate a conformance or same-type requirement}}
func bar<T : Numeric>(_ x: T, _: T) -> T {
    return 1 + x
}

@differentiable(,) // expected-error {{expected either 'wrt:' or a function specifier label, e.g. 'jvp:', or 'vjp:'}}
func bar(_ x: Float, _: Float) -> Float {
  return 1 + x
}

// expected-warning @+1 {{'jvp:' and 'vjp:' arguments in '@differentiable' attribute are deprecated}}
@differentiable(vjp: foo(_:_:),) // expected-error {{unexpected ',' separator}}
func bar(_ x: Float, _: Float) -> Float {
  return 1 + x
}

// expected-warning @+1 {{'jvp:' and 'vjp:' arguments in '@differentiable' attribute are deprecated}}
@differentiable(vjp: foo(_:_:), where T) // expected-error {{unexpected ',' separator}}
func bar<T : Numeric>(_ x: T, _: T) -> T {
    return 1 + x
}

@differentiable(wrt: x, linear) // expected-error {{expected either 'wrt:' or a function specifier label, e.g. 'jvp:', or 'vjp:'}}
func slope4(_ x: Float) -> Float {
  return 4 * x
}

@differentiable(wrt: x, linear, vjp: const5) // expected-error {{expected either 'wrt:' or a function specifier label, e.g. 'jvp:', or 'vjp:'}}
func slope5(_ x: Float) -> Float {
  return 5 * x
}

// expected-warning @+1 {{'jvp:' and 'vjp:' arguments in '@differentiable' attribute are deprecated}}
@differentiable(wrt: x, vjp: const6, linear) // expected-error {{expected either 'wrt:' or a function specifier label, e.g. 'jvp:', or 'vjp:'}}
func slope5(_ x: Float) -> Float {
  return 6 * x
}

func localDifferentiableDeclaration() {
  // Okay
  @differentiable
  func foo1(_ x: Float) -> Float

  // Not okay. Derivative registration can only be non-local.
  // expected-warning @+2 {{'jvp:' and 'vjp:' arguments in '@differentiable' attribute are deprecated}}
  // expected-error @+1 {{attribute '@differentiable(jvp:vjp:)' can only be used in a non-local scope}}
  @differentiable(vjp: dfoo2)
  func foo2(_ x: Float) -> Float
}
