// RUN: %target-swift-frontend -parse -verify %s

// TODO(TF-1021): Remove "deprecated 'jvp:' and 'vjp:' argument" warnings.

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
@differentiable(vjp: foo(_:_:)) // okay
// expected-warning @+1 {{'jvp:' and 'vjp:' arguments in '@differentiable' attribute are deprecated}}
@differentiable(vjp: foo(_:_:) where T : FloatingPoint) // okay
func bar<T : Numeric>(_ x: T, _: T) -> T {
    return 1 + x
}

// expected-warning @+1 {{'jvp:' and 'vjp:' arguments in '@differentiable' attribute are deprecated}}
@differentiable(vjp: foo(_:_:)) // okay
// expected-warning @+1 {{'jvp:' and 'vjp:' arguments in '@differentiable' attribute are deprecated}}
@differentiable(wrt: (self, x, y), vjp: foo(_:_:)) // okay
func bar(_ x: Float, _ y: Float) -> Float {
  return 1 + x
}

// expected-warning @+1 {{'jvp:' and 'vjp:' arguments in '@differentiable' attribute are deprecated}}
@differentiable(vjp: foo(_:_:)) // okay
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

// expected-error @+1 {{expected either 'wrt:' or a function specifier label, e.g. 'jvp:', or 'vjp:'}}
@differentiable(3)
func bar(_ x: Float, _: Float) -> Float {
  return 1 + x
}

// expected-error @+1 {{expected either 'wrt:' or a function specifier label, e.g. 'jvp:', or 'vjp:'}}
@differentiable(foo(_:_:))
func bar(_ x: Float, _: Float) -> Float {
  return 1 + x
}

// expected-warning @+2 {{'jvp:' and 'vjp:' arguments in '@differentiable' attribute are deprecated}}
// expected-error @+1 {{expected either 'wrt:' or a function specifier label, e.g. 'jvp:', or 'vjp:'}}
@differentiable(vjp: foo(_:_:), 3)
func bar(_ x: Float, _: Float) -> Float {
  return 1 + x
}

// expected-error @+1 {{expected either 'wrt:' or a function specifier label, e.g. 'jvp:', or 'vjp:'}}
@differentiable(wrt: (x), foo(_:_:))
func bar(_ x: Float, _: Float) -> Float {
  return 1 + x
}

// expected-error @+1 {{expected either 'wrt:' or a function specifier label, e.g. 'jvp:', or 'vjp:'}}
@differentiable(wrt: x, y)
func bar(_ x: Float, _ y: Float) -> Float {
  return 1 + x
}

// expected-error @+1 {{expected either 'wrt:' or a function specifier label, e.g. 'jvp:', or 'vjp:'}}
@differentiable(wrt: 0, 1)
func two(x: Float, y: Float) -> Float {
  return x + y
}

// expected-error @+1 {{expected either 'wrt:' or a function specifier label, e.g. 'jvp:', or 'vjp:'}}
@differentiable(wrt: 0, y)
func two(x: Float, y: Float) -> Float {
  return x + y
}

// expected-error @+1 {{unexpected ',' separator}}
@differentiable(wrt: 0,)
func two(x: Float, y: Float) -> Float {
  return x + y
}

// expected-warning @+2 {{'jvp:' and 'vjp:' arguments in '@differentiable' attribute are deprecated}}
// expected-error @+1 {{expected ')' in 'differentiable' attribute}}
@differentiable(vjp: foo(_:_:)
func bar(_ x: Float, _: Float) -> Float {
  return 1 + x
}

// expected-warning @+2 {{'jvp:' and 'vjp:' arguments in '@differentiable' attribute are deprecated}}
// expected-error @+1 {{expected ':' or '==' to indicate a conformance or same-type requirement}}
@differentiable(vjp: foo(_:_:) where T)
func bar<T : Numeric>(_ x: T, _: T) -> T {
    return 1 + x
}

// expected-error @+1 {{expected either 'wrt:' or a function specifier label, e.g. 'jvp:', or 'vjp:'}}
@differentiable(,)
func bar(_ x: Float, _: Float) -> Float {
  return 1 + x
}

// expected-warning @+2 {{'jvp:' and 'vjp:' arguments in '@differentiable' attribute are deprecated}}
// expected-error @+1 {{unexpected ',' separator}}
@differentiable(vjp: foo(_:_:),)
func bar(_ x: Float, _: Float) -> Float {
  return 1 + x
}

// expected-warning @+2 {{'jvp:' and 'vjp:' arguments in '@differentiable' attribute are deprecated}}
// expected-error @+1 {{unexpected ',' separator}}
@differentiable(vjp: foo(_:_:), where T)
func bar<T : Numeric>(_ x: T, _: T) -> T {
    return 1 + x
}

// expected-error @+1 {{expected either 'wrt:' or a function specifier label, e.g. 'jvp:', or 'vjp:'}}
@differentiable(wrt: x, linear)
func slope4(_ x: Float) -> Float {
  return 4 * x
}

// expected-error @+1 {{expected either 'wrt:' or a function specifier label, e.g. 'jvp:', or 'vjp:'}}
@differentiable(wrt: x, linear, vjp: const5)
func slope5(_ x: Float) -> Float {
  return 5 * x
}

// expected-warning @+2 {{'jvp:' and 'vjp:' arguments in '@differentiable' attribute are deprecated}}
// expected-error @+1 {{expected either 'wrt:' or a function specifier label, e.g. 'jvp:', or 'vjp:'}}
@differentiable(wrt: x, vjp: const6, linear)
func slope5(_ x: Float) -> Float {
  return 6 * x
}
