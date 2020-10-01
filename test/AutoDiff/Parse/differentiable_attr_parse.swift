// RUN: %target-swift-frontend -parse -verify %s

/// Good

struct Foo {
  @differentiable
  var x: Float
}

@differentiable // okay
func bar(_ x: Float, _: Float) -> Float {
  return 1 + x
}

@differentiable(where T : FloatingPoint) // okay
func bar<T : Numeric>(_ x: T, _: T) -> T {
    return 1 + x
}

@differentiable(wrt: (self, x, y)) // okay
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
@differentiable(wrt: (self)) // okay
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

// expected-error @+1 {{expected 'wrt:' or 'where'}}
@differentiable(3)
func bar(_ x: Float, _: Float) -> Float {
  return 1 + x
}

// expected-error @+1 {{expected 'wrt:' or 'where'}}
@differentiable(foo(_:_:))
func bar(_ x: Float, _: Float) -> Float {
  return 1 + x
}

// expected-error @+1 {{expected 'wrt:' or 'where'}}
@differentiable(wrt: (x), foo(_:_:))
func bar(_ x: Float, _: Float) -> Float {
  return 1 + x
}

// expected-error @+1 {{expected 'wrt:' or 'where'}}
@differentiable(wrt: x, y)
func bar(_ x: Float, _ y: Float) -> Float {
  return 1 + x
}

// expected-error @+1 {{expected 'wrt:' or 'where'}}
@differentiable(wrt: 0, 1)
func two(x: Float, y: Float) -> Float {
  return x + y
}

// expected-error @+1 {{expected 'wrt:' or 'where'}}
@differentiable(wrt: 0, y)
func two(x: Float, y: Float) -> Float {
  return x + y
}

// expected-error @+1 {{unexpected ',' separator}}
@differentiable(wrt: 0,)
func two(x: Float, y: Float) -> Float {
  return x + y
}

// expected-error @+1 {{expected ')' in 'differentiable' attribute}}
@differentiable(wrt: (x)
func bar(_ x: Float, _: Float) -> Float {
  return 1 + x
}

// expected-error @+1 {{expected ':' or '==' to indicate a conformance or same-type requirement}}
@differentiable(wrt: (x) where T)
func bar<T : Numeric>(_ x: T, _: T) -> T {
    return 1 + x
}

// expected-error @+1 {{expected 'wrt:' or 'where'}}
@differentiable(,)
func bar(_ x: Float, _: Float) -> Float {
  return 1 + x
}

// expected-error @+1 {{unexpected ',' separator}}
@differentiable(wrt: (x),)
func bar(_ x: Float, _: Float) -> Float {
  return 1 + x
}

// expected-error @+1 {{unexpected ',' separator}}
@differentiable(wrt: (x), where T)
func bar<T : Numeric>(_ x: T, _: T) -> T {
    return 1 + x
}

// expected-error @+1 {{expected 'wrt:' or 'where'}}
@differentiable(wrt: x, linear)
func slope4(_ x: Float) -> Float {
  return 4 * x
}

// expected-error @+1 {{expected 'wrt:' or 'where'}}
@differentiable(wrt: x, linear)
func slope5(_ x: Float) -> Float {
  return 5 * x
}

// Test removed `jvp:' and 'vjp:' arguments.
// expected-error @+1 {{expected 'wrt:' or 'where' in '@differentiable' attribute}}
@differentiable(jvp: foo, vjp: foo)
func bar(_ x: Float, _: Float) -> Float {
  return 1 + x
}
