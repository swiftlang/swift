// RUN: %target-swift-frontend -parse -verify %s

/// Good

// expected-warning @+1 {{'@differentiable' has been renamed to '@differentiable(reverse)'}} {{16-16=(reverse)}}
@differentiable
// expected-warning @+1 {{'@differentiable' has been renamed to '@differentiable(reverse)'}} {{17-17=reverse, }}
@differentiable(wrt: x)
func please_use_reverse(_ x: Float, _ y: Float) -> Float { x }

struct Foo {
  @differentiable(reverse)
  var x: Float
}

@differentiable(reverse) // okay
func bar(_ x: Float, _: Float) -> Float {
  return 1 + x
}

@differentiable(reverse where T : FloatingPoint) // okay
func bar<T : Numeric>(_ x: T, _: T) -> T {
    return 1 + x
}

@differentiable(reverse, wrt: (self, x, y)) // okay
func bar(_ x: Float, _ y: Float) -> Float {
  return 1 + x
}

@differentiable(reverse) // okay
func bar(_ x: Float, _: Float) -> Float {
  return 1 + x
}

@differentiable(reverse, wrt: x) // okay
func bar(_ x: Float, _: Float) -> Float {
  return 1 + x
}

@differentiable(reverse, wrt: (x)) // okay
func bar(_ x: Float, _: Float) -> Float {
  return 1 + x
}

@differentiable(reverse, wrt: self) // okay
func bar(_ x: Float, _: Float) -> Float {
  return 1 + x
}

@_transparent
@differentiable(reverse) // okay
@inlinable
func playWellWithOtherAttrs(_ x: Float, _: Float) -> Float {
  return 1 + x
}

@_transparent
@differentiable(reverse, wrt: (self)) // okay
public func squareRoot() -> Self {
  var lhs = self
  lhs.formSquareRoot()
  return lhs
}

@differentiable(_linear) // okay
// expected-error @+1 {{unsupported differentiability kind '_forward'; only 'reverse' is supported}} {{17-25=reverse}}
@differentiable(_forward)
// expected-error @+1 {{unknown differentiability kind 'horse'; only 'reverse' is supported}} {{17-22=reverse}}
@differentiable(horse) // okay
func identity(_ x: Float) -> Float {
  return x
}

@differentiable(_linear, wrt: x) // okay
// expected-error @+1 {{unsupported differentiability kind '_forward'; only 'reverse' is supported}} {{17-25=reverse}}
@differentiable(_forward, wrt: x)
// expected-error @+1 {{unknown differentiability kind 'horse'; only 'reverse' is supported}} {{17-22=reverse}}
@differentiable(horse, wrt: x) // okay
func slope2(_ x: Float) -> Float {
  return 2 * x
}

@differentiable(reverse, wrt: y) // ok
func two(x: Float, y: Float) -> Float {
  return x + y
}

@differentiable(reverse, wrt: (x, y)) // ok
func two(x: Float, y: Float) -> Float {
  return x + y
}

@differentiable(reverse, wrt: (0, y)) // ok
func two(x: Float, y: Float) -> Float {
  return x + y
}

@differentiable(reverse, wrt: (x, 1)) // ok
func two(x: Float, y: Float) -> Float {
  return x + y
}

@differentiable(reverse, wrt: (0, 1)) // ok
func two(x: Float, y: Float) -> Float {
  return x + y
}

@differentiable(reverse, wrt: $x) // ok
func two(x: Float, y: Float) -> Float {
  return x + y
}

/// Bad

// expected-error @+1 {{expected 'wrt:' or 'where'}}
@differentiable(reverse, 3)
func bar(_ x: Float, _: Float) -> Float {
  return 1 + x
}

// expected-error @+1 {{expected 'wrt:' or 'where'}}
@differentiable(reverse, foo(_:_:))
func bar(_ x: Float, _: Float) -> Float {
  return 1 + x
}

// expected-error @+1 {{expected 'wrt:' or 'where'}}
@differentiable(reverse, wrt: (x), foo(_:_:))
func bar(_ x: Float, _: Float) -> Float {
  return 1 + x
}

// expected-error @+1 {{expected 'wrt:' or 'where'}}
@differentiable(reverse, wrt: x, y)
func bar(_ x: Float, _ y: Float) -> Float {
  return 1 + x
}

// expected-error @+1 {{expected 'wrt:' or 'where'}}
@differentiable(reverse, wrt: 0, 1)
func two(x: Float, y: Float) -> Float {
  return x + y
}

// expected-error @+1 {{expected 'wrt:' or 'where'}}
@differentiable(reverse, wrt: 0, y)
func two(x: Float, y: Float) -> Float {
  return x + y
}

// expected-error @+1 {{unexpected ',' separator}}
@differentiable(reverse, wrt: 0,)
func two(x: Float, y: Float) -> Float {
  return x + y
}

// expected-error @+1 {{expected ')' in 'differentiable' attribute}}
@differentiable(reverse, wrt: (x)
func bar(_ x: Float, _: Float) -> Float {
  return 1 + x
}

// expected-error @+1 {{expected ':' or '==' to indicate a conformance or same-type requirement}}
@differentiable(reverse, wrt: (x) where T)
func bar<T : Numeric>(_ x: T, _: T) -> T {
    return 1 + x
}

// expected-error @+1 {{expected 'wrt:' or 'where'}}
@differentiable(reverse, ,)
func bar(_ x: Float, _: Float) -> Float {
  return 1 + x
}

// expected-error @+1 {{unexpected ',' separator}}
@differentiable(reverse, wrt: (x),)
func bar(_ x: Float, _: Float) -> Float {
  return 1 + x
}

// expected-error @+1 {{unexpected ',' separator}}
@differentiable(reverse, wrt: (x), where T)
func bar<T : Numeric>(_ x: T, _: T) -> T {
    return 1 + x
}

// expected-error @+1 {{expected 'wrt:' or 'where'}}
@differentiable(reverse, wrt: x, linear)
func slope4(_ x: Float) -> Float {
  return 4 * x
}

// expected-error @+1 {{expected 'wrt:' or 'where'}}
@differentiable(reverse, wrt: x, linear)
func slope5(_ x: Float) -> Float {
  return 5 * x
}

// Test removed `jvp:' and 'vjp:' arguments.
// expected-error @+1 {{expected 'wrt:' or 'where' in '@differentiable' attribute}}
@differentiable(reverse, jvp: foo, vjp: foo)
func bar(_ x: Float, _: Float) -> Float {
  return 1 + x
}
