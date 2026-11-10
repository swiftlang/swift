// RUN: %target-swift-frontend -parse -verify %s

/// Good

// expected-cparser-warning @+1 {{'@differentiable' has been renamed to '@differentiable(reverse)'}} {{16-16=(reverse)}} expected-astgen-warning@+1 {{'@differentiable' has been renamed to '@differentiable(reverse)'}}
@differentiable // expected-astgen-error {{expected '(' in attribute}} expected-astgen-error {{expected argument for '@differentiable' attribute}} expected-astgen-error {{expected ')' to end attribute}}
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
// expected-cparser-error @+1 {{unsupported differentiability kind '_forward'; only 'reverse' is supported}} {{17-25=reverse}} expected-astgen-error@+1 {{unsupported differentiability kind '_forward'; only 'reverse' is supported}}
@differentiable(_forward)
// expected-error @+1 {{unknown differentiability kind 'horse'; only 'reverse' is supported}} {{17-22=reverse}}
@differentiable(horse) // okay
func identity(_ x: Float) -> Float {
  return x
}

@differentiable(_linear, wrt: x) // okay
// expected-cparser-error @+1 {{unsupported differentiability kind '_forward'; only 'reverse' is supported}} {{17-25=reverse}} expected-astgen-error@+1 {{unsupported differentiability kind '_forward'; only 'reverse' is supported}}
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

// expected-cparser-error @+1 {{expected 'wrt:' or 'where'}} expected-astgen-error@+1 {{unexpected code '3' in attribute}}
@differentiable(reverse, 3)
func bar(_ x: Float, _: Float) -> Float {
  return 1 + x
}

// expected-cparser-error @+1 {{expected 'wrt:' or 'where'}} expected-astgen-error@+1 {{unexpected code 'foo(_:_:)' in attribute}}
@differentiable(reverse, foo(_:_:))
func bar(_ x: Float, _: Float) -> Float {
  return 1 + x
}

// expected-cparser-error @+1 {{expected 'wrt:' or 'where'}} expected-astgen-error@+1 {{unexpected code 'foo(_:_:)' in attribute}}
@differentiable(reverse, wrt: (x), foo(_:_:))
func bar(_ x: Float, _: Float) -> Float {
  return 1 + x
}

// expected-cparser-error @+1 {{expected 'wrt:' or 'where'}} expected-astgen-error@+1 {{unexpected code 'y' in attribute}}
@differentiable(reverse, wrt: x, y) // expected-astgen-error {{unexpected ',' separator}}
func bar(_ x: Float, _ y: Float) -> Float {
  return 1 + x
}

// expected-cparser-error @+1 {{expected 'wrt:' or 'where'}} expected-astgen-error@+1 {{unexpected code '1' in attribute}}
@differentiable(reverse, wrt: 0, 1) // expected-astgen-error {{unexpected ',' separator}}
func two(x: Float, y: Float) -> Float {
  return x + y
}

// expected-cparser-error @+1 {{expected 'wrt:' or 'where'}} expected-astgen-error@+1 {{unexpected code 'y' in attribute}}
@differentiable(reverse, wrt: 0, y) // expected-astgen-error {{unexpected ',' separator}}
func two(x: Float, y: Float) -> Float {
  return x + y
}

// expected-error @+1 {{unexpected ',' separator}} {{32-33=}}
@differentiable(reverse, wrt: 0,) //
func two(x: Float, y: Float) -> Float {
  return x + y
}

// expected-cparser-error @+1 {{expected ')' in 'differentiable' attribute}} expected-astgen-error@+1 {{expected ')' to end attribute}}
@differentiable(reverse, wrt: (x) // expected-astgen-note {{to match this opening '('}}
func bar(_ x: Float, _: Float) -> Float {
  return 1 + x
}

// expected-error @+1 {{expected ':' or '==' to indicate a conformance or same-type requirement}}
@differentiable(reverse, wrt: (x) where T)
func bar<T : Numeric>(_ x: T, _: T) -> T {
    return 1 + x
}

// expected-cparser-error @+1 {{expected 'wrt:' or 'where'}} expected-astgen-error@+1 {{unexpected code ',' in attribute}}
@differentiable(reverse, ,)
func bar(_ x: Float, _: Float) -> Float {
  return 1 + x
}

// expected-cparser-error @+1 {{unexpected ',' separator}} {{34-35=}}
@differentiable(reverse, wrt: (x),)
func bar(_ x: Float, _: Float) -> Float {
  return 1 + x
}

// expected-cparser-error @+1 {{unexpected ',' separator}} {{34-35=}} expected-astgen-error@+1 {{expected ':' or '==' to indicate a conformance or same-type requirement}}
@differentiable(reverse, wrt: (x), where T)
func bar<T : Numeric>(_ x: T, _: T) -> T {
    return 1 + x
}

// expected-cparser-error @+1 {{expected 'wrt:' or 'where'}} expected-astgen-error@+1 {{unexpected code 'linear' in attribute}}
@differentiable(reverse, wrt: x, linear) // expected-astgen-error {{unexpected ',' separator}}
func slope4(_ x: Float) -> Float {
  return 4 * x
}

// expected-cparser-error @+1 {{expected 'wrt:' or 'where'}} expected-astgen-error@+1 {{unexpected code 'linear' in attribute}}
@differentiable(reverse, wrt: x, linear) // expected-astgen-error {{unexpected ',' separator}}
func slope5(_ x: Float) -> Float {
  return 5 * x
}

// Test removed `jvp:' and 'vjp:' arguments.
// expected-cparser-error @+1 {{expected 'wrt:' or 'where' in '@differentiable' attribute}} expected-astgen-error@+1 {{unexpected code 'jvp: foo, vjp: foo' in attribute}}
@differentiable(reverse, jvp: foo, vjp: foo)
func bar(_ x: Float, _: Float) -> Float {
  return 1 + x
}
