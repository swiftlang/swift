// RUN: %target-swift-frontend -parse -verify %s

/// Good

@differentiable(reverse, adjoint: foo(_:_:)) // okay
func bar(_ x: Float, _: Float) -> Float {
  return 1 + x
}

@differentiable(reverse, adjoint: foo(_:_:) where T : FloatingPoint) // okay
func bar<T : Numeric>(_ x: T, _: T) -> T {
    return 1 + x
}

@differentiable(reverse, wrt: (self, .0, .1), adjoint: foo(_:_:)) // okay
func bar(_ x: Float, _: Float) -> Float {
  return 1 + x
}

@differentiable(reverse, wrt: (self, .0, .1), primal: bar, adjoint: foo(_:_:)) // okay
func bar(_ x: Float, _: Float) -> Float {
  return 1 + x
}

@differentiable(reverse) // okay
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
@differentiable(reverse, wrt: (self), adjoint: _adjointSquareRoot) // okay
public func squareRoot() -> Self {
  var lhs = self
  lhs.formSquareRoot()
  return lhs
}

/// Bad

@differentiable(primal: bar) // expected-error {{expected a differentiation mode ('forward' or 'reverse')}}
func bar(_ x: Float, _: Float) -> Float {
  return 1 + x
}

@differentiable(reverse, 3) // expected-error {{expected a configuration, e.g. 'wrt:', 'primal:' or 'adjoint:'}}
func bar(_ x: Float, _: Float) -> Float {
  return 1 + x
}

@differentiable(reverse, foo(_:_:)) // expected-error {{expected a configuration, e.g. 'wrt:', 'primal:' or 'adjoint:'}}
func bar(_ x: Float, _: Float) -> Float {
  return 1 + x
}

@differentiable(reverse, wrt: (1), adjoint: foo(_:_:)) // expected-error {{expected a parameter, which can be the index of a function parameter with a leading dot (e.g. '.0'), or 'self'}}
func bar(_ x: Float, _: Float) -> Float {
  return 1 + x
}

@differentiable(reverse, adjoint: foo(_:_:) // expected-error {{expected ')' in 'differentiable' attribute}}
func bar(_ x: Float, _: Float) -> Float {
  return 1 + x
}

@differentiable(reverse, adjoint: foo(_:_:) where T) // expected-error {{expected ':' or '==' to indicate a conformance or same-type requirement}}
func bar<T : Numeric>(_ x: T, _: T) -> T {
    return 1 + x
}
