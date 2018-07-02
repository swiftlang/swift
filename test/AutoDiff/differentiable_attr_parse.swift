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

/// Bad

@differentiable(primal: bar) // expected-error {{expected a differentiation mode ('forward' or 'reverse')}}
func bar(_ x: Float, _: Float) -> Float {
  return 1 + x
}

@differentiable(reverse, 3) // expected-error {{missing label 'adjoint:' in '@differentiable' attribute}}
func bar(_ x: Float, _: Float) -> Float {
  return 1 + x
}

@differentiable(reverse, foo(_:_:)) // expected-error {{missing label 'adjoint:' in '@differentiable' attribute}}
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
