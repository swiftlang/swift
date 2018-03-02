// RUN: %target-swift-frontend -parse -verify %s

/// Good

@differentiable(gradient: foo(_:_:)) // okay
func bar(_ x: Float, _: Float) -> Float {
  return 1 + x
}

@differentiable(gradient: foo(_:_:) where T : FloatingPoint) // okay
func bar<T : Numeric>(_ x: T, _: T) -> T {
    return 1 + x
}

@differentiable(withRespectTo: (self, .0, .1), gradient: foo(_:_:)) // okay
func bar(_ x: Float, _: Float) -> Float {
  return 1 + x
}

/// Bad

@differentiable(3) // expected-error {{missing label 'gradient:' in '@differentiable' attribute}}
func bar(_ x: Float, _: Float) -> Float {
  return 1 + x
}

@differentiable(foo(_:_:)) // expected-error {{missing label 'gradient:' in '@differentiable' attribute}}
func bar(_ x: Float, _: Float) -> Float {
  return 1 + x
}

@differentiable(withRespectTo: (1), gradient: foo(_:_:)) // expected-error {{expected an argument, which can be the index of a function argument with a leading dot (e.g. '.0'), or 'self'}}
func bar(_ x: Float, _: Float) -> Float {
  return 1 + x
}

@differentiable(gradient: foo(_:_:) // expected-error {{expected ')' in 'differentiable' attribute}}
func bar(_ x: Float, _: Float) -> Float {
  return 1 + x
}

@differentiable(gradient: foo(_:_:) where T) // expected-error {{expected ':' or '==' to indicate a conformance or same-type requirement}}
func bar<T : Numeric>(_ x: T, _: T) -> T {
    return 1 + x
}
