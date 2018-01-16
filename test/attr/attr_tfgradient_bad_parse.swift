// RUN: %target-swift-frontend -parse -verify %s

import Foundation

/// Good

@differentiable(gradient: foo(_:_:)) // okay
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

@differentiable(gradient: foo(_:_:) // expected-error {{expected ')' in 'differentiable' attribute}}
func bar(_ x: Float, _: Float) -> Float {
  return 1 + x
}
