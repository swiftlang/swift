// RUN: %target-swift-frontend -emit-sil -verify %s

import _Differentiation

// Nil coalescing overload accepting non-throwing autoclosure
func ??(_ x: Float?, _ y: @autoclosure () -> Float) -> Float {
  if x == nil {
    return y()
  }
  return x!
}

// expected-error @+1 {{function is not differentiable}}
@differentiable(reverse)
// expected-note @+1 {{when differentiating this function definition}}
func errorInoutAlias(ff: F) -> Float {
  var y = ff.i?.first { $0 >= 0.0 } ?? 0.0
  while 0.0 < y {
    // As for now, we do not support differentiation here since `y` is passed as `inout_alias`.
    // Currently we only support arguments with Object value category.
    // TODO: support captured argument with Address value category.
    // expected-note @+1 {{expression is not differentiable}}
    y = ff.g() ?? y
  }
  return y
}

public struct F: Differentiable {
  @noDerivative var i: [Float]? {return nil}
  func g() -> Float? {return nil}
}

// TODO: support closures capturing multiple arguments
// expected-error @+1 {{function is not differentiable}}
@differentiable(reverse)
// expected-note @+1 {{when differentiating this function definition}}
func errorManyArgs(_ x: Float?, _ a: Float, _ b: Float) -> Float {
  // expected-note @+1 {{expression is not differentiable}}
  return x ?? a + b
}
