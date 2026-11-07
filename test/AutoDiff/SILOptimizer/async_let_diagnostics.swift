// RUN: %target-swift-frontend -emit-sil -verify %s
// REQUIRES: concurrency

import _Differentiation

// expected-error @+2 {{function is not differentiable}}
// expected-note @+2 {{when differentiating this function definition}}
@differentiable(reverse)
func scalarTangentTupleCapture(_ x: Float) async -> Float {
  let pair = (x, true)
  // expected-note @+1 {{expression is not differentiable}}
  async let result = pair.1 ? pair.0 * pair.0 : pair.0
  return await result
}

// expected-error @+2 {{function is not differentiable}}
// expected-note @+2 {{when differentiating this function definition}}
@differentiable(reverse)
func tupleResult(_ x: Float) async -> Float {
  // expected-note @+1 {{expression is not differentiable}}
  async let result = (x, x * x)
  let pair = await result
  return pair.0 + pair.1
}

// expected-error @+2 {{function is not differentiable}}
// expected-note @+2 {{when differentiating this function definition}}
@differentiable(reverse)
func nestedTupleResult(_ x: Float) async -> Float {
  // expected-note @+1 {{expression is not differentiable}}
  async let result = (x, (x * x, x))
  let pair = await result
  return pair.0 + pair.1.0 + pair.1.1
}

// expected-error @+2 {{function is not differentiable}}
// expected-note @+2 {{when differentiating this function definition}}
@differentiable(reverse)
func tupleCapture(_ x: Float) async -> Float {
  let pair = (x, x * x)
  // expected-note @+1 {{expression is not differentiable}}
  async let result = pair.0 * pair.1
  return await result
}

// expected-error @+2 {{function is not differentiable}}
// expected-note @+2 {{when differentiating this function definition}}
@differentiable(reverse)
func nestedTupleCapture(_ x: Float) async -> Float {
  let pair = (x, (x * x, x))
  // expected-note @+1 {{expression is not differentiable}}
  async let result = pair.0 * pair.1.0 * pair.1.1
  return await result
}
