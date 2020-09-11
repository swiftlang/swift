// RUN: %target-swift-frontend -emit-sil -verify %s

// This test file contains SIL diagnostics tests for differentiable functions
// such as escaping capture errors.
// NOTE: Only add tests for errors that would occur after the differentiation
// transform.

func nonescapingArgument(f: @differentiable (Float, Float) -> Float) -> Float {
  return gradient(at: 1) { x in f(x, x) }
}

// expected-note @+2 {{parameter 'f' is implicitly non-escaping}}
func nonescapingArgumentError(
  f: @differentiable (Float, Float) -> Float
) -> @differentiable (Float) -> Float{
  // expected-error @+2 {{escaping closure captures non-escaping parameter 'f'}}
  // expected-note @+1 {{captured here}}
  return { x in f(x, x) }
}
