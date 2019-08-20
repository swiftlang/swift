// RUN: %target-swift-frontend -Xllvm -run-jvp-generation -emit-sil -verify %s

// TODO: move these tests back into `autodiff_diagnostics.swift` once
// forward mode reaches feature parity with reverse mode.

//===----------------------------------------------------------------------===//
// Basic function
//===----------------------------------------------------------------------===//

func one_to_one_0(_ x: Float) -> Float {
  return x + 2
}

_ = derivative(at: 0, in: one_to_one_0) // okay!

//===----------------------------------------------------------------------===//
// Function composition
//===----------------------------------------------------------------------===//

func base(_ x: Float) -> Float {
  // expected-error @+2 2 {{expression is not differentiable}}
  // expected-note @+1 2 {{cannot differentiate through a non-differentiable result; do you want to use 'withoutDerivative(at:)'?}}
  return Float(Int(x))
}

// TODO: Fix nested differentiation diagnostics. Need to fix indirect differentiation invokers.
func nested(_ x: Float) -> Float {
  // xpected-note @+1 {{when differentiating this function call}}
  return base(x)
}

func middle(_ x: Float) -> Float {
  // xpected-note @+1 {{when differentiating this function call}}
  return nested(x)
}

func middle2(_ x: Float) -> Float {
  // xpected-note @+1 {{when differentiating this function call}}
  return middle(x)
}

func func_to_diff(_ x: Float) -> Float {
  // xpected-note @+1 {{expression is not differentiable}}
  return middle2(x)
}

func calls_diff_of_nested(_ x: Float) -> Float {
  // xpected-error @+1 {{function is not differentiable}}
  return derivative(at: x, in: func_to_diff)
}

//===----------------------------------------------------------------------===//
// Inout arguments
//===----------------------------------------------------------------------===//

func activeInoutArg(_ x: Float) -> Float {
  var a = x
  // expected-note @+1 {{cannot differentiate through 'inout' arguments}}
  a += x
  return a
}
// expected-error @+1 {{function is not differentiable}}
_ = differential(at: .zero, in: activeInoutArg(_:))

func activeInoutArgTuple(_ x: Float) -> Float {
  var tuple = (x, x)
  // expected-note @+1 {{cannot differentiate through 'inout' arguments}}
  tuple.0 *= x
  return x * tuple.0
}
// expected-error @+1 {{function is not differentiable}}
_ = differential(at: .zero, in: activeInoutArgTuple(_:))

//===----------------------------------------------------------------------===//
// Non-varied results
//===----------------------------------------------------------------------===//

func one() -> Float {
  return 1
}
@differentiable
func nonVariedResult(_ x: Float) -> Float {
  // expected-warning @+1 2 {{result does not depend on differentiation arguments and will always have a zero derivative; do you want to use 'withoutDerivative(at:)'?}} {{10-10=withoutDerivative(at:}}
  return one()
}

//===----------------------------------------------------------------------===//
// Subset parameters
//===----------------------------------------------------------------------===//

func nondiff(_ f: @differentiable (Float, @nondiff Float) -> Float) -> Float {
  // expected-note @+2 {{cannot differentiate with respect to a '@nondiff' parameter}}
  // expected-error @+1 {{function is not differentiable}}
  return derivative(at: 2, 3) { (x, y) in f(x * x, y) }
}
