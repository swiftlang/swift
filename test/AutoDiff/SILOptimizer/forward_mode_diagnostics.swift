// RUN: %target-swift-frontend -enable-experimental-forward-mode-differentiation -emit-sil -verify %s

// Test forward-mode differentiation transform diagnostics.

// TODO: Move these tests back into `autodiff_diagnostics.swift` once
// forward mode reaches feature parity with reverse mode.

import _Differentiation

//===----------------------------------------------------------------------===//
// Basic function
//===----------------------------------------------------------------------===//

@differentiable
func basic(_ x: Float) -> Float {
  return x
}

//===----------------------------------------------------------------------===//
// Control flow
//===----------------------------------------------------------------------===//

// expected-error @+1 {{function is not differentiable}}
@differentiable
// expected-note @+2 {{when differentiating this function definition}}
// expected-note @+1 {{forward-mode differentiation does not yet support control flow}}
func cond(_ x: Float) -> Float {
  if x > 0 {
    return x * x
  }
  return x + x
}

//===----------------------------------------------------------------------===//
// Non-varied results
//===----------------------------------------------------------------------===//

@differentiable
func nonVariedResult(_ x: Float) -> Float {
  // TODO(TF-788): Re-enable non-varied result warning.
  // xpected-warning @+1 {{result does not depend on differentiation arguments and will always have a zero derivative; do you want to use 'withoutDerivative(at:)'?}} {{10-10=withoutDerivative(at:}} {{15-15=)}}
  return 0
}

//===----------------------------------------------------------------------===//
// Multiple results
//===----------------------------------------------------------------------===//

// TODO(TF-983): Support differentiation of multiple results.
/*
func multipleResults(_ x: Float) -> (Float, Float) {
  return (x, x)
}
@differentiable
func usesMultipleResults(_ x: Float) -> Float {
  let tuple = multipleResults(x)
  return tuple.0 + tuple.1
}
*/

//===----------------------------------------------------------------------===//
// `inout` parameter differentiation
//===----------------------------------------------------------------------===//

// expected-error @+1 {{function is not differentiable}}
@differentiable
// expected-note @+1 {{when differentiating this function definition}}
func activeInoutParamNonactiveInitialResult(_ x: Float) -> Float {
  var result: Float = 1
  // expected-note @+1 {{cannot differentiate through 'inout' arguments}}
  result += x
  return result
}

// expected-error @+1 {{function is not differentiable}}
@differentiable
// expected-note @+1 {{when differentiating this function definition}}
func activeInoutParamTuple(_ x: Float) -> Float {
  var tuple = (x, x)
  // expected-note @+1 {{cannot differentiate through 'inout' arguments}}
  tuple.0 *= x
  return x * tuple.0
}

// expected-error @+1 {{function is not differentiable}}
@differentiable
// expected-note @+2 {{when differentiating this function definition}}
// expected-note @+1 {{forward-mode differentiation does not yet support control flow}}
func activeInoutParamControlFlow(_ array: [Float]) -> Float {
  var result: Float = 1
  for i in withoutDerivative(at: array).indices {
    result += array[i]
  }
  return result
}

struct Mut: Differentiable {}
extension Mut {
  @differentiable(wrt: x)
  mutating func mutatingMethod(_ x: Mut) {}
}

// FIXME(TF-984): Forward-mode crash due to unset tangent buffer.
/*
@differentiable(wrt: x)
func nonActiveInoutParam(_ nonactive: inout Mut, _ x: Mut) -> Mut {
  return nonactive.mutatingMethod(x)
}
*/

// FIXME(TF-984): Forward-mode crash due to unset tangent buffer.
/*
@differentiable(wrt: x)
func activeInoutParamMutatingMethod(_ x: Mut) -> Mut {
  var result = x
  result = result.mutatingMethod(result)
  return result
}
*/

// FIXME(TF-984): Forward-mode crash due to unset tangent buffer.
/*
@differentiable(wrt: x)
func activeInoutParamMutatingMethodVar(_ nonactive: inout Mut, _ x: Mut) -> Mut {
  var result = nonactive
  result = result.mutatingMethod(x)
  return result
}
*/

// FIXME(TF-984): Forward-mode crash due to unset tangent buffer.
/*
@differentiable(wrt: x)
func activeInoutParamMutatingMethodTuple(_ nonactive: inout Mut, _ x: Mut) -> Mut {
  var result = (nonactive, x)
  let result2 = result.0.mutatingMethod(result.0)
  return result2
}
*/

//===----------------------------------------------------------------------===//
// Subset parameter differentiation thunks
//===----------------------------------------------------------------------===//

// FIXME(SR-13046): Non-differentiability diagnostic crash due to invalid source location.
/*
func testNoDerivativeParameter(_ f: @differentiable (Float, @noDerivative Float) -> Float) -> Float {
  return derivative(at: 2, 3) { (x, y) in f(x * x, y) }
}
*/
