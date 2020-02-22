// RUN: %target-swift-frontend -enable-experimental-forward-mode-differentiation -emit-sil -verify %s

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
  // expected-error @+2 {{expression is not differentiable}}
  // expected-note @+1 {{cannot differentiate through a non-differentiable result; do you want to use 'withoutDerivative(at:)'?}}
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
// Multiple results
//===----------------------------------------------------------------------===//

func multipleResults(_ x: Float) -> (Float, Float) {
  return (x, x)
}
// expected-error @+1 {{function is not differentiable}}
@differentiable
// expected-note @+1 {{when differentiating this function definition}}
func usesMultipleResults(_ x: Float) -> Float {
  // expected-note @+1 {{cannot differentiate through multiple results}}
  let tuple = multipleResults(x)
  return tuple.0 + tuple.1
}

//===----------------------------------------------------------------------===//
// Inout arguments
//===----------------------------------------------------------------------===//

// expected-error @+1 {{function is not differentiable}}
@differentiable
// expected-note @+1 {{when differentiating this function definition}}
func activeInoutArgNonactiveInitialResult(_ x: Float) -> Float {
  var result: Float = 1
  // expected-note @+1 {{cannot differentiate through 'inout' arguments}}
  result += x
  return result
}

// expected-error @+1 {{function is not differentiable}}
@differentiable
// expected-note @+1 {{when differentiating this function definition}}
func activeInoutArgTuple(_ x: Float) -> Float {
  var tuple = (x, x)
  // expected-note @+1 {{cannot differentiate through 'inout' arguments}}
  tuple.0 *= x
  return x * tuple.0
}

// expected-error @+1 {{function is not differentiable}}
@differentiable
// expected-note @+2 {{when differentiating this function definition}}
// expected-note @+1 {{forward-mode differentiation does not yet support control flow}}
func activeInoutArgControlFlow(_ array: [Float]) -> Float {
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
func nonActiveInoutArg(_ nonactive: inout Mut, _ x: Mut) -> Mut {
  return nonactive.mutatingMethod(x)
}
*/

// FIXME(TF-984): Forward-mode crash due to unset tangent buffer.
/*
@differentiable(wrt: x)
func activeInoutArgMutatingMethod(_ x: Mut) -> Mut {
  var result = x
  result = result.mutatingMethod(result)
  return result
}
*/

// FIXME(TF-984): Forward-mode crash due to unset tangent buffer.
/*
@differentiable(wrt: x)
func activeInoutArgMutatingMethodVar(_ nonactive: inout Mut, _ x: Mut) -> Mut {
  var result = nonactive
  result = result.mutatingMethod(x)
  return result
}
*/

// FIXME(TF-984): Forward-mode crash due to unset tangent buffer.
/*
@differentiable(wrt: x)
func activeInoutArgMutatingMethodTuple(_ nonactive: inout Mut, _ x: Mut) -> Mut {
  var result = (nonactive, x)
  let result2 = result.0.mutatingMethod(result.0)
  return result2
}
*/

//===----------------------------------------------------------------------===//
// Non-varied results
//===----------------------------------------------------------------------===//

func one() -> Float {
  return 1
}
@differentiable
func nonVariedResult(_ x: Float) -> Float {
  // TODO(TF-788): Re-enable non-varied result warning.
  // xpected-warning @+1 2 {{result does not depend on differentiation arguments and will always have a zero derivative; do you want to use 'withoutDerivative(at:)'?}} {{10-10=withoutDerivative(at:}}
  return one()
}

//===----------------------------------------------------------------------===//
// Subset parameters
//===----------------------------------------------------------------------===//

func nondiff(_ f: @differentiable (Float, @noDerivative Float) -> Float) -> Float {
  // expected-note @+2 {{cannot differentiate with respect to a '@noDerivative' parameter}}
  // expected-error @+1 {{function is not differentiable}}
  return derivative(at: 2, 3) { (x, y) in f(x * x, y) }
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
