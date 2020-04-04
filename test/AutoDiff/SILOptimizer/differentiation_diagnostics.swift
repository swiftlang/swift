// RUN: %target-swift-frontend -emit-sil -verify %s

// Test differentiation transform diagnostics.

import _Differentiation

//===----------------------------------------------------------------------===//
// Basic function
//===----------------------------------------------------------------------===//

@differentiable
func basic(_ x: Float) -> Float {
  return x + 2
}

//===----------------------------------------------------------------------===//
// Control flow
//===----------------------------------------------------------------------===//

@differentiable
func conditional(_ x: Float, _ flag: Bool) -> Float {
  let y: Float
  if flag {
    y = x + 1
  } else {
    y = x
  }
  return y
}

// TF-433: Test `try_apply` differentiation.

func throwing() throws -> Void {}

// expected-error @+2 {{function is not differentiable}}
// expected-note @+2 {{when differentiating this function definition}}
@differentiable
func try_apply(_ x: Float) -> Float {
  // expected-note @+1 {{cannot differentiate unsupported control flow}}
  try! throwing()
  return x
}

func rethrowing(_ x: () throws -> Void) rethrows -> Void {}

// expected-error @+2 {{function is not differentiable}}
// expected-note @+2 {{when differentiating this function definition}}
@differentiable
func try_apply_rethrows(_ x: Float) -> Float {
  // expected-note @+1 {{cannot differentiate unsupported control flow}}
  rethrowing({})
  return x
}

//===----------------------------------------------------------------------===//
// Unreachable
//===----------------------------------------------------------------------===//

let _: @differentiable (Float) -> Float = { x in
  let _ = x + 1
  // expected-error @+1 {{missing return in a closure expected to return 'Float'}}
}

//===----------------------------------------------------------------------===//
// Conversion to `@differentiable(linear)` (not yet supported)
//===----------------------------------------------------------------------===//

// expected-error @+1 {{conversion to '@differentiable(linear)' function type is not yet supported}}
let _: @differentiable(linear) (Float) -> Float = { x in x }
