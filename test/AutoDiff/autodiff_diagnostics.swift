// RUN: %target-swift-frontend -emit-sil -verify %s

//===----------------------------------------------------------------------===//
// Top-level (before primal/adjoint synthesis)
//===----------------------------------------------------------------------===//

// expected-note @+1 {{value defined here}}
func foo(_ f: (Float) -> Float) -> Float {
  // expected-error @+1 {{differentiating an opaque function is not supported yet}}
  return #gradient(f)(0)
}

//===----------------------------------------------------------------------===//
// Basic function
//===----------------------------------------------------------------------===//

func one_to_one_0(_ x: Float) -> Float {
  return x + 2
}

_ = #gradient(one_to_one_0) // okay!

//===----------------------------------------------------------------------===//
// Generics
//===----------------------------------------------------------------------===//

// expected-note @+3 {{differentiating generic functions is not supported yet}}
// expected-error @+2 {{function is not differentiable}}
@differentiable(reverse)
func generic<T: FloatingPoint>(_ x: T) -> T {
  return x + 1
}

//===----------------------------------------------------------------------===//
// Function composition
//===----------------------------------------------------------------------===//

func uses_optionals(_ x: Float) -> Float {
  var maybe: Float? = 10
  maybe = x
  // expected-note @+1 {{differentiating control flow is not supported yet}}
  return maybe!
}

_ = #gradient(uses_optionals) // expected-error {{function is not differentiable}}

func f0(_ x: Float) -> Float {
  return x // okay!
}

func nested(_ x: Float) -> Float {
  return #gradient(f0)(x) // expected-note {{nested differentiation is not supported yet}}
}

func middle(_ x: Float) -> Float {
  let y = uses_optionals(x)
  return nested(y) // expected-note {{when differentiating this function call}}
}

func middle2(_ x: Float) -> Float {
  return middle(x) // expected-note {{when differentiating this function call}}
}

func func_to_diff(_ x: Float) -> Float {
  return middle2(x) // expected-note {{expression is not differentiable}}
}

func calls_grad_of_nested(_ x: Float) -> Float {
  return #gradient(func_to_diff)(x) // expected-error {{function is not differentiable}}
}

//===----------------------------------------------------------------------===//
// Control flow
//===----------------------------------------------------------------------===//

func if_else(_ x: Float, _ flag: Bool) -> Float {
  let y: Float
  // expected-note @+1 {{differentiating control flow is not supported yet}}
  if flag {
    y = x + 1
  } else {
    y = x
  }
  return y
}

// expected-error @+1 {{function is not differentiable}}
_ = #gradient(if_else, wrt: .0)
