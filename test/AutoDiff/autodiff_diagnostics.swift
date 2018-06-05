// RUN: %target-swift-frontend -emit-sil -verify %s

func foo(_ f: @escaping (Float) -> Float) -> Float {
  return #gradient(f)(0) // expected-error {{differentiating an opaque function is not supported yet}}

// func foo(_ f: @escaping (Float) -> Float) -> Float {
//   return #gradient(of: f)(0) // expected-error {{differentiating an opaque function is not supported yet}}
// }

func f0(_ x: Float) -> Float {
  return x
}

func nested(_ x: Float) -> Float {
  return #gradient(f0)(x) // expected-note {{nested differentiation is not supported yet}}
}

func loadstore(_ x: Float) -> Float {
  var maybe: Float = 10
  maybe = x
  return maybe
}

func middle(_ x: Float) -> Float {
  let y = loadstore(x)
  return nested(y) // expected-note {{when differentiating this function call}}
}

func middle2(_ x: Float) -> Float {
  return middle(x) // expected-note {{when differentiating this function call}}
}

func func_to_diff(_ x: Float) -> Float {
  return middle2(x) // expected-note {{when differentiating this function call}}
}

func calls_grad_of_nested(_ x: Float) -> Float {
  return #gradient(func_to_diff)(x) // expected-error {{applying differential operator on a non-differentiable function}}
}
