// RUN: %target-swift-frontend -emit-sil %s
// REQUIRES: asserts

// TF-1315: Pullback subset thunk generation crash due to unmapped parameter
// index for `inout` differentiability parameters.

import _Differentiation

func foo(_ x: Int, _ y: Float, _ z: inout Float) {}

@derivative(of: foo, wrt: (y, z))
func vjpFoo(_ x: Int, _ y: Float, _ z: inout Float) -> (
  value: Void, pullback: (inout Float) -> Float
) {
  fatalError()
}

@differentiable
func TF_1315(_ x: Float) -> Float {
  var x = x
  // The call to `foo` below triggers pullback subset parameter thunk generation.
  // `foo` original function type: `(Int, Float, inout Float) -> ()`
  //     Actual parameter indices: 1, 2
  //    Desired parameter indices: 2
  foo(1, 2, &x)
  return x
}
