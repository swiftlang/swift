// RUN: %target-swift-frontend -emit-sil -verify %s

func simpleStoreLoad(x: Float) -> Float {
  var y = x
  y = x + 1
  // expected-note @+1 {{expression is not differentiable}}
  return y
}
// expected-error @+1 {{function is not differentiable}}
let _: @autodiff (Float) -> Float = simpleStoreLoad(x:)

// TODO: Add file checks.
