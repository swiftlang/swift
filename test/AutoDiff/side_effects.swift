// RUN: %target-swift-frontend -emit-sil -verify %s

func simpleStoreLoad(x: Float) -> Float {
  var y = x
  y = x + 1
  // expected-error @+1 {{expression is not differentiable}}
  return y
}
let _: @autodiff (Float) -> Float = simpleStoreLoad(x:)

// TODO: Add file checks.
