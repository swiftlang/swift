// RUN: %target-swift-frontend -emit-sil -verify %s

func simpleStoreLoad(x: Float) -> Float {
  var y = x
  y = x + 1
  // expected-note @+1 {{expression is not differentiable}}
  return y
}
// expected-error @+1 {{function is not differentiable}}
let _: @autodiff (Float) -> Float = simpleStoreLoad(x:)

var global: Float = 10

// Test differentiation of write to non-useful global variable.
let _: @autodiff (Float) -> Float = { x in
  global = x
  return x * x
}

// Test differentiation of write to non-useful local variable.
let _: @autodiff (Float) -> Float = { x in
  var local = x // expected-warning {{initialization of variable 'local' was never used}}
  return x + x
}

// Test differentiation of write to useful global variable.
// expected-error @+1 {{function is not differentiable}}
let _: @autodiff (Float) -> Float = { x in
  global = x
  // expected-note @+1 {{expression is not differentiable}}
  return global + x
}

// Test differentiation of write to useful local variable.
// expected-error @+1 {{function is not differentiable}}
let _: @autodiff (Float) -> Float = { x in
  var local = x // expected-warning {{variable 'local' was never mutated}}
  // expected-note @+1 {{expression is not differentiable}}
  return local + x
}

// TODO: Add file checks.
