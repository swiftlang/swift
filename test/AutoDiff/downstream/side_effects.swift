// RUN: %target-swift-frontend -emit-sil -verify %s

func simpleStoreLoad(x: Float) -> Float {
  var y = x
  y = x + 1
  y = x + y
  return y
}
let _: @differentiable (Float) -> Float = simpleStoreLoad(x:)

var global: Float = 10

// Test differentiation of write to non-useful global variable.
let _: @differentiable (Float) -> Float = { x in
  global = x
  return x * x
}

// Test differentiation of write to non-useful local variable.
let _: @differentiable (Float) -> Float = { x in
  var local = x // expected-warning {{initialization of variable 'local' was never used}}
  return x + x
}

// Test differentiation of write to useful global variable.
// expected-error @+1 {{function is not differentiable}}
let _: @differentiable (Float) -> Float = { x in
  // expected-note @+1 {{cannot differentiate writes to global variables}}
  global = x
  return global + x
}

// Test differentiation of mutation to captured variables.
func testMutableCaptures() {
  var y: Float = 10
  // expected-error @+1 {{function is not differentiable}}
  let _: @differentiable (Float) -> Float = { x in
    // expected-note @+1 {{cannot differentiate writes to mutable captures}}
    y = x
    return y + x
  }
}

// Test differentiation of write to useful local variable.
let _: @differentiable (Float) -> Float = { x in
  var local = x // expected-warning {{variable 'local' was never mutated}}
  return local + x
}

// Test differentiation with partial application of @noescape closure.
// Addresses SR-9653.
func noEscapePartialApplyTest() {
  var y: Float = 0 // expected-warning {{variable 'y' was written to, but never read}}
  let _ = gradient(at: 0) { (x: Float) -> Float in
    y = x
    return x + x
  }
}

// TF-529: Crash when apply's result is active but arguments aren't.
struct TF_529_Vector<T: Numeric & Differentiable>: AdditiveArithmetic & Differentiable {
  var x, y: T
}

@differentiable
func TF_529<T>(x: TF_529_Vector<T>) -> TF_529_Vector<T> {
  var zero = TF_529_Vector<T>.zero
  zero = x
  return zero
}

// TODO: Add file checks.
