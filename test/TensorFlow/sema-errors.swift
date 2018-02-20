// RUN: %target-swift-frontend -O -emit-sil -verify %s

// This file contains tests that produce errors in the semantic analysis pass
// or earlier.  These have to be split from the other SIL tests because if there
// is a type checking error, then the SIL passes aren't run.

import TensorFlow


// Make sure we cleanly diagnose this.
public func testAmbiguous() {
  #tfop("foo")  // expected-error {{type of expression is ambiguous without more context}}
}

public func testExpressibleByLiteral() {
  let _: Tensor<Int64> = 1 // ok
  let _: Tensor<Float> = 3.333333 // ok
  let _: Tensor<Bool> = false // ok
  let _: Tensor<Int32> = [1, 2, 3, 4] // ok
  let _: Tensor<Float> = [1, 2.0, 3, 4] // ok
  let _: Tensor<Bool> = [[[true, false, false, true]]] // ok
  let _: Tensor<Float> = [[[true, false, false, true]]] // expected-error {{cannot convert value of type 'Bool' to expected element type 'Tensor<Float>'}}
  let _: Tensor<Float> = Tensor([[[true, false, false, true]]]) // ok
}
