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
  let _: Tensor<Int32> = [1, 2, 3, 4] // ok
  let _: Tensor<Float> = [1, 2.0, 3, 4] // ok
  let _: Tensor<Bool> = [[[true, false, false, true]]] // ok
  let _: Tensor<Float> = [[[true, false, false, true]]] // expected-error {{cannot convert value of type 'Bool' to expected element type 'TensorElementLiteral<Float>'}}
  let _: Tensor<Float> = Tensor([[[true, false, false, true]]]) // ok
}


func testTensorFlowFunctionTypes() {
  var tf_fn, tf_fn2 : @convention(tensorflow) () -> ()
  var fn : () -> ()
  var cfn : @convention(c) () -> ()

  tf_fn = tf_fn2

  tf_fn = fn // expected-error {{TensorFlow functions cannot be converted to other function types}}
  fn = tf_fn // expected-error {{TensorFlow functions cannot be converted to other function types}}

  tf_fn = cfn // expected-error {{TensorFlow functions cannot be converted to other function types}}
  cfn = tf_fn // expected-error {{TensorFlow functions cannot be converted to other function types}}
}


// These are testcases that show the next steps in "@convention(tensorflow)" support.

func takesTFFunc(fn : @convention(tensorflow) (Tensor<Float>) -> Tensor<Float>) {
}

func testTFFunc() {
  let one = Tensor<Float>(1)

  // FIXME: This should work.
  takesTFFunc { $0 + 1 }  // expected-error {{TensorFlow functions cannot be converted to other function types}}

  // FIXME: This should generate an error saying that you cannot capture "one"
  // because TensorFlow functions cannot capture values.
  takesTFFunc { $0 + one }  // expected-error {{TensorFlow functions cannot be converted to other function types}}


  // FIXME: Should eventually be supported.
  @convention(tensorflow) // expected-error {{attribute can only be applied to types, not declarations}}
  func inner1(a : Tensor<Float>) -> Tensor<Float> {
    return a+1
  }
  takesTFFunc(fn: inner1)  // expected-error {{TensorFlow functions cannot be converted to other function types}}
}

