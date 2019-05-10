// RUN: %target-swift-frontend -O -emit-sil -verify %s

// This file contains tests that produce errors in the semantic analysis pass
// or earlier. These must be split from other SIL tests because SIL passes are
// not run when there are type-checking errors.

import TensorFlow

public func testExpressibleByLiteral() {
  let _: Tensor<Int32> = [1, 2, 3, 4] // ok
  let _: Tensor<Float> = [1, 2.0, 3, 4] // ok
  let _: Tensor<Bool> = [[[true, false, false, true]]] // ok
  // expected-error @+1 4 {{generic struct '_TensorElementLiteral' requires that 'Float' conform to 'ExpressibleByBooleanLiteral'}}
  let _: Tensor<Float> = [[[true, false, false, true]]]
  let _: Tensor<Float> = Tensor([[[true, false, false, true]]]) // ok
}

func testTensorFlowFunctionTypes() {
  // expected-error@+2 {{convention 'tensorflow' not supported}}
  // expected-error@+1 {{convention 'tensorflow' not supported}}
  var tf_fn, tf_fn2 : @convention(tensorflow) () -> ()

  tf_fn = tf_fn2
  tf_fn2 = tf_fn
}

// These are testcases that show the next steps in "@convention(tensorflow)" support.

// expected-error@+1 {{convention 'tensorflow' not supported}}
func takesTFFunc(fn : @convention(tensorflow) (Tensor<Float>) -> Tensor<Float>) {
}

func testTFFunc() {
  let one = Tensor<Float>(1)

  // It is fine to pass a closure as a tensorflow function. The parititioner will
  // validate it and throw an error if it cannot be partitioned.
  takesTFFunc { $0 + 1 }

  // This type checks even though it captures a value. The SILGen pass will
  // check if the closures that are tensorflow functions do not have captures.
  takesTFFunc { $0 + one }
  // FIXME: Should eventually be supported.
  // xpected-error @+2 {{convention 'tensorflow' not supported}}
  // expected-error @+1 {{attribute can only be applied to types, not declarations}}
  @convention(tensorflow)
  func inner1(a : Tensor<Float>) -> Tensor<Float> {
    return a+1
  }
  takesTFFunc(fn: inner1)
}

