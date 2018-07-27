// RUN: %target-swift-frontend -Xllvm -tf-strict-deabstraction -emit-sil %s -verify

// This file contains tests that used to be in ./crashers.swift that produced
// expected errors in the deabstraction pass, which prevented partitioning from
// running.
//
// Once we move partitioning into the mandatory pipeline, we should be able to
// move these tests back into ./crashers.swift.

import TensorFlow

// b/75247714: #tfop crashes when attribute argument is a tuple
public func test75247714() {
  // expected-error @+1 {{attribute 'bar' cannot be an enum, struct, or tuple}}
  let _ : () = #tfop("foo", bar: (1, 2))
}


// b/76115311
func genericMethod76115311<Scalar : Numeric>(with value: Scalar = 0) -> Tensor<Scalar> {
  // expected-error @+1 {{operand has unrecognized type}}
  return #tfop("FooOp", value)
}

public func b76115311() {
  let matrix: Tensor<Float> = genericMethod76115311()
  _ = matrix+matrix
}
