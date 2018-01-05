// RUN: %target-swift-frontend -emit-sil -verify %s

import TensorFlow


// Verify TensorElementProtocol makes it into the test.
func testTensorElementProtocol<T : TensorElementProtocol>(a : T) -> T {
  return a
}


func testTensor() {
  _ = Tensor<Float>(1.0)
  _ = Tensor<Int>(2)
  _ = Tensor<Int>([1.0, 2.0]) // expected-error {{cannot convert value of type '[Double]'}}
}

