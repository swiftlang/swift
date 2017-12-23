// RUN: %target-swift-frontend -emit-sil -verify %s

import TensorFlow


// Verify TensorElementProtocol makes it into the test.
func testTensorElementProtocol<T : TensorElementProtocol>(a : T) -> T {
  return a
}


func testTensor() {
  _ = Tensor<Float>(zeroD: 1.0)
  _ = Tensor<Int>(zeroD: 2)

  _ = Tensor<Int>(zeroD: 2.0) // expected-error {{'Double' is not convertible to 'Int'}}
}

