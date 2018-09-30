// This test file captures the diagnostics related to TensorFlow that are
// generated during the SILGen phase.
// RUN: %target-swift-frontend -O -emit-sil -verify %s

import TensorFlow

func takesTFFunc(fn : @convention(tensorflow) (Tensor<Float>) -> Tensor<Float>) {
}

// Check that TensorFlow functions cannot capture values.
func testTFFunc() {
  let one = Tensor<Float>(1)
  takesTFFunc { $0 + one } // expected-error {{TensorFlow functions cannot capture values}}
}


