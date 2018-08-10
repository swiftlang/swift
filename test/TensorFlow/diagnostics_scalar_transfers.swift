// RUN: %target-swift-frontend -Xllvm -tf-warn-scalar-transfer=true -O -emit-sil -verify %s

// Tests that we diagnose scalar transfers when tf-warn-scalar-transfer=true. There are parallel
// tests in diagnostics.swift ensuring that we do not diagnose the transfers when
// tf-warn-scalar-transfer=false.

import TensorFlow

// expected-warning @+1 {{'x' implicitly copied to the accelerator}}
public func scalarToAccelerator(x: Float) -> Tensor<Float> {
  return Tensor(x) + 1  // expected-note {{value used here}}
}
