// RUN: %target-swift-frontend -Xllvm -tf-warn-send-recv -Xllvm -tf-warn-scalar-transfer=true -O -emit-sil -verify %s

// Tests that we diagnose scalar transfers when tf-warn-scalar-transfer=true. There are parallel
// tests in diagnostics.swift ensuring that we do not diagnost the transfers when
// tf-warn-scalar-transfer=false.

import TensorFlow

// expected-warning @+1 {{'x' implicitly copied to the accelerator}}
public func scalarToAccelerator(x: Float) -> Tensor<Float> {
  return Tensor(x) + 1  // expected-note {{value used here}}
}

public func scalarToHost() {
  var i = Tensor(0)
  while i < 10 {  // expected-warning {{value implicitly copied to the host}}
    print("Running loop body")
    i += 1
  }
}
