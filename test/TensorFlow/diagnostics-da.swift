// RUN: %target-swift-frontend -Xllvm -tf-dump-intermediates -O -emit-sil -verify %s

import TensorFlow

// These are diagnostics detected by deabstraction.

// Verify we reject multiple attempts to configure hardware.
public func testDeviceInvalid() {
  TensorFlow.enableTPU() // expected-note {{previous configuration is specified here}}
  TensorFlow.enableTPU() // expected-error {{device configuration specified multiple times}}
}

public func shapeError() {
  // expected-error @+1 {{tensor literal should have 9 scalars for this shape, but has 8}}
  let _ = Tensor<Float>(shape: [1, 3, 3, 1],
                        scalars: [0, 1, 0, 1, 1, 1, 0, 1])
}
