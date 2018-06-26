// RUN: %target-swift-frontend -Xllvm -tf-dump-intermediates -O -emit-sil -verify %s

import TensorFlow

// These are diagnostics detected by deabstraction.

// Verify we reject multiple attempts to configure hardware.
public func testDeviceInvalid() {
  TensorFlow.enableTPU() // expected-note {{previous configuration is specified here}}
  TensorFlow.enableTPU() // expected-error {{device configuration specified multiple times}}
}

