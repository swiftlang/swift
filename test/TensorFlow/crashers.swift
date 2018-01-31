// RUN: %target-swift-frontend -Xllvm -tf-dump-intermediates -O -emit-sil %s -verify

// This file contains various regression tests that crashed the compiler.

import TensorFlow

public func iftest(z: Tensor<Int>, y: Tensor<Int>, c: Bool, d: Bool) -> Tensor<Int> {
  // expected-warning @-1 {{'c' implicitly copied to the accelerator}}

  var a = z  // expected-warning {{value implicitly copied to the accelerator}}
  if c { // expected-note {{value used here}}
    if d { fatalError() }
    a = a + a // expected-note {{value used here}}
  } else {
    if d { fatalError() }

    // expected-warning @+1 {{value implicitly copied to the accelerator}}
    a = Tensor<Int>([1,2,3])  // expected-error {{GraphGen cannot lower a 'receive' from the host yet}}
  }

  a = a * a  // expected-note {{value used here}}
  return a
}

