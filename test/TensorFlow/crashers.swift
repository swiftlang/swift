// RUN: %target-swift-frontend -Xllvm -tf-dump-intermediates -O -emit-sil %s -verify

// This file contains various regression tests that crashed the compiler.

import TensorFlow

var someGlobal = Tensor<Int>(1)

public func iftest(z: Tensor<Int>, y: Tensor<Int>, c: Bool, d: Bool) -> Tensor<Int> {
  // expected-warning @-1 {{'c' implicitly copied to the accelerator}}

  var a = z  // expected-warning {{value implicitly copied to the accelerator}}
  if c { // expected-note {{value used here}}
    if d { fatalError() }
    a = a + a // expected-note {{value used here}}
  } else {
    if d { fatalError() }

    // expected-warning @+1 {{value implicitly copied to the accelerator}}
    a = someGlobal  // expected-error {{GraphGen cannot lower a 'receive' from the host yet}}
  }

  a = a * a  // expected-note {{value used here}}
  return a
}

// This crashed the partition pass because there were no ops outside the loop at
// the return site, and this prevented the return block from being included in
// the post dom set for the partitioning pass.
public func postdom_crash1(w1: Tensor<Float>, inputBatch: Tensor<Float>) {
  let iterationCount = 1000
  for _ in 0..<iterationCount {
    _ = inputBatch ⊗ w1  // expected-note 2 {{value used here}}
  }  // expected-warning 2 {{value implicitly copied to the accelerator}}
}

// This crashed the partitioning pass because the 1.0 scalar was hoisted out of
// the loop.  The partitioning pass tried to sink it back in, but failed.
public func sinking_crash(w1: Tensor<Float>) {
  for _ in 0..<1000 {
    let pred = w1+w1 // expected-warning {{value implicitly copied to the accelerator}}
    let _ = 1.0 / Tensor<Float>(pred.unitCountTensor)
  }
}

// This crashed the partitioning pass because the end point of the program was
// calculated to be inside the loop, but the startpoint was outside.
public func endpointComputationCrash() {
  var w1 = Tensor<Float>(shape: [2, 4], repeating: 0.5)

  for _ in 0..<1000 {
    w1 -= w1
  }
}

// This crashed lower graph because it produced an error about not being able
// to lower a send and there wasn't enough error recovery to handle it well.
public func lowerGraphCrash(x: Tensor<Int>) {
  _ = x*x  // expected-note {{value used here}}
  for _ in 0..<1000 {
    _ = x+someGlobal // expected-note {{value used here}}
    // expected-error @+1 {{GraphGen cannot lower a 'receive' from the host yet}}
  } // expected-warning {{value implicitly copied to the accelerator}}
} // expected-warning {{value implicitly copied to the accelerator}}
