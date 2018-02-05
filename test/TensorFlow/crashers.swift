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


// This was a prototype runtime test that crashed due to bb arg invalidation
// problems.
public func testStraightLineXORTraining() { // expected-note 5 {{value used here}}
  // Hyper-parameters
  let iterationCount = 1000
  let learningRate: Float = 0.2

  // Parameters
  var w1 = Tensor<Float>(shape: [2, 4], repeating: 0.5)
  var w2 = Tensor<Float>(shape: [4, 1], repeating: 0.5)

  // expected-error @+1 {{GraphGen cannot lower a 'receive' from the host yet}}
  var b1 = Tensor<Float>.zeros(shape: [1, 4]) // expected-warning 5 {{implicitly copied}}
  var b2 = Tensor<Float>.zeros(shape: [1, 1])

  // Training data
  let inputBatch = Tensor<Float>(
    [[0.0, 0.0], [0.0, 1.0], [1.0, 0.0], [1.0, 1.0]]
    ).toDevice()
  let outputBatch = Tensor<Float>([[0.0], [1.0], [1.0], [0.0]]).toDevice()

  // Training loop
  for _ in 0..<iterationCount {
    let mmul1 = inputBatch ⊗ w1
    let l1 = mmul1 + b1
    let o1 = sigmoid(l1) // expected-warning 2 {{implicitly copied}}
    let mmul2 = o1 ⊗ w2
    let l2 = mmul2 + b2
    let pred = sigmoid(l2) 

    // Loss
    let sub = outputBatch - pred
    let sqr = sub * sub
    let _ = sqr.mean()  // expected-warning 2 {{implicitly copied}}

    // Gradient
    let dSqr = 1 / Tensor<Float>(pred.unitCountTensor)
    let dSub = 2 * sub * dSqr
    let dPred = -dSub
    let dL2 = dPred * pred * (1 - pred)
    let dMmul2 = dL2
    let dB2 = dL2
    let dO1 = dMmul2 ⊗ w2.transposed()  // expected-warning {{implicitly copied}} expected-note {{value used here}}
    let dW2 = o1.transposed() ⊗ dMmul2  // expected-warning {{implicitly copied}}
    let dL1 = dO1 * l1 * (1 - l1)
    let dMmul1 = dL1
    let dB1 = dL1
    let dW1 = inputBatch ⊗ dMmul1

    // Descent
    w1 -= (dW1 * learningRate)
    b1 -= (dB1 * learningRate)
    w2 -= (dW2 * learningRate)
    b2 -= (dB2 * learningRate)
  }
}
