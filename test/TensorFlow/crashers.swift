// RUN: %target-swift-frontend -Xllvm -tf-dump-intermediates -O -emit-sil %s -verify

// This file contains various regression tests that crashed the compiler.

import TensorFlow

var someGlobal = Tensor<Int32>(1)

public func iftest(z: Tensor<Int32>, y: Tensor<Int32>, c: Bool, d: Bool) -> Tensor<Int32> {
  // expected-warning @-1 {{'c' implicitly copied to the accelerator}}
  // expected-warning @-2 {{'z' implicitly copied to the accelerator}}

  var a = z
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
  // expected-warning @-1 {{'w1' implicitly copied to the accelerator}}
  // expected-warning @-2 {{'inputBatch' implicitly copied to the accelerator}}
  let iterationCount = 1000
  for _ in 0..<iterationCount {
    _ = inputBatch ⊗ w1  // expected-note 2 {{value used here}}
  }
}

// This crashed the partitioning pass because the 1.0 scalar was hoisted out of
// the loop.  The partitioning pass tried to sink it back in, but failed.
public func sinking_crash(w1: Tensor<Float>) {
  // expected-warning @-1 {{'w1' implicitly copied to the accelerator}}
  for _ in 0..<1000 {
    let pred = w1+w1  // expected-note {{value used here}}
    let _ = 1.0 / Tensor<Float>(pred.scalarCountTensor)
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
public func lowerGraphCrash(x: Tensor<Int32>) {
  // expected-warning @-1 {{'x' implicitly copied to the accelerator}}

  _ = x*x  // expected-note {{value used here}}
  for _ in 0..<1000 {
    _ = x+someGlobal // expected-warning {{value implicitly copied to the accelerator}}
    // expected-error @-1 {{GraphGen cannot lower a 'receive' from the host yet}}
  }
}


// This was a prototype runtime test that crashed due to bb arg invalidation
// problems.
public func testStraightLineXORTraining() {
  // Hyper-parameters
  let iterationCount = 1000
  let learningRate: Float = 0.2

  // Training data
  let inputBatch = Tensor<Float>(
    [[0.0, 0.0], [0.0, 1.0], [1.0, 0.0], [1.0, 1.0]]
  )
  let outputBatch = Tensor<Float>([[0.0], [1.0], [1.0], [0.0]])

  // Parameters
  var w1 = Tensor<Float>(shape: [2, 4], repeating: 0.5)
  var w2 = Tensor<Float>(shape: [4, 1], repeating: 0.5)

  var b1 = Tensor<Float>(zeros: [1, 4])
  var b2 = Tensor<Float>(zeros: [1, 1])

  // Training loop
  for _ in 0..<iterationCount {
    let mmul1 = inputBatch ⊗ w1
    let l1 = mmul1 + b1
    let o1 = sigmoid(l1)
    let mmul2 = o1 ⊗ w2
    let l2 = mmul2 + b2
    let pred = sigmoid(l2)

    // Loss
    let sub = outputBatch - pred
    let _ = sub * sub

    // Gradient
    let dSqr = 1 / Tensor<Float>(pred.scalarCountTensor)
    let dSub = 2 * sub * dSqr
    let dPred = -dSub
    let dL2 = dPred * pred * (1 - pred)
    let dMmul2 = dL2
    let dB2 = dL2
    let dO1 = dMmul2 ⊗ w2.transposed(withPermutations: 1, 0)
    let dW2 = o1.transposed(withPermutations: 1, 0) ⊗ dMmul2
    let dL1 = dO1 * l1 * (1 - l1)
    let dMmul1 = dL1
    let dB1 = dL1

    // Statically detected shape mismatch!
    // expected-error @+1 {{(op: 'MatMul') with input shapes: [4,2], [4,4]}}
    let dW1 = inputBatch ⊗ dMmul1

    // Descent
    w1 -= (dW1 * learningRate)
    b1 -= (dB1 * learningRate)
    w2 -= (dW2 * learningRate)
    b2 -= (dB2 * learningRate)
  }
}


// This testcase exposed bb argument and source location manipulation problems.
public func testEagerLoop() -> Int32 { // expected-note 5 {{value used here}}
  var a = Tensor<Int32>(6)
  // expected-error @+1 {{GraphGen cannot lower a 'send' to the host yet}}
  var count = Tensor<Int32>(0)  // expected-warning 7 {{value implicitly copied to the host}}
  while a.elementsEqual(1).scalar! { // expected-warning 2 {{implicitly copied}} expected-note {{value used here}}
    if (a % 2).elementsEqual(0).scalar! { // expected-warning 2 {{implicitly copied}} expected-note {{value used here}}
      a = a / 2
    } else {
      a = 3 * a + 1
    }
    count += 1 // expected-note {{value used here}}
  }
  return count.scalar!  // expected-note {{value used here}}
}

// This crashed sinking a "tensor to scalar" operation in mean out the bottom
// of the loop.
@_silgen_name("opaque_generic_function")
func opaqueGenericFunction<T>(_ a : T)

@inline(never)
public func sinkTensorToScalarCrash() {
  var loss = Float.infinity
  let w1 = Tensor<Float>(shape: [4, 1], repeating: 0.1)
  var w2 = Tensor<Float>(shape: [4, 1], repeating: 0.5)

  // expected-error @+1 {{cannot lower a 'receive' from the host}}
  for _ in 0...10000 {  // expected-warning {{implicitly copied}}
    w2 -= w1
    loss = w2.mean() // expected-warning {{implicitly copied}}
  }

  opaqueGenericFunction(loss)
}

public extension Tensor {
  // This is a theoretical operation that takes a generic scalar value as an
  // attribute.
  @_inlineable @inline(__always)
  func genericAttr<T : AccelerableByTensorFlow>(axis: T) -> Tensor {
    return #tfop("ExampleOp", handle, axis: axis, axisType: T.self)
  }
}

public func testGenericThing() {
  let a = Tensor<Float>(zeros: [1,2])
  // expected-error @+1 {{Op type not registered 'ExampleOp'}}
  let b = a.genericAttr(axis: 42)
  _ = b+b
}


// b/75247714: #tfop crashes when attribute argument is a tuple
public func test75247714() {
  // expected-error @+1 {{attribute 'bar' requires a constant argument}}
  let _ : () = #tfop("foo", bar: (1, 2))
}

// b/76058387: Deabstraction crasher
public func testPropagateScalarOperands() {
  let bounds = 0..<10
  let scalars = bounds.map(Float.init)
  let x = Tensor<Float>(shape: [2, 5], scalars: scalars).toDevice()
  _ = x * x
}


// b/76033645
public func tensorEndPointComputation() -> Int {
  let retval = 42
  var i: Int32 = 0
  var x = Tensor(1)
  repeat {
    x += x
    i += 1
  } while i < 10

  return retval
}
