// RUN: %target-swift-frontend -Xllvm -tf-dump-intermediates -O -emit-sil %s -verify | %FileCheck %s

// This file contains various regression tests that crashed the compiler.

import TensorFlow

var someGlobal = Tensor<Int32>(1)

public func iftest(z: Tensor<Int32>, y: Tensor<Int32>, c: Bool, d: Bool) -> Tensor<Int32> {
  // expected-warning @-1 {{'z' implicitly copied to the accelerator}}

  var a = z
  if c {
    if d { fatalError() }
    a = a + a // expected-note {{value used here}}
  } else {
    if d { fatalError() }

    // expected-warning @+1 {{value implicitly copied to the accelerator}}
    a = someGlobal
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
    _ = inputBatch • w1  // expected-note 2 {{value used here}}
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
    let mmul1 = inputBatch • w1
    let l1 = mmul1 + b1
    let o1 = sigmoid(l1)
    let mmul2 = o1 • w2
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
    let dO1 = dMmul2 • w2.transposed(withPermutations: 1, 0)
    let dW2 = o1.transposed(withPermutations: 1, 0) • dMmul2
    let dL1 = dO1 * l1 * (1 - l1)
    let dMmul1 = dL1
    let dB1 = dL1

    // Statically detected shape mismatch!
    // expected-error @+1 {{(op: 'MatMul') with input shapes: [4,2], [4,4]}}
    let dW1 = inputBatch • dMmul1

    // Descent
    w1 -= (dW1 * learningRate)
    b1 -= (dB1 * learningRate)
    w2 -= (dW2 * learningRate)
    b2 -= (dB2 * learningRate)
  }
}

// FIXME: Add back testEagerLoop() https://github.com/google/swift/issues/28

// This crashed sinking a "tensor to scalar" operation in mean out the bottom
// of the loop.
@_silgen_name("opaque_generic_function")
func opaqueGenericFunction<T>(_ a : T)

@inline(never)
public func sinkTensorToScalarCrash() {
  var loss = Float.infinity
  let w1 = Tensor<Float>(shape: [4, 1], repeating: 0.1)
  var w2 = Tensor<Float>(shape: [4, 1], repeating: 0.5)

  for _ in 0...10000 {
    w2 -= w1
    loss = w2.mean()
  }

  opaqueGenericFunction(loss)
}

public extension Tensor {
  // This is a theoretical operation that takes a generic scalar value as an
  // attribute.
  @inlinable @inline(__always)
  func genericAttr<T : AccelerableByTensorFlow>(axis: T) -> Tensor {
    // expected-error @+1 {{op named 'ExampleOp' is not registered in TensorFlow}}
    let ret: TensorHandle<Scalar> = #tfop("ExampleOp", handle, axis: axis, axisType$dtype: T.tensorFlowDataType)
    return Tensor<Scalar>(handle: ret)
  }
}

public func testGenericThing() {
  let a = Tensor<Float>(zeros: [1,2])
  let b = a.genericAttr(axis: 42)
  _ = b+b
}


// b/76058387: Deabstraction crasher
public func testPropagateScalarOperands() {
  let bounds = 0..<10
  let scalars : [Float] = bounds.map(Float.init)
  let x = Tensor<Float>(shape: [2, 5], scalars: scalars).toAccelerator()
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


// This was a crash marking arguments outside the tensor region.
public func argumentCrash() {
  for i : Int8 in 1...10 {
    let x = Tensor(i)
    _hostOp(x+x)
  }
}

// b/76310230 tf-partition crashes on uninlinable multiresult function
@inline(never)
func twoHandles() -> (TensorHandle<Int32>, ResourceHandle) {
  fatalError()
}

public func testMultiResultUninlinable() {
  let (x1, _) = twoHandles()  // expected-warning {{value implicitly copied to the accelerator}}
  let _: TensorHandle<Float> = #tfop("Identity", x1)  // expected-note {{value used here}}
}

// Test support for copying multiple result outputs.
public func testMultiOutputsFnResults() -> (Tensor<Float>,  Tensor<Float>) {
  // expected-error @+1 {{op named 'MultResult' is not registered in TensorFlow}}
  let (x1, y1): (TensorHandle<Float>, TensorHandle<Float>) = #tfop("MultResult")
  let x = Tensor<Float>(handle: x1)
  let y = Tensor<Float>(handle: y1)
  return (x.toHost(),y.toHost())
}

// expected-warning @+2{{implicitly copied to the accelerator}}
// expected-warning @+1{{implicitly copied to the accelerator}}
public func testMultiResultOp_tfop(x: Tensor<Float>, y: Tensor<Float>) {
  let (loss, backprop) : (TensorHandle<Float>, TensorHandle<Float>) =
    #tfop("SoftmaxCrossEntropyWithLogits", x, y)
  // expected-note @-1{{value used here}}
  // expected-note @-2{{value used here}}
  _hostOp(loss)
  _hostOp(backprop)
}

// expected-warning @+2{{implicitly copied to the accelerator}}
// expected-warning @+1{{implicitly copied to the accelerator}}
public func testMultiResultOp_rawop(x: Tensor<Float>, y: Tensor<Float>) {
  // expected-note @+2{{value used here}}
  // expected-note @+1{{value used here}}
  let results = TensorFlow.Raw.softmaxCrossEntropyWithLogits(features: x, labels: y)
  _hostOp(results.loss)
  _hostOp(results.backprop)
}

public func testMultiResultOp_send_recv() {
  var x = Tensor<Float>([[1.0]])  // expected-warning {{implicitly copied to the host}}
  // Accelerator -> Host
  _hostOp(x)
  x += [[2.0]]
  // expected-warning @+1{{implicitly copied to the host}}
  let results = TensorFlow.Raw.softmaxCrossEntropyWithLogits(features: x, labels: x)
  // Accelerator -> Host
  _hostOp(results.loss)
  // expected-note @+1{{value used here}}
  let adjustedLoss = results.loss.scalar! + 3.0
  // Host -> Accelerator
  let y = Tensor<Float>(adjustedLoss)
  _hostOp(y)
}

// CHECK-LABEL: --- TFPartition Accelerator Result: {{.*}}testMultiResultOp_send_recv{{.*}}
// CHECK:  graph_op "tfc.SendToHost"
// CHECK:  graph_op "tfc.SendToHost"
// CHECK:  graph_op "tfc.RecvFromHost"

var globalThing: Int32!

public func testStructExtractBBArg(x: Tensor<Float>) -> Tensor<Int32> {
  _ = x.toAccelerator() + 1
  //  %21 = argument of bb2 : $Int32                    // user: %22
  // [Send]   %22 = struct_extract %21 : $Int32, #Int32._value // user: %23
  return Tensor<Int32>(globalThing)
}

public func SR8191() {
  let t = Tensor<Float>(1.0)
  var i = 0
  repeat {
    let y = t + t // expected-warning {{value implicitly copied to the host}}
    _hostOp(y)
    i += 1
  } while i < 10
}

// CHECK-LABEL: --- XLA CFG Canonicalize: {{.*}}SR8191{{.*}}
// CHECK: [sequence
// CHECK:   <while Preheader: {{bb[0-9]+}}, Header: {{bb[0-9]+}}, exit: [[EXIT:bb[0-9]+]]
// CHECK:     [sequence
// CHECK:       {condition Header: {{bb[0-9]+}}
// CHECK:         block {{bb[0-9]+}}
// CHECK:         block {{bb[0-9]+}}}
// CHECK:       block {{bb[0-9]+}}]>
// CHECK:   block [[EXIT]]]

// `a` and `b` are both arguments to the tensor program, which starts at
// "let _= a + b", and ends in that BB. So the tensor start point and tensor end
// point should both be in that BB.
public func SR8226(n: Float, m: Float, cond: Bool, cond2: Bool) {
  // expected-warning @+1{{implicitly copied to the accelerator}}
  let a = Tensor<Float>([m, n])

  if cond {
    // expected-warning @+1{{implicitly copied to the accelerator}}
    let b = Tensor<Float>([m, n])
    if cond2 {
      // expected-note @+2{{value used here}}
      // expected-note @+1{{value used here}}
      let _ = a + b
    }
  }
}

// Tensor `b` as a BB arg to the loop header block does not have any important
// users, and thus get deleted. This will in turn delete the associated
// retain/release insts on that tensor.

public func SR8228_unusedTensorBBArg(n: Int32, x: Tensor<Float>) {
  var a = Tensor<Float>(1.0)
  // expected-warning @+1{{variable 'b' was written to, but never read}}
  var b = x
  var i: Int32 = 0
  while i < n {
    a += a
    b = a
    i += 1
  }
}

// When handling cond_br with ThensorHandle<Elem> in PartitionCloner, only Builtin.i1
// was considered for Elem type. As a result, Elem of Bool type crashed the compiler.
public func SR8222_cond_br_TensorHandle_Bool() {
  let t = Tensor<Float>(1.0)
  var i = Tensor<Int32>(0)
  repeat {
    let y = t + t
    // expected-warning @-1{{implicitly copied to the host}}
    _hostOp(y)
    i += 1
  } while i != Tensor<Int32>(10)
}

// Previously, dead instruction cleanup during deabstraction crashed while
// compiling the following code. (Instructions were deleted from a basic block
// while traversing the basic block, causing iterator invalidation).
func SR8316_helper() -> Tensor<Int32> {
  return Tensor(1)
}
func SR8316_main() {
  // It is important that `float` is let-bound so that a debug_value
  // instruction is produced, which is what triggered the original crash.
  let float: Float = 0.1 // expected-warning {{immutable value 'float' was never used}}
  _ = SR8316_helper()
}

func readDataset() -> (images: Tensor<Float>, labels: Tensor<Int32>)? {
    _hostOp("Reading the data.")
    return (Tensor<Float>(1.0), Tensor<Int32>(1))
}

public func constFoldingBug() {
  guard let _ = readDataset() else {
    return
  }
}

// This function failed SIL verification due to a non-inlined SILDebugLocation.
public func SR8419(iterationCount: Int) {
  let images = Tensor<Float>(ones: [1000, 784])
  let batchSize = Float(images.shape[0])

  _hostOp("Begin training for \(iterationCount) iterations.")

  for _ in 0...iterationCount {
    let bound = Int32(batchSize)/25
    for _ in 0..<bound {
      let _ = Tensor(1)
    }
  }
}

// If `deabstractedCallee` gets deabstracted before `inlineDeabstracted_*`,
// then the insts in `deabstractedCallee` get deabstracted twice. There was
// a bug where the compiler crashed when deabstracting certain graph_ops twice:
//  - graph_ops with InputLists
//  - graph_ops that pack results into aggregate structs
// There is no guaranteed deabstraction order, so this test isn't guaranteed to
// catch the problem. Sandwiching `deabstractedCallee` between two callers
// makes this test catch the problem as long as the order happens to be linear
// up or down.
struct AggregateStruct {
  let a, b: Tensor<Float>
}
public func inlineDeabstracted_a() -> Tensor<Float> {
  return deabstractedCallee([1, 2, 3])
}
// expected-warning @+1 {{implicitly copied}}
public func deabstractedCallee(_ t: Tensor<Float>) -> Tensor<Float> {
  // expected-error @+3 {{op named 'Dummy' is not registered in TensorFlow}}
  // expected-error @+2 {{op named 'Dummy' is not registered in TensorFlow}}
  // expected-error @+1 {{op named 'Dummy' is not registered in TensorFlow}}
  let aggregate: AggregateStruct = #tfop("Dummy") // packs results

  // expected-note @+1 {{value used here}}
  return t ++ aggregate.a // concat uses an InputList
}
public func inlineDeabstracted_b() -> Tensor<Float> {
  return deabstractedCallee([1, 2, 3])
}

// b/118507040: The SIL location of a synthesized host instruction for
// switch_case based control flow handling cannot be ReturnKind.
@inline(never)
func foo() -> Int32? {
  return 0
}

func getLogpLex() {
  if let _ = foo() {
    _ = Tensor<Float>(1.0)
    return
  } else {
    // For the SILLocation associated with this return stmt/inst,
    // The LocationKind is ReturnKind.
    //
    // When we synthesize host code to send case id #1 to the accelerator, make
    // sure the host inst will NOT use ReturnKind as the LocationKind.
    return
  }
}

public func b118507040() {
  getLogpLex()
  _ = Tensor<Float>(1.0)
}

// Make sure that #tfop can deal with operands that are LValues.
func tfopLValueOperand() -> Tensor<Float> {
  var t = Tensor<Float>(0)
  t = #tfop("Add", t, Tensor<Float>(1), T$dtype: Float.tensorFlowDataType)
  return t
}
