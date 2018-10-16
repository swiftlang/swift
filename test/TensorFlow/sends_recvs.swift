// RUN: %target-swift-frontend -Xllvm -tf-dump-intermediates -Xllvm -tf-dump-graph -Xllvm -tf-module-level-graph=false -O -emit-sil %s -verify | %FileCheck %s

// In this file, send means accelerator->host, and recv means the opposite.

import TensorFlow

public func test1Send() {
  var a = Tensor<Float>(1.0)
  // One send.
  _hostOp(a.toHost())
  a += 1
  // This one should not be a send.
  _hostOp(a.toHost())
}

// CHECK-LABEL: --- TFPartition Host Result: {{.*}}test1Send{{.*}}
// CHECK:      function_ref @_swift_tfc_StartTensorComputation
// CHECK-NEXT: [[TC:%.*]] = apply

// CHECK: integer_literal $Builtin.Int64, 0
// CHECK-NEXT: [[TENSOR_ID:%.*]] = struct $Int
// CHECK:      // function_ref static TensorHandle.receiveFromAccelerator(_:_:)
// CHECK-NEXT:      [[RECEIVE_H:%.*]] = function_ref @
// CHECK-NEXT: apply [[RECEIVE_H]]<Float>([[TC]], [[TENSOR_ID]]

// CHECK:      function_ref @_swift_tfc_FinishTensorComputation


public func test1SendWithParam(x: Float) {
  TensorFlow.enableGPU()
  var a = Tensor<Float>(x)
  // One send.
  _hostOp(a.toHost())
  a += 1
  // This one should not be a send.
  _hostOp(a.toHost())
}

// CPU function takes no input arg.
// CHECK-LABEL: --- TFDevicePartition Per-Device Function Extraction Result: {{.*}}test1SendWithParam{{.*}}CPU{{.*}}
// CHECK: bb0:
// CHECK: graph_op "tfc.D2DTensorRecv

// GPU function takes the input arg of x.
// CHECK-LABEL: --- TFDevicePartition Per-Device Function Extraction Result: {{.*}}test1SendWithParam{{.*}}GPU{{.*}}
// CHECK: bb0(%0 : @unowned $TensorHandle
// CHECK: graph_op "tfc.D2DTensorSend

// The _Send node should be hooked up as a control dependency on the return
 // node, so that Sends gets run before the function returns.
// CHECK:        function {
// CHECK:          name: "{{.*}}test1SendWithParam{{.*}}.tf_GPU.device_partition"
// CHECK:          node_def {
// CHECK:            name: "RunControlDependency"
// CHECK:            op: "Identity"
// CHECK-NEXT:       input: "op/test1SendWithParam.x_
// CHECK-NEXT:       input: "^tf_send_0"
// CHECK:        function {
// CHECK:          name: "{{.*}}test1SendWithParam{{.*}}.tf_CPU.device_partition"

public func test2Sends() {
  var a = Tensor<Float>(1.0)
  // One send.
  _hostOp(a.toHost())
  a += 2
  // Another send.
  _hostOp(a.toHost())
  a += 3
  // This one should not be a send.
  _hostOp(a.toHost())
}

// CHECK-LABEL: --- TFPartition Host Result: {{.*}}test2Send{{.*}}
// CHECK:      function_ref @_swift_tfc_StartTensorComputation
// CHECK-NEXT: [[TC:%.*]] = apply

// The first receive is over tensor id 0.
// CHECK: integer_literal $Builtin.Int64, 0
// CHECK-NEXT: [[TENSOR_ID0:%.*]] = struct $Int
// CHECK:      // function_ref static TensorHandle.receiveFromAccelerator(_:_:)
// CHECK-NEXT:      [[RECEIVE_H0:%.*]] = function_ref @
// CHECK-NEXT: apply [[RECEIVE_H0]]<Float>([[TC]], [[TENSOR_ID0]]

// The second receive is over tensor id 1.
// CHECK: function_ref _hostOp
// CHECK: integer_literal $Builtin.Int64, 1
// CHECK-NEXT: [[TENSOR_ID1:%.*]] = struct $Int
// CHECK:      // function_ref static TensorHandle.receiveFromAccelerator(_:_:)
// CHECK-NEXT:      [[RECEIVE_H1:%.*]] = function_ref @
// CHECK-NEXT: apply [[RECEIVE_H1]]<Float>([[TC]], [[TENSOR_ID1]]

// CHECK:      function_ref @_swift_tfc_FinishTensorComputation

public func testSendsInABranch(_ c: Bool) {
  var a = Tensor<Float>(1.0)
  if c {
    a += a
    // One send.
    _hostOp(a.toHost())
  }
  a += a
  // This one should not be a send.
  _hostOp(a.toHost())
}

// For testSendsInABranch(), we are generating a stateful while op.
// CHECK-LABEL: --- TFPartition GraphDef Proto:
// CHECK:  op: "If"

public func testSendsInALoopCPU() {
  let maxCount = 10
  var count = 1
  var a = Tensor<Float>(1.0)
  while count < maxCount {
    a += a
    // One send.
    _hostOp(a.toHost())
    count += 1
  }
  a += a
  // This one should not be a send.
  _hostOp(a.toHost())
}

// For testSendsInALoopCPU(), we are generating a stateful while op.
// CHECK-LABEL: --- TFPartition GraphDef Proto:
// CHECK:  op: "While"

public func testSendsInALoopGPU() {
  TensorFlow.enableGPU()
  let maxCount = 10
  // a cannot be an integer tensor due to a TensorFlow Eigen bug (b/77737504).
  var a = Tensor<Float>(1)
  var count = 1

  while count < maxCount {
    a += a
    // One send.
    _hostOp(a.toHost())
    count += 1
  }
  a += a
  let _ = a.array
}

// CHECK-LABEL: --- TFDevicePartition Cross Device Tensor Transfer Annotation Result: {{.*}}testSendsInALoopGPU{{.*}}

// In the loop head (where cond and body are evaluated), send a to CPU, which
// then sends it to host.
// CHECK:      bb1
// CHECK:      graph_op "tfc.TensorTransfer
// CHECK:      graph_op "tfc.SendToHost

// In the loop body (a trivial back-edge), we should not need to do transfer,
// but since we are replicating BB args to BB1 to all devices, there is
// currently another transfer, which we can optimize away in the future.
// CHECK:      bb2
// CHECK:      graph_op "tfc.TensorTransfer

public func testSendsInALoopTPU() {
  TensorFlow.enableTPU()
  let maxCount = 10
  // a cannot be an integer tensor due to a TensorFlow Eigen bug (b/77737504).
  var a = Tensor<Float>(1)
  var count = 1

  while count < maxCount {
    a = _addScalarTensorsWithShape(a, a)
    // One send.
    _hostOp(a.toHost())
    count += 1
  }
  a += a
  let _ = a.array
}

// There are currently two tensor transfers, both involving TPU->CPU. The first
// one is to provide input to _hostOp(). The second one is to send the TPU-based
// tensor `a` to CPU at the end of each loop iteration, to maintain the current
// invariant in device partitioning pass that BB args are always replicated
// across all devices. That second transfer can be eliminated later.

// CHECK-LABEL: --- TFDevicePartition Per-Device Function Extraction Result: {{.*}}testSendsInALoopTPU{{.*}}TPU{{.*}}
// CHECK: bb1
// CHECK:   graph_op "tfc.D2DTensorSend
// CHECK:   graph_op "tfc.D2DTensorSend

public func testSendsInALoopWithNoResultTensor() {
  let maxCount = 10
  var count = 1
  var a = Tensor<Float>(1.0)
  while count < maxCount {
    a += a
    // One send.
    _hostOp(a.toHost())
    count += 1
  }
}

// No result tensor from this accelerator function.
// CHECK-LABEL: --- TFPartition Accelerator Result: {{.*}}testSendsInALoopWithNoResultTensor{{.*}}
//
// CHECK:      sil {{.*}}testSendsInALoopWithNoResultTensor{{.*}} () -> () {
// CHECK:        return {{.*}} : $()
// CHECK-NEXT: } // end sil function {{.*}}testSendsInALoopWithNoResultTensor{{.*}}

// FIXME: Eliminate the sends/receives in this case, since host does not use the
// value x.scalar! in any interesting way other than sending it back to device.
// On the other hand, this likely will not happen in a real use case.
public func test1RecvScalarCPU() {
  let x = Tensor<Float>(1.0) // expected-warning {{value implicitly copied to the host}}
  let y = x.scalar! + 2.0 // expected-note {{value used here}}

  let z = Tensor<Float>(y)
  let result = z+z
  let _ = result.scalar
}

// On device, we send x, and then receive the same value via x.scalar!
//
// CHECK-LABEL: --- TFPartition Accelerator Result: {{.*}}test1RecvScalarCPU{{.*}}
//
// CHECK:      graph_op "tfc.SendToHost
// Ideally this generic type should be changed to TensorHandle<Float>
// CHECK:      [[X2:%.*]] = graph_op "tfc.RecvFromHost
// the promoted tensor add on "x.scalar! + 2.0"
// CHECK:      graph_op "Add"([[X2]] : $TensorHandle<Builtin.FPIEEE32>, {{.*}} : $TensorHandle<Builtin.FPIEEE32>
// z + z
// CHECK:      graph_op "Add"
// CHECK:      graph_op "Add"({{.*}}) {T$dtype: i32 1, __device

// On host, we receive x, extract its scalar value, and then make a scalar
// tensor to send back to device.
//
// CHECK-LABEL: --- TFPartition Host Result: {{.*}}test1RecvScalarCPU{{.*}}
// CHECK:      function_ref @_swift_tfc_StartTensorComputation
// CHECK:      // function_ref static TensorHandle.receiveFromAccelerator
// CHECK-NEXT: function_ref
// CHECK-NEXT: [[X_HANDLE:%.*]] = apply
// CHECK:      // function_ref static TensorHandle.scalar(_:)
// CHECK-NEXT: [[MAKE_SCALAR_TENSOR_FN:%.*]] = function_ref
// CHECK-NEXT: [[NEW_X_HANDLE:%.*]] = apply [[MAKE_SCALAR_TENSOR_FN]]<Float>(
// CHECK:      // function_ref TensorHandle.sendToAccelerator(_:_:)
// CHECK-NEXT: [[SEND_FN:%.*]] = function_ref
// CHECK-NEXT: apply [[SEND_FN]]<Float>({{.*}}, {{.*}}, [[NEW_X_HANDLE]])
// CHECK:      function_ref @_swift_tfc_FinishTensorComputation


public func test1RecvScalarGPU() {
  TensorFlow.enableGPU()
  let x = Tensor<Float>(1.0) // expected-warning {{value implicitly copied to the host}}
  let y = x.scalar! + 2.0 // expected-note {{value used here}}
  let z = Tensor<Float>(y)
  let result = z+z
  let _ = result.scalar
}

// On receiving x.scalar in CPU, send it to ALL devices, because
// "+ 2.0" is promoted to run on all devices.
// CHECK-LABEL: --- TFDevicePartition Cross Device Tensor Transfer Annotation Result: {{.*}}test1RecvScalarGPU{{.*}}
// CHECK:      graph_op "tfc.RecvFromHost
// CHECK-NEXT: graph_op "tfc.TensorTransfer

// CHECK-LABEL: --- TFDevicePartition Per-Device Function Extraction Result: {{.*}}test1RecvScalarGPU{{.*}}CPU{{.*}}
// CHECK: graph_op "tfc.D2DTensorSend

// CHECK-LABEL: --- TFDevicePartition Per-Device Function Extraction Result: {{.*}}test1RecvScalarGPU{{.*}}GPU{{.*}}
// CHECK: graph_op "tfc.D2DTensorRecv

public func test1RecvScalarTPU() {
  TensorFlow.enableTPU()
  let x = Tensor<Float>(1.0) // expected-warning {{value implicitly copied to the host}}
  let y = x.scalar! + 2 // expected-note {{value used here}}
  let z = Tensor<Float>(y)
  let result = z + z
  _hostOp(result.toHost())
}

// On receiving x.scalar in CPU, add a __shapes pseudo attr for XLA
// compilation. The shape array attr gets propagated to TensorTransfer.
//
// CHECK-LABEL: --- TFDevicePartition Cross Device Tensor Transfer Annotation Result: {{.*}}test1RecvScalarTPU{{.*}}
// CHECK:      [[X_SCALAR_CPU:%.*]] = graph_op "tfc.RecvFromHost"() {tensorId: i32 0, __device: "/job:localhost/replica:0/task:0/device:CPU:0", __shapes: [$TensorShape: [$Int32: ]]} : $TensorHandle
// CHECK:      [[X_SCALAR_TPU:%.*]] = graph_op "tfc.TensorTransfer"([[X_SCALAR_CPU]] : $TensorHandle{{.*}}) {transferId: i32 0, srcDevice: "/job:localhost/replica:0/task:0/device:CPU:0", destDevice: "ALL_DEVICES", __shapes
// This is the promoted scalar add that computes x.scalar! + 2
// CHECK-NEXT: graph_op "Add"([[X_SCALAR_TPU]] : $TensorHandle
// This is z + z
// CHECK:      [[RESULT:%.*]] = graph_op "Add
// CHECK-NEXT: return [[RESULT]]

@inline(never)
public func atariSim(_ a: Tensor<Float>) -> Tensor<Float> {
  return a
}

public func test1RecvTensorCPU() {
  let a = Tensor<Float>(1.0) // expected-warning {{value implicitly copied to the host}}
  // One send.
  _hostOp(a.toHost())
  // One recv.
  var b = atariSim(a).toAccelerator()
  b += a
  let _ = b.array
}

// CHECK-LABEL: --- TFPartition Accelerator Result: {{.*}}test1RecvTensor{{.*}}
//
// CHECK:      graph_op "tfc.SendToHost{{.*}}([[A:%.*]] : $TensorHandle<Float>
// CHECK:      [[B:%.*]] = graph_op "tfc.RecvFromHost
// CHECK:      graph_op "Add"([[B]] : $TensorHandle<Float>, [[A]] : $TensorHandle<Float>

// CHECK-LABEL: --- TFPartition Host Result: {{.*}}test1RecvTensor{{.*}}
// CHECK:      function_ref @_swift_tfc_StartTensorComputation
// CHECK:      // function_ref static TensorHandle.receiveFromAccelerator
// CHECK-NEXT: function_ref
// CHECK-NEXT: [[A_HANDLE:%.*]] = apply
// CHECK-NEXT: [[A_TENSOR:%.*]] = struct $Tensor<Float> ([[A_HANDLE]]
// CHECK:      // function_ref {{.*}} atariSim(_:)
// CHECK-NEXT: [[ATARI_FN:%.*]] = function_ref
// CHECK-NEXT: [[B_TENSOR:%.*]] = apply [[ATARI_FN]]([[A_TENSOR]])
// CHECK-NEXT: [[B_HANDLE:%.*]] = struct_extract [[B_TENSOR]] : $Tensor<Float>, #Tensor.handle
// CHECK:      // function_ref TensorHandle.sendToAccelerator(_:_:)
// CHECK-NEXT: [[SEND_FN:%.*]] = function_ref
// CHECK-NEXT: apply [[SEND_FN]]<Float>({{.*}}, {{.*}}, [[B_HANDLE]])
// CHECK:      function_ref @_swift_tfc_FinishTensorComputation

public func test1RecvTensorTPU() {
  TensorFlow.enableTPU()
  let a_tpu_h: TensorHandle<Float> = #tfop("Const", dtype$dtype: 1, value$tensor: 1.0, __device: "TPU_SYSTEM")
  let a_tpu = Tensor<Float>(handle: a_tpu_h)
  // Tensor transfer for the param of atariSim(): TPU->CPU, and then CPU->host.
  let a_host = a_tpu.toHost(shape: [])
  // For the result of atariSim(): host -> CPU, and then CPU->TPU.
  var b = atariSim(a_host).toAccelerator(shape: [])
  b += a_tpu
  _hostOp(b)
}

public func test1RecvTensorTPU_ToHostNoShape_Error() {
  TensorFlow.enableTPU()
  // expected-error @+1 {{TPU outfeed dequeue supports dequeuing a single tensor -- did you specify shape?}}
  let a_tpu_h: TensorHandle<Float> = #tfop("Const", dtype$dtype: 1, value$tensor: 1.0, __device: "TPU_SYSTEM")
  let a_tpu = Tensor<Float>(handle: a_tpu_h)
  // Tensor transfer for the param of atariSim(): TPU->CPU, and then CPU->host.
  let a_host = a_tpu.toHost()
  // For the result of atariSim(): host -> CPU, and then CPU->TPU.
  var b = atariSim(a_host).toAccelerator(shape: [])
  b += a_tpu
  _hostOp(b)
}

public func test1RecvTensorTPU_ToAcceleratorNoShape_Error() {
  TensorFlow.enableTPU()
  let a_tpu_h: TensorHandle<Float> = #tfop("Const", dtype$dtype: 1, value$tensor: 1.0, __device: "TPU_SYSTEM")
  let a_tpu = Tensor<Float>(handle: a_tpu_h)
  // Tensor transfer for the param of atariSim(): TPU->CPU, and then CPU->host.
  let a_host = a_tpu.toHost(shape: [])
  // For the result of atariSim(): host -> CPU, and then CPU->TPU.
  var b = atariSim(a_host).toAccelerator()
  // This is the correct location
  // expected-error @+1 {{TPU infeed enqueue supports enqueuing a single tensor -- did you specify shape?}}
  b += a_tpu
  _hostOp(b)
}

// Specifying shapes for CPU<->GPU sends/recvs should not hurt.
public func test1RecvTensorGPU_WithShapes() {
  TensorFlow.enableGPU()
  let a_gpu_h: TensorHandle<Float> = #tfop("Const", dtype$dtype: 1, value$tensor: 1.0, __device: "/job:localhost/replica:0/task:0/device:CPU:0")
  let a_gpu = Tensor<Float>(handle: a_gpu_h)
  // One send.
  // Tensor transfer for the param of atariSim(): GPU->CPU, and then CPU->host.
  let a_host = a_gpu.toHost(shape: [])
  // One recv.
  // For the result of atariSim(): host -> CPU, and then CPU->GPU.
  var b = atariSim(a_host).toAccelerator(shape: [])
  b += a_gpu
  _hostOp(b)
}

public func testRecvsInALoop() {
  let maxCount = 10
  var count = 1
  var a = Tensor<Float>(1.0)
  while count < maxCount {
    // One recv.
    let b = atariSim(a.toHost()).toAccelerator()
    a += b
    count += 1
  }
  a += a
  // This one should not be a send.
  _hostOp(a.toHost())
}

let globalIterator: ResourceHandle =
  #tfop("Iterator", shared_name: "foo", container: "bar",
        output_types$dtype: [1], output_shapes: [TensorShape()])
let globalNextIterator: ResourceHandle =
  #tfop("IteratorGetNextAsOptional", globalIterator,
        output_types$dtype: [1], output_shapes: [TensorShape()])

public func resourceHandlesCanBeSentOrReceived() {
  // The following constant is a workaround to extend the deabstraction scope so
  // that a resource handle is not an input argument to the tensor program. This
  // will go away when we convert arg tensors to Swift->TF sends.
  let _ = Tensor<Float>(1.0)
  // expected-warning @+2 {{value implicitly copied to the accelerator}}
  let _: VariantHandle =
    #tfop("IteratorGetNextAsOptional", globalIterator,
    output_types$dtype: [1], output_shapes: [TensorShape()])
  // expected-warning @+2 {{value implicitly copied to the host}}
  let localIterator: ResourceHandle =
    #tfop("Iterator", shared_name: "foo", container: "bar",
          output_types$dtype: [1], output_shapes: [TensorShape()])
  _hostOp(localIterator) // expected-note {{value used here}}
  // The following constant is a workaround to extend the deabstraction scope so
  // that a resource handle is sent back to the host instead of being an output.
  let _ = Tensor<Float>(1.0)
}

public func resourceHandlesCanBeResults() {
  let iterator: ResourceHandle =
    #tfop("Iterator", shared_name: "foo", container: "bar",
  	      output_types$dtype: [1], output_shapes: [TensorShape()])
  _hostOp(iterator)
}


public func variantHandlesCanBeSentOrReceived() {
  // The following constant is a workaround to extend the deabstraction scope so
  // that a resource handle is not an input argument to the tensor program. This
  // will go away when we convert arg tensors to Swift->TF sends.
  let _ = Tensor<Float>(1.0)
  // expected-warning @+2 {{value implicitly copied to the accelerator}}
  let localNextIterator: VariantHandle =
    #tfop("IteratorGetNextAsOptional", globalNextIterator,
    output_types$dtype: [1], output_shapes: [TensorShape()])
  // expected-warning @-2 {{value implicitly copied to the host}}
  _hostOp(localNextIterator) // expected-note {{value used here}}
  // The following constant is a workaround to extend the deabstraction scope so
  // that a resource handle is sent back to the host instead of being an output.
  let _ = Tensor<Float>(1.0)
}

public func variantHandlesCanBeResults() {
  let iterator: ResourceHandle =
    #tfop("Iterator", shared_name: "foo", container: "bar",
  	      output_types$dtype: [1], output_shapes: [TensorShape()])
  let nextIterator: VariantHandle =
    #tfop("IteratorGetNextAsOptional", iterator,
    output_types$dtype: [1], output_shapes: [TensorShape()])
  _hostOp(nextIterator)
}
