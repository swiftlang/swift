// RUN: %target-swift-frontend -Xllvm -tf-dump-intermediates -Xllvm -tf-dump-graph -O -emit-sil %s -verify | %FileCheck %s

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


// expected-warning@+1 {{'x' implicitly copied to the accelerator}}
public func test1SendWithParam(x: Float) {
  TensorFlow.enableGPU()
  var a = Tensor<Float>(x) // expected-note {{value used here}}
  // One send.
  _hostOp(a.toHost())
  a += 1
  // This one should not be a send.
  _hostOp(a.toHost())
}

// GPU function takes the input arg of x.
// CHECK-LABEL: --- TFDevicePartition Per-Device Function Extraction Result: {{.*}}test1SendWithParam{{.*}}GPU{{.*}}
// CHECK: bb0(%0 : $TensorHandle
// CHECK: builtin "__tfop_tfc.D2DTensorSend

// CPU function takes no input arg.
// CHECK-LABEL: --- TFDevicePartition Per-Device Function Extraction Result: {{.*}}test1SendWithParam{{.*}}CPU{{.*}}
// CHECK: bb0:
// CHECK: builtin "__tfop_tfc.D2DTensorRecv

// The _Send node should be hooked up as a control dependency on the return
 // node, so that Sends gets run before the function returns.
// CHECK:        function {
// CHECK:          name: "{{.*}}test1SendWithParam{{.*}}.tf_CPU.device_partition"
// CHECK:        function {
// CHECK:          name: "{{.*}}test1SendWithParam{{.*}}.tf_GPU.device_partition"
// CHECK:          node_def {
// CHECK:            name: "RunControlDependency"
// CHECK:            op: "Identity"
// CHECK-NEXT:       input: "op/test1SendWithParam.x_
// CHECK-NEXT:       input: "^tf_send_0"

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

// CHECK-LABEL: --- TFDevicePartition Cross Device Tensor Transfer Annotation Result: {{.*}}testSendsInALoopGPU{{.*}}

// In the loop head (where cond and body are evaluated), send a to CPU, which
// then sends it to host.
// CHECK:      bb1
// CHECK:      builtin "__tfop_tfc.TensorTransfer
// CHECK:      builtin "__tfop_tfc.SendToHost

// In the loop body (a trivial back-edge), we should not need to do transfer,
// but since we are replicating BB args to BB1 to all devices, there is
// currently another transfer, which we can optimize away in the future.
// CHECK:      bb2
// CHECK:      builtin "__tfop_tfc.TensorTransfer

public func testSendsInALoopWithNoResultTensor() {
  let maxCount = 10
  var count = 1
  var a = Tensor<Float>(1.0)
  while count < maxCount {
    a += a
    // One send.
    print(a.toHost())
    count += 1
  }
}

// No result tensor from this accelerator function.
// CHECK-LABEL: --- TFPartition Accelerator Result: {{.*}}testSendsInALoopWithNoResultTensor{{.*}}
//
// CHECK:      sil {{.*}}testSendsInALoopWithNoResultTensor{{.*}} () -> () {
// CHECK:        return {{.*}} : $()
// CHECK-NEXT: } // end sil function {{.*}}testSendsInALoopWithNoResultTensor{{.*}}

public func testCannotSendResource() {
  // expected-error @+2 {{This value type cannot be sent/received}}
  let iterator: ResourceHandle =
    #tfop("Iterator", shared_name: "foo", container: "bar")

  print(iterator)
  let _ = Tensor<Float>(1.0)
}

// FIXME: Eliminate the sends/receives in this case, since host does not use the
// value x.scalar! in any interesting way other than sending it back to device.
// On the other hand, this likely will not happen in a real use case.
public func test1RecvScalarCPU() {
  let x = Tensor<Float>(1.0) // expected-warning {{value implicitly copied to the host}}
  let y = x.scalar! + 2.0 // expected-note {{value used here}}
  // expected-warning @-1 {{value implicitly copied to the accelerator}}

  let z = Tensor<Float>(y)
  let result = z+z
  let _ = result.scalar
}

// On device, we send x, and then receive the same value via x.scalar!
//
// CHECK-LABEL: --- TFPartition Accelerator Result: {{.*}}test1RecvScalarCPU{{.*}}
//
// CHECK:      builtin "__tfop_tfc.SendToHost
// Ideally this generic type should be changed to TensorHandle<Float>
// CHECK:      [[X2:%.*]] = builtin "__tfop_tfc.RecvFromHost
// the promoted tensor add on "x.scalar! + 2.0"
// CHECK:      builtin "__tfop_Add,$in,$in,device"([[X2]] : $TensorHandle<Builtin.FPIEEE32>, {{.*}} : $TensorHandle<Builtin.FPIEEE32>
// z + z
// CHECK:      builtin "__tfop_Add,$in,$in,T,device"

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
  // expected-warning @-1 {{value implicitly copied to the accelerator}}

  let z = Tensor<Float>(y)
  let result = z+z
  let _ = result.scalar
}

// On receiving x.scalar in CPU, send it to ALL devices, because
// "+ 2.0" is promoted to run on all devices.
// CHECK-LABEL: --- TFDevicePartition Cross Device Tensor Transfer Annotation Result: {{.*}}test1RecvScalarGPU{{.*}}
// CHECK:      builtin "__tfop_tfc.RecvFromHost
// CHECK:      string_literal utf8 "/device:CPU:0"
// CHECK-NEXT: string_literal utf8 "ALL_DEVICES"
// CHECK-NEXT: builtin "__tfop_tfc.TensorTransfer

// CHECK-LABEL: --- TFDevicePartition Per-Device Function Extraction Result: {{.*}}test1RecvScalarGPU{{.*}}GPU{{.*}}
// CHECK: builtin "__tfop_tfc.D2DTensorRecv

// CHECK-LABEL: --- TFDevicePartition Per-Device Function Extraction Result: {{.*}}test1RecvScalarGPU{{.*}}CPU{{.*}}
// CHECK: builtin "__tfop_tfc.D2DTensorSend

@inline(never)
public func atariSim(_ a: Tensor<Float>) -> Tensor<Float> {
  return a
}

public func test1RecvTensor() {
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
// CHECK:      builtin "__tfop_tfc.SendToHost{{.*}}<TensorHandle<Float>>([[A:%.*]] : $TensorHandle<Float>
// CHECK:      [[B:%.*]] = builtin "__tfop_tfc.RecvFromHost
// CHECK:      builtin "__tfop_Add,$in,$in,T,device"([[B]] : $TensorHandle<Float>, [[A]] : $TensorHandle<Float>

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
