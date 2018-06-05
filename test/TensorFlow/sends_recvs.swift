// RUN: %target-swift-frontend -Xllvm -tf-dump-intermediates -Xllvm -tf-dump-graph -O -emit-sil -verify %s
// RUN: %target-swift-frontend -Xllvm -tf-dump-intermediates -O -emit-sil %s -verify | %FileCheck %s

import TensorFlow

public func test1Send() {
  var a = Tensor<Float>(1.0)
  // One send.
  print(a.toHost())
  a += 1
  // This one should not be a send.
  print(a.toHost())
}

// CHECK-LABEL: --- TFPartition Host Result: {{.*}}test1Send{{.*}}
// CHECK:      function_ref @_swift_tfc_StartTensorComputation
// CHECK-NEXT: [[TC:%.*]] = apply

// CHECK: integer_literal $Builtin.Int64, 0
// CHECK-NEXT: [[TENSOR_ID:%.*]] = struct $Int
// CHECK:      // function_ref static TensorHandle.receiveFromDevice(_:_:)
// CHECK-NEXT:      [[RECEIVE_H:%.*]] = function_ref @
// CHECK-NEXT: apply [[RECEIVE_H]]<Float>([[TC]], [[TENSOR_ID]]

// CHECK:      function_ref @_swift_tfc_FinishTensorComputation


public func test2Sends() {
  var a = Tensor<Float>(1.0)
  // One send.
  print(a.toHost())
  a += 2
  // Another send.
  print(a.toHost())
  a += 3
  // This one should not be a send.
  print(a.toHost())
}

// CHECK-LABEL: --- TFPartition Host Result: {{.*}}test2Send{{.*}}
// CHECK:      function_ref @_swift_tfc_StartTensorComputation
// CHECK-NEXT: [[TC:%.*]] = apply

// The first receive is over tensor id 0.
// CHECK: integer_literal $Builtin.Int64, 0
// CHECK-NEXT: [[TENSOR_ID0:%.*]] = struct $Int
// CHECK:      // function_ref static TensorHandle.receiveFromDevice(_:_:)
// CHECK-NEXT:      [[RECEIVE_H0:%.*]] = function_ref @
// CHECK-NEXT: apply [[RECEIVE_H0]]<Float>([[TC]], [[TENSOR_ID0]]

// The second receive is over tensor id 1.
// CHECK: function_ref print(_:separator:terminator:)
// CHECK: integer_literal $Builtin.Int64, 1
// CHECK-NEXT: [[TENSOR_ID1:%.*]] = struct $Int
// CHECK:      // function_ref static TensorHandle.receiveFromDevice(_:_:)
// CHECK-NEXT:      [[RECEIVE_H1:%.*]] = function_ref @
// CHECK-NEXT: apply [[RECEIVE_H1]]<Float>([[TC]], [[TENSOR_ID1]]

// CHECK:      function_ref @_swift_tfc_FinishTensorComputation

public func testSendsInALoop() {
  let maxCount = 10
  var count = 1
  var a = Tensor<Float>(1.0)
  while count < maxCount {
    a += a
    // One send.
    print(a.toHost())
    count += 1
  }
  a += a
  // This one should not be a send.
  print(a.toHost())
}

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
public func test1RecvScalar() {
  let x = Tensor<Float>(1.0) // expected-warning {{value implicitly copied to the host}}
  let y = x.scalar! + 2.0 // expected-note {{value used here}}
  // expected-warning @-1 {{value implicitly copied to the accelerator}}

  let z = Tensor<Float>(y)
  let result = z+z
  let _ = result.scalar
}

// On device, we send x, and then receive the same value via x.scalar!
//
// CHECK-LABEL: --- TFPartition Accelerator Result: {{.*}}test1RecvScalar{{.*}}
//
// CHECK:      builtin "tensorflowSend_1"<TensorHandle<Float>>([[X:%.*]] : $TensorHandle<Float>)
// Ideally this generic type should be changed to TensorHandle<Float>
// CHECK:      [[X2:%.*]] = builtin "tensorflowReceive_0"<TensorHandle<Builtin.FPIEEE32>>
// the promoted tensor add on "x.scalar! + 2.0"
// CHECK:      builtin "__tfop_Add,$in,$in,device"([[X2]] : $TensorHandle<Builtin.FPIEEE32>, {{.*}} : $TensorHandle<Builtin.FPIEEE32>
// z + z
// CHECK:      builtin "__tfop_Add,$in,$in,T,device"

// On host, we receive x, extract its scalar value, and then make a scalar
// tensor to send back to device.
//
// CHECK-LABEL: --- TFPartition Host Result: {{.*}}test1RecvScalar{{.*}}
// CHECK:      function_ref @_swift_tfc_StartTensorComputation
// CHECK:      // function_ref static TensorHandle.receiveFromDevice
// CHECK-NEXT: function_ref 
// CHECK-NEXT: [[X_HANDLE:%.*]] = apply
// CHECK:      // function_ref static TensorHandle.scalar(_:)
// CHECK-NEXT: [[MAKE_SCALAR_TENSOR_FN:%.*]] = function_ref
// CHECK-NEXT: [[NEW_X_HANDLE:%.*]] = apply [[MAKE_SCALAR_TENSOR_FN]]<Float>(
// CHECK:      // function_ref TensorHandle.sendToDevice(_:_:)
// CHECK-NEXT: [[SEND_FN:%.*]] = function_ref
// CHECK-NEXT: apply [[SEND_FN]]<Float>({{.*}}, {{.*}}, [[NEW_X_HANDLE]])
// CHECK:      function_ref @_swift_tfc_FinishTensorComputation

@inline(never)
public func atariSim(_ a: Tensor<Float>) -> Tensor<Float> {
  return a
}

public func test1RecvTensor() {
  let a = Tensor<Float>(1.0) // expected-warning {{value implicitly copied to the host}}
  // One send.
  print(a.toHost())
  // One recv.
  var b = atariSim(a).toDevice()
  b += a
  let _ = b.array
}

// CHECK-LABEL: --- TFPartition Accelerator Result: {{.*}}test1RecvTensor{{.*}}
//
// CHECK:      builtin "tensorflowSend_1"<TensorHandle<Float>>([[A:%.*]] : $TensorHandle<Float>)
// CHECK-NEXT: [[B:%.*]] = builtin "tensorflowReceive_0"<TensorHandle<Float>>
// CHECK:      builtin "__tfop_Add,$in,$in,T,device"([[B]] : $TensorHandle<Float>, [[A]] : $TensorHandle<Float>

// CHECK-LABEL: --- TFPartition Host Result: {{.*}}test1RecvTensor{{.*}}
// CHECK:      function_ref @_swift_tfc_StartTensorComputation
// CHECK:      // function_ref static TensorHandle.receiveFromDevice
// CHECK-NEXT: function_ref 
// CHECK-NEXT: [[A_HANDLE:%.*]] = apply
// CHECK-NEXT: [[A_TENSOR:%.*]] = struct $Tensor<Float> ([[A_HANDLE]]
// CHECK:      // function_ref atariSim(_:)
// CHECK-NEXT: [[ATARI_FN:%.*]] = function_ref
// CHECK-NEXT: [[B_TENSOR:%.*]] = apply [[ATARI_FN]]([[A_TENSOR]])
// CHECK-NEXT: [[B_HANDLE:%.*]] = struct_extract [[B_TENSOR]] : $Tensor<Float>, #Tensor.handle
// CHECK:      // function_ref TensorHandle.sendToDevice(_:_:)
// CHECK-NEXT: [[SEND_FN:%.*]] = function_ref
// CHECK-NEXT: apply [[SEND_FN]]<Float>({{.*}}, {{.*}}, [[B_HANDLE]])
// CHECK:      function_ref @_swift_tfc_FinishTensorComputation
