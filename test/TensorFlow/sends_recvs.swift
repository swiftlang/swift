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
  // expected-error @+2 {{This value is not receivable}}
  let iterator: ResourceHandle =
    #tfop("Iterator", shared_name: "foo", container: "bar")

  print(iterator)
  let _ = Tensor<Float>(1.0)
}
