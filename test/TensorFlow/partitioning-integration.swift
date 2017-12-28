// RUN: %target-swift-frontend -Xllvm -tf-dump-intermediates -O -emit-sil %s | %FileCheck %s

// TODO: Check diagnostics by enabling -verify mode.

import TensorFlow

public func testTensor() {
  var x = Tensor<Float>(oneD: 1.0, 2.0, 3.0)
  x += x
  x -= x
  print(x)
  var y = Tensor1D<Float>(1, 2, 3.0)
  y += y
  print(y)
}

// CHECK-LABEL: --- TFPartition Accelerator Result: _T04main10testTensoryyF
// CHECK:  sil private @_T04main10testTensoryyF.tf_partition : $@callee_owned (TensorHandle<Float>) -> TensorHandle<Float> {
// CHECK: bb0(%0 : $TensorHandle<Float>):
// CHECK-NEXT:   %1 = builtin "__tfop_Add__tt:t__"(%0 : $TensorHandle<Float>, %0 : $TensorHandle<Float>) : $TensorHandle<Float>
// CHECK-NEXT:   %2 = builtin "__tfop_Sub__tt:t__"(%1 : $TensorHandle<Float>, %1 : $TensorHandle<Float>) : $TensorHandle<Float>
// CHECK-NEXT:   %3 = builtin "tensorflowSend_1"<TensorHandle<Float>>(%2 : $TensorHandle<Float>) : $()
// CHECK-NEXT:   %4 = builtin "tensorflowReceive_0"<TensorHandle<Float>>() : $TensorHandle<Float>
// CHECK-NEXT:   %5 = builtin "__tfop_Add__tt:t__"(%4 : $TensorHandle<Float>, %4 : $TensorHandle<Float>) : $TensorHandle<Float>
// CHECK-NEXT:   return %5 : $TensorHandle<Float>



public func testScalar(f: Float) {
  var x = Tensor<Float>(zeroD: f) + Tensor<Float>(zeroD: 1.0)
  x += x
  print(x)
}

// CHECK-LABEL: --- TFPartition Accelerator Result: {{.*}}testScalar{{.*}}
// CHECK: sil private @{{.*}}testScalar{{.*}} : $@callee_owned (TensorHandle<Float>) -> TensorHandle<Float> {
// CHECK: bb0(%0 : $TensorHandle<Float>):
// CHECK-NEXT:   %1 = metatype $@thick Float.Type
// CHECK-NEXT:   %2 = float_literal $Builtin.FPIEEE32, 0x3F800000 // 1
// CHECK-NEXT:   %3 = builtin "__tfop_Const__dc:t__"(%1 : $@thick Float.Type, %2 : $Builtin.FPIEEE32) : $TensorHandle<Float>
// CHECK-NEXT:   %4 = builtin "__tfop_Add__tt:t__"(%0 : $TensorHandle<Float>, %3 : $TensorHandle<Float>) : $TensorHandle<Float>
// CHECK-NEXT:   %5 = builtin "__tfop_Add__tt:t__"(%4 : $TensorHandle<Float>, %4 : $TensorHandle<Float>) : $TensorHandle<Float>
// CHECK-NEXT:   return %5 : $TensorHandle<Float>
// CHECK-NEXT: }

