// RUN: %target-swift-frontend -Xllvm -tf-dump-intermediates -O -tensorflow -emit-sil %s | %FileCheck %s

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

// CHECK: --- TFPartition Accelerator Result: _T04main10testTensoryyF
// CHECK:  sil private @_T04main10testTensoryyF.tf_partition : $@callee_owned (TensorHandle<Float>) -> TensorHandle<Float> {
// CHECK: bb0(%0 : $TensorHandle<Float>):
// CHECK:   %1 = builtin "__tfop_Add__tt__"(%0 : $TensorHandle<Float>, %0 : $TensorHandle<Float>) : $TensorHandle<Float>
// CHECK:   %2 = builtin "__tfop_Sub__tt__"(%1 : $TensorHandle<Float>, %1 : $TensorHandle<Float>) : $TensorHandle<Float>
// CHECK:   %3 = builtin "tensorflowSend_1"<TensorHandle<Float>>(%2 : $TensorHandle<Float>) : $()
// CHECK:   %4 = builtin "tensorflowReceive_0"<TensorHandle<Float>>() : $TensorHandle<Float>
// CHECK:   %5 = builtin "__tfop_Add__tt__"(%4 : $TensorHandle<Float>, %4 : $TensorHandle<Float>) : $TensorHandle<Float>
// CHECK:   return %5 : $TensorHandle<Float>


