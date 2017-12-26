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
// CHECK:  sil private @_T04main10testTensoryyF.tf_partition : $@callee_owned (TensorCore<Float>) -> TensorCore<Float> {
// CHECK: bb0(%0 : $TensorCore<Float>):
// CHECK:   %1 = builtin "__tfop_Add__tt__"(%0 : $TensorCore<Float>, %0 : $TensorCore<Float>) : $TensorCore<Float>
// CHECK:   %2 = builtin "__tfop_Sub__tt__"(%1 : $TensorCore<Float>, %1 : $TensorCore<Float>) : $TensorCore<Float>
// CHECK:   %3 = builtin "tensorflowSend_1"<TensorCore<Float>>(%2 : $TensorCore<Float>) : $()
// CHECK:   %4 = builtin "tensorflowReceive_0"<TensorCore<Float>>() : $TensorCore<Float>
// CHECK:   %5 = builtin "__tfop_Add__tt__"(%4 : $TensorCore<Float>, %4 : $TensorCore<Float>) : $TensorCore<Float>
// CHECK:   return %5 : $TensorCore<Float>


