// RUN: %target-swift-frontend -Xllvm -tf-dump-intermediates -Xllvm -tf-dump-graph -Xllvm -tf-target-tpu -Xllvm -tf-tpu-use-infeed -O -emit-sil -verify %s
// RUN: %target-swift-frontend -Xllvm -tf-dump-intermediates -Xllvm -tf-dump-graph -Xllvm -tf-target-tpu -Xllvm -tf-tpu-use-infeed -O -emit-sil -verify %s | %FileCheck %s
import TensorFlow

public func testDataset() {
  let x: Tensor<Int32> = #tfop(
    "tfc.makeIteratorGetNextWithDatasets",
    filepath: "dummy_path")
  let y = x + 1
  print(y.array.scalars[0])
}

// CHECK-LABEL: --- TFPartition Accelerator Result: {{.*}}testDataset{{.*}}
// CHECK: bb0:
// CHECK-NEXT:   %0 = string_literal utf8 "dummy_path"
// CHECK-NEXT:   %1 = builtin "__tfop_tfc.makeIteratorGetNextWithDatasets,filepath"(%0 : $Builtin.RawPointer) : $TensorHandle<Int32>
// CHECK:        %6 = builtin "__tfop_Add,$in,$in"(%1 : $TensorHandle<Int32>, {{.*}} : $TensorHandle<Int32>)
// CHECK-NEXT:   return %6 : $TensorHandle<Int32>
