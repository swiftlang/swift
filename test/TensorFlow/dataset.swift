// RUN: %target-swift-frontend -Xllvm -tf-dump-intermediates -O -emit-sil -verify %s
// RUN: %target-swift-frontend -Xllvm -tf-dump-intermediates -O -emit-sil -verify %s | %FileCheck %s
import TensorFlow

public func testDataset() {
  TensorFlow.enableTPU(infeed: true)
  let x: Tensor<Int32> = #tfop(
    "tfc.makeIteratorGetNextWithDatasets",
    filepath: "dummy_path")
  let y = x + 1
  print(y.array.scalars[0])
}

// CHECK-LABEL: --- TFPartition Accelerator Result: {{.*}}testDataset{{.*}}
// CHECK: bb0:
// CHECK:   [[STR:%[0-9]+]] = string_literal utf8 "dummy_path"
// CHECK:   [[GETNEXT:%[0-9]+]] = builtin "__tfop_tfc.makeIteratorGetNextWithDatasets,filepath"([[STR]] : $Builtin.RawPointer) : $TensorHandle<Int32>
// CHECK:        [[RESULT:%[0-9]+]] = builtin "__tfop_Add,$in,$in"([[GETNEXT]] : $TensorHandle<Int32>, {{.*}} : $TensorHandle<Int32>)
// CHECK-NEXT:   return [[RESULT]] : $TensorHandle<Int32>
