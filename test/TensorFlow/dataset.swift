// RUN: %target-swift-frontend -Xllvm -tf-dump-intermediates -Xllvm -tf-dump-graph -O -emit-sil -verify %s
// RUN: %target-swift-frontend -Xllvm -tf-dump-intermediates -Xllvm -tf-dump-graph -O -emit-sil -verify %s | %FileCheck %s
import TensorFlow

public func testDatasetWithFakeData() {
  TensorFlow.enableTPU(infeed: true)
  let x: Tensor<Float> = #tfop(
    "tfc.makeIteratorGetNextWithDatasets",
    dataSource: "fake",
    filePath: "dummy_path",
    batchSize: 1,
    outputShapes: [TensorShape()])
  let y = x + 1
  print(y.array.scalars[0])
}

// CHECK-LABEL: --- TFPartition Accelerator Result: {{.*}}testDatasetWithFakeData{{.*}}
// CHECK: bb0:
// CHECK:        [[GETNEXT:%[0-9]+]] = builtin "__tfop_tfc.makeIteratorGetNextWithDatasets{{.*}} : $TensorHandle<Float>
// CHECK:        [[RESULT:%[0-9]+]] = builtin "__tfop_Add,$in,$in"([[GETNEXT]] : $TensorHandle<Float>, {{.*}} : $TensorHandle<Float>)
// CHECK-NEXT:   return [[RESULT]] : $TensorHandle<Float>

public func testDatasetWithMNIST() {
  TensorFlow.enableTPU(infeed: true)
  let (images1, labels1): (TensorHandle<Float>, TensorHandle<Int32>) = #tfop(
    "tfc.makeIteratorGetNextWithDatasets",
    dataSource: "mnist",
    filePath: "some_path",
    batchSize: 64,
    output_shapes: [TensorShape(64,224,224,3), TensorShape(64)])
  let images : Tensor<Float> = #tfop("Identity", images1)
  let labels : Tensor<Int32> = #tfop("Identity", labels1)
  // Confirm we can add more nodes to the graph.
  let imagesMod = images + 1
  let labelsMod = labels + 2
  print(imagesMod.array.scalars[0])
  print(labelsMod.array.scalars[0])
}

// CHECK-LABEL: --- TFPartition Accelerator Result: {{.*}}testDatasetWithMNIST{{.*}}
// CHECK: bb0:
// CHECK:  builtin "__tfop_tfc.makeIteratorGetNextWithDatasets{{.*}} : $(TensorHandle<Float>, TensorHandle<Int32>)
// CHECK-NEXT:  tuple_extract {{.*}} : $(TensorHandle<Float>, TensorHandle<Int32>), 0
// CHECK-NEXT:  tuple_extract {{.*}} : $(TensorHandle<Float>, TensorHandle<Int32>), 1
// CHECK: builtin "__tfop_Add,$in,$in"(
// CHECK: builtin "__tfop_Add,$in,$in"(
// The operands can appear in arbitrary order here.
// CHECK:  [[RESULT:%.*]] = tuple ({{.*}} : $TensorHandle<{{.*}}>, {{.*}} : $TensorHandle<{{.*}}>)
// CHECK-NEXT:  return [[RESULT]] : $(TensorHandle<{{.*}}>, TensorHandle<{{.*}}>)
