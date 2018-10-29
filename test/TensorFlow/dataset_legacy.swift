// RUN: %target-swift-frontend -Xllvm -tf-dump-intermediates -Xllvm -tf-dump-graph -O -emit-sil -verify %s | %FileCheck %s
import TensorFlow

public func testDatasetWithFakeData() {
  TensorFlow.enableTPU(infeed: true)
  let x: TensorHandle<Float> = #tfop(
    "tfc.makeIteratorGetNextWithDatasets",
    dataSource: "fake",
    filePath: "dummy_path",
    batchSize: Int64(1),
    outputShapes: [TensorShape()])
  let y = Tensor<Float>(handle: x) + 1
  _hostOp(y.array.scalars[0])
}

// CHECK-LABEL: --- TFPartition Accelerator Result: {{.*}}testDatasetWithFakeData{{.*}}
// CHECK: bb0:
// CHECK:        [[GETNEXT:%[0-9]+]] = graph_op "tfc.makeIteratorGetNextWithDatasets{{.*}} : $TensorHandle<Float>
// CHECK:        [[RESULT:%[0-9]+]] = graph_op "Add"([[GETNEXT]] : $TensorHandle<Float>, {{.*}} : $TensorHandle<Float>
// CHECK-NEXT:   return [[RESULT]] : $TensorHandle<Float>

public func testDatasetWithMNIST() {
  TensorFlow.enableTPU(infeed: true)
  let (images1, labels1): (TensorHandle<Float>, TensorHandle<Int32>) = #tfop(
    "tfc.makeIteratorGetNextWithDatasets",
    dataSource: "mnist",
    filePath: "some_path",
    batchSize: Int64(64),
    output_shapes: [TensorShape(64,224,224,3), TensorShape(64)])
  let images : TensorHandle<Float> = #tfop("Identity", images1)
  let labels : TensorHandle<Int32> = #tfop("Identity", labels1)
  // Confirm we can add more nodes to the graph.
  let imagesMod = Tensor<Float>(handle: images) + 1
  let labelsMod = Tensor<Int32>(handle: labels) + 2
  _hostOp(imagesMod.array.scalars[0])
  _hostOp(labelsMod.array.scalars[0])
}

// CHECK-LABEL: --- TFPartition Accelerator Result: {{.*}}testDatasetWithMNIST{{.*}}
// CHECK: bb0:
// CHECK:  (%0, %1) = graph_op "tfc.makeIteratorGetNextWithDatasets{{.*}} : $TensorHandle<Float>, $TensorHandle<Int32>
// CHECK: graph_op "Add"(
// CHECK: graph_op "Add"(
// The operands can appear in arbitrary order here.
// CHECK:  [[RESULT:%.*]] = tuple ({{.*}} : $TensorHandle<{{.*}}>, {{.*}} : $TensorHandle<{{.*}}>)
// CHECK-NEXT:  return [[RESULT]] : $(TensorHandle<{{.*}}>, TensorHandle<{{.*}}>)
