// RUN: %target-swift-frontend -Xllvm -tf-dump-intermediates -Xllvm -tf-dump-graph -O -emit-sil -verify %s | %FileCheck %s
// RUN: %target-swift-frontend -Xllvm -tf-dump-intermediates -Xllvm -tf-dump-graph -Xllvm -tf-strict-deabstraction -O -emit-sil -verify %s | %FileCheck %s --check-prefix=STRICTDA
import TensorFlow

public func testDatasetWithFakeData() {
  TensorFlow.enableTPU(infeed: true)
  let x: TensorHandle<Float> = #tfop(
    "tfc.makeIteratorGetNextWithDatasets",
    dataSource: "fake",
    filePath: "dummy_path",
    batchSize: 1,
    outputShapes: [TensorShape()])
  let y = Tensor<Float>(handle: x) + 1
  print(y.array.scalars[0])
}

// CHECK-LABEL: --- TFPartition Accelerator Result: {{.*}}testDatasetWithFakeData{{.*}}
// CHECK: bb0:
// CHECK:        [[GETNEXT:%[0-9]+]] = builtin "__tfop_tfc.makeIteratorGetNextWithDatasets{{.*}} : $TensorHandle<Float>
// CHECK:        [[RESULT:%[0-9]+]] = builtin "__tfop_Add,$in,$in,T,__device"([[GETNEXT]] : $TensorHandle<Float>, {{.*}} : $TensorHandle<Float>
// CHECK-NEXT:   return [[RESULT]] : $TensorHandle<Float>

// STRICTDA-LABEL: --- TFPartition Accelerator Result: {{.*}}testDatasetWithFakeData{{.*}}
// STRICTDA: bb0:
// STRICTDA:        [[GETNEXT:%[0-9]+]] = graph_op "tfc.makeIteratorGetNextWithDatasets{{.*}} : $TensorHandle<Float>
// STRICTDA:        [[RESULT:%[0-9]+]] = graph_op "Add,i,i"([[GETNEXT]] : $TensorHandle<Float>, {{.*}} : $TensorHandle<Float>
// STRICTDA-NEXT:   return [[RESULT]] : $TensorHandle<Float>

public func testDatasetWithMNIST() {
  TensorFlow.enableTPU(infeed: true)
  let (images1, labels1): (TensorHandle<Float>, TensorHandle<Int32>) = #tfop(
    "tfc.makeIteratorGetNextWithDatasets",
    dataSource: "mnist",
    filePath: "some_path",
    batchSize: 64,
    output_shapes: [TensorShape(64,224,224,3), TensorShape(64)])
  let images : TensorHandle<Float> = #tfop("Identity", images1)
  let labels : TensorHandle<Int32> = #tfop("Identity", labels1)
  // Confirm we can add more nodes to the graph.
  let imagesMod = Tensor<Float>(handle: images) + 1
  let labelsMod = Tensor<Int32>(handle: labels) + 2
  print(imagesMod.array.scalars[0])
  print(labelsMod.array.scalars[0])
}

// CHECK-LABEL: --- TFPartition Accelerator Result: {{.*}}testDatasetWithMNIST{{.*}}
// CHECK: bb0:
// CHECK:  builtin "__tfop_tfc.makeIteratorGetNextWithDatasets{{.*}} : $(TensorHandle<Float>, TensorHandle<Int32>)
// CHECK-NEXT:  tuple_extract {{.*}} : $(TensorHandle<Float>, TensorHandle<Int32>), 0
// CHECK-NEXT:  tuple_extract {{.*}} : $(TensorHandle<Float>, TensorHandle<Int32>), 1
// CHECK: builtin "__tfop_Add,$in,$in,T,__device"(
// CHECK: builtin "__tfop_Add,$in,$in,T,__device"(
// The operands can appear in arbitrary order here.
// CHECK:  [[RESULT:%.*]] = tuple ({{.*}} : $TensorHandle<{{.*}}>, {{.*}} : $TensorHandle<{{.*}}>)
// CHECK-NEXT:  return [[RESULT]] : $(TensorHandle<{{.*}}>, TensorHandle<{{.*}}>)

// STRICTDA-LABEL: --- TFPartition Accelerator Result: {{.*}}testDatasetWithMNIST{{.*}}
// STRICTDA: bb0:
// STRICTDA:  (%0, %1) = graph_op "tfc.makeIteratorGetNextWithDatasets{{.*}} : $TensorHandle<Float>, $TensorHandle<Int32>
// STRICTDA: graph_op "Add,i,i"(
// STRICTDA: graph_op "Add,i,i"(
// The operands can appear in arbitrary order here.
// STRICTDA:  [[RESULT:%.*]] = tuple ({{.*}} : $TensorHandle<{{.*}}>, {{.*}} : $TensorHandle<{{.*}}>)
// STRICTDA-NEXT:  return [[RESULT]] : $(TensorHandle<{{.*}}>, TensorHandle<{{.*}}>)
