// RUN: %target-swift-frontend -Xllvm -tf-dump-intermediates -Xllvm -tf-dump-graph -O -emit-sil -verify %s | %FileCheck %s
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


// Creates a dataset, which produces one float scalar value in each get next
// call.
// TODO: declare with @convention tensorflow
// Enforce no sends/recvs, and all logic is lowered to TF graph.
@TensorFlowGraph
public func createMockDataSet() -> VariantHandle {
  let values = Tensor<Float>([1.0, 2.0, 3.0])
  // REGISTER_OP("TensorSliceDataset")
  //   .Input("components: Toutput_types")
  //   .Output("handle: variant")
  //   .Attr("Toutput_types: list(type) >= 1")
  //   .Attr("output_shapes: list(shape) >= 1")
  let dataset : VariantHandle = #tfop("TensorSliceDataset",
                                      [values],
                                      Toutput_types: [Float.self],
                                      output_shapes: [TensorShape()])
  return dataset
}

// The lowered graph should only contain the graph function, and not any
// top-level nodes.
//
// CHECK-LABEL: --- TFPartition Accelerator Result: {{.*}}createMockDataSet{{.*}}
// CHECK-NOT:   node {
// CHECK:       function {
// CHECK-NEXT:    signature {
// CHECK-NEXT:      name: "{{.*}}createMockDataSet{{.*}}.tf_only"
// CHECK:           output_arg {
// CHECK-NEXT:        name: "op_createmockdataset{{.*}}"
// CHECK-NEXT:        type: DT_VARIANT
// CHECK-NEXT:      }

// TODO: support taking the following function typed parameter.
// _ datasetCreator : @convention(tensorflow) () -> VariantHandle
// TODO(SR-8117): Support "" for container and shared_name.
public func model() {
  // REGISTER_OP("OneShotIterator")
  //   .Output("handle: resource")
  //   .Attr("dataset_factory: func")
  //   .Attr("output_types: list(type) >= 1")
  //   .Attr("output_shapes: list(shape) >= 1")
  //   .Attr("container: string = ''")
  //   .Attr("shared_name: string = ''")
  let iterator: ResourceHandle = #tfop(
    "OneShotIterator",
    container: "some_container",
    dataset_factory : /*datasetCreator*/createMockDataSet,
    output_types: [Float.self],
    output_shapes: [TensorShape()],
    shared_name: "some_name"
  )

  // REGISTER_OP("IteratorGetNext")
  //   .Input("iterator: resource")
  //   .Output("components: output_types")
  //   .Attr("output_types: list(type) >= 1")
  //   .Attr("output_shapes: list(shape) >= 1")
  let one: TensorHandle<Float> = #tfop("IteratorGetNext",
                                       iterator,
                                       output_types: [Float.self],
                                       output_shapes: [TensorShape()])
  _hostOp(one)
}

// The lowered graph should only contain two graph functions, plus the
// top-level nodes for calling the grah function lowered from model().

// CHECK-LABEL: --- TFPartition Accelerator Result: {{.*}}model{{.*}}
// CHECK:      node {
// CHECK-NEXT:   name: "{{.*}}model{{.*}}"
// CHECK-NEXT:   op: "{{.*}}model{{.*}}.tf_CPU.device_partition"
// CHECK:      node {
// CHECK-NEXT:  name: "tfc_output_0_{{.*}}model{{.*}}"

// CHECK:       function {
// CHECK:       function {
// CHECK-NOT:   function {
