// RUN: %target-swift-frontend -Xllvm -tf-dump-intermediates -Xllvm -tf-dump-graph -O -emit-sil -verify %s | %FileCheck %s
import TensorFlow

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
                                      Toutput_types$dtype: [Float.tensorFlowDataType],
                                      output_shapes: [TensorShape()])
  return dataset
}

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
    output_types$dtype: [Float.tensorFlowDataType],
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
                                       output_types$dtype: [Float.tensorFlowDataType],
                                       output_shapes: [TensorShape()])
  _hostOp(one)
}

// CHECK-LABEL: --- TFPartition GraphDef Proto

// The lowered graph should only contain two graph functions, plus the
// top-level nodes for calling the grah function lowered from model().

// CHECK:      node {
// CHECK-NEXT:   name: "{{.*}}model{{.*}}"
// CHECK-NEXT:   op: "{{.*}}model{{.*}}.tf_CPU.device_partition"
// CHECK:      node {
// CHECK-NEXT:  name: "tfc_output_0_{{.*}}model{{.*}}"

// CHECK:       function {
// CHECK:       function {
// CHECK-NOT:   function {

// Ideally we want to check the following as well, but the two graph functions
// above do not have a deterministic ordering in the GraphDef proto, which will
// cause the test to be flakey.

// HECK-NEXT:    signature {
// HECK-NEXT:      name: "{{.*}}createMockDataSet{{.*}}.tf_only"
// HECK:           output_arg {
// HECK-NEXT:        name: "op_createmockdataset{{.*}}"
// HECK-NEXT:        type: DT_VARIANT
// HECK-NEXT:      }
