// RUN: %target-run-simple-swift
// REQUIRES: executable_test
// REQUIRES: swift_test_mode_optimize
//
// Retain/release tests.

import TensorFlow

#if TPU
import TensorFlowUnittestTPU
#else
import TensorFlowUnittest
#endif
import StdlibUnittest

var DatasetTests = TestSuite("Dataset")

// Creates a dataset, which produces one float scalar value in each get next
// call.
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

func getNextScalarFloatTensor(_ iterator: ResourceHandle) -> Tensor<Float> {
  // REGISTER_OP("IteratorGetNext")
  //   .Input("iterator: resource")
  //   .Output("components: output_types")
  //   .Attr("output_types: list(type) >= 1")
  //   .Attr("output_shapes: list(shape) >= 1")
  let ret: TensorHandle<Float> = #tfop("IteratorGetNext",
                                       iterator,
                                       output_types: [Float.self],
                                       output_shapes: [TensorShape()])
  return Tensor(handle: ret)
}

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
    dataset_factory : createMockDataSet,
    output_types: [Float.self],
    output_shapes: [TensorShape()]
  )

  let one = getNextScalarFloatTensor(iterator)
  _hostOp(one)
  expectNearlyEqualWithScalarTensor(1.0, one)

  let two = getNextScalarFloatTensor(iterator)
  _hostOp(two)
  expectNearlyEqualWithScalarTensor(2.0, two)

  let three = getNextScalarFloatTensor(iterator)
  _hostOp(three)
  expectNearlyEqualWithScalarTensor(3.0, three)

  // TODO: do not crash when TF emits "Fatal error: End of sequence"
  // let error: TensorHandle<Float> = #tfop("IteratorGetNext",
  //                                      iterator,
  //                                      output_types: [Float.self],
  //                                      output_shapes: [TensorShape()])
}

DatasetTests.testAllBackends("Basic") {
  model()
}

runAllTests()
