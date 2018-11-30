// RUN: %target-run-simple-swift %swift-tensorflow-test-run-extra-options
// RUN: %target-run-dynamic-compilation-swift %swift-tensorflow-test-run-extra-options
// REQUIRES: executable_test
// REQUIRES: swift_test_mode_optimize
//
// Compiler-only testing for TPU graph lowering (e.g. shape requirements by XLA).

// TODO: re-enable TPU compilation, when we are able to handle returning tensor
// `three` below produced on TF CPU to the TPU graph function, without running
// into the "missing tensor shape" issue. One option is to convert return
// tensors to TF->host sends, so that in this case we can send directly from TF
// CPU to host, even if the primary device is TPU.

// UN: %target-swift-frontend -Xllvm -tf-dump-intermediates -Xllvm -tf-dump-graph -Xllvm -tf-target-tpu -O -emit-sil %s >/dev/null
//
// Dataset tests.

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
  // A dataset graph function must run on TF CPU.
  TensorFlow.enableCPU()
  let values = Tensor<Float>([1.0, 2.0, 3.0])
  // REGISTER_OP("TensorSliceDataset")
  //   .Input("components: Toutput_types")
  //   .Output("handle: variant")
  //   .Attr("Toutput_types: list(type) >= 1")
  //   .Attr("output_shapes: list(shape) >= 1")
  let dataset : VariantHandle = #tfop("TensorSliceDataset",
                                      [values],
                                      Toutput_types$dtype: [Float.tensorFlowDataType],
                                      output_shapes: [nil as TensorShape?])
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
                                       output_types$dtype: [Float.tensorFlowDataType],
                                       output_shapes: [nil as TensorShape?])
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
    output_types$dtype: [Float.tensorFlowDataType],
    output_shapes: [nil as TensorShape?]
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

  // Running the commented-out code below will cause the process to exit, with
  // TF error message "End of sequence" printed on STDERR. The code is commented
  // out because running it will unfortunately cause the test to fail.

  // let _: TensorHandle<Float> = #tfop("IteratorGetNext",
  //                                    iterator,
  //                                    output_types$dtype: [Float.tensorFlowDataType],
  //                                    output_shapes: [nil as TensorShape?])
}

#if !CUDA
DatasetTests.testAllBackends("Basic") {
  // OneShotIterator is not supported on GPU.
  model()
}

DatasetTests.testAllBackends("MultiValue") {
  enableCPU()
  let elements1: Tensor<Int32> = [0, 1, 2]
  let elements2: Tensor<Int32> = [10, 11, 12]
  let outputTypes = [Int32.tensorFlowDataType, Int32.tensorFlowDataType]
  let outputShapes: [TensorShape?] = [nil, nil]
  let dataset: VariantHandle = #tfop(
    "TensorSliceDataset", [elements1, elements2],
    Toutput_types$dtype: outputTypes,
    output_shapes: outputShapes
  )
  let iterator: ResourceHandle = #tfop(
    "IteratorV2", shared_name: "blah", container: "earth",
    output_types$dtype: outputTypes, output_shapes: outputShapes
  )
  #tfop("MakeIterator", dataset, iterator) as Void
  var next: (TensorHandle<Int32>, TensorHandle<Int32>) = #tfop(
    "IteratorGetNext", iterator,
    output_types$dtype: outputTypes, output_shapes: outputShapes
  )
  expectEqual(0, Tensor(handle: next.0).scalarized())
  expectEqual(10, Tensor(handle: next.1).scalarized())
  next = #tfop(
    "IteratorGetNext", iterator,
    output_types$dtype: outputTypes, output_shapes: outputShapes
  )
  expectEqual(1, Tensor(handle: next.0).scalarized())
  expectEqual(11, Tensor(handle: next.1).scalarized())
  next = #tfop(
    "IteratorGetNext", iterator,
    output_types$dtype: outputTypes, output_shapes: outputShapes
  )
  expectEqual(2, Tensor(handle: next.0).scalarized())
  expectEqual(12, Tensor(handle: next.1).scalarized())
}
#endif

runAllTests()
