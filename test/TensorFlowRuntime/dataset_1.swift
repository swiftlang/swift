// RUN: %target-run-eager-swift %swift-tensorflow-test-run-extra-options
// REQUIRES: executable_test
// REQUIRES: swift_test_mode_optimize
//
// Compiler-only testing for TPU graph lowering (e.g. shape requirements by XLA).

// TODO: re-enable TPU compilation, when we are able to handle returning tensor
// `three` below produced on TF CPU to the TPU graph function, without running
// into the "missing tensor shape" issue. One option is to convert return
// tensors to TF->host sends, so that in this case we can send directly from TF
// CPU to host, even if the primary device is TPU.

// Dataset tests.

import TensorFlow

#if TPU
import TensorFlowUnittestTPU
#else
import TensorFlowUnittest
#endif
import StdlibUnittest

var DatasetTests = TestSuite("Dataset")

#if !CUDA
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
