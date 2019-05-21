// RUN: %target-run-simple-swift %swift-tensorflow-test-run-extra-options
// REQUIRES: executable_test

// Dataset tests.

import TensorFlow
import TensorFlowUnittest
import StdlibUnittest

var DatasetTests = TestSuite("Dataset")

#if !CUDA
struct SimpleOutput : TensorGroup {
  let a: TensorHandle<Int32>
  let b: TensorHandle<Int32>
}
DatasetTests.testAllBackends("MultiValue") {
  let elements1: Tensor<Int32> = [0, 1, 2]
  let elements2: Tensor<Int32> = [10, 11, 12]
  let outputTypes = [Int32.tensorFlowDataType, Int32.tensorFlowDataType]
  let outputShapes: [TensorShape?] = [nil, nil]
  let dataset: VariantHandle = Raw.tensorSliceDataset(
    components: [elements1, elements2],
    outputShapes: outputShapes
  )
  let iterator: ResourceHandle = Raw.iteratorV2(sharedName: "blah",
    container: "earth", outputTypes: outputTypes, outputShapes: outputShapes
  )
  Raw.makeIterator(dataset: dataset, iterator: iterator)
  var next: SimpleOutput = Raw.iteratorGetNext(
    iterator: iterator, outputShapes: outputShapes
  )
  expectEqual(0, Tensor(handle: next.a).scalarized())
  expectEqual(10, Tensor(handle: next.b).scalarized())
  next = Raw.iteratorGetNext(
    iterator: iterator, outputShapes: outputShapes
  )
  expectEqual(1, Tensor(handle: next.a).scalarized())
  expectEqual(11, Tensor(handle: next.b).scalarized())
  next = Raw.iteratorGetNext(
    iterator: iterator, outputShapes: outputShapes
  )
  expectEqual(2, Tensor(handle: next.a).scalarized())
  expectEqual(12, Tensor(handle: next.b).scalarized())
}
#endif

runAllTests()
