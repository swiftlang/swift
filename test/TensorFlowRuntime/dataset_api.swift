// RUN: %target-run-simple-swift
// REQUIRES: executable_test
// REQUIRES: swift_test_mode_optimize
//
// Dataset API tests.

import TensorFlow

#if TPU
import TensorFlowUnittestTPU
#else
import TensorFlowUnittest
#endif
import StdlibUnittest

var DatasetAPITests = TestSuite("DatasetAPI")

DatasetAPITests.testAllBackends("SingleValueManualIterator") {
  // [[1], [2], [3], [4], [5]]
  let scalars = Tensor<Float>(rangeFrom: 0, to: 5, stride: 1).reshaped(to: [5, 1])
  let dataset = SingleValueDataset(elements: scalars, elementShape: [1])
  var iterator = dataset.makeIterator()
  var i: Int32 = 0
  while let item = iterator.next() {
    expectEqual(scalars[i].array, item.array)
    i += 1
  }
}

DatasetAPITests.testAllBackends("SingleValueDatasetIteration") {
  // [[1], [2], [3], [4], [5]]
  let scalars = Tensor<Float>(rangeFrom: 0, to: 5, stride: 1).reshaped(to: [5, 1])
  let dataset = SingleValueDataset(elements: scalars, elementShape: [1])
  var i: Int32 = 0
  for item in dataset {
    expectEqual(scalars[i].array, item.array)
    i += 1
  }
}

// FIXME: Only public @TensorFlowGraph functions are being partitioned.
@TensorFlowGraph
public func addOne(_ x: Tensor<Float>) -> Tensor<Float> {
  return x + 1
}

DatasetAPITests.testAllBackends("SingleValueMap") {
  let scalars = Tensor<Float>(rangeFrom: 0, to: 5, stride: 1)
  let dataset = SingleValueDataset(elements: scalars, elementShape: [])
  let result: SingleValueDataset = dataset.map(addOne)
  expectEqual(result.flatMap { $0.scalars }, [1, 2, 3, 4, 5])
}

runAllTests()
