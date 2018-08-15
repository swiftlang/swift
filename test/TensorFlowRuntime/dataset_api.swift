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

DatasetAPITests.testAllBackends("SingleValueMapAndFilter") {
  let scalars = Tensor<Float>(rangeFrom: 0, to: 5, stride: 1)
  let dataset = SingleValueDataset(elements: scalars, elementShape: [1])
  @TensorFlowGraph
  func isEven(_ x: Tensor<Float>) -> Tensor<Bool> {
    return x % 2 == Tensor<Float>(0) ? Tensor(true) : Tensor(false)
  }
  let booleanFlags: SingleValueDataset = dataset.map(isEven)
  expectEqual(booleanFlags.flatMap { $0.scalars }, [true, false, true, false, true])
  let evenDataset: SingleValueDataset = dataset.filter(isEven)
  expectEqual(evenDataset.flatMap { $0.scalars }, [0, 2, 4])
}

runAllTests()
