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
  let scalars = Tensor<Float>(rangeFrom: 0, to: 5, stride: 1)
    .reshaped(to: [5, 1])
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
  let scalars = Tensor<Float>(rangeFrom: 0, to: 5, stride: 1)
    .reshaped(to: [5, 1])
  let dataset = SingleValueDataset(elements: scalars, elementShape: [1])
  var i: Int32 = 0
  for item in dataset {
    expectEqual(scalars[i].array, item.array)
    i += 1
  }
}

DatasetAPITests.testAllBackends("SingleValueTransformations") {
  let scalars = Tensor<Float>(rangeFrom: 0, to: 5, stride: 1)
  let dataset = SingleValueDataset(elements: scalars, elementShape: [])
  let shuffled = dataset.shuffled(sampleCount: 5, randomSeed: 42)
  expectEqual([0, 4, 1, 3, 2], shuffled.map { $0.scalar! })
}

// FIXME: Only public @TensorFlowGraph functions are being partitioned.
@TensorFlowGraph
public func addOne(_ x: Tensor<Float>) -> Tensor<Float> {
  return x + 1
}

DatasetAPITests.testAllBackends("SingleValueHOFs") {
  let scalars = Tensor<Float>(rangeFrom: 0, to: 5, stride: 1)
  let dataset = SingleValueDataset(elements: scalars, elementShape: [])
  let result: SingleValueDataset = dataset.map(addOne)
  expectEqual(result.flatMap { $0.scalars }, [1, 2, 3, 4, 5])
}

DatasetAPITests.testAllBackends("DoubleValueDatasetIteration") {
  let scalars1 = Tensor<Float>(rangeFrom: 0, to: 5, stride: 1)
  let scalars2 = Tensor<Float>(rangeFrom: 5, to: 10, stride: 1)
  let dataset = DoubleValueDataset(elements: (scalars1, scalars2),
                                   elementShapes: ([], []))
  var i: Int32 = 0
  for (item1, item2) in dataset {
    expectEqual(scalars1[i].array, item1.array)
    expectEqual(scalars2[i].array, item2.array)
    i += 1
  }
}

runAllTests()
