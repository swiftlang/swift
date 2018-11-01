// RUN: %target-run-simple-swift
// RUN: %target-run-disable-deabstraction-swift
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

// TODO: fix the test suite for GPU.
// We have errors like:
// Not found: No registered 'ZipDataset' OpKernel for GPU devices compatible with node {{node op/_S4mainyycfU4_.tf_GPU.70.64_/device_GPU_0_7}} = ZipDataset[N=2, output_shapes=[<unknown>, <unknown>], output_types=[DT_FLOAT, DT_FLOAT], _device="/device:GPU:0"](tf_recv_55, tf_recv_56)
#if !CUDA
DatasetAPITests.testAllBackends("SingleValueManualIterator") {
  // [[1], [2], [3], [4], [5]]
  let scalars = Tensor<Float>(rangeFrom: 0, to: 5, stride: 1)
    .reshaped(to: [5, 1])
  let dataset = Dataset(elements: scalars)
  var iterator = dataset.makeIterator()
  var i: Int32 = 0
  while let item = iterator.next() {
    expectEqual(scalars[i].array, item.array)
    i += 1
  }
}

DatasetAPITests.testAllBackends("DatasetIteration") {
  // [[1], [2], [3], [4], [5]]
  let scalars = Tensor<Float>(rangeFrom: 0, to: 5, stride: 1)
    .reshaped(to: [5, 1])
  let dataset = Dataset(elements: scalars)
  var i: Int32 = 0
  for item in dataset {
    expectEqual(scalars[i].array, item.array)
    i += 1
  }
}

DatasetAPITests.testAllBackends("SingleValueTransformations") {
  let scalars = Tensor<Float>(rangeFrom: 0, to: 5, stride: 1)
  let dataset = Dataset(elements: scalars)
  let shuffled = dataset.shuffled(sampleCount: 5, randomSeed: 42)
  expectEqual([0, 4, 1, 3, 2], shuffled.map { $0.scalar! })
}

// TODO: This test uses function attributes, which dynamic compilation does not
// support yet.
#if !TF_DYNAMIC_COMPILATION
DatasetAPITests.testAllBackends("SingleValueHOFs") {
  let scalars = Tensor<Float>(rangeFrom: 0, to: 5, stride: 1)
  let dataset = Dataset(elements: scalars)
  let addedOne: Dataset = dataset.map { $0 + 1 }
  expectEqual([1, 2, 3, 4, 5], addedOne.flatMap { $0.scalars })
  let evens: Dataset = dataset.filter { Tensor($0 % 2 == Tensor(0)) }
  expectEqual([0, 2, 4], evens.flatMap { $0.scalars })
}
#endif // !TF_DYNAMIC_COMPILATION

DatasetAPITests.testAllBackends("SingleValueBatched") {
  let scalars = Tensor<Float>(rangeFrom: 0, to: 5, stride: 1)
  let dataset = Dataset(elements: scalars)
  let batched = dataset.batched(2)

  var iterator = batched.makeIterator()
  expectEqual([0, 1], iterator.next()!.scalars)
  expectEqual([2, 3], iterator.next()!.scalars)
  expectEqual([4], iterator.next()!.scalars)
}

// TODO(SR-9156): Make this test work in graph mode.
#if TF_DYNAMIC_COMPILATION
DatasetAPITests.testAllBackends("DoubleValueDatasetIteration") {
  let scalars1 = Tensor<Float>(rangeFrom: 0, to: 5, stride: 1)
  let scalars2 = Tensor<Int32>(rangeFrom: 5, to: 10, stride: 1)
  let datasetLeft = Dataset(elements: scalars1)
  let datasetRight = Dataset(elements: scalars2)
  var i: Int32 = 0
  for pair in zip(datasetLeft, datasetRight) {
    expectEqual(scalars1[i].array, pair.a.array)
    expectEqual(scalars2[i].array, pair.b.array)
    i += 1
  }
}
#endif // TF_DYNAMIC_COMPILATION

#endif //!CUDA

runAllTests()
