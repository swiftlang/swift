// RUN: %target-run-eager-swift %swift-tensorflow-test-run-extra-options

// SR-9737: hanging tests in GPE GPU mode
// UN: %target-run-gpe-swift %swift-tensorflow-test-run-extra-options

// REQUIRES: executable_test
// REQUIRES: swift_test_mode_optimize
//
// This test suite is for tensor API and has been created because tensor.swift
// has static shape restrictions for TPU send/receive. Until the restriction is
// resolved, API tests that incur send/receive should reside here.“

import TensorFlow
#if TPU
import TensorFlowUnittestTPU
#else
import TensorFlowUnittest
#endif
import StdlibUnittest

var TensorNonTPUTests = TestSuite("TensorNonTPU")

TensorNonTPUTests.testAllBackends("SliceUpdate") {
  var t1 = Tensor<Float>([[1, 2, 3], [4, 5, 6]])
  t1[0] = Tensor(zeros: [3])
  expectEqual(ShapedArray(shape:[2, 3], scalars: [0, 0, 0, 4, 5, 6]), t1.array)
  var t2 = t1
  t2[0][2] = Tensor(3)
  expectEqual(ShapedArray(shape:[2, 3], scalars: [0, 0, 3, 4, 5, 6]), t2.array)
  var t3 = Tensor<Bool>([[true, true, true], [false, false, false]])
  t3[0][1] = Tensor(false)
  expectEqual(ShapedArray(shape:[2, 3],
                          scalars: [true, false, true, false, false, false]),
              t3.array)
  var t4 = Tensor<Bool>([[true, true, true], [false, false, false]])
  t4[0] = Tensor(repeating: false, shape: [3])
  expectEqual(ShapedArray(repeating: false, shape: [2, 3]), t4.array)
}

TensorNonTPUTests.testAllBackends("BroadcastTensor") {
  // 1 -> 2 x 3 x 4
  let one = Tensor<Float>(1)
  var target = Tensor<Float>(repeating: 0.0, shape: [2, 3, 4])
  let broadcasted = one.broadcast(like: target)
  expectEqual(Tensor(repeating: 1, shape: [2, 3, 4]), broadcasted)
  target .= Tensor(repeating: 1, shape: [1, 3, 1])
  expectEqual(Tensor(repeating: 1, shape: [2, 3, 4]), target)
}

runAllTests()
