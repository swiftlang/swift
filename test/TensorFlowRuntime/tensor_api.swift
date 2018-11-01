// RUN: %target-run-simple-swift
// RUN: %target-run-disable-deabstraction-swift
// REQUIRES: executable_test
// REQUIRES: swift_test_mode_optimize
//
// This test suite is for tensor API and has been created because tensor.swift
// has static shape restrictions for TPU send/receive. Until the restriction is
// resolved, API tests that incur send/receive should reside here.“

import TensorFlow
import TensorFlowUnittest
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
  t4[0] = Tensor(shape: [3], repeating: false)
  expectEqual(ShapedArray(shape:[2, 3], repeating: false), t4.array)
}

runAllTests()
