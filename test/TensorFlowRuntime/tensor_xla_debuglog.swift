// RUN: %target-run-simple-swift %swift-tensorflow-test-run-extra-options
// RUN: %target-run-dynamic-compilation-swift %swift-tensorflow-test-run-extra-options
// REQUIRES: executable_test
// REQUIRES: swift_test_mode_optimize
//
// XLA tests, with debug logging enabled.

import TensorFlow
#if TPU
import TensorFlowUnittestTPU
#else
import TensorFlowUnittest
#endif
import StdlibUnittest

var XLATests = TestSuite("XLADebug")

// Exercise XLA with debug logging.
XLATests.test("XWPlusB_XLA") {
#if !TPU
  _RuntimeConfig.printsDebugLog = true
  _RuntimeConfig.executionMode = .xla

  // Shape: 1 x 4
  let x = Tensor<Float>([[1.0, 2.0, 2.0, 1.0]])
  // Shape: 4 x 2
  let w = Tensor<Float>([[1.0, 0.0], [3.0, 0.0], [2.0, 3.0], [1.0, 0.0]])
  // Shape: 2
  let b = Tensor<Float>([0.5, 0.5])
  // Do xW+b!
  let result = x • w + b
  expectEqual([1, 2], result.shape)
  expectEqual([12.5, 6.5], result.scalars)
#endif
}

runAllTests()
