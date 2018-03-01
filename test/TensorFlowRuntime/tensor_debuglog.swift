// RUN: %target-run-simple-swift
// REQUIRES: executable_test
//
// Tensor API tests, with debug logging enabled.

import TensorFlow
#if TPU
import TestUtilsTPU
#else
import TestUtils
#endif
import StdlibUnittest

var TensorTests = TestSuite("TensorDebug")

// Exercise debug logging, to make sure the extra code only executed under debug
// logging will not crash the binary or change its output.
TensorTests.testAllBackends("XWPlusB") {
  _RuntimeConfig.printsDebugLog = true

  // Shape: 1 x 4
  let x = Tensor<Float>([[1.0, 2.0, 2.0, 1.0]])
  // Shape: 4 x 2
  let w = Tensor<Float>([[1.0, 0.0], [3.0, 0.0], [2.0, 3.0], [1.0, 0.0]])
  // Shape: 2
  let b = Tensor<Float>([0.5, 0.5])
  // Do xW+b!
  let result = x ⊗ w + b
  expectEqual([1, 2], result.shape)
  expectEqual([12.5, 6.5], result.scalars)
}

TensorTests.test("XWPlusB_XLA") {
#if !TPU
  _RuntimeConfig.printsDebugLog = true
  // Also exercise XLA here, before we turn it on for more tests.
  _RuntimeConfig.executionMode = .xla

  // Shape: 1 x 4
  let x = Tensor<Float>([[1.0, 2.0, 2.0, 1.0]])
  // Shape: 4 x 2
  let w = Tensor<Float>([[1.0, 0.0], [3.0, 0.0], [2.0, 3.0], [1.0, 0.0]])
  // Shape: 2
  let b = Tensor<Float>([0.5, 0.5])
  // Do xW+b!
  let result = x ⊗ w + b
  expectEqual([1, 2], result.shape)
  expectEqual([12.5, 6.5], result.scalars)
#endif
}

runAllTests()
