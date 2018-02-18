// RUN: %target-run-simple-swift
// REQUIRES: executable_test
//
// Tensor API tests.

import TensorFlow
import TestUtils
import StdlibUnittest

var TensorTests = TestSuite("TensorDebug")

// Exercise debug logging, to make sure the extra code only executed under debug
// logging will not crash the binary or change its output.
TensorTests.testCPUAndGPU("XWPlusB") {
  _RuntimeConfig.printsDebugLog = true
  // Also exercise XLA here, before we turn it on for more tests.
  _RuntimeConfig.usesXLA = true

  // Shape: 1 x 4
  let x = Tensor([[1.0, 2.0, 2.0, 1.0]])
  // Shape: 4 x 2
  let w = Tensor([[1.0, 0.0], [3.0, 0.0], [2.0, 3.0], [1.0, 0.0]])
  // Shape: 2
  let b = Tensor([0.5, 0.5])
  // Do xW+b!
  let result = x ⊗ w + b
  expectEqual([1, 2], result.shape)
  expectEqual([12.5, 6.5], result.scalars)
}

runAllTests()
