// RUN: %target-run-simple-swift
// RUN: %target-run-disable-deabstraction-swift
// REQUIRES: executable_test
// REQUIRES: swift_test_mode_optimize
//
// Compiler-only testing for TPU graph lowering (e.g. shape requirements by XLA).
// RUN: %target-swift-frontend -Xllvm -tf-dump-intermediates -Xllvm -tf-dump-graph -Xllvm -tf-target-tpu -O -emit-sil %s >/dev/null
//
// Tensor API tests, with debug logging enabled.

import TensorFlow
#if TPU
import TensorFlowUnittestTPU
#else
import TensorFlowUnittest
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
  let result = x • w + b
  expectEqual([1, 2], result.shape)
  expectEqual([12.5, 6.5], result.scalars)
}

runAllTests()
