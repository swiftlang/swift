// RUN: %target-run-simple-swift
// RUN: %target-run-dynamic-compilation-swift
// REQUIRES: executable_test
// REQUIRES: swift_test_mode_optimize
//
// Retain/release tests.

import TensorFlow

#if TPU
import TensorFlowUnittestTPU
#else
import TensorFlowUnittest
#endif
import StdlibUnittest

var RetainReleaseTests = TestSuite("RetainRelease")

RetainReleaseTests.testAllBackends("Basic") {
  let t1 = Tensor<Float>(1.2)
  let _ = t1 + t1
  let _ = t1.array
}

runAllTests()
