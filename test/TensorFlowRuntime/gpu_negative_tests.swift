// RUN: %target-run-simple-swift
// RUN: %target-run-dynamic-compilation-swift
// REQUIRES: executable_test
// REQUIRES: swift_test_mode_optimize
// XFAIL: *

// This test suite contains expected failure tests for GPU execution.

import TensorFlow
import TensorFlowUnittest
import StdlibUnittest

var GPUNegativeTests = TestSuite("GPUNegative")

// Make sure this test suite would fail when running on any device.
GPUNegativeTests.testAllBackends("dummy_failed_test") {
  expectEqual(0, 1)
}

// This test would fail when running under GPU, because Assert does not have a
// GPU kernel.
#if CUDA
public func noAssertOnGPU() {
  let a = Tensor<Float>(1.0)
  _hostOp(a)
  #tfop("Assert", Tensor<Bool>(true), [a]) as Void
  expectEqualWithScalarTensor(1, a)
}
GPUNegativeTests.testAllBackends("noAssertOnGPU", noAssertOnGPU)
#endif // CUDA

runAllTests()

