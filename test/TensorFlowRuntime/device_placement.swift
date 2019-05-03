// RUN: %target-run-eager-swift %swift-tensorflow-test-run-extra-options
// RUN: %target-run-gpe-swift %swift-tensorflow-test-run-extra-options
// REQUIRES: executable_test
// REQUIRES: swift_test_mode_optimize

// Test device placement API.

import TensorFlow
#if TPU
import TensorFlowUnittestTPU
#else
import TensorFlowUnittest
#endif
import StdlibUnittest

var DevicePlacementTests = TestSuite("DevicePlacement")

DevicePlacementTests.testAllBackends("GroupWithSize2") {
  // Need 2 or more CPU devices for this test.
  let x = Tensor<Float>(1.0)

  withDefaultDevice() {
    let t = Tensor<Float>(1.0) + Tensor<Float>(2.0)
    expectEqualWithScalarTensor(3, t)
  }
}

runAllTests()
