// RUN: %target-run-use-device-stack-swift %swift-tensorflow-test-run-extra-options
// RUN: %target-run-gpe-swift %swift-tensorflow-test-run-extra-options
// REQUIRES: executable_test
// REQUIRES: swift_test_mode_optimize

// Tests on collective ops, as a building block for data/model parallel programs.

import TensorFlow
#if TPU
import TensorFlowUnittestTPU
#else
import TensorFlowUnittest
#endif
import StdlibUnittest

var WithDeviceTests = TestSuite("WithDevice")

WithDeviceTests.testAllBackends("Basic") {
  func foo() {
    let x = Tensor<Float>(1.0)
    let y = x + x
    expectEqualWithScalarTensor(2, y)
    _hostOp(y)
  }
  withDevice(.cpu, 0) {
    foo()
  }
  withDevice(.cpu, 1) {
    foo()
  }
  #if CUDA
  withDevice(.gpu, 0) {
    foo()
  }
  #endif
}

_RuntimeConfig.cpuDeviceCount = 3
runAllTests()
