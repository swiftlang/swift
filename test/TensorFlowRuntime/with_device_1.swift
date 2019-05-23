// RUN: %target-run-simple-swift %swift-tensorflow-test-run-extra-options
// REQUIRES: executable_test

// Tests on collective ops, as a building block for data/model parallel programs.

import TensorFlow
import TensorFlowUnittest
import StdlibUnittest

var WithDeviceTests = TestSuite("WithDevice")

WithDeviceTests.testAllBackends("ConfigTest") {
  // Run some tensor code to trigger runtime configuration.
  print(Tensor<Float>(0.0) + Tensor<Float>(1.0))
  expectEqual(3, _RuntimeConfig.cpuDeviceCount)
}

WithDeviceTests.testAllBackends("Basic") {
  func foo() {
    let x = Tensor<Float>(1.0)
    let y = x + x
    expectEqualWithScalarTensor(2, y)
    print(y)
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

runAllTests()
