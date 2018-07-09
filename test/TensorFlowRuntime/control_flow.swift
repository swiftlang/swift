// RUN: %target-run-simple-swift
// REQUIRES: executable_test
// REQUIRES: swift_test_mode_optimize

// Control flow related tests.

import TensorFlow
import TensorFlowUnittest
import StdlibUnittest

var ControlFlowTests = TestSuite("ControlFlow")

@inline(never)
public  func testSwitchEnum(_ a: Tensor<Float>?,
                            _ expectedVal: Float) {
  var b = Tensor<Float>(2.0)
  if let a = a {
    b += a
  }
  b -= 1.0
  expectNearlyEqualWithScalarTensor(expectedVal, b)
}
ControlFlowTests.testAllBackends("testSwitchEnum") {
  testSwitchEnum(Tensor<Float>(1.0), 2.0)
  testSwitchEnum(nil, 1.0)
}

runAllTests()
