// RUN: %target-run-simple-swift
// REQUIRES: executable_test
// REQUIRES: swift_test_mode_optimize
//
// Tensor AD runtime tests.

import TensorFlow
import TensorFlowUnittest
import TensorFlowRuntime

var TensorADTests = TestSuite("TensorAD")

TensorADTests.testAllBackends("SimpleAdjointCall") {
  let adjPlus = #adjoint(Tensor<Float>.+)
  let x = Tensor<Float>(1)
  expectNearlyEqual(1, adjPlus(x, x, originalValue: x + x, seed: x))
}

runAllTests()
