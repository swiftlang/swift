// RUN: %target-run-simple-swift
// REQUIRES: executable_test
// REQUIRES: swift_test_mode_optimize
//
// Tensor AD runtime tests.

import TensorFlow
import StdlibUnittest
import TensorFlowUnittest

var TensorADTests = TestSuite("TensorAD")

TensorADTests.testAllBackends("SimpleAdjointCall") {
  let adjPlus = #adjoint(Tensor<Float>.+)
  let x = Tensor<Float>(1)
  expectNearlyEqual(1, adjPlus(x, x, x + x, x).0.scalarized())
}

runAllTests()
