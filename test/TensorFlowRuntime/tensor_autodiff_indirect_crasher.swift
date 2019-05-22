// RUN: %target-run-simple-swift
// REQUIRES: executable_test
//
// FIXME(TF-326): Re-enable `-O` after deserialization failure fix.
// UNSUPPORTED: swift_test_mode_optimize
//
// Tensor indirect passing AD runtime tests.

import TensorFlow
import StdlibUnittest
import TensorFlowUnittest

var TensorADTests = TestSuite("TensorIndirectAD")

TensorADTests.testAllBackends("TF-324") {
  @differentiable(where T : TensorFlowFloatingPoint)
  func TF_324<T>(_ lhs: T, _ rhs: Tensor<T>) -> Tensor<T> where T : FloatingPoint {
    return pow(Tensor(lhs), rhs)
  }
}

runAllTests()
