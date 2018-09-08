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
  let adjMatmul: (
    Tensor<Float>,
    Tensor<Float>,
    Tensor<Float>,
    Tensor<Float>
  ) -> (Tensor<Float>, Tensor<Float>) = #adjoint(matmul)
  let x = Tensor<Float>([[1]])
  let (d0, d1) = adjMatmul(x, x, x, x)
  expectNearlyEqual(1, d0.scalarized())
  expectNearlyEqual(1, d1.scalarized())
}

runAllTests()
