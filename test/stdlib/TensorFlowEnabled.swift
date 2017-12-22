// RUN: %target-run-simple-swift
// REQUIRES: tensorflow

import StdlibUnittest

let TensorFlowEnabled = TestSuite("TensorFlowEnabled")
TensorFlowEnabled.test("TensorFlowEnabled") {
  expectPrinted("1", 1)
}

runAllTests()
