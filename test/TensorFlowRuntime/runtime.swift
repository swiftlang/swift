// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest
import TensorFlow

var RuntimeTests = TestSuite("Runtime")

RuntimeTests.test("Runtime/BasicTest") {
  let tensorHandles: [AnyTensorHandle] = []
  // The first two params are currently no-ops.
  let program = _TFCStartTensorProgram("some_bytes", 0, tensorHandles, 0)
  let outputValues = _TFCFinishTensorProgram(program)
  expectEqual(outputValues.count, 0)
}

runAllTests()
