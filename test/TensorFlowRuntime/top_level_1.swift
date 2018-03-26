// RUN: %target-run-simple-swift
// REQUIRES: executable_test

// This tests various issues with top level code, ensuring deabstraction and
// other things work here.  This is not intended to be a place to test device
// specifics, so we can keep it simple and just test CPU.

import TensorFlow
import TestUtils
import StdlibUnittest

var TopLevelTests = TestSuite("TopLevel")

TopLevelTests.testCPU("TopLevel") {
  var x = Tensor<Int8>([1,2,3])*2
  x = x + x
  expectEqual(x.array, ShapedArray(shape: [3], scalars: [4, 8, 12]))
}

runAllTests()
