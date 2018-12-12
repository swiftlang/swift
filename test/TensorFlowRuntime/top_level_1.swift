// RUN: %target-run-simple-swift %swift-tensorflow-test-run-extra-options
// RUN: %target-run-dynamic-compilation-swift %swift-tensorflow-test-run-extra-options
// REQUIRES: executable_test
// REQUIRES: swift_test_mode_optimize

// This tests various issues with top level code, ensuring deabstraction and
// other things work here.  This is not intended to be a place to test device
// specifics, so we can keep it simple and just test CPU.

import TensorFlow
import TensorFlowUnittest
import StdlibUnittest

var TopLevelTests = TestSuite("TopLevel")

#if !CUDA
TopLevelTests.testCPUOrGPU("TopLevel") {
  var x = Tensor<Int8>([1,2,3])*2
  x = x + x
  expectEqual(x.array, ShapedArray(shape: [3], scalars: [4, 8, 12]))
}
#endif

runAllTests()
