// RUN: %target-run-simple-swift %swift-tensorflow-test-run-extra-options
// RUN: %target-run-dynamic-compilation-swift %swift-tensorflow-test-run-extra-options
// REQUIRES: executable_test
// REQUIRES: swift_test_mode_optimize

// Tests on collective ops, as a building block for data/model parallel programs.

import TensorFlow
#if TPU
import TensorFlowUnittestTPU
#else
import TensorFlowUnittest
#endif
import StdlibUnittest

var CollectiveTests = TestSuite("Collective")

// TODO: Probably should be disabled for TPU execution.
CollectiveTests.testAllBackends("SingletonGroup") {
  let x = Tensor<Float>(1.0)
  let t = Raw.collectiveReduce(x, groupSize: 1, groupKey: 1, instanceKey: 1,
                               mergeOp: .add, finalOp: .id, subdivOffsets: [0])
  _hostOp(t)
  expectEqualWithScalarTensor(1, t)
}

runAllTests()
