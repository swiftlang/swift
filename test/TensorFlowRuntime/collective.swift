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
  let handle: TensorHandle<Float> =
    #tfop("CollectiveReduce", x,
          T$dtype: Float.tensorFlowDataType,
          merge_op: "Add",
          final_op: "Id",
          subdiv_offsets: [Int64(0)],
          group_key: Int64(1),
          group_size: Int64(1),
          instance_key: Int64(1))
  let t = Tensor(handle: handle)
  _hostOp(t)
  expectEqualWithScalarTensor(1, t)
}

runAllTests()
