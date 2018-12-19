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

// Run 2 collective ops concurrently in graph mode.
//
// TODO: To enable this test in eager mode, need to put each collective op (or
// the enclosing graph function) in a separate swift thread.
#if !TF_DYNAMIC_COMPILATION
CollectiveTests.testAllBackends("GroupWithSize2") {
  // Need 2 or more CPU devices for this test.
  let x = Tensor<Float>(1.0)

  // Cannot use raw op due to the use of `__device`.
  // TODO: replace with user facing syntax like `withDevice`.
  let handle: TensorHandle<Float> =
    #tfop("CollectiveReduce", x,
          T$dtype: Float.tensorFlowDataType,
          merge_op: "Add",
          final_op: "Id",
          subdiv_offsets: [Int64(0)],
          group_key: Int64(2),
          group_size: Int64(2),
          instance_key: Int64(2),
          __device: "/job:localhost/replica:0/task:0/device:CPU:0")

  let _: TensorHandle<Float> =
    #tfop("CollectiveReduce", x,
          T$dtype: Float.tensorFlowDataType,
          merge_op: "Add",
          final_op: "Id",
          subdiv_offsets: [Int64(0)],
          group_key: Int64(2),
          group_size: Int64(2),
          instance_key: Int64(2),
          __device: "/job:localhost/replica:0/task:0/device:CPU:1")

  let t = Tensor(handle: handle)
  _hostOp(t)
  expectEqualWithScalarTensor(2, t)
}
#else
CollectiveTests.testAllBackends("GroupWithSize2") {
  // Need 2 or more CPU devices for this test.
  let x = Tensor<Float>(1.0)

  _runOnNDevices(2) { i in
    withDevice(.cpu, UInt(i)) {
      let t = Raw.collectiveReduce(x, groupSize: 2, groupKey: 2, instanceKey: 2,
          mergeOp: .add, finalOp: .id, subdivOffsets: [0])

      _hostOp(t)
      expectEqualWithScalarTensor(2, t)
    }
  }
}
#endif // TF_DYNAMIC_COMPILATION

_RuntimeConfig.cpuDeviceCount = 3
runAllTests()
