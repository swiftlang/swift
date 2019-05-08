// RUN: %target-run-simple-swift %swift-tensorflow-test-run-extra-options
// REQUIRES: executable_test

// Tests on collective ops, as a building block for data/model parallel programs.

import TensorFlow
#if TPU
import TensorFlowUnittestTPU
#else
import TensorFlowUnittest
#endif
import StdlibUnittest

var CollectiveTests = TestSuite("Collective")

CollectiveTests.testAllBackends("ConfigTest") {
  // Run some tensor code to trigger runtime configuration.
  _hostOp(Tensor<Float>(0.0) + Tensor<Float>(1.0))
  expectEqual(3, _RuntimeConfig.cpuDeviceCount)
}

// TODO: Probably should be disabled for TPU execution.
CollectiveTests.testAllBackends("SingletonGroup") {
  let x = Tensor<Float>(1.0)
  let t = Raw.collectiveReduce(x, groupSize: 1, groupKey: 1, instanceKey: 1,
                               mergeOp: .add, finalOp: .id, subdivOffsets: [0],
                               waitFor: [])
  _hostOp(t)
  expectEqualWithScalarTensor(1, t)
}

CollectiveTests.testAllBackends("GroupWithSize2_threads") {
  // Need 2 or more CPU devices for this test.
  let x = Tensor<Float>(1.0)

  _runOnNDevices(2) { i in
    withDevice(.cpu, UInt(i)) {
      let t = Raw.collectiveReduce(x, groupSize: 2, groupKey: 3, instanceKey: 3,
          mergeOp: .add, finalOp: .id, subdivOffsets: [0], waitFor: [])

      _hostOp(t)
      expectEqualWithScalarTensor(2, t)
    }
  }
}

runAllTests()
