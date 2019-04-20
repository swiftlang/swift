// FIXME: TFPartition fails in `GraphFunctionDeviceInfo::finalizeUsedDevices()`
// because used device set includes RUNTIME device.
// UN: %target-run-gpe-swift %swift-tensorflow-test-run-extra-options

// RUN: %target-run-eager-swift %swift-tensorflow-test-run-extra-options
// REQUIRES: executable_test
// REQUIRES: swift_test_mode_optimize
//
// Compiler-only testing for TPU graph lowering (e.g. shape requirements by XLA).
// RUN: %target-swift-frontend -Xllvm -tf-dump-intermediates -Xllvm -tf-dump-graph -Xllvm -tf-target-tpu -O -emit-sil %s >/dev/null
//
// Tensor API tests.

import TensorFlow
#if TPU
import TensorFlowUnittestTPU
#else
import TensorFlowUnittest
#endif
import StdlibUnittest

TensorTests.testAllBackends("ReductionToScalar") {
  let _: Tensor<Float> = [1, 2, 3, 4, 5]
  // expectEqual(x.mean(), 3)
  // TODO: Test other reduction ops here. Currently code motion isn't
  // smart enough to avoid send/receive.

  // TODO: remove the extra code below once TPU execution supports 0 output
  // tensors (b/111123797)
  let extra = Tensor<Float>(1.0)
  _hostOp(extra)
}

// For now it is sufficient to run remote tests with test cases in this file
// only. When creating new test files, consider simply calling runAllTests().
#if CUDA
// RemoteSession does not work for GPU because partitioning logic gets confused
// with multiple devices.
runAllTests()
#else
runAllTestsWithRemoteSession()
#endif  // CUDA
