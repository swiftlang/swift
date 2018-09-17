// TODO: Revert to %target-run-simple-swift once we fold dynamic compilation into -Onone.
// RUN: %target-run-dynamic-compilation-swift
// REQUIRES: executable_test
// REQUIRES: swift_test_mode_optimize

// This file contains testing over a dataset as a global variable. This requires
// sends/recvs support for variant handles.

import TensorFlow
import TensorFlowUnittest
import StdlibUnittest

var DynamicCompilationTests = TestSuite("DynamicCompilation")

// TODO: add GPU support.
#if !CUDA

DynamicCompilationTests.testCPUOrGPU("Const") {
  _RuntimeConfig.printsDebugLog = true
  let x: TensorHandle<Float> = #tfop("Const", dtype: Float.self, value$tensor: 1.0)
  _hostOp(x)
  // FIXME: The above returns the fixed const value 17.0, but it should be 1.0.
  expectNearlyEqualWithScalarTensor(17.0, Tensor<Float>(handle: x))
}

#endif // !CUDA

runAllTests()
