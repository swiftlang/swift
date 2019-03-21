// RUN: %target-run-eager-swift %swift-tensorflow-test-run-extra-options

// SR-9737: hanging tests in GPE GPU mode
// UN: %target-run-gpe-swift %swift-tensorflow-test-run-extra-options

// REQUIRES: executable_test
// REQUIRES: swift_test_mode_optimize

import CTensorFlow
import TensorFlow
#if TPU
import TensorFlowUnittestTPU
#else
import TensorFlowUnittest
#endif
import StdlibUnittest

var RuntimeEntryPointTests = TestSuite("RuntimeEntryPoint")

RuntimeEntryPointTests.testCPUOrGPU("RoundTrip_CTensorHandle_AnyTensorHandle") {
  let zero: TensorHandle<Float> = Tensor<Float>(0.0).handle
  var cHandle = zero._cTensorHandle
  let status = TF_NewStatus()
  // We must do a copy, i.e. a retain on the tensor handle, to make sure it won't
  // get double-free'd when both `zero` and `anyHandle` below go out of scope.
  cHandle = TFE_TensorHandleCopySharingTensor(cHandle, status)
  expectEqual(TF_OK, TF_GetCode(status))
  TF_DeleteStatus(status)
  let anyHandle = _TFCCreateTensorHandleFromC(cHandle)
  let tensor = Tensor(handle: anyHandle as! TensorHandle<Float>)
  print(tensor)
  // This line hangs in GPE
  expectEqual(Tensor(0.0), tensor)
}

runAllTests()
