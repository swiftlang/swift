// RUN: %target-run-simple-swift %swift-tensorflow-test-run-extra-options
// REQUIRES: executable_test

import CTensorFlow
import TensorFlow
import TensorFlowUnittest
import StdlibUnittest

var RuntimeEntryPointTests = TestSuite("RuntimeEntryPoint")

RuntimeEntryPointTests.testAllBackends("RoundTrip_CTensorHandle_AnyTensorHandle") {
  let zero: TensorHandle<Float> = Tensor<Float>(0.0).handle
  var cHandle = zero._cTensorHandle
  let status = TF_NewStatus()
  // We must do a copy, i.e. a retain on the tensor handle, to make sure it won't
  // get double-free'd when both `zero` and `anyHandle` below go out of scope.
  cHandle = TFE_TensorHandleCopySharingTensor(cHandle, status)
  expectEqual(TF_OK, TF_GetCode(status))
  TF_DeleteStatus(status)
  let anyHandle = TFETensorHandle(_owning: cHandle)
  let tensor = Tensor(handle: anyHandle as! TensorHandle<Float>)
  print(tensor)
  // This line hangs in GPE
  expectEqual(Tensor(0.0), tensor)
}

runAllTests()
