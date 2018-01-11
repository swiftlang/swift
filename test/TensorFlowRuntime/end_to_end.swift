// RUN: %target-run-simple-swift
// REQUIRES: executable_test
//
// Some simple end to end tests.

import CTensorFlow
import TensorFlow
import StdlibUnittest

var RuntimeTests = TestSuite("e2e")

// The C Type of s is TF_Status*.
func checkOk(_ s: OpaquePointer?) {
  precondition(TF_OK == TF_GetCode(s), String(cString: TF_Message(s)))
}

// The C Type of s is TF_Status*.
func checkFloatValueNear(_ outputTensor: CTensorHandle, _ expectedVal: Float) {
  let s = TF_NewStatus()
  let out_t = TFE_TensorHandleResolve(outputTensor, s)
  checkOk(s)
  TF_DeleteStatus(s)

  expectEqual(TF_TensorByteSize(out_t), 4)
  // This does: actualValue = *(float*)TF_TensorData(out_t)
  let actualValue: CFloat = TF_TensorData(out_t)!.bindMemory(to: CFloat.self, capacity: 1).pointee
  print("The actual output float value is \(actualValue)")
  expectLT(expectedVal - actualValue, 0.0001)
  TF_DeleteTensor(out_t)
}

@inline(never)
public func testTanh() {
  let x = Tensor<Float>(1.2)
  let y = tanh(x)

  checkFloatValueNear(y.handle.cTensorHandle, 0.833655)
}

RuntimeTests.test("e2e/BasicTanhTest") {
  testTanh()
}

runAllTests()
