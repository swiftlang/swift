// RUN: %target-run-simple-swift %swift-tensorflow-test-run-extra-options
// RUN: %target-run-dynamic-compilation-swift %swift-tensorflow-test-run-extra-options
// REQUIRES: executable_test
// REQUIRES: swift_test_mode_optimize
//
// Runtime API and sync mode testing.

import StdlibUnittest
import CTensorFlow
import TensorFlow

// TODO(SR-7983): Investigate why this is necessary.
import SwiftOnoneSupport

var RuntimeTests = TestSuite("SyncRuntime")

_RuntimeConfig.usesSynchronousExecution = true

func checkOk(_ s: OpaquePointer?) {
  precondition(TF_OK == TF_GetCode(s), String(cString: TF_Message(s)))
}

func makeTensorHandle(_ value: Float) -> CTensorHandle {
  return _TFCCreateCTensorHandle(value, TF_FLOAT)
}

func expectNearlyEqual(_ outputTensor: CTensorHandle, _ expectedVal: Float) {
  let s = TF_NewStatus()
  let tensor = TFE_TensorHandleResolve(outputTensor, s)
  checkOk(s)
  TF_DeleteStatus(s)
  assert(TF_TensorByteSize(tensor) == MemoryLayout<Float>.stride)
  let actualValue = TF_TensorData(tensor).assumingMemoryBound(to: Float.self).pointee
  _hostOp("The actual output float value is \(actualValue)")
  expectLT(abs(expectedVal - actualValue), 0.0001)
  TF_DeleteTensor(tensor)
}

func decodeHex(_ string: String) -> [UInt8] {
  func hexToInt(_ c : UInt8) -> UInt8 {
    switch c {
    case UInt8(ascii: "0")...UInt8(ascii: "9"): return c - UInt8(ascii: "0")
    case UInt8(ascii: "a")...UInt8(ascii: "f"): return c - UInt8(ascii: "a") + 10
    case UInt8(ascii: "A")...UInt8(ascii: "F"): return c - UInt8(ascii: "A") + 10
    default: fatalError("invalid hexadecimal character")
    }
  }
  var result: [UInt8] = []
  assert(string.count & 1 == 0, "must get a pair of hexadecimal characters")
  var it = string.utf8.makeIterator()
  while let byte1 = it.next(),
  let byte2 = it.next() {  // we know we have an even byte length.
    result.append((hexToInt(byte1) << 4) | hexToInt(byte2))
  }
  return result
}

RuntimeTests.test("BasicTanhSync") {
  // The corresponding Swift program:
  //   public func g(value: Float) {
  //     let b = Tensor<Float>([value])
  //     let a = tanh(b)
  //     print(a)
  //   }
  // func foo() {
  //   g(value: 1.2)
  // }
  // SomeTests.testAllBackends("foo", foo)
  //
  let graphProto = """
    0A4E0A237466635F696E7075745F305F53346D61696E31673576616C75657953665F74462E7466120B506C616365686F6C6465722A0D0A05736861706512043A0218012A0B0A056474797065120230010A84010A207466635F66756E635F53346D61696E31673576616C75657953665F74462E7466122C53346D61696E31673576616C75657953665F74462E74665F4350552E6465766963655F706172746974696F6E1A237466635F696E7075745F305F53346D61696E31673576616C75657953665F74462E7466220D2F6465766963653A4350553A300A5B0A247466635F6F75747075745F305F53346D61696E31673576616C75657953665F74462E746612084964656E746974791A207466635F66756E635F53346D61696E31673576616C75657953665F74462E74662A070A0154120230011287020A84020A630A2C53346D61696E31673576616C75657953665F74462E74665F4350552E6465766963655F706172746974696F6E12090A056172675F3018011A280A246F705F5F73346D61696E31673576616C75657973665F74665F74665F6370755F33305F3618011A4B0A246F702E5F53346D61696E31673576616C75657953665F74462E74665F4350552E33302E36120454616E681A056172675F30220D2F6465766963653A4350553A302A070A01541202300122500A246F705F5F73346D61696E31673576616C75657973665F74665F74665F6370755F33305F3612286F702E5F53346D61696E31673576616C75657953665F74462E74665F4350552E33302E363A793A302204081A100C
    """
  let bytes = decodeHex(graphProto)

  let computation = _TFCStartTensorComputation(
    /*programByteAddress:*/ bytes,
    /*programByteCount:*/ bytes.count,
    /*entryFunctionNameAddress:*/ "S4main1g5valueySf_tF.tf",
    /*tensorArgumentAddress:*/ [makeTensorHandle(1.2)],
    /*tensorArgumentCount:*/ 1,
    /*helperFunctionCount:*/ 0,
    /*resultCount:*/ 1
  )

  let outputBuffer = UnsafeMutablePointer<CTensorHandle>.allocate(capacity: 1)
  _TFCFinishTensorComputation(computation, outputBuffer, 1)
  expectNearlyEqual(outputBuffer[0], 0.833655)
  outputBuffer.deallocate()
}

runAllTests()
