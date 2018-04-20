// RUN: %target-run-simple-swift
// REQUIRES: executable_test
// REQUIRES: swift_test_mode_optimize

import StdlibUnittest
import CTensorFlow
import TensorFlow

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
  print("The actual output float value is \(actualValue)")
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
  // TODO: Regenerate a proto that's not using "the_function".
  let graphProto = """
    12E8010AE5010A3A0A0C7468655F66756E6374696F6E12090A056172675F3018011A1F0A1B6\
    F705F5F7430317431673576616C75657973665F74665F335F313318011A320A076172675F30\
    5F30120B506C616365686F6C6465722A0D0A05736861706512043A0218012A0B0A056474797\
    065120230011A330A1B6F702E5F5430317431673576616C75657953665F74462E332E313312\
    0454616E681A056172675F302A070A015412023001223E0A1B6F705F5F74303174316735766\
    16C75657973665F74665F335F3133121F6F702E5F5430317431673576616C75657953665F74\
    462E332E31333A793A3022020818
    """
  let bytes = decodeHex(graphProto)

  // "the_function" as function name is only supported in eager mode.
  _RuntimeConfig.usesTFEagerAPI = true

  let computation = _TFCStartTensorComputation(
    /*programByteAddress:*/ bytes,
    /*programByteCount:*/ bytes.count,
    /*entryFunctionNameAddress:*/ "the_function",
    /*tensorArgumentAddress:*/ [makeTensorHandle(1.2)],
    /*tensorArgumentCount:*/ 1,
    /*resultCount:*/ 1
  )

  let outputBuffer = UnsafeMutablePointer<CTensorHandle>.allocate(capacity: 1)
  _TFCFinishTensorComputation(computation, outputBuffer, 1)
  expectNearlyEqual(outputBuffer[0], 0.833655)
  outputBuffer.deallocate()
}

runAllTests()
