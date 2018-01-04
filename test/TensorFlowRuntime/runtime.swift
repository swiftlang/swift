// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest
import CTensorFlow
import TensorFlow

var RuntimeTests = TestSuite("Runtime")

// The C Type of s is TF_Status*.
func checkOk(_ s: OpaquePointer?) {
  precondition(TF_OK == TF_GetCode(s), String(cString: TF_Message(s)))
}

func createFloatTensorHandle(_ f: Float) -> AnyTensorHandle {
  let s = TF_NewStatus()
  let in_t = TF_AllocateTensor(TF_FLOAT, nil, 0, 4)

  // This chunk of code does: *reinterpret_cast<float*>(TF_TensorData(in_t)) = f
  let inputRawPtr: UnsafeMutableRawPointer = TF_TensorData(in_t)!
  let inputPtr = inputRawPtr.bindMemory(to: CFloat.self, capacity: 1)
  inputPtr.pointee = CFloat(f)

  let in_h = TFE_NewTensorHandle(in_t, s)
  checkOk(s)
  TF_DeleteTensor(in_t)
  TF_DeleteStatus(s)
  return AnyTensorHandle(cTensorHandle: in_h)
}

func checkFloatValueNear(_ outputTensor: AnyTensorHandle, _ expectedVal: Float) {
  let s = TF_NewStatus()
  let out_t = TFE_TensorHandleResolve(outputTensor.cTensorHandle, s)
  checkOk(s)
  TF_DeleteStatus(s)

  precondition(TF_TensorByteSize(out_t) == 4)
  // This does: actualValue = *(float*)TF_TensorData(out_t)
  let actualValue: CFloat = TF_TensorData(out_t)!.bindMemory(to: CFloat.self, capacity: 1).pointee
  print("The actual output float value is \(actualValue)")
  precondition(expectedVal - actualValue < 0.0001, String(actualValue))
  TF_DeleteTensor(out_t)
}

func runProgram(_ progName: String,
  _ graphProto: StaticString,
  _ inputTensors: AnyTensorHandle...
) -> [AnyTensorHandle] {
  print("The input graph of program \(progName) has \(graphProto.utf8CodeUnitCount) bytes.")

  let program = _TFCStartTensorProgram(graphProto.utf8Start,
                                       graphProto.utf8CodeUnitCount,
                                       UnsafePointer<AnyTensorHandle>(inputTensors),
                                       inputTensors.count,
                                       /*number of output tensors*/1)
  let outputBuffer = UnsafeMutablePointer<AnyTensorHandle>.allocate(capacity: 1)
  _TFCFinishTensorProgram(program, outputBuffer, 1)

  // Load the result tensor, taking ownership from the unsafe pointer.
  let resultTensor = outputBuffer.move()
  outputBuffer.deallocate()

  print("Program \(progName) produced a tensor.")

  return [resultTensor]
}

func runTanh() {
  /* The corresponding Swift program:
   func g() {
     let a = Tensor1D<Float>(1,2,3)
     let c = a.tanh()
     print(c)
   }
   */
  let graphProto: StaticString = "\n4\n\u{0C}the_function\u{12}\t\n\u{05}arg_0\u{18}\u{01}\u{1A}\u{19}\n\u{15}op__t04main1gyyf_11_6\u{18}\u{01}\u{1A}-\n\u{15}op._T04main1gyyF.11.6\u{12}\u{04}Tanh\u{1A}\u{05}arg_0*\u{07}\n\u{01}T\u{12}\u{02}0\u{01}\"2\n\u{15}op__t04main1gyyf_11_6\u{12}\u{19}op._T04main1gyyF.11.6:y:0"

  let outputTensors = runProgram("Tanh", graphProto, createFloatTensorHandle(1.2))

  checkFloatValueNear(outputTensors[0], 0.833655)
}

func runAdd() {
  /* The corresponding Swift program:
  func f() {
    let a = Tensor1D<Float>(1,2,3)
    let b = Tensor1D<Float>(1,2,4)
    let c = a+b
    print(c)
  }
  */
  let graphProto: StaticString = "\nE\n\u{0C}the_function\u{12}\t\n\u{05}arg_0\u{18}\u{01}\u{12}\t\n\u{05}arg_1\u{18}\u{01}\u{1A}\u{1F}\n\u{1B}op__t04main8test_addyyf_4_6\u{18}\u{01}\u{1A}9\n\u{1B}op._T04main8test_addyyF.4.6\u{12}\u{03}Add\u{1A}\u{05}arg_0\u{1A}\u{05}arg_1*\u{07}\n\u{01}T\u{12}\u{02}0\u{01}\">\n\u{1B}op__t04main8test_addyyf_4_6\u{12}\u{1F}op._T04main8test_addyyF.4.6:z:0"

  let outputTensors = runProgram("Add", graphProto, createFloatTensorHandle(1.2), createFloatTensorHandle(3.4))

  checkFloatValueNear(outputTensors[0], 1.2+3.4)
}

RuntimeTests.test("Runtime/BasicTanhTest") {
  runTanh()
}

RuntimeTests.test("Runtime/BasicAddTest") {
  runAdd()
}

runAllTests()
