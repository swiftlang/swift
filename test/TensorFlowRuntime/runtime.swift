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

func createFloatTensorHandle(_ f: Float) -> CTensorHandle {
  let s = TF_NewStatus()
  let in_t = TF_AllocateTensor(TF_FLOAT, nil, 0, 4)

  // This chunk of code does: *reinterpret_cast<float*>(TF_TensorData(in_t)) = f
  let inputRawPtr: UnsafeMutableRawPointer = TF_TensorData(in_t)!
  let inputPtr = inputRawPtr.bindMemory(to: CFloat.self, capacity: 1)
  inputPtr.pointee = CFloat(f)

  let cTensorHandle = TFE_NewTensorHandle(in_t, s)!
  checkOk(s)
  TF_DeleteTensor(in_t)
  TF_DeleteStatus(s)
  return cTensorHandle
}

func checkFloatValueNear(_ outputTensor: CTensorHandle, _ expectedVal: Float) {
  let s = TF_NewStatus()
  let out_t = TFE_TensorHandleResolve(outputTensor, s)
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
  _ inputTensors: [CTensorHandle],
  shouldAbort: Bool = false
) -> [CTensorHandle] {
  print("The input graph of program \(progName) has \(graphProto.utf8CodeUnitCount) bytes.")

  let program = _TFCStartTensorProgram(graphProto.utf8Start,
                                       graphProto.utf8CodeUnitCount,
                                       inputTensors,
                                       inputTensors.count,
                                       /*number of output tensors*/1)
  let outputBuffer = UnsafeMutablePointer<CTensorHandle>.allocate(capacity: 1)

  defer {
    outputBuffer.deallocate()
  }

  if (shouldAbort) {
    _TFCTerminateTensorProgram(program)
    return []
  }

  _TFCFinishTensorProgram(program, outputBuffer, 1)
  // Load the result tensor, taking ownership from the unsafe pointer.
  let resultTensor = outputBuffer.move()

  print("Program \(progName) produced a tensor.")

  return [resultTensor]
}

// 0 input and output tensors.
func runConst() {
  /* The corresponding Swift program:
   func testTestorExample() {
     let x = Tensor<Float>(2.0)
   }
   */
  let graphProto: StaticString = "\n\u{0E}\n\u{0C}the_function\u{1A}V\n%op._T03e2e17testTestorExampleyyF.9.13\u{12}\u{05}Const*\u{19}\n\u{05}value\u{12}\u{10}B\u{0E}\u{08}\u{01}\u{12}\u{04}\u{12}\u{02}\u{08}\u{01}*\u{04}\0\0\0@*\u{0B}\n\u{05}dtype\u{12}\u{02}0\u{01}"

  let program = _TFCStartTensorProgram(graphProto.utf8Start,
                                       graphProto.utf8CodeUnitCount,
                                       /*inputTensors=*/[],
                                       /*inputTensors.count=*/0,
                                       /*number of output tensors*/0)

  // Even though there is no output tensor, we create outputBuffer via
  // UnsafeMutablePointer.allocate(). The generated SIL code differs from what
  // TF compiler would generate.
  let outputBuffer = UnsafeMutablePointer<CTensorHandle>.allocate(capacity: 0)
  _TFCFinishTensorProgram(program, outputBuffer, 0)
  outputBuffer.deallocate()
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

  let outputTensors = runProgram("Tanh", graphProto, [createFloatTensorHandle(1.2)])

  checkFloatValueNear(outputTensors[0], 0.833655)
}

func runAdd(shouldAbort: Bool) {
  /* The corresponding Swift program:
  func f() {
    let a = Tensor1D<Float>(1,2,3)
    let b = Tensor1D<Float>(1,2,4)
    let c = a+b
    print(c)
  }
  */
  let graphProto: StaticString = "\nE\n\u{0C}the_function\u{12}\t\n\u{05}arg_0\u{18}\u{01}\u{12}\t\n\u{05}arg_1\u{18}\u{01}\u{1A}\u{1F}\n\u{1B}op__t04main8test_addyyf_4_6\u{18}\u{01}\u{1A}9\n\u{1B}op._T04main8test_addyyF.4.6\u{12}\u{03}Add\u{1A}\u{05}arg_0\u{1A}\u{05}arg_1*\u{07}\n\u{01}T\u{12}\u{02}0\u{01}\">\n\u{1B}op__t04main8test_addyyf_4_6\u{12}\u{1F}op._T04main8test_addyyF.4.6:z:0"

  let outputTensors = runProgram("Add", graphProto,
                                 [createFloatTensorHandle(1.2),
                                  createFloatTensorHandle(3.4)],
                                 shouldAbort: shouldAbort)

  if (!shouldAbort) {
      checkFloatValueNear(outputTensors[0], 1.2+3.4)
  }
}

RuntimeTests.test("Runtime/BasicConstTest") {
  runConst()
}

RuntimeTests.test("Runtime/BasicTanhTest") {
  runTanh()
}

RuntimeTests.test("Runtime/BasicAddTest") {
  runAdd(shouldAbort: false)
}

RuntimeTests.test("Runtime/AddThenAbbortTest") {
  runAdd(shouldAbort: true)
}

RuntimeTests.test("Runtime/BasicTanhSyncTest") {
  _TFCRuntimeConfig.usesSynchronousExecution = true
  runTanh()
}

// Tests are currently executed sequentially, so if there are any subsequent
// tests intended for async runtime, set _TFCRuntimeConfig.usesSynchronousExecution = false
// for those tests.

runAllTests()
