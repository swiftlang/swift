//===-- CompilerRuntime.swift ---------------------------------*- swift -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines the Swift Run-time API for TensorFlow computation.
//
//===----------------------------------------------------------------------===//

import CTensorFlow

// The C type is TF_Status*
public typealias CTF_Status = OpaquePointer?

public class TensorProgram {
  // Kept for debugging. Currently unused.
  let inputTensors: [AnyTensorHandle]

  let outputTensors: [AnyTensorHandle]

  init(inputs: [AnyTensorHandle], outputs: [AnyTensorHandle]) {
    inputTensors = inputs
    outputTensors = outputs
  }
}

func checkOk(_ s: CTF_Status) {
  precondition(TF_GetCode(s) == TF_OK, String(cString: TF_Message(s)))
}

// The call sequence for the APIs below must be one of the two:
// _TFCStartTensorProgram() -> _TFCTerminateTensorProgram()
// _TFCStartTensorProgram() -> _TFCFinishTensorProgram()

// Load the TF computation from a binary TF FunctionDef proto given by 'bytes'
// and 'size', start the computation, and return a state object as a unique
// identifier for that computation.
//
// For now we only support programs that output 1 tensor. To support multiple
// output tensors, we may need to extend this API to pass in a
// outputTensorCount.
public // COMPILER_INTRINSIC
func _TFCStartTensorProgram(
  _ bytes: UnsafeRawPointer,
  _ size: Int,
  _ inputTensors: UnsafePointer<AnyTensorHandle>,
  _ numInputTensors: Int
) -> TensorProgram {
  let s = TF_NewStatus()

  let tfFunc = TF_FunctionImportFunctionDef(bytes, size, s)
  checkOk(s)

  // Now we start the graph computation.
  let opts = TFE_NewContextOptions()
  let ctx = TFE_NewContext(opts, s)
  checkOk(s)
  TFE_DeleteContextOptions(opts)

  TFE_ContextAddFunction(ctx, tfFunc, s)
  checkOk(s)
  TF_DeleteFunction(tfFunc)

  let op = TFE_NewOp(ctx, "the_function", s)
  checkOk(s)
  let inputTensorArray : [AnyTensorHandle] =
    Array(UnsafeBufferPointer(start: inputTensors, count: numInputTensors))
  for inputTensor in inputTensorArray {
    let cTensorHandle = inputTensor.cTensorHandle
    TFE_OpAddInput(op, cTensorHandle, s)
    checkOk(s)
  }

  var retValCount: CInt = 1
  var retVals = [CTensorHandle](repeating: nil, count: Int(retValCount))
  TFE_Execute(op, &retVals, &retValCount, s)
  checkOk(s)
  precondition(retValCount == 1)
  TFE_DeleteOp(op)

  let outputTensors = retVals.map(AnyTensorHandle.init)

  TFE_DeleteContext(ctx, s)
  checkOk(s)
  TF_DeleteStatus(s)

  return TensorProgram(inputs: inputTensorArray, outputs: outputTensors)
}

// Terminate the computation as given by 'program', and clean up the state.
public // COMPILER_INTRINSIC
func _TFCTerminateTensorProgram(program: TensorProgram) {
  // Since we current do all the tensor computation in _TFCStartTensorProgram(),
  // this function is a no op.
}

// Wait for completion the computation as given by 'program', and returns output
// handles.
public // COMPILER_INTRINSIC
func _TFCFinishTensorProgram(
  _ program: TensorProgram
) -> [AnyTensorHandle] {
  return program.outputTensors
}
