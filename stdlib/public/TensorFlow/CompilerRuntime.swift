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

@_versioned
func checkOk(_ s: CTF_Status) {
  precondition(TF_GetCode(s) == TF_OK, String(cString: TF_Message(s)))
}

// The call sequence for the APIs below must be one of the two:
//    init -> terminate()
//    init -> finish()
// The finish/terminate APIs may only be called once.
//
public final class TensorProgram {
  let outputTensors: [CTensorHandle]

  // Load the TF computation from a binary TF FunctionDef proto given by 'bytes'
  // and 'size', start the computation, and return a state object as a unique
  // identifier for that computation.
  @_versioned
  init(programByteAddress: UnsafeRawPointer,
       programByteCount: Int,
       tensorArgumentAddress: UnsafePointer<CTensorHandle>,
       tensorArgumentCount: Int,
       // TODO(clattner): resultCount should go away when the runtime is
       // implemented with an async design.
       resultCount: Int) {
    let inputTensors = UnsafeBufferPointer(start: tensorArgumentAddress,
                                           count: tensorArgumentCount)

    let s = TF_NewStatus()
    let tfFunc = TF_FunctionImportFunctionDef(programByteAddress,
                                              programByteCount, s)
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

    for inputTensor in inputTensors {
      TFE_OpAddInput(op, inputTensor, s)
      checkOk(s)
    }

    var retVals = [CTensorHandle?](repeating: nil, count: resultCount)
    var retValCount = CInt(resultCount)
    TFE_Execute(op, &retVals, &retValCount, s)
    checkOk(s)
    assert(Int(retValCount) == resultCount,
           "internal compiler error, result count mismatch!")
    TFE_DeleteOp(op)
    TFE_DeleteContext(ctx, s)
    checkOk(s)
    TF_DeleteStatus(s)

    // Now that all the elements have been filled in, remove a level of
    // optional.
    self.outputTensors = retVals.map { $0! }
  }

  // Terminate the computation as given by 'program', and clean up the state.
  //
  // TODO(hongm): add real logic, including handling input/output and errors.
  @_versioned
  func terminate() {
  }

  // Wait for completion the computation as given by 'program', and returns
  // output handles.
  //
  // TODO(hongm): add real logic, including handling input/output and errors.
  @_versioned
  func finish() -> [CTensorHandle] {
    return outputTensors
  }
}


//===----------------------------------------------------------------------===//
//  Compiler runtime entrypoints
//===----------------------------------------------------------------------===//

// These are the entrypoints that are well-known to the compiler internals.  The
// signatures and forms must not be changed without updating the compiler.  Any
// code put into the body of these functions will end up being inlined into the
// user code, so they are generally just wrappers around the implementation
// above.

// Load the TF computation from a binary TF FunctionDef proto given by 'bytes'
// and 'size', start the computation, and return a state object as a unique
// identifier for that computation.
//
@_inlineable
@_silgen_name("_swift_tfc_StartTensorProgram")
public func _TFCStartTensorProgram(
  _ programByteAddress: UnsafeRawPointer,
  _ programByteCount: Int,
  _ tensorArgumentAddress: UnsafePointer<CTensorHandle>,
  _ tensorArgumentCount: Int,
  // TODO(clattner): resultCount should go away when the runtime is implemented
  // with an async design.
  _ resultCount: Int
) -> TensorProgram {
  return TensorProgram(programByteAddress: programByteAddress,
                       programByteCount: programByteCount,
                       tensorArgumentAddress: tensorArgumentAddress,
                       tensorArgumentCount: tensorArgumentCount,
                       resultCount: resultCount)
}

// Terminate the computation as given by 'program', and clean up the state.
//
@_inlineable
@_silgen_name("_swift_tfc_TerminateTensorProgram")
public func _TFCTerminateTensorProgram(_ program: TensorProgram) {
  program.terminate()
}

/// Wait for completion the computation as given by 'program', and returns
/// results.
///
/// NOTE: The result address as passed in is pointing to uninitialized memory,
/// this must initialize the memory, transfering ownership of the tensor handles
/// to the caller.
///
@_inlineable
@_silgen_name("_swift_tfc_FinishTensorProgram")
public func _TFCFinishTensorProgram(
  _ program: TensorProgram,
  _ tensorResultAddress: UnsafeMutablePointer<CTensorHandle>,
  _ tensorResultCount: Int) {

  let results = program.finish()
  assert(results.count == tensorResultCount,
         "internal compiler error: result count mismatch!")

  let resultBuffer = UnsafeMutableBufferPointer(start: tensorResultAddress,
                                                count: tensorResultCount)
  _ = resultBuffer.initialize(from: results)
}

/// This function transforms a scalar value into a TensorHandle.
@_inlineable
@_silgen_name("_swift_tfc_CreateCTensorHandle")
public func _TFC_CreateCTensorHandle<T>(_ value : T,
                                        _ dtype: TF_DataType) -> CTensorHandle {
  let tensor = TF_AllocateTensor(dtype, nil, 0, MemoryLayout<T>.stride)

  // This chunk of code does: *reinterpret_cast<float*>(TF_TensorData(in_t)) = f
  TF_TensorData(tensor).bindMemory(to: T.self, capacity: 1).pointee = value

  let status = TF_NewStatus()
  let cTensorHandle = TFE_NewTensorHandle(tensor, status)
  checkOk(status)
  TF_DeleteStatus(status)
  TF_DeleteTensor(tensor)
  return cTensorHandle!
}

