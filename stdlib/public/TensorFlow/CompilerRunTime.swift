//===-- CompilerRunTime.swift ---------------------------------*- swift -*-===//
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

// TODO(hongm): Rename to CompilerRuntime.swift.

import Swift

// TODO(hongm): replace the dummy impl below with a real one.
public class TensorProgram {
  let inputTensors: [AnyTensorHandle]
  // let session_handle: UnsafePointer<CChar>

  init(_ input: [AnyTensorHandle]) {
      inputTensors = input
  }
}

// This corresponds to the TensorHandle class in TF eager C API.
public class AnyTensorHandle {
  // TODO(hongm): replace this dummy impl with something like
  // UnsafePointer<TF_TensorHandle>
  public let value: Int
  public init(_ v: Int) {
      value = v
  }
}

// The call sequence for the APIs below must be one of the two:
// _TFCStartTensorProgram() -> _TFCTerminateTensorProgram()
// _TFCStartTensorProgram() -> _TFCFinishTensorProgram()

// Load the TF computation from a binary TF FunctionDef proto given by 'bytes'
// and 'size', start the computation, and return a state object as a unique
// identifier for that computation.
//
// TODO(hongm): add real logic, including handling input/output and errors.
public func _TFCStartTensorProgram(
  _ bytes: UnsafeRawPointer,
  _ size: Int,
  _ inputTensors: AnyTensorHandle...
) -> TensorProgram {
  let program = TensorProgram(inputTensors)
  return program
}

// Terminate the computation as given by 'program', and clean up the state.
//
// TODO(hongm): add real logic, including handling input/output and errors.
public func _TFCTerminateTensorProgram(program: TensorProgram) {
}

// Wait for completion the computation as given by 'program', and returns output
// handles.
//
// TODO(hongm): add real logic, including handling input/output and errors.
public func _TFCFinishTensorProgram(
  _ program: TensorProgram
) -> [AnyTensorHandle] {
  return program.inputTensors
}
