//===-- TensorHandle.swift ------------------------------------*- swift -*-===//
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
// This file defines the TensorHandle type.
//
//===----------------------------------------------------------------------===//

import CTensorFlow

// The C type is TF_TensorHandle*
public typealias CTensorHandle = OpaquePointer

/// TensorHandle<Unit> is the type used by "ops" and the #tfop() syntax
/// specifically.  It includes an element type, which the tf-compiler internals
/// depend on to know what the dtype of params are when they are extracted out
/// into a tensor program.
public final class TensorHandle<Unit: AccelerableTensorUnit> {
  // This is the underlying "TF_TensorHandle*" which this TensorHandle
  // represents.
  //
  // NOTE: The compiler knows that TensorHandle has a single stored property,
  // and assumes that this is it.  Changing the design of TensorHandle will
  // require tweaking the compiler.
  public let cTensorHandle: CTensorHandle

  public init(cTensorHandle: CTensorHandle) {
    self.cTensorHandle = cTensorHandle
  }

  deinit {
    TFE_DeleteTensorHandle(cTensorHandle)
  }
}

extension TensorHandle {
  @_versioned
  @inline(never)
  func makeHostCopy() -> ShapedArray<Unit> {
    let status = TF_NewStatus()
    let cTensor = TFE_TensorHandleResolve(cTensorHandle, status)
    checkOk(status)
    TF_DeleteStatus(status)
    return ShapedArray(moving: cTensor!)
  }

  convenience init(copyingFromCTensor cTensor: CTensor) {
    let status = TF_NewStatus()
    let cHandle = TFE_NewTensorHandle(cTensor, status)
    checkOk(status)
    TF_DeleteStatus(status)
    self.init(cTensorHandle: cHandle!)
  }
}

// For "print", REPL, and Playgrounds integeration, we'll eventually want to
// implement this, probably in terms of fetching a summary.  For now, this is
// disabled.
#if false
/// Make "print(someTensor)" print a pretty form of the tensor.
extension TensorHandle : CustomStringConvertible {
  public var description: String {
    fatalError("unimplemented")
  }
}

// Make Tensors show up nicely in the Xcode Playground results sidebar.
extension TensorHandle : CustomPlaygroundQuickLookable {
  public var customPlaygroundQuickLook: PlaygroundQuickLook {
    return .text(description)
  }
}
#endif
