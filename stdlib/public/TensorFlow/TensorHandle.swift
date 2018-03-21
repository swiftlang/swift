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

/// TensorHandle<Scalar> is the type used by "ops" and the #tfop() syntax
/// specifically.  It includes an element type, which the tf-compiler internals
/// depend on to know what the dtype of params are when they are extracted out
/// into a tensor program.
@_fixed_layout // required because the compiler accesses cTensorHandle directly.
public final class TensorHandle<Scalar : AccelerableByTensorFlow> {
  /// This is the underlying "TF_TensorHandle*" which this TensorHandle
  /// represents.
  ///
  /// - Note: The compiler knows that TensorHandle has a single stored property,
  /// and assumes that this is it.  Changing the design of TensorHandle will
  /// require tweaking the compiler.
  public let cTensorHandle: CTensorHandle

  init(copyingFromCTensor cTensor: CTensor) {
    let status = TF_NewStatus()
    let cTensorHandle = TFE_NewTensorHandle(cTensor, status)
    checkOk(status)
    self.cTensorHandle = cTensorHandle!

    TF_DeleteStatus(status)
  }

  /// Create a tensor handle with a closure that initializes the underlying
  /// buffer.
  ///
  /// - Note: `scalarsInitializer` must initialize all scalars in the underlying
  /// buffer.
  @_versioned
  convenience init(
    shape: [Int32],
    scalarsInitializer: (UnsafeMutablePointer<Scalar>) -> Void
  ) {
    let contiguousSize = shape.lazy.map(Int.init).reduce(1, *)
    let byteCount = contiguousSize * MemoryLayout<Scalar>.stride
    // Initialize tensor and copy data.
    // TF_AllocateTensor() never returns nil.
    let cTensor = TF_AllocateTensor(
      Scalar.cDataType,
      shape.map(Int64.init),
      Int32(shape.count),
      byteCount
    )!
    assert(TF_TensorByteSize(cTensor) == byteCount)
    let addr = TF_TensorData(cTensor).assumingMemoryBound(to: Scalar.self)
    scalarsInitializer(addr)

    self.init(copyingFromCTensor: cTensor)
    TF_DeleteTensor(cTensor)
  }

  deinit {
    debugLog("De-initializing TensorHandle.")
    TFE_DeleteTensorHandle(cTensorHandle)
    debugLog("Returning from deinit of TensorHandle.")
  }
}

internal extension TensorHandle {
  /// Create a shaped array with contents of the underlying tensor. If the
  /// tensor is on the accelerator, it will be copied to the host.
  /// - Returns: A shaped array.
  @_versioned
  @inline(never)
  func makeHostCopy() -> ShapedArray<Scalar> {
    return ShapedArray(cTensorHandle: cTensorHandle)
  }
}

internal extension ShapedArray where Scalar : AccelerableByTensorFlow {
  @_versioned
  @inline(never)
  init(cTensorHandle: CTensorHandle) {
    let status = TF_NewStatus()
    // If the tensor is on the accelerator, we need to copy it to the host.
    // NOTE: This will not perform a copy if the handle is already on the host.
    let context = _ExecutionContext.global
    let hostHandle: CTensorHandle = context.withMutableCContext { ctx in
      debugLog("Calling TFE_TensorHandleCopyToDevice().")
      let ret = TFE_TensorHandleCopyToDevice(
        cTensorHandle, ctx, context.cpuDeviceName, status
      )
      checkOk(status)
      return ret!
    }
    defer { TFE_DeleteTensorHandle(hostHandle) }
    // Materialize the tensor on the host.
    debugLog("Resolving tensor.")
    let cTensor = TFE_TensorHandleResolve(hostHandle, status)
    checkOk(status)
    TF_DeleteStatus(status)
    debugLog("# of dims is \(TF_NumDims(cTensor!))")
    debugLog("Returning a shaped array.")
    self.init(owning: cTensor!)
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
