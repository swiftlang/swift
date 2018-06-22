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

/// `TensorHandle` is the type used by ops and the `#tfop()` syntax
/// specifically. It includes a `Scalar` type, which compiler internals depend
/// on to determine the datatypes of parameters when they are extracted
/// into a tensor program.
@_fixed_layout // required because the compiler accesses cTensorHandle directly.
public final class TensorHandle<Scalar : AccelerableByTensorFlow> {
  /// The underlying `TF_TensorHandle *`.
  ///
  /// - Note: The compiler knows that `TensorHandle` has a single stored
  /// property, and assumes that this is it. Changing the design of
  /// `TensorHandle` will require tweaking the compiler.
  public let cTensorHandle: CTensorHandle

  @_versioned
  init(copyingFromCTensor cTensor: CTensor) {
    let status = TF_NewStatus()
    let cTensorHandle = TFE_NewTensorHandle(cTensor, status)
    checkOk(status)
    self.cTensorHandle = cTensorHandle!

    TF_DeleteStatus(status)
  }

  @_versioned
  init(owning cTensorHandle: CTensorHandle) {
    self.cTensorHandle = cTensorHandle
  }

  /// Create a `TensorHandle` with a closure that initializes the underlying
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
  /// Create a `ShapedArray` with contents of the underlying `TensorHandle`. If
  /// the `TensorHandle` is on the accelerator, it will be copied to the host.
  /// - Returns: A `ShapedArray`.
  @_versioned
  @inline(never)
  func makeHostCopy() -> ShapedArray<Scalar> {
    return ShapedArray(cTensorHandle: cTensorHandle)
  }
}

extension TensorHandle : TensorSendableReceivable {
  @_inlineable @_versioned
  static func receiveFromAccelerator(_ computation: _TensorComputation,
                                     _ tensorId: Int
  ) -> TensorHandle<Scalar> {
    debugLog("Receiving tensor of id \(tensorId) and type \(Scalar.self).")
    let status = TF_NewStatus()
    let cTensor: CTensor = TF_DequeueNamedTensor(
      computation.cSession, Int32(tensorId), status)
    checkOk(status)
    TF_DeleteStatus(status)
    let tensorHandle = TensorHandle<Scalar>(copyingFromCTensor: cTensor)
    TF_DeleteTensor(cTensor)
    if _RuntimeConfig.printsDebugLog {
      debugLog("The received tensor of id \(tensorId) has content:")
      dumpTensorContent(tensorHandle.cTensorHandle, Scalar.self)
    }
    return tensorHandle
  }

  @_inlineable @_versioned
  func sendToAccelerator(_ computation: _TensorComputation,
                         _ tensorId: Int) {
    if _RuntimeConfig.printsDebugLog {
      debugLog("Sending tensor of id \(tensorId) and type \(Scalar.self) with:")
      dumpTensorContent(self.cTensorHandle, Scalar.self)
    }
    let status = TF_NewStatus()
    let cTensor = TFE_TensorHandleResolve(self.cTensorHandle, status)
    checkOk(status)
    TF_EnqueueNamedTensor(
      computation.cSession, Int32(tensorId), cTensor, status)
    debugLog("Tensor is sent.")
    checkOk(status)
    TF_DeleteStatus(status)
    TF_DeleteTensor(cTensor)
  }

  @_inlineable @_versioned
  static func scalar(_ scalar: Scalar) -> TensorHandle<Scalar> {
    debugLog("Creating a tensor from scalar \(scalar).")
    let cTensorHandle = _TFCCreateCTensorHandle(scalar, Scalar.cDataType)
    return TensorHandle<Scalar>(owning: cTensorHandle)
  }
}

internal extension ShapedArray where Scalar : AccelerableByTensorFlow {
  @_versioned
  @inline(never)
  init(cTensorHandle: CTensorHandle) {
    let status = TF_NewStatus()
    // If the `CTensorHandle` is on the accelerator, it needs to be copied to
    // host.
    // NOTE: This will not perform a copy if the handle is already on the host.
    let context = _ExecutionContext.global
    let hostHandle: CTensorHandle = context.withMutableCContext { ctx in
      debugLog("Calling TFE_TensorHandleCopyToDevice().")
      let ret = TFE_TensorHandleCopyToDevice(
        cTensorHandle, ctx, context.cpuDeviceName, status
      )
      checkOk(status)
      internalConsistencyCheck(ret != nil,
                               "TFE_TensorHandleCopyToDevice() returned nil.")
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
