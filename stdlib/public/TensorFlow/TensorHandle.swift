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

/// `_AnyTensorHandle` is the scalar-agnostic base type for `TensorHandle`, used
/// specifically for low-level, type-erased passings of Swift-level tensor
/// handles in the compiler.
@_fixed_layout // required because the compiler accesses cTensorHandle directly.
public class _AnyTensorHandle {
  /// The underlying `TF_TensorHandle *`.
  ///
  /// - Note: The compiler knows that `_AnyTensorHandle` has a single stored
  /// property, and assumes that this is it. Changing the design of
  /// `TensorHandle` will require tweaking the compiler.
  public let cTensorHandle: CTensorHandle

  @usableFromInline
  init(copyingFromCTensor cTensor: CTensor) {
    let status = TF_NewStatus()
    let cTensorHandle = TFE_NewTensorHandle(cTensor, status)
    checkOk(status)
    self.cTensorHandle = cTensorHandle!
    TF_DeleteStatus(status)
  }

  @usableFromInline
  init(owning cTensorHandle: CTensorHandle) {
    self.cTensorHandle = cTensorHandle
  }

  deinit {
    debugLog("De-initializing TensorHandle.")
    TFE_DeleteTensorHandle(cTensorHandle)
    debugLog("Returning from deinit of TensorHandle.")
  }
}

/// `TensorHandle` is the type used by ops and the `#tfop()` syntax
/// specifically. It includes a `Scalar` type, which compiler internals depend
/// on to determine the datatypes of parameters when they are extracted
/// into a tensor program.
@_fixed_layout // required because the compiler accesses cTensorHandle directly.
public final class TensorHandle<Scalar> : _AnyTensorHandle
  where Scalar : AccelerableByTensorFlow {
  /// Create a `TensorHandle` with a closure that initializes the underlying
  /// buffer.
  ///
  /// - Note: `scalarsInitializer` must initialize all scalars in the underlying
  /// buffer.
  @usableFromInline
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
}

internal extension TensorHandle {
  /// Create a `ShapedArray` with contents of the underlying `TensorHandle`. If
  /// the `TensorHandle` is on the accelerator, it will be copied to the host.
  /// - Returns: A `ShapedArray`.
  @usableFromInline
  @inline(never)
  func makeHostCopy() -> ShapedArray<Scalar> {
    return ShapedArray(cTensorHandle: cTensorHandle)
  }
}

extension TensorHandle : TensorSendableReceivable {
  @inlinable
  static func receiveFromAccelerator(_ computation: _TensorComputation,
                                     _ tensorID: Int
  ) -> TensorHandle<Scalar> {
    debugLog("Receiving tensor of id \(tensorID) and type \(Scalar.self).")
    let status = TF_NewStatus()
    internalConsistencyCheck(status != nil)
    let tensorHandle: TensorHandle<Scalar>
    if _RuntimeConfig.usesTFEagerAPI {
      let context = _ExecutionContext.global
      let cTensorHandle = TFE_DequeueNamedTensorFromCtx(
        context.eagerContext, Int32(tensorID), Scalar.cDataType, status)
      checkOk(status)
      tensorHandle = TensorHandle<Scalar>(owning: cTensorHandle!)
    } else {
      let cTensor: CTensor! = TF_DequeueNamedTensor(
        computation.cSession, Int32(tensorID), status)
      checkOk(status)
      internalConsistencyCheck(
        cTensor != nil,
        "TF_DequeueNamedTensor() cannot return nil when the status is OK.")
      TF_DeleteStatus(status)
      tensorHandle = TensorHandle<Scalar>(copyingFromCTensor: cTensor)
      TF_DeleteTensor(cTensor)
    }
    if _RuntimeConfig.printsDebugLog {
      debugLog("The received tensor of id \(tensorID) has content:")
      dumpTensorContent(tensorHandle.cTensorHandle, Scalar.self)
    }
    return tensorHandle
  }

  @inlinable
  func sendToAccelerator(_ computation: _TensorComputation,
                         _ tensorID: Int) {
    if _RuntimeConfig.printsDebugLog {
      debugLog("Sending tensor of id \(tensorID) and type \(Scalar.self) with:")
      dumpTensorContent(cTensorHandle, Scalar.self)
    }
    let status = TF_NewStatus()
    internalConsistencyCheck(status != nil)
    if _RuntimeConfig.usesTFEagerAPI {
      let context = _ExecutionContext.global
      TFE_EnqueueNamedTensorFromCtx(
        context.eagerContext, Int32(tensorID), cTensorHandle, status)
    } else {
      let cTensor = TFE_TensorHandleResolve(cTensorHandle, status)
      checkOk(status)
      TF_EnqueueNamedTensor(
        computation.cSession, Int32(tensorID), cTensor, status)
      TF_DeleteTensor(cTensor)
    }
    debugLog("Tensor is sent.")
    checkOk(status)
    TF_DeleteStatus(status)
  }

  @inlinable
  static func scalar(_ scalar: Scalar) -> TensorHandle<Scalar> {
    debugLog("Creating a tensor from scalar \(scalar).")
    let cTensorHandle = _TFCCreateCTensorHandle(scalar, Scalar.cDataType)
    return TensorHandle<Scalar>(owning: cTensorHandle)
  }
}

internal extension ShapedArray where Scalar : AccelerableByTensorFlow {
  @usableFromInline
  @inline(never)
  init(cTensorHandle: CTensorHandle) {
    let status = TF_NewStatus()
    // If the `CTensorHandle` is on the accelerator, it needs to be copied to
    // host.
    // NOTE: This will not perform a copy if the handle is already on the host.
    let context = _ExecutionContext.global
    debugLog("Calling TFE_TensorHandleCopyToDevice().")
    let hostHandle: CTensorHandle! = TFE_TensorHandleCopyToDevice(
      cTensorHandle, context.eagerContext, context.cpuDeviceName, status)
    checkOk(status)
    internalConsistencyCheck(hostHandle != nil,
                             "TFE_TensorHandleCopyToDevice() returned nil.")
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
