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
@_fixed_layout // required because the compiler accesses _cTensorHandle directly.
public class _AnyTensorHandle {
  /// The underlying `TF_TensorHandle *`.
  ///
  /// - Note: The compiler knows that `_AnyTensorHandle` has a single stored
  /// property, and assumes that this is it. Changing the design of
  /// `TensorHandle` will require tweaking the compiler.
  public let _cTensorHandle: CTensorHandle
  
  /// Private initializer from a `CTensorHandle`. Should only be called from
  /// `TensorHandle<Scalar>.init`.
  fileprivate init(base: CTensorHandle) {
    self._cTensorHandle = base
  }
}

/// `TensorHandle` is the type used by ops and the `#tfop()` syntax
/// specifically. It includes a `Scalar` type, which compiler internals depend
/// on to determine the datatypes of parameters when they are extracted
/// into a tensor program.
@_fixed_layout // required because the compiler accesses _cTensorHandle directly.
public final class TensorHandle<Scalar> : _AnyTensorHandle
  where Scalar : AccelerableByTensorFlow {
  public init(_owning cTensorHandle: CTensorHandle) {
    super.init(base: cTensorHandle)
  }
  
  @usableFromInline
  convenience init(copyingFromCTensor cTensor: CTensor) {
    let status = TF_NewStatus()
    let cTensorHandle = TFE_NewTensorHandle(cTensor, status)
    checkOk(status)
    self.init(_owning: cTensorHandle!)
    TF_DeleteStatus(status)
  }

  deinit {
    debugLog("De-initializing TensorHandle.")
    TFE_DeleteTensorHandle(_cTensorHandle)
    debugLog("Returning from deinit of TensorHandle.")
  }
}

// TODO: We should add a "Scalar : TensorBuffersArePlainScalars" (better
// protocol name TBD) constraint to this extension, because these initializers
// only work for Tensors whose underlying buffers are filled with plain
// scalars.  In particular, this will exclude String scalars.
extension TensorHandle {
  /// Create a `TensorHandle` with a closure that initializes the underlying
  /// buffer.  The Scalar must not be a String.
  ///
  /// - Note: `scalarsInitializer` must initialize all scalars in the underlying
  /// buffer.
  @usableFromInline
  convenience init(
    shape: [Int32],
    scalarsInitializer: (UnsafeMutablePointer<Scalar>) -> Void
  ) {
    precondition(Scalar.self != String.self,
                 "Cannot use scalarsInitializer with String.")
    let contiguousSize = shape.lazy.map(Int.init).reduce(1, *)
    let byteCount = contiguousSize * MemoryLayout<Scalar>.stride
    // Initialize tensor and copy data.
    // TF_AllocateTensor() never returns nil.
    let cTensor = TF_AllocateTensor(
      Scalar.tensorFlowDataType.cDataType,
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

  /// Create a `TensorHandle` with the specified `scalars` and `shape`.  The
  /// Scalar must not be a String.
  @usableFromInline
  convenience init(_ scalars: [Scalar], shape: [Int32]) {
    precondition(Scalar.self != String.self,
                 "Cannot use scalarsInitializer with String.")
    let contiguousSize = shape.map(Int.init).reduce(1, *)
    precondition(scalars.count == contiguousSize,
                 "The number of scalars does not match the shape.")
    self.init(
      shape: shape,
      scalarsInitializer: { addr in
        scalars.withUnsafeBufferPointer { ptr in
          addr.assign(from: ptr.baseAddress!, count: contiguousSize)
        }
      }
    )
  }
}

extension TensorHandle where Scalar == String {
  /// Create a `TensorHandle<String>` with the specified `strings` and `shape`.
  @usableFromInline
  convenience init(strings: [String], shape: [Int32]) {
    let contiguousSize = shape.map(Int.init).reduce(1, *)
    precondition(strings.count == contiguousSize,
                 "The number of strings does not match the shape.")

    let cStrings = strings.map({ $0.utf8CString })

    let tfEncodedSizes = cStrings.map({ cString -> Int in
      // The cStrings are null-terminated. TF_StringEncodedSize wants the
      // string's byte count not including null-terminator.
      return TF_StringEncodedSize(cString.count - 1)
    })

    // Format information copied from tensorflow/c/c_api.h:
    // The format for TF_STRING tensors is:
    //   start_offset: array[uint64]
    //   data:         byte[...]
    //
    //   The string length (as a varint), followed by the contents of the string
    //   is encoded at data[start_offset[i]]].

    // The size of the "start_offset" region.
    let startOffsetsByteCount = strings.count * MemoryLayout<UInt64>.stride

    // The size of the "data" region.
    let dataByteCount =
        tfEncodedSizes.reduce(0, +) * MemoryLayout<UInt8>.stride

    // Allocate the tensor.
    let cTensor = TF_AllocateTensor(
      Scalar.tensorFlowDataType.cDataType,
      shape.map(Int64.init),
      Int32(shape.count),
      startOffsetsByteCount + dataByteCount
    )!
    assert(TF_TensorByteSize(cTensor) == startOffsetsByteCount + dataByteCount)
    let tensorAddr = TF_TensorData(cTensor)

    // Initialize the "start_offset" region.
    var startOffset: UInt64 = 0
    var startOffsetAddr =
        tensorAddr?.bindMemory(to: UInt64.self, capacity: strings.count)
    for tfEncodedSize in tfEncodedSizes {
      startOffsetAddr!.initialize(to: startOffset)
      startOffsetAddr = startOffsetAddr?.advanced(by: 1)
      startOffset = startOffset + UInt64(tfEncodedSize)
    }

    // Initialize the "data" region.
    var dataAddr = tensorAddr?.advanced(by: startOffsetsByteCount).bindMemory(to: Int8.self, capacity: dataByteCount)
    let status = TF_NewStatus()
    for (cString, tfEncodedSize) in zip(cStrings, tfEncodedSizes) {
      let _ = cString.withUnsafeBufferPointer { buffer in
        TF_StringEncode(buffer.baseAddress, buffer.count - 1, dataAddr, tfEncodedSize, status)
      }
      checkOk(status)
      dataAddr = dataAddr?.advanced(by: tfEncodedSize)
    }
    TF_DeleteStatus(status)

    self.init(copyingFromCTensor: cTensor)
    TF_DeleteTensor(cTensor)
    return
  }
}

internal extension TensorHandle {
  /// Create a `ShapedArray` with contents of the underlying `TensorHandle`. If
  /// the `TensorHandle` is on the accelerator, it will be copied to the host.
  /// - Returns: A `ShapedArray`.
  @usableFromInline
  @inline(never)
  func makeHostCopy() -> ShapedArray<Scalar> {
    return ShapedArray(cTensorHandle: _cTensorHandle)
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
        context.eagerContext, Int32(tensorID),
        Scalar.tensorFlowDataType.cDataType, status)
      checkOk(status)
      tensorHandle = TensorHandle<Scalar>(_owning: cTensorHandle!)
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
      dumpTensorContent(tensorHandle._cTensorHandle, Scalar.self)
    }
    return tensorHandle
  }

  @inlinable
  func sendToAccelerator(_ computation: _TensorComputation,
                         _ tensorID: Int) {
    if _RuntimeConfig.printsDebugLog {
      debugLog("Sending tensor of id \(tensorID) and type \(Scalar.self) with:")
      dumpTensorContent(_cTensorHandle, Scalar.self)
    }
    let status = TF_NewStatus()
    internalConsistencyCheck(status != nil)
    if _RuntimeConfig.usesTFEagerAPI {
      let context = _ExecutionContext.global
      TFE_EnqueueNamedTensorFromCtx(
        context.eagerContext, Int32(tensorID), _cTensorHandle, status)
    } else {
      let cTensor = TFE_TensorHandleResolve(_cTensorHandle, status)
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
    let cTensorHandle = _TFCCreateCTensorHandle(
        scalar, Scalar.tensorFlowDataType.cDataType)
    return TensorHandle<Scalar>(_owning: cTensorHandle)
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

/// `ResourceHandle` is the type used by ops and the `#tfop()` syntax to
/// represent TensorFlow "resource" values.
public final class ResourceHandle : _AnyTensorHandle {
  @usableFromInline
  init(owning cTensorHandle: CTensorHandle) {
    super.init(base: cTensorHandle)
  }

  deinit {
    debugLog("De-initializing TensorHandle.")
    TFE_DeleteTensorHandle(_cTensorHandle)
    debugLog("Returning from deinit of ResourceHandle.")
  }
}

extension ResourceHandle : TensorSendableReceivable {
  @inlinable
  static func receiveFromAccelerator(
    _ computation: _TensorComputation,
    _ tensorID: Int
  ) -> ResourceHandle {
    debugLog("Receiving resource tensor of id \(tensorID).")
    let status = TF_NewStatus()
    let context = _ExecutionContext.global
    let cTensorHandle: CTensorHandle! = TFE_DequeueNamedTensorFromCtx(
      context.eagerContext, Int32(tensorID), TF_RESOURCE, status)
    checkOk(status)
    TF_DeleteStatus(status)
    debugLog("Done receiving resource tensor of id \(tensorID).")
    return ResourceHandle(owning: cTensorHandle)    
  }

  @inlinable
  func sendToAccelerator(_ computation: _TensorComputation,
                         _ tensorID: Int) {
    debugLog("Sending resource tensor of id \(tensorID).")
    let status = TF_NewStatus()
    let context = _ExecutionContext.global
    TFE_EnqueueNamedTensorFromCtx(
      context.eagerContext, Int32(tensorID), self._cTensorHandle, status)
    TF_DeleteStatus(status)
    debugLog("Done sending resource tensor of id \(tensorID).")
  }

  // TODO: remove this dummy Scalar typealias, currently required in order to
  // conform to TensorSendableReceivable.
  typealias Scalar = Float
  @inlinable
  static func scalar(_ scalar: Scalar) -> ResourceHandle {
    fatalError("Unsupported")
  }
}

/// `VariantHandle` is the type used by ops and the `#tfop()` syntax to
/// represent TensorFlow "variant" values.
public final class VariantHandle : _AnyTensorHandle {
  @usableFromInline
  init(owning cTensorHandle: CTensorHandle) {
    super.init(base: cTensorHandle)
  }

  deinit {
    debugLog("De-initializing TensorHandle.")
    TFE_DeleteTensorHandle(_cTensorHandle)
    debugLog("Returning from deinit of VariantHandle.")
  }
}

extension VariantHandle : TensorSendableReceivable {
  @inlinable
  static func receiveFromAccelerator(
    _ computation: _TensorComputation,
    _ tensorID: Int
  ) -> VariantHandle {
    debugLog("Receiving variant tensor of id \(tensorID).")
    let status = TF_NewStatus()
    let context = _ExecutionContext.global
    let cTensorHandle: CTensorHandle! = TFE_DequeueNamedTensorFromCtx(
      context.eagerContext, Int32(tensorID), TF_VARIANT, status)
    checkOk(status)
    TF_DeleteStatus(status)
    debugLog("Done receiving variant tensor of id \(tensorID).")
    return VariantHandle(owning: cTensorHandle)    
  }

  @inlinable
  func sendToAccelerator(_ computation: _TensorComputation,
                         _ tensorID: Int) {
    debugLog("Sending variant tensor of id \(tensorID).")
    let status = TF_NewStatus()
    let context = _ExecutionContext.global
    TFE_EnqueueNamedTensorFromCtx(
      context.eagerContext, Int32(tensorID), self._cTensorHandle, status)
    TF_DeleteStatus(status)
    debugLog("Done sending variant tensor of id \(tensorID).")
  }

  // TODO: remove this dummy Scalar typealias, currently required in order to
  // conform to TensorSendableReceivable.
  typealias Scalar = Float
  @inlinable
  static func scalar(_ scalar: Scalar) -> VariantHandle {
    fatalError("Unsupported")
  }
}
