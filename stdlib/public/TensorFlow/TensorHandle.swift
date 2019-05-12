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
  /// The underlying `TFE_TensorHandle *`.
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

/// `TensorHandle` is the type used by ops. It includes a `Scalar` type, which 
/// compiler internals use to determine the datatypes of parameters when they 
/// are extracted into a tensor program.
@_fixed_layout // required because the compiler accesses _cTensorHandle directly.
public final class TensorHandle<Scalar> : _AnyTensorHandle
  where Scalar : _TensorFlowDataTypeCompatible {
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

  /// Create a `TensorHandle` with a closure that initializes the underlying
  /// buffer.
  ///
  /// Users initializing `TensorHandle`s with non-`String` scalars should use
  /// the `init(shape:scalarsInitializer:)` initializer instead of this one. It
  /// enforces additional constraints on the buffer that hold for all
  /// non-`String` scalars.
  ///
  /// `bufferInitializer` receives a buffer with exactly `byteCount` bytes of
  /// capacity. `bufferInitializer` must initialize the entire buffer.
  @usableFromInline
  convenience init(
    shape: [Int],
    byteCount: Int,
    bufferInitializer: (UnsafeMutableRawPointer) -> Void
  ) {
    let cTensor = TF_AllocateTensor(
      Scalar.tensorFlowDataType._cDataType,
      shape.map(Int64.init),
      Int32(shape.count),
      byteCount
    )!
    assert(TF_TensorByteSize(cTensor) == byteCount)
    bufferInitializer(TF_TensorData(cTensor))
    self.init(copyingFromCTensor: cTensor)
    TF_DeleteTensor(cTensor)
  }

  /// Return true if the underlying tensor is concrete (as opposed to being
  /// symbolic).
  public var isConcrete: Bool {
    return TFE_TensorHandleIsConcrete(_cTensorHandle) != 0
  }
}

extension TensorHandle where Scalar : TensorFlowScalar {
  /// Create a `TensorHandle` with a closure that initializes the underlying
  /// buffer.
  ///
  /// `scalarsInitializer` receives a buffer with exactly enough capacity to
  /// hold the scalars in a tensor with shape `shape`. `scalarsInitializer`
  /// must initialize the entire buffer, with contiguous scalars in row-major
  /// order.
  @usableFromInline
  convenience init(
    shape: [Int],
    scalarsInitializer: (UnsafeMutablePointer<Scalar>) -> Void
  ) {
    let contiguousSize = shape.reduce(1, *)
    let byteCount = contiguousSize * MemoryLayout<Scalar>.stride
    self.init(shape: shape, byteCount: byteCount) { buffer in
      scalarsInitializer(buffer.bindMemory(to: Scalar.self,
                                           capacity: contiguousSize))
    }
  }
}

internal extension TensorHandle {
  /// Create a `ShapedArray` with contents of the underlying `TensorHandle`. If
  /// the `TensorHandle` is on the accelerator, it will be copied to the host.
  /// - Returns: A `ShapedArray`.
  @usableFromInline
  @inline(never)
  func makeHostCopy() -> ShapedArray<Scalar> {
    internalConsistencyCheck(isConcrete)
    debugLog("Calling makeHostCopy() with c handle \(_cTensorHandle)")
    return ShapedArray(cTensorHandle: _cTensorHandle)
  }
}

internal extension ShapedArray where Scalar : _TensorFlowDataTypeCompatible {
  @usableFromInline
  @inline(never)
  init(cTensorHandle: CTensorHandle) {
    internalConsistencyCheck(TFE_TensorHandleIsConcrete(cTensorHandle) != 0)
    let status = TF_NewStatus()
    let cTensor = TFE_TensorHandleResolve(cTensorHandle, status)
    checkOk(status)
    TF_DeleteStatus(status)
    internalConsistencyCheck(cTensor != nil)
    debugLog("# of dims is \(TF_NumDims(cTensor!))")
    debugLog("Returning a shaped array.")
    self.init(owning: cTensor!)
  }
}

/// `ResourceHandle` is the type used by ops to represent TensorFlow "resource" 
/// values.
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

/// `VariantHandle` is the type used by ops to represent TensorFlow "variant"
/// values.
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
