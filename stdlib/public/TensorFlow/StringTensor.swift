//===-- StringTensor.swift -----------------------------------*- swift -*-===//
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
// Defines the StringTensor type.
//
//===----------------------------------------------------------------------===//

import CTensorFlow

//===----------------------------------------------------------------------===//
// StringTensor
//===----------------------------------------------------------------------===//

/// `StringTensor` is a multi-dimensional array whose elements are Strings.
@_fixed_layout
public struct StringTensor {
  /// The underlying `TensorHandle`.
  /// - Note: `handle` is public to allow user defined ops, but should not
  /// normally be used otherwise.
  public let handle: TensorHandle<String>

  @inlinable
  public init(handle: TensorHandle<String>) {
    self.handle = handle
  }
}

//===----------------------------------------------------------------------===//
// Compiler intrinsics
//===----------------------------------------------------------------------===//

/// This compiler builtin is known by the partitioning pass, which recognizes it
/// and promotes calls to it to being in graph when it can. This signature was
/// designed to align with the requirements of the `Const` TensorFlow operation.
@usableFromInline @inline(never)
@_silgen_name("__tf_string_tensor_from_scalars")
func _TFStringTensorFromScalars(
  _ scalars: [String], shape: [Int32]
) -> TensorHandle<String> {
  let contiguousSize = shape.map(Int.init).reduce(1, *)
  precondition(scalars.count == contiguousSize,
               "The number of scalars does not match the shape.")

  // utf8CString is null-terminated. TF APIs want the strings without
  // null-terminators.
  let cStrings = scalars.map { $0.utf8CString.dropLast() }

  let tfEncodedSizes = cStrings.map { TF_StringEncodedSize($0.count) }

  // Format information copied from tensorflow/c/c_api.h:
  // The format for TF_STRING tensors is:
  //   start_offset: array[uint64]
  //   data:         byte[...]
  //
  //   The string length (as a varint), followed by the contents of the string
  //   is encoded at data[start_offset[i]]].

  // The size of the "start_offset" region.
  let startOffsetsByteCount = scalars.count * MemoryLayout<UInt64>.stride

  // The size of the "data" region.
  let dataByteCount = tfEncodedSizes.reduce(0, +) * MemoryLayout<UInt8>.stride

  return TensorHandle(
    shape: shape,
    byteCount: startOffsetsByteCount + dataByteCount,
    bufferInitializer: { tensorBuffer in
      // Initialize the "start_offset" region.
      var startOffset: UInt64 = 0
      var startOffsetAddr =
        tensorBuffer.bindMemory(to: UInt64.self, capacity: scalars.count)
      for tfEncodedSize in tfEncodedSizes {
        startOffsetAddr.initialize(to: startOffset)
        startOffsetAddr = startOffsetAddr.advanced(by: 1)
        startOffset = startOffset + UInt64(tfEncodedSize)
      }

      // Initialize the "data" region.
      var dataAddr = tensorBuffer.advanced(by: startOffsetsByteCount)
        .bindMemory(to: Int8.self, capacity: dataByteCount)
      let status = TF_NewStatus()
      for (cString, tfEncodedSize) in zip(cStrings, tfEncodedSizes) {
        let _ = cString.withUnsafeBufferPointer { buffer in
          TF_StringEncode(buffer.baseAddress, buffer.count, dataAddr,
                          tfEncodedSize, status)
        }
        checkOk(status)
        dataAddr = dataAddr.advanced(by: tfEncodedSize)
      }
      TF_DeleteStatus(status)
    }
  )
}

@usableFromInline @inline(never)
@_silgen_name("__tf_string_tensor_from_scalar")
func _TFStringTensorFromScalar(_ scalar: String) -> TensorHandle<String> {
  return _TFStringTensorFromScalars([scalar], shape: [])
}

@usableFromInline @inline(never)
@_silgen_name("__tf_string_tensor_from_scalars_1d")
func _TFStringTensorFromScalars1D(_ scalars: [String]) -> TensorHandle<String> {
  return _TFStringTensorFromScalars(scalars, shape: [Int32(scalars.count)])
}

//===----------------------------------------------------------------------===//
// Initialization
//===----------------------------------------------------------------------===//

public extension StringTensor {
  /// Creates a tensor from a scalar value.
  @inlinable @inline(__always)
  init(_ value: String) {
    self.init(handle: _TFStringTensorFromScalar(value))
  }

  /// Creates a 1D tensor in from contiguous scalars.
  ///
  /// - Parameters:
  ///   - vector: The scalar contents of the tensor.
  ///
  @inlinable @inline(__always)
  init(_ vector: [String]) {
    self.init(handle: _TFStringTensorFromScalars1D(vector))
  }
}

//===----------------------------------------------------------------------===//
// Array conversion
//===----------------------------------------------------------------------===//

public extension StringTensor {
  @inlinable
  var array: ShapedArray<String> {
    @inline(__always)
    get {
      debugLog("Returning a host copy of string array.")
      return handle.makeHostCopy()
    }
  }

  @inlinable
  var scalars: [String] {
    return array.scalars
  }
}
