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

/// `StringTensor` is a multi-dimensional array whose elements are `String`s.
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
// Initialization
//===----------------------------------------------------------------------===//

public extension StringTensor {
  @inlinable
  init(shape: TensorShape, scalars: [String]) {
    let contiguousSize = shape.contiguousSize
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

    let handle = TensorHandle<String>(
      shape: shape.dimensions,
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
          _ = cString.withUnsafeBufferPointer { buffer in
            TF_StringEncode(buffer.baseAddress, buffer.count, dataAddr,
                            tfEncodedSize, status)
          }
          checkOk(status)
          dataAddr = dataAddr.advanced(by: tfEncodedSize)
        }
        TF_DeleteStatus(status)
      }
    )
    self.init(handle: handle)
  }

  /// Creates a 0-D `StringTensor` from a scalar value.
  @inlinable
  init(_ value: String) {
    self.init(shape: [], scalars: [value])
  }

  /// Creates a 1-D `StringTensor` in from contiguous scalars.
  @inlinable
  init(_ scalars: [String]) {
    self.init(shape: [scalars.count], scalars: scalars)
  }
}

//===----------------------------------------------------------------------===//
// Array conversion
//===----------------------------------------------------------------------===//

public extension StringTensor {
  var array: ShapedArray<String> {
    debugLog("Returning a host copy of string array.")
    return handle.makeHostCopy()
  }

  var scalars: [String] {
    return array.scalars
  }
}
