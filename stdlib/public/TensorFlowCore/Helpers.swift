//===-- Helpers.swift ----------------------------------------*- swift -*-===//
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

import CTensorFlow

//===----------------------------------------------------------------------===//
// Tensor
//===----------------------------------------------------------------------===//

@inlinable @inline(never)
@_silgen_name("__tf_to_accel")
public func _TFToAccelerator<Scalar>(_ handle: TensorHandle<Scalar>) -> TensorHandle<Scalar> {
  return handle
}

@inlinable @inline(never)
@_silgen_name("__tf_to_host")
public func _TFToHost<Scalar>(_ handle: TensorHandle<Scalar>)
  -> TensorHandle<Scalar> {
  return handle
}

/// This function converts a `TensorHandle` that is known to have a 0-d value
/// into the scalar that it produces. This is intended for use in op definitions
/// where it is known that the op always returns a 0-d tensor. It is not for use
/// in general code.
@inlinable @inline(__always)
public func _TFGetScalarOrDie<Scalar : TensorFlowScalar>(
  _ handle: TensorHandle<Scalar>
) -> Scalar {
  return Scalar._getScalarOrDie(handle)
}

/// This function converts a `TensorHandle` into a scalar if it is 0-d, or
/// returns nil otherwise.
@inlinable @inline(__always)
public func _TFGetScalar<Scalar : TensorFlowScalar>(
  _ handle: TensorHandle<Scalar>
) -> Scalar? {
  return Scalar._getScalar(handle)
}

/// This compiler builtin is known by the partitioning pass, which recognizes it
/// and promotes calls to it to being in graph when it can. This signature was
/// designed to align with the requirements of the `Const` TensorFlow operation.
@inlinable @inline(never)
@_silgen_name("__tf_tensor_from_scalars")
public func _TFTensorFromScalars<Scalar : TensorFlowScalar>(
  _ scalars: [Scalar], shape: [Int]
) -> TensorHandle<Scalar> {
  let contiguousSize = shape.reduce(1, *)
  precondition(scalars.count == contiguousSize,
               "The number of scalars does not match the shape.")
  return TensorHandle(
    shape: shape,
    scalarsInitializer: { addr in
      scalars.withUnsafeBufferPointer { ptr in
        addr.assign(from: ptr.baseAddress!, count: contiguousSize)
      }
    }
  )
}

/// In graph mode, the deabstraction pass transforms this function call to
/// either a "Const" graph_op (if `scalar` is a compile-time constant), or a
/// "tfc.scalarToTensor" graph_op. In the latter case, the partition pass uses
/// it to do scalar promotion, and transforms it away before entering graph
/// lowering. e.g. For user code:
///   let x_scalar = x_tensor.mean()
///   let y_scalar = y_tensor.mean()
///   let z_scalar = x_scalar + y_scalar
///   let z_tensor = Tensor(z_scalar)
///
/// The scalar addition can be promoted into graph, through the
/// "tfc.scalarToTensor" graph_op generated from Tensor(z_scalar). In this
/// example, the _getScalarOrDie() call generated from mean() will be "cancelled
/// out" with "tfc.scalarToTensor", such that we avoid generating scalar on the
/// host, and then converting it back to a graph tensor.
///
/// In eager mode, this function is executed directly.
@inlinable @inline(never)
@_silgen_name("__tf_tensor_from_scalar")
public func _TFTensorFromScalar<Scalar : TensorFlowScalar>(
  _ scalar: Scalar
) -> TensorHandle<Scalar> {
  return _TFTensorFromScalars([scalar], shape: [])
}

@inlinable @inline(never)
@_silgen_name("__tf_tensor_from_scalars_1d")
public func _TFTensorFromScalars1D<Scalar : TensorFlowScalar>(_ scalars: [Scalar])
  -> TensorHandle<Scalar> {
  return _TFTensorFromScalars(scalars, shape: [scalars.count])
}

@inlinable @inline(__always)
public func _TFHoistable<Scalar>(_ fn: () -> TensorHandle<Scalar>)
  -> TensorHandle<Scalar> {
  return Scalar._hoistableClosure(fn)
}

//===----------------------------------------------------------------------===//
// StringTensor
//===----------------------------------------------------------------------===//

/// This compiler builtin is known by the partitioning pass, which recognizes it
/// and promotes calls to it to being in graph when it can. This signature was
/// designed to align with the requirements of the `Const` TensorFlow operation.
@inlinable @inline(never)
@_silgen_name("__tf_string_tensor_from_strings")
public func _TFStringTensorFromStrings(
  _ scalars: [String], shape: [Int]
) -> TensorHandle<String> {
  let contiguousSize = shape.reduce(1, *)
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
}

@inlinable @inline(never)
@_silgen_name("__tf_string_tensor_from_string")
public func _TFStringTensorFromString(_ scalar: String) -> TensorHandle<String> {
  return _TFStringTensorFromStrings([scalar], shape: [])
}

@inlinable @inline(never)
@_silgen_name("__tf_string_tensor_from_strings_1d")
public func _TFStringTensorFromStrings1D(_ scalars: [String]) -> TensorHandle<String> {
  return _TFStringTensorFromStrings(scalars, shape: [scalars.count])
}
