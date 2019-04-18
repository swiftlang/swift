//===-- ArrayOps.swift ----------------------------------------*- swift -*-===//
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
// This file contains some Array ops that cannot be properly handled by #tfop.
//
// TODO: These should be deleted once we can properly generate raw ops for these.
//
//===----------------------------------------------------------------------===//

import CTensorFlow

public extension Raw {
  /// Saves tensors in V2 checkpoint format.
  ///
  /// By default, saves the named tensors in full.  If the caller wishes to save
  /// specific slices of full tensors, "shape_and_slices" should be non-empty strings
  /// and correspondingly well-formed.
  ///
  /// - Parameters:
  ///   - prefix: Must have a single element. The prefix of the V2 checkpoint to which we
  ///     write the tensors.
  ///   - tensor_names: shape {N}. The names of the tensors to be saved.
  ///   - shape_and_slices: shape {N}.  The slice specs of the tensors to be saved.
  ///     Empty strings indicate that they are non-partitioned tensors.
  ///   - tensors: `N` tensors to save.
  @inlinable @inline(__always)
  static func saveV2(
    prefix: StringTensor,
    tensorNames: StringTensor,
    shapeAndSlices: StringTensor,
    tensors: [AnyTensor]
  ) {
    let s: CTFStatus = TF_NewStatus()
    defer { TF_DeleteStatus(s) }
    let op: CTFEOp = TFE_NewOp(_ExecutionContext.global.eagerContext, "SaveV2", s)
    defer { TFE_DeleteOp(op) }
    let _ = _TFCOpAddInputFromTensorGroup(op, prefix, s)
    let _ = _TFCOpAddInputFromTensorGroup(op, tensorNames, s)
    let _ = _TFCOpAddInputFromTensorGroup(op, shapeAndSlices, s)
    let _ = _TFCOpAddInputFromAnyTensors(op, tensors, s)
    let _ = _TFCOpSetAttrTypeArray(op, "dtypes", tensors.map { $0._tensorFlowDataType })
    return _TFCExecuteOp(op, s)
  }

  /// Restores tensors from a V2 checkpoint.
  ///
  /// For backward compatibility with the V1 format, this Op currently allows
  /// restoring from a V1 checkpoint as well:
  ///   - This Op first attempts to find the V2 index file pointed to by "prefix", and
  ///     if found proceed to read it as a V2 checkpoint;
  ///   - Otherwise the V1 read path is invoked.
  /// Relying on this behavior is not recommended, as the ability to fall back to read
  /// V1 might be deprecated and eventually removed.
  ///
  /// By default, restores the named tensors in full.  If the caller wishes to restore
  /// specific slices of stored tensors, "shape_and_slices" should be non-empty
  /// strings and correspondingly well-formed.
  ///
  /// Callers must ensure all the named tensors are indeed stored in the checkpoint.
  ///
  /// - Parameters:
  ///   - prefix: Must have a single element.  The prefix of a V2 checkpoint.
  ///   - tensor_names: shape {N}.  The names of the tensors to be restored.
  ///   - shape_and_slices: shape {N}.  The slice specs of the tensors to be restored.
  ///     Empty strings indicate that they are non-partitioned tensors.
  ///
  /// - Attr dtypes: shape {N}.  The list of expected dtype for the tensors.  Must match
  ///   those stored in the checkpoint.
  ///
  /// - Output tensors: shape {N}.  The restored tensors, whose shapes are read from the
  ///   checkpoint directly.
  @inlinable @inline(__always)
  static func restoreV2(
    prefix: StringTensor,
    tensorNames: StringTensor,
    shapeAndSlices: StringTensor,
    dtypes: [TensorDataType]
  ) -> [AnyTensor] {
    let s: CTFStatus = TF_NewStatus()
    defer { TF_DeleteStatus(s) }
    let op: CTFEOp = TFE_NewOp(_ExecutionContext.global.eagerContext, "RestoreV2", s)
    defer { TFE_DeleteOp(op) }
    let _ = _TFCOpAddInputFromTensorGroup(op, prefix, s)
    let _ = _TFCOpAddInputFromTensorGroup(op, tensorNames, s)
    let _ = _TFCOpAddInputFromTensorGroup(op, shapeAndSlices, s)
    let _ = _TFCOpSetAttrTypeArray(op, "dtypes", dtypes)

    var count: Int32 = Int32(dtypes.count)
    let buffer: UnsafeMutablePointer<CTensorHandle> =
      UnsafeMutablePointer.allocate(capacity: Int(count))
    defer { buffer.deallocate() }
    _TFCEagerExecute(op, UnsafeMutablePointer<CTensorHandle?>(buffer), &count, s)
    checkOk(s)

    var out: [AnyTensor] = []
    var cursor = buffer
    for type in dtypes {
      out.append(makeTensor(dataType: type, owning: cursor.pointee))
      cursor = cursor.advanced(by: 1)
    }
    return out
  }

  /// Splits a tensor into `numSplit` tensors along one dimension.
  ///
  /// - Parameters:
  ///   - splitDim: 0-D.  The dimension along which to split.  Must be in the range
  ///     `[-rank(value), rank(value))`.
  ///   - value: The tensor to split.
  ///   - numSplit: The number of splits to create.
  ///
  /// - Returns: Tensors whose shape matches that of `value`
  ///   except along `axis`, where their sizes are
  ///   `value.shape[axis] / numSplit`.
  @inlinable @inline(__always)
  static func split<T: TensorFlowScalar>(
    splitDim: Tensor<Int32>,
    value: Tensor<T>,
    numSplit: Int64
  ) -> [Tensor<T>] {
    let s: CTFStatus = TF_NewStatus()
    defer { TF_DeleteStatus(s) }
    let op: CTFEOp = TFE_NewOp(_ExecutionContext.global.eagerContext, "Split", s)
    defer { TFE_DeleteOp(op) }
    let _ = _TFCOpAddInputFromTensorGroup(op, splitDim, s)
    let _ = _TFCOpAddInputFromTensorGroup(op, value, s)
    TFE_OpSetAttrInt(op, "num_split", numSplit)
    TFE_OpSetAttrType(op, "T", T.tensorFlowDataType._cDataType)
    var count: Int32 = Int32(numSplit)
    let buffer: UnsafeMutablePointer<CTensorHandle> =
      UnsafeMutablePointer.allocate(capacity: Int(count))
    defer { buffer.deallocate() }
    _TFCEagerExecute(op, UnsafeMutablePointer<CTensorHandle?>(buffer), &count, s)
    checkOk(s)

    var out: [Tensor<T>] = []
    var cursor = buffer
    for _ in 0..<numSplit {
      out.append(Tensor<T>(handle: TensorHandle(_owning: cursor.pointee)))
      cursor = cursor.advanced(by: 1)
    }
    return out
  }

  /// Splits a tensor into `numSplit` tensors along one dimension.
  ///
  /// - Parameters:
  ///   - value: The tensor to split.
  ///   - sizeSplits: list containing the sizes of each output tensor along the split
  ///     dimension. Must sum to the dimension of value along split_dim.
  ///     Can contain one -1 indicating that dimension is to be inferred.
  ///   - splitDim: 0-D.  The dimension along which to split.  Must be in the range
  ///     `[-rank(value), rank(value))`.
  ///
  /// - Returns: Tensors whose shape matches that of `value`
  ///   except along `axis`, where their sizes are
  ///   `size_splits[i]`.
  @inlinable @inline(__always)
  static func splitV<T: TensorFlowScalar, Tlen: BinaryInteger & TensorFlowScalar>(
    value: Tensor<T>,
    sizeSplits: Tensor<Tlen>,
    splitDim: Tensor<Int32>,
    numSplit: Int64
  ) -> [Tensor<T>] {
    let s: CTFStatus = TF_NewStatus()
    defer { TF_DeleteStatus(s) }
    let op: CTFEOp = TFE_NewOp(_ExecutionContext.global.eagerContext, "SplitV", s)
    defer { TFE_DeleteOp(op) }
    let _ = _TFCOpAddInputFromTensorGroup(op, value, s)
    let _ = _TFCOpAddInputFromTensorGroup(op, sizeSplits, s)
    let _ = _TFCOpAddInputFromTensorGroup(op, splitDim, s)
    TFE_OpSetAttrInt(op, "num_split", numSplit)
    TFE_OpSetAttrType(op, "T", T.tensorFlowDataType._cDataType)
    TFE_OpSetAttrType(op, "Tlen", Tlen.tensorFlowDataType._cDataType)
    var count: Int32 = Int32(numSplit)
    let buffer: UnsafeMutablePointer<CTensorHandle> =
      UnsafeMutablePointer.allocate(capacity: Int(count))
    defer { buffer.deallocate() }
    _TFCEagerExecute(op, UnsafeMutablePointer<CTensorHandle?>(buffer), &count, s)
    checkOk(s)

    var out: [Tensor<T>] = []
    var cursor = buffer
    for _ in 0..<numSplit {
      out.append(Tensor<T>(handle: TensorHandle(_owning: cursor.pointee)))
      cursor = cursor.advanced(by: 1)
    }
    return out
  }
}
