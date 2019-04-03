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
}
