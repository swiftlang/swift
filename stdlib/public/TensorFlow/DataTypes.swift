//===-- DataTypes.swift ---------------------------------------*- swift -*-===//
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
// This file defines the TensorFlowScalar and related helpers.
//
// TODO:
// - Many ops that support int32 and int64 don't support int8 and int16.
//   Consider removing Int8's and Int16's conformance to
//   TensorFlowScalar.
//
//===----------------------------------------------------------------------===//

import CTensorFlow

/// A TensorFlow dynamic type value. Can be created from types that conform to
/// `TensorFlowScalar`.
// This simply wraps TF_DataTypes, which allows user code to handle TF_DataTypes
// without importing CTensorFlow, which would import a bunch of distracting
// declarations from the TF C API.
@_fixed_layout
public struct TensorDataType {
  @usableFromInline
  internal var cDataType: TF_DataType

  @inlinable
  internal init(_ cDataType: TF_DataType) {
    self.cDataType = cDataType
  }
}

/// A data type compatible with TensorFlow.
public protocol _TensorFlowDataTypeCompatible {
  /// The underlying TensorFlow data type.
  @inlinable
  static var tensorFlowDataType: TensorDataType { get }

  // Hooks used by the TFPartition pass for primitive operations on tensors.
  // These should not be called directly or implemented.

  /// This converts a TensorHandle that is known to have a 0d value into
  /// the scalar that it produces.  Users should call the _TFGetScalarOrDie
  /// wrapper function.
  static func _getScalarOrDie(_ handle: TensorHandle<Self>) -> Self

  /// This converts a TensorHandle into a scalar if it is 0d, or returns nil
  /// otherwise.  Users should call the _TFGetScalar wrapper function.
  static func _getScalar(_ handle: TensorHandle<Self>) -> Self?

  /// This indicates that it is safe to hoist the specified computation that
  /// creates a tensor to being a parameter that is passed in from outside of
  /// the tensor program.
  static func _hoistableClosure(_ fn: () -> TensorHandle<Self>)
    -> TensorHandle<Self>
}

/// A scalar data type compatible with TensorFlow.
///
/// Types that conform to `TensorFlowScalar` can be used as the `Scalar`
/// associated type of `Tensor`.
public protocol TensorFlowScalar : _TensorFlowDataTypeCompatible {}

// This is the implementation of the _getScalarOrDie requirement for each
// concrete type below.  We use this round-about approach to implement the
// global _TFGetScalarOrDie function in order to ensure that the noinline
// SIL functions below have non-generic type signatures.  This is important for
// the inner workings of the partitioning pass.
private func _TFGetScalarOrDieImpl<Scalar>(
  _ handle: TensorHandle<Scalar>
) -> Scalar {
  return handle.makeHostCopy().scalar!
}

// This is the implementation of the _getScalar requirement for each concrete
// type below.  We use this round-about approach to implement the
// global _TFGetScalar function in order to ensure that the noinline
// SIL functions below have non-generic type signatures.  This is important for
// the inner workings of the partitioning pass.
private func _TFGetScalarImpl<Scalar>(
  _ handle: TensorHandle<Scalar>
) -> Scalar? {
  return handle.makeHostCopy().scalar
}

extension Bool : TensorFlowScalar {
  @inlinable
  public static var tensorFlowDataType: TensorDataType {
    return TensorDataType(TF_BOOL)
  }
  @_silgen_name("__tf_get_scalar_or_die_Bool") @inline(never)
  public static func _getScalarOrDie(_ handle: TensorHandle<Bool>) -> Bool {
    return _TFGetScalarOrDieImpl(handle)
  }
  @_silgen_name("__tf_get_scalar_Bool") @inline(never)
  public static func _getScalar(_ handle: TensorHandle<Bool>) -> Bool? {
    return _TFGetScalarImpl(handle)
  }
  @_silgen_name("__tf_hoistable_Bool") @_optimize(none) @inline(never)
  public static func _hoistableClosure(_ fn: () -> TensorHandle<Bool>)
    -> TensorHandle<Bool> {
    return fn()
  }
}

extension Int8 : TensorFlowScalar {
  @inlinable
  public static var tensorFlowDataType: TensorDataType {
    return TensorDataType(TF_INT8)
  }
  @_silgen_name("__tf_get_scalar_or_die_Int8") @inline(never)
  public static func _getScalarOrDie(_ handle: TensorHandle<Int8>) -> Int8 {
    return _TFGetScalarOrDieImpl(handle)
  }
  @_silgen_name("__tf_get_scalar_Int8") @inline(never)
  public static func _getScalar(_ handle: TensorHandle<Int8>) -> Int8? {
    return _TFGetScalarImpl(handle)
  }
  @_silgen_name("__tf_hoistable_Int8") @_optimize(none) @inline(never)
  public static func _hoistableClosure(_ fn: () -> TensorHandle<Int8>)
    -> TensorHandle<Int8> {
    return fn()
  }
}

extension UInt8 : TensorFlowScalar {
  @inlinable
  public static var tensorFlowDataType: TensorDataType {
    return TensorDataType(TF_UINT8)
  }
  @_silgen_name("__tf_get_scalar_or_die_UInt8") @inline(never)
  public static func _getScalarOrDie(_ handle: TensorHandle<UInt8>) -> UInt8 {
    return _TFGetScalarOrDieImpl(handle)
  }
  @_silgen_name("__tf_get_scalar_UInt8") @inline(never)
  public static func _getScalar(_ handle: TensorHandle<UInt8>) -> UInt8? {
    return _TFGetScalarImpl(handle)
  }
  @_silgen_name("__tf_hoistable_UInt8") @_optimize(none) @inline(never)
  public static func _hoistableClosure(_ fn: () -> TensorHandle<UInt8>)
    -> TensorHandle<UInt8> {
    return fn()
  }
}

extension Int16 : TensorFlowScalar {
  @inlinable
  public static var tensorFlowDataType: TensorDataType {
    return TensorDataType(TF_INT16)
  }
  @_silgen_name("__tf_get_scalar_or_die_Int16") @inline(never)
  public static func _getScalarOrDie(_ handle: TensorHandle<Int16>) -> Int16 {
    return _TFGetScalarOrDieImpl(handle)
  }
  @_silgen_name("__tf_get_scalar_Int16") @inline(never)
  public static func _getScalar(_ handle: TensorHandle<Int16>) -> Int16? {
    return _TFGetScalarImpl(handle)
  }
  @_silgen_name("__tf_hoistable_Int16") @_optimize(none) @inline(never)
  public static func _hoistableClosure(_ fn: () -> TensorHandle<Int16>)
    -> TensorHandle<Int16> {
    return fn()
  }
}

extension UInt16 : TensorFlowScalar {
  @inlinable
  public static var tensorFlowDataType: TensorDataType {
    return TensorDataType(TF_UINT16)
  }
  @_silgen_name("__tf_get_scalar_or_die_UInt16") @inline(never)
  public static func _getScalarOrDie(_ handle: TensorHandle<UInt16>) -> UInt16 {
    return _TFGetScalarOrDieImpl(handle)
  }
  @_silgen_name("__tf_get_scalar_UInt16") @inline(never)
  public static func _getScalar(_ handle: TensorHandle<UInt16>) -> UInt16? {
    return _TFGetScalarImpl(handle)
  }
  @_silgen_name("__tf_hoistable_UInt16") @_optimize(none) @inline(never)
  public static func _hoistableClosure(_ fn: () -> TensorHandle<UInt16>)
    -> TensorHandle<UInt16> {
    return fn()
  }
}

extension Int32 : TensorFlowScalar {
  @inlinable
  public static var tensorFlowDataType: TensorDataType {
    return TensorDataType(TF_INT32)
  }
  @_silgen_name("__tf_get_scalar_or_die_Int32") @inline(never)
  public static func _getScalarOrDie(_ handle: TensorHandle<Int32>) -> Int32 {
    return _TFGetScalarOrDieImpl(handle)
  }
  @_silgen_name("__tf_get_scalar_Int32") @inline(never)
  public static func _getScalar(_ handle: TensorHandle<Int32>) -> Int32? {
    return _TFGetScalarImpl(handle)
  }
  @_silgen_name("__tf_hoistable_Int32") @_optimize(none) @inline(never)
  public static func _hoistableClosure(_ fn: () -> TensorHandle<Int32>)
    -> TensorHandle<Int32> {
    return fn()
  }
}

extension UInt32 : TensorFlowScalar {
  @inlinable
  public static var tensorFlowDataType: TensorDataType {
    return TensorDataType(TF_UINT32)
  }
  @_silgen_name("__tf_get_scalar_or_die_UInt32") @inline(never)
  public static func _getScalarOrDie(_ handle: TensorHandle<UInt32>) -> UInt32 {
    return _TFGetScalarOrDieImpl(handle)
  }
  @_silgen_name("__tf_get_scalar_UInt32") @inline(never)
  public static func _getScalar(_ handle: TensorHandle<UInt32>) -> UInt32? {
    return _TFGetScalarImpl(handle)
  }
  @_silgen_name("__tf_hoistable_UInt32") @_optimize(none) @inline(never)
  public static func _hoistableClosure(_ fn: () -> TensorHandle<UInt32>)
    -> TensorHandle<UInt32> {
    return fn()
  }
}

extension Int64 : TensorFlowScalar {
  @inlinable
  public static var tensorFlowDataType: TensorDataType {
    return TensorDataType(TF_INT64)
  }
  @_silgen_name("__tf_get_scalar_or_die_Int64") @inline(never)
  public static func _getScalarOrDie(_ handle: TensorHandle<Int64>) -> Int64 {
    return _TFGetScalarOrDieImpl(handle)
  }
  @_silgen_name("__tf_get_scalar_Int64") @inline(never)
  public static func _getScalar(_ handle: TensorHandle<Int64>) -> Int64? {
    return _TFGetScalarImpl(handle)
  }
  @_silgen_name("__tf_hoistable_Int64") @_optimize(none) @inline(never)
  public static func _hoistableClosure(_ fn: () -> TensorHandle<Int64>)
    -> TensorHandle<Int64> {
    return fn()
  }
}

extension UInt64 : TensorFlowScalar {
  @inlinable
  public static var tensorFlowDataType: TensorDataType {
    return TensorDataType(TF_UINT64)
  }
  @_silgen_name("__tf_get_scalar_or_die_UInt64") @inline(never)
  public static func _getScalarOrDie(_ handle: TensorHandle<UInt64>) -> UInt64 {
    return _TFGetScalarOrDieImpl(handle)
  }
  @_silgen_name("__tf_get_scalar_UInt64") @inline(never)
  public static func _getScalar(_ handle: TensorHandle<UInt64>) -> UInt64? {
    return _TFGetScalarImpl(handle)
  }
  @_silgen_name("__tf_hoistable_UInt64") @_optimize(none) @inline(never)
  public static func _hoistableClosure(_ fn: () -> TensorHandle<UInt64>)
    -> TensorHandle<UInt64> {
    return fn()
  }
}

@_fixed_layout
public struct BFloat16 {
  @usableFromInline var data: Int16 = 0
  private init() {}
}

extension BFloat16 : TensorFlowScalar {
  @inlinable
  public static var tensorFlowDataType: TensorDataType {
    return TensorDataType(TF_BFLOAT16)
  }
  @_silgen_name("__tf_get_scalar_or_die_BFloat16") @inline(never)
  public static func _getScalarOrDie
    (_ handle: TensorHandle<BFloat16>
  ) -> BFloat16 {
    return _TFGetScalarOrDieImpl(handle)
  }
  @_silgen_name("__tf_get_scalar_BFloat16") @inline(never)
  public static func _getScalar(_ handle: TensorHandle<BFloat16>) -> BFloat16? {
    return _TFGetScalarImpl(handle)
  }
  @_silgen_name("__tf_hoistable_BFloat16") @_optimize(none) @inline(never)
  public static func _hoistableClosure(
    _ fn: () -> TensorHandle<BFloat16>
  ) -> TensorHandle<BFloat16> {
    return fn()
  }
}

extension Float : TensorFlowScalar {
  @inlinable
  public static var tensorFlowDataType: TensorDataType {
    return TensorDataType(TF_FLOAT)
  }
  @_silgen_name("__tf_get_scalar_or_die_Float") @inline(never)
  public static func _getScalarOrDie(_ handle: TensorHandle<Float>) -> Float {
    return _TFGetScalarOrDieImpl(handle)
  }
  @_silgen_name("__tf_get_scalar_Float") @inline(never)
  public static func _getScalar(_ handle: TensorHandle<Float>) -> Float? {
    return _TFGetScalarImpl(handle)
  }
  @_silgen_name("__tf_hoistable_Float") @_optimize(none) @inline(never)
  public static func _hoistableClosure(_ fn: () -> TensorHandle<Float>)
    -> TensorHandle<Float> {
    return fn()
  }
}

extension Double : TensorFlowScalar {
  @inlinable
  public static var tensorFlowDataType: TensorDataType {
    return TensorDataType(TF_DOUBLE)
  }
  @_silgen_name("__tf_get_scalar_or_die_Double") @inline(never)
  public static func _getScalarOrDie(_ handle: TensorHandle<Double>) -> Double {
    return _TFGetScalarOrDieImpl(handle)
  }
  @_silgen_name("__tf_get_scalar_Double") @inline(never)
  public static func _getScalar(_ handle: TensorHandle<Double>) -> Double? {
    return _TFGetScalarImpl(handle)
  }
  @_silgen_name("__tf_hoistable_Double") @_optimize(none) @inline(never)
  public static func _hoistableClosure(_ fn: () -> TensorHandle<Double>)
    -> TensorHandle<Double> {
    return fn()
  }
}

extension String : _TensorFlowDataTypeCompatible {
  @inlinable
  public static var tensorFlowDataType: TensorDataType {
    return TensorDataType(TF_STRING)
  }
  @_silgen_name("__tf_get_scalar_or_die_String") @inline(never)
  public static func _getScalarOrDie(_ handle: TensorHandle<String>) -> String {
    return _TFGetScalarOrDieImpl(handle)
  }
  @_silgen_name("__tf_get_scalar_String") @inline(never)
  public static func _getScalar(_ handle: TensorHandle<String>) -> String? {
    return _TFGetScalarImpl(handle)
  }
  @_silgen_name("__tf_hoistable_String") @_optimize(none) @inline(never)
  public static func _hoistableClosure(_ fn: () -> TensorHandle<String>)
    -> TensorHandle<String> {
    return fn()
  }
}
