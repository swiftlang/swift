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
// This file defines the AccelerableTensorUnit and related helpers.
//
// TODO:
// - Many ops that support int32 and int64 don't support int8 and int16.
//   Consider removing Int8's and Int16's conformance to
//   AccelerableTensorUnit.
//
//===----------------------------------------------------------------------===//

import CTensorFlow

public enum TensorDataType {
  case float
  case double
  case int32
  case uint8
  case int16
  case int8
  case string
  case complex
  case int64
  case bool
  case qint8
  case quint8
  case qint32
  case bfloat16
  case qint16
  case quint16
  case uint16
  case complex128
  case half
  case resource
  case variant
  case uint32
  case uint64
}

extension TensorDataType {
  @_versioned
  var cDataType: TF_DataType {
    switch self {
    case .float: return TF_FLOAT
    case .double: return TF_DOUBLE
    case .int32: return TF_INT32
    case .uint8: return TF_UINT8
    case .int16: return TF_INT16
    case .int8: return TF_INT8
    case .string: return TF_STRING
    case .complex: return TF_COMPLEX
    case .int64: return TF_INT64
    case .bool: return TF_BOOL
    case .qint8: return TF_QINT8
    case .quint8: return TF_QUINT8
    case .qint32: return TF_QINT32
    case .bfloat16: return TF_BFLOAT16
    case .qint16: return TF_QINT16
    case .quint16: return TF_QUINT16
    case .uint16: return TF_UINT16
    case .complex128: return TF_COMPLEX128
    case .half: return TF_HALF
    case .resource: return TF_RESOURCE
    case .variant: return TF_VARIANT
    case .uint32: return TF_UINT32
    case .uint64: return TF_UINT64
    }
  }
}

public protocol AccelerableTensorUnit {
  // FIXME: This should be an underscored requirement (because it is not meant
  // to be visible to clients) and should probably be changed to return a
  // TF_DataType.
  static var dataType: TensorDataType { get }

  // Hooks used by the TFPartition pass for primitive operations on tensors.
  // These should not be called directly or implemented.

  /// This converts a TensorHandle that is known to have a 0d value into
  /// the scalar that it produces.  Users should call the _TFGetScalarOrDie
  /// wrapper function.
  static func _getScalarOrDie(_ handle: TensorHandle<Self>) -> Self
  // TODO: Move _TFMakeScalarTensor and _TFScalarize to this model.
}

// This is the implementation of the _getScalarOrDie requirement for each
// concrete type below.  We use this round-about approach to implement the
// global _TFGetScalarOrDie function in order to ensure that the noinline
// SIL functions below have non-generic type signatures.  This is important for
// the inner workings of the partitioning pass.
private func _TFGetScalarOrDieImpl<Unit>(_ handle: TensorHandle<Unit>) -> Unit {
  return handle.makeHostCopy().scalar!
}


internal extension AccelerableTensorUnit {
  @_versioned
  static var cDataType: TF_DataType {
    return dataType.cDataType
  }
}

extension Bool : AccelerableTensorUnit {
  public static var dataType: TensorDataType {
    return .bool
  }

  @_silgen_name("__tf_get_scalar_or_die_Bool") @inline(never)
  public static func _getScalarOrDie(_ handle: TensorHandle<Bool>) -> Bool {
    return _TFGetScalarOrDieImpl(handle)
  }
}

extension Int8 : AccelerableTensorUnit {
  public static var dataType: TensorDataType {
    return .int8
  }
  @_silgen_name("__tf_get_scalar_or_die_Int8") @inline(never)
  public static func _getScalarOrDie(_ handle: TensorHandle<Int8>) -> Int8 {
    return _TFGetScalarOrDieImpl(handle)
  }
}

extension UInt8 : AccelerableTensorUnit {
  public static var dataType: TensorDataType {
    return .uint8
  }
  @_silgen_name("__tf_get_scalar_or_die_UInt8") @inline(never)
  public static func _getScalarOrDie(_ handle: TensorHandle<UInt8>) -> UInt8 {
    return _TFGetScalarOrDieImpl(handle)
  }
}

extension Int16 : AccelerableTensorUnit {
  public static var dataType: TensorDataType {
    return .int16
  }
  @_silgen_name("__tf_get_scalar_or_die_Int16") @inline(never)
  public static func _getScalarOrDie(_ handle: TensorHandle<Int16>) -> Int16 {
    return _TFGetScalarOrDieImpl(handle)
  }
}

extension UInt16 : AccelerableTensorUnit {
  public static var dataType: TensorDataType {
    return .uint16
  }
  @_silgen_name("__tf_get_scalar_or_die_UInt16") @inline(never)
  public static func _getScalarOrDie(_ handle: TensorHandle<UInt16>) -> UInt16 {
    return _TFGetScalarOrDieImpl(handle)
  }
}

extension Int32 : AccelerableTensorUnit {
  public static var dataType: TensorDataType {
    return .int32
  }
  @_silgen_name("__tf_get_scalar_or_die_Int32") @inline(never)
  public static func _getScalarOrDie(_ handle: TensorHandle<Int32>) -> Int32 {
    return _TFGetScalarOrDieImpl(handle)
  }
}

extension UInt32 : AccelerableTensorUnit {
  public static var dataType: TensorDataType {
    return .uint32
  }
  @_silgen_name("__tf_get_scalar_or_die_UInt32") @inline(never)
  public static func _getScalarOrDie(_ handle: TensorHandle<UInt32>) -> UInt32 {
    return _TFGetScalarOrDieImpl(handle)
  }
}

extension Int64 : AccelerableTensorUnit {
  public static var dataType: TensorDataType {
    return .int64
  }
  @_silgen_name("__tf_get_scalar_or_die_Int64") @inline(never)
  public static func _getScalarOrDie(_ handle: TensorHandle<Int64>) -> Int64 {
    return _TFGetScalarOrDieImpl(handle)
  }
}

extension UInt64 : AccelerableTensorUnit {
  public static var dataType: TensorDataType {
    return .uint64
  }
  @_silgen_name("__tf_get_scalar_or_die_UInt64") @inline(never)
  public static func _getScalarOrDie(_ handle: TensorHandle<UInt64>) -> UInt64 {
    return _TFGetScalarOrDieImpl(handle)
  }
}

extension Int : AccelerableTensorUnit {
  public static var dataType: TensorDataType {
    return .int64
  }
  @_silgen_name("__tf_get_scalar_or_die_Int") @inline(never)
  public static func _getScalarOrDie(_ handle: TensorHandle<Int>) -> Int {
    return _TFGetScalarOrDieImpl(handle)
  }
}

extension UInt : AccelerableTensorUnit {
  public static var dataType: TensorDataType {
    return .uint64
  }
  @_silgen_name("__tf_get_scalar_or_die_UInt") @inline(never)
  public static func _getScalarOrDie(_ handle: TensorHandle<UInt>) -> UInt {
    return _TFGetScalarOrDieImpl(handle)
  }
}

extension Float : AccelerableTensorUnit {
  public static var dataType: TensorDataType {
    return .float
  }
  @_silgen_name("__tf_get_scalar_or_die_Float") @inline(never)
  public static func _getScalarOrDie(_ handle: TensorHandle<Float>) -> Float {
    return _TFGetScalarOrDieImpl(handle)
  }
}

extension Double : AccelerableTensorUnit {
  public static var dataType: TensorDataType {
    return .double
  }
  @_silgen_name("__tf_get_scalar_or_die_Double") @inline(never)
  public static func _getScalarOrDie(_ handle: TensorHandle<Double>) -> Double {
    return _TFGetScalarOrDieImpl(handle)
  }
}
