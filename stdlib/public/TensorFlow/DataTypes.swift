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
  static var dataType: TensorDataType { get }
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
}

extension Int8 : AccelerableTensorUnit {
  public static var dataType: TensorDataType {
    return .int8
  }
}

extension UInt8 : AccelerableTensorUnit {
  public static var dataType: TensorDataType {
    return .uint8
  }
}

extension Int16 : AccelerableTensorUnit {
  public static var dataType: TensorDataType {
    return .int16
  }
}

extension UInt16 : AccelerableTensorUnit {
  public static var dataType: TensorDataType {
    return .uint16
  }
}

extension Int32 : AccelerableTensorUnit {
  public static var dataType: TensorDataType {
    return .int32
  }
}

extension UInt32 : AccelerableTensorUnit {
  public static var dataType: TensorDataType {
    return .uint32
  }
}

extension Int64 : AccelerableTensorUnit {
  public static var dataType: TensorDataType {
    return .int64
  }
}

extension UInt64 : AccelerableTensorUnit {
  public static var dataType: TensorDataType {
    return .uint64
  }
}

extension Int : AccelerableTensorUnit {
  public static var dataType: TensorDataType {
    return .int64
  }
}

extension UInt : AccelerableTensorUnit {
  public static var dataType: TensorDataType {
    return .uint64
  }
}

extension Float : AccelerableTensorUnit {
  public static var dataType: TensorDataType {
    return .float
  }
}

extension Double : AccelerableTensorUnit {
  public static var dataType: TensorDataType {
    return .double
  }
}
