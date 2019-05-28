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

public extension TensorDataType {
  public var _cDataType: TF_DataType {
    return TF_DataType(rawValue: _internalStorageType)
  }

  public init(_ cDataType: TF_DataType) {
    self.init(rawValue: cDataType.rawValue)
  }
}

@usableFromInline
internal func makeTensor(
  dataType: TensorDataType,
  owning pointer: CTensorHandle
) -> AnyTensor {
  switch dataType._cDataType {
  case TF_BOOL:
    return Tensor<Bool>(handle: TensorHandle(_owning: pointer))
  case TF_INT8:
    return Tensor<Int8>(handle: TensorHandle(_owning: pointer))
  case TF_UINT8:
    return Tensor<UInt8>(handle: TensorHandle(_owning: pointer))
  case TF_INT16:
    return Tensor<Int16>(handle: TensorHandle(_owning: pointer))
  case TF_UINT16:
    return Tensor<UInt16>(handle: TensorHandle(_owning: pointer))
  case TF_INT32:
    return Tensor<Int32>(handle: TensorHandle(_owning: pointer))
  case TF_UINT32:
    return Tensor<UInt32>(handle: TensorHandle(_owning: pointer))
  case TF_INT64:
    return Tensor<Int64>(handle: TensorHandle(_owning: pointer))
  case TF_UINT64:
    return Tensor<UInt64>(handle: TensorHandle(_owning: pointer))
  case TF_BFLOAT16:
    return Tensor<BFloat16>(handle: TensorHandle(_owning: pointer))
  case TF_FLOAT:
    return Tensor<Float>(handle: TensorHandle(_owning: pointer))
  case TF_DOUBLE:
    return Tensor<Double>(handle: TensorHandle(_owning: pointer))
  case TF_STRING:
    fatalError("StringTensor does not conform to AnyTensor")
  default:
    fatalError("Unhandled type: \(dataType)")
  }
}

/// A data type compatible with TensorFlow.
public protocol _TensorFlowDataTypeCompatible {
  /// The underlying TensorFlow data type.
  @inlinable
  static var tensorFlowDataType: TensorDataType { get }
}

/// A scalar data type compatible with TensorFlow.
///
/// Types that conform to `TensorFlowScalar` can be used as the `Scalar`
/// associated type of `Tensor`.
//
// This includes all `_TensorFlowDataTypeCompatible` types except `String`.
public protocol TensorFlowScalar : _TensorFlowDataTypeCompatible {}

public typealias TensorFlowNumeric = TensorFlowScalar & Numeric
public typealias TensorFlowSignedNumeric = TensorFlowScalar & SignedNumeric
public typealias TensorFlowInteger = TensorFlowScalar & BinaryInteger

/// A floating-point data type that conforms to `Differentiable` and is
/// compatible with TensorFlow.
///
/// - Note: `Tensor` conditionally conforms to `Differentiable` when the
///   `Scalar` associated type conforms `TensorFlowFloatingPoint`.
public protocol TensorFlowFloatingPoint :
  TensorFlowScalar & BinaryFloatingPoint & Differentiable
  where Self.RawSignificand: FixedWidthInteger,
        Self == Self.TangentVector,
        Self == Self.AllDifferentiableVariables {}

extension Float : TensorFlowFloatingPoint {}
extension Double : TensorFlowFloatingPoint {}

extension Bool : TensorFlowScalar {
  @inlinable
  public static var tensorFlowDataType: TensorDataType {
    return TensorDataType(TF_BOOL)
  }
}

extension Int8 : TensorFlowScalar {
  @inlinable
  public static var tensorFlowDataType: TensorDataType {
    return TensorDataType(TF_INT8)
  }
}

extension UInt8 : TensorFlowScalar {
  @inlinable
  public static var tensorFlowDataType: TensorDataType {
    return TensorDataType(TF_UINT8)
  }
}

extension Int16 : TensorFlowScalar {
  @inlinable
  public static var tensorFlowDataType: TensorDataType {
    return TensorDataType(TF_INT16)
  }
}

extension UInt16 : TensorFlowScalar {
  @inlinable
  public static var tensorFlowDataType: TensorDataType {
    return TensorDataType(TF_UINT16)
  }
}

extension Int32 : TensorFlowScalar {
  @inlinable
  public static var tensorFlowDataType: TensorDataType {
    return TensorDataType(TF_INT32)
  }
}

extension UInt32 : TensorFlowScalar {
  @inlinable
  public static var tensorFlowDataType: TensorDataType {
    return TensorDataType(TF_UINT32)
  }
}

extension Int64 : TensorFlowScalar {
  @inlinable
  public static var tensorFlowDataType: TensorDataType {
    return TensorDataType(TF_INT64)
  }
}

extension UInt64 : TensorFlowScalar {
  @inlinable
  public static var tensorFlowDataType: TensorDataType {
    return TensorDataType(TF_UINT64)
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
}

extension Float : TensorFlowScalar {
  @inlinable
  public static var tensorFlowDataType: TensorDataType {
    return TensorDataType(TF_FLOAT)
  }
}

extension Double : TensorFlowScalar {
  @inlinable
  public static var tensorFlowDataType: TensorDataType {
    return TensorDataType(TF_DOUBLE)
  }
}

extension String : _TensorFlowDataTypeCompatible {
  @inlinable
  public static var tensorFlowDataType: TensorDataType {
    return TensorDataType(TF_STRING)
  }
}
