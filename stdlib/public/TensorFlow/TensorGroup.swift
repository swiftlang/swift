//===-- TensorGroup.swift -------------------------------------*- swift -*-===//
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
// This file defines the TensorGroup type.
//
//===----------------------------------------------------------------------===//

import CTensorFlow

/// A protocol for types that can be mapped to Array<CTensorHandle>.
///
/// This protocol is divided from TensorGroup in order for the number of
/// tensors to be determined at runtime.  For example, Array<Tensor<Float>> may
/// have an unknown number of elements compile time.
public protocol TensorArrayProtocol {
  /// Writes the tensor handles to `address`, which must be allocated
  /// with enough capacity to hold `_tensorHandleCount` handles. The tensor
  /// handles written to `address` are borrowed: this container still
  /// owns them.
  func _unpackTensorHandles(into address: UnsafeMutablePointer<CTensorHandle>?)

  var _tensorHandleCount: Int32 { get }
}

/// A protocol for types that can be mapped to and from Array<CTensorHandle>.
/// When a TensorGroup is used as an argument, it gets passed to the
/// tensor operation as an argument list whose elements are the tensor fields of
/// the type.  When a TensorGroup is used as a result, it gets initialized
/// with its tensor fields set to the tensor operation's tensor results.
///
/// TODO: Add a derived conformance to TensorGroup so that users don't have
/// to write the conformance themselves.
public protocol TensorGroup : TensorArrayProtocol {
  /// The types of the tensor stored properties in this type.
  static var _typeList: [TensorDataType] { get }

  /// An array of `nil`s with the same number of elements as `_outputTypeList`.
  /// The `nil` represents unknown shape.
  // TODO: This is a protocol requirement so that conformances can provide
  // custom const-evaluable implementations. When the const-evaluator is
  // powerful enough to evaluate the default implementation, remove this
  // requirement.
  static var _unknownShapeList: [TensorShape?] { get }

  /// Initializes a value of this type, taking ownership of the
  /// `_tensorHandleCount` tensors that are at `tensorHandles`.
  init(_owning tensorHandles: UnsafePointer<CTensorHandle>?)
}

public extension TensorGroup {
  /// The number of tensor fields in this type.
  static var _tensorHandleCount: Int32 { return Int32(Self._typeList.count) }
  var _tensorHandleCount: Int32 { return Int32(Self._typeList.count) }

  /// An array of `nil`s with the same number of elements as `_outputTypeList`.
  /// The `nil` represents unknown shape.
  static var _unknownShapeList: [TensorShape?] {
    return Array(repeating: nil, count: _typeList.count)
  }
}

//===----------------------------------------------------------------------===//
// Conform standard TensorFlow types to TensorGroup
//===----------------------------------------------------------------------===//

extension TensorHandle : TensorGroup {
  @inlinable
  public static var _unknownShapeList: [TensorShape?] {
    return [nil]
  }

  @inlinable
  public static var _typeList: [TensorDataType] {
    return [Scalar.tensorFlowDataType]
  }

  public func _unpackTensorHandles(
      into address: UnsafeMutablePointer<CTensorHandle>?) {
    address!.initialize(to: _cTensorHandle)
  }

  public convenience init(_owning tensorHandles: UnsafePointer<CTensorHandle>?) {
    self.init(_owning: tensorHandles!.pointee)
  }
}

extension ResourceHandle : TensorGroup {
  @inlinable
  public static var _unknownShapeList: [TensorShape?] {
    return [nil]
  }

  @inlinable
  public static var _typeList: [TensorDataType] {
    return [TensorDataType(TF_RESOURCE)]
  }

  public func _unpackTensorHandles(
      into address: UnsafeMutablePointer<CTensorHandle>?) {
    address!.initialize(to: _cTensorHandle)
  }

  public convenience init(_owning tensorHandles: UnsafePointer<CTensorHandle>?) {
    self.init(owning: tensorHandles!.pointee)
  }
}

extension VariantHandle : TensorGroup {
  @inlinable
  public static var _unknownShapeList: [TensorShape?] {
    return [nil]
  }

  @inlinable
  public static var _typeList: [TensorDataType] {
    return [TensorDataType(TF_VARIANT)]
  }

  public func _unpackTensorHandles(
      into address: UnsafeMutablePointer<CTensorHandle>?) {
    address!.initialize(to: _cTensorHandle)
  }

  public convenience init(_owning tensorHandles: UnsafePointer<CTensorHandle>?) {
    self.init(owning: tensorHandles!.pointee)
  }
}

extension Tensor : TensorGroup {
  @inlinable
  public static var _unknownShapeList: [TensorShape?] {
    return [nil]
  }

  @inlinable
  public static var _typeList: [TensorDataType] {
    return [Scalar.tensorFlowDataType]
  }

  public func _unpackTensorHandles(
      into address: UnsafeMutablePointer<CTensorHandle>?) {
    address!.initialize(to: handle._cTensorHandle)
  }

  public init(_owning tensorHandles: UnsafePointer<CTensorHandle>?) {
    self.init(handle: TensorHandle(_owning: tensorHandles!.pointee))
  }
}

extension TensorElementLiteral : TensorGroup {
  @inlinable
  public static var _unknownShapeList: [TensorShape?] {
    return [nil]
  }

  @inlinable
  public static var _typeList: [TensorDataType] {
    return [Scalar.tensorFlowDataType]
  }

  public func _unpackTensorHandles(
      into address: UnsafeMutablePointer<CTensorHandle>?) {
    address!.initialize(to: handle._cTensorHandle)
  }

  public init(_owning tensorHandles: UnsafePointer<CTensorHandle>?) {
    self.init(handle: TensorHandle(_owning: tensorHandles!.pointee))
  }
}

extension StringTensor : TensorGroup {
  @inlinable
  public static var _unknownShapeList: [TensorShape?] {
    return [nil]
  }

  @inlinable
  public static var _typeList: [TensorDataType] {
    return [String.tensorFlowDataType]
  }

  public func _unpackTensorHandles(
      into address: UnsafeMutablePointer<CTensorHandle>?) {
    address!.initialize(to: handle._cTensorHandle)
  }

  public init(_owning tensorHandles: UnsafePointer<CTensorHandle>?) {
    self.init(handle: TensorHandle(_owning: tensorHandles!.pointee))
  }
}

extension Array : TensorArrayProtocol where Element : TensorArrayProtocol {
  public func _unpackTensorHandles(into address: UnsafeMutablePointer<CTensorHandle>?) {
    var ptr = address
    for elem in self {
      elem._unpackTensorHandles(into: ptr)
      ptr = ptr!.advanced(by: Int(elem._tensorHandleCount))
    }
  }
  public var _tensorHandleCount: Int32 {
    var count: Int32 = 0
    for elem in self { count += elem._tensorHandleCount }
    return count
  }
}
