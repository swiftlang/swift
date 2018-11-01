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

/// A protocol for types that can be used as tensor operation inputs. When a
/// TensorGroup is used as an input, it gets passed to the tensor operation as
/// an input list whose elements are the tensor fields of the type.
///
/// This protocol is divided from OutputTensorGroup in order for the number of
/// tensors to be determined at runtime. For example, Array<Tensor<Float>> may
/// have an unknown number of elements compile time.
public protocol InputTensorGroup {
  /// Writes the tensor handles to `address`, which must be allocated
  /// with enough capacity to hold `_tensorHandleCount` handles. The tensor
  /// handles written to `address` are borrowed: this container still
  /// owns them.
  func _unpackTensorHandles(into address: UnsafeMutablePointer<CTensorHandle>?)

  var _inputTensorHandleCount : Int32 { get }
}

/// A protocol for types that can be used as tensor operation outputs. When a
/// TensorGroup is used as an output, it gets initialized with its tensor fields
/// set to the tensor operation's tensor outputs.
/// The number of tensors must be known at compile time.
public protocol OutputTensorGroup {
  /// The types of the tensor stored properties in this type.
  static var _outputTypeList: [TensorDataType] { get }

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

public extension OutputTensorGroup {
  /// The number of tensor fields in this type.
  static var _outputTensorHandleCount: Int32 {
    return Int32(_outputTypeList.count)
  }

  /// An array of `nil`s with the same number of elements as `_outputTypeList`.
  /// The `nil` represents unknown shape.
  static var _unknownShapeList: [TensorShape?] {
    return Array(repeating: nil, count: Int(_outputTensorHandleCount))
  }
}

/// A protocol for types that can be used as tensor operation inputs and
/// outputs. When a TensorGroup is used as an input, it gets passed to the
/// tensor operation as an input list whose elements are the tensor fields of
/// the type. When a TensorGroup is used as an output, it gets initialized
/// with its tensor fields set to the tensor operation's tensor outputs.
///
/// TODO: Add a derived conformance to TensorGroup so that users don't have
/// to write the conformance themselves.
public protocol TensorGroup : InputTensorGroup & OutputTensorGroup {}

//===----------------------------------------------------------------------===//
// Conform standard TensorFlow types to TensorGroup
//===----------------------------------------------------------------------===//

extension TensorHandle : TensorGroup {
  @inlinable
  public static var _outputTypeList: [TensorDataType] {
    return [Scalar.tensorFlowDataType]
  }

  @inlinable
  public static var _unknownShapeList: [TensorShape?] {
    return [nil]
  }

  public var _inputTensorHandleCount : Int32 {
    return Int32(TensorHandle._outputTypeList.count)
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
  public static var _outputTypeList: [TensorDataType] {
    return [TensorDataType(TF_RESOURCE)]
  }

  @inlinable
  public static var _unknownShapeList: [TensorShape?] {
    return [nil]
  }

  public var _inputTensorHandleCount : Int32 {
    return Int32(ResourceHandle._outputTypeList.count)
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
  public static var _outputTypeList: [TensorDataType] {
    return [TensorDataType(TF_VARIANT)]
  }

  @inlinable
  public static var _unknownShapeList: [TensorShape?] {
    return [nil]
  }

  public var _inputTensorHandleCount : Int32 {
    return Int32(VariantHandle._outputTypeList.count)
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
  public static var _outputTypeList: [TensorDataType] {
    return [Scalar.tensorFlowDataType]
  }

  @inlinable
  public static var _unknownShapeList: [TensorShape?] {
    return [nil]
  }

  public var _inputTensorHandleCount : Int32 {
    return Int32(Tensor._outputTypeList.count)
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
  public static var _outputTypeList: [TensorDataType] {
    return [Scalar.tensorFlowDataType]
  }

  @inlinable
  public static var _unknownShapeList: [TensorShape?] {
    return [nil]
  }

  public var _inputTensorHandleCount : Int32 {
    return Int32(TensorElementLiteral._outputTypeList.count)
  }

  public func _unpackTensorHandles(
      into address: UnsafeMutablePointer<CTensorHandle>?) {
    address!.initialize(to: handle._cTensorHandle)
  }

  public init(_owning tensorHandles: UnsafePointer<CTensorHandle>?) {
    self.init(handle: TensorHandle(_owning: tensorHandles!.pointee))
  }
}

extension Array : InputTensorGroup where Element : InputTensorGroup {
  public func _unpackTensorHandles(into address: UnsafeMutablePointer<CTensorHandle>?) {
    var ptr = address
    for elem in self {
      elem._unpackTensorHandles(into: ptr)
      ptr = ptr!.advanced(by: Int(elem._inputTensorHandleCount))
    }
  }
  public var _inputTensorHandleCount : Int32 {
    var count: Int32 = 0
    for elem in self { count += elem._inputTensorHandleCount }
    return count
  }
}
