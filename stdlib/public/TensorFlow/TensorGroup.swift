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

/// A protocol for types that can be used as tensor operation inputs and
/// outputs. When a TensorGroup is used as an input, it gets passed to the
/// tensor operation as an input list whose elements are the tensor fields of
/// the type. When a TensorGroup is used as an output, it gets initialized
/// with its tensor fields set to the tensor operation's tensor outputs.
///
/// TODO: Add a derived conformance to TensorGroup so that users don't have
/// to write the conformance themselves.
public protocol TensorGroup {
  /// The types of the tensor stored properties in this type.
  static var _typeList: [TensorDataType] { get }

  /// Writes the tensor handles to `address`, which must be allocated
  /// with enough capacity to hold `_tensorHandleCount` handles. The tensor
  /// handles written to `address` are borrowed: this container still
  /// owns them.
  func _unpackTensorHandles(into address: UnsafeMutablePointer<CTensorHandle>?)

  /// Initializes a value of this type, taking ownership of the
  /// `_tensorHandleCount` tensors that are at `tensorHandles`.
  init(_owning tensorHandles: UnsafePointer<CTensorHandle>?)
}

public extension TensorGroup {
  /// The number of tensor fields in this type.
  static var _tensorHandleCount: Int32 {
    return Int32(_typeList.count)
  }

  /// An array of `nil`s with size equal to `_tensorHandleCount`. The `nil`
  /// represents unknown shape.
  static var _unknownShapeList: [TensorShape?] {
    return Array(repeating: nil, count: Int(_tensorHandleCount))
  }
}

//===----------------------------------------------------------------------===//
// Conform standard TensorFlow types to TensorGroup
//===----------------------------------------------------------------------===//

extension TensorHandle : TensorGroup where Scalar : AccelerableByTensorFlow {
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

extension Tensor : TensorGroup where Scalar : AccelerableByTensorFlow {
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

extension TensorElementLiteral : TensorGroup where Scalar : AccelerableByTensorFlow {
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
