//===-- HackyTensorflowMigrationSupport.swift -----------------*- swift -*-===//
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
// This file defines the TensorGroup,TensorDataType and TensorArrayProtocol
// types.
//
//===----------------------------------------------------------------------===//

// This whole file is a hack in order to allow moving TensorFlow out of the
// swift compiler and to build it independently. There is an obviously more
// general version of this using associated types or something that can be
// given a general name.

public typealias CTensorHandle = OpaquePointer

/// A TensorFlow dynamic type value that can be created from types that conform
/// to `TensorFlowScalar`.
// This simply wraps a `TF_DataType` and allows user code to handle
// `TF_DataType` without importing CTensorFlow, which pollutes the namespace
// with TensorFlow C API declarations.
public struct TensorDataType {
  public var _internalStorageType: UInt32

  public init(rawValue: UInt32) {
    self._internalStorageType = rawValue
  }
}

/// A protocol representing types that can be mapped to `Array<CTensorHandle>`.
///
/// This protocol is defined separately from `TensorGroup` in order for the
/// number of tensors to be determined at runtime. For example,
/// `[Tensor<Float>]` may have an unknown number of elements at compile time.
///
/// This protocol can be derived automatically for structs whose stored
/// properties all conform to the `TensorGroup` protocol. It cannot be derived
/// automatically for structs whose properties all conform to
/// `TensorArrayProtocol` due to the constructor requirement (i.e., in such
/// cases it would be impossible to know how to break down `count` among the
/// stored properties).
public protocol TensorArrayProtocol {
  /// Writes the tensor handles to `address`, which must be allocated
  /// with enough capacity to hold `_tensorHandleCount` handles. The tensor
  /// handles written to `address` are borrowed: this container still
  /// owns them.
  func _unpackTensorHandles(into address: UnsafeMutablePointer<CTensorHandle>?)

  var _tensorHandleCount: Int32 { get }
  var _typeList: [TensorDataType] { get }

  init(_owning tensorHandles: UnsafePointer<CTensorHandle>?, count: Int)
}

/// A protocol representing types that can be mapped to and from
/// `Array<CTensorHandle>`.
///
/// When a `TensorGroup` is used as an argument to a tensor operation, it is
/// passed as an argument list whose elements are the tensor fields of the type.
///
/// When a `TensorGroup` is returned as a result of a tensor operation, it is
/// initialized with its tensor fields set to the tensor operation's tensor
/// results.
public protocol TensorGroup : TensorArrayProtocol {

  /// The types of the tensor stored properties in this type.
  static var _typeList: [TensorDataType] { get }

  /// Initializes a value of this type, taking ownership of the
  /// `_tensorHandleCount` tensors starting at address `tensorHandles`.
  init(_owning tensorHandles: UnsafePointer<CTensorHandle>?)
}
