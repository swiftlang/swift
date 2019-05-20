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
  
  init(handles: [_AnyTensorHandle])
  
  func tensorHandles() -> ([_AnyTensorHandle])
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

  /// An array of `nil`s with the same number of elements as `_outputTypeList`.
  /// The `nil` represents unknown shape.
  // TODO: This is a protocol requirement so that conformances can provide
  // custom const-evaluable implementations. When the const-evaluator is
  // powerful enough to evaluate the default implementation, remove this
  // requirement.
  static var _unknownShapeList: [TensorShape?] { get }

  /// Initializes a value of this type, taking ownership of the
  /// `_tensorHandleCount` tensors starting at address `tensorHandles`.
  init(_owning tensorHandles: UnsafePointer<CTensorHandle>?)

  override init(handles: [_AnyTensorHandle])
}

public extension TensorGroup {
  /// The number of tensor fields in this type.
  static var _tensorHandleCount: Int32 { return Int32(Self._typeList.count) }

  /// An array of `nil`s with the same number of elements as `_outputTypeList`.
  /// The `nil` represents unknown shape.
  static var _unknownShapeList: [TensorShape?] {
    return Array(repeating: nil, count: _typeList.count)
  }
  
  // The following instance properties are from `TensorArrayProtocol`.
  var _tensorHandleCount: Int32 { return Int32(Self._typeList.count) }
  var _typeList: [TensorDataType] { return Self._typeList }

  init(_owning tensorHandles: UnsafePointer<CTensorHandle>?, count: Int) {
    precondition(count == Self._typeList.count)
    self.init(_owning: tensorHandles)
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

  public init(_owning tensorHandles: UnsafePointer<CTensorHandle>?) {
    self.init(_owning: tensorHandles!.pointee)
  }

  public func tensorHandles() -> ([_AnyTensorHandle]){
    return [self.handle]
  }

  public convenience init(handles: [_AnyTensorHandle]) {
    self.init(handle: handles[0])
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

  public init(_owning tensorHandles: UnsafePointer<CTensorHandle>?) {
    self.init(owning: tensorHandles!.pointee)
  }

  public func tensorHandles() -> ([_AnyTensorHandle]){
    return [self.handle]
  }

  public convenience init(handles: [_AnyTensorHandle]) {
    self.init(handle: handles[0])
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

  public init(_owning tensorHandles: UnsafePointer<CTensorHandle>?) {
    self.init(owning: tensorHandles!.pointee)
  }

  public func tensorHandles() -> ([_AnyTensorHandle]){
    return [self.handle]
  }

  public convenience init(handles: [_AnyTensorHandle]) {
    self.init(handle: handles[0])
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

  public func tensorHandles() -> ([_AnyTensorHandle]){
    return [self.handle.handle]
  }

  public init(handles: [_AnyTensorHandle]) {
    self.init(handle: TensorHandle(handle: handles[0]))
  }
}

extension _TensorElementLiteral : TensorGroup {
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

  public func tensorHandles() -> ([_AnyTensorHandle]){
    return [self.handle.handle]
  }

  public init(handles: [_AnyTensorHandle]) {
    self.init(handle: TensorHandle(handle: handles[0]))
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

  public func tensorHandles() -> ([_AnyTensorHandle]){
    return [self.handle.handle]
  }

  public init(handles: [_AnyTensorHandle]) {
    self.init(handle: TensorHandle(handle: handles[0]))
  }
}

extension Array : TensorArrayProtocol where Element : TensorGroup {
  public func _unpackTensorHandles(into address: UnsafeMutablePointer<CTensorHandle>?) {
    var ptr = address
    for elem in self {
      elem._unpackTensorHandles(into: ptr)
      ptr = ptr!.advanced(by: Int(elem._tensorHandleCount))
    }
  }

  public var _tensorHandleCount: Int32 {
    return Element._tensorHandleCount * Int32(count)
  }

  public var _typeList: [TensorDataType] {
    return Array<TensorDataType>([[TensorDataType]](
      repeating: Element._typeList,
      count: count).joined())
  }

  public init(_owning tensorHandles: UnsafePointer<CTensorHandle>?, count: Int) {
    let size = count / Int(Element._tensorHandleCount)
    self = Array((0..<size).map { Element.init(
      _owning: tensorHandles?.advanced(by: $0 * Int(Element._tensorHandleCount)))
    })
  }

  public func tensorHandles() -> ([_AnyTensorHandle]){
    var result: [_AnyTensorHandle] = []
    result.reserveCapacity(Int(self._tensorHandleCount))
    for elem in self {
      result += (elem.tensorHandles())
    }
    return result
  }

  public init(handles: [_AnyTensorHandle]) {
    let size = handles.count / Int(Element._tensorHandleCount)
    self = Array((0..<size).map {
        let start = $0 * Int(Element._tensorHandleCount)
        let end = start + Int(Element._tensorHandleCount)
        let elemHandles = Array<_AnyTensorHandle>(handles[start..<end])
        return Element.init(handles: elemHandles)
      })
  }
}
