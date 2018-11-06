//===-- TensorPair.swift --------------------------------------*- swift -*-===//
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
// This file defines the TensorPair type.
//
//===----------------------------------------------------------------------===//

/// A 2-tuple-like struct that conforms to TensorGroup so that you can use
/// 2-tuples in APIs that require TensorGroups.
public struct TensorPair<T : TensorGroup, U : TensorGroup> {
  public var first: T
  public var second: U

  public init(_ first: T, _ second: U) {
    self.first = first
    self.second = second
  }
}

extension TensorPair : TensorGroup {
  @inlinable
  public static var _typeList: [TensorDataType] {
    return T._typeList + U._typeList
  }

  @inlinable
  public static var _unknownShapeList: [TensorShape?] {
    return T._unknownShapeList + U._unknownShapeList
  }

  public func _unpackTensorHandles(
      into address: UnsafeMutablePointer<CTensorHandle>?) {
    first._unpackTensorHandles(into: address)
    let secondAddress = address.map {
      $0.advanced(by: Int(T._tensorHandleCount))
    }
    second._unpackTensorHandles(into: secondAddress)
  }

  public init(_owning tensorHandles: UnsafePointer<CTensorHandle>?) {
    self.first = T(_owning: tensorHandles)
    let secondTensorHandles = tensorHandles.map {
      $0.advanced(by: Int(T._tensorHandleCount))
    }
    self.second = U(_owning: secondTensorHandles)
  }
}
