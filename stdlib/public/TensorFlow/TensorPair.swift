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
public struct TensorPair<A : TensorGroup, B : TensorGroup> {
  public var a: A
  public var b: B

  public init(_ a: A, _ b: B) {
    self.a = a
    self.b = b
  }
}

extension TensorPair : TensorGroup {
  @inlinable
  public static var _outputTypeList: [TensorDataType] {
    return A._outputTypeList + B._outputTypeList
  }

  @inlinable
  public static var _unknownShapeList: [TensorShape?] {
    return A._unknownShapeList + B._unknownShapeList
  }

  public var _inputTensorHandleCount: Int32 {
    return a._inputTensorHandleCount + b._inputTensorHandleCount
  }

  public func _unpackTensorHandles(
      into address: UnsafeMutablePointer<CTensorHandle>?) {
    a._unpackTensorHandles(into: address)
    let bAddress = address.map {
      $0.advanced(by: Int(A._outputTensorHandleCount))
    }
    b._unpackTensorHandles(into: bAddress)
  }

  public init(_owning tensorHandles: UnsafePointer<CTensorHandle>?) {
    self.a = A(_owning: tensorHandles)
    let bTensorHandles = tensorHandles.map {
      $0.advanced(by: Int(A._outputTensorHandleCount))
    }
    self.b = B(_owning: bTensorHandles)
  }
}
