//===-- TensorProtocol.swift ----------------------------------*- swift -*-===//
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
// This file defines the TensorProtocol type.
//
//===----------------------------------------------------------------------===//

public protocol TensorProtocol {
  /// Scalar type.
  associatedtype Scalar : TensorFlowScalar

  /// The underlying `TensorHandle`.
  /// - Note: Do NOT remove this. This is a compiler requirement.
  var handle: TensorHandle<Scalar> { get }

  /// Initialize from a `TensorHandle`.
  /// - Note: Do NOT remove this. This is a compiler requirement.
  init(handle: TensorHandle<Scalar>)
}
