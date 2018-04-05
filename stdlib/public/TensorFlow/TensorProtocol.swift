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

/// The protocol generalizing unranked tensors (`Tensor`) and ranked tensors
/// (`Tensor1D`, `Tensor2D`, etc). Common tensor properties and operations are
/// defined on `TensorProtocol`.
///
/// - Note: `TensorProtocol` intentionally does not refine `Collection` in order
/// for predictable and guaranteed acceleration. Methods for indexing/slicing
/// are defined as standalone subscript methods. Users are encouraged to use
/// aggregate operations rather than loops when working with `TensorProtocol`
/// instances.
public protocol TensorProtocol {
  /// Scalar type.
  associatedtype Scalar : AccelerableByTensorFlow
  /// Tensor type of the same rank with scalar type `Bool`.
  associatedtype BoolTensor : TensorProtocol where BoolTensor.Scalar == Bool

  /// The number of dimensions of the tensor.
  var rank: Int32 { get }

  /// The dimensions of the tensor.
  var shape: TensorShape { get }

  /// The number of scalars in the tensor.
  /// Always equal to the product of the elements of `shape`.
  var scalarCount: Int32 { get }

  /// The scalars of the tensor, in row-major order.
  var scalars: [Scalar] { get }

  /// The underlying `TensorHandle`.
  /// - Note: Do NOT remove this. This is a compiler requirement.
  var handle: TensorHandle<Scalar> { get }

  /// Initialize from a `TensorHandle`.
  /// - Note: Do NOT remove this. This is a compiler requirement.
  init(handle: TensorHandle<Scalar>)
}

//===----------------------------------------------------------------------===//
// Memory transfer markers
//===----------------------------------------------------------------------===//

/// TODO: Remove when send/receive semantics gets revisited.
public extension TensorProtocol {
  /// Mark memory transfer to device.
  @_inlineable @inline(__always)
  func toDevice() -> Self {
    return Self(handle: _TFSend(handle))
  }

  /// Mark memory transfer to host.
  @_inlineable @inline(__always)
  func toHost() -> Self {
    return Self(handle: _TFReceive(handle))
  }
}

//===----------------------------------------------------------------------===//
// Parameter aggregate
//===----------------------------------------------------------------------===//

public protocol ParameterAggregate {
  associatedtype Parameter : TensorProtocol
    where Parameter.Scalar : FloatingPoint
  typealias Scalar = Parameter.Scalar
  /// Update self with the gradient value using the `updateParameter` function.
  mutating func update(with gradient: Self,
                       by updateParameter: (inout Parameter, Parameter) -> Void)
}
