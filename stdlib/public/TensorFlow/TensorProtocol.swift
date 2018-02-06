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
// Tensor protocol generalizing unranked tensors (Tensor) and ranked tensors
// (Tensor1D, Tensor2D, etc). Common tensor properties and operations are
// defined on TensorProtocol.
//
// NOTE: TensorProtocol intentionally does not refine Collection in order for
// predictable and guaranteed acceleration. Methods for indexing/slicing are
// defined as standalone subscript methods. Users are encouraged to use
// aggregate operations rather than loops when working with TensorProtocol
// instances.
//
//===----------------------------------------------------------------------===//

public protocol TensorProtocol {
  /// Unit type.
  associatedtype Unit : AccelerableTensorUnit
  /// Tensor type of the same rank with unit type Bool.
  associatedtype BoolTensor : TensorProtocol where BoolTensor.Unit == Bool

  /// Rank: the number of dimensions of the tensor.
  var rank: Int { get }

  /// Shape: the sizes of the tensor along each dimension.
  var shape: [Int] { get }

  /// The number of units in the tensor.
  /// Always equal to the product of the elements of `shape`.
  var unitCount: Int { get }

  /// Units of the tensor, in row-major order.
  var units: [Unit] { get }

  /// Underlying tensor handle.
  var handle: TensorHandle<Unit> { get }

  /// Initialize from a TensorHandle.
  init(_ handle: TensorHandle<Unit>)
}
