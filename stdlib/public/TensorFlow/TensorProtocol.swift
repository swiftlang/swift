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
  /// Scalar type.
  associatedtype Scalar : AccelerableByTensorFlow
  /// Tensor type of the same rank with scalar type Bool.
  associatedtype BoolTensor : TensorProtocol where BoolTensor.Scalar == Bool

  /// The number of dimensions of the tensor.
  var rank: Int32 { get }

  /// The sizes of the tensor along each dimension.
  var shape: TensorShape { get }

  /// The number of scalars in the tensor.
  /// Always equal to the product of the elements of `shape`.
  var scalarCount: Int32 { get }

  /// Scalars of the tensor, in row-major order.
  var scalars: [Scalar] { get }

  /// Underlying tensor handle.
  /// - Note: Do NOT remove this. This is a compiler requirement.
  var handle: TensorHandle<Scalar> { get }

  /// Initialize from a TensorHandle.
  /// - Note: Do NOT remove this. This is a compiler requirement.
  init(handle: TensorHandle<Scalar>)
}

public extension TensorProtocol where Scalar : Numeric {
  /// Perform an element-wise type conversion from a Bool tensor.
  @_inlineable @inline(__always)
  init(_ other: BoolTensor) {
    self.init(handle: #tfop("Cast", other.handle, DstT: Scalar.self))
  }
}

public protocol ParameterAggregate {
  associatedtype Parameter : TensorProtocol
  mutating func update(with gradient: Self,
                       by updateParameter: (inout Parameter, Parameter) -> Void)
}
