//===-- TensorXD.swift ----------------------------------------*- swift -*-===//
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

/// Concrete type for one dimensional Tensors.
public struct Tensor1D<Element : TensorElementProtocol> : RankedTensor {
  public var underlyingTensor: Tensor<Element>

  public typealias BoolTensor = Tensor1D<Bool>
  public typealias Shape = Int

  @_inlineable
  public static var rank: Int { return 1 }

  @_inlineable
  public var rank: Int { return Tensor1D.rank }

  @_versioned
  @_inlineable
  internal init(underlying: Tensor<Element>) {
    self.underlyingTensor = underlying
  }

  @_inlineable
  public init?(_ other: Tensor<Element>) {
    guard other.rank == Tensor1D.rank else { return nil }
    self.init(underlying: other)
  }

  @_inlineable
  public init(identicallyRanked other: Tensor<Element>) {
    precondition(other.rank == Tensor1D.rank, "Rank must be \(Tensor1D.rank)")
    self.init(underlying: other)
  }

  @_inlineable
  public var shape: Int {
    return underlyingTensor.shape[0]
  }

  @_inlineable
  public var count: Int {
    return shape
  }
}

// Array literal support.
extension Tensor1D : ExpressibleByArrayLiteral {
  // The type of the elements of an array literal.
  public typealias ArrayLiteralElement = Element

  // Creates an instance initialized with the given elements.
  @_inlineable
  public init(arrayLiteral elements: Element...) {
    self.init(elements)
  }
}

// Initializers
public extension Tensor1D {
  /// Perform an element conversion from Tensor1D<U> to Tensor1D<Element>.
  @_inlineable
  public init<U>(_ other: Tensor1D<U>) {
    self.init(underlying: Tensor(other.underlyingTensor))
  }

  /// values initializer, takes an array of values.
  @_inlineable
  init(_ elements: [Element]) {
    underlyingTensor = Tensor<Element>(elements)
  }

  /// values initializer, takes a vararg list of values.
  @_inlineable
  init(_ elements: Element...) {
    self.init(elements)
  }

#if true  // FIXME: temporary until the partitioner can handle arrays.
  @_inlineable
  init(_ value: Element) {
    underlyingTensor = Tensor(value)
  }
#endif
}

public extension Tensor1D where Element : Numeric {
  /// Zero initializer, takes a single integer as the shape.
  @_inlineable
  static func zeros(shape: Int) -> Tensor1D {
    return Tensor1D(underlying: Tensor<Element>.zeros(shape: shape))
  }

  /// One initializer, takes a single integer as the shape.
  @_inlineable
  static func ones(shape: Int) -> Tensor1D {
    return Tensor1D(underlying: Tensor<Element>.ones(shape: shape))
  }
}

public extension Tensor1D where Element : FloatingPoint {
  @_inlineable
  static func randomNormal(
    shape: (Int, Int), mean: Double = 0, stddev: Double = 1
  ) -> Tensor1D {
    return Tensor1D(underlying:
      Tensor<Element>.randomNormal(
        shape: [shape.0],
        mean: mean,
        stddev: stddev
      )
    )
  }
}

// Subscripting a 1D tensor produces a scalar.
public extension Tensor1D {
  @_inlineable
  subscript(index: Int) -> Element {
    return underlyingTensor[scalar: index]
  }
}

/// Concrete type for two dimensional Tensors.
public struct Tensor2D<Element : TensorElementProtocol> : RankedTensor {
  public var underlyingTensor: Tensor<Element>

  public typealias BoolTensor = Tensor2D<Bool>
  public typealias Shape = (Int, Int)

  @_inlineable
  public static var rank: Int { return 2 }

  @_inlineable
  public var rank: Int { return Tensor2D.rank }

  @_versioned
  internal init(underlying: Tensor<Element>) {
    self.underlyingTensor = underlying
  }

  @_inlineable
  public init?(_ other: Tensor<Element>) {
    guard other.rank == Tensor2D.rank else { return nil }
    self.underlyingTensor = other
  }

  @_inlineable
  public init(identicallyRanked other: Tensor<Element>) {
    precondition(other.rank == Tensor2D.rank, "Rank must be \(Tensor2D.rank)")
    self.init(underlying: other)
  }

  @_inlineable
  public var shape: (Int, Int) {
    return (underlyingTensor.shape[0], underlyingTensor.shape[1])
  }
}

// Array literal support.
extension Tensor2D : ExpressibleByArrayLiteral {
  // The type of the elements of an array literal.
  public typealias ArrayLiteralElement = [Element]

  // Creates an instance initialized with the given elements.
  @_inlineable
  public init(arrayLiteral elements: [Element]...) {
    self.init(elements)
  }
}

// Initializers.
public extension Tensor2D {
  /// Perform an element conversion from TensorXD<U> to TensorXD<Element>.
  @_inlineable
  init<U>(_ other: Tensor2D<U>) {
    self.init(underlying: Tensor(other.underlyingTensor))
  }

  /// values initializer, takes an array of values.
  @_inlineable
  init(_ elements: [[Element]]) {
    underlyingTensor = Tensor<Element>(elements)
  }
}

public extension Tensor2D where Element : Numeric {
  /// Zero initializer, takes a tuple of integers as the shape.
  @_inlineable
  public static func zeros(shape: (Int, Int)) -> Tensor1D<Element> {
    return Tensor1D(underlying:
      Tensor<Element>.zeros(shape: shape.0, shape.1))
  }

  /// One initializer, takes a tuple of integers as the shape.
  @_inlineable
  public static func ones(shape: (Int, Int)) -> Tensor1D<Element> {
    return Tensor1D(underlying:
      Tensor<Element>.ones(shape: shape.0, shape.1))
  }

  @_inlineable
  static func eye(shape: (Int, Int)) -> Tensor2D {
    return Tensor2D(underlying:
      Tensor<Element>.eye(rowCount: shape.0, columnCount: shape.1))
  }
}

public extension Tensor2D where Element : FloatingPoint {
  @_inlineable
  public static func randomNormal(
    shape: (Int, Int), mean: Double = 0, stddev: Double = 1
  ) -> Tensor2D {
    return Tensor2D(underlying:
      Tensor<Element>.randomNormal(
        shape: [shape.0, shape.1],
        mean: mean,
        stddev: stddev
      )
    )
  }
}

// Subscripting a tensor produces a smaller tensor.
public extension Tensor2D {
  @_inlineable
  subscript(index: Int) -> Tensor1D<Element> {
    return Tensor1D(underlying: underlyingTensor[tensor: index])
  }

  @_inlineable
  subscript(index1: Int, index2: Int) -> Element {
    return underlyingTensor[scalar: index1, index2]
  }
}

// Overloads for broadcasting to various shapes.  The current approach we're
// taking for broadcasting is that scalars can be mixed with tensors directly,
// and that tensor dimensions of 1 are automatically expanded to match wider
// tensor dimensions as needed (just like NumPy), but that rank changes need an
// explicit broadcast(x) call to make it clear that the rank is changing.  If
// this is too onerous in practice, we can pick other approaches.
public extension Tensor1D {
  @_inlineable
  var broadcast: Tensor2D<Element> {
    return Tensor2D(underlying: underlyingTensor.broadcast(toRank: 2))
  }
}

/// Value-preserving conversion initializer
public extension Tensor {
  init(_ tensor: Tensor1D<Element>) {
    self = tensor.underlyingTensor
  }

  init(_ tensor: Tensor2D<Element>) {
    self = tensor.underlyingTensor
  }
}
