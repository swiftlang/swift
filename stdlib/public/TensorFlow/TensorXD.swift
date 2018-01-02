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
public struct Tensor1D<T : TensorElementProtocol> : RankedTensor {
  public var underlyingTensor: Tensor<T>

  public typealias Element = T
  public typealias BoolTensor = Tensor1D<Bool>

  @_inlineable
  public var rank : Int { return 1 }

  @_inlineable
  public init?(_ t: Tensor<T>) {
    guard t.rank == 1 else { return nil }
    self.underlyingTensor = t
  }

  @_inlineable
  public init(knownRank t: Tensor<Element>) {
    assert(t.rank == 1)
    self.underlyingTensor = t
  }

  @_inlineable
  public var count : Int { return shape[0] }
}

// Array literal support.
extension Tensor1D : ExpressibleByArrayLiteral {
  // The type of the elements of an array literal.
  public typealias ArrayLiteralElement = T

  // Creates an instance initialized with the given elements.
  @_inlineable
  public init(arrayLiteral elements: T...) {
    underlyingTensor = Tensor(oneD: elements)
  }
}

// Initializers
public extension Tensor1D {
  /// Perform an element conversion from Tensor1D<U> to Tensor1D<T>.
  @_inlineable
  public init<U>(_ t : Tensor1D<U>) {
    self = Tensor1D(knownRank: Tensor<Element>(t.underlyingTensor))
  }

  /// Zero initializer, takes a list of dimensions (always 1 for 1D tensors).
  @_inlineable
  init(zeros dimension: Int) {
    underlyingTensor = Tensor(zeros: dimension)
  }
  @_inlineable
  init(ones dimension: Int) {
    underlyingTensor = Tensor(ones: dimension)
  }

  // values initializer, takes an array of values.
  @_inlineable
  init(_ values: [T]) {
    underlyingTensor = Tensor(oneD: values)
  }

  // values initializer, takes a vararg list of values.
  @_inlineable
  init(_ values: T...) {
    underlyingTensor = Tensor(oneD: values)
  }

#if true  // FIXME: temporary until the partitioner can handle arrays.
  @_inlineable
  init(_ value : T) {
    underlyingTensor = Tensor(zeroD: value)
  }
#endif
  @_inlineable
  public init(randomNormal dimension1: Int,
              mean: Double = 0, stddev: Double = 1) {
    underlyingTensor = Tensor(randomNormal: dimension1,
                              mean: mean, stddev: stddev)
  }
}

// Subscripting a 1D tensor produces a scalar.
public extension Tensor1D {
  @_inlineable
  subscript(idx : Int) -> T {
    return underlyingTensor[scalar: idx]
  }
}

/// Concrete type for two dimensional Tensors.
public struct Tensor2D<T : TensorElementProtocol> : RankedTensor {
  public var underlyingTensor: Tensor<T>

  public typealias Element = T
  public typealias BoolTensor = Tensor2D<Bool>

  @_inlineable
  public var rank : Int { return 2 }

  @_inlineable
  public init?(_ t: Tensor<T>) {
    guard t.rank == 2 else { return nil }
    self.underlyingTensor = t
  }

  @_inlineable
  public init(knownRank t: Tensor<Element>) {
    assert(t.rank == 2)
    self.underlyingTensor = t
  }
}

// Array literal support.
extension Tensor2D : ExpressibleByArrayLiteral {
  // The type of the elements of an array literal.
  public typealias ArrayLiteralElement = [T]

  // Creates an instance initialized with the given elements.
  @_inlineable
  public init(arrayLiteral elements: [T]...) {
    underlyingTensor = Tensor(twoD: elements)
  }
}

// Initializers.
public extension Tensor2D {
  /// Perform an element conversion from TensorXD<U> to TensorXD<T>.
  @_inlineable
  public init<U>(_ t : Tensor2D<U>) {
    self = Tensor2D(knownRank: Tensor<Element>(t.underlyingTensor))
  }

  /// values initializer, takes an array of values.
  @_inlineable
  init(_ values: [[T]]) {
    underlyingTensor = Tensor(twoD: values)
  }

  /// values initializer, takes a vararg list of values.
  @_inlineable
  init(_ values: [T]...) {
    underlyingTensor = Tensor(twoD: values)
  }

  /// Zero initializer, takes a list of dimensions (always 2 for 2D tensors).
  @_inlineable
  init(zeros dimension1: Int, _ dimension2: Int) {
    underlyingTensor = Tensor(zeros: dimension1, dimension2)
  }
  @_inlineable
  init(ones dimension1: Int, _ dimension2: Int) {
    underlyingTensor = Tensor(ones: dimension1, dimension2)
  }

  @_inlineable
  init?(zeros dimensions: [Int]) {
    self.init(Tensor(zeros: dimensions))
  }
  @_inlineable
  init?(ones dimensions: [Int]) {
    self.init(Tensor(ones: dimensions))
  }

  @_inlineable
  public init(randomNormal dimension1: Int, _ dimension2: Int,
              mean: Double = 0, stddev: Double = 1) {
    underlyingTensor = Tensor(randomNormal: dimension1, dimension2,
                              mean: mean, stddev: stddev)
  }

  @_inlineable
  public init(eye dimensions: Int...) {
    underlyingTensor = Tensor(eye: dimensions)
  }
}

// Subscripting a tensor produces a smaller tensor.
public extension Tensor2D {
  @_inlineable
  subscript(idx : Int) -> Tensor1D<T> {
    return Tensor1D(underlyingTensor[tensor: idx])!
  }

  @_inlineable
  subscript(idx1 : Int, idx2 : Int) -> T {
    return underlyingTensor[scalar: idx1, idx2]
  }
}

// Sum tensor along one axis, producing a Tensor1D.
public extension Tensor2D {
  @_inlineable
  func sum(axis: Int) -> Tensor1D<T> {
    return Tensor1D(knownRank: underlyingTensor.sum(axis: axis))
  }
  @_inlineable
  func max(axis: Int) -> Tensor1D<T> {
    return Tensor1D(knownRank: underlyingTensor.max(axis: axis))
  }
}


// Random APIs.
public extension Tensor2D {
  @_inlineable
  static func •(lhs: Tensor1D<T>, rhs : Tensor2D<T>) -> Tensor1D<T> {
    return Tensor1D(lhs.underlyingTensor • rhs.underlyingTensor)!
  }
}

// Overloads for broadcasting to various shapes.  The current approach we're
// taking for broadcasting is that scalars can be mixed with tensors directly,
// and that tensor dimensions of 1 are automatically expanded to match wider
// tensor dimensions as needed (just like NumPy), but that rank changes need an
// explicit broadcast(x) call to make it clear that the rank is changing.  If
// this is too onerous in practice, we can pick other approaches.
@_inlineable
public func broadcast<T>(_ tensor : Tensor1D<T>) -> Tensor2D<T> {
  return Tensor2D(tensor.underlyingTensor.broadcastTo(rank: 2))!
}


