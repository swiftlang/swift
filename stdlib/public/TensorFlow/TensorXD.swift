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
public struct Tensor1D<Unit : AccelerableTensorUnit> : RankedTensor {
  public var underlyingTensor: Tensor<Unit>

  public typealias BoolTensor = Tensor1D<Bool>
  public typealias Shape = Int

  @_inlineable
  public static var rank: Int { return 1 }

  @_inlineable
  public var shape: Shape {
    return underlyingTensor.shape[0]
  }

  @_versioned
  @_inlineable
  internal init(underlying: Tensor<Unit>) {
    self.underlyingTensor = underlying
  }

  @_inlineable
  public init?(_ other: Tensor<Unit>) {
    guard other.rank == Tensor1D.rank else { return nil }
    self.init(underlying: other)
  }

  @_inlineable
  public init(identicallyRanked other: Tensor<Unit>) {
    // Assertion disabled because "x.rank" causes a copy of X
    // back to the host.  TODO(clattner): need a better way of
    // exposing rank information in our model.
    //assert(other.rank == Tensor1D.rank)
    self.init(underlying: other)
  }
}

// Array literal support.
extension Tensor1D : ExpressibleByArrayLiteral {
  // The type of the elements of an array literal.
  public typealias ArrayLiteralElement = Unit

  // Creates an instance initialized with the given units.
  @_inlineable
  public init(arrayLiteral elements: Unit...) {
    self.init(elements)
  }
}

/// Perform an element-wise type conversion from `Tensor1D<T>`.
extension Tensor1D where Unit : Numeric {
  @_inlineable
  public init<T : Numeric>(_ other: Tensor1D<T>) {
    self.init(underlying: Tensor<Unit>(other.underlyingTensor))
  }
}

// Initializers
public extension Tensor1D {
  /// values initializer, takes an array of values.
  @_inlineable
  init(_ units: [Unit]) {
    self.init(underlying: Tensor<Unit>(units))
  }

  /// values initializer, takes a vararg list of values.
  @_inlineable
  init(_ units: Unit...) {
    self.init(units)
  }
}

public extension Tensor1D where Unit : Numeric {
  /// Zero initializer, takes a single integer as the shape.
  @_inlineable
  static func zeros(shape: Int) -> Tensor1D {
    return Tensor1D(underlying: Tensor<Unit>.zeros(shape: shape))
  }

  /// One initializer, takes a single integer as the shape.
  @_inlineable
  static func ones(shape: Int) -> Tensor1D {
    return Tensor1D(underlying: Tensor<Unit>.ones(shape: shape))
  }
}

public extension Tensor1D where Unit : FloatingPoint {
  @_inlineable
  static func randomNormal(
    shape: (Int, Int), mean: Double = 0, stddev: Double = 1
  ) -> Tensor1D {
    return Tensor1D(underlying:
      Tensor<Unit>.randomNormal(
        shape: [shape.0],
        mean: mean,
        stddev: stddev
      )
    )
  }
}

// Subscripting a 1D tensor produces a scalar.
public extension Tensor1D {
  subscript(index: Int) -> Unit {
    @inline(never) get {
      fatalError("Unimplemented")
    }
  }
}

/// Concrete type for two dimensional Tensors.
public struct Tensor2D<Unit : AccelerableTensorUnit> : RankedTensor {
  public var underlyingTensor: Tensor<Unit>

  public typealias BoolTensor = Tensor2D<Bool>
  public typealias Shape = (Int, Int)

  @_inlineable
  public static var rank: Int { return 2 }

  @_inlineable
  public var rank: Int { return Tensor2D.rank }

  @_inlineable
  public var shape: Shape {
    let dynamicShape = underlyingTensor.shape
    return (dynamicShape[0], dynamicShape[1])
  }

  @_versioned
  internal init(underlying: Tensor<Unit>) {
    self.underlyingTensor = underlying
  }

  @_inlineable
  public init?(_ other: Tensor<Unit>) {
    guard other.rank == Tensor2D.rank else { return nil }
    self.init(underlying: other)
  }

  @_inlineable
  public init(identicallyRanked other: Tensor<Unit>) {
    // FIXME: Check ranks
    self.init(underlying: other)
  }
}

// Array literal support.
extension Tensor2D : ExpressibleByArrayLiteral {
  // The type of the elements of an array literal.
  public typealias ArrayLiteralUnit = [Unit]

  // Creates an instance initialized with the given elements.
  @_inlineable
  public init(arrayLiteral elements: [Unit]...) {
    self.init(elements)
  }
}

/// Perform an element-wise type conversion from `Tensor2D<T>`.
extension Tensor2D where Unit : Numeric {
  @_inlineable
  public init<T : Numeric>(_ other: Tensor2D<T>) {
    self.init(underlying: Tensor<Unit>(other.underlyingTensor))
  }
}

// Initializers.
public extension Tensor2D {
  /// values initializer, takes an array of values.
  @_inlineable
  init(_ elements: [[Unit]]) {
    self.init(underlying: Tensor<Unit>(elements))
  }

#if true  // FIXME: temporary until the partitioner can handle arrays.
  @_inlineable
  init(_ value: Unit) {
    underlyingTensor = Tensor(value)
  }
#endif
}

public extension Tensor2D where Unit : Numeric {
  /// Zero initializer, takes a tuple of integers as the shape.
  @_inlineable
  public static func zeros(shape: (Int, Int)) -> Tensor1D<Unit> {
    return Tensor1D(underlying:
      Tensor<Unit>.zeros(shape: shape.0, shape.1))
  }

  /// One initializer, takes a tuple of integers as the shape.
  @_inlineable
  public static func ones(shape: (Int, Int)) -> Tensor1D<Unit> {
    return Tensor1D(underlying:
      Tensor<Unit>.ones(shape: shape.0, shape.1))
  }

  @_inlineable
  static func eye(shape: (Int, Int)) -> Tensor2D {
    return Tensor2D(underlying:
      Tensor<Unit>.eye(rowCount: shape.0, columnCount: shape.1))
  }
}

public extension Tensor2D where Unit : FloatingPoint {
  @_inlineable
  public static func randomNormal(
    shape: (Int, Int), mean: Double = 0, stddev: Double = 1
  ) -> Tensor2D {
    return Tensor2D(underlying:
      Tensor<Unit>.randomNormal(
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
  subscript(index: Int) -> Tensor1D<Unit> {
    return Tensor1D(underlying: underlyingTensor[index])
  }

  subscript(index1: Int, index2: Int) -> Unit {
    @inline(never) get {
      fatalError("Implement")
    }
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
  func rankLifted() -> Tensor2D<Unit> {
    return Tensor2D(underlying: underlyingTensor.rankLifted(by: 1))
  }
}

/// Value-preserving conversion initializer
public extension Tensor {
  init(_ tensor: Tensor1D<Unit>) {
    self = tensor.underlyingTensor
  }

  init(_ tensor: Tensor2D<Unit>) {
    self = tensor.underlyingTensor
  }
}
