//===-- TensorShape.swift -------------------------------------*- swift -*-===//
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

// NOTE: it may be possible to edit `TensorShape` to support "labeled tensors".
// Dimensions may be either an Int or an enum representing a label.

/// A struct representing the shape of a tensor.
///
/// `TensorShape` is a thin wrapper around an array of integers that represent
/// shape dimensions. All tensor types use `TensorShape` to represent their shape.
@_fixed_layout
public struct TensorShape : ExpressibleByArrayLiteral {
  /// The dimensions of the shape.
  public var dimensions: [Int]

  /// Initialize with an array of dimensions. The rank of the tensor is the
  /// length of the array.
  /// - Parameter dimensions: The shape dimensions.
  @inlinable @inline(__always)
  public init(_ dimensions: [Int]) {
    self.dimensions = dimensions
  }

  /// Initialize with a collection of dimensions. The rank of the tensor is the
  /// length of the collection.
  /// - Parameter dimensions: The shape dimensions.
  @inlinable @inline(__always)
  public init<C : Collection>(_ dimensions: C) where C.Element == Int {
    self.dimensions = Array(dimensions)
  }

  /// Initialize with an array literal representing the shape dimensions. The rank
  /// of the tensor is the number of dimensions.
  /// - Parameter dimensions: The shape dimensions.
  @inlinable @inline(__always)
  public init(arrayLiteral elements: Int...) {
    self.init(elements)
  }

  /// Initialize with variadic elements representing the shape dimensions. The rank
  /// of the tensor is the number of elements.
  /// - Parameter dimensions: The shape dimensions.
  @inlinable @inline(__always)
  public init(_ elements: Int...) {
    self.init(elements)
  }

  @inlinable @inline(__always)
  public init(repeating repeatedValue: Int, count: Int) {
    self.init(Array(repeating: repeatedValue, count: count))
  }

  /// The rank of the shape (i.e. the number of dimensions).
  @inlinable
  public var rank: Int {
    @inline(__always)
    get {
      return dimensions.count
    }
  }

  /// The size of the shape as a contiguously stored array.
  @inlinable
  public var contiguousSize: Int {
    @inline(__always)
    get {
      return dimensions.reduce(1, *)
    }
  }
}

public extension TensorShape {
  /// The rank of the shape (i.e. the number of dimensions).
  @inlinable
  var count: Int {
    @inline(__always)
    get {
      return dimensions.count
    }
  }

  @inlinable
  var indices: Range<Int> {
    @inline(__always)
    get {
      return dimensions.indices.lowerBound
        ..< dimensions.indices.upperBound
    }
  }

  @inlinable
  var startIndex: Int {
    @inline(__always)
    get {
      return dimensions.startIndex
    }
  }

  @inlinable
  var endIndex: Int {
    @inline(__always)
    get {
      return dimensions.endIndex
    }
  }

  /// Access the size of the i-th dimension.
  /// - Parameter index: The index of a dimension.
  @inlinable
  subscript(index: Int) -> Int {
    @inline(__always)
    _read { yield dimensions[index] }
    @inline(__always)
    _modify { yield &dimensions[index] }
  }

  /// Access the size of the i-th dimension.
  /// - Parameter index: The index of a dimension.
  @inlinable
  subscript(bounds: Range<Int>) -> TensorShape {
    @inline(__always)
    get {
      return TensorShape(dimensions[bounds])
    }
    @inline(__always)
    set {
      dimensions[bounds] = ArraySlice(newValue.dimensions)
    }
  }
}

extension TensorShape : Equatable {
  @inlinable @inline(__always)
  public static func == (lhs: TensorShape, rhs: TensorShape) -> Bool {
    return lhs.dimensions == rhs.dimensions
  }
}

extension TensorShape : Codable {
  @inlinable
  public func encode(to encoder: Encoder) throws {
    var container = encoder.singleValueContainer()
    try container.encode(dimensions)
  }

  @inlinable
  public init(from decoder: Decoder) throws {
    let container = try decoder.singleValueContainer()
    let dimensions = try container.decode([Int].self)
    self.init(dimensions)
  }
}
