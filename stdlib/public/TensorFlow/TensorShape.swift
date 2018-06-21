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
// Dimensions may be either an Int32 or an enum representing a label.

/// A struct representing the shape of a tensor.
///
/// `TensorShape` is a thin wrapper around an array of integers that represent
/// shape dimensions. All tensor types use `TensorShape` to represent their shape.
@_fixed_layout
public struct TensorShape : ExpressibleByArrayLiteral {
  /// The dimensions of the shape.
  @usableFromInline
  internal var dimensions: [Int32]

  /// Initialize with an array of dimensions. The rank of the tensor is the
  /// length of the array.
  /// - Parameter dimensions: The shape dimensions.
  @inlinable @inline(__always)
  public init(_ dimensions: [Int32]) {
    self.dimensions = dimensions
  }

  /// Initialize with an array literal representing the shape dimensions. The rank
  /// of the tensor is the number of dimensions.
  /// - Parameter dimensions: The shape dimensions.
  @inlinable @inline(__always)
  public init(arrayLiteral elements: Int32...) {
    self.init(elements)
  }

  /// Initialize with variadic elements representing the shape dimensions. The rank
  /// of the tensor is the number of elements.
  /// - Parameter dimensions: The shape dimensions.
  @inlinable @inline(__always)
  public init(_ elements: Int32...) {
    self.init(elements)
  }

  /// The rank of the shape.
  @inlinable
  public var rank: Int32 {
    @inline(__always)
    get {
      return Int32(dimensions.count)
    }
  }

  /// The size of the shape as a contiguously stored array.
  @inlinable
  public var contiguousSize: Int32 {
    @inline(__always)
    get {
      return dimensions.reduce(1, *)
    }
  }
}

public extension TensorShape {
  @inlinable
  var count: Int32 {
    @inline(__always)
    get {
      return Int32(dimensions.count)
    }
  }

  @inlinable
  var indices: Range<Int32> {
    @inline(__always)
    get {
      return Int32(dimensions.indices.lowerBound)
        ..< Int32(dimensions.indices.upperBound)
    }
  }

  @inlinable
  var startIndex: Int32 {
    @inline(__always)
    get {
      return Int32(dimensions.startIndex)
    }
  }

  @inlinable
  var endIndex: Int32 {
    @inline(__always)
    get {
      return Int32(dimensions.endIndex)
    }
  }

  /// Access the size of the i-th dimension.
  /// - Parameter index: The index of a dimension.
  @inlinable
  subscript(index: Int32) -> Int32 {
    @inline(__always)
    get {
      return dimensions[Int(index)]
    }
    @inline(__always)
    set {
      dimensions[Int(index)] = newValue
    }
  }

  /// Access the size of the i-th dimension.
  /// - Parameter index: The index of a dimension.
  @inlinable
  subscript(bounds: Range<Int32>) -> TensorShape {
    @inline(__always)
    get {
      return TensorShape(
        Array(dimensions[Int(bounds.lowerBound)..<Int(bounds.upperBound)])
      )
    }
    @inline(__always)
    set {
      let bounds = Int(bounds.lowerBound)..<Int(bounds.upperBound)
      dimensions[bounds] = ArraySlice(newValue.dimensions)
    }
  }
}

extension TensorShape : Equatable {
  @inlinable @inline(__always)
  public static func ==(lhs: TensorShape, rhs: TensorShape) -> Bool {
    return lhs.dimensions == rhs.dimensions
  }
}
