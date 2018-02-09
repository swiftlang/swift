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
//
// A struct representing the shape of a tensor.
//
// TensorShape is a thin wrapper around an array of integers that represent
// shape dimensions. All tensor types use TensorShape to represent their shape.
//
//===----------------------------------------------------------------------===//

// NOTE: it may be possible to edit TensorShape to support "labeled tensors".
// Dimensions may be either an Int or an enum representing a label.

public struct TensorShape : ExpressibleByArrayLiteral {
  /// The dimensions of the shape.
  @_versioned
  internal var dimensions: [Int32]

  /// Initialize with an array of dimensions. The rank of the tensor is the
  /// length of the array.
  /// - Parameter dimensions: the shape dimensions.
  @_inlineable @inline(__always)
  public init(_ dimensions: [Int32]) {
    self.dimensions = dimensions
  }

  /// Initialize with an array literal, representing shape dimensions. The rank
  /// of the tensor is the length of the dimensions.
  /// - Parameter dimensions: the shape dimensions.
  @_inlineable @inline(__always)
  public init(arrayLiteral elements: Int32...) {
    self.init(elements)
  }

  /// Initialize with variadic elements, representing shape dimensions. The rank
  /// of the tensor is the length of the elements.
  /// - Parameter dimensions: the shape dimensions.
  @_inlineable @inline(__always)
  public init(_ elements: Int32...) {
    self.init(elements)
  }

  /// The rank of the shape.
  @_inlineable
  public var rank: Int {
    @inline(__always)
    get {
      return dimensions.count
    }
  }

  /// The size of the shape as a contiguously stored array.
  @_inlineable
  public var contiguousSize: Int {
    @inline(__always)
    get {
      return dimensions.lazy.map(Int.init).reduce(1, *)
    }
  }
}

public extension TensorShape {
  @_inlineable
  var count: Int {
    @inline(__always)
    get {
      return dimensions.count
    }
  }

  @_inlineable
  var indices: CountableRange<Int> {
    @inline(__always)
    get {
      return dimensions.indices
    }
  }

  @_inlineable
  var startIndex: Int {
    @inline(__always)
    get {
      return dimensions.startIndex
    }
  }

  @_inlineable
  var endIndex: Int {
    @inline(__always)
    get {
      return dimensions.endIndex
    }
  }

  /// Access the size of the i-th dimension.
  /// - Parameter index: the index of a dimension.
  @_inlineable
  subscript(index: Int) -> Int32 {
    @inline(__always)
    get {
      return dimensions[index]
    }
    @inline(__always)
    set {
      dimensions[index] = newValue
    }
  }

  /// Access the size of the i-th dimension.
  /// - Parameter index: the index of a dimension.
  @_inlineable
  subscript(bounds: Range<Int>) -> TensorShape {
    @inline(__always)
    get {
      return TensorShape(Array(dimensions[bounds]))
    }
    @inline(__always)
    set {
      dimensions[bounds] = ArraySlice(newValue.dimensions)
    }
  }
}

extension TensorShape : Equatable {
  @_inlineable @inline(__always)
  public static func ==(lhs: TensorShape, rhs: TensorShape) -> Bool {
    return lhs.dimensions == rhs.dimensions
  }
}
