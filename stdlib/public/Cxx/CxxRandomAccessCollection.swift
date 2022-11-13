//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

/// Bridged C++ iterator that allows computing the distance between two of its
/// instances, and advancing an instance by a given number of elements.
///
/// Mostly useful for conforming a type to the `CxxRandomAccessCollection`
/// protocol and should not generally be used directly.
///
/// - SeeAlso: https://en.cppreference.com/w/cpp/named_req/RandomAccessIterator
public protocol UnsafeCxxRandomAccessIterator: UnsafeCxxInputIterator {
  associatedtype Distance: BinaryInteger

  static func -(lhs: Self, rhs: Self) -> Distance
  static func +=(lhs: inout Self, rhs: Distance)
}

extension UnsafePointer: UnsafeCxxRandomAccessIterator {}

extension UnsafeMutablePointer: UnsafeCxxRandomAccessIterator {}

public protocol CxxRandomAccessCollection: CxxSequence, RandomAccessCollection {
  override associatedtype RawIterator: UnsafeCxxRandomAccessIterator
  override associatedtype Iterator = CxxIterator<Self>
  override associatedtype Element = RawIterator.Pointee
  override associatedtype Index = Int
  override associatedtype Indices = Range<Int>
  override associatedtype SubSequence = Slice<Self>

  /// Do not implement this function manually in Swift.
  func __beginUnsafe() -> RawIterator

  /// Do not implement this function manually in Swift.
  func __endUnsafe() -> RawIterator
}

extension CxxRandomAccessCollection {
  @inlinable
  public var startIndex: Int {
    return 0
  }

  @inlinable
  public var endIndex: Int {
    return count
  }

  @inlinable
  public var count: Int {
    return Int(__endUnsafe() - __beginUnsafe())
  }

  /// A C++ implementation of the subscript might be more performant. This 
  /// overload should only be used if the C++ type does not define `operator[]`.
  @inlinable
  public subscript(_ index: Int) -> Element {
    _read {
      // Not using CxxIterator here to avoid making a copy of the collection.
      var rawIterator = __beginUnsafe()
      rawIterator += RawIterator.Distance(index)
      yield rawIterator.pointee as! Element
    }
  }
}
