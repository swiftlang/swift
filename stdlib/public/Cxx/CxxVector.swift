//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

/// A C++ type that represents a vector of values.
///
/// C++ standard library type `std::vector` conforms to this protocol.
public protocol CxxVector<Element>: ExpressibleByArrayLiteral {
  associatedtype Element
  associatedtype RawIterator: UnsafeCxxRandomAccessIterator
    where RawIterator.Pointee == Element
  associatedtype RawMutableIterator: UnsafeCxxMutableRandomAccessIterator
    where RawMutableIterator.Pointee == Element

  init()

  /// Do not implement this function manually in Swift.
  mutating func __beginUnsafe() -> RawIterator

  mutating func push_back(_ element: Element)

  /// Do not implement this function manually in Swift.
  @discardableResult
  mutating func __eraseUnsafe(_ iterator: RawIterator) -> RawMutableIterator
}

extension CxxVector {
  /// Creates a C++ vector containing the elements of a Swift Sequence.
  ///
  /// This initializes the vector by copying every element of the sequence.
  ///
  /// - Complexity: O(*n*), where *n* is the number of elements in the Swift
  ///   sequence
  @inlinable
  public init<S: Sequence>(_ sequence: __shared S) where S.Element == Element {
    self.init()
    for item in sequence {
      self.push_back(item)
    }
  }

  @inlinable
  public init(arrayLiteral elements: Element...) {
    self.init(elements)
  }

  @discardableResult
  @inlinable
  public mutating func remove(at index: Int) -> Element {
    // Not using CxxIterator here to avoid making a copy of the collection.
    var rawIterator = self.__beginUnsafe()
    rawIterator += RawIterator.Distance(index)
    let element = rawIterator.pointee
    self.__eraseUnsafe(rawIterator)
    return element
  }
}
