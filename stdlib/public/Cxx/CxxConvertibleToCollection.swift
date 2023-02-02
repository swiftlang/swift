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

/// A C++ type that can be converted to a Swift collection.
public protocol CxxConvertibleToCollection<Element> {
  associatedtype Element
  associatedtype RawIterator: UnsafeCxxInputIterator
    where RawIterator.Pointee == Element

  /// Do not implement this function manually in Swift.
  mutating func __beginUnsafe() -> RawIterator

  /// Do not implement this function manually in Swift.
  mutating func __endUnsafe() -> RawIterator
}

extension CxxConvertibleToCollection {
  @inlinable
  internal func forEach(_ body: (RawIterator.Pointee) -> Void) {
    var mutableSelf = self
    var rawIterator = mutableSelf.__beginUnsafe()
    let endIterator = mutableSelf.__endUnsafe()
    while rawIterator != endIterator {
      body(rawIterator.pointee)
      rawIterator = rawIterator.successor()
    }
    withExtendedLifetime(mutableSelf) {}
  }
}

extension RangeReplaceableCollection {
  /// Creates a collection containing the elements of a C++ container.
  ///
  /// This initializes the collection by copying every element of the C++
  /// container.
  ///
  /// - Complexity: O(*n*), where *n* is the number of elements in the C++
  ///   container when each element is copied in O(1). Note that this might not
  ///   be true for certain C++ types, e.g. those with a custom copy
  ///   constructor that performs additional logic.
  public init<C: CxxConvertibleToCollection>(_ elements: C)
    where C.RawIterator.Pointee == Element {

    self.init()
    elements.forEach { self.append($0) }
  }
}

extension SetAlgebra {
  /// Creates a set containing the elements of a C++ container.
  ///
  /// This initializes the set by copying every element of the C++ container.
  ///
  /// - Complexity: O(*n*), where *n* is the number of elements in the C++
  ///   container when each element is copied in O(1). Note that this might not
  ///   be true for certain C++ types, e.g. those with a custom copy
  ///   constructor that performs additional logic.
  public init<C: CxxConvertibleToCollection>(_ elements: C)
    where C.RawIterator.Pointee == Element {

    self.init()
    elements.forEach { self.insert($0) }
  }
}
