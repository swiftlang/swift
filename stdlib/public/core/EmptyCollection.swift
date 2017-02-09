//===--- EmptyCollection.swift - A collection with no elements ------------===//
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
//  Sometimes an operation is best expressed in terms of some other,
//  larger operation where one of the parameters is an empty
//  collection.  For example, we can erase elements from an Array by
//  replacing a subrange with the empty collection.
//
//===----------------------------------------------------------------------===//

/// An iterator that never produces an element.
///
/// - SeeAlso: `EmptyCollection<Element>`.
public struct EmptyIterator<Element> : IteratorProtocol, Sequence {
  /// Creates an instance.
  public init() {}

  /// Returns `nil`, indicating that there are no more elements.
  public mutating func next() -> Element? {
    return nil
  }
}

/// A collection whose element type is `Element` but that is always empty.
public struct EmptyCollection<Element> :
  RandomAccessCollection, MutableCollection
{
  /// A type that represents a valid position in the collection.
  ///
  /// Valid indices consist of the position of every element and a
  /// "past the end" position that's not valid for use as a subscript.
  public typealias Index = Int
  public typealias IndexDistance = Int
  public typealias SubSequence = EmptyCollection<Element>

  /// Creates an instance.
  public init() {}

  /// Always zero, just like `endIndex`.
  public var startIndex: Index {
    return 0
  }

  /// Always zero, just like `startIndex`.
  public var endIndex: Index {
    return 0
  }

  /// Always traps.
  ///
  /// `EmptyCollection` does not have any element indices, so it is not
  /// possible to advance indices.
  public func index(after i: Index) -> Index {
    _preconditionFailure("EmptyCollection can't advance indices")
  }

  /// Always traps.
  ///
  /// `EmptyCollection` does not have any element indices, so it is not
  /// possible to advance indices.
  public func index(before i: Index) -> Index {
    _preconditionFailure("EmptyCollection can't advance indices")
  }

  /// Returns an empty iterator.
  public func makeIterator() -> EmptyIterator<Element> {
    return EmptyIterator()
  }

  /// Accesses the element at the given position.
  ///
  /// Must never be called, since this collection is always empty.
  public subscript(position: Index) -> Element {
    get {
      _preconditionFailure("Index out of range")
    }
    set {
      _preconditionFailure("Index out of range")
    }
  }

  public subscript(bounds: Range<Index>) -> EmptyCollection<Element> {
    get {
      _precondition(bounds.lowerBound == 0 && bounds.upperBound == 0,
        "Index out of range")
      return self
    }
    set {
      _precondition(bounds.lowerBound == 0 && bounds.upperBound == 0,
        "Index out of range")
    }
  }

  /// The number of elements (always zero).
  public var count: Int {
    return 0
  }

  public func index(_ i: Index, offsetBy n: IndexDistance) -> Index {
    _precondition(i == startIndex && n == 0, "Index out of range")
    return i
  }

  public func index(
    _ i: Index, offsetBy n: IndexDistance, limitedBy limit: Index
  ) -> Index? {
    _precondition(i == startIndex && limit == startIndex,
      "Index out of range")
    return n == 0 ? i : nil
  }

  /// The distance between two indexes (always zero).
  public func distance(from start: Index, to end: Index) -> IndexDistance {
    _precondition(start == 0, "From must be startIndex (or endIndex)")
    _precondition(end == 0, "To must be endIndex (or startIndex)")
    return 0
  }

  public func _failEarlyRangeCheck(_ index: Index, bounds: Range<Index>) {
    _precondition(index == 0, "out of bounds")
    _precondition(bounds == Range(indices),
      "invalid bounds for an empty collection")
  }

  public func _failEarlyRangeCheck(
    _ range: Range<Index>, bounds: Range<Index>
  ) {
    _precondition(range == Range(indices),
      "invalid range for an empty collection")
    _precondition(bounds == Range(indices),
      "invalid bounds for an empty collection")
  }

  public typealias Indices = CountableRange<Int>
}

extension EmptyCollection : Equatable {
  public static func == (
    lhs: EmptyCollection<Element>, rhs: EmptyCollection<Element>
  ) -> Bool {
    return true
  }
}

@available(*, unavailable, renamed: "EmptyIterator")
public struct EmptyGenerator<Element> {}

extension EmptyIterator {
  @available(*, unavailable, renamed: "makeIterator()")
  public func generate() -> EmptyIterator<Element> {
    Builtin.unreachable()
  }
}
