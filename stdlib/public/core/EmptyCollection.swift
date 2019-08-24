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

/// A collection whose element type is `Element` but that is always empty.
@frozen // trivial-implementation
public struct EmptyCollection<Element> {
  // no properties

  /// Creates an instance.
  @inlinable // trivial-implementation
  public init() {}
}

extension EmptyCollection {
  /// An iterator that never produces an element.
  @frozen // trivial-implementation
  public struct Iterator {
    // no properties
  
    /// Creates an instance.
    @inlinable // trivial-implementation
    public init() {}
  }  
}

extension EmptyCollection.Iterator: IteratorProtocol, Sequence {
  /// Returns `nil`, indicating that there are no more elements.
  @inlinable // trivial-implementation
  public mutating func next() -> Element? {
    return nil
  }
}

extension EmptyCollection: Sequence {
  /// Returns an empty iterator.
  @inlinable // trivial-implementation
  public func makeIterator() -> Iterator {
    return Iterator()
  }
}

extension EmptyCollection: RandomAccessCollection, MutableCollection {
  /// A type that represents a valid position in the collection.
  ///
  /// Valid indices consist of the position of every element and a
  /// "past the end" position that's not valid for use as a subscript.
  public typealias Index = Int
  public typealias Indices = Range<Int>
  public typealias SubSequence = EmptyCollection<Element>

  /// Always zero, just like `endIndex`.
  @inlinable // trivial-implementation
  public var startIndex: Index {
    return 0
  }

  /// Always zero, just like `startIndex`.
  @inlinable // trivial-implementation
  public var endIndex: Index {
    return 0
  }

  /// Always traps.
  ///
  /// `EmptyCollection` does not have any element indices, so it is not
  /// possible to advance indices.
  @inlinable // trivial-implementation
  public func index(after i: Index) -> Index {
    _preconditionFailure("EmptyCollection can't advance indices")
  }

  /// Always traps.
  ///
  /// `EmptyCollection` does not have any element indices, so it is not
  /// possible to advance indices.
  @inlinable // trivial-implementation
  public func index(before i: Index) -> Index {
    _preconditionFailure("EmptyCollection can't advance indices")
  }

  /// Accesses the element at the given position.
  ///
  /// Must never be called, since this collection is always empty.
  @inlinable // trivial-implementation
  public subscript(position: Index) -> Element {
    get {
      _preconditionFailure("Index out of range")
    }
    set {
      _preconditionFailure("Index out of range")
    }
  }

  @inlinable // trivial-implementation
  public subscript(bounds: Range<Index>) -> SubSequence {
    get {
      _debugPrecondition(bounds.lowerBound == 0 && bounds.upperBound == 0,
        "Index out of range")
      return self
    }
    set {
      _debugPrecondition(bounds.lowerBound == 0 && bounds.upperBound == 0,
        "Index out of range")
    }
  }

  /// The number of elements (always zero).
  @inlinable // trivial-implementation
  public var count: Int {
    return 0
  }

  @inlinable // trivial-implementation
  public func index(_ i: Index, offsetBy n: Int) -> Index {
    _debugPrecondition(i == startIndex && n == 0, "Index out of range")
    return i
  }

  @inlinable // trivial-implementation
  public func index(
    _ i: Index, offsetBy n: Int, limitedBy limit: Index
  ) -> Index? {
    _debugPrecondition(i == startIndex && limit == startIndex,
      "Index out of range")
    return n == 0 ? i : nil
  }

  /// The distance between two indexes (always zero).
  @inlinable // trivial-implementation
  public func distance(from start: Index, to end: Index) -> Int {
    _debugPrecondition(start == 0, "From must be startIndex (or endIndex)")
    _debugPrecondition(end == 0, "To must be endIndex (or startIndex)")
    return 0
  }

  @inlinable // trivial-implementation
  public func _failEarlyRangeCheck(_ index: Index, bounds: Range<Index>) {
    _debugPrecondition(index == 0, "out of bounds")
    _debugPrecondition(bounds == indices, "invalid bounds for an empty collection")
  }

  @inlinable // trivial-implementation
  public func _failEarlyRangeCheck(
    _ range: Range<Index>, bounds: Range<Index>
  ) {
    _debugPrecondition(range == indices, "invalid range for an empty collection")
    _debugPrecondition(bounds == indices, "invalid bounds for an empty collection")
  }
}

extension EmptyCollection: Equatable {
  @inlinable // trivial-implementation
  public static func == (
    lhs: EmptyCollection<Element>, rhs: EmptyCollection<Element>
  ) -> Bool {
    return true
  }
}
