//===--- EmptyCollection.swift - A collection with no elements ------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
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
  /// Construct an instance.
  public init() {}

  /// Returns `nil`, indicating that there are no more elements.
  public mutating func next() -> Element? {
    return nil
  }
}

/// A collection whose element type is `Element` but that is always empty.
public struct EmptyCollection<Element> :
  RandomAccessCollection
  // FIXME: swift-3-indexing-model - conform to MutableCollection as well
{
  /// A type that represents a valid position in the collection.
  ///
  /// Valid indices consist of the position of every element and a
  /// "past the end" position that's not valid for use as a subscript.
  public typealias Index = Int
  public typealias IndexDistance = Int

  /// Construct an instance.
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
  /// EmptyCollection does not have any element indices, so it is not
  /// possible to advance indices.
  @warn_unused_result
  public func next(i: Index) -> Index {
    fatalError("EmptyCollection can't advance indices")
  }

  /// Always traps.
  ///
  /// EmptyCollection does not have any element indices, so it is not
  /// possible to advance indices.
  @warn_unused_result
  public func previous(i: Index) -> Index {
    fatalError("EmptyCollection can't advance indices")
  }

  /// Returns an empty iterator.
  ///
  /// - Complexity: O(1).
  public func makeIterator() -> EmptyIterator<Element> {
    return EmptyIterator()
  }

  /// Access the element at `position`.
  ///
  /// Should never be called, since this collection is always empty.
  public subscript(position: Index) -> Element {
    _preconditionFailure("Index out of range")
  }

  /// The number of elements (always zero).
  public var count: Int {
    return 0
  }

  /// Always traps.
  ///
  /// EmptyCollection does not have any element indices, so it is not
  /// possible to advance indices.
  @warn_unused_result
  public func advance(i: Index, by n: IndexDistance) -> Index {
    fatalError("EmptyCollection can't advance indices")
  }

  /// Always traps.
  ///
  /// EmptyCollection does not have any element indices, so it is not
  /// possible to advance indices.
  @warn_unused_result
  public func advance(i: Index, by n: IndexDistance, limit: Index) -> Index {
    fatalError("EmptyCollection can't advance indices")
  }

  /// The distance between two indexes (always zero).
  @warn_unused_result
  public func distance(from start: Index, to end: Index) -> IndexDistance {
    _precondition(start == 0, "From must be startIndex (or endIndex)")
    _precondition(end == 0, "To must be endIndex (or startIndex)")
    return 0
  }

  // TODO: swift-3-indexing-model - fast fail any others from RandomAccessCollection (and up inheritance)?
}

@available(*, unavailable, renamed="EmptyIterator")
public struct EmptyGenerator<Element> {}

extension EmptyIterator {
  @available(*, unavailable, renamed="iterator")
  public func generate() -> EmptyIterator<Element> {
    fatalError("unavailable function can't be called")
  }
}
