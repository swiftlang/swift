//===--- Reverse.swift - Lazy sequence reversal ---------------------------===//
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

// FIXME(ABI)(compiler limitation): we should have just one type,
// `ReversedCollection`, that has conditional conformances to
// `RandomAccessCollection`, and possibly `MutableCollection` and
// `RangeReplaceableCollection`.

// FIXME: swift-3-indexing-model - should gyb ReversedXxx & ReversedRandomAccessXxx

/// An index that traverses the same positions as an underlying index,
/// with inverted traversal direction.
public struct ReversedIndex<Base : Collection> : Comparable {
  public init(_ base: Base.Index) {
    self.base = base
  }

  /// The position corresponding to `self` in the underlying collection.
  public let base: Base.Index
}

@warn_unused_result
public func == <Base : Collection>(
  lhs: ReversedIndex<Base>,
  rhs: ReversedIndex<Base>
) -> Bool {
  return lhs.base == rhs.base
}

@warn_unused_result
public func < <Base : Collection>(
  lhs: ReversedIndex<Base>,
  rhs: ReversedIndex<Base>
) -> Bool {
  // Note ReversedIndex has inverted logic compared to base Base.Index
  return lhs.base > rhs.base
}

/// A Collection that presents the elements of its `Base` collection
/// in reverse order.
///
/// - Note: This type is the result of `x.reversed()` where `x` is a
///   collection having bidirectional indices.
///
/// The `reversed()` method is always lazy when applied to a collection
/// with bidirectional indices, but does not implicitly confer
/// laziness on algorithms applied to its result.  In other words, for
/// ordinary collections `c` having bidirectional indices:
///
/// * `c.reversed()` does not create new storage
/// * `c.reversed().map(f)` maps eagerly and returns a new array
/// * `c.lazy.reversed().map(f)` maps lazily and returns a `LazyMapCollection`
///
/// - See also: `ReversedRandomAccessCollection`
public struct ReversedCollection<
  Base : BidirectionalCollection
> : BidirectionalCollection {
  /// Creates an instance that presents the elements of `base` in
  /// reverse order.
  ///
  /// - Complexity: O(1)
  internal init(_base: Base) {
    self._base = _base
  }

  /// A type that represents a valid position in the collection.
  ///
  /// Valid indices consist of the position of every element and a
  /// "past the end" position that's not valid for use as a subscript.
  public typealias Index = ReversedIndex<Base>

  public typealias IndexDistance = Base.IndexDistance

  /// A type that provides the sequence's iteration interface and
  /// encapsulates its iteration state.
  public typealias Iterator = IndexingIterator<ReversedCollection>

  public var startIndex: Index {
    return ReversedIndex(_base.endIndex)
  }

  public var endIndex: Index {
    return ReversedIndex(_base.startIndex)
  }

  @warn_unused_result
  public func successor(of i: Index) -> Index {
    return ReversedIndex(_base.predecessor(of: i.base))
  }

  @warn_unused_result
  public func predecessor(of i: Index) -> Index {
    return ReversedIndex(_base.successor(of: i.base))
  }

  @warn_unused_result
  public func index(n: IndexDistance, stepsFrom i: Index) -> Index {
    // FIXME: swift-3-indexing-model: `-n` can trap on Int.min.
    return ReversedIndex(_base.index(-n, stepsFrom: i.base))
  }

  @warn_unused_result
  public func index(n: IndexDistance, stepsFrom i: Index, limitedBy limit: Index) -> Index {
    // FIXME: swift-3-indexing-model: `-n` can trap on Int.min.
    //return ReversedIndex(_base.index(-n, stepsFrom: i.base, limitedBy: ???)
    fatalError("FIXME: swift-3-indexing-model")
  }

  @warn_unused_result
  public func distance(from start: Index, to end: Index) -> IndexDistance {
    //return
    fatalError("FIXME: swift-3-indexing-model")
  }

  public typealias _Element = Base.Iterator.Element
  public subscript(position: Index) -> Base.Iterator.Element {
    return _base[_base.predecessor(of: position.base)]
  }

  public subscript(bounds: Range<Index>) -> BidirectionalSlice<ReversedCollection> {
    return BidirectionalSlice(base: self, bounds: bounds)
  }

  public let _base: Base
}

/// An index that traverses the same positions as an underlying index,
/// with inverted traversal direction.
public struct ReversedRandomAccessIndex<
  Base : RandomAccessCollection
> : Comparable {
  public init(_ base: Base.Index) {
    self.base = base
  }

  /// The position corresponding to `self` in the underlying collection.
  public let base: Base.Index
}

@warn_unused_result
public func == <Base : Collection>(
  lhs: ReversedRandomAccessIndex<Base>,
  rhs: ReversedRandomAccessIndex<Base>
) -> Bool {
  return lhs.base == rhs.base
}

@warn_unused_result
public func < <Base : Collection>(
  lhs: ReversedRandomAccessIndex<Base>,
  rhs: ReversedRandomAccessIndex<Base>
) -> Bool {
  // Note ReversedRandomAccessIndex has inverted logic compared to base Base.Index
  return lhs.base > rhs.base
}

/// A Collection that presents the elements of its `Base` collection
/// in reverse order.
///
/// - Note: This type is the result of `x.reversed()` where `x` is a
///   collection having random access indices.
/// - See also: `ReversedCollection`
public struct ReversedRandomAccessCollection<
  Base : RandomAccessCollection
> : RandomAccessCollection {
  // FIXME: swift-3-indexing-model: tests for ReverseRandomAccessIndex and
  // ReverseRandomAccessCollection.

  /// Creates an instance that presents the elements of `base` in
  /// reverse order.
  ///
  /// - Complexity: O(1)
  internal init(_base: Base) {
    self._base = _base
  }

  /// A type that represents a valid position in the collection.
  ///
  /// Valid indices consist of the position of every element and a
  /// "past the end" position that's not valid for use as a subscript.
  public typealias Index = ReversedRandomAccessIndex<Base>

  public typealias IndexDistance = Base.IndexDistance

  /// A type that provides the sequence's iteration interface and
  /// encapsulates its iteration state.
  public typealias Iterator = IndexingIterator<
    ReversedRandomAccessCollection
  >

  public var startIndex: Index {
    return ReversedRandomAccessIndex(_base.endIndex)
  }

  public var endIndex: Index {
    return ReversedRandomAccessIndex(_base.startIndex)
  }

  @warn_unused_result
  public func successor(of i: Index) -> Index {
    return ReversedRandomAccessIndex(_base.predecessor(of: i.base))
  }

  @warn_unused_result
  public func predecessor(of i: Index) -> Index {
    return ReversedRandomAccessIndex(_base.successor(of: i.base))
  }

  @warn_unused_result
  public func index(n: IndexDistance, stepsFrom i: Index) -> Index {
    // FIXME: swift-3-indexing-model: `-n` can trap on Int.min.
    // FIXME: swift-3-indexing-model: tests.
    return ReversedRandomAccessIndex(_base.index(-n, stepsFrom: i.base))
  }

  @warn_unused_result
  public func index(n: IndexDistance, stepsFrom i: Index, limitedBy limit: Index) -> Index {
    // FIXME: swift-3-indexing-model: `-n` can trap on Int.min.
    // FIXME: swift-3-indexing-model: tests.
    return Index(_base.index(-n, stepsFrom: i.base, limitedBy: limit.base))
  }

  @warn_unused_result
  public func distance(from start: Index, to end: Index) -> IndexDistance {
    // FIXME: swift-3-indexing-model: tests.
    return _base.distance(from: end.base, to: start.base)
  }

  public typealias _Element = Base.Iterator.Element
  // FIXME(compiler limitation): this typealias should be inferred.

  public subscript(position: Index) -> Base.Iterator.Element {
    return _base[_base.predecessor(of: position.base)]
  }

  // FIXME: swift-3-indexing-model: the rest of methods.

  public let _base: Base
}

extension BidirectionalCollection {
  /// Returns the elements of `self` in reverse order.
  ///
  /// - Complexity: O(1)
  @warn_unused_result
  public func reversed() -> ReversedCollection<Self> {
    return ReversedCollection(_base: self)
  }
}

extension RandomAccessCollection {
  /// Returns the elements of `self` in reverse order.
  ///
  /// - Complexity: O(1)
  @warn_unused_result
  public func reversed() -> ReversedRandomAccessCollection<Self> {
    return ReversedRandomAccessCollection(_base: self)
  }
}

extension LazyCollectionProtocol
  where
  Self : BidirectionalCollection,
  Elements : BidirectionalCollection {

  /// Returns the elements of `self` in reverse order.
  ///
  /// - Complexity: O(1)
  @warn_unused_result
  public func reversed() -> LazyBidirectionalCollection<
    ReversedCollection<Elements>
  > {
    return ReversedCollection(_base: elements).lazy
  }
}

extension LazyCollectionProtocol
  where
  Self : RandomAccessCollection,
  Elements : RandomAccessCollection {

  /// Returns the elements of `self` in reverse order.
  ///
  /// - Complexity: O(1)
  @warn_unused_result
  public func reversed() -> LazyRandomAccessCollection<
    ReversedRandomAccessCollection<Elements>
  > {
    return ReversedRandomAccessCollection(_base: elements).lazy
  }
}

extension ReversedCollection {
  @available(*, unavailable, message: "use the 'reversed()' method on the collection")
  public init(_ base: Base) {
    fatalError("unavailable function can't be called")
  }
}

extension ReversedRandomAccessCollection {
  @available(*, unavailable, message: "use the 'reversed()' method on the collection")
  public init(_ base: Base) {
    fatalError("unavailable function can't be called")
  }
}

extension BidirectionalCollection {
  @available(*, unavailable, renamed: "reversed")
  public func reverse() -> ReversedCollection<Self> {
    fatalError("unavailable function can't be called")
  }
}

extension RandomAccessCollection {
  @available(*, unavailable, renamed: "reversed")
  public func reverse() -> ReversedRandomAccessCollection<Self> {
    fatalError("unavailable function can't be called")
  }
}

extension LazyCollectionProtocol
  where
  Self : BidirectionalCollection,
  Elements : BidirectionalCollection
{

  @available(*, unavailable, renamed: "reversed")
  public func reverse() -> LazyCollection<
    ReversedCollection<Elements>
  > {
    fatalError("unavailable function can't be called")
  }
}

extension LazyCollectionProtocol
  where
  Self : RandomAccessCollection,
  Elements : RandomAccessCollection
{
  @available(*, unavailable, renamed: "reversed")
  public func reverse() -> LazyCollection<
    ReversedRandomAccessCollection<Elements>
  > {
    fatalError("unavailable function can't be called")
  }
}

// ${'Local Variables'}:
// eval: (read-only-mode 1)
// End:
