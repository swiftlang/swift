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

/// An index that traverses the same positions as an underlying index,
/// with inverted traversal direction.
public struct ReverseIndex<Base> : Comparable {
  public init(_ base: Base) { self.base = base }

  /// The successor position in the underlying (un-reversed)
  /// collection.
  ///
  /// If `self` is `advance(c.reverse.startIndex, n)`, then:
  /// - `self.base` is `advance(c.endIndex, -n)`.
  /// - if `n` != `c.count`, then `c.reverse[self]` is 
  ///   equivalent to `[self.base.predecessor()]`.
  public let base: Base
}

@warn_unused_result
public func == <Base> (
  lhs: ReverseIndex<Base>, rhs: ReverseIndex<Base>
) -> Bool {
  fatalError("FIXME: swift-3-indexing-model")
}

@warn_unused_result
public func < <Base> (
  lhs: ReverseIndex<Base>, rhs: ReverseIndex<Base>
) -> Bool {
  fatalError("FIXME: swift-3-indexing-model")
}
// FIXME: swift-3-indexing-model: forward all operations from Comparable
// for performance.

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
/// - See also: `ReverseRandomAccessCollection`
public struct ReverseCollection<
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
  public typealias Index = ReverseIndex<Base.Index>

  public typealias IndexDistance = Base.IndexDistance

  /// A type that provides the sequence's iteration interface and
  /// encapsulates its iteration state.
  public typealias Iterator = IndexingIterator<ReverseCollection>

  public var startIndex: Index {
    return _base.startIndex
  }

  public var endIndex: Index {
    return _base.endIndex
  }

  @warn_unused_result
  public func next(i: Index) -> Index {
    return Index(_base.previous(i.base))
  }

  @warn_unused_result
  public func previous(i: Index) -> Index {
    return Index(_base.next(i.base))
  }

  @warn_unused_result
  public func advance(i: Index, by n: IndexDistance) -> Index {
    // FIXME: swift-3-indexing-model: `-n` can trap on Int.min.
    return Index(_base.advance(i.base, by: -n))
  }

  @warn_unused_result
  public func advance(i: Index, by n: IndexDistance, limit: Index) -> Index {
    // FIXME: swift-3-indexing-model: `-n` can trap on Int.min.
    //return Index(_base.advance(i.base, by: -n, limit: ???)
    fatalError("FIXME: swift-3-indexing-model")
  }

  @warn_unused_result
  public func distance(from start: Index, to end: Index) -> IndexDistance {
    //return
    fatalError("FIXME: swift-3-indexing-model")
  }

  public typealias _Element = Base.Iterator.Element
  public subscript(position: Index) -> Base.Iterator.Element {
    return _base[_base.previous(position).base]
  }

  public let _base: Base
}

/// A Collection that presents the elements of its `Base` collection
/// in reverse order.
///
/// - Note: This type is the result of `x.reversed()` where `x` is a
///   collection having random access indices.
/// - See also: `ReverseCollection`
public struct ReverseRandomAccessCollection<
  Base : RandomAccessCollection
> : RandomAccessCollection {
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
  public typealias Index = ReverseIndex<Base.Index>

  public typealias IndexDistance = Base.IndexDistance

  /// A type that provides the sequence's iteration interface and
  /// encapsulates its iteration state.
  public typealias Iterator = IndexingIterator<
    ReverseRandomAccessCollection
  >

  public var startIndex: Index {
    return _base.startIndex
  }

  public var endIndex: Index {
    return _base.endIndex
  }

  @warn_unused_result
  public func next(i: Index) -> Index {
    return Index(_base.previous(i.base))
  }

  @warn_unused_result
  public func previous(i: Index) -> Index {
    return Index(_base.next(i.base))
  }

  @warn_unused_result
  public func advance(i: Index, by n: IndexDistance) -> Index {
    // FIXME: swift-3-indexing-model: `-n` can trap on Int.min.
    return Index(_base.advance(i.base, by: -n))
  }

  @warn_unused_result
  public func advance(i: Index, by n: IndexDistance, limit: Index) -> Index {
    // FIXME: swift-3-indexing-model: `-n` can trap on Int.min.
    //return Index(_base.advance(i.base, by: -n, limit: ???)
    fatalError("FIXME: swift-3-indexing-model")
  }

  @warn_unused_result
  public func distance(from start: Index, to end: Index) -> IndexDistance {
    //return
    fatalError("FIXME: swift-3-indexing-model")
  }

  public typealias _Element = Base.Iterator.Element
  // FIXME(compiler limitation): this typealias should be inferred.

  public subscript(position: Index) -> Base.Iterator.Element {
    return _base[_base.previous(position).base]
  }

  // FIXME: swift-3-indexing-model: the rest of methods.

  public let _base: Base
}

extension BidirectionalCollection {
  /// Returns the elements of `self` in reverse order.
  ///
  /// - Complexity: O(1)
  @warn_unused_result
  public func reversed() -> ReverseCollection<Self> {
    return ReverseCollection(_base: self)
  }
}

extension RandomAccessCollection {
  /// Returns the elements of `self` in reverse order.
  ///
  /// - Complexity: O(1)
  @warn_unused_result
  public func reversed() -> ReverseRandomAccessCollection<Self> {
    return ReverseRandomAccessCollection(_base: self)
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
  public func reversed() -> LazyCollection<
    ReverseCollection<Elements>
  > {
    return ReverseCollection(_base: elements).lazy
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
  public func reversed() -> LazyCollection<
    ReverseRandomAccessCollection<Elements>
  > {
    return ReverseRandomAccessCollection(_base: elements).lazy
  }
}

extension ReverseCollection {
  @available(*, unavailable, message="use the 'reversed()' method on the collection")
  public init(_ base: Base) {
    fatalError("unavailable function can't be called")
  }
}

extension ReverseRandomAccessCollection {
  @available(*, unavailable, message="use the 'reversed()' method on the collection")
  public init(_ base: Base) {
    fatalError("unavailable function can't be called")
  }
}

extension BidirectionalCollection {
  @available(*, unavailable, renamed="reversed")
  public func reverse() -> ReverseCollection<Self> {
    fatalError("unavailable function can't be called")
  }
}

extension RandomAccessCollection {
  @available(*, unavailable, renamed="reversed")
  public func reverse() -> ReverseRandomAccessCollection<Self> {
    fatalError("unavailable function can't be called")
  }
}

extension LazyCollectionProtocol
  where
  Self : BidirectionalCollection,
  Elements : BidirectionalCollection
{

  @available(*, unavailable, renamed="reversed")
  public func reverse() -> LazyCollection<
    ReverseCollection<Elements>
  > {
    fatalError("unavailable function can't be called")
  }
}

extension LazyCollectionProtocol
  where
  Self : RandomAccessCollection,
  Elements : RandomAccessCollection
{
  @available(*, unavailable, renamed="reversed")
  public func reverse() -> LazyCollection<
    ReverseRandomAccessCollection<Elements>
  > {
    fatalError("unavailable function can't be called")
  }
}

// ${'Local Variables'}:
// eval: (read-only-mode 1)
// End:
