//===--- Reverse.swift - Lazy sequence reversal ---------------*- swift -*-===//
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

public protocol ReverseIndexType : BidirectionalIndexType {
  typealias Base : BidirectionalIndexType
  
  /// A type that can represent the number of steps between pairs of
  /// `ReverseIndex` values where one value is reachable from the other.
  typealias Distance: _SignedIntegerType = Base.Distance

  /// The successor position in the underlying (un-reversed)
  /// collection.
  ///
  /// If `self` is `advance(c.reverse.startIndex, n)`, then:
  /// - `self.base` is `advance(c.endIndex, -n)`.
  /// - if `n` != `c.count`, then `c.reverse[self]` is 
  ///   equivalent to `[self.base.predecessor()]`.
  var base: Base { get }

  init(_ base: Base)
}

extension BidirectionalIndexType where Self : ReverseIndexType {
  /// Returns the next consecutive value after `self`.
  ///
  /// - Requires: The next value is representable.
  public func successor() -> Self {
    return Self(base.predecessor())
  }

  /// Returns the previous consecutive value before `self`.
  ///
  /// - Requires: The previous value is representable.
  public func predecessor() -> Self {
    return Self(base.successor())
  }
}

/// A wrapper for a `BidirectionalIndexType` that reverses its
/// direction of traversal.
public struct ReverseIndex<Base: BidirectionalIndexType>
: BidirectionalIndexType, ReverseIndexType {
  public typealias Distance = Base.Distance
  
  public init(_ base: Base) { self.base = base }
  
  /// The successor position in the underlying (un-reversed)
  /// collection.
  ///
  /// If `self` is `advance(c.reverse.startIndex, n)`, then:
  /// - `self.base` is `advance(c.endIndex, -n)`.
  /// - if `n` != `c.count`, then `c.reverse[self]` is 
  ///   equivalent to `[self.base.predecessor()]`.
  public let base: Base

  @available(*, unavailable, renamed="Base")
  public typealias I = Base
}

@warn_unused_result
public func == <Base> (
  lhs: ReverseIndex<Base>, rhs: ReverseIndex<Base>
) -> Bool {
  return lhs.base == rhs.base
}

/// A wrapper for a `RandomAccessIndexType` that reverses its
/// direction of traversal.
public struct ReverseRandomAccessIndex<Base: RandomAccessIndexType>
  : RandomAccessIndexType, ReverseIndexType {

  public typealias Distance = Base.Distance
  
  public init(_ base: Base) { self.base = base }
  
  /// The successor position in the underlying (un-reversed)
  /// collection.
  ///
  /// If `self` is `advance(c.reverse.startIndex, n)`, then:
  /// - `self.base` is `advance(c.endIndex, -n)`.
  /// - if `n` != `c.count`, then `c.reverse[self]` is 
  ///   equivalent to `[self.base.predecessor()]`.
  public let base: Base

  public func distanceTo(other: ReverseRandomAccessIndex) -> Distance {
    return other.base.distanceTo(base)
  }

  public func advancedBy(n: Distance) -> ReverseRandomAccessIndex {
    return ReverseRandomAccessIndex(base.advancedBy(-n))
  }

  @available(*, unavailable, renamed="Base")
  public typealias I = Base
}

public protocol _ReverseCollectionType : CollectionType {
  typealias Index : ReverseIndexType
  typealias Base : CollectionType
  var _base: Base {get}
}

extension CollectionType
  where Self : _ReverseCollectionType, Self.Base.Index : RandomAccessIndexType {
  public var startIndex : ReverseRandomAccessIndex<Self.Base.Index> {
    return ReverseRandomAccessIndex(_base.endIndex)
  }
}

extension _ReverseCollectionType
  where Self : CollectionType, Self.Index.Base == Self.Base.Index
{
  public var startIndex : Index { return Self.Index(_base.endIndex) }
  public var endIndex : Index { return Self.Index(_base.startIndex) }
  public subscript(position: Index) -> Self.Base.Generator.Element {
    return _base[position.base.predecessor()]
  }
}

/// A Collection that presents the elements of its `Base` collection
/// in reverse order.
///
/// - Note: This type is the result of `x.reverse()` where `x` is a
///   collection having bidirectional indices.
///
/// The `reverse()` method is always lazy when applied to a collection
/// with bidirectional indices, but does not implicitly confer
/// laziness on algorithms applied to its result.  In other words, for
/// ordinary collections `c` having bidirectional indices:
///
/// * `c.reverse()` does not create new storage
/// * `c.reverse().map(f)` maps eagerly and returns a new array
/// * `c.lazy.reverse().map(f)` maps lazily and returns a `LazyMapCollection`
///
/// - See also: `ReverseRandomAccessCollection`
public struct ReverseCollection<
  Base : CollectionType where Base.Index : BidirectionalIndexType
> : CollectionType, _ReverseCollectionType {
  /// Creates an instance that presents the elements of `base` in
  /// reverse order.
  ///
  /// - Complexity: O(1)
  public init(_ base: Base) {
    self._base = base
  }

  /// A type that represents a valid position in the collection.
  ///
  /// Valid indices consist of the position of every element and a
  /// "past the end" position that's not valid for use as a subscript.
  public typealias Index = ReverseIndex<Base.Index>

  /// A type that provides the *sequence*'s iteration interface and
  /// encapsulates its iteration state.
  public typealias Generator = IndexingGenerator<ReverseCollection>
  
  public let _base: Base

  @available(*, unavailable, renamed="Base")
  public typealias T = Base
}

/// A Collection that presents the elements of its `Base` collection
/// in reverse order.
///
/// - Note: This type is the result of `x.reverse()` where `x` is a
///   collection having random access indices.
/// - See also: `ReverseCollection`
public struct ReverseRandomAccessCollection<
  Base : CollectionType where Base.Index : RandomAccessIndexType
> : _ReverseCollectionType {
  /// Creates an instance that presents the elements of `base` in
  /// reverse order.
  ///
  /// - Complexity: O(1)
  public init(_ base: Base) {
    self._base = base
  }

  /// A type that represents a valid position in the collection.
  ///
  /// Valid indices consist of the position of every element and a
  /// "past the end" position that's not valid for use as a subscript.
  public typealias Index = ReverseRandomAccessIndex<Base.Index>
  
  /// A type that provides the *sequence*'s iteration interface and
  /// encapsulates its iteration state.
  public typealias Generator = IndexingGenerator<
    ReverseRandomAccessCollection
  >

  public let _base: Base

  @available(*, unavailable, renamed="Base")
  public typealias T = Base
}

extension CollectionType where Index : BidirectionalIndexType {
  /// Return the elements of `self` in reverse order.
  ///
  /// - Complexity: O(1)
  @warn_unused_result
  public func reverse() -> ReverseCollection<Self> {
    return ReverseCollection(self)
  }
}

extension CollectionType where Index : RandomAccessIndexType {
  /// Return the elements of `self` in reverse order.
  ///
  /// - Complexity: O(1)
  @warn_unused_result
  public func reverse() -> ReverseRandomAccessCollection<Self> {
    return ReverseRandomAccessCollection(self)
  }
}

extension LazyCollectionType
where Index : BidirectionalIndexType, Elements.Index : BidirectionalIndexType {
  /// Return the elements of `self` in reverse order.
  ///
  /// - Complexity: O(1)
  @warn_unused_result
  public func reverse() -> LazyCollection<
    ReverseCollection<Elements>
  > {
    return ReverseCollection(elements).lazy
  }
}

extension LazyCollectionType
where Index : RandomAccessIndexType, Elements.Index : RandomAccessIndexType {
  /// Return the elements of `self` in reverse order.
  ///
  /// - Complexity: O(1)
  @warn_unused_result
  public func reverse() -> LazyCollection<
    ReverseRandomAccessCollection<Elements>
  > {
    return ReverseRandomAccessCollection(elements).lazy
  }
}

/// Return an `Array` containing the elements of `source` in reverse
/// order.
@available(*, unavailable, message="call the 'reverse()' method on the collection")
public func reverse<C:CollectionType where C.Index: BidirectionalIndexType>(
  source: C
) -> [C.Generator.Element] {
  fatalError("unavailable function can't be called")
}

@available(*, unavailable, renamed="ReverseCollection")
public struct BidirectionalReverseView<
  Base : CollectionType where Base.Index : BidirectionalIndexType
> {}

@available(*, unavailable, renamed="ReverseRandomAccessCollection")
public struct RandomAccessReverseView<
  Base : CollectionType where Base.Index : RandomAccessIndexType
> {}

// ${'Local Variables'}:
// eval: (read-only-mode 1)
// End:
