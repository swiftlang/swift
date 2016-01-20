//===--- Filter.swift -----------------------------------------*- swift -*-===//
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

/// An iterator over the elements traversed by some base iterator that also
/// satisfy a given predicate.
///
/// - Note: This is the associated `Iterator` of `LazyFilterSequence`
/// and `LazyFilterCollection`.
public struct LazyFilterIterator<
  Base : IteratorProtocol
> : IteratorProtocol, Sequence {
  /// Advances to the next element and returns it, or `nil` if no next
  /// element exists.
  ///
  /// - Requires: `next()` has not been applied to a copy of `self`
  ///   since the copy was made, and no preceding call to `self.next()`
  ///   has returned `nil`.
  public mutating func next() -> Base.Element? {
    while let n = _base.next() {
      if _predicate(n) {
        return n
      }
    }
    return nil
  }

  /// Creates an instance that produces the elements `x` of `base`
  /// for which `predicate(x) == true`.
  internal init(
    _ base: Base,
    whereElementsSatisfy predicate: (Base.Element) -> Bool
  ) {
    self._base = base
    self._predicate = predicate
  }

  /// The underlying iterator whose elements are being filtered.
  public var base: Base { return _base }

  internal var _base: Base
  
  /// The predicate used to determine which elements produced by
  /// `base` are also produced by `self`.
  internal var _predicate: (Base.Element) -> Bool
}

/// A sequence whose elements consist of the elements of some base
/// sequence that also satisfy a given predicate.
///
/// - Note: `s.lazy.filter { ... }`, for an arbitrary sequence `s`,
///   is a `LazyFilterSequence`.
public struct LazyFilterSequence<Base : Sequence>
  : LazySequenceProtocol {
  
  /// Return an *iterator* over the elements of this *sequence*.
  ///
  /// - Complexity: O(1).
  public func iterator() -> LazyFilterIterator<Base.Iterator> {
    return LazyFilterIterator(
      base.iterator(), whereElementsSatisfy: _include)
  }

  /// Creates an instance consisting of the elements `x` of `base` for
  /// which `predicate(x) == true`.
  public // @testable
  init(
    _base base: Base,
    whereElementsSatisfy predicate: (Base.Iterator.Element) -> Bool
  ) {
    self.base = base
    self._include = predicate
  }

  /// The underlying sequence whose elements are being filtered
  public let base: Base

  /// The predicate used to determine which elements of `base` are
  /// also elements of `self`.
  internal let _include: (Base.Iterator.Element) -> Bool
}

/// The `Index` used for subscripting a `LazyFilterCollection`.
///
/// The positions of a `LazyFilterIndex` correspond to those positions
/// `p` in its underlying collection `c` such that `c[p]`
/// satisfies the predicate with which the `LazyFilterIndex` was
/// initialized.
/// 
/// - Note: The performance of advancing a `LazyFilterIndex`
///   depends on how sparsely the filtering predicate is satisfied,
///   and may not offer the usual performance given by models of
///   `ForwardIndex`.
public struct LazyFilterIndex<
  BaseElements: Collection
> : ForwardIndex {
  /// Returns the next consecutive value after `self`.
  ///
  /// - Requires: The next value is representable.
  ///
  /// - Complexity: Amortized O(M), where M is the average distance in
  ///   the base collection between elements that satisfy the
  ///   predicate.
  ///
  /// - Note: This operation may not satisfy the expected complexity
  ///   for models of `ForwardIndex`.
  public func successor() -> LazyFilterIndex {
    for p in base.successor()..<_baseElements.endIndex {
      if _include(_baseElements[p]) {
        return LazyFilterIndex(
          _baseElements: _baseElements, base: p, _include: _include)
      }
    }
    return LazyFilterIndex(
      _baseElements: _baseElements, base: _baseElements.endIndex,
      _include: _include)
  }

  internal let _baseElements: BaseElements

  /// The position corresponding to `self` in the underlying collection.
  public let base: BaseElements.Index

  /// The predicate used to determine which elements of `base` are
  /// also elements of `self`.
  internal let _include: (BaseElements.Iterator.Element) -> Bool
}

/// Returns `true` iff `lhs` is identical to `rhs`.
@warn_unused_result
public func == <Base : Collection>(
  lhs: LazyFilterIndex<Base>,
  rhs: LazyFilterIndex<Base>
) -> Bool {
  return lhs.base == rhs.base
}

/// A lazy `Collection` wrapper that includes the elements of an
/// underlying collection that satisfy a predicate.
///
/// - Note: The performance of accessing `startIndex`, `first`, any methods
///   that depend on `startIndex`, or of advancing a `LazyFilterIndex` depends
///   on how sparsely the filtering predicate is satisfied, and may not offer
///   the usual performance given by `Collection` or `ForwardIndex`. Be
///   aware, therefore, that general operations on `LazyFilterCollection`
///   instances may not have the documented complexity.
public struct LazyFilterCollection<
  Base : Collection
> : LazyCollectionProtocol {

  /// A type that represents a valid position in the collection.
  ///
  /// Valid indices consist of the position of every element and a
  /// "past the end" position that's not valid for use as a subscript.
  public typealias Index = LazyFilterIndex<Base>

  /// Construct an instance containing the elements of `base` that
  /// satisfy `predicate`.
  public // @testable
  init(
    _ base: Base,
    whereElementsSatisfy predicate: (Base.Iterator.Element) -> Bool
  ) {
    self._base = base
    self._predicate = predicate
  }

  /// The position of the first element in a non-empty collection.
  ///
  /// In an empty collection, `startIndex == endIndex`.
  ///
  /// - Complexity: O(N), where N is the ratio between unfiltered and
  ///   filtered collection lengths.
  public var startIndex: Index {
    var first = _base.startIndex
    while first != _base.endIndex {
      if _predicate(_base[first]) {
        break
      }
      first._successorInPlace()
    }
    return LazyFilterIndex(
      _baseElements: _base, base: first, _include: _predicate)
  }

  /// The collection's "past the end" position.
  ///
  /// `endIndex` is not a valid argument to `subscript`, and is always
  /// reachable from `startIndex` by zero or more applications of
  /// `successor()`.
  ///
  /// - Complexity: O(1).
  public var endIndex: Index {
    return LazyFilterIndex(
      _baseElements: _base, base: _base.endIndex, _include: _predicate)
  }

  /// Access the element at `position`.
  ///
  /// - Requires: `position` is a valid position in `self` and
  /// `position != endIndex`.
  public subscript(position: Index) -> Base.Iterator.Element {
    return _base[position.base]
  }

  /// Return an *iterator* over the elements of this *sequence*.
  ///
  /// - Complexity: O(1).
  public func iterator() -> LazyFilterIterator<Base.Iterator> {
    return LazyFilterIterator(
      _base.iterator(), whereElementsSatisfy: _predicate)
  }

  var _base: Base
  var _predicate: (Base.Iterator.Element) -> Bool
}

extension LazySequenceProtocol {
  /// Return the elements of `self` that satisfy `predicate`.
  ///
  /// - Note: The elements of the result are computed on-demand, as
  ///   the result is used. No buffering storage is allocated and each
  ///   traversal step invokes `predicate` on one or more underlying
  ///   elements.
  @warn_unused_result
  public func filter(
    predicate: (Elements.Iterator.Element) -> Bool
  ) -> LazyFilterSequence<Self.Elements> {
    return LazyFilterSequence(
      _base: self.elements, whereElementsSatisfy: predicate)
  }
}

extension LazyCollectionProtocol {
  /// Return the elements of `self` that satisfy `predicate`.
  ///
  /// - Note: The elements of the result are computed on-demand, as
  ///   the result is used. No buffering storage is allocated and each
  ///   traversal step invokes `predicate` on one or more underlying
  ///   elements.
  @warn_unused_result
  public func filter(
    predicate: (Elements.Iterator.Element) -> Bool
  ) -> LazyFilterCollection<Self.Elements> {
    return LazyFilterCollection(
      self.elements, whereElementsSatisfy: predicate)
  }
}

@available(*, unavailable, renamed="LazyFilterIterator")
public struct LazyFilterGenerator<Base : IteratorProtocol> {}

extension LazyFilterSequence {
  @available(*, unavailable, renamed="iterator")
  public func generate() -> LazyFilterIterator<Base.Iterator> {
    _abstract()
  }
}

extension LazyFilterCollection {
  @available(*, unavailable, renamed="iterator")
  public func generate() -> LazyFilterIterator<Base.Iterator> {
    _abstract()
  }
}

// ${'Local Variables'}:
// eval: (read-only-mode 1)
// End:
