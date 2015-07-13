// RUN: %target-run-stdlib-swift
// REQUIRES: executable_test

import StdlibUnittest

//===--- Filter.swift -----------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

/// The lazy `SequenceType` returned by `filter(c)` where `c` is a
/// `SequenceType`.
public struct _prext_FilterSequence<Base : SequenceType> : _prext_LazySequenceType {
  /// Return a *generator* over the elements of this *sequence*.
  ///
  /// - Complexity: O(1).
  public func generate() -> FilterGenerator<Base.Generator> {
    return FilterGenerator(_base: _base.generate(), _include: _include)
  }

  var _base: Base
  var _include: (Base.Generator.Element)->Bool
}

/// The `Index` used for subscripting a `_prext_FilterCollection`.
///
/// - Note: The performance of advancing a `_prext_FilterIndex` depends on
///   how sparsely the filtering predicate is satisfied, and its
///   conformance to `ForwardIndexType` needs to be understood in that
///   context.
public struct _prext_FilterIndex<
  Base: CollectionType
> : ForwardIndexType {
  /// Returns the next consecutive value after `self`.
  ///
  /// - Requires: The next value is representable.
  /// - Complexity: O(M), where M is the number of following elements
  ///   in the underlying sequence that don't satisfy the predicate.
  public func successor() -> _prext_FilterIndex {
    for nextPos in _pos.successor()..<_end {
      if _include(_base[nextPos]) {
        return _prext_FilterIndex(
          _pos: nextPos, _end: _end,
          _base: _base, _include: _include)
      }
    }
    return _prext_FilterIndex(
      _pos: _end, _end: _end, _base: _base, _include: _include)
  }
  
  internal var _pos: Base.Index
  internal var _end: Base.Index
  internal var _base: Base
  internal var _include: (Base.Generator.Element)->Bool
}

/// Returns `true` iff `lhs` is identical to `rhs`.
public func == <Base: CollectionType>(
  lhs: _prext_FilterIndex<Base>,
  rhs: _prext_FilterIndex<Base>
) -> Bool {
  return lhs._pos == rhs._pos
}

/// A lazy `CollectionType` wrapper that includes the elements of an
/// underlying collection that satisfy a predicate.  Not
/// automatically returned by `filter(x)` for two reasons:
///
/// * The O(1) guarantee of our `Index` would be iffy at best, since
///   it advances an underlying `Index` until the predicate is
///   satisfied.  Be aware that a `_prext_FilterCollection` may not offer
///   the expected efficiency for this reason.
///
/// * Constructing an `Array` from a `CollectionType` measures the length
///   of the collection before traversing it to read the elements.
///   This causes the filter predicate to be called twice for each
///   element of the underlying collection, which is surprising.
public struct _prext_FilterCollection<Base : CollectionType> : _prext_LazyCollectionType {

  /// A type that represents a valid position in the collection.
  ///
  /// Valid indices consist of the position of every element and a
  /// "past the end" position that's not valid for use as a subscript.
  public typealias Index = _prext_FilterIndex<Base>

  /// Construct an instance containing the elements of `base` that
  /// satisfy `predicate`.
  public init(
    _ base: Base,
    includeElement predicate: (Base.Generator.Element)->Bool
  ) {
    self._base = base
    self._include = predicate
  }

  /// The position of the first element in a non-empty collection.
  ///
  /// In an empty collection, `startIndex == endIndex`.
  ///
  /// - Complexity: O(N), where N is the ratio between unfiltered and
  ///   filtered collection counts.
  public var startIndex: Index {
    var first = _base.startIndex
    while first != _base.endIndex {
      if _include(_base[first]) {
        break
      }
      ++first
    }
    return _prext_FilterIndex(
      _pos: first, _end: _base.endIndex, _base: _base, _include: _include)
  }

  /// The collection's "past the end" position.
  ///
  /// `endIndex` is not a valid argument to `subscript`, and is always
  /// reachable from `startIndex` by zero or more applications of
  /// `successor()`.
  ///
  /// - Complexity: O(1).
  public var endIndex: Index {
    return _prext_FilterIndex(
      _pos: _base.endIndex, _end: _base.endIndex,
      _base: _base, _include: _include)
  }

  /// Access the element at `position`.
  ///
  /// - Requires: `position` is a valid position in `self` and
  /// `position != endIndex`.
  public subscript(position: Index) -> Base.Generator.Element {
    return _base[position._pos]
  }

  /// Return a *generator* over the elements of this *sequence*.
  ///
  /// - Complexity: O(1).
  public func generate() -> FilterGenerator<Base.Generator> {
    return FilterGenerator(_base: _base.generate(), _include: _include)
  }

  var _base: Base
  var _include: (Base.Generator.Element)->Bool
}

extension _prext_LazySequenceType {
  /// Return a `_prext_FilterSequence` that uses this sequence as a base.
  /// The elements to be included in the result are computed lazily,
  /// each time the result is traversed, by calling the
  /// `includeElement` function on elements of the base sequence.
  public func filter(
    includeElement: (Elements.Generator.Element)->Bool
  ) -> _prext_FilterSequence<Self.Elements> {
    return _prext_FilterSequence(_base: self.elements, _include: includeElement)
  }
}

runAllTests()
