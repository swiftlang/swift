//===--- Index.swift - A position in a Collection ---------------------===//
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
//  ForwardIndex, BidirectionalIndex, and RandomAccessIndex
//
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
//===--- ForwardIndex -------------------------------------------------===//

/// This protocol is an implementation detail of `ForwardIndex`; do
/// not use it directly.
///
/// Its requirements are inherited by `ForwardIndex` and thus must
/// be satisfied by types conforming to that protocol.
public protocol _Incrementable : Equatable {
  /// Returns the next consecutive value in a discrete sequence of
  /// `Self` values.
  ///
  /// - Precondition: `self` has a well-defined successor.
  @warn_unused_result
  func successor() -> Self // DONE: swift-3-indexing-model - replicated
}

extension _Incrementable {
  @inline(__always)
  public mutating func _successorInPlace() { self = self.successor() } // DONE: swift-3-indexing-model - replicated
}

//===----------------------------------------------------------------------===//
// A dummy type that we can use when we /don't/ want to create an
// ambiguity indexing Range<T> outside a generic context.  See the
// implementation of Range for details.
public struct _DisabledRangeIndex_ {
  init() {
    _sanityCheckFailure("Nobody should ever create one.")
  }
}

//===----------------------------------------------------------------------===//

/// Represents a discrete value in a series, where a value's
/// successor, if any, is reachable by applying the value's
/// `successor()` method.

// TODO: swift-3-indexing-model - will become unavailable once work is completed
//@available(*, unavailable, message="subsumed by Collection in Swift 3")
public protocol ForwardIndex : _Incrementable {
  /// A type that can represent the number of steps between pairs of
  /// `Self` values where one value is reachable from the other.
  ///
  /// Reachability is defined by the ability to produce one value from
  /// the other via zero or more applications of `successor`.
  associatedtype Distance : _SignedInteger = Int

  // See the implementation of Range for an explanation of this
  // associated type
  associatedtype _DisabledRangeIndex = _DisabledRangeIndex_
}

//===----------------------------------------------------------------------===//
//===--- BidirectionalIndex -------------------------------------------===//

// TODO: swift-3-indexing-model - will become unavailable once work is completed
//@available(*, unavailable, message="subsumed by Collection in Swift 3")
public protocol BidirectionalIndex : ForwardIndex {
  /// Returns the previous consecutive value in a discrete sequence.
  ///
  /// If `self` has a well-defined successor,
  /// `self.successor().predecessor() == self`.  If `self` has a
  /// well-defined predecessor, `self.predecessor().successor() ==
  /// self`.
  ///
  /// - Precondition: `self` has a well-defined predecessor.
  @warn_unused_result
  func predecessor() -> Self  // DONE: swift-3-indexing-model - replicated

  mutating func _predecessorInPlace()  // DONE: swift-3-indexing-model - replicated
}

extension BidirectionalIndex {
  @inline(__always)
  public mutating func _predecessorInPlace() { // DONE: swift-3-indexing-model - replicated
    self = self.predecessor()
  }
}

//===----------------------------------------------------------------------===//
//===--- RandomAccessIndex ------------------------------------------------===//

/// Used to force conformers of RandomAccessIndex to implement
/// `advanced(by:)` methods and `distance(to:)`.
public protocol _RandomAccessAmbiguity {
  associatedtype Distance : _SignedInteger = Int
}

extension _RandomAccessAmbiguity {
  @warn_unused_result
  public func advanced(by n: Distance) -> Self {
    fatalError("advanced(by:) not implemented")
  }
}

/// An index that can be offset by an arbitrary number of positions,
/// and can measure the distance to any reachable value, in O(1).

// TODO: swift-3-indexing-model - will become unavailable once work is completed
//@available(*, deprecated, message="it will be subsumed by collections implementing RandomAccessCollection in Swift 3")

public protocol RandomAccessIndex : BidirectionalIndex, Strideable,
  _RandomAccessAmbiguity {

  @warn_unused_result
  func distance(to other: Self) -> Distance

  @warn_unused_result
  func advanced(by n: Distance) -> Self

  @warn_unused_result
  func advanced(by n: Distance, limit: Self) -> Self
}

extension RandomAccessIndex {
  public static func _failEarlyRangeCheck(index: Self, bounds: Range<Self>) {
    _precondition(
      bounds.startIndex <= index,
      "index is out of bounds: index designates a position before bounds.startIndex")
    _precondition(
      index < bounds.endIndex,
      "index is out of bounds: index designates the bounds.endIndex position or a position after it")
  }

  public static func _failEarlyRangeCheck2(
    rangeStart rangeStart: Self,
    rangeEnd: Self,
    boundsStart: Self,
    boundsEnd: Self
  ) {
    let range = rangeStart..<rangeEnd
    let bounds = boundsStart..<boundsEnd
    _precondition(
      bounds.startIndex <= range.startIndex,
      "range.startIndex is out of bounds: index designates a position before bounds.startIndex")
    _precondition(
      bounds.startIndex <= range.endIndex,
      "range.endIndex is out of bounds: index designates a position before bounds.startIndex")

    _precondition(
      range.startIndex <= bounds.endIndex,
      "range.startIndex is out of bounds: index designates a position after bounds.endIndex")
    _precondition(
      range.endIndex <= bounds.endIndex,
      "range.startIndex is out of bounds: index designates a position after bounds.endIndex")
  }

  @_transparent
  @warn_unused_result
  public func advanced(by n: Distance, limit: Self) -> Self {
    let d = self.distance(to: limit)
    if d == 0 || (d > 0 ? d <= n : d >= n) {
      return limit
    }
    return self.advanced(by: n)
  }
}

@available(*, unavailable, message="Please use your collections prior(Index) function")
public prefix func -- <T : BidirectionalIndex> (inout i: T) -> T {
  fatalError("unavailable operator can't be called")
}

@available(*, unavailable, message="Please use your collections prior(Index) function")
public postfix func -- <T : BidirectionalIndex> (inout i: T) -> T {
  fatalError("unavailable operator can't be called")
}

@available(*, unavailable, message="Please use your collections next(Index) function")
public prefix func ++ <T : _Incrementable> (inout i: T) -> T {
  fatalError("unavailable operator can't be called")
}

@available(*, unavailable, message="Please use your collections next(Index) function")
public postfix func ++ <T : _Incrementable> (inout i: T) -> T {
  fatalError("unavailable operator can't be called")
}

@available(*, unavailable, renamed="ForwardIndex")
public typealias ForwardIndexType = ForwardIndex

@available(*, unavailable, renamed="BidirectionalIndex")
public typealias BidirectionalIndexType = BidirectionalIndex

@available(*, unavailable, renamed="RandomAccessIndex")
public typealias RandomAccessIndexType = RandomAccessIndex
