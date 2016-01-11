//===--- Index.swift - A position in a CollectionType ---------------------===//
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
//  ForwardIndexType, BidirectionalIndexType, and RandomAccessIndexType
//
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
//===--- Dispatching advance and distance functions -----------------------===//
// These generic functions are for user consumption; they dispatch to the
// appropriate implementation for T.

@available(*, unavailable, message="call the 'distanceTo(end)' method on the index")
public func distance<T : ForwardIndexType>(start: T, _ end: T) -> T.Distance {
  fatalError("unavailable function can't be called")
}

@available(*, unavailable, message="call the 'advancedBy(n)' method on the index")
public func advance<T : ForwardIndexType>(start: T, _ n: T.Distance) -> T {
  fatalError("unavailable function can't be called")
}

@available(*, unavailable, message="call the 'advancedBy(n, limit:)' method on the index")
public func advance<T : ForwardIndexType>(start: T, _ n: T.Distance, _ end: T) -> T {
  fatalError("unavailable function can't be called")
}

//===----------------------------------------------------------------------===//
//===--- ForwardIndexType -------------------------------------------------===//

/// This protocol is an implementation detail of `ForwardIndexType`; do
/// not use it directly.
///
/// Its requirements are inherited by `ForwardIndexType` and thus must
/// be satisfied by types conforming to that protocol.
public protocol _Incrementable : Equatable {
  /// Return the next consecutive value in a discrete sequence of
  /// `Self` values.
  ///
  /// - Requires: `self` has a well-defined successor.
  @warn_unused_result
  func successor() -> Self

  mutating func _successorInPlace()
}

extension _Incrementable {
  @inline(__always)
  public mutating func _successorInPlace() { self = self.successor() }
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

/// Replace `i` with its `successor()` and return the updated value of
/// `i`.
@_transparent
@available(*, deprecated, message="it will be removed in Swift 3")
public prefix func ++ <T : _Incrementable> (inout i: T) -> T {
  i._successorInPlace()
  return i
}

/// Replace `i` with its `successor()` and return the original
/// value of `i`.
@_transparent
@available(*, deprecated, message="it will be removed in Swift 3")
public postfix func ++ <T : _Incrementable> (inout i: T) -> T {
  let ret = i
  i._successorInPlace()
  return ret
}

/// Represents a discrete value in a series, where a value's
/// successor, if any, is reachable by applying the value's
/// `successor()` method.
public protocol ForwardIndexType : _Incrementable {
  /// A type that can represent the number of steps between pairs of
  /// `Self` values where one value is reachable from the other.
  ///
  /// Reachability is defined by the ability to produce one value from
  /// the other via zero or more applications of `successor`.
  typealias Distance : _SignedIntegerType = Int

  // See the implementation of Range for an explanation of this
  // associated type
  typealias _DisabledRangeIndex = _DisabledRangeIndex_

  /// Performs a range check in O(1), or a no-op when a range check is not
  /// implementable in O(1).
  ///
  /// The range check, if performed, is equivalent to:
  ///
  ///     precondition(bounds.contains(index))
  ///
  /// Use this function to perform a cheap range check for QoI purposes when
  /// memory safety is not a concern.  Do not rely on this range check for
  /// memory safety.
  ///
  /// The default implementation for forward and bidirectional indices is a
  /// no-op.  The default implementation for random access indices performs a
  /// range check.
  ///
  /// - Complexity: O(1).
  static func _failEarlyRangeCheck(index: Self, bounds: Range<Self>)

  /// Performs a range check in O(1), or a no-op when a range check is not
  /// implementable in O(1).
  ///
  /// The range check, if performed, is equivalent to:
  ///
  ///     precondition(
  ///       bounds.contains(range.startIndex) ||
  ///       range.startIndex == bounds.endIndex)
  ///     precondition(
  ///       bounds.contains(range.endIndex) ||
  ///       range.endIndex == bounds.endIndex)
  ///
  /// Use this function to perform a cheap range check for QoI purposes when
  /// memory safety is not a concern.  Do not rely on this range check for
  /// memory safety.
  ///
  /// The default implementation for forward and bidirectional indices is a
  /// no-op.  The default implementation for random access indices performs a
  /// range check.
  ///
  /// - Complexity: O(1).
  static func _failEarlyRangeCheck2(
    rangeStart: Self, rangeEnd: Self, boundsStart: Self, boundsEnd: Self)
  // FIXME: the suffix `2` in the name, and passing `startIndex` and `endIndex`
  // separately (rather than as a range) are workarounds for a compiler defect.
  // <rdar://problem/21855350> Rejects-valid: rejects code that has two Self
  // types in non-direct-argument-type position

  /// Return the result of advancing `self` by `n` positions.
  ///
  /// - Returns:
  ///   - If `n > 0`, the result of applying `successor` to `self` `n` times.
  ///   - If `n < 0`, the result of applying `predecessor` to `self` `-n` times.
  ///   - Otherwise, `self`.
  ///
  /// - Requires: `n >= 0` if only conforming to `ForwardIndexType`
  /// - Complexity:
  ///   - O(1) if conforming to `RandomAccessIndexType`
  ///   - O(`abs(n)`) otherwise
  @warn_unused_result
  func advancedBy(n: Distance) -> Self

  /// Return the result of advancing `self` by `n` positions, or until it
  /// equals `limit`.
  ///
  /// - Returns:
  ///   - If `n > 0`, the result of applying `successor` to `self` `n` times
  ///     but not past `limit`.
  ///   - If `n < 0`, the result of applying `predecessor` to `self` `-n` times
  ///     but not past `limit`.
  ///   - Otherwise, `self`.
  ///
  /// - Requires: `n >= 0` if only conforming to `ForwardIndexType`.
  ///
  /// - Complexity:
  ///   - O(1) if conforming to `RandomAccessIndexType`
  ///   - O(`abs(n)`) otherwise
  @warn_unused_result
  func advancedBy(n: Distance, limit: Self) -> Self

  /// Measure the distance between `self` and `end`.
  ///
  /// - Requires:
  ///   - `start` and `end` are part of the same sequence when conforming to
  ///     `RandomAccessSequenceType`.
  ///   - `end` is reachable from `self` by incrementation otherwise.
  ///
  /// - Complexity:
  ///   - O(1) if conforming to `RandomAccessIndexType`
  ///   - O(`n`) otherwise, where `n` is the function's result.
  @warn_unused_result
  func distanceTo(end: Self) -> Distance
}

// advance and distance implementations

extension ForwardIndexType {
  public static func _failEarlyRangeCheck(index: Self, bounds: Range<Self>) {
    // Can't perform range checks in O(1) on forward indices.
  }

  public static func _failEarlyRangeCheck2(
    rangeStart: Self, rangeEnd: Self, boundsStart: Self, boundsEnd: Self
  ) {
    // Can't perform range checks in O(1) on forward indices.
  }

  /// Do not use this method directly; call advancedBy(n) instead.
  @_transparent
  @warn_unused_result
  internal func _advanceForward(n: Distance) -> Self {
    _precondition(n >= 0,
        "Only BidirectionalIndexType can be advanced by a negative amount")
    var p = self
    var i : Distance = 0
    while i != n {
      p._successorInPlace()
      i = i + 1
    }
    return p
  }

  /// Do not use this method directly; call advancedBy(n, limit) instead.
  @_transparent
  @warn_unused_result
  internal func _advanceForward(n: Distance, _ limit: Self) -> Self {
    _precondition(n >= 0,
        "Only BidirectionalIndexType can be advanced by a negative amount")
    var p = self
    var i : Distance = 0
    while i != n {
      if p == limit { break }
      p._successorInPlace()
      i = i + 1
    }
    return p
  }

  @warn_unused_result
  public func advancedBy(n: Distance) -> Self {
    return self._advanceForward(n)
  }

  @warn_unused_result
  public func advancedBy(n: Distance, limit: Self) -> Self {
    return self._advanceForward(n, limit)
  }

  @warn_unused_result
  public func distanceTo(end: Self) -> Distance {
    var p = self
    var count: Distance = 0
    while p != end {
      count += 1
      p._successorInPlace()
    }
    return count
  }
}

//===----------------------------------------------------------------------===//
//===--- BidirectionalIndexType -------------------------------------------===//


/// An *index* that can step backwards via application of its
/// `predecessor()` method.
public protocol BidirectionalIndexType : ForwardIndexType {
  /// Return the previous consecutive value in a discrete sequence.
  ///
  /// If `self` has a well-defined successor,
  /// `self.successor().predecessor() == self`.  If `self` has a
  /// well-defined predecessor, `self.predecessor().successor() ==
  /// self`.
  ///
  /// - Requires: `self` has a well-defined predecessor.
  @warn_unused_result
  func predecessor() -> Self

  mutating func _predecessorInPlace()
}

extension BidirectionalIndexType {
  @inline(__always)
  public mutating func _predecessorInPlace() {
    self = self.predecessor()
  }

  @warn_unused_result
  public func advancedBy(n: Distance) -> Self {
    if n >= 0 {
      return _advanceForward(n)
    }
    var p = self
    var i: Distance = n
    while i != 0 {
      p._predecessorInPlace()
      i._successorInPlace()
    }
    return p
  }

  @warn_unused_result
  public func advancedBy(n: Distance, limit: Self) -> Self {
    if n >= 0 {
      return _advanceForward(n, limit)
    }
    var p = self
    var i: Distance = n
    while i != 0 && p != limit {
      p._predecessorInPlace()
      i._successorInPlace()
    }
    return p
  }
}

/// Replace `i` with its `predecessor()` and return the updated value
/// of `i`.
@_transparent
@available(*, deprecated, message="it will be removed in Swift 3")
public prefix func -- <T : BidirectionalIndexType> (inout i: T) -> T {
  i._predecessorInPlace()
  return i
}


/// Replace `i` with its `predecessor()` and return the original
/// value of `i`.
@_transparent
@available(*, deprecated, message="it will be removed in Swift 3")
public postfix func -- <T : BidirectionalIndexType> (inout i: T) -> T {
  let ret = i
  i._predecessorInPlace()
  return ret
}

//===----------------------------------------------------------------------===//
//===--- RandomAccessIndexType --------------------------------------------===//

/// Used to force conformers of RandomAccessIndexType to implement
/// `advancedBy` methods and `distanceTo`.
public protocol _RandomAccessAmbiguity {
  typealias Distance : _SignedIntegerType = Int
}

extension _RandomAccessAmbiguity {
  @warn_unused_result
  public func advancedBy(n: Distance) -> Self {
    fatalError("advancedBy(n) not implemented")
  }
}

/// An *index* that can be offset by an arbitrary number of positions,
/// and can measure the distance to any reachable value, in O(1).
public protocol RandomAccessIndexType : BidirectionalIndexType, Strideable,
  _RandomAccessAmbiguity {

  @warn_unused_result
  func distanceTo(other: Self) -> Distance

  @warn_unused_result
  func advancedBy(n: Distance) -> Self

  @warn_unused_result
  func advancedBy(n: Distance, limit: Self) -> Self
}

extension RandomAccessIndexType {
  public static func _failEarlyRangeCheck(index: Self, bounds: Range<Self>) {
    _precondition(
      bounds.startIndex <= index,
      "index is out of bounds: index designates a position before bounds.startIndex")
    _precondition(
      index < bounds.endIndex,
      "index is out of bounds: index designates the bounds.endIndex position or a position after it")
  }

  public static func _failEarlyRangeCheck2(
    rangeStart: Self, rangeEnd: Self, boundsStart: Self, boundsEnd: Self
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
  public func advancedBy(n: Distance, limit: Self) -> Self {
    let d = self.distanceTo(limit)
    if d == 0 || (d > 0 ? d <= n : d >= n) {
      return limit
    }
    return self.advancedBy(n)
  }
}
