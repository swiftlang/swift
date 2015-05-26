//===--- Index.swift - A position in a CollectionType ---------------------===//
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
//
//  ForwardIndexType, BidirectionalIndexType, and RandomAccessIndexType
//
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
//===--- Dispatching advance and distance functions -----------------------===//
// These generic functions are for user consumption; they dispatch to the
// appropriate implementation for T.

/// Measure the distance between `start` and `end`.
///
/// If `T` models `RandomAccessIndexType`, requires that `start` and `end` are
/// part of the same sequence, and executes in O(1).
///
/// Otherwise, requires that `end` is reachable from `start` by
/// incrementation, and executes in O(N), where N is the function's
/// result.
public func distance<T : ForwardIndexType>(start: T, _ end: T) -> T.Distance {
  return start._distanceTo(end)
}

/// Return the result of advancing `start` by `n` positions.  If `T`
/// models `RandomAccessIndexType`, executes in O(1).  Otherwise,
/// executes in O(`abs(n)`).  If `T` does not model
/// `BidirectionalIndexType`, requires that `n` is non-negative.
public func advance<T : ForwardIndexType>(start: T, _ n: T.Distance) -> T {
  return start~>_advance(n)
}

/// Return the result of advancing start by `n` positions, or until it
/// equals `end`.  If `T` models `RandomAccessIndexType`, executes in
/// O(1).  Otherwise, executes in O(`abs(n)`).  If `T` does not model
/// `BidirectionalIndexType`, requires that `n` is non-negative.
public func advance<T : ForwardIndexType>(start: T, _ n: T.Distance, _ end: T) -> T {
  return start~>_advance(n, end)
}

public struct _Advance {}
public func _advance<D>(n: D) -> (_Advance, (D)) {
  return (_Advance(), (n: n))
}
public func _advance<D, I>(n: D, _ end: I) -> (_Advance, (D, I)) {
  return (_Advance(), (n, end))
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
@transparent
public prefix func ++ <T : _Incrementable> (inout i: T) -> T {
  i._successorInPlace()
  return i
}

/// Replace `i` with its `successor()` and return the original
/// value of `i`.
@transparent
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

  // This requirement allows generic distance() to find default
  // implementations.  Only the author of F and the author of a
  // refinement of F having a non-default distance implementation need
  // to know about it.  These refinements are expected to be rare
  // (which is why defaulted requirements are a win)

  // Do not use these operators directly; call distance(start, end)
  // and advance(start, n) instead
  func _distanceTo(Self) -> Distance
  func ~> (start:Self, _ : (_Advance, Distance)) -> Self
  func ~> (start:Self, _ : (_Advance, (Distance, Self))) -> Self
}

// advance and distance implementations

extension ForwardIndexType {
  /// Do not use this operator directly; call distance(start, end) instead.
  public func _distanceTo(other: Self) -> Distance {
    var p = self
    var count: Distance = 0
    let end = other
    while p != end {
      ++count
      ++p
    }
    return count
  }
}

/// Do not use this operator directly; call advance(start, n) instead.
@transparent
public func ~> <T : ForwardIndexType>(
  start: T, rest: (_Advance, T.Distance)
) -> T {
  let n = rest.1
  return _advanceForward(start, n)
}

internal
func _advanceForward<T : ForwardIndexType>(start: T, _ n: T.Distance) -> T {
  _precondition(n >= 0,
      "Only BidirectionalIndexType can be advanced by a negative amount")
  var p = start
  for var i: T.Distance = 0; i != n; ++i {
    ++p
  }
  return p
}

/// Do not use this operator directly; call advance(start, n, end) instead.
@transparent
public func ~> <T : ForwardIndexType>(
  start:T, rest: ( _Advance, (T.Distance, T))
) -> T {
  return _advanceForward(start, rest.1.0, rest.1.1)
}

internal
func _advanceForward<T : ForwardIndexType>(
  start: T, _ n: T.Distance, _ end: T
) -> T {
  _precondition(n >= 0,
      "Only BidirectionalIndexType can be advanced by a negative amount")
  var p = start
  for var i: T.Distance = 0; i != n && p != end; ++i {
    ++p
  }
  return p
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
  func predecessor() -> Self

  mutating func _predecessorInPlace()
}

extension BidirectionalIndexType {
  @inline(__always)
  public mutating func _predecessorInPlace() { self = self.predecessor() }
}

/// Replace `i` with its `predecessor()` and return the updated value
/// of `i`.
@transparent
public prefix func -- <T : BidirectionalIndexType> (inout i: T) -> T {
  i._predecessorInPlace()
  return i
}


/// Replace `i` with its `predecessor()` and return the original
/// value of `i`.
@transparent
public postfix func -- <T : BidirectionalIndexType> (inout i: T) -> T {
  let ret = i
  i._predecessorInPlace()
  return ret
}

// advance implementation

/// Do not use this operator directly; call advance(start, n) instead.
@transparent
public func ~> <T : BidirectionalIndexType>(
  start:T , rest: (_Advance, T.Distance)
) -> T {
  let n = rest.1
  if n >= 0 {
    return _advanceForward(start, n)
  }
  var p = start
  for var i: T.Distance = n; i != 0; ++i {
    --p
  }
  return p
}

/// Do not use this operator directly; call advance(start, n, end) instead.
@transparent
public func ~> <T : BidirectionalIndexType>(
  start:T, rest: (_Advance, (T.Distance, T))
) -> T {
  let n = rest.1.0
  let end = rest.1.1

  if n >= 0 {
    return _advanceForward(start, n, end)
  }
  var p = start
  for var i: T.Distance = n; i != 0 && p != end; ++i {
    --p
  }
  return p
}

//===----------------------------------------------------------------------===//
//===--- RandomAccessIndexType --------------------------------------------===//

/// An *index* that can be offset by an arbitrary number of positions,
/// and can measure the distance to any reachable value, in O(1).
public protocol RandomAccessIndexType : BidirectionalIndexType, Strideable {
  /// Return the minimum number of applications of `successor` or
  /// `predecessor` required to reach `other` from `self`.
  ///
  /// - Complexity: O(1).
  ///
  /// Axioms:
  ///
  ///     x.distanceTo(x.successor())) == 1
  ///     x.distanceTo(x.predecessor())) == -1
  ///     x.advancedBy(x.distanceTo(y)) == y
  func distanceTo(other: Self) -> Distance

  /// Return `self` offset by `n` steps.
  ///
  /// - Returns: If `n > 0`, the result of applying `successor` to
  ///   `self` `n` times.  If `n < 0`, the result of applying
  ///   `predecessor` to `self` `n` times. Otherwise, `self`.
  ///
  /// - Complexity: O(1).
  ///
  /// Axioms:
  ///
  ///     x.advancedBy(0) == x
  ///     x.advancedBy(1) == x.successor()
  ///     x.advancedBy(-1) == x.predecessor()
  ///     x.distanceTo(x.advancedBy(m)) == m
  func advancedBy(n: Distance) -> Self
}

// advance and distance implementations

extension RandomAccessIndexType {
  /// Do not use this operator directly; call distance(start, end) instead.
  @transparent
  public func _distanceTo(other: Self) -> Distance {
    let end = other
    return self.distanceTo(end)
  }
}


/// Do not use this operator directly; call advance(start, n) instead.
@transparent
public func ~> <T : RandomAccessIndexType>(
  start:T, rest:(_Advance, (T.Distance))
) -> T {
  let n = rest.1
  return start.advancedBy(n)
}

/// Do not use this operator directly; call advance(start, n, end) instead.
@transparent
public func ~> <T : RandomAccessIndexType>(
  start:T, rest:(_Advance, (T.Distance, T))
) -> T {
  let n = rest.1.0
  let end = rest.1.1

  let d = start.distanceTo(end)
  let amount = n
  if n < 0 {
    if d < 0 && d > n {
      return end
    }
  }
  else {
    if d > 0 && d < n {
      return end
    }
  }
  return start.advancedBy(amount)
}

