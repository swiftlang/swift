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
public func distance<T: ForwardIndexType>(start: T, end: T) -> T.Distance {
  return start~>_distanceTo(end)
}

/// Return the result of advancing `start` by `n` positions.  If `T`
/// models `RandomAccessIndexType`, executes in O(1).  Otherwise,
/// executes in O(`abs(n)`).  If `T` does not model
/// `BidirectionalIndexType`, requires that `n` is non-negative.
///
/// `advance(i, n)` is a synonym for `i++n'
public func advance<T: ForwardIndexType>(start: T, n: T.Distance) -> T {
  return start~>_advance(n)
}

/// Return the result of advancing start by `n` positions, or until it
/// equals `end`.  If `T` models `RandomAccessIndexType`, executes in
/// O(1).  Otherwise, executes in O(`abs(n)`).  If `T` does not model
/// `BidirectionalIndexType`, requires that `n` is non-negative.
public func advance<T: ForwardIndexType>(start: T, n: T.Distance, end: T) -> T {
  return start~>_advance(n, end)
}

/// Operation tags for distance and advance
///
/// Operation tags allow us to use a single operator (~>) for
/// dispatching every generic function with a default implementation.
/// Only authors of specialized distance implementations need to touch
/// this tag.
public struct _Distance {}
public func _distanceTo<I>(end: I) -> (_Distance, (I)) {
  return (_Distance(), (end))
}

public struct _Advance {}
public func _advance<D>(n: D) -> (_Advance, (D)) {
  return (_Advance(), (n: n))
}
public func _advance<D, I>(n: D, end: I) -> (_Advance, (D, I)) {
  return (_Advance(), (n, end))
}

//===----------------------------------------------------------------------===//
//===--- ForwardIndexType -------------------------------------------------===//

// Protocols with default implementations are broken into two parts, a
// base and a more-refined part.  From the user's point-of-view,
// however, _ForwardIndexType and ForwardIndexType should look like a single
// protocol.  This technique gets used throughout the standard library
// to break otherwise-cyclic protocol dependencies, which the compiler
// isn't yet smart enough to handle.

public protocol _Incrementable : Equatable {
  func successor() -> Self
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

public protocol _ForwardIndexType : _Incrementable {
  typealias Distance : _SignedIntegerType = Int

  // See the implementation of Range for an explanation of these
  // associated types.
  typealias _DisabledRangeIndex = _DisabledRangeIndex_
}

@transparent
public prefix func ++ <T : _Incrementable> (inout x: T) -> T {
  x = x.successor()
  return x
}

@transparent
public postfix func ++ <T : _Incrementable> (inout x: T) -> T {
  var ret = x
  x = x.successor()
  return ret
}

public protocol ForwardIndexType : _ForwardIndexType {
  // This requirement allows generic distance() to find default
  // implementations.  Only the author of F and the author of a
  // refinement of F having a non-default distance implementation need
  // to know about it.  These refinements are expected to be rare
  // (which is why defaulted requirements are a win)

  // Do not use these operators directly; call distance(start, end)
  // and advance(start, n) instead
  func ~> (start:Self, _ : (_Distance, Self)) -> Distance
  func ~> (start:Self, _ : (_Advance, Distance)) -> Self
  func ~> (start:Self, _ : (_Advance, (Distance, Self))) -> Self
}

// advance and distance implementations

/// Do not use this operator directly; call distance(start, end) instead
public
func ~> <T: _ForwardIndexType>(start:T, rest: (_Distance, T)) -> T.Distance {
  var p = start
  var count: T.Distance = 0
  let end = rest.1
  while p != end {
    ++count
    ++p
  }
  return count
}

/// Do not use this operator directly; call advance(start, n) instead
@transparent public
func ~> <T: _ForwardIndexType>(
  start: T, rest: (_Advance, T.Distance)
) -> T {
  let n = rest.1
  return _advanceForward(start, n)
}

internal
func _advanceForward<T: _ForwardIndexType>(start: T, n: T.Distance) -> T {
  _precondition(n >= 0,
      "Only BidirectionalIndexType can be advanced by a negative amount")
  var p = start
  for var i: T.Distance = 0; i != n; ++i {
    ++p
  }
  return p
}

/// Do not use this operator directly; call advance(start, n, end) instead
@transparent public
func ~> <T: _ForwardIndexType>(
  start:T, rest: ( _Advance, (T.Distance, T))
) -> T {
  return _advanceForward(start, rest.1.0, rest.1.1)
}

internal
func _advanceForward<T: _ForwardIndexType>(
  start: T, n: T.Distance, end: T
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
public protocol _BidirectionalIndexType : _ForwardIndexType {
  func predecessor() -> Self
}

public protocol BidirectionalIndexType 
  : ForwardIndexType, _BidirectionalIndexType {}

@transparent
public prefix func -- <T: _BidirectionalIndexType> (inout x: T) -> T {
  x = x.predecessor()
  return x
}


@transparent
public postfix func -- <T: _BidirectionalIndexType> (inout x: T) -> T {
  var ret = x
  x = x.predecessor()
  return ret
}

// advance implementation

/// Do not use this operator directly; call advance(start, n) instead
@transparent public
func ~> <T: _BidirectionalIndexType>(
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

/// Do not use this operator directly; call advance(start, n, end) instead
@transparent public
func ~> <T: _BidirectionalIndexType>(
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
public protocol _RandomAccessIndexType : _BidirectionalIndexType, Strideable {
  func distanceTo(Self) -> Distance
  func advancedBy(Distance) -> Self
}

public protocol RandomAccessIndexType
  : BidirectionalIndexType, _RandomAccessIndexType {
  /* typealias Distance : IntegerArithmeticType*/
}

// advance and distance implementations

/// Do not use this operator directly; call distance(start, end) instead
@transparent public
func ~> <T: _RandomAccessIndexType>(start:T, rest:(_Distance, (T)))
-> T.Distance {
  let end = rest.1
  return start.distanceTo(end)
}

/// Do not use this operator directly; call advance(start, n) instead
@transparent public
func ~> <T: _RandomAccessIndexType>(
  start:T, rest:(_Advance, (T.Distance))
) -> T {
  let n = rest.1
  return start.advancedBy(n)
}

/// Do not use this operator directly; call advance(start, n, end) instead
@transparent public
func ~> <T: _RandomAccessIndexType>(
  start:T, rest:(_Advance, (T.Distance, T))
) -> T {
  let n = rest.1.0
  let end = rest.1.1

  let d = start.distanceTo(end)
  var amount = n
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

