//===-- DropFirst.swift ---------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

/// A sequence that lazily consumes and drops `n` elements from an underlying
/// `Base` iterator before possibly returning the first available element.
///
/// The underlying iterator's sequence may be infinite.
///
/// This is a class - we require reference semantics to keep track
/// of how many elements we've already dropped from the underlying sequence.
@_versioned
@_fixed_layout
public class _DropFirstSequence<Base : IteratorProtocol>
    : Sequence, IteratorProtocol {

  @_versioned
  internal var _iterator: Base
  @_versioned
  internal let _limit: Int
  @_versioned
  internal var _dropped: Int

  @_versioned
  @_inlineable
  internal init(_iterator: Base, limit: Int, dropped: Int = 0) {
    self._iterator = _iterator
    self._limit = limit
    self._dropped = dropped
  }

  @_versioned
  @_inlineable
  internal func makeIterator() -> _DropFirstSequence<Base> {
    return self
  }

  @_versioned
  @_inlineable
  internal func next() -> Base.Element? {
    while _dropped < _limit {
      if _iterator.next() == nil {
        _dropped = _limit
        return nil
      }
      _dropped += 1
    }
    return _iterator.next()
  }

  @_versioned
  @_inlineable
  internal func dropFirst(_ n: Int) -> AnySequence<Base.Element> {
    // If this is already a _DropFirstSequence, we need to fold in
    // the current drop count and drop limit so no data is lost.
    //
    // i.e. [1,2,3,4].dropFirst(1).dropFirst(1) should be equivalent to
    // [1,2,3,4].dropFirst(2).
    return AnySequence(
      _DropFirstSequence(
        _iterator: _iterator, limit: _limit + n, dropped: _dropped))
  }
}
