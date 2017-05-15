//===-- Prefix.swift ------------------------------------------------------===//
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

/// A sequence that lazily consumes up to `n` elements from an underlying
/// `Base` iterator.
///
/// The underlying iterator's sequence may be infinite.
///
/// This is a class - we require reference semantics to keep track
/// of how many elements we've already taken from the underlying sequence.
@_fixed_layout
@_versioned
public class _PrefixSequence<Base : IteratorProtocol>
    : Sequence, IteratorProtocol {
  @_versioned
  internal let _maxLength: Int
  @_versioned
  internal var _iterator: Base
  @_versioned
  internal var _taken: Int

  @_versioned
  @_inlineable
  internal init(_iterator: Base, maxLength: Int, taken: Int = 0) {
    self._iterator = _iterator
    self._maxLength = maxLength
    self._taken = taken
  }

  @_versioned
  @_inlineable
  internal func makeIterator() -> _PrefixSequence<Base> {
    return self
  }

  @_versioned
  @_inlineable
  internal func next() -> Base.Element? {
    if _taken >= _maxLength { return nil }
    _taken += 1

    if let next = _iterator.next() {
      return next
    }

    _taken = _maxLength
    return nil
  }

  @_versioned
  @_inlineable
  internal func prefix(_ maxLength: Int) -> AnySequence<Base.Element> {
    return AnySequence(
      _PrefixSequence(
        _iterator: _iterator,
        maxLength: Swift.min(maxLength, self._maxLength),
        taken: _taken))
  }
}
