//===-- DropWhile.swift ---------------------------------------------------===//
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
@_fixed_layout
@_versioned
public class _DropWhileSequence<Base : IteratorProtocol>
    : Sequence, IteratorProtocol {

      typealias Element = Base.Element

  @_versioned
  internal var _iterator: Base
  @_versioned
  internal var _nextElement: Base.Element?

  @_versioned
  @_inlineable
  internal init(
    iterator: Base,
    nextElement: Base.Element?,
    predicate: (Base.Element) throws -> Bool
  ) rethrows {
    self._iterator = iterator
    self._nextElement = nextElement ?? _iterator.next()

    while try _nextElement.flatMap(predicate) == true {
      _nextElement = _iterator.next()
    }
  }

  @_versioned
  @_inlineable
  internal func makeIterator() -> _DropWhileSequence<Base> {
    return self
  }

  @_versioned
  @_inlineable
  internal func next() -> Element? {
    guard _nextElement != nil else {
      return _iterator.next()
    }

    let next = _nextElement
    _nextElement = nil
    return next
  }

  @_versioned
  @_inlineable
  internal func drop(
    while predicate: (Element) throws -> Bool
  ) rethrows -> AnySequence<Element> {
    // If this is already a _DropWhileSequence, avoid multiple
    // layers of wrapping and keep the same iterator.
    return try AnySequence(
      _DropWhileSequence(
        iterator: _iterator, nextElement: _nextElement, predicate: predicate))
  }
}
