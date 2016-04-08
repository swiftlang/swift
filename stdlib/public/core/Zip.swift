//===----------------------------------------------------------------------===//
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

/// A sequence of pairs built out of two underlying sequences, where
/// the elements of the `i`th pair are the `i`th elements of each
/// underlying sequence.
public func zip<Sequence1 : Sequence, Sequence2 : Sequence>(
  _ sequence1: Sequence1, _ sequence2: Sequence2
) -> Zip2Sequence<Sequence1, Sequence2> {
  return Zip2Sequence(_sequence1: sequence1, _sequence2: sequence2)
}

/// An iterator for `Zip2Sequence`.
public struct Zip2Iterator<
  Iterator1 : IteratorProtocol, Iterator2 : IteratorProtocol
> : IteratorProtocol {
  /// The type of element returned by `next()`.
  public typealias Element = (Iterator1.Element, Iterator2.Element)

  /// Construct around a pair of underlying iterators.
  internal init(_ iterator1: Iterator1, _ iterator2: Iterator2) {
    (_baseStream1, _baseStream2) = (iterator1, iterator2)
  }

  /// Advance to the next element and return it, or `nil` if no next
  /// element exists.
  ///
  /// - Precondition: `next()` has not been applied to a copy of `self`
  ///   since the copy was made, and no preceding call to `self.next()`
  ///   has returned `nil`.
  public mutating func next() -> Element? {
    // The next() function needs to track if it has reached the end. If we
    // didn't, and the first sequence is longer than the second, then when we
    // have already exhausted the second sequence, on every subsequent call to
    // next() we would consume and discard one additional element from the
    // first sequence, even though next() had already returned nil.

    if _reachedEnd {
      return nil
    }

    guard let element1 = _baseStream1.next(), element2 = _baseStream2.next() else {
      _reachedEnd = true
      return nil
    }

    return (element1, element2)
  }

  internal var _baseStream1: Iterator1
  internal var _baseStream2: Iterator2
  internal var _reachedEnd: Bool = false
}

/// A sequence of pairs built out of two underlying sequences, where
/// the elements of the `i`th pair are the `i`th elements of each
/// underlying sequence.
public struct Zip2Sequence<Sequence1 : Sequence, Sequence2 : Sequence>
  : Sequence {

  public typealias Stream1 = Sequence1.Iterator
  public typealias Stream2 = Sequence2.Iterator

  /// A type whose instances can produce the elements of this
  /// sequence, in order.
  public typealias Iterator = Zip2Iterator<Stream1, Stream2>

  @available(*, unavailable, renamed: "Iterator")
  public typealias Generator = Iterator

  /// Construct an instance that makes pairs of elements from `sequence1` and
  /// `sequence2`.
  public // @testable
  init(_sequence1 sequence1: Sequence1, _sequence2 sequence2: Sequence2) {
    (_sequence1, _sequence2) = (sequence1, sequence2)
  }

  /// Returns an iterator over the elements of this sequence.
  ///
  /// - Complexity: O(1).
  public func makeIterator() -> Iterator {
    return Iterator(
      _sequence1.makeIterator(),
      _sequence2.makeIterator())
  }

  internal let _sequence1: Sequence1
  internal let _sequence2: Sequence2
}
