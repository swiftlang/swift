//===----------------------------------------------------------------------===//
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

/// A sequence of pairs built out of two underlying sequences, where
/// the elements of the `i`th pair are the `i`th elements of each
/// underlying sequence.
public func zip<Sequence1 : SequenceType, Sequence2 : SequenceType>(
  sequence1: Sequence1, _ sequence2: Sequence2
) -> Zip2Sequence<Sequence1, Sequence2> {
  return Zip2Sequence(sequence1, sequence2)
}

/// An iterator for `Zip2Sequence`.
public struct Zip2Iterator<
  Iterator1 : IteratorProtocol, Iterator2 : IteratorProtocol
> : IteratorProtocol {
  /// The type of element returned by `next()`.
  public typealias Element = (Iterator1.Element, Iterator2.Element)

  /// Construct around a pair of underlying iterators.
  public init(_ iterator1: Iterator1, _ iterator2: Iterator2) {
    _baseStreams = (iterator1, iterator2)
  }

  /// Advance to the next element and return it, or `nil` if no next
  /// element exists.
  ///
  /// - Requires: `next()` has not been applied to a copy of `self`
  ///   since the copy was made, and no preceding call to `self.next()`
  ///   has returned `nil`.
  public mutating func next() -> Element? {
    // The next() function needs to track if it has reached the end.  If we
    // didn't, and the first sequence is shorter than the second, then, when we
    // have already exhausted the second sequence, on every subsequent call to
    // next() we would consume and discard one additional element from the
    // first sequence, even though next() return nil.

    if _reachedEnd {
      return nil
    }

    guard let e0 = _baseStreams.0.next() else {
      _reachedEnd = true
      return nil
    }

    guard let e1 = _baseStreams.1.next() else {
      _reachedEnd = true
      return nil
    }

    return .Some((e0, e1))
  }

  internal var _baseStreams: (Iterator1, Iterator2)
  internal var _reachedEnd: Bool = false
}

/// A sequence of pairs built out of two underlying sequences, where
/// the elements of the `i`th pair are the `i`th elements of each
/// underlying sequence.
public struct Zip2Sequence<Sequence1 : SequenceType, Sequence2 : SequenceType>
  : SequenceType {

  public typealias Stream1 = Sequence1.Iterator
  public typealias Stream2 = Sequence2.Iterator

  /// A type whose instances can produce the elements of this
  /// sequence, in order.
  public typealias Iterator = Zip2Iterator<Stream1, Stream2>

  /// Construct an instance that makes pairs of elements from `sequence1` and
  /// `sequence2`.
  public init(_ sequence1: Sequence1, _ sequence2: Sequence2) {
    _sequences = (sequence1, sequence2)
  }

  /// Return an *iterator* over the elements of this *sequence*.
  ///
  /// - Complexity: O(1).
  public func generate() -> Iterator {
    return Iterator(
      _sequences.0.generate(),
      _sequences.1.generate())
  }

  internal let _sequences: (Sequence1, Sequence2)
}

