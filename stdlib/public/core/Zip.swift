//===----------------------------------------------------------------------===//
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

/// Creates a sequence of pairs built out of two underlying sequences.
///
/// In the `Zip2Sequence` instance returned by this function, the elements of
/// the *i*th pair are the *i*th elements of each underlying sequence. The
/// following example uses the `zip(_:_:)` function to iterate over an array
/// of strings and a countable range at the same time:
///
///     let words = ["one", "two", "three", "four"]
///     let numbers = 1...4
///
///     for (word, number) in zip(words, numbers) {
///         print("\(word): \(number)")
///     }
///     // Prints "one: 1"
///     // Prints "two: 2"
///     // Prints "three: 3"
///     // Prints "four: 4"
///
/// If the two sequences passed to `zip(_:_:)` are different lengths, the
/// resulting sequence is the same length as the shorter sequence. In this
/// example, the resulting array is the same length as `words`:
///
///     let naturalNumbers = 1...Int.max
///     let zipped = Array(zip(words, naturalNumbers))
///     // zipped == [("one", 1), ("two", 2), ("three", 3), ("four", 4)]
///
/// - Parameters:
///   - sequence1: The first sequence or collection to zip.
///   - sequence2: The second sequence or collection to zip.
/// - Returns: A sequence of tuple pairs, where the elements of each pair are
///   corresponding elements of `sequence1` and `sequence2`.
@inlinable // generic-performance
public func zip<Sequence1, Sequence2>(
  _ sequence1: Sequence1, _ sequence2: Sequence2
) -> Zip2Sequence<Sequence1, Sequence2> {
  return Zip2Sequence(sequence1, sequence2)
}

/// A sequence of pairs built out of two underlying sequences.
///
/// In a `Zip2Sequence` instance, the elements of the *i*th pair are the *i*th
/// elements of each underlying sequence. To create a `Zip2Sequence` instance,
/// use the `zip(_:_:)` function.
///
/// The following example uses the `zip(_:_:)` function to iterate over an
/// array of strings and a countable range at the same time:
///
///     let words = ["one", "two", "three", "four"]
///     let numbers = 1...4
///
///     for (word, number) in zip(words, numbers) {
///         print("\(word): \(number)")
///     }
///     // Prints "one: 1"
///     // Prints "two: 2"
///     // Prints "three: 3"
///     // Prints "four: 4"
@frozen // generic-performance
public struct Zip2Sequence<Sequence1: Sequence, Sequence2: Sequence> {
  @usableFromInline // generic-performance
  internal let _sequence1: Sequence1
  @usableFromInline // generic-performance
  internal let _sequence2: Sequence2

  /// Creates an instance that makes pairs of elements from `sequence1` and
  /// `sequence2`.
  @inlinable // generic-performance
  internal init(_ sequence1: Sequence1, _ sequence2: Sequence2) {
    (_sequence1, _sequence2) = (sequence1, sequence2)
  }
}

extension Zip2Sequence {
  /// An iterator for `Zip2Sequence`.
  @frozen // generic-performance
  public struct Iterator {
    @usableFromInline // generic-performance
    internal var _baseStream1: Sequence1.Iterator
    @usableFromInline // generic-performance
    internal var _baseStream2: Sequence2.Iterator
    @usableFromInline // generic-performance
    internal var _reachedEnd: Bool = false

    /// Creates an instance around a pair of underlying iterators.
    @inlinable // generic-performance
    internal init(
    _ iterator1: Sequence1.Iterator, 
    _ iterator2: Sequence2.Iterator
    ) {
      (_baseStream1, _baseStream2) = (iterator1, iterator2)
    }
  }
}

extension Zip2Sequence.Iterator: IteratorProtocol {
  /// The type of element returned by `next()`.
  public typealias Element = (Sequence1.Element, Sequence2.Element)

  /// Advances to the next element and returns it, or `nil` if no next element
  /// exists.
  ///
  /// Once `nil` has been returned, all subsequent calls return `nil`.
  @inlinable // generic-performance
  public mutating func next() -> Element? {
    // The next() function needs to track if it has reached the end.  If we
    // didn't, and the first sequence is longer than the second, then when we
    // have already exhausted the second sequence, on every subsequent call to
    // next() we would consume and discard one additional element from the
    // first sequence, even though next() had already returned nil.

    if _reachedEnd {
      return nil
    }

    guard let element1 = _baseStream1.next(),
          let element2 = _baseStream2.next() else {
      _reachedEnd = true
      return nil
    }

    return (element1, element2)
  }
}

extension Zip2Sequence: Sequence {
  public typealias Element = (Sequence1.Element, Sequence2.Element)

  /// Returns an iterator over the elements of this sequence.
  @inlinable // generic-performance
  public __consuming func makeIterator() -> Iterator {
    return Iterator(
      _sequence1.makeIterator(),
      _sequence2.makeIterator())
  }

  @inlinable // generic-performance
  public var underestimatedCount: Int {
    return Swift.min(
      _sequence1.underestimatedCount,
      _sequence2.underestimatedCount
    )
  }
}

extension Zip2Sequence: Sendable where Sequence1: Sendable,
                                       Sequence2: Sendable { }
extension Zip2Sequence.Iterator: Sendable where Sequence1.Iterator: Sendable,
                                                Sequence2.Iterator: Sendable { }
