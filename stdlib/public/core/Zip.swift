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
///     // Prints "two: 2
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
@_inlineable // FIXME(sil-serialize-all)
public func zip<Sequence1, Sequence2>(
  _ sequence1: Sequence1, _ sequence2: Sequence2
) -> Zip2Sequence<Sequence1, Sequence2> {
  return Zip2Sequence(_sequence1: sequence1, _sequence2: sequence2)
}

/// Creates a collection of pairs built out of two underlying collections.
///
/// In the `Zip2Collection` instance returned by this function, the elements of
/// the *i*th pair are the *i*th elements of each underlying collection. The
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
///     // Prints "two: 2
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
///   - collection1: The first collection to zip.
///   - collection2: The second collection to zip.
/// - Returns: A sequence of tuple pairs, where the elements of each pair are
///   corresponding elements of `collection1` and `collection2`.
@_inlineable // FIXME(sil-serialize-all)
public func zip<Collection1, Collection2>(
  _ collection1: Collection1, _ collection2: Collection2
) -> Zip2Collection<Collection1, Collection2> {
  return Zip2Collection(_collection1: collection1, _collection2: collection2)
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
///     // Prints "two: 2
///     // Prints "three: 3"
///     // Prints "four: 4"
@_fixed_layout // FIXME(sil-serialize-all)
public struct Zip2Sequence<Sequence1 : Sequence, Sequence2 : Sequence> {
  @_versioned // FIXME(sil-serialize-all)
  internal let _sequence1: Sequence1
  @_versioned // FIXME(sil-serialize-all)
  internal let _sequence2: Sequence2

  @available(*, deprecated, renamed: "Sequence1.Iterator")
  public typealias Stream1 = Sequence1.Iterator
  @available(*, deprecated, renamed: "Sequence2.Iterator")
  public typealias Stream2 = Sequence2.Iterator

  /// Creates an instance that makes pairs of elements from `sequence1` and
  /// `sequence2`.
  @_inlineable // FIXME(sil-serialize-all)
  public // @testable
  init(_sequence1 sequence1: Sequence1, _sequence2 sequence2: Sequence2) {
    (_sequence1, _sequence2) = (sequence1, sequence2)
  }
}

extension Zip2Sequence {
  /// An iterator for `Zip2Sequence`.
  @_fixed_layout // FIXME(sil-serialize-all)
  public struct Iterator {
    @_versioned // FIXME(sil-serialize-all)
    internal var _baseStream1: Sequence1.Iterator
    @_versioned // FIXME(sil-serialize-all)
    internal var _baseStream2: Sequence2.Iterator
    @_versioned // FIXME(sil-serialize-all)
    internal var _reachedEnd: Bool = false

    /// Creates an instance around a pair of underlying iterators.
    @_inlineable // FIXME(sil-serialize-all)
    @_versioned // FIXME(sil-serialize-all)
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
  @_inlineable // FIXME(sil-serialize-all)
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
  @_inlineable // FIXME(sil-serialize-all)
  public func makeIterator() -> Iterator {
    return Iterator(
      _sequence1.makeIterator(),
      _sequence2.makeIterator())
  }
}

public struct Zip2Collection<Collection1 : Collection, Collection2 : Collection> {
  @_versioned // FIXME(sil-serialize-all)
  internal let _collection1: Collection1
  @_versioned // FIXME(sil-serialize-all)
  internal let _collection2: Collection2
  
  /// Creates an instance that makes pairs of elements from `collection1` and
  /// `collection2`.
  @_inlineable // FIXME(sil-serialize-all)
  public // @testable
  init(
    _collection1 collection1: Collection1, _collection2 collection2: Collection2
  ) {
    (_collection1, _collection2) = (collection1, collection2)
  }
}

extension Zip2Collection: Sequence {
  public typealias Element = (Collection1.Element, Collection2.Element)
  public typealias Iterator = Zip2Sequence<Collection1, Collection2>.Iterator
  
  /// Returns an iterator over the elements of this collection.
  @_inlineable // FIXME(sil-serialize-all)
  public func makeIterator() -> Iterator {
    return Zip2Sequence.Iterator(
      _collection1.makeIterator(),
      _collection2.makeIterator())
  }
}

extension Zip2Collection {
  public struct Index {
    @_versioned // FIXME(sil-serialize-all)
    internal var _index1: Collection1.Index
    @_versioned // FIXME(sil-serialize-all)
    internal var _index2: Collection2.Index
    
    /// Creates an instance that makes pairs of elements from `index1` and
    /// `index2`.
    @_inlineable // FIXME(sil-serialize-all)
    public // @testable
    init(_index1 index1: Collection1.Index, _index2 index2: Collection2.Index) {
      (_index1, _index2) = (index1, index2)
    }
  }
}
  
extension Zip2Collection.Index: Comparable {
    public static func == (
      lhs: Zip2Collection<Collection1, Collection2>.Index,
      rhs: Zip2Collection<Collection1, Collection2>.Index
    ) -> Bool {
      // yes, this is an || not an &&. this makes
      // the first endIndex of either match
      return lhs._index1 == rhs._index1
          || lhs._index2 == rhs._index2
    }
    
    public static func < (
      lhs: Zip2Collection<Collection1, Collection2>.Index,
      rhs: Zip2Collection<Collection1, Collection2>.Index
    ) -> Bool {
      return lhs._index1 < rhs._index1
    }
}
  
extension Zip2Collection: Collection {
  public var startIndex: Index {
    return Index(_index1: _collection1.startIndex, _index2: _collection2.startIndex)
  }

  public var endIndex: Index {
    return Index(_index1: _collection1.endIndex, _index2: _collection2.endIndex)
  }

  public subscript(i: Index) -> Element {
    return (_collection1[i._index1], _collection2[i._index2])
  }

  public func index(after: Index) -> Index {
    return Index(
      _index1: _collection1.index(after: after._index1),
      _index2: _collection2.index(after: after._index2)
    )
  }
}

// @available(*, deprecated, renamed: "Zip2Sequence.Iterator")
public typealias Zip2Iterator<T, U> = Zip2Sequence<T, U>.Iterator where T: Sequence, U: Sequence
