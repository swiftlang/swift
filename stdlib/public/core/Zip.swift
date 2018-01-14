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
///     // Prints "two: 2
///     // Prints "three: 3"
///     // Prints "four: 4"
@_fixed_layout // FIXME(sil-serialize-all)
public struct Zip2Sequence<Sequence1 : Sequence, Sequence2 : Sequence> {
  @_versioned // FIXME(sil-serialize-all)
  internal let _sequence1: Sequence1
  @_versioned // FIXME(sil-serialize-all)
  internal let _sequence2: Sequence2

  /// Creates an instance that makes pairs of elements from `sequence1` and
  /// `sequence2`.
  @_inlineable // FIXME(sil-serialize-all)
  @_versioned
  internal // @testable
  init(_ sequence1: Sequence1, _ sequence2: Sequence2) {
    self._sequence1 = sequence1
    self._sequence2 = sequence2
  }
}

extension Zip2Sequence {
  /// An iterator for `Zip2Sequence`.
  @_fixed_layout // FIXME(sil-serialize-all)
  public struct Iterator {
    @_versioned // FIXME(sil-serialize-all)
    internal var _iterator1: Sequence1.Iterator
    @_versioned // FIXME(sil-serialize-all)
    internal var _iterator2: Sequence2.Iterator
    @_versioned // FIXME(sil-serialize-all)
    internal var _reachedEnd: Bool = false

    /// Creates an instance around a pair of underlying iterators.
    @_inlineable // FIXME(sil-serialize-all)
    @_versioned // FIXME(sil-serialize-all)
    internal init(
      _ iterator1: Sequence1.Iterator, 
      _ iterator2: Sequence2.Iterator
    ) {
      self._iterator1 = iterator1
      self._iterator2 = iterator2
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

    guard let element1 = _iterator1.next(),
          let element2 = _iterator2.next() else {
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
  return Zip2Collection(collection1, collection2)
}

public struct Zip2Collection<Collection1 : Collection, Collection2 : Collection> {
  @_versioned // FIXME(sil-serialize-all)
  internal let _collection1: Collection1
  @_versioned // FIXME(sil-serialize-all)
  internal let _collection2: Collection2
  
  /// Creates an instance that makes pairs of elements from `collection1` and
  /// `collection2`.
  // FIXME: this should be inlineable, but this results in a compilation error:
  // "error: 'let' property '_collection1' may not be initialized directly; use "self.init(...)" or "self = ..." instead"
  // @_inlineable // FIXME(sil-serialize-all)
  @_versioned
  internal init(_ collection1: Collection1, _ collection2: Collection2) {
    self._collection1 = collection1
    self._collection2 = collection2
  }
}

extension Zip2Collection: Sequence {
  public typealias Element = (Collection1.Element, Collection2.Element)
  public typealias Iterator = Zip2Sequence<Collection1, Collection2>.Iterator
  
  /// Returns an iterator over the elements of this collection.
  @_inlineable // FIXME(sil-serialize-all)
  public func makeIterator() -> Iterator {
    return Zip2Sequence(_collection1, _collection2).makeIterator()
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
    // FIXME: this should be inlineable, but this results in a compilation 
    // error similar to the one for Zip2Collection.init above
    // @_inlineable // FIXME(sil-serialize-all)
    @_versioned
    internal init(_ index1: Collection1.Index, _ index2: Collection2.Index) {
      self._index1 = index1
      self._index2 = index2
    }
  }
}

extension Zip2Collection.Index: Comparable {
  @_inlineable
  public static func == (
    lhs: Zip2Collection<Collection1, Collection2>.Index,
    rhs: Zip2Collection<Collection1, Collection2>.Index
  ) -> Bool {
    // yes, this is an || not an &&. this makes
    // the first endIndex of either match
    return lhs._index1 == rhs._index1
        || lhs._index2 == rhs._index2
  }
    
  @_inlineable
  public static func < (
    lhs: Zip2Collection<Collection1, Collection2>.Index,
    rhs: Zip2Collection<Collection1, Collection2>.Index
  ) -> Bool {
    return lhs._index1 < rhs._index1
  }
}
  
extension Zip2Collection: Collection {
  @_inlineable
  public var startIndex: Index {
    return Index(_collection1.startIndex, _collection2.startIndex)
  }

  @_inlineable
  public var endIndex: Index {
    return Index(_collection1.endIndex, _collection2.endIndex)
  }

  @_inlineable
  public subscript(i: Index) -> Element {
    return (_collection1[i._index1], _collection2[i._index2])
  }

  @_inlineable
  public func index(after: Index) -> Index {
    _precondition(after >= startIndex)
    _precondition(after < endIndex)

    return Index(
      _collection1.index(after: after._index1),
      _collection2.index(after: after._index2)
    )
  }

  @_inlineable
  public func index(_ i: Index, offsetBy n: Int) -> Index {
    _precondition(i >= startIndex)
    _precondition(i <= endIndex)

    let j = Index(
      _collection1.index(i._index1, offsetBy: n),
      _collection2.index(i._index2, offsetBy: n)
    )
    
    _precondition(j >= startIndex)
    _precondition(j <= endIndex)
    
    return j
  }

  @_inlineable
  public func index(_ i: Index, offsetBy n: Int, limitedBy limit: Index) -> Index? {
    _debugPrecondition(i >= startIndex)
    _debugPrecondition(i <= endIndex)
    
    guard let i1 = _collection1.index(i._index1, offsetBy: n, limitedBy: limit._index1),
          let i2 = _collection2.index(i._index2, offsetBy: n, limitedBy: limit._index2)
    else { return nil }

    let j = Index(i1, i2)
    
    _debugPrecondition(j >= startIndex)
    _debugPrecondition(j <= endIndex)
    
    return j
  }

  @_inlineable
  public func distance(from start: Index, to end: Index) -> Int {
    // This is necessary because non-random zips don't compute which
    // is shorter eagerly, so it could be either.
    if end._index1 == _collection1.endIndex 
    || end._index2 == _collection2.endIndex
    || start._index1 == _collection1.endIndex // inverted distances are a thing
    || start._index2 == _collection2.endIndex {
      let d1 = _collection1.distance(from: start._index1, to: end._index1)
      let d2 = _collection2.distance(from: start._index2, to: end._index2)
      return Swift.min(d1,d2)
    }
    else {
      let d = _collection1.distance(from: start._index1, to: end._index1)
      _debugPrecondition(d == _collection2.distance(from: start._index2, to: end._index2),
        "Unexpected difference between two zipped collections.")
      return d
    }

  }
}

extension Zip2Collection: BidirectionalCollection
where Collection1: RandomAccessCollection, Collection2: RandomAccessCollection {
  // Being bidirectional doesn't help us, because to calculate the shorter
  // length we need to find the max distance of both collections.
  // So no new features added here.
}

extension Zip2Collection: RandomAccessCollection
where Collection1: RandomAccessCollection, Collection2: RandomAccessCollection {
  
  @_inlineable
  public var endIndex: Index {
    let i1: Collection1.Index, i2: Collection2.Index
    let c1 = _collection1.count, c2 = _collection2.count
    
    if c1 < c2 {
      i1 = _collection1.endIndex
      i2 = _collection2.index(_collection2.startIndex, offsetBy: c1)
    }
    else {
      i1 = _collection1.index(_collection1.startIndex, offsetBy: c2)
      i2 = _collection2.endIndex
    }
    
    return Index(i1, i2)
  }
  
  @_inlineable
  public func index(before i: Index) -> Index {
    _debugPrecondition(i > startIndex)
    _debugPrecondition(i <= endIndex)

    return Index(
      _collection1.index(before: i._index1),
      _collection2.index(before: i._index2)
    )
  }

  @_inlineable
  public func distance(from start: Index, to end: Index) -> Int {
    let d = _collection1.distance(from: start._index1, to: end._index1)
    _debugPrecondition(d == _collection2.distance(from: start._index2, to: end._index2),
      "Unexpected difference between two zipped collections.")
    return d
  }
}

extension Zip2Collection: Equatable
where Collection1: Equatable, Collection2: Equatable {
  public static func == (
    lhs: Zip2Collection<Collection1,Collection2>,
    rhs: Zip2Collection<Collection1,Collection2>
  ) -> Bool {
    return lhs._collection1 == rhs._collection1
        && lhs._collection2 == rhs._collection2
  }
}

@available(*, deprecated, renamed: "Zip2Sequence.Iterator")
public typealias Zip2Iterator<T, U> = Zip2Sequence<T, U>.Iterator where T: Sequence, U: Sequence

extension Zip2Sequence {
  @available(*, deprecated, renamed: "Sequence1.Iterator")
  public typealias Stream1 = Sequence1.Iterator
  @available(*, deprecated, renamed: "Sequence2.Iterator")
  public typealias Stream2 = Sequence2.Iterator  
}

