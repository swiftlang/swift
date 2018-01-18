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
public func zip<Left, Right>(
  _ left: Left, _ right: Right
) -> Zip2<Left, Right> {
  return Zip2(left, right)
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
public struct Zip2<Left: Sequence, Right: Sequence> {
  public let left: Left
  public let right: Right
  
  @_inlineable
  @_versioned
  internal init(_ left: Left, _ right: Right) {
    self.left = left
    self.right = right
  }
}

extension Zip2 {
  /// An iterator for `Zip2Sequence`.
  @_fixed_layout // FIXME(sil-serialize-all)
  public struct Iterator {
    public var left: Left.Iterator
    public var right: Right.Iterator

    @_versioned // FIXME(sil-serialize-all)
    internal var _reachedEnd: Bool = false
    
    /// Creates an instance around a pair of underlying iterators.
    @_inlineable // FIXME(sil-serialize-all)
    @_versioned // FIXME(sil-serialize-all)
    internal init(
      _ left: Left.Iterator,
      _ right: Right.Iterator
    ) {
      self.left = left
      self.right = right
    }
  }
}

extension Zip2.Iterator: IteratorProtocol {
  /// The type of element returned by `next()`.
  public typealias Element = (Left.Element, Right.Element)
  
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
    
    guard let l = left.next(),
      let r = right.next() else {
        _reachedEnd = true
        return nil
    }
    
    return (l, r)
  }
}

extension Zip2: Sequence {
  public typealias Element = (Left.Element, Right.Element)
  public typealias SubSequence = Zip2<Left.SubSequence, Right.SubSequence>
  
  @_inlineable // FIXME(sil-serialize-all)
  public func dropFirst(_ n: Int) -> SubSequence {
    return zip(left.dropFirst(n),right.dropFirst(n))
  }
  
  @_inlineable // FIXME(sil-serialize-all)
  public func dropLast(_ n: Int) -> SubSequence {
    return zip(left.dropLast(n),right.dropLast(n))
  }
  
  @_inlineable // FIXME(sil-serialize-all)
  public func drop(
    while predicate: (Element) throws -> Bool
  ) rethrows -> SubSequence {
    // implementation of this function is problematic, because it needs two
    // passes over right to operate independendly on left and right using the
    // single predicate that takes both...    
    var i = 0
    var rightIterator = right.makeIterator()
    let leftSubSequence = try left.drop { l in
      guard let r = rightIterator.next() else { return false }
      i += 1
      return try predicate(l,r)
    }
    return zip(leftSubSequence, right.dropFirst(i))
  }
  
  @_inlineable // FIXME(sil-serialize-all)
  public func prefix(_ maxLength: Int) -> SubSequence {
    return zip(left.prefix(maxLength),right.prefix(maxLength))
  }
  
  @_inlineable // FIXME(sil-serialize-all)
  public func prefix(
    while predicate: (Element) throws -> Bool
  ) rethrows -> SubSequence {
    // similar to drop(while:), this currently takes two passes over right
    var i = 0
    var rightIterator = right.makeIterator()
    let leftSubSequence = try left.prefix { l in
      guard let r = rightIterator.next() else { return false }
      i += 1
      return try predicate(l,r)
    }
    return zip(leftSubSequence, right.prefix(i))
  }
  
  @_inlineable // FIXME(sil-serialize-all)
  public func suffix(_ maxLength: Int) -> SubSequence {
    return zip(left.suffix(maxLength),right.suffix(maxLength))
  }

  @_inlineable // FIXME(sil-serialize-all)
  public func split(
    maxSplits: Int, omittingEmptySubsequences: Bool,
    whereSeparator isSeparator: (Element) throws -> Bool
  ) rethrows -> [SubSequence] {
    // needs similar solution to drop(while:)
    fatalError()
  }
  
  /// Returns an iterator over the elements of this sequence.
  @_inlineable // FIXME(sil-serialize-all)
  public func makeIterator() -> Iterator {
    return Iterator(
      left.makeIterator(),
      right.makeIterator())
  }

  // ensure propogation of intentional underestimates from bases
  @_inlineable // FIXME(sil-serialize-all)
  public var underestimatedCount: Int {
    return Swift.min(left.underestimatedCount, right.underestimatedCount)
  }
}

// Because this type needs to work accross both Zip2<A,B> and 
// Zip2<A.SubSequence,B.SubSequence>, it needs to be a non-nested
// type.
@_fixed_layout
public struct _Zip2Index<Left: Comparable, Right: Comparable> {
  @_versioned // FIXME(sil-serialize-all)
  internal var left: Left
  @_versioned // FIXME(sil-serialize-all)
  internal var right: Right
  
  /// Creates an instance that makes pairs of elements from `index1` and
  /// `index2`.
  @_inlineable // FIXME(sil-serialize-all)
  @_versioned
  internal init(_ left: Left, _ right: Right) {
    self.left = left
    self.right = right
  }
}

extension _Zip2Index: Comparable {
  @_inlineable
  public static func == (
    lhs: _Zip2Index<Left, Right>,
    rhs: _Zip2Index<Left, Right>
  ) -> Bool {
    // yes, this is an || not an &&. this makes
    // the first endIndex of either match
    return lhs.left == rhs.left
        || lhs.right == rhs.right
  }
  
  @_inlineable
  public static func < (
    lhs: _Zip2Index<Left, Right>,
    rhs: _Zip2Index<Left, Right>
  ) -> Bool {
    let result = lhs.left < rhs.left
    _debugPrecondition(result == (lhs.right < rhs.right),
       "Unexpected left/right mismatch in Zip2 index comparison")
    return result
  }
}

extension Zip2: Collection where Left: Collection, Right: Collection {
  public typealias Index = _Zip2Index<Left.Index,Right.Index>

  @_inlineable
  public var startIndex: Index {
    return Index(left.startIndex, right.startIndex)
  }
  
  @_inlineable
  public var endIndex: Index {
    return Index(left.endIndex, right.endIndex)
  }
  
  @_inlineable
  public subscript(i: Index) -> Element {
    return (left[i.left], right[i.right])
  }
  
  @_inlineable
  public subscript(bounds: Range<Index>) -> SubSequence {
    let leftRange = bounds.lowerBound.left..<bounds.upperBound.left
    let rightRange = bounds.lowerBound.right..<bounds.upperBound.right
    return zip(left[leftRange],right[rightRange])
  }

  @_inlineable
  public func index(after: Index) -> Index {
//    _failEarlyRangeCheck(after, bounds: startIndex..<endIndex)
    
    return Index(
      left.index(after: after.left),
      right.index(after: after.right)
    )
  }
  
  @_inlineable
  public func index(_ i: Index, offsetBy n: Int) -> Index {
    _precondition(i >= startIndex, "Index out of range")
    _precondition(i <= endIndex, "Index out of range")
    
    let j = Index(
      left.index(i.left, offsetBy: n),
      right.index(i.right, offsetBy: n)
    )
    
    _debugPrecondition(j >= startIndex, "Unexpected zipped index out of bounds.")
    _debugPrecondition(j <= endIndex, "Unexpected zipped index out of bounds.")
    
    return j
  }
  
  @_inlineable
  public func index(_ i: Index, offsetBy n: Int, limitedBy limit: Index) -> Index? {
    _precondition(i >= startIndex, "Index out of range")
    _precondition(i <= endIndex, "Index out of range")
    
    guard let i1 = left.index(i.left, offsetBy: n, limitedBy: limit.left),
      let i2 = right.index(i.right, offsetBy: n, limitedBy: limit.right)
      else { return nil }
    
    let j = Index(i1, i2)
    
    _debugPrecondition(j >= startIndex, "Unexpected zipped index out of bounds.")
    _debugPrecondition(j <= endIndex, "Unexpected zipped index out of bounds.")
    
    return j
  }
  
  @_inlineable
  public func distance(from start: Index, to end: Index) -> Int {
    // This is necessary because non-random zips don't compute which
    // is shorter eagerly, so it could be either.
    if end.left == left.endIndex
      || end.right == right.endIndex
      || start.left == left.endIndex // inverted distances are a thing
      || start.right == right.endIndex {
      let d1 = left.distance(from: start.left, to: end.left)
      let d2 = right.distance(from: start.right, to: end.right)
      return Swift.min(d1,d2)
    }
    else {
      let d = left.distance(from: start.left, to: end.left)
      _debugPrecondition(d == right.distance(from: start.right, to: end.right),
        "Unexpected difference between two zipped collections.")
      return d
    }
  }
}

extension Zip2: BidirectionalCollection
where Left: RandomAccessCollection, Right: RandomAccessCollection {
  // Being bidirectional doesn't help us, because to calculate the shorter
  // length we need to find the max distance of both collections.
  // So no new features added here.
}

extension Zip2: RandomAccessCollection
where Left: RandomAccessCollection, Right: RandomAccessCollection {

  @_inlineable
  public var endIndex: Index {
    let i1: Left.Index, i2: Right.Index
    let c1 = left.count, c2 = right.count
    
    if c1 < c2 {
      i1 = left.endIndex
      i2 = right.index(right.startIndex, offsetBy: c1)
    }
    else {
      i1 = left.index(left.startIndex, offsetBy: c2)
      i2 = right.endIndex
    }
    
    return Index(i1, i2)
  }
  
  @_inlineable
  public func index(before i: Index) -> Index {
    _precondition(i > startIndex, "Index out of range")
    _precondition(i <= endIndex, "Index out of range")

    return Index(
      left.index(before: i.left),
      right.index(before: i.right)
    )
  }

  @_inlineable
  public func distance(from start: Index, to end: Index) -> Int {
    let d = left.distance(from: start.left, to: end.left)
    _debugPrecondition(d == right.distance(from: start.right, to: end.right),
      "Unexpected difference between two zipped collections.")
    return d
  }
}

extension Zip2: Equatable
where Left: Equatable, Right: Equatable {
  public static func == (
    lhs: Zip2<Left,Right>,
    rhs: Zip2<Left,Right>
  ) -> Bool {
    return lhs.left == rhs.left
        && lhs.right == rhs.right
  }
}

@available(*, deprecated, renamed: "Zip2.Iterator")
public typealias Zip2Iterator<T, U> = Zip2Sequence<T, U>.Iterator where T: Sequence, U: Sequence

@available(*, deprecated, renamed: "Zip2")
public typealias Zip2Sequence<T, U> = Zip2<T, U> where T: Sequence, U: Sequence

extension Zip2 {
  @available(*, deprecated, renamed: "Left")
  public typealias Sequence1 = Left
  @available(*, deprecated, renamed: "Right")
  public typealias Sequence2 = Right  
  @available(*, deprecated, renamed: "Left.Iterator")
  public typealias Stream1 = Left.Iterator
  @available(*, deprecated, renamed: "Right.Iterator")
  public typealias Stream2 = Right.Iterator  
}

