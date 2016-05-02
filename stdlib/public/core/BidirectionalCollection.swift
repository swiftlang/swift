//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

/// A type that provides subscript access to its elements, with
/// bidirectional index traversal.
///
/// - Important: In most cases, it's best to ignore this protocol and use
///   `BidirectionalCollection` instead, as it has a more complete interface.
public protocol BidirectionalIndexable : Indexable {
  // FIXME(ABI)(compiler limitation): there is no reason for this protocol
  // to exist apart from missing compiler features that we emulate with it.
  //
  // This protocol is almost an implementation detail of the standard
  // library.

  /// Returns the position immediately preceding `i`.
  ///
  /// - If `i >= startIndex && i < endIndex`,
  ///   `index(before: index(after: i)) == i`.
  /// 
  /// - If `i > startIndex && i <= endIndex`
  ///   `index(after: index(before: i)) == i`.
  ///
  /// - Precondition: `i > startIndex && i <= endIndex` 
  @warn_unused_result
  func index(before i: Index) -> Index

  /// Replaces `i` with its predecessor.
  func formIndex(before i: inout Index)
}

/// A collection that supports backward as well as forward traversal.
///
/// For any index `i` into a bidirectional collection `c`:
///
/// - If `i >= c.startIndex && i < c.endIndex`,
///   `c.index(before: c.index(after: i)) == i`.
///
/// - If `i > c.startIndex && i <= c.endIndex`
///   `c.index(after: c.index(before: i)) == i`.
public protocol BidirectionalCollection
  : BidirectionalIndexable, Collection {

// TODO: swift-3-indexing-model - replaces functionality in BidirectionalIndex
  /// Returns the position immediately preceding `i`.
  ///
  /// - Precondition: `i > startIndex && i <= endIndex` 
  @warn_unused_result
  func index(before i: Index) -> Index

  /// Replaces `i` with its predecessor.
  ///
  /// - Precondition: `i > startIndex && i <= endIndex`
  func formIndex(before i: inout Index)

  associatedtype SubSequence : BidirectionalIndexable, Collection
    = BidirectionalSlice<Self>
  // FIXME(compiler limitation):
  // associatedtype SubSequence : BidirectionalCollection

  associatedtype Indices : BidirectionalIndexable, Collection
    = DefaultBidirectionalIndices<Self>
  // FIXME(compiler limitation):
  // associatedtype Indices : BidirectionalCollection

  // TODO: swift-3-indexing-model: tests.
  /// The last element of `self`, or `nil` if `self` is empty.
  var last: Iterator.Element? { get }
}

/// Default implementation for bidirectional collections.
extension BidirectionalIndexable {

  @inline(__always)
  public func formIndex(before i: inout Index) {
    i = index(before: i)
  }

  @warn_unused_result
  public func index(_ i: Index, offsetBy n: IndexDistance) -> Index {
    if n >= 0 {
      return _advanceForward(i, by: n)
    }
    var i = i
    for _ in stride(from: 0, to: n, by: -1) {
      formIndex(before: &i)
    }
    return i
  }

  @warn_unused_result
  public func index(
    _ i: Index, offsetBy n: IndexDistance, limitedBy limit: Index
  ) -> Index? {
    if n >= 0 {
      return _advanceForward(i, by: n, limitedBy: limit)
    }
    var i = i
    for _ in stride(from: 0, to: n, by: -1) {
      if i == limit {
        return nil
      }
      formIndex(before: &i)
    }
    return i
  }

  @warn_unused_result
  public func distance(from start: Index, to end: Index) -> IndexDistance {
    var start = start
    var count: IndexDistance = 0

    if start < end {
      while start != end {
        count += 1 as IndexDistance
        formIndex(after: &start)
      }
    }
    else if start > end {
      while start != end {
        count -= 1 as IndexDistance
        formIndex(before: &start)
      }
    }

    return count
  }
}

/// Supply the default "slicing" `subscript` for `BidirectionalCollection`
/// models that accept the default associated `SubSequence`,
/// `BidirectionalSlice<Self>`.
extension BidirectionalIndexable where SubSequence == BidirectionalSlice<Self> {
  public subscript(bounds: Range<Index>) -> BidirectionalSlice<Self> {
    _failEarlyRangeCheck(bounds, bounds: startIndex..<endIndex)
    return BidirectionalSlice(base: self, bounds: bounds)
  }
}

extension BidirectionalCollection where SubSequence == Self {
  /// If `!self.isEmpty`, remove the last element and return it, otherwise
  /// return `nil`.
  ///
  /// - Complexity: O(1)
  @warn_unused_result
  public mutating func popLast() -> Iterator.Element? {
    guard !isEmpty else { return nil }
    let element = last!
    self = self[startIndex..<index(before: endIndex)]
    return element
  }

  /// Remove an element from the end.
  ///
  /// - Complexity: O(1)
  /// - Precondition: `!self.isEmpty`
  @discardableResult
  public mutating func removeLast() -> Iterator.Element {
    let element = last!
    self = self[startIndex..<index(before: endIndex)]
    return element
  }

  /// Remove the last `n` elements.
  ///
  /// - Complexity:
  ///   - O(1) if `Self` conforms to `RandomAccessCollection`
  ///   - O(n) otherwise
  /// - Precondition: `n >= 0 && self.count >= n`.
  public mutating func removeLast(_ n: Int) {
    if n == 0 { return }
    _precondition(n >= 0, "number of elements to remove should be non-negative")
    _precondition(count >= numericCast(n),
      "can't remove more items from a collection than it contains")
    self = self[startIndex..<index(endIndex, offsetBy: numericCast(-n))]
  }
}

extension BidirectionalCollection {
  /// Returns a subsequence containing all but the last `n` elements.
  ///
  /// - Precondition: `n >= 0`
  /// - Complexity: O(`n`)
  @warn_unused_result
  public func dropLast(_ n: Int) -> SubSequence {
    _precondition(
      n >= 0, "Can't drop a negative number of elements from a collection")
    let end = index(
      endIndex,
      offsetBy: numericCast(-n),
      limitedBy: startIndex) ?? startIndex
    return self[startIndex..<end]
  }

  /// Returns a slice, up to `maxLength` in length, containing the
  /// final elements of `self`.
  ///
  /// If `maxLength` exceeds `s.count`, the result contains all
  /// the elements of `self`.
  ///
  /// - Precondition: `maxLength >= 0`
  /// - Complexity: O(`maxLength`)
  @warn_unused_result
  public func suffix(_ maxLength: Int) -> SubSequence {
    _precondition(
      maxLength >= 0,
      "Can't take a suffix of negative length from a collection")
    let start = index(
      endIndex,
      offsetBy: numericCast(-maxLength),
      limitedBy: startIndex) ?? startIndex
    return self[start..<endIndex]
  }
}

