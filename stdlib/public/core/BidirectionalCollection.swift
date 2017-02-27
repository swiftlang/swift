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

/// A type that provides subscript access to its elements, with bidirectional
/// index traversal.
///
/// In most cases, it's best to ignore this protocol and use the
/// `BidirectionalCollection` protocol instead, because it has a more complete
/// interface.
@available(*, deprecated, message: "it will be removed in Swift 4.0.  Please use 'BidirectionalCollection' instead")
public typealias BidirectionalIndexable = _BidirectionalIndexable
public protocol _BidirectionalIndexable : _Indexable {
  // FIXME(ABI)#22 (Recursive Protocol Constraints): there is no reason for this protocol
  // to exist apart from missing compiler features that we emulate with it.
  // rdar://problem/20531108
  //
  // This protocol is almost an implementation detail of the standard
  // library.

  /// Returns the position immediately before the given index.
  ///
  /// - Parameter i: A valid index of the collection. `i` must be greater than
  ///   `startIndex`.
  /// - Returns: The index value immediately before `i`.
  func index(before i: Index) -> Index

  /// Replaces the given index with its predecessor.
  ///
  /// - Parameter i: A valid index of the collection. `i` must be greater than
  ///   `startIndex`.
  func formIndex(before i: inout Index)
}

/// A collection that supports backward as well as forward traversal.
///
/// Bidirectional collections offer traversal backward from any valid index,
/// not including a collection's `startIndex`. Bidirectional collections can
/// therefore offer additional operations, such as a `last` property that
/// provides efficient access to the last element and a `reversed()` method
/// that presents the elements in reverse order. In addition, bidirectional
/// collections have more efficient implementations of some sequence and
/// collection methods, such as `suffix(_:)`.
///
/// Conforming to the BidirectionalCollection Protocol
/// ==================================================
///
/// To add `BidirectionalProtocol` conformance to your custom types, implement
/// the `index(before:)` method in addition to the requirements of the
/// `Collection` protocol.
///
/// Indices that are moved forward and backward in a bidirectional collection
/// move by the same amount in each direction. That is, for any index `i` into
/// a bidirectional collection `c`:
///
/// - If `i >= c.startIndex && i < c.endIndex`,
///   `c.index(before: c.index(after: i)) == i`.
/// - If `i > c.startIndex && i <= c.endIndex`
///   `c.index(after: c.index(before: i)) == i`.
public protocol BidirectionalCollection
  : _BidirectionalIndexable, Collection {

// TODO: swift-3-indexing-model - replaces functionality in BidirectionalIndex
  /// Returns the position immediately before the given index.
  ///
  /// - Parameter i: A valid index of the collection. `i` must be greater than
  ///   `startIndex`.
  /// - Returns: The index value immediately before `i`.
  func index(before i: Index) -> Index

  /// Replaces the given index with its predecessor.
  ///
  /// - Parameter i: A valid index of the collection. `i` must be greater than
  ///   `startIndex`.
  func formIndex(before i: inout Index)

  /// A sequence that can represent a contiguous subrange of the collection's
  /// elements.
  associatedtype SubSequence : _BidirectionalIndexable, Collection
    = BidirectionalSlice<Self>
  // FIXME(ABI)#93 (Recursive Protocol Constraints):
  // FIXME(ABI)#94 (Associated Types with where clauses):
  // This is dependent on both recursive protocol constraints AND associated
  // types with where clauses.
  // associatedtype SubSequence : BidirectionalCollection

  /// A type that represents the indices that are valid for subscripting the
  /// collection, in ascending order.
  associatedtype Indices : _BidirectionalIndexable, Collection
    = DefaultBidirectionalIndices<Self>
  // FIXME(ABI)#95 (Recursive Protocol Constraints):
  // FIXME(ABI)#96 (Associated Types with where clauses):
  // This is dependent on both recursive protocol constraints AND associated
  // types with where clauses.
  // associatedtype Indices : BidirectionalCollection

  /// The indices that are valid for subscripting the collection, in ascending
  /// order.
  ///
  /// A collection's `indices` property can hold a strong reference to the
  /// collection itself, causing the collection to be non-uniquely referenced.
  /// If you mutate the collection while iterating over its indices, a strong
  /// reference can cause an unexpected copy of the collection. To avoid the
  /// unexpected copy, use the `index(after:)` method starting with
  /// `startIndex` to produce indices instead.
  ///
  ///     var c = MyFancyCollection([10, 20, 30, 40, 50])
  ///     var i = c.startIndex
  ///     while i != c.endIndex {
  ///         c[i] /= 5
  ///         i = c.index(after: i)
  ///     }
  ///     // c == MyFancyCollection([2, 4, 6, 8, 10])
  var indices: Indices { get }

  // TODO: swift-3-indexing-model: tests.
  /// The last element of the collection.
  ///
  /// If the collection is empty, the value of this property is `nil`.
  ///
  ///     let numbers = [10, 20, 30, 40, 50]
  ///     if let lastNumber = numbers.last {
  ///         print(lastNumber)
  ///     }
  ///     // Prints "50"
  ///
  /// - Complexity: O(1)
  var last: Iterator.Element? { get }

  /// Accesses a contiguous subrange of the collection's elements.
  ///
  /// The accessed slice uses the same indices for the same elements as the
  /// original collection uses. Always use the slice's `startIndex` property
  /// instead of assuming that its indices start at a particular value.
  ///
  /// This example demonstrates getting a slice of an array of strings, finding
  /// the index of one of the strings in the slice, and then using that index
  /// in the original array.
  ///
  ///     let streets = ["Adams", "Bryant", "Channing", "Douglas", "Evarts"]
  ///     let streetsSlice = streets[2 ..< streets.endIndex]
  ///     print(streetsSlice)
  ///     // Prints "["Channing", "Douglas", "Evarts"]"
  ///
  ///     let index = streetsSlice.index(of: "Evarts")    // 4
  ///     print(streets[index!])
  ///     // Prints "Evarts"
  ///
  /// - Parameter bounds: A range of the collection's indices. The bounds of
  ///   the range must be valid indices of the collection.
  subscript(bounds: Range<Index>) -> SubSequence { get }
}

/// Default implementation for bidirectional collections.
extension _BidirectionalIndexable {

  @inline(__always)
  public func formIndex(before i: inout Index) {
    i = index(before: i)
  }

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
extension BidirectionalCollection where SubSequence == BidirectionalSlice<Self> {
  public subscript(bounds: Range<Index>) -> BidirectionalSlice<Self> {
    _failEarlyRangeCheck(bounds, bounds: startIndex..<endIndex)
    return BidirectionalSlice(base: self, bounds: bounds)
  }
}

extension BidirectionalCollection where SubSequence == Self {
  /// Removes and returns the last element of the collection.
  ///
  /// You can use `popLast()` to remove the last element of a collection that
  /// might be empty. The `removeLast()` method must be used only on a
  /// nonempty collection.
  ///
  /// - Returns: The last element of the collection if the collection has one
  ///   or more elements; otherwise, `nil`.
  ///
  /// - Complexity: O(1).
  /// - SeeAlso: `removeLast()`
  public mutating func popLast() -> Iterator.Element? {
    guard !isEmpty else { return nil }
    let element = last!
    self = self[startIndex..<index(before: endIndex)]
    return element
  }

  /// Removes and returns the last element of the collection.
  ///
  /// The collection must not be empty. To remove the last element of a
  /// collection that might be empty, use the `popLast()` method instead.
  ///
  /// - Returns: The last element of the collection.
  ///
  /// - Complexity: O(1)
  /// - SeeAlso: `popLast()`
  @discardableResult
  public mutating func removeLast() -> Iterator.Element {
    let element = last!
    self = self[startIndex..<index(before: endIndex)]
    return element
  }

  /// Removes the given number of elements from the end of the collection.
  ///
  /// - Parameter n: The number of elements to remove. `n` must be greater
  ///   than or equal to zero, and must be less than or equal to the number of
  ///   elements in the collection.
  ///
  /// - Complexity: O(1) if the collection conforms to
  ///   `RandomAccessCollection`; otherwise, O(*n*), where *n* is the length
  ///   of the collection.
  public mutating func removeLast(_ n: Int) {
    if n == 0 { return }
    _precondition(n >= 0, "number of elements to remove should be non-negative")
    _precondition(count >= numericCast(n),
      "can't remove more items from a collection than it contains")
    self = self[startIndex..<index(endIndex, offsetBy: numericCast(-n))]
  }
}

extension BidirectionalCollection {
  /// Returns a subsequence containing all but the specified number of final
  /// elements.
  ///
  /// If the number of elements to drop exceeds the number of elements in the
  /// collection, the result is an empty subsequence.
  ///
  ///     let numbers = [1, 2, 3, 4, 5]
  ///     print(numbers.dropLast(2))
  ///     // Prints "[1, 2, 3]"
  ///     print(numbers.dropLast(10))
  ///     // Prints "[]"
  ///
  /// - Parameter n: The number of elements to drop off the end of the
  ///   collection. `n` must be greater than or equal to zero.
  /// - Returns: A subsequence that leaves off `n` elements from the end.
  ///
  /// - Complexity: O(*n*), where *n* is the number of elements to drop.
  public func dropLast(_ n: Int) -> SubSequence {
    _precondition(
      n >= 0, "Can't drop a negative number of elements from a collection")
    let end = index(
      endIndex,
      offsetBy: numericCast(-n),
      limitedBy: startIndex) ?? startIndex
    return self[startIndex..<end]
  }

  /// Returns a subsequence, up to the given maximum length, containing the
  /// final elements of the collection.
  ///
  /// If the maximum length exceeds the number of elements in the collection,
  /// the result contains the entire collection.
  ///
  ///     let numbers = [1, 2, 3, 4, 5]
  ///     print(numbers.suffix(2))
  ///     // Prints "[4, 5]"
  ///     print(numbers.suffix(10))
  ///     // Prints "[1, 2, 3, 4, 5]"
  ///
  /// - Parameter maxLength: The maximum number of elements to return.
  ///   `maxLength` must be greater than or equal to zero.
  /// - Returns: A subsequence terminating at the end of the collection with at
  ///   most `maxLength` elements.
  ///
  /// - Complexity: O(*n*), where *n* is equal to `maxLength`.
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

