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
public typealias BidirectionalIndexable = BidirectionalCollection

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
public protocol BidirectionalCollection: Collection
where SubSequence: BidirectionalCollection, Indices: BidirectionalCollection {
  // FIXME(ABI): Associated type inference requires this.
  associatedtype Element

  // FIXME(ABI): Associated type inference requires this.
  associatedtype Index

  // FIXME(ABI): Associated type inference requires this.
  associatedtype SubSequence

  // FIXME(ABI): Associated type inference requires this.
  associatedtype Indices

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
  var last: Element? { get }

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
  ///     let index = streetsSlice.firstIndex(of: "Evarts")    // 4
  ///     print(streets[index!])
  ///     // Prints "Evarts"
  ///
  /// - Parameter bounds: A range of the collection's indices. The bounds of
  ///   the range must be valid indices of the collection.
  subscript(bounds: Range<Index>) -> SubSequence { get }

  // FIXME(ABI): Associated type inference requires this.
  subscript(position: Index) -> Element { get }

  // FIXME(ABI): Associated type inference requires this.
  var startIndex: Index { get }

  // FIXME(ABI): Associated type inference requires this.
  var endIndex: Index { get }
}

/// Default implementation for bidirectional collections.
extension BidirectionalCollection {

  @inlinable // FIXME(sil-serialize-all)
  @inline(__always)
  public func formIndex(before i: inout Index) {
    i = index(before: i)
  }

  @inlinable // FIXME(sil-serialize-all)
  public func index(_ i: Index, offsetBy n: Int) -> Index {
    if n >= 0 {
      return _advanceForward(i, by: n)
    }
    var i = i
    for _ in stride(from: 0, to: n, by: -1) {
      formIndex(before: &i)
    }
    return i
  }

  @inlinable // FIXME(sil-serialize-all)
  public func index(
    _ i: Index, offsetBy n: Int, limitedBy limit: Index
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

  @inlinable // FIXME(sil-serialize-all)
  public func distance(from start: Index, to end: Index) -> Int {
    var start = start
    var count = 0

    if start < end {
      while start != end {
        count += 1
        formIndex(after: &start)
      }
    }
    else if start > end {
      while start != end {
        count -= 1
        formIndex(before: &start)
      }
    }

    return count
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
  @inlinable // FIXME(sil-serialize-all)
  public mutating func popLast() -> Element? {
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
  @inlinable // FIXME(sil-serialize-all)
  @discardableResult
  public mutating func removeLast() -> Element {
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
  @inlinable // FIXME(sil-serialize-all)
  public mutating func removeLast(_ n: Int) {
    if n == 0 { return }
    _precondition(n >= 0, "Number of elements to remove should be non-negative")
    _precondition(count >= n,
      "Can't remove more items from a collection than it contains")
    self = self[startIndex..<index(endIndex, offsetBy: -n)]
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
  @inlinable // FIXME(sil-serialize-all)
  public func dropLast(_ n: Int) -> SubSequence {
    _precondition(
      n >= 0, "Can't drop a negative number of elements from a collection")
    let end = index(
      endIndex,
      offsetBy: -n,
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
  @inlinable // FIXME(sil-serialize-all)
  public func suffix(_ maxLength: Int) -> SubSequence {
    _precondition(
      maxLength >= 0,
      "Can't take a suffix of negative length from a collection")
    let start = index(
      endIndex,
      offsetBy: -maxLength,
      limitedBy: startIndex) ?? startIndex
    return self[start..<endIndex]
  }
}

