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

/// A collection that supports efficient random-access index traversal.
///
/// Random-access collections can move indices any distance and 
/// measure the distance between indices in O(1) time. Therefore, the
/// fundamental difference between random-access and bidirectional collections
/// is that operations that depend on index movement or distance measurement
/// offer significantly improved efficiency. For example, a random-access
/// collection's `count` property is calculated in O(1) instead of requiring
/// iteration of an entire collection.
///
/// Conforming to the RandomAccessCollection Protocol
/// =================================================
///
/// The `RandomAccessCollection` protocol adds further constraints on the
/// associated `Indices` and `SubSequence` types, but otherwise imposes no
/// additional requirements over the `BidirectionalCollection` protocol.
/// However, in order to meet the complexity guarantees of a random-access
/// collection, either the index for your custom type must conform to the
/// `Strideable` protocol or you must implement the `index(_:offsetBy:)` and
/// `distance(from:to:)` methods with O(1) efficiency.
public protocol RandomAccessCollection<Element>: BidirectionalCollection
where SubSequence: RandomAccessCollection, Indices: RandomAccessCollection
{
  // FIXME: Associated type inference requires these.
  override associatedtype Element
  override associatedtype Index
  override associatedtype SubSequence
  override associatedtype Indices

  /// The indices that are valid for subscripting the collection, in ascending
  /// order.
  ///
  /// A collection's `indices` property can hold a strong reference to the
  /// collection itself, causing the collection to be nonuniquely referenced.
  /// If you mutate the collection while iterating over its indices, a strong
  /// reference can result in an unexpected copy of the collection. To avoid
  /// the unexpected copy, use the `index(after:)` method starting with
  /// `startIndex` to produce indices instead.
  ///
  ///     var c = MyFancyCollection([10, 20, 30, 40, 50])
  ///     var i = c.startIndex
  ///     while i != c.endIndex {
  ///         c[i] /= 5
  ///         i = c.index(after: i)
  ///     }
  ///     // c == MyFancyCollection([2, 4, 6, 8, 10])
  override var indices: Indices { get }

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
  ///
  /// - Complexity: O(1)
  override subscript(bounds: Range<Index>) -> SubSequence { get }

  // FIXME: Associated type inference requires these.
  @_borrowed
  override subscript(position: Index) -> Element { get }
  override var startIndex: Index { get }
  override var endIndex: Index { get }

  /// Returns the position immediately before the given index.
  ///
  /// - Parameter i: A valid index of the collection. `i` must be greater than
  ///   `startIndex`.
  /// - Returns: The index value immediately before `i`.
  override func index(before i: Index) -> Index

  /// Replaces the given index with its predecessor.
  ///
  /// - Parameter i: A valid index of the collection. `i` must be greater than
  ///   `startIndex`.
  override func formIndex(before i: inout Index)

  /// Returns the position immediately after the given index.
  ///
  /// The successor of an index must be well defined. For an index `i` into a
  /// collection `c`, calling `c.index(after: i)` returns the same index every
  /// time.
  ///
  /// - Parameter i: A valid index of the collection. `i` must be less than
  ///   `endIndex`.
  /// - Returns: The index value immediately after `i`.
  override func index(after i: Index) -> Index

  /// Replaces the given index with its successor.
  ///
  /// - Parameter i: A valid index of the collection. `i` must be less than
  ///   `endIndex`.
  override func formIndex(after i: inout Index)

  /// Returns an index that is the specified distance from the given index.
  ///
  /// The following example obtains an index advanced four positions from a
  /// string's starting index and then prints the character at that position.
  ///
  ///     let s = "Swift"
  ///     let i = s.index(s.startIndex, offsetBy: 4)
  ///     print(s[i])
  ///     // Prints "t"
  ///
  /// The value passed as `distance` must not offset `i` beyond the bounds of
  /// the collection.
  ///
  /// - Parameters:
  ///   - i: A valid index of the collection.
  ///   - distance: The distance to offset `i`. `distance` must not be negative
  ///     unless the collection conforms to the `BidirectionalCollection`
  ///     protocol.
  /// - Returns: An index offset by `distance` from the index `i`. If
  ///   `distance` is positive, this is the same value as the result of
  ///   `distance` calls to `index(after:)`. If `distance` is negative, this
  ///   is the same value as the result of `abs(distance)` calls to
  ///   `index(before:)`.
  ///
  /// - Complexity: O(1)
  @_nonoverride func index(_ i: Index, offsetBy distance: Int) -> Index

  /// Returns an index that is the specified distance from the given index,
  /// unless that distance is beyond a given limiting index.
  ///
  /// The following example obtains an index advanced four positions from a
  /// string's starting index and then prints the character at that position.
  /// The operation doesn't require going beyond the limiting `s.endIndex`
  /// value, so it succeeds.
  ///
  ///     let s = "Swift"
  ///     if let i = s.index(s.startIndex, offsetBy: 4, limitedBy: s.endIndex) {
  ///         print(s[i])
  ///     }
  ///     // Prints "t"
  ///
  /// The next example attempts to retrieve an index six positions from
  /// `s.startIndex` but fails, because that distance is beyond the index
  /// passed as `limit`.
  ///
  ///     let j = s.index(s.startIndex, offsetBy: 6, limitedBy: s.endIndex)
  ///     print(j)
  ///     // Prints "nil"
  ///
  /// The value passed as `distance` must not offset `i` beyond the bounds of
  /// the collection, unless the index passed as `limit` prevents offsetting
  /// beyond those bounds.
  ///
  /// - Parameters:
  ///   - i: A valid index of the collection.
  ///   - distance: The distance to offset `i`. `distance` must not be negative
  ///     unless the collection conforms to the `BidirectionalCollection`
  ///     protocol.
  ///   - limit: A valid index of the collection to use as a limit. If
  ///     `distance > 0`, a limit that is less than `i` has no effect.
  ///     Likewise, if `distance < 0`, a limit that is greater than `i` has no
  ///     effect.
  /// - Returns: An index offset by `distance` from the index `i`, unless that
  ///   index would be beyond `limit` in the direction of movement. In that
  ///   case, the method returns `nil`.
  ///
  /// - Complexity: O(1)
  @_nonoverride func index(
    _ i: Index, offsetBy distance: Int, limitedBy limit: Index
  ) -> Index?

  /// Returns the distance between two indices.
  ///
  /// Unless the collection conforms to the `BidirectionalCollection` protocol,
  /// `start` must be less than or equal to `end`.
  ///
  /// - Parameters:
  ///   - start: A valid index of the collection.
  ///   - end: Another valid index of the collection. If `end` is equal to
  ///     `start`, the result is zero.
  /// - Returns: The distance between `start` and `end`. The result can be
  ///   negative only if the collection conforms to the
  ///   `BidirectionalCollection` protocol.
  ///
  /// - Complexity: O(1)
  @_nonoverride func distance(from start: Index, to end: Index) -> Int
}

// TODO: swift-3-indexing-model - (By creating an ambiguity?), try to
// make sure RandomAccessCollection models implement
// index(_:offsetBy:) and distance(from:to:), or they will get the
// wrong complexity.

/// Default implementation for random access collections.
extension RandomAccessCollection {
  /// Returns an index that is the specified distance from the given index,
  /// unless that distance is beyond a given limiting index.
  ///
  /// The following example obtains an index advanced four positions from an
  /// array's starting index and then prints the element at that position. The
  /// operation doesn't require going beyond the limiting `numbers.endIndex`
  /// value, so it succeeds.
  ///
  ///     let numbers = [10, 20, 30, 40, 50]
  ///     let i = numbers.index(numbers.startIndex, offsetBy: 4)
  ///     print(numbers[i])
  ///     // Prints "50"
  ///
  /// The next example attempts to retrieve an index ten positions from
  /// `numbers.startIndex`, but fails, because that distance is beyond the
  /// index passed as `limit`.
  ///
  ///     let j = numbers.index(numbers.startIndex,
  ///                           offsetBy: 10,
  ///                           limitedBy: numbers.endIndex)
  ///     print(j)
  ///     // Prints "nil"
  ///
  /// The value passed as `distance` must not offset `i` beyond the bounds of
  /// the collection, unless the index passed as `limit` prevents offsetting
  /// beyond those bounds.
  ///
  /// - Parameters:
  ///   - i: A valid index of the array.
  ///   - distance: The distance to offset `i`.
  ///   - limit: A valid index of the collection to use as a limit. If
  ///     `distance > 0`, `limit` should be greater than `i` to have any
  ///     effect. Likewise, if `distance < 0`, `limit` should be less than `i`
  ///     to have any effect.
  /// - Returns: An index offset by `distance` from the index `i`, unless that
  ///   index would be beyond `limit` in the direction of movement. In that
  ///   case, the method returns `nil`.
  ///
  /// - Complexity: O(1)
  @inlinable
  public func index(
    _ i: Index, offsetBy distance: Int, limitedBy limit: Index
  ) -> Index? {
    // FIXME: swift-3-indexing-model: tests.
    let l = self.distance(from: i, to: limit)
    if distance > 0 ? l >= 0 && l < distance : l <= 0 && distance < l {
      return nil
    }
    return index(i, offsetBy: distance)
  }
}

// Provides an alternative default associated type witness for Indices
// for random access collections with strideable indices.
extension RandomAccessCollection where Index: Strideable, Index.Stride == Int {
  @_implements(Collection, Indices)
  public typealias _Default_Indices = Range<Index>
}

extension RandomAccessCollection
where Index: Strideable,
      Index.Stride == Int,
      Indices == Range<Index> {

  /// The indices that are valid for subscripting the collection, in ascending
  /// order.
  @inlinable
  public var indices: Range<Index> {
    return startIndex..<endIndex
  }

  /// Returns the position immediately after the given index.
  ///
  /// - Parameter i: A valid index of the collection. `i` must be less than
  ///   `endIndex`.
  /// - Returns: The index value immediately after `i`.
  @inlinable
  public func index(after i: Index) -> Index {
    // FIXME: swift-3-indexing-model: tests for the trap.
    unsafe _failEarlyRangeCheck(
      i, bounds: Range(uncheckedBounds: (startIndex, endIndex)))
    return i.advanced(by: 1)
  }

  /// Returns the position immediately after the given index.
  ///
  /// - Parameter i: A valid index of the collection. `i` must be greater than
  ///   `startIndex`.
  /// - Returns: The index value immediately before `i`.
  @inlinable // protocol-only
  public func index(before i: Index) -> Index {
    let result = i.advanced(by: -1)
    // FIXME: swift-3-indexing-model: tests for the trap.
    unsafe _failEarlyRangeCheck(
      result, bounds: Range(uncheckedBounds: (startIndex, endIndex)))
    return result
  }

  /// Returns an index that is the specified distance from the given index.
  ///
  /// The following example obtains an index advanced four positions from an
  /// array's starting index and then prints the element at that position.
  ///
  ///     let numbers = [10, 20, 30, 40, 50]
  ///     let i = numbers.index(numbers.startIndex, offsetBy: 4)
  ///     print(numbers[i])
  ///     // Prints "50"
  ///
  /// The value passed as `distance` must not offset `i` beyond the bounds of
  /// the collection.
  ///
  /// - Parameters:
  ///   - i: A valid index of the collection.
  ///   - distance: The distance to offset `i`.
  /// - Returns: An index offset by `distance` from the index `i`. If
  ///   `distance` is positive, this is the same value as the result of
  ///   `distance` calls to `index(after:)`. If `distance` is negative, this
  ///   is the same value as the result of `abs(distance)` calls to
  ///   `index(before:)`.
  ///
  /// - Complexity: O(1)
  @inlinable
  public func index(_ i: Index, offsetBy distance: Index.Stride) -> Index {
    let result = i.advanced(by: distance)
    // This range check is not precise, tighter bounds exist based on `n`.
    // Unfortunately, we would need to perform index manipulation to
    // compute those bounds, which is probably too slow in the general
    // case.
    // FIXME: swift-3-indexing-model: tests for the trap.
    unsafe _failEarlyRangeCheck(
      result, bounds: ClosedRange(uncheckedBounds: (startIndex, endIndex)))
    return result
  }
  
  /// Returns the distance between two indices.
  ///
  /// - Parameters:
  ///   - start: A valid index of the collection.
  ///   - end: Another valid index of the collection. If `end` is equal to
  ///     `start`, the result is zero.
  /// - Returns: The distance between `start` and `end`.
  ///
  /// - Complexity: O(1)
  @inlinable
  public func distance(from start: Index, to end: Index) -> Index.Stride {
    // FIXME: swift-3-indexing-model: tests for traps.
    unsafe _failEarlyRangeCheck(
      start, bounds: ClosedRange(uncheckedBounds: (startIndex, endIndex)))
    unsafe _failEarlyRangeCheck(
      end, bounds: ClosedRange(uncheckedBounds: (startIndex, endIndex)))
    return start.distance(to: end)
  }
}


