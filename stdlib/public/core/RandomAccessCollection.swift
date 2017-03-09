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
/// In most cases, it's best to ignore this protocol and use the
/// `RandomAccessCollection` protocol instead, because it has a more complete
/// interface.
@available(*, deprecated, message: "it will be removed in Swift 4.0.  Please use 'RandomAccessCollection' instead")
public typealias RandomAccessIndexable = _RandomAccessIndexable
public protocol _RandomAccessIndexable : _BidirectionalIndexable {
  // FIXME(ABI)#54 (Recursive Protocol Constraints): there is no reason for this protocol
  // to exist apart from missing compiler features that we emulate with it.
  // rdar://problem/20531108
  //
  // This protocol is almost an implementation detail of the standard
  // library.
}

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
public protocol RandomAccessCollection :
  _RandomAccessIndexable, BidirectionalCollection
{
  /// A collection that represents a contiguous subrange of the collection's
  /// elements.
  associatedtype SubSequence : _RandomAccessIndexable, BidirectionalCollection
    = RandomAccessSlice<Self>
  // FIXME(ABI)#102 (Recursive Protocol Constraints):
  // associatedtype SubSequence : RandomAccessCollection

  /// A type that represents the indices that are valid for subscripting the
  /// collection, in ascending order.
  associatedtype Indices : _RandomAccessIndexable, BidirectionalCollection
    = DefaultRandomAccessIndices<Self>
  // FIXME(ABI)#103 (Recursive Protocol Constraints):
  // associatedtype Indices : RandomAccessCollection

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
  var indices: Indices { get }

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

/// Supply the default "slicing" `subscript` for `RandomAccessCollection`
/// models that accept the default associated `SubSequence`,
/// `RandomAccessSlice<Self>`.
extension RandomAccessCollection where SubSequence == RandomAccessSlice<Self> {
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
  public subscript(bounds: Range<Index>) -> RandomAccessSlice<Self> {
    _failEarlyRangeCheck(bounds, bounds: startIndex..<endIndex)
    return RandomAccessSlice(base: self, bounds: bounds)
  }
}

// TODO: swift-3-indexing-model - Make sure RandomAccessCollection has
// documented complexity guarantees, e.g. for index(_:offsetBy:).

// TODO: swift-3-indexing-model - (By creating an ambiguity?), try to
// make sure RandomAccessCollection models implement
// index(_:offsetBy:) and distance(from:to:), or they will get the
// wrong complexity.

/// Default implementation for random access collections.
extension _RandomAccessIndexable {
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
  /// The value passed as `n` must not offset `i` beyond the bounds of the
  /// collection, unless the index passed as `limit` prevents offsetting
  /// beyond those bounds.
  ///
  /// - Parameters:
  ///   - i: A valid index of the array.
  ///   - n: The distance to offset `i`.
  ///   - limit: A valid index of the collection to use as a limit. If `n > 0`,
  ///     `limit` should be greater than `i` to have any effect. Likewise, if
  ///     `n < 0`, `limit` should be less than `i` to have any effect.
  /// - Returns: An index offset by `n` from the index `i`, unless that index
  ///   would be beyond `limit` in the direction of movement. In that case,
  ///   the method returns `nil`.
  ///
  /// - Complexity: O(1)
  public func index(
    _ i: Index, offsetBy n: IndexDistance, limitedBy limit: Index
  ) -> Index? {
    // FIXME: swift-3-indexing-model: tests.
    let l = distance(from: i, to: limit)
    if n > 0 ? l >= 0 && l < n : l <= 0 && n < l {
      return nil
    }
    return index(i, offsetBy: n)
  }
}

extension RandomAccessCollection
where Index : Strideable,
      Index.Stride == IndexDistance,
      Indices == CountableRange<Index> {

  /// The indices that are valid for subscripting the collection, in ascending
  /// order.
  public var indices: CountableRange<Index> {
    return startIndex..<endIndex
  }

  /// Returns the position immediately after the given index.
  ///
  /// - Parameter i: A valid index of the collection. `i` must be less than
  ///   `endIndex`.
  /// - Returns: The index value immediately after `i`.
  public func index(after i: Index) -> Index {
    // FIXME: swift-3-indexing-model: tests for the trap.
    _failEarlyRangeCheck(
      i, bounds: Range(uncheckedBounds: (startIndex, endIndex)))
    return i.advanced(by: 1)
  }

  /// Returns the position immediately after the given index.
  ///
  /// - Parameter i: A valid index of the collection. `i` must be greater than
  ///   `startIndex`.
  /// - Returns: The index value immediately before `i`.
  public func index(before i: Index) -> Index {
    let result = i.advanced(by: -1)
    // FIXME: swift-3-indexing-model: tests for the trap.
    _failEarlyRangeCheck(
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
  /// The value passed as `n` must not offset `i` beyond the bounds of the
  /// collection.
  ///
  /// - Parameters:
  ///   - i: A valid index of the collection.
  ///   - n: The distance to offset `i`.
  /// - Returns: An index offset by `n` from the index `i`. If `n` is positive,
  ///   this is the same value as the result of `n` calls to `index(after:)`.
  ///   If `n` is negative, this is the same value as the result of `-n` calls
  ///   to `index(before:)`.
  ///
  /// - Complexity: O(1)
  public func index(_ i: Index, offsetBy n: Index.Stride) -> Index {
    let result = i.advanced(by: n)
    // This range check is not precise, tighter bounds exist based on `n`.
    // Unfortunately, we would need to perform index manipulation to
    // compute those bounds, which is probably too slow in the general
    // case.
    // FIXME: swift-3-indexing-model: tests for the trap.
    _failEarlyRangeCheck(
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
  public func distance(from start: Index, to end: Index) -> Index.Stride {
    // FIXME: swift-3-indexing-model: tests for traps.
    _failEarlyRangeCheck(
      start, bounds: ClosedRange(uncheckedBounds: (startIndex, endIndex)))
    _failEarlyRangeCheck(
      end, bounds: ClosedRange(uncheckedBounds: (startIndex, endIndex)))
    return start.distance(to: end)
  }
}


