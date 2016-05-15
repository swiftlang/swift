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

/// A collection that supports efficient random access index traversal.
///
/// - Important: In most cases, it's best to ignore this protocol and use
///   `RandomAccessCollection` instead, as it has a more complete interface.
public protocol RandomAccessIndexable : BidirectionalIndexable {
  // FIXME(ABI)(compiler limitation): there is no reason for this protocol
  // to exist apart from missing compiler features that we emulate with it.
  //
  // This protocol is almost an implementation detail of the standard
  // library.
}

/// A collection that supports efficient random access index traversal.
///
/// - Note: the fundamental difference between
///   `RandomAccessCollection` and `BidirectionalCollection` is that
///   the following are O(N) in `BidirectionalCollection` but O(1) in
///   `RandomAccessCollection`:
///
///   - `c.count`
///   - `c.index(i, offsetBy: n)`
///   - `c.index(i, offsetBy: n, limitedBy: l)`
///   - `c.distance(from: i, to: j)`
public protocol RandomAccessCollection :
  RandomAccessIndexable, BidirectionalCollection
{

  associatedtype SubSequence : RandomAccessIndexable, BidirectionalCollection
    = RandomAccessSlice<Self>
  // FIXME(compiler limitation):
  // associatedtype SubSequence : RandomAccessCollection

  associatedtype Indices : RandomAccessIndexable, BidirectionalCollection
    = DefaultRandomAccessIndices<Self>
  // FIXME(compiler limitation):
  // associatedtype Indices : RandomAccessCollection
}

/// Supply the default "slicing" `subscript` for `RandomAccessCollection`
/// models that accept the default associated `SubSequence`,
/// `RandomAccessSlice<Self>`.
extension RandomAccessCollection where SubSequence == RandomAccessSlice<Self> {
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
extension RandomAccessIndexable {
  /// Returns the result of advancing `i` by `n` positions, or `nil`
  /// if doing so would pass `limit`.
  ///
  /// - Returns:
  ///   - `nil` if `(limit > i) == (n > 0) && abs(distance(i, limit)) < abs(n)`
  ///   - Otherwise, `index(i, offsetBy: n)`
  ///
  /// - Complexity:
  ///   - O(1)
  @warn_unused_result
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

  public var indices: CountableRange<Index> {
    return startIndex..<endIndex
  }

  internal func _validityChecked(_ i: Index) -> Index {
    _precondition(i >= startIndex && i <= endIndex, "index out of range")
    return i
  }
  
  /// Returns the position immediately after `i`.
  ///
  /// - Precondition: `(startIndex..<endIndex).contains(i)`
  @warn_unused_result
  public func index(after i: Index) -> Index {
    return _validityChecked(i.advanced(by: 1))
  }

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
  public func index(before i: Index) -> Index {
    return _validityChecked(i.advanced(by: -1))
  }

  /// Returns the result of advancing `i` by `n` positions.
  ///
  /// - Returns:
  ///   - If `n > 0`, the `n`th successor of `i`.
  ///   - If `n < 0`, the `n`th predecessor of `i`.
  ///   - Otherwise, `i` unmodified.
  ///
  /// - Precondition:
  ///   - If `n > 0`, `n <= self.distance(from: i, to: self.endIndex)`
  ///   - If `n < 0`, `n >= self.distance(from: i, to: self.startIndex)`
  ///
  /// - Complexity: O(1)
  @warn_unused_result
  public func index(_ i: Index, offsetBy n: Index.Stride) -> Index {
    return _validityChecked(i.advanced(by: n))
  }
  
  /// Returns the distance from `start` to `end`.
  ///
  /// - Complexity: O(1)
  @warn_unused_result
  public func distance(from start: Index, to end: Index) -> Index.Stride {
    return start.distance(to: end)
  }
}


