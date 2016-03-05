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

// TODO: swift-3-indexing-model - Add in BidirectionalCollection protocol documentation
public protocol BidirectionalCollection : Collection {
// TODO: swift-3-indexing-model - replaces functionality in BidirectionalIndex
  /// Returns the previous consecutive `Index` in a discrete sequence of
  /// `Index` values.
  ///
  /// If `i` has a well-defined successor, `self.previous(self.next(i)) == i`.
  /// If `i` has a well-defined predecessor, `self.next(self.previous(i)) == i`.
  ///
  /// - Precondition: `i` has a well-defined predecessor.
  @warn_unused_result
  func previous(i: Index) -> Index

  func _previousInPlace(i: inout Index)

  // FIXME(compiler limitation):
  // associatedtype SubSequence : BidirectionalCollection

  // FIXME(compiler limitation):
  // associatedtype Indices : BidirectionalCollection
}

/// Default implementation for bidirectional collections.
extension BidirectionalCollection {
// TODO: swift-3-indexing-model - stub to allow things to compile, remove when we have real implementations
  public func previous(i: Index) -> Index {
    fatalError("FIXME: swift-3-indexing-model")
  }

  @inline(__always)
  public func _previousInPlace(i: inout Index) {
    i = previous(i)
  }

  @warn_unused_result
  public func advance(i: Index, by n: IndexDistance) -> Index {
    if n >= 0 {
      return _advanceForward(i, by: n)
    }
    var i = i
    // FIXME: swift-3-indexing-model: There's a corner case here, -IntXX.min is
    // not representable.
    for _ in 0..<(-n) {
      _previousInPlace(&i)
    }
    return i
  }

  @warn_unused_result
  public func advance(i: Index, by n: IndexDistance, limit: Index) -> Index {
    if n >= 0 {
      return _advanceForward(i, by: n, limit: limit)
    }
    var i = i
    // FIXME: swift-3-indexing-model: There's a corner case here, -IntXX.min is
    // not representable.
    for _ in 0..<(-n) {
      if (limit == i) {
        break;
      }
      _previousInPlace(&i)
    }
    return i
  }

// TODO: swift-3-indexing-model - once Index is Comparable something like following is possible, right?
  //  @warn_unused_result
  //  public func distance(from start: Index, to end: Index) -> IndexDistance {
  //    var start = start
  //    var count: IndexDistance = 0
  //
  //    if start < end {
  //      while start != end {
  //        count = count + 1
  //        _nextInPlace(&start)
  //      }
  //    }
  //    else if start > end {
  //      while start != end {
  //        count = count - 1
  //        _previousInPlace(&start)
  //      }
  //    }
  //
  //    return count
  //  }
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
    self = self[startIndex..<previous(endIndex)]
    return element
  }

  /// Remove an element from the end.
  ///
  /// - Complexity: O(1)
  /// - Precondition: `!self.isEmpty`
  public mutating func removeLast() -> Iterator.Element {
    let element = last!
    self = self[startIndex..<previous(endIndex)]
    return element
  }

  /// Remove the last `n` elements.
  ///
  /// - Complexity:
  ///   - O(1) if `Index` conforms to `RandomAccessIndex`
  ///   - O(n) otherwise
  /// - Precondition: `n >= 0 && self.count >= n`.
  public mutating func removeLast(n: Int) {
    if n == 0 { return }
    _precondition(n >= 0, "number of elements to remove should be non-negative")
    _precondition(count >= numericCast(n),
      "can't remove more items from a collection than it contains")
    self = self[startIndex..<advance(endIndex, by: numericCast(-n))]
  }
}

extension BidirectionalCollection {
  /// Returns a subsequence containing all but the last `n` elements.
  ///
  /// - Precondition: `n >= 0`
  /// - Complexity: O(`n`)
  @warn_unused_result
  public func dropLast(n: Int) -> SubSequence {
    _precondition(
      n >= 0, "Can't drop a negative number of elements from a collection")
    let end = advance(endIndex, by: numericCast(-n), limit: startIndex)
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
  public func suffix(maxLength: Int) -> SubSequence {
    _precondition(
      maxLength >= 0,
      "Can't take a suffix of negative length from a collection")
    let start = advance(endIndex, by: numericCast(-maxLength), limit: startIndex)
    return self[start..<endIndex]
  }
}

