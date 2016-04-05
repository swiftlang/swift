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

  @warn_unused_result
  func predecessor(of i: Index) -> Index

  func formPredecessor(i: inout Index)
}

// TODO: swift-3-indexing-model - Add in BidirectionalCollection protocol documentation
public protocol BidirectionalCollection
  : BidirectionalIndexable, Collection {

// TODO: swift-3-indexing-model - replaces functionality in BidirectionalIndex
  /// Returns the previous consecutive `Index` in a discrete sequence of
  /// `Index` values.
  ///
  /// If `i` has a well-defined successor, `self.predecessor(of: self.successor(of: i)) == i`.
  /// If `i` has a well-defined predecessor, `self.successor(of: self.predecessor(of: i)) == i`.
  ///
  /// - Precondition: `i` has a well-defined predecessor.
  @warn_unused_result
  func predecessor(of i: Index) -> Index

  func formPredecessor(i: inout Index)

  associatedtype SubSequence : BidirectionalIndexable, Collection
    = BidirectionalSlice<Self>
  // FIXME(compiler limitation):
  // associatedtype SubSequence : BidirectionalCollection

  associatedtype Indices : BidirectionalIndexable, Collection
    = DefaultBidirectionalIndices<Self>
  // FIXME(compiler limitation):
  // associatedtype Indices : BidirectionalCollection

  // TODO: swift-3-indexing-model: tests.
  var last: Iterator.Element? { get }
}

/// Default implementation for bidirectional collections.
extension BidirectionalIndexable {

  @inline(__always)
  public func formPredecessor(i: inout Index) {
    i = predecessor(of: i)
  }

  @warn_unused_result
  public func index(n: IndexDistance, stepsFrom i: Index) -> Index {
    if n >= 0 {
      return _advanceForward(i, by: n)
    }
    var i = i
    for _ in stride(from: 0, to: n, by: -1) {
      formPredecessor(&i)
    }
    return i
  }

  @warn_unused_result
  public func index(n: IndexDistance, stepsFrom i: Index, limitedBy limit: Index) -> Index {
    if n >= 0 {
      return _advanceForward(i, by: n, limitedBy: limit)
    }
    var i = i
    for _ in stride(from: 0, to: n, by: -1) {
      if (limit == i) {
        break;
      }
      formPredecessor(&i)
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
        formSuccessor(&start)
      }
    }
    else if start > end {
      while start != end {
        count -= 1 as IndexDistance
        formPredecessor(&start)
      }
    }

    return count
  }
}

/// Supply optimized defaults for `BidirectionalCollection` models that use
/// some model of `Strideable` as their `Index`.
extension BidirectionalIndexable where Index : Strideable {
  @warn_unused_result
  public func predecessor(of i: Index) -> Index {
    // FIXME: swift-3-indexing-model: range check i: should allow `endIndex`.
    //_failEarlyRangeCheck(i, bounds: startIndex..<endIndex)

    return i.advanced(by: -1)
  }

  /*
  @warn_unused_result
  public func index(n: IndexDistance, stepsFrom i: Index) -> Index {
    // FIXME: swift-3-indexing-model: range check i

    // FIXME: swift-3-indexing-model - error: cannot invoke 'advanced' with an argument list of type '(by: Self.IndexDistance)'
    return i.advanced(by: n)
  }

  @warn_unused_result
  public func index(n: IndexDistance, stepsFrom i: Index, limitedBy limit: Index) -> Index {
    // FIXME: swift-3-indexing-model: range check i

    // FIXME: swift-3-indexing-model - error: cannot invoke 'advanced' with an argument list of type '(by: Self.IndexDistance)'
    let i = i.advanced(by: n)
    if (i >= limit) {
      return limit
    }
    return i
  }

  @warn_unused_result
  public func distance(from start: Index, to end: Index) -> IndexDistance {
    // FIXME: swift-3-indexing-model: range check supplies start and end?

    // FIXME: swift-3-indexing-model - error: cannot invoke 'distance' with an argument list of type '(to: Self.Index)'
    return start.distance(to: end)
  }
  */
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
    self = self[startIndex..<predecessor(of: endIndex)]
    return element
  }

  /// Remove an element from the end.
  ///
  /// - Complexity: O(1)
  /// - Precondition: `!self.isEmpty`
  public mutating func removeLast() -> Iterator.Element {
    let element = last!
    self = self[startIndex..<predecessor(of: endIndex)]
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
    self = self[startIndex..<index(numericCast(-n), stepsFrom: endIndex)]
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
    let end = index(numericCast(-n), stepsFrom: endIndex, limitedBy: startIndex)
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
    let start = index(numericCast(-maxLength), stepsFrom: endIndex, limitedBy: startIndex)
    return self[start..<endIndex]
  }
}

