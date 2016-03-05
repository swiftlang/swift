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

// TODO: swift-3-indexing-model - Add in RandomAccessCollection protocol documentation
public protocol RandomAccessCollection : BidirectionalCollection {

  associatedtype Index : Strideable
  // FIXME(compiler limitation): where Index.Distance == IndexDistance
  // FIXME(swift-3-indexing-model, design): does this requirement to conform to
  // `Strideable` limit possible collection designs?

  // FIXME(compiler limitation):
  // associatedtype SubSequence : RandomAccessCollection

  // FIXME(compiler limitation):
  // associatedtype Indices : RandomAccessCollection
}

/// Default implementation for random access collections.
extension RandomAccessCollection {

  public func _failEarlyRangeCheck(index: Index, bounds: Range<Index>) {
    fatalError("implement") // TODO: swift-3-indexing-model - implement
/*
    _precondition(
      bounds.startIndex <= index,
      "index is out of bounds: index designates a position before bounds.startIndex")
    _precondition(
      index < bounds.endIndex,
      "index is out of bounds: index designates the bounds.endIndex position or a position after it")
*/
  }

  public func _failEarlyRangeCheck(
    rangeStart rangeStart: Index,
    rangeEnd: Index,
    boundsStart: Index,
    boundsEnd: Index
  ) {
/*
    let range = rangeStart..<rangeEnd
    let bounds = boundsStart..<boundsEnd
    _precondition(
      bounds.startIndex <= range.startIndex,
      "range.startIndex is out of bounds: index designates a position before bounds.startIndex")
    _precondition(
      bounds.startIndex <= range.endIndex,
      "range.endIndex is out of bounds: index designates a position before bounds.startIndex")

    _precondition(
      range.startIndex <= bounds.endIndex,
      "range.startIndex is out of bounds: index designates a position after bounds.endIndex")
    _precondition(
      range.endIndex <= bounds.endIndex,
      "range.startIndex is out of bounds: index designates a position after bounds.endIndex")
*/
    fatalError("implement") // TODO: swift-3-indexing-model - implement
  }

// TODO: swift-3-indexing-model - implement optimized version of the following
  // advance(i: Index, by n: IndexDistance) -> Index
  // advance(i: Index, by n: IndexDistance, limit: Index) -> Index
  // distance(from start: Index, to end: Index) -> IndexDistance

/*
  @warn_unused_result
  public func advanced(by n: Distance, limit: Self) -> Self {
    let d = self.distance(to: limit)
    if d == 0 || (d > 0 ? d <= n : d >= n) {
      return limit
    }
    return self.advanced(by: n)
  }
*/
}
