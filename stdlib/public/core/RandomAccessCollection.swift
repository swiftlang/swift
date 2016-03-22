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

public protocol RandomAccessIndexable : BidirectionalIndexable {
  // FIXME(ABI)(compiler limitation): there is no reason for this protocol
  // to exist apart from missing compiler features that we emulate with it.
  //
  // This protocol is almost an implementation detail of the standard
  // library.
}

// TODO: swift-3-indexing-model - Add in RandomAccessCollection protocol documentation
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

/// Default implementation for random access collections.
extension RandomAccessCollection {

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
