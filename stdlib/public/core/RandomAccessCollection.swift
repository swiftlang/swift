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

/// Default implementation for random access collections.
extension RandomAccessCollection {
// TODO: swift-3-indexing-model - implement optimized version of the following
// (also un-xfail corresponding test in validation-test/stdlib/Index.swift.gyb)
  //func index(i: Index, offsetBy n: IndexDistance) -> Index
  //@warn_unused_result
  //func index(
  //  i: Index, offsetBy n: IndexDistance, limitedBy limit: Index
  //) -> Index?
  //distance(from start: Index, to end: Index) -> IndexDistance
}
