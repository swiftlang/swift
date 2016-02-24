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
  
  associatedtype Index : Strideable // FIXME: where Index.Distance == IndexDistance
  
}

/// Default implementation for random access collections.
extension RandomAccessCollection {

  public func _failEarlyRangeCheck(index: Index, bounds: Range<Index>) {
    fatalError("implement") // TODO: swift-3-indexing-model - implement
  }
  
  public func _failEarlyRangeCheck(
    rangeStart rangeStart: Index,
    rangeEnd: Index,
    boundsStart: Index,
    boundsEnd: Index
  ) {
      fatalError("implement") // TODO: swift-3-indexing-model - implement
  }
  
// TODO: swift-3-indexing-model - implement optimized version of the following
  // advance(i: Index, by n: IndexDistance) -> Index
  // advance(i: Index, by n: IndexDistance, limit: Index) -> Index
  // distance(from start: Index, to end: Index) -> IndexDistance
  
}