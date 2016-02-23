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
  
  // TODO: swift-3-indexing-model - replaces functionality in BidirectionalIndex
  func _previousInPlace(inout i: Index)
}

/// Default implementation for bidirectional collections.
extension BidirectionalCollection {
// TODO: swift-3-indexing-model - stub to allow things to compile, remove when we have real implementations
  public func previous(i: Index) -> Index {
    fatalError("collections need to implement, this is a temp stub to make things compile")
  }
  
  @inline(__always)
  public func _previousInPlace(inout i: Index) {
    i = previous(i)
  }
  
  @warn_unused_result
  public func advance(i: Index, by n: IndexDistance) -> Index {
    if n >= 0 {
      return _advanceForward(i, by: n)
    }
    var i = i
    for var offset: IndexDistance = n; offset != 0; offset = offset + 1 {
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
    for var offset: IndexDistance = n; offset != 0 && i != limit;
      offset = offset + 1 {
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