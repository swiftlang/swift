//===--- FlatMap.swift ----------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

extension LazySequenceProtocol {
  /// Returns the concatenated results of mapping `transform` over
  /// `self`.  Equivalent to 
  ///
  ///     self.map(transform).flatten()
  ///
  /// - Complexity: O(1)
  @warn_unused_result
  public func flatMap<SegmentOfResult : Sequence>(
    transform: (Elements.Iterator.Element) -> SegmentOfResult
  ) -> LazySequence<
    FlattenSequence<LazyMapSequence<Elements, SegmentOfResult>>> {
    return self.map(transform).flatten()
  }
}

extension LazyCollectionProtocol {
  /// Returns the concatenated results of mapping `transform` over
  /// `self`.  Equivalent to 
  ///
  ///     self.map(transform).flatten()
  ///
  /// - Complexity: O(1)
  @warn_unused_result
  public func flatMap<SegmentOfResult : Collection>(
    transform: (Elements.Iterator.Element) -> SegmentOfResult
  ) -> LazyCollection<
    FlattenCollection<
      LazyMapCollection<Elements, SegmentOfResult>>
  > {
    return self.map(transform).flatten()
  }
}

extension LazyCollectionProtocol where Elements.Index : BidirectionalIndex
{
  /// Returns the concatenated results of mapping `transform` over
  /// `self`.  Equivalent to 
  ///
  ///     self.map(transform).flatten()
  ///
  /// - Complexity: O(1)
  @warn_unused_result
  public func flatMap<
    SegmentOfResult : Collection
    where SegmentOfResult.Index : BidirectionalIndex
  >(
    transform: (Elements.Iterator.Element) -> SegmentOfResult
  ) -> LazyCollection<
    FlattenBidirectionalCollection<
      LazyMapCollection<Elements, SegmentOfResult>
  >> {
    return self.map(transform).flatten()
  }
}
