//===--- FlatMap.swift ----------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
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
  public func flatMap<Intermediate: Sequence>(
    transform: (Elements.Iterator.Element)->Intermediate
  ) -> LazySequence<
    FlattenSequence<LazyMapSequence<Elements, Intermediate>>> {
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
  public func flatMap<Intermediate: Collection>(
    transform: (Elements.Iterator.Element)->Intermediate
  ) -> LazyCollection<
    FlattenCollection<
      LazyMapCollection<Elements, Intermediate>>
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
    Intermediate: Collection
    where Intermediate.Index : BidirectionalIndex
  >(
    transform: (Elements.Iterator.Element)->Intermediate
  ) -> LazyCollection<
    FlattenBidirectionalCollection<
      LazyMapCollection<Elements, Intermediate>
  >> {
    return self.map(transform).flatten()
  }
}
