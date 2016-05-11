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
    _ transform: (Elements.Iterator.Element) -> SegmentOfResult
  ) -> LazySequence<
    FlattenSequence<LazyMapSequence<Elements, SegmentOfResult>>> {
    return self.map(transform).flatten()
  }
  
  /// Returns a `LazyMapSequence` containing the concatenated non-nil
  /// results of mapping transform over this `Sequence`.  The elements of
  /// the result are computed lazily, each time they are read.
  ///
  /// Use this method to receive only nonoptional values when your
  /// transformation produces an optional value.
  ///
  /// - Parameter transform: A closure that accepts an element of this
  /// sequence as its argument and returns an optional value.
  @warn_unused_result
  public func flatMap<U>(
    _ transform: (Self.Elements.Iterator.Element) -> U?
  ) -> LazyMapSequence<
    LazyFilterSequence<
      LazyMapSequence<Self.Elements, U?>>,
    U
  > {
    return self.map(transform).filter { $0 != nil }.map { $0! }
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
    _ transform: (Elements.Iterator.Element) -> SegmentOfResult
  ) -> LazyCollection<
    FlattenCollection<
      LazyMapCollection<Elements, SegmentOfResult>>
  > {
    return self.map(transform).flatten()
  }
  
  /// Returns a `LazyMapCollection` containing the concatenated non-nil
  /// results of mapping transform over this collection.  The elements of
  /// the result are computed lazily, each time they are read.
  ///
  /// Use this method to receive only nonoptional values when your
  /// transformation produces an optional value.
  ///
  /// - Parameter transform: A closure that accepts an element of this
  /// collection as its argument and returns an optional value.
  @warn_unused_result
  public func flatMap<U>(
    _ transform: (Self.Elements.Iterator.Element) -> U?
  ) -> LazyMapCollection<
    LazyFilterCollection<
      LazyMapCollection<Self.Elements, U?>>,
    U
  > {
    return self.map(transform).filter { $0 != nil }.map { $0! }
  }
}

extension LazyCollectionProtocol
  where
  Self : BidirectionalCollection,
  Elements : BidirectionalCollection
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
    where SegmentOfResult : BidirectionalCollection
  >(
    _ transform: (Elements.Iterator.Element) -> SegmentOfResult
  ) -> LazyCollection<
    FlattenBidirectionalCollection<
      LazyMapBidirectionalCollection<Elements, SegmentOfResult>
  >> {
    return self.map(transform).flatten()
  }
  
  /// Returns a `LazyMapBidirectionalCollection` containing the concatenated non-nil
  /// results of mapping transform over this collection.  The elements of
  /// the result are computed lazily, each time they are read.
  ///
  /// Use this method to receive only nonoptional values when your
  /// transformation produces an optional value.
  ///
  /// - Parameter transform: A closure that accepts an element of this
  /// collection as its argument and returns an optional value.
  public func flatMap<U>(
    _ transform: (Self.Elements.Iterator.Element) -> U?
  ) -> LazyMapBidirectionalCollection<
    LazyFilterBidirectionalCollection<
      LazyMapBidirectionalCollection<Self.Elements, U?>>,
    U
  > {
    return self.map(transform).filter { $0 != nil }.map { $0! }
  }
}
