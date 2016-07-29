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
  ///     self.map(transform).joined()
  ///
  /// - Complexity: O(1)
  public func flatMap<SegmentOfResult : Sequence>(
    _ transform: @escaping (Elements.Iterator.Element) -> SegmentOfResult
  ) -> LazySequence<
    FlattenSequence<LazyMapSequence<Elements, SegmentOfResult>>> {
    return self.map(transform).joined()
  }
  
  /// Returns a `LazyMapSequence` containing the concatenated non-nil
  /// results of mapping transform over this `Sequence`.
  ///
  /// Use this method to receive only nonoptional values when your
  /// transformation produces an optional value.
  ///
  /// - Parameter transform: A closure that accepts an element of this
  /// sequence as its argument and returns an optional value.
  public func flatMap<ElementOfResult>(
    _ transform: @escaping (Elements.Iterator.Element) -> ElementOfResult?
  ) -> LazyMapSequence<
    LazyFilterSequence<
      LazyMapSequence<Elements, ElementOfResult?>>,
    ElementOfResult
  > {
    return self.map(transform).filter { $0 != nil }.map { $0! }
  }
}

extension LazyCollectionProtocol {
  /// Returns the concatenated results of mapping `transform` over
  /// `self`.  Equivalent to 
  ///
  ///     self.map(transform).joined()
  ///
  /// - Complexity: O(1)
  public func flatMap<SegmentOfResult : Collection>(
    _ transform: @escaping (Elements.Iterator.Element) -> SegmentOfResult
  ) -> LazyCollection<
    FlattenCollection<
      LazyMapCollection<Elements, SegmentOfResult>>
  > {
    return self.map(transform).joined()
  }
  
  /// Returns a `LazyMapCollection` containing the concatenated non-nil
  /// results of mapping transform over this collection.
  ///
  /// Use this method to receive only nonoptional values when your
  /// transformation produces an optional value.
  ///
  /// - Parameter transform: A closure that accepts an element of this
  /// collection as its argument and returns an optional value.
  public func flatMap<ElementOfResult>(
    _ transform: @escaping (Elements.Iterator.Element) -> ElementOfResult?
  ) -> LazyMapCollection<
    LazyFilterCollection<
      LazyMapCollection<Elements, ElementOfResult?>>,
    ElementOfResult
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
  ///     self.map(transform).joined()
  ///
  /// - Complexity: O(1)
  public func flatMap<SegmentOfResult : Collection>(
    _ transform: @escaping (Elements.Iterator.Element) -> SegmentOfResult
  ) -> LazyCollection<
    FlattenBidirectionalCollection<
      LazyMapBidirectionalCollection<Elements, SegmentOfResult>>>
    where SegmentOfResult : BidirectionalCollection {
    return self.map(transform).joined()
  }
  
  /// Returns a `LazyMapBidirectionalCollection` containing the concatenated non-nil
  /// results of mapping transform over this collection.
  ///
  /// Use this method to receive only nonoptional values when your
  /// transformation produces an optional value.
  ///
  /// - Parameter transform: A closure that accepts an element of this
  /// collection as its argument and returns an optional value.
  public func flatMap<ElementOfResult>(
    _ transform: @escaping (Elements.Iterator.Element) -> ElementOfResult?
  ) -> LazyMapBidirectionalCollection<
    LazyFilterBidirectionalCollection<
      LazyMapBidirectionalCollection<Elements, ElementOfResult?>>,
    ElementOfResult
  > {
    return self.map(transform).filter { $0 != nil }.map { $0! }
  }
}
