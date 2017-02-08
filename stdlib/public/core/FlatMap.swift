//===--- FlatMap.swift ----------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

extension LazySequenceProtocol {
  /// Returns the concatenated results of mapping the given transformation over
  /// this sequence.
  ///
  /// Use this method to receive a single-level sequence when your
  /// transformation produces a sequence or collection for each element.
  /// Calling `flatMap(_:)` on a sequence `s` is equivalent to calling
  /// `s.map(transform).joined()`.
  ///
  /// - Complexity: O(1)
  public func flatMap<SegmentOfResult : Sequence>(
    _ transform: @escaping (Elements.Iterator.Element) -> SegmentOfResult
  ) -> LazySequence<
    FlattenSequence<LazyMapSequence<Elements, SegmentOfResult>>> {
    return self.map(transform).joined()
  }
  
  /// Returns the non-`nil` results of mapping the given transformation over
  /// this sequence.
  ///
  /// Use this method to receive a sequence of nonoptional values when your
  /// transformation produces an optional value.
  ///
  /// - Parameter transform: A closure that accepts an element of this sequence
  ///   as its argument and returns an optional value.
  ///
  /// - Complexity: O(1)
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
  /// Returns the concatenated results of mapping the given transformation over
  /// this collection.
  ///
  /// Use this method to receive a single-level collection when your
  /// transformation produces a collection for each element.
  /// Calling `flatMap(_:)` on a collection `c` is equivalent to calling
  /// `c.map(transform).joined()`.
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
  
  /// Returns the non-`nil` results of mapping the given transformation over
  /// this collection.
  ///
  /// Use this method to receive a collection of nonoptional values when your
  /// transformation produces an optional value.
  ///
  /// - Parameter transform: A closure that accepts an element of this
  ///   collection as its argument and returns an optional value.
  ///
  /// - Complexity: O(1)
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
  /// Returns the concatenated results of mapping the given transformation over
  /// this collection.
  ///
  /// Use this method to receive a single-level collection when your
  /// transformation produces a collection for each element.
  /// Calling `flatMap(_:)` on a collection `c` is equivalent to calling
  /// `c.map(transform).joined()`.
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
  
  /// Returns the non-`nil` results of mapping the given transformation over
  /// this collection.
  ///
  /// Use this method to receive a collection of nonoptional values when your
  /// transformation produces an optional value.
  ///
  /// - Parameter transform: A closure that accepts an element of this
  ///   collection as its argument and returns an optional value.
  ///
  /// - Complexity: O(1)
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
