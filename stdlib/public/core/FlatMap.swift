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

public typealias LazyFlatMapSequence<Elements, SegmentOfResult> = 
  LazySequence<FlattenSequence<LazyMapSequence<Elements, SegmentOfResult>>>
public typealias LazyCompactMapSequence<Elements, ElementOfResult> = 
  LazyMapSequence<LazyFilterSequence<LazyMapSequence<Elements, ElementOfResult?>>, ElementOfResult>

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
  @inlinable // FIXME(sil-serialize-all)
  public func flatMap<SegmentOfResult>(
    _ transform: @escaping (Elements.Element) -> SegmentOfResult
  ) -> LazyFlatMapSequence<Elements, SegmentOfResult> {
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
  @inlinable // FIXME(sil-serialize-all)
  public func compactMap<ElementOfResult>(
    _ transform: @escaping (Elements.Element) -> ElementOfResult?
  ) -> LazyCompactMapSequence<Elements, ElementOfResult> {
    return self.map(transform).filter { $0 != nil }.map { $0! }
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
  @inline(__always)
  @available(swift, deprecated: 4.1, renamed: "compactMap(_:)",
    message: "Please use compactMap(_:) for the case where closure returns an optional value")
  public func flatMap<ElementOfResult>(
    _ transform: @escaping (Elements.Element) -> ElementOfResult?
  ) -> LazyMapSequence<
    LazyFilterSequence<
      LazyMapSequence<Elements, ElementOfResult?>>,
    ElementOfResult
  > {
    return self.compactMap(transform)
  }
}

public typealias LazyFlatMapCollection<Elements, SegmentOfResult> =
  LazyCollection<FlattenCollection<LazyMapCollection<Elements, SegmentOfResult>>>
public typealias LazyCompactMapCollection<Elements, ElementOfResult> =
  LazyMapCollection<LazyFilterCollection<LazyMapCollection<Elements, ElementOfResult?>>, ElementOfResult>

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
  @inlinable // FIXME(sil-serialize-all)
  public func flatMap<SegmentOfResult>(
    _ transform: @escaping (Elements.Element) -> SegmentOfResult
  ) -> LazyFlatMapCollection<Elements, SegmentOfResult> {
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
  @inlinable // FIXME(sil-serialize-all)
  public func compactMap<ElementOfResult>(
    _ transform: @escaping (Elements.Element) -> ElementOfResult?
  ) -> LazyCompactMapCollection<Elements, ElementOfResult> {
    return self.map(transform).filter { $0 != nil }.map { $0! }
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
  @available(swift, deprecated: 4.1, renamed: "compactMap(_:)",
    message: "Please use compactMap(_:) for the case where closure returns an optional value")
  @inlinable // FIXME(sil-serialize-all)
  public func flatMap<ElementOfResult>(
    _ transform: @escaping (Elements.Element) -> ElementOfResult?
  ) -> LazyMapCollection<
    LazyFilterCollection<
      LazyMapCollection<Elements, ElementOfResult?>>,
    ElementOfResult
  > {
    return self.map(transform).filter { $0 != nil }.map { $0! }
  }
}
