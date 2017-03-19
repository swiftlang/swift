//===--- SequenceWrapper.swift - sequence/collection wrapper protocols ----===//
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
//
//  To create a Sequence that forwards requirements to an
//  underlying Sequence, have it conform to this protocol.
//
//===----------------------------------------------------------------------===//

/// A type that is just a wrapper over some base Sequence
@_show_in_interface
public // @testable
protocol _SequenceWrapper : Sequence {
  associatedtype Base : Sequence
  associatedtype Iterator = Base.Iterator
  associatedtype SubSequence = Base.SubSequence
  
  var _base: Base { get }
}

extension _SequenceWrapper  {
  public var underestimatedCount: Int {
    return _base.underestimatedCount
  }

  public func _preprocessingPass<R>(
    _ preprocess: () throws -> R
  ) rethrows -> R? {
    return try _base._preprocessingPass(preprocess)
  }
}

extension _SequenceWrapper where Iterator == Base.Iterator {
  public func makeIterator() -> Iterator {
    return self._base.makeIterator()
  }
  
  @discardableResult
  public func _copyContents(
    initializing buf: UnsafeMutableBufferPointer<Iterator.Element>
  ) -> (Iterator, UnsafeMutableBufferPointer<Iterator.Element>.Index) {
    return _base._copyContents(initializing: buf)
  }
}

extension _SequenceWrapper where Iterator.Element == Base.Iterator.Element {
  public func map<T>(
    _ transform: (Iterator.Element) throws -> T
  ) rethrows -> [T] {
    return try _base.map(transform)
  }
  
  public func filter(
    _ isIncluded: (Iterator.Element) throws -> Bool
  ) rethrows -> [Iterator.Element] {
    return try _base.filter(isIncluded)
  }

  public func forEach(_ body: (Iterator.Element) throws -> Void) rethrows {
    return try _base.forEach(body)
  }
  
  public func _customContainsEquatableElement(
    _ element: Iterator.Element
  ) -> Bool? { 
    return _base._customContainsEquatableElement(element)
  }
  
  public func _copyToContiguousArray()
    -> ContiguousArray<Iterator.Element> {
    return _base._copyToContiguousArray()
  }
}

extension _SequenceWrapper where SubSequence == Base.SubSequence {
  public func dropFirst(_ n: Int) -> SubSequence {
    return _base.dropFirst(n)
  }
  public func dropLast(_ n: Int) -> SubSequence {
    return _base.dropLast(n)
  }
  public func prefix(_ maxLength: Int) -> SubSequence {
    return _base.prefix(maxLength)
  }
  public func suffix(_ maxLength: Int) -> SubSequence {
    return _base.suffix(maxLength)
  }
}

extension _SequenceWrapper
  where SubSequence == Base.SubSequence, Iterator.Element == Base.Iterator.Element {

  public func drop(
    while predicate: (Iterator.Element) throws -> Bool
  ) rethrows -> SubSequence {
    return try _base.drop(while: predicate)
  }

  public func prefix(
    while predicate: (Iterator.Element) throws -> Bool
  ) rethrows -> SubSequence {
    return try _base.prefix(while: predicate)
  }
  
  public func split(
    maxSplits: Int, omittingEmptySubsequences: Bool,
    whereSeparator isSeparator: (Iterator.Element) throws -> Bool
  ) rethrows -> [SubSequence] {
    return try _base.split(
      maxSplits: maxSplits,
      omittingEmptySubsequences: omittingEmptySubsequences,
      whereSeparator: isSeparator
    )
  }
}

//===--- _CollectionWrapper -----------------------------------------------===//
// TODO: this needs testing, especially for plain forward and bidirectional
// collections, as our one use case currently is random access.

public protocol _CollectionWrapper : _SequenceWrapper, Collection {
  // Note: associated type requirements are stated here only where compiler
  // limitations prevent Collection from stating them fully
  associatedtype _Element = Base._Element
  associatedtype Base : Collection
  associatedtype Index = Base.Index
  associatedtype IndexDistance = Base.IndexDistance
#if false
  // TODO: Segments is not fully realized yet; we need to make it Bidirectional
  // when the Collection is, etc.  We also need a way to synthesize higher-level
  // indices from segment indices.
  associatedtype Segments : Collection = Base.Segments
#endif
  associatedtype SubSequence : Collection = Base.SubSequence
  associatedtype Indices : Collection = Base.Indices
  var _base : Base { get }
}

extension _CollectionWrapper {
  public var isEmpty: Bool { return _base.isEmpty }
  // Because these are specialized by Collection, we need to explicitly forward
  // here to avoid ambiguity with the one from _SequenceWrapper
  public var underestimatedCount: Int { return _base.underestimatedCount }
  public func _preprocessingPass<R>(
    _ preprocess: () throws -> R
  ) rethrows -> R? {
    return try _base._preprocessingPass(preprocess)
  }
}

#if false
// TODO: Segments is not fully realized yet; we need to make it Bidirectional
// when the Collection is, etc.  We also need a way to synthesize higher-level
// indices from segment indices.
extension _CollectionWrapper
where Segments == Base.Segments {
  public var segments : Segments? { return _base.segments }
}
#endif

extension _CollectionWrapper
where Iterator.Element == Base.Iterator.Element {
  public func withExistingUnsafeBuffer<R>(
    _ body: (UnsafeBufferPointer<Iterator.Element>) throws -> R
  ) rethrows -> R? {
    return try _base.withExistingUnsafeBuffer(body)
  }
  public var first: Iterator.Element? { return _base.first }

  // Because these asre specialized by Collection, we need to explicitly forward
  // here to avoid ambiguity with the one from _SequenceWrapper
  public func map<T>(
    _ transform: (Iterator.Element) throws -> T
  ) rethrows -> [T] {
    return try _base.map(transform)
  }
  public func _copyToContiguousArray()
    -> ContiguousArray<Iterator.Element> {
    return _base._copyToContiguousArray()
  }
}

extension _CollectionWrapper
  where SubSequence == Base.SubSequence, Iterator.Element == Base.Iterator.Element {

  public func drop(
    while predicate: (Iterator.Element) throws -> Bool
  ) rethrows -> SubSequence {
    return try _base.drop(while: predicate)
  }

  public func prefix(
    while predicate: (Iterator.Element) throws -> Bool
  ) rethrows -> SubSequence {
    return try _base.prefix(while: predicate)
  }
  
  public func split(
    maxSplits: Int, omittingEmptySubsequences: Bool,
    whereSeparator isSeparator: (Iterator.Element) throws -> Bool
  ) rethrows -> [SubSequence] {
    return try _base.split(
      maxSplits: maxSplits,
      omittingEmptySubsequences: omittingEmptySubsequences,
      whereSeparator: isSeparator
    )
  }
}

extension _CollectionWrapper
where Iterator.Element == Base.Iterator.Element, Index == Base.Index {
  public subscript(position: Index) -> Iterator.Element { return _base[position] }
  public func _customIndexOfEquatableElement(
    _ element: Iterator.Element
  ) -> Index?? {
    return _base._customIndexOfEquatableElement(element)
  }
}

extension _CollectionWrapper
where _Element == Base._Element, 
Index == Base.Index {
  public subscript(position: Index) -> _Element { return _base[position] }
}

extension _CollectionWrapper
where SubSequence == Base.SubSequence {
  // Because this is specialized by Collection, we need to explicitly forward
  // here to avoid ambiguity with the one from _SequenceWrapper
  public func dropFirst(_ n: Int) -> SubSequence {
    return _base.dropFirst(n)
  }
  public func dropLast(_ n: Int) -> SubSequence {
    return _base.dropLast(n)
  }
  public func prefix(_ maxLength: Int) -> SubSequence {
    return _base.prefix(maxLength)
  }
  public func suffix(_ maxLength: Int) -> SubSequence {
    return _base.suffix(maxLength)
  }
}

extension _CollectionWrapper
where SubSequence == Base.SubSequence, Index == Base.Index {
  public subscript(bounds: Range<Index>) -> SubSequence { return _base[bounds] }
  public func prefix(upTo end: Index) -> SubSequence {
    return _base.prefix(upTo: end)
  }
  public func suffix(from start: Index) -> SubSequence {
    return _base.suffix(from: start)
  }
  public func prefix(through lastElementOfResult: Index) -> SubSequence {
    return _base.prefix(through: lastElementOfResult)
  }
}

extension _CollectionWrapper
where Indices == Base.Indices {
  public var indices: Indices { return _base.indices }
}

extension _CollectionWrapper
where IndexDistance == Base.IndexDistance {
  public var count: IndexDistance { return _base.count }
}

extension _CollectionWrapper
where IndexDistance == Base.IndexDistance, Index == Base.Index {
  public func index(_ i: Index, offsetBy n: IndexDistance) -> Index {
    return _base.index(i, offsetBy: n)
  }
  public func formIndex(_ i: inout Index, offsetBy n: IndexDistance) {
    _base.formIndex(&i, offsetBy: n)
  }
  public func index(
    _ i: Index, offsetBy n: IndexDistance, limitedBy limit: Index
  ) -> Index? {
    return _base.index(i, offsetBy: n, limitedBy: limit)
  }
  public func formIndex(
    _ i: inout Index, offsetBy n: IndexDistance, limitedBy limit: Index
  ) -> Bool {
    return _base.formIndex(&i, offsetBy: n, limitedBy: limit)
  }
  public func distance(from start: Index, to end: Index) -> IndexDistance {
    return _base.distance(from: start, to: end)
  }
}

extension _CollectionWrapper
where Index == Base.Index {
  public var startIndex : Index { return _base.startIndex }
  public var endIndex : Index { return _base.endIndex }
  
  public func _failEarlyRangeCheck(_ index: Index, bounds: Range<Index>) {
    _base._failEarlyRangeCheck(index, bounds: bounds)
  }
  public func _failEarlyRangeCheck(_ index: Index, bounds: ClosedRange<Index>) {
    _base._failEarlyRangeCheck(index, bounds: bounds)
  }
  public func _failEarlyRangeCheck(
    _ range: Range<Index>, bounds: Range<Index>
  ) {
    _base._failEarlyRangeCheck(range, bounds: bounds)
  }
  public func index(after i: Index) -> Index {
    return _base.index(after: i)
  }
  public func formIndex(after i: inout Index) {
    return _base.formIndex(after: &i)
  }
}

public protocol _BidirectionalCollectionWrapper
: BidirectionalCollection, _CollectionWrapper {
  // Note: associated type requirements are stated here only where compiler
  // limitations prevent BidirectionalCollection from stating them fully
  associatedtype Base : BidirectionalCollection
//  associatedtype Segments : BidirectionalCollection = Base.Segments
  associatedtype SubSequence : BidirectionalCollection = Base.SubSequence
  associatedtype Indices : BidirectionalCollection = Base.Indices
  var _base : Base { get }
}

extension _BidirectionalCollectionWrapper where Index == Base.Index {
  public func index(before i: Index) -> Index { return _base.index(before: i) }
  public func formIndex(before i: inout Index) { _base.formIndex(before: &i) }
}

// Because these functions are specialized by BidirectionalCollection, they need
// to be redeclared here to avoid ambiguity with the ones from _CollectionWrapper
extension _BidirectionalCollectionWrapper
where IndexDistance == Base.IndexDistance, 
  Index == Base.Index {
  public func index(_ i: Index, offsetBy n: IndexDistance) -> Index {
    return _base.index(i, offsetBy: n)
  }
  public func index(
    _ i: Index, offsetBy n: IndexDistance, limitedBy limit: Index
  ) -> Index? {
    return _base.index(i, offsetBy: n, limitedBy: limit)
  }
  public func distance(from start: Index, to end: Index) -> IndexDistance {
    return _base.distance(from: start, to: end)
  }
}

// Because these functions are specialized by BidirectionalCollection, they need
// to be redeclared here to avoid ambiguity with the ones from _CollectionWrapper
extension _BidirectionalCollectionWrapper
where SubSequence == Base.SubSequence {
  public func dropLast(_ n: Int) -> SubSequence {
    return _base.dropLast(n)
  }
  public func suffix(_ maxLength: Int) -> SubSequence {
    return _base.suffix(maxLength)
  }
}

public protocol _RandomAccessCollectionWrapper
: RandomAccessCollection, _BidirectionalCollectionWrapper {
  // Note: associated type requirements are stated here only where compiler
  // limitations prevent RandomAccessCollection from stating them fully
  associatedtype Base : RandomAccessCollection
//  associatedtype Segments : RandomAccessCollection = Base.Segments
  associatedtype SubSequence : RandomAccessCollection = Base.SubSequence
  associatedtype Indices : RandomAccessCollection = Base.Indices
  var _base : Base { get }
}

// Because these functions are specialized by RandomAccessCollection, they need
// to be redeclared here to avoid ambiguity with the ones from
// _BidirectionalCollectionWrapper
extension _RandomAccessCollectionWrapper
where IndexDistance == Base.IndexDistance, 
  Index == Base.Index {
  public func index(_ i: Index, offsetBy n: IndexDistance) -> Index {
    return _base.index(i, offsetBy: n)
  }
  public func index(
    _ i: Index, offsetBy n: IndexDistance, limitedBy limit: Index
  ) -> Index? {
    return _base.index(i, offsetBy: n, limitedBy: limit)
  }
  public func distance(from start: Index, to end: Index) -> IndexDistance {
    return _base.distance(from: start, to: end)
  }
}
