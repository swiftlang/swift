//===----------------------------------------------------------------------===//
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

import StdlibUnittest

public protocol Wrapper {
  associatedtype Base
  init(wrapping base: Base)
  var base: Base { get set }
}

public protocol LoggingType : Wrapper {
  associatedtype Log : AnyObject
}

extension LoggingType {
  public var log: Log.Type {
    return Log.self
  }

  public var selfType: Any.Type {
    return type(of: self)
  }
}

//===----------------------------------------------------------------------===//
// Iterator
//===----------------------------------------------------------------------===//

public class IteratorLog {
  public static func dispatchTester<I>(
    _ iterator: I
  ) -> LoggingIterator<LoggingIterator<I>> {
    return LoggingIterator(wrapping: LoggingIterator(wrapping: iterator))
  }
  public static var next = TypeIndexed(0)
}

public struct LoggingIterator<Base : IteratorProtocol>
  : IteratorProtocol, LoggingType {

  public typealias Log = IteratorLog

  public init(wrapping base: Base) {
    self.base = base
  }

  public mutating func next() -> Base.Element? {
    Log.next[selfType] += 1
    return base.next()
  }

  public var base: Base
}

//===----------------------------------------------------------------------===//
// Sequence and Collection logs
//===----------------------------------------------------------------------===//

public class SequenceLog {
  public static func dispatchTester<S>(
    _ s: S
  ) -> LoggingSequence<LoggingSequence<S>> {
    return LoggingSequence(wrapping: LoggingSequence(wrapping: s))
  }
  public static var makeIterator = TypeIndexed(0)
  public static var underestimatedCount = TypeIndexed(0)
  public static var map = TypeIndexed(0)
  public static var filter = TypeIndexed(0)
  public static var forEach = TypeIndexed(0)
  public static var dropFirst = TypeIndexed(0)
  public static var dropLast = TypeIndexed(0)
  public static var dropWhile = TypeIndexed(0)
  public static var prefixWhile = TypeIndexed(0)
  public static var prefixMaxLength = TypeIndexed(0)
  public static var suffixMaxLength = TypeIndexed(0)
  public static var split = TypeIndexed(0)
  public static var _customContainsEquatableElement = TypeIndexed(0)
  public static var _preprocessingPass = TypeIndexed(0)
  public static var _copyToContiguousArray = TypeIndexed(0)
  public static var _copyContents = TypeIndexed(0)
}

public class CollectionLog : SequenceLog {
  public class func dispatchTester<C>(
    _ c: C
  ) -> LoggingCollection<LoggingCollection<C>> {
    return LoggingCollection(wrapping: LoggingCollection(wrapping: c))
  }
  public static var startIndex = TypeIndexed(0)
  public static var endIndex = TypeIndexed(0)
  public static var subscriptIndex = TypeIndexed(0)
  public static var subscriptRange = TypeIndexed(0)
  public static var _failEarlyRangeCheckIndex = TypeIndexed(0)
  public static var _failEarlyRangeCheckRange = TypeIndexed(0)
  public static var successor = TypeIndexed(0)
  public static var formSuccessor = TypeIndexed(0)
  public static var indices = TypeIndexed(0)
  public static var prefixUpTo = TypeIndexed(0)
  public static var prefixThrough = TypeIndexed(0)
  public static var suffixFrom = TypeIndexed(0)
  public static var isEmpty = TypeIndexed(0)
  public static var count = TypeIndexed(0)
  public static var _customIndexOfEquatableElement = TypeIndexed(0)
  public static var first = TypeIndexed(0)
  public static var advance = TypeIndexed(0)
  public static var advanceLimit = TypeIndexed(0)
  public static var distance = TypeIndexed(0)
}

public class BidirectionalCollectionLog : SequenceLog {
  public class func dispatchTester<C>(
    _ c: C
  ) -> LoggingBidirectionalCollection<LoggingBidirectionalCollection<C>> {
    return LoggingBidirectionalCollection(
      wrapping: LoggingBidirectionalCollection(wrapping: c))
  }
  public static var predecessor = TypeIndexed(0)
  public static var formPredecessor = TypeIndexed(0)
  public static var last = TypeIndexed(0)
}

public class MutableCollectionLog : CollectionLog {
  public class func dispatchTester<C>(
    _ c: C
  ) -> LoggingMutableCollection<LoggingMutableCollection<C>> {
    return LoggingMutableCollection(
      wrapping: LoggingMutableCollection(wrapping: c))
  }
  public static var subscriptIndexSet = TypeIndexed(0)
  public static var subscriptRangeSet = TypeIndexed(0)
  public static var partitionBy = TypeIndexed(0)
  public static var _withUnsafeMutableBufferPointerIfSupported = TypeIndexed(0)
  public static var _withUnsafeMutableBufferPointerIfSupportedNonNilReturns =
    TypeIndexed(0)
}

/// Data container to keep track of how many times each `Base` type calls methods
/// of `RangeReplaceableCollection`.
///
/// Each static variable is a mapping of Type -> Number of calls.
public class RangeReplaceableCollectionLog : CollectionLog {
  public static var init_ = TypeIndexed(0)
  public static var initRepeating = TypeIndexed(0)
  public static var initWithSequence = TypeIndexed(0)

  public static var _customRemoveLast = TypeIndexed(0)
  public static var _customRemoveLastN = TypeIndexed(0)
  public static var append = TypeIndexed(0)
  public static var appendContentsOf = TypeIndexed(0)
  public static var insert = TypeIndexed(0)
  public static var insertContentsOf = TypeIndexed(0)
  public static var removeAll = TypeIndexed(0)
  public static var removeAt = TypeIndexed(0)
  public static var removeFirst = TypeIndexed(0)
  public static var removeFirstN = TypeIndexed(0)
  public static var removeSubrange = TypeIndexed(0)
  public static var replaceSubrange = TypeIndexed(0)
  public static var reserveCapacity = TypeIndexed(0)

  public class func dispatchTester<C>(
    _ rrc: C
  ) -> LoggingRangeReplaceableCollection<LoggingRangeReplaceableCollection<C>> {
    return LoggingRangeReplaceableCollection(
      wrapping: LoggingRangeReplaceableCollection(wrapping: rrc)
    )
  }
}

//===----------------------------------------------------------------------===//
// Sequence and Collection that count method calls
//===----------------------------------------------------------------------===//

/// Interposes between `Sequence` method calls to increment each method's
/// counter.
public struct LoggingSequence<
  Base : Sequence
> : Sequence, LoggingType {

  public var base: Base

  public typealias Log = SequenceLog

  public init(wrapping base: Base) {
    self.base = base
  }

  public typealias Iterator = LoggingIterator<Base.Iterator>

  public func makeIterator() -> Iterator {
    Log.makeIterator[selfType] += 1
    return LoggingIterator(wrapping: base.makeIterator())
  }

  public var underestimatedCount: Int {
    Log.underestimatedCount[selfType] += 1
    return base.underestimatedCount
  }

  public func map<T>(
    _ transform: (Base.Iterator.Element) throws -> T
  ) rethrows -> [T] {
    Log.map[selfType] += 1
    return try base.map(transform)
  }

  public func filter(
    _ isIncluded: (Base.Iterator.Element) throws -> Bool
  ) rethrows -> [Base.Iterator.Element] {
    Log.filter[selfType] += 1
    return try base.filter(isIncluded)
  }

  public func forEach(
    _ body: (Base.Iterator.Element) throws -> Void
  ) rethrows {
    Log.forEach[selfType] += 1
    try base.forEach(body)
  }

  public typealias SubSequence = Base.SubSequence

  public func dropFirst(_ n: Int) -> SubSequence {
    Log.dropFirst[selfType] += 1
    return base.dropFirst(n)
  }

  public func dropLast(_ n: Int) -> SubSequence {
    Log.dropLast[selfType] += 1
    return base.dropLast(n)
  }

  public func drop(
    while predicate: (Base.Iterator.Element) throws -> Bool
  ) rethrows -> SubSequence {
    Log.dropWhile[selfType] += 1
    return try base.drop(while: predicate)
  }

  public func prefix(_ maxLength: Int) -> SubSequence {
    Log.prefixMaxLength[selfType] += 1
    return base.prefix(maxLength)
  }

  public func prefix(
    while predicate: (Base.Iterator.Element) throws -> Bool
  ) rethrows -> SubSequence {
    Log.prefixWhile[selfType] += 1
    return try base.prefix(while: predicate)
  }

  public func suffix(_ maxLength: Int) -> SubSequence {
    Log.suffixMaxLength[selfType] += 1
    return base.suffix(maxLength)
  }

  public func split(
    maxSplits: Int = Int.max,
    omittingEmptySubsequences: Bool = true,
    whereSeparator isSeparator: (Base.Iterator.Element) throws -> Bool
  ) rethrows -> [SubSequence] {
    Log.split[selfType] += 1
    return try base.split(
      maxSplits: maxSplits,
      omittingEmptySubsequences: omittingEmptySubsequences,
      whereSeparator: isSeparator)
  }

  public func _customContainsEquatableElement(
    _ element: Base.Iterator.Element
  ) -> Bool? {
    Log._customContainsEquatableElement[selfType] += 1
    return base._customContainsEquatableElement(element)
  }

  /// If `self` is multi-pass (i.e., a `Collection`), invoke
  /// `preprocess` on `self` and return its result.  Otherwise, return
  /// `nil`.
  public func _preprocessingPass<R>(
    _ preprocess: () throws -> R
  ) rethrows -> R? {
    Log._preprocessingPass[selfType] += 1
    return try base._preprocessingPass(preprocess)
  }

  /// Create a native array buffer containing the elements of `self`,
  /// in the same order.
  public func _copyToContiguousArray()
    -> ContiguousArray<Base.Iterator.Element> {
    Log._copyToContiguousArray[selfType] += 1
    return base._copyToContiguousArray()
  }

  /// Copy a Sequence into an array.
  public func _copyContents(
    initializing buffer: UnsafeMutableBufferPointer<Iterator.Element>
  ) -> (Iterator,UnsafeMutableBufferPointer<Iterator.Element>.Index) {
    Log._copyContents[selfType] += 1
    let (it,idx) = base._copyContents(initializing: buffer)
    return (Iterator(wrapping: it),idx)
  }



}
/// Interposes between `Collection` method calls to increment each method's
/// counter.
public struct LoggingCollection<
  Base : Collection & Collection
> : Collection, LoggingType {

  public var base: Base

  public typealias Log = CollectionLog

  public init(wrapping base: Base) {
    self.base = base
  }

  public typealias Iterator = LoggingIterator<Base.Iterator>

  public func makeIterator() -> Iterator {
    Log.makeIterator[selfType] += 1
    return LoggingIterator(wrapping: base.makeIterator())
  }

  public var underestimatedCount: Int {
    Log.underestimatedCount[selfType] += 1
    return base.underestimatedCount
  }

  public func map<T>(
    _ transform: (Base.Iterator.Element) throws -> T
  ) rethrows -> [T] {
    Log.map[selfType] += 1
    return try base.map(transform)
  }

  public func filter(
    _ isIncluded: (Base.Iterator.Element) throws -> Bool
  ) rethrows -> [Base.Iterator.Element] {
    Log.filter[selfType] += 1
    return try base.filter(isIncluded)
  }

  public func forEach(
    _ body: (Base.Iterator.Element) throws -> Void
  ) rethrows {
    Log.forEach[selfType] += 1
    try base.forEach(body)
  }

  public typealias SubSequence = Base.SubSequence

  public func dropFirst(_ n: Int) -> SubSequence {
    Log.dropFirst[selfType] += 1
    return base.dropFirst(n)
  }

  public func dropLast(_ n: Int) -> SubSequence {
    Log.dropLast[selfType] += 1
    return base.dropLast(n)
  }

  public func drop(
    while predicate: (Base.Iterator.Element) throws -> Bool
  ) rethrows -> SubSequence {
    Log.dropWhile[selfType] += 1
    return try base.drop(while: predicate)
  }

  public func prefix(_ maxLength: Int) -> SubSequence {
    Log.prefixMaxLength[selfType] += 1
    return base.prefix(maxLength)
  }

  public func prefix(
    while predicate: (Base.Iterator.Element) throws -> Bool
  ) rethrows -> SubSequence {
    Log.prefixWhile[selfType] += 1
    return try base.prefix(while: predicate)
  }

  public func suffix(_ maxLength: Int) -> SubSequence {
    Log.suffixMaxLength[selfType] += 1
    return base.suffix(maxLength)
  }

  public func split(
    maxSplits: Int = Int.max,
    omittingEmptySubsequences: Bool = true,
    whereSeparator isSeparator: (Base.Iterator.Element) throws -> Bool
  ) rethrows -> [SubSequence] {
    Log.split[selfType] += 1
    return try base.split(
      maxSplits: maxSplits,
      omittingEmptySubsequences: omittingEmptySubsequences,
      whereSeparator: isSeparator)
  }

  public func _customContainsEquatableElement(
    _ element: Base.Iterator.Element
  ) -> Bool? {
    Log._customContainsEquatableElement[selfType] += 1
    return base._customContainsEquatableElement(element)
  }

  /// If `self` is multi-pass (i.e., a `Collection`), invoke
  /// `preprocess` on `self` and return its result.  Otherwise, return
  /// `nil`.
  public func _preprocessingPass<R>(
    _ preprocess: () throws -> R
  ) rethrows -> R? {
    Log._preprocessingPass[selfType] += 1
    return try base._preprocessingPass(preprocess)
  }

  /// Create a native array buffer containing the elements of `self`,
  /// in the same order.
  public func _copyToContiguousArray()
    -> ContiguousArray<Base.Iterator.Element> {
    Log._copyToContiguousArray[selfType] += 1
    return base._copyToContiguousArray()
  }

  /// Copy a Sequence into an array.
  public func _copyContents(
    initializing buffer: UnsafeMutableBufferPointer<Iterator.Element>
  ) -> (Iterator,UnsafeMutableBufferPointer<Iterator.Element>.Index) {
    Log._copyContents[selfType] += 1
    let (it,idx) = base._copyContents(initializing: buffer)
    return (Iterator(wrapping: it),idx)
  }

  public typealias Index = Base.Index

  public var startIndex: Index {
    Log.startIndex[selfType] += 1
    return base.startIndex
  }

  public var endIndex: Index {
    Log.endIndex[selfType] += 1
    return base.endIndex
  }

  public subscript(position: Index) -> Base.Iterator.Element {
    get {
      Log.subscriptIndex[selfType] += 1
      return base[position]
    }
  }

  public subscript(bounds: Range<Index>) -> SubSequence {
    get {
      Log.subscriptRange[selfType] += 1
      return base[bounds]
    }
  }

  public func _failEarlyRangeCheck(_ index: Index, bounds: Range<Index>) {
    Log._failEarlyRangeCheckIndex[selfType] += 1
    base._failEarlyRangeCheck(index, bounds: bounds)
  }

  public func _failEarlyRangeCheck(_ range: Range<Index>, bounds: Range<Index>) {
    Log._failEarlyRangeCheckRange[selfType] += 1
    base._failEarlyRangeCheck(range, bounds: bounds)
  }

  public func index(after i: Index) -> Index {
    Log.successor[selfType] += 1
    return base.index(after: i)
  }

  public func formIndex(after i: inout Index) {
    Log.formSuccessor[selfType] += 1
    base.formIndex(after: &i)
  }

  public typealias Indices = Base.Indices

  public var indices: Indices {
    Log.indices[selfType] += 1
    return base.indices
  }

  public func prefix(upTo end: Index) -> SubSequence {
    Log.prefixUpTo[selfType] += 1
    return base.prefix(upTo: end)
  }

  public func prefix(through position: Index) -> SubSequence {
    Log.prefixThrough[selfType] += 1
    return base.prefix(through: position)
  }

  public func suffix(from start: Index) -> SubSequence {
    Log.suffixFrom[selfType] += 1
    return base.suffix(from: start)
  }

  public var isEmpty: Bool {
    Log.isEmpty[selfType] += 1
    return base.isEmpty
  }

  public var count: Int {
    Log.count[selfType] += 1
    return base.count
  }

  public func _customIndexOfEquatableElement(
    _ element: Base.Iterator.Element
  ) -> Index?? {
    Log._customIndexOfEquatableElement[selfType] += 1
    return base._customIndexOfEquatableElement(element)
  }

  public var first: Base.Iterator.Element? {
    Log.first[selfType] += 1
    return base.first
  }

  public func index(_ i: Index, offsetBy n: Int) -> Index {
    Log.advance[selfType] += 1
    return base.index(i, offsetBy: n)
  }

  public func index(
    _ i: Index, offsetBy n: Int, limitedBy limit: Index
  ) -> Index? {
    Log.advanceLimit[selfType] += 1
    return base.index(i, offsetBy: n, limitedBy: limit)
  }

  public func distance(from start: Index, to end: Index) -> Int {
    Log.distance[selfType] += 1
    return base.distance(from: start, to: end)
  }


}
/// Interposes between `Collection` method calls to increment each method's
/// counter.
public struct LoggingBidirectionalCollection<
  Base : Collection & BidirectionalCollection
> : BidirectionalCollection, Collection, LoggingType {

  public var base: Base

  public typealias Log = CollectionLog

  public init(wrapping base: Base) {
    self.base = base
  }

  public typealias Iterator = LoggingIterator<Base.Iterator>

  public func makeIterator() -> Iterator {
    Log.makeIterator[selfType] += 1
    return LoggingIterator(wrapping: base.makeIterator())
  }

  public var underestimatedCount: Int {
    Log.underestimatedCount[selfType] += 1
    return base.underestimatedCount
  }

  public func map<T>(
    _ transform: (Base.Iterator.Element) throws -> T
  ) rethrows -> [T] {
    Log.map[selfType] += 1
    return try base.map(transform)
  }

  public func filter(
    _ isIncluded: (Base.Iterator.Element) throws -> Bool
  ) rethrows -> [Base.Iterator.Element] {
    Log.filter[selfType] += 1
    return try base.filter(isIncluded)
  }

  public func forEach(
    _ body: (Base.Iterator.Element) throws -> Void
  ) rethrows {
    Log.forEach[selfType] += 1
    try base.forEach(body)
  }

  public typealias SubSequence = Base.SubSequence

  public func dropFirst(_ n: Int) -> SubSequence {
    Log.dropFirst[selfType] += 1
    return base.dropFirst(n)
  }

  public func dropLast(_ n: Int) -> SubSequence {
    Log.dropLast[selfType] += 1
    return base.dropLast(n)
  }

  public func drop(
    while predicate: (Base.Iterator.Element) throws -> Bool
  ) rethrows -> SubSequence {
    Log.dropWhile[selfType] += 1
    return try base.drop(while: predicate)
  }

  public func prefix(_ maxLength: Int) -> SubSequence {
    Log.prefixMaxLength[selfType] += 1
    return base.prefix(maxLength)
  }

  public func prefix(
    while predicate: (Base.Iterator.Element) throws -> Bool
  ) rethrows -> SubSequence {
    Log.prefixWhile[selfType] += 1
    return try base.prefix(while: predicate)
  }

  public func suffix(_ maxLength: Int) -> SubSequence {
    Log.suffixMaxLength[selfType] += 1
    return base.suffix(maxLength)
  }

  public func split(
    maxSplits: Int = Int.max,
    omittingEmptySubsequences: Bool = true,
    whereSeparator isSeparator: (Base.Iterator.Element) throws -> Bool
  ) rethrows -> [SubSequence] {
    Log.split[selfType] += 1
    return try base.split(
      maxSplits: maxSplits,
      omittingEmptySubsequences: omittingEmptySubsequences,
      whereSeparator: isSeparator)
  }

  public func _customContainsEquatableElement(
    _ element: Base.Iterator.Element
  ) -> Bool? {
    Log._customContainsEquatableElement[selfType] += 1
    return base._customContainsEquatableElement(element)
  }

  /// If `self` is multi-pass (i.e., a `Collection`), invoke
  /// `preprocess` on `self` and return its result.  Otherwise, return
  /// `nil`.
  public func _preprocessingPass<R>(
    _ preprocess: () throws -> R
  ) rethrows -> R? {
    Log._preprocessingPass[selfType] += 1
    return try base._preprocessingPass(preprocess)
  }

  /// Create a native array buffer containing the elements of `self`,
  /// in the same order.
  public func _copyToContiguousArray()
    -> ContiguousArray<Base.Iterator.Element> {
    Log._copyToContiguousArray[selfType] += 1
    return base._copyToContiguousArray()
  }

  /// Copy a Sequence into an array.
  public func _copyContents(
    initializing buffer: UnsafeMutableBufferPointer<Iterator.Element>
  ) -> (Iterator,UnsafeMutableBufferPointer<Iterator.Element>.Index) {
    Log._copyContents[selfType] += 1
    let (it,idx) = base._copyContents(initializing: buffer)
    return (Iterator(wrapping: it),idx)
  }

  public typealias Index = Base.Index

  public var startIndex: Index {
    Log.startIndex[selfType] += 1
    return base.startIndex
  }

  public var endIndex: Index {
    Log.endIndex[selfType] += 1
    return base.endIndex
  }

  public subscript(position: Index) -> Base.Iterator.Element {
    get {
      Log.subscriptIndex[selfType] += 1
      return base[position]
    }
  }

  public subscript(bounds: Range<Index>) -> SubSequence {
    get {
      Log.subscriptRange[selfType] += 1
      return base[bounds]
    }
  }

  public func _failEarlyRangeCheck(_ index: Index, bounds: Range<Index>) {
    Log._failEarlyRangeCheckIndex[selfType] += 1
    base._failEarlyRangeCheck(index, bounds: bounds)
  }

  public func _failEarlyRangeCheck(_ range: Range<Index>, bounds: Range<Index>) {
    Log._failEarlyRangeCheckRange[selfType] += 1
    base._failEarlyRangeCheck(range, bounds: bounds)
  }

  public func index(after i: Index) -> Index {
    Log.successor[selfType] += 1
    return base.index(after: i)
  }

  public func formIndex(after i: inout Index) {
    Log.formSuccessor[selfType] += 1
    base.formIndex(after: &i)
  }

  public typealias Indices = Base.Indices

  public var indices: Indices {
    Log.indices[selfType] += 1
    return base.indices
  }

  public func prefix(upTo end: Index) -> SubSequence {
    Log.prefixUpTo[selfType] += 1
    return base.prefix(upTo: end)
  }

  public func prefix(through position: Index) -> SubSequence {
    Log.prefixThrough[selfType] += 1
    return base.prefix(through: position)
  }

  public func suffix(from start: Index) -> SubSequence {
    Log.suffixFrom[selfType] += 1
    return base.suffix(from: start)
  }

  public var isEmpty: Bool {
    Log.isEmpty[selfType] += 1
    return base.isEmpty
  }

  public var count: Int {
    Log.count[selfType] += 1
    return base.count
  }

  public func _customIndexOfEquatableElement(
    _ element: Base.Iterator.Element
  ) -> Index?? {
    Log._customIndexOfEquatableElement[selfType] += 1
    return base._customIndexOfEquatableElement(element)
  }

  public var first: Base.Iterator.Element? {
    Log.first[selfType] += 1
    return base.first
  }

  public func index(_ i: Index, offsetBy n: Int) -> Index {
    Log.advance[selfType] += 1
    return base.index(i, offsetBy: n)
  }

  public func index(
    _ i: Index, offsetBy n: Int, limitedBy limit: Index
  ) -> Index? {
    Log.advanceLimit[selfType] += 1
    return base.index(i, offsetBy: n, limitedBy: limit)
  }

  public func distance(from start: Index, to end: Index) -> Int {
    Log.distance[selfType] += 1
    return base.distance(from: start, to: end)
  }


  public func index(before i: Index) -> Index {
    BidirectionalCollectionLog.predecessor[selfType] += 1
    return base.index(before: i)
  }

  public func formIndex(before i: inout Index) {
    BidirectionalCollectionLog.formPredecessor[selfType] += 1
    base.formIndex(before: &i)
  }

  public var last: Iterator.Element? {
    BidirectionalCollectionLog.last[selfType] += 1
    return base.last
  }
}
/// Interposes between `Collection` method calls to increment each method's
/// counter.
public struct LoggingRandomAccessCollection<
  Base : Collection & RandomAccessCollection
> : RandomAccessCollection, Collection, LoggingType {

  public var base: Base

  public typealias Log = CollectionLog

  public init(wrapping base: Base) {
    self.base = base
  }

  public typealias Iterator = LoggingIterator<Base.Iterator>

  public func makeIterator() -> Iterator {
    Log.makeIterator[selfType] += 1
    return LoggingIterator(wrapping: base.makeIterator())
  }

  public var underestimatedCount: Int {
    Log.underestimatedCount[selfType] += 1
    return base.underestimatedCount
  }

  public func map<T>(
    _ transform: (Base.Iterator.Element) throws -> T
  ) rethrows -> [T] {
    Log.map[selfType] += 1
    return try base.map(transform)
  }

  public func filter(
    _ isIncluded: (Base.Iterator.Element) throws -> Bool
  ) rethrows -> [Base.Iterator.Element] {
    Log.filter[selfType] += 1
    return try base.filter(isIncluded)
  }

  public func forEach(
    _ body: (Base.Iterator.Element) throws -> Void
  ) rethrows {
    Log.forEach[selfType] += 1
    try base.forEach(body)
  }

  public typealias SubSequence = Base.SubSequence

  public func dropFirst(_ n: Int) -> SubSequence {
    Log.dropFirst[selfType] += 1
    return base.dropFirst(n)
  }

  public func dropLast(_ n: Int) -> SubSequence {
    Log.dropLast[selfType] += 1
    return base.dropLast(n)
  }

  public func drop(
    while predicate: (Base.Iterator.Element) throws -> Bool
  ) rethrows -> SubSequence {
    Log.dropWhile[selfType] += 1
    return try base.drop(while: predicate)
  }

  public func prefix(_ maxLength: Int) -> SubSequence {
    Log.prefixMaxLength[selfType] += 1
    return base.prefix(maxLength)
  }

  public func prefix(
    while predicate: (Base.Iterator.Element) throws -> Bool
  ) rethrows -> SubSequence {
    Log.prefixWhile[selfType] += 1
    return try base.prefix(while: predicate)
  }

  public func suffix(_ maxLength: Int) -> SubSequence {
    Log.suffixMaxLength[selfType] += 1
    return base.suffix(maxLength)
  }

  public func split(
    maxSplits: Int = Int.max,
    omittingEmptySubsequences: Bool = true,
    whereSeparator isSeparator: (Base.Iterator.Element) throws -> Bool
  ) rethrows -> [SubSequence] {
    Log.split[selfType] += 1
    return try base.split(
      maxSplits: maxSplits,
      omittingEmptySubsequences: omittingEmptySubsequences,
      whereSeparator: isSeparator)
  }

  public func _customContainsEquatableElement(
    _ element: Base.Iterator.Element
  ) -> Bool? {
    Log._customContainsEquatableElement[selfType] += 1
    return base._customContainsEquatableElement(element)
  }

  /// If `self` is multi-pass (i.e., a `Collection`), invoke
  /// `preprocess` on `self` and return its result.  Otherwise, return
  /// `nil`.
  public func _preprocessingPass<R>(
    _ preprocess: () throws -> R
  ) rethrows -> R? {
    Log._preprocessingPass[selfType] += 1
    return try base._preprocessingPass(preprocess)
  }

  /// Create a native array buffer containing the elements of `self`,
  /// in the same order.
  public func _copyToContiguousArray()
    -> ContiguousArray<Base.Iterator.Element> {
    Log._copyToContiguousArray[selfType] += 1
    return base._copyToContiguousArray()
  }

  /// Copy a Sequence into an array.
  public func _copyContents(
    initializing buffer: UnsafeMutableBufferPointer<Iterator.Element>
  ) -> (Iterator,UnsafeMutableBufferPointer<Iterator.Element>.Index) {
    Log._copyContents[selfType] += 1
    let (it,idx) = base._copyContents(initializing: buffer)
    return (Iterator(wrapping: it),idx)
  }

  public typealias Index = Base.Index

  public var startIndex: Index {
    Log.startIndex[selfType] += 1
    return base.startIndex
  }

  public var endIndex: Index {
    Log.endIndex[selfType] += 1
    return base.endIndex
  }

  public subscript(position: Index) -> Base.Iterator.Element {
    get {
      Log.subscriptIndex[selfType] += 1
      return base[position]
    }
  }

  public subscript(bounds: Range<Index>) -> SubSequence {
    get {
      Log.subscriptRange[selfType] += 1
      return base[bounds]
    }
  }

  public func _failEarlyRangeCheck(_ index: Index, bounds: Range<Index>) {
    Log._failEarlyRangeCheckIndex[selfType] += 1
    base._failEarlyRangeCheck(index, bounds: bounds)
  }

  public func _failEarlyRangeCheck(_ range: Range<Index>, bounds: Range<Index>) {
    Log._failEarlyRangeCheckRange[selfType] += 1
    base._failEarlyRangeCheck(range, bounds: bounds)
  }

  public func index(after i: Index) -> Index {
    Log.successor[selfType] += 1
    return base.index(after: i)
  }

  public func formIndex(after i: inout Index) {
    Log.formSuccessor[selfType] += 1
    base.formIndex(after: &i)
  }

  public typealias Indices = Base.Indices

  public var indices: Indices {
    Log.indices[selfType] += 1
    return base.indices
  }

  public func prefix(upTo end: Index) -> SubSequence {
    Log.prefixUpTo[selfType] += 1
    return base.prefix(upTo: end)
  }

  public func prefix(through position: Index) -> SubSequence {
    Log.prefixThrough[selfType] += 1
    return base.prefix(through: position)
  }

  public func suffix(from start: Index) -> SubSequence {
    Log.suffixFrom[selfType] += 1
    return base.suffix(from: start)
  }

  public var isEmpty: Bool {
    Log.isEmpty[selfType] += 1
    return base.isEmpty
  }

  public var count: Int {
    Log.count[selfType] += 1
    return base.count
  }

  public func _customIndexOfEquatableElement(
    _ element: Base.Iterator.Element
  ) -> Index?? {
    Log._customIndexOfEquatableElement[selfType] += 1
    return base._customIndexOfEquatableElement(element)
  }

  public var first: Base.Iterator.Element? {
    Log.first[selfType] += 1
    return base.first
  }

  public func index(_ i: Index, offsetBy n: Int) -> Index {
    Log.advance[selfType] += 1
    return base.index(i, offsetBy: n)
  }

  public func index(
    _ i: Index, offsetBy n: Int, limitedBy limit: Index
  ) -> Index? {
    Log.advanceLimit[selfType] += 1
    return base.index(i, offsetBy: n, limitedBy: limit)
  }

  public func distance(from start: Index, to end: Index) -> Int {
    Log.distance[selfType] += 1
    return base.distance(from: start, to: end)
  }


  public func index(before i: Index) -> Index {
    BidirectionalCollectionLog.predecessor[selfType] += 1
    return base.index(before: i)
  }

  public func formIndex(before i: inout Index) {
    BidirectionalCollectionLog.formPredecessor[selfType] += 1
    base.formIndex(before: &i)
  }

  public var last: Iterator.Element? {
    BidirectionalCollectionLog.last[selfType] += 1
    return base.last
  }
}
/// Interposes between `MutableCollection` method calls to increment each method's
/// counter.
public struct LoggingMutableCollection<
  Base : MutableCollection & Collection
> : Collection, MutableCollection, LoggingType {

  public var base: Base

  public typealias Log = MutableCollectionLog

  public init(wrapping base: Base) {
    self.base = base
  }

  public typealias Iterator = LoggingIterator<Base.Iterator>

  public func makeIterator() -> Iterator {
    Log.makeIterator[selfType] += 1
    return LoggingIterator(wrapping: base.makeIterator())
  }

  public var underestimatedCount: Int {
    Log.underestimatedCount[selfType] += 1
    return base.underestimatedCount
  }

  public func map<T>(
    _ transform: (Base.Iterator.Element) throws -> T
  ) rethrows -> [T] {
    Log.map[selfType] += 1
    return try base.map(transform)
  }

  public func filter(
    _ isIncluded: (Base.Iterator.Element) throws -> Bool
  ) rethrows -> [Base.Iterator.Element] {
    Log.filter[selfType] += 1
    return try base.filter(isIncluded)
  }

  public func forEach(
    _ body: (Base.Iterator.Element) throws -> Void
  ) rethrows {
    Log.forEach[selfType] += 1
    try base.forEach(body)
  }

  public typealias SubSequence = Base.SubSequence

  public func dropFirst(_ n: Int) -> SubSequence {
    Log.dropFirst[selfType] += 1
    return base.dropFirst(n)
  }

  public func dropLast(_ n: Int) -> SubSequence {
    Log.dropLast[selfType] += 1
    return base.dropLast(n)
  }

  public func drop(
    while predicate: (Base.Iterator.Element) throws -> Bool
  ) rethrows -> SubSequence {
    Log.dropWhile[selfType] += 1
    return try base.drop(while: predicate)
  }

  public func prefix(_ maxLength: Int) -> SubSequence {
    Log.prefixMaxLength[selfType] += 1
    return base.prefix(maxLength)
  }

  public func prefix(
    while predicate: (Base.Iterator.Element) throws -> Bool
  ) rethrows -> SubSequence {
    Log.prefixWhile[selfType] += 1
    return try base.prefix(while: predicate)
  }

  public func suffix(_ maxLength: Int) -> SubSequence {
    Log.suffixMaxLength[selfType] += 1
    return base.suffix(maxLength)
  }

  public func split(
    maxSplits: Int = Int.max,
    omittingEmptySubsequences: Bool = true,
    whereSeparator isSeparator: (Base.Iterator.Element) throws -> Bool
  ) rethrows -> [SubSequence] {
    Log.split[selfType] += 1
    return try base.split(
      maxSplits: maxSplits,
      omittingEmptySubsequences: omittingEmptySubsequences,
      whereSeparator: isSeparator)
  }

  public func _customContainsEquatableElement(
    _ element: Base.Iterator.Element
  ) -> Bool? {
    Log._customContainsEquatableElement[selfType] += 1
    return base._customContainsEquatableElement(element)
  }

  /// If `self` is multi-pass (i.e., a `Collection`), invoke
  /// `preprocess` on `self` and return its result.  Otherwise, return
  /// `nil`.
  public func _preprocessingPass<R>(
    _ preprocess: () throws -> R
  ) rethrows -> R? {
    Log._preprocessingPass[selfType] += 1
    return try base._preprocessingPass(preprocess)
  }

  /// Create a native array buffer containing the elements of `self`,
  /// in the same order.
  public func _copyToContiguousArray()
    -> ContiguousArray<Base.Iterator.Element> {
    Log._copyToContiguousArray[selfType] += 1
    return base._copyToContiguousArray()
  }

  /// Copy a Sequence into an array.
  public func _copyContents(
    initializing buffer: UnsafeMutableBufferPointer<Iterator.Element>
  ) -> (Iterator,UnsafeMutableBufferPointer<Iterator.Element>.Index) {
    Log._copyContents[selfType] += 1
    let (it,idx) = base._copyContents(initializing: buffer)
    return (Iterator(wrapping: it),idx)
  }

  public typealias Index = Base.Index

  public var startIndex: Index {
    Log.startIndex[selfType] += 1
    return base.startIndex
  }

  public var endIndex: Index {
    Log.endIndex[selfType] += 1
    return base.endIndex
  }

  public subscript(position: Index) -> Base.Iterator.Element {
    get {
      Log.subscriptIndex[selfType] += 1
      return base[position]
    }
    set {
      Log.subscriptIndexSet[selfType] += 1
      base[position] = newValue
    }
  }

  public subscript(bounds: Range<Index>) -> SubSequence {
    get {
      Log.subscriptRange[selfType] += 1
      return base[bounds]
    }
    set {
      Log.subscriptRangeSet[selfType] += 1
      base[bounds] = newValue
    }
  }

  public func _failEarlyRangeCheck(_ index: Index, bounds: Range<Index>) {
    Log._failEarlyRangeCheckIndex[selfType] += 1
    base._failEarlyRangeCheck(index, bounds: bounds)
  }

  public func _failEarlyRangeCheck(_ range: Range<Index>, bounds: Range<Index>) {
    Log._failEarlyRangeCheckRange[selfType] += 1
    base._failEarlyRangeCheck(range, bounds: bounds)
  }

  public func index(after i: Index) -> Index {
    Log.successor[selfType] += 1
    return base.index(after: i)
  }

  public func formIndex(after i: inout Index) {
    Log.formSuccessor[selfType] += 1
    base.formIndex(after: &i)
  }

  public typealias Indices = Base.Indices

  public var indices: Indices {
    Log.indices[selfType] += 1
    return base.indices
  }

  public func prefix(upTo end: Index) -> SubSequence {
    Log.prefixUpTo[selfType] += 1
    return base.prefix(upTo: end)
  }

  public func prefix(through position: Index) -> SubSequence {
    Log.prefixThrough[selfType] += 1
    return base.prefix(through: position)
  }

  public func suffix(from start: Index) -> SubSequence {
    Log.suffixFrom[selfType] += 1
    return base.suffix(from: start)
  }

  public var isEmpty: Bool {
    Log.isEmpty[selfType] += 1
    return base.isEmpty
  }

  public var count: Int {
    Log.count[selfType] += 1
    return base.count
  }

  public func _customIndexOfEquatableElement(
    _ element: Base.Iterator.Element
  ) -> Index?? {
    Log._customIndexOfEquatableElement[selfType] += 1
    return base._customIndexOfEquatableElement(element)
  }

  public var first: Base.Iterator.Element? {
    Log.first[selfType] += 1
    return base.first
  }

  public func index(_ i: Index, offsetBy n: Int) -> Index {
    Log.advance[selfType] += 1
    return base.index(i, offsetBy: n)
  }

  public func index(
    _ i: Index, offsetBy n: Int, limitedBy limit: Index
  ) -> Index? {
    Log.advanceLimit[selfType] += 1
    return base.index(i, offsetBy: n, limitedBy: limit)
  }

  public func distance(from start: Index, to end: Index) -> Int {
    Log.distance[selfType] += 1
    return base.distance(from: start, to: end)
  }

  public mutating func partition(
    by belongsInSecondPartition: (Iterator.Element) throws -> Bool
  ) rethrows -> Index {
    Log.partitionBy[selfType] += 1
    return try base.partition(by: belongsInSecondPartition)
  }

  public mutating func _withUnsafeMutableBufferPointerIfSupported<R>(
    _ body: (inout UnsafeMutableBufferPointer<Iterator.Element>) throws -> R
  ) rethrows -> R? {
    Log._withUnsafeMutableBufferPointerIfSupported[selfType] += 1
    let result = try base._withUnsafeMutableBufferPointerIfSupported(body)
    if result != nil {
      Log._withUnsafeMutableBufferPointerIfSupportedNonNilReturns[selfType] += 1
    }
    return result
  }

}
/// Interposes between `MutableCollection` method calls to increment each method's
/// counter.
public struct LoggingMutableBidirectionalCollection<
  Base : MutableCollection & BidirectionalCollection
> : BidirectionalCollection, MutableCollection, LoggingType {

  public var base: Base

  public typealias Log = MutableCollectionLog

  public init(wrapping base: Base) {
    self.base = base
  }

  public typealias Iterator = LoggingIterator<Base.Iterator>

  public func makeIterator() -> Iterator {
    Log.makeIterator[selfType] += 1
    return LoggingIterator(wrapping: base.makeIterator())
  }

  public var underestimatedCount: Int {
    Log.underestimatedCount[selfType] += 1
    return base.underestimatedCount
  }

  public func map<T>(
    _ transform: (Base.Iterator.Element) throws -> T
  ) rethrows -> [T] {
    Log.map[selfType] += 1
    return try base.map(transform)
  }

  public func filter(
    _ isIncluded: (Base.Iterator.Element) throws -> Bool
  ) rethrows -> [Base.Iterator.Element] {
    Log.filter[selfType] += 1
    return try base.filter(isIncluded)
  }

  public func forEach(
    _ body: (Base.Iterator.Element) throws -> Void
  ) rethrows {
    Log.forEach[selfType] += 1
    try base.forEach(body)
  }

  public typealias SubSequence = Base.SubSequence

  public func dropFirst(_ n: Int) -> SubSequence {
    Log.dropFirst[selfType] += 1
    return base.dropFirst(n)
  }

  public func dropLast(_ n: Int) -> SubSequence {
    Log.dropLast[selfType] += 1
    return base.dropLast(n)
  }

  public func drop(
    while predicate: (Base.Iterator.Element) throws -> Bool
  ) rethrows -> SubSequence {
    Log.dropWhile[selfType] += 1
    return try base.drop(while: predicate)
  }

  public func prefix(_ maxLength: Int) -> SubSequence {
    Log.prefixMaxLength[selfType] += 1
    return base.prefix(maxLength)
  }

  public func prefix(
    while predicate: (Base.Iterator.Element) throws -> Bool
  ) rethrows -> SubSequence {
    Log.prefixWhile[selfType] += 1
    return try base.prefix(while: predicate)
  }

  public func suffix(_ maxLength: Int) -> SubSequence {
    Log.suffixMaxLength[selfType] += 1
    return base.suffix(maxLength)
  }

  public func split(
    maxSplits: Int = Int.max,
    omittingEmptySubsequences: Bool = true,
    whereSeparator isSeparator: (Base.Iterator.Element) throws -> Bool
  ) rethrows -> [SubSequence] {
    Log.split[selfType] += 1
    return try base.split(
      maxSplits: maxSplits,
      omittingEmptySubsequences: omittingEmptySubsequences,
      whereSeparator: isSeparator)
  }

  public func _customContainsEquatableElement(
    _ element: Base.Iterator.Element
  ) -> Bool? {
    Log._customContainsEquatableElement[selfType] += 1
    return base._customContainsEquatableElement(element)
  }

  /// If `self` is multi-pass (i.e., a `Collection`), invoke
  /// `preprocess` on `self` and return its result.  Otherwise, return
  /// `nil`.
  public func _preprocessingPass<R>(
    _ preprocess: () throws -> R
  ) rethrows -> R? {
    Log._preprocessingPass[selfType] += 1
    return try base._preprocessingPass(preprocess)
  }

  /// Create a native array buffer containing the elements of `self`,
  /// in the same order.
  public func _copyToContiguousArray()
    -> ContiguousArray<Base.Iterator.Element> {
    Log._copyToContiguousArray[selfType] += 1
    return base._copyToContiguousArray()
  }

  /// Copy a Sequence into an array.
  public func _copyContents(
    initializing buffer: UnsafeMutableBufferPointer<Iterator.Element>
  ) -> (Iterator,UnsafeMutableBufferPointer<Iterator.Element>.Index) {
    Log._copyContents[selfType] += 1
    let (it,idx) = base._copyContents(initializing: buffer)
    return (Iterator(wrapping: it),idx)
  }

  public typealias Index = Base.Index

  public var startIndex: Index {
    Log.startIndex[selfType] += 1
    return base.startIndex
  }

  public var endIndex: Index {
    Log.endIndex[selfType] += 1
    return base.endIndex
  }

  public subscript(position: Index) -> Base.Iterator.Element {
    get {
      Log.subscriptIndex[selfType] += 1
      return base[position]
    }
    set {
      Log.subscriptIndexSet[selfType] += 1
      base[position] = newValue
    }
  }

  public subscript(bounds: Range<Index>) -> SubSequence {
    get {
      Log.subscriptRange[selfType] += 1
      return base[bounds]
    }
    set {
      Log.subscriptRangeSet[selfType] += 1
      base[bounds] = newValue
    }
  }

  public func _failEarlyRangeCheck(_ index: Index, bounds: Range<Index>) {
    Log._failEarlyRangeCheckIndex[selfType] += 1
    base._failEarlyRangeCheck(index, bounds: bounds)
  }

  public func _failEarlyRangeCheck(_ range: Range<Index>, bounds: Range<Index>) {
    Log._failEarlyRangeCheckRange[selfType] += 1
    base._failEarlyRangeCheck(range, bounds: bounds)
  }

  public func index(after i: Index) -> Index {
    Log.successor[selfType] += 1
    return base.index(after: i)
  }

  public func formIndex(after i: inout Index) {
    Log.formSuccessor[selfType] += 1
    base.formIndex(after: &i)
  }

  public typealias Indices = Base.Indices

  public var indices: Indices {
    Log.indices[selfType] += 1
    return base.indices
  }

  public func prefix(upTo end: Index) -> SubSequence {
    Log.prefixUpTo[selfType] += 1
    return base.prefix(upTo: end)
  }

  public func prefix(through position: Index) -> SubSequence {
    Log.prefixThrough[selfType] += 1
    return base.prefix(through: position)
  }

  public func suffix(from start: Index) -> SubSequence {
    Log.suffixFrom[selfType] += 1
    return base.suffix(from: start)
  }

  public var isEmpty: Bool {
    Log.isEmpty[selfType] += 1
    return base.isEmpty
  }

  public var count: Int {
    Log.count[selfType] += 1
    return base.count
  }

  public func _customIndexOfEquatableElement(
    _ element: Base.Iterator.Element
  ) -> Index?? {
    Log._customIndexOfEquatableElement[selfType] += 1
    return base._customIndexOfEquatableElement(element)
  }

  public var first: Base.Iterator.Element? {
    Log.first[selfType] += 1
    return base.first
  }

  public func index(_ i: Index, offsetBy n: Int) -> Index {
    Log.advance[selfType] += 1
    return base.index(i, offsetBy: n)
  }

  public func index(
    _ i: Index, offsetBy n: Int, limitedBy limit: Index
  ) -> Index? {
    Log.advanceLimit[selfType] += 1
    return base.index(i, offsetBy: n, limitedBy: limit)
  }

  public func distance(from start: Index, to end: Index) -> Int {
    Log.distance[selfType] += 1
    return base.distance(from: start, to: end)
  }

  public mutating func partition(
    by belongsInSecondPartition: (Iterator.Element) throws -> Bool
  ) rethrows -> Index {
    Log.partitionBy[selfType] += 1
    return try base.partition(by: belongsInSecondPartition)
  }

  public mutating func _withUnsafeMutableBufferPointerIfSupported<R>(
    _ body: (inout UnsafeMutableBufferPointer<Iterator.Element>) throws -> R
  ) rethrows -> R? {
    Log._withUnsafeMutableBufferPointerIfSupported[selfType] += 1
    let result = try base._withUnsafeMutableBufferPointerIfSupported(body)
    if result != nil {
      Log._withUnsafeMutableBufferPointerIfSupportedNonNilReturns[selfType] += 1
    }
    return result
  }

  public func index(before i: Index) -> Index {
    BidirectionalCollectionLog.predecessor[selfType] += 1
    return base.index(before: i)
  }

  public func formIndex(before i: inout Index) {
    BidirectionalCollectionLog.formPredecessor[selfType] += 1
    base.formIndex(before: &i)
  }

  public var last: Iterator.Element? {
    BidirectionalCollectionLog.last[selfType] += 1
    return base.last
  }
}
/// Interposes between `MutableCollection` method calls to increment each method's
/// counter.
public struct LoggingMutableRandomAccessCollection<
  Base : MutableCollection & RandomAccessCollection
> : RandomAccessCollection, MutableCollection, LoggingType {

  public var base: Base

  public typealias Log = MutableCollectionLog

  public init(wrapping base: Base) {
    self.base = base
  }

  public typealias Iterator = LoggingIterator<Base.Iterator>

  public func makeIterator() -> Iterator {
    Log.makeIterator[selfType] += 1
    return LoggingIterator(wrapping: base.makeIterator())
  }

  public var underestimatedCount: Int {
    Log.underestimatedCount[selfType] += 1
    return base.underestimatedCount
  }

  public func map<T>(
    _ transform: (Base.Iterator.Element) throws -> T
  ) rethrows -> [T] {
    Log.map[selfType] += 1
    return try base.map(transform)
  }

  public func filter(
    _ isIncluded: (Base.Iterator.Element) throws -> Bool
  ) rethrows -> [Base.Iterator.Element] {
    Log.filter[selfType] += 1
    return try base.filter(isIncluded)
  }

  public func forEach(
    _ body: (Base.Iterator.Element) throws -> Void
  ) rethrows {
    Log.forEach[selfType] += 1
    try base.forEach(body)
  }

  public typealias SubSequence = Base.SubSequence

  public func dropFirst(_ n: Int) -> SubSequence {
    Log.dropFirst[selfType] += 1
    return base.dropFirst(n)
  }

  public func dropLast(_ n: Int) -> SubSequence {
    Log.dropLast[selfType] += 1
    return base.dropLast(n)
  }

  public func drop(
    while predicate: (Base.Iterator.Element) throws -> Bool
  ) rethrows -> SubSequence {
    Log.dropWhile[selfType] += 1
    return try base.drop(while: predicate)
  }

  public func prefix(_ maxLength: Int) -> SubSequence {
    Log.prefixMaxLength[selfType] += 1
    return base.prefix(maxLength)
  }

  public func prefix(
    while predicate: (Base.Iterator.Element) throws -> Bool
  ) rethrows -> SubSequence {
    Log.prefixWhile[selfType] += 1
    return try base.prefix(while: predicate)
  }

  public func suffix(_ maxLength: Int) -> SubSequence {
    Log.suffixMaxLength[selfType] += 1
    return base.suffix(maxLength)
  }

  public func split(
    maxSplits: Int = Int.max,
    omittingEmptySubsequences: Bool = true,
    whereSeparator isSeparator: (Base.Iterator.Element) throws -> Bool
  ) rethrows -> [SubSequence] {
    Log.split[selfType] += 1
    return try base.split(
      maxSplits: maxSplits,
      omittingEmptySubsequences: omittingEmptySubsequences,
      whereSeparator: isSeparator)
  }

  public func _customContainsEquatableElement(
    _ element: Base.Iterator.Element
  ) -> Bool? {
    Log._customContainsEquatableElement[selfType] += 1
    return base._customContainsEquatableElement(element)
  }

  /// If `self` is multi-pass (i.e., a `Collection`), invoke
  /// `preprocess` on `self` and return its result.  Otherwise, return
  /// `nil`.
  public func _preprocessingPass<R>(
    _ preprocess: () throws -> R
  ) rethrows -> R? {
    Log._preprocessingPass[selfType] += 1
    return try base._preprocessingPass(preprocess)
  }

  /// Create a native array buffer containing the elements of `self`,
  /// in the same order.
  public func _copyToContiguousArray()
    -> ContiguousArray<Base.Iterator.Element> {
    Log._copyToContiguousArray[selfType] += 1
    return base._copyToContiguousArray()
  }

  /// Copy a Sequence into an array.
  public func _copyContents(
    initializing buffer: UnsafeMutableBufferPointer<Iterator.Element>
  ) -> (Iterator,UnsafeMutableBufferPointer<Iterator.Element>.Index) {
    Log._copyContents[selfType] += 1
    let (it,idx) = base._copyContents(initializing: buffer)
    return (Iterator(wrapping: it),idx)
  }

  public typealias Index = Base.Index

  public var startIndex: Index {
    Log.startIndex[selfType] += 1
    return base.startIndex
  }

  public var endIndex: Index {
    Log.endIndex[selfType] += 1
    return base.endIndex
  }

  public subscript(position: Index) -> Base.Iterator.Element {
    get {
      Log.subscriptIndex[selfType] += 1
      return base[position]
    }
    set {
      Log.subscriptIndexSet[selfType] += 1
      base[position] = newValue
    }
  }

  public subscript(bounds: Range<Index>) -> SubSequence {
    get {
      Log.subscriptRange[selfType] += 1
      return base[bounds]
    }
    set {
      Log.subscriptRangeSet[selfType] += 1
      base[bounds] = newValue
    }
  }

  public func _failEarlyRangeCheck(_ index: Index, bounds: Range<Index>) {
    Log._failEarlyRangeCheckIndex[selfType] += 1
    base._failEarlyRangeCheck(index, bounds: bounds)
  }

  public func _failEarlyRangeCheck(_ range: Range<Index>, bounds: Range<Index>) {
    Log._failEarlyRangeCheckRange[selfType] += 1
    base._failEarlyRangeCheck(range, bounds: bounds)
  }

  public func index(after i: Index) -> Index {
    Log.successor[selfType] += 1
    return base.index(after: i)
  }

  public func formIndex(after i: inout Index) {
    Log.formSuccessor[selfType] += 1
    base.formIndex(after: &i)
  }

  public typealias Indices = Base.Indices

  public var indices: Indices {
    Log.indices[selfType] += 1
    return base.indices
  }

  public func prefix(upTo end: Index) -> SubSequence {
    Log.prefixUpTo[selfType] += 1
    return base.prefix(upTo: end)
  }

  public func prefix(through position: Index) -> SubSequence {
    Log.prefixThrough[selfType] += 1
    return base.prefix(through: position)
  }

  public func suffix(from start: Index) -> SubSequence {
    Log.suffixFrom[selfType] += 1
    return base.suffix(from: start)
  }

  public var isEmpty: Bool {
    Log.isEmpty[selfType] += 1
    return base.isEmpty
  }

  public var count: Int {
    Log.count[selfType] += 1
    return base.count
  }

  public func _customIndexOfEquatableElement(
    _ element: Base.Iterator.Element
  ) -> Index?? {
    Log._customIndexOfEquatableElement[selfType] += 1
    return base._customIndexOfEquatableElement(element)
  }

  public var first: Base.Iterator.Element? {
    Log.first[selfType] += 1
    return base.first
  }

  public func index(_ i: Index, offsetBy n: Int) -> Index {
    Log.advance[selfType] += 1
    return base.index(i, offsetBy: n)
  }

  public func index(
    _ i: Index, offsetBy n: Int, limitedBy limit: Index
  ) -> Index? {
    Log.advanceLimit[selfType] += 1
    return base.index(i, offsetBy: n, limitedBy: limit)
  }

  public func distance(from start: Index, to end: Index) -> Int {
    Log.distance[selfType] += 1
    return base.distance(from: start, to: end)
  }

  public mutating func partition(
    by belongsInSecondPartition: (Iterator.Element) throws -> Bool
  ) rethrows -> Index {
    Log.partitionBy[selfType] += 1
    return try base.partition(by: belongsInSecondPartition)
  }

  public mutating func _withUnsafeMutableBufferPointerIfSupported<R>(
    _ body: (inout UnsafeMutableBufferPointer<Iterator.Element>) throws -> R
  ) rethrows -> R? {
    Log._withUnsafeMutableBufferPointerIfSupported[selfType] += 1
    let result = try base._withUnsafeMutableBufferPointerIfSupported(body)
    if result != nil {
      Log._withUnsafeMutableBufferPointerIfSupportedNonNilReturns[selfType] += 1
    }
    return result
  }

  public func index(before i: Index) -> Index {
    BidirectionalCollectionLog.predecessor[selfType] += 1
    return base.index(before: i)
  }

  public func formIndex(before i: inout Index) {
    BidirectionalCollectionLog.formPredecessor[selfType] += 1
    base.formIndex(before: &i)
  }

  public var last: Iterator.Element? {
    BidirectionalCollectionLog.last[selfType] += 1
    return base.last
  }
}
/// Interposes between `RangeReplaceableCollection` method calls to increment each method's
/// counter.
public struct LoggingRangeReplaceableCollection<
  Base : RangeReplaceableCollection & Collection
> : Collection, RangeReplaceableCollection, LoggingType {

  public var base: Base

  public typealias Log = RangeReplaceableCollectionLog

  public init(wrapping base: Base) {
    self.base = base
  }

  public typealias Iterator = LoggingIterator<Base.Iterator>

  public func makeIterator() -> Iterator {
    Log.makeIterator[selfType] += 1
    return LoggingIterator(wrapping: base.makeIterator())
  }

  public var underestimatedCount: Int {
    Log.underestimatedCount[selfType] += 1
    return base.underestimatedCount
  }

  public func map<T>(
    _ transform: (Base.Iterator.Element) throws -> T
  ) rethrows -> [T] {
    Log.map[selfType] += 1
    return try base.map(transform)
  }

  public func filter(
    _ isIncluded: (Base.Iterator.Element) throws -> Bool
  ) rethrows -> [Base.Iterator.Element] {
    Log.filter[selfType] += 1
    return try base.filter(isIncluded)
  }

  public func forEach(
    _ body: (Base.Iterator.Element) throws -> Void
  ) rethrows {
    Log.forEach[selfType] += 1
    try base.forEach(body)
  }

  public typealias SubSequence = Base.SubSequence

  public func dropFirst(_ n: Int) -> SubSequence {
    Log.dropFirst[selfType] += 1
    return base.dropFirst(n)
  }

  public func dropLast(_ n: Int) -> SubSequence {
    Log.dropLast[selfType] += 1
    return base.dropLast(n)
  }

  public func drop(
    while predicate: (Base.Iterator.Element) throws -> Bool
  ) rethrows -> SubSequence {
    Log.dropWhile[selfType] += 1
    return try base.drop(while: predicate)
  }

  public func prefix(_ maxLength: Int) -> SubSequence {
    Log.prefixMaxLength[selfType] += 1
    return base.prefix(maxLength)
  }

  public func prefix(
    while predicate: (Base.Iterator.Element) throws -> Bool
  ) rethrows -> SubSequence {
    Log.prefixWhile[selfType] += 1
    return try base.prefix(while: predicate)
  }

  public func suffix(_ maxLength: Int) -> SubSequence {
    Log.suffixMaxLength[selfType] += 1
    return base.suffix(maxLength)
  }

  public func split(
    maxSplits: Int = Int.max,
    omittingEmptySubsequences: Bool = true,
    whereSeparator isSeparator: (Base.Iterator.Element) throws -> Bool
  ) rethrows -> [SubSequence] {
    Log.split[selfType] += 1
    return try base.split(
      maxSplits: maxSplits,
      omittingEmptySubsequences: omittingEmptySubsequences,
      whereSeparator: isSeparator)
  }

  public func _customContainsEquatableElement(
    _ element: Base.Iterator.Element
  ) -> Bool? {
    Log._customContainsEquatableElement[selfType] += 1
    return base._customContainsEquatableElement(element)
  }

  /// If `self` is multi-pass (i.e., a `Collection`), invoke
  /// `preprocess` on `self` and return its result.  Otherwise, return
  /// `nil`.
  public func _preprocessingPass<R>(
    _ preprocess: () throws -> R
  ) rethrows -> R? {
    Log._preprocessingPass[selfType] += 1
    return try base._preprocessingPass(preprocess)
  }

  /// Create a native array buffer containing the elements of `self`,
  /// in the same order.
  public func _copyToContiguousArray()
    -> ContiguousArray<Base.Iterator.Element> {
    Log._copyToContiguousArray[selfType] += 1
    return base._copyToContiguousArray()
  }

  /// Copy a Sequence into an array.
  public func _copyContents(
    initializing buffer: UnsafeMutableBufferPointer<Iterator.Element>
  ) -> (Iterator,UnsafeMutableBufferPointer<Iterator.Element>.Index) {
    Log._copyContents[selfType] += 1
    let (it,idx) = base._copyContents(initializing: buffer)
    return (Iterator(wrapping: it),idx)
  }

  public typealias Index = Base.Index

  public var startIndex: Index {
    Log.startIndex[selfType] += 1
    return base.startIndex
  }

  public var endIndex: Index {
    Log.endIndex[selfType] += 1
    return base.endIndex
  }

  public subscript(position: Index) -> Base.Iterator.Element {
    get {
      Log.subscriptIndex[selfType] += 1
      return base[position]
    }
  }

  public subscript(bounds: Range<Index>) -> SubSequence {
    get {
      Log.subscriptRange[selfType] += 1
      return base[bounds]
    }
  }

  public func _failEarlyRangeCheck(_ index: Index, bounds: Range<Index>) {
    Log._failEarlyRangeCheckIndex[selfType] += 1
    base._failEarlyRangeCheck(index, bounds: bounds)
  }

  public func _failEarlyRangeCheck(_ range: Range<Index>, bounds: Range<Index>) {
    Log._failEarlyRangeCheckRange[selfType] += 1
    base._failEarlyRangeCheck(range, bounds: bounds)
  }

  public func index(after i: Index) -> Index {
    Log.successor[selfType] += 1
    return base.index(after: i)
  }

  public func formIndex(after i: inout Index) {
    Log.formSuccessor[selfType] += 1
    base.formIndex(after: &i)
  }

  public typealias Indices = Base.Indices

  public var indices: Indices {
    Log.indices[selfType] += 1
    return base.indices
  }

  public func prefix(upTo end: Index) -> SubSequence {
    Log.prefixUpTo[selfType] += 1
    return base.prefix(upTo: end)
  }

  public func prefix(through position: Index) -> SubSequence {
    Log.prefixThrough[selfType] += 1
    return base.prefix(through: position)
  }

  public func suffix(from start: Index) -> SubSequence {
    Log.suffixFrom[selfType] += 1
    return base.suffix(from: start)
  }

  public var isEmpty: Bool {
    Log.isEmpty[selfType] += 1
    return base.isEmpty
  }

  public var count: Int {
    Log.count[selfType] += 1
    return base.count
  }

  public func _customIndexOfEquatableElement(
    _ element: Base.Iterator.Element
  ) -> Index?? {
    Log._customIndexOfEquatableElement[selfType] += 1
    return base._customIndexOfEquatableElement(element)
  }

  public var first: Base.Iterator.Element? {
    Log.first[selfType] += 1
    return base.first
  }

  public func index(_ i: Index, offsetBy n: Int) -> Index {
    Log.advance[selfType] += 1
    return base.index(i, offsetBy: n)
  }

  public func index(
    _ i: Index, offsetBy n: Int, limitedBy limit: Index
  ) -> Index? {
    Log.advanceLimit[selfType] += 1
    return base.index(i, offsetBy: n, limitedBy: limit)
  }

  public func distance(from start: Index, to end: Index) -> Int {
    Log.distance[selfType] += 1
    return base.distance(from: start, to: end)
  }


  public init() {
    self.base = Base()
    Log.init_[selfType] += 1
  }

  public init(repeating repeatedValue: Iterator.Element, count: Int) {
    self.base = Base(repeating: repeatedValue, count: count)
    Log.initRepeating[selfType] += 1
  }

  public init<S : Sequence>(_ elements: S)
    where S.Iterator.Element == Iterator.Element {
    self.base = Base(elements)
    Log.initWithSequence[selfType] += 1
  }

  public mutating func _customRemoveLast() -> Base.Iterator.Element? {
    Log._customRemoveLast[selfType] += 1
    return base._customRemoveLast()
  }

  public mutating func _customRemoveLast(_ n: Int) -> Bool {
    Log._customRemoveLastN[selfType] += 1
    return base._customRemoveLast(n)
  }

  public mutating func append(_ newElement: Base.Iterator.Element) {
    Log.append[selfType] += 1
    base.append(newElement)
  }

  public mutating func append<S : Sequence>(
    contentsOf newElements: S
  ) where S.Iterator.Element == Base.Iterator.Element {
    Log.appendContentsOf[selfType] += 1
    base.append(contentsOf: newElements)
  }

  public mutating func insert(
    _ newElement: Base.Iterator.Element, at i: Index
  ) {
    Log.insert[selfType] += 1
    base.insert(newElement, at: i)
  }

  public mutating func insert<C : Collection>(
    contentsOf newElements: C, at i: Index
  ) where C.Iterator.Element == Base.Iterator.Element {
    Log.insertContentsOf[selfType] += 1
    base.insert(contentsOf: newElements, at: i)
  }

  public mutating func removeAll(keepingCapacity keepCapacity: Bool) {
    Log.removeAll[selfType] += 1
    base.removeAll(keepingCapacity: keepCapacity)
  }

  @discardableResult
  public mutating func remove(at index: Index) -> Base.Iterator.Element {
    Log.removeAt[selfType] += 1
    return base.remove(at: index)
  }

  @discardableResult
  public mutating func removeFirst() -> Base.Iterator.Element {
    Log.removeFirst[selfType] += 1
    return base.removeFirst()
  }

  public mutating func removeFirst(_ n: Int) {
    Log.removeFirstN[selfType] += 1
    base.removeFirst(n)
  }

  public mutating func removeSubrange(_ bounds: Range<Index>) {
    Log.removeSubrange[selfType] += 1
    base.removeSubrange(bounds)
  }

  public mutating func replaceSubrange<C : Collection>(
    _ bounds: Range<Index>, with newElements: C
  ) where C.Iterator.Element == Base.Iterator.Element {
    Log.replaceSubrange[selfType] += 1
    base.replaceSubrange(bounds, with: newElements)
  }

  public mutating func reserveCapacity(_ n: Int) {
    Log.reserveCapacity[selfType] += 1
    base.reserveCapacity(n)
  }
}
/// Interposes between `RangeReplaceableCollection` method calls to increment each method's
/// counter.
public struct LoggingRangeReplaceableBidirectionalCollection<
  Base : RangeReplaceableCollection & BidirectionalCollection
> : BidirectionalCollection, RangeReplaceableCollection, LoggingType {

  public var base: Base

  public typealias Log = RangeReplaceableCollectionLog

  public init(wrapping base: Base) {
    self.base = base
  }

  public typealias Iterator = LoggingIterator<Base.Iterator>

  public func makeIterator() -> Iterator {
    Log.makeIterator[selfType] += 1
    return LoggingIterator(wrapping: base.makeIterator())
  }

  public var underestimatedCount: Int {
    Log.underestimatedCount[selfType] += 1
    return base.underestimatedCount
  }

  public func map<T>(
    _ transform: (Base.Iterator.Element) throws -> T
  ) rethrows -> [T] {
    Log.map[selfType] += 1
    return try base.map(transform)
  }

  public func filter(
    _ isIncluded: (Base.Iterator.Element) throws -> Bool
  ) rethrows -> [Base.Iterator.Element] {
    Log.filter[selfType] += 1
    return try base.filter(isIncluded)
  }

  public func forEach(
    _ body: (Base.Iterator.Element) throws -> Void
  ) rethrows {
    Log.forEach[selfType] += 1
    try base.forEach(body)
  }

  public typealias SubSequence = Base.SubSequence

  public func dropFirst(_ n: Int) -> SubSequence {
    Log.dropFirst[selfType] += 1
    return base.dropFirst(n)
  }

  public func dropLast(_ n: Int) -> SubSequence {
    Log.dropLast[selfType] += 1
    return base.dropLast(n)
  }

  public func drop(
    while predicate: (Base.Iterator.Element) throws -> Bool
  ) rethrows -> SubSequence {
    Log.dropWhile[selfType] += 1
    return try base.drop(while: predicate)
  }

  public func prefix(_ maxLength: Int) -> SubSequence {
    Log.prefixMaxLength[selfType] += 1
    return base.prefix(maxLength)
  }

  public func prefix(
    while predicate: (Base.Iterator.Element) throws -> Bool
  ) rethrows -> SubSequence {
    Log.prefixWhile[selfType] += 1
    return try base.prefix(while: predicate)
  }

  public func suffix(_ maxLength: Int) -> SubSequence {
    Log.suffixMaxLength[selfType] += 1
    return base.suffix(maxLength)
  }

  public func split(
    maxSplits: Int = Int.max,
    omittingEmptySubsequences: Bool = true,
    whereSeparator isSeparator: (Base.Iterator.Element) throws -> Bool
  ) rethrows -> [SubSequence] {
    Log.split[selfType] += 1
    return try base.split(
      maxSplits: maxSplits,
      omittingEmptySubsequences: omittingEmptySubsequences,
      whereSeparator: isSeparator)
  }

  public func _customContainsEquatableElement(
    _ element: Base.Iterator.Element
  ) -> Bool? {
    Log._customContainsEquatableElement[selfType] += 1
    return base._customContainsEquatableElement(element)
  }

  /// If `self` is multi-pass (i.e., a `Collection`), invoke
  /// `preprocess` on `self` and return its result.  Otherwise, return
  /// `nil`.
  public func _preprocessingPass<R>(
    _ preprocess: () throws -> R
  ) rethrows -> R? {
    Log._preprocessingPass[selfType] += 1
    return try base._preprocessingPass(preprocess)
  }

  /// Create a native array buffer containing the elements of `self`,
  /// in the same order.
  public func _copyToContiguousArray()
    -> ContiguousArray<Base.Iterator.Element> {
    Log._copyToContiguousArray[selfType] += 1
    return base._copyToContiguousArray()
  }

  /// Copy a Sequence into an array.
  public func _copyContents(
    initializing buffer: UnsafeMutableBufferPointer<Iterator.Element>
  ) -> (Iterator,UnsafeMutableBufferPointer<Iterator.Element>.Index) {
    Log._copyContents[selfType] += 1
    let (it,idx) = base._copyContents(initializing: buffer)
    return (Iterator(wrapping: it),idx)
  }

  public typealias Index = Base.Index

  public var startIndex: Index {
    Log.startIndex[selfType] += 1
    return base.startIndex
  }

  public var endIndex: Index {
    Log.endIndex[selfType] += 1
    return base.endIndex
  }

  public subscript(position: Index) -> Base.Iterator.Element {
    get {
      Log.subscriptIndex[selfType] += 1
      return base[position]
    }
  }

  public subscript(bounds: Range<Index>) -> SubSequence {
    get {
      Log.subscriptRange[selfType] += 1
      return base[bounds]
    }
  }

  public func _failEarlyRangeCheck(_ index: Index, bounds: Range<Index>) {
    Log._failEarlyRangeCheckIndex[selfType] += 1
    base._failEarlyRangeCheck(index, bounds: bounds)
  }

  public func _failEarlyRangeCheck(_ range: Range<Index>, bounds: Range<Index>) {
    Log._failEarlyRangeCheckRange[selfType] += 1
    base._failEarlyRangeCheck(range, bounds: bounds)
  }

  public func index(after i: Index) -> Index {
    Log.successor[selfType] += 1
    return base.index(after: i)
  }

  public func formIndex(after i: inout Index) {
    Log.formSuccessor[selfType] += 1
    base.formIndex(after: &i)
  }

  public typealias Indices = Base.Indices

  public var indices: Indices {
    Log.indices[selfType] += 1
    return base.indices
  }

  public func prefix(upTo end: Index) -> SubSequence {
    Log.prefixUpTo[selfType] += 1
    return base.prefix(upTo: end)
  }

  public func prefix(through position: Index) -> SubSequence {
    Log.prefixThrough[selfType] += 1
    return base.prefix(through: position)
  }

  public func suffix(from start: Index) -> SubSequence {
    Log.suffixFrom[selfType] += 1
    return base.suffix(from: start)
  }

  public var isEmpty: Bool {
    Log.isEmpty[selfType] += 1
    return base.isEmpty
  }

  public var count: Int {
    Log.count[selfType] += 1
    return base.count
  }

  public func _customIndexOfEquatableElement(
    _ element: Base.Iterator.Element
  ) -> Index?? {
    Log._customIndexOfEquatableElement[selfType] += 1
    return base._customIndexOfEquatableElement(element)
  }

  public var first: Base.Iterator.Element? {
    Log.first[selfType] += 1
    return base.first
  }

  public func index(_ i: Index, offsetBy n: Int) -> Index {
    Log.advance[selfType] += 1
    return base.index(i, offsetBy: n)
  }

  public func index(
    _ i: Index, offsetBy n: Int, limitedBy limit: Index
  ) -> Index? {
    Log.advanceLimit[selfType] += 1
    return base.index(i, offsetBy: n, limitedBy: limit)
  }

  public func distance(from start: Index, to end: Index) -> Int {
    Log.distance[selfType] += 1
    return base.distance(from: start, to: end)
  }


  public init() {
    self.base = Base()
    Log.init_[selfType] += 1
  }

  public init(repeating repeatedValue: Iterator.Element, count: Int) {
    self.base = Base(repeating: repeatedValue, count: count)
    Log.initRepeating[selfType] += 1
  }

  public init<S : Sequence>(_ elements: S)
    where S.Iterator.Element == Iterator.Element {
    self.base = Base(elements)
    Log.initWithSequence[selfType] += 1
  }

  public mutating func _customRemoveLast() -> Base.Iterator.Element? {
    Log._customRemoveLast[selfType] += 1
    return base._customRemoveLast()
  }

  public mutating func _customRemoveLast(_ n: Int) -> Bool {
    Log._customRemoveLastN[selfType] += 1
    return base._customRemoveLast(n)
  }

  public mutating func append(_ newElement: Base.Iterator.Element) {
    Log.append[selfType] += 1
    base.append(newElement)
  }

  public mutating func append<S : Sequence>(
    contentsOf newElements: S
  ) where S.Iterator.Element == Base.Iterator.Element {
    Log.appendContentsOf[selfType] += 1
    base.append(contentsOf: newElements)
  }

  public mutating func insert(
    _ newElement: Base.Iterator.Element, at i: Index
  ) {
    Log.insert[selfType] += 1
    base.insert(newElement, at: i)
  }

  public mutating func insert<C : Collection>(
    contentsOf newElements: C, at i: Index
  ) where C.Iterator.Element == Base.Iterator.Element {
    Log.insertContentsOf[selfType] += 1
    base.insert(contentsOf: newElements, at: i)
  }

  public mutating func removeAll(keepingCapacity keepCapacity: Bool) {
    Log.removeAll[selfType] += 1
    base.removeAll(keepingCapacity: keepCapacity)
  }

  @discardableResult
  public mutating func remove(at index: Index) -> Base.Iterator.Element {
    Log.removeAt[selfType] += 1
    return base.remove(at: index)
  }

  @discardableResult
  public mutating func removeFirst() -> Base.Iterator.Element {
    Log.removeFirst[selfType] += 1
    return base.removeFirst()
  }

  public mutating func removeFirst(_ n: Int) {
    Log.removeFirstN[selfType] += 1
    base.removeFirst(n)
  }

  public mutating func removeSubrange(_ bounds: Range<Index>) {
    Log.removeSubrange[selfType] += 1
    base.removeSubrange(bounds)
  }

  public mutating func replaceSubrange<C : Collection>(
    _ bounds: Range<Index>, with newElements: C
  ) where C.Iterator.Element == Base.Iterator.Element {
    Log.replaceSubrange[selfType] += 1
    base.replaceSubrange(bounds, with: newElements)
  }

  public mutating func reserveCapacity(_ n: Int) {
    Log.reserveCapacity[selfType] += 1
    base.reserveCapacity(n)
  }
  public func index(before i: Index) -> Index {
    BidirectionalCollectionLog.predecessor[selfType] += 1
    return base.index(before: i)
  }

  public func formIndex(before i: inout Index) {
    BidirectionalCollectionLog.formPredecessor[selfType] += 1
    base.formIndex(before: &i)
  }

  public var last: Iterator.Element? {
    BidirectionalCollectionLog.last[selfType] += 1
    return base.last
  }
}
/// Interposes between `RangeReplaceableCollection` method calls to increment each method's
/// counter.
public struct LoggingRangeReplaceableRandomAccessCollection<
  Base : RangeReplaceableCollection & RandomAccessCollection
> : RandomAccessCollection, RangeReplaceableCollection, LoggingType {

  public var base: Base

  public typealias Log = RangeReplaceableCollectionLog

  public init(wrapping base: Base) {
    self.base = base
  }

  public typealias Iterator = LoggingIterator<Base.Iterator>

  public func makeIterator() -> Iterator {
    Log.makeIterator[selfType] += 1
    return LoggingIterator(wrapping: base.makeIterator())
  }

  public var underestimatedCount: Int {
    Log.underestimatedCount[selfType] += 1
    return base.underestimatedCount
  }

  public func map<T>(
    _ transform: (Base.Iterator.Element) throws -> T
  ) rethrows -> [T] {
    Log.map[selfType] += 1
    return try base.map(transform)
  }

  public func filter(
    _ isIncluded: (Base.Iterator.Element) throws -> Bool
  ) rethrows -> [Base.Iterator.Element] {
    Log.filter[selfType] += 1
    return try base.filter(isIncluded)
  }

  public func forEach(
    _ body: (Base.Iterator.Element) throws -> Void
  ) rethrows {
    Log.forEach[selfType] += 1
    try base.forEach(body)
  }

  public typealias SubSequence = Base.SubSequence

  public func dropFirst(_ n: Int) -> SubSequence {
    Log.dropFirst[selfType] += 1
    return base.dropFirst(n)
  }

  public func dropLast(_ n: Int) -> SubSequence {
    Log.dropLast[selfType] += 1
    return base.dropLast(n)
  }

  public func drop(
    while predicate: (Base.Iterator.Element) throws -> Bool
  ) rethrows -> SubSequence {
    Log.dropWhile[selfType] += 1
    return try base.drop(while: predicate)
  }

  public func prefix(_ maxLength: Int) -> SubSequence {
    Log.prefixMaxLength[selfType] += 1
    return base.prefix(maxLength)
  }

  public func prefix(
    while predicate: (Base.Iterator.Element) throws -> Bool
  ) rethrows -> SubSequence {
    Log.prefixWhile[selfType] += 1
    return try base.prefix(while: predicate)
  }

  public func suffix(_ maxLength: Int) -> SubSequence {
    Log.suffixMaxLength[selfType] += 1
    return base.suffix(maxLength)
  }

  public func split(
    maxSplits: Int = Int.max,
    omittingEmptySubsequences: Bool = true,
    whereSeparator isSeparator: (Base.Iterator.Element) throws -> Bool
  ) rethrows -> [SubSequence] {
    Log.split[selfType] += 1
    return try base.split(
      maxSplits: maxSplits,
      omittingEmptySubsequences: omittingEmptySubsequences,
      whereSeparator: isSeparator)
  }

  public func _customContainsEquatableElement(
    _ element: Base.Iterator.Element
  ) -> Bool? {
    Log._customContainsEquatableElement[selfType] += 1
    return base._customContainsEquatableElement(element)
  }

  /// If `self` is multi-pass (i.e., a `Collection`), invoke
  /// `preprocess` on `self` and return its result.  Otherwise, return
  /// `nil`.
  public func _preprocessingPass<R>(
    _ preprocess: () throws -> R
  ) rethrows -> R? {
    Log._preprocessingPass[selfType] += 1
    return try base._preprocessingPass(preprocess)
  }

  /// Create a native array buffer containing the elements of `self`,
  /// in the same order.
  public func _copyToContiguousArray()
    -> ContiguousArray<Base.Iterator.Element> {
    Log._copyToContiguousArray[selfType] += 1
    return base._copyToContiguousArray()
  }

  /// Copy a Sequence into an array.
  public func _copyContents(
    initializing buffer: UnsafeMutableBufferPointer<Iterator.Element>
  ) -> (Iterator,UnsafeMutableBufferPointer<Iterator.Element>.Index) {
    Log._copyContents[selfType] += 1
    let (it,idx) = base._copyContents(initializing: buffer)
    return (Iterator(wrapping: it),idx)
  }

  public typealias Index = Base.Index

  public var startIndex: Index {
    Log.startIndex[selfType] += 1
    return base.startIndex
  }

  public var endIndex: Index {
    Log.endIndex[selfType] += 1
    return base.endIndex
  }

  public subscript(position: Index) -> Base.Iterator.Element {
    get {
      Log.subscriptIndex[selfType] += 1
      return base[position]
    }
  }

  public subscript(bounds: Range<Index>) -> SubSequence {
    get {
      Log.subscriptRange[selfType] += 1
      return base[bounds]
    }
  }

  public func _failEarlyRangeCheck(_ index: Index, bounds: Range<Index>) {
    Log._failEarlyRangeCheckIndex[selfType] += 1
    base._failEarlyRangeCheck(index, bounds: bounds)
  }

  public func _failEarlyRangeCheck(_ range: Range<Index>, bounds: Range<Index>) {
    Log._failEarlyRangeCheckRange[selfType] += 1
    base._failEarlyRangeCheck(range, bounds: bounds)
  }

  public func index(after i: Index) -> Index {
    Log.successor[selfType] += 1
    return base.index(after: i)
  }

  public func formIndex(after i: inout Index) {
    Log.formSuccessor[selfType] += 1
    base.formIndex(after: &i)
  }

  public typealias Indices = Base.Indices

  public var indices: Indices {
    Log.indices[selfType] += 1
    return base.indices
  }

  public func prefix(upTo end: Index) -> SubSequence {
    Log.prefixUpTo[selfType] += 1
    return base.prefix(upTo: end)
  }

  public func prefix(through position: Index) -> SubSequence {
    Log.prefixThrough[selfType] += 1
    return base.prefix(through: position)
  }

  public func suffix(from start: Index) -> SubSequence {
    Log.suffixFrom[selfType] += 1
    return base.suffix(from: start)
  }

  public var isEmpty: Bool {
    Log.isEmpty[selfType] += 1
    return base.isEmpty
  }

  public var count: Int {
    Log.count[selfType] += 1
    return base.count
  }

  public func _customIndexOfEquatableElement(
    _ element: Base.Iterator.Element
  ) -> Index?? {
    Log._customIndexOfEquatableElement[selfType] += 1
    return base._customIndexOfEquatableElement(element)
  }

  public var first: Base.Iterator.Element? {
    Log.first[selfType] += 1
    return base.first
  }

  public func index(_ i: Index, offsetBy n: Int) -> Index {
    Log.advance[selfType] += 1
    return base.index(i, offsetBy: n)
  }

  public func index(
    _ i: Index, offsetBy n: Int, limitedBy limit: Index
  ) -> Index? {
    Log.advanceLimit[selfType] += 1
    return base.index(i, offsetBy: n, limitedBy: limit)
  }

  public func distance(from start: Index, to end: Index) -> Int {
    Log.distance[selfType] += 1
    return base.distance(from: start, to: end)
  }


  public init() {
    self.base = Base()
    Log.init_[selfType] += 1
  }

  public init(repeating repeatedValue: Iterator.Element, count: Int) {
    self.base = Base(repeating: repeatedValue, count: count)
    Log.initRepeating[selfType] += 1
  }

  public init<S : Sequence>(_ elements: S)
    where S.Iterator.Element == Iterator.Element {
    self.base = Base(elements)
    Log.initWithSequence[selfType] += 1
  }

  public mutating func _customRemoveLast() -> Base.Iterator.Element? {
    Log._customRemoveLast[selfType] += 1
    return base._customRemoveLast()
  }

  public mutating func _customRemoveLast(_ n: Int) -> Bool {
    Log._customRemoveLastN[selfType] += 1
    return base._customRemoveLast(n)
  }

  public mutating func append(_ newElement: Base.Iterator.Element) {
    Log.append[selfType] += 1
    base.append(newElement)
  }

  public mutating func append<S : Sequence>(
    contentsOf newElements: S
  ) where S.Iterator.Element == Base.Iterator.Element {
    Log.appendContentsOf[selfType] += 1
    base.append(contentsOf: newElements)
  }

  public mutating func insert(
    _ newElement: Base.Iterator.Element, at i: Index
  ) {
    Log.insert[selfType] += 1
    base.insert(newElement, at: i)
  }

  public mutating func insert<C : Collection>(
    contentsOf newElements: C, at i: Index
  ) where C.Iterator.Element == Base.Iterator.Element {
    Log.insertContentsOf[selfType] += 1
    base.insert(contentsOf: newElements, at: i)
  }

  public mutating func removeAll(keepingCapacity keepCapacity: Bool) {
    Log.removeAll[selfType] += 1
    base.removeAll(keepingCapacity: keepCapacity)
  }

  @discardableResult
  public mutating func remove(at index: Index) -> Base.Iterator.Element {
    Log.removeAt[selfType] += 1
    return base.remove(at: index)
  }

  @discardableResult
  public mutating func removeFirst() -> Base.Iterator.Element {
    Log.removeFirst[selfType] += 1
    return base.removeFirst()
  }

  public mutating func removeFirst(_ n: Int) {
    Log.removeFirstN[selfType] += 1
    base.removeFirst(n)
  }

  public mutating func removeSubrange(_ bounds: Range<Index>) {
    Log.removeSubrange[selfType] += 1
    base.removeSubrange(bounds)
  }

  public mutating func replaceSubrange<C : Collection>(
    _ bounds: Range<Index>, with newElements: C
  ) where C.Iterator.Element == Base.Iterator.Element {
    Log.replaceSubrange[selfType] += 1
    base.replaceSubrange(bounds, with: newElements)
  }

  public mutating func reserveCapacity(_ n: Int) {
    Log.reserveCapacity[selfType] += 1
    base.reserveCapacity(n)
  }
  public func index(before i: Index) -> Index {
    BidirectionalCollectionLog.predecessor[selfType] += 1
    return base.index(before: i)
  }

  public func formIndex(before i: inout Index) {
    BidirectionalCollectionLog.formPredecessor[selfType] += 1
    base.formIndex(before: &i)
  }

  public var last: Iterator.Element? {
    BidirectionalCollectionLog.last[selfType] += 1
    return base.last
  }
}

//===----------------------------------------------------------------------===//
// Collections that count calls to `_withUnsafeMutableBufferPointerIfSupported`
//===----------------------------------------------------------------------===//

/// Interposes between `_withUnsafeMutableBufferPointerIfSupported` method calls
/// to increment a counter. Calls to this method from within dispatched methods
/// are uncounted by the standard logging collection wrapper.
public struct BufferAccessLoggingMutableCollection<
  Base : MutableCollection & Collection
> : MutableCollection, Collection, LoggingType {

  public var base: Base

  public typealias Log = MutableCollectionLog

  public typealias SubSequence = Base.SubSequence
  public typealias Iterator = Base.Iterator
  public typealias Element = Base.Element

  public init(wrapping base: Base) {
    self.base = base
  }

  public func makeIterator() -> Iterator {
    return base.makeIterator()
  }

  public typealias Index = Base.Index
  public typealias Indices = Base.Indices

  public var startIndex: Index {
    return base.startIndex
  }

  public var endIndex: Index {
    return base.endIndex
  }

  public var indices: Indices {
    return base.indices
  }

  public subscript(position: Index) -> Element {
    get {
      return base[position]
    }
    set {
      base[position] = newValue
    }
  }

  public subscript(bounds: Range<Index>) -> SubSequence {
    get {
      return base[bounds]
    }
    set {
      base[bounds] = newValue
    }
  }

  public func index(after i: Index) -> Index {
    return base.index(after: i)
  }


  public func index(_ i: Index, offsetBy n: Int) -> Index {
    return base.index(i, offsetBy: n)
  }

  public func distance(from start: Index, to end: Index) -> Int {
    return base.distance(from: start, to: end)
  }

  public mutating func _withUnsafeMutableBufferPointerIfSupported<R>(
    _ body: (inout UnsafeMutableBufferPointer<Iterator.Element>) throws -> R
  ) rethrows -> R? {
    Log._withUnsafeMutableBufferPointerIfSupported[selfType] += 1
    let result = try base._withUnsafeMutableBufferPointerIfSupported(body)
    if result != nil {
      Log._withUnsafeMutableBufferPointerIfSupportedNonNilReturns[selfType] += 1
    }
    return result
  }
}
/// Interposes between `_withUnsafeMutableBufferPointerIfSupported` method calls
/// to increment a counter. Calls to this method from within dispatched methods
/// are uncounted by the standard logging collection wrapper.
public struct BufferAccessLoggingMutableBidirectionalCollection<
  Base : MutableCollection & BidirectionalCollection
> : MutableCollection, BidirectionalCollection, LoggingType {

  public var base: Base

  public typealias Log = MutableCollectionLog

  public typealias SubSequence = Base.SubSequence
  public typealias Iterator = Base.Iterator
  public typealias Element = Base.Element

  public init(wrapping base: Base) {
    self.base = base
  }

  public func makeIterator() -> Iterator {
    return base.makeIterator()
  }

  public typealias Index = Base.Index
  public typealias Indices = Base.Indices

  public var startIndex: Index {
    return base.startIndex
  }

  public var endIndex: Index {
    return base.endIndex
  }

  public var indices: Indices {
    return base.indices
  }

  public subscript(position: Index) -> Element {
    get {
      return base[position]
    }
    set {
      base[position] = newValue
    }
  }

  public subscript(bounds: Range<Index>) -> SubSequence {
    get {
      return base[bounds]
    }
    set {
      base[bounds] = newValue
    }
  }

  public func index(after i: Index) -> Index {
    return base.index(after: i)
  }

  public func index(before i: Index) -> Index {
    return base.index(before: i)
  }

  public func index(_ i: Index, offsetBy n: Int) -> Index {
    return base.index(i, offsetBy: n)
  }

  public func distance(from start: Index, to end: Index) -> Int {
    return base.distance(from: start, to: end)
  }

  public mutating func _withUnsafeMutableBufferPointerIfSupported<R>(
    _ body: (inout UnsafeMutableBufferPointer<Iterator.Element>) throws -> R
  ) rethrows -> R? {
    Log._withUnsafeMutableBufferPointerIfSupported[selfType] += 1
    let result = try base._withUnsafeMutableBufferPointerIfSupported(body)
    if result != nil {
      Log._withUnsafeMutableBufferPointerIfSupportedNonNilReturns[selfType] += 1
    }
    return result
  }
}
/// Interposes between `_withUnsafeMutableBufferPointerIfSupported` method calls
/// to increment a counter. Calls to this method from within dispatched methods
/// are uncounted by the standard logging collection wrapper.
public struct BufferAccessLoggingMutableRandomAccessCollection<
  Base : MutableCollection & RandomAccessCollection
> : MutableCollection, RandomAccessCollection, LoggingType {

  public var base: Base

  public typealias Log = MutableCollectionLog

  public typealias SubSequence = Base.SubSequence
  public typealias Iterator = Base.Iterator
  public typealias Element = Base.Element

  public init(wrapping base: Base) {
    self.base = base
  }

  public func makeIterator() -> Iterator {
    return base.makeIterator()
  }

  public typealias Index = Base.Index
  public typealias Indices = Base.Indices

  public var startIndex: Index {
    return base.startIndex
  }

  public var endIndex: Index {
    return base.endIndex
  }

  public var indices: Indices {
    return base.indices
  }

  public subscript(position: Index) -> Element {
    get {
      return base[position]
    }
    set {
      base[position] = newValue
    }
  }

  public subscript(bounds: Range<Index>) -> SubSequence {
    get {
      return base[bounds]
    }
    set {
      base[bounds] = newValue
    }
  }

  public func index(after i: Index) -> Index {
    return base.index(after: i)
  }

  public func index(before i: Index) -> Index {
    return base.index(before: i)
  }

  public func index(_ i: Index, offsetBy n: Int) -> Index {
    return base.index(i, offsetBy: n)
  }

  public func distance(from start: Index, to end: Index) -> Int {
    return base.distance(from: start, to: end)
  }

  public mutating func _withUnsafeMutableBufferPointerIfSupported<R>(
    _ body: (inout UnsafeMutableBufferPointer<Iterator.Element>) throws -> R
  ) rethrows -> R? {
    Log._withUnsafeMutableBufferPointerIfSupported[selfType] += 1
    let result = try base._withUnsafeMutableBufferPointerIfSupported(body)
    if result != nil {
      Log._withUnsafeMutableBufferPointerIfSupportedNonNilReturns[selfType] += 1
    }
    return result
  }
}

//===----------------------------------------------------------------------===//
// Custom assertions
//===----------------------------------------------------------------------===//

public func expectCustomizable<T : LoggingType>(
  _: T, _ counters: TypeIndexed<Int>,
  _ message: @autoclosure () -> String = "",
  stackTrace: SourceLocStack = SourceLocStack(),
  showFrame: Bool = true,
  file: String = #file, line: UInt = #line
) where
  T.Base : LoggingType,
  T.Log == T.Base.Log {
  let newTrace = stackTrace.pushIf(showFrame, file: file, line: line)
  expectNotEqual(0, counters[T.self], message(), stackTrace: newTrace)
  expectEqual(
    counters[T.self], counters[T.Base.self], message(), stackTrace: newTrace)
}

public func expectNotCustomizable<T : LoggingType>(
  _: T, _ counters: TypeIndexed<Int>,
  _ message: @autoclosure () -> String = "",
  stackTrace: SourceLocStack = SourceLocStack(),
  showFrame: Bool = true,
  file: String = #file, line: UInt = #line
) where
  T.Base : LoggingType,
  T.Log == T.Base.Log {
  let newTrace = stackTrace.pushIf(showFrame, file: file, line: line)
  expectNotEqual(0, counters[T.self], message(), stackTrace: newTrace)
  expectEqual(0, counters[T.Base.self], message(), stackTrace: newTrace)
}
