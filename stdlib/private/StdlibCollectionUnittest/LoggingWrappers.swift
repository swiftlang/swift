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

public protocol LoggingType: Wrapper {
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

public struct LoggingIterator<Base : IteratorProtocol> {
  public var base: Base
}

extension LoggingIterator: LoggingType {
  public typealias Log = IteratorLog

  public init(wrapping base: Base) {
    self.base = base
  }
}

extension LoggingIterator: IteratorProtocol {
  public mutating func next() -> Base.Element? {
    Log.next[selfType] += 1
    return base.next()
  }
}

//===----------------------------------------------------------------------===//
// Sequence and Collection logs
//===----------------------------------------------------------------------===//

// FIXME: it's not clear if it's really worth this hierarchy. the 
// test.log pattern requires all the static properties be at the top
// since Log is an associated type that cannot be refined in extensions
// that add functionality.

public class SequenceLog {
  // Sequence
  public static var makeIterator = TypeIndexed(0)
  public static var underestimatedCount = TypeIndexed(0)
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
  // Collection
  public static var startIndex = TypeIndexed(0)
  public static var endIndex = TypeIndexed(0)
  public static var subscriptIndex = TypeIndexed(0)
  public static var subscriptRange = TypeIndexed(0)
  public static var _failEarlyRangeCheckIndex = TypeIndexed(0)
  public static var _failEarlyRangeCheckRange = TypeIndexed(0)
  public static var successor = TypeIndexed(0)
  public static var formSuccessor = TypeIndexed(0)
  public static var indices = TypeIndexed(0)
  public static var isEmpty = TypeIndexed(0)
  public static var count = TypeIndexed(0)
  public static var _customIndexOfEquatableElement = TypeIndexed(0)
  public static var advance = TypeIndexed(0)
  public static var advanceLimit = TypeIndexed(0)
  public static var distance = TypeIndexed(0)
  // BidirectionalCollection
  public static var predecessor = TypeIndexed(0)
  public static var formPredecessor = TypeIndexed(0)
  // MutableCollection
  public static var subscriptIndexSet = TypeIndexed(0)
  public static var subscriptRangeSet = TypeIndexed(0)
  public static var partitionBy = TypeIndexed(0)
  public static var _withUnsafeMutableBufferPointerIfSupported = TypeIndexed(0)
  public static var _withUnsafeMutableBufferPointerIfSupportedNonNilReturns =
    TypeIndexed(0)
  // RangeReplaceableCollection
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

  public class func dispatchTester<S>(
    _ s: S
  ) -> LoggingSequence<LoggingSequence<S>> {
    return LoggingSequence(wrapping: LoggingSequence(wrapping: s))
  }
}

public class CollectionLog : SequenceLog {
  public override class func dispatchTester<C>(
    _ c: C
  ) -> LoggingCollection<LoggingCollection<C>> {
    return LoggingCollection(wrapping: LoggingCollection(wrapping: c))
  }
}

public class BidirectionalCollectionLog : CollectionLog {
  public override class func dispatchTester<C>(
    _ c: C
  ) -> LoggingBidirectionalCollection<LoggingBidirectionalCollection<C>> {
    return LoggingBidirectionalCollection(
      wrapping: LoggingBidirectionalCollection(wrapping: c))
  }
}

public class MutableCollectionLog : CollectionLog {
  public override class func dispatchTester<C>(
    _ c: C
  ) -> LoggingMutableCollection<LoggingMutableCollection<C>> {
    return LoggingMutableCollection(
      wrapping: LoggingMutableCollection(wrapping: c))
  }
}

/// Data container to keep track of how many times each `Base` type calls methods
/// of `RangeReplaceableCollection`.
///
/// Each static variable is a mapping of Type -> Number of calls.
public class RangeReplaceableCollectionLog : CollectionLog {
  public override class func dispatchTester<C>(
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
public struct LoggingSequence<Base : Sequence> {
  public var base: Base
}

extension LoggingSequence: LoggingType {
  // I know, I know. It doesn't matter though. The benefit of the whole logging
  // class hiearchy is unclear...
  public typealias Log = SequenceLog

  public init(wrapping base: Base) {
    self.base = base
  }
}

extension LoggingSequence: Sequence {
  public typealias Element = Base.Element
  public typealias Iterator = LoggingIterator<Base.Iterator>
  public typealias SubSequence = Base.SubSequence

  public func makeIterator() -> Iterator {
    SequenceLog.makeIterator[selfType] += 1
    return LoggingIterator(wrapping: base.makeIterator())
  }

  public var underestimatedCount: Int {
    SequenceLog.underestimatedCount[selfType] += 1
    return base.underestimatedCount
  }

  // public func dropFirst(_ n: Int) -> SubSequence {
  //   SequenceLog.dropFirst[selfType] += 1
  //   return base.dropFirst(n)
  // }
  //
  // public func dropLast(_ n: Int) -> SubSequence {
  //   SequenceLog.dropLast[selfType] += 1
  //   return base.dropLast(n)
  // }
  //
  // public func drop(
  //   while predicate: (Element) throws -> Bool
  // ) rethrows -> SubSequence {
  //   SequenceLog.dropWhile[selfType] += 1
  //   return try base.drop(while: predicate)
  // }
  //
  // public func prefix(_ maxLength: Int) -> SubSequence {
  //   SequenceLog.prefixMaxLength[selfType] += 1
  //   return base.prefix(maxLength)
  // }
  //
  // public func prefix(
  //   while predicate: (Element) throws -> Bool
  // ) rethrows -> SubSequence {
  //   SequenceLog.prefixWhile[selfType] += 1
  //   return try base.prefix(while: predicate)
  // }
  //
  // public func suffix(_ maxLength: Int) -> SubSequence {
  //   SequenceLog.suffixMaxLength[selfType] += 1
  //   return base.suffix(maxLength)
  // }

  public func split(
    maxSplits: Int = Int.max,
    omittingEmptySubsequences: Bool = true,
    whereSeparator isSeparator: (Element) throws -> Bool
  ) rethrows -> [SubSequence] {
    SequenceLog.split[selfType] += 1
    return try base.split(
      maxSplits: maxSplits,
      omittingEmptySubsequences: omittingEmptySubsequences,
      whereSeparator: isSeparator)
  }

  public func _customContainsEquatableElement(_ element: Element) -> Bool? {
    SequenceLog._customContainsEquatableElement[selfType] += 1
    return base._customContainsEquatableElement(element)
  }

  /// If `self` is multi-pass (i.e., a `Collection`), invoke
  /// `preprocess` on `self` and return its result.  Otherwise, return
  /// `nil`.
  public func _preprocessingPass<R>(
    _ preprocess: () throws -> R
  ) rethrows -> R? {
    SequenceLog._preprocessingPass[selfType] += 1
    return try base._preprocessingPass(preprocess)
  }

  /// Create a native array buffer containing the elements of `self`,
  /// in the same order.
  public func _copyToContiguousArray() -> ContiguousArray<Element> {
    SequenceLog._copyToContiguousArray[selfType] += 1
    return base._copyToContiguousArray()
  }

  /// Copy a Sequence into an array.
  public func _copyContents(
    initializing buffer: UnsafeMutableBufferPointer<Element>
  ) -> (Iterator,UnsafeMutableBufferPointer<Element>.Index) {
    SequenceLog._copyContents[selfType] += 1
    let (it,idx) = base._copyContents(initializing: buffer)
    return (Iterator(wrapping: it),idx)
  }
}

public typealias LoggingCollection<Base: Collection> = LoggingSequence<Base>

extension LoggingCollection: Collection {  
  public typealias Index = Base.Index
  public typealias Indices = Base.Indices

  public var startIndex: Index {
    CollectionLog.startIndex[selfType] += 1
    return base.startIndex
  }

  public var endIndex: Index {
    CollectionLog.endIndex[selfType] += 1
    return base.endIndex
  }

  public subscript(position: Index) -> Element {
    CollectionLog.subscriptIndex[selfType] += 1
    return base[position]
  }

  public subscript(bounds: Range<Index>) -> SubSequence {
    CollectionLog.subscriptRange[selfType] += 1
    return base[bounds]
  }

  public func _failEarlyRangeCheck(_ index: Index, bounds: Range<Index>) {
    CollectionLog._failEarlyRangeCheckIndex[selfType] += 1
    base._failEarlyRangeCheck(index, bounds: bounds)
  }

  public func _failEarlyRangeCheck(_ range: Range<Index>, bounds: Range<Index>) {
    CollectionLog._failEarlyRangeCheckRange[selfType] += 1
    base._failEarlyRangeCheck(range, bounds: bounds)
  }

  public func index(after i: Index) -> Index {
    CollectionLog.successor[selfType] += 1
    return base.index(after: i)
  }

  public func formIndex(after i: inout Index) {
    CollectionLog.formSuccessor[selfType] += 1
    base.formIndex(after: &i)
  }

  public var indices: Indices {
    CollectionLog.indices[selfType] += 1
    return base.indices
  }

  public var isEmpty: Bool {
    CollectionLog.isEmpty[selfType] += 1
    return base.isEmpty
  }

  public var count: Int {
    CollectionLog.count[selfType] += 1
    return base.count
  }

  public func _customIndexOfEquatableElement(
    _ element: Element
  ) -> Index?? {
    CollectionLog._customIndexOfEquatableElement[selfType] += 1
    return base._customIndexOfEquatableElement(element)
  }

  public func index(_ i: Index, offsetBy n: Int) -> Index {
    CollectionLog.advance[selfType] += 1
    return base.index(i, offsetBy: n)
  }

  public func index(
    _ i: Index, offsetBy n: Int, limitedBy limit: Index
  ) -> Index? {
    CollectionLog.advanceLimit[selfType] += 1
    return base.index(i, offsetBy: n, limitedBy: limit)
  }

  public func distance(from start: Index, to end: Index) -> Int {
    CollectionLog.distance[selfType] += 1
    return base.distance(from: start, to: end)
  }
}

public typealias LoggingBidirectionalCollection<
  Base: BidirectionalCollection
> = LoggingCollection<Base>

extension LoggingBidirectionalCollection: BidirectionalCollection {
  public func index(before i: Index) -> Index {
    BidirectionalCollectionLog.predecessor[selfType] += 1
    return base.index(before: i)
  }

  public func formIndex(before i: inout Index) {
    BidirectionalCollectionLog.formPredecessor[selfType] += 1
    base.formIndex(before: &i)
  }
}

public typealias LoggingRandomAccessCollection<Base: RandomAccessCollection> 
  = LoggingBidirectionalCollection<Base>
  
extension LoggingRandomAccessCollection: RandomAccessCollection { }

public typealias LoggingMutableCollection<Base: MutableCollection>
  = LoggingCollection<Base>

extension LoggingMutableCollection: MutableCollection {
  public subscript(position: Index) -> Element {
    get {
      MutableCollectionLog.subscriptIndex[selfType] += 1
      return base[position]
    }
    set {
      MutableCollectionLog.subscriptIndexSet[selfType] += 1
      base[position] = newValue
    }
  }

  public subscript(bounds: Range<Index>) -> SubSequence {
    get {
      MutableCollectionLog.subscriptRange[selfType] += 1
      return base[bounds]
    }
    set {
      MutableCollectionLog.subscriptRangeSet[selfType] += 1
      base[bounds] = newValue
    }
  }

  public mutating func partition(
     by belongsInSecondPartition: (Iterator.Element) throws -> Bool
   ) rethrows -> Index {
     Log.partitionBy[selfType] += 1
     return try base.partition(by: belongsInSecondPartition)
   }
   
  public mutating func _withUnsafeMutableBufferPointerIfSupported<R>(
    _ body: (inout UnsafeMutableBufferPointer<Element>) throws -> R
  ) rethrows -> R? {
    MutableCollectionLog._withUnsafeMutableBufferPointerIfSupported[selfType] += 1
    let result = try base._withUnsafeMutableBufferPointerIfSupported(body)
    if result != nil {
      Log._withUnsafeMutableBufferPointerIfSupportedNonNilReturns[selfType] += 1
    }
    return result
  }

}

public typealias LoggingMutableBidirectionalCollection<
  Base: MutableCollection & BidirectionalCollection
> = LoggingMutableCollection<Base>

public typealias LoggingMutableRandomAccessCollection<
  Base: MutableCollection & RandomAccessCollection
> = LoggingMutableCollection<Base>

public typealias LoggingRangeReplaceableCollection<
  Base: RangeReplaceableCollection
> = LoggingCollection<Base>

extension LoggingRangeReplaceableCollection: RangeReplaceableCollection {
  public init() {
    self.base = Base()
    RangeReplaceableCollectionLog.init_[selfType] += 1
  }

  public init(repeating repeatedValue: Element, count: Int) {
    self.base = Base(repeating: repeatedValue, count: count)
    RangeReplaceableCollectionLog.initRepeating[selfType] += 1
  }

  public init<S : Sequence>(_ elements: S)
    where S.Element == Element {
    self.base = Base(elements)
    RangeReplaceableCollectionLog.initWithSequence[selfType] += 1
  }

  public mutating func _customRemoveLast() -> Element? {
    RangeReplaceableCollectionLog._customRemoveLast[selfType] += 1
    return base._customRemoveLast()
  }

  public mutating func _customRemoveLast(_ n: Int) -> Bool {
    RangeReplaceableCollectionLog._customRemoveLastN[selfType] += 1
    return base._customRemoveLast(n)
  }

  public mutating func append(_ newElement: Element) {
    RangeReplaceableCollectionLog.append[selfType] += 1
    base.append(newElement)
  }

  public mutating func append<S : Sequence>(
    contentsOf newElements: S
  ) where S.Element == Element {
    RangeReplaceableCollectionLog.appendContentsOf[selfType] += 1
    base.append(contentsOf: newElements)
  }

  public mutating func insert(
    _ newElement: Element, at i: Index
  ) {
    RangeReplaceableCollectionLog.insert[selfType] += 1
    base.insert(newElement, at: i)
  }

  public mutating func insert<C : Collection>(
    contentsOf newElements: C, at i: Index
  ) where C.Element == Element {
    RangeReplaceableCollectionLog.insertContentsOf[selfType] += 1
    base.insert(contentsOf: newElements, at: i)
  }

  public mutating func removeAll(keepingCapacity keepCapacity: Bool) {
    RangeReplaceableCollectionLog.removeAll[selfType] += 1
    base.removeAll(keepingCapacity: keepCapacity)
  }

  @discardableResult
  public mutating func remove(at index: Index) -> Element {
    RangeReplaceableCollectionLog.removeAt[selfType] += 1
    return base.remove(at: index)
  }

  @discardableResult
  public mutating func removeFirst() -> Element {
    RangeReplaceableCollectionLog.removeFirst[selfType] += 1
    return base.removeFirst()
  }

  public mutating func removeFirst(_ n: Int) {
    RangeReplaceableCollectionLog.removeFirstN[selfType] += 1
    base.removeFirst(n)
  }

  public mutating func removeSubrange(_ bounds: Range<Index>) {
    RangeReplaceableCollectionLog.removeSubrange[selfType] += 1
    base.removeSubrange(bounds)
  }

  public mutating func replaceSubrange<C : Collection>(
    _ bounds: Range<Index>, with newElements: C
  ) where C.Element == Element {
    RangeReplaceableCollectionLog.replaceSubrange[selfType] += 1
    base.replaceSubrange(bounds, with: newElements)
  }

  public mutating func reserveCapacity(_ n: Int) {
    RangeReplaceableCollectionLog.reserveCapacity[selfType] += 1
    base.reserveCapacity(n)
  }
}

public typealias LoggingRangeReplaceableBidirectionalCollection<
  Base: BidirectionalCollection & RangeReplaceableCollection
> = LoggingRangeReplaceableCollection<Base>

public typealias LoggingRangeReplaceableRandomAccessCollection<
  Base: RandomAccessCollection & RangeReplaceableCollection
> = LoggingRangeReplaceableCollection<Base>

//===----------------------------------------------------------------------===//
// Collections that count calls to `_withUnsafeMutableBufferPointerIfSupported`
//===----------------------------------------------------------------------===//

/// Interposes between `_withUnsafeMutableBufferPointerIfSupported` method calls
/// to increment a counter. Calls to this method from within dispatched methods
/// are uncounted by the standard logging collection wrapper.
public struct BufferAccessLoggingMutableCollection<
  Base : MutableCollection
> {
  public var base: Base  
}

extension BufferAccessLoggingMutableCollection: LoggingType {
  public typealias Log = MutableCollectionLog
  
  public init(wrapping base: Base) {
    self.base = base
  }
}

extension BufferAccessLoggingMutableCollection: Sequence {
  public typealias SubSequence = Base.SubSequence
  public typealias Iterator = Base.Iterator
  public typealias Element = Base.Element

  public func makeIterator() -> Iterator {
    return base.makeIterator()
  }
}

extension BufferAccessLoggingMutableCollection: MutableCollection {
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
    _ body: (inout UnsafeMutableBufferPointer<Element>) throws -> R
  ) rethrows -> R? {
    Log._withUnsafeMutableBufferPointerIfSupported[selfType] += 1
    let result = try base._withUnsafeMutableBufferPointerIfSupported(body)
    if result != nil {
      Log._withUnsafeMutableBufferPointerIfSupportedNonNilReturns[selfType] += 1
    }
    return result
  }
}

public typealias BufferAccessLoggingMutableBidirectionalCollection<
  Base: MutableCollection & BidirectionalCollection
> = BufferAccessLoggingMutableCollection<Base>

extension BufferAccessLoggingMutableBidirectionalCollection: BidirectionalCollection {
  public func index(before i: Index) -> Index {
    return base.index(before: i)
  }
}

public typealias BufferAccessLoggingMutableRandomAccessCollection<
  Base: MutableCollection & RandomAccessCollection
> = BufferAccessLoggingMutableCollection<Base>

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
