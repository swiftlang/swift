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

/// State shared by all generators of a MinimalSequence.
internal class _MinimalIteratorSharedState<T> {
  internal init(_ data: [T]) {
    self.data = data
  }

  internal let data: [T]
  internal var i: Int = 0
  internal var underestimatedCount: Int = 0
}

//===----------------------------------------------------------------------===//
// MinimalIterator
//===----------------------------------------------------------------------===//

/// An IteratorProtocol that implements the protocol contract in the most
/// narrow way possible.
///
/// This generator will return `nil` only once.
public struct MinimalIterator<T> : IteratorProtocol {
  public init<S : Sequence>(_ s: S) where S.Element == T {
    self._sharedState = _MinimalIteratorSharedState(Array(s))
  }

  public init(_ data: [T]) {
    self._sharedState = _MinimalIteratorSharedState(data)
  }

  internal init(_ _sharedState: _MinimalIteratorSharedState<T>) {
    self._sharedState = _sharedState
  }

  public func next() -> T? {
    if _sharedState.i == _sharedState.data.count {
      return nil
    }
    defer { _sharedState.i += 1 }
    return _sharedState.data[_sharedState.i]
  }

  internal let _sharedState: _MinimalIteratorSharedState<T>
}

// A protocol to identify MinimalIterator.
public protocol _MinimalIterator {}
extension MinimalIterator : _MinimalIterator {}

//===----------------------------------------------------------------------===//
// MinimalSequence
//===----------------------------------------------------------------------===//

public enum UnderestimatedCountBehavior {
  /// Return the actual number of elements.
  case precise

  /// Return the actual number of elements divided by 2.
  case half

  /// Return an overestimated count.  Useful to test how algorithms reserve
  /// memory.
  case overestimate

  /// Return the provided value.
  case value(Int)
}

/// A Sequence that implements the protocol contract in the most
/// narrow way possible.
///
/// This sequence is consumed when its generator is advanced.
public struct MinimalSequence<T> : Sequence, CustomDebugStringConvertible {
  public init<S : Sequence>(
    elements: S,
    underestimatedCount: UnderestimatedCountBehavior = .value(0)
  ) where S.Element == T {
    let data = Array(elements)
    self._sharedState = _MinimalIteratorSharedState(data)

    switch underestimatedCount {
    case .precise:
      self._sharedState.underestimatedCount = data.count

    case .half:
      self._sharedState.underestimatedCount = data.count / 2

    case .overestimate:
      self._sharedState.underestimatedCount = data.count * 3 + 5

    case .value(let count):
      self._sharedState.underestimatedCount = count
    }
  }

  public let timesMakeIteratorCalled = ResettableValue(0)

  public func makeIterator() -> MinimalIterator<T> {
    timesMakeIteratorCalled.value += 1
    return MinimalIterator(_sharedState)
  }

  public var underestimatedCount: Int {
    return Swift.max(0, self._sharedState.underestimatedCount - self._sharedState.i)
  }

  public var debugDescription: String {
    return "MinimalSequence(\(_sharedState.data[_sharedState.i..<_sharedState.data.count]))"
  }

  internal let _sharedState: _MinimalIteratorSharedState<T>
}

//===----------------------------------------------------------------------===//
// Index invalidation checking
//===----------------------------------------------------------------------===//

internal enum _CollectionOperation : Equatable {
  case reserveCapacity(capacity: Int)
  case append
  case appendContentsOf(count: Int)
  case replaceRange(subRange: Range<Int>, replacementCount: Int)
  case insert(atIndex: Int)
  case insertContentsOf(atIndex: Int, count: Int)
  case removeAtIndex(index: Int)
  case removeLast
  case removeRange(subRange: Range<Int>)
  case removeAll(keepCapacity: Bool)

  internal func _applyTo(
    elementsLastMutatedStateIds: [Int],
    endIndexLastMutatedStateId: Int,
    nextStateId: Int
  ) -> ([Int], Int) {
    var newElementsIds = elementsLastMutatedStateIds
    var newEndIndexId = endIndexLastMutatedStateId
    switch self {
    case .reserveCapacity:
      let invalidIndices = newElementsIds.indices
      newElementsIds.replaceSubrange(
        invalidIndices,
        with: repeatElement(nextStateId, count: invalidIndices.count))
      newEndIndexId = nextStateId

    case .append:
      newElementsIds.append(nextStateId)
      newEndIndexId = nextStateId

    case .appendContentsOf(let count):
      newElementsIds.append(contentsOf:
        repeatElement(nextStateId, count: count))
      newEndIndexId = nextStateId

    case .replaceRange(let subRange, let replacementCount):
      newElementsIds.replaceSubrange(
        subRange,
        with: repeatElement(nextStateId, count: replacementCount))

      let invalidIndices = subRange.lowerBound..<newElementsIds.endIndex
      newElementsIds.replaceSubrange(
        invalidIndices,
        with: repeatElement(nextStateId, count: invalidIndices.count))
      newEndIndexId = nextStateId

    case .insert(let atIndex):
      newElementsIds.insert(nextStateId, at: atIndex)

      let invalidIndices = atIndex..<newElementsIds.endIndex
      newElementsIds.replaceSubrange(
        invalidIndices,
        with: repeatElement(nextStateId, count: invalidIndices.count))
      newEndIndexId = nextStateId

    case .insertContentsOf(let atIndex, let count):
      newElementsIds.insert(
        contentsOf: repeatElement(nextStateId, count: count),
        at: atIndex)

      let invalidIndices = atIndex..<newElementsIds.endIndex
      newElementsIds.replaceSubrange(
        invalidIndices,
        with: repeatElement(nextStateId, count: invalidIndices.count))
      newEndIndexId = nextStateId

    case .removeAtIndex(let index):
      newElementsIds.remove(at: index)

      let invalidIndices = index..<newElementsIds.endIndex
      newElementsIds.replaceSubrange(
        invalidIndices,
        with: repeatElement(nextStateId, count: invalidIndices.count))
      newEndIndexId = nextStateId

    case .removeLast:
      newElementsIds.removeLast()
      newEndIndexId = nextStateId

    case .removeRange(let subRange):
      newElementsIds.removeSubrange(subRange)

      let invalidIndices = subRange.lowerBound..<newElementsIds.endIndex
      newElementsIds.replaceSubrange(
        invalidIndices,
        with: repeatElement(nextStateId, count: invalidIndices.count))
      newEndIndexId = nextStateId

    case .removeAll(let keepCapacity):
      newElementsIds.removeAll(keepingCapacity: keepCapacity)
      newEndIndexId = nextStateId
    }
    return (newElementsIds, newEndIndexId)
  }
}

internal func == (
  lhs: _CollectionOperation,
  rhs: _CollectionOperation
) -> Bool {
  switch (lhs, rhs) {
  case (.reserveCapacity(let lhsCapacity), .reserveCapacity(let rhsCapacity)):
    return lhsCapacity == rhsCapacity

  case (.append, .append):
    return true

  case (.appendContentsOf(let lhsCount), .appendContentsOf(let rhsCount)):
    return lhsCount == rhsCount

  case (
    .replaceRange(let lhsSubRange, let lhsReplacementCount),
    .replaceRange(let rhsSubRange, let rhsReplacementCount)):

    return lhsSubRange == rhsSubRange &&
      lhsReplacementCount == rhsReplacementCount

  case (.insert(let lhsAtIndex), .insert(let rhsAtIndex)):
    return lhsAtIndex == rhsAtIndex

  case (
    .insertContentsOf(let lhsAtIndex, let lhsCount),
    .insertContentsOf(let rhsAtIndex, let rhsCount)):

    return lhsAtIndex == rhsAtIndex && lhsCount == rhsCount

  case (.removeAtIndex(let lhsIndex), .removeAtIndex(let rhsIndex)):
    return lhsIndex == rhsIndex

  case (.removeLast, .removeLast):
    return true

  case (.removeRange(let lhsSubRange), .removeRange(let rhsSubRange)):
    return lhsSubRange == rhsSubRange

  case (.removeAll(let lhsKeepCapacity), .removeAll(let rhsKeepCapacity)):
    return lhsKeepCapacity == rhsKeepCapacity

  default:
    return false
  }
}

public struct _CollectionState : Equatable, Hashable {
  internal static var _nextUnusedState: Int = 0
  internal static var _namedStates: [String : _CollectionState] = [:]

  internal let _id: Int
  internal let _elementsLastMutatedStateIds: [Int]
  internal let _endIndexLastMutatedStateId: Int

  internal init(
    id: Int,
    elementsLastMutatedStateIds: [Int],
    endIndexLastMutatedStateId: Int) {
    self._id = id
    self._elementsLastMutatedStateIds = elementsLastMutatedStateIds
    self._endIndexLastMutatedStateId = endIndexLastMutatedStateId
  }

  public init(newRootStateForElementCount count: Int) {
    self._id = _CollectionState._nextUnusedState
    _CollectionState._nextUnusedState += 1
    self._elementsLastMutatedStateIds =
      Array(repeatElement(self._id, count: count))
    self._endIndexLastMutatedStateId = self._id
  }

  internal init(name: String, elementCount: Int) {
    if let result = _CollectionState._namedStates[name] {
      self = result
    } else {
      self = _CollectionState(newRootStateForElementCount: elementCount)
      _CollectionState._namedStates[name] = self
    }
  }

  public var hashValue: Int {
    return _id.hashValue
  }
}

public func == (lhs: _CollectionState, rhs: _CollectionState) -> Bool {
  return lhs._id == rhs._id
}

internal struct _CollectionStateTransition {
  internal let _previousState: _CollectionState
  internal let _operation: _CollectionOperation
  internal let _nextState: _CollectionState

  internal static var _allTransitions:
    [_CollectionState : Box<[_CollectionStateTransition]>] = [:]

  internal init(
    previousState: _CollectionState,
    operation: _CollectionOperation,
    nextState: _CollectionState
  ) {
    var transitions =
      _CollectionStateTransition._allTransitions[previousState]
    if transitions == nil {
      transitions = Box<[_CollectionStateTransition]>([])
      _CollectionStateTransition._allTransitions[previousState] = transitions
    }
    if let i = transitions!.value.firstIndex(where: { $0._operation == operation }) {
      self = transitions!.value[i]
      return
    }
    self._previousState = previousState
    self._operation = operation
    self._nextState = nextState
    transitions!.value.append(self)
  }

  internal init(
    previousState: _CollectionState,
    operation: _CollectionOperation
  ) {
    let nextStateId = _CollectionState._nextUnusedState
    _CollectionState._nextUnusedState += 1
    let (newElementStates, newEndIndexState) = operation._applyTo(
      elementsLastMutatedStateIds: previousState._elementsLastMutatedStateIds,
      endIndexLastMutatedStateId: previousState._endIndexLastMutatedStateId,
      nextStateId: nextStateId)
    let nextState = _CollectionState(
      id: nextStateId,
      elementsLastMutatedStateIds: newElementStates,
      endIndexLastMutatedStateId: newEndIndexState)
    self = _CollectionStateTransition(
      previousState: previousState,
      operation: operation,
      nextState: nextState)
  }
}


//===----------------------------------------------------------------------===//
// MinimalIndex
//===----------------------------------------------------------------------===//

/// Asserts that the two indices are allowed to participate in a binary
/// operation.
internal func _expectCompatibleIndices(
  _ first: MinimalIndex,
  _ second: MinimalIndex,

  _ message: @autoclosure () -> String = "",
  stackTrace: SourceLocStack = SourceLocStack(),
  showFrame: Bool = true,
  file: String = #file, line: UInt = #line
) {
  if first._collectionState._id == second._collectionState._id {
    // Fast path: the indices are derived from the same state.
    return
  }

  // The indices are derived from different states.  Check that they point
  // to a self-consistent view of the collection.
  if first._collectionState._id > second._collectionState._id {
    return _expectCompatibleIndices(second, first)
  }

  func lastMutatedStateId(
    of i: MinimalIndex,
    in state: _CollectionState
  ) -> Int {
    let offset = i.position
    if offset == state._elementsLastMutatedStateIds.endIndex {
      return state._id
    }
    return state._elementsLastMutatedStateIds[offset]
  }

  let newestCollectionState = second._collectionState
  let expectedFirstIndexLastMutatedStateId =
    lastMutatedStateId(of: first, in: newestCollectionState)

  expectEqual(
    expectedFirstIndexLastMutatedStateId,
    first._collectionState._id,
    "Indices are not compatible:\n" +
    "first: \(first)\n" +
    "second: \(second)\n" +
    "first element last mutated in state id: \(first._collectionState._id)\n" +
    "expected state id: \(expectedFirstIndexLastMutatedStateId)\n" +
    "newest collection state: \(newestCollectionState)",
    stackTrace: stackTrace.pushIf(showFrame, file: file, line: line))

  // To make writing assertions easier, perform a trap.
  if expectedFirstIndexLastMutatedStateId != first._collectionState._id {
    fatalError("Indices are not compatible")
  }
}

public struct MinimalIndex : Comparable {
  public init(
    collectionState: _CollectionState,
    position: Int,
    startIndex: Int,
    endIndex: Int
  ) {
    expectTrapping(
      position,
      in: startIndex...endIndex)
    self = MinimalIndex(
      _collectionState: collectionState,
      uncheckedPosition: position)
  }

  internal init(
    _collectionState: _CollectionState,
    uncheckedPosition: Int
  ) {
    self._collectionState = _collectionState
    self.position = uncheckedPosition
  }

  public let _collectionState: _CollectionState
  public let position: Int

  public static var trapOnRangeCheckFailure = ResettableValue(true)

}

public func == (lhs: MinimalIndex, rhs: MinimalIndex) -> Bool {
  _expectCompatibleIndices(lhs, rhs)
  return lhs.position == rhs.position
}

public func < (lhs: MinimalIndex, rhs: MinimalIndex) -> Bool {
  _expectCompatibleIndices(lhs, rhs)
  return lhs.position < rhs.position
}


//===----------------------------------------------------------------------===//
// MinimalStrideableIndex
//===----------------------------------------------------------------------===//

/// Asserts that the two indices are allowed to participate in a binary
/// operation.
internal func _expectCompatibleIndices(
  _ first: MinimalStrideableIndex,
  _ second: MinimalStrideableIndex,

  _ message: @autoclosure () -> String = "",
  stackTrace: SourceLocStack = SourceLocStack(),
  showFrame: Bool = true,
  file: String = #file, line: UInt = #line
) {
  if first._collectionState._id == second._collectionState._id {
    // Fast path: the indices are derived from the same state.
    return
  }

  // The indices are derived from different states.  Check that they point
  // to a self-consistent view of the collection.
  if first._collectionState._id > second._collectionState._id {
    return _expectCompatibleIndices(second, first)
  }

  func lastMutatedStateId(
    of i: MinimalStrideableIndex,
    in state: _CollectionState
  ) -> Int {
    let offset = i.position
    if offset == state._elementsLastMutatedStateIds.endIndex {
      return state._id
    }
    return state._elementsLastMutatedStateIds[offset]
  }

  let newestCollectionState = second._collectionState
  let expectedFirstIndexLastMutatedStateId =
    lastMutatedStateId(of: first, in: newestCollectionState)

  expectEqual(
    expectedFirstIndexLastMutatedStateId,
    first._collectionState._id,
    "Indices are not compatible:\n" +
    "first: \(first)\n" +
    "second: \(second)\n" +
    "first element last mutated in state id: \(first._collectionState._id)\n" +
    "expected state id: \(expectedFirstIndexLastMutatedStateId)\n" +
    "newest collection state: \(newestCollectionState)",
    stackTrace: stackTrace.pushIf(showFrame, file: file, line: line))

  // To make writing assertions easier, perform a trap.
  if expectedFirstIndexLastMutatedStateId != first._collectionState._id {
    fatalError("Indices are not compatible")
  }
}

public struct MinimalStrideableIndex : Comparable {
  public init(
    collectionState: _CollectionState,
    position: Int,
    startIndex: Int,
    endIndex: Int
  ) {
    expectTrapping(
      position,
      in: startIndex...endIndex)
    self = MinimalStrideableIndex(
      _collectionState: collectionState,
      uncheckedPosition: position)
  }

  internal init(
    _collectionState: _CollectionState,
    uncheckedPosition: Int
  ) {
    self._collectionState = _collectionState
    self.position = uncheckedPosition
  }

  public let _collectionState: _CollectionState
  public let position: Int

  public static var trapOnRangeCheckFailure = ResettableValue(true)

  public var timesAdvancedCalled = ResettableValue(0)
  public var timesDistanceCalled = ResettableValue(0)
}

public func == (lhs: MinimalStrideableIndex, rhs: MinimalStrideableIndex) -> Bool {
  _expectCompatibleIndices(lhs, rhs)
  return lhs.position == rhs.position
}

public func < (lhs: MinimalStrideableIndex, rhs: MinimalStrideableIndex) -> Bool {
  _expectCompatibleIndices(lhs, rhs)
  return lhs.position < rhs.position
}


extension MinimalStrideableIndex : Strideable {
  public typealias Stride = Int

  public func distance(to other: MinimalStrideableIndex) -> Int {
    timesDistanceCalled.value += 1
    _expectCompatibleIndices(self, other)
    return other.position - position
  }

  public func advanced(by n: Int) -> MinimalStrideableIndex {
    timesAdvancedCalled.value += 1
    return MinimalStrideableIndex(
      _collectionState: _collectionState,
      uncheckedPosition: position + n)
  }
}

//===----------------------------------------------------------------------===//
// Minimal***[Mutable]?Collection
//===----------------------------------------------------------------------===//


/// A minimal implementation of `Collection` with extra checks.
public struct MinimalCollection<T> : Collection {
  /// Creates a collection with given contents, but a unique modification
  /// history.  No other instance has the same modification history.
  public init<S : Sequence>(
    elements: S,
    underestimatedCount: UnderestimatedCountBehavior = .value(0)
  ) where S.Element == T {
    self._elements = Array(elements)

    self._collectionState = _CollectionState(
      newRootStateForElementCount: self._elements.count)

    switch underestimatedCount {
    case .precise:
      self.underestimatedCount = _elements.count

    case .half:
      self.underestimatedCount = _elements.count / 2

    case .overestimate:
      self.underestimatedCount = _elements.count * 3 + 5

    case .value(let count):
      self.underestimatedCount = count
    }
  }


  public let timesMakeIteratorCalled = ResettableValue(0)

  public func makeIterator() -> MinimalIterator<T> {
    timesMakeIteratorCalled.value += 1
    return MinimalIterator(_elements)
  }

  public typealias Index = MinimalIndex

  internal func _index(forPosition i: Int) -> MinimalIndex {
    return MinimalIndex(
      collectionState: _collectionState,
      position: i,
      startIndex: _elements.startIndex,
      endIndex: _elements.endIndex)
  }

  internal func _uncheckedIndex(forPosition i: Int) -> MinimalIndex {
    return MinimalIndex(
      _collectionState: _collectionState,
      uncheckedPosition: i)
  }

  public let timesStartIndexCalled = ResettableValue(0)

  public var startIndex: MinimalIndex {
    timesStartIndexCalled.value += 1
    return _uncheckedIndex(forPosition: _elements.startIndex)
  }

  public let timesEndIndexCalled = ResettableValue(0)

  public var endIndex: MinimalIndex {
    timesEndIndexCalled.value += 1
    return _uncheckedIndex(forPosition: _elements.endIndex)
  }


  public func _failEarlyRangeCheck(
    _ index: MinimalIndex,
    bounds: Range<MinimalIndex>
  ) {
    _expectCompatibleIndices(
      _uncheckedIndex(forPosition: index.position),
      index)

    _expectCompatibleIndices(
      _uncheckedIndex(forPosition: bounds.lowerBound.position),
      bounds.lowerBound)
    _expectCompatibleIndices(
      _uncheckedIndex(forPosition: bounds.upperBound.position),
      bounds.upperBound)

    expectTrapping(
      index.position,
      in: bounds.lowerBound.position..<bounds.upperBound.position)
  }

  public func _failEarlyRangeCheck(
    _ range: Range<MinimalIndex>,
    bounds: Range<MinimalIndex>
  ) {
    _expectCompatibleIndices(
      _uncheckedIndex(forPosition: range.lowerBound.position),
      range.lowerBound)
    _expectCompatibleIndices(
      _uncheckedIndex(forPosition: range.upperBound.position),
      range.upperBound)

    _expectCompatibleIndices(
      _uncheckedIndex(forPosition: bounds.lowerBound.position),
      bounds.lowerBound)
    _expectCompatibleIndices(
      _uncheckedIndex(forPosition: bounds.upperBound.position),
      bounds.upperBound)

    expectTrapping(
      range.lowerBound.position..<range.upperBound.position,
      in: bounds.lowerBound.position..<bounds.upperBound.position)
  }

  public func index(after i: MinimalIndex) -> MinimalIndex {
    _failEarlyRangeCheck(i, bounds: startIndex..<endIndex)
    return _uncheckedIndex(forPosition: i.position + 1)
  }


  public func distance(from start: MinimalIndex, to end: MinimalIndex)
    -> Int {
    _precondition(start <= end,
      "Only BidirectionalCollections can have end come before start")
    // FIXME: swift-3-indexing-model: perform a range check properly.
    if start != endIndex {
      _failEarlyRangeCheck(start, bounds: startIndex..<endIndex)
    }
    if end != endIndex {
      _failEarlyRangeCheck(end, bounds: startIndex..<endIndex)
    }
    return end.position - start.position
  }

  public func index(_ i: Index, offsetBy n: Int) -> Index {
    _precondition(n >= 0,
      "Only BidirectionalCollections can be advanced by a negative amount")
    // FIXME: swift-3-indexing-model: perform a range check properly.
    if i != endIndex {
      _failEarlyRangeCheck(i, bounds: startIndex..<endIndex)
    }
    return _index(forPosition: i.position + n)
  }

  public subscript(i: MinimalIndex) -> T {
    get {
      _failEarlyRangeCheck(i, bounds: startIndex..<endIndex)
      return _elements[i.position]
    }
  }

  public subscript(bounds: Range<MinimalIndex>) -> Slice<MinimalCollection<T>> {
    get {
      _failEarlyRangeCheck(bounds, bounds: startIndex..<endIndex)
      return Slice(base: self, bounds: bounds)
    }
  }


  public var underestimatedCount: Int

  internal var _elements: [T]
  internal let _collectionState: _CollectionState
}


/// A minimal implementation of `Collection` with extra checks.
public struct MinimalRangeReplaceableCollection<T> : Collection, RangeReplaceableCollection {
  /// Creates a collection with given contents, but a unique modification
  /// history.  No other instance has the same modification history.
  public init<S : Sequence>(
    elements: S,
    underestimatedCount: UnderestimatedCountBehavior = .value(0)
  ) where S.Element == T {
    self._elements = Array(elements)

    self._collectionState = _CollectionState(
      newRootStateForElementCount: self._elements.count)

    switch underestimatedCount {
    case .precise:
      self.underestimatedCount = _elements.count

    case .half:
      self.underestimatedCount = _elements.count / 2

    case .overestimate:
      self.underestimatedCount = _elements.count * 3 + 5

    case .value(let count):
      self.underestimatedCount = count
    }
  }

  public init() {
    self.underestimatedCount = 0
    self._elements = []
    self._collectionState =
      _CollectionState(name: "\(type(of: self))", elementCount: 0)
  }

  public init<S : Sequence>(_ elements: S) where S.Element == T {
    self.underestimatedCount = 0
    self._elements = Array(elements)
    self._collectionState =
      _CollectionState(newRootStateForElementCount: self._elements.count)
  }

  public let timesMakeIteratorCalled = ResettableValue(0)

  public func makeIterator() -> MinimalIterator<T> {
    timesMakeIteratorCalled.value += 1
    return MinimalIterator(_elements)
  }

  public typealias Index = MinimalIndex

  internal func _index(forPosition i: Int) -> MinimalIndex {
    return MinimalIndex(
      collectionState: _collectionState,
      position: i,
      startIndex: _elements.startIndex,
      endIndex: _elements.endIndex)
  }

  internal func _uncheckedIndex(forPosition i: Int) -> MinimalIndex {
    return MinimalIndex(
      _collectionState: _collectionState,
      uncheckedPosition: i)
  }

  public let timesStartIndexCalled = ResettableValue(0)

  public var startIndex: MinimalIndex {
    timesStartIndexCalled.value += 1
    return _uncheckedIndex(forPosition: _elements.startIndex)
  }

  public let timesEndIndexCalled = ResettableValue(0)

  public var endIndex: MinimalIndex {
    timesEndIndexCalled.value += 1
    return _uncheckedIndex(forPosition: _elements.endIndex)
  }


  public func _failEarlyRangeCheck(
    _ index: MinimalIndex,
    bounds: Range<MinimalIndex>
  ) {
    _expectCompatibleIndices(
      _uncheckedIndex(forPosition: index.position),
      index)

    _expectCompatibleIndices(
      _uncheckedIndex(forPosition: bounds.lowerBound.position),
      bounds.lowerBound)
    _expectCompatibleIndices(
      _uncheckedIndex(forPosition: bounds.upperBound.position),
      bounds.upperBound)

    expectTrapping(
      index.position,
      in: bounds.lowerBound.position..<bounds.upperBound.position)
  }

  public func _failEarlyRangeCheck(
    _ range: Range<MinimalIndex>,
    bounds: Range<MinimalIndex>
  ) {
    _expectCompatibleIndices(
      _uncheckedIndex(forPosition: range.lowerBound.position),
      range.lowerBound)
    _expectCompatibleIndices(
      _uncheckedIndex(forPosition: range.upperBound.position),
      range.upperBound)

    _expectCompatibleIndices(
      _uncheckedIndex(forPosition: bounds.lowerBound.position),
      bounds.lowerBound)
    _expectCompatibleIndices(
      _uncheckedIndex(forPosition: bounds.upperBound.position),
      bounds.upperBound)

    expectTrapping(
      range.lowerBound.position..<range.upperBound.position,
      in: bounds.lowerBound.position..<bounds.upperBound.position)
  }

  public func index(after i: MinimalIndex) -> MinimalIndex {
    _failEarlyRangeCheck(i, bounds: startIndex..<endIndex)
    return _uncheckedIndex(forPosition: i.position + 1)
  }


  public func distance(from start: MinimalIndex, to end: MinimalIndex)
    -> Int {
    _precondition(start <= end,
      "Only BidirectionalCollections can have end come before start")
    // FIXME: swift-3-indexing-model: perform a range check properly.
    if start != endIndex {
      _failEarlyRangeCheck(start, bounds: startIndex..<endIndex)
    }
    if end != endIndex {
      _failEarlyRangeCheck(end, bounds: startIndex..<endIndex)
    }
    return end.position - start.position
  }

  public func index(_ i: Index, offsetBy n: Int) -> Index {
    _precondition(n >= 0,
      "Only BidirectionalCollections can be advanced by a negative amount")
    // FIXME: swift-3-indexing-model: perform a range check properly.
    if i != endIndex {
      _failEarlyRangeCheck(i, bounds: startIndex..<endIndex)
    }
    return _index(forPosition: i.position + n)
  }

  public subscript(i: MinimalIndex) -> T {
    get {
      _failEarlyRangeCheck(i, bounds: startIndex..<endIndex)
      return _elements[i.position]
    }
  }

  public subscript(bounds: Range<MinimalIndex>) -> Slice<MinimalRangeReplaceableCollection<T>> {
    get {
      _failEarlyRangeCheck(bounds, bounds: startIndex..<endIndex)
      return Slice(base: self, bounds: bounds)
    }
  }

  public mutating func reserveCapacity(_ n: Int) {
    _willMutate(.reserveCapacity(capacity: n))
    _elements.reserveCapacity(n)
    reservedCapacity = Swift.max(reservedCapacity, n)
  }

  public mutating func append(_ x: T) {
    _willMutate(.append)
    _elements.append(x)
  }

  public mutating func append<S : Sequence>(contentsOf newElements: S)
    where S.Element == T {
    let oldCount = count
    _elements.append(contentsOf: newElements)
    let newCount = count
    _willMutate(.appendContentsOf(count: newCount - oldCount))
  }

  public mutating func replaceSubrange<C>(
    _ subRange: Range<MinimalIndex>,
    with newElements: C
  ) where C : Collection, C.Element == T {
    let oldCount = count
    _elements.replaceSubrange(
      subRange.lowerBound.position..<subRange.upperBound.position,
      with: newElements)
    let newCount = count
    _willMutate(.replaceRange(
      subRange: subRange.lowerBound.position..<subRange.upperBound.position,
      replacementCount:
        subRange.upperBound.position - subRange.lowerBound.position
        + newCount - oldCount))
  }

  public mutating func insert(_ newElement: T, at i: MinimalIndex) {
    _willMutate(.insert(atIndex: i.position))
    _elements.insert(newElement, at: i.position)
  }

  public mutating func insert<S : Collection>(
    contentsOf newElements: S, at i: MinimalIndex
  ) where S.Element == T {
    let oldCount = count
    _elements.insert(contentsOf: newElements, at: i.position)
    let newCount = count

    if newCount - oldCount != 0 {
      _willMutate(.insertContentsOf(
        atIndex: i.position,
        count: newCount - oldCount))
    }
  }

  @discardableResult
  public mutating func remove(at i: MinimalIndex) -> T {
    _willMutate(.removeAtIndex(index: i.position))
    return _elements.remove(at: i.position)
  }

  @discardableResult
  public mutating func removeLast() -> T {
    _willMutate(.removeLast)
    return _elements.removeLast()
  }

  public mutating func removeSubrange(_ subRange: Range<MinimalIndex>) {
    if !subRange.isEmpty {
      _willMutate(.removeRange(
        subRange: subRange.lowerBound.position..<subRange.upperBound.position))
    }
    _elements.removeSubrange(
      subRange.lowerBound.position..<subRange.upperBound.position
    )
  }

  public mutating func removeAll(keepingCapacity keepCapacity: Bool = false) {
    _willMutate(.removeAll(keepCapacity: keepCapacity))
    // Ignore the value of `keepCapacity`.
    _elements.removeAll(keepingCapacity: false)
  }

  internal mutating func _willMutate(_ operation: _CollectionOperation) {
    _collectionState = _CollectionStateTransition(
      previousState: _collectionState,
      operation: operation)._nextState
  }

  public var underestimatedCount: Int
  public var reservedCapacity: Int = 0

  internal var _elements: [T]
  internal var _collectionState: _CollectionState
}


/// A minimal implementation of `Collection` with extra checks.
public struct MinimalMutableCollection<T> : Collection, MutableCollection {
  /// Creates a collection with given contents, but a unique modification
  /// history.  No other instance has the same modification history.
  public init<S : Sequence>(
    elements: S,
    underestimatedCount: UnderestimatedCountBehavior = .value(0)
  ) where S.Element == T {
    self._elements = Array(elements)

    self._collectionState = _CollectionState(
      newRootStateForElementCount: self._elements.count)

    switch underestimatedCount {
    case .precise:
      self.underestimatedCount = _elements.count

    case .half:
      self.underestimatedCount = _elements.count / 2

    case .overestimate:
      self.underestimatedCount = _elements.count * 3 + 5

    case .value(let count):
      self.underestimatedCount = count
    }
  }


  public let timesMakeIteratorCalled = ResettableValue(0)

  public func makeIterator() -> MinimalIterator<T> {
    timesMakeIteratorCalled.value += 1
    return MinimalIterator(_elements)
  }

  public typealias Index = MinimalIndex

  internal func _index(forPosition i: Int) -> MinimalIndex {
    return MinimalIndex(
      collectionState: _collectionState,
      position: i,
      startIndex: _elements.startIndex,
      endIndex: _elements.endIndex)
  }

  internal func _uncheckedIndex(forPosition i: Int) -> MinimalIndex {
    return MinimalIndex(
      _collectionState: _collectionState,
      uncheckedPosition: i)
  }

  public let timesStartIndexCalled = ResettableValue(0)

  public var startIndex: MinimalIndex {
    timesStartIndexCalled.value += 1
    return _uncheckedIndex(forPosition: _elements.startIndex)
  }

  public let timesEndIndexCalled = ResettableValue(0)

  public var endIndex: MinimalIndex {
    timesEndIndexCalled.value += 1
    return _uncheckedIndex(forPosition: _elements.endIndex)
  }


  public func _failEarlyRangeCheck(
    _ index: MinimalIndex,
    bounds: Range<MinimalIndex>
  ) {
    _expectCompatibleIndices(
      _uncheckedIndex(forPosition: index.position),
      index)

    _expectCompatibleIndices(
      _uncheckedIndex(forPosition: bounds.lowerBound.position),
      bounds.lowerBound)
    _expectCompatibleIndices(
      _uncheckedIndex(forPosition: bounds.upperBound.position),
      bounds.upperBound)

    expectTrapping(
      index.position,
      in: bounds.lowerBound.position..<bounds.upperBound.position)
  }

  public func _failEarlyRangeCheck(
    _ range: Range<MinimalIndex>,
    bounds: Range<MinimalIndex>
  ) {
    _expectCompatibleIndices(
      _uncheckedIndex(forPosition: range.lowerBound.position),
      range.lowerBound)
    _expectCompatibleIndices(
      _uncheckedIndex(forPosition: range.upperBound.position),
      range.upperBound)

    _expectCompatibleIndices(
      _uncheckedIndex(forPosition: bounds.lowerBound.position),
      bounds.lowerBound)
    _expectCompatibleIndices(
      _uncheckedIndex(forPosition: bounds.upperBound.position),
      bounds.upperBound)

    expectTrapping(
      range.lowerBound.position..<range.upperBound.position,
      in: bounds.lowerBound.position..<bounds.upperBound.position)
  }

  public func index(after i: MinimalIndex) -> MinimalIndex {
    _failEarlyRangeCheck(i, bounds: startIndex..<endIndex)
    return _uncheckedIndex(forPosition: i.position + 1)
  }


  public func distance(from start: MinimalIndex, to end: MinimalIndex)
    -> Int {
    _precondition(start <= end,
      "Only BidirectionalCollections can have end come before start")
    // FIXME: swift-3-indexing-model: perform a range check properly.
    if start != endIndex {
      _failEarlyRangeCheck(start, bounds: startIndex..<endIndex)
    }
    if end != endIndex {
      _failEarlyRangeCheck(end, bounds: startIndex..<endIndex)
    }
    return end.position - start.position
  }

  public func index(_ i: Index, offsetBy n: Int) -> Index {
    _precondition(n >= 0,
      "Only BidirectionalCollections can be advanced by a negative amount")
    // FIXME: swift-3-indexing-model: perform a range check properly.
    if i != endIndex {
      _failEarlyRangeCheck(i, bounds: startIndex..<endIndex)
    }
    return _index(forPosition: i.position + n)
  }

  public subscript(i: MinimalIndex) -> T {
    get {
      _failEarlyRangeCheck(i, bounds: startIndex..<endIndex)
      return _elements[i.position]
    }
    set {
      _failEarlyRangeCheck(i, bounds: startIndex..<endIndex)
      _elements[i.position] = newValue
    }
  }

  public subscript(bounds: Range<MinimalIndex>) -> Slice<MinimalMutableCollection<T>> {
    get {
      _failEarlyRangeCheck(bounds, bounds: startIndex..<endIndex)
      return Slice(base: self, bounds: bounds)
    }
    set {
      _failEarlyRangeCheck(bounds, bounds: startIndex..<endIndex)
      _writeBackMutableSlice(&self, bounds: bounds, slice: newValue)
    }
  }


  public var underestimatedCount: Int

  internal var _elements: [T]
  internal let _collectionState: _CollectionState
}


/// A minimal implementation of `Collection` with extra checks.
public struct MinimalMutableRangeReplaceableCollection<T> : Collection, MutableCollection, RangeReplaceableCollection {
  /// Creates a collection with given contents, but a unique modification
  /// history.  No other instance has the same modification history.
  public init<S : Sequence>(
    elements: S,
    underestimatedCount: UnderestimatedCountBehavior = .value(0)
  ) where S.Element == T {
    self._elements = Array(elements)

    self._collectionState = _CollectionState(
      newRootStateForElementCount: self._elements.count)

    switch underestimatedCount {
    case .precise:
      self.underestimatedCount = _elements.count

    case .half:
      self.underestimatedCount = _elements.count / 2

    case .overestimate:
      self.underestimatedCount = _elements.count * 3 + 5

    case .value(let count):
      self.underestimatedCount = count
    }
  }

  public init() {
    self.underestimatedCount = 0
    self._elements = []
    self._collectionState =
      _CollectionState(name: "\(type(of: self))", elementCount: 0)
  }

  public init<S : Sequence>(_ elements: S) where S.Element == T {
    self.underestimatedCount = 0
    self._elements = Array(elements)
    self._collectionState =
      _CollectionState(newRootStateForElementCount: self._elements.count)
  }

  public let timesMakeIteratorCalled = ResettableValue(0)

  public func makeIterator() -> MinimalIterator<T> {
    timesMakeIteratorCalled.value += 1
    return MinimalIterator(_elements)
  }

  public typealias Index = MinimalIndex

  internal func _index(forPosition i: Int) -> MinimalIndex {
    return MinimalIndex(
      collectionState: _collectionState,
      position: i,
      startIndex: _elements.startIndex,
      endIndex: _elements.endIndex)
  }

  internal func _uncheckedIndex(forPosition i: Int) -> MinimalIndex {
    return MinimalIndex(
      _collectionState: _collectionState,
      uncheckedPosition: i)
  }

  public let timesStartIndexCalled = ResettableValue(0)

  public var startIndex: MinimalIndex {
    timesStartIndexCalled.value += 1
    return _uncheckedIndex(forPosition: _elements.startIndex)
  }

  public let timesEndIndexCalled = ResettableValue(0)

  public var endIndex: MinimalIndex {
    timesEndIndexCalled.value += 1
    return _uncheckedIndex(forPosition: _elements.endIndex)
  }


  public func _failEarlyRangeCheck(
    _ index: MinimalIndex,
    bounds: Range<MinimalIndex>
  ) {
    _expectCompatibleIndices(
      _uncheckedIndex(forPosition: index.position),
      index)

    _expectCompatibleIndices(
      _uncheckedIndex(forPosition: bounds.lowerBound.position),
      bounds.lowerBound)
    _expectCompatibleIndices(
      _uncheckedIndex(forPosition: bounds.upperBound.position),
      bounds.upperBound)

    expectTrapping(
      index.position,
      in: bounds.lowerBound.position..<bounds.upperBound.position)
  }

  public func _failEarlyRangeCheck(
    _ range: Range<MinimalIndex>,
    bounds: Range<MinimalIndex>
  ) {
    _expectCompatibleIndices(
      _uncheckedIndex(forPosition: range.lowerBound.position),
      range.lowerBound)
    _expectCompatibleIndices(
      _uncheckedIndex(forPosition: range.upperBound.position),
      range.upperBound)

    _expectCompatibleIndices(
      _uncheckedIndex(forPosition: bounds.lowerBound.position),
      bounds.lowerBound)
    _expectCompatibleIndices(
      _uncheckedIndex(forPosition: bounds.upperBound.position),
      bounds.upperBound)

    expectTrapping(
      range.lowerBound.position..<range.upperBound.position,
      in: bounds.lowerBound.position..<bounds.upperBound.position)
  }

  public func index(after i: MinimalIndex) -> MinimalIndex {
    _failEarlyRangeCheck(i, bounds: startIndex..<endIndex)
    return _uncheckedIndex(forPosition: i.position + 1)
  }


  public func distance(from start: MinimalIndex, to end: MinimalIndex)
    -> Int {
    _precondition(start <= end,
      "Only BidirectionalCollections can have end come before start")
    // FIXME: swift-3-indexing-model: perform a range check properly.
    if start != endIndex {
      _failEarlyRangeCheck(start, bounds: startIndex..<endIndex)
    }
    if end != endIndex {
      _failEarlyRangeCheck(end, bounds: startIndex..<endIndex)
    }
    return end.position - start.position
  }

  public func index(_ i: Index, offsetBy n: Int) -> Index {
    _precondition(n >= 0,
      "Only BidirectionalCollections can be advanced by a negative amount")
    // FIXME: swift-3-indexing-model: perform a range check properly.
    if i != endIndex {
      _failEarlyRangeCheck(i, bounds: startIndex..<endIndex)
    }
    return _index(forPosition: i.position + n)
  }

  public subscript(i: MinimalIndex) -> T {
    get {
      _failEarlyRangeCheck(i, bounds: startIndex..<endIndex)
      return _elements[i.position]
    }
    set {
      _failEarlyRangeCheck(i, bounds: startIndex..<endIndex)
      _elements[i.position] = newValue
    }
  }

  public subscript(bounds: Range<MinimalIndex>) -> Slice<MinimalMutableRangeReplaceableCollection<T>> {
    get {
      _failEarlyRangeCheck(bounds, bounds: startIndex..<endIndex)
      return Slice(base: self, bounds: bounds)
    }
    set {
      _failEarlyRangeCheck(bounds, bounds: startIndex..<endIndex)
      _writeBackMutableSlice(&self, bounds: bounds, slice: newValue)
    }
  }

  public mutating func reserveCapacity(_ n: Int) {
    _willMutate(.reserveCapacity(capacity: n))
    _elements.reserveCapacity(n)
    reservedCapacity = Swift.max(reservedCapacity, n)
  }

  public mutating func append(_ x: T) {
    _willMutate(.append)
    _elements.append(x)
  }

  public mutating func append<S : Sequence>(contentsOf newElements: S)
    where S.Element == T {
    let oldCount = count
    _elements.append(contentsOf: newElements)
    let newCount = count
    _willMutate(.appendContentsOf(count: newCount - oldCount))
  }

  public mutating func replaceSubrange<C>(
    _ subRange: Range<MinimalIndex>,
    with newElements: C
  ) where C : Collection, C.Element == T {
    let oldCount = count
    _elements.replaceSubrange(
      subRange.lowerBound.position..<subRange.upperBound.position,
      with: newElements)
    let newCount = count
    _willMutate(.replaceRange(
      subRange: subRange.lowerBound.position..<subRange.upperBound.position,
      replacementCount:
        subRange.upperBound.position - subRange.lowerBound.position
        + newCount - oldCount))
  }

  public mutating func insert(_ newElement: T, at i: MinimalIndex) {
    _willMutate(.insert(atIndex: i.position))
    _elements.insert(newElement, at: i.position)
  }

  public mutating func insert<S : Collection>(
    contentsOf newElements: S, at i: MinimalIndex
  ) where S.Element == T {
    let oldCount = count
    _elements.insert(contentsOf: newElements, at: i.position)
    let newCount = count

    if newCount - oldCount != 0 {
      _willMutate(.insertContentsOf(
        atIndex: i.position,
        count: newCount - oldCount))
    }
  }

  @discardableResult
  public mutating func remove(at i: MinimalIndex) -> T {
    _willMutate(.removeAtIndex(index: i.position))
    return _elements.remove(at: i.position)
  }

  @discardableResult
  public mutating func removeLast() -> T {
    _willMutate(.removeLast)
    return _elements.removeLast()
  }

  public mutating func removeSubrange(_ subRange: Range<MinimalIndex>) {
    if !subRange.isEmpty {
      _willMutate(.removeRange(
        subRange: subRange.lowerBound.position..<subRange.upperBound.position))
    }
    _elements.removeSubrange(
      subRange.lowerBound.position..<subRange.upperBound.position
    )
  }

  public mutating func removeAll(keepingCapacity keepCapacity: Bool = false) {
    _willMutate(.removeAll(keepCapacity: keepCapacity))
    // Ignore the value of `keepCapacity`.
    _elements.removeAll(keepingCapacity: false)
  }

  internal mutating func _willMutate(_ operation: _CollectionOperation) {
    _collectionState = _CollectionStateTransition(
      previousState: _collectionState,
      operation: operation)._nextState
  }

  public var underestimatedCount: Int
  public var reservedCapacity: Int = 0

  internal var _elements: [T]
  internal var _collectionState: _CollectionState
}


/// A minimal implementation of `Collection` with extra checks.
public struct MinimalBidirectionalCollection<T> : BidirectionalCollection {
  /// Creates a collection with given contents, but a unique modification
  /// history.  No other instance has the same modification history.
  public init<S : Sequence>(
    elements: S,
    underestimatedCount: UnderestimatedCountBehavior = .value(0)
  ) where S.Element == T {
    self._elements = Array(elements)

    self._collectionState = _CollectionState(
      newRootStateForElementCount: self._elements.count)

    switch underestimatedCount {
    case .precise:
      self.underestimatedCount = _elements.count

    case .half:
      self.underestimatedCount = _elements.count / 2

    case .overestimate:
      self.underestimatedCount = _elements.count * 3 + 5

    case .value(let count):
      self.underestimatedCount = count
    }
  }


  public let timesMakeIteratorCalled = ResettableValue(0)

  public func makeIterator() -> MinimalIterator<T> {
    timesMakeIteratorCalled.value += 1
    return MinimalIterator(_elements)
  }

  public typealias Index = MinimalIndex

  internal func _index(forPosition i: Int) -> MinimalIndex {
    return MinimalIndex(
      collectionState: _collectionState,
      position: i,
      startIndex: _elements.startIndex,
      endIndex: _elements.endIndex)
  }

  internal func _uncheckedIndex(forPosition i: Int) -> MinimalIndex {
    return MinimalIndex(
      _collectionState: _collectionState,
      uncheckedPosition: i)
  }

  public let timesStartIndexCalled = ResettableValue(0)

  public var startIndex: MinimalIndex {
    timesStartIndexCalled.value += 1
    return _uncheckedIndex(forPosition: _elements.startIndex)
  }

  public let timesEndIndexCalled = ResettableValue(0)

  public var endIndex: MinimalIndex {
    timesEndIndexCalled.value += 1
    return _uncheckedIndex(forPosition: _elements.endIndex)
  }


  public func _failEarlyRangeCheck(
    _ index: MinimalIndex,
    bounds: Range<MinimalIndex>
  ) {
    _expectCompatibleIndices(
      _uncheckedIndex(forPosition: index.position),
      index)

    _expectCompatibleIndices(
      _uncheckedIndex(forPosition: bounds.lowerBound.position),
      bounds.lowerBound)
    _expectCompatibleIndices(
      _uncheckedIndex(forPosition: bounds.upperBound.position),
      bounds.upperBound)

    expectTrapping(
      index.position,
      in: bounds.lowerBound.position..<bounds.upperBound.position)
  }

  public func _failEarlyRangeCheck(
    _ range: Range<MinimalIndex>,
    bounds: Range<MinimalIndex>
  ) {
    _expectCompatibleIndices(
      _uncheckedIndex(forPosition: range.lowerBound.position),
      range.lowerBound)
    _expectCompatibleIndices(
      _uncheckedIndex(forPosition: range.upperBound.position),
      range.upperBound)

    _expectCompatibleIndices(
      _uncheckedIndex(forPosition: bounds.lowerBound.position),
      bounds.lowerBound)
    _expectCompatibleIndices(
      _uncheckedIndex(forPosition: bounds.upperBound.position),
      bounds.upperBound)

    expectTrapping(
      range.lowerBound.position..<range.upperBound.position,
      in: bounds.lowerBound.position..<bounds.upperBound.position)
  }

  public func index(after i: MinimalIndex) -> MinimalIndex {
    _failEarlyRangeCheck(i, bounds: startIndex..<endIndex)
    return _uncheckedIndex(forPosition: i.position + 1)
  }

  public func index(before i: MinimalIndex) -> MinimalIndex {
    // FIXME: swift-3-indexing-model: perform a range check and use
    // return _uncheckedIndex(forPosition: i.position - 1)
    return _index(forPosition: i.position - 1)
  }

  public func distance(from start: MinimalIndex, to end: MinimalIndex)
    -> Int {
    // FIXME: swift-3-indexing-model: perform a range check properly.
    if start != endIndex {
      _failEarlyRangeCheck(start, bounds: startIndex..<endIndex)
    }
    if end != endIndex {
      _failEarlyRangeCheck(end, bounds: startIndex..<endIndex)
    }
    return end.position - start.position
  }

  public func index(_ i: Index, offsetBy n: Int) -> Index {
    // FIXME: swift-3-indexing-model: perform a range check properly.
    if i != endIndex {
      _failEarlyRangeCheck(i, bounds: startIndex..<endIndex)
    }
    return _index(forPosition: i.position + n)
  }

  public subscript(i: MinimalIndex) -> T {
    get {
      _failEarlyRangeCheck(i, bounds: startIndex..<endIndex)
      return _elements[i.position]
    }
  }

  public subscript(bounds: Range<MinimalIndex>) -> Slice<MinimalBidirectionalCollection<T>> {
    get {
      _failEarlyRangeCheck(bounds, bounds: startIndex..<endIndex)
      return Slice(base: self, bounds: bounds)
    }
  }


  public var underestimatedCount: Int

  internal var _elements: [T]
  internal let _collectionState: _CollectionState
}


/// A minimal implementation of `Collection` with extra checks.
public struct MinimalRangeReplaceableBidirectionalCollection<T> : BidirectionalCollection, RangeReplaceableCollection {
  /// Creates a collection with given contents, but a unique modification
  /// history.  No other instance has the same modification history.
  public init<S : Sequence>(
    elements: S,
    underestimatedCount: UnderestimatedCountBehavior = .value(0)
  ) where S.Element == T {
    self._elements = Array(elements)

    self._collectionState = _CollectionState(
      newRootStateForElementCount: self._elements.count)

    switch underestimatedCount {
    case .precise:
      self.underestimatedCount = _elements.count

    case .half:
      self.underestimatedCount = _elements.count / 2

    case .overestimate:
      self.underestimatedCount = _elements.count * 3 + 5

    case .value(let count):
      self.underestimatedCount = count
    }
  }

  public init() {
    self.underestimatedCount = 0
    self._elements = []
    self._collectionState =
      _CollectionState(name: "\(type(of: self))", elementCount: 0)
  }

  public init<S : Sequence>(_ elements: S) where S.Element == T {
    self.underestimatedCount = 0
    self._elements = Array(elements)
    self._collectionState =
      _CollectionState(newRootStateForElementCount: self._elements.count)
  }

  public let timesMakeIteratorCalled = ResettableValue(0)

  public func makeIterator() -> MinimalIterator<T> {
    timesMakeIteratorCalled.value += 1
    return MinimalIterator(_elements)
  }

  public typealias Index = MinimalIndex

  internal func _index(forPosition i: Int) -> MinimalIndex {
    return MinimalIndex(
      collectionState: _collectionState,
      position: i,
      startIndex: _elements.startIndex,
      endIndex: _elements.endIndex)
  }

  internal func _uncheckedIndex(forPosition i: Int) -> MinimalIndex {
    return MinimalIndex(
      _collectionState: _collectionState,
      uncheckedPosition: i)
  }

  public let timesStartIndexCalled = ResettableValue(0)

  public var startIndex: MinimalIndex {
    timesStartIndexCalled.value += 1
    return _uncheckedIndex(forPosition: _elements.startIndex)
  }

  public let timesEndIndexCalled = ResettableValue(0)

  public var endIndex: MinimalIndex {
    timesEndIndexCalled.value += 1
    return _uncheckedIndex(forPosition: _elements.endIndex)
  }


  public func _failEarlyRangeCheck(
    _ index: MinimalIndex,
    bounds: Range<MinimalIndex>
  ) {
    _expectCompatibleIndices(
      _uncheckedIndex(forPosition: index.position),
      index)

    _expectCompatibleIndices(
      _uncheckedIndex(forPosition: bounds.lowerBound.position),
      bounds.lowerBound)
    _expectCompatibleIndices(
      _uncheckedIndex(forPosition: bounds.upperBound.position),
      bounds.upperBound)

    expectTrapping(
      index.position,
      in: bounds.lowerBound.position..<bounds.upperBound.position)
  }

  public func _failEarlyRangeCheck(
    _ range: Range<MinimalIndex>,
    bounds: Range<MinimalIndex>
  ) {
    _expectCompatibleIndices(
      _uncheckedIndex(forPosition: range.lowerBound.position),
      range.lowerBound)
    _expectCompatibleIndices(
      _uncheckedIndex(forPosition: range.upperBound.position),
      range.upperBound)

    _expectCompatibleIndices(
      _uncheckedIndex(forPosition: bounds.lowerBound.position),
      bounds.lowerBound)
    _expectCompatibleIndices(
      _uncheckedIndex(forPosition: bounds.upperBound.position),
      bounds.upperBound)

    expectTrapping(
      range.lowerBound.position..<range.upperBound.position,
      in: bounds.lowerBound.position..<bounds.upperBound.position)
  }

  public func index(after i: MinimalIndex) -> MinimalIndex {
    _failEarlyRangeCheck(i, bounds: startIndex..<endIndex)
    return _uncheckedIndex(forPosition: i.position + 1)
  }

  public func index(before i: MinimalIndex) -> MinimalIndex {
    // FIXME: swift-3-indexing-model: perform a range check and use
    // return _uncheckedIndex(forPosition: i.position - 1)
    return _index(forPosition: i.position - 1)
  }

  public func distance(from start: MinimalIndex, to end: MinimalIndex)
    -> Int {
    // FIXME: swift-3-indexing-model: perform a range check properly.
    if start != endIndex {
      _failEarlyRangeCheck(start, bounds: startIndex..<endIndex)
    }
    if end != endIndex {
      _failEarlyRangeCheck(end, bounds: startIndex..<endIndex)
    }
    return end.position - start.position
  }

  public func index(_ i: Index, offsetBy n: Int) -> Index {
    // FIXME: swift-3-indexing-model: perform a range check properly.
    if i != endIndex {
      _failEarlyRangeCheck(i, bounds: startIndex..<endIndex)
    }
    return _index(forPosition: i.position + n)
  }

  public subscript(i: MinimalIndex) -> T {
    get {
      _failEarlyRangeCheck(i, bounds: startIndex..<endIndex)
      return _elements[i.position]
    }
  }

  public subscript(bounds: Range<MinimalIndex>) -> Slice<MinimalRangeReplaceableBidirectionalCollection<T>> {
    get {
      _failEarlyRangeCheck(bounds, bounds: startIndex..<endIndex)
      return Slice(base: self, bounds: bounds)
    }
  }

  public mutating func reserveCapacity(_ n: Int) {
    _willMutate(.reserveCapacity(capacity: n))
    _elements.reserveCapacity(n)
    reservedCapacity = Swift.max(reservedCapacity, n)
  }

  public mutating func append(_ x: T) {
    _willMutate(.append)
    _elements.append(x)
  }

  public mutating func append<S : Sequence>(contentsOf newElements: S)
    where S.Element == T {
    let oldCount = count
    _elements.append(contentsOf: newElements)
    let newCount = count
    _willMutate(.appendContentsOf(count: newCount - oldCount))
  }

  public mutating func replaceSubrange<C>(
    _ subRange: Range<MinimalIndex>,
    with newElements: C
  ) where C : Collection, C.Element == T {
    let oldCount = count
    _elements.replaceSubrange(
      subRange.lowerBound.position..<subRange.upperBound.position,
      with: newElements)
    let newCount = count
    _willMutate(.replaceRange(
      subRange: subRange.lowerBound.position..<subRange.upperBound.position,
      replacementCount:
        subRange.upperBound.position - subRange.lowerBound.position
        + newCount - oldCount))
  }

  public mutating func insert(_ newElement: T, at i: MinimalIndex) {
    _willMutate(.insert(atIndex: i.position))
    _elements.insert(newElement, at: i.position)
  }

  public mutating func insert<S : Collection>(
    contentsOf newElements: S, at i: MinimalIndex
  ) where S.Element == T {
    let oldCount = count
    _elements.insert(contentsOf: newElements, at: i.position)
    let newCount = count

    if newCount - oldCount != 0 {
      _willMutate(.insertContentsOf(
        atIndex: i.position,
        count: newCount - oldCount))
    }
  }

  @discardableResult
  public mutating func remove(at i: MinimalIndex) -> T {
    _willMutate(.removeAtIndex(index: i.position))
    return _elements.remove(at: i.position)
  }

  @discardableResult
  public mutating func removeLast() -> T {
    _willMutate(.removeLast)
    return _elements.removeLast()
  }

  public mutating func removeSubrange(_ subRange: Range<MinimalIndex>) {
    if !subRange.isEmpty {
      _willMutate(.removeRange(
        subRange: subRange.lowerBound.position..<subRange.upperBound.position))
    }
    _elements.removeSubrange(
      subRange.lowerBound.position..<subRange.upperBound.position
    )
  }

  public mutating func removeAll(keepingCapacity keepCapacity: Bool = false) {
    _willMutate(.removeAll(keepCapacity: keepCapacity))
    // Ignore the value of `keepCapacity`.
    _elements.removeAll(keepingCapacity: false)
  }

  internal mutating func _willMutate(_ operation: _CollectionOperation) {
    _collectionState = _CollectionStateTransition(
      previousState: _collectionState,
      operation: operation)._nextState
  }

  public var underestimatedCount: Int
  public var reservedCapacity: Int = 0

  internal var _elements: [T]
  internal var _collectionState: _CollectionState
}


/// A minimal implementation of `Collection` with extra checks.
public struct MinimalMutableBidirectionalCollection<T> : BidirectionalCollection, MutableCollection {
  /// Creates a collection with given contents, but a unique modification
  /// history.  No other instance has the same modification history.
  public init<S : Sequence>(
    elements: S,
    underestimatedCount: UnderestimatedCountBehavior = .value(0)
  ) where S.Element == T {
    self._elements = Array(elements)

    self._collectionState = _CollectionState(
      newRootStateForElementCount: self._elements.count)

    switch underestimatedCount {
    case .precise:
      self.underestimatedCount = _elements.count

    case .half:
      self.underestimatedCount = _elements.count / 2

    case .overestimate:
      self.underestimatedCount = _elements.count * 3 + 5

    case .value(let count):
      self.underestimatedCount = count
    }
  }


  public let timesMakeIteratorCalled = ResettableValue(0)

  public func makeIterator() -> MinimalIterator<T> {
    timesMakeIteratorCalled.value += 1
    return MinimalIterator(_elements)
  }

  public typealias Index = MinimalIndex

  internal func _index(forPosition i: Int) -> MinimalIndex {
    return MinimalIndex(
      collectionState: _collectionState,
      position: i,
      startIndex: _elements.startIndex,
      endIndex: _elements.endIndex)
  }

  internal func _uncheckedIndex(forPosition i: Int) -> MinimalIndex {
    return MinimalIndex(
      _collectionState: _collectionState,
      uncheckedPosition: i)
  }

  public let timesStartIndexCalled = ResettableValue(0)

  public var startIndex: MinimalIndex {
    timesStartIndexCalled.value += 1
    return _uncheckedIndex(forPosition: _elements.startIndex)
  }

  public let timesEndIndexCalled = ResettableValue(0)

  public var endIndex: MinimalIndex {
    timesEndIndexCalled.value += 1
    return _uncheckedIndex(forPosition: _elements.endIndex)
  }


  public func _failEarlyRangeCheck(
    _ index: MinimalIndex,
    bounds: Range<MinimalIndex>
  ) {
    _expectCompatibleIndices(
      _uncheckedIndex(forPosition: index.position),
      index)

    _expectCompatibleIndices(
      _uncheckedIndex(forPosition: bounds.lowerBound.position),
      bounds.lowerBound)
    _expectCompatibleIndices(
      _uncheckedIndex(forPosition: bounds.upperBound.position),
      bounds.upperBound)

    expectTrapping(
      index.position,
      in: bounds.lowerBound.position..<bounds.upperBound.position)
  }

  public func _failEarlyRangeCheck(
    _ range: Range<MinimalIndex>,
    bounds: Range<MinimalIndex>
  ) {
    _expectCompatibleIndices(
      _uncheckedIndex(forPosition: range.lowerBound.position),
      range.lowerBound)
    _expectCompatibleIndices(
      _uncheckedIndex(forPosition: range.upperBound.position),
      range.upperBound)

    _expectCompatibleIndices(
      _uncheckedIndex(forPosition: bounds.lowerBound.position),
      bounds.lowerBound)
    _expectCompatibleIndices(
      _uncheckedIndex(forPosition: bounds.upperBound.position),
      bounds.upperBound)

    expectTrapping(
      range.lowerBound.position..<range.upperBound.position,
      in: bounds.lowerBound.position..<bounds.upperBound.position)
  }

  public func index(after i: MinimalIndex) -> MinimalIndex {
    _failEarlyRangeCheck(i, bounds: startIndex..<endIndex)
    return _uncheckedIndex(forPosition: i.position + 1)
  }

  public func index(before i: MinimalIndex) -> MinimalIndex {
    // FIXME: swift-3-indexing-model: perform a range check and use
    // return _uncheckedIndex(forPosition: i.position - 1)
    return _index(forPosition: i.position - 1)
  }

  public func distance(from start: MinimalIndex, to end: MinimalIndex)
    -> Int {
    // FIXME: swift-3-indexing-model: perform a range check properly.
    if start != endIndex {
      _failEarlyRangeCheck(start, bounds: startIndex..<endIndex)
    }
    if end != endIndex {
      _failEarlyRangeCheck(end, bounds: startIndex..<endIndex)
    }
    return end.position - start.position
  }

  public func index(_ i: Index, offsetBy n: Int) -> Index {
    // FIXME: swift-3-indexing-model: perform a range check properly.
    if i != endIndex {
      _failEarlyRangeCheck(i, bounds: startIndex..<endIndex)
    }
    return _index(forPosition: i.position + n)
  }

  public subscript(i: MinimalIndex) -> T {
    get {
      _failEarlyRangeCheck(i, bounds: startIndex..<endIndex)
      return _elements[i.position]
    }
    set {
      _failEarlyRangeCheck(i, bounds: startIndex..<endIndex)
      _elements[i.position] = newValue
    }
  }

  public subscript(bounds: Range<MinimalIndex>) -> Slice<MinimalMutableBidirectionalCollection<T>> {
    get {
      _failEarlyRangeCheck(bounds, bounds: startIndex..<endIndex)
      return Slice(base: self, bounds: bounds)
    }
    set {
      _failEarlyRangeCheck(bounds, bounds: startIndex..<endIndex)
      _writeBackMutableSlice(&self, bounds: bounds, slice: newValue)
    }
  }


  public var underestimatedCount: Int

  internal var _elements: [T]
  internal let _collectionState: _CollectionState
}


/// A minimal implementation of `Collection` with extra checks.
public struct MinimalMutableRangeReplaceableBidirectionalCollection<T> : BidirectionalCollection, MutableCollection, RangeReplaceableCollection {
  /// Creates a collection with given contents, but a unique modification
  /// history.  No other instance has the same modification history.
  public init<S : Sequence>(
    elements: S,
    underestimatedCount: UnderestimatedCountBehavior = .value(0)
  ) where S.Element == T {
    self._elements = Array(elements)

    self._collectionState = _CollectionState(
      newRootStateForElementCount: self._elements.count)

    switch underestimatedCount {
    case .precise:
      self.underestimatedCount = _elements.count

    case .half:
      self.underestimatedCount = _elements.count / 2

    case .overestimate:
      self.underestimatedCount = _elements.count * 3 + 5

    case .value(let count):
      self.underestimatedCount = count
    }
  }

  public init() {
    self.underestimatedCount = 0
    self._elements = []
    self._collectionState =
      _CollectionState(name: "\(type(of: self))", elementCount: 0)
  }

  public init<S : Sequence>(_ elements: S) where S.Element == T {
    self.underestimatedCount = 0
    self._elements = Array(elements)
    self._collectionState =
      _CollectionState(newRootStateForElementCount: self._elements.count)
  }

  public let timesMakeIteratorCalled = ResettableValue(0)

  public func makeIterator() -> MinimalIterator<T> {
    timesMakeIteratorCalled.value += 1
    return MinimalIterator(_elements)
  }

  public typealias Index = MinimalIndex

  internal func _index(forPosition i: Int) -> MinimalIndex {
    return MinimalIndex(
      collectionState: _collectionState,
      position: i,
      startIndex: _elements.startIndex,
      endIndex: _elements.endIndex)
  }

  internal func _uncheckedIndex(forPosition i: Int) -> MinimalIndex {
    return MinimalIndex(
      _collectionState: _collectionState,
      uncheckedPosition: i)
  }

  public let timesStartIndexCalled = ResettableValue(0)

  public var startIndex: MinimalIndex {
    timesStartIndexCalled.value += 1
    return _uncheckedIndex(forPosition: _elements.startIndex)
  }

  public let timesEndIndexCalled = ResettableValue(0)

  public var endIndex: MinimalIndex {
    timesEndIndexCalled.value += 1
    return _uncheckedIndex(forPosition: _elements.endIndex)
  }


  public func _failEarlyRangeCheck(
    _ index: MinimalIndex,
    bounds: Range<MinimalIndex>
  ) {
    _expectCompatibleIndices(
      _uncheckedIndex(forPosition: index.position),
      index)

    _expectCompatibleIndices(
      _uncheckedIndex(forPosition: bounds.lowerBound.position),
      bounds.lowerBound)
    _expectCompatibleIndices(
      _uncheckedIndex(forPosition: bounds.upperBound.position),
      bounds.upperBound)

    expectTrapping(
      index.position,
      in: bounds.lowerBound.position..<bounds.upperBound.position)
  }

  public func _failEarlyRangeCheck(
    _ range: Range<MinimalIndex>,
    bounds: Range<MinimalIndex>
  ) {
    _expectCompatibleIndices(
      _uncheckedIndex(forPosition: range.lowerBound.position),
      range.lowerBound)
    _expectCompatibleIndices(
      _uncheckedIndex(forPosition: range.upperBound.position),
      range.upperBound)

    _expectCompatibleIndices(
      _uncheckedIndex(forPosition: bounds.lowerBound.position),
      bounds.lowerBound)
    _expectCompatibleIndices(
      _uncheckedIndex(forPosition: bounds.upperBound.position),
      bounds.upperBound)

    expectTrapping(
      range.lowerBound.position..<range.upperBound.position,
      in: bounds.lowerBound.position..<bounds.upperBound.position)
  }

  public func index(after i: MinimalIndex) -> MinimalIndex {
    _failEarlyRangeCheck(i, bounds: startIndex..<endIndex)
    return _uncheckedIndex(forPosition: i.position + 1)
  }

  public func index(before i: MinimalIndex) -> MinimalIndex {
    // FIXME: swift-3-indexing-model: perform a range check and use
    // return _uncheckedIndex(forPosition: i.position - 1)
    return _index(forPosition: i.position - 1)
  }

  public func distance(from start: MinimalIndex, to end: MinimalIndex)
    -> Int {
    // FIXME: swift-3-indexing-model: perform a range check properly.
    if start != endIndex {
      _failEarlyRangeCheck(start, bounds: startIndex..<endIndex)
    }
    if end != endIndex {
      _failEarlyRangeCheck(end, bounds: startIndex..<endIndex)
    }
    return end.position - start.position
  }

  public func index(_ i: Index, offsetBy n: Int) -> Index {
    // FIXME: swift-3-indexing-model: perform a range check properly.
    if i != endIndex {
      _failEarlyRangeCheck(i, bounds: startIndex..<endIndex)
    }
    return _index(forPosition: i.position + n)
  }

  public subscript(i: MinimalIndex) -> T {
    get {
      _failEarlyRangeCheck(i, bounds: startIndex..<endIndex)
      return _elements[i.position]
    }
    set {
      _failEarlyRangeCheck(i, bounds: startIndex..<endIndex)
      _elements[i.position] = newValue
    }
  }

  public subscript(bounds: Range<MinimalIndex>) -> Slice<MinimalMutableRangeReplaceableBidirectionalCollection<T>> {
    get {
      _failEarlyRangeCheck(bounds, bounds: startIndex..<endIndex)
      return Slice(base: self, bounds: bounds)
    }
    set {
      _failEarlyRangeCheck(bounds, bounds: startIndex..<endIndex)
      _writeBackMutableSlice(&self, bounds: bounds, slice: newValue)
    }
  }

  public mutating func reserveCapacity(_ n: Int) {
    _willMutate(.reserveCapacity(capacity: n))
    _elements.reserveCapacity(n)
    reservedCapacity = Swift.max(reservedCapacity, n)
  }

  public mutating func append(_ x: T) {
    _willMutate(.append)
    _elements.append(x)
  }

  public mutating func append<S : Sequence>(contentsOf newElements: S)
    where S.Element == T {
    let oldCount = count
    _elements.append(contentsOf: newElements)
    let newCount = count
    _willMutate(.appendContentsOf(count: newCount - oldCount))
  }

  public mutating func replaceSubrange<C>(
    _ subRange: Range<MinimalIndex>,
    with newElements: C
  ) where C : Collection, C.Element == T {
    let oldCount = count
    _elements.replaceSubrange(
      subRange.lowerBound.position..<subRange.upperBound.position,
      with: newElements)
    let newCount = count
    _willMutate(.replaceRange(
      subRange: subRange.lowerBound.position..<subRange.upperBound.position,
      replacementCount:
        subRange.upperBound.position - subRange.lowerBound.position
        + newCount - oldCount))
  }

  public mutating func insert(_ newElement: T, at i: MinimalIndex) {
    _willMutate(.insert(atIndex: i.position))
    _elements.insert(newElement, at: i.position)
  }

  public mutating func insert<S : Collection>(
    contentsOf newElements: S, at i: MinimalIndex
  ) where S.Element == T {
    let oldCount = count
    _elements.insert(contentsOf: newElements, at: i.position)
    let newCount = count

    if newCount - oldCount != 0 {
      _willMutate(.insertContentsOf(
        atIndex: i.position,
        count: newCount - oldCount))
    }
  }

  @discardableResult
  public mutating func remove(at i: MinimalIndex) -> T {
    _willMutate(.removeAtIndex(index: i.position))
    return _elements.remove(at: i.position)
  }

  @discardableResult
  public mutating func removeLast() -> T {
    _willMutate(.removeLast)
    return _elements.removeLast()
  }

  public mutating func removeSubrange(_ subRange: Range<MinimalIndex>) {
    if !subRange.isEmpty {
      _willMutate(.removeRange(
        subRange: subRange.lowerBound.position..<subRange.upperBound.position))
    }
    _elements.removeSubrange(
      subRange.lowerBound.position..<subRange.upperBound.position
    )
  }

  public mutating func removeAll(keepingCapacity keepCapacity: Bool = false) {
    _willMutate(.removeAll(keepCapacity: keepCapacity))
    // Ignore the value of `keepCapacity`.
    _elements.removeAll(keepingCapacity: false)
  }

  internal mutating func _willMutate(_ operation: _CollectionOperation) {
    _collectionState = _CollectionStateTransition(
      previousState: _collectionState,
      operation: operation)._nextState
  }

  public var underestimatedCount: Int
  public var reservedCapacity: Int = 0

  internal var _elements: [T]
  internal var _collectionState: _CollectionState
}


/// A minimal implementation of `Collection` with extra checks.
public struct MinimalRandomAccessCollection<T> : RandomAccessCollection {
  /// Creates a collection with given contents, but a unique modification
  /// history.  No other instance has the same modification history.
  public init<S : Sequence>(
    elements: S,
    underestimatedCount: UnderestimatedCountBehavior = .value(0)
  ) where S.Element == T {
    self._elements = Array(elements)

    self._collectionState = _CollectionState(
      newRootStateForElementCount: self._elements.count)

    switch underestimatedCount {
    case .precise:
      self.underestimatedCount = _elements.count

    case .half:
      self.underestimatedCount = _elements.count / 2

    case .overestimate:
      self.underestimatedCount = _elements.count * 3 + 5

    case .value(let count):
      self.underestimatedCount = count
    }
  }


  public let timesMakeIteratorCalled = ResettableValue(0)

  public func makeIterator() -> MinimalIterator<T> {
    timesMakeIteratorCalled.value += 1
    return MinimalIterator(_elements)
  }

  public typealias Index = MinimalIndex

  internal func _index(forPosition i: Int) -> MinimalIndex {
    return MinimalIndex(
      collectionState: _collectionState,
      position: i,
      startIndex: _elements.startIndex,
      endIndex: _elements.endIndex)
  }

  internal func _uncheckedIndex(forPosition i: Int) -> MinimalIndex {
    return MinimalIndex(
      _collectionState: _collectionState,
      uncheckedPosition: i)
  }

  public let timesStartIndexCalled = ResettableValue(0)

  public var startIndex: MinimalIndex {
    timesStartIndexCalled.value += 1
    return _uncheckedIndex(forPosition: _elements.startIndex)
  }

  public let timesEndIndexCalled = ResettableValue(0)

  public var endIndex: MinimalIndex {
    timesEndIndexCalled.value += 1
    return _uncheckedIndex(forPosition: _elements.endIndex)
  }

  // FIXME: this shouldn't be necessary, should come by default
  public typealias Indices = DefaultIndices<MinimalRandomAccessCollection<T>>

  public func _failEarlyRangeCheck(
    _ index: MinimalIndex,
    bounds: Range<MinimalIndex>
  ) {
    _expectCompatibleIndices(
      _uncheckedIndex(forPosition: index.position),
      index)

    _expectCompatibleIndices(
      _uncheckedIndex(forPosition: bounds.lowerBound.position),
      bounds.lowerBound)
    _expectCompatibleIndices(
      _uncheckedIndex(forPosition: bounds.upperBound.position),
      bounds.upperBound)

    expectTrapping(
      index.position,
      in: bounds.lowerBound.position..<bounds.upperBound.position)
  }

  public func _failEarlyRangeCheck(
    _ range: Range<MinimalIndex>,
    bounds: Range<MinimalIndex>
  ) {
    _expectCompatibleIndices(
      _uncheckedIndex(forPosition: range.lowerBound.position),
      range.lowerBound)
    _expectCompatibleIndices(
      _uncheckedIndex(forPosition: range.upperBound.position),
      range.upperBound)

    _expectCompatibleIndices(
      _uncheckedIndex(forPosition: bounds.lowerBound.position),
      bounds.lowerBound)
    _expectCompatibleIndices(
      _uncheckedIndex(forPosition: bounds.upperBound.position),
      bounds.upperBound)

    expectTrapping(
      range.lowerBound.position..<range.upperBound.position,
      in: bounds.lowerBound.position..<bounds.upperBound.position)
  }

  public func index(after i: MinimalIndex) -> MinimalIndex {
    _failEarlyRangeCheck(i, bounds: startIndex..<endIndex)
    return _uncheckedIndex(forPosition: i.position + 1)
  }

  public func index(before i: MinimalIndex) -> MinimalIndex {
    // FIXME: swift-3-indexing-model: perform a range check and use
    // return _uncheckedIndex(forPosition: i.position - 1)
    return _index(forPosition: i.position - 1)
  }

  public func distance(from start: MinimalIndex, to end: MinimalIndex)
    -> Int {
    // FIXME: swift-3-indexing-model: perform a range check properly.
    if start != endIndex {
      _failEarlyRangeCheck(start, bounds: startIndex..<endIndex)
    }
    if end != endIndex {
      _failEarlyRangeCheck(end, bounds: startIndex..<endIndex)
    }
    return end.position - start.position
  }

  public func index(_ i: Index, offsetBy n: Int) -> Index {
    // FIXME: swift-3-indexing-model: perform a range check properly.
    if i != endIndex {
      _failEarlyRangeCheck(i, bounds: startIndex..<endIndex)
    }
    return _index(forPosition: i.position + n)
  }

  public subscript(i: MinimalIndex) -> T {
    get {
      _failEarlyRangeCheck(i, bounds: startIndex..<endIndex)
      return _elements[i.position]
    }
  }

  public subscript(bounds: Range<MinimalIndex>) -> Slice<MinimalRandomAccessCollection<T>> {
    get {
      _failEarlyRangeCheck(bounds, bounds: startIndex..<endIndex)
      return Slice(base: self, bounds: bounds)
    }
  }


  public var underestimatedCount: Int

  internal var _elements: [T]
  internal let _collectionState: _CollectionState
}


/// A minimal implementation of `Collection` with extra checks.
public struct MinimalRandomAccessCollectionWithStrideableIndex<T> : RandomAccessCollection {
  /// Creates a collection with given contents, but a unique modification
  /// history.  No other instance has the same modification history.
  public init<S : Sequence>(
    elements: S,
    underestimatedCount: UnderestimatedCountBehavior = .value(0)
  ) where S.Element == T {
    self._elements = Array(elements)

    self._collectionState = _CollectionState(
      newRootStateForElementCount: self._elements.count)

    switch underestimatedCount {
    case .precise:
      self.underestimatedCount = _elements.count

    case .half:
      self.underestimatedCount = _elements.count / 2

    case .overestimate:
      self.underestimatedCount = _elements.count * 3 + 5

    case .value(let count):
      self.underestimatedCount = count
    }
  }


  public let timesMakeIteratorCalled = ResettableValue(0)

  public func makeIterator() -> MinimalIterator<T> {
    timesMakeIteratorCalled.value += 1
    return MinimalIterator(_elements)
  }

  public typealias Index = MinimalStrideableIndex

  internal func _index(forPosition i: Int) -> MinimalStrideableIndex {
    return MinimalStrideableIndex(
      collectionState: _collectionState,
      position: i,
      startIndex: _elements.startIndex,
      endIndex: _elements.endIndex)
  }

  internal func _uncheckedIndex(forPosition i: Int) -> MinimalStrideableIndex {
    return MinimalStrideableIndex(
      _collectionState: _collectionState,
      uncheckedPosition: i)
  }

  public let timesStartIndexCalled = ResettableValue(0)

  public var startIndex: MinimalStrideableIndex {
    timesStartIndexCalled.value += 1
    return _uncheckedIndex(forPosition: _elements.startIndex)
  }

  public let timesEndIndexCalled = ResettableValue(0)

  public var endIndex: MinimalStrideableIndex {
    timesEndIndexCalled.value += 1
    return _uncheckedIndex(forPosition: _elements.endIndex)
  }

  public typealias Indices = Range<MinimalStrideableIndex>

  public func _failEarlyRangeCheck(
    _ index: MinimalStrideableIndex,
    bounds: Range<MinimalStrideableIndex>
  ) {
    _expectCompatibleIndices(
      _uncheckedIndex(forPosition: index.position),
      index)

    _expectCompatibleIndices(
      _uncheckedIndex(forPosition: bounds.lowerBound.position),
      bounds.lowerBound)
    _expectCompatibleIndices(
      _uncheckedIndex(forPosition: bounds.upperBound.position),
      bounds.upperBound)

    expectTrapping(
      index.position,
      in: bounds.lowerBound.position..<bounds.upperBound.position)
  }

  public func _failEarlyRangeCheck(
    _ range: Range<MinimalStrideableIndex>,
    bounds: Range<MinimalStrideableIndex>
  ) {
    _expectCompatibleIndices(
      _uncheckedIndex(forPosition: range.lowerBound.position),
      range.lowerBound)
    _expectCompatibleIndices(
      _uncheckedIndex(forPosition: range.upperBound.position),
      range.upperBound)

    _expectCompatibleIndices(
      _uncheckedIndex(forPosition: bounds.lowerBound.position),
      bounds.lowerBound)
    _expectCompatibleIndices(
      _uncheckedIndex(forPosition: bounds.upperBound.position),
      bounds.upperBound)

    expectTrapping(
      range.lowerBound.position..<range.upperBound.position,
      in: bounds.lowerBound.position..<bounds.upperBound.position)
  }

  public func index(after i: MinimalStrideableIndex) -> MinimalStrideableIndex {
    _failEarlyRangeCheck(i, bounds: startIndex..<endIndex)
    return _uncheckedIndex(forPosition: i.position + 1)
  }

  public func index(before i: MinimalStrideableIndex) -> MinimalStrideableIndex {
    // FIXME: swift-3-indexing-model: perform a range check and use
    // return _uncheckedIndex(forPosition: i.position - 1)
    return _index(forPosition: i.position - 1)
  }

  public func distance(from start: MinimalStrideableIndex, to end: MinimalStrideableIndex)
    -> Int {
    // FIXME: swift-3-indexing-model: perform a range check properly.
    if start != endIndex {
      _failEarlyRangeCheck(start, bounds: startIndex..<endIndex)
    }
    if end != endIndex {
      _failEarlyRangeCheck(end, bounds: startIndex..<endIndex)
    }
    return end.position - start.position
  }

  public func index(_ i: Index, offsetBy n: Int) -> Index {
    // FIXME: swift-3-indexing-model: perform a range check properly.
    if i != endIndex {
      _failEarlyRangeCheck(i, bounds: startIndex..<endIndex)
    }
    return _index(forPosition: i.position + n)
  }

  public subscript(i: MinimalStrideableIndex) -> T {
    get {
      _failEarlyRangeCheck(i, bounds: startIndex..<endIndex)
      return _elements[i.position]
    }
  }

  public subscript(bounds: Range<MinimalStrideableIndex>) -> Slice<MinimalRandomAccessCollectionWithStrideableIndex<T>> {
    get {
      _failEarlyRangeCheck(bounds, bounds: startIndex..<endIndex)
      return Slice(base: self, bounds: bounds)
    }
  }


  public var underestimatedCount: Int

  internal var _elements: [T]
  internal let _collectionState: _CollectionState
}


/// A minimal implementation of `Collection` with extra checks.
public struct MinimalRangeReplaceableRandomAccessCollection<T> : RandomAccessCollection, RangeReplaceableCollection {
  /// Creates a collection with given contents, but a unique modification
  /// history.  No other instance has the same modification history.
  public init<S : Sequence>(
    elements: S,
    underestimatedCount: UnderestimatedCountBehavior = .value(0)
  ) where S.Element == T {
    self._elements = Array(elements)

    self._collectionState = _CollectionState(
      newRootStateForElementCount: self._elements.count)

    switch underestimatedCount {
    case .precise:
      self.underestimatedCount = _elements.count

    case .half:
      self.underestimatedCount = _elements.count / 2

    case .overestimate:
      self.underestimatedCount = _elements.count * 3 + 5

    case .value(let count):
      self.underestimatedCount = count
    }
  }

  public init() {
    self.underestimatedCount = 0
    self._elements = []
    self._collectionState =
      _CollectionState(name: "\(type(of: self))", elementCount: 0)
  }

  public init<S : Sequence>(_ elements: S) where S.Element == T {
    self.underestimatedCount = 0
    self._elements = Array(elements)
    self._collectionState =
      _CollectionState(newRootStateForElementCount: self._elements.count)
  }

  public let timesMakeIteratorCalled = ResettableValue(0)

  public func makeIterator() -> MinimalIterator<T> {
    timesMakeIteratorCalled.value += 1
    return MinimalIterator(_elements)
  }

  public typealias Index = MinimalIndex

  internal func _index(forPosition i: Int) -> MinimalIndex {
    return MinimalIndex(
      collectionState: _collectionState,
      position: i,
      startIndex: _elements.startIndex,
      endIndex: _elements.endIndex)
  }

  internal func _uncheckedIndex(forPosition i: Int) -> MinimalIndex {
    return MinimalIndex(
      _collectionState: _collectionState,
      uncheckedPosition: i)
  }

  public let timesStartIndexCalled = ResettableValue(0)

  public var startIndex: MinimalIndex {
    timesStartIndexCalled.value += 1
    return _uncheckedIndex(forPosition: _elements.startIndex)
  }

  public let timesEndIndexCalled = ResettableValue(0)

  public var endIndex: MinimalIndex {
    timesEndIndexCalled.value += 1
    return _uncheckedIndex(forPosition: _elements.endIndex)
  }

  // FIXME: this shouldn't be necessary, should come by default
  public typealias Indices = DefaultIndices<MinimalRangeReplaceableRandomAccessCollection<T>>

  public func _failEarlyRangeCheck(
    _ index: MinimalIndex,
    bounds: Range<MinimalIndex>
  ) {
    _expectCompatibleIndices(
      _uncheckedIndex(forPosition: index.position),
      index)

    _expectCompatibleIndices(
      _uncheckedIndex(forPosition: bounds.lowerBound.position),
      bounds.lowerBound)
    _expectCompatibleIndices(
      _uncheckedIndex(forPosition: bounds.upperBound.position),
      bounds.upperBound)

    expectTrapping(
      index.position,
      in: bounds.lowerBound.position..<bounds.upperBound.position)
  }

  public func _failEarlyRangeCheck(
    _ range: Range<MinimalIndex>,
    bounds: Range<MinimalIndex>
  ) {
    _expectCompatibleIndices(
      _uncheckedIndex(forPosition: range.lowerBound.position),
      range.lowerBound)
    _expectCompatibleIndices(
      _uncheckedIndex(forPosition: range.upperBound.position),
      range.upperBound)

    _expectCompatibleIndices(
      _uncheckedIndex(forPosition: bounds.lowerBound.position),
      bounds.lowerBound)
    _expectCompatibleIndices(
      _uncheckedIndex(forPosition: bounds.upperBound.position),
      bounds.upperBound)

    expectTrapping(
      range.lowerBound.position..<range.upperBound.position,
      in: bounds.lowerBound.position..<bounds.upperBound.position)
  }

  public func index(after i: MinimalIndex) -> MinimalIndex {
    _failEarlyRangeCheck(i, bounds: startIndex..<endIndex)
    return _uncheckedIndex(forPosition: i.position + 1)
  }

  public func index(before i: MinimalIndex) -> MinimalIndex {
    // FIXME: swift-3-indexing-model: perform a range check and use
    // return _uncheckedIndex(forPosition: i.position - 1)
    return _index(forPosition: i.position - 1)
  }

  public func distance(from start: MinimalIndex, to end: MinimalIndex)
    -> Int {
    // FIXME: swift-3-indexing-model: perform a range check properly.
    if start != endIndex {
      _failEarlyRangeCheck(start, bounds: startIndex..<endIndex)
    }
    if end != endIndex {
      _failEarlyRangeCheck(end, bounds: startIndex..<endIndex)
    }
    return end.position - start.position
  }

  public func index(_ i: Index, offsetBy n: Int) -> Index {
    // FIXME: swift-3-indexing-model: perform a range check properly.
    if i != endIndex {
      _failEarlyRangeCheck(i, bounds: startIndex..<endIndex)
    }
    return _index(forPosition: i.position + n)
  }

  public subscript(i: MinimalIndex) -> T {
    get {
      _failEarlyRangeCheck(i, bounds: startIndex..<endIndex)
      return _elements[i.position]
    }
  }

  public subscript(bounds: Range<MinimalIndex>) -> Slice<MinimalRangeReplaceableRandomAccessCollection<T>> {
    get {
      _failEarlyRangeCheck(bounds, bounds: startIndex..<endIndex)
      return Slice(base: self, bounds: bounds)
    }
  }

  public mutating func reserveCapacity(_ n: Int) {
    _willMutate(.reserveCapacity(capacity: n))
    _elements.reserveCapacity(n)
    reservedCapacity = Swift.max(reservedCapacity, n)
  }

  public mutating func append(_ x: T) {
    _willMutate(.append)
    _elements.append(x)
  }

  public mutating func append<S : Sequence>(contentsOf newElements: S)
    where S.Element == T {
    let oldCount = count
    _elements.append(contentsOf: newElements)
    let newCount = count
    _willMutate(.appendContentsOf(count: newCount - oldCount))
  }

  public mutating func replaceSubrange<C>(
    _ subRange: Range<MinimalIndex>,
    with newElements: C
  ) where C : Collection, C.Element == T {
    let oldCount = count
    _elements.replaceSubrange(
      subRange.lowerBound.position..<subRange.upperBound.position,
      with: newElements)
    let newCount = count
    _willMutate(.replaceRange(
      subRange: subRange.lowerBound.position..<subRange.upperBound.position,
      replacementCount:
        subRange.upperBound.position - subRange.lowerBound.position
        + newCount - oldCount))
  }

  public mutating func insert(_ newElement: T, at i: MinimalIndex) {
    _willMutate(.insert(atIndex: i.position))
    _elements.insert(newElement, at: i.position)
  }

  public mutating func insert<S : Collection>(
    contentsOf newElements: S, at i: MinimalIndex
  ) where S.Element == T {
    let oldCount = count
    _elements.insert(contentsOf: newElements, at: i.position)
    let newCount = count

    if newCount - oldCount != 0 {
      _willMutate(.insertContentsOf(
        atIndex: i.position,
        count: newCount - oldCount))
    }
  }

  @discardableResult
  public mutating func remove(at i: MinimalIndex) -> T {
    _willMutate(.removeAtIndex(index: i.position))
    return _elements.remove(at: i.position)
  }

  @discardableResult
  public mutating func removeLast() -> T {
    _willMutate(.removeLast)
    return _elements.removeLast()
  }

  public mutating func removeSubrange(_ subRange: Range<MinimalIndex>) {
    if !subRange.isEmpty {
      _willMutate(.removeRange(
        subRange: subRange.lowerBound.position..<subRange.upperBound.position))
    }
    _elements.removeSubrange(
      subRange.lowerBound.position..<subRange.upperBound.position
    )
  }

  public mutating func removeAll(keepingCapacity keepCapacity: Bool = false) {
    _willMutate(.removeAll(keepCapacity: keepCapacity))
    // Ignore the value of `keepCapacity`.
    _elements.removeAll(keepingCapacity: false)
  }

  internal mutating func _willMutate(_ operation: _CollectionOperation) {
    _collectionState = _CollectionStateTransition(
      previousState: _collectionState,
      operation: operation)._nextState
  }

  public var underestimatedCount: Int
  public var reservedCapacity: Int = 0

  internal var _elements: [T]
  internal var _collectionState: _CollectionState
}


/// A minimal implementation of `Collection` with extra checks.
public struct MinimalRangeReplaceableRandomAccessCollectionWithStrideableIndex<T> : RandomAccessCollection, RangeReplaceableCollection {
  /// Creates a collection with given contents, but a unique modification
  /// history.  No other instance has the same modification history.
  public init<S : Sequence>(
    elements: S,
    underestimatedCount: UnderestimatedCountBehavior = .value(0)
  ) where S.Element == T {
    self._elements = Array(elements)

    self._collectionState = _CollectionState(
      newRootStateForElementCount: self._elements.count)

    switch underestimatedCount {
    case .precise:
      self.underestimatedCount = _elements.count

    case .half:
      self.underestimatedCount = _elements.count / 2

    case .overestimate:
      self.underestimatedCount = _elements.count * 3 + 5

    case .value(let count):
      self.underestimatedCount = count
    }
  }

  public init() {
    self.underestimatedCount = 0
    self._elements = []
    self._collectionState =
      _CollectionState(name: "\(type(of: self))", elementCount: 0)
  }

  public init<S : Sequence>(_ elements: S) where S.Element == T {
    self.underestimatedCount = 0
    self._elements = Array(elements)
    self._collectionState =
      _CollectionState(newRootStateForElementCount: self._elements.count)
  }

  public let timesMakeIteratorCalled = ResettableValue(0)

  public func makeIterator() -> MinimalIterator<T> {
    timesMakeIteratorCalled.value += 1
    return MinimalIterator(_elements)
  }

  public typealias Index = MinimalStrideableIndex

  internal func _index(forPosition i: Int) -> MinimalStrideableIndex {
    return MinimalStrideableIndex(
      collectionState: _collectionState,
      position: i,
      startIndex: _elements.startIndex,
      endIndex: _elements.endIndex)
  }

  internal func _uncheckedIndex(forPosition i: Int) -> MinimalStrideableIndex {
    return MinimalStrideableIndex(
      _collectionState: _collectionState,
      uncheckedPosition: i)
  }

  public let timesStartIndexCalled = ResettableValue(0)

  public var startIndex: MinimalStrideableIndex {
    timesStartIndexCalled.value += 1
    return _uncheckedIndex(forPosition: _elements.startIndex)
  }

  public let timesEndIndexCalled = ResettableValue(0)

  public var endIndex: MinimalStrideableIndex {
    timesEndIndexCalled.value += 1
    return _uncheckedIndex(forPosition: _elements.endIndex)
  }

  public typealias Indices = Range<MinimalStrideableIndex>

  public func _failEarlyRangeCheck(
    _ index: MinimalStrideableIndex,
    bounds: Range<MinimalStrideableIndex>
  ) {
    _expectCompatibleIndices(
      _uncheckedIndex(forPosition: index.position),
      index)

    _expectCompatibleIndices(
      _uncheckedIndex(forPosition: bounds.lowerBound.position),
      bounds.lowerBound)
    _expectCompatibleIndices(
      _uncheckedIndex(forPosition: bounds.upperBound.position),
      bounds.upperBound)

    expectTrapping(
      index.position,
      in: bounds.lowerBound.position..<bounds.upperBound.position)
  }

  public func _failEarlyRangeCheck(
    _ range: Range<MinimalStrideableIndex>,
    bounds: Range<MinimalStrideableIndex>
  ) {
    _expectCompatibleIndices(
      _uncheckedIndex(forPosition: range.lowerBound.position),
      range.lowerBound)
    _expectCompatibleIndices(
      _uncheckedIndex(forPosition: range.upperBound.position),
      range.upperBound)

    _expectCompatibleIndices(
      _uncheckedIndex(forPosition: bounds.lowerBound.position),
      bounds.lowerBound)
    _expectCompatibleIndices(
      _uncheckedIndex(forPosition: bounds.upperBound.position),
      bounds.upperBound)

    expectTrapping(
      range.lowerBound.position..<range.upperBound.position,
      in: bounds.lowerBound.position..<bounds.upperBound.position)
  }

  public func index(after i: MinimalStrideableIndex) -> MinimalStrideableIndex {
    _failEarlyRangeCheck(i, bounds: startIndex..<endIndex)
    return _uncheckedIndex(forPosition: i.position + 1)
  }

  public func index(before i: MinimalStrideableIndex) -> MinimalStrideableIndex {
    // FIXME: swift-3-indexing-model: perform a range check and use
    // return _uncheckedIndex(forPosition: i.position - 1)
    return _index(forPosition: i.position - 1)
  }

  public func distance(from start: MinimalStrideableIndex, to end: MinimalStrideableIndex)
    -> Int {
    // FIXME: swift-3-indexing-model: perform a range check properly.
    if start != endIndex {
      _failEarlyRangeCheck(start, bounds: startIndex..<endIndex)
    }
    if end != endIndex {
      _failEarlyRangeCheck(end, bounds: startIndex..<endIndex)
    }
    return end.position - start.position
  }

  public func index(_ i: Index, offsetBy n: Int) -> Index {
    // FIXME: swift-3-indexing-model: perform a range check properly.
    if i != endIndex {
      _failEarlyRangeCheck(i, bounds: startIndex..<endIndex)
    }
    return _index(forPosition: i.position + n)
  }

  public subscript(i: MinimalStrideableIndex) -> T {
    get {
      _failEarlyRangeCheck(i, bounds: startIndex..<endIndex)
      return _elements[i.position]
    }
  }

  public subscript(bounds: Range<MinimalStrideableIndex>) -> Slice<MinimalRangeReplaceableRandomAccessCollectionWithStrideableIndex<T>> {
    get {
      _failEarlyRangeCheck(bounds, bounds: startIndex..<endIndex)
      return Slice(base: self, bounds: bounds)
    }
  }

  public mutating func reserveCapacity(_ n: Int) {
    _willMutate(.reserveCapacity(capacity: n))
    _elements.reserveCapacity(n)
    reservedCapacity = Swift.max(reservedCapacity, n)
  }

  public mutating func append(_ x: T) {
    _willMutate(.append)
    _elements.append(x)
  }

  public mutating func append<S : Sequence>(contentsOf newElements: S)
    where S.Element == T {
    let oldCount = count
    _elements.append(contentsOf: newElements)
    let newCount = count
    _willMutate(.appendContentsOf(count: newCount - oldCount))
  }

  public mutating func replaceSubrange<C>(
    _ subRange: Range<MinimalStrideableIndex>,
    with newElements: C
  ) where C : Collection, C.Element == T {
    let oldCount = count
    _elements.replaceSubrange(
      subRange.lowerBound.position..<subRange.upperBound.position,
      with: newElements)
    let newCount = count
    _willMutate(.replaceRange(
      subRange: subRange.lowerBound.position..<subRange.upperBound.position,
      replacementCount:
        subRange.upperBound.position - subRange.lowerBound.position
        + newCount - oldCount))
  }

  public mutating func insert(_ newElement: T, at i: MinimalStrideableIndex) {
    _willMutate(.insert(atIndex: i.position))
    _elements.insert(newElement, at: i.position)
  }

  public mutating func insert<S : Collection>(
    contentsOf newElements: S, at i: MinimalStrideableIndex
  ) where S.Element == T {
    let oldCount = count
    _elements.insert(contentsOf: newElements, at: i.position)
    let newCount = count

    if newCount - oldCount != 0 {
      _willMutate(.insertContentsOf(
        atIndex: i.position,
        count: newCount - oldCount))
    }
  }

  @discardableResult
  public mutating func remove(at i: MinimalStrideableIndex) -> T {
    _willMutate(.removeAtIndex(index: i.position))
    return _elements.remove(at: i.position)
  }

  @discardableResult
  public mutating func removeLast() -> T {
    _willMutate(.removeLast)
    return _elements.removeLast()
  }

  public mutating func removeSubrange(_ subRange: Range<MinimalStrideableIndex>) {
    if !subRange.isEmpty {
      _willMutate(.removeRange(
        subRange: subRange.lowerBound.position..<subRange.upperBound.position))
    }
    _elements.removeSubrange(
      subRange.lowerBound.position..<subRange.upperBound.position
    )
  }

  public mutating func removeAll(keepingCapacity keepCapacity: Bool = false) {
    _willMutate(.removeAll(keepCapacity: keepCapacity))
    // Ignore the value of `keepCapacity`.
    _elements.removeAll(keepingCapacity: false)
  }

  internal mutating func _willMutate(_ operation: _CollectionOperation) {
    _collectionState = _CollectionStateTransition(
      previousState: _collectionState,
      operation: operation)._nextState
  }

  public var underestimatedCount: Int
  public var reservedCapacity: Int = 0

  internal var _elements: [T]
  internal var _collectionState: _CollectionState
}


/// A minimal implementation of `Collection` with extra checks.
public struct MinimalMutableRandomAccessCollection<T> : RandomAccessCollection, MutableCollection {
  /// Creates a collection with given contents, but a unique modification
  /// history.  No other instance has the same modification history.
  public init<S : Sequence>(
    elements: S,
    underestimatedCount: UnderestimatedCountBehavior = .value(0)
  ) where S.Element == T {
    self._elements = Array(elements)

    self._collectionState = _CollectionState(
      newRootStateForElementCount: self._elements.count)

    switch underestimatedCount {
    case .precise:
      self.underestimatedCount = _elements.count

    case .half:
      self.underestimatedCount = _elements.count / 2

    case .overestimate:
      self.underestimatedCount = _elements.count * 3 + 5

    case .value(let count):
      self.underestimatedCount = count
    }
  }


  public let timesMakeIteratorCalled = ResettableValue(0)

  public func makeIterator() -> MinimalIterator<T> {
    timesMakeIteratorCalled.value += 1
    return MinimalIterator(_elements)
  }

  public typealias Index = MinimalIndex

  internal func _index(forPosition i: Int) -> MinimalIndex {
    return MinimalIndex(
      collectionState: _collectionState,
      position: i,
      startIndex: _elements.startIndex,
      endIndex: _elements.endIndex)
  }

  internal func _uncheckedIndex(forPosition i: Int) -> MinimalIndex {
    return MinimalIndex(
      _collectionState: _collectionState,
      uncheckedPosition: i)
  }

  public let timesStartIndexCalled = ResettableValue(0)

  public var startIndex: MinimalIndex {
    timesStartIndexCalled.value += 1
    return _uncheckedIndex(forPosition: _elements.startIndex)
  }

  public let timesEndIndexCalled = ResettableValue(0)

  public var endIndex: MinimalIndex {
    timesEndIndexCalled.value += 1
    return _uncheckedIndex(forPosition: _elements.endIndex)
  }

  // FIXME: this shouldn't be necessary, should come by default
  public typealias Indices = DefaultIndices<MinimalMutableRandomAccessCollection<T>>

  public func _failEarlyRangeCheck(
    _ index: MinimalIndex,
    bounds: Range<MinimalIndex>
  ) {
    _expectCompatibleIndices(
      _uncheckedIndex(forPosition: index.position),
      index)

    _expectCompatibleIndices(
      _uncheckedIndex(forPosition: bounds.lowerBound.position),
      bounds.lowerBound)
    _expectCompatibleIndices(
      _uncheckedIndex(forPosition: bounds.upperBound.position),
      bounds.upperBound)

    expectTrapping(
      index.position,
      in: bounds.lowerBound.position..<bounds.upperBound.position)
  }

  public func _failEarlyRangeCheck(
    _ range: Range<MinimalIndex>,
    bounds: Range<MinimalIndex>
  ) {
    _expectCompatibleIndices(
      _uncheckedIndex(forPosition: range.lowerBound.position),
      range.lowerBound)
    _expectCompatibleIndices(
      _uncheckedIndex(forPosition: range.upperBound.position),
      range.upperBound)

    _expectCompatibleIndices(
      _uncheckedIndex(forPosition: bounds.lowerBound.position),
      bounds.lowerBound)
    _expectCompatibleIndices(
      _uncheckedIndex(forPosition: bounds.upperBound.position),
      bounds.upperBound)

    expectTrapping(
      range.lowerBound.position..<range.upperBound.position,
      in: bounds.lowerBound.position..<bounds.upperBound.position)
  }

  public func index(after i: MinimalIndex) -> MinimalIndex {
    _failEarlyRangeCheck(i, bounds: startIndex..<endIndex)
    return _uncheckedIndex(forPosition: i.position + 1)
  }

  public func index(before i: MinimalIndex) -> MinimalIndex {
    // FIXME: swift-3-indexing-model: perform a range check and use
    // return _uncheckedIndex(forPosition: i.position - 1)
    return _index(forPosition: i.position - 1)
  }

  public func distance(from start: MinimalIndex, to end: MinimalIndex)
    -> Int {
    // FIXME: swift-3-indexing-model: perform a range check properly.
    if start != endIndex {
      _failEarlyRangeCheck(start, bounds: startIndex..<endIndex)
    }
    if end != endIndex {
      _failEarlyRangeCheck(end, bounds: startIndex..<endIndex)
    }
    return end.position - start.position
  }

  public func index(_ i: Index, offsetBy n: Int) -> Index {
    // FIXME: swift-3-indexing-model: perform a range check properly.
    if i != endIndex {
      _failEarlyRangeCheck(i, bounds: startIndex..<endIndex)
    }
    return _index(forPosition: i.position + n)
  }

  public subscript(i: MinimalIndex) -> T {
    get {
      _failEarlyRangeCheck(i, bounds: startIndex..<endIndex)
      return _elements[i.position]
    }
    set {
      _failEarlyRangeCheck(i, bounds: startIndex..<endIndex)
      _elements[i.position] = newValue
    }
  }

  public subscript(bounds: Range<MinimalIndex>) -> Slice<MinimalMutableRandomAccessCollection<T>> {
    get {
      _failEarlyRangeCheck(bounds, bounds: startIndex..<endIndex)
      return Slice(base: self, bounds: bounds)
    }
    set {
      _failEarlyRangeCheck(bounds, bounds: startIndex..<endIndex)
      _writeBackMutableSlice(&self, bounds: bounds, slice: newValue)
    }
  }


  public var underestimatedCount: Int

  internal var _elements: [T]
  internal let _collectionState: _CollectionState
}


/// A minimal implementation of `Collection` with extra checks.
public struct MinimalMutableRangeReplaceableRandomAccessCollection<T> : RandomAccessCollection, MutableCollection, RangeReplaceableCollection {
  /// Creates a collection with given contents, but a unique modification
  /// history.  No other instance has the same modification history.
  public init<S : Sequence>(
    elements: S,
    underestimatedCount: UnderestimatedCountBehavior = .value(0)
  ) where S.Element == T {
    self._elements = Array(elements)

    self._collectionState = _CollectionState(
      newRootStateForElementCount: self._elements.count)

    switch underestimatedCount {
    case .precise:
      self.underestimatedCount = _elements.count

    case .half:
      self.underestimatedCount = _elements.count / 2

    case .overestimate:
      self.underestimatedCount = _elements.count * 3 + 5

    case .value(let count):
      self.underestimatedCount = count
    }
  }

  public init() {
    self.underestimatedCount = 0
    self._elements = []
    self._collectionState =
      _CollectionState(name: "\(type(of: self))", elementCount: 0)
  }

  public init<S : Sequence>(_ elements: S) where S.Element == T {
    self.underestimatedCount = 0
    self._elements = Array(elements)
    self._collectionState =
      _CollectionState(newRootStateForElementCount: self._elements.count)
  }

  public let timesMakeIteratorCalled = ResettableValue(0)

  public func makeIterator() -> MinimalIterator<T> {
    timesMakeIteratorCalled.value += 1
    return MinimalIterator(_elements)
  }

  public typealias Index = MinimalIndex

  internal func _index(forPosition i: Int) -> MinimalIndex {
    return MinimalIndex(
      collectionState: _collectionState,
      position: i,
      startIndex: _elements.startIndex,
      endIndex: _elements.endIndex)
  }

  internal func _uncheckedIndex(forPosition i: Int) -> MinimalIndex {
    return MinimalIndex(
      _collectionState: _collectionState,
      uncheckedPosition: i)
  }

  public let timesStartIndexCalled = ResettableValue(0)

  public var startIndex: MinimalIndex {
    timesStartIndexCalled.value += 1
    return _uncheckedIndex(forPosition: _elements.startIndex)
  }

  public let timesEndIndexCalled = ResettableValue(0)

  public var endIndex: MinimalIndex {
    timesEndIndexCalled.value += 1
    return _uncheckedIndex(forPosition: _elements.endIndex)
  }

  // FIXME: this shouldn't be necessary, should come by default
  public typealias Indices = DefaultIndices<MinimalMutableRangeReplaceableRandomAccessCollection<T>>

  public func _failEarlyRangeCheck(
    _ index: MinimalIndex,
    bounds: Range<MinimalIndex>
  ) {
    _expectCompatibleIndices(
      _uncheckedIndex(forPosition: index.position),
      index)

    _expectCompatibleIndices(
      _uncheckedIndex(forPosition: bounds.lowerBound.position),
      bounds.lowerBound)
    _expectCompatibleIndices(
      _uncheckedIndex(forPosition: bounds.upperBound.position),
      bounds.upperBound)

    expectTrapping(
      index.position,
      in: bounds.lowerBound.position..<bounds.upperBound.position)
  }

  public func _failEarlyRangeCheck(
    _ range: Range<MinimalIndex>,
    bounds: Range<MinimalIndex>
  ) {
    _expectCompatibleIndices(
      _uncheckedIndex(forPosition: range.lowerBound.position),
      range.lowerBound)
    _expectCompatibleIndices(
      _uncheckedIndex(forPosition: range.upperBound.position),
      range.upperBound)

    _expectCompatibleIndices(
      _uncheckedIndex(forPosition: bounds.lowerBound.position),
      bounds.lowerBound)
    _expectCompatibleIndices(
      _uncheckedIndex(forPosition: bounds.upperBound.position),
      bounds.upperBound)

    expectTrapping(
      range.lowerBound.position..<range.upperBound.position,
      in: bounds.lowerBound.position..<bounds.upperBound.position)
  }

  public func index(after i: MinimalIndex) -> MinimalIndex {
    _failEarlyRangeCheck(i, bounds: startIndex..<endIndex)
    return _uncheckedIndex(forPosition: i.position + 1)
  }

  public func index(before i: MinimalIndex) -> MinimalIndex {
    // FIXME: swift-3-indexing-model: perform a range check and use
    // return _uncheckedIndex(forPosition: i.position - 1)
    return _index(forPosition: i.position - 1)
  }

  public func distance(from start: MinimalIndex, to end: MinimalIndex)
    -> Int {
    // FIXME: swift-3-indexing-model: perform a range check properly.
    if start != endIndex {
      _failEarlyRangeCheck(start, bounds: startIndex..<endIndex)
    }
    if end != endIndex {
      _failEarlyRangeCheck(end, bounds: startIndex..<endIndex)
    }
    return end.position - start.position
  }

  public func index(_ i: Index, offsetBy n: Int) -> Index {
    // FIXME: swift-3-indexing-model: perform a range check properly.
    if i != endIndex {
      _failEarlyRangeCheck(i, bounds: startIndex..<endIndex)
    }
    return _index(forPosition: i.position + n)
  }

  public subscript(i: MinimalIndex) -> T {
    get {
      _failEarlyRangeCheck(i, bounds: startIndex..<endIndex)
      return _elements[i.position]
    }
    set {
      _failEarlyRangeCheck(i, bounds: startIndex..<endIndex)
      _elements[i.position] = newValue
    }
  }

  public subscript(bounds: Range<MinimalIndex>) -> Slice<MinimalMutableRangeReplaceableRandomAccessCollection<T>> {
    get {
      _failEarlyRangeCheck(bounds, bounds: startIndex..<endIndex)
      return Slice(base: self, bounds: bounds)
    }
    set {
      _failEarlyRangeCheck(bounds, bounds: startIndex..<endIndex)
      _writeBackMutableSlice(&self, bounds: bounds, slice: newValue)
    }
  }

  public mutating func reserveCapacity(_ n: Int) {
    _willMutate(.reserveCapacity(capacity: n))
    _elements.reserveCapacity(n)
    reservedCapacity = Swift.max(reservedCapacity, n)
  }

  public mutating func append(_ x: T) {
    _willMutate(.append)
    _elements.append(x)
  }

  public mutating func append<S : Sequence>(contentsOf newElements: S)
    where S.Element == T {
    let oldCount = count
    _elements.append(contentsOf: newElements)
    let newCount = count
    _willMutate(.appendContentsOf(count: newCount - oldCount))
  }

  public mutating func replaceSubrange<C>(
    _ subRange: Range<MinimalIndex>,
    with newElements: C
  ) where C : Collection, C.Element == T {
    let oldCount = count
    _elements.replaceSubrange(
      subRange.lowerBound.position..<subRange.upperBound.position,
      with: newElements)
    let newCount = count
    _willMutate(.replaceRange(
      subRange: subRange.lowerBound.position..<subRange.upperBound.position,
      replacementCount:
        subRange.upperBound.position - subRange.lowerBound.position
        + newCount - oldCount))
  }

  public mutating func insert(_ newElement: T, at i: MinimalIndex) {
    _willMutate(.insert(atIndex: i.position))
    _elements.insert(newElement, at: i.position)
  }

  public mutating func insert<S : Collection>(
    contentsOf newElements: S, at i: MinimalIndex
  ) where S.Element == T {
    let oldCount = count
    _elements.insert(contentsOf: newElements, at: i.position)
    let newCount = count

    if newCount - oldCount != 0 {
      _willMutate(.insertContentsOf(
        atIndex: i.position,
        count: newCount - oldCount))
    }
  }

  @discardableResult
  public mutating func remove(at i: MinimalIndex) -> T {
    _willMutate(.removeAtIndex(index: i.position))
    return _elements.remove(at: i.position)
  }

  @discardableResult
  public mutating func removeLast() -> T {
    _willMutate(.removeLast)
    return _elements.removeLast()
  }

  public mutating func removeSubrange(_ subRange: Range<MinimalIndex>) {
    if !subRange.isEmpty {
      _willMutate(.removeRange(
        subRange: subRange.lowerBound.position..<subRange.upperBound.position))
    }
    _elements.removeSubrange(
      subRange.lowerBound.position..<subRange.upperBound.position
    )
  }

  public mutating func removeAll(keepingCapacity keepCapacity: Bool = false) {
    _willMutate(.removeAll(keepCapacity: keepCapacity))
    // Ignore the value of `keepCapacity`.
    _elements.removeAll(keepingCapacity: false)
  }

  internal mutating func _willMutate(_ operation: _CollectionOperation) {
    _collectionState = _CollectionStateTransition(
      previousState: _collectionState,
      operation: operation)._nextState
  }

  public var underestimatedCount: Int
  public var reservedCapacity: Int = 0

  internal var _elements: [T]
  internal var _collectionState: _CollectionState
}


/// A Sequence that uses as many default implementations as
/// `Sequence` can provide.
public struct DefaultedSequence<Element> : Sequence {
  public let base: MinimalSequence<Element>

  public init(base: MinimalSequence<Element>) {
    self.base = base
  }

  public init<S : Sequence>(
    elements: S,
    underestimatedCount: UnderestimatedCountBehavior = .value(0)
  ) where S.Element == Element {
    self.init(base: MinimalSequence(
      elements: elements, underestimatedCount: underestimatedCount))
  }

  public func makeIterator() -> MinimalIterator<Element> {
    return base.makeIterator()
  }

  public var underestimatedCount: Int {
    return base.underestimatedCount
  }
}


/// A Collection that uses as many default implementations as
/// `Collection` can provide.
public struct DefaultedCollection<Element> : Collection {
  public typealias Base = MinimalCollection<Element>
  public typealias Iterator = MinimalIterator<Element>
  public typealias Index = MinimalIndex


  public let base: Base

  public init(base: Base) {
    self.base = base
  }

  public init(_ array: [Element]) {
    self.base = Base(elements: array)
  }

  public init(elements: [Element]) {
    self.base = Base(elements: elements)
  }

  public init<S : Sequence>(
    elements: S,
    underestimatedCount: UnderestimatedCountBehavior = .value(0)
  ) where S.Element == Element {
    self.init(base:
      MinimalCollection(elements: elements, underestimatedCount: underestimatedCount))
  }

  public var underestimatedCount: Int {
    return base.underestimatedCount
  }

  public let timesMakeIteratorCalled = ResettableValue(0)

  public func makeIterator() -> MinimalIterator<Element> {
    timesMakeIteratorCalled.value += 1
    return base.makeIterator()
  }


  public let timesSuccessorCalled = ResettableValue(0)

  public func index(after i: MinimalIndex) -> MinimalIndex {
    timesSuccessorCalled.value += 1
    return base.index(after: i)
  }




  public let timesStartIndexCalled = ResettableValue(0)

  public var startIndex: MinimalIndex {
    timesStartIndexCalled.value += 1
    return base.startIndex
  }

  public let timesEndIndexCalled = ResettableValue(0)

  public var endIndex: MinimalIndex {
    timesEndIndexCalled.value += 1
    return base.endIndex
  }

  public subscript(i: MinimalIndex) -> Element {
    get {
      return base[i]
    }
  }

    // FIXME: swift-3-indexing-model: use defaults.
//     if Self not in ['DefaultedCollection', 'DefaultedBidirectionalCollection', 'DefaultedRandomAccessCollection', 'DefaultedMutableCollection', 'DefaultedRangeReplaceableCollection']:

  public subscript(bounds: Range<MinimalIndex>) -> Slice<DefaultedCollection<Base.Element>> {
    get {
      // FIXME: swift-3-indexing-model: range check.
      return Slice(base: self, bounds: bounds)
    }
  }

}

/*
FIXME: swift-3-indexing-model: uncomment this.
public struct DefaultedForwardRangeReplaceableSlice<Element>
  : RangeReplaceableCollection {

  public typealias Self_ = DefaultedForwardRangeReplaceableSlice<Element>
  public typealias Base = MinimalCollection<Element>
  public typealias Iterator = MinimalIterator<Element>
  public typealias Index = MinimalIndex

  public var base: Base
  public var startIndex: Index
  public var endIndex: Index

  public init() {
    expectSliceType(Self_.self)

    self.base = Base()
    self.startIndex = base.startIndex
    self.endIndex = base.endIndex
  }

  public init(base: Base) {
    self.base = base
    self.startIndex = base.startIndex
    self.endIndex = base.endIndex
  }

  public init(base: Base, bounds: Range<Index>) {
    self.base = base
    self.startIndex = bounds.lowerBound
    self.endIndex = bounds.upperBound
  }

  public init(_ array: [Element]) {
    self = DefaultedForwardRangeReplaceableSlice(
      base: Base(elements: array))
  }

  public init(elements: [Element]) {
    self = DefaultedForwardRangeReplaceableSlice(
      base: Base(elements: elements))
  }

  public func makeIterator() -> MinimalIterator<Element> {
    return MinimalIterator(Array(self))
  }

  public subscript(index: Index) -> Element {
    Index._failEarlyRangeCheck(index, bounds: startIndex..<endIndex)
    return base[index]
  }

  public subscript(bounds: Range<Index>) -> Self_ {
    Index._failEarlyRangeCheck2(
      rangeStart: bounds.lowerBound,
      rangeEnd: bounds.upperBound,
      boundsStart: startIndex,
      boundsEnd: endIndex)
    return DefaultedForwardRangeReplaceableSlice(
      base: base, bounds: bounds)
  }

  public mutating func replaceSubrange<
    C : Collection
  >(
    _ subRange: Range<Index>,
    with newElements: C
  ) where C.Element == Element {
    let startOffset = startIndex.position
    let endOffset =
      endIndex.position
      - subRange.count
      + numericCast(newElements.count) as Int
    Index._failEarlyRangeCheck2(
      rangeStart: subRange.lowerBound,
      rangeEnd: subRange.upperBound,
      boundsStart: startIndex,
      boundsEnd: endIndex)
    base.replaceSubrange(subRange, with: newElements)
    startIndex = base.startIndex.advanced(by: startOffset)
    endIndex = base.startIndex.advanced(by: endOffset)
  }
}
*/


/// A Collection that uses as many default implementations as
/// `Collection` can provide.
public struct DefaultedRangeReplaceableCollection<Element> : Collection, RangeReplaceableCollection {
  public typealias Base = MinimalRangeReplaceableCollection<Element>
  public typealias Iterator = MinimalIterator<Element>
  public typealias Index = MinimalIndex


  public var base: Base

  public init(base: Base) {
    self.base = base
  }

  public init(_ array: [Element]) {
    self.base = Base(elements: array)
  }

  public init(elements: [Element]) {
    self.base = Base(elements: elements)
  }

  public init<S : Sequence>(
    elements: S,
    underestimatedCount: UnderestimatedCountBehavior = .value(0)
  ) where S.Element == Element {
    self.init(base:
      MinimalRangeReplaceableCollection(elements: elements, underestimatedCount: underestimatedCount))
  }

  public var underestimatedCount: Int {
    return base.underestimatedCount
  }

  public let timesMakeIteratorCalled = ResettableValue(0)

  public func makeIterator() -> MinimalIterator<Element> {
    timesMakeIteratorCalled.value += 1
    return base.makeIterator()
  }


  public let timesSuccessorCalled = ResettableValue(0)

  public func index(after i: MinimalIndex) -> MinimalIndex {
    timesSuccessorCalled.value += 1
    return base.index(after: i)
  }




  public let timesStartIndexCalled = ResettableValue(0)

  public var startIndex: MinimalIndex {
    timesStartIndexCalled.value += 1
    return base.startIndex
  }

  public let timesEndIndexCalled = ResettableValue(0)

  public var endIndex: MinimalIndex {
    timesEndIndexCalled.value += 1
    return base.endIndex
  }

  public subscript(i: MinimalIndex) -> Element {
    get {
      return base[i]
    }
  }

    // FIXME: swift-3-indexing-model: use defaults.
//     if Self not in ['DefaultedCollection', 'DefaultedBidirectionalCollection', 'DefaultedRandomAccessCollection', 'DefaultedMutableCollection', 'DefaultedRangeReplaceableCollection']:

  public subscript(bounds: Range<MinimalIndex>) -> Slice<DefaultedRangeReplaceableCollection<Base.Element>> {
    get {
      // FIXME: swift-3-indexing-model: range check.
      return Slice(base: self, bounds: bounds)
    }
  }

  public init() {
    base = Base()
  }

  public mutating func replaceSubrange<C>(
    _ subRange: Range<DefaultedRangeReplaceableCollection<Element>.Index>,
    with newElements: C
  ) where C : Collection, C.Element == Element {
    base.replaceSubrange(subRange, with: newElements)
  }
}

/*
FIXME: swift-3-indexing-model: uncomment this.
public struct DefaultedForwardRangeReplaceableSlice<Element>
  : RangeReplaceableCollection {

  public typealias Self_ = DefaultedForwardRangeReplaceableSlice<Element>
  public typealias Base = MinimalRangeReplaceableCollection<Element>
  public typealias Iterator = MinimalIterator<Element>
  public typealias Index = MinimalIndex

  public var base: Base
  public var startIndex: Index
  public var endIndex: Index

  public init() {
    expectSliceType(Self_.self)

    self.base = Base()
    self.startIndex = base.startIndex
    self.endIndex = base.endIndex
  }

  public init(base: Base) {
    self.base = base
    self.startIndex = base.startIndex
    self.endIndex = base.endIndex
  }

  public init(base: Base, bounds: Range<Index>) {
    self.base = base
    self.startIndex = bounds.lowerBound
    self.endIndex = bounds.upperBound
  }

  public init(_ array: [Element]) {
    self = DefaultedForwardRangeReplaceableSlice(
      base: Base(elements: array))
  }

  public init(elements: [Element]) {
    self = DefaultedForwardRangeReplaceableSlice(
      base: Base(elements: elements))
  }

  public func makeIterator() -> MinimalIterator<Element> {
    return MinimalIterator(Array(self))
  }

  public subscript(index: Index) -> Element {
    Index._failEarlyRangeCheck(index, bounds: startIndex..<endIndex)
    return base[index]
  }

  public subscript(bounds: Range<Index>) -> Self_ {
    Index._failEarlyRangeCheck2(
      rangeStart: bounds.lowerBound,
      rangeEnd: bounds.upperBound,
      boundsStart: startIndex,
      boundsEnd: endIndex)
    return DefaultedForwardRangeReplaceableSlice(
      base: base, bounds: bounds)
  }

  public mutating func replaceSubrange<
    C : Collection
  >(
    _ subRange: Range<Index>,
    with newElements: C
  ) where C.Element == Element {
    let startOffset = startIndex.position
    let endOffset =
      endIndex.position
      - subRange.count
      + numericCast(newElements.count) as Int
    Index._failEarlyRangeCheck2(
      rangeStart: subRange.lowerBound,
      rangeEnd: subRange.upperBound,
      boundsStart: startIndex,
      boundsEnd: endIndex)
    base.replaceSubrange(subRange, with: newElements)
    startIndex = base.startIndex.advanced(by: startOffset)
    endIndex = base.startIndex.advanced(by: endOffset)
  }
}
*/


/// A Collection that uses as many default implementations as
/// `Collection` can provide.
public struct DefaultedMutableCollection<Element> : Collection, MutableCollection {
  public typealias Base = MinimalMutableCollection<Element>
  public typealias Iterator = MinimalIterator<Element>
  public typealias Index = MinimalIndex


  public var base: Base

  public init(base: Base) {
    self.base = base
  }

  public init(_ array: [Element]) {
    self.base = Base(elements: array)
  }

  public init(elements: [Element]) {
    self.base = Base(elements: elements)
  }

  public init<S : Sequence>(
    elements: S,
    underestimatedCount: UnderestimatedCountBehavior = .value(0)
  ) where S.Element == Element {
    self.init(base:
      MinimalMutableCollection(elements: elements, underestimatedCount: underestimatedCount))
  }

  public var underestimatedCount: Int {
    return base.underestimatedCount
  }

  public let timesMakeIteratorCalled = ResettableValue(0)

  public func makeIterator() -> MinimalIterator<Element> {
    timesMakeIteratorCalled.value += 1
    return base.makeIterator()
  }


  public let timesSuccessorCalled = ResettableValue(0)

  public func index(after i: MinimalIndex) -> MinimalIndex {
    timesSuccessorCalled.value += 1
    return base.index(after: i)
  }




  public let timesStartIndexCalled = ResettableValue(0)

  public var startIndex: MinimalIndex {
    timesStartIndexCalled.value += 1
    return base.startIndex
  }

  public let timesEndIndexCalled = ResettableValue(0)

  public var endIndex: MinimalIndex {
    timesEndIndexCalled.value += 1
    return base.endIndex
  }

  public subscript(i: MinimalIndex) -> Element {
    get {
      return base[i]
    }
    set {
      base[i] = newValue
    }
  }

    // FIXME: swift-3-indexing-model: use defaults.
//     if Self not in ['DefaultedCollection', 'DefaultedBidirectionalCollection', 'DefaultedRandomAccessCollection', 'DefaultedMutableCollection', 'DefaultedRangeReplaceableCollection']:

  public subscript(bounds: Range<MinimalIndex>) -> Slice<DefaultedMutableCollection<Base.Element>> {
    get {
      // FIXME: swift-3-indexing-model: range check.
      return Slice(base: self, bounds: bounds)
    }
    set {
      _writeBackMutableSlice(&self, bounds: bounds, slice: newValue)
    }
  }

}

/*
FIXME: swift-3-indexing-model: uncomment this.
public struct DefaultedForwardRangeReplaceableSlice<Element>
  : RangeReplaceableCollection {

  public typealias Self_ = DefaultedForwardRangeReplaceableSlice<Element>
  public typealias Base = MinimalMutableCollection<Element>
  public typealias Iterator = MinimalIterator<Element>
  public typealias Index = MinimalIndex

  public var base: Base
  public var startIndex: Index
  public var endIndex: Index

  public init() {
    expectSliceType(Self_.self)

    self.base = Base()
    self.startIndex = base.startIndex
    self.endIndex = base.endIndex
  }

  public init(base: Base) {
    self.base = base
    self.startIndex = base.startIndex
    self.endIndex = base.endIndex
  }

  public init(base: Base, bounds: Range<Index>) {
    self.base = base
    self.startIndex = bounds.lowerBound
    self.endIndex = bounds.upperBound
  }

  public init(_ array: [Element]) {
    self = DefaultedForwardRangeReplaceableSlice(
      base: Base(elements: array))
  }

  public init(elements: [Element]) {
    self = DefaultedForwardRangeReplaceableSlice(
      base: Base(elements: elements))
  }

  public func makeIterator() -> MinimalIterator<Element> {
    return MinimalIterator(Array(self))
  }

  public subscript(index: Index) -> Element {
    Index._failEarlyRangeCheck(index, bounds: startIndex..<endIndex)
    return base[index]
  }

  public subscript(bounds: Range<Index>) -> Self_ {
    Index._failEarlyRangeCheck2(
      rangeStart: bounds.lowerBound,
      rangeEnd: bounds.upperBound,
      boundsStart: startIndex,
      boundsEnd: endIndex)
    return DefaultedForwardRangeReplaceableSlice(
      base: base, bounds: bounds)
  }

  public mutating func replaceSubrange<
    C : Collection
  >(
    _ subRange: Range<Index>,
    with newElements: C
  ) where C.Element == Element {
    let startOffset = startIndex.position
    let endOffset =
      endIndex.position
      - subRange.count
      + numericCast(newElements.count) as Int
    Index._failEarlyRangeCheck2(
      rangeStart: subRange.lowerBound,
      rangeEnd: subRange.upperBound,
      boundsStart: startIndex,
      boundsEnd: endIndex)
    base.replaceSubrange(subRange, with: newElements)
    startIndex = base.startIndex.advanced(by: startOffset)
    endIndex = base.startIndex.advanced(by: endOffset)
  }
}
*/


/// A Collection that uses as many default implementations as
/// `Collection` can provide.
public struct DefaultedMutableRangeReplaceableCollection<Element> : Collection, MutableCollection, RangeReplaceableCollection {
  public typealias Base = MinimalMutableRangeReplaceableCollection<Element>
  public typealias Iterator = MinimalIterator<Element>
  public typealias Index = MinimalIndex


  public var base: Base

  public init(base: Base) {
    self.base = base
  }

  public init(_ array: [Element]) {
    self.base = Base(elements: array)
  }

  public init(elements: [Element]) {
    self.base = Base(elements: elements)
  }

  public init<S : Sequence>(
    elements: S,
    underestimatedCount: UnderestimatedCountBehavior = .value(0)
  ) where S.Element == Element {
    self.init(base:
      MinimalMutableRangeReplaceableCollection(elements: elements, underestimatedCount: underestimatedCount))
  }

  public var underestimatedCount: Int {
    return base.underestimatedCount
  }

  public let timesMakeIteratorCalled = ResettableValue(0)

  public func makeIterator() -> MinimalIterator<Element> {
    timesMakeIteratorCalled.value += 1
    return base.makeIterator()
  }


  public let timesSuccessorCalled = ResettableValue(0)

  public func index(after i: MinimalIndex) -> MinimalIndex {
    timesSuccessorCalled.value += 1
    return base.index(after: i)
  }




  public let timesStartIndexCalled = ResettableValue(0)

  public var startIndex: MinimalIndex {
    timesStartIndexCalled.value += 1
    return base.startIndex
  }

  public let timesEndIndexCalled = ResettableValue(0)

  public var endIndex: MinimalIndex {
    timesEndIndexCalled.value += 1
    return base.endIndex
  }

  public subscript(i: MinimalIndex) -> Element {
    get {
      return base[i]
    }
    set {
      base[i] = newValue
    }
  }

    // FIXME: swift-3-indexing-model: use defaults.
//     if Self not in ['DefaultedCollection', 'DefaultedBidirectionalCollection', 'DefaultedRandomAccessCollection', 'DefaultedMutableCollection', 'DefaultedRangeReplaceableCollection']:

  public subscript(bounds: Range<MinimalIndex>) -> Slice<DefaultedMutableRangeReplaceableCollection<Base.Element>> {
    get {
      // FIXME: swift-3-indexing-model: range check.
      return Slice(base: self, bounds: bounds)
    }
    set {
      _writeBackMutableSlice(&self, bounds: bounds, slice: newValue)
    }
  }

  public init() {
    base = Base()
  }

  public mutating func replaceSubrange<C>(
    _ subRange: Range<DefaultedMutableRangeReplaceableCollection<Element>.Index>,
    with newElements: C
  ) where C : Collection, C.Element == Element {
    base.replaceSubrange(subRange, with: newElements)
  }
}

/*
FIXME: swift-3-indexing-model: uncomment this.
public struct DefaultedForwardRangeReplaceableSlice<Element>
  : RangeReplaceableCollection {

  public typealias Self_ = DefaultedForwardRangeReplaceableSlice<Element>
  public typealias Base = MinimalMutableRangeReplaceableCollection<Element>
  public typealias Iterator = MinimalIterator<Element>
  public typealias Index = MinimalIndex

  public var base: Base
  public var startIndex: Index
  public var endIndex: Index

  public init() {
    expectSliceType(Self_.self)

    self.base = Base()
    self.startIndex = base.startIndex
    self.endIndex = base.endIndex
  }

  public init(base: Base) {
    self.base = base
    self.startIndex = base.startIndex
    self.endIndex = base.endIndex
  }

  public init(base: Base, bounds: Range<Index>) {
    self.base = base
    self.startIndex = bounds.lowerBound
    self.endIndex = bounds.upperBound
  }

  public init(_ array: [Element]) {
    self = DefaultedForwardRangeReplaceableSlice(
      base: Base(elements: array))
  }

  public init(elements: [Element]) {
    self = DefaultedForwardRangeReplaceableSlice(
      base: Base(elements: elements))
  }

  public func makeIterator() -> MinimalIterator<Element> {
    return MinimalIterator(Array(self))
  }

  public subscript(index: Index) -> Element {
    Index._failEarlyRangeCheck(index, bounds: startIndex..<endIndex)
    return base[index]
  }

  public subscript(bounds: Range<Index>) -> Self_ {
    Index._failEarlyRangeCheck2(
      rangeStart: bounds.lowerBound,
      rangeEnd: bounds.upperBound,
      boundsStart: startIndex,
      boundsEnd: endIndex)
    return DefaultedForwardRangeReplaceableSlice(
      base: base, bounds: bounds)
  }

  public mutating func replaceSubrange<
    C : Collection
  >(
    _ subRange: Range<Index>,
    with newElements: C
  ) where C.Element == Element {
    let startOffset = startIndex.position
    let endOffset =
      endIndex.position
      - subRange.count
      + numericCast(newElements.count) as Int
    Index._failEarlyRangeCheck2(
      rangeStart: subRange.lowerBound,
      rangeEnd: subRange.upperBound,
      boundsStart: startIndex,
      boundsEnd: endIndex)
    base.replaceSubrange(subRange, with: newElements)
    startIndex = base.startIndex.advanced(by: startOffset)
    endIndex = base.startIndex.advanced(by: endOffset)
  }
}
*/


/// A Collection that uses as many default implementations as
/// `Collection` can provide.
public struct DefaultedBidirectionalCollection<Element> : BidirectionalCollection {
  public typealias Base = MinimalBidirectionalCollection<Element>
  public typealias Iterator = MinimalIterator<Element>
  public typealias Index = MinimalIndex


  public let base: Base

  public init(base: Base) {
    self.base = base
  }

  public init(_ array: [Element]) {
    self.base = Base(elements: array)
  }

  public init(elements: [Element]) {
    self.base = Base(elements: elements)
  }

  public init<S : Sequence>(
    elements: S,
    underestimatedCount: UnderestimatedCountBehavior = .value(0)
  ) where S.Element == Element {
    self.init(base:
      MinimalBidirectionalCollection(elements: elements, underestimatedCount: underestimatedCount))
  }

  public var underestimatedCount: Int {
    return base.underestimatedCount
  }

  public let timesMakeIteratorCalled = ResettableValue(0)

  public func makeIterator() -> MinimalIterator<Element> {
    timesMakeIteratorCalled.value += 1
    return base.makeIterator()
  }


  public let timesSuccessorCalled = ResettableValue(0)

  public func index(after i: MinimalIndex) -> MinimalIndex {
    timesSuccessorCalled.value += 1
    return base.index(after: i)
  }

  public let timesPredecessorCalled = ResettableValue(0)

  public func index(before i: MinimalIndex) -> MinimalIndex {
    timesPredecessorCalled.value += 1
    return base.index(before: i)
  }



  public let timesStartIndexCalled = ResettableValue(0)

  public var startIndex: MinimalIndex {
    timesStartIndexCalled.value += 1
    return base.startIndex
  }

  public let timesEndIndexCalled = ResettableValue(0)

  public var endIndex: MinimalIndex {
    timesEndIndexCalled.value += 1
    return base.endIndex
  }

  public subscript(i: MinimalIndex) -> Element {
    get {
      return base[i]
    }
  }

    // FIXME: swift-3-indexing-model: use defaults.
//     if Self not in ['DefaultedCollection', 'DefaultedBidirectionalCollection', 'DefaultedRandomAccessCollection', 'DefaultedMutableCollection', 'DefaultedRangeReplaceableCollection']:

  public subscript(bounds: Range<MinimalIndex>) -> Slice<DefaultedBidirectionalCollection<Base.Element>> {
    get {
      // FIXME: swift-3-indexing-model: range check.
      return Slice(base: self, bounds: bounds)
    }
  }

}

/*
FIXME: swift-3-indexing-model: uncomment this.
public struct DefaultedBidirectionalRangeReplaceableSlice<Element>
  : RangeReplaceableCollection {

  public typealias Self_ = DefaultedBidirectionalRangeReplaceableSlice<Element>
  public typealias Base = MinimalBidirectionalCollection<Element>
  public typealias Iterator = MinimalIterator<Element>
  public typealias Index = MinimalIndex

  public var base: Base
  public var startIndex: Index
  public var endIndex: Index

  public init() {
    expectSliceType(Self_.self)

    self.base = Base()
    self.startIndex = base.startIndex
    self.endIndex = base.endIndex
  }

  public init(base: Base) {
    self.base = base
    self.startIndex = base.startIndex
    self.endIndex = base.endIndex
  }

  public init(base: Base, bounds: Range<Index>) {
    self.base = base
    self.startIndex = bounds.lowerBound
    self.endIndex = bounds.upperBound
  }

  public init(_ array: [Element]) {
    self = DefaultedBidirectionalRangeReplaceableSlice(
      base: Base(elements: array))
  }

  public init(elements: [Element]) {
    self = DefaultedBidirectionalRangeReplaceableSlice(
      base: Base(elements: elements))
  }

  public func makeIterator() -> MinimalIterator<Element> {
    return MinimalIterator(Array(self))
  }

  public subscript(index: Index) -> Element {
    Index._failEarlyRangeCheck(index, bounds: startIndex..<endIndex)
    return base[index]
  }

  public subscript(bounds: Range<Index>) -> Self_ {
    Index._failEarlyRangeCheck2(
      rangeStart: bounds.lowerBound,
      rangeEnd: bounds.upperBound,
      boundsStart: startIndex,
      boundsEnd: endIndex)
    return DefaultedBidirectionalRangeReplaceableSlice(
      base: base, bounds: bounds)
  }

  public mutating func replaceSubrange<
    C : Collection
  >(
    _ subRange: Range<Index>,
    with newElements: C
  ) where C.Element == Element {
    let startOffset = startIndex.position
    let endOffset =
      endIndex.position
      - subRange.count
      + numericCast(newElements.count) as Int
    Index._failEarlyRangeCheck2(
      rangeStart: subRange.lowerBound,
      rangeEnd: subRange.upperBound,
      boundsStart: startIndex,
      boundsEnd: endIndex)
    base.replaceSubrange(subRange, with: newElements)
    startIndex = base.startIndex.advanced(by: startOffset)
    endIndex = base.startIndex.advanced(by: endOffset)
  }
}
*/


/// A Collection that uses as many default implementations as
/// `Collection` can provide.
public struct DefaultedRangeReplaceableBidirectionalCollection<Element> : BidirectionalCollection, RangeReplaceableCollection {
  public typealias Base = MinimalRangeReplaceableBidirectionalCollection<Element>
  public typealias Iterator = MinimalIterator<Element>
  public typealias Index = MinimalIndex


  public var base: Base

  public init(base: Base) {
    self.base = base
  }

  public init(_ array: [Element]) {
    self.base = Base(elements: array)
  }

  public init(elements: [Element]) {
    self.base = Base(elements: elements)
  }

  public init<S : Sequence>(
    elements: S,
    underestimatedCount: UnderestimatedCountBehavior = .value(0)
  ) where S.Element == Element {
    self.init(base:
      MinimalRangeReplaceableBidirectionalCollection(elements: elements, underestimatedCount: underestimatedCount))
  }

  public var underestimatedCount: Int {
    return base.underestimatedCount
  }

  public let timesMakeIteratorCalled = ResettableValue(0)

  public func makeIterator() -> MinimalIterator<Element> {
    timesMakeIteratorCalled.value += 1
    return base.makeIterator()
  }


  public let timesSuccessorCalled = ResettableValue(0)

  public func index(after i: MinimalIndex) -> MinimalIndex {
    timesSuccessorCalled.value += 1
    return base.index(after: i)
  }

  public let timesPredecessorCalled = ResettableValue(0)

  public func index(before i: MinimalIndex) -> MinimalIndex {
    timesPredecessorCalled.value += 1
    return base.index(before: i)
  }



  public let timesStartIndexCalled = ResettableValue(0)

  public var startIndex: MinimalIndex {
    timesStartIndexCalled.value += 1
    return base.startIndex
  }

  public let timesEndIndexCalled = ResettableValue(0)

  public var endIndex: MinimalIndex {
    timesEndIndexCalled.value += 1
    return base.endIndex
  }

  public subscript(i: MinimalIndex) -> Element {
    get {
      return base[i]
    }
  }

    // FIXME: swift-3-indexing-model: use defaults.
//     if Self not in ['DefaultedCollection', 'DefaultedBidirectionalCollection', 'DefaultedRandomAccessCollection', 'DefaultedMutableCollection', 'DefaultedRangeReplaceableCollection']:

  public subscript(bounds: Range<MinimalIndex>) -> Slice<DefaultedRangeReplaceableBidirectionalCollection<Base.Element>> {
    get {
      // FIXME: swift-3-indexing-model: range check.
      return Slice(base: self, bounds: bounds)
    }
  }

  public init() {
    base = Base()
  }

  public mutating func replaceSubrange<C>(
    _ subRange: Range<DefaultedRangeReplaceableBidirectionalCollection<Element>.Index>,
    with newElements: C
  ) where C : Collection, C.Element == Element {
    base.replaceSubrange(subRange, with: newElements)
  }
}

/*
FIXME: swift-3-indexing-model: uncomment this.
public struct DefaultedBidirectionalRangeReplaceableSlice<Element>
  : RangeReplaceableCollection {

  public typealias Self_ = DefaultedBidirectionalRangeReplaceableSlice<Element>
  public typealias Base = MinimalRangeReplaceableBidirectionalCollection<Element>
  public typealias Iterator = MinimalIterator<Element>
  public typealias Index = MinimalIndex

  public var base: Base
  public var startIndex: Index
  public var endIndex: Index

  public init() {
    expectSliceType(Self_.self)

    self.base = Base()
    self.startIndex = base.startIndex
    self.endIndex = base.endIndex
  }

  public init(base: Base) {
    self.base = base
    self.startIndex = base.startIndex
    self.endIndex = base.endIndex
  }

  public init(base: Base, bounds: Range<Index>) {
    self.base = base
    self.startIndex = bounds.lowerBound
    self.endIndex = bounds.upperBound
  }

  public init(_ array: [Element]) {
    self = DefaultedBidirectionalRangeReplaceableSlice(
      base: Base(elements: array))
  }

  public init(elements: [Element]) {
    self = DefaultedBidirectionalRangeReplaceableSlice(
      base: Base(elements: elements))
  }

  public func makeIterator() -> MinimalIterator<Element> {
    return MinimalIterator(Array(self))
  }

  public subscript(index: Index) -> Element {
    Index._failEarlyRangeCheck(index, bounds: startIndex..<endIndex)
    return base[index]
  }

  public subscript(bounds: Range<Index>) -> Self_ {
    Index._failEarlyRangeCheck2(
      rangeStart: bounds.lowerBound,
      rangeEnd: bounds.upperBound,
      boundsStart: startIndex,
      boundsEnd: endIndex)
    return DefaultedBidirectionalRangeReplaceableSlice(
      base: base, bounds: bounds)
  }

  public mutating func replaceSubrange<
    C : Collection
  >(
    _ subRange: Range<Index>,
    with newElements: C
  ) where C.Element == Element {
    let startOffset = startIndex.position
    let endOffset =
      endIndex.position
      - subRange.count
      + numericCast(newElements.count) as Int
    Index._failEarlyRangeCheck2(
      rangeStart: subRange.lowerBound,
      rangeEnd: subRange.upperBound,
      boundsStart: startIndex,
      boundsEnd: endIndex)
    base.replaceSubrange(subRange, with: newElements)
    startIndex = base.startIndex.advanced(by: startOffset)
    endIndex = base.startIndex.advanced(by: endOffset)
  }
}
*/


/// A Collection that uses as many default implementations as
/// `Collection` can provide.
public struct DefaultedMutableBidirectionalCollection<Element> : BidirectionalCollection, MutableCollection {
  public typealias Base = MinimalMutableBidirectionalCollection<Element>
  public typealias Iterator = MinimalIterator<Element>
  public typealias Index = MinimalIndex


  public var base: Base

  public init(base: Base) {
    self.base = base
  }

  public init(_ array: [Element]) {
    self.base = Base(elements: array)
  }

  public init(elements: [Element]) {
    self.base = Base(elements: elements)
  }

  public init<S : Sequence>(
    elements: S,
    underestimatedCount: UnderestimatedCountBehavior = .value(0)
  ) where S.Element == Element {
    self.init(base:
      MinimalMutableBidirectionalCollection(elements: elements, underestimatedCount: underestimatedCount))
  }

  public var underestimatedCount: Int {
    return base.underestimatedCount
  }

  public let timesMakeIteratorCalled = ResettableValue(0)

  public func makeIterator() -> MinimalIterator<Element> {
    timesMakeIteratorCalled.value += 1
    return base.makeIterator()
  }


  public let timesSuccessorCalled = ResettableValue(0)

  public func index(after i: MinimalIndex) -> MinimalIndex {
    timesSuccessorCalled.value += 1
    return base.index(after: i)
  }

  public let timesPredecessorCalled = ResettableValue(0)

  public func index(before i: MinimalIndex) -> MinimalIndex {
    timesPredecessorCalled.value += 1
    return base.index(before: i)
  }



  public let timesStartIndexCalled = ResettableValue(0)

  public var startIndex: MinimalIndex {
    timesStartIndexCalled.value += 1
    return base.startIndex
  }

  public let timesEndIndexCalled = ResettableValue(0)

  public var endIndex: MinimalIndex {
    timesEndIndexCalled.value += 1
    return base.endIndex
  }

  public subscript(i: MinimalIndex) -> Element {
    get {
      return base[i]
    }
    set {
      base[i] = newValue
    }
  }

    // FIXME: swift-3-indexing-model: use defaults.
//     if Self not in ['DefaultedCollection', 'DefaultedBidirectionalCollection', 'DefaultedRandomAccessCollection', 'DefaultedMutableCollection', 'DefaultedRangeReplaceableCollection']:

  public subscript(bounds: Range<MinimalIndex>) -> Slice<DefaultedMutableBidirectionalCollection<Base.Element>> {
    get {
      // FIXME: swift-3-indexing-model: range check.
      return Slice(base: self, bounds: bounds)
    }
    set {
      _writeBackMutableSlice(&self, bounds: bounds, slice: newValue)
    }
  }

}

/*
FIXME: swift-3-indexing-model: uncomment this.
public struct DefaultedBidirectionalRangeReplaceableSlice<Element>
  : RangeReplaceableCollection {

  public typealias Self_ = DefaultedBidirectionalRangeReplaceableSlice<Element>
  public typealias Base = MinimalMutableBidirectionalCollection<Element>
  public typealias Iterator = MinimalIterator<Element>
  public typealias Index = MinimalIndex

  public var base: Base
  public var startIndex: Index
  public var endIndex: Index

  public init() {
    expectSliceType(Self_.self)

    self.base = Base()
    self.startIndex = base.startIndex
    self.endIndex = base.endIndex
  }

  public init(base: Base) {
    self.base = base
    self.startIndex = base.startIndex
    self.endIndex = base.endIndex
  }

  public init(base: Base, bounds: Range<Index>) {
    self.base = base
    self.startIndex = bounds.lowerBound
    self.endIndex = bounds.upperBound
  }

  public init(_ array: [Element]) {
    self = DefaultedBidirectionalRangeReplaceableSlice(
      base: Base(elements: array))
  }

  public init(elements: [Element]) {
    self = DefaultedBidirectionalRangeReplaceableSlice(
      base: Base(elements: elements))
  }

  public func makeIterator() -> MinimalIterator<Element> {
    return MinimalIterator(Array(self))
  }

  public subscript(index: Index) -> Element {
    Index._failEarlyRangeCheck(index, bounds: startIndex..<endIndex)
    return base[index]
  }

  public subscript(bounds: Range<Index>) -> Self_ {
    Index._failEarlyRangeCheck2(
      rangeStart: bounds.lowerBound,
      rangeEnd: bounds.upperBound,
      boundsStart: startIndex,
      boundsEnd: endIndex)
    return DefaultedBidirectionalRangeReplaceableSlice(
      base: base, bounds: bounds)
  }

  public mutating func replaceSubrange<
    C : Collection
  >(
    _ subRange: Range<Index>,
    with newElements: C
  ) where C.Element == Element {
    let startOffset = startIndex.position
    let endOffset =
      endIndex.position
      - subRange.count
      + numericCast(newElements.count) as Int
    Index._failEarlyRangeCheck2(
      rangeStart: subRange.lowerBound,
      rangeEnd: subRange.upperBound,
      boundsStart: startIndex,
      boundsEnd: endIndex)
    base.replaceSubrange(subRange, with: newElements)
    startIndex = base.startIndex.advanced(by: startOffset)
    endIndex = base.startIndex.advanced(by: endOffset)
  }
}
*/


/// A Collection that uses as many default implementations as
/// `Collection` can provide.
public struct DefaultedMutableRangeReplaceableBidirectionalCollection<Element> : BidirectionalCollection, MutableCollection, RangeReplaceableCollection {
  public typealias Base = MinimalMutableRangeReplaceableBidirectionalCollection<Element>
  public typealias Iterator = MinimalIterator<Element>
  public typealias Index = MinimalIndex


  public var base: Base

  public init(base: Base) {
    self.base = base
  }

  public init(_ array: [Element]) {
    self.base = Base(elements: array)
  }

  public init(elements: [Element]) {
    self.base = Base(elements: elements)
  }

  public init<S : Sequence>(
    elements: S,
    underestimatedCount: UnderestimatedCountBehavior = .value(0)
  ) where S.Element == Element {
    self.init(base:
      MinimalMutableRangeReplaceableBidirectionalCollection(elements: elements, underestimatedCount: underestimatedCount))
  }

  public var underestimatedCount: Int {
    return base.underestimatedCount
  }

  public let timesMakeIteratorCalled = ResettableValue(0)

  public func makeIterator() -> MinimalIterator<Element> {
    timesMakeIteratorCalled.value += 1
    return base.makeIterator()
  }


  public let timesSuccessorCalled = ResettableValue(0)

  public func index(after i: MinimalIndex) -> MinimalIndex {
    timesSuccessorCalled.value += 1
    return base.index(after: i)
  }

  public let timesPredecessorCalled = ResettableValue(0)

  public func index(before i: MinimalIndex) -> MinimalIndex {
    timesPredecessorCalled.value += 1
    return base.index(before: i)
  }



  public let timesStartIndexCalled = ResettableValue(0)

  public var startIndex: MinimalIndex {
    timesStartIndexCalled.value += 1
    return base.startIndex
  }

  public let timesEndIndexCalled = ResettableValue(0)

  public var endIndex: MinimalIndex {
    timesEndIndexCalled.value += 1
    return base.endIndex
  }

  public subscript(i: MinimalIndex) -> Element {
    get {
      return base[i]
    }
    set {
      base[i] = newValue
    }
  }

    // FIXME: swift-3-indexing-model: use defaults.
//     if Self not in ['DefaultedCollection', 'DefaultedBidirectionalCollection', 'DefaultedRandomAccessCollection', 'DefaultedMutableCollection', 'DefaultedRangeReplaceableCollection']:

  public subscript(bounds: Range<MinimalIndex>) -> Slice<DefaultedMutableRangeReplaceableBidirectionalCollection<Base.Element>> {
    get {
      // FIXME: swift-3-indexing-model: range check.
      return Slice(base: self, bounds: bounds)
    }
    set {
      _writeBackMutableSlice(&self, bounds: bounds, slice: newValue)
    }
  }

  public init() {
    base = Base()
  }

  public mutating func replaceSubrange<C>(
    _ subRange: Range<DefaultedMutableRangeReplaceableBidirectionalCollection<Element>.Index>,
    with newElements: C
  ) where C : Collection, C.Element == Element {
    base.replaceSubrange(subRange, with: newElements)
  }
}

/*
FIXME: swift-3-indexing-model: uncomment this.
public struct DefaultedBidirectionalRangeReplaceableSlice<Element>
  : RangeReplaceableCollection {

  public typealias Self_ = DefaultedBidirectionalRangeReplaceableSlice<Element>
  public typealias Base = MinimalMutableRangeReplaceableBidirectionalCollection<Element>
  public typealias Iterator = MinimalIterator<Element>
  public typealias Index = MinimalIndex

  public var base: Base
  public var startIndex: Index
  public var endIndex: Index

  public init() {
    expectSliceType(Self_.self)

    self.base = Base()
    self.startIndex = base.startIndex
    self.endIndex = base.endIndex
  }

  public init(base: Base) {
    self.base = base
    self.startIndex = base.startIndex
    self.endIndex = base.endIndex
  }

  public init(base: Base, bounds: Range<Index>) {
    self.base = base
    self.startIndex = bounds.lowerBound
    self.endIndex = bounds.upperBound
  }

  public init(_ array: [Element]) {
    self = DefaultedBidirectionalRangeReplaceableSlice(
      base: Base(elements: array))
  }

  public init(elements: [Element]) {
    self = DefaultedBidirectionalRangeReplaceableSlice(
      base: Base(elements: elements))
  }

  public func makeIterator() -> MinimalIterator<Element> {
    return MinimalIterator(Array(self))
  }

  public subscript(index: Index) -> Element {
    Index._failEarlyRangeCheck(index, bounds: startIndex..<endIndex)
    return base[index]
  }

  public subscript(bounds: Range<Index>) -> Self_ {
    Index._failEarlyRangeCheck2(
      rangeStart: bounds.lowerBound,
      rangeEnd: bounds.upperBound,
      boundsStart: startIndex,
      boundsEnd: endIndex)
    return DefaultedBidirectionalRangeReplaceableSlice(
      base: base, bounds: bounds)
  }

  public mutating func replaceSubrange<
    C : Collection
  >(
    _ subRange: Range<Index>,
    with newElements: C
  ) where C.Element == Element {
    let startOffset = startIndex.position
    let endOffset =
      endIndex.position
      - subRange.count
      + numericCast(newElements.count) as Int
    Index._failEarlyRangeCheck2(
      rangeStart: subRange.lowerBound,
      rangeEnd: subRange.upperBound,
      boundsStart: startIndex,
      boundsEnd: endIndex)
    base.replaceSubrange(subRange, with: newElements)
    startIndex = base.startIndex.advanced(by: startOffset)
    endIndex = base.startIndex.advanced(by: endOffset)
  }
}
*/


/// A Collection that uses as many default implementations as
/// `Collection` can provide.
public struct DefaultedRandomAccessCollection<Element> : RandomAccessCollection {
  public typealias Base = MinimalRandomAccessCollection<Element>
  public typealias Iterator = MinimalIterator<Element>
  public typealias Index = MinimalIndex

  // FIXME: this shouldn't be necessary, should come by default
  public typealias Indices = DefaultIndices<DefaultedRandomAccessCollection<Element>>

  public let base: Base

  public init(base: Base) {
    self.base = base
  }

  public init(_ array: [Element]) {
    self.base = Base(elements: array)
  }

  public init(elements: [Element]) {
    self.base = Base(elements: elements)
  }

  public init<S : Sequence>(
    elements: S,
    underestimatedCount: UnderestimatedCountBehavior = .value(0)
  ) where S.Element == Element {
    self.init(base:
      MinimalRandomAccessCollection(elements: elements, underestimatedCount: underestimatedCount))
  }

  public var underestimatedCount: Int {
    return base.underestimatedCount
  }

  public let timesMakeIteratorCalled = ResettableValue(0)

  public func makeIterator() -> MinimalIterator<Element> {
    timesMakeIteratorCalled.value += 1
    return base.makeIterator()
  }


  public let timesSuccessorCalled = ResettableValue(0)

  public func index(after i: MinimalIndex) -> MinimalIndex {
    timesSuccessorCalled.value += 1
    return base.index(after: i)
  }

  public let timesPredecessorCalled = ResettableValue(0)

  public func index(before i: MinimalIndex) -> MinimalIndex {
    timesPredecessorCalled.value += 1
    return base.index(before: i)
  }

  public func distance(from start: MinimalIndex, to end: MinimalIndex)
    -> Int {
    return base.distance(from: start, to: end)
  }

  public func index(_ i: Index, offsetBy n: Int) -> Index {
    return base.index(i, offsetBy: n)
  }


  public let timesStartIndexCalled = ResettableValue(0)

  public var startIndex: MinimalIndex {
    timesStartIndexCalled.value += 1
    return base.startIndex
  }

  public let timesEndIndexCalled = ResettableValue(0)

  public var endIndex: MinimalIndex {
    timesEndIndexCalled.value += 1
    return base.endIndex
  }

  public subscript(i: MinimalIndex) -> Element {
    get {
      return base[i]
    }
  }

    // FIXME: swift-3-indexing-model: use defaults.
//     if Self not in ['DefaultedCollection', 'DefaultedBidirectionalCollection', 'DefaultedRandomAccessCollection', 'DefaultedMutableCollection', 'DefaultedRangeReplaceableCollection']:

  public subscript(bounds: Range<MinimalIndex>) -> Slice<DefaultedRandomAccessCollection<Base.Element>> {
    get {
      // FIXME: swift-3-indexing-model: range check.
      return Slice(base: self, bounds: bounds)
    }
  }

}

/*
FIXME: swift-3-indexing-model: uncomment this.
public struct DefaultedRandomAccessRangeReplaceableSlice<Element>
  : RangeReplaceableCollection {

  public typealias Self_ = DefaultedRandomAccessRangeReplaceableSlice<Element>
  public typealias Base = MinimalRandomAccessCollection<Element>
  public typealias Iterator = MinimalIterator<Element>
  public typealias Index = MinimalIndex

  public var base: Base
  public var startIndex: Index
  public var endIndex: Index

  public init() {
    expectSliceType(Self_.self)

    self.base = Base()
    self.startIndex = base.startIndex
    self.endIndex = base.endIndex
  }

  public init(base: Base) {
    self.base = base
    self.startIndex = base.startIndex
    self.endIndex = base.endIndex
  }

  public init(base: Base, bounds: Range<Index>) {
    self.base = base
    self.startIndex = bounds.lowerBound
    self.endIndex = bounds.upperBound
  }

  public init(_ array: [Element]) {
    self = DefaultedRandomAccessRangeReplaceableSlice(
      base: Base(elements: array))
  }

  public init(elements: [Element]) {
    self = DefaultedRandomAccessRangeReplaceableSlice(
      base: Base(elements: elements))
  }

  public func makeIterator() -> MinimalIterator<Element> {
    return MinimalIterator(Array(self))
  }

  public subscript(index: Index) -> Element {
    Index._failEarlyRangeCheck(index, bounds: startIndex..<endIndex)
    return base[index]
  }

  public subscript(bounds: Range<Index>) -> Self_ {
    Index._failEarlyRangeCheck2(
      rangeStart: bounds.lowerBound,
      rangeEnd: bounds.upperBound,
      boundsStart: startIndex,
      boundsEnd: endIndex)
    return DefaultedRandomAccessRangeReplaceableSlice(
      base: base, bounds: bounds)
  }

  public mutating func replaceSubrange<
    C : Collection
  >(
    _ subRange: Range<Index>,
    with newElements: C
  ) where C.Element == Element {
    let startOffset = startIndex.position
    let endOffset =
      endIndex.position
      - subRange.count
      + numericCast(newElements.count) as Int
    Index._failEarlyRangeCheck2(
      rangeStart: subRange.lowerBound,
      rangeEnd: subRange.upperBound,
      boundsStart: startIndex,
      boundsEnd: endIndex)
    base.replaceSubrange(subRange, with: newElements)
    startIndex = base.startIndex.advanced(by: startOffset)
    endIndex = base.startIndex.advanced(by: endOffset)
  }
}
*/


/// A Collection that uses as many default implementations as
/// `Collection` can provide.
public struct DefaultedRandomAccessCollectionWithStrideableIndex<Element> : RandomAccessCollection {
  public typealias Base = MinimalRandomAccessCollectionWithStrideableIndex<Element>
  public typealias Iterator = MinimalIterator<Element>
  public typealias Index = MinimalStrideableIndex

  public typealias Indices = Range<MinimalStrideableIndex>

  public let base: Base

  public init(base: Base) {
    self.base = base
  }

  public init(_ array: [Element]) {
    self.base = Base(elements: array)
  }

  public init(elements: [Element]) {
    self.base = Base(elements: elements)
  }

  public init<S : Sequence>(
    elements: S,
    underestimatedCount: UnderestimatedCountBehavior = .value(0)
  ) where S.Element == Element {
    self.init(base:
      MinimalRandomAccessCollectionWithStrideableIndex(elements: elements, underestimatedCount: underestimatedCount))
  }

  public var underestimatedCount: Int {
    return base.underestimatedCount
  }

  public let timesMakeIteratorCalled = ResettableValue(0)

  public func makeIterator() -> MinimalIterator<Element> {
    timesMakeIteratorCalled.value += 1
    return base.makeIterator()
  }


  public let timesStartIndexCalled = ResettableValue(0)

  public var startIndex: MinimalStrideableIndex {
    timesStartIndexCalled.value += 1
    return base.startIndex
  }

  public let timesEndIndexCalled = ResettableValue(0)

  public var endIndex: MinimalStrideableIndex {
    timesEndIndexCalled.value += 1
    return base.endIndex
  }

  public subscript(i: MinimalStrideableIndex) -> Element {
    get {
      return base[i]
    }
  }

    // FIXME: swift-3-indexing-model: use defaults.
//     if Self not in ['DefaultedCollection', 'DefaultedBidirectionalCollection', 'DefaultedRandomAccessCollection', 'DefaultedMutableCollection', 'DefaultedRangeReplaceableCollection']:

  public subscript(bounds: Range<MinimalStrideableIndex>) -> Slice<DefaultedRandomAccessCollectionWithStrideableIndex<Base.Element>> {
    get {
      // FIXME: swift-3-indexing-model: range check.
      return Slice(base: self, bounds: bounds)
    }
  }

}

/*
FIXME: swift-3-indexing-model: uncomment this.
public struct DefaultedRandomAccessRangeReplaceableSlice<Element>
  : RangeReplaceableCollection {

  public typealias Self_ = DefaultedRandomAccessRangeReplaceableSlice<Element>
  public typealias Base = MinimalRandomAccessCollectionWithStrideableIndex<Element>
  public typealias Iterator = MinimalIterator<Element>
  public typealias Index = MinimalStrideableIndex

  public var base: Base
  public var startIndex: Index
  public var endIndex: Index

  public init() {
    expectSliceType(Self_.self)

    self.base = Base()
    self.startIndex = base.startIndex
    self.endIndex = base.endIndex
  }

  public init(base: Base) {
    self.base = base
    self.startIndex = base.startIndex
    self.endIndex = base.endIndex
  }

  public init(base: Base, bounds: Range<Index>) {
    self.base = base
    self.startIndex = bounds.lowerBound
    self.endIndex = bounds.upperBound
  }

  public init(_ array: [Element]) {
    self = DefaultedRandomAccessRangeReplaceableSlice(
      base: Base(elements: array))
  }

  public init(elements: [Element]) {
    self = DefaultedRandomAccessRangeReplaceableSlice(
      base: Base(elements: elements))
  }

  public func makeIterator() -> MinimalIterator<Element> {
    return MinimalIterator(Array(self))
  }

  public subscript(index: Index) -> Element {
    Index._failEarlyRangeCheck(index, bounds: startIndex..<endIndex)
    return base[index]
  }

  public subscript(bounds: Range<Index>) -> Self_ {
    Index._failEarlyRangeCheck2(
      rangeStart: bounds.lowerBound,
      rangeEnd: bounds.upperBound,
      boundsStart: startIndex,
      boundsEnd: endIndex)
    return DefaultedRandomAccessRangeReplaceableSlice(
      base: base, bounds: bounds)
  }

  public mutating func replaceSubrange<
    C : Collection
  >(
    _ subRange: Range<Index>,
    with newElements: C
  ) where C.Element == Element {
    let startOffset = startIndex.position
    let endOffset =
      endIndex.position
      - subRange.count
      + numericCast(newElements.count) as Int
    Index._failEarlyRangeCheck2(
      rangeStart: subRange.lowerBound,
      rangeEnd: subRange.upperBound,
      boundsStart: startIndex,
      boundsEnd: endIndex)
    base.replaceSubrange(subRange, with: newElements)
    startIndex = base.startIndex.advanced(by: startOffset)
    endIndex = base.startIndex.advanced(by: endOffset)
  }
}
*/


/// A Collection that uses as many default implementations as
/// `Collection` can provide.
public struct DefaultedRangeReplaceableRandomAccessCollection<Element> : RandomAccessCollection, RangeReplaceableCollection {
  public typealias Base = MinimalRangeReplaceableRandomAccessCollection<Element>
  public typealias Iterator = MinimalIterator<Element>
  public typealias Index = MinimalIndex

  // FIXME: this shouldn't be necessary, should come by default
  public typealias Indices = DefaultIndices<DefaultedRangeReplaceableRandomAccessCollection<Element>>

  public var base: Base

  public init(base: Base) {
    self.base = base
  }

  public init(_ array: [Element]) {
    self.base = Base(elements: array)
  }

  public init(elements: [Element]) {
    self.base = Base(elements: elements)
  }

  public init<S : Sequence>(
    elements: S,
    underestimatedCount: UnderestimatedCountBehavior = .value(0)
  ) where S.Element == Element {
    self.init(base:
      MinimalRangeReplaceableRandomAccessCollection(elements: elements, underestimatedCount: underestimatedCount))
  }

  public var underestimatedCount: Int {
    return base.underestimatedCount
  }

  public let timesMakeIteratorCalled = ResettableValue(0)

  public func makeIterator() -> MinimalIterator<Element> {
    timesMakeIteratorCalled.value += 1
    return base.makeIterator()
  }


  public let timesSuccessorCalled = ResettableValue(0)

  public func index(after i: MinimalIndex) -> MinimalIndex {
    timesSuccessorCalled.value += 1
    return base.index(after: i)
  }

  public let timesPredecessorCalled = ResettableValue(0)

  public func index(before i: MinimalIndex) -> MinimalIndex {
    timesPredecessorCalled.value += 1
    return base.index(before: i)
  }

  public func distance(from start: MinimalIndex, to end: MinimalIndex)
    -> Int {
    return base.distance(from: start, to: end)
  }

  public func index(_ i: Index, offsetBy n: Int) -> Index {
    return base.index(i, offsetBy: n)
  }


  public let timesStartIndexCalled = ResettableValue(0)

  public var startIndex: MinimalIndex {
    timesStartIndexCalled.value += 1
    return base.startIndex
  }

  public let timesEndIndexCalled = ResettableValue(0)

  public var endIndex: MinimalIndex {
    timesEndIndexCalled.value += 1
    return base.endIndex
  }

  public subscript(i: MinimalIndex) -> Element {
    get {
      return base[i]
    }
  }

    // FIXME: swift-3-indexing-model: use defaults.
//     if Self not in ['DefaultedCollection', 'DefaultedBidirectionalCollection', 'DefaultedRandomAccessCollection', 'DefaultedMutableCollection', 'DefaultedRangeReplaceableCollection']:

  public subscript(bounds: Range<MinimalIndex>) -> Slice<DefaultedRangeReplaceableRandomAccessCollection<Base.Element>> {
    get {
      // FIXME: swift-3-indexing-model: range check.
      return Slice(base: self, bounds: bounds)
    }
  }

  public init() {
    base = Base()
  }

  public mutating func replaceSubrange<C>(
    _ subRange: Range<DefaultedRangeReplaceableRandomAccessCollection<Element>.Index>,
    with newElements: C
  ) where C : Collection, C.Element == Element {
    base.replaceSubrange(subRange, with: newElements)
  }
}

/*
FIXME: swift-3-indexing-model: uncomment this.
public struct DefaultedRandomAccessRangeReplaceableSlice<Element>
  : RangeReplaceableCollection {

  public typealias Self_ = DefaultedRandomAccessRangeReplaceableSlice<Element>
  public typealias Base = MinimalRangeReplaceableRandomAccessCollection<Element>
  public typealias Iterator = MinimalIterator<Element>
  public typealias Index = MinimalIndex

  public var base: Base
  public var startIndex: Index
  public var endIndex: Index

  public init() {
    expectSliceType(Self_.self)

    self.base = Base()
    self.startIndex = base.startIndex
    self.endIndex = base.endIndex
  }

  public init(base: Base) {
    self.base = base
    self.startIndex = base.startIndex
    self.endIndex = base.endIndex
  }

  public init(base: Base, bounds: Range<Index>) {
    self.base = base
    self.startIndex = bounds.lowerBound
    self.endIndex = bounds.upperBound
  }

  public init(_ array: [Element]) {
    self = DefaultedRandomAccessRangeReplaceableSlice(
      base: Base(elements: array))
  }

  public init(elements: [Element]) {
    self = DefaultedRandomAccessRangeReplaceableSlice(
      base: Base(elements: elements))
  }

  public func makeIterator() -> MinimalIterator<Element> {
    return MinimalIterator(Array(self))
  }

  public subscript(index: Index) -> Element {
    Index._failEarlyRangeCheck(index, bounds: startIndex..<endIndex)
    return base[index]
  }

  public subscript(bounds: Range<Index>) -> Self_ {
    Index._failEarlyRangeCheck2(
      rangeStart: bounds.lowerBound,
      rangeEnd: bounds.upperBound,
      boundsStart: startIndex,
      boundsEnd: endIndex)
    return DefaultedRandomAccessRangeReplaceableSlice(
      base: base, bounds: bounds)
  }

  public mutating func replaceSubrange<
    C : Collection
  >(
    _ subRange: Range<Index>,
    with newElements: C
  ) where C.Element == Element {
    let startOffset = startIndex.position
    let endOffset =
      endIndex.position
      - subRange.count
      + numericCast(newElements.count) as Int
    Index._failEarlyRangeCheck2(
      rangeStart: subRange.lowerBound,
      rangeEnd: subRange.upperBound,
      boundsStart: startIndex,
      boundsEnd: endIndex)
    base.replaceSubrange(subRange, with: newElements)
    startIndex = base.startIndex.advanced(by: startOffset)
    endIndex = base.startIndex.advanced(by: endOffset)
  }
}
*/


/// A Collection that uses as many default implementations as
/// `Collection` can provide.
public struct DefaultedMutableRandomAccessCollection<Element> : RandomAccessCollection, MutableCollection {
  public typealias Base = MinimalMutableRandomAccessCollection<Element>
  public typealias Iterator = MinimalIterator<Element>
  public typealias Index = MinimalIndex

  // FIXME: this shouldn't be necessary, should come by default
  public typealias Indices = DefaultIndices<DefaultedMutableRandomAccessCollection<Element>>

  public var base: Base

  public init(base: Base) {
    self.base = base
  }

  public init(_ array: [Element]) {
    self.base = Base(elements: array)
  }

  public init(elements: [Element]) {
    self.base = Base(elements: elements)
  }

  public init<S : Sequence>(
    elements: S,
    underestimatedCount: UnderestimatedCountBehavior = .value(0)
  ) where S.Element == Element {
    self.init(base:
      MinimalMutableRandomAccessCollection(elements: elements, underestimatedCount: underestimatedCount))
  }

  public var underestimatedCount: Int {
    return base.underestimatedCount
  }

  public let timesMakeIteratorCalled = ResettableValue(0)

  public func makeIterator() -> MinimalIterator<Element> {
    timesMakeIteratorCalled.value += 1
    return base.makeIterator()
  }


  public let timesSuccessorCalled = ResettableValue(0)

  public func index(after i: MinimalIndex) -> MinimalIndex {
    timesSuccessorCalled.value += 1
    return base.index(after: i)
  }

  public let timesPredecessorCalled = ResettableValue(0)

  public func index(before i: MinimalIndex) -> MinimalIndex {
    timesPredecessorCalled.value += 1
    return base.index(before: i)
  }

  public func distance(from start: MinimalIndex, to end: MinimalIndex)
    -> Int {
    return base.distance(from: start, to: end)
  }

  public func index(_ i: Index, offsetBy n: Int) -> Index {
    return base.index(i, offsetBy: n)
  }


  public let timesStartIndexCalled = ResettableValue(0)

  public var startIndex: MinimalIndex {
    timesStartIndexCalled.value += 1
    return base.startIndex
  }

  public let timesEndIndexCalled = ResettableValue(0)

  public var endIndex: MinimalIndex {
    timesEndIndexCalled.value += 1
    return base.endIndex
  }

  public subscript(i: MinimalIndex) -> Element {
    get {
      return base[i]
    }
    set {
      base[i] = newValue
    }
  }

    // FIXME: swift-3-indexing-model: use defaults.
//     if Self not in ['DefaultedCollection', 'DefaultedBidirectionalCollection', 'DefaultedRandomAccessCollection', 'DefaultedMutableCollection', 'DefaultedRangeReplaceableCollection']:

  public subscript(bounds: Range<MinimalIndex>) -> Slice<DefaultedMutableRandomAccessCollection<Base.Element>> {
    get {
      // FIXME: swift-3-indexing-model: range check.
      return Slice(base: self, bounds: bounds)
    }
    set {
      _writeBackMutableSlice(&self, bounds: bounds, slice: newValue)
    }
  }

}

/*
FIXME: swift-3-indexing-model: uncomment this.
public struct DefaultedRandomAccessRangeReplaceableSlice<Element>
  : RangeReplaceableCollection {

  public typealias Self_ = DefaultedRandomAccessRangeReplaceableSlice<Element>
  public typealias Base = MinimalMutableRandomAccessCollection<Element>
  public typealias Iterator = MinimalIterator<Element>
  public typealias Index = MinimalIndex

  public var base: Base
  public var startIndex: Index
  public var endIndex: Index

  public init() {
    expectSliceType(Self_.self)

    self.base = Base()
    self.startIndex = base.startIndex
    self.endIndex = base.endIndex
  }

  public init(base: Base) {
    self.base = base
    self.startIndex = base.startIndex
    self.endIndex = base.endIndex
  }

  public init(base: Base, bounds: Range<Index>) {
    self.base = base
    self.startIndex = bounds.lowerBound
    self.endIndex = bounds.upperBound
  }

  public init(_ array: [Element]) {
    self = DefaultedRandomAccessRangeReplaceableSlice(
      base: Base(elements: array))
  }

  public init(elements: [Element]) {
    self = DefaultedRandomAccessRangeReplaceableSlice(
      base: Base(elements: elements))
  }

  public func makeIterator() -> MinimalIterator<Element> {
    return MinimalIterator(Array(self))
  }

  public subscript(index: Index) -> Element {
    Index._failEarlyRangeCheck(index, bounds: startIndex..<endIndex)
    return base[index]
  }

  public subscript(bounds: Range<Index>) -> Self_ {
    Index._failEarlyRangeCheck2(
      rangeStart: bounds.lowerBound,
      rangeEnd: bounds.upperBound,
      boundsStart: startIndex,
      boundsEnd: endIndex)
    return DefaultedRandomAccessRangeReplaceableSlice(
      base: base, bounds: bounds)
  }

  public mutating func replaceSubrange<
    C : Collection
  >(
    _ subRange: Range<Index>,
    with newElements: C
  ) where C.Element == Element {
    let startOffset = startIndex.position
    let endOffset =
      endIndex.position
      - subRange.count
      + numericCast(newElements.count) as Int
    Index._failEarlyRangeCheck2(
      rangeStart: subRange.lowerBound,
      rangeEnd: subRange.upperBound,
      boundsStart: startIndex,
      boundsEnd: endIndex)
    base.replaceSubrange(subRange, with: newElements)
    startIndex = base.startIndex.advanced(by: startOffset)
    endIndex = base.startIndex.advanced(by: endOffset)
  }
}
*/


/// A Collection that uses as many default implementations as
/// `Collection` can provide.
public struct DefaultedMutableRangeReplaceableRandomAccessCollection<Element> : RandomAccessCollection, MutableCollection, RangeReplaceableCollection {
  public typealias Base = MinimalMutableRangeReplaceableRandomAccessCollection<Element>
  public typealias Iterator = MinimalIterator<Element>
  public typealias Index = MinimalIndex

  // FIXME: this shouldn't be necessary, should come by default
  public typealias Indices = DefaultIndices<DefaultedMutableRangeReplaceableRandomAccessCollection<Element>>

  public var base: Base

  public init(base: Base) {
    self.base = base
  }

  public init(_ array: [Element]) {
    self.base = Base(elements: array)
  }

  public init(elements: [Element]) {
    self.base = Base(elements: elements)
  }

  public init<S : Sequence>(
    elements: S,
    underestimatedCount: UnderestimatedCountBehavior = .value(0)
  ) where S.Element == Element {
    self.init(base:
      MinimalMutableRangeReplaceableRandomAccessCollection(elements: elements, underestimatedCount: underestimatedCount))
  }

  public var underestimatedCount: Int {
    return base.underestimatedCount
  }

  public let timesMakeIteratorCalled = ResettableValue(0)

  public func makeIterator() -> MinimalIterator<Element> {
    timesMakeIteratorCalled.value += 1
    return base.makeIterator()
  }


  public let timesSuccessorCalled = ResettableValue(0)

  public func index(after i: MinimalIndex) -> MinimalIndex {
    timesSuccessorCalled.value += 1
    return base.index(after: i)
  }

  public let timesPredecessorCalled = ResettableValue(0)

  public func index(before i: MinimalIndex) -> MinimalIndex {
    timesPredecessorCalled.value += 1
    return base.index(before: i)
  }

  public func distance(from start: MinimalIndex, to end: MinimalIndex)
    -> Int {
    return base.distance(from: start, to: end)
  }

  public func index(_ i: Index, offsetBy n: Int) -> Index {
    return base.index(i, offsetBy: n)
  }


  public let timesStartIndexCalled = ResettableValue(0)

  public var startIndex: MinimalIndex {
    timesStartIndexCalled.value += 1
    return base.startIndex
  }

  public let timesEndIndexCalled = ResettableValue(0)

  public var endIndex: MinimalIndex {
    timesEndIndexCalled.value += 1
    return base.endIndex
  }

  public subscript(i: MinimalIndex) -> Element {
    get {
      return base[i]
    }
    set {
      base[i] = newValue
    }
  }

    // FIXME: swift-3-indexing-model: use defaults.
//     if Self not in ['DefaultedCollection', 'DefaultedBidirectionalCollection', 'DefaultedRandomAccessCollection', 'DefaultedMutableCollection', 'DefaultedRangeReplaceableCollection']:

  public subscript(bounds: Range<MinimalIndex>) -> Slice<DefaultedMutableRangeReplaceableRandomAccessCollection<Base.Element>> {
    get {
      // FIXME: swift-3-indexing-model: range check.
      return Slice(base: self, bounds: bounds)
    }
    set {
      _writeBackMutableSlice(&self, bounds: bounds, slice: newValue)
    }
  }

  public init() {
    base = Base()
  }

  public mutating func replaceSubrange<C>(
    _ subRange: Range<DefaultedMutableRangeReplaceableRandomAccessCollection<Element>.Index>,
    with newElements: C
  ) where C : Collection, C.Element == Element {
    base.replaceSubrange(subRange, with: newElements)
  }
}

/*
FIXME: swift-3-indexing-model: uncomment this.
public struct DefaultedRandomAccessRangeReplaceableSlice<Element>
  : RangeReplaceableCollection {

  public typealias Self_ = DefaultedRandomAccessRangeReplaceableSlice<Element>
  public typealias Base = MinimalMutableRangeReplaceableRandomAccessCollection<Element>
  public typealias Iterator = MinimalIterator<Element>
  public typealias Index = MinimalIndex

  public var base: Base
  public var startIndex: Index
  public var endIndex: Index

  public init() {
    expectSliceType(Self_.self)

    self.base = Base()
    self.startIndex = base.startIndex
    self.endIndex = base.endIndex
  }

  public init(base: Base) {
    self.base = base
    self.startIndex = base.startIndex
    self.endIndex = base.endIndex
  }

  public init(base: Base, bounds: Range<Index>) {
    self.base = base
    self.startIndex = bounds.lowerBound
    self.endIndex = bounds.upperBound
  }

  public init(_ array: [Element]) {
    self = DefaultedRandomAccessRangeReplaceableSlice(
      base: Base(elements: array))
  }

  public init(elements: [Element]) {
    self = DefaultedRandomAccessRangeReplaceableSlice(
      base: Base(elements: elements))
  }

  public func makeIterator() -> MinimalIterator<Element> {
    return MinimalIterator(Array(self))
  }

  public subscript(index: Index) -> Element {
    Index._failEarlyRangeCheck(index, bounds: startIndex..<endIndex)
    return base[index]
  }

  public subscript(bounds: Range<Index>) -> Self_ {
    Index._failEarlyRangeCheck2(
      rangeStart: bounds.lowerBound,
      rangeEnd: bounds.upperBound,
      boundsStart: startIndex,
      boundsEnd: endIndex)
    return DefaultedRandomAccessRangeReplaceableSlice(
      base: base, bounds: bounds)
  }

  public mutating func replaceSubrange<
    C : Collection
  >(
    _ subRange: Range<Index>,
    with newElements: C
  ) where C.Element == Element {
    let startOffset = startIndex.position
    let endOffset =
      endIndex.position
      - subRange.count
      + numericCast(newElements.count) as Int
    Index._failEarlyRangeCheck2(
      rangeStart: subRange.lowerBound,
      rangeEnd: subRange.upperBound,
      boundsStart: startIndex,
      boundsEnd: endIndex)
    base.replaceSubrange(subRange, with: newElements)
    startIndex = base.startIndex.advanced(by: startOffset)
    endIndex = base.startIndex.advanced(by: endOffset)
  }
}
*/
