// RUN: %target-run-simple-swift
// REQUIRES: executable_test

// https://bugs.swift.org/browse/SR-122
//
// Summary
// =======
//
// This file implements a prototype for a collection model where
// indices can't move themselves forward or backward.  Instead, the
// corresponding collection moves the indices.
//
// Problem
// =======
//
// In practice it has turned out that every one of our concrete
// collection's non random-access indices holds a reference to the
// collection it traverses.  This introduces complexity in
// implementations (especially as we try to avoid multiple-reference
// effects that can cause unnecessary COW copies -- see `Dictionary`
// and `Set`) and presumably translates into less-efficient codegen.
// We should consider other schemes.
//
// Solution
// ========
//
// Change indices so that they can't be moved forward or backward by
// themselves (`i.successor()`).  Then indices can store the minimal
// amount of information about the element position in the collection,
// and avoid keeping a reference to the whole collection.

public protocol MyGeneratorType {
  typealias Element
  mutating func next() -> Element?
}
public protocol MySequenceType {
  typealias Generator : MyGeneratorType
  typealias SubSequence /* : MySequenceType */
  func generate() -> Generator
  @warn_unused_result
  func map<T>(
    @noescape transform: (Generator.Element) throws -> T
  ) rethrows -> [T]
}
extension MySequenceType {
  @warn_unused_result
  public func map<T>(
    @noescape transform: (Generator.Element) throws -> T
  ) rethrows -> [T] {
    var result: [T] = []
    for element in OldSequence(self) {
      result.append(try transform(element))
    }
    return result
  }
}

//------------------------------------------------------------------------
// Bridge between the old world and the new world

struct OldSequence<S : MySequenceType> : SequenceType {
  let _base: S
  init(_ base: S) {
    self._base = base
  }
  func generate() -> OldGenerator<S.Generator> {
    return OldGenerator(_base.generate())
  }
}

struct OldGenerator<G : MyGeneratorType> : GeneratorType {
  var _base: G
  init(_ base: G) {
    self._base = base
  }
  mutating func next() -> G.Element? {
    return _base.next()
  }
}

// End of the bridge
//------------------------------------------------------------------------

public protocol MyIndexableType {
  typealias Index : MyIndexType
  typealias _Element
  typealias UnownedHandle
  var startIndex: Index { get }
  var endIndex: Index { get }
  subscript(i: Index) -> _Element { get }

  init(from handle: UnownedHandle)
  var unownedHandle: UnownedHandle { get }

  @warn_unused_result
  func next(i: Index) -> Index

  func _nextInPlace(inout i: Index)

  func _failEarlyRangeCheck(index: Index, bounds: MyRange<Index>)

  func _failEarlyRangeCheck2(
    rangeStart: Index, rangeEnd: Index, boundsStart: Index, boundsEnd: Index)
}
extension MyIndexableType {
  @inline(__always)
  public func _nextInPlace(inout i: Index) {
    i = next(i)
  }
}

public protocol MyForwardCollectionType : MySequenceType, MyIndexableType {
  typealias Generator = DefaultGenerator<Self>
  typealias Index : MyIndexType
  typealias SubSequence : MySequenceType /* : MyForwardCollectionType */
    = MySlice<Self>
  typealias UnownedHandle = Self // DefaultUnownedForwardCollection<Self>
  typealias IndexRange : MyIndexRangeType, MySequenceType, MyIndexableType /* : MyForwardCollectionType */
    // FIXME: where IndexRange.Generator.Element == Index
    // FIXME: where IndexRange.Index == Index
    = DefaultForwardIndexRange<Self>

  var startIndex: Index { get }
  var endIndex: Index { get }
  subscript(i: Index) -> Generator.Element { get }
  subscript(bounds: MyRange<Index>) -> SubSequence { get }

  init(from handle: UnownedHandle)
  var unownedHandle: UnownedHandle { get }

  @warn_unused_result
  func next(i: Index) -> Index

  @warn_unused_result
  func advance(i: Index, by: Index.Distance) -> Index

  @warn_unused_result
  func advance(i: Index, by: Index.Distance, limit: Index) -> Index

  @warn_unused_result
  func distanceFrom(start: Index, to: Index) -> Index.Distance

  func _failEarlyRangeCheck(index: Index, bounds: MyRange<Index>)

  func _failEarlyRangeCheck2(
    rangeStart: Index, rangeEnd: Index, boundsStart: Index, boundsEnd: Index)

  var indices: IndexRange { get }

  @warn_unused_result
  func _customIndexOfEquatableElement(element: Generator.Element) -> Index??

  var first: Generator.Element? { get }

  var isEmpty: Bool { get }

  var count: Index.Distance { get }
}
extension MyForwardCollectionType
  // FIXME: this constraint shouldn't be necessary.
  where IndexRange.Index == Index
  {

  // FIXME: do we want this overload?  Would we provide such an overload
  // for every method that accepts ranges of indices?
  // FIXME: can we have a generic subscript on MyIndexRangeType instead?
  public subscript(bounds: IndexRange) -> SubSequence {
    return self[MyRange(start: bounds.startIndex, end: bounds.endIndex)]
  }
}

extension MyForwardCollectionType {
  /// Do not use this method directly; call advancedBy(n) instead.
  @inline(__always)
  @warn_unused_result
  internal func _advanceForward(i: Index, by n: Index.Distance) -> Index {
    _precondition(n >= 0,
      "Only BidirectionalIndexType can be advanced by a negative amount")

    var i = i
    for var offset: Index.Distance = 0; offset != n; offset = offset + 1 {
      _nextInPlace(&i)
    }
    return i
  }

  /// Do not use this method directly; call advancedBy(n, limit) instead.
  @inline(__always)
  @warn_unused_result
  internal func _advanceForward(
    i: Index, by n: Index.Distance, limit: Index
  ) -> Index {
    _precondition(n >= 0,
      "Only BidirectionalIndexType can be advanced by a negative amount")

    var i = i
    for var offset: Index.Distance = 0; offset != n && i != limit; offset = offset + 1 {
      _nextInPlace(&i)
    }
    return i
  }

  @warn_unused_result
  public func advance(i: Index, by n: Index.Distance) -> Index {
    return self._advanceForward(i, by: n)
  }

  @warn_unused_result
  public func advance(i: Index, by n: Index.Distance, limit: Index) -> Index {
    return self._advanceForward(i, by: n, limit: limit)
  }

  @warn_unused_result
  public func distanceFrom(start: Index, to end: Index) -> Index.Distance {
    var start = start
    var count: Index.Distance = 0
    while start != end {
      count = count + 1
      _nextInPlace(&start)
    }
    return count
  }

  public func _failEarlyRangeCheck(
    index: Index, bounds: MyRange<Index>) {
    // Can't perform range checks in O(1) on forward indices.
  }

  public func _failEarlyRangeCheck2(
    rangeStart: Index, rangeEnd: Index, boundsStart: Index, boundsEnd: Index
  ) {
    // Can't perform range checks in O(1) on forward indices.
  }

  @warn_unused_result
  public func _customIndexOfEquatableElement(
    element: Generator.Element
  ) -> Index?? {
    return nil
  }

  public var first: Generator.Element? {
    return isEmpty ? nil : self[startIndex]
  }

  public var isEmpty: Bool {
    return startIndex == endIndex
  }

  public var count: Index.Distance {
    return distanceFrom(startIndex, to: endIndex)
  }
}
extension MyForwardCollectionType
  where Generator == DefaultGenerator<Self> {

  public func generate() -> DefaultGenerator<Self> {
    return DefaultGenerator(self)
  }
}
extension MyForwardCollectionType
  where UnownedHandle == Self
  // where UnownedHandle == DefaultUnownedForwardCollection<Self>
{

  public init(from handle: UnownedHandle) {
    self = handle
  }

  public var unownedHandle: UnownedHandle {
    return self
  }
}
extension MyForwardCollectionType
  where SubSequence == MySlice<Self> {

  public subscript(bounds: MyRange<Index>) -> SubSequence {
    return MySlice(base: self, start: bounds.startIndex, end: bounds.endIndex)
  }
}
extension MyForwardCollectionType
  where IndexRange == DefaultForwardIndexRange<Self> {

  public var indices: IndexRange {
    return DefaultForwardIndexRange(
      collection: self, startIndex: startIndex, endIndex: endIndex)
  }
}
extension MyForwardCollectionType
  where Index : MyRandomAccessIndex {

  @warn_unused_result
  public func next(i: Index) -> Index {
    return advance(i, by: 1)
  }

  @warn_unused_result
  public func advance(i: Index, by n: Index.Distance) -> Index {
    _precondition(n >= 0,
      "Can't advance an Index of MyForwardCollectionType by a negative amount")
    return i.advancedBy(n)
  }

  @warn_unused_result
  public func advance(i: Index, by n: Index.Distance, limit: Index) -> Index {
    _precondition(n >= 0,
      "Can't advance an Index of MyForwardCollectionType by a negative amount")
    let d = i.distanceTo(limit)
    _precondition(d >= 0,
      "The specified limit is behind the index")
    if d <= n {
      return limit
    }
    return i.advancedBy(n)
  }
}
extension MyForwardCollectionType
  where
  Generator.Element : Equatable,
  IndexRange.Generator.Element == Index // FIXME
{
  public func indexOf(element: Generator.Element) -> Index? {
    if let result = _customIndexOfEquatableElement(element) {
      return result
    }
    for i in OldSequence(self.indices) {
      if self[i] == element {
        return i
      }
    }
    return nil
  }

  public func indexOf_optimized(element: Generator.Element) -> Index? {
    if let result = _customIndexOfEquatableElement(element) {
      return result
    }
    var i = startIndex
    while i != endIndex {
      if self[i] == element {
        return i
      }
      _nextInPlace(&i)
    }
    return nil
  }
}
extension MyForwardCollectionType
  where SubSequence == Self {

  @warn_unused_result
  public mutating func popFirst() -> Generator.Element? {
    guard !isEmpty else { return nil }
    let element = first!
    self = self[MyRange(start: self.next(startIndex), end: endIndex)]
    return element
  }
}

public protocol MyBidirectionalCollectionType : MyForwardCollectionType {
  @warn_unused_result
  func previous(i: Index) -> Index

  func _previousInPlace(inout i: Index)
}

extension MyBidirectionalCollectionType {
  @inline(__always)
  public func _previousInPlace(inout i: Index) {
    i = previous(i)
  }

  @warn_unused_result
  public func advance(i: Index, by n: Index.Distance) -> Index {
    if n >= 0 {
      return _advanceForward(i, by: n)
    }
    var i = i
    for var offset: Index.Distance = n; offset != 0; offset = offset + 1 {
      _previousInPlace(&i)
    }
    return i
  }

  @warn_unused_result
  public func advance(i: Index, by n: Index.Distance, limit: Index) -> Index {
    if n >= 0 {
      return _advanceForward(i, by: n, limit: limit)
    }
    var i = i
    for var offset: Index.Distance = n; offset != 0 && i != limit;
        offset = offset + 1 {
      _previousInPlace(&i)
    }
    return i
  }
}
extension MyBidirectionalCollectionType
  where Index : MyRandomAccessIndex {

  @warn_unused_result
  public func previous(i: Index) -> Index {
    return advance(i, by: -1)
  }

  @warn_unused_result
  public func advance(i: Index, by n: Index.Distance) -> Index {
    return i.advancedBy(n)
  }

  @warn_unused_result
  public func advance(i: Index, by n: Index.Distance, limit: Index) -> Index {
    let d = i.distanceTo(limit)
    if d == 0 || (d > 0 ? d <= n : d >= n) {
      return limit
    }
    return i.advancedBy(n)
  }
}

public protocol MyRandomAccessCollectionType : MyBidirectionalCollectionType {
  typealias Index : MyRandomAccessIndex
}

public struct DefaultUnownedForwardCollection<Collection : MyForwardCollectionType> {
  internal let _collection: Collection

  public init(_ collection: Collection) {
    self._collection = collection
  }
}
public struct DefaultForwardIndexRange<Collection : MyIndexableType /* MyForwardCollectionType */>
  : MyForwardCollectionType, MyIndexRangeType
{
  internal let _unownedCollection: Collection.UnownedHandle
  public let startIndex: Collection.Index
  public let endIndex: Collection.Index

  // FIXME: remove explicit typealiases.
  public typealias _Element = Collection.Index
  public typealias Generator = DefaultForwardIndexRangeGenerator<Collection>
  public typealias Index = Collection.Index
  public typealias SubSequence = DefaultForwardIndexRange<Collection>
  public typealias UnownedHandle = DefaultForwardIndexRange<Collection>

  public init(
    collection: Collection,
    startIndex: Collection.Index,
    endIndex: Collection.Index
  ) {
    self._unownedCollection = collection.unownedHandle
    self.startIndex = startIndex
    self.endIndex = endIndex
  }

  // FIXME: use DefaultGenerator when the type checker bug is fixed.
  public func generate() -> Generator {
    return DefaultForwardIndexRangeGenerator(
      Collection(from: _unownedCollection),
      start: startIndex,
      end: endIndex)
  }

  public subscript(i: Collection.Index) -> Collection.Index {
    return i
  }

  public subscript(bounds: MyRange<Index>) -> DefaultForwardIndexRange<Collection> {
    fatalError("implement")
  }

  public init(from handle: UnownedHandle) {
    self = handle
  }

  public var unownedHandle: UnownedHandle {
    return self
  }

  @warn_unused_result
  public func next(i: Index) -> Index {
    return Collection(from: _unownedCollection).next(i)
  }
}

// FIXME: use DefaultGenerator when the type checker bug is fixed.
public struct DefaultForwardIndexRangeGenerator<Collection : MyIndexableType /* MyForwardCollectionType */>
  : MyGeneratorType
{
  internal let _collection: Collection
  internal var _i: Collection.Index
  internal var _endIndex: Collection.Index

  public init(
    _ collection: Collection,
    start: Collection.Index,
    end: Collection.Index
  ) {
    self._collection = collection
    self._i = collection.startIndex
    self._endIndex = end
  }

  public mutating func next() -> Collection.Index? {
    if _i == _endIndex {
      return nil
    }
    let result = _i
    _i = _collection.next(_i)
    return result
  }
}

public struct MyRange<Index : MyIndexType> : MyIndexRangeType {
  public let startIndex: Index
  public let endIndex: Index

  public init(start: Index, end: Index) {
    self.startIndex = start
    self.endIndex = end
  }

  public subscript(i: Index) -> Index {
    return i
  }
}

// FIXME: in order for all this to be usable, we need to unify MyRange and
// MyHalfOpenInterval.  We can do that by constraining the Bound to comparable,
// and providing a conditional conformance to collection when the Bound is
// strideable.
public struct MyHalfOpenInterval<Bound : Comparable> {
  public let start: Bound
  public let end: Bound
}
public struct MySlice<Collection : MyIndexableType /* : MyForwardCollectionType */>
  : MyForwardCollectionType
//  : MyIndexableType
{
  internal let _base: Collection
  public let startIndex: Collection.Index
  public let endIndex: Collection.Index
  // FIXME: remove explicit typealiases.
  public typealias _Element = Collection._Element
  public typealias Index = Collection.Index
  public init(
    base: Collection, start: Collection.Index, end: Collection.Index) {
    self._base = base
    self.startIndex = start
    self.endIndex = end
  }
  public subscript(i: Collection.Index) -> Collection._Element {
    _base._failEarlyRangeCheck(
      i, bounds: MyRange(start: startIndex, end: endIndex))
    return _base[i]
  }

  public typealias Generator = DefaultGenerator<MySlice>
  public func generate() -> Generator {
    return DefaultGenerator(self)
  }

  public typealias SubSequence = MySlice
  public subscript(bounds: MyRange<Index>) -> SubSequence {
    _base._failEarlyRangeCheck2(
      bounds.startIndex, rangeEnd: bounds.endIndex,
      boundsStart: startIndex, boundsEnd: endIndex)
    return MySlice(base: _base, start: bounds.startIndex, end: bounds.endIndex)
  }

  @warn_unused_result
  public func next(i: Index) -> Index {
    return _base.next(i)
  }

  public func _failEarlyRangeCheck(index: Index, bounds: MyRange<Index>) {
    fatalError("FIXME")
  }

  public func _failEarlyRangeCheck2(
    rangeStart: Index, rangeEnd: Index, boundsStart: Index, boundsEnd: Index) {
    fatalError("FIXME")
  }

  // FIXME: use Collection.UnownedHandle instead.
  public typealias UnownedHandle = MySlice
  public var unownedHandle: UnownedHandle { return self }
  public init(from handle: UnownedHandle) {
    self = handle
  }

  // FIXME: use DefaultForwardIndexRange instead.
  public typealias IndexRange = MySliceIndexRange<MySlice>
  public var indices: IndexRange {
    return MySliceIndexRange(
      collection: self, startIndex: startIndex, endIndex: endIndex)
  }
}
public struct MySliceIndexRange<Collection : MyIndexableType /* MyForwardCollectionType */>
  //: MyForwardCollectionType
  : MyIndexRangeType, MySequenceType, MyIndexableType
{
  internal let _unownedCollection: Collection.UnownedHandle
  public let startIndex: Collection.Index
  public let endIndex: Collection.Index

  // FIXME: remove explicit typealiases.
  public typealias _Element = Collection.Index
  public typealias Generator = DefaultForwardIndexRangeGenerator<Collection>
  public typealias Index = Collection.Index
  public typealias SubSequence = MySliceIndexRange<Collection>
  public typealias UnownedHandle = MySliceIndexRange<Collection>

  public init(
    collection: Collection,
    startIndex: Collection.Index,
    endIndex: Collection.Index
  ) {
    self._unownedCollection = collection.unownedHandle
    self.startIndex = startIndex
    self.endIndex = endIndex
  }

  public func generate() -> Generator {
    return DefaultForwardIndexRangeGenerator(
      Collection(from: _unownedCollection),
      start: startIndex,
      end: endIndex)
  }

  public subscript(i: Collection.Index) -> Collection.Index {
    return i
  }

  public subscript(bounds: MyRange<Index>) -> MySliceIndexRange<Collection> {
    fatalError("implement")
  }

  public init(from handle: UnownedHandle) {
    self = handle
  }

  public var unownedHandle: UnownedHandle {
    return self
  }

  @warn_unused_result
  public func next(i: Index) -> Index {
    return Collection(from: _unownedCollection).next(i)
  }

  public func _failEarlyRangeCheck(index: Index, bounds: MyRange<Index>) {
  }

  public func _failEarlyRangeCheck2(
    rangeStart: Index, rangeEnd: Index, boundsStart: Index, boundsEnd: Index) {
  }

  public typealias IndexRange = MySliceIndexRange
  public var indices: IndexRange {
    return self
  }
}

public struct MyMutableSlice<Collection : MyMutableCollectionType> {}
public struct MyAnyGenerator<Element> : MyGeneratorType {
  public init<G : MyGeneratorType>(_ g: G) {
    fatalError("FIXME")
  }
  public mutating func next() -> Element? {
    fatalError("FIXME")
  }
}
public struct MyAnySequence<Element> : MySequenceType {
  public typealias SubSequence = MyAnySequence<Element>
  public init<S : MySequenceType>(_ s: S) {
    fatalError("FIXME")
  }
  public func generate() -> MyAnyGenerator<Element> {
    fatalError("FIXME")
  }
}
public struct DefaultGenerator<Collection : MyIndexableType>
  : MyGeneratorType {
  internal let _collection: Collection
  internal var _i: Collection.Index

  public init(_ collection: Collection) {
    self._collection = collection
    self._i = collection.startIndex
  }

  public mutating func next() -> Collection._Element? {
    if _i == _collection.endIndex {
      return nil
    }
    let result = _collection[_i]
    _i = _collection.next(_i)
    return result
  }
}
public protocol MyMutableCollectionType : MyForwardCollectionType {
  typealias SubSequence : MyForwardCollectionType = MyMutableSlice<Self>
  subscript(i: Index) -> Generator.Element { get set }
}

public protocol MyIndexType : Equatable {
  // Move to CollectionType?
  typealias Distance : SignedIntegerType = Int
}
public protocol MyIndexRangeType : Equatable {
  typealias Index : MyIndexType
  var startIndex: Index { get }
  var endIndex: Index { get }
}

public func == <IR : MyIndexRangeType> (lhs: IR, rhs: IR) -> Bool {
  return
    lhs.startIndex == rhs.startIndex &&
    lhs.endIndex == rhs.endIndex
}

/*
public protocol MyRandomAccessIndexType : MyBidirectionalIndexType, MyStrideable,
  _RandomAccessAmbiguity {

  @warn_unused_result
  func distanceTo(other: Self) -> Distance

  @warn_unused_result
  func advancedBy(n: Distance) -> Self

  @warn_unused_result
  func advancedBy(n: Distance, limit: Self) -> Self
}

extension MyRandomAccessIndexType {
  public func _failEarlyRangeCheck(index: Self, bounds: MyRange<Self>) {
    _precondition(
      bounds.startIndex <= index,
      "index is out of bounds: index designates a position before bounds.startIndex")
    _precondition(
      index < bounds.endIndex,
      "index is out of bounds: index designates the bounds.endIndex position or a position after it")
  }

  public func _failEarlyRangeCheck2(
    rangeStart: Self, rangeEnd: Self, boundsStart: Self, boundsEnd: Self
  ) {
    let range = MyRange(startIndex: rangeStart, endIndex: rangeEnd)
    let bounds = MyRange(startIndex: boundsStart, endIndex: boundsEnd)
    _precondition(
      bounds.startIndex <= range.startIndex,
      "range.startIndex is out of bounds: index designates a position before bounds.startIndex")
    _precondition(
      bounds.startIndex <= range.endIndex,
      "range.endIndex is out of bounds: index designates a position before bounds.startIndex")

    _precondition(
      range.startIndex <= bounds.endIndex,
      "range.startIndex is out of bounds: index designates a position after bounds.endIndex")
    _precondition(
      range.endIndex <= bounds.endIndex,
      "range.startIndex is out of bounds: index designates a position after bounds.endIndex")
  }

  @transparent
  @warn_unused_result
  public func advancedBy(n: Distance, limit: Self) -> Self {
    let d = self.distanceTo(limit)
    if d == 0 || (d > 0 ? d <= n : d >= n) {
      return limit
    }
    return self.advancedBy(n)
  }
}
*/
//------------

public protocol MyStrideable : Comparable {
  typealias Distance : SignedNumberType

  @warn_unused_result
  func distanceTo(other: Self) -> Distance

  @warn_unused_result
  func advancedBy(n: Distance) -> Self
}

public protocol MyRandomAccessIndex : MyIndexType, MyStrideable {}

extension Int : MyIndexType {}
extension Int : MyStrideable {}
extension Int : MyRandomAccessIndex {}

//------------------------------------------------------------------------
// Array

public struct MyArray<Element> : MyForwardCollectionType {
  internal var _elements: [Element] = []

  init() {}
  init(_ elements: [Element]) {
    self._elements = elements
  }

  public var startIndex: Int {
    return _elements.startIndex
  }
  public var endIndex: Int {
    return _elements.endIndex
  }
  public subscript(i: Int) -> Element {
    return _elements[i]
  }
}

//------------------------------------------------------------------------
// Simplest Forward Collection

public struct MySimplestForwardCollection<Element> : MyForwardCollectionType {
  internal let _elements: [Element]

  public init(_ elements: [Element]) {
    self._elements = elements
  }

  public var startIndex: MySimplestForwardCollectionIndex {
    return MySimplestForwardCollectionIndex(_elements.startIndex)
  }

  public var endIndex: MySimplestForwardCollectionIndex {
    return MySimplestForwardCollectionIndex(_elements.endIndex)
  }

  @warn_unused_result
  public func next(i: MySimplestForwardCollectionIndex) -> MySimplestForwardCollectionIndex {
    return MySimplestForwardCollectionIndex(i._index + 1)
  }

  public subscript(i: MySimplestForwardCollectionIndex) -> Element {
    return _elements[i._index]
  }
}

public struct MySimplestForwardCollectionIndex : MyIndexType {
  internal let _index: Int
  internal init(_ index: Int) {
    self._index = index
  }
}

public func == (
  lhs: MySimplestForwardCollectionIndex,
  rhs: MySimplestForwardCollectionIndex
) -> Bool {
  return lhs._index == rhs._index
}

//------------------------------------------------------------------------
// Simplest Bidirectional Collection

public struct MySimplestBidirectionalCollection<Element> : MyBidirectionalCollectionType {
  internal let _elements: [Element]

  public init(_ elements: [Element]) {
    self._elements = elements
  }

  public var startIndex: MySimplestBidirectionalCollectionIndex {
    return MySimplestBidirectionalCollectionIndex(_elements.startIndex)
  }

  public var endIndex: MySimplestBidirectionalCollectionIndex {
    return MySimplestBidirectionalCollectionIndex(_elements.endIndex)
  }

  @warn_unused_result
  public func next(i: MySimplestBidirectionalCollectionIndex) -> MySimplestBidirectionalCollectionIndex {
    return MySimplestBidirectionalCollectionIndex(i._index + 1)
  }

  @warn_unused_result
  public func previous(i: MySimplestBidirectionalCollectionIndex) -> MySimplestBidirectionalCollectionIndex {
    return MySimplestBidirectionalCollectionIndex(i._index - 1)
  }

  public subscript(i: MySimplestBidirectionalCollectionIndex) -> Element {
    return _elements[i._index]
  }
}

public struct MySimplestBidirectionalCollectionIndex : MyIndexType {
  internal let _index: Int
  internal init(_ index: Int) {
    self._index = index
  }
}

public func == (
  lhs: MySimplestBidirectionalCollectionIndex,
  rhs: MySimplestBidirectionalCollectionIndex
) -> Bool {
  return lhs._index == rhs._index
}

//------------------------------------------------------------------------
// Simplest Bidirectional Collection

public struct MySimplestRandomAccessCollection<Element> : MyRandomAccessCollectionType {
  internal let _elements: [Element]

  public init(_ elements: [Element]) {
    self._elements = elements
  }

  // FIXME: 'typealias Index' should be inferred.
  public typealias Index = MySimplestRandomAccessCollectionIndex

  public var startIndex: MySimplestRandomAccessCollectionIndex {
    return MySimplestRandomAccessCollectionIndex(_elements.startIndex)
  }

  public var endIndex: MySimplestRandomAccessCollectionIndex {
    return MySimplestRandomAccessCollectionIndex(_elements.endIndex)
  }

  public subscript(i: MySimplestRandomAccessCollectionIndex) -> Element {
    return _elements[i._index]
  }
}

public struct MySimplestRandomAccessCollectionIndex : MyRandomAccessIndex {
  internal let _index: Int
  internal init(_ index: Int) {
    self._index = index
  }

  @warn_unused_result
  public func distanceTo(other: MySimplestRandomAccessCollectionIndex) -> Int {
    return other._index - _index
  }

  @warn_unused_result
  public func advancedBy(n: Int) -> MySimplestRandomAccessCollectionIndex {
    return MySimplestRandomAccessCollectionIndex(_index + n)
  }
}

public func == (
  lhs: MySimplestRandomAccessCollectionIndex,
  rhs: MySimplestRandomAccessCollectionIndex
) -> Bool {
  return lhs._index == rhs._index
}

public func < (
  lhs: MySimplestRandomAccessCollectionIndex,
  rhs: MySimplestRandomAccessCollectionIndex
) -> Bool {
  return lhs._index < rhs._index
}

//------------------------------------------------------------------------

// FIXME: how does AnyCollection look like in the new scheme?

import StdlibUnittest

// Also import modules which are used by StdlibUnittest internally. This
// workaround is needed to link all required libraries in case we compile
// StdlibUnittest with -sil-serialize-all.
import SwiftPrivate
#if _runtime(_ObjC)
import ObjectiveC
#endif

var NewCollection = TestSuite("NewCollection")

NewCollection.test("indexOf") {
  expectEqual(1, MyArray([1,2,3]).indexOf(2))
  expectEmpty(MyArray([1,2,3]).indexOf(42))
}

NewCollection.test("first") {
  expectOptionalEqual(1, MyArray([1,2,3]).first)
  expectEmpty(MyArray<Int>().first)
}

NewCollection.test("count") {
  expectEqual(3, MyArray([1,2,3]).count)
  expectEqual(0, MyArray<Int>().count)
}

NewCollection.test("isEmpty") {
  expectFalse(MyArray([1,2,3]).isEmpty)
  expectTrue(MyArray<Int>().isEmpty)
}

NewCollection.test("popFirst") {
  let c = MyArray([1,2,3])
  var s0 = c[c.indices]
  var s = c[MyRange(start: c.startIndex, end: c.endIndex)]
  expectOptionalEqual(1, s.popFirst())
  expectOptionalEqual(2, s.popFirst())
  expectOptionalEqual(3, s.popFirst())
  expectEmpty(s.popFirst())
}

runAllTests()

