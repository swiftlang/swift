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
// Swift standard library defines three kinds of collection indices:
// forward, bidirectional and random access.  A collection uses one of
// these indices based on the capabilities of the backing data
// structure.  For example, a singly-linked list can only have forward
// indices, a tree with parent pointers has bidirectional indices, and
// Array and Deque has random access indices.
//
// It turned out that in practice, every one of the non-random-access
// indices holds a reference to the collection it traverses, or to
// some part of it, to implement `.successor()` and `.predecessor()`.
// This introduces extra complexity in implementations and presumably
// translates into less-efficient code that does reference counting on
// indices.  Indices referencing collections also conflicts with COW
// -- a live index makes a collection non-uniquely referenced, causing
// unnecessary copies (see `Dictionary` and `Set`, that have to use a
// double-indirection trick to avoid these extra copies).  We should
// consider other schemes that don't require these tricks.
//
// Eliminating all reference-countable members from indices implies that
// indices need to essentially encode the path to the element within the data
// structure.  Since one is free to choose the encoding, we think that it
// should be possible to choose it in such a way that indices are cheaply
// comparable.
//
// In this new model, indices don't have any method or property
// requirements (these APIs were moved to Collection), so index
// protocols were eliminated.  Instead, we are introducing
// `CollectionType`, `BidirectionalCollectionType` and
// `RandomAccessCollectionType`.  These protocols naturally compose
// with `MutableCollectionType` and `RangeReplaceableCollectionType`:
//
//     protocol SequenceType {}
//     protocol CollectionType : SequenceType {}
//
//       protocol MutableCollectionType : CollectionType {}
//       protocol RangeReplaceableCollectionType : CollectionType {}
//
//       protocol BidirectionalCollectionType : CollectionType {}
//         protocol RandomAccessCollectionType : BidirectionalCollectionType {}
//
// Proposed Solution
// =================
//
// Change indices so that they can't be moved forward or backward by
// themselves (`i.successor()` is not allowed).  Then indices can
// store the minimal amount of information only about the element
// position in the collection.  Usually index can be represented as
// one or a couple of integers that encode the "path" in the
// data structure from the root to the element.  In this
// representation, only a collection can move indices (e.g.,
// `c.next(i)`).
//
// Advantages:
// * indices don't need to keep a reference to the collection.
//   - indices are simpler to implement.
//   - indices are not reference-countable, and thus cheaper to
//     handle.
// * the hierarchy of index protocols is removed, and instead we add
//   protocols for forward, bidirectional and random-access
//   collections.  This is closer to how people generally talk about
//   collections.  Writing a generic constraint for bidirectional and
//   random-access collections becomes simpler.
//
// Disadvantages:
// * a value-typed linked list can't conform to CollectionType.  A
//   reference-typed one can.

// Issues
// ======
//
// 1. Conflicting requirements for `MyRange`:
//
// * range bounds need to be comparable and incrementable, in order for
//   `MyRange` to conform to `MyForwardCollectionType`,
//
// * we frequently want to use `MyRange` as a "transport" data type, just
//   to carry a pair of indices around.  Indices are neither comparable nor
//   incrementable.
//
// Possible solution: conditional conformance for `MyRange` to
// `MyForwardCollectionType` when the bounds are comparable and
// incrementable (when the bounds conform to
// `MyRandomAccessCollectionType`?).
//
// 2. We can't specify constraints on associated types.  This forces many
//    trivial algorithms to specify useless constraints.

infix operator  ...* { associativity none precedence 135 }
infix operator  ..<* { associativity none precedence 135 }

public protocol MyGeneratorType {
  associatedtype Element
  mutating func next() -> Element?
}
public protocol MySequenceType {
  associatedtype Generator : MyGeneratorType
  associatedtype SubSequence /* : MySequenceType */

  func generate() -> Generator

  @warn_unused_result
  func map<T>(
    @noescape transform: (Generator.Element) throws -> T
  ) rethrows -> [T]

  @warn_unused_result
  func dropFirst(n: Int) -> SubSequence

  @warn_unused_result
  func dropLast(n: Int) -> SubSequence

  @warn_unused_result
  func prefix(maxLength: Int) -> SubSequence

  @warn_unused_result
  func suffix(maxLength: Int) -> SubSequence
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

  @warn_unused_result
  public func dropFirst(n: Int) -> SubSequence {
    _precondition(n >= 0, "Can't drop a negative number of elements from a collection")
    fatalError("implement")
  }

  @warn_unused_result
  public func dropLast(n: Int) -> SubSequence {
    _precondition(n >= 0, "Can't drop a negative number of elements from a collection")
    fatalError("implement")
  }

  @warn_unused_result
  public func prefix(maxLength: Int) -> SubSequence {
    _precondition(maxLength >= 0, "Can't take a prefix of negative length from a collection")
    fatalError("implement")
  }

  @warn_unused_result
  public func suffix(maxLength: Int) -> SubSequence {
    _precondition(maxLength >= 0, "Can't take a suffix of negative length from a collection")
    fatalError("implement")
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
  associatedtype Index : Comparable
  associatedtype _Element
  associatedtype UnownedHandle
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
  associatedtype Generator = DefaultGenerator<Self>
  associatedtype Index : Comparable
  associatedtype SubSequence : MySequenceType /* : MyForwardCollectionType */
    = MySlice<Self>
  associatedtype UnownedHandle = Self // DefaultUnownedForwardCollection<Self>
  associatedtype IndexRange : MyIndexRangeType, MySequenceType, MyIndexableType /* : MyForwardCollectionType */
    // FIXME: where IndexRange.Generator.Element == Index
    // FIXME: where IndexRange.Index == Index
    = DefaultForwardIndexRange<Self>
  associatedtype IndexDistance : SignedIntegerType = Int

  var startIndex: Index { get }
  var endIndex: Index { get }
  subscript(i: Index) -> Generator.Element { get }
  subscript(bounds: MyRange<Index>) -> SubSequence { get }

  init(from handle: UnownedHandle)
  var unownedHandle: UnownedHandle { get }

  @warn_unused_result
  func next(i: Index) -> Index

  @warn_unused_result
  func advance(i: Index, by: IndexDistance) -> Index

  @warn_unused_result
  func advance(i: Index, by: IndexDistance, limit: Index) -> Index

  @warn_unused_result
  func distanceFrom(start: Index, to: Index) -> IndexDistance

  func _failEarlyRangeCheck(index: Index, bounds: MyRange<Index>)

  func _failEarlyRangeCheck2(
    rangeStart: Index, rangeEnd: Index, boundsStart: Index, boundsEnd: Index)

  var indices: IndexRange { get }

  @warn_unused_result
  func _customIndexOfEquatableElement(element: Generator.Element) -> Index??

  var first: Generator.Element? { get }

  var isEmpty: Bool { get }

  var count: IndexDistance { get }
}

extension MyForwardCollectionType {
  /// Do not use this method directly; call advancedBy(n) instead.
  @inline(__always)
  @warn_unused_result
  internal func _advanceForward(i: Index, by n: IndexDistance) -> Index {
    _precondition(n >= 0,
      "Only BidirectionalIndexType can be advanced by a negative amount")

    var i = i
    for var offset: IndexDistance = 0; offset != n; offset = offset + 1 {
      _nextInPlace(&i)
    }
    return i
  }

  /// Do not use this method directly; call advancedBy(n, limit) instead.
  @inline(__always)
  @warn_unused_result
  internal func _advanceForward(
    i: Index, by n: IndexDistance, limit: Index
  ) -> Index {
    _precondition(n >= 0,
      "Only BidirectionalIndexType can be advanced by a negative amount")

    var i = i
    for var offset: IndexDistance = 0; offset != n && i != limit; offset = offset + 1 {
      _nextInPlace(&i)
    }
    return i
  }

  @warn_unused_result
  public func advance(i: Index, by n: IndexDistance) -> Index {
    return self._advanceForward(i, by: n)
  }

  @warn_unused_result
  public func advance(i: Index, by n: IndexDistance, limit: Index) -> Index {
    return self._advanceForward(i, by: n, limit: limit)
  }

  @warn_unused_result
  public func distanceFrom(start: Index, to end: Index) -> IndexDistance {
    var start = start
    var count: IndexDistance = 0
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

  public var count: IndexDistance {
    return distanceFrom(startIndex, to: endIndex)
  }

  @warn_unused_result
  public func dropFirst(n: Int) -> SubSequence {
    _precondition(n >= 0, "Can't drop a negative number of elements from a collection")
/*
    let start = advance(startIndex, by: numericCast(n), limit: endIndex)
    return self[start..<endIndex]
*/
    fatalError()
  }

  @warn_unused_result
  public func dropLast(n: Int) -> SubSequence {
    _precondition(n >= 0, "Can't drop a negative number of elements from a collection")
    let amount = max(0, numericCast(count) - n)
    let end = advance(startIndex, by: numericCast(amount), limit: endIndex)
    return self[startIndex..<*end]
  }

  @warn_unused_result
  public func prefix(maxLength: Int) -> SubSequence {
    _precondition(maxLength >= 0, "Can't take a prefix of negative length from a collection")
    let end = advance(startIndex, by: numericCast(maxLength), limit: endIndex)
    return self[startIndex..<*end]
  }

  @warn_unused_result
  public func suffix(maxLength: Int) -> SubSequence {
    _precondition(maxLength >= 0, "Can't take a suffix of negative length from a collection")
    let amount = max(0, numericCast(count) - maxLength)
    let start = advance(startIndex, by: numericCast(amount), limit: endIndex)
    return self[start..<*endIndex]
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
  where
  Index : MyStrideable,
  Index.Distance == IndexDistance {

  @warn_unused_result
  public func next(i: Index) -> Index {
    return advance(i, by: 1)
  }

  @warn_unused_result
  public func advance(i: Index, by n: IndexDistance) -> Index {
    _precondition(n >= 0,
      "Can't advance an Index of MyForwardCollectionType by a negative amount")
    return i.advancedBy(n)
  }

  @warn_unused_result
  public func advance(i: Index, by n: IndexDistance, limit: Index) -> Index {
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
  public func advance(i: Index, by n: IndexDistance) -> Index {
    if n >= 0 {
      return _advanceForward(i, by: n)
    }
    var i = i
    for var offset: IndexDistance = n; offset != 0; offset = offset + 1 {
      _previousInPlace(&i)
    }
    return i
  }

  @warn_unused_result
  public func advance(i: Index, by n: IndexDistance, limit: Index) -> Index {
    if n >= 0 {
      return _advanceForward(i, by: n, limit: limit)
    }
    var i = i
    for var offset: IndexDistance = n; offset != 0 && i != limit;
        offset = offset + 1 {
      _previousInPlace(&i)
    }
    return i
  }
}
extension MyBidirectionalCollectionType
  where
  Index : MyStrideable,
  Index.Distance == IndexDistance {

  @warn_unused_result
  public func previous(i: Index) -> Index {
    return advance(i, by: -1)
  }

  @warn_unused_result
  public func advance(i: Index, by n: IndexDistance) -> Index {
    return i.advancedBy(n)
  }

  @warn_unused_result
  public func advance(i: Index, by n: IndexDistance, limit: Index) -> Index {
    let d = i.distanceTo(limit)
    if d == 0 || (d > 0 ? d <= n : d >= n) {
      return limit
    }
    return i.advancedBy(n)
  }
}

public protocol MyRandomAccessCollectionType : MyBidirectionalCollectionType {
  associatedtype Index : MyStrideable
    // FIXME: where Index.Distance == IndexDistance
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

  internal init(
    _unownedCollection: Collection.UnownedHandle,
    startIndex: Collection.Index,
    endIndex: Collection.Index
  ) {
    self._unownedCollection = _unownedCollection
    self.startIndex = startIndex
    self.endIndex = endIndex
  }

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

public struct MyRange<Index : Comparable> : MyIndexRangeType {
  public let startIndex: Index
  public let endIndex: Index

  public init(start: Index, end: Index) {
    _precondition(start <= end, "Can't form a backwards MyRange")
    self.startIndex = start
    self.endIndex = end
  }

  public subscript(i: Index) -> Index {
    return i
  }
}

public func ..<*
  <Index : Comparable>(lhs: Index, rhs: Index) -> MyRange<Index> {
  return MyRange(start: lhs, end: rhs)
}

// FIXME: replace this type with a conditional conformance on MyRange.
public struct MyIterableRange<Index : MyStrideable> :
  MyBidirectionalCollectionType {

  public let startIndex: Index
  public let endIndex: Index

  public init(start: Index, end: Index) {
    _precondition(start <= end, "Can't form a backwards MyIterableRange")
    self.startIndex = start
    self.endIndex = end
  }

  @warn_unused_result
  public func next(i: Index) -> Index {
    let result = i.advancedBy(1)
    _precondition(startIndex <= result, "can't advance past endIndex")
    return i.advancedBy(1)
  }

  @warn_unused_result
  public func previous(i: Index) -> Index {
    let result = i.advancedBy(-1)
    _precondition(result <= endIndex, "can't advance before startIndex")
    return result
  }

  public subscript(i: Index) -> Index {
    return i
  }
}

public func ..<*
  <Index : MyStrideable>(lhs: Index, rhs: Index) -> MyIterableRange<Index> {
  return MyIterableRange(start: lhs, end: rhs)
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

  internal init(
    _unownedCollection: Collection.UnownedHandle,
    startIndex: Collection.Index,
    endIndex: Collection.Index
  ) {
    self._unownedCollection = _unownedCollection
    self.startIndex = startIndex
    self.endIndex = endIndex
  }

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
  associatedtype SubSequence : MyForwardCollectionType = MyMutableSlice<Self>
  subscript(i: Index) -> Generator.Element { get set }
}

public protocol MyIndexRangeType : Equatable {
  associatedtype Index : Equatable
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

//------------------------------------------------------------------------
// Bubble sort

extension MyMutableCollectionType
  where
  IndexRange.Generator.Element == Index,
  IndexRange.Index == Index
{
  public mutating func bubbleSortInPlace(
    @noescape isOrderedBefore: (Generator.Element, Generator.Element) -> Bool
  ) {
    if isEmpty { return }
    if next(startIndex) == endIndex { return }

    while true {
      var swapped = false
      for i in OldSequence(indices) {
        if i == endIndex { break }
        let ni = next(i)
        if ni == endIndex { break }
        if isOrderedBefore(self[ni], self[i]) {
          swap(&self[i], &self[ni])
          swapped = true
        }
      }
      if !swapped {
        break
      }
    }
  }
}

extension MyMutableCollectionType
  where
  Generator.Element : Comparable,
  IndexRange.Generator.Element == Index,
  IndexRange.Index == Index
{
  public mutating func bubbleSortInPlace() {
    bubbleSortInPlace { $0 < $1 }
  }
}

//------------------------------------------------------------------------
// Bubble sort

extension MyRandomAccessCollectionType
  where
  IndexRange.Generator.Element == Index,
  IndexRange.Index == Index
{
  public func lowerBoundOf(
    element: Generator.Element,
    @noescape isOrderedBefore: (Generator.Element, Generator.Element) -> Bool
  ) -> Index {
    var low = startIndex
    var subrangeCount = count
    while subrangeCount != 0 {
      let midOffset = subrangeCount / 2
      let mid = advance(low, by: midOffset)
      if isOrderedBefore(self[mid], element) {
        low = next(mid)
        subrangeCount -= midOffset + 1
      } else {
        subrangeCount = midOffset
      }
    }
    return low
  }
}

extension MyRandomAccessCollectionType
  where
  Generator.Element : Comparable,
  IndexRange.Generator.Element == Index,
  IndexRange.Index == Index
{
  public func lowerBoundOf(element: Generator.Element) -> Index {
    return lowerBoundOf(element) { $0 < $1 }
  }
}

//------------

public protocol MyStrideable : Comparable {
  associatedtype Distance : SignedNumberType

  @warn_unused_result
  func distanceTo(other: Self) -> Distance

  @warn_unused_result
  func advancedBy(n: Distance) -> Self
}

extension Int : MyStrideable {}

//------------------------------------------------------------------------
// Array

public struct MyArray<Element> :
  MyForwardCollectionType,
  MyRandomAccessCollectionType,
  MyMutableCollectionType
{
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
    get {
      return _elements[i]
    }
    set {
      _elements[i] = newValue
    }
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

public struct MySimplestForwardCollectionIndex : Comparable {
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

public func < (
  lhs: MySimplestForwardCollectionIndex,
  rhs: MySimplestForwardCollectionIndex
) -> Bool {
  return lhs._index < rhs._index
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

public struct MySimplestBidirectionalCollectionIndex : Comparable {
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

public func < (
  lhs: MySimplestBidirectionalCollectionIndex,
  rhs: MySimplestBidirectionalCollectionIndex
) -> Bool {
  return lhs._index < rhs._index
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

public struct MySimplestRandomAccessCollectionIndex : MyStrideable {
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
// Simplest Strideable

public struct MySimplestStrideable : MyStrideable {
  internal let _value: Int
  internal init(_ value: Int) {
    self._value = value
  }

  @warn_unused_result
  public func distanceTo(other: MySimplestStrideable) -> Int {
    return _value.distanceTo(other._value)
  }

  @warn_unused_result
  public func advancedBy(n: Int) -> MySimplestStrideable {
    return MySimplestStrideable(_value.advancedBy(n))
  }
}

public func == (
  lhs: MySimplestStrideable,
  rhs: MySimplestStrideable
) -> Bool {
  return lhs._value == rhs._value
}

public func < (
  lhs: MySimplestStrideable,
  rhs: MySimplestStrideable
) -> Bool {
  return lhs._value < rhs._value
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

NewCollection.test("bubbleSortInPlace") {
  var a = MyArray([4,3,2,1])
  a.bubbleSortInPlace()
  expectEqual([1,2,3,4], a._elements)
}

NewCollection.test("lowerBoundOf/empty") {
  var a = MyArray<Int>([])
  expectEqual(0, a.lowerBoundOf(3))
}

NewCollection.test("lowerBoundOf/one") {
  var a = MyArray<Int>([10])
  expectEqual(0, a.lowerBoundOf(9))
  expectEqual(0, a.lowerBoundOf(10))
  expectEqual(1, a.lowerBoundOf(11))
}

NewCollection.test("lowerBoundOf") {
  var a = MyArray([1,2,2,3,3,3,3,3,3,3,3,4,5,6,7])
  expectEqual(3, a.lowerBoundOf(3))
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
  var s0 = c[c.startIndex..<*c.endIndex]
  var s = c[MyRange(start: c.startIndex, end: c.endIndex)]
  expectOptionalEqual(1, s.popFirst())
  expectOptionalEqual(2, s.popFirst())
  expectOptionalEqual(3, s.popFirst())
  expectEmpty(s.popFirst())
}

NewCollection.test("RangeLiterals") {
  let comparable = MinimalComparableValue(0)
  let strideable = MySimplestStrideable(0)

  var comparableRange = comparable..<*comparable
  expectType(MyRange<MinimalComparableValue>.self, &comparableRange)

  var strideableRange = strideable..<*strideable
  expectType(MyIterableRange<MySimplestStrideable>.self, &strideableRange)

  for _ in OldSequence(0..<*10) {}
}

runAllTests()

