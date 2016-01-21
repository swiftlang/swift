//===----------------------------------------------------------------------===//
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

/// A protocol representing the minimal requirements of
/// `Collection`.
///
/// - Note: In most cases, it's best to ignore this protocol and use
///   `Collection` instead, as it has a more complete interface.
//
// This protocol is almost an implementation detail of the standard
// library; it is used to deduce things like the `SubSequence` and
// `Iterator` type from a minimal collection, but it is also used in
// exposed places like as a constraint on IndexingIterator.
public protocol Indexable {
  /// A type that represents a valid position in the collection.
  ///
  /// Valid indices consist of the position of every element and a
  /// "past the end" position that's not valid for use as a subscript.
  associatedtype Index : ForwardIndex

  /// The position of the first element in a non-empty collection.
  ///
  /// In an empty collection, `startIndex == endIndex`.
  ///
  /// - Complexity: O(1)
  var startIndex: Index {get}

  /// The collection's "past the end" position.
  ///
  /// `endIndex` is not a valid argument to `subscript`, and is always
  /// reachable from `startIndex` by zero or more applications of
  /// `successor()`.
  ///
  /// - Complexity: O(1)
  var endIndex: Index {get}

  // The declaration of _Element and subscript here is a trick used to
  // break a cyclic conformance/deduction that Swift can't handle.  We
  // need something other than a Collection.Iterator.Element that can
  // be used as IndexingIterator<T>'s Element.  Here we arrange for
  // the Collection itself to have an Element type that's deducible from
  // its subscript.  Ideally we'd like to constrain this Element to be the same
  // as Collection.Iterator.Element (see below), but we have no way of
  // expressing it today.
  associatedtype _Element

  /// Returns the element at the given `position`.
  ///
  /// - Complexity: O(1)
  subscript(position: Index) -> _Element {get}
}

public protocol MutableIndexable {
  associatedtype Index : ForwardIndex

  var startIndex: Index {get}
  var endIndex: Index {get}

  associatedtype _Element

  subscript(position: Index) -> _Element {get set}
}

/// The iterator used for collections that don't specify one.
public struct IndexingIterator<Elements : Indexable>
 : IteratorProtocol, Sequence {

  /// Create a *iterator* over the given collection.
  public /// @testable
  init(_ elements: Elements) {
    self._elements = elements
    self._position = elements.startIndex
  }

  /// Advance to the next element and return it, or `nil` if no next
  /// element exists.
  ///
  /// - Requires: No preceding call to `self.next()` has returned `nil`.
  public mutating func next() -> Elements._Element? {
    if _position == _elements.endIndex { return nil }
    let element = _elements[_position]
    _position._successorInPlace()
    return element
  }

  internal let _elements: Elements
  internal var _position: Elements.Index
}

/// A multi-pass *sequence* with addressable positions.
///
/// Positions are represented by an associated `Index` type.  Whereas
/// an arbitrary *sequence* may be consumed as it is traversed, a
/// *collection* is multi-pass: any element may be revisited merely by
/// saving its index.
///
/// The sequence view of the elements is identical to the collection
/// view.  In other words, the following code binds the same series of
/// values to `x` as does `for x in self {}`:
///
///     for i in startIndex..<endIndex {
///       let x = self[i]
///     }
public protocol Collection : Indexable, Sequence {
  /// A type that provides the *sequence*'s iteration interface and
  /// encapsulates its iteration state.
  ///
  /// By default, a `Collection` satisfies `Sequence` by
  /// supplying a `IndexingIterator` as its associated `Iterator`
  /// type.
  associatedtype Iterator : IteratorProtocol = IndexingIterator<Self>

  // FIXME: Needed here so that the Iterator is properly deduced from
  // a custom iterator() function.  Otherwise we get an
  // IndexingIterator. <rdar://problem/21539115>
  func iterator() -> Iterator
  
  // FIXME: should be constrained to Collection
  // (<rdar://problem/20715009> Implement recursive protocol
  // constraints)
  
  /// A `Sequence` that can represent a contiguous subrange of `self`'s
  /// elements.
  ///
  /// - Note: This associated type appears as a requirement in
  ///   `Sequence`, but is restated here with stricter
  ///   constraints: in a `Collection`, the `SubSequence` should
  ///   also be a `Collection`.
  associatedtype SubSequence: Indexable, Sequence = Slice<Self>

  /// Returns the element at the given `position`.
  subscript(position: Index) -> Iterator.Element {get}

  /// Returns a collection representing a contiguous sub-range of
  /// `self`'s elements.
  ///
  /// - Complexity: O(1)
  subscript(bounds: Range<Index>) -> SubSequence {get}

  /// Returns `self[startIndex..<end]`
  ///
  /// - Complexity: O(1)
  @warn_unused_result
  func prefixUpTo(end: Index) -> SubSequence

  /// Returns `self[start..<endIndex]`
  ///
  /// - Complexity: O(1)
  @warn_unused_result
  func suffixFrom(start: Index) -> SubSequence

  /// Returns `prefixUpTo(position.successor())`
  ///
  /// - Complexity: O(1)
  @warn_unused_result
  func prefixThrough(position: Index) -> SubSequence

  /// Returns `true` iff `self` is empty.
  var isEmpty: Bool { get }

  /// Returns the number of elements.
  ///
  /// - Complexity: O(1) if `Index` conforms to `RandomAccessIndex`;
  ///   O(N) otherwise.
  var length: Index.Distance { get }
  
  // The following requirement enables dispatching for indexOf when
  // the element type is Equatable.
  
  /// Returns `Optional(Optional(index))` if an element was found;
  /// `nil` otherwise.
  ///
  /// - Complexity: O(N).
  @warn_unused_result
  func _customIndexOfEquatableElement(element: Iterator.Element) -> Index??

  /// Returns the first element of `self`, or `nil` if `self` is empty.
  var first: Iterator.Element? { get }
}

/// Supply the default `iterator()` method for `Collection` models
/// that accept the default associated `Iterator`,
/// `IndexingIterator<Self>`.
extension Collection where Iterator == IndexingIterator<Self> {
  public func iterator() -> IndexingIterator<Self> {
    return IndexingIterator(self)
  }
}

/// Supply the default "slicing" `subscript`  for `Collection` models
/// that accept the default associated `SubSequence`, `Slice<Self>`.
extension Collection where SubSequence == Slice<Self> {
  public subscript(bounds: Range<Index>) -> Slice<Self> {
    Index._failEarlyRangeCheck2(
      bounds.startIndex, rangeEnd: bounds.endIndex,
      boundsStart: startIndex, boundsEnd: endIndex)
    return Slice(_base: self, bounds: bounds)
  }
}

extension Collection where SubSequence == Self {
  /// If `!self.isEmpty`, remove the first element and return it, otherwise
  /// return `nil`.
  ///
  /// - Complexity: O(`self.length`)
  @warn_unused_result
  public mutating func popFirst() -> Iterator.Element? {
    guard !isEmpty else { return nil }
    let element = first!
    self = self[startIndex.successor()..<endIndex]
    return element
  }

  /// If `!self.isEmpty`, remove the last element and return it, otherwise
  /// return `nil`.
  ///
  /// - Complexity: O(`self.length`)
  @warn_unused_result
  public mutating func popLast() -> Iterator.Element? {
    guard !isEmpty else { return nil }
    let lastElementIndex = startIndex.advancedBy(numericCast(length) - 1)
    let element = self[lastElementIndex]
    self = self[startIndex..<lastElementIndex]
    return element
  }
}

/// Default implementations of core requirements
extension Collection {
  /// Returns `true` iff `self` is empty.
  ///
  /// - Complexity: O(1)
  public var isEmpty: Bool {
    return startIndex == endIndex
  }

  /// Returns the first element of `self`, or `nil` if `self` is empty.
  ///
  /// - Complexity: O(1)
  public var first: Iterator.Element? {
    // NB: Accessing `startIndex` may not be O(1) for some lazy collections,
    // so instead of testing `isEmpty` and then returning the first element,
    // we'll just rely on the fact that the generator always yields the
    // first element first.
    var i = iterator()
    return i.next()
  }

  /// Returns a value less than or equal to the number of elements in
  /// `self`, *nondestructively*.
  ///
  /// - Complexity: O(`length`).
  public var underestimatedLength: Int {
    return numericCast(length)
  }

  /// Returns the number of elements.
  ///
  /// - Complexity: O(1) if `Index` conforms to `RandomAccessIndex`;
  ///   O(N) otherwise.
  public var length: Index.Distance {
    return startIndex.distanceTo(endIndex)
  }

  /// Customization point for `Sequence.indexOf()`.
  ///
  /// Define this method if the collection can find an element in less than
  /// O(N) by exploiting collection-specific knowledge.
  ///
  /// - Returns: `nil` if a linear search should be attempted instead,
  ///   `Optional(nil)` if the element was not found, or
  ///   `Optional(Optional(index))` if an element was found.
  ///
  /// - Complexity: O(`length`).
  @warn_unused_result
  public // dispatching
  func _customIndexOfEquatableElement(_: Iterator.Element) -> Index?? {
    return nil
  }
}

//===----------------------------------------------------------------------===//
// Default implementations for Collection
//===----------------------------------------------------------------------===//

extension Collection {
  /// Return an `Array` containing the results of mapping `transform`
  /// over `self`.
  ///
  /// - Complexity: O(N).
  @warn_unused_result
  public func map<T>(
    @noescape transform: (Iterator.Element) throws -> T
  ) rethrows -> [T] {
    let length: Int = numericCast(self.length)
    if length == 0 {
      return []
    }

    var result = ContiguousArray<T>()
    result.reserveCapacity(length)

    var i = self.startIndex

    for _ in 0..<length {
      result.append(try transform(self[i]))
      i = i.successor()
    }

    _expectEnd(i, self)
    return Array(result)
  }

  /// Returns a subsequence containing all but the first `n` elements.
  ///
  /// - Requires: `n >= 0`
  /// - Complexity: O(`n`)
  @warn_unused_result
  public func dropFirst(n: Int) -> SubSequence {
    _require(n >= 0, "Can't drop a negative number of elements from a collection")
    let start = startIndex.advancedBy(numericCast(n), limit: endIndex)
    return self[start..<endIndex]
  }

  /// Returns a subsequence containing all but the last `n` elements.
  ///
  /// - Requires: `n >= 0`
  /// - Complexity: O(`self.length`)
  @warn_unused_result
  public func dropLast(n: Int) -> SubSequence {
    _require(
      n >= 0, "Can't drop a negative number of elements from a collection")
    let amount = Swift.max(0, numericCast(length) - n)
    let end = startIndex.advancedBy(numericCast(amount), limit: endIndex)
    return self[startIndex..<end]
  }

  /// Returns a subsequence, up to `maxLength` in length, containing the
  /// initial elements.
  ///
  /// If `maxLength` exceeds `self.length`, the result contains all
  /// the elements of `self`.
  ///
  /// - Requires: `maxLength >= 0`
  /// - Complexity: O(`maxLength`)
  @warn_unused_result
  public func prefix(maxLength: Int) -> SubSequence {
    _require(
      maxLength >= 0,
      "Can't take a prefix of negative length from a collection")
    let end = startIndex.advancedBy(numericCast(maxLength), limit: endIndex)
    return self[startIndex..<end]
  }

  /// Returns a slice, up to `maxLength` in length, containing the
  /// final elements of `self`.
  ///
  /// If `maxLength` exceeds `s.length`, the result contains all
  /// the elements of `self`.
  ///
  /// - Requires: `maxLength >= 0`
  /// - Complexity: O(`self.length`)
  @warn_unused_result
  public func suffix(maxLength: Int) -> SubSequence {
    _require(
      maxLength >= 0,
      "Can't take a suffix of negative length from a collection")
    let amount = Swift.max(0, numericCast(length) - maxLength)
    let start = startIndex.advancedBy(numericCast(amount), limit: endIndex)
    return self[start..<endIndex]
  }

  /// Returns `self[startIndex..<end]`
  ///
  /// - Complexity: O(1)
  @warn_unused_result
  public func prefixUpTo(end: Index) -> SubSequence {
    return self[startIndex..<end]
  }

  /// Returns `self[start..<endIndex]`
  ///
  /// - Complexity: O(1)
  @warn_unused_result
  public func suffixFrom(start: Index) -> SubSequence {
    return self[start..<endIndex]
  }

  /// Returns `prefixUpTo(position.successor())`
  ///
  /// - Complexity: O(1)
  @warn_unused_result
  public func prefixThrough(position: Index) -> SubSequence {
    return prefixUpTo(position.successor())
  }

  /// Returns the maximal `SubSequence`s of `self`, in order, that
  /// don't contain elements satisfying the predicate `isSeparator`.
  ///
  /// - Parameter maxSplits: The maximum number of `SubSequence`s to
  ///   return, minus 1.
  ///   If `maxSplits + 1` `SubSequence`s are returned, the last one is
  ///   a suffix of `self` containing *all* the elements of `self` following the
  ///   last split point.
  ///   The default value is `Int.max`.
  ///
  /// - Parameter omitEmptySubsequences: If `false`, an empty `SubSequence`
  ///   is produced in the result for each pair of consecutive elements
  ///   satisfying `isSeparator`.
  ///   The default value is `true`.
  ///
  /// - Requires: `maxSplit >= 0`
  @warn_unused_result
  public func split(
    maxSplits: Int = Int.max,
    omitEmptySubsequences: Bool = true,
    @noescape isSeparator: (Iterator.Element) throws -> Bool
  ) rethrows -> [SubSequence] {
    _require(maxSplits >= 0, "Must take zero or more splits")

    var result: [SubSequence] = []
    var subSequenceStart: Index = startIndex

    func appendSubsequence(end end: Index) -> Bool {
      if subSequenceStart == end && omitEmptySubsequences {
        return false
      }
      result.append(self[subSequenceStart..<end])
      return true
    }

    if maxSplits == 0 || isEmpty {
      appendSubsequence(end: endIndex)
      return result
    }

    var subSequenceEnd = subSequenceStart
    let cachedEndIndex = endIndex
    while subSequenceEnd != cachedEndIndex {
      if try isSeparator(self[subSequenceEnd]) {
        let didAppend = appendSubsequence(end: subSequenceEnd)
        subSequenceEnd._successorInPlace()
        subSequenceStart = subSequenceEnd
        if didAppend && result.length == maxSplits {
          break
        }
        continue
      }
      subSequenceEnd._successorInPlace()
    }

    if subSequenceStart != cachedEndIndex || !omitEmptySubsequences {
      result.append(self[subSequenceStart..<cachedEndIndex])
    }

    return result
  }
}

extension Collection where Iterator.Element : Equatable {
  /// Returns the maximal `SubSequence`s of `self`, in order, around a
  /// `separator` element.
  ///
  /// - Parameter maxSplits: The maximum number of `SubSequence`s to
  ///   return, minus 1.
  ///   If `maxSplit + 1` `SubSequence`s are returned, the last one is
  ///   a suffix of `self` containing *all* the elements of `self` following the
  ///   last split point.
  ///   The default value is `Int.max`.
  ///
  /// - Parameter omitEmptySubsequences: If `false`, an empty `SubSequence`
  ///   is produced in the result for each pair of consecutive elements
  ///   equal to `separator`.
  ///   The default value is `true`.
  ///
  /// - Requires: `maxSplit >= 0`
  @warn_unused_result
  public func split(
    separator: Iterator.Element,
    maxSplits: Int = Int.max,
    omitEmptySubsequences: Bool = true
  ) -> [SubSequence] {
  return split(maxSplits, omitEmptySubsequences: omitEmptySubsequences,
      isSeparator: { $0 == separator })
  }
}

extension Collection where Index : BidirectionalIndex {
  /// Returns a subsequence containing all but the last `n` elements.
  ///
  /// - Requires: `n >= 0`
  /// - Complexity: O(`n`)
  @warn_unused_result
  public func dropLast(n: Int) -> SubSequence {
    _require(
      n >= 0, "Can't drop a negative number of elements from a collection")
    let end = endIndex.advancedBy(numericCast(-n), limit: startIndex)
    return self[startIndex..<end]
  }

  /// Returns a slice, up to `maxLength` in length, containing the
  /// final elements of `self`.
  ///
  /// If `maxLength` exceeds `s.length`, the result contains all
  /// the elements of `self`.
  ///
  /// - Requires: `maxLength >= 0`
  /// - Complexity: O(`maxLength`)
  @warn_unused_result
  public func suffix(maxLength: Int) -> SubSequence {
    _require(
      maxLength >= 0,
      "Can't take a suffix of negative length from a collection")
    let start = endIndex.advancedBy(numericCast(-maxLength), limit: startIndex)
    return self[start..<endIndex]
  }
}

extension Collection where SubSequence == Self {
  /// Remove the element at `startIndex` and return it.
  ///
  /// - Complexity: O(1)
  /// - Requires: `!self.isEmpty`.
  public mutating func removeFirst() -> Iterator.Element {
    _require(!isEmpty, "can't remove items from an empty collection")
    let element = first!
    self = self[startIndex.successor()..<endIndex]
    return element
  }

  /// Remove the first `n` elements.
  ///
  /// - Complexity:
  ///   - O(1) if `Index` conforms to `RandomAccessIndex`
  ///   - O(n) otherwise
  /// - Requires: `n >= 0 && self.length >= n`.
  public mutating func removeFirst(n: Int) {
    if n == 0 { return }
    _require(n >= 0, "number of elements to remove should be non-negative")
    _require(length >= numericCast(n),
      "can't remove more items from a collection than it contains")
    self = self[startIndex.advancedBy(numericCast(n))..<endIndex]
  }
}

extension Collection
  where
  SubSequence == Self,
  Index : BidirectionalIndex {

  /// Remove an element from the end.
  ///
  /// - Complexity: O(1)
  /// - Requires: `!self.isEmpty`
  public mutating func removeLast() -> Iterator.Element {
    let element = last!
    self = self[startIndex..<endIndex.predecessor()]
    return element
  }

  /// Remove the last `n` elements.
  ///
  /// - Complexity:
  ///   - O(1) if `Index` conforms to `RandomAccessIndex`
  ///   - O(n) otherwise
  /// - Requires: `n >= 0 && self.length >= n`.
  public mutating func removeLast(n: Int) {
    if n == 0 { return }
    _require(n >= 0, "number of elements to remove should be non-negative")
    _require(length >= numericCast(n),
      "can't remove more items from a collection than it contains")
    self = self[startIndex..<endIndex.advancedBy(numericCast(-n))]
  }
}

extension Sequence
  where Self : _ArrayProtocol, Self.Element == Self.Iterator.Element {
  // A fast implementation for when you are backed by a contiguous array.
  public func _initializeTo(ptr: UnsafeMutablePointer<Iterator.Element>)
    -> UnsafeMutablePointer<Iterator.Element> {
    let s = self._baseAddressIfContiguous
    if s != nil {
      let length = self.length
      ptr.initializeFrom(s, count: length)
      _fixLifetime(self._owner)
      return ptr + length
    } else {
      var p = ptr
      for x in self {
        p.initializePointee(x)
        p += 1
      }
      return p
    }
  }
}

extension Collection {
  public func _preprocessingPass<R>(@noescape preprocess: (Self) -> R) -> R? {
    return preprocess(self)
  }
}

/// A *collection* that supports subscript assignment.
///
/// For any instance `a` of a type conforming to
/// `MutableCollection`, :
///
///     a[i] = x
///     let y = a[i]
///
/// is equivalent to:
///
///     a[i] = x
///     let y = x
///
public protocol MutableCollection : MutableIndexable, Collection {
  // FIXME: should be constrained to MutableCollection
  // (<rdar://problem/20715009> Implement recursive protocol
  // constraints)
  associatedtype SubSequence : Collection /*: MutableCollection*/
    = MutableSlice<Self>

  /// Access the element at `position`.
  ///
  /// - Requires: `position` indicates a valid position in `self` and
  ///   `position != endIndex`.
  ///
  /// - Complexity: O(1)
  subscript(position: Index) -> Iterator.Element {get set}

  /// Returns a collection representing a contiguous sub-range of
  /// `self`'s elements.
  ///
  /// - Complexity: O(1) for the getter, O(`bounds.length`) for the setter.
  subscript(bounds: Range<Index>) -> SubSequence {get set}

  /// Call `body(p)`, where `p` is a pointer to the collection's
  /// mutable contiguous storage.  If no such storage exists, it is
  /// first created.  If the collection does not support an internal
  /// representation in a form of mutable contiguous storage, `body` is not
  /// called and `nil` is returned.
  ///
  /// Often, the optimizer can eliminate bounds- and uniqueness-checks
  /// within an algorithm, but when that fails, invoking the
  /// same algorithm on `body`\ 's argument lets you trade safety for
  /// speed.
  mutating func _withUnsafeMutableBufferPointerIfSupported<R>(
    @noescape body: (UnsafeMutablePointer<Iterator.Element>, Int) throws -> R
  ) rethrows -> R?
  // FIXME: the signature should use UnsafeMutableBufferPointer, but the
  // compiler can't handle that.
  //
  // <rdar://problem/21933004> Restore the signature of
  // _withUnsafeMutableBufferPointerIfSupported() that mentions
  // UnsafeMutableBufferPointer
}

extension MutableCollection {
  public mutating func _withUnsafeMutableBufferPointerIfSupported<R>(
    @noescape body: (UnsafeMutablePointer<Iterator.Element>, Int) throws -> R
  ) rethrows -> R? {
    return nil
  }

  public subscript(bounds: Range<Index>) -> MutableSlice<Self> {
    get {
      Index._failEarlyRangeCheck2(
        bounds.startIndex, rangeEnd: bounds.endIndex,
        boundsStart: startIndex, boundsEnd: endIndex)
      return MutableSlice(_base: self, bounds: bounds)
    }
    set {
      _writeBackMutableSlice(&self, bounds: bounds, slice: newValue)
    }
  }
}

internal func _writeBackMutableSlice<
  C : MutableCollection,
  Slice_ : Collection
  where
  C._Element == Slice_.Iterator.Element,
  C.Index == Slice_.Index
>(inout self_: C, bounds: Range<C.Index>, slice: Slice_) {
  C.Index._failEarlyRangeCheck2(
    bounds.startIndex, rangeEnd: bounds.endIndex,
    boundsStart: self_.startIndex, boundsEnd: self_.endIndex)
  // FIXME(performance): can we use
  // _withUnsafeMutableBufferPointerIfSupported?  Would that create inout
  // aliasing violations if the newValue points to the same buffer?

  var selfElementIndex = bounds.startIndex
  let selfElementsEndIndex = bounds.endIndex
  var newElementIndex = slice.startIndex
  let newElementsEndIndex = slice.endIndex

  while selfElementIndex != selfElementsEndIndex &&
    newElementIndex != newElementsEndIndex {

    self_[selfElementIndex] = slice[newElementIndex]
    selfElementIndex._successorInPlace()
    newElementIndex._successorInPlace()
  }

  _require(
    selfElementIndex == selfElementsEndIndex,
    "Cannot replace a slice of a MutableCollection with a slice of a larger size")
  _require(
    newElementIndex == newElementsEndIndex,
    "Cannot replace a slice of a MutableCollection with a slice of a smaller size")
}

@available(*, unavailable, message="Bit enum has been deprecated. Please use Int instead.")
public enum Bit {}

@available(*, unavailable, renamed="CollectionDefaultIterator")
public struct IndexingGenerator<Elements : Indexable> {}

@available(*, unavailable, renamed="Collection")
public typealias CollectionType = Collection

extension Collection {
  @available(*, unavailable, renamed="Iterator")
  public typealias Generator = Iterator

  @available(*, unavailable, renamed="iterator")
  public func generate() -> Iterator {
    _abstract()
  }

  @available(*, unavailable, message="Removed in Swift 3. Please use underestimatedCount peoperty.")
  public func underestimateCount() -> Int {
    _abstract()
  }

  @available(*, unavailable, message="Please use split(_:omitEmptySubsequences:isSeparator:) instead")
  public func split(
    maxSplit: Int = Int.max,
    allowEmptySlices: Bool = false,
    @noescape isSeparator: (Iterator.Element) throws -> Bool
  ) rethrows -> [SubSequence] {
    _abstract()
  }
}

extension Collection where Iterator.Element : Equatable {
  @available(*, unavailable, message="Please use split(_:maxSplits:omitEmptySubsequences:) instead")
  public func split(
    separator: Iterator.Element,
    maxSplit: Int = Int.max,
    allowEmptySlices: Bool = false
  ) -> [SubSequence] {
    _abstract()
  }
}
@available(*, unavailable, renamed="MutableCollection")
public typealias MutableCollectionType = MutableCollection

@available(*, unavailable, message="PermutationGenerator has been removed in Swift 3")
public struct PermutationGenerator<C: Collection> {}

@available(*, unavailable, message="Please use 'Collection where SubSequence : MutableCollection'")
public typealias MutableSliceable = Collection
