//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

@available(*, unavailable, message="access the 'count' property on the collection")
public func count <T : CollectionType>(x: T) -> T.Index.Distance {
  fatalError("unavailable function can't be called")
}

/// A protocol representing the minimal requirements of
/// `CollectionType`.
///
/// - Note: In most cases, it's best to ignore this protocol and use
///   `CollectionType` instead, as it has a more complete interface.
//
// This protocol is almost an implementation detail of the standard
// library; it is used to deduce things like the `SubSequence` and
// `Generator` type from a minimal collection, but it is also used in
// exposed places like as a constraint on IndexingGenerator.
public protocol Indexable {
  /// A type that represents a valid position in the collection.
  ///
  /// Valid indices consist of the position of every element and a
  /// "past the end" position that's not valid for use as a subscript.
  typealias Index : ForwardIndexType

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
  // need something other than a CollectionType.Generator.Element that can
  // be used as IndexingGenerator<T>'s Element.  Here we arrange for the
  // CollectionType itself to have an Element type that's deducible from
  // its subscript.  Ideally we'd like to constrain this
  // Element to be the same as CollectionType.Generator.Element (see
  // below), but we have no way of expressing it today.
  typealias _Element

  /// Returns the element at the given `position`.
  ///
  /// - Complexity: O(1)
  subscript(position: Index) -> _Element {get}
}

public protocol MutableIndexable {
  typealias Index : ForwardIndexType

  var startIndex: Index {get}
  var endIndex: Index {get}

  typealias _Element

  subscript(position: Index) -> _Element {get set}
}

/// A *generator* for an arbitrary *collection*.  Provided `C`
/// conforms to the other requirements of `Indexable`,
/// `IndexingGenerator<C>` can be used as the result of `C`'s
/// `generate()` method.  For example:
///
///      struct MyCollection : CollectionType {
///        struct Index : ForwardIndexType { /* implementation hidden */ }
///        subscript(i: Index) -> MyElement { /* implementation hidden */ }
///        func generate() -> IndexingGenerator<MyCollection> { // <===
///          return IndexingGenerator(self)
///        }
///      }
public struct IndexingGenerator<Elements : Indexable>
 : GeneratorType, SequenceType {
  
  /// Create a *generator* over the given collection.
  public init(_ elements: Elements) {
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
public protocol CollectionType : Indexable, SequenceType {
  /// A type that provides the *sequence*'s iteration interface and
  /// encapsulates its iteration state.
  ///
  /// By default, a `CollectionType` satisfies `SequenceType` by
  /// supplying an `IndexingGenerator` as its associated `Generator`
  /// type.
  typealias Generator: GeneratorType = IndexingGenerator<Self>

  // FIXME: Needed here so that the Generator is properly deduced from
  // a custom generate() function.  Otherwise we get an
  // IndexingGenerator. <rdar://problem/21539115>
  func generate() -> Generator
  
  // FIXME: should be constrained to CollectionType
  // (<rdar://problem/20715009> Implement recursive protocol
  // constraints)
  
  /// A `SequenceType` that can represent a contiguous subrange of `self`'s
  /// elements.
  ///
  /// - Note: This associated type appears as a requirement in
  ///   `SequenceType`, but is restated here with stricter
  ///   constraints: in a `CollectionType`, the `SubSequence` should
  ///   also be a `CollectionType`.
  typealias SubSequence: Indexable, SequenceType = Slice<Self>

  /// Returns the element at the given `position`.
  subscript(position: Index) -> Generator.Element {get}

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
  /// - Complexity: O(1) if `Index` conforms to `RandomAccessIndexType`;
  ///   O(N) otherwise.
  var count: Index.Distance { get }
  
  // The following requirement enables dispatching for indexOf when
  // the element type is Equatable.
  
  /// Returns `Optional(Optional(index))` if an element was found;
  /// `nil` otherwise.
  ///
  /// - Complexity: O(N).
  @warn_unused_result
  func _customIndexOfEquatableElement(element: Generator.Element) -> Index??

  /// Returns the first element of `self`, or `nil` if `self` is empty.
  var first: Generator.Element? { get }
}

/// Supply the default `generate()` method for `CollectionType` models
/// that accept the default associated `Generator`,
/// `IndexingGenerator<Self>`.
extension CollectionType where Generator == IndexingGenerator<Self> {
  public func generate() -> IndexingGenerator<Self> {
    return IndexingGenerator(self)
  }
}

/// Supply the default "slicing" `subscript`  for `CollectionType` models
/// that accept the default associated `SubSequence`, `Slice<Self>`.
extension CollectionType where SubSequence == Slice<Self> {
  public subscript(bounds: Range<Index>) -> Slice<Self> {
    Index._failEarlyRangeCheck2(
      bounds.startIndex, rangeEnd: bounds.endIndex,
      boundsStart: startIndex, boundsEnd: endIndex)
    return Slice(base: self, bounds: bounds)
  }
}

extension CollectionType where SubSequence == Self {
  /// If `!self.isEmpty`, remove the first element and return it, otherwise
  /// return `nil`.
  ///
  /// - Complexity: O(`self.count`)
  @warn_unused_result
  public mutating func popFirst() -> Generator.Element? {
    guard !isEmpty else { return nil }
    let element = first!
    self = self[startIndex.successor()..<endIndex]
    return element
  }

  /// If `!self.isEmpty`, remove the last element and return it, otherwise
  /// return `nil`.
  ///
  /// - Complexity: O(`self.count`)
  @warn_unused_result
  public mutating func popLast() -> Generator.Element? {
    guard !isEmpty else { return nil }
    let lastElementIndex = startIndex.advancedBy(numericCast(count) - 1)
    let element = self[lastElementIndex]
    self = self[startIndex..<lastElementIndex]
    return element
  }
}

/// Default implementations of core requirements
extension CollectionType {
  /// Returns `true` iff `self` is empty.
  ///
  /// - Complexity: O(1)
  public var isEmpty: Bool {
    return startIndex == endIndex
  }

  /// Returns the first element of `self`, or `nil` if `self` is empty.
  ///
  /// - Complexity: O(1)
  public var first: Generator.Element? {
    return isEmpty ? nil : self[startIndex]
  }

  /// Returns a value less than or equal to the number of elements in
  /// `self`, *nondestructively*.
  ///
  /// - Complexity: O(N).
  public func underestimateCount() -> Int {
    return numericCast(count)
  }

  /// Returns the number of elements.
  ///
  /// - Complexity: O(1) if `Index` conforms to `RandomAccessIndexType`;
  ///   O(N) otherwise.
  public var count: Index.Distance {
    return startIndex.distanceTo(endIndex)
  }

  /// Customization point for `SequenceType.indexOf()`.
  ///
  /// Define this method if the collection can find an element in less than
  /// O(N) by exploiting collection-specific knowledge.
  ///
  /// - Returns: `nil` if a linear search should be attempted instead,
  ///   `Optional(nil)` if the element was not found, or
  ///   `Optional(Optional(index))` if an element was found.
  ///
  /// - Complexity: O(N).
  @warn_unused_result
  public // dispatching
  func _customIndexOfEquatableElement(_: Generator.Element) -> Index?? {
    return nil
  }
}

//===----------------------------------------------------------------------===//
// Default implementations for CollectionType
//===----------------------------------------------------------------------===//

extension CollectionType {
  /// Return an `Array` containing the results of mapping `transform`
  /// over `self`.
  ///
  /// - Complexity: O(N).
  @warn_unused_result
  public func map<T>(
    @noescape transform: (Generator.Element) throws -> T
  ) rethrows -> [T] {
    let count: Int = numericCast(self.count)
    if count == 0 {
      return []
    }

    var result = ContiguousArray<T>()
    result.reserveCapacity(count)

    var i = self.startIndex

    for _ in 0..<count {
      result.append(try transform(self[i++]))
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
    _precondition(n >= 0, "Can't drop a negative number of elements from a collection")
    let start = startIndex.advancedBy(numericCast(n), limit: endIndex)
    return self[start..<endIndex]
  }

  /// Returns a subsequence containing all but the last `n` elements.
  ///
  /// - Requires: `n >= 0`
  /// - Complexity: O(`self.count`)
  @warn_unused_result
  public func dropLast(n: Int) -> SubSequence {
    _precondition(n >= 0, "Can't drop a negative number of elements from a collection")
    let amount = max(0, numericCast(count) - n)
    let end = startIndex.advancedBy(numericCast(amount), limit: endIndex)
    return self[startIndex..<end]
  }

  /// Returns a subsequence, up to `maxLength` in length, containing the
  /// initial elements.
  ///
  /// If `maxLength` exceeds `self.count`, the result contains all
  /// the elements of `self`.
  ///
  /// - Requires: `maxLength >= 0`
  /// - Complexity: O(`maxLength`)
  @warn_unused_result
  public func prefix(maxLength: Int) -> SubSequence {
    _precondition(maxLength >= 0, "Can't take a prefix of negative length from a collection")
    let end = startIndex.advancedBy(numericCast(maxLength), limit: endIndex)
    return self[startIndex..<end]
  }

  /// Returns a slice, up to `maxLength` in length, containing the
  /// final elements of `s`.
  ///
  /// If `maxLength` exceeds `s.count`, the result contains all
  /// the elements of `s`.
  ///
  /// - Requires: `maxLength >= 0`
  /// - Complexity: O(`self.count`)
  @warn_unused_result
  public func suffix(maxLength: Int) -> SubSequence {
    _precondition(maxLength >= 0, "Can't take a suffix of negative length from a collection")
    let amount = max(0, numericCast(count) - maxLength)
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
  /// - Parameter maxSplit: The maximum number of `SubSequence`s to
  ///   return, minus 1.
  ///   If `maxSplit + 1` `SubSequence`s are returned, the last one is
  ///   a suffix of `self` containing the remaining elements.
  ///   The default value is `Int.max`.
  ///
  /// - Parameter allowEmptySubsequences: If `true`, an empty `SubSequence`
  ///   is produced in the result for each pair of consecutive elements
  ///   satisfying `isSeparator`.
  ///   The default value is `false`.
  ///
  /// - Requires: `maxSplit >= 0`
  @warn_unused_result
  public func split(
    maxSplit: Int = Int.max,
    allowEmptySlices: Bool = false,
    @noescape isSeparator: (Generator.Element) throws -> Bool
  ) rethrows -> [SubSequence] {
    _precondition(maxSplit >= 0, "Must take zero or more splits")

    var result: [SubSequence] = []
    var subSequenceStart: Index = startIndex

    func appendSubsequence(end end: Index) -> Bool {
      if subSequenceStart == end && !allowEmptySlices {
        return false
      }
      result.append(self[subSequenceStart..<end])
      return true
    }

    if maxSplit == 0 || isEmpty {
      appendSubsequence(end: endIndex)
      return result
    }

    var subSequenceEnd = subSequenceStart
    let cachedEndIndex = endIndex
    while subSequenceEnd != cachedEndIndex {
      if try isSeparator(self[subSequenceEnd]) {
        let didAppend = appendSubsequence(end: subSequenceEnd)
        ++subSequenceEnd
        subSequenceStart = subSequenceEnd
        if didAppend && result.count == maxSplit {
          break
        }
        continue
      }
      ++subSequenceEnd
    }

    if subSequenceStart != cachedEndIndex || allowEmptySlices {
      result.append(self[subSequenceStart..<cachedEndIndex])
    }

    return result
  }
}

extension CollectionType where Generator.Element : Equatable {
  /// Returns the maximal `SubSequence`s of `self`, in order, around a
  /// `separator` element.
  ///
  /// - Parameter maxSplit: The maximum number of `SubSequence`s to
  ///   return, minus 1.
  ///   If `maxSplit + 1` `SubSequence`s are returned, the last one is
  ///   a suffix of `self` containing the remaining elements.
  ///   The default value is `Int.max`.
  ///
  /// - Parameter allowEmptySubsequences: If `true`, an empty `SubSequence`
  ///   is produced in the result for each pair of consecutive elements
  ///   satisfying `isSeparator`.
  ///   The default value is `false`.
  ///
  /// - Requires: `maxSplit >= 0`
  @warn_unused_result
  public func split(
    separator: Generator.Element,
    maxSplit: Int = Int.max,
    allowEmptySlices: Bool = false
  ) -> [SubSequence] {
  return split(maxSplit, allowEmptySlices: allowEmptySlices,
      isSeparator: { $0 == separator })
  }
}

extension CollectionType where Index : BidirectionalIndexType {
  /// Returns a subsequence containing all but the last `n` elements.
  ///
  /// - Requires: `n >= 0`
  /// - Complexity: O(`n`)
  @warn_unused_result
  public func dropLast(n: Int) -> SubSequence {
    _precondition(n >= 0, "Can't drop a negative number of elements from a collection")
    let end = endIndex.advancedBy(numericCast(-n), limit: startIndex)
    return self[startIndex..<end]
  }

  /// Returns a slice, up to `maxLength` in length, containing the
  /// final elements of `s`.
  ///
  /// If `maxLength` exceeds `s.count`, the result contains all
  /// the elements of `s`.
  ///
  /// - Requires: `maxLength >= 0`
  /// - Complexity: O(`maxLength`)
  @warn_unused_result
  public func suffix(maxLength: Int) -> SubSequence {
    _precondition(maxLength >= 0, "Can't take a suffix of negative length from a collection")
    let start = endIndex.advancedBy(numericCast(-maxLength), limit: startIndex)
    return self[start..<endIndex]
  }
}

extension CollectionType where SubSequence == Self {
  /// Remove the element at `startIndex` and return it.
  ///
  /// - Complexity: O(1)
  /// - Requires: `!self.isEmpty`.
  public mutating func removeFirst() -> Generator.Element {
    _precondition(!isEmpty, "can't remove items from an empty collection")
    let element = first!
    self = self[startIndex.successor()..<endIndex]
    return element
  }

  /// Remove the first `n` elements.
  ///
  /// - Complexity:
  ///   - O(1) if `Index` conforms to `RandomAccessIndexType`
  ///   - O(n) otherwise
  /// - Requires: `n >= 0 && self.count >= n`.
  public mutating func removeFirst(n: Int) {
    if n == 0 { return }
    _precondition(n >= 0, "number of elements to remove should be non-negative")
    _precondition(count >= numericCast(n),
      "can't remove more items from a collection than it contains")
    self = self[startIndex.advancedBy(numericCast(n))..<endIndex]
  }
}

extension CollectionType
  where
  SubSequence == Self,
  Index : BidirectionalIndexType {

  /// Remove an element from the end.
  ///
  /// - Complexity: O(1)
  /// - Requires: `!self.isEmpty`
  public mutating func removeLast() -> Generator.Element {
    let element = last!
    self = self[startIndex..<endIndex.predecessor()]
    return element
  }

  /// Remove the last `n` elements.
  ///
  /// - Complexity:
  ///   - O(1) if `Index` conforms to `RandomAccessIndexType`
  ///   - O(n) otherwise
  /// - Requires: `n >= 0 && self.count >= n`.
  public mutating func removeLast(n: Int) {
    if n == 0 { return }
    _precondition(n >= 0, "number of elements to remove should be non-negative")
    _precondition(count >= numericCast(n),
      "can't remove more items from a collection than it contains")
    self = self[startIndex..<endIndex.advancedBy(numericCast(-n))]
  }
}

extension SequenceType
  where Self : _ArrayType, Self.Element == Self.Generator.Element {
  // A fast implementation for when you are backed by a contiguous array.
  public func _initializeTo(ptr: UnsafeMutablePointer<Generator.Element>)
    -> UnsafeMutablePointer<Generator.Element> {
    let s = self._baseAddressIfContiguous
    if s != nil {
      let count = self.count
      ptr.initializeFrom(s, count: count)
      _fixLifetime(self._owner)
      return ptr + count
    } else {
      var p = ptr
      for x in self {
        p++.initialize(x)
      }
      return p
    }
  }
}

extension CollectionType {
  public func _preprocessingPass<R>(preprocess: (Self)->R) -> R? {
    return preprocess(self)
  }
}

/// Returns `true` iff `x` is empty.
@available(*, unavailable, message="access the 'isEmpty' property on the collection")
public func isEmpty<C: CollectionType>(x: C) -> Bool {
  fatalError("unavailable function can't be called")
}

/// Returns the first element of `x`, or `nil` if `x` is empty.
@available(*, unavailable, message="access the 'first' property on the collection")
public func first<C: CollectionType>(x: C) -> C.Generator.Element? {
  fatalError("unavailable function can't be called")
}

/// Returns the last element of `x`, or `nil` if `x` is empty.
@available(*, unavailable, message="access the 'last' property on the collection")
public func last<C: CollectionType where C.Index: BidirectionalIndexType>(
  x: C
) -> C.Generator.Element? {
  fatalError("unavailable function can't be called")
}

/// A *collection* that supports subscript assignment.
///
/// For any instance `a` of a type conforming to
/// `MutableCollectionType`, :
///
///     a[i] = x
///     let y = a[i]
///
/// is equivalent to:
///
///     a[i] = x
///     let y = x
///
public protocol MutableCollectionType : MutableIndexable, CollectionType {
  // FIXME: should be constrained to MutableCollectionType
  // (<rdar://problem/20715009> Implement recursive protocol
  // constraints)
  typealias SubSequence : CollectionType /*: MutableCollectionType*/
    = MutableSlice<Self>

  /// Access the element at `position`.
  ///
  /// - Requires: `position` indicates a valid position in `self` and
  ///   `position != endIndex`.
  ///
  /// - Complexity: O(1)
  subscript(position: Index) -> Generator.Element {get set}

  /// Returns a collection representing a contiguous sub-range of
  /// `self`'s elements.
  ///
  /// - Complexity: O(1) for the getter, O(`bounds.count`) for the setter.
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
    @noescape body: (UnsafeMutablePointer<Generator.Element>, Int) throws -> R
  ) rethrows -> R?
  // FIXME: the signature should use UnsafeMutableBufferPointer, but the
  // compiler can't handle that.
  //
  // <rdar://problem/21933004> Restore the signature of
  // _withUnsafeMutableBufferPointerIfSupported() that mentions
  // UnsafeMutableBufferPointer
}

extension MutableCollectionType {
  public mutating func _withUnsafeMutableBufferPointerIfSupported<R>(
    @noescape body: (UnsafeMutablePointer<Generator.Element>, Int) throws -> R
  ) rethrows -> R? {
    return nil
  }

  public subscript(bounds: Range<Index>) -> MutableSlice<Self> {
    get {
      Index._failEarlyRangeCheck2(
        bounds.startIndex, rangeEnd: bounds.endIndex,
        boundsStart: startIndex, boundsEnd: endIndex)
      return MutableSlice(base: self, bounds: bounds)
    }
    set {
      _writeBackMutableSlice(&self, bounds: bounds, slice: newValue)
    }
  }
}

internal func _writeBackMutableSlice<
  Collection : MutableCollectionType,
  Slice_ : CollectionType
  where
  Collection._Element == Slice_.Generator.Element,
  Collection.Index == Slice_.Index
>(inout self_: Collection, bounds: Range<Collection.Index>, slice: Slice_) {
  Collection.Index._failEarlyRangeCheck2(
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
    ++selfElementIndex
    ++newElementIndex
  }

  _precondition(
    selfElementIndex == selfElementsEndIndex,
    "Can not replace a slice of a MutableCollectionType with a slice of a larger size")
  _precondition(
    newElementIndex == newElementsEndIndex,
    "Can not replace a slice of a MutableCollectionType with a slice of a smaller size")
}

/// Returns the range of `x`'s valid index values.
///
/// The result's `endIndex` is the same as that of `x`.  Because
/// `Range` is half-open, iterating the values of the result produces
/// all valid subscript arguments for `x`, omitting its `endIndex`.
@available(*, unavailable, message="access the 'indices' property on the collection")
public func indices<
    C : CollectionType>(x: C) -> Range<C.Index> {
  fatalError("unavailable function can't be called")
}

/// A *generator* that adapts a *collection* `C` and any *sequence* of
/// its `Index` type to present the collection's elements in a
/// permuted order.
public struct PermutationGenerator<
  C: CollectionType, Indices: SequenceType
  where C.Index == Indices.Generator.Element
> : GeneratorType, SequenceType {
  var seq : C
  var indices : Indices.Generator

  /// The type of element returned by `next()`.
  public typealias Element = C.Generator.Element

  /// Advance to the next element and return it, or `nil` if no next
  /// element exists.
  ///
  /// - Requires: No preceding call to `self.next()` has returned `nil`.
  public mutating func next() -> Element? {
    return indices.next().map { seq[$0] }
  }

  /// Construct a *generator* over a permutation of `elements` given
  /// by `indices`.
  ///
  /// - Requires: `elements[i]` is valid for every `i` in `indices`.
  public init(elements: C, indices: Indices) {
    self.seq = elements
    self.indices = indices.generate()
  }
}

/// A *collection* with mutable slices.
///
/// For example,
///
///      x[i..<j] = someExpression
///      x[i..<j].mutatingMethod()
public protocol MutableSliceable : CollectionType, MutableCollectionType {
  subscript(_: Range<Index>) -> SubSequence { get set }
}

@available(*, unavailable, message="Use the dropFirst() method instead.") 
public func dropFirst<Seq : CollectionType>(s: Seq) -> Seq.SubSequence {
  fatalError("unavailable function can't be called")
}

@available(*, unavailable, message="Use the dropLast() method instead.")
public func dropLast<
  S : CollectionType
  where S.Index: BidirectionalIndexType
>(s: S) -> S.SubSequence {
  fatalError("unavailable function can't be called")
}

@available(*, unavailable, message="Use the prefix() method.")
public func prefix<S : CollectionType>(s: S, _ maxLength: Int) -> S.SubSequence {
  fatalError("unavailable function can't be called")
}

@available(*, unavailable, message="Use the suffix() method instead.")
public func suffix<
  S : CollectionType where S.Index: BidirectionalIndexType
>(s: S, _ maxLength: Int) -> S.SubSequence {
  fatalError("unavailable function can't be called")
}

@available(*, unavailable, renamed="CollectionType")
public struct Sliceable {}

