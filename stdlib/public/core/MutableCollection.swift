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

// TODO: swift-3-indexing-model - can this extend Indexable instead of all this duplication?
/// A type that supports subscript assignment to a mutable collection.
public protocol MutableIndexable {
  // This protocol is almost an implementation detail of the standard
  // library; it is used to deduce things like the `SubSequence` and
  // `Iterator` type from a minimal collection, but it is also used in
  // exposed places like as a constraint on `IndexingIterator`.

  /// A type that represents a valid position in the collection.
  ///
  /// Valid indices consist of the position of every element and a
  /// "past the end" position that's not valid for use as a subscript.
  // TODO: swift-3-indexing-model - Index only needs to be comparable or must be comparable..?
  associatedtype Index : Comparable

  /// The position of the first element in a non-empty collection.
  ///
  /// In an empty collection, `startIndex == endIndex`.
  ///
  /// - Complexity: O(1)
  var startIndex: Index { get }

  /// The collection's "past the end" position.
  ///
  /// `endIndex` is not a valid argument to `subscript`, and is always
  /// reachable from `startIndex` by zero or more applications of
  /// `successor()`.
  ///
  /// - Complexity: O(1)
  var endIndex: Index { get }

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
  subscript(position: Index) -> _Element { get set }

  /// Performs a range check in O(1), or a no-op when a range check is not
  /// implementable in O(1).
  ///
  /// The range check, if performed, is equivalent to:
  ///
  ///     precondition(bounds.contains(index))
  ///
  /// Use this function to perform a cheap range check for QoI purposes when
  /// memory safety is not a concern.  Do not rely on this range check for
  /// memory safety.
  ///
  /// The default implementation for forward and bidirectional indices is a
  /// no-op.  The default implementation for random access indices performs a
  /// range check.
  ///
  /// - Complexity: O(1).
  func _failEarlyRangeCheck(index: Index, bounds: Range<Index>)

  /// Performs a range check in O(1), or a no-op when a range check is not
  /// implementable in O(1).
  ///
  /// The range check, if performed, is equivalent to:
  ///
  ///     precondition(
  ///       bounds.contains(range.startIndex) ||
  ///       range.startIndex == bounds.endIndex)
  ///     precondition(
  ///       bounds.contains(range.endIndex) ||
  ///       range.endIndex == bounds.endIndex)
  ///
  /// Use this function to perform a cheap range check for QoI purposes when
  /// memory safety is not a concern.  Do not rely on this range check for
  /// memory safety.
  ///
  /// The default implementation for forward and bidirectional indices is a
  /// no-op.  The default implementation for random access indices performs a
  /// range check.
  ///
  /// - Complexity: O(1).
  func _failEarlyRangeCheck(
    rangeStart rangeStart: Index,
    rangeEnd: Index,
    boundsStart: Index,
    boundsEnd: Index)
  // TODO: swift-3-indexing-model - can we change the above to the following? (possible compiler issue)
  //  func _failEarlyRangeCheck(range: Range<Index>, bounds: Range<Index>)

  /// Returns the next consecutive `Index` in a discrete sequence of
  /// `Index` values.
  ///
  /// - Precondition: `i` has a well-defined successor.
  @warn_unused_result
  func next(i: Index) -> Index

  func _nextInPlace(i: inout Index)
}

// TODO: swift-3-indexing-model - review the following
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
  /// - Precondition: `position` indicates a valid position in `self` and
  ///   `position != endIndex`.
  ///
  /// - Complexity: O(1)
  subscript(position: Index) -> Iterator.Element {get set}

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
    @noescape body: (UnsafeMutablePointer<Iterator.Element>, Int) throws -> R
  ) rethrows -> R?
  // FIXME: the signature should use UnsafeMutableBufferPointer, but the
  // compiler can't handle that.
  //
  // <rdar://problem/21933004> Restore the signature of
  // _withUnsafeMutableBufferPointerIfSupported() that mentions
  // UnsafeMutableBufferPointer
}

// TODO: swift-3-indexing-model - review the following
extension MutableCollection {
  public mutating func _withUnsafeMutableBufferPointerIfSupported<R>(
    @noescape body: (UnsafeMutablePointer<Iterator.Element>, Int) throws -> R
  ) rethrows -> R? {
    return nil
  }

  public subscript(bounds: Range<Index>) -> MutableSlice<Self> {
    get {
      _failEarlyRangeCheck(
        rangeStart: bounds.startIndex,
        rangeEnd: bounds.endIndex,
        boundsStart: startIndex,
        boundsEnd: endIndex)
      return MutableSlice(_base: self, bounds: bounds)
    }
    set {
      _writeBackMutableSlice(&self, bounds: bounds, slice: newValue)
    }
  }
}

// TODO: swift-3-indexing-model - review the following
internal func _writeBackMutableSlice<
  C : MutableCollection,
  Slice_ : Collection
  where
  C._Element == Slice_.Iterator.Element,
  C.Index == Slice_.Index
>(self_: inout C, bounds: Range<C.Index>, slice: Slice_) {
  fatalError("FIXME: swift-3-indexing-model")
  /*
  C._failEarlyRangeCheck(
    rangeStart: bounds.startIndex,
    rangeEnd: bounds.endIndex,
    boundsStart: self_.startIndex,
    boundsEnd: self_.endIndex)
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

  _precondition(
    selfElementIndex == selfElementsEndIndex,
    "Cannot replace a slice of a MutableCollection with a slice of a larger size")
  _precondition(
    newElementIndex == newElementsEndIndex,
    "Cannot replace a slice of a MutableCollection with a slice of a smaller size")
  */
}

@available(*, unavailable, renamed="MutableCollection")
public typealias MutableCollectionType = MutableCollection

@available(*, unavailable, message="Please use 'Collection where SubSequence : MutableCollection'")
public typealias MutableSliceable = Collection
