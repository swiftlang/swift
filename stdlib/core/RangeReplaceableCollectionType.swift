//===--- RangeReplaceableCollectionType.swift -----------------*- swift -*-===//
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
//
//  A Collection protocol with replaceRange
//
//===----------------------------------------------------------------------===//

/// A *collection* that supports replacement of an arbitrary subRange
/// of elements with the elements of another collection.
public protocol RangeReplaceableCollectionType : ExtensibleCollectionType {
  //===--- Fundamental Requirements ---------------------------------------===//

  /// Replace the given `subRange` of elements with `newValues`.
  ///
  /// Invalidates all indices with respect to `self`.
  ///
  /// Complexity: O(\ `countElements(subRange)`\ ) if
  /// `subRange.endIndex == self.endIndex` and `isEmpty(newValues)`\ ,
  /// O(\ `countElements(self)`\ + \`countElements(newValues)`\ ) otherwise.
  mutating func replaceRange<
    C: CollectionType where C.Generator.Element == Self.Generator.Element
  >(
    subRange: Range<Index>, with newValues: C
  )

  //===--- Derivable Requirements (see free functions below) --------------===//
  /// Insert `newElement` at index `i`.
  ///
  /// Invalidates all indices with respect to `self`.
  ///
  /// Complexity: O(\ `countElements(self)`\ ).
  ///
  /// Can be implemented as::
  ///
  ///   Swift.insert(&self, newElement, atIndex: i)
  mutating func insert(newElement: Generator.Element, atIndex i: Index)

  /// Insert `newValues` at index `i`
  ///
  /// Invalidates all indices with respect to `self`.
  ///
  /// Complexity: O(\ `countElements(self) + countElements(newValues)`\ ).
  ///
  /// Can be implemented as::
  ///
  ///   Swift.splice(&self, newValues, atIndex: i)
  mutating func splice<
    S : CollectionType where S.Generator.Element == Generator.Element
  >(newValues: S, atIndex i: Index)

  /// Remove the element at index `i`
  ///
  /// Invalidates all indices with respect to `self`.
  ///
  /// Complexity: O(\ `countElements(self)`\ ).
  ///
  /// Can be implemented as::
  ///
  ///   Swift.removeAtIndex(&self, i)
  mutating func removeAtIndex(i: Index) -> Generator.Element
  
  /// Remove the indicated `subRange` of elements
  ///
  /// Invalidates all indices with respect to `self`.
  ///
  /// Complexity: O(\ `countElements(self)`\ ).
  ///
  /// Can be implemented as::
  ///
  ///   Swift.removeRange(&self, subRange)
  mutating func removeRange(subRange: Range<Index>)

  /// Remove all elements
  ///
  /// Invalidates all indices with respect to `self`.
  ///
  /// :param: `keepCapacity`, if `true`, is a non-binding request to
  ///    avoid releasing storage, which can be a useful optimization
  ///    when `self` is going to be grown again.
  ///
  /// Complexity: O(\ `countElements(self)`\ ).
  ///
  /// Can be implemented as::
  ///
  ///   Swift.removeAll(&self, keepCapacity: keepCapacity)
  mutating func removeAll(#keepCapacity: Bool /*= false*/)
}

/// Insert an element at index `i` in O(N).
public func insert<
  C: RangeReplaceableCollectionType
>(inout x: C, newElement: C.Generator.Element, atIndex i: C.Index) {
    x.replaceRange(i..<i, with: CollectionOfOne(newElement))
}

/// Insert the elements of `newValues` at index `i` 
public func splice<
  C: RangeReplaceableCollectionType,
  S : CollectionType where S.Generator.Element == C.Generator.Element
>(inout x: C, newValues: S, atIndex i: C.Index) {
  x.replaceRange(i..<i, with: newValues)
}

// FIXME: Trampoline helper to make the typechecker happy.  For some
// reason we can't call x.replaceRange directly in the places where
// this is used.  <rdar://problem/17863882>
internal func _replaceRange<
  C0: RangeReplaceableCollectionType, C1: CollectionType
    where C0.Generator.Element == C1.Generator.Element
>(
  inout x: C0,  subRange: Range<C0.Index>, with newValues: C1
) {
  x.replaceRange(subRange, with: newValues)
}

/// Remove and return the element at the given index.  Worst case complexity:
/// O(N).  Requires: `index` < `count`
public func removeAtIndex<
  C: RangeReplaceableCollectionType
>(inout x: C, index: C.Index) -> C.Generator.Element {
  _precondition(!isEmpty(x), "can't remove from an empty collection")
  let result = x[index]
  _replaceRange(&x, index...index, with: EmptyCollection())
  return result
}

/// Remove the elements in the given subrange.  Complexity: O(N)
public func removeRange<
  C: RangeReplaceableCollectionType
>(inout x: C, subRange: Range<C.Index>) {
  _replaceRange(&x, subRange, with: EmptyCollection())
}

/// Erase all the elements of `x`.  `keepCapacity` is a non-binding
/// request to maintain allocated memory. Complexity: O(N)
public func removeAll<
  C: RangeReplaceableCollectionType
>(inout x: C, keepCapacity: Bool = false) {
  if !keepCapacity {
    x = C()
  }
  else {
    _replaceRange(&x, indices(x), with: EmptyCollection())
  }
}

/// Append elements from `newElements` to `x`.  Complexity:
/// O(N)
public func extend<
  C: RangeReplaceableCollectionType,
  S : CollectionType where S.Generator.Element == C.Generator.Element
>(inout x: C, newElements: S) {
  x.replaceRange(x.endIndex..<x.endIndex, with: newElements)
}

/// Remove an element from the end of `x`  in O(1).
/// Requires: `x` is nonempty
public func removeLast<
  C: RangeReplaceableCollectionType where C.Index : BidirectionalIndexType
>(
  inout x: C
) -> C.Generator.Element {
  _precondition(!isEmpty(x), "can't removeLast from an empty collection")
  return removeAtIndex(&x, x.endIndex.predecessor())
}
  
