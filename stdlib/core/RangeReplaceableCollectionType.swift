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

public protocol RangeReplaceableCollectionType : ExtensibleCollectionType {
  //===--- Fundamental Requirements ---------------------------------------===//

  /// Replace the given `subRange` of elements with `newValues`.
  /// Complexity: O(\ `countElements(subRange)`\ ) if `subRange.endIndex
  /// == self.endIndex` and `isEmpty(newValues)`\ , O(N) otherwise.
  mutating func replaceRange<
    C: CollectionType where C.Generator.Element == Self.Generator.Element
  >(
    subRange: Range<Index>, with newValues: C
  )

  //===--- Derivable Requirements (see free functions below) --------------===//
  mutating func insert(newElement: Generator.Element, atIndex i: Index)
  
  mutating func splice<
    S : CollectionType where S.Generator.Element == Generator.Element
  >(newValues: S, atIndex i: Index)

  mutating func removeAtIndex(_: Index) -> Generator.Element
  mutating func removeRange(_: Range<Index>)

  mutating func removeAll()
  mutating func removeAll(#keepCapacity: Bool)
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

/// Append elements from `newValues` to `x`.  Complexity:
/// O(N)
public func extend<
  C: RangeReplaceableCollectionType,
  S : CollectionType where S.Generator.Element == C.Generator.Element
>(inout x: C, newValues: S) {
  x.replaceRange(x.endIndex..<x.endIndex, with: newValues)
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
  
