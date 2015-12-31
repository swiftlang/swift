//===--- RangeReplaceableCollectionType.swift -----------------*- swift -*-===//
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
//
//  A Collection protocol with replaceRange
//
//===----------------------------------------------------------------------===//

/// A *collection* that supports replacement of an arbitrary subRange
/// of elements with the elements of another collection.
public protocol RangeReplaceableCollectionType : CollectionType {
  //===--- Fundamental Requirements ---------------------------------------===//

  /// Create an empty instance.
  init()

  /// Replace the given `subRange` of elements with `newElements`.
  ///
  /// Invalidates all indices with respect to `self`.
  ///
  /// - Complexity: O(`subRange.count`) if
  ///   `subRange.endIndex == self.endIndex` and `newElements.isEmpty`,
  ///   O(`self.count` + `newElements.count`) otherwise.
  mutating func replaceRange<
    C : CollectionType where C.Generator.Element == Generator.Element
  >(
    subRange: Range<Index>, with newElements: C
  )

  /*
  We could have these operators with default implementations, but the compiler
  crashes:

  <rdar://problem/16566712> Dependent type should have been substituted by Sema
  or SILGen

  func +<
    S : SequenceType
    where S.Generator.Element == Generator.Element
  >(_: Self, _: S) -> Self

  func +<
    S : SequenceType
    where S.Generator.Element == Generator.Element
  >(_: S, _: Self) -> Self

  func +<
    S : CollectionType
    where S.Generator.Element == Generator.Element
  >(_: Self, _: S) -> Self

  func +<
    RC : RangeReplaceableCollectionType
    where RC.Generator.Element == Generator.Element
  >(_: Self, _: S) -> Self
*/

  /// A non-binding request to ensure `n` elements of available storage.
  ///
  /// This works as an optimization to avoid multiple reallocations of
  /// linear data structures like `Array`.  Conforming types may
  /// reserve more than `n`, exactly `n`, less than `n` elements of
  /// storage, or even ignore the request completely.
  mutating func reserveCapacity(n: Index.Distance)

  //===--- Derivable Requirements -----------------------------------------===//

  /// Creates a collection instance that contains `elements`.
  init<
    S : SequenceType where S.Generator.Element == Generator.Element
  >(_ elements: S)

  /// Append `x` to `self`.
  ///
  /// Applying `successor()` to the index of the new element yields
  /// `self.endIndex`.
  ///
  /// - Complexity: Amortized O(1).
  mutating func append(x: Generator.Element)

  /*
  The 'appendContentsOf' requirement should be an operator, but the compiler crashes:

  <rdar://problem/16566712> Dependent type should have been substituted by Sema
  or SILGen

  func +=<
    S : SequenceType
    where S.Generator.Element == Generator.Element
  >(inout _: Self, _: S)
  */

  /// Append the elements of `newElements` to `self`.
  ///
  /// - Complexity: O(*length of result*).
  mutating func appendContentsOf<
    S : SequenceType
    where S.Generator.Element == Generator.Element
  >(newElements: S)

  /// Insert `newElement` at index `i`.
  ///
  /// Invalidates all indices with respect to `self`.
  ///
  /// - Complexity: O(`self.count`).
  mutating func insert(newElement: Generator.Element, atIndex i: Index)

  /// Insert `newElements` at index `i`.
  ///
  /// Invalidates all indices with respect to `self`.
  ///
  /// - Complexity: O(`self.count + newElements.count`).
  mutating func insertContentsOf<
    S : CollectionType where S.Generator.Element == Generator.Element
  >(newElements: S, at i: Index)

  /// Remove the element at index `i`.
  ///
  /// Invalidates all indices with respect to `self`.
  ///
  /// - Complexity: O(`self.count`).
  mutating func removeAtIndex(i: Index) -> Generator.Element

  /// Customization point for `removeLast()`.  Implement this function if you
  /// want to replace the default implementation.
  ///
  /// - Returns: A non-nil value if the operation was performed.
  @warn_unused_result
  mutating func _customRemoveLast() -> Generator.Element?

  /// Customization point for `removeLast(_:)`.  Implement this function if you
  /// want to replace the default implementation.
  ///
  /// - Returns: True if the operation was performed.
  @warn_unused_result
  mutating func _customRemoveLast(n: Int) -> Bool

  /// Remove the element at `startIndex` and return it.
  ///
  /// - Complexity: O(`self.count`)
  /// - Requires: `!self.isEmpty`.
  mutating func removeFirst() -> Generator.Element

  /// Remove the first `n` elements.
  ///
  /// - Complexity: O(`self.count`)
  /// - Requires: `n >= 0 && self.count >= n`.
  mutating func removeFirst(n: Int)

  /// Remove the indicated `subRange` of elements.
  ///
  /// Invalidates all indices with respect to `self`.
  ///
  /// - Complexity: O(`self.count`).
  mutating func removeRange(subRange: Range<Index>)

  /// Remove all elements.
  ///
  /// Invalidates all indices with respect to `self`.
  ///
  /// - parameter keepCapacity: If `true`, is a non-binding request to
  ///    avoid releasing storage, which can be a useful optimization
  ///    when `self` is going to be grown again.
  ///
  /// - Complexity: O(`self.count`).
  mutating func removeAll(keepCapacity keepCapacity: Bool /*= false*/)

}

//===----------------------------------------------------------------------===//
// Default implementations for RangeReplaceableCollectionType
//===----------------------------------------------------------------------===//

extension RangeReplaceableCollectionType {
  public init<
    S : SequenceType where S.Generator.Element == Generator.Element
  >(_ elements: S) {
    self.init()
    appendContentsOf(elements)
  }

  public mutating func append(newElement: Generator.Element) {
    insert(newElement, atIndex: endIndex)
  }

  public mutating func appendContentsOf<
    S : SequenceType where S.Generator.Element == Generator.Element
  >(newElements: S) {
    for element in newElements {
      append(element)
    }
  }

  public mutating func insert(
    newElement: Generator.Element, atIndex i: Index
  ) {
    replaceRange(i..<i, with: CollectionOfOne(newElement))
  }

  public mutating func insertContentsOf<
    C : CollectionType where C.Generator.Element == Generator.Element
  >(newElements: C, at i: Index) {
    replaceRange(i..<i, with: newElements)
  }

  public mutating func removeAtIndex(index: Index) -> Generator.Element {
    _precondition(!isEmpty, "can't remove from an empty collection")
    let result: Generator.Element = self[index]
    replaceRange(index...index, with: EmptyCollection())
    return result
  }

  public mutating func removeRange(subRange: Range<Index>) {
    replaceRange(subRange, with: EmptyCollection())
  }

  public mutating func removeFirst(n: Int) {
    if n == 0 { return }
    _precondition(n >= 0, "number of elements to remove should be non-negative")
    _precondition(count >= numericCast(n),
      "can't remove more items from a collection than it has")
    let end = startIndex.advancedBy(numericCast(n))
    removeRange(startIndex..<end)
  }

  public mutating func removeFirst() -> Generator.Element {
    _precondition(!isEmpty,
      "can't remove first element from an empty collection")
    let firstElement = first!
    removeFirst(1)
    return firstElement
  }

  public mutating func removeAll(keepCapacity keepCapacity: Bool = false) {
    if !keepCapacity {
      self = Self()
    }
    else {
      replaceRange(indices, with: EmptyCollection())
    }
  }

  public mutating func reserveCapacity(n: Index.Distance) {}
}

extension RangeReplaceableCollectionType where SubSequence == Self {
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
  /// - Complexity: O(1)
  /// - Requires: `self.count >= n`.
  public mutating func removeFirst(n: Int) {
    if n == 0 { return }
    _precondition(n >= 0, "number of elements to remove should be non-negative")
    _precondition(count >= numericCast(n),
      "can't remove more items from a collection than it contains")
    self = self[startIndex.advancedBy(numericCast(n))..<endIndex]
  }
}

extension RangeReplaceableCollectionType {
  @warn_unused_result
  public mutating func _customRemoveLast() -> Generator.Element? {
    return nil
  }

  @warn_unused_result
  public mutating func _customRemoveLast(n: Int) -> Bool {
    return false
  }
}

extension RangeReplaceableCollectionType
  where
  Index : BidirectionalIndexType,
  SubSequence == Self {

  @warn_unused_result
  public mutating func _customRemoveLast() -> Generator.Element? {
    let element = last!
    self = self[startIndex..<endIndex.predecessor()]
    return element
  }

  @warn_unused_result
  public mutating func _customRemoveLast(n: Int) -> Bool {
    self = self[startIndex..<endIndex.advancedBy(numericCast(-n))]
    return true
  }
}

extension RangeReplaceableCollectionType where Index : BidirectionalIndexType {
  /// Remove an element from the end.
  ///
  /// - Complexity: O(1)
  /// - Requires: `!self.isEmpty`
  public mutating func removeLast() -> Generator.Element {
    _precondition(!isEmpty, "can't remove last element from an empty collection")
    if let result = _customRemoveLast() {
      return result
    }
    return removeAtIndex(endIndex.predecessor())
  }

  /// Remove the last `n` elements.
  ///
  /// - Complexity: O(`self.count`)
  /// - Requires: `n >= 0 && self.count >= n`.
  public mutating func removeLast(n: Int) {
    if n == 0 { return }
    _precondition(n >= 0, "number of elements to remove should be non-negative")
    _precondition(count >= numericCast(n),
      "can't remove more items from a collection than it contains")
    if _customRemoveLast(n) {
      return
    }
    let end = endIndex
    removeRange(end.advancedBy(numericCast(-n))..<end)
  }
}

/// Insert `newElement` into `x` at index `i`.
///
/// Invalidates all indices with respect to `x`.
///
/// - Complexity: O(`x.count`).
@available(*, unavailable, message="call the 'insert()' method on the collection")
public func insert<
    C: RangeReplaceableCollectionType
>(inout x: C, _ newElement: C.Generator.Element, atIndex i: C.Index) {
  fatalError("unavailable function can't be called")
}

/// Insert `newElements` into `x` at index `i`.
///
/// Invalidates all indices with respect to `x`.
///
/// - Complexity: O(`x.count + newElements.count`).
@available(*, unavailable, message="call the 'insertContentsOf()' method on the collection")
public func splice<
    C: RangeReplaceableCollectionType,
    S : CollectionType where S.Generator.Element == C.Generator.Element
>(inout x: C, _ newElements: S, atIndex i: C.Index) {
  fatalError("unavailable function can't be called")
}

/// Remove from `x` and return the element at index `i`.
///
/// Invalidates all indices with respect to `x`.
///
/// - Complexity: O(`x.count`).
@available(*, unavailable, message="call the 'removeAtIndex()' method on the collection")
public func removeAtIndex<
    C: RangeReplaceableCollectionType
>(inout x: C, _ index: C.Index) -> C.Generator.Element {
  fatalError("unavailable function can't be called")
}

/// Remove from `x` the indicated `subRange` of elements.
///
/// Invalidates all indices with respect to `x`.
///
/// - Complexity: O(`x.count`).
@available(*, unavailable, message="call the 'removeRange()' method on the collection")
public func removeRange<
    C: RangeReplaceableCollectionType
>(inout x: C, _ subRange: Range<C.Index>) {
  fatalError("unavailable function can't be called")
}

/// Remove all elements from `x`.
///
/// Invalidates all indices with respect to `x`.
///
/// - parameter keepCapacity: If `true`, is a non-binding request to
///    avoid releasing storage, which can be a useful optimization
///    when `x` is going to be grown again.
///
/// - Complexity: O(`x.count`).
@available(*, unavailable, message="call the 'removeAll()' method on the collection")
public func removeAll<
    C: RangeReplaceableCollectionType
>(inout x: C, keepCapacity: Bool = false) {
  fatalError("unavailable function can't be called")
}

/// Append elements from `newElements` to `x`.
///
/// - Complexity: O(N).
@available(*, unavailable, message="call the 'appendContentsOf()' method on the collection")
public func extend<
    C: RangeReplaceableCollectionType,
    S : SequenceType where S.Generator.Element == C.Generator.Element
>(inout x: C, _ newElements: S) {
  fatalError("unavailable function can't be called")
}

extension RangeReplaceableCollectionType {
  @available(*, unavailable, renamed="appendContentsOf")
  public mutating func extend<
    S : SequenceType
    where S.Generator.Element == Generator.Element
  >(newElements: S) {
    fatalError("unavailable function can't be called")
  }

  @available(*, unavailable, renamed="insertContentsOf")
  public mutating func splice<
    S : CollectionType where S.Generator.Element == Generator.Element
  >(newElements: S, atIndex i: Index) {
    fatalError("unavailable function can't be called")
  }
}

/// Remove an element from the end of `x`  in O(1).
///
/// - Requires: `x` is nonempty.
@available(*, unavailable, message="call the 'removeLast()' method on the collection")
public func removeLast<
    C: RangeReplaceableCollectionType where C.Index : BidirectionalIndexType
>(inout x: C) -> C.Generator.Element {
  fatalError("unavailable function can't be called")
}

@warn_unused_result
public func +<
    C : RangeReplaceableCollectionType,
    S : SequenceType
    where S.Generator.Element == C.Generator.Element
>(lhs: C, rhs: S) -> C {
  var lhs = lhs
  // FIXME: what if lhs is a reference type?  This will mutate it.
  lhs.appendContentsOf(rhs)
  return lhs
}

@warn_unused_result
public func +<
    C : RangeReplaceableCollectionType,
    S : SequenceType
    where S.Generator.Element == C.Generator.Element
>(lhs: S, rhs: C) -> C {
  var result = C()
  result.reserveCapacity(rhs.count + numericCast(lhs.underestimateCount()))
  result.appendContentsOf(lhs)
  result.appendContentsOf(rhs)
  return result
}

@warn_unused_result
public func +<
    C : RangeReplaceableCollectionType,
    S : CollectionType
    where S.Generator.Element == C.Generator.Element
>(lhs: C, rhs: S) -> C {
  var lhs = lhs
  // FIXME: what if lhs is a reference type?  This will mutate it.
  lhs.reserveCapacity(lhs.count + numericCast(rhs.count))
  lhs.appendContentsOf(rhs)
  return lhs
}

@warn_unused_result
public func +<
    RRC1 : RangeReplaceableCollectionType,
    RRC2 : RangeReplaceableCollectionType 
    where RRC1.Generator.Element == RRC2.Generator.Element
>(lhs: RRC1, rhs: RRC2) -> RRC1 {
  var lhs = lhs
  // FIXME: what if lhs is a reference type?  This will mutate it.
  lhs.reserveCapacity(lhs.count + numericCast(rhs.count))
  lhs.appendContentsOf(rhs)
  return lhs
}

@available(*, unavailable, renamed="RangeReplaceableCollectionType")
public typealias ExtensibleCollectionType = RangeReplaceableCollectionType
