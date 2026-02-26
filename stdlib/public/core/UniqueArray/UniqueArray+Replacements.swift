//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift Collections open source project
//
// Copyright (c) 2024 - 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
//
//===----------------------------------------------------------------------===//

#if !COLLECTIONS_SINGLE_MODULE
import InternalCollectionsUtilities
import ContainersPreview
#endif

#if compiler(>=6.2)

@available(SwiftStdlib 5.0, *)
extension UniqueArray where Element: ~Copyable {
  /// Replaces the specified range of elements by a given count of new items,
  /// using a callback to directly initialize array storage by populating
  /// an output span.
  ///
  /// The number of new items need not match the number of elements being
  /// removed.
  ///
  /// This method has the same overall effect as calling
  ///
  ///     try array.removeSubrange(subrange)
  ///     try array.insert(
  ///       addingCount: newItemCount,
  ///       at: subrange.lowerBound,
  ///       initializingWith: initializer)
  ///
  /// Except it performs faster (by a constant factor), by avoiding moving
  /// some items in the array twice.
  ///
  /// If the array does not have sufficient capacity to perform the replacement,
  /// then this reallocates storage to extend its capacity, using a geometric
  /// growth rate.
  ///
  /// If the callback fails to fully populate its output span or if
  /// it throws an error, then the array keeps all items that were
  /// successfully initialized before the callback terminated the prepend.
  ///
  /// Partial insertions create a gap in array storage that needs to be
  /// closed by moving newly inserted items to their correct positions given
  /// the adjusted count. This adds some overhead compared to adding exactly as
  /// many items as promised.
  ///
  /// - Parameters:
  ///   - subrange: The subrange of the array to replace. The bounds of
  ///      the range must be valid indices in the array.
  ///   - newItemCount: the maximum number of items to replace the old subrange.
  ///   - initializer: A callback that gets called at most once to directly
  ///      populate newly reserved storage within the array. The function
  ///      is always called with an empty output span.
  ///
  /// - Complexity: O(`self.count` + `newItemCount`) in addition to the complexity
  ///    of the callback invocations.
  @inlinable
  public mutating func replace<E: Error>(
    removing subrange: Range<Int>,
    addingCount newItemCount: Int,
    initializingWith initializer: (inout OutputSpan<Element>) throws(E) -> Void
  ) throws(E) -> Void {
    _storage._checkValidBounds(subrange)
    precondition(newItemCount >= 0, "Cannot add a negative number of items")
    // FIXME: Avoid moving the subsequent elements twice on resize.
    _ensureFreeCapacity(newItemCount - subrange.count)
    try _storage._uncheckedReplace(
      removing: subrange,
      addingCount: newItemCount,
      initializingWith: initializer)
  }
}

@available(SwiftStdlib 5.0, *)
extension UniqueArray where Element: ~Copyable {
#if COLLECTIONS_UNSTABLE_CONTAINERS_PREVIEW
  /// Replaces the specified range of elements by a given count of new items,
  /// using callbacks to consume old items, and to then insert new ones.
  ///
  /// The number of new items need not match the number of elements being
  /// removed.
  ///
  /// This method has the same overall effect as calling
  ///
  ///     try array.consume(subrange, consumingWith: consumer)
  ///     try array.insert(
  ///       addingCount: newItemCount,
  ///       at: subrange.lowerBound,
  ///       initializingWith: initializer)
  ///
  /// Except it performs faster (by a constant factor), by avoiding moving
  /// some items in the deque twice.
  ///
  /// If the array does not have sufficient capacity to perform the replacement,
  /// then this reallocates storage to extend its capacity, using a geometric
  /// growth rate.
  ///
  /// The `consumer` callback is not required to fully depopulate its input
  /// span. Any items the callback leaves in the span still get removed and
  /// discarded from the array before insertions begin.
  ///
  /// The `initializer` callback is not required to fully populate its
  /// output span, and it is allowed to throw an error. In such cases, the
  /// deque keeps all items that were successfully initialized before the
  /// callback terminated.
  ///
  /// Partial insertions create a gap in array storage that needs to be
  /// closed by moving newly inserted items to their correct positions given
  /// the adjusted count. This adds some overhead compared to adding exactly as
  /// many items as promised.
  ///
  /// - Parameters:
  ///   - subrange: The subrange of the deque to replace. The bounds of
  ///      the range must be valid indices in the deque.
  ///   - consumer: A callback that gets called at most once to consume
  ///      the elements to be removed directly from the deque's storage. The
  ///      function is always called with a non-empty input span.
  ///   - newItemCount: the maximum number of items to replace the old subrange.
  ///   - initializer: A callback that gets called at most once to directly
  ///      populate newly reserved storage within the deque. The function
  ///      is always called with an empty output span.
  ///
  /// - Complexity: O(`self.count` + `newItemCount`) in addition to the
  ///    complexity of the callback invocations.
  @inlinable
  public mutating func replace<E: Error>(
    removing subrange: Range<Int>,
    consumingWith consumer: (inout InputSpan<Element>) -> Void,
    addingCount newItemCount: Int,
    initializingWith initializer: (inout OutputSpan<Element>) throws(E) -> Void
  ) throws(E) -> Void {
    _storage._checkValidBounds(subrange)
    precondition(newItemCount >= 0, "Cannot add a negative number of items")
    _ensureFreeCapacity(newItemCount - subrange.count)
    try _storage._uncheckedReplace(
      removing: subrange,
      consumingWith: consumer,
      addingCount: newItemCount,
      initializingWith: initializer)
  }
#endif
}

@available(SwiftStdlib 5.0, *)
extension UniqueArray where Element: ~Copyable {
  /// Replaces the specified range of elements by moving the elements of a
  /// fully initialized buffer into their place. On return, the buffer is left
  /// in an uninitialized state.
  ///
  /// This method has the effect of removing the specified range of elements
  /// from the array and inserting the new elements starting at the same
  /// location. The number of new elements need not match the number of elements
  /// being removed.
  ///
  /// If the array does not have sufficient capacity to perform the replacement,
  /// then this reallocates the array's storage to extend its capacity, using a
  /// geometric growth rate.
  ///
  /// If you pass a zero-length range as the `subrange` parameter, this method
  /// inserts the elements of `newElements` at `subrange.lowerBound`. Calling
  /// the `insert(copying:at:)` method instead is preferred in this case.
  ///
  /// Likewise, if you pass a zero-length buffer as the `newElements`
  /// parameter, this method removes the elements in the given subrange
  /// without replacement. Calling the `removeSubrange(_:)` method instead is
  /// preferred in this case.
  ///
  /// - Parameters:
  ///   - subrange: The subrange of the array to replace. The bounds of
  ///     the range must be valid indices in the array.
  ///   - newElements: A fully initialized buffer whose contents to move into
  ///     the array.
  ///
  /// - Complexity: O(`self.count` + `newElements.count`)
  @_alwaysEmitIntoClient
  public mutating func replace(
    removing subrange: Range<Int>,
    moving newElements: UnsafeMutableBufferPointer<Element>,
  ) {
    // FIXME: Avoid moving the subsequent elements twice.
    _ensureFreeCapacity(newElements.count - subrange.count)
    _storage.replace(removing: subrange, moving: newElements)
  }

#if COLLECTIONS_UNSTABLE_CONTAINERS_PREVIEW
  /// Replaces the specified range of elements by moving the contents of an
  /// input span into their place. On return, the span is left empty.
  ///
  /// This method has the effect of removing the specified range of elements
  /// from the array and inserting the new elements starting at the same
  /// location. The number of new elements need not match the number of elements
  /// being removed.
  ///
  /// If the array does not have sufficient capacity to perform the replacement,
  /// then this reallocates the array's storage to extend its capacity, using a
  /// geometric growth rate.
  ///
  /// If you pass a zero-length range as the `subrange` parameter, this method
  /// inserts the elements of `newElements` at `subrange.lowerBound`. Calling
  /// the `insert(moving:at:)` method instead is preferred in this case.
  ///
  /// Likewise, if you pass a zero-length buffer as the `newElements`
  /// parameter, this method removes the elements in the given subrange
  /// without replacement. Calling the `removeSubrange(_:)` method instead is
  /// preferred in this case.
  ///
  /// - Parameters:
  ///   - subrange: The subrange of the array to replace. The bounds of
  ///     the range must be valid indices in the array.
  ///   - items: An input span whose contents are to be moved into the array.
  ///
  /// - Complexity: O(`self.count` + `items.count`)
  @_alwaysEmitIntoClient
  public mutating func replace(
    removing subrange: Range<Int>,
    moving items: inout InputSpan<Element>
  ) {
    // FIXME: Avoid moving the subsequent elements twice.
    _ensureFreeCapacity(items.count - subrange.count)
    _storage.replace(removing: subrange, moving: &items)
  }
#endif

  /// Replaces the specified range of elements by moving the contents of an
  /// output span into their place. On return, the span is left empty.
  ///
  /// This method has the effect of removing the specified range of elements
  /// from the array and inserting the new elements starting at the same
  /// location. The number of new elements need not match the number of elements
  /// being removed.
  ///
  /// If the array does not have sufficient capacity to perform the replacement,
  /// then this reallocates the array's storage to extend its capacity, using a
  /// geometric growth rate.
  ///
  /// If you pass a zero-length range as the `subrange` parameter, this method
  /// inserts the elements of `newElements` at `subrange.lowerBound`. Calling
  /// the `insert(moving:at:)` method instead is preferred in this case.
  ///
  /// Likewise, if you pass a zero-length buffer as the `newElements`
  /// parameter, this method removes the elements in the given subrange
  /// without replacement. Calling the `removeSubrange(_:)` method instead is
  /// preferred in this case.
  ///
  /// - Parameters:
  ///   - subrange: The subrange of the array to replace. The bounds of
  ///     the range must be valid indices in the array.
  ///   - items: An output span whose contents are to be moved into the array.
  ///
  /// - Complexity: O(`self.count` + `items.count`)
  @_alwaysEmitIntoClient
  public mutating func replace(
    removing subrange: Range<Int>,
    moving items: inout OutputSpan<Element>
  ) {
    // FIXME: Avoid moving the subsequent elements twice.
    _ensureFreeCapacity(items.count - subrange.count)
    _storage.replace(removing: subrange, moving: &items)
  }

  /// Replaces the specified range of elements by moving the elements of a
  /// another array into their place.  On return, the source array
  /// becomes empty, but it is not destroyed, and it preserves its original
  /// storage capacity.
  ///
  /// This method has the effect of removing the specified range of elements
  /// from the array and inserting the new elements starting at the same
  /// location. The number of new elements need not match the number of elements
  /// being removed.
  ///
  /// If the array does not have sufficient capacity to hold enough elements,
  /// then this reallocates the array's storage to extend its capacity, using a
  /// geometric growth rate.
  ///
  /// If you pass a zero-length range as the `subrange` parameter, this method
  /// inserts the elements of `newElements` at `subrange.lowerBound`. Calling
  /// the `insert(copying:at:)` method instead is preferred in this case.
  ///
  /// Likewise, if you pass a zero-length buffer as the `newElements`
  /// parameter, this method removes the elements in the given subrange
  /// without replacement. Calling the `removeSubrange(_:)` method instead is
  /// preferred in this case.
  ///
  /// - Parameters:
  ///   - subrange: The subrange of the array to replace. The bounds of
  ///     the range must be valid indices in the array.
  ///   - newElements: An array whose contents to move into `self`.
  ///
  /// - Complexity: O(`self.count` + `newElements.count`)
  @_alwaysEmitIntoClient
  public mutating func replace(
    removing subrange: Range<Int>,
    moving newElements: inout RigidArray<Element>,
  ) {
    // FIXME: Avoid moving the subsequent elements twice.
    _ensureFreeCapacity(newElements.count - subrange.count)
    _storage.replace(removing: subrange, moving: &newElements)
  }
}

@available(SwiftStdlib 5.0, *)
extension UniqueArray where Element: ~Copyable {
  /// Replaces the specified range of elements by moving the elements of a
  /// given array into their place, consuming it in the process.
  ///
  /// This method has the effect of removing the specified range of elements
  /// from the array and inserting the new elements starting at the same
  /// location. The number of new elements need not match the number of elements
  /// being removed.
  ///
  /// If the array does not have sufficient capacity to perform the replacement,
  /// then this reallocates the array's storage to extend its capacity, using a
  /// geometric growth rate.
  ///
  /// If you pass a zero-length range as the `subrange` parameter, this method
  /// inserts the elements of `newElements` at `subrange.lowerBound`. Calling
  /// the `insert(copying:at:)` method instead is preferred in this case.
  ///
  /// Likewise, if you pass a zero-length buffer as the `newElements`
  /// parameter, this method removes the elements in the given subrange
  /// without replacement. Calling the `removeSubrange(_:)` method instead is
  /// preferred in this case.
  ///
  /// - Parameters:
  ///   - subrange: The subrange of the array to replace. The bounds of
  ///     the range must be valid indices in the array.
  ///   - newElements: An array whose contents to move into `self`.
  ///
  /// - Complexity: O(`self.count` + `newElements.count`)
  @_alwaysEmitIntoClient
  public mutating func replace(
    removing subrange: Range<Int>,
    consuming newElements: consuming RigidArray<Element>,
  ) {
    replace(removing: subrange, moving: &newElements)
  }
}

@available(SwiftStdlib 5.0, *)
extension UniqueArray {
  /// Replaces the specified subrange of elements by copying the elements of
  /// the given buffer pointer, which must be fully initialized.
  ///
  /// This method has the effect of removing the specified range of elements
  /// from the array and inserting the new elements starting at the same location.
  /// The number of new elements need not match the number of elements being
  /// removed.
  ///
  /// If the capacity of the array isn't sufficient to perform the replacement,
  /// then this reallocates the array's storage to extend its capacity, using a
  /// geometric growth rate.
  ///
  /// If you pass a zero-length range as the `subrange` parameter, this method
  /// inserts the elements of `newElements` at `subrange.lowerBound`. Calling
  /// the `insert(copying:at:)` method instead is preferred in this case.
  ///
  /// Likewise, if you pass a zero-length buffer as the `newElements`
  /// parameter, this method removes the elements in the given subrange
  /// without replacement. Calling the `removeSubrange(_:)` method instead is
  /// preferred in this case.
  ///
  /// - Parameters:
  ///   - subrange: The subrange of the array to replace. The bounds of
  ///     the range must be valid indices in the array.
  ///   - newElements: The new elements to copy into the collection.
  ///
  /// - Complexity: O(*n* + *m*), where *n* is count of this array and
  ///   *m* is the count of `newElements`.
  @inlinable
  @inline(__always)
  public mutating func replace(
    removing subrange: Range<Int>,
    copying newElements: UnsafeBufferPointer<Element>
  ) {
    // FIXME: Avoid moving the subsequent elements twice.
    _ensureFreeCapacity(newElements.count)
    unsafe _storage.replace(removing: subrange, copying: newElements)
  }

  /// Replaces the specified subrange of elements by copying the elements of
  /// the given buffer pointer, which must be fully initialized.
  ///
  /// This method has the effect of removing the specified range of elements
  /// from the array and inserting the new elements starting at the same location.
  /// The number of new elements need not match the number of elements being
  /// removed.
  ///
  /// If the capacity of the array isn't sufficient to perform the replacement,
  /// then this reallocates the array's storage to extend its capacity, using a
  /// geometric growth rate.
  ///
  /// If you pass a zero-length range as the `subrange` parameter, this method
  /// inserts the elements of `newElements` at `subrange.lowerBound`. Calling
  /// the `insert(copying:at:)` method instead is preferred in this case.
  ///
  /// Likewise, if you pass a zero-length buffer as the `newElements`
  /// parameter, this method removes the elements in the given subrange
  /// without replacement. Calling the `removeSubrange(_:)` method instead is
  /// preferred in this case.
  ///
  /// - Parameters:
  ///   - subrange: The subrange of the array to replace. The bounds of
  ///     the range must be valid indices in the array.
  ///   - newElements: The new elements to copy into the collection.
  ///
  /// - Complexity: O(*n* + *m*), where *n* is count of this array and
  ///   *m* is the count of `newElements`.
  @inlinable
  @inline(__always)
  public mutating func replace(
    removing subrange: Range<Int>,
    copying newElements: UnsafeMutableBufferPointer<Element>
  ) {
    unsafe self.replace(
      removing: subrange,
      copying: UnsafeBufferPointer(newElements))
  }

  /// Replaces the specified subrange of elements by copying the elements of
  /// the given span.
  ///
  /// This method has the effect of removing the specified range of elements
  /// from the array and inserting the new elements starting at the same location.
  /// The number of new elements need not match the number of elements being
  /// removed.
  ///
  /// If the capacity of the array isn't sufficient to perform the replacement,
  /// then this reallocates the array's storage to extend its capacity, using a
  /// geometric growth rate.
  ///
  /// If you pass a zero-length range as the `subrange` parameter, this method
  /// inserts the elements of `newElements` at `subrange.lowerBound`. Calling
  /// the `insert(copying:at:)` method instead is preferred in this case.
  ///
  /// Likewise, if you pass a zero-length span as the `newElements`
  /// parameter, this method removes the elements in the given subrange
  /// without replacement. Calling the `removeSubrange(_:)` method instead is
  /// preferred in this case.
  ///
  /// - Parameters:
  ///   - subrange: The subrange of the array to replace. The bounds of
  ///     the range must be valid indices in the array.
  ///   - newElements: The new elements to copy into the collection.
  ///
  /// - Complexity: O(*n* + *m*), where *n* is count of this array and
  ///   *m* is the count of `newElements`.
  @inlinable
  public mutating func replace(
    removing subrange: Range<Int>,
    copying newElements: Span<Element>
  ) {
    // FIXME: Avoid moving the subsequent elements twice.
    _ensureFreeCapacity(newElements.count)
    _storage.replace(removing: subrange, copying: newElements)
  }
  
#if COLLECTIONS_UNSTABLE_CONTAINERS_PREVIEW
  /// Replaces the specified subrange of elements by copying the elements of
  /// the given container.
  ///
  /// This method has the effect of removing the specified range of elements
  /// from the array and inserting the new elements starting at the same location.
  /// The number of new elements need not match the number of elements being
  /// removed.
  ///
  /// If the capacity of the array isn't sufficient to perform the replacement,
  /// then this reallocates the array's storage to extend its capacity, using a
  /// geometric growth rate.
  ///
  /// If you pass a zero-length range as the `subrange` parameter, this method
  /// inserts the elements of `newElements` at `subrange.lowerBound`. Calling
  /// the `insert(copying:at:)` method instead is preferred in this case.
  ///
  /// Likewise, if you pass a zero-length container as the `newElements`
  /// parameter, this method removes the elements in the given subrange
  /// without replacement. Calling the `removeSubrange(_:)` method instead is
  /// preferred in this case.
  ///
  /// - Parameters:
  ///   - subrange: The subrange of the array to replace. The bounds of
  ///     the range must be valid indices in the array.
  ///   - newElements: The new elements to copy into the collection.
  ///
  /// - Complexity: O(*n* + *m*), where *n* is count of this array and
  ///   *m* is the count of `newElements`.
  @inlinable
  @inline(__always)
  public mutating func replace<
    C: Container<Element> & ~Copyable & ~Escapable
  >(
    removing subrange: Range<Int>,
    copying newElements: borrowing C
  ) {
    // FIXME: Avoid moving the subsequent elements twice.
    let c = newElements.count
    _ensureFreeCapacity(c - subrange.count)
    _storage._replace(
      removing: subrange,
      copyingContainer: newElements,
      newCount: c)
  }
#endif

  /// Replaces the specified subrange of elements by copying the elements of
  /// the given collection.
  ///
  /// This method has the effect of removing the specified range of elements
  /// from the array and inserting the new elements starting at the same location.
  /// The number of new elements need not match the number of elements being
  /// removed.
  ///
  /// If the capacity of the array isn't sufficient to perform the replacement,
  /// then this reallocates the array's storage to extend its capacity, using a
  /// geometric growth rate.
  ///
  /// If you pass a zero-length range as the `subrange` parameter, this method
  /// inserts the elements of `newElements` at `subrange.lowerBound`. Calling
  /// the `insert(copying:at:)` method instead is preferred in this case.
  ///
  /// Likewise, if you pass a zero-length collection as the `newElements`
  /// parameter, this method removes the elements in the given subrange
  /// without replacement. Calling the `removeSubrange(_:)` method instead is
  /// preferred in this case.
  ///
  /// - Parameters:
  ///   - subrange: The subrange of the array to replace. The bounds of
  ///     the range must be valid indices in the array.
  ///   - newElements: The new elements to copy into the collection.
  ///
  /// - Complexity: O(*n* + *m*), where *n* is count of this array and
  ///   *m* is the count of `newElements`.
  @inlinable
  @inline(__always)
  public mutating func replace(
    removing subrange: Range<Int>,
    copying newElements: __owned some Collection<Element>
  ) {
    // FIXME: Avoid moving the subsequent elements twice.
    let c = newElements.count
    _ensureFreeCapacity(c)
    _storage._replace(
      removing: subrange,
      copyingCollection: newElements,
      newCount: c)
  }
  
#if COLLECTIONS_UNSTABLE_CONTAINERS_PREVIEW
  /// Replaces the specified subrange of elements by copying the elements of
  /// the given container.
  ///
  /// This method has the effect of removing the specified range of elements
  /// from the array and inserting the new elements starting at the same location.
  /// The number of new elements need not match the number of elements being
  /// removed.
  ///
  /// If the capacity of the array isn't sufficient to perform the replacement,
  /// then this reallocates the array's storage to extend its capacity, using a
  /// geometric growth rate.
  ///
  /// If you pass a zero-length range as the `subrange` parameter, this method
  /// inserts the elements of `newElements` at `subrange.lowerBound`. Calling
  /// the `insert(copying:at:)` method instead is preferred in this case.
  ///
  /// Likewise, if you pass a zero-length container as the `newElements`
  /// parameter, this method removes the elements in the given subrange
  /// without replacement. Calling the `removeSubrange(_:)` method instead is
  /// preferred in this case.
  ///
  /// - Parameters:
  ///   - subrange: The subrange of the array to replace. The bounds of
  ///     the range must be valid indices in the array.
  ///   - newElements: The new elements to copy into the collection.
  ///
  /// - Complexity: O(*n* + *m*), where *n* is count of this array and
  ///   *m* is the count of `newElements`.
  @inlinable
  @inline(__always)
  public mutating func replaceSubrange<
    C: Container<Element> & Collection<Element>
  >(
    _ subrange: Range<Int>,
    copying newElements: borrowing C
  ) {
    // FIXME: Avoid moving the subsequent elements twice.
    let c = newElements.count
    _ensureFreeCapacity(c - subrange.count)
    _storage._replace(
      removing: subrange,
      copyingContainer: newElements,
      newCount: c)
  }
#endif
}
#endif
