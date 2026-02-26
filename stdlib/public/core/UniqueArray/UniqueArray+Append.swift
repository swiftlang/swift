//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift Collections open source project
//
// Copyright (c) 2024 - 2026 Apple Inc. and the Swift project authors
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
  /// Adds an element to the end of the array.
  ///
  /// If the array does not have sufficient capacity to hold any more elements,
  /// then this reallocates the array's storage to grow its capacity, using a
  /// geometric growth rate.
  ///
  /// - Parameter item: The element to append to the collection.
  ///
  /// - Complexity: O(1) as amortized over many invocations on the same array.
  @inlinable
  public mutating func append(_ item: consuming Element) {
    _ensureFreeCapacity(1)
    _storage.append(item)
  }
}

@available(SwiftStdlib 5.0, *)
extension UniqueArray where Element: ~Copyable {
  /// Append a given number of items to the end of this array by populating
  /// an output span.
  ///
  /// If the array does not have sufficient capacity to hold the requested
  /// number of new elements, then this reallocates the array's storage to
  /// grow its capacity, using a geometric growth rate.
  ///
  /// If the callback fails to fully populate its output span or if
  /// it throws an error, then the array keeps all items that were
  /// successfully initialized before the callback terminated the insertion.
  ///
  /// - Parameters:
  ///    - newItemCount: The number of items to append to the array.
  ///    - initializer: A callback that gets called at most once to directly
  ///       populate newly reserved storage within the array. The function
  ///       is allowed to initialize fewer than `uninitializedCount` items.
  ///       The array is appended however many items the callback adds to the
  ///       output span before it returns (or before it throws an error).
  ///
  /// - Complexity: O(`uninitializedCount`)
  @_alwaysEmitIntoClient
  public mutating func append<E: Error>(
    addingCount newItemCount: Int,
    initializingWith initializer: (inout OutputSpan<Element>) throws(E) -> Void
  ) throws(E) {
    _ensureFreeCapacity(newItemCount)
    return try _storage.append(
      addingCount: newItemCount,
      initializingWith: initializer)
  }
}

@available(SwiftStdlib 5.0, *)
extension UniqueArray where Element: ~Copyable {
  /// Moves the elements of a buffer to the end of this array, leaving the
  /// buffer uninitialized.
  ///
  /// If the array does not have sufficient capacity to hold all items in the
  /// buffer, then this reallocates the array's storage to grow its capacity,
  /// using a geometric growth rate.
  ///
  /// - Parameters:
  ///    - items: A fully initialized buffer whose contents to move into
  ///        the array.
  ///
  /// - Complexity: O(`count` + `items.count`)
  @_alwaysEmitIntoClient
  public mutating func append(
    moving items: UnsafeMutableBufferPointer<Element>
  ) {
    _ensureFreeCapacity(items.count)
    _storage.append(moving: items)
  }
  
#if COLLECTIONS_UNSTABLE_CONTAINERS_PREVIEW
  /// Moves the elements of a input span to the end of this array, leaving the
  /// span empty.
  ///
  /// If the array does not have sufficient capacity to hold all new items,
  /// then this reallocates the array's storage to grow its capacity,
  /// using a geometric growth rate.
  ///
  /// - Parameters:
  ///    - items: An input span whose contents need to be appended to this array.
  ///
  /// - Complexity: O(`items.count`)
  @_alwaysEmitIntoClient
  public mutating func append(
    moving items: inout InputSpan<Element>
  ) {
    _ensureFreeCapacity(items.count)
    _storage.append(moving: &items)
  }
#endif

  /// Moves the elements of a output span to the end of this array, leaving the
  /// span empty.
  ///
  /// If the array does not have sufficient capacity to hold all new items,
  /// then this reallocates the array's storage to grow its capacity,
  /// using a geometric growth rate.
  ///
  /// - Parameters:
  ///    - items: An output span whose contents need to be appended to this array.
  ///
  /// - Complexity: O(`items.count`)
  @_alwaysEmitIntoClient
  public mutating func append(
    moving items: inout OutputSpan<Element>
  ) {
    _ensureFreeCapacity(items.count)
    _storage.append(moving: &items)
  }

  /// Appends the elements of a given array to the end of this array by moving
  /// them between the containers. On return, the input array becomes empty, but
  /// it is not destroyed, and it preserves its original storage capacity.
  ///
  /// If the target array does not have sufficient capacity to hold all items
  /// in the source array, then this automatically grows the target array's
  /// capacity, using a geometric growth rate.
  ///
  /// - Parameters:
  ///    - items: An array whose items to move to the end of this array.
  ///
  /// - Complexity: O(`items.count`) when amortized over many invocations on
  ///     the same array
  @_alwaysEmitIntoClient
  public mutating func append(
    moving items: inout RigidArray<Element>
  ) {
    // FIXME: Remove this in favor of a generic algorithm over range-replaceable containers
    _ensureFreeCapacity(items.count)
    _storage.append(moving: &items)
  }
}

@available(SwiftStdlib 5.0, *)
extension UniqueArray where Element: ~Copyable {
#if COLLECTIONS_UNSTABLE_CONTAINERS_PREVIEW
  /// Appends the elements of a given container to the end of this array by
  /// consuming the source container.
  ///
  /// If the target array does not have sufficient capacity to hold all items
  /// in the source array, then this automatically grows the target array's
  /// capacity, using a geometric growth rate.
  ///
  /// - Parameters:
  ///    - items: An array whose items to move to the end of this array.
  ///
  /// - Complexity: O(`items.count`)
  @_alwaysEmitIntoClient
  public mutating func append(
    consuming items: consuming RigidArray<Element>
  ) {
    // FIXME: Remove this in favor of a generic algorithm over consumable containers
    var items = items
    self.append(moving: &items)
  }
#endif
}

@available(SwiftStdlib 5.0, *)
extension UniqueArray {
  /// Copies the elements of a buffer to the end of this array.
  ///
  /// If the array does not have sufficient capacity to hold all items
  /// in the source buffer, then this automatically grows the array's
  /// capacity, using a geometric growth rate.
  ///
  /// - Parameters:
  ///    - newElements: A fully initialized buffer whose contents to copy into
  ///       the array.
  ///
  /// - Complexity: O(`newElements.count`) when amortized over many
  ///     invocations on the same array.
  @_alwaysEmitIntoClient
  public mutating func append(
    copying newElements: UnsafeBufferPointer<Element>
  ) {
    _ensureFreeCapacity(newElements.count)
    unsafe _storage.append(copying: newElements)
  }

  /// Copies the elements of a buffer to the end of this array.
  ///
  /// If the array does not have sufficient capacity to hold enough elements,
  /// then this reallocates the array's storage to extend its capacity, using
  /// a geometric growth rate.
  ///
  /// - Parameters:
  ///    - newElements: A fully initialized buffer whose contents to copy into
  ///       the array.
  ///
  /// - Complexity: O(`newElements.count`) when amortized over many
  ///     invocations on the same array.
  @_alwaysEmitIntoClient
  public mutating func append(
    copying newElements: UnsafeMutableBufferPointer<Element>
  ) {
    unsafe self.append(copying: UnsafeBufferPointer(newElements))
  }

  /// Copies the elements of a span to the end of this array.
  ///
  /// If the array does not have sufficient capacity to hold enough elements,
  /// then this reallocates the array's storage to extend its capacity, using a
  /// geometric growth rate.
  ///
  /// - Parameters:
  ///    - newElements: A span whose contents to copy into the array.
  ///
  /// - Complexity: O(`newElements.count`) when amortized over many
  ///     invocations on the same array.
  @_alwaysEmitIntoClient
  public mutating func append(copying newElements: Span<Element>) {
    _ensureFreeCapacity(newElements.count)
    _storage.append(copying: newElements)
  }

#if COLLECTIONS_UNSTABLE_CONTAINERS_PREVIEW
  @inlinable
  internal mutating func _append<
    Source: BorrowingSequence<Element> & ~Copyable & ~Escapable
  >(
    copying newElements: borrowing Source
  ) {
    _ensureFreeCapacity(newElements.underestimatedCount)
    var it = newElements.makeBorrowingIterator()
    while true {
      let span = it.nextSpan()
      if span.isEmpty { break }
      _ensureFreeCapacity(span.count)
      _storage.append(copying: span)
    }
  }
  // FIXME: Add _append(copyingContainer:), forwarding to the same method on RigidArray
#endif

#if COLLECTIONS_UNSTABLE_CONTAINERS_PREVIEW
  /// Copies the elements of a borrowing sequence to the end of this array.
  ///
  /// If the array does not have sufficient capacity to hold enough elements,
  /// then this reallocates the array's storage to extend its capacity, using
  /// a geometric growth rate. If the input sequence does not provide a precise
  /// estimate of its count, then the array's storage may need to be resized
  /// more than once.
  ///
  /// - Parameters:
  ///    - newElements: A container whose contents to copy into the array.
  ///
  /// - Complexity: O(`newElements.count`), when amortized over many invocations
  ///    over the same array.
  @_alwaysEmitIntoClient
  @inline(__always)
  public mutating func append<
    Source: BorrowingSequence<Element> & ~Copyable & ~Escapable
  >(
    copying newElements: borrowing Source
  ) {
    self._append(copying: newElements)
  }

#endif

  /// Copies the elements of a sequence to the end of this array.
  ///
  /// If the array does not have sufficient capacity to hold enough elements,
  /// then this reallocates the array's storage to extend its capacity, using
  /// a geometric growth rate. If the input sequence does not provide a precise
  /// estimate of its count, then the array's storage may need to be resized
  /// more than once.
  ///
  /// - Parameters:
  ///    - newElements: The new elements to copy into the array.
  ///
  /// - Complexity: O(*m*), where *m* is the length of `newElements`, when
  ///     amortized over many invocations over the same array.
  @_alwaysEmitIntoClient
  public mutating func append(copying newElements: some Sequence<Element>) {
    let done: Void? = newElements.withContiguousStorageIfAvailable { buffer in
      _ensureFreeCapacity(buffer.count)
      unsafe _storage.append(copying: buffer)
      return
    }
    if done != nil { return }

    _ensureFreeCapacity(newElements.underestimatedCount)
    var it = _storage._append(prefixOf: newElements)
    while let item = it.next() {
      _ensureFreeCapacity(1)
      _storage.append(item)
    }
  }
  
#if COLLECTIONS_UNSTABLE_CONTAINERS_PREVIEW
  /// Copies the elements of a container to the end of this array.
  ///
  /// If the array does not have sufficient capacity to hold enough elements,
  /// then this reallocates the array's storage to extend its capacity, using a
  /// geometric growth rate.
  ///
  /// - Parameters:
  ///    - newElements: A container whose contents to copy into the array.
  ///
  /// - Complexity: O(`newElements.count`), when amortized over many invocations
  ///    over the same array.
  @_alwaysEmitIntoClient
  @inline(__always)
  public mutating func append<
    Source: BorrowingSequence<Element> & Sequence<Element>
  >(copying newElements: Source) {
    self._append(copying: newElements)
  }
#endif
}

#endif
