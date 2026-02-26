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
extension RigidArray where Element: ~Copyable {
  /// Adds an element to the end of the array.
  ///
  /// If the array does not have sufficient capacity to hold any more elements,
  /// then this triggers a runtime error.
  ///
  /// - Parameter item: The element to append to the collection.
  ///
  /// - Complexity: O(1)
  @inlinable
  public mutating func append(_ item: consuming Element) {
    precondition(!isFull, "RigidArray capacity overflow")
    unsafe _storage.initializeElement(at: _count, to: item)
    _count &+= 1
  }

  /// Adds an element to the end of the array, if possible.
  ///
  /// If the array does not have sufficient capacity to hold any more elements,
  /// then this returns the given item without appending it; otherwise it
  /// returns nil.
  ///
  /// - Parameter item: The element to append to the array.
  /// - Returns: `item` if the array is full; otherwise nil.
  ///
  /// - Complexity: O(1)
  @inlinable
  public mutating func pushLast(_ item: consuming Element) -> Element? {
    if isFull { return item }
    append(item)
    return nil
  }
}

@available(SwiftStdlib 5.0, *)
extension RigidArray where Element: ~Copyable {
  /// Append a given number of items to the end of this array by populating
  /// an output span.
  ///
  /// If the array does not have sufficient capacity to store the new items in
  /// the buffer, then this triggers a runtime error.
  ///
  /// If the callback fails to fully populate its output span or if
  /// it throws an error, then the array keeps all items that were
  /// successfully initialized before the callback terminated the insertion.
  ///
  /// - Parameters:
  ///    - newItemCount: The number of items to append to the array.
  ///    - initializer: A callback that gets called at most once to directly
  ///       populate newly reserved storage within the array. The function
  ///       is allowed to initialize fewer than `newItemCount` items.
  ///       The array is appended however many items the callback adds to
  ///       the output span before it returns (or before it throws an error).
  ///
  /// - Complexity: O(`newItemCount`)
  @_alwaysEmitIntoClient
  public mutating func append<E: Error>(
    addingCount newItemCount: Int,
    initializingWith initializer: (inout OutputSpan<Element>) throws(E) -> Void
  ) throws(E) {
    precondition(newItemCount >= 0, "Cannot add a negative number of items")
    precondition(freeCapacity >= newItemCount, "RigidArray capacity overflow")
    let buffer = _freeSpace._extracting(first: newItemCount)
    var span = OutputSpan(buffer: buffer, initializedCount: 0)
    defer {
      _count &+= span.finalize(for: buffer)
      span = OutputSpan()
    }
    return try initializer(&span)
  }
}

@available(SwiftStdlib 5.0, *)
extension RigidArray where Element: ~Copyable {
  /// Moves the elements of a buffer to the end of this array, leaving the
  /// buffer uninitialized.
  ///
  /// If the array does not have sufficient capacity to hold all items in the
  /// buffer, then this triggers a runtime error.
  ///
  /// - Parameters:
  ///    - items: A fully initialized buffer whose contents to move into
  ///        the array.
  ///
  /// - Complexity: O(`items.count`)
  @_alwaysEmitIntoClient
  public mutating func append(
    moving items: UnsafeMutableBufferPointer<Element>
  ) {
    precondition(items.count <= freeCapacity, "RigidArray capacity overflow")
    guard items.count > 0 else { return }
    let c = unsafe _freeSpace._moveInitializePrefix(from: items)
    assert(c == items.count)
    _count &+= items.count
  }

#if COLLECTIONS_UNSTABLE_CONTAINERS_PREVIEW
  /// Moves the elements of a input span to the end of this array, leaving the
  /// span empty.
  ///
  /// If the array does not have sufficient capacity to hold all items in its
  /// storage, then this triggers a runtime error.
  ///
  /// - Parameters:
  ///    - items: An input span whose contents need to be appended to this array.
  ///
  /// - Complexity: O(`items.count`)
  @_alwaysEmitIntoClient
  public mutating func append(
    moving items: inout InputSpan<Element>
  ) {
    items.withUnsafeMutableBufferPointer { buffer, count in
      let source = buffer._extracting(last: count)
      unsafe self.append(moving: source)
      count = 0
    }
  }
#endif

  /// Moves the elements of an output span to the end of this array, leaving the
  /// span empty.
  ///
  /// If the array does not have sufficient capacity to hold all items in its
  /// storage, then this triggers a runtime error.
  ///
  /// - Parameters:
  ///    - items: An output span whose contents need to be appended to this array.
  ///
  /// - Complexity: O(`items.count`)
  @_alwaysEmitIntoClient
  public mutating func append(
    moving items: inout OutputSpan<Element>
  ) {
    items.withUnsafeMutableBufferPointer { buffer, count in
      let source = buffer._extracting(first: count)
      unsafe self.append(moving: source)
      count = 0
    }
  }

  /// Appends the elements of a given array to the end of this array by moving
  /// them between the containers. On return, the input array becomes empty, but
  /// it is not destroyed, and it preserves its original storage capacity.
  ///
  /// If the target array does not have sufficient capacity to hold all items
  /// in the source array, then this triggers a runtime error.
  ///
  /// - Parameters:
  ///    - items: An array whose items to move to the end of this array.
  ///
  /// - Complexity: O(`items.count`)
  @_alwaysEmitIntoClient
  public mutating func append(
    moving items: inout RigidArray<Element>
  ) {
    // FIXME: Remove this in favor of a generic algorithm over range-replaceable containers
    items.edit { span in
      self.append(moving: &span)
    }
  }
}

@available(SwiftStdlib 5.0, *)
extension RigidArray where Element: ~Copyable {
#if COLLECTIONS_UNSTABLE_CONTAINERS_PREVIEW
  /// Appends the elements of a given container to the end of this array by
  /// consuming the source container.
  ///
  /// If the target array does not have sufficient capacity to hold all items
  /// in the source array, then this triggers a runtime error.
  ///
  /// - Parameters:
  ///    - items: A container whose contents to move into this array.
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
extension RigidArray {
  /// Copies the elements of a buffer to the end of this array.
  ///
  /// If the array does not have sufficient capacity to hold all items in the
  /// buffer, then this triggers a runtime error.
  ///
  /// - Parameters:
  ///    - newElements: A fully initialized buffer whose contents to copy into
  ///       the array.
  ///
  /// - Complexity: O(`newElements.count`)
  @_alwaysEmitIntoClient
  public mutating func append(
    copying newElements: UnsafeBufferPointer<Element>
  ) {
    precondition(
      newElements.count <= freeCapacity,
      "RigidArray capacity overflow")
    guard newElements.count > 0 else { return }
    unsafe _freeSpace.baseAddress.unsafelyUnwrapped.initialize(
      from: newElements.baseAddress.unsafelyUnwrapped, count: newElements.count)
    _count &+= newElements.count
  }

  /// Copies the elements of a buffer to the end of this array.
  ///
  /// If the array does not have sufficient capacity to hold all items in the
  /// buffer, then this triggers a runtime error.
  ///
  /// - Parameters:
  ///    - newElements: A fully initialized buffer whose contents to copy into
  ///        the array.
  ///
  /// - Complexity: O(`newElements.count`)
  @_alwaysEmitIntoClient
  public mutating func append(
    copying items: UnsafeMutableBufferPointer<Element>
  ) {
    unsafe self.append(copying: UnsafeBufferPointer(items))
  }

  /// Copies the elements of a span to the end of this array.
  ///
  /// If the array does not have sufficient capacity to hold all items in the
  /// span, then this triggers a runtime error.
  ///
  /// - Parameters:
  ///    - newElements: A span whose contents to copy into the array.
  ///
  /// - Complexity: O(`newElements.count`)
  @_alwaysEmitIntoClient
  public mutating func append(copying items: Span<Element>) {
    unsafe items.withUnsafeBufferPointer { source in
      unsafe self.append(copying: source)
    }
  }

  @_alwaysEmitIntoClient
  @inline(__always)
  internal mutating func _append<S: Sequence<Element>>(
    prefixOf items: S
  ) -> S.Iterator {
    let (it, c) = unsafe items._copyContents(initializing: _freeSpace)
    _count += c
    return it
  }
  
#if COLLECTIONS_UNSTABLE_CONTAINERS_PREVIEW
  @inlinable
  internal mutating func _append<
    Source: BorrowingSequence<Element> & ~Copyable & ~Escapable
  >(
    copying newElements: borrowing Source
  ) {
    let target = _freeSpace
    _count += newElements._copyContents(intoPrefixOf: target)
  }
#endif

#if COLLECTIONS_UNSTABLE_CONTAINERS_PREVIEW
  /// Copies the elements of a borrowing sequence to the end of this array.
  ///
  /// If the array does not have sufficient capacity to hold all items in the
  /// source sequence, then this triggers a runtime error.
  ///
  /// - Parameters:
  ///    - newElements: A container whose contents to copy into the array.
  ///
  /// - Complexity: O(`newElements.count`)
  @_alwaysEmitIntoClient
  @inline(__always)
  public mutating func append<
    Source: BorrowingSequence<Element> & ~Copyable & ~Escapable
  >(
    copying newElements: borrowing Source
  ) {
    _append(copying: newElements)
  }
#endif

  /// Copies the elements of a sequence to the end of this array.
  ///
  /// If the array does not have sufficient capacity to hold all items in the
  /// sequence, then this triggers a runtime error.
  ///
  /// - Parameters:
  ///    - newElements: The new elements to copy into the array.
  ///
  /// - Complexity: O(*m*), where *m* is the length of `newElements`.
  @_alwaysEmitIntoClient
  public mutating func append(copying newElements: some Sequence<Element>) {
    let done: Void? = newElements.withContiguousStorageIfAvailable { buffer in
      unsafe self.append(copying: buffer)
      return
    }
    if done != nil { return }

    var it = self._append(prefixOf: newElements)
    precondition(it.next() == nil, "RigidArray capacity overflow")
  }
  
#if COLLECTIONS_UNSTABLE_CONTAINERS_PREVIEW
  /// Copies the elements of a borrowing sequence to the end of this array.
  ///
  /// If the array does not have sufficient capacity to hold all items in the
  /// source sequence, then this triggers a runtime error.
  ///
  /// - Parameters:
  ///    - newElements: The new elements to copy into the array.
  ///
  /// - Complexity: O(*m*), where *m* is the length of `newElements`.
  @_alwaysEmitIntoClient
  @inline(__always)
  public mutating func append<
    Source: BorrowingSequence<Element> & Sequence<Element>
  >(copying newElements: Source) {
    _append(copying: newElements)
  }
#endif
}
#endif
