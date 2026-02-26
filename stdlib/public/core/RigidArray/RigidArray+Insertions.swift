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
  /// Inserts a new element into the array at the specified position.
  ///
  /// If the array does not have sufficient capacity to hold any more elements,
  /// then this triggers a runtime error.
  ///
  /// The new element is inserted before the element currently at the specified
  /// index. If you pass the array's `endIndex` as the `index` parameter, then
  /// the new element is appended to the container.
  ///
  /// All existing elements at or following the specified position are moved to
  /// make room for the new item.
  ///
  /// - Parameter item: The new element to insert into the array.
  /// - Parameter index: The position at which to insert the new element.
  ///   `index` must be a valid index in the array.
  ///
  /// - Complexity: O(`self.count`)
  @inlinable
  public mutating func insert(_ item: consuming Element, at index: Int) {
    _checkValidIndex(index)
    precondition(!isFull, "RigidArray capacity overflow")
    if index < count {
      let source = unsafe _storage.extracting(index ..< count)
      let target = unsafe _storage.extracting(index + 1 ..< count + 1)
      let last = unsafe target.moveInitialize(fromContentsOf: source)
      assert(last == target.endIndex)
    }
    unsafe _storage.initializeElement(at: index, to: item)
    _count += 1
  }
}

@available(SwiftStdlib 5.0, *)
extension RigidArray where Element: ~Copyable {
  /// Inserts a given number of new items into this array at the specified
  /// position, using a callback to directly initialize array storage by
  /// populating an output span.
  ///
  /// Existing elements in the array's storage are moved towards the back as
  /// needed to make room for the new items.
  ///
  /// If the capacity of the array isn't sufficient to accommodate the specified
  /// number of new elements, then this method triggers a runtime error.
  ///
  ///     var buffer = RigidArray<Int>(capacity: 20)
  ///     buffer.append([-999, 999])
  ///     var i = 0
  ///     buffer.insert(capacity: 3, at: 1) { target in
  ///       while !target.isFull {
  ///         target.append(i)
  ///         i += 1
  ///       }
  ///     }
  ///     // `buffer` now contains [-999, 0, 1, 2, 999]
  ///
  /// If the callback fails to fully populate its output span or if
  /// it throws an error, then the array keeps all items that were
  /// successfully initialized before the callback terminated the insertion.
  ///
  /// Partial insertions create a gap in array storage that needs to be
  /// closed by moving already inserted items to their correct positions given
  /// the adjusted count. This adds some overhead compared to adding exactly as
  /// many items as promised.
  ///
  /// - Parameters:
  ///    - newItemCount: The maximum number of items to insert into the array.
  ///    - index: The position at which to insert the new items.
  ///       `index` must be a valid index in the array.
  ///    - initializer: A callback that gets called at most once to directly
  ///       populate newly reserved storage within the array. The function
  ///      is always called with an empty output span.
  ///
  /// - Complexity: O(`self.count` + `newItemCount`) in addition to the complexity
  ///    of the callback invocations.
  @_alwaysEmitIntoClient
  @inline(__always)
  public mutating func insert<E: Error>(
    addingCount newItemCount: Int,
    at index: Int,
    initializingWith initializer: (inout OutputSpan<Element>) throws(E) -> Void
  ) throws(E) {
    _checkValidIndex(index)
    precondition(newItemCount >= 0, "Cannot add a negative number of items")
    precondition(newItemCount <= freeCapacity, "RigidArray capacity overflow")
    let target = unsafe _openGap(at: index, count: newItemCount)
    _count &+= newItemCount
    var span = OutputSpan(buffer: target, initializedCount: 0)
    defer {
      let c = span.finalize(for: target)
      if c < newItemCount {
        _closeGap(at: index &+ c, count: newItemCount &- c)
        _count &-= newItemCount &- c
      }
      span = OutputSpan()
    }
    try initializer(&span)
  }
}

@available(SwiftStdlib 5.0, *)
extension RigidArray where Element: ~Copyable {
  /// Moves the elements of a fully initialized buffer into this array,
  /// starting at the specified position, and leaving the buffer
  /// uninitialized.
  ///
  /// All existing elements at or following the specified position are moved to
  /// make room for the new items.
  ///
  /// If the capacity of the array isn't sufficient to accommodate the new
  /// elements, then this method triggers a runtime error.
  ///
  /// - Parameters:
  ///    - items: A fully initialized buffer whose contents to move into
  ///        the array.
  ///    - index: The position at which to insert the new items.
  ///       `index` must be a valid index in the array.
  ///
  /// - Complexity: O(`self.count` + `items.count`)
  @_alwaysEmitIntoClient
  public mutating func insert(
    moving items: UnsafeMutableBufferPointer<Element>,
    at index: Int
  ) {
    insert(addingCount: items.count, at: index) { target in
      target._append(moving: items)
    }
  }

#if COLLECTIONS_UNSTABLE_CONTAINERS_PREVIEW
  /// Moves the elements of an input span into this array,
  /// starting at the specified position, and leaving the span empty.
  ///
  /// All existing elements at or following the specified position are moved to
  /// make room for the new items.
  ///
  /// If the capacity of the array isn't sufficient to accommodate the new
  /// elements, then this method triggers a runtime error.
  ///
  /// - Parameters:
  ///    - items: An input span whose contents to move into
  ///        the array.
  ///    - index: The position at which to insert the new items.
  ///       `index` must be a valid index in the array.
  ///
  /// - Complexity: O(`self.count` + `items.count`)
  @_alwaysEmitIntoClient
  public mutating func insert(
    moving items: inout InputSpan<Element>,
    at index: Int
  ) {
    items.withUnsafeMutableBufferPointer { buffer, count in
      let source = buffer._extracting(last: count)
      unsafe self.insert(moving: source, at: index)
      count = 0
    }
  }
#endif

  /// Moves the elements of an output span into this array,
  /// starting at the specified position, and leaving the span empty.
  ///
  /// All existing elements at or following the specified position are moved to
  /// make room for the new items.
  ///
  /// If the capacity of the array isn't sufficient to accommodate the new
  /// elements, then this method triggers a runtime error.
  ///
  /// - Parameters:
  ///    - items: An output span whose contents to move into
  ///        the array.
  ///    - index: The position at which to insert the new items.
  ///       `index` must be a valid index in the array.
  ///
  /// - Complexity: O(`self.count` + `items.count`)
  @_alwaysEmitIntoClient
  public mutating func insert(
    moving items: inout OutputSpan<Element>,
    at index: Int
  ) {
    items.withUnsafeMutableBufferPointer { buffer, count in
      let source = buffer._extracting(first: count)
      unsafe self.insert(moving: source, at: index)
      count = 0
    }
  }

  /// Inserts the elements of a given array into the given position in this
  /// array by moving them between the containers. On return, the input array
  /// becomes empty, but it is not destroyed, and it preserves its original
  /// storage capacity.
  ///
  /// All existing elements at or following the specified position are moved to
  /// make room for the new items.
  ///
  /// If the capacity of the array isn't sufficient to accommodate the new
  /// elements, then this method triggers a runtime error.
  ///
  /// - Parameters:
  ///    - items: An array whose contents to move into `self`.
  ///    - index: The position at which to insert the new items.
  ///       `index` must be a valid index in the array.
  ///
  /// - Complexity: O(`count` + `items.count`)
  @_alwaysEmitIntoClient
  public mutating func insert(
    moving items: inout RigidArray<Element>,
    at index: Int
  ) {
    // FIXME: Remove this in favor of a generic algorithm over consumable containers
    guard !items.isEmpty else { return }
    items.edit { source in
      self.insert(moving: &source, at: index)
    }
  }
}

@available(SwiftStdlib 5.0, *)
extension RigidArray where Element: ~Copyable {
#if COLLECTIONS_UNSTABLE_CONTAINERS_PREVIEW
  /// Inserts the elements of a given array into the given position in this
  /// array by consuming the source container.
  ///
  /// All existing elements at or following the specified position are moved to
  /// make room for the new items.
  ///
  /// If the capacity of the array isn't sufficient to accommodate the new
  /// elements, then this method triggers a runtime error.
  ///
  /// - Parameters:
  ///    - items: The array whose contents to move into `self`.
  ///    - index: The position at which to insert the new items.
  ///       `index` must be a valid index in the array.
  ///
  /// - Complexity: O(`count` + `items.count`)
  @_alwaysEmitIntoClient
  public mutating func insert(
    consuming items: consuming RigidArray<Element>,
    at index: Int
  ) {
    // FIXME: Remove this in favor of a generic algorithm over consumable containers
    var items = items
    self.insert(moving: &items, at: index)
  }
#endif
}

@available(SwiftStdlib 5.0, *)
extension RigidArray {
  /// Copies the elements of a fully initialized buffer pointer into this
  /// array at the specified position.
  ///
  /// The new elements are inserted before the element currently at the
  /// specified index. If you pass the array’s `endIndex` as the `index`
  /// parameter, then the new elements are appended to the end of the array.
  ///
  /// All existing elements at or following the specified position are moved to
  /// make room for the new items.
  ///
  /// If the capacity of the array isn't sufficient to accommodate the new
  /// elements, then this method triggers a runtime error.
  ///
  /// - Parameters:
  ///    - newElements: The new elements to insert into the array. The buffer
  ///       must be fully initialized.
  ///    - index: The position at which to insert the new elements. It must be
  ///       a valid index of the array.
  ///
  /// - Complexity: O(`count` + `newElements.count`)
  @inlinable
  public mutating func insert(
    copying newElements: UnsafeBufferPointer<Element>, at index: Int
  ) {
    guard newElements.count > 0 else { return }
    self.insert(addingCount: newElements.count, at: index) { target in
      target._append(copying: newElements)
    }
  }

  /// Copies the elements of a fully initialized buffer pointer into this
  /// array at the specified position.
  ///
  /// The new elements are inserted before the element currently at the
  /// specified index. If you pass the array’s `endIndex` as the `index`
  /// parameter, then the new elements are appended to the end of the array.
  ///
  /// All existing elements at or following the specified position are moved to
  /// make room for the new item.
  ///
  /// If the capacity of the array isn't sufficient to accommodate the new
  /// elements, then this method triggers a runtime error.
  ///
  /// - Parameters:
  ///    - newElements: The new elements to insert into the array. The buffer
  ///       must be fully initialized.
  ///    - index: The position at which to insert the new elements. It must be
  ///       a valid index of the array.
  ///
  /// - Complexity: O(`count` + `newElements.count`)
  @inlinable
  @inline(__always)
  public mutating func insert(
    copying newElements: UnsafeMutableBufferPointer<Element>,
    at index: Int
  ) {
    unsafe self.insert(copying: UnsafeBufferPointer(newElements), at: index)
  }

  /// Copies the elements of a span into this array at the specified position.
  ///
  /// The new elements are inserted before the element currently at the
  /// specified index. If you pass the array’s `endIndex` as the `index`
  /// parameter, then the new elements are appended to the end of the array.
  ///
  /// All existing elements at or following the specified position are moved to
  /// make room for the new item.
  ///
  /// If the capacity of the array isn't sufficient to accommodate the new
  /// elements, then this method triggers a runtime error.
  ///
  /// - Parameters:
  ///    - newElements: The new elements to insert into the array.
  ///    - index: The position at which to insert the new elements. It must be
  ///        a valid index of the array.
  ///
  /// - Complexity: O(`count` + `newElements.count`)
  @inlinable
  @inline(__always)
  public mutating func insert(
    copying newElements: Span<Element>, at index: Int
  ) {
    guard newElements.count > 0 else { return }
    self.insert(addingCount: newElements.count, at: index) { target in
      target._append(copying: newElements)
    }
  }

#if COLLECTIONS_UNSTABLE_CONTAINERS_PREVIEW
  @_alwaysEmitIntoClient
  internal mutating func _insertContainer<
    C: Container<Element> & ~Copyable & ~Escapable
  >(
    at index: Int,
    copying items: borrowing C,
    newCount: Int
  ) {
    var it = items.makeBorrowingIterator()
    insert(addingCount: newCount, at: index) { target in
      while !target.isFull {
        let source = it.nextSpan(maximumCount: target.freeCapacity)
        precondition(!source.isEmpty, "Broken container: mismatching count")
        target._append(copying: source)
      }
    }
    precondition(it.nextSpan().isEmpty, "Broken container: mismatching count")
  }
#endif

  @inlinable
  internal mutating func _insertCollection(
    at index: Int,
    copying items: some Collection<Element>,
    newCount: Int
  ) {
    _checkValidIndex(index)
    precondition(newCount <= freeCapacity, "RigidArray capacity overflow")
    let gap = unsafe _openGap(at: index, count: newCount)

    let done: Void? = items.withContiguousStorageIfAvailable { buffer in
      let i = unsafe gap._initializePrefix(copying: buffer)
      precondition(
        i == newCount,
        "Broken Collection: count doesn't match contents")
      _count += newCount
    }
    if done != nil { return }

    var (it, copied) = unsafe items._copyContents(initializing: gap)
    precondition(
      it.next() == nil && copied == newCount,
      "Broken Collection: count doesn't match contents")
    _count += newCount
  }

#if COLLECTIONS_UNSTABLE_CONTAINERS_PREVIEW
  /// Copies the elements of a container into this array at the specified
  /// position.
  ///
  /// The new elements are inserted before the element currently at the
  /// specified index. If you pass the array’s `endIndex` as the `index`
  /// parameter, then the new elements are appended to the end of the array.
  ///
  /// All existing elements at or following the specified position are moved to
  /// make room for the new item.
  ///
  /// If the capacity of the array isn't sufficient to accommodate the new
  /// elements, then this method triggers a runtime error.
  ///
  /// - Parameters:
  ///    - newElements: The new elements to insert into the array.
  ///    - index: The position at which to insert the new elements. It must be
  ///        a valid index of the array.
  ///
  /// - Complexity: O(`self.count` + `newElements.count`).
  @_alwaysEmitIntoClient
  @inline(__always)
  public mutating func insert<
    C: Container<Element> & ~Copyable & ~Escapable
  >(
    copying newElements: borrowing C, at index: Int
  ) {
    _insertContainer(
      at: index, copying: newElements, newCount: newElements.count)
  }
#endif

  /// Copies the elements of a collection into this array at the specified
  /// position.
  ///
  /// The new elements are inserted before the element currently at the
  /// specified index. If you pass the array’s `endIndex` as the `index`
  /// parameter, then the new elements are appended to the end of the array.
  ///
  /// All existing elements at or following the specified position are moved
  /// to make room for the new item.
  ///
  /// If the capacity of the array isn't sufficient to accommodate the new
  /// elements, then this method triggers a runtime error.
  ///
  /// - Parameters:
  ///    - newElements: The new elements to insert into the array.
  ///    - index: The position at which to insert the new elements. It must be
  ///        a valid index of the array.
  ///
  /// - Complexity: O(`count` + `newElements.count`)
  @inlinable
  @inline(__always)
  public mutating func insert(
    copying newElements: some Collection<Element>, at index: Int
  ) {
    _insertCollection(
      at: index, copying: newElements, newCount: newElements.count)
  }

#if COLLECTIONS_UNSTABLE_CONTAINERS_PREVIEW
  /// Copies the elements of a container into this array at the specified
  /// position.
  ///
  /// The new elements are inserted before the element currently at the
  /// specified index. If you pass the array’s `endIndex` as the `index`
  /// parameter, then the new elements are appended to the end of the array.
  ///
  /// All existing elements at or following the specified position are moved to
  /// make room for the new item.
  ///
  /// If the capacity of the array isn't sufficient to accommodate the new
  /// elements, then this method triggers a runtime error.
  ///
  /// - Parameters:
  ///    - newElements: The new elements to insert into the array.
  ///    - index: The position at which to insert the new elements. It must be
  ///        a valid index of the array.
  ///
  /// - Complexity: O(`self.count` + `newElements.count`)
  @_alwaysEmitIntoClient
  @inline(__always)
  public mutating func insert<
    C: Container<Element> & Collection<Element>
  >(
    copying newElements: borrowing C, at index: Int
  ) {
    _insertContainer(
      at: index, copying: newElements, newCount: newElements.count)
  }
#endif
}

#endif
