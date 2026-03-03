//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

@available(SwiftStdlib 6.4, *)
extension RigidArray where Element: ~Copyable {
  /// Adds an element to the end of the array.
  ///
  /// If the array does not have sufficient capacity to hold any more elements,
  /// then this triggers a runtime error.
  ///
  /// - Parameter item: The element to append to the collection.
  ///
  /// - Complexity: O(1)
  @available(SwiftStdlib 6.4, *)
  @_alwaysEmitIntoClient
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
  @available(SwiftStdlib 6.4, *)
  @_alwaysEmitIntoClient
  public mutating func pushLast(_ item: consuming Element) -> Element? {
    if isFull { return item }
    append(item)
    return nil
  }
}

@available(SwiftStdlib 6.4, *)
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
  @available(SwiftStdlib 6.4, *)
  @_alwaysEmitIntoClient
  public mutating func append<E: Error>(
    addingCount newItemCount: Int,
    initializingWith initializer: (inout OutputSpan<Element>) throws(E) -> Void
  ) throws(E) {
    precondition(newItemCount >= 0, "Cannot add a negative number of items")
    precondition(freeCapacity >= newItemCount, "RigidArray capacity overflow")
    let buffer = unsafe _freeSpace._extracting(first: newItemCount)
    var span = unsafe OutputSpan(buffer: buffer, initializedCount: 0)
    defer {
      _count &+= unsafe span.finalize(for: buffer)
      span = OutputSpan()
    }
    return try initializer(&span)
  }
}

@available(SwiftStdlib 6.4, *)
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
  @available(SwiftStdlib 6.4, *)
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
  @available(SwiftStdlib 6.4, *)
  @_alwaysEmitIntoClient
  public mutating func append(
    moving items: inout OutputSpan<Element>
  ) {
    unsafe items.withUnsafeMutableBufferPointer { buffer, count in
      let source = unsafe buffer._extracting(first: count)
      unsafe self.append(moving: source)
      count = 0
    }
  }
}

@available(SwiftStdlib 6.4, *)
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
  @available(SwiftStdlib 6.4, *)
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
  @available(SwiftStdlib 6.4, *)
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
  @available(SwiftStdlib 6.4, *)
  @_alwaysEmitIntoClient
  public mutating func append(copying items: Span<Element>) {
    unsafe items.withUnsafeBufferPointer { source in
      unsafe self.append(copying: source)
    }
  }

  @_alwaysEmitIntoClient
  @_transparent
  internal mutating func _append<S: Sequence<Element>>(
    prefixOf items: S
  ) -> S.Iterator {
    let (it, c) = unsafe items._copyContents(initializing: _freeSpace)
    _count += c
    return it
  }

  /// Copies the elements of a sequence to the end of this array.
  ///
  /// If the array does not have sufficient capacity to hold all items in the
  /// sequence, then this triggers a runtime error.
  ///
  /// - Parameters:
  ///    - newElements: The new elements to copy into the array.
  ///
  /// - Complexity: O(*m*), where *m* is the length of `newElements`.
  @available(SwiftStdlib 6.4, *)
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
}
