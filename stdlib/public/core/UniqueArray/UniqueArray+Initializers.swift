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
  /// Initializes a new unique array with no elements.
  @inlinable
  public init() {
    _storage = .init(capacity: 0)
  }
  
  /// Initializes a new unique array with the specified capacity and no elements.
  @inlinable
  public init(capacity: Int) {
    _storage = .init(capacity: capacity)
  }
}

@available(SwiftStdlib 5.0, *)
extension UniqueArray where Element: ~Copyable {
  /// Creates a new array with the specified capacity, directly initializing
  /// its storage using an output span.
  ///
  /// - Parameters:
  ///   - capacity: The storage capacity of the new array.
  ///   - body: A callback that gets called at most once to directly
  ///       populate newly reserved storage within the array. The function
  ///       is allowed to add fewer than `capacity` items. The array is
  ///       initialized with however many items the callback adds to the
  ///       output span before it returns (or before it throws an error).
  @inlinable
  public init<E: Error>(
    capacity: Int,
    initializingWith body: (inout OutputSpan<Element>) throws(E) -> Void
  ) throws(E) {
    self.init(capacity: capacity)
    try edit(body)
  }
}

@available(SwiftStdlib 5.0, *)
extension UniqueArray /*where Element: Copyable*/ {
  /// Creates a new array containing the specified number of a single,
  /// repeated value.
  ///
  /// - Parameters:
  ///   - repeatedValue: The element to repeat.
  ///   - count: The number of times to repeat the value passed in the
  ///     `repeating` parameter. `count` must be zero or greater.
  ///
  /// - Complexity: O(`count`)
  public init(repeating repeatedValue: Element, count: Int) {
    self.init(consuming: RigidArray(repeating: repeatedValue, count: count))
  }
}

@available(SwiftStdlib 5.0, *)
extension UniqueArray where Element: ~Copyable {
  /// Creates a new unique array taking over the storage of the specified
  /// rigid array instance, consuming it in the process.
  ///
  /// - Complexity: O(1)
  @inlinable
  public init(consuming storage: consuming RigidArray<Element>) {
    self._storage = storage
  }
}

@available(SwiftStdlib 5.0, *)
extension UniqueArray /*where Element: Copyable*/ {
#if COLLECTIONS_UNSTABLE_CONTAINERS_PREVIEW
  /// Creates a new array with the specified initial capacity, holding a copy
  /// of the contents of a given borrowing sequence.
  ///
  /// - Parameters:
  ///   - capacity: The storage capacity of the new array, or nil to allocate
  ///      just enough capacity to store the contents.
  ///   - contents: A sequence whose contents to copy into the new array.
  ///      The sequence must not contain more than `capacity` elements.
  @_alwaysEmitIntoClient
  @inline(__always)
  public init<Source: BorrowingSequence<Element> & ~Copyable & ~Escapable>(
    capacity: Int? = nil,
    copying contents: borrowing Source
  ) {
    self.init(capacity: capacity ?? 0)
    self.append(copying: contents)
  }
#endif

  /// Creates a new array with the specified initial capacity, holding a copy
  /// of the contents of a given sequence.
  ///
  /// - Parameters:
  ///   - capacity: The storage capacity of the new array, or nil to allocate
  ///      just enough capacity to store the contents.
  ///   - contents: The sequence whose contents to copy into the new array.
  @_alwaysEmitIntoClient
  @inline(__always)
  public init(
    capacity: Int? = nil,
    copying contents: some Sequence<Element>
  ) {
    self.init(capacity: capacity ?? 0)
    self.append(copying: contents)
  }
  
#if COLLECTIONS_UNSTABLE_CONTAINERS_PREVIEW
  /// Creates a new array with the specified initial capacity, holding a copy
  /// of the contents of a given container.
  ///
  /// - Parameters:
  ///   - capacity: The storage capacity of the new array, or nil to allocate
  ///      just enough capacity to store the contents.
  ///   - contents: The container whose contents to copy into the new array.
  @_alwaysEmitIntoClient
  @inline(__always)
  public init<Source: BorrowingSequence<Element> & Sequence<Element>>(
    capacity: Int? = nil,
    copying contents: Source
  ) {
    self.init(capacity: capacity ?? 0)
    self.append(copying: contents)
  }
#endif

  /// Creates a new array with the specified capacity, holding a copy
  /// of the contents of the given span.
  ///
  /// - Parameters:
  ///   - capacity: The storage capacity of the new array, or nil to allocate
  ///      just enough capacity to store the contents of the span.
  ///   - span: The span whose contents to copy into the new array.
  ///      The span must not contain more than `capacity` elements.
  @_alwaysEmitIntoClient
  @inline(__always)
  public init(
    capacity: Int? = nil,
    copying span: Span<Element>
  ) {
    self.init(capacity: capacity ?? span.count)
    self.append(copying: span)
  }
}

#endif
