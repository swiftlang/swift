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

#if compiler(<6.2)

/// A dynamically self-resizing, heap allocated, noncopyable array
/// of potentially noncopyable elements.
@frozen
@available(*, unavailable, message: "UniqueArray requires a Swift 6.2 toolchain")
public struct UniqueArray<Element: ~Copyable>: ~Copyable {
  @usableFromInline
  internal var _storage: RigidArray<Element>

  @inlinable
  public init() {
    fatalError()
  }
}

#else

/// A dynamically self-resizing, heap allocated, noncopyable array of
/// potentially noncopyable elements.
///
/// `UniqueArray` instances automatically resize their underlying storage as
/// needed to accommodate newly inserted items, using a geometric growth curve.
/// This lets code using `UniqueArray` avoid having to allocate enough
/// capacity in advance; on the other hand, it makes it difficult to tell
/// when and where such reallocations may happen.
///
/// For example, appending an element to a `UniqueArray` has highly variable
/// complexity; often, it runs at a constant cost, but if the operation has to
/// resize storage, then the cost of an individual append suddenly becomes
/// proportional to the size of the whole array.
///
/// The geometric growth curve allows the cost of such latency spikes to
/// get amortized across repeated invocations, bringing the average cost back
/// to O(1); but the spikes make this construct less suitable for use cases that
/// expect predictable, consistent performance on every operation.
///
/// Implicit growth also makes it more difficult to predict/analyze the amount
/// of memory an algorithm would need. Developers targeting environments with
/// stringent limits on heap allocations may prefer to avoid using dynamically
/// resizing container types as a matter of policy. The type `RigidArray` provides
/// a fixed-capacity array variant that caters specifically for these use cases,
/// trading ease-of-use for more consistent/predictable execution.
/// For copyable elements, the copy-on-write `Array` type is an
/// even more convenient and expressive choice.
@available(SwiftStdlib 5.0, *)
@frozen
public struct UniqueArray<Element: ~Copyable>: ~Copyable {
  @usableFromInline
  internal var _storage: RigidArray<Element>

  @_alwaysEmitIntoClient
  package init(_storage: consuming RigidArray<Element>) {
    self._storage = _storage
  }
}

@available(SwiftStdlib 5.0, *)
extension UniqueArray: Sendable where Element: Sendable & ~Copyable {}

//MARK: - Basics

@available(SwiftStdlib 5.0, *)
extension UniqueArray where Element: ~Copyable {
  /// The maximum number of elements this array can hold without having to
  /// reallocate its storage.
  ///
  /// - Complexity: O(1)
  @inlinable
  @inline(__always)
  public var capacity: Int { _assumeNonNegative(_storage.capacity) }

  /// The number of additional elements that can be added to this array without
  /// reallocating its storage.
  ///
  /// - Complexity: O(1)
  @inlinable
  @inline(__always)
  public var freeCapacity: Int {
    _assumeNonNegative(_storage.capacity &- _storage.count)
  }
}

//MARK: - Span creation

@available(SwiftStdlib 5.0, *)
extension UniqueArray where Element: ~Copyable {
  /// A span over the elements of this array, providing direct read-only access.
  ///
  /// - Complexity: O(1)
  public var span: Span<Element> {
    @_lifetime(borrow self)
    @inlinable
    get {
      _storage.span
    }
  }

  /// A mutable span over the elements of this array, providing direct
  /// mutating access.
  ///
  /// - Complexity: O(1)
  @available(SwiftStdlib 5.0, *)
  public var mutableSpan: MutableSpan<Element> {
    @_lifetime(&self)
    @inlinable
    mutating get {
      _storage.mutableSpan
    }
  }
}

@available(SwiftStdlib 5.0, *)
extension UniqueArray where Element: ~Copyable {
  /// Arbitrarily edit the storage underlying this array by invoking a
  /// user-supplied closure with a mutable `OutputSpan` view over it.
  /// This method calls its function argument at most once, allowing it to
  /// arbitrarily modify the contents of the output span it is given.
  /// The argument is free to add, remove or reorder any items; however,
  /// it is not allowed to replace the span or change its capacity.
  ///
  /// When the function argument finishes (whether by returning or throwing an
  /// error) the rigid array instance is updated to match the final contents of
  /// the output span.
  ///
  /// - Parameter body: A function that edits the contents of this array through
  ///    an `OutputSpan` argument. This method invokes this function
  ///    at most once.
  /// - Returns: This method returns the result of its function argument.
  /// - Complexity: Adds O(1) overhead to the complexity of the function
  ///    argument.
  @inlinable @inline(__always)
  public mutating func edit<E: Error, R: ~Copyable>(
    _ body: (inout OutputSpan<Element>) throws(E) -> R
  ) throws(E) -> R {
    try _storage.edit(body)
  }
}

//MARK: - Container primitives

@available(SwiftStdlib 5.0, *)
extension UniqueArray where Element: ~Copyable {
  /// Return a borrowing span over the maximal storage chunk following the
  /// specified position in the array. The span provides direct read-only access
  /// to all array elements in the range `index ..< count`.
  ///
  /// - Parameter index: A valid index in the array, including the end index.
  ///
  /// - Complexity: O(1)
  @inlinable
  @_lifetime(borrow self)
  public func span(after index: inout Int) -> Span<Element> {
    _storage.span(after: &index)
  }

  /// Return a borrowing span over the maximal storage chunk preceding the
  /// specified position in the array. The span provides direct read-only access
  /// to all array elements in the range `0 ..< index`.
  ///
  /// - Parameter index: A valid index in the array, including the end index.
  ///
  /// - Complexity: O(1)
  @inlinable
  @_lifetime(borrow self)
  public func span(before index: inout Int) -> Span<Element> {
    _storage.span(before: &index)
  }
}

@available(SwiftStdlib 5.0, *)
extension UniqueArray where Element: ~Copyable {
  /// Return a mutable span over the maximal storage chunk following the
  /// specified position in the array. The span provides direct mutating access
  /// to all array elements in the range `index ..< count`.
  ///
  /// - Parameter index: A valid index in the array, including the end index.
  ///
  /// - Complexity: O(1)
  @_lifetime(&self)
  public mutating func mutableSpan(
    after index: inout Int
  ) -> MutableSpan<Element> {
    _storage.mutableSpan(after: &index)
  }

  /// Return a mutable span over the maximal storage chunk preceding the specified
  /// position in the array. The span provides direct mutating access to all
  /// array elements in the range `0 ..< index`.
  ///
  /// - Parameter index: A valid index in the array, including the end index.
  ///
  /// - Complexity: O(1)
  @_lifetime(&self)
  public mutating func mutableSpan(
    before index: inout Int
  ) -> MutableSpan<Element> {
    _storage.mutableSpan(before: &index)
  }
}

//MARK: - Resizing

@_alwaysEmitIntoClient
@_transparent
internal func _growUniqueArrayCapacity(_ capacity: Int) -> Int {
  // A growth factor of 1.5 seems like a reasonable compromise between
  // over-allocating memory and wasting cycles on repeatedly resizing storage.
  let c = (3 &* UInt(bitPattern: capacity) &+ 1) / 2
  return Int(bitPattern: c)
}

@available(SwiftStdlib 5.0, *)
extension UniqueArray where Element: ~Copyable {
  /// Grow or shrink the capacity of a unique array instance without discarding
  /// its contents.
  ///
  /// This operation replaces the array's storage buffer with a newly allocated
  /// buffer of the specified capacity, moving all existing elements
  /// to its new storage. The old storage is then deallocated.
  ///
  /// - Parameter newCapacity: The desired new capacity. `newCapacity` must be
  ///    greater than or equal to the current count.
  ///
  /// - Complexity: O(`count`)
  @inlinable
  public mutating func reallocate(capacity: Int) {
    _storage.reallocate(capacity: capacity)
  }

  /// Ensure that the array has capacity to store the specified number of
  /// elements, by growing its storage buffer if necessary.
  ///
  /// If `capacity < n`, then this operation reallocates the unique array's
  /// storage to grow it; on return, the array's capacity becomes `n`.
  /// Otherwise the array is left as is.
  ///
  /// - Complexity: O(`count`)
  @inlinable
  public mutating func reserveCapacity(_ n: Int) {
    _storage.reserveCapacity(n)
  }

  @_alwaysEmitIntoClient
  @_transparent
  internal mutating func _ensureFreeCapacity(_ freeCapacity: Int) {
    guard _storage.freeCapacity < freeCapacity else { return }
    _ensureFreeCapacitySlow(freeCapacity)
  }

  @_alwaysEmitIntoClient
  @_transparent
  internal func _grow(freeCapacity: Int) -> Int {
    Swift.max(
      count + freeCapacity,
      _growUniqueArrayCapacity(capacity))
  }

  @inlinable
  internal mutating func _ensureFreeCapacitySlow(_ freeCapacity: Int) {
    let newCapacity = _grow(freeCapacity: freeCapacity)
    reallocate(capacity: newCapacity)
  }
}

//MARK: - Copying helpers

@available(SwiftStdlib 5.0, *)
extension UniqueArray {
  /// Copy the contents of this array into a newly allocated unique array
  /// instance with just enough capacity to hold all its elements.
  ///
  /// - Complexity: O(`count`)
  @_alwaysEmitIntoClient
  public func clone() -> Self {
    UniqueArray(consuming: _storage.clone())
  }
  
  /// Copy the contents of this array into a newly allocated unique array
  /// instance with the specified capacity.
  ///
  /// - Parameter capacity: The desired capacity of the resulting unique array.
  ///    `capacity` must be greater than or equal to `count`.
  ///
  /// - Complexity: O(`count`)
  @inlinable
  @inline(__always)
  public func clone(capacity: Int) -> Self {
    UniqueArray(consuming: _storage.clone(capacity: capacity))
  }
}
#endif
