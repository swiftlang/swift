//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift Collections open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
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

#if COLLECTIONS_UNSTABLE_CONTAINERS_PREVIEW
@available(SwiftStdlib 5.0, *)
extension UniqueArray: BorrowingSequence where Element: ~Copyable {
  public typealias BorrowingIterator = RigidArray<Element>.BorrowingIterator
  
  public var estimatedCount: EstimatedCount {
    .exactly(count)
  }

  @_alwaysEmitIntoClient
  @inline(__always)
  public func makeBorrowingIterator() -> BorrowingIterator {
    self._storage.makeBorrowingIterator()
  }
}
#endif

#if COLLECTIONS_UNSTABLE_CONTAINERS_PREVIEW
@available(SwiftStdlib 5.0, *)
extension UniqueArray: Container where Element: ~Copyable {
}
#endif

@available(SwiftStdlib 5.0, *)
extension UniqueArray where Element: ~Copyable {
  /// A Boolean value indicating whether this array contains no elements.
  ///
  /// - Complexity: O(1)
  @inlinable
  @inline(__always)
  public var isEmpty: Bool { _storage.isEmpty }

  /// The number of elements in this array.
  ///
  /// - Complexity: O(1)
  @inlinable
  @inline(__always)
  public var count: Int { _storage.count }
}

@available(SwiftStdlib 5.0, *)
extension UniqueArray where Element: ~Copyable {
  /// A type that represents a position in the array: an integer offset from the
  /// start.
  ///
  /// Valid indices consist of the position of every element and a "past the
  /// end” position that’s not valid for use as a subscript argument.
  public typealias Index = Int

  /// The position of the first element in a nonempty array. This is always zero.
  ///
  /// - Complexity: O(1)
  @inlinable
  @inline(__always)
  public var startIndex: Int { _storage.startIndex }

  /// The array’s "past the end” position—that is, the position one greater than
  /// the last valid subscript argument. This is always equal to array's count.
  ///
  /// - Complexity: O(1)
  @inlinable
  @inline(__always)
  public var endIndex: Int { _storage.count }

  /// The range of indices that are valid for subscripting the array.
  ///
  /// - Complexity: O(1)
  @inlinable
  @inline(__always)
  public var indices: Range<Int> { _storage.indices }

  /// Accesses the element at the specified position.
  ///
  /// - Parameter position: The position of the element to access.
  ///     The position must be a valid index of the array that is not equal
  ///     to the `endIndex` property.
  ///
  /// - Complexity: O(1)
  @inlinable
  public subscript(position: Int) -> Element {
    @inline(__always)
    unsafeAddress {
      _storage._ptr(to: position)
    }
    @inline(__always)
    unsafeMutableAddress {
      _storage._mutablePtr(to: position)
    }
  }
}

@available(SwiftStdlib 5.0, *)
extension UniqueArray where Element: ~Copyable {
  /// Exchanges the values at the specified indices of the array.
  ///
  /// Both parameters must be valid indices of the array and not equal to
  /// endIndex. Passing the same index as both `i` and `j` has no effect.
  ///
  /// - Parameter i: The index of the first value to swap.
  /// - Parameter j: The index of the second valud to swap.
  ///
  /// - Complexity: O(1)
  @inlinable
  public mutating func swapAt(_ i: Int, _ j: Int) {
    _storage.swapAt(i, j)
  }
}


@available(SwiftStdlib 5.0, *)
extension UniqueArray where Element: ~Copyable {
  /// Returns the position immediately after the given index.
  ///
  /// - Note: To improve performance, this method does not validate that the
  ///    index is valid before incrementing it. Index validation is
  ///    deferred until the resulting index is used to access an element.
  ///    This optimization may be removed in future versions; do not rely on it.
  ///
  /// - Parameter index: A valid index of the array. `i` must be less
  ///     than `endIndex`.
  /// - Returns: The index immediately following `i`.
  /// - Complexity: O(1)
  @_alwaysEmitIntoClient
  @inline(__always)
  public func index(after index: Int) -> Int { index + 1 }
  
  /// Returns the position immediately before the given index.
  ///
  /// - Note: To improve performance, this method does not validate that the
  ///    index is valid before decrementing it. Index validation is
  ///    deferred until the resulting index is used to access an element.
  ///    This optimization may be removed in future versions; do not rely on it.
  ///
  /// - Parameter index: A valid index of the array. `i` must be greater
  ///     than `startIndex`.
  /// - Returns: The index immediately preceding `i`.
  /// - Complexity: O(1)
  @_alwaysEmitIntoClient
  @inline(__always)
  public func index(before index: Int) -> Int { index - 1 }

  /// Replaces the given index with its successor.
  ///
  /// - Note: To improve performance, this method does not validate that the
  ///    given index is valid before incrementing it. Index validation is
  ///    deferred until the resulting index is used to access an element.
  ///    This optimization may be removed in future versions; do not rely on it.
  ///
  /// - Parameter index: A valid index of the array. `i` must be less
  ///     than `endIndex`.
  /// - Complexity: O(1)
  @_alwaysEmitIntoClient
  @inline(__always)
  public func formIndex(after index: inout Int) { index += 1 }

  /// Replaces the given index with its predecessor.
  ///
  /// - Note: To improve performance, this method does not validate that the
  ///    given index is valid before decrementing it. Index validation is
  ///    deferred until the resulting index is used to access an element.
  ///    This optimization may be removed in future versions; do not rely on it.
  ///
  /// - Parameter index: A valid index of the array. `i` must be greater than
  ///     `startIndex`.
  /// - Complexity: O(1)
  @_alwaysEmitIntoClient
  @inline(__always)
  public func formIndex(before index: inout Int) { index -= 1 }

  /// Returns an index that is the specified distance from the given index.
  ///
  /// The value passed as `n` must not offset `index` beyond the bounds of the
  /// array.
  ///
  /// - Note: To improve performance, this method does not validate that the
  ///    given index is valid before offseting it. Index validation is
  ///    deferred until the resulting index is used to access an element.
  ///    This optimization may be removed in future versions; do not rely on it.
  ///
  /// - Parameter index: A valid index of the array.
  /// - Parameter n: The distance by which to offset `index`.
  /// - Returns: An index offset by distance from `index`. If `n` is positive,
  ///    this is the same value as the result of `n` calls to `index(after:)`.
  ///    If `n` is negative, this is the same value as the result of `abs(n)`
  ///    calls to `index(before:)`.
  /// - Complexity: O(1)
  @_alwaysEmitIntoClient
  @inline(__always)
  public func index(_ index: Int, offsetBy n: Int) -> Int {
    index + n
  }
  
  /// Returns the distance between two indices.
  ///
  /// - Note: To improve performance, this method does not validate that the
  ///    given index is valid before offseting it. Index validation is
  ///    deferred until the resulting index is used to access an element.
  ///    This optimization may be removed in future versions; do not rely on it.
  ///
  /// - Parameter start: A valid index of the collection.
  /// - Parameter end: Another valid index of the collection. If end is equal
  ///    to start, the result is zero.
  /// - Returns: The distance between `start` and `end`.
  /// - Complexity: O(1)
  @_alwaysEmitIntoClient
  @inline(__always)
  public func distance(from start: Index, to end: Index) -> Int {
    end - start
  }
  
  /// Offsets the given index by the specified distance, but no further than
  /// the given limiting index.
  ///
  /// If the operation was able to offset `index` by exactly the requested
  /// number of steps without hitting `limit`, then on return `n` is set to `0`,
  /// and `index` is set to the adjusted index.
  ///
  /// If the operation hits the limit before it can take the requested number
  /// of steps, then on return `index` is set to `limit`, and `n` is set
  /// to the number of steps that couldn't be taken.
  ///
  /// The value passed as `n` must not offset `index` beyond the bounds of the
  /// container, unless the index passed as `limit` prevents offsetting beyond
  /// those bounds.
  ///
  /// - Note: To improve performance, this method does not validate that the
  ///    given index is valid before offseting it. Index validation is
  ///    deferred until the resulting index is used to access an element.
  ///    This optimization may be removed in future versions; do not rely on it.
  ///
  /// - Parameter index: A valid index of the array. On return, `index` is
  ///    set to `limit` if
  /// - Parameter n: The distance to offset `index`.
  ///    On return, `n` is set to zero if the operation succeeded without
  ///    hitting the limit; otherwise, `n` reflects the number of steps that
  ///    couldn't be taken.
  /// - Parameter limit: A valid index of the array to use as a limit.
  ///    If `n > 0`, a limit that is less than `index` has no effect.
  ///    Likewise, if `n < 0`, a limit that is greater than `index` has no
  ///    effect.
  /// - Complexity: O(1)
  @_alwaysEmitIntoClient
  public func formIndex(
    _ index: inout Index, offsetBy n: inout Int, limitedBy limit: Index
  ) {
    _storage.formIndex(&index, offsetBy: &n, limitedBy: limit)
  }

  /// Return a span over the array's storage that begins with the element at
  /// the given index, and extends to the end of the contiguous storage chunk
  /// that contains it, but no more than `maximumCount` items.
  ///
  /// On return, the index is updated to address the next item following the
  /// end of the returned span.
  ///
  /// This method can be used to efficiently process the items of a container in
  /// bulk, by directly iterating over its piecewise contiguous pieces of
  /// storage:
  ///
  ///     var index = items.startIndex
  ///     while true {
  ///       let span = items.nextSpan(after: &index, maximumCount: 4)
  ///       if span.isEmpty { break }
  ///       // Process items in `span`
  ///     }
  ///
  /// The `maximumCount` argument gives the caller control over the number of
  /// items it receives from the iterator. This lets the caller avoid getting
  /// more elements than it would be able to immediately process, which would
  /// significantly complicate container use.
  ///
  /// If the caller is able to process any number available items, it can signal
  /// that by passing `Int.max` as the `maximumCount`, or simply by calling the
  /// `nexSpan(after:)` method, which does precisely that. This is frequently
  /// the case when the caller simply wants to iterate over the entire
  /// container in a single loop.
  ///
  /// `maximumCount` sets an upper bound. To read a specific number of items,
  /// the caller usually needs to invoke `nextSpan` in a loop:
  ///
  ///     var items: some Container<Int>
  ///     var index = items.startIndex
  ///     var remainder = numberOfItemsToRead
  ///     while remainder > 0 {
  ///       let next = items.nextSpan(after: &index, maximumCount: remainder)
  ///       guard !next.isEmpty else {
  ///         // Container does not have enough items
  ///         break
  ///       }
  ///       remainder -= next.count
  ///       // Process items in `next`
  ///     }
  ///
  /// - Note: The spans returned by this method are not guaranteed to be
  ///    disjunct. Some containers may use the same storage chunk (or parts of a
  ///    storage chunk) multiple times, to repeat their contents.
  ///
  /// - Note: Repeated invocations of `nextSpan` on the same container and index
  ///    are not guaranteed to return identical results. (This is particularly
  ///    the case with containers that can store contents in their "inline"
  ///    representation. Such containers may not always have a unique address
  ///    in memory; the locations of the spans exposed by this method may vary
  ///    between different borrows of the same container.)
  ///
  /// - Parameter index: A valid index in the container, including the end
  ///     index. On return, this index is advanced by the count of the resulting
  ///     span, to simplify iteration.
  /// - Parameter maximumCount: The maximum number of items the caller is able
  ///     to process immediately. `maximumCount` must be greater than zero.
  ///     If you are able to process an arbitrary number of items, set
  ///     `maximumCount` to `Int.max`, or call the `nextSpan(after:)` method.
  /// - Returns: A span over contiguous storage that starts at the given index.
  ///     If the input index is the end index, then this returns an empty span.
  ///     Otherwise the result is non-empty, with its first element matching the
  ///     element at the input index.
  /// - Complexity: O(1)
  @inlinable
  @_lifetime(borrow self)
  public func nextSpan(
    after index: inout Int, maximumCount: Int
  ) -> Span<Element> {
    _storage.nextSpan(after: &index, maximumCount: maximumCount)
  }
}

#endif
