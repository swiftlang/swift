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
extension UniqueArray where Element: ~Copyable {
  /// A Boolean value indicating whether this array contains no elements.
  ///
  /// - Complexity: O(1)
  @available(SwiftStdlib 6.4, *)
  @_alwaysEmitIntoClient
  @_transparent
  public var isEmpty: Bool {
    _storage.isEmpty
  }

  /// The number of elements in this array.
  ///
  /// - Complexity: O(1)
  @available(SwiftStdlib 6.4, *)
  @_alwaysEmitIntoClient
  @_transparent
  public var count: Int {
    _storage.count
  }
}

@available(SwiftStdlib 6.4, *)
extension UniqueArray where Element: ~Copyable {
  /// A type that represents a position in the array: an integer offset from the
  /// start.
  ///
  /// Valid indices consist of the position of every element and a "past the
  /// end” position that’s not valid for use as a subscript argument.
  @available(SwiftStdlib 6.4, *)
  public typealias Index = Int

  /// The position of the first element in a nonempty array. This is always zero.
  ///
  /// - Complexity: O(1)
  @available(SwiftStdlib 6.4, *)
  @_alwaysEmitIntoClient
  @_transparent
  public var startIndex: Int {
    _storage.startIndex
  }

  /// The array’s "past the end” position—that is, the position one greater than
  /// the last valid subscript argument. This is always equal to array's count.
  ///
  /// - Complexity: O(1)
  @available(SwiftStdlib 6.4, *)
  @_alwaysEmitIntoClient
  @_transparent
  public var endIndex: Int {
    _storage.count
  }

  /// The range of indices that are valid for subscripting the array.
  ///
  /// - Complexity: O(1)
  @available(SwiftStdlib 6.4, *)
  @_alwaysEmitIntoClient
  @_transparent
  public var indices: Range<Int> {
    _storage.indices
  }

  /// Accesses the element at the specified position.
  ///
  /// - Parameter position: The position of the element to access.
  ///     The position must be a valid index of the array that is not equal
  ///     to the `endIndex` property.
  ///
  /// - Complexity: O(1)
  @available(SwiftStdlib 6.4, *)
  @_alwaysEmitIntoClient
  public subscript(position: Int) -> Element {
    @_transparent
    @_unsafeSelfDependentResult
    borrow {
      unsafe _storage._ptr(to: position).pointee
    }

    @_transparent
    @_unsafeSelfDependentResult
    mutate {
      unsafe &_storage._mutablePtr(to: position).pointee
    }
  }
}

@available(SwiftStdlib 6.4, *)
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
  @available(SwiftStdlib 6.4, *)
  @_alwaysEmitIntoClient
  public mutating func swapAt(_ i: Int, _ j: Int) {
    _storage.swapAt(i, j)
  }
}

@available(SwiftStdlib 6.4, *)
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
  @available(SwiftStdlib 6.4, *)
  @_alwaysEmitIntoClient
  @_transparent
  public func index(after index: Int) -> Int {
    index + 1
  }
  
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
  @available(SwiftStdlib 6.4, *)
  @_alwaysEmitIntoClient
  @_transparent
  public func index(before index: Int) -> Int {
    index - 1
  }

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
  @available(SwiftStdlib 6.4, *)
  @_alwaysEmitIntoClient
  @_transparent
  public func formIndex(after index: inout Int) {
    index += 1
  }

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
  @available(SwiftStdlib 6.4, *)
  @_alwaysEmitIntoClient
  @_transparent
  public func formIndex(before index: inout Int) {
    index -= 1
  }

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
  @available(SwiftStdlib 6.4, *)
  @_alwaysEmitIntoClient
  @_transparent
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
  @available(SwiftStdlib 6.4, *)
  @_alwaysEmitIntoClient
  @_transparent
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
  @available(SwiftStdlib 6.4, *)
  @_alwaysEmitIntoClient
  public func formIndex(
    _ index: inout Index,
    offsetBy n: inout Int,
    limitedBy limit: Index
  ) {
    _storage.formIndex(&index, offsetBy: &n, limitedBy: limit)
  }
}
