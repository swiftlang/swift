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
extension _RigidArray where Element: ~Copyable {
  /// Removes and returns the element at the specified position.
  ///
  /// All the elements following the specified position are moved to close the
  /// gap.
  ///
  /// - Parameter i: The position of the element to remove. `index` must be
  ///   a valid index of the array that is not equal to the end index.
  /// - Returns: The removed element.
  ///
  /// - Complexity: O(`count`)
  @available(SwiftStdlib 6.4, *)
  @export(implementation)
  @discardableResult
  internal mutating func remove(at index: Int) -> Element {
    _checkItemIndex(index)
    let old = unsafe _storage.moveElement(from: index)
    _closeGap(at: index, count: 1)
    _count -= 1
    return old
  }

  /// Removes all elements from the array, preserving its allocated capacity.
  ///
  /// - Complexity: O(*n*), where *n* is the original count of the array.
  @available(SwiftStdlib 6.4, *)
  @export(implementation)
  internal mutating func removeAll() {
    unsafe _items.deinitialize()
    _count = 0
  }

  /// Removes and returns the last element of the array.
  ///
  /// The array must not be empty.
  ///
  /// - Returns: The last element of the original array.
  ///
  /// - Complexity: O(1)
  @available(SwiftStdlib 6.4, *)
  @export(implementation)
  @discardableResult
  internal mutating func removeLast() -> Element {
    _precondition(!isEmpty, "Cannot remove last element from an empty array")
    let old = unsafe _storage.moveElement(from: _count - 1)
    _count -= 1
    return old
  }

  /// Removes and discards the specified number of elements from the end of the
  /// array.
  ///
  /// Attempting to remove more elements than exist in the array
  /// triggers a runtime error.
  ///
  /// - Parameter k: The number of elements to remove from the array.
  ///   `k` must be greater than or equal to zero and must not exceed
  ///   the count of the array.
  ///
  /// - Complexity: O(`k`)
  @available(SwiftStdlib 6.4, *)
  @export(implementation)
  internal mutating func removeLast(_ k: Int) {
    if k == 0 { return }
    _precondition(
      k >= 0 && k <= _count,
      "Count of elements to remove is out of bounds")
    unsafe _storage.extracting(
      Range(uncheckedBounds: (_count - k, _count))
    ).deinitialize()
    _count &-= k
  }

  /// Removes the specified subrange of elements from the array.
  ///
  /// All the elements following the specified subrange are moved to close the
  /// resulting gap.
  ///
  /// - Parameter bounds: The subrange of the array to remove. The bounds
  ///   of the range must be valid indices of the array.
  ///
  /// - Complexity: O(`count`)
  @available(SwiftStdlib 6.4, *)
  @export(implementation)
  internal mutating func removeSubrange(_  bounds: Range<Int>) {
    _checkValidBounds(bounds)
    guard !bounds.isEmpty else { return }
    unsafe _storage.extracting(bounds).deinitialize()
    _closeGap(at: bounds.lowerBound, count: bounds.count)
    _count -= bounds.count
  }

  /// Removes the specified subrange of elements from the array.
  ///
  /// - Parameter bounds: The subrange of the array to remove. The bounds of the
  ///   range must be valid indices of the array.
  ///
  /// - Complexity: O(`count`)
  @available(SwiftStdlib 6.4, *)
  @export(implementation)
  internal mutating func removeSubrange(_  bounds: some RangeExpression<Int>) {
    // FIXME: Remove this in favor of a standard algorithm.
    removeSubrange(bounds.relative(to: indices))
  }
}

@available(SwiftStdlib 6.4, *)
extension _RigidArray where Element: ~Copyable {
  /// Removes and returns the last element of the array, if there is one.
  ///
  /// - Returns: The last element of the array if the array is not empty;
  ///     otherwise, `nil`.
  ///
  /// - Complexity: O(1)
  @available(SwiftStdlib 6.4, *)
  @export(implementation)
  internal mutating func popLast() -> Element? {
    // FIXME: Remove this in favor of a standard algorithm.
    if isEmpty { return nil }
    return removeLast()
  }
}

@available(SwiftStdlib 6.5, *)
extension _RigidArray where Element: ~Copyable {
  /// Returns a new input span that takes ownership of up to n elements from the
  /// end of the array.
  ///
  /// The `InputSpan` returned by this function represents a mutation of the
  /// array, and the array cannot be used until the `InputSpan` is consumed or
  /// destroyed. When it goes out of scope, any elements it still holds are
  /// deinitialized.
  ///
  /// The returned input span has a `capacity` equal to the number of elements
  /// it holds, and has no free capacity for prepending.
  ///
  /// - Parameter n: The maximum number of elements to consume.
  ///     `n` must not be negative. If it exceeds `count`,
  ///     all elements are consumed.
  /// - Returns: An `InputSpan` containing the consumed elements, in their
  ///     original order.
  ///
  /// - Complexity: O(1)
  @available(SwiftStdlib 6.5, *)
  @export(implementation)
  @_lifetime(&self)
  internal mutating func consumeSuffix(
    maxCount n: Int
  ) -> InputSpan<Element> {
    _precondition(
      n >= 0, "Can't consume a negative number of elements"
    )
    let c = Swift.min(n, _count)
    let buffer = unsafe _storage.extracting(
      Range(uncheckedBounds: (_count &- c, _count))
    )
    let result = unsafe InputSpan<Element>(
      _uncheckedBuffer: buffer, initializedCount: c
    )
    _count &-= c
    return unsafe _overrideLifetime(result, mutating: &self)
  }
}
