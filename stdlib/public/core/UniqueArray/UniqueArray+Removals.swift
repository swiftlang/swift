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

extension UniqueArray where Element: ~Copyable {
  /// Removes all elements from the array, preserving its allocated capacity.
  ///
  /// - Complexity: O(*n*), where *n* is the original count of the array.
  @export(implementation)
  public mutating func removeAll() {
    _storage.removeAll()
  }

  /// Removes and returns the last element of the array.
  ///
  /// The array must not be empty.
  ///
  /// - Returns: The last element of the original array.
  ///
  /// - Complexity: O(1)
  @discardableResult
  @export(implementation)
  public mutating func removeLast() -> Element {
    _storage.removeLast()
  }

  /// Removes and discards the specified number of elements from the end of the
  /// array.
  ///
  /// Attempting to remove more elements than exist in the array triggers a
  /// runtime error.
  ///
  /// - Parameter k: The number of elements to remove from the array.
  ///   `k` must be greater than or equal to zero and must not exceed
  ///    the count of the array.
  ///
  /// - Complexity: O(`k`)
  @export(implementation)
  public mutating func removeLast(_ k: Int) {
    _storage.removeLast(k)
  }

  /// Removes and returns the element at the specified position.
  ///
  /// All the elements following the specified position are moved to close the
  /// gap.
  ///
  /// - Parameter index: The position of the element to remove. `index` must be
  ///   a valid index of the array that is not equal to the end index.
  /// - Returns: The removed element.
  ///
  /// - Complexity: O(`self.count`)
  @discardableResult
  @export(implementation)
  public mutating func remove(at index: Int) -> Element {
    _storage.remove(at: index)
  }

  /// Removes the specified subrange of elements from the array.
  ///
  /// All the elements following the specified subrange are moved to close the
  /// resulting gap.
  ///
  /// - Parameter bounds: The subrange of the array to remove. The bounds
  ///   of the range must be valid indices of the array.
  ///
  /// - Complexity: O(`self.count`)
  @export(implementation)
  public mutating func removeSubrange(_  bounds: Range<Int>) {
    _storage.removeSubrange(bounds)
  }

  /// Removes the specified subrange of elements from the array.
  ///
  /// - Parameter bounds: The subrange of the array to remove. The bounds
  ///   of the range must be valid indices of the array.
  ///
  /// - Complexity: O(`self.count`)
  @export(implementation)
  public mutating func removeSubrange(_  bounds: some RangeExpression<Int>) {
    // FIXME: Remove this in favor of a standard algorithm.
    removeSubrange(bounds.relative(to: indices))
  }
}

extension UniqueArray where Element: ~Copyable {
  /// Removes and returns the last element of the array, if there is one.
  ///
  /// - Returns: The last element of the array if the array is not empty;
  ///    otherwise, `nil`.
  ///
  /// - Complexity: O(1)
  @export(implementation)
  public mutating func popLast() -> Element? {
    if isEmpty { return nil }
    return removeLast()
  }
}

@available(SwiftStdlib 6.5, *)
extension UniqueArray where Element: ~Copyable {
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
  public mutating func consumeSuffix(
    maxCount n: Int
  ) -> InputSpan<Element> {
    _storage.consumeSuffix(maxCount: n)
  }

  /// Calls a closure with an input span that takes ownership of up to n
  /// elements from the end of the array.
  ///
  /// The closure takes ownership of the elements: it can do anything with them,
  /// including nothing. Any elements still initialized when the input span
  /// goes out of scope are deinitialized, whether the closure returns normally
  /// or by throwing.
  ///
  /// The input span has a capacity equal to the number of elements it holds.
  ///
  /// - Parameters:
  ///   - n: The maximum number of elements to consume.
  ///       `n` must not be negative. If it exceeds `count`,
  ///       all elements are consumed.
  ///   - consumer: A closure that takes ownership of an `InputSpan`
  ///       containing the consumed elements, in their original order.
  /// - Returns: The value returned by `consumer`.
  @available(SwiftStdlib 6.5, *)
  @export(implementation)
  public mutating func consumeSuffix<E: Error, R: ~Copyable>(
    maxCount n: Int,
    consumingWith consumer: (consuming InputSpan<Element>) throws(E) -> R
  ) throws(E) -> R {
    let suffix = consumeSuffix(maxCount: n)
    return try consumer(suffix)
  }

  /// Removes all elements from the array, and returns an input span
  /// that takes ownership of the elements.
  ///
  /// The `InputSpan` returned by this function represents a mutation of the
  /// array, and the array cannot be used until the `InputSpan` is consumed or
  /// destroyed. When it goes out of scope, any elements it still holds are
  /// deinitialized.
  ///
  /// The returned input span has a `capacity` equal to the number of elements
  /// it holds, and has no free capacity for prepending. The array keeps its
  /// allocated capacity.
  ///
  /// - Returns: An `InputSpan` containing the elements that were in the array.
  ///
  /// - Complexity: O(1)
  @available(SwiftStdlib 6.5, *)
  @export(implementation)
  @_lifetime(&self)
  public mutating func consumeAll() -> InputSpan<Element> {
    consumeSuffix(maxCount: count)
  }

  /// Removes all elements from the array by passing them to a closure
  /// through an input span.
  ///
  /// The closure takes ownership of the elements: it can do anything with them,
  /// including nothing. Any elements still initialized when the input span
  /// goes out of scope are deinitialized, whether the closure returns normally
  /// or by throwing.
  ///
  /// The input span has a capacity equal to the array's count.
  ///
  /// - Parameter consumer: A closure that takes ownership of an `InputSpan`
  ///     containing the elements of this array.
  /// - Returns: The value returned by `consumer`.
  @available(SwiftStdlib 6.5, *)
  @export(implementation)
  public mutating func consumeAll<E: Error, R: ~Copyable>(
    consumingWith consumer: (consuming InputSpan<Element>) throws(E) -> R
  ) throws(E) -> R {
    try consumeSuffix(maxCount: count, consumingWith: consumer)
  }
}
