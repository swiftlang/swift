//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import Swift

/// Provides atomic operations on an `Int`-representable value of a trivial type
/// (such as a simple Int-backed enum type) that is stored at a stable memory
/// location.
@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
extension UnsafeAtomic where Value: RawRepresentable, Value.RawValue == Int {
  @_transparent
  @usableFromInline
  internal var _rawValue: UnsafeAtomic<Int> {
    UnsafeAtomic<Int>(_raw: _ptr)
  }

  /// Atomically loads and returns the current value,
  /// with the specified memory ordering.
  @_semantics("atomics.constant_ordering")
  @_transparent @_alwaysEmitIntoClient
  public func load(ordering: AtomicLoadOrdering) -> Value {
    Value(rawValue: _rawValue.load(ordering: ordering))!
  }

  /// Atomically sets the current value to `desired`,
  /// with the specified memory ordering.
  @_semantics("atomics.constant_ordering")
  @_transparent @_alwaysEmitIntoClient
  public func store(
    _ desired: Value,
    ordering: AtomicStoreOrdering
  ) {
    _rawValue.store(desired.rawValue, ordering: ordering)
  }

  /// Atomically sets the current value to `desired` and returns the previous
  /// value, with the specified memory ordering.
  ///
  /// - Returns: The original value.
  @_semantics("atomics.constant_ordering")
  @_transparent @_alwaysEmitIntoClient
  public func exchange(
    _ desired: Value,
    ordering: AtomicUpdateOrdering
  ) -> Value {
    Value(rawValue: _rawValue.exchange(desired.rawValue, ordering: ordering))!
  }

  /// Perform an atomic compare and exchange operation with
  /// the specified memory ordering.
  ///
  /// This operation is equivalent to the following pseudocode:
  ///
  /// ```
  /// atomic(self, ordering: ordering) { value in
  ///   guard value == expected else { return (false, value) }
  ///   value = desired
  ///   return (true, expected)
  /// }
  /// ```
  ///
  /// This method implements a "strong" compare and exchange operation
  /// that does not permit spurious failures.
  @_semantics("atomics.constant_ordering")
  @_transparent @_alwaysEmitIntoClient
  public func compareExchange(
    expected: Value,
    desired: Value,
    ordering: AtomicUpdateOrdering
  ) -> (exchanged: Bool, original: Value) {
    let (success, original) = _rawValue.compareExchange(
      expected: expected.rawValue,
      desired: desired.rawValue,
      ordering: ordering)
    return (success, Value(rawValue: original)!)
  }

  /// Perform an atomic compare and exchange operation with the specified
  /// success/failure memory orderings.
  ///
  /// This operation is equivalent to the following pseudocode:
  ///
  /// ```
  /// atomic(self) { currentValue in
  ///   let original = currentValue
  ///   guard original == expected else { return (false, original) }
  ///   currentValue = desired
  ///   return (true, original)
  /// }
  /// ```
  ///
  /// The `ordering` argument specifies the memory ordering to use when the
  /// operation manages to update the current value, while `failureOrdering`
  /// will be used when the operation leaves the value intact.
  ///
  /// The `failureOrdering` argument currently isn't allowed to introduce a
  /// synchronization constraint that isn't also guaranteed by `ordering`.
  /// (This limitation may be lifted in the future.) For example:
  ///
  /// ```
  /// // OK
  /// ptr.compareExchange(
  ///   expected: 1, desired: 2,
  ///   ordering: .acquiringAndReleasing, failureOrdering: .relaxed)
  ///
  /// // Not supported:
  /// ptr.compareExchange(
  ///   expected: 3, desired: 0,
  ///   ordering: .releasing, failureOrdering: .acquiring)
  /// ```
  @_semantics("atomics.constant_ordering")
  @_transparent @_alwaysEmitIntoClient
  public func compareExchange(
    expected: Value,
    desired: Value,
    ordering: AtomicUpdateOrdering,
    failureOrdering: AtomicLoadOrdering
  ) -> (exchanged: Bool, original: Value) {
    let (success, original) = _rawValue.compareExchange(
      expected: expected.rawValue,
      desired: desired.rawValue,
      ordering: ordering,
      failureOrdering: failureOrdering)
    return (success, Value(rawValue: original)!)
  }
}
