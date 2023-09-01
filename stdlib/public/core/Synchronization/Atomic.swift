//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift Atomics open source project
//
// Copyright (c) 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

/// An atomic value.
@available(SwiftStdlib 5.10, *)
@frozen
public struct Atomic<Value: AtomicValue>: ~Copyable {
  @usableFromInline
  let value: UnsafeCell<Value>

  /// Initializes a value of this atomic with the given initial value.
  ///
  /// - Parameter initialValue: The initial value to set this atomic.
  @available(SwiftStdlib 5.10, *)
  @inlinable
  public init(_ initialValue: consuming Value) {
    value = UnsafeCell<Value>(initialValue)
  }

  @inlinable
  deinit {
    _ = Value.dispose(at: value.address)
    value.address.deinitialize(count: 1)
  }
}

@available(SwiftStdlib 5.10, *)
extension Atomic {
  /// Atomically loads and returns the current value, applying the specified
  /// memory ordering.
  ///
  /// - Parameter ordering: The memory ordering to apply on this operation.
  /// - Returns: The current value.
  @available(SwiftStdlib 5.10, *)
  @_semantics("atomics.requires_constant_orderings")
  @_transparent
  @_alwaysEmitIntoClient
  public func load(ordering: AtomicLoadOrdering) -> Value {
    Value.atomicLoad(at: value.address, ordering: ordering)
  }

  /// Atomically sets the current value to `desired`, applying the specified
  /// memory ordering.
  ///
  /// - Parameter desired: The desired new value.
  /// - Parameter ordering: The memory ordering to apply on this operation.
  @available(SwiftStdlib 5.10, *)
  @_semantics("atomics.requires_constant_orderings")
  @_transparent
  @_alwaysEmitIntoClient
  public func store(
    _ desired: consuming Value,
    ordering: AtomicStoreOrdering
  ) {
    Value.atomicStore(desired, at: value.address, ordering: ordering)
  }

  /// Atomically sets the current value to `desired` and returns the original
  /// value, applying the specified memory ordering.
  ///
  /// - Parameter desired: The desired new value.
  /// - Parameter ordering: The memory ordering to apply on this operation.
  /// - Returns: The original value.
  @available(SwiftStdlib 5.10, *)
  @_semantics("atomics.requires_constant_orderings")
  @_transparent
  @_alwaysEmitIntoClient
  public func exchange(
    _ desired: consuming Value,
    ordering: AtomicUpdateOrdering
  ) -> Value {
    Value.atomicExchange(desired, at: value.address, ordering: ordering)
  }

  /// Perform an atomic compare and exchange operation on the current value,
  /// applying the specified memory ordering.
  ///
  /// This operation performs the following algorithm as a single atomic
  /// transaction:
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
  /// This method implements a "strong" compare and exchange operation
  /// that does not permit spurious failures.
  ///
  /// - Parameter expected: The expected current value.
  /// - Parameter desired: The desired new value.
  /// - Parameter ordering: The memory ordering to apply on this operation.
  /// - Returns: A tuple `(exchanged, original)`, where `exchanged` is true if
  ///   the exchange was successful, and `original` is the original value.
  @available(SwiftStdlib 5.10, *)
  @_semantics("atomics.requires_constant_orderings")
  @_transparent
  @_alwaysEmitIntoClient
  public func compareExchange(
    expected: Value,
    desired: consuming Value,
    ordering: AtomicUpdateOrdering
  ) -> (exchanged: Bool, original: Value) {
    Value.atomicCompareExchange(
      expected: expected,
      desired: desired,
      at: value.address,
      ordering: ordering
    )
  }

  /// Perform an atomic compare and exchange operation on the current value,
  /// applying the specified success/failure memory orderings.
  ///
  /// This operation performs the following algorithm as a single atomic
  /// transaction:
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
  /// The `successOrdering` argument specifies the memory ordering to use when
  /// the operation manages to update the current value, while `failureOrdering`
  /// will be used when the operation leaves the value intact.
  ///
  /// This method implements a "strong" compare and exchange operation
  /// that does not permit spurious failures.
  ///
  /// - Parameter expected: The expected current value.
  /// - Parameter desired: The desired new value.
  /// - Parameter successOrdering: The memory ordering to apply if this
  ///    operation performs the exchange.
  /// - Parameter failureOrdering: The memory ordering to apply on this
  ///    operation does not perform the exchange.
  /// - Returns: A tuple `(exchanged, original)`, where `exchanged` is true if
  ///   the exchange was successful, and `original` is the original value.
  @available(SwiftStdlib 5.10, *)
  @_semantics("atomics.requires_constant_orderings")
  @_transparent
  @_alwaysEmitIntoClient
  public func compareExchange(
    expected: Value,
    desired: consuming Value,
    successOrdering: AtomicUpdateOrdering,
    failureOrdering: AtomicLoadOrdering
  ) -> (exchanged: Bool, original: Value) {
    Value.atomicCompareExchange(
      expected: expected,
      desired: desired,
      at: value.address,
      successOrdering: successOrdering,
      failureOrdering: failureOrdering
    )
  }

  /// Perform an atomic weak compare and exchange operation on the current
  /// value, applying the memory ordering. This compare-exchange variant is
  /// allowed to spuriously fail; it is designed to be called in a loop until
  /// it indicates a successful exchange has happened.
  ///
  /// This operation performs the following algorithm as a single atomic
  /// transaction:
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
  /// (In this weak form, transient conditions may cause the `original ==
  /// expected` check to sometimes return false when the two values are in fact
  /// the same.)
  ///
  /// - Parameter expected: The expected current value.
  /// - Parameter desired: The desired new value.
  /// - Parameter ordering: The memory ordering to apply on this operation.
  /// - Returns: A tuple `(exchanged, original)`, where `exchanged` is true if
  ///   the exchange was successful, and `original` is the original value.
  @available(SwiftStdlib 5.10, *)
  @_semantics("atomics.requires_constant_orderings")
  @_transparent
  @_alwaysEmitIntoClient
  public func weakCompareExchange(
    expected: Value,
    desired: consuming Value,
    ordering: AtomicUpdateOrdering
  ) -> (exchanged: Bool, original: Value) {
    Value.atomicWeakCompareExchange(
      expected: expected,
      desired: desired,
      at: value.address,
      ordering: ordering
    )
  }

  /// Perform an atomic weak compare and exchange operation on the current
  /// value, applying the specified success/failure memory orderings. This
  /// compare-exchange variant is allowed to spuriously fail; it is designed to
  /// be called in a loop until it indicates a successful exchange has happened.
  ///
  /// This operation performs the following algorithm as a single atomic
  /// transaction:
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
  /// (In this weak form, transient conditions may cause the `original ==
  /// expected` check to sometimes return false when the two values are in fact
  /// the same.)
  ///
  /// The `ordering` argument specifies the memory ordering to use when the
  /// operation manages to update the current value, while `failureOrdering`
  /// will be used when the operation leaves the value intact.
  ///
  /// - Parameter expected: The expected current value.
  /// - Parameter desired: The desired new value.
  /// - Parameter successOrdering: The memory ordering to apply if this
  ///    operation performs the exchange.
  /// - Parameter failureOrdering: The memory ordering to apply on this
  ///    operation does not perform the exchange.
  /// - Returns: A tuple `(exchanged, original)`, where `exchanged` is true if
  ///   the exchange was successful, and `original` is the original value.
  @available(SwiftStdlib 5.10, *)
  @_semantics("atomics.requires_constant_orderings")
  @_transparent
  @_alwaysEmitIntoClient
  public func weakCompareExchange(
    expected: Value,
    desired: consuming Value,
    successOrdering: AtomicUpdateOrdering,
    failureOrdering: AtomicLoadOrdering
  ) -> (exchanged: Bool, original: Value) {
    Value.atomicWeakCompareExchange(
      expected: expected,
      desired: desired,
      at: value.address,
      successOrdering: successOrdering,
      failureOrdering: failureOrdering
    )
  }
}
