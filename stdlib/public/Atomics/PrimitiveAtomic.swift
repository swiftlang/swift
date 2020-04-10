//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import Swift

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
public protocol _PrimitiveAtomic {
  /// Atomically loads and returns the value referenced by the given pointer,
  /// applying the specified memory ordering.
  ///
  /// - Parameter pointer: A memory location previously initialized with a value
  ///   returned by `prepareAtomicStorage(for:)`.
  /// - Parameter ordering: The memory ordering to apply on this operation.
  /// - Returns: The current value referenced by `pointer`.
  @_semantics("atomics.requires_constant_orderings")
  static func _atomicLoad(
    at pointer: UnsafeMutablePointer<Self>,
    ordering: AtomicLoadOrdering
  ) -> Self

  /// Atomically sets the value referenced by `pointer` to `desired`,
  /// applying the specified memory ordering.
  ///
  /// - Parameter desired: The desired new value.
  /// - Parameter pointer: A memory location previously initialized with a value
  ///   returned by `prepareAtomicStorage(for:)`.
  /// - Parameter ordering: The memory ordering to apply on this operation.
  @_semantics("atomics.requires_constant_orderings")
  static func _atomicStore(
    _ desired: __owned Self,
    at pointer: UnsafeMutablePointer<Self>,
    ordering: AtomicStoreOrdering
  )

  /// Atomically sets the value referenced by `pointer` to `desired` and returns
  /// the original value, applying the specified memory ordering.
  ///
  /// - Parameter desired: The desired new value.
  /// - Parameter pointer: A memory location previously initialized with a value
  ///   returned by `prepareAtomicStorage(for:)`.
  /// - Parameter ordering: The memory ordering to apply on this operation.
  /// - Returns: The original value.
  @_semantics("atomics.requires_constant_orderings")
  static func _atomicExchange(
    _ desired: __owned Self,
    at pointer: UnsafeMutablePointer<Self>,
    ordering: AtomicUpdateOrdering
  ) -> Self

  /// Perform an atomic compare and exchange operation on the value referenced
  /// by `pointer`, applying the specified memory ordering.
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
  /// - Parameter pointer: A memory location previously initialized with a value
  ///   returned by `prepareAtomicStorage(for:)`.
  /// - Parameter ordering: The memory ordering to apply on this operation.
  /// - Returns: A tuple `(exchanged, original)`, where `exchanged` is true if
  ///   the exchange was successful, and `original` is the original value.
  @_semantics("atomics.requires_constant_orderings")
  static func _atomicCompareExchange(
    expected: Self,
    desired: __owned Self,
    at pointer: UnsafeMutablePointer<Self>,
    ordering: AtomicUpdateOrdering
  ) -> (exchanged: Bool, original: Self)

  /// Perform an atomic compare and exchange operation on the value referenced
  /// by `pointer`, applying the specified success/failure memory orderings.
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
  /// The `ordering` argument specifies the memory ordering to use when the
  /// operation manages to update the current value, while `failureOrdering`
  /// will be used when the operation leaves the value intact.
  ///
  /// This method implements a "strong" compare and exchange operation
  /// that does not permit spurious failures.
  ///
  /// - Parameter expected: The expected current value.
  /// - Parameter desired: The desired new value.
  /// - Parameter pointer: A memory location previously initialized with a value
  ///   returned by `prepareSelf(for:)`.
  /// - Parameter ordering: The memory ordering to apply on this operation.
  /// - Returns: A tuple `(exchanged, original)`, where `exchanged` is true if
  ///   the exchange was successful, and `original` is the original value.
  @_semantics("atomics.requires_constant_orderings")
  static func _atomicCompareExchange(
    expected: Self,
    desired: __owned Self,
    at pointer: UnsafeMutablePointer<Self>,
    ordering: AtomicUpdateOrdering,
    failureOrdering: AtomicLoadOrdering
  ) -> (exchanged: Bool, original: Self)

  /// Perform an atomic weak compare and exchange operation on the value
  /// referenced by `pointer`, applying the specified success/failure memory
  /// orderings. This compare-exchange variant is allowed to spuriously fail; it
  /// is designed to be called in a loop until it indicates a successful
  /// exchange has happened.
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
  /// - Parameter pointer: A memory location previously initialized with a value
  ///   returned by `prepareAtomicStorage(for:)`.
  /// - Parameter ordering: The memory ordering to apply on this operation.
  /// - Returns: A tuple `(exchanged, original)`, where `exchanged` is true if
  ///   the exchange was successful, and `original` is the original value.
  @_semantics("atomics.requires_constant_orderings")
  static func _atomicWeakCompareExchange(
    expected: Self,
    desired: __owned Self,
    at pointer: UnsafeMutablePointer<Self>,
    ordering: AtomicUpdateOrdering,
    failureOrdering: AtomicLoadOrdering
  ) -> (exchanged: Bool, original: Self)
}
