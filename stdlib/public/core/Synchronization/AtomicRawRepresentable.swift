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

@available(SwiftStdlib 5.10, *)
extension RawRepresentable
where
  RawValue: AtomicValue,
  RawValue.AtomicStorage == RawValue
{
  public typealias AtomicStorage = RawValue

  @available(SwiftStdlib 5.10, *)
  @inline(__always)
  @_alwaysEmitIntoClient
  public consuming func _encodeAtomicStorage() -> RawValue {
    rawValue
  }

  /// Prepare this atomic storage value for deinitialization, extracting the
  /// logical value it represents. This invalidates this atomic storage; you
  /// must not perform any operations on it after this call (except for
  /// deinitialization).
  ///
  /// This call prevents resource leaks when destroying the storage
  /// representation of certain `AtomicValue` types. (In particular, ones that
  /// model strong references.)
  ///
  /// Note: This is not an atomic operation. Logically, it implements a custom
  /// destructor for the underlying non-copiable value.
  @available(SwiftStdlib 5.10, *)
  @inline(__always)
  @_alwaysEmitIntoClient
  public static func _decodeAtomicStorage(
    _ storage: consuming RawValue
  ) -> Self {
    Self(rawValue: storage)!
  }

  /// Atomically loads and returns the value referenced by the given pointer,
  /// applying the specified memory ordering.
  ///
  /// - Parameter pointer: A memory location previously initialized with a value
  ///   returned by `prepareAtomicRepresentation(for:)`.
  /// - Parameter ordering: The memory ordering to apply on this operation.
  /// - Returns: The current value referenced by `pointer`.
  @available(SwiftStdlib 5.10, *)
  @_semantics("atomics.requires_constant_orderings")
  @_transparent
  @_alwaysEmitIntoClient
  public static func _atomicLoad(
    at address: UnsafeMutablePointer<RawValue>,
    ordering: AtomicLoadOrdering
  ) -> Self {
    Self(rawValue: RawValue._atomicLoad(at: address, ordering: ordering))!
  }

  /// Atomically sets the value referenced by `pointer` to `desired`,
  /// applying the specified memory ordering.
  ///
  /// - Parameter desired: The desired new value.
  /// - Parameter pointer: A memory location previously initialized with a value
  ///   returned by `prepareAtomicRepresentation(for:)`.
  /// - Parameter ordering: The memory ordering to apply on this operation.
  @available(SwiftStdlib 5.10, *)
  @_semantics("atomics.requires_constant_orderings")
  @_transparent
  @_alwaysEmitIntoClient
  public static func _atomicStore(
    _ desired: Self,
    at address: UnsafeMutablePointer<RawValue>,
    ordering: AtomicStoreOrdering
  ) {
    RawValue._atomicStore(
      desired.rawValue,
      at: address,
      ordering: ordering
    )
  }

  /// Atomically sets the value referenced by `pointer` to `desired` and returns
  /// the original value, applying the specified memory ordering.
  ///
  /// - Parameter desired: The desired new value.
  /// - Parameter pointer: A memory location previously initialized with a value
  ///   returned by `prepareAtomicRepresentation(for:)`.
  /// - Parameter ordering: The memory ordering to apply on this operation.
  /// - Returns: The original value.
  @available(SwiftStdlib 5.10, *)
  @_semantics("atomics.requires_constant_orderings")
  @_transparent
  @_alwaysEmitIntoClient
  public static func _atomicExchange(
    _ desired: Self,
    at address: UnsafeMutablePointer<RawValue>,
    ordering: AtomicUpdateOrdering
  ) -> Self {
    Self(
      rawValue: RawValue._atomicExchange(
        desired.rawValue,
        at: address,
        ordering: ordering
      )
    )!
  }

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
  /// The `successOrdering` argument specifies the memory ordering to use when
  /// the operation manages to update the current value, while `failureOrdering`
  /// will be used when the operation leaves the value intact.
  ///
  /// This method implements a "strong" compare and exchange operation
  /// that does not permit spurious failures.
  ///
  /// - Parameter expected: The expected current value.
  /// - Parameter desired: The desired new value.
  /// - Parameter pointer: A memory location previously initialized with a value
  ///   returned by `prepareSelf(for:)`.
  /// - Parameter successOrdering: The memory ordering to apply if this
  ///    operation performs the exchange.
  /// - Parameter failureOrdering: The memory ordering to apply on this
  ///    operation if it does not perform the exchange.
  /// - Returns: A tuple `(exchanged, original)`, where `exchanged` is true if
  ///   the exchange was successful, and `original` is the original value.
  @available(SwiftStdlib 5.10, *)
  @_semantics("atomics.requires_constant_orderings")
  @_transparent
  @_alwaysEmitIntoClient
  public static func _atomicCompareExchange(
    expected: Self,
    desired: Self,
    at address: UnsafeMutablePointer<RawValue>,
    successOrdering: AtomicUpdateOrdering,
    failureOrdering: AtomicLoadOrdering
  ) -> (exchanged: Bool, original: Self) {
    let raw = RawValue._atomicCompareExchange(
      expected: expected.rawValue,
      desired: desired.rawValue,
      at: address,
      successOrdering: successOrdering,
      failureOrdering: failureOrdering
    )

    return (exchanged: raw.exchanged, original: Self(rawValue: raw.original)!)
  }

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
  /// The `successOrdering` argument specifies the memory ordering to use when the
  /// operation manages to update the current value, while `failureOrdering`
  /// will be used when the operation leaves the value intact.
  ///
  /// - Parameter expected: The expected current value.
  /// - Parameter desired: The desired new value.
  /// - Parameter pointer: A memory location previously initialized with a value
  ///   returned by `prepareAtomicRepresentation(for:)`.
  /// - Parameter successOrdering: The memory ordering to apply if this
  ///    operation performs the exchange.
  /// - Parameter failureOrdering: The memory ordering to apply on this
  ///    operation if it does not perform the exchange.
  /// - Returns: A tuple `(exchanged, original)`, where `exchanged` is true if
  ///   the exchange was successful, and `original` is the original value.
  @available(SwiftStdlib 5.10, *)
  @_semantics("atomics.requires_constant_orderings")
  @_transparent
  @_alwaysEmitIntoClient
  public static func _atomicWeakCompareExchange(
    expected: Self,
    desired: Self,
    at address: UnsafeMutablePointer<RawValue>,
    successOrdering: AtomicUpdateOrdering,
    failureOrdering: AtomicLoadOrdering
  ) -> (exchanged: Bool, original: Self) {
    let raw = RawValue._atomicWeakCompareExchange(
      expected: expected.rawValue,
      desired: desired.rawValue,
      at: address,
      successOrdering: successOrdering,
      failureOrdering: failureOrdering
    )

    return (exchanged: raw.exchanged, original: Self(rawValue: raw.original)!)
  }
}
