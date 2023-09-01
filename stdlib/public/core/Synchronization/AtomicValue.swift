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

/// The storage representation for an atomic value, providing pointer-based
/// atomic operations. This is a low-level implementation detail of atomic
/// types; instead of directly handling conforming types, it is usually better
/// to use the `UnsafeAtomic` or `ManagedAtomic` generics -- these provide more
/// convenient and safer interfaces while also ensuring that the storage is
/// correctly constructed and destroyed.
///
/// Logically, atomic storage representations are neither value- nor reference
/// types: they should be treated as non-copiable values with a custom
/// destructor. Such constructs cannot currently be modeled directly in Swift,
/// so types conforming to this protocol must be handled carefully to prevent
/// accidental copying. For example, it usually isn't safe to pass around atomic
/// storage representations as function arguments or return values. Instead,
/// they are usually addressed through unsafe pointers.
@available(SwiftStdlib 5.10, *)
public protocol AtomicValue {
  /// The type whose values this storage representation is representing.
  //associatedtype Value

  /// Encode the supplied value into its atomic storage representation.
  ///
  /// Note: This is not an atomic operation. This call may have side effects
  /// (such as unpaired retains of strong references) that will need to be
  /// undone by calling `dispose()` before the storage value is deinitialized.
  //init(_ value: __owned Value)

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
  static func dispose(at pointer: UnsafeMutablePointer<Self>) -> Self

  /// Atomically loads and returns the value referenced by the given pointer,
  /// applying the specified memory ordering.
  ///
  /// - Parameter pointer: A memory location previously initialized with a value
  ///   returned by `prepareAtomicRepresentation(for:)`.
  /// - Parameter ordering: The memory ordering to apply on this operation.
  /// - Returns: The current value referenced by `pointer`.
  @_semantics("atomics.requires_constant_orderings")
  static func atomicLoad(
    at pointer: UnsafeMutablePointer<Self>,
    ordering: AtomicLoadOrdering
  ) -> Self

  /// Atomically sets the value referenced by `pointer` to `desired`,
  /// applying the specified memory ordering.
  ///
  /// - Parameter desired: The desired new value.
  /// - Parameter pointer: A memory location previously initialized with a value
  ///   returned by `prepareAtomicRepresentation(for:)`.
  /// - Parameter ordering: The memory ordering to apply on this operation.
  @_semantics("atomics.requires_constant_orderings")
  static func atomicStore(
    _ desired: consuming Self,
    at pointer: UnsafeMutablePointer<Self>,
    ordering: AtomicStoreOrdering
  )

  /// Atomically sets the value referenced by `pointer` to `desired` and returns
  /// the original value, applying the specified memory ordering.
  ///
  /// - Parameter desired: The desired new value.
  /// - Parameter pointer: A memory location previously initialized with a value
  ///   returned by `prepareAtomicRepresentation(for:)`.
  /// - Parameter ordering: The memory ordering to apply on this operation.
  /// - Returns: The original value.
  @_semantics("atomics.requires_constant_orderings")
  static func atomicExchange(
    _ desired: consuming Self,
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
  ///   returned by `prepareAtomicRepresentation(for:)`.
  /// - Parameter ordering: The memory ordering to apply on this operation.
  /// - Returns: A tuple `(exchanged, original)`, where `exchanged` is true if
  ///   the exchange was successful, and `original` is the original value.
  @_semantics("atomics.requires_constant_orderings")
  static func atomicCompareExchange(
    expected: Self,
    desired: consuming Self,
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
  ///    operation does not perform the exchange.
  /// - Returns: A tuple `(exchanged, original)`, where `exchanged` is true if
  ///   the exchange was successful, and `original` is the original value.
  @_semantics("atomics.requires_constant_orderings")
  static func atomicCompareExchange(
    expected: Self,
    desired: consuming Self,
    at pointer: UnsafeMutablePointer<Self>,
    successOrdering: AtomicUpdateOrdering,
    failureOrdering: AtomicLoadOrdering
  ) -> (exchanged: Bool, original: Self)

  /// Perform an atomic weak compare and exchange operation on the value
  /// referenced by `pointer`, applying the specified memory orderings.
  /// This compare-exchange variant is allowed to spuriously fail; it
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
  /// - Parameter expected: The expected current value.
  /// - Parameter desired: The desired new value.
  /// - Parameter pointer: A memory location previously initialized with a value
  ///   returned by `prepareAtomicRepresentation(for:)`.
  /// - Parameter ordering: The memory ordering to apply on this operation.
  /// - Returns: A tuple `(exchanged, original)`, where `exchanged` is true if
  ///   the exchange was successful, and `original` is the original value.
  @_semantics("atomics.requires_constant_orderings")
  static func atomicWeakCompareExchange(
    expected: Self,
    desired: consuming Self,
    at pointer: UnsafeMutablePointer<Self>,
    ordering: AtomicUpdateOrdering
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
  ///    operation does not perform the exchange.
  /// - Returns: A tuple `(exchanged, original)`, where `exchanged` is true if
  ///   the exchange was successful, and `original` is the original value.
  @_semantics("atomics.requires_constant_orderings")
  static func atomicWeakCompareExchange(
    expected: Self,
    desired: consuming Self,
    at pointer: UnsafeMutablePointer<Self>,
    successOrdering: AtomicUpdateOrdering,
    failureOrdering: AtomicLoadOrdering
  ) -> (exchanged: Bool, original: Self)
}

@available(SwiftStdlib 5.10, *)
extension AtomicValue {
  @available(SwiftStdlib 5.10, *)
  @_semantics("atomics.requires_constant_orderings")
  @_transparent
  @_alwaysEmitIntoClient
  public static func atomicCompareExchange(
    expected: Self,
    desired: consuming Self,
    at pointer: UnsafeMutablePointer<Self>,
    ordering: AtomicUpdateOrdering
  ) -> (exchanged: Bool, original: Self) {
    atomicCompareExchange(
      expected: expected,
      desired: desired,
      at: pointer,
      successOrdering: ordering,
      failureOrdering: ._failureOrdering(for: ordering)
    )
  }

  @available(SwiftStdlib 5.10, *)
  @_semantics("atomics.requires_constant_orderings")
  @_transparent
  @_alwaysEmitIntoClient
  public static func atomicWeakCompareExchange(
    expected: Self,
    desired: consuming Self,
    at pointer: UnsafeMutablePointer<Self>,
    ordering: AtomicUpdateOrdering
  ) -> (exchanged: Bool, original: Self) {
    atomicWeakCompareExchange(
      expected: expected,
      desired: desired,
      at: pointer,
      successOrdering: ordering,
      failureOrdering: ._failureOrdering(for: ordering)
    )
  }
}
