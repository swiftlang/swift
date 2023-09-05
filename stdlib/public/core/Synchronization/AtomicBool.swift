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

extension Int8 {
  @inline(__always)
  @_alwaysEmitIntoClient
  var _boolValue: Bool {
    Bool(Builtin.trunc_Int8_Int1(_value))
  }
}

@available(SwiftStdlib 5.10, *)
extension Bool: AtomicValue {
  public typealias AtomicStorage = Int8

  @available(SwiftStdlib 5.10, *)
  @inline(__always)
  @_alwaysEmitIntoClient
  public consuming func _encodeAtomicStorage() -> Int8 {
    self ? 1 : 0
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
    _ storage: consuming Int8
  ) -> Bool {
    storage._boolValue
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
    at address: UnsafeMutablePointer<Int8>,
    ordering: AtomicLoadOrdering
  ) -> Bool {
    address._atomicLoad(ordering: ordering)._boolValue
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
    _ desired: consuming Bool,
    at address: UnsafeMutablePointer<Int8>,
    ordering: AtomicStoreOrdering
  ) {
    address._atomicStore(desired._encodeAtomicStorage(), ordering: ordering)
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
    _ desired: consuming Bool,
    at address: UnsafeMutablePointer<Int8>,
    ordering: AtomicUpdateOrdering
  ) -> Bool {
    address._atomicExchange(
      desired._encodeAtomicStorage(),
      ordering: ordering
    )._boolValue
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
    expected: Bool,
    desired: consuming Bool,
    at address: UnsafeMutablePointer<Int8>,
    successOrdering: AtomicUpdateOrdering,
    failureOrdering: AtomicLoadOrdering
  ) -> (exchanged: Bool, original: Bool) {
    let result = address._atomicCompareExchange(
      expected: expected._encodeAtomicStorage(),
      desired: desired._encodeAtomicStorage(),
      successOrdering: successOrdering,
      failureOrdering: failureOrdering
    )

    return (exchanged: result.exchanged, original: result.original._boolValue)
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
    expected: Bool,
    desired: consuming Bool,
    at address: UnsafeMutablePointer<Int8>,
    successOrdering: AtomicUpdateOrdering,
    failureOrdering: AtomicLoadOrdering
  ) -> (exchanged: Bool, original: Bool) {
    let result = address._atomicWeakCompareExchange(
      expected: expected._encodeAtomicStorage(),
      desired: desired._encodeAtomicStorage(),
      successOrdering: successOrdering,
      failureOrdering: failureOrdering
    )

    return (exchanged: result.exchanged, original: result.original._boolValue)
  }
}

extension Bool {
  /// Perform an atomic logical AND operation on the value referenced by
  /// `pointer` and return the original value, applying the specified memory
  /// ordering.
  ///
  /// - Parameter operand: A boolean value.
  /// - Parameter pointer: A memory location previously initialized with a value
  ///   returned by `prepareAtomicRepresentation(for:)`.
  /// - Parameter ordering: The memory ordering to apply on this operation.
  /// - Returns: The original value before the operation.
  @available(SwiftStdlib 5.10, *)
  @_semantics("atomics.requires_constant_orderings")
  @_transparent
  @_alwaysEmitIntoClient
  public static func _atomicLoadThenLogicalAnd(
    with operand: Bool,
    at address: UnsafeMutablePointer<Int8>,
    ordering: AtomicUpdateOrdering
  ) -> Bool {
    address._atomicLoadThenBitwiseAnd(
      with: operand._encodeAtomicStorage(),
      ordering: ordering
    )._boolValue
  }

  /// Perform an atomic logical OR operation on the value referenced by
  /// `pointer` and return the original value, applying the specified memory
  /// ordering.
  ///
  /// - Parameter operand: A boolean value.
  /// - Parameter pointer: A memory location previously initialized with a value
  ///   returned by `prepareAtomicRepresentation(for:)`.
  /// - Parameter ordering: The memory ordering to apply on this operation.
  /// - Returns: The original value before the operation.
  @available(SwiftStdlib 5.10, *)
  @_semantics("atomics.requires_constant_orderings")
  @_transparent
  @_alwaysEmitIntoClient
  public static func _atomicLoadThenLogicalOr(
    with operand: Bool,
    at address: UnsafeMutablePointer<Int8>,
    ordering: AtomicUpdateOrdering
  ) -> Bool {
    address._atomicLoadThenBitwiseOr(
      with: operand._encodeAtomicStorage(),
      ordering: ordering
    )._boolValue
  }

  /// Perform an atomic logical XOR operation on the value referenced by
  /// `pointer` and return the original value, applying the specified memory
  /// ordering.
  ///
  /// - Parameter operand: A boolean value.
  /// - Parameter pointer: A memory location previously initialized with a value
  ///   returned by `prepareAtomicRepresentation(for:)`.
  /// - Parameter ordering: The memory ordering to apply on this operation.
  /// - Returns: The original value before the operation.
  @available(SwiftStdlib 5.10, *)
  @_semantics("atomics.requires_constant_orderings")
  @_transparent
  @_alwaysEmitIntoClient
  public static func _atomicLoadThenLogicalXor(
    with operand: Bool,
    at address: UnsafeMutablePointer<Int8>,
    ordering: AtomicUpdateOrdering
  ) -> Bool {
    address._atomicLoadThenBitwiseXor(
      with: operand._encodeAtomicStorage(),
      ordering: ordering
    )._boolValue
  }
}

@available(SwiftStdlib 5.10, *)
extension Atomic where Value == Bool {
  /// Perform an atomic logical AND operation and return the original value, applying
  /// the specified memory ordering.
  ///
  /// - Parameter operand: A boolean value.
  /// - Parameter ordering: The memory ordering to apply on this operation.
  /// - Returns: The original value before the operation.
  @available(SwiftStdlib 5.10, *)
  @_semantics("atomics.requires_constant_orderings")
  @_transparent
  @_alwaysEmitIntoClient
  public func loadThenLogicalAnd(
    with operand: Value,
    ordering: AtomicUpdateOrdering
  ) -> Value {
    Bool._atomicLoadThenLogicalAnd(
      with: operand,
      at: value.address,
      ordering: ordering
    )
  }

  /// Perform an atomic logical OR operation and return the original value, applying
  /// the specified memory ordering.
  ///
  /// - Parameter operand: A boolean value.
  /// - Parameter ordering: The memory ordering to apply on this operation.
  /// - Returns: The original value before the operation.
  @available(SwiftStdlib 5.10, *)
  @_semantics("atomics.requires_constant_orderings")
  @_transparent
  @_alwaysEmitIntoClient
  public func loadThenLogicalOr(
    with operand: Value,
    ordering: AtomicUpdateOrdering
  ) -> Value {
    Bool._atomicLoadThenLogicalOr(
      with: operand,
      at: value.address,
      ordering: ordering
    )
  }

  /// Perform an atomic logical XOR operation and return the original value, applying
  /// the specified memory ordering.
  ///
  /// - Parameter operand: A boolean value.
  /// - Parameter ordering: The memory ordering to apply on this operation.
  /// - Returns: The original value before the operation.
  @available(SwiftStdlib 5.10, *)
  @_semantics("atomics.requires_constant_orderings")
  @_transparent
  @_alwaysEmitIntoClient
  public func loadThenLogicalXor(
    with operand: Value,
    ordering: AtomicUpdateOrdering
  ) -> Value {
    Bool._atomicLoadThenLogicalXor(
      with: operand,
      at: value.address,
      ordering: ordering
    )
  }
}

@available(SwiftStdlib 5.10, *)
extension Atomic where Value == Bool {
  /// Perform an atomic logical AND operation and return the original value, applying
  /// the specified memory ordering.
  ///
  /// - Parameter operand: A boolean value.
  /// - Parameter ordering: The memory ordering to apply on this operation.
  /// - Returns: The original value before the operation.
  @available(SwiftStdlib 5.10, *)
  @_semantics("atomics.requires_constant_orderings")
  @_transparent
  @_alwaysEmitIntoClient
  public func logicalAndThenLoad(
    with operand: Value,
    ordering: AtomicUpdateOrdering
  ) -> Value {
    let original = Bool._atomicLoadThenLogicalAnd(
      with: operand,
      at: value.address,
      ordering: ordering
    )

    return original && operand
  }

  /// Perform an atomic logical OR operation and return the original value, applying
  /// the specified memory ordering.
  ///
  /// - Parameter operand: A boolean value.
  /// - Parameter ordering: The memory ordering to apply on this operation.
  /// - Returns: The original value before the operation.
  @available(SwiftStdlib 5.10, *)
  @_semantics("atomics.requires_constant_orderings")
  @_transparent
  @_alwaysEmitIntoClient
  public func logicalOrThenLoad(
    with operand: Value,
    ordering: AtomicUpdateOrdering
  ) -> Value {
    let original = Bool._atomicLoadThenLogicalOr(
      with: operand,
      at: value.address,
      ordering: ordering
    )

    return original || operand
  }

  /// Perform an atomic logical XOR operation and return the original value, applying
  /// the specified memory ordering.
  ///
  /// - Parameter operand: A boolean value.
  /// - Parameter ordering: The memory ordering to apply on this operation.
  /// - Returns: The original value before the operation.
  @available(SwiftStdlib 5.10, *)
  @_semantics("atomics.requires_constant_orderings")
  @_transparent
  @_alwaysEmitIntoClient
  public func logicalXorThenLoad(
    with operand: Value,
    ordering: AtomicUpdateOrdering
  ) -> Value {
    let original = Bool._atomicLoadThenLogicalXor(
      with: operand,
      at: value.address,
      ordering: ordering
    )

    return original != operand
  }
}
