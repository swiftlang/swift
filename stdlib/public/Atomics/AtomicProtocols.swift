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
public protocol AtomicProtocol {
  associatedtype AtomicStorage

  static func atomicStorage(for value: Self) -> AtomicStorage

  static func deinitializeAtomicStorage(
    at address: UnsafeMutablePointer<AtomicStorage>
  )

  @_semantics("has_constant_evaluable_arguments")
  static func atomicLoad(
    at address: UnsafeMutablePointer<AtomicStorage>,
    ordering: AtomicLoadOrdering
  ) -> Self

  @_semantics("has_constant_evaluable_arguments")
  static func atomicStore(
    at address: UnsafeMutablePointer<AtomicStorage>,
    _ desired: Self,
    ordering: AtomicStoreOrdering
  )

  @_semantics("has_constant_evaluable_arguments")
  static func atomicExchange(
    at address: UnsafeMutablePointer<AtomicStorage>,
    _ desired: Self,
    ordering: AtomicUpdateOrdering
  ) -> Self

  @_semantics("has_constant_evaluable_arguments")
  static func atomicCompareExchange(
    at address: UnsafeMutablePointer<AtomicStorage>,
    expected: Self,
    desired: Self,
    ordering: AtomicUpdateOrdering
  ) -> (exchanged: Bool, original: Self)

  @_semantics("has_constant_evaluable_arguments")
  static func atomicCompareExchange(
    at address: UnsafeMutablePointer<AtomicStorage>,
    expected: Self,
    desired: Self,
    ordering: AtomicUpdateOrdering, // Note: no default
    failureOrdering: AtomicLoadOrdering // Note: no default
  ) -> (exchanged: Bool, original: Self)
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
extension AtomicProtocol where AtomicStorage == Self {
  @inlinable
  public static func atomicStorage(for value: Self) -> AtomicStorage {
    return value
  }

  @inlinable
  public static func deinitializeAtomicStorage(
    at address: UnsafeMutablePointer<AtomicStorage>
  ) {
    address.deinitialize(count: 1)
  }
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
public protocol AtomicInteger: AtomicProtocol, FixedWidthInteger {
  static func atomicLoadThenWrappingIncrement(
    at address: UnsafeMutablePointer<AtomicStorage>,
    by operand: Self,
    ordering: AtomicUpdateOrdering
  ) -> Self

  static func atomicLoadThenWrappingDecrement(
    at address: UnsafeMutablePointer<AtomicStorage>,
    by operand: Self,
    ordering: AtomicUpdateOrdering
  ) -> Self

  static func atomicLoadThenBitwiseAnd(
    at address: UnsafeMutablePointer<AtomicStorage>,
    with operand: Self,
    ordering: AtomicUpdateOrdering
  ) -> Self

  static func atomicLoadThenBitwiseOr(
    at address: UnsafeMutablePointer<AtomicStorage>,
    with operand: Self,
    ordering: AtomicUpdateOrdering
  ) -> Self

  static func atomicLoadThenBitwiseXor(
    at address: UnsafeMutablePointer<AtomicStorage>,
    with operand: Self,
    ordering: AtomicUpdateOrdering
  ) -> Self
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
public protocol NullableAtomic: AtomicProtocol {
  associatedtype NullableAtomicStorage

  static func nullableAtomicStorage(for value: Self?) -> NullableAtomicStorage

  static func deinitializeNullableAtomicStorage(
    at address: UnsafeMutablePointer<NullableAtomicStorage>
  )

  static func atomicOptionalLoad(
    at address: UnsafeMutablePointer<NullableAtomicStorage>,
    ordering: AtomicLoadOrdering
  ) -> Self?

  static func atomicOptionalStore(
    at address: UnsafeMutablePointer<NullableAtomicStorage>,
    _ desired: Self?,
    ordering: AtomicStoreOrdering
  )

  static func atomicOptionalExchange(
    at address: UnsafeMutablePointer<NullableAtomicStorage>,
    _ desired: Self?,
    ordering: AtomicUpdateOrdering
  ) -> Self?

  static func atomicOptionalCompareExchange(
    at address: UnsafeMutablePointer<NullableAtomicStorage>,
    expected: Self?,
    desired: Self?,
    ordering: AtomicUpdateOrdering
  ) -> (exchanged: Bool, original: Self?)

  static func atomicOptionalCompareExchange(
    at address: UnsafeMutablePointer<NullableAtomicStorage>,
    expected: Self?,
    desired: Self?,
    ordering: AtomicUpdateOrdering, // Note: no default
    failureOrdering: AtomicLoadOrdering // Note: no default
  ) -> (exchanged: Bool, original: Self?)
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
extension NullableAtomic where NullableAtomicStorage == Self? {
  @_transparent @_alwaysEmitIntoClient
  public static func nullableAtomicStorage(
    for value: Self?
  ) -> NullableAtomicStorage {
    return value
  }

  @inlinable
  public static func deinitializeNullableAtomicStorage(
    at address: UnsafeMutablePointer<Self?>
  ) {
    address.deinitialize(count: 1)
  }
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
extension Optional: AtomicProtocol where Wrapped: NullableAtomic {
  public typealias AtomicStorage = Wrapped.NullableAtomicStorage

  @_transparent @_alwaysEmitIntoClient
  public static func atomicStorage(for value: Self) -> AtomicStorage {
    return Wrapped.nullableAtomicStorage(for: value)
  }

  @_transparent @_alwaysEmitIntoClient
  public static func deinitializeAtomicStorage(
    at address: UnsafeMutablePointer<AtomicStorage>
  ) {
    Wrapped.deinitializeNullableAtomicStorage(at: address)
  }

  @_semantics("has_constant_evaluable_arguments")
  @_transparent @_alwaysEmitIntoClient
  public static func atomicLoad(
    at address: UnsafeMutablePointer<AtomicStorage>,
    ordering: AtomicLoadOrdering
  ) -> Self {
    Wrapped.atomicOptionalLoad(at: address, ordering: ordering)
  }

  @_semantics("has_constant_evaluable_arguments")
  @_transparent @_alwaysEmitIntoClient
  public static func atomicStore(
    at address: UnsafeMutablePointer<AtomicStorage>,
    _ desired: Self,
    ordering: AtomicStoreOrdering
  ) {
    Wrapped.atomicOptionalStore(at: address, desired, ordering: ordering)
  }

  @_semantics("has_constant_evaluable_arguments")
  @_transparent @_alwaysEmitIntoClient
  public static func atomicExchange(
    at address: UnsafeMutablePointer<AtomicStorage>,
    _ desired: Self,
    ordering: AtomicUpdateOrdering
  ) -> Self {
    Wrapped.atomicOptionalExchange(at: address, desired, ordering: ordering)
  }

  @_semantics("has_constant_evaluable_arguments")
  @_transparent @_alwaysEmitIntoClient
  public static func atomicCompareExchange(
    at address: UnsafeMutablePointer<AtomicStorage>,
    expected: Self,
    desired: Self,
    ordering: AtomicUpdateOrdering
  ) -> (exchanged: Bool, original: Self) {
    Wrapped.atomicOptionalCompareExchange(
      at: address,
      expected: expected,
      desired: desired,
      ordering: ordering)
  }

  @_semantics("has_constant_evaluable_arguments")
  @_transparent @_alwaysEmitIntoClient
  public static func atomicCompareExchange(
    at address: UnsafeMutablePointer<AtomicStorage>,
    expected: Self,
    desired: Self,
    ordering: AtomicUpdateOrdering, // Note: no default
    failureOrdering: AtomicLoadOrdering // Note: no default
  ) -> (exchanged: Bool, original: Self) {
    Wrapped.atomicOptionalCompareExchange(
      at: address,
      expected: expected,
      desired: desired,
      ordering: ordering,
      failureOrdering: failureOrdering)
  }
}
