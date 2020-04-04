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
public protocol NullableAtomic: AtomicProtocol {
  associatedtype NullableAtomicStorage

  static func nullableAtomicStorage(
    for value: __owned Self?
  ) -> NullableAtomicStorage

  static func deinitializeNullableAtomicStorage(
    at pointer: UnsafeMutablePointer<NullableAtomicStorage>
  )

  @_semantics("has_constant_evaluable_arguments")
  static func atomicOptionalLoad(
    at pointer: UnsafeMutablePointer<NullableAtomicStorage>,
    ordering: AtomicLoadOrdering
  ) -> Self?

  @_semantics("has_constant_evaluable_arguments")
  static func atomicOptionalStore(
    _ desired: __owned Self?,
    at pointer: UnsafeMutablePointer<NullableAtomicStorage>,
    ordering: AtomicStoreOrdering
  )

  @_semantics("has_constant_evaluable_arguments")
  static func atomicOptionalExchange(
    _ desired: __owned Self?,
    at pointer: UnsafeMutablePointer<NullableAtomicStorage>,
    ordering: AtomicUpdateOrdering
  ) -> Self?

  @_semantics("has_constant_evaluable_arguments")
  static func atomicOptionalCompareExchange(
    expected: Self?,
    desired: __owned Self?,
    at pointer: UnsafeMutablePointer<NullableAtomicStorage>,
    ordering: AtomicUpdateOrdering
  ) -> (exchanged: Bool, original: Self?)

  @_semantics("has_constant_evaluable_arguments")
  static func atomicOptionalCompareExchange(
    expected: Self?,
    desired: __owned Self?,
    at pointer: UnsafeMutablePointer<NullableAtomicStorage>,
    ordering: AtomicUpdateOrdering,
    failureOrdering: AtomicLoadOrdering
  ) -> (exchanged: Bool, original: Self?)

  @_semantics("has_constant_evaluable_arguments")
  static func atomicOptionalWeakCompareExchange(
    expected: Self?,
    desired: __owned Self?,
    at pointer: UnsafeMutablePointer<NullableAtomicStorage>,
    ordering: AtomicUpdateOrdering,
    failureOrdering: AtomicLoadOrdering
  ) -> (exchanged: Bool, original: Self?)
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
extension NullableAtomic where NullableAtomicStorage == Self? {
  @_transparent @_alwaysEmitIntoClient
  public static func nullableAtomicStorage(
    for value: __owned Self?
  ) -> NullableAtomicStorage {
    return value
  }

  @inlinable
  public static func deinitializeNullableAtomicStorage(
    at pointer: UnsafeMutablePointer<Self?>
  ) {
    pointer.deinitialize(count: 1)
  }
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
extension Optional: AtomicProtocol where Wrapped: NullableAtomic {
  public typealias AtomicStorage = Wrapped.NullableAtomicStorage

  @_transparent @_alwaysEmitIntoClient
  public static func atomicStorage(for value: __owned Self) -> AtomicStorage {
    return Wrapped.nullableAtomicStorage(for: value)
  }

  @_transparent @_alwaysEmitIntoClient
  public static func deinitializeAtomicStorage(
    at pointer: UnsafeMutablePointer<AtomicStorage>
  ) {
    Wrapped.deinitializeNullableAtomicStorage(at: pointer)
  }

  @_semantics("has_constant_evaluable_arguments")
  @_transparent @_alwaysEmitIntoClient
  public static func atomicLoad(
    at pointer: UnsafeMutablePointer<AtomicStorage>,
    ordering: AtomicLoadOrdering
  ) -> Self {
    Wrapped.atomicOptionalLoad(at: pointer, ordering: ordering)
  }

  @_semantics("has_constant_evaluable_arguments")
  @_transparent @_alwaysEmitIntoClient
  public static func atomicStore(
    _ desired: __owned Self,
    at pointer: UnsafeMutablePointer<AtomicStorage>,
    ordering: AtomicStoreOrdering
  ) {
    Wrapped.atomicOptionalStore(desired, at: pointer, ordering: ordering)
  }

  @_semantics("has_constant_evaluable_arguments")
  @_transparent @_alwaysEmitIntoClient
  public static func atomicExchange(
    _ desired: __owned Self,
    at pointer: UnsafeMutablePointer<AtomicStorage>,
    ordering: AtomicUpdateOrdering
  ) -> Self {
    Wrapped.atomicOptionalExchange(desired, at: pointer, ordering: ordering)
  }

  @_semantics("has_constant_evaluable_arguments")
  @_transparent @_alwaysEmitIntoClient
  public static func atomicCompareExchange(
    expected: Self,
    desired: __owned Self,
    at pointer: UnsafeMutablePointer<AtomicStorage>,
    ordering: AtomicUpdateOrdering
  ) -> (exchanged: Bool, original: Self) {
    Wrapped.atomicOptionalCompareExchange(
      expected: expected,
      desired: desired,
      at: pointer,
      ordering: ordering)
  }

  @_semantics("has_constant_evaluable_arguments")
  @_transparent @_alwaysEmitIntoClient
  public static func atomicCompareExchange(
    expected: Self,
    desired: __owned Self,
    at pointer: UnsafeMutablePointer<AtomicStorage>,
    ordering: AtomicUpdateOrdering,
    failureOrdering: AtomicLoadOrdering
  ) -> (exchanged: Bool, original: Self) {
    Wrapped.atomicOptionalCompareExchange(
      expected: expected,
      desired: desired,
      at: pointer,
      ordering: ordering,
      failureOrdering: failureOrdering)
  }

  @_semantics("has_constant_evaluable_arguments")
  @_transparent @_alwaysEmitIntoClient
  public static func atomicWeakCompareExchange(
    expected: Self,
    desired: __owned Self,
    at pointer: UnsafeMutablePointer<AtomicStorage>,
    ordering: AtomicUpdateOrdering,
    failureOrdering: AtomicLoadOrdering
  ) -> (exchanged: Bool, original: Self) {
    Wrapped.atomicOptionalWeakCompareExchange(
      expected: expected,
      desired: desired,
      at: pointer,
      ordering: ordering,
      failureOrdering: failureOrdering)
  }
}
