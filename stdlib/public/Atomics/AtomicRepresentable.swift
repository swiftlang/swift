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
public protocol AtomicRepresentable: AtomicProtocol, RawRepresentable
where RawValue: AtomicProtocol, AtomicStorage == RawValue.AtomicStorage {}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
extension AtomicRepresentable {
  //public typealias AtomicStorage = RawValue.AtomicStorage

  @inlinable
  public static func atomicStorage(for value: Self) -> RawValue.AtomicStorage {
    return RawValue.atomicStorage(for: value.rawValue)
  }

  @inlinable
  public static func deinitializeAtomicStorage(
    at address: UnsafeMutablePointer<AtomicStorage>
  ) {
    address.deinitialize(count: 1)
  }

  @_semantics("has_constant_evaluable_arguments")
  @_transparent @_alwaysEmitIntoClient
  public static func atomicLoad(
    at address: UnsafeMutablePointer<AtomicStorage>,
    ordering: AtomicLoadOrdering
  ) -> Self {
    let raw = RawValue.atomicLoad(at: address, ordering: ordering)
    return Self(rawValue: raw)!
  }

  @_semantics("has_constant_evaluable_arguments")
  @_transparent @_alwaysEmitIntoClient
  public static func atomicStore(
    at address: UnsafeMutablePointer<AtomicStorage>,
    _ desired: Self,
    ordering: AtomicStoreOrdering
  ) {
    RawValue.atomicStore(
      at: address,
      desired.rawValue,
      ordering: ordering)
  }

  @_semantics("has_constant_evaluable_arguments")
  @_transparent @_alwaysEmitIntoClient
  public static func atomicExchange(
    at address: UnsafeMutablePointer<AtomicStorage>,
    _ desired: Self,
    ordering: AtomicUpdateOrdering
  ) -> Self {
    let raw = RawValue.atomicExchange(
      at: address,
      desired.rawValue,
      ordering: ordering)
    return Self(rawValue: raw)!
  }

  @_semantics("has_constant_evaluable_arguments")
  @_transparent @_alwaysEmitIntoClient
  public static func atomicCompareExchange(
    at address: UnsafeMutablePointer<AtomicStorage>,
    expected: Self,
    desired: Self,
    ordering: AtomicUpdateOrdering
  ) -> (exchanged: Bool, original: Self) {
    let (exchanged, raw) = RawValue.atomicCompareExchange(
      at: address,
      expected: expected.rawValue,
      desired: desired.rawValue,
      ordering: ordering)
    return (exchanged, Self(rawValue: raw)!)
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
    let (exchanged, raw) = RawValue.atomicCompareExchange(
      at: address,
      expected: expected.rawValue,
      desired: desired.rawValue,
      ordering: ordering,
      failureOrdering: failureOrdering)
    return (exchanged, Self(rawValue: raw)!)
  }
}
