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
    _ desired: Self,
    at address: UnsafeMutablePointer<AtomicStorage>,
    ordering: AtomicStoreOrdering
  )

  @_semantics("has_constant_evaluable_arguments")
  static func atomicExchange(
    _ desired: Self,
    at address: UnsafeMutablePointer<AtomicStorage>,
    ordering: AtomicUpdateOrdering
  ) -> Self

  @_semantics("has_constant_evaluable_arguments")
  static func atomicCompareExchange(
    expected: Self,
    desired: Self,
    at address: UnsafeMutablePointer<AtomicStorage>,
    ordering: AtomicUpdateOrdering
  ) -> (exchanged: Bool, original: Self)

  @_semantics("has_constant_evaluable_arguments")
  static func atomicCompareExchange(
    expected: Self,
    desired: Self,
    at address: UnsafeMutablePointer<AtomicStorage>,
    ordering: AtomicUpdateOrdering,
    failureOrdering: AtomicLoadOrdering
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
