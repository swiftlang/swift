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
public protocol AtomicInteger: AtomicProtocol, FixedWidthInteger {
  /// Perform an atomic wrapping increment operation on the value referenced by
  /// `pointer` and return the original value, applying the specified memory
  /// ordering.
  ///
  /// Note: This operation silently wraps around on overflow, like the
  /// `&+=` operator does on integer values.
  ///
  /// - Parameter operand: The value to add to the current value.
  /// - Parameter pointer: A memory location previously initialized with a value
  ///   returned by `atomicStorage(for:)`.
  /// - Parameter ordering: The memory ordering to apply on this operation.
  /// - Returns: The original value before the operation.
  @_semantics("atomics.requires_constant_orderings")
  static func atomicLoadThenWrappingIncrement(
    by operand: Self,
    at pointer: UnsafeMutablePointer<AtomicStorage>,
    ordering: AtomicUpdateOrdering
  ) -> Self

  /// Perform an atomic wrapping decrement operation on the value referenced by
  /// `pointer` and return the original value, applying the specified memory
  /// ordering.
  ///
  /// Note: This operation silently wraps around on overflow, like the
  /// `&-=` operator does on integer values.
  ///
  /// - Parameter operand: The value to subtract from the current value.
  /// - Parameter pointer: A memory location previously initialized with a value
  ///   returned by `atomicStorage(for:)`.
  /// - Parameter ordering: The memory ordering to apply on this operation.
  /// - Returns: The original value before the operation.
  @_semantics("atomics.requires_constant_orderings")
  static func atomicLoadThenWrappingDecrement(
    by operand: Self,
    at pointer: UnsafeMutablePointer<AtomicStorage>,
    ordering: AtomicUpdateOrdering
  ) -> Self

  /// Perform an atomic bitwise AND operation on the value referenced by
  /// `pointer` and return the original value, applying the specified memory
  /// ordering.
  ///
  /// - Parameter operand: An integer value.
  /// - Parameter pointer: A memory location previously initialized with a value
  ///   returned by `atomicStorage(for:)`.
  /// - Parameter ordering: The memory ordering to apply on this operation.
  /// - Returns: The original value before the operation.
  @_semantics("atomics.requires_constant_orderings")
  static func atomicLoadThenBitwiseAnd(
    with operand: Self,
    at pointer: UnsafeMutablePointer<AtomicStorage>,
    ordering: AtomicUpdateOrdering
  ) -> Self

  /// Perform an atomic bitwise OR operation on the value referenced by
  /// `pointer` and return the original value, applying the specified memory
  /// ordering.
  ///
  /// - Parameter operand: An integer value.
  /// - Parameter pointer: A memory location previously initialized with a value
  ///   returned by `atomicStorage(for:)`.
  /// - Parameter ordering: The memory ordering to apply on this operation.
  /// - Returns: The original value before the operation.
  @_semantics("atomics.requires_constant_orderings")
  static func atomicLoadThenBitwiseOr(
    with operand: Self,
    at pointer: UnsafeMutablePointer<AtomicStorage>,
    ordering: AtomicUpdateOrdering
  ) -> Self

  /// Perform an atomic bitwise XOR operation on the value referenced by
  /// `pointer` and return the original value, applying the specified memory
  /// ordering.
  ///
  /// - Parameter operand: An integer value.
  /// - Parameter pointer: A memory location previously initialized with a value
  ///   returned by `atomicStorage(for:)`.
  /// - Parameter ordering: The memory ordering to apply on this operation.
  /// - Returns: The original value before the operation.
  @_semantics("atomics.requires_constant_orderings")
  static func atomicLoadThenBitwiseXor(
    with operand: Self,
    at pointer: UnsafeMutablePointer<AtomicStorage>,
    ordering: AtomicUpdateOrdering
  ) -> Self
}
