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
  @_semantics("has_constant_evaluable_arguments")
  static func atomicLoadThenWrappingIncrement(
    by operand: Self,
    at pointer: UnsafeMutablePointer<AtomicStorage>,
    ordering: AtomicUpdateOrdering
  ) -> Self

  @_semantics("has_constant_evaluable_arguments")
  static func atomicLoadThenWrappingDecrement(
    by operand: Self,
    at pointer: UnsafeMutablePointer<AtomicStorage>,
    ordering: AtomicUpdateOrdering
  ) -> Self

  @_semantics("has_constant_evaluable_arguments")
  static func atomicLoadThenBitwiseAnd(
    with operand: Self,
    at pointer: UnsafeMutablePointer<AtomicStorage>,
    ordering: AtomicUpdateOrdering
  ) -> Self

  @_semantics("has_constant_evaluable_arguments")
  static func atomicLoadThenBitwiseOr(
    with operand: Self,
    at pointer: UnsafeMutablePointer<AtomicStorage>,
    ordering: AtomicUpdateOrdering
  ) -> Self

  @_semantics("has_constant_evaluable_arguments")
  static func atomicLoadThenBitwiseXor(
    with operand: Self,
    at pointer: UnsafeMutablePointer<AtomicStorage>,
    ordering: AtomicUpdateOrdering
  ) -> Self
}
