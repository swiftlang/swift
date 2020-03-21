//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import Swift

/// Provides atomic operations on an unsafe mutable pointer value that is stored
/// at a stable memory location.
@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
@frozen
public struct UnsafeAtomicMutablePointer<Pointee> {
  public typealias Value = UnsafeMutablePointer<Pointee>?

  @usableFromInline
  internal let _ptr: UnsafeMutableRawPointer

  @_transparent // Debug performance
  public init(at address: UnsafeMutablePointer<Value>) {
    self._ptr = UnsafeMutableRawPointer(address)
  }
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
extension UnsafeAtomicMutablePointer {
  @inlinable
  public var address: UnsafeMutablePointer<Value> {
    _ptr.assumingMemoryBound(to: Value.self)
  }

  public static func create(
    initialValue: Value
  ) -> UnsafeAtomicMutablePointer {
    let ptr = UnsafeMutablePointer<Value>.allocate(capacity: 1)
    ptr.initialize(to: initialValue)
    return UnsafeAtomicMutablePointer(at: ptr)
  }

  public func destroy() {
    _ptr.deallocate()
  }
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
extension UnsafeAtomicMutablePointer {
  /// Atomically loads and returns the current value,
  /// with the specified memory ordering.
  @_semantics("has_constant_evaluable_arguments")
  @_transparent @_alwaysEmitIntoClient
  public func load(ordering: AtomicLoadOrdering) -> Value {
    let value = _ptr._atomicLoadWord(ordering: ordering)
    return UnsafeMutablePointer(bitPattern: value)
  }
}


@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
extension UnsafeAtomicMutablePointer {
  /// Atomically sets the current value to `desired`,
  /// with the specified memory ordering.
  @_semantics("has_constant_evaluable_arguments")
  @_transparent @_alwaysEmitIntoClient
  public func store(
    _ desired: Value,
    ordering: AtomicStoreOrdering
  ) {
    let desiredWord = UInt(bitPattern: desired)
    _ptr._atomicStoreWord(desiredWord, ordering: ordering)
  }
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
extension UnsafeAtomicMutablePointer {
  /// Atomically sets the current value to `desired` and returns the previous
  /// value, with the specified memory ordering.
  ///
  /// - Returns: The original value.
  @_semantics("has_constant_evaluable_arguments")
  @_transparent @_alwaysEmitIntoClient
  public func exchange(
    _ desired: Value,
    ordering: AtomicUpdateOrdering
  ) -> Value {
    let desiredWord = UInt(bitPattern: desired)
    let resultWord = _ptr._atomicExchangeWord(desiredWord, ordering: ordering)
    return UnsafeMutablePointer(bitPattern: resultWord)
  }
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
extension UnsafeAtomicMutablePointer {
  /// Perform an atomic compare and exchange operation with
  /// the specified ordering constraints.
  ///
  /// This operation is equivalent to the following pseudocode:
  ///
  /// ```
  /// atomic(self, ordering: ordering) { value in
  ///   guard value == expected else { return (false, value) }
  ///   value = desired
  ///   return (true, expected)
  /// }
  /// ```
  ///
  /// This method implements a "strong" compare and exchange operation
  /// that does not permit spurious failures.
  @_semantics("has_constant_evaluable_arguments")
  @_transparent @_alwaysEmitIntoClient
  public func compareExchange(
    expected: Value,
    desired: Value,
    ordering: AtomicUpdateOrdering
  ) -> (exchanged: Bool, original: Value) {
    let desiredWord = UInt(bitPattern: desired)
    let expectedWord = UInt(bitPattern: expected)
    let (success, originalWord) = _ptr._atomicCompareExchangeWord(
      expected: expectedWord,
      desired: desiredWord,
      ordering: ordering)
    return (success, UnsafeMutablePointer(bitPattern: originalWord))
  }
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
extension UnsafeAtomicMutablePointer {
  /// Perform an atomic compare and exchange operation with the specified
  /// success/failure memory orderings.
  ///
  /// This operation is equivalent to the following pseudocode:
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
  /// The `failureOrdering` argument currently isn't allowed to introduce a
  /// synchronization constraint that isn't also guaranteed by `ordering`.
  /// (This limitation may be lifted in the future.) For example:
  ///
  /// ```
  /// // OK
  /// ptr.compareExchange(
  ///   expected: 1, desired: 2,
  ///   ordering: .acquiringAndReleasing, failureOrdering: .relaxed)
  ///
  /// // Not supported:
  /// ptr.compareExchange(
  ///   expected: 3, desired: 0,
  ///   ordering: .releasing, failureOrdering: .acquiring)
  /// ```
  @_semantics("has_constant_evaluable_arguments")
  @_transparent @_alwaysEmitIntoClient
  public func compareExchange(
    expected: Value,
    desired: Value,
    ordering: AtomicUpdateOrdering,
    failureOrdering: AtomicLoadOrdering
  ) -> (exchanged: Bool, original: Value) {
    let (success, originalWord) = _ptr._atomicCompareExchangeWord(
      expected: UInt(bitPattern: expected),
      desired: UInt(bitPattern: desired),
      ordering: ordering,
      failureOrdering: failureOrdering)
    return (success, UnsafeMutablePointer(bitPattern: originalWord))
  }
}
