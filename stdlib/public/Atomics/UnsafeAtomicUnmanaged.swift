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

/// Provides atomic operations on an unmanaged object reference that is stored
/// at a stable memory location.
@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
@frozen
public struct UnsafeAtomicUnmanaged<Instance: AnyObject> {
  public typealias Value = Unmanaged<Instance>?

  @usableFromInline
  internal let _ptr: UnsafeMutableRawPointer

  @_transparent // Debug performance
  public init(at address: UnsafeMutablePointer<Value>) {
    self._ptr = UnsafeMutableRawPointer(address)
  }
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
extension UnsafeAtomicUnmanaged {
  @inlinable
  public var address: UnsafeMutablePointer<Value> {
    _ptr.assumingMemoryBound(to: Value.self)
  }

  @inlinable
  public static func create(initialValue: Value) -> Self {
    let ptr = UnsafeMutablePointer<Value>.allocate(capacity: 1)
    ptr.initialize(to: initialValue)
    return Self(at: ptr)
  }

  public func destroy() {
    _ptr.deallocate()
  }
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
extension UnsafeAtomicUnmanaged {
  /// Atomically loads and returns the current value,
  /// with the specified memory ordering.
  @_semantics("has_constant_evaluable_arguments")
  @_transparent @_alwaysEmitIntoClient
  public func load(ordering: AtomicLoadOrdering) -> Value {
    let value = _ptr._atomicLoadWord(ordering: ordering)
    guard let p = UnsafeRawPointer(bitPattern: value) else { return nil }
    return Unmanaged.fromOpaque(p)
  }
}


@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
extension UnsafeAtomicUnmanaged {
  /// Atomically sets the current value to `desired`,
  /// with the specified memory ordering.
  @_semantics("has_constant_evaluable_arguments")
  @_transparent @_alwaysEmitIntoClient
  public func store(
    _ desired: Value,
    ordering: AtomicStoreOrdering
  ) {
    let desiredWord = UInt(bitPattern: desired?.toOpaque())
    _ptr._atomicStoreWord(desiredWord, ordering: ordering)
  }
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
extension UnsafeAtomicUnmanaged {
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
    let desiredWord = UInt(bitPattern: desired?.toOpaque())
    let resultWord = _ptr._atomicExchangeWord(desiredWord, ordering: ordering)
    guard let r = UnsafeRawPointer(bitPattern: resultWord) else { return nil }
    return Unmanaged.fromOpaque(r)
  }
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
extension UnsafeAtomicUnmanaged {
  /// Perform an atomic compare and exchange operation with
  /// with the specified memory ordering.
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
    let expectedWord = UInt(bitPattern: expected?.toOpaque())
    let desiredWord = UInt(bitPattern: desired?.toOpaque())
    let (success, originalWord) = _ptr._atomicCompareExchangeWord(
      expected: expectedWord,
      desired: desiredWord,
      ordering: ordering)
    let original: Value
    if let p = UnsafeRawPointer(bitPattern: originalWord) {
      original = Unmanaged.fromOpaque(p)
    } else {
      original = nil
    }
    return (success, original)
  }
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
extension UnsafeAtomicUnmanaged {
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
    let expectedWord = UInt(bitPattern: expected?.toOpaque())
    let desiredWord = UInt(bitPattern: desired?.toOpaque())
    let (success, originalWord) = _ptr._atomicCompareExchangeWord(
      expected: expectedWord,
      desired: desiredWord,
      ordering: ordering,
      failureOrdering: failureOrdering)
    let original: Value
    if let p = UnsafeRawPointer(bitPattern: originalWord) {
      original = Unmanaged.fromOpaque(p)
    } else {
      original = nil
    }
    return (success, original)
  }
}
