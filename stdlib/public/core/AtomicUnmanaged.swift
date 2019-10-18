//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

/// Provides atomic operations on an unmanaged object reference that is stored
/// at a stable memory location.
@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
@frozen
public struct AtomicUnmanaged<Instance: AnyObject>: Anchored {
  public typealias Value = Unmanaged<Instance>?

  @usableFromInline
  internal let _anchor: AnyObject

  @usableFromInline
  internal let _ptr: UnsafeMutableRawPointer

  @_transparent // Debug performance
  public init(at address: UnsafeMutablePointer<Value>, in anchor: AnyObject) {
    self._anchor = anchor
    self._ptr = UnsafeMutableRawPointer(address)
  }

  public static var defaultInitialValue: Value { nil }
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
extension AtomicUnmanaged {
  /// Atomically loads and returns the current value,
  /// with the specified memory ordering.
  @_transparent @_alwaysEmitIntoClient
  public func load(ordering: AtomicLoadOrdering) -> Value {
    let value = _ptr._atomicLoadWord(ordering: ordering)
    guard let p = UnsafeRawPointer(bitPattern: value) else { return nil }
    return Unmanaged.fromOpaque(p)
  }
}


@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
extension AtomicUnmanaged {
  /// Atomically sets the current value to `desired`,
  /// with the specified memory ordering.
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
extension AtomicUnmanaged {
  /// Atomically sets the current value to `desired` and returns the previous
  /// value, with the specified memory ordering.
  ///
  /// - Returns: The original value.
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
extension AtomicUnmanaged {
  /// Perform an atomic compare and exchange operation with
  /// with the specified memory ordering.
  ///
  /// This operation is equivalent to the following pseudo code:
  ///
  /// ```
  /// atomic(self, ordering: ordering) { value in
  ///   if value == expected {
  ///      value = desired
  ///      return true
  ///   } else {
  ///      expected = value
  ///      return false
  ///   }
  /// }
  /// ```
  @_transparent @_alwaysEmitIntoClient
  public func compareExchange(
    expected: inout Value,
    desired: Value,
    ordering: AtomicUpdateOrdering
  ) -> Bool {
    var expectedWord = UInt(bitPattern: expected?.toOpaque())
    let desiredWord = UInt(bitPattern: desired?.toOpaque())
    let success = _ptr._atomicCompareExchangeWord(
      expected: &expectedWord,
      desired: desiredWord,
      ordering: ordering)
    if let p = UnsafeRawPointer(bitPattern: expectedWord) {
      expected = Unmanaged.fromOpaque(p)
    } else {
      expected = nil
    }
    return success
  }
}
