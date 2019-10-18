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

/// Provides atomic operations on an unsafe mutable pointer value that is stored
/// at a stable memory location.
@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
@frozen
public struct AtomicUnsafeMutablePointer<Pointee>: Anchored {
  public typealias Value = UnsafeMutablePointer<Pointee>?

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
extension AtomicUnsafeMutablePointer {
  /// Atomically loads and returns the current value,
  /// with the specified memory ordering.
  @_transparent @_alwaysEmitIntoClient
  public func load(ordering: AtomicLoadOrdering = .acquiring) -> Value {
    let value = _ptr._atomicLoadWord(ordering: ordering)
    return UnsafeMutablePointer(bitPattern: value)
  }
}


@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
extension AtomicUnsafeMutablePointer {
  /// Atomically sets the current value to `desired`,
  /// with the specified memory ordering.
  @_transparent @_alwaysEmitIntoClient
  public func store(
    _ desired: Value,
    ordering: AtomicStoreOrdering = .releasing
  ) {
    let desiredWord = UInt(bitPattern: desired)
    _ptr._atomicStoreWord(desiredWord, ordering: ordering)
  }
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
extension AtomicUnsafeMutablePointer {
  /// Atomically sets the current value to `desired` and returns the previous
  /// value, with the specified memory ordering.
  ///
  /// - Returns: The original value.
  @_transparent @_alwaysEmitIntoClient
  public func exchange(
    _ desired: Value,
    ordering: AtomicUpdateOrdering = .acquiringAndReleasing
  ) -> Value {
    let desiredWord = UInt(bitPattern: desired)
    let resultWord = _ptr._atomicExchangeWord(desiredWord, ordering: ordering)
    return UnsafeMutablePointer(bitPattern: resultWord)
  }
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
extension AtomicUnsafeMutablePointer {
  /// Perform an atomic compare and exchange operation with
  /// the specified ordering constraints.
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
    ordering: AtomicUpdateOrdering = .acquiringAndReleasing
  ) -> Bool {
    var expectedWord = UInt(bitPattern: expected)
    let desiredWord = UInt(bitPattern: desired)
    let success = _ptr._atomicCompareExchangeWord(
      expected: &expectedWord,
      desired: desiredWord,
      ordering: ordering)
    expected = UnsafeMutablePointer(bitPattern: expectedWord)
    return success
  }
}
