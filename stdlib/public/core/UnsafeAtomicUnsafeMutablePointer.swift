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
public struct UnsafeAtomicUnsafeMutablePointer<Pointee> {
  public typealias Value = UnsafeMutablePointer<Pointee>?

  @usableFromInline
  internal let _ptr: UnsafeMutableRawPointer

  @_transparent // Debug performance
  public init(_ address: UnsafeMutablePointer<Value>) {
    self._ptr = UnsafeMutableRawPointer(address)
  }
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
extension UnsafeAtomicUnsafeMutablePointer {
  /// Atomically loads and returns the current value,
  /// with the specified memory ordering.
  @_transparent @_alwaysEmitIntoClient
  public func load(ordering: AtomicLoadOrdering) -> Value {
    let value = _ptr._atomicLoadWord(ordering: ordering)
    return UnsafeMutablePointer(bitPattern: value)
  }
}


@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
extension UnsafeAtomicUnsafeMutablePointer {
  /// Atomically sets the current value to `desired`,
  /// with the specified memory ordering.
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
extension UnsafeAtomicUnsafeMutablePointer {
  /// Atomically sets the current value to `desired` and returns the previous
  /// value, with the specified memory ordering.
  ///
  /// - Returns: The original value.
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
extension UnsafeAtomicUnsafeMutablePointer {
  /// Perform an atomic compare and exchange operation with
  /// the specified ordering constraints.
  ///
  /// This operation is equivalent to the following pseudocode:
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
  ///
  /// This method implements a "strong" compare and exchange operation
  /// that does not permit spurious failures.
  @_transparent @_alwaysEmitIntoClient
  public func compareExchange(
    expected: inout Value,
    desired: Value,
    ordering: AtomicUpdateOrdering
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
