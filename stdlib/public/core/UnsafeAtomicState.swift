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

/// Provides atomic operations on an `Int`-representable value of a trivial type
/// (such as a simple Int-backed enum type) that is stored at a stable memory
/// location.
@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
@frozen
public struct UnsafeAtomicState<Value: RawRepresentable>
where Value.RawValue == Int {
  @usableFromInline
  internal let _ptr: UnsafeMutableRawPointer

  @_transparent // Debug performance
  public init(at address: UnsafeMutablePointer<Int>) {
    _precondition(_isPOD(Value.self))
    self._ptr = UnsafeMutableRawPointer(address)
  }
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
extension UnsafeAtomicState {
  @inlinable
  public var address: UnsafeMutablePointer<Int> {
    _ptr.assumingMemoryBound(to: Int.self)
  }
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
extension UnsafeAtomicState {
  /// Atomically loads and returns the current value,
  /// with the specified memory ordering.
  @_transparent @_alwaysEmitIntoClient
  public func load(ordering: AtomicLoadOrdering) -> Value {
    let v = Int(bitPattern: _ptr._atomicLoadWord(ordering: ordering))
    return Value(rawValue: v)!
  }

  /// Atomically sets the current value to `desired`,
  /// with the specified memory ordering.
  @_transparent @_alwaysEmitIntoClient
  public func store(
    _ desired: Value,
    ordering: AtomicStoreOrdering
  ) {
    let desiredWord = UInt(bitPattern: desired.rawValue)
    _ptr._atomicStoreWord(desiredWord, ordering: ordering)
  }

  /// Atomically sets the current value to `desired` and returns the previous
  /// value, with the specified memory ordering.
  ///
  /// - Returns: The original value.
  @_transparent @_alwaysEmitIntoClient
  public func exchange(
    _ desired: Value,
    ordering: AtomicUpdateOrdering
  ) -> Value {
    let desiredWord = UInt(bitPattern: desired.rawValue)
    let resultWord = _ptr._atomicExchangeWord(desiredWord, ordering: ordering)
    return Value(rawValue: Int(bitPattern: resultWord))!
  }

  /// Perform an atomic compare and exchange operation with
  /// the specified memory ordering.
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
  @_transparent @_alwaysEmitIntoClient
  public func compareExchange(
    expected: Value,
    desired: Value,
    ordering: AtomicUpdateOrdering
  ) -> (exchanged: Bool, original: Value) {
    let expectedWord = UInt(bitPattern: expected.rawValue)
    let (success, originalWord) = _ptr._atomicCompareThenExchangeWord(
      expected: expectedWord,
      desired: UInt(bitPattern: desired.rawValue),
      ordering: ordering)
    return (success, Value(rawValue: Int(bitPattern: originalWord))!)
  }
}
