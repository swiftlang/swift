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

/// An atomic optional strong reference that can be set (initialized) exactly
/// once, but read many times.
@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
@frozen
public struct AtomicInitializableReference<Instance: AnyObject>: Anchored {
  public typealias Value = Instance?

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
extension AtomicInitializableReference {
  /// Atomically initializes this reference if its current value is nil, then
  /// returns the initialized value. If this reference is already initialized,
  /// then `initialize(to:)` discards its supplied argument and returns the
  /// current value without updating it.
  ///
  /// The following example demonstrates how this can be used to implement a
  /// thread-safe lazily initialized reference:
  ///
  /// ```
  /// @Anchored var _foo: AtomicInitializableReference<Foo>
  ///
  /// // This is safe to call concurrently from multiple threads.
  /// var atomicLazyFoo: Foo {
  ///     if let foo = _foo.load() { return foo }
  ///     let foo = Foo()
  ///     return _foo.initialize(to: foo)
  /// }
  /// ```
  ///
  /// The operation establishes a memory barrier, i.e., it uses the ordering
  /// `AtomicUpdateOrdering.barrier`.
  @_transparent
  public func initialize(
    to desired: __owned Instance
  ) -> Instance {
    let desiredUnmanaged = Unmanaged.passRetained(desired)
    let desiredWord = UInt(bitPattern: desiredUnmanaged.toOpaque())
    var expectedWord: UInt = 0
    let success = _ptr._atomicCompareExchangeWord(
      expected: &expectedWord,
      desired: desiredWord,
      ordering: .barrier)
    if !success {
      // The reference has already been initialized. Balance the retain that
      // we performed on `desired`.
      desiredUnmanaged.release()
    }
    _precondition(expectedWord != 0)
    let result = Unmanaged<Instance>.fromOpaque(
      UnsafeRawPointer(bitPattern: expectedWord)!)
    return result.takeUnretainedValue()
  }
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
extension AtomicInitializableReference {
  /// Atomically loads and returns the current value of this reference.
  ///
  /// The load operation is performed with the memory ordering
  /// `AtomicLoadOrdering.acquiring`.
  @_transparent
  public func load() -> Instance? {
    let value = _ptr._atomicLoadWord(ordering: .acquiring)
    guard let ptr = UnsafeRawPointer(bitPattern: value) else { return nil }
    return Unmanaged.fromOpaque(ptr).takeUnretainedValue()
  }
}
