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

/// An atomic optional strong reference that can be set (initialized) exactly
/// once, but read many times.
@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
@frozen
public struct UnsafePointerToAtomicLazyReference<Instance: AnyObject> {
  public typealias Value = Instance?
  public typealias AtomicStorage = Instance?

  @usableFromInline
  internal let _ptr: UnsafeMutablePointer<AtomicStorage>

  @_transparent // Debug performance
  public init(@_nonEphemeral at pointer: UnsafeMutablePointer<AtomicStorage>) {
    self._ptr = pointer
  }
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
extension UnsafePointerToAtomicLazyReference {
  @inlinable
  public static func create() -> Self {
    let ptr = UnsafeMutablePointer<AtomicStorage>.allocate(capacity: 1)
    ptr.initialize(to: nil)
    return Self(at: ptr)
  }

  public func destroy() {
    _ptr.deinitialize(count: 1)
    _ptr.deallocate()
  }
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
extension UnsafePointerToAtomicLazyReference {
  /// Atomically initializes this reference if its current value is nil, then
  /// returns the initialized value. If this reference is already initialized,
  /// then `initialize(to:)` discards its supplied argument and returns the
  /// current value without updating it.
  ///
  /// The following example demonstrates how this can be used to implement a
  /// thread-safe lazily initialized reference:
  ///
  /// ```
  /// class Image {
  ///   var _histogram: UnsafePointerToAtomicLazyReference<Histogram> = ...
  ///
  ///   // This is safe to call concurrently from multiple threads.
  ///   var atomicLazyHistogram: Histogram {
  ///     if let histogram = _histogram.load() { return foo }
  ///     // Note that code here may run concurrently on
  ///     // multiple threads, but only one of them will get to
  ///     // succeed setting the reference.
  ///     let histogram = ...
  ///     return _histogram.storeIfNil(foo)
  /// }
  /// ```
  ///
  /// This operation uses acquiring-and-releasing memory ordering.
  @_transparent
  public func storeIfNil(_ desired: __owned Instance) -> Instance {
    let desiredUnmanaged = Unmanaged.passRetained(desired)
    let desiredPtr = desiredUnmanaged.toOpaque()
    let (current, won) = Builtin.cmpxchg_acqrel_acquire_Word(
      _ptr._rawValue,
      // Note: this assumes nil is mapped to 0
      0._builtinWordValue,
      Int(bitPattern: desiredPtr)._builtinWordValue)
    if !Bool(_builtinBooleanLiteral: won) {
      // The reference has already been initialized. Balance the retain that
      // we performed on `desired`.
      desiredUnmanaged.release()
      let raw = UnsafeRawPointer(bitPattern: Int(current)).unsafelyUnwrapped
      let result = Unmanaged<Instance>.fromOpaque(raw)
      return result.takeUnretainedValue()
    }
    return desiredUnmanaged.takeUnretainedValue()
  }
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
extension UnsafePointerToAtomicLazyReference {
  /// Atomically loads and returns the current value of this reference.
  ///
  /// The load operation is performed with the memory ordering
  /// `AtomicLoadOrdering.acquiring`.
  @_transparent
  public func load() -> Instance? {
    let value = Builtin.atomicload_acquire_Word(_ptr._rawValue)
    guard let ptr = UnsafeRawPointer(bitPattern: Int(value)) else { return nil }
    return Unmanaged.fromOpaque(ptr).takeUnretainedValue()
  }
}
