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
public struct UnsafeAtomicLazyReference<Instance: AnyObject> {
  public typealias Value = Instance?

  @usableFromInline
  internal let _ptr: UnsafeMutablePointer<Storage>

  @_transparent // Debug performance
  public init(@_nonEphemeral at pointer: UnsafeMutablePointer<Storage>) {
    self._ptr = pointer
  }
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
extension UnsafeAtomicLazyReference {
  @frozen
  public struct Storage {
    @usableFromInline
    internal let _word: Builtin.Word

    @inlinable @inline(__always)
    internal init(_word: Builtin.Word) {
      self._word = _word
    }

    @inlinable @inline(__always)
    public init() {
      // Note: this assumes nil is mapped to 0
      _word = 0._builtinWordValue
    }

    @inlinable @inline(__always)
    @discardableResult
    public mutating func dispose() -> Value {
      guard let ptr = UnsafeRawPointer(bitPattern: Int(_word)) else {
        return nil
      }
      self = Self.init()
      return Unmanaged.fromOpaque(ptr).takeRetainedValue()
    }
  }
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
extension UnsafeAtomicLazyReference {
  @inlinable
  public static func create() -> Self {
    let ptr = UnsafeMutablePointer<Storage>.allocate(capacity: 1)
    ptr.initialize(to: Storage())
    return Self(at: ptr)
  }

  @discardableResult
  @inlinable
  public func destroy() -> Value {
    let result = _ptr.pointee.dispose()
    _ptr.deallocate()
    return result
  }
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
extension UnsafeAtomicLazyReference {
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
  ///   var _histogram: UnsafeAtomicLazyReference<Histogram> = ...
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
  public func storeIfNil(_ desired: __owned Instance) -> Instance {
    let desiredUnmanaged = Unmanaged.passRetained(desired)
    let desiredInt = Int(bitPattern: desiredUnmanaged.toOpaque())
    let (current, won) = Builtin.cmpxchg_acqrel_acquire_Word(
      _ptr._rawValue,
      Storage()._word,
      desiredInt._builtinWordValue)
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
extension UnsafeAtomicLazyReference {
  /// Atomically loads and returns the current value of this reference.
  ///
  /// The load operation is performed with the memory ordering
  /// `AtomicLoadOrdering.acquiring`.
  public func load() -> Instance? {
    let value = Builtin.atomicload_acquire_Word(_ptr._rawValue)
    guard let ptr = UnsafeRawPointer(bitPattern: Int(value)) else { return nil }
    return Unmanaged.fromOpaque(ptr).takeUnretainedValue()
  }
}
