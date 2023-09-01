//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SwiftShims

// FIXME: Better documentation, maybe with examples.

/// A low level non-recursive system lock.
///
/// This unsafe lock type exposes unsafe locking and unlocking operations when
/// used incorrectly could cause deadlocks, attempts to reacquire the same lock
/// that's already acquired, or even forgetting to call 'unlock()'. Invalid
/// usage of these operations may also violate Swift's concurrency model.
@available(SwiftStdlib 5.10, *)
@frozen
public struct UnsafeLock: ~Copyable {
  @usableFromInline
  let value = UnsafeCell<SwiftShims._SwiftLock>()

  /// Initializes a value of this lock.
  @available(SwiftStdlib 5.10, *)
  public init() {
    swift_stdlib_lock_init(value.address)
  }

  deinit {
    swift_stdlib_lock_destroy(value.address)
  }

  /// Acquires the lock and ensures a single thread will execute the critical
  /// section.
  @available(SwiftStdlib 5.10, *)
  public borrowing func lock() {
    swift_stdlib_lock_lock(value.address)
  }

  /// Releases the lock and allows other threads to acquire the lock.
  @available(SwiftStdlib 5.10, *)
  public borrowing func unlock() {
    swift_stdlib_lock_unlock(value.address)
  }
}

/// A general purpose non-recursive system lock.
@available(SwiftStdlib 5.10, *)
@frozen
public struct Lock<T>: ~Copyable {
  @usableFromInline
  let value: UnsafeCell<T>

  @usableFromInline
  let lock = UnsafeLock()

  /// Initializes a value of this lock with the given initial value.
  ///
  /// - Parameter initialValue: The initial value to set this lock.
  @available(SwiftStdlib 5.10, *)
  @inlinable
  public init(_ initialValue: consuming T) {
    self.value = UnsafeCell<T>(initialValue)
  }

  /// Acquires the lock and performs the passed closure as the critical section
  /// and releases after execution.
  @available(SwiftStdlib 5.10, *)
  @inlinable
  public borrowing func whileLocked<U>(
    _ body: (inout T) throws -> U
  ) rethrows -> U {
    lock.lock()

    defer {
      lock.unlock()
    }

    return try body(&value.address.pointee)
  }
}

/// A reference to a value within a lock.
@available(SwiftStdlib 5.10, *)
@frozen
public struct LockedValue<T>: ~Copyable {
  @usableFromInline
  let address: UnsafeMutablePointer<T>

  /// The value within a lock.
  ///
  /// All operations on this value are guaranteed to be done by exactly one
  /// thread at a time.
  @available(SwiftStdlib 5.10, *)
  @_transparent
  public var value: T {
    unsafeAddress {
      UnsafePointer<T>(address)
    }

    nonmutating unsafeMutableAddress {
      address
    }
  }

  @available(SwiftStdlib 5.10, *)
  @inlinable
  init(_ address: UnsafeMutablePointer<T>) {
    self.address = address
  }
}
