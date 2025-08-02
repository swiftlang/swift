//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift Atomics open source project
//
// Copyright (c) 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

@usableFromInline
typealias pthread_mutex_t = UnsafeRawPointer?

@usableFromInline
@_extern(c, "pthread_mutex_lock")
func pthread_mutex_lock(_: UnsafeMutablePointer<pthread_mutex_t>) -> CInt

@usableFromInline
@_extern(c, "pthread_mutex_trylock")
func pthread_mutex_trylock(_: UnsafeMutablePointer<pthread_mutex_t>) -> CInt

@usableFromInline
@_extern(c, "pthread_mutex_unlock")
func pthread_mutex_unlock(_: UnsafeMutablePointer<pthread_mutex_t>) -> CInt

@usableFromInline
@_extern(c, "pthread_mutex_destroy")
func pthread_mutex_destroy(_: UnsafeMutablePointer<pthread_mutex_t>) -> CInt

@available(SwiftStdlib 6.0, *)
@frozen
@_staticExclusiveOnly
public struct _MutexHandle: ~Copyable {
  // We must allocate an out-of-line value for pthread mutex because they may
  // store interior pointers which would make moving a value of it illegal.
  @usableFromInline
  let value: UnsafeMutablePointer<pthread_mutex_t>

  @available(SwiftStdlib 6.0, *)
  @_alwaysEmitIntoClient
  @_transparent
  public init() {
    value = .allocate(capacity: 1)
    value.initialize(to: nil)
  }

  @available(SwiftStdlib 6.0, *)
  @_alwaysEmitIntoClient
  @_transparent
  internal borrowing func _lock() {
    _ = pthread_mutex_lock(value)
  }

  @available(SwiftStdlib 6.0, *)
  @_alwaysEmitIntoClient
  @_transparent
  internal borrowing func _tryLock() -> Bool {
    pthread_mutex_trylock(value) == 0
  }

  @available(SwiftStdlib 6.0, *)
  @_alwaysEmitIntoClient
  @_transparent
  internal borrowing func _unlock() {
    _ = pthread_mutex_unlock(value)
  }

  @available(SwiftStdlib 6.0, *)
  @_alwaysEmitIntoClient
  @_transparent
  deinit {
    _ = pthread_mutex_destroy(value)
    value.deallocate()
  }
}
