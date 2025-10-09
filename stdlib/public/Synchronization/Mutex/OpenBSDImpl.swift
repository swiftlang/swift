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
typealias _swift_stdlib_pthread_mutex_t = UnsafeRawPointer?

@usableFromInline
@_extern(c, "pthread_mutex_lock")
func _swift_stdlib_pthread_mutex_lock(
  _: UnsafeMutablePointer<_swift_stdlib_pthread_mutex_t>
) -> CInt

@usableFromInline
@_extern(c, "pthread_mutex_trylock")
func _swift_stdlib_pthread_mutex_trylock(
  _: UnsafeMutablePointer<_swift_stdlib_pthread_mutex_t>
) -> CInt

@usableFromInline
@_extern(c, "pthread_mutex_unlock")
func _swift_stdlib_pthread_mutex_unlock(
  _: UnsafeMutablePointer<_swift_stdlib_pthread_mutex_t>
) -> CInt

@usableFromInline
@_extern(c, "pthread_mutex_destroy")
func _swift_stdlib_pthread_mutex_destroy(
  _: UnsafeMutablePointer<_swift_stdlib_pthread_mutex_t>
) -> CInt

@available(SwiftStdlib 6.0, *)
@frozen
@_staticExclusiveOnly
public struct _MutexHandle: ~Copyable {
  // We must allocate an out-of-line value for pthread mutex because they may
  // store interior pointers which would make moving a value of it illegal.
  @usableFromInline
  let value: UnsafeMutablePointer<_swift_stdlib_pthread_mutex_t>

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
    _ = _swift_stdlib_pthread_mutex_lock(value)
  }

  @available(SwiftStdlib 6.0, *)
  @_alwaysEmitIntoClient
  @_transparent
  internal borrowing func _tryLock() -> Bool {
    _swift_stdlib_pthread_mutex_trylock(value) == 0
  }

  @available(SwiftStdlib 6.0, *)
  @_alwaysEmitIntoClient
  @_transparent
  internal borrowing func _unlock() {
    _ = _swift_stdlib_pthread_mutex_unlock(value)
  }

  @available(SwiftStdlib 6.0, *)
  @_alwaysEmitIntoClient
  @_transparent
  deinit {
    _ = _swift_stdlib_pthread_mutex_destroy(value)
    value.deallocate()
  }
}
