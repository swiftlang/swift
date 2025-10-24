//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift Atomics open source project
//
// Copyright (c) 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

@usableFromInline
typealias _swift_stdlib_lock = UInt32

@usableFromInline
typealias _swift_stdlib_lock_t = UnsafeMutablePointer<_swift_stdlib_lock>

@usableFromInline
@_extern(c, "os_unfair_lock_lock")
func _swift_stdlib_os_unfair_lock_lock(_: _swift_stdlib_lock_t)

@usableFromInline
@_extern(c, "os_unfair_lock_unlock")
func _swift_stdlib_os_unfair_lock_unlock(_: _swift_stdlib_lock_t)

@usableFromInline
@_extern(c, "os_unfair_lock_trylock")
func _swift_stdlib_os_unfair_lock_trylock(_: _swift_stdlib_lock_t) -> Bool

@available(SwiftStdlib 6.0, *)
@frozen
@_staticExclusiveOnly
public struct _MutexHandle: ~Copyable {
  @usableFromInline
  let value: _Cell<_swift_stdlib_lock>

  @available(SwiftStdlib 6.0, *)
  @_alwaysEmitIntoClient
  @_transparent
  public init() {
    value = _Cell(_swift_stdlib_lock())
  }

  @available(SwiftStdlib 6.0, *)
  @_alwaysEmitIntoClient
  @_transparent
  internal borrowing func _lock() {
    unsafe _swift_stdlib_os_unfair_lock_lock(value._address)
  }

  @available(SwiftStdlib 6.0, *)
  @_alwaysEmitIntoClient
  @_transparent
  internal borrowing func _tryLock() -> Bool {
    unsafe _swift_stdlib_os_unfair_lock_trylock(value._address)
  }

  @available(SwiftStdlib 6.0, *)
  @_alwaysEmitIntoClient
  @_transparent
  internal borrowing func _unlock() {
    unsafe _swift_stdlib_os_unfair_lock_unlock(value._address)
  }
}
