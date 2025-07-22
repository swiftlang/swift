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
typealias os_unfair_lock = UInt32

@usableFromInline
typealias os_unfair_lock_t = UnsafeMutablePointer<os_unfair_lock>

@usableFromInline
@_extern(c, "os_unfair_lock_lock")
func os_unfair_lock_lock(_: os_unfair_lock_t)

@usableFromInline
@_extern(c, "os_unfair_lock_unlock")
func os_unfair_lock_unlock(_: os_unfair_lock_t)

@usableFromInline
@_extern(c, "os_unfair_lock_trylock")
func os_unfair_lock_trylock(_: os_unfair_lock_t) -> Bool

@available(SwiftStdlib 6.0, *)
@frozen
@_staticExclusiveOnly
public struct _MutexHandle: ~Copyable {
  @usableFromInline
  let value: _Cell<os_unfair_lock>

  @available(SwiftStdlib 6.0, *)
  @_alwaysEmitIntoClient
  @_transparent
  public init() {
    value = _Cell(os_unfair_lock())
  }

  @available(SwiftStdlib 6.0, *)
  @_alwaysEmitIntoClient
  @_transparent
  internal borrowing func _lock() {
    unsafe os_unfair_lock_lock(value._address)
  }

  @available(SwiftStdlib 6.0, *)
  @_alwaysEmitIntoClient
  @_transparent
  internal borrowing func _tryLock() -> Bool {
    unsafe os_unfair_lock_trylock(value._address)
  }

  @available(SwiftStdlib 6.0, *)
  @_alwaysEmitIntoClient
  @_transparent
  internal borrowing func _unlock() {
    unsafe os_unfair_lock_unlock(value._address)
  }
}
