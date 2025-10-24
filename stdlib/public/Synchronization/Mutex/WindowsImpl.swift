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

#if _pointerBitWidth(_64)
@usableFromInline
typealias _swift_stdlib_lock = UInt64
#elseif _pointerBitWidth(_32)
@usableFromInline
typealias _swift_stdlib_lock = UInt32
#else
#error("Unsupported platform")
#endif

@usableFromInline
typealias _swift_stdlib_lock_t = UnsafeMutablePointer<_swift_stdlib_lock>

@usableFromInline
@_extern(c, "AcquireSRWLockExclusive")
func _swift_stdlib_AcquireSRWLockExclusive(_: _swift_stdlib_lock_t)

@usableFromInline
@_extern(c, "ReleaseSRWLockExclusive")
func _swift_stdlib_ReleaseSRWLockExclusive(_: _swift_stdlib_lock_t)

@usableFromInline
@_extern(c, "TryAcquireSRWLockExclusive")
func _swift_stdlib_TryAcquireSRWLockExclusive(_: _swift_stdlib_lock_t) -> UInt8

@available(SwiftStdlib 6.0, *)
@frozen
@safe
@_staticExclusiveOnly
public struct _MutexHandle: ~Copyable {
  @usableFromInline
  let value: _Cell<_swift_stdlib_lock>

  @available(SwiftStdlib 6.0, *)
  @_alwaysEmitIntoClient
  @_transparent
  public init() {
    unsafe value = _Cell(_swift_stdlib_lock())
  }

  @available(SwiftStdlib 6.0, *)
  @_alwaysEmitIntoClient
  @_transparent
  internal borrowing func _lock() {
    unsafe _swift_stdlib_AcquireSRWLockExclusive(value._address)
  }

  @available(SwiftStdlib 6.0, *)
  @_alwaysEmitIntoClient
  @_transparent
  internal borrowing func _tryLock() -> Bool {
    // Windows BOOLEAN gets imported as 'UInt8'...
    unsafe _swift_stdlib_TryAcquireSRWLockExclusive(value._address) != 0
  }

  @available(SwiftStdlib 6.0, *)
  @_alwaysEmitIntoClient
  @_transparent
  internal borrowing func _unlock() {
    unsafe _swift_stdlib_ReleaseSRWLockExclusive(value._address)
  }
}
