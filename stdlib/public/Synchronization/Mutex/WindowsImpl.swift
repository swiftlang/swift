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
typealias SRWLOCK = UInt64
#elseif _pointerBitWidth(_32)
@usableFromInline
typealias SRWLOCK = UInt32
#else
#error("Unsupported platform")
#endif

@usableFromInline
typealias PSRWLOCK = UnsafeMutablePointer<SRWLOCK>

@usableFromInline
@_extern(c, "AcquireSRWLockExclusive")
func AcquireSRWLockExclusive(_: PSRWLOCK)

@usableFromInline
@_extern(c, "ReleaseSRWLockExclusive")
func ReleaseSRWLockExclusive(_: PSRWLOCK)

@usableFromInline
@_extern(c, "TryAcquireSRWLockExclusive")
func TryAcquireSRWLockExclusive(_: PSRWLOCK) -> UInt8

@available(SwiftStdlib 6.0, *)
@frozen
@safe
@_staticExclusiveOnly
public struct _MutexHandle: ~Copyable {
  @usableFromInline
  let value: _Cell<SRWLOCK>

  @available(SwiftStdlib 6.0, *)
  @_alwaysEmitIntoClient
  @_transparent
  public init() {
    unsafe value = _Cell(SRWLOCK())
  }

  @available(SwiftStdlib 6.0, *)
  @_alwaysEmitIntoClient
  @_transparent
  internal borrowing func _lock() {
    unsafe AcquireSRWLockExclusive(value._address)
  }

  @available(SwiftStdlib 6.0, *)
  @_alwaysEmitIntoClient
  @_transparent
  internal borrowing func _tryLock() -> Bool {
    // Windows BOOLEAN gets imported as 'UInt8'...
    unsafe TryAcquireSRWLockExclusive(value._address) != 0
  }

  @available(SwiftStdlib 6.0, *)
  @_alwaysEmitIntoClient
  @_transparent
  internal borrowing func _unlock() {
    unsafe ReleaseSRWLockExclusive(value._address)
  }
}
