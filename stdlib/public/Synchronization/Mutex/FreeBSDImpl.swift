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

#if _pointerBitWidth(_64)
// struct umutex
@usableFromInline
typealias _swift_stdlib_lock = (
  // lwpid_t owner;
  Int32,

  // uint32_t flags;
  UInt32,

  // uint32_t ceilings[2];
  (UInt32, UInt32),

  // uintptr_t rb_link;
  UInt,

  // uint32_t spare[2];
  (UInt32, UInt32)
)
#elseif _pointerBitWidth(_32)
// struct umutex
@usableFromInline
typealias _swift_stdlib_lock = (
  // lwpid_t owner;
  Int32,

  // uint32_t flags;
  UInt32,

  // uint32_t ceilings[2];
  (UInt32, UInt32),

  // uintptr_t rb_link
  UInt,

  // uint32_t pad;
  UInt32,

  // uint32_t spare[2];
  (UInt32, UInt32)
)
#else
#error("Unsupported platform")
#endif

@_alwaysEmitIntoClient
@_transparent
var _swift_stdlib_UMTX_OP_MUTEX_TRYLOCK: CInt {
  4
}

@_alwaysEmitIntoClient
@_transparent
var _swift_stdlib_UMTX_OP_MUTEX_LOCK: CInt {
  5
}

@_alwaysEmitIntoClient
@_transparent
var _swift_stdlib_UMTX_OP_MUTEX_UNLOCK: CInt {
  6
}

@usableFromInline
@_extern(c, "_umtx_op")
func _swift_stdlib_umtx_op(
  obj: UnsafeMutablePointer<_swift_stdlib_lock>,
  op: CInt,
  val: CUnsignedLong,
  uaddr: UnsafeRawPointer?,
  uaddr: UnsafeRawPointer?
)

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
#if _pointerBitWidth(_64)
    value = _Cell((0, 0, (0, 0), 0, (0, 0)))
#elseif _pointerBitWidth(_32)
    value = _Cell((0, 0, (0, 0), 0, 0, (0, 0)))
#else
#error("Unsupported platform")
#endif
  }

  @available(SwiftStdlib 6.0, *)
  @_alwaysEmitIntoClient
  @_transparent
  internal borrowing func _lock() {
    _swift_stdlib_umtx_op(
      obj: value._address,
      op: _swift_stdlib_UMTX_OP_MUTEX_LOCK,
      val: 0,
      uaddr: nil,
      uaddr2: nil
    )
  }

  @available(SwiftStdlib 6.0, *)
  @_alwaysEmitIntoClient
  @_transparent
  internal borrowing func _tryLock() -> Bool {
    _swift_stdlib_umtx_op(
      obj: value._address,
      op: _swift_stdlib_UMTX_OP_MUTEX_TRYLOCK,
      val: 0,
      uaddr: nil,
      uaddr2: nil
    ) != -1
  }

  @available(SwiftStdlib 6.0, *)
  @_alwaysEmitIntoClient
  @_transparent
  internal borrowing func _unlock() {
    _swift_stdlib_umtx_op(
      obj: value._address,
      op: _swift_stdlib_UMTX_OP_MUTEX_UNLOCK,
      val: 0,
      uaddr: nil,
      uaddr2: nil
    )
  }
}
