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
typealias umutex = (
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
typealias umutex = (
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
var UMTX_OP_MUTEX_TRYLOCK: CInt {
  4
}

@_alwaysEmitIntoClient
@_transparent
var UMTX_OP_MUTEX_LOCK: CInt {
  5
}

@_alwaysEmitIntoClient
@_transparent
var UMTX_OP_MUTEX_UNLOCK: CInt {
  6
}

@usableFromInline
@_extern(c, "_umtx_op")
func _umtx_op(
  _: UnsafeMutablePointer<umutex>,
  _: CInt,
  _: CUnsignedLong,
  _: UnsafeRawPointer?,
  _: UnsafeRawPointer?
)

@available(SwiftStdlib 6.0, *)
@frozen
@_staticExclusiveOnly
public struct _MutexHandle: ~Copyable {
  @usableFromInline
  let value: _Cell<umutex>

  @available(SwiftStdlib 6.0, *)
  @_alwaysEmitIntoClient
  @_transparent
  public init() {
    value = _Cell(umutex())
  }

  @available(SwiftStdlib 6.0, *)
  @_alwaysEmitIntoClient
  @_transparent
  internal borrowing func _lock() {
    _umtx_op(value._address, UMTX_OP_MUTEX_LOCK, 0, nil, nil)
  }

  @available(SwiftStdlib 6.0, *)
  @_alwaysEmitIntoClient
  @_transparent
  internal borrowing func _tryLock() -> Bool {
    _umtx_op(value._address, UMTX_OP_MUTEX_TRYLOCK, 0, nil, nil) != -1
  }

  @available(SwiftStdlib 6.0, *)
  @_alwaysEmitIntoClient
  @_transparent
  internal borrowing func _unlock() {
    _umtx_op(value._address, UMTX_OP_MUTEX_UNLOCK, 0, nil, nil)
  }
}
