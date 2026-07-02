//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Mutex for FreeBSD, backed by the kernel `umutex` facility (the same primitive
// libthr's pthread_mutex uses).
//
// The kernel `UMTX_OP_MUTEX_LOCK`/`UNLOCK` operations are the *contended* path:
// they assume userspace has already attempted the uncontended acquire (a CAS of
// `m_owner` from `UMUTEX_UNOWNED` to the current thread id) and only fall into
// the kernel when that fails. Calling the kernel op directly on an unowned
// mutex parks the caller with no thread to wake it, which deadlocks under
// contention on aarch64. We therefore do the userspace CAS fast-path first and
// only enter the kernel on contention, mirroring libthr; the atomics + syscall
// fallback live in `_SynchronizationShims.h`.
//
//===----------------------------------------------------------------------===//

import _SynchronizationShims
import Glibc

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
    unsafe _swift_stdlib_umutex_lock(value._address)
  }

  @available(SwiftStdlib 6.0, *)
  @_alwaysEmitIntoClient
  @_transparent
  internal borrowing func _tryLock() -> Bool {
    unsafe _swift_stdlib_umutex_trylock(value._address)
  }

  @available(SwiftStdlib 6.0, *)
  @_alwaysEmitIntoClient
  @_transparent
  internal borrowing func _unlock() {
    unsafe _swift_stdlib_umutex_unlock(value._address)
  }
}
