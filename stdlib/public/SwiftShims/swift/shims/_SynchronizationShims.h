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

#ifndef SWIFT_STDLIB_SYNCHRONIZATION_SHIMS_H
#define SWIFT_STDLIB_SYNCHRONIZATION_SHIMS_H

#include "SwiftStdbool.h"
#include "SwiftStdint.h"

#if defined(__wasi__) && defined(__wasm__)
// Note: `__wasilibc_use_busy_futex` is a thread-local Wasm global defined by
// wasi-libc in `__wasilibc_busywait.c`:
// https://github.com/WebAssembly/wasi-libc/blob/wasi-sdk-32/libc-top-half/musl/src/thread/wasi-threads/__wasilibc_busywait.c#L33
//
// It becomes nonzero on the current thread after calling
// `__wasilibc_enable_futex_busywait_on_current_thread()`.
//
// Background:
// https://github.com/WebAssembly/wasi-libc/pull/562
static inline __swift_uint32_t _swift_stdlib_wasilibc_use_busy_futex_get() {
  __swift_uint32_t val;
  __asm__(
      ".globaltype __wasilibc_use_busy_futex, i32\n"
      "global.get __wasilibc_use_busy_futex\n"
      "local.set %0\n"
      : "=r"(val));
  return val;
}
#endif

#if defined(__linux__)
#include <errno.h>
#include <linux/futex.h>
#include <sys/syscall.h>
#include <unistd.h>

static inline __swift_uint32_t _swift_stdlib_gettid() {
  static __thread __swift_uint32_t tid = 0;

  if (tid == 0) {
    tid = syscall(SYS_gettid);
  }

  return tid;
}

// Plain-futex wait: sleeps if *addr == expected. Returns 0 on success
// (woken by FUTEX_WAKE) or the errno value (EAGAIN=11, EINTR=4 are the
// expected retryable cases).
static inline __swift_uint32_t _swift_stdlib_futex_wait(
    __swift_uint32_t *addr, __swift_uint32_t expected) {
  int ret = syscall(SYS_futex, addr, FUTEX_WAIT_PRIVATE, expected,
                    /* timeout */ NULL);

  if (ret == 0) {
    return 0;
  }

  return errno;
}

// Plain-futex wake: wakes up to `count` waiters parked on `addr`.
// Returns the number woken on success, or the errno value on failure.
static inline __swift_uint32_t _swift_stdlib_futex_wake(
    __swift_uint32_t *addr, __swift_uint32_t count) {
  int ret = syscall(SYS_futex, addr, FUTEX_WAKE_PRIVATE, count);

  if (ret >= 0) {
    return (__swift_uint32_t)ret;
  }

  return errno;
}

#endif // defined(__linux__)

#if defined(__FreeBSD__)
#include <stddef.h>
#include <sys/types.h>
#include <sys/thr.h>
#include <sys/umtx.h>

// Current thread's kernel LWP id, cached; used as the umutex owner value.
static inline __swift_uint32_t _swift_stdlib_gettid(void) {
  static __thread __swift_uint32_t tid = 0;
  if (tid == 0) {
    long t = 0;
    thr_self(&t);
    tid = (__swift_uint32_t)t;
  }
  return tid;
}

// Lock a FreeBSD umutex, following libthr's normal-mutex protocol:
//   * uncontended: CAS m_owner UNOWNED -> tid in userspace;
//   * contended: loop — re-acquire when the word becomes unowned (preserving the
//     UMUTEX_CONTESTED bit), otherwise wait in the kernel with UMTX_OP_MUTEX_WAIT
//     until the owner changes.
// (Calling UMTX_OP_MUTEX_LOCK directly, or a bare CAS without this loop, drops
//  wakeups and deadlocks under contention on aarch64.)
static inline void _swift_stdlib_umutex_lock(struct umutex *mutex) {
  volatile __swift_uint32_t *owner = (volatile __swift_uint32_t *)&mutex->m_owner;
  __swift_uint32_t id = _swift_stdlib_gettid();
  __swift_uint32_t expected = UMUTEX_UNOWNED;
  if (__atomic_compare_exchange_n(owner, &expected, id, 0,
                                  __ATOMIC_ACQUIRE, __ATOMIC_RELAXED)) {
    return;
  }
  for (;;) {
    __swift_uint32_t cur = __atomic_load_n(owner, __ATOMIC_RELAXED);
    if ((cur & ~UMUTEX_CONTESTED) == UMUTEX_UNOWNED) {
      __swift_uint32_t e = cur;
      if (__atomic_compare_exchange_n(owner, &e, id | cur, 0,
                                      __ATOMIC_ACQUIRE, __ATOMIC_RELAXED)) {
        return;
      }
    } else {
      _umtx_op(mutex, UMTX_OP_MUTEX_WAIT, 0, NULL, NULL);
    }
  }
}

static inline __swift_bool _swift_stdlib_umutex_trylock(struct umutex *mutex) {
  volatile __swift_uint32_t *owner = (volatile __swift_uint32_t *)&mutex->m_owner;
  __swift_uint32_t expected = UMUTEX_UNOWNED;
  return __atomic_compare_exchange_n(owner, &expected, _swift_stdlib_gettid(), 0,
                                     __ATOMIC_ACQUIRE, __ATOMIC_RELAXED) ? 1 : 0;
}

// Unlock: uncontended fast release (CAS m_owner tid -> UNOWNED); if the mutex is
// contended (CONTESTED bit set), hand off through the kernel so a waiter is woken.
static inline void _swift_stdlib_umutex_unlock(struct umutex *mutex) {
  volatile __swift_uint32_t *owner = (volatile __swift_uint32_t *)&mutex->m_owner;
  __swift_uint32_t expected = _swift_stdlib_gettid();
  if (__atomic_compare_exchange_n(owner, &expected, UMUTEX_UNOWNED, 0,
                                  __ATOMIC_RELEASE, __ATOMIC_RELAXED)) {
    return;
  }
  _umtx_op(mutex, UMTX_OP_MUTEX_UNLOCK, 0, NULL, NULL);
}

#endif // defined(__FreeBSD__)

#endif // SWIFT_STDLIB_SYNCHRONIZATION_SHIMS_H
