//==--- ulock.h - 32-bit futex-based lock for Linux ------------ -*-C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Implements a 32-bit futex-based locking primitive with priority inversion
// support.
//
// This has comparable performance to pthread_mutex_t (on x86-64, it's slower
// under contention, but not much different otherwise; other architectures may
// vary), but it only takes up 32 bits instead of 40 *bytes*.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_THREADING_IMPL_LINUX_ULOCK_H
#define SWIFT_THREADING_IMPL_LINUX_ULOCK_H

// This file is specific to Linux; we're just going to assume we can use
// various GCC/Clang extensions here.

#include <linux/futex.h>
#include <sys/syscall.h>
#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>

#include <atomic>
#include <cstdint>
#include <cerrno>

#include "swift/Threading/Errors.h"
#include "swift/Threading/ThreadSanitizer.h"

namespace swift {
namespace threading_impl {
namespace linux {

typedef std::int32_t ulock_t;

#define ULOCK_INITIALIZER 0
#define ulock_fastpath(x) __builtin_expect((x), true)

inline int ulock_get_tid(void) {
  static __thread int tid;
  if (tid == 0)
    tid = syscall(SYS_gettid);
  return tid;
}

inline int ulock_futex(ulock_t *lock, int op) {
  if (syscall(SYS_futex, lock, op | FUTEX_PRIVATE_FLAG, 0, nullptr, nullptr,
              0) == 0)
    return 0;
  return errno;
}

inline void ulock_lock(ulock_t *lock) {
  const ulock_t tid = ulock_get_tid();
  for (;;) {
    ulock_t zero = 0;
    if (ulock_fastpath(__atomic_compare_exchange_n(lock, &zero, tid,
                                                   true, __ATOMIC_ACQUIRE,
                                                   __ATOMIC_RELAXED)))
      break;

    int err = ulock_futex(lock, FUTEX_LOCK_PI);
    if (err == 0)
      break;
    if (err != EAGAIN && err != EINTR) {
      swift::threading::fatal("ulock_lock: FUTEX_LOCK_PI failed on %p "
                              "(lock word 0x%x, tid %d): errno %d",
                              lock, __atomic_load_n(lock, __ATOMIC_RELAXED),
                              tid, err);
    }
  }

  tsan::acquire(lock);
}

inline bool ulock_trylock(ulock_t *lock) {
  ulock_t zero = 0;
  if (ulock_fastpath(__atomic_compare_exchange_n(lock, &zero, ulock_get_tid(),
                                                 true, __ATOMIC_ACQUIRE,
                                                 __ATOMIC_RELAXED))
      || ulock_futex(lock, FUTEX_TRYLOCK_PI) == 0) {
    tsan::acquire(lock);
    return true;
  }

  return false;
}

inline void ulock_unlock(ulock_t *lock) {
  tsan::release(lock);

  const ulock_t tid = ulock_get_tid();
  for (;;) {
    ulock_t expected = tid;
    if (ulock_fastpath(__atomic_compare_exchange_n(lock, &expected, 0,
                                                   true, __ATOMIC_RELEASE,
                                                   __ATOMIC_RELAXED)))
      break;

    int err = ulock_futex(lock, FUTEX_UNLOCK_PI);
    if (err == 0)
      break;
    if (err != EAGAIN && err != EINTR) {
      swift::threading::fatal("ulock_unlock: FUTEX_UNLOCK_PI failed on %p "
                              "(lock word 0x%x, tid %d): errno %d",
                              lock, __atomic_load_n(lock, __ATOMIC_RELAXED),
                              tid, err);
    }
  }
}

} // namespace linux
} // namespace threading_impl
} // namespace swift

#endif // SWIFT_THREADING_IMPL_LINUX_ULOCK_H
