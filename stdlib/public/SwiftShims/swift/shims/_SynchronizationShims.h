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
#include <sys/types.h>
#include <sys/umtx.h>
#endif

#endif // SWIFT_STDLIB_SYNCHRONIZATION_SHIMS_H
