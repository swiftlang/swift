//===--- SafeReadMemory.h - Safe memory reading (POSIX) ---------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  Safely read from the current process's memory.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SAFEREADMEMORY_H_
#define SWIFT_SAFEREADMEMORY_H_

#ifdef __linux__

#include <setjmp.h>
#include <signal.h>
#include <string.h>

struct _swift_read_memory_context {
  bool             reading;
  struct sigaction sa_old_segv;
  struct sigaction sa_old_bus;
  sigjmp_buf       fault_buf;
};

typedef struct _swift_read_memory_context *swift_read_memory_context_t;

/* Retrieve the context for the current thread */
extern "C" swift_read_memory_context_t _swift_begin_reading_memory(void);

static inline bool _swift_read_memory(swift_read_memory_context_t ctx,
                        const void *from,
                        void *to,
                        size_t length) {
  if (!sigsetjmp(ctx->fault_buf, 1)) {
    memcpy(to, from, length);
    return true;
  }
  return false;
}

/* Release the context */
extern "C" void _swift_end_reading_memory(swift_read_memory_context_t ctx);

#endif // __linux__

#endif // SWIFT_SAFEREADMEMORY_H_
