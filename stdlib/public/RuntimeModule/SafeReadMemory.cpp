//===--- SafeReadMemory.cpp - Safe memory reading (POSIX) -------*- C++ -*-===//
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

#include "modules/OS/SafeReadMemory.h"

#ifdef __linux__

namespace {
  thread_local struct _swift_read_memory_context ctx;

  void _swift_handle_fault(int sig) {
    (void)sig;
    siglongjmp(ctx.fault_buf, -1);
  }
}

extern "C" swift_read_memory_context_t _swift_begin_reading_memory(void)
{
  if (!ctx.reading) {
    ctx.reading = true;

    struct sigaction sa;
    sigfillset(&sa.sa_mask);
    sa.sa_handler = _swift_handle_fault;
    sa.sa_flags = SA_NODEFER;

    sigaction(SIGSEGV, &sa, &ctx.sa_old_segv);
    sigaction(SIGBUS, &sa, &ctx.sa_old_bus);
  }

  return &ctx;
}

extern "C" void _swift_end_reading_memory(swift_read_memory_context_t pctx)
{
  if (pctx->reading) {
    sigaction(SIGSEGV, &pctx->sa_old_segv, NULL);
    sigaction(SIGBUS, &pctx->sa_old_bus, NULL);

    pctx->reading = false;
  }
}

#endif // __linux__
