//===--- tsan_invisible_barrier.c - A barrier TSan cannot see -------------===//
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
// Orders threads in a test without giving ThreadSanitizer a happens-before edge
// between them, so a test can control interleaving without hiding the
// synchronization it is checking for. It serves the same purpose as the
// test-only barrier in the TSan runtime, but is an independent implementation:
// a plain generation-counting barrier.
//
// It works because this file is compiled WITHOUT -fsanitize=thread: TSan
// neither instruments nor intercepts these atomics, so it never sees them. Do
// not add the sanitizer to the build line for this file.
//
//===----------------------------------------------------------------------===//

#include "tsan_invisible_barrier.h"

#include <sched.h>
#include <stdatomic.h>

static uint32_t threadCount;
static _Atomic uint32_t arrived;
static _Atomic uint32_t generation;

void swift_tsan_invisible_barrier_init(uint32_t numThreads) {
  threadCount = numThreads;
  atomic_store(&arrived, 0);
  atomic_store(&generation, 0);
}

void swift_tsan_invisible_barrier_wait(void) {
  uint32_t gen = atomic_load(&generation);
  if (atomic_fetch_add(&arrived, 1) + 1 == threadCount) {
    // Last one in: reset for reuse, then release the others.
    atomic_store(&arrived, 0);
    atomic_fetch_add(&generation, 1);
    return;
  }
  while (atomic_load(&generation) == gen)
    sched_yield();
}
