//===--- tsan_invisible_barrier.h - A barrier TSan cannot see -------------===//
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

#ifndef SWIFT_TEST_TSAN_INVISIBLE_BARRIER_H
#define SWIFT_TEST_TSAN_INVISIBLE_BARRIER_H

#include <stdint.h>

/// Initialize the barrier for \p numThreads threads.
void swift_tsan_invisible_barrier_init(uint32_t numThreads);

/// Block until all of the threads have called this. Creates no happens-before
/// edge as far as ThreadSanitizer can tell.
void swift_tsan_invisible_barrier_wait(void);

#endif
