/*===------------------ Embedded Swift Platform for Threads --------*- C -*-===*
 *
 * This source file is part of the Swift.org open source project
 *
 * Copyright (c) 2026 Apple Inc. and the Swift project authors
 * Licensed under Apache License v2.0 with Runtime Library Exception
 *
 * See https: *swift.org/LICENSE.txt for license information
 * See https: *swift.org/CONTRIBUTORS.txt for the list of Swift project authors
 *
 *===----------------------------------------------------------------------=== *
 *
 * This file provides a description of the functions that a platform is expected
 * to provide to create threads in Embedded Swift. The thread-pool executor uses
 * them.
 *
 *===----------------------------------------------------------------------===*/

#ifndef SWIFT_PLATFORM_THREADS_H
#define SWIFT_PLATFORM_THREADS_H

#include "Types.h"

#if defined(__cplusplus)
extern "C" {
#endif

/**
 * Creates a thread that runs `start(arg)` and discards its result.
 *
 * The platform releases the thread's resources when `start` returns.
 *
 * - Returns: Nonzero if the thread was created, zero otherwise.
 *
 * This function is required only by code that creates threads.
 *
 * This function can be implemented as `pthread_create(...) == 0` and, on
 * success, `pthread_detach` of the new thread.
 */
__swift_ptrdiff_t _swift_thread_create(
    __swift_thread_start_t EMBEDDED_SWIFT_NONNULL start,
    void * EMBEDDED_SWIFT_NULLABLE arg);

/**
 * Returns the number of threads that can run at the same time, at least 1.
 *
 * This function is required only by code that creates threads, such as the
 * thread-pool executor. It can be implemented as a call to
 * `sysconf(_SC_NPROCESSORS_ONLN)`. A platform that cannot query its processor
 * count, such as WASI, can read the count from the program's environment
 * instead.
 */
__swift_size_t _swift_thread_getHardwareConcurrency(void);

#if defined(__cplusplus)
}
#endif

#endif
