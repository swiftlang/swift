/*===------------------ Embedded Swift Platform for Semaphores -----*- C -*-===*
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
 * to provide to make one execution context wait for another in Embedded Swift.
 * The thread-pool executor uses them.
 *
 *===----------------------------------------------------------------------===*/

#ifndef SWIFT_PLATFORM_SEMAPHORE_H
#define SWIFT_PLATFORM_SEMAPHORE_H

#include "Types.h"

#if defined(__cplusplus)
extern "C" {
#endif

/**
 * Initializes a counting semaphore.
 *
 * - Parameters:
 *   - semaphore: Opaque caller-owned semaphore storage initialized by this
 *     function and later passed to the other `_swift_semaphore_*` functions.
 *     The contents are private to the platform implementation. The storage is
 *     at least EMBEDDED_SWIFT_SEMAPHORE_NUM_WORDS pointer-sized words and has
 *     pointer alignment.
 *   - value: The initial count.
 *
 * The `_swift_semaphore_*` functions are required only by code that waits for
 * another thread.
 */
void _swift_semaphore_init(void * EMBEDDED_SWIFT_NONNULL semaphore,
                           __swift_size_t value);

/**
 * Destroys a semaphore initialized by `_swift_semaphore_init`.
 *
 * - Parameters:
 *   - semaphore: The semaphore to destroy. No execution context may be
 *     blocked in `_swift_semaphore_wait` on it. It may be destroyed as soon
 *     as the last `_swift_semaphore_wait` on it returns, even while the
 *     `_swift_semaphore_post` call that ended that wait is still running.
 */
void _swift_semaphore_destroy(void * EMBEDDED_SWIFT_NONNULL semaphore);

/**
 * Blocks until the semaphore's count is nonzero, then decrements it.
 */
void _swift_semaphore_wait(void * EMBEDDED_SWIFT_NONNULL semaphore);

/**
 * Increments the semaphore's count, waking one execution context blocked in
 * `_swift_semaphore_wait` if there is one.
 */
void _swift_semaphore_post(void * EMBEDDED_SWIFT_NONNULL semaphore);

#if defined(__cplusplus)
}
#endif

#endif
