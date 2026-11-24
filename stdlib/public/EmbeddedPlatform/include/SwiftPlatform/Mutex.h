/*===------------------ Embedded Swift Platform for Mutexes --------*- C -*-===*
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
 * to provide to work with mutexes in Embedded Swift. It provides the basis
 * for the Mutex type in the Synchronization library, and is also used by the
 * concurrency runtime.
 *
 *===----------------------------------------------------------------------===*/

#ifndef SWIFT_PLATFORM_MUTEX_H
#define SWIFT_PLATFORM_MUTEX_H

#include "Types.h"

#if defined(__cplusplus)
extern "C" {
#endif

/**
 * Initializes a mutex.
 *
 * - Parameters:
 *   - mutex: Opaque caller-owned mutex storage initialized by this function
 *     and later passed to the other `_swift_mutex_*` functions. The contents
 *     are private to the platform implementation. The storage is at least
 *     EMBEDDED_SWIFT_MUTEX_NUM_WORDS pointer-sized words and has pointer
 *     alignment.
 *   - flags: Flags controlling mutex behavior.
 *
 * This function is required when using Synchronization.Mutex.
 */
void _swift_mutex_init(void * EMBEDDED_SWIFT_NONNULL mutex,
                       swift_mutex_flags_t flags);

/**
 * Destroys a mutex initialized by `_swift_mutex_init`.
 *
 * - Parameters:
 *   - mutex: The mutex to destroy. Must not be locked.
 */
void _swift_mutex_destroy(void * EMBEDDED_SWIFT_NONNULL mutex);

/**
 * Acquires a mutex, blocking or spinning until ownership is obtained.
 *
 * - Parameters:
 *   - mutex: The mutex to acquire.
 */
void _swift_mutex_lock(void * EMBEDDED_SWIFT_NONNULL mutex);

/**
 * Releases a mutex held by the current execution context.
 *
 * - Parameters:
 *   - mutex: The mutex to release.
 */
void _swift_mutex_unlock(void * EMBEDDED_SWIFT_NONNULL mutex);

/**
 * Attempts to acquire a mutex without blocking.
 *
 * - Parameters:
 *   - mutex: The mutex to attempt to acquire.
 *
 * - Returns: Nonzero if the mutex was acquired, or zero if it was not acquired.
 *
 * This function is used to implement
 * `Synchronization.Mutex.withLockIfAvailable(_:)`.
 *
 * - Important: The implementation must not be subject to spurious failures of
 *   the kind that can occur when using a weak atomic compare-and-exchange
 *   operation. As such, if your implementation is based on an atomic
 *   compare-and-exchange operation, it _must_ use the strong variant or execute
 *   the operation in a loop so that it is not subject to spurious failures.
 *
 *   If your implementation is based on C11's `mtx_trylock()` or C++11's
 *   `std::mutex::try_lock()`, be aware that those languages' standards also
 *   allow for spurious failures.
 *
 *   POSIX's `pthread_mutex_trylock()` function does not allow for spurious
 *   failures.
 */
__swift_ptrdiff_t _swift_mutex_tryLock(void * EMBEDDED_SWIFT_NONNULL mutex);

/**
 * Initializes a recursive mutex, i.e., one that can be acquired more than
 * once by the same execution context without deadlocking.
 *
 * - Parameters:
 *   - mutex: Opaque caller-owned mutex storage initialized by this function
 *     and later passed to the other `_swift_mutexRecursive_*` functions. The
 *     contents are private to the platform implementation. The storage is at
 *     least EMBEDDED_SWIFT_MUTEX_RECURSIVE_NUM_WORDS pointer-sized words and
 *     has pointer alignment.
 *   - flags: Flags controlling mutex behavior.
 *
 * This function is required when using Synchronization.Mutex with a
 * recursive locking policy.
 */
void _swift_mutexRecursive_init(void * EMBEDDED_SWIFT_NONNULL mutex,
                                swift_mutex_flags_t flags);

/**
 * Destroys a recursive mutex initialized by `_swift_mutexRecursive_init`.
 *
 * - Parameters:
 *   - mutex: The mutex to destroy. Must not be locked.
 */
void _swift_mutexRecursive_destroy(void * EMBEDDED_SWIFT_NONNULL mutex);

/**
 * Acquires a recursive mutex, blocking or spinning until ownership is
 * obtained. If the current execution context already holds the mutex, this
 * increments its recursion count instead of deadlocking.
 *
 * - Parameters:
 *   - mutex: The mutex to acquire.
 */
void _swift_mutexRecursive_lock(void * EMBEDDED_SWIFT_NONNULL mutex);

/**
 * Releases one level of recursive ownership of a mutex held by the current
 * execution context.
 *
 * - Parameters:
 *   - mutex: The mutex to release.
 */
void _swift_mutexRecursive_unlock(void * EMBEDDED_SWIFT_NONNULL mutex);

#if defined(__cplusplus)
}
#endif

#endif
