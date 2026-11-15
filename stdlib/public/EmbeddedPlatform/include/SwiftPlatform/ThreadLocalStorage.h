/*===------------- Swift Platform Thread Local Storage -------------*- C -*-===*
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
 * This file provides declarations for functions that a platform must provide to
 * support Swift. While Swift does not provide a general mechanism for
 * thread-local storage, the Swift runtime does require a fixed set of
 * thread-local entries (SWIFT_TLS_KEY_COUNT) from the platform to support
 * dynamic exclusivity checking, concurrency, and observation.
 *
 *===----------------------------------------------------------------------===*/

#ifndef SWIFT_PLATFORM_THREAD_LOCAL_STORAGE_H
#define SWIFT_PLATFORM_THREAD_LOCAL_STORAGE_H

#include "Types.h"

#if defined(__cplusplus)
extern "C" {
#endif

/**
 * Initializes a reserved TLS key. `key` is one of the numeric reserved keys
 * described by `SWIFT_TLS_KEY_COUNT`. `destructor` may be NULL. This function
 * is called at most once for each key that needs a destructor.
 */
void _swift_tls_init(__swift_tls_key_t key,
                     __swift_tls_dtor_t EMBEDDED_SWIFT_NULLABLE destructor);

/**
 * Returns the value stored for a TLS key in the current execution context, or
 * NULL if no value has been stored.
 *
 * This function may be called before `_swift_tls_init`. In that case, it must
 * return NULL until a value is stored for the key.
 *
 * Precondition: `key < SWIFT_TLS_KEY_COUNT`.
 */
void * EMBEDDED_SWIFT_NULLABLE _swift_tls_get(__swift_tls_key_t key);

/**
 * Stores a value for a TLS key in the current execution context.
 *
 * This function may be called before `_swift_tls_init`. The platform must make
 * storage available for the key on demand in that case.
 *
 * Precondition: `key < SWIFT_TLS_KEY_COUNT`.
 */
void _swift_tls_set(__swift_tls_key_t key,
                    void * EMBEDDED_SWIFT_NULLABLE value);

#if defined(__cplusplus)
}
#endif

#endif
