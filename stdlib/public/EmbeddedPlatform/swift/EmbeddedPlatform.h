/*===------------------ Embedded Swift Platform Declarations ------*- C -*-===*
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
 * to provide to work with Embedded Swift. A "platform" in this case is broadly
 * construed, and can be anything from a couple of function implementations
 * within a standalone Embedded Swift program to a full-fledged SDK for an
 * OS. Either way, any of these functions can be implemented either in Swift
 * (using `@c`) or in C and linked into the final Embedded Swift executable.
 *
 * Many of these functions precisely match the signatures of functions defined
 * by POSIX. This is deliberate, because it allows simple pass-through
 * implementations for POSIX systems.
 *
 *===----------------------------------------------------------------------===*/

#ifndef EMBEDDED_SWIFT_PLATFORM_H
#define EMBEDDED_SWIFT_PLATFORM_H

#include "SwiftPlatform/Errors.h"
#include "SwiftPlatform/Heap.h"
#include "SwiftPlatform/Mutex.h"
#include "SwiftPlatform/Random.h"
#include "SwiftPlatform/StandardOutput.h"
#include "SwiftPlatform/Types.h"
#include "SwiftPlatform/Version.h"

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

/**
 * Returns nonzero when the current execution context is the platform's main
 * execution context.
 */
__swift_ptrdiff_t _swift_thread_isMain(void);

/**
 * Exit the program.
 *
 * - Parameters:
 *   - code: The exit code, which is typically 0 for normal termination.
 *
 * This function must not return.
 *
 * This function can be implemented directly with a call to the POSIX exit()
 * function.
 */
void _swift_exit(int code) EMBEDDED_SWIFT_NORETURN;

#if defined(__cplusplus)
}
#endif

#undef EMBEDDED_SWIFT_NAME
#undef EMBEDDED_SWIFT_OPTION_SET

#endif
