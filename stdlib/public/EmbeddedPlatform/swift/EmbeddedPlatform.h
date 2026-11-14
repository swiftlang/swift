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

#include "SwiftPlatform/Heap.h"
#include "SwiftPlatform/Mutex.h"
#include "SwiftPlatform/StandardOutput.h"
#include "SwiftPlatform/Types.h"
#include "SwiftPlatform/Version.h"

#if defined(__cplusplus)
extern "C" {
#endif

/**
 * Reports a fatal error before the program is terminated.
 *
 * A platform may implement this as an empty function. After returning from this
 * function the standard library terminates the program using a unique trap
 * whose debug info also carries the stop reason and source location.
 *
 * - Parameters:
 *   - message: The UTF-8 code points that make up the failure message. It is
 *     not NULL-terminated. May be NULL when messageCount is 0.
 *   - messageCount: The number of UTF-8 code points in message.
 *   - flags: Flags that describe more about the error condition. The lower 8
 *     bits contain a swift_error_kind_t / SwiftErrorKind value.
 */
void _swift_reportError(
    const unsigned char * EMBEDDED_SWIFT_NULLABLE EMBEDDED_SWIFT_COUNTED_BY(messageCount) message,
    __swift_size_t messageCount,
    __swift_options_t flags
);

/**
 * Reports a fatal error at a given file/line.
 *
 * A platform may implement this as an empty function. After returning from
 * this function the standard library terminates the program using a unique trap
 * whose debug info also carries the stop reason and source location.
 *
 * - Parameters:
 *   - message: The UTF-8 code points that make up the failure message. It is
 *     not NULL-terminated. May be NULL when messageCount is 0.
 *   - messageCount: The number of UTF-8 code points in message.
 *   - fileName: The UTF-8 code points that make up the file name. It is
 *     not NULL-terminated. May be NULL when fileNameCount is 0.
 *   - fileNameCount: The number of UTF-8 code points in fileName.
 *   - line: The line number within the file at which the error occurred.
 *   - flags: Flags that describe more about the error condition. The lower 8
 *     bits contain a swift_error_kind_t / SwiftErrorKind value.
 */
void _swift_reportErrorAt(
    const unsigned char * EMBEDDED_SWIFT_NULLABLE EMBEDDED_SWIFT_COUNTED_BY(messageCount) message,
    __swift_size_t messageCount,
    const unsigned char * EMBEDDED_SWIFT_NULLABLE EMBEDDED_SWIFT_COUNTED_BY(fileNameCount) fileName,
    __swift_size_t fileNameCount,
    __swift_size_t line,
    __swift_options_t flags
);

/**
 * Generates random bytes into the given buffer.
 *
 * - Parameters:
 *   - buffer: The buffer into which the random bytes should be generated.
 *   - nbytes: The number of bytes that should be generated into the buffer.
 *
 * This function is required when using Swift's SystemRandomNumberGenerator, the
 * default random number generator used for shuffling elements and producing
 * random values. While this function is encouraged to use a cryptographically
 * secure algorithm, it is not required to do so.
 *
 * Note that this function is not used to provide random seeding for the hash
 * functions used in Set and Dictionary. Those operations use
 * `_swift_generateRandomHashSeed`.
 *
 * This function can be implemented as a direct call to `arc4random_buf`.
 */
void _swift_generateRandom(void * EMBEDDED_SWIFT_NONNULL EMBEDDED_SWIFT_SIZED_BY(nbytes) buffer, __swift_size_t nbytes);

/**
 * Generates random bytes intended for a hashing seed into the given buffer.
 *
 * - Parameters:
 *   - buffer: The buffer into which the random bytes should be generated.
 *   - nbytes: The number of bytes that should be generated into the buffer.
 *
 * This function is required when using Swift's hashed collections, such as Set
 * and Dictionary, to provide random seeding for the hash functions. Random
 * seeding makes hash values differ from one execute to the next, mitigating
 * against denial-of-service attacks that target a known hash function. The
 * random number generator provided here need not be cryptographically
 * secure. An implementation may choose to provide constant values rather than a
 * random seed to make hashing deterministic.
 *
 * This function can be implemented as a direct call to `arc4random_buf`.
 */
void _swift_generateRandomHashSeed(void * EMBEDDED_SWIFT_NONNULL EMBEDDED_SWIFT_SIZED_BY(nbytes) buffer, __swift_size_t nbytes);

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
