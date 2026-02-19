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

#include <stddef.h>
//
#if __has_feature(nullability)
#define EMBEDDED_SWIFT_NONNULL _Nonnull
#define EMBEDDED_SWIFT_NULLABLE _Nullable
#else
#define EMBEDDED_SWIFT_NONNULL
#define EMBEDDED_SWIFT_NULLABLE
#endif

/**
 * Allocates memory and places the resulting pointer in `*memptr`.
 *
 * Parameters:
 *   - `memptr`: the resulting pointer will be written into *memptr on success.
 *   - `size`: the minimum number of bytes to allocate.
 *   - `alignment`: the minimum alignment of the resulting pointer, which must
 *     be a power of at least as large as `sizeof(void *)`.
 *
 * Returns 0 on success, any other value on failure.
 *
 * This function is required when using any Embedded Swift facility that
 * requires memory allocation from the heap, whether explicitly (e.g., via the
 * `allocate` operation on unsafe pointers) or implicitly (e.g., creating a
 * copy-on-write array or an instance of a class type).
 * 
 * This function can be implemented as a direct call to `posix_memalign`.
 */
int _swift_alignedAllocate(void * EMBEDDED_SWIFT_NULLABLE * EMBEDDED_SWIFT_NONNULL memptr, size_t alignment, size_t size);

/**
 * Frees the memory referenced by `ptr`.
 *
 * Parameters:
 *   - `ptr`: The pointer to be freed. If it is NULL, the operation does
 *     nothing.
 *
 * This function is required when using any Embedded Swift facility that
 * requires memory allocation from the heap, whether explicitly (e.g., via the
 * `allocate` operation on unsafe pointers) or implicitly (e.g., creating a
 * copy-on-write array or an instance of a class type).
 * 
 * This function can be implemented as a direct call to `free`.
 */
void _swift_free(void * EMBEDDED_SWIFT_NONNULL ptr);

/**
 * Writes a single character to standard output.
 *
 * Parameters:
 *   - `c`: the character to write, which will be converted to an
 *     `unsigned char`.
 *
 * Returns the character that was written.
 *
 * This function is required when using the Embedded Swift print() facilities.
 *
 * This function can be implemented as a direct call to `putchar`.
 */
int _swift_writeCharToStandardOutput(int c);

/**
 * Generates random bytes into the given buffer.
 *
 * Parameters:
 *   - `buffer`: the buffer into which the random bytes should be generated.
 *   - `nbytes`: the number of bytes that should be generated into the buffer.
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
void _swift_generateRandom(void * EMBEDDED_SWIFT_NONNULL buffer, size_t nbytes);

/**
 * Generates random bytes intended for a hashing seed into the given buffer.
 *
 * Parameters:
 *   - `buffer`: the buffer into which the random bytes should be generated.
 *   - `nbytes`: the number of bytes that should be generated into the buffer.
 *
 * This function is required when using Swift's hashed collections, such as Set
 * and Dictionary, to provide random seeding for the hash functions. Random
 * seeding makes hash values differ from one execute to the next, mitigating
 * against denial-of-service attacks that target a known hash function. The
 * random number generator provided here need not be cryptographically
 * secure. An implementation may choose to provide constant values random than a
 * random seed to make hashing deterministic.
 *
 * This function can be implemented as a direct call to `arc4random_buf`.
 */
void _swift_generateRandomHashSeed(void * EMBEDDED_SWIFT_NONNULL buffer, size_t nbytes);

/**
 * Retrieve a pointer that will be used to retain information needed for Swift's
 * dynamic exclusivity checking.
 *
 * Returns the pointer most recently passed to `_swift_setExclusivityTLS` on
 * this thread. If `_swift_setExclusivityTLS` has not been called on this
 * thread, returns NULL.
 *
 * In a single-threaded environment, the `_swift_getExclusivityTLS` and
 * `_swift_setExclusivityTLS` functions can get and set a global variable that
 * is initialized to NULL. In a multi-threaded environment, the variable will
 * need to be in thread-local storage (e.g., using C11 `_Thread_local`) or a
 * similar facility.
 *
 * This function is required when using Swift's dynamic exclusivity checking,
 * which is enabled by the Swift compiler option `-enforce-exclusivity=checked`
 * and required when the compiler cannot statically prove that all accesses to a
 * given variable (such as a global variable or a stored instance property of a
 * class) respect the exclusivity model.
 */
void * EMBEDDED_SWIFT_NULLABLE _swift_getExclusivityTLS(void);

/**
 * Set the pointer that will be used to retain information needed for Swift's
 * dynamic exclusivity checking.
 *
 * Parameters:
 *   - `ptr`: the pointer to set. A subsequent call to
 *     `_swift_getExclusivityTLS` on the same thread (without an intervening
 *     call to `_swift_setExclusivityTLS`) shall return `ptr`.
 *
 * See `_swift_getExclusivityTLS` for more information about dynamic exclusivity
 * checking.
 */
void _swift_setExclusivityTLS(void * EMBEDDED_SWIFT_NULLABLE ptr);

#endif
