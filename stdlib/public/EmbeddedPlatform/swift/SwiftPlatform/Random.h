/*===----------------- Swift Platform Random Numbers --------------*- C -*-===*
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
 * support Swift's random number generation. This includes support for
 * initializing the hash seed for hashed collections (e.g., Set/Dictionary)
 * as well as the SystemRandomNumberGenerator type.
 *
 *===----------------------------------------------------------------------===*/

#ifndef SWIFT_PLATFORM_RANDOM_H
#define SWIFT_PLATFORM_RANDOM_H

#include "Types.h"

#if defined(__cplusplus)
extern "C" {
#endif

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

#if defined(__cplusplus)
}
#endif

#endif
