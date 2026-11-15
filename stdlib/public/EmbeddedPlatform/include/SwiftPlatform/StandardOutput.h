/*===----------------- Swift Platform Standard Output --------------*- C -*-===*
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
 * This file provides declarations for functions that a platform must provide
 * to support Swift's standard output, e.g., the print() function.
 *
 *===----------------------------------------------------------------------===*/

#ifndef SWIFT_PLATFORM_STANDARD_OUTPUT_H
#define SWIFT_PLATFORM_STANDARD_OUTPUT_H

#include "Types.h"

#if defined(__cplusplus)
extern "C" {
#endif

/**
 * Acquires the standard output stream lock.
 *
 * Embedded Swift holds this lock across a complete print operation, including
 * its terminator. The lock must be recursive: formatting a value can itself
 * call print, and output writes may acquire the same stream lock internally.
 * Each acquisition must be paired with `_swift_unlockStandardOutput` in the
 * same execution context.
 *
 * Single-threaded platforms can implement both locking functions as no-ops.
 */
void _swift_lockStandardOutput(void);

/**
 * Releases one level of ownership of the standard output stream lock.
 */
void _swift_unlockStandardOutput(void);

/**
 * Writes a sequence of UTF-8 code points to standard output.
 *
 * - Parameters:
 *   - chars: The UTF-8 code points to standard output. It is not
 *     NULL-terminated.
 *   - count: The number of UTF-8 code points.
 *
 * - Returns: The number of characters that were written.
 *
 * This function is required when using the Embedded Swift print() facilities.
 *
 * This function can be implemented as a call to fwrite or printf with the
 * specified number of code points.
 *
 * This function can be called with the standard output stream lock held.
 * A single print operation can invoke it multiple times. Locking only within
 * this function does not serialize an entire print operation.
 */
__swift_size_t _swift_writeToStandardOutput(
    const unsigned char * EMBEDDED_SWIFT_NULLABLE EMBEDDED_SWIFT_COUNTED_BY(count) chars,
    __swift_size_t count);

#if defined(__cplusplus)
}
#endif

#endif
