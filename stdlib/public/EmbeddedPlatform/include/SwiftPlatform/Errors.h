/*===----------------- Swift Platform Error Handling --------------*- C -*-===*
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
 * support Swift's error reporting. These functions are called in debug builds
 * in response to a runtime error such as a failed assertion, precondition
 * violation, or fatalError().
 *
 *===----------------------------------------------------------------------===*/

#ifndef SWIFT_PLATFORM_ERRORS_H
#define SWIFT_PLATFORM_ERRORS_H

#include "Types.h"

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

#if defined(__cplusplus)
}
#endif

#endif
