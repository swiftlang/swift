/*===----------------- Swift Platform Versioning ------------------*- C -*-===*
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
 * This file provides declarations to provide versioning for the Swift Platform
 * Abstraction Layer.
 *
 *===----------------------------------------------------------------------===*/

#ifndef SWIFT_PLATFORM_VERSION_H
#define SWIFT_PLATFORM_VERSION_H

#include "Types.h"

#if defined(__cplusplus)
extern "C" {
#endif

/**
 * Major version number for the Swift Platform Abstraction Layer.
 *
 * The major version number will be increased when there is a breaking change to
 * existing APIs.
 */
#define EMBEDDED_SWIFT_PLATFORM_VERSION_MAJOR 1

/**
 * Minor version number for the Swift Platform Abstraction Layer.
 *
 * The minor version number will be increased when new APIs or flags are
 * introduced in a manner that is not breaking for existing clients. The
 * entrypoints might be optional, where the entrypoint is needed only when
 * certain Swift functionality is used.
 */
#define EMBEDDED_SWIFT_PLATFORM_VERSION_MINOR 2

/**
 * Determine the version of the platform abstraction layer that the Embedded
 * Swift library was built with.
 *
 * - Parameters:
 *   - major: The value of EMBEDDED_SWIFT_PLATFORM_VERSION_MAJOR that the
 *     Swift standard library was built with.
 *   - minor: The value of EMBEDDED_SWIFT_PLATFORM_VERSION_MINOR that the
 *     Swift standard library was built with.
 *
 * This function is provided by the Swift standard library, and declared here
 * for the convenience of C clients.
 */
void swift_getPlatformLayerVersion(
  __swift_ptrdiff_t * EMBEDDED_SWIFT_NONNULL EMBEDDED_SWIFT_SINGLE major,
  __swift_ptrdiff_t * EMBEDDED_SWIFT_NONNULL EMBEDDED_SWIFT_SINGLE minor);

#if defined(__cplusplus)
}
#endif

#endif
