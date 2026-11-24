/*===------------------ Embedded Swift Platform for Clocks ---------*- C -*-===*
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
 * to provide to work with clocks in Embedded Swift. It provides the basis
 * for the ContinuousClock and SuspendingClock types in the standard library.
 *
 * Many of these functions precisely match the signatures of functions defined
 * by POSIX. This is deliberate, because it allows simple pass-through
 * implementations for POSIX systems.
 *
 *===----------------------------------------------------------------------===*/

#ifndef SWIFT_PLATFORM_CLOCKS_H
#define SWIFT_PLATFORM_CLOCKS_H

#include "Types.h"

#if defined(__cplusplus)
extern "C" {
#endif

/**
 * Reads the continuous clock, which keeps advancing while the system is
 * asleep, as a time since an arbitrary fixed point such as system boot.
 *
 * - Parameter nanoseconds: Receives the remaining nanoseconds, in
 *   [0, 999999999].
 *
 * When the program is linked with dead stripping (`-dead_strip` or
 * `--gc-sections`), the `_swift_clockContinuous_*` functions are required only
 * by code that reads the continuous clock.
 *
 * This function can be implemented as a call to `clock_gettime` with
 * `CLOCK_MONOTONIC_RAW` on Darwin or `CLOCK_BOOTTIME` on Linux.
 */
void _swift_clockContinuous_getTime(
    __swift_int64_t * EMBEDDED_SWIFT_NONNULL EMBEDDED_SWIFT_SINGLE seconds,
    __swift_int64_t * EMBEDDED_SWIFT_NONNULL EMBEDDED_SWIFT_SINGLE nanoseconds);

/**
 * Reports the resolution of the continuous clock.
 *
 * This function can be implemented as a call to `clock_getres`.
 */
void _swift_clockContinuous_getResolution(
    __swift_int64_t * EMBEDDED_SWIFT_NONNULL EMBEDDED_SWIFT_SINGLE seconds,
    __swift_int64_t * EMBEDDED_SWIFT_NONNULL EMBEDDED_SWIFT_SINGLE nanoseconds);

/**
 * Reads the suspending clock, which does not advance while the system is
 * asleep, as a time since an arbitrary fixed point such as system boot.
 *
 * - Parameter nanoseconds: Receives the remaining nanoseconds, in
 *   [0, 999999999].
 *
 * When the program is linked with dead stripping, the
 * `_swift_clockSuspending_*` functions are required only by code that reads
 * the suspending clock. A platform that never sleeps can read the same time
 * source as the continuous clock.
 *
 * This function can be implemented as a call to `clock_gettime` with
 * `CLOCK_UPTIME_RAW` on Darwin or `CLOCK_MONOTONIC` on Linux.
 */
void _swift_clockSuspending_getTime(
    __swift_int64_t * EMBEDDED_SWIFT_NONNULL EMBEDDED_SWIFT_SINGLE seconds,
    __swift_int64_t * EMBEDDED_SWIFT_NONNULL EMBEDDED_SWIFT_SINGLE nanoseconds);

/**
 * Reports the resolution of the suspending clock.
 *
 * This function can be implemented as a call to `clock_getres`.
 */
void _swift_clockSuspending_getResolution(
    __swift_int64_t * EMBEDDED_SWIFT_NONNULL EMBEDDED_SWIFT_SINGLE seconds,
    __swift_int64_t * EMBEDDED_SWIFT_NONNULL EMBEDDED_SWIFT_SINGLE nanoseconds);

#if defined(__cplusplus)
}
#endif

#endif
