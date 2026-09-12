/*===------ EmbeddedPlatformClockPOSIX.c -------------------------*- C -*-===*
 *
 * This source file is part of the Swift.org open source project
 *
 * Copyright (c) 2026 Apple Inc. and the Swift project authors
 * Licensed under Apache License v2.0 with Runtime Library Exception
 *
 * See https://swift.org/LICENSE.txt for license information
 * See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
 *
 *===----------------------------------------------------------------------===*
 *
 * An implementation of the Embedded Swift platform clock hooks on top of the
 * POSIX clock facilities.
 *
 * The mapping from the Swift clocks to the native clocks matches the default
 * implementations in stdlib/public/Concurrency/Clock.cpp, so that enabling the
 * platform abstraction layer does not change the observable clock behavior on
 * these targets.
 *
 *===----------------------------------------------------------------------===*/

#include "swift/EmbeddedPlatform.h"

#include <errno.h>
#include <time.h>

#if defined(__APPLE__)
#define SWIFT_CONTINUOUS_CLOCK CLOCK_MONOTONIC_RAW
#define SWIFT_SUSPENDING_CLOCK CLOCK_UPTIME_RAW
#define SWIFT_SUSPENDING_RESOLUTION_CLOCK CLOCK_UPTIME_RAW
#elif defined(__linux__)
#define SWIFT_CONTINUOUS_CLOCK CLOCK_BOOTTIME
#define SWIFT_SUSPENDING_CLOCK CLOCK_MONOTONIC
/* Clock.cpp asks CLOCK_MONOTONIC_RAW for the suspending clock's resolution
 * even though it reads the time from CLOCK_MONOTONIC. Reproduce that here
 * rather than quietly change what minimumResolution reports on Linux. */
#define SWIFT_SUSPENDING_RESOLUTION_CLOCK CLOCK_MONOTONIC_RAW
#elif defined(__FreeBSD__) || defined(__OpenBSD__)
#define SWIFT_CONTINUOUS_CLOCK CLOCK_MONOTONIC
#define SWIFT_SUSPENDING_CLOCK CLOCK_UPTIME
#define SWIFT_SUSPENDING_RESOLUTION_CLOCK CLOCK_UPTIME
#else
#define SWIFT_CONTINUOUS_CLOCK CLOCK_MONOTONIC
#define SWIFT_SUSPENDING_CLOCK CLOCK_MONOTONIC
#define SWIFT_SUSPENDING_RESOLUTION_CLOCK CLOCK_MONOTONIC
#endif

static void swift_platform_getTime(clockid_t clock,
                                   __swift_int64_t *seconds,
                                   __swift_int64_t *nanoseconds) {
  struct timespec ts;
  clock_gettime(clock, &ts);
  *seconds = ts.tv_sec;
  *nanoseconds = ts.tv_nsec;
}

static void swift_platform_getResolution(clockid_t clock,
                                         __swift_int64_t *seconds,
                                         __swift_int64_t *nanoseconds) {
  struct timespec ts;
  clock_getres(clock, &ts);
  *seconds = ts.tv_sec;
  *nanoseconds = ts.tv_nsec;
}

void _swift_clockContinuous_getTime(__swift_int64_t *seconds,
                                    __swift_int64_t *nanoseconds) {
  swift_platform_getTime(SWIFT_CONTINUOUS_CLOCK, seconds, nanoseconds);
}

void _swift_clockContinuous_getResolution(__swift_int64_t *seconds,
                                          __swift_int64_t *nanoseconds) {
  swift_platform_getResolution(SWIFT_CONTINUOUS_CLOCK, seconds, nanoseconds);
}

void _swift_clockSuspending_getTime(__swift_int64_t *seconds,
                                    __swift_int64_t *nanoseconds) {
  swift_platform_getTime(SWIFT_SUSPENDING_CLOCK, seconds, nanoseconds);
}

void _swift_clockSuspending_getResolution(__swift_int64_t *seconds,
                                          __swift_int64_t *nanoseconds) {
  swift_platform_getResolution(SWIFT_SUSPENDING_RESOLUTION_CLOCK,
                               seconds, nanoseconds);
}

void _swift_clock_sleep(__swift_int64_t seconds, __swift_int64_t nanoseconds) {
  struct timespec ts;
  ts.tv_sec = (time_t)seconds;
  ts.tv_nsec = (long)nanoseconds;
  while (nanosleep(&ts, &ts) == -1 && errno == EINTR) {
  }
}
