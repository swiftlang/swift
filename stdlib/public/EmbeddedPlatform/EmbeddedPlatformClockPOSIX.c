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
 * An implementation of the Embedded Swift platform clock hooks on top of
 * `clock_gettime`, `clock_getres` and `nanosleep`. Each clock reads the native
 * clock that the Concurrency runtime's Clock.cpp reads outside Embedded Swift.
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
#define SWIFT_SUSPENDING_RESOLUTION_CLOCK CLOCK_MONOTONIC_RAW
#elif defined(__FreeBSD__) || defined(__OpenBSD__)
#define SWIFT_CONTINUOUS_CLOCK CLOCK_MONOTONIC
#define SWIFT_SUSPENDING_CLOCK CLOCK_UPTIME
#define SWIFT_SUSPENDING_RESOLUTION_CLOCK CLOCK_UPTIME
#elif defined(__wasi__) || defined(__EMSCRIPTEN__)
#define SWIFT_CONTINUOUS_CLOCK CLOCK_MONOTONIC
#define SWIFT_SUSPENDING_CLOCK CLOCK_MONOTONIC
#define SWIFT_SUSPENDING_RESOLUTION_CLOCK CLOCK_MONOTONIC
#else
#error Missing platform clock definitions
#endif

static void trap_if(int failed) {
  if (failed) {
    __builtin_trap();
  }
}

static void read_clock(clockid_t clock, __swift_int64_t *seconds,
                       __swift_int64_t *nanoseconds) {
  struct timespec ts;
  trap_if(clock_gettime(clock, &ts) != 0);
  *seconds = ts.tv_sec;
  *nanoseconds = ts.tv_nsec;
}

static void read_resolution(clockid_t clock, __swift_int64_t *seconds,
                            __swift_int64_t *nanoseconds) {
  struct timespec ts;
  trap_if(clock_getres(clock, &ts) != 0);
  *seconds = ts.tv_sec;
  *nanoseconds = ts.tv_nsec;
}

void _swift_clockContinuous_getTime(__swift_int64_t *seconds,
                                    __swift_int64_t *nanoseconds) {
  read_clock(SWIFT_CONTINUOUS_CLOCK, seconds, nanoseconds);
}

void _swift_clockContinuous_getResolution(__swift_int64_t *seconds,
                                          __swift_int64_t *nanoseconds) {
  read_resolution(SWIFT_CONTINUOUS_CLOCK, seconds, nanoseconds);
}

void _swift_clockSuspending_getTime(__swift_int64_t *seconds,
                                    __swift_int64_t *nanoseconds) {
  read_clock(SWIFT_SUSPENDING_CLOCK, seconds, nanoseconds);
}

void _swift_clockSuspending_getResolution(__swift_int64_t *seconds,
                                          __swift_int64_t *nanoseconds) {
  read_resolution(SWIFT_SUSPENDING_RESOLUTION_CLOCK, seconds, nanoseconds);
}

void _swift_clock_sleep(__swift_int64_t seconds, __swift_int64_t nanoseconds) {
  struct timespec ts;
  ts.tv_sec = (time_t)seconds;
  ts.tv_nsec = (long)nanoseconds;
  while (nanosleep(&ts, &ts) == -1) {
    trap_if(errno != EINTR);
  }
}
