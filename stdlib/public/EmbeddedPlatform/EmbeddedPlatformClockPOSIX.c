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
 * POSIX clock facilities. The mapping from Swift clocks to native clocks
 * matches the default implementations in stdlib/public/Concurrency/Clock.cpp
 * so that enabling the platform abstraction layer does not change the
 * observable clock behavior on these targets.
 *
 *===----------------------------------------------------------------------===*/

#include "swift/EmbeddedPlatform.h"

#include <errno.h>
#include <time.h>

static clockid_t swift_embedded_platform_time_clock(
    swift_clock_id_t clock_id) {
  switch (clock_id) {
  case SWIFT_CLOCK_CONTINUOUS:
#if defined(__APPLE__)
    return CLOCK_MONOTONIC_RAW;
#elif defined(__linux__)
    return CLOCK_BOOTTIME;
#else
    return CLOCK_MONOTONIC;
#endif
  case SWIFT_CLOCK_SUSPENDING:
#if defined(__APPLE__)
    return CLOCK_UPTIME_RAW;
#elif defined(__FreeBSD__) || defined(__OpenBSD__)
    return CLOCK_UPTIME;
#else
    return CLOCK_MONOTONIC;
#endif
  case SWIFT_CLOCK_WALL:
    return CLOCK_REALTIME;
  }
  /* Unreachable: the runtime only passes the declared clock IDs. */
  return CLOCK_REALTIME;
}

/* The resolution mapping matches the clock_getres calls in Clock.cpp, which
 * on Linux query CLOCK_MONOTONIC_RAW for the suspending clock even though
 * the corresponding clock_gettime uses CLOCK_MONOTONIC. */
static clockid_t swift_embedded_platform_resolution_clock(
    swift_clock_id_t clock_id) {
#if defined(__linux__)
  if (clock_id == SWIFT_CLOCK_SUSPENDING)
    return CLOCK_MONOTONIC_RAW;
#endif
  return swift_embedded_platform_time_clock(clock_id);
}

void _swift_clock_getTime(swift_clock_id_t clock_id,
                          long long *seconds,
                          long long *nanoseconds) {
  struct timespec ts;
  clock_gettime(swift_embedded_platform_time_clock(clock_id), &ts);
  *seconds = ts.tv_sec;
  *nanoseconds = ts.tv_nsec;
}

void _swift_clock_getResolution(swift_clock_id_t clock_id,
                                long long *seconds,
                                long long *nanoseconds) {
  struct timespec ts;
  clock_getres(swift_embedded_platform_resolution_clock(clock_id), &ts);
  *seconds = ts.tv_sec;
  *nanoseconds = ts.tv_nsec;
}

void _swift_clock_sleep(long long seconds, long long nanoseconds) {
  struct timespec ts;
  ts.tv_sec = (time_t)seconds;
  ts.tv_nsec = (long)nanoseconds;
  while (nanosleep(&ts, &ts) == -1 && errno == EINTR) {
  }
}
