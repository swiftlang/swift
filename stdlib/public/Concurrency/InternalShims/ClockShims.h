//===--- ClockShims.h - Clock IDs and types for the Swift clocks -*- C -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
// C library values for Runtime/Clock. ExecutorChecks.cpp checks each value at
// compile time.
//===----------------------------------------------------------------------===//

#ifndef SWIFT_CONCURRENCY_CLOCK_SHIMS_H
#define SWIFT_CONCURRENCY_CLOCK_SHIMS_H

// The system clocks of the runtime clocks, as in EmbeddedPlatformClockPOSIX.c.
#if defined(__APPLE__)
typedef unsigned int swift_concurrency_clockid_t;
#define SWIFT_CONCURRENCY_CONTINUOUS_CLOCK 4            // CLOCK_MONOTONIC_RAW
#define SWIFT_CONCURRENCY_SUSPENDING_CLOCK 8            // CLOCK_UPTIME_RAW
#define SWIFT_CONCURRENCY_SUSPENDING_RESOLUTION_CLOCK 8 // CLOCK_UPTIME_RAW
#define SWIFT_CONCURRENCY_WALL_CLOCK 0                  // CLOCK_REALTIME
#elif defined(__linux__)
typedef int swift_concurrency_clockid_t;
#define SWIFT_CONCURRENCY_CONTINUOUS_CLOCK 7            // CLOCK_BOOTTIME
#define SWIFT_CONCURRENCY_SUSPENDING_CLOCK 1            // CLOCK_MONOTONIC
#define SWIFT_CONCURRENCY_SUSPENDING_RESOLUTION_CLOCK 4 // CLOCK_MONOTONIC_RAW
#define SWIFT_CONCURRENCY_WALL_CLOCK 0                  // CLOCK_REALTIME
#elif defined(__FreeBSD__)
typedef int swift_concurrency_clockid_t;
#define SWIFT_CONCURRENCY_CONTINUOUS_CLOCK 4            // CLOCK_MONOTONIC
#define SWIFT_CONCURRENCY_SUSPENDING_CLOCK 5            // CLOCK_UPTIME
#define SWIFT_CONCURRENCY_SUSPENDING_RESOLUTION_CLOCK 5 // CLOCK_UPTIME
#define SWIFT_CONCURRENCY_WALL_CLOCK 0                  // CLOCK_REALTIME
#elif defined(__OpenBSD__)
typedef int swift_concurrency_clockid_t;
#define SWIFT_CONCURRENCY_CONTINUOUS_CLOCK 3            // CLOCK_MONOTONIC
#define SWIFT_CONCURRENCY_SUSPENDING_CLOCK 5            // CLOCK_UPTIME
#define SWIFT_CONCURRENCY_SUSPENDING_RESOLUTION_CLOCK 5 // CLOCK_UPTIME
#define SWIFT_CONCURRENCY_WALL_CLOCK 0                  // CLOCK_REALTIME
#elif defined(__wasi__)
// A wasi-libc clock ID is the address of a clock object. Swift code uses the
// selectors below, and swift_concurrency_systemClock gives the clock ID.
struct __clockid;
typedef const struct __clockid *swift_concurrency_clockid_t;
extern const struct __clockid _CLOCK_MONOTONIC;
extern const struct __clockid _CLOCK_REALTIME;
#define SWIFT_CONCURRENCY_CONTINUOUS_CLOCK 1            // CLOCK_MONOTONIC
#define SWIFT_CONCURRENCY_SUSPENDING_CLOCK 1            // CLOCK_MONOTONIC
#define SWIFT_CONCURRENCY_SUSPENDING_RESOLUTION_CLOCK 1 // CLOCK_MONOTONIC
#define SWIFT_CONCURRENCY_WALL_CLOCK 0                  // CLOCK_REALTIME
static inline swift_concurrency_clockid_t
swift_concurrency_systemClock(int selector) {
  return selector == SWIFT_CONCURRENCY_WALL_CLOCK ? &_CLOCK_REALTIME
                                                  : &_CLOCK_MONOTONIC;
}
#elif defined(_WIN32) && !defined(__cplusplus)
// The Win32 time functions, so that Swift calls them with the calling
// convention of WINAPI (__stdcall on 32-bit x86).
typedef struct {
  unsigned long dwLowDateTime;
  unsigned long dwHighDateTime;
} swift_concurrency_filetime;
__declspec(dllimport) void __stdcall
QueryInterruptTimePrecise(unsigned long long *lpInterruptTimePrecise);
__declspec(dllimport) void __stdcall
QueryUnbiasedInterruptTimePrecise(unsigned long long *lpUnbiasedInterruptTimePrecise);
__declspec(dllimport) void __stdcall
GetSystemTimePreciseAsFileTime(swift_concurrency_filetime *lpSystemTimeAsFileTime);
__declspec(dllimport) unsigned long __stdcall
SleepEx(unsigned long dwMilliseconds, int bAlertable);
#elif defined(__EMSCRIPTEN__)
typedef int swift_concurrency_clockid_t;
#define SWIFT_CONCURRENCY_CONTINUOUS_CLOCK 1            // CLOCK_MONOTONIC
#define SWIFT_CONCURRENCY_SUSPENDING_CLOCK 1            // CLOCK_MONOTONIC
#define SWIFT_CONCURRENCY_SUSPENDING_RESOLUTION_CLOCK 1 // CLOCK_MONOTONIC
#define SWIFT_CONCURRENCY_WALL_CLOCK 0                  // CLOCK_REALTIME
#elif defined(__CYGWIN__) || defined(__HAIKU__)
// CLOCK_MONOTONIC for the continuous and suspending clocks: the clock of
// std::chrono::steady_clock in libstdc++, which Clock.cpp used here.
#if defined(__CYGWIN__)
typedef unsigned long swift_concurrency_clockid_t;
#define SWIFT_CONCURRENCY_CONTINUOUS_CLOCK 4            // CLOCK_MONOTONIC
#define SWIFT_CONCURRENCY_SUSPENDING_CLOCK 4            // CLOCK_MONOTONIC
#define SWIFT_CONCURRENCY_SUSPENDING_RESOLUTION_CLOCK 4 // CLOCK_MONOTONIC
#define SWIFT_CONCURRENCY_WALL_CLOCK 1                  // CLOCK_REALTIME
#else
typedef int swift_concurrency_clockid_t;
#define SWIFT_CONCURRENCY_CONTINUOUS_CLOCK 0            // CLOCK_MONOTONIC
#define SWIFT_CONCURRENCY_SUSPENDING_CLOCK 0            // CLOCK_MONOTONIC
#define SWIFT_CONCURRENCY_SUSPENDING_RESOLUTION_CLOCK 0 // CLOCK_MONOTONIC
#define SWIFT_CONCURRENCY_WALL_CLOCK -1                 // CLOCK_REALTIME
#endif
#elif __STDC_HOSTED__ && !defined(_WIN32)
#error "The Swift clocks need an explicit clock branch for this platform in ClockShims.h"
#endif

// `time_t` and `struct timespec` of the C library.
#if defined(__OpenBSD__) || defined(__wasi__) || defined(__EMSCRIPTEN__) ||   \
    (defined(__FreeBSD__) && !defined(__i386__))
typedef __INT64_TYPE__ swift_concurrency_time_t;
#else
typedef long swift_concurrency_time_t;
#endif

typedef struct {
  swift_concurrency_time_t tv_sec;
  long tv_nsec;
} swift_concurrency_timespec;

#endif // SWIFT_CONCURRENCY_CLOCK_SHIMS_H
