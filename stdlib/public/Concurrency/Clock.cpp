//===--- Clock.cpp - Time and clock resolution ----------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/Runtime/Concurrency.h"

#if __has_include(<time.h>)
#define HAS_TIME 1
#include <time.h>
#endif
#if defined(_WIN32)
#define WIN32_LEAN_AND_MEAN
#define NOMINMAX
#include <Windows.h>
#endif

using namespace swift;

SWIFT_EXPORT_FROM(swift_Concurrency)
SWIFT_CC(swift)
void swift_get_time(
  long long *seconds,
  long long *nanoseconds,
  swift_clock_id clock_id) {
  switch (clock_id) {
    case swift_clock_id_continuous: {
#if defined(__linux__) && HAS_TIME
      struct timespec continuous;
      clock_gettime(CLOCK_BOOTTIME, &continuous);
      *seconds = continuous.tv_sec;
      *nanoseconds = continuous.tv_nsec;
#elif defined(__APPLE__) && HAS_TIME
      struct timespec continuous;
      clock_gettime(CLOCK_MONOTONIC_RAW, &continuous);
      *seconds = continuous.tv_sec;
      *nanoseconds = continuous.tv_nsec;
#elif (defined(__OpenBSD__) || defined(__FreeBSD__) || defined(__wasi__)) && HAS_TIME
      struct timespec continuous;
      clock_gettime(CLOCK_MONOTONIC, &continuous);
      *seconds = continuous.tv_sec;
      *nanoseconds = continuous.tv_nsec;
#elif defined(_WIN32)
      LARGE_INTEGER freq;
      QueryPerformanceFrequency(&freq);
      LARGE_INTEGER count;
      QueryPerformanceCounter(&count);
      *seconds = count.QuadPart / freq.QuadPart;
      if (freq.QuadPart < 1000000000) {
        *nanoseconds = 
            ((count.QuadPart % freq.QuadPart) * 1000000000) / freq.QuadPart;
      } else {
        *nanoseconds = 
            (count.QuadPart % freq.QuadPart) * (1000000000.0 / freq.QuadPart);
      }
#else
#error Missing platform continuous time definition
#endif
      return;
    }
    case swift_clock_id_suspending: {
#if defined(__linux__) && HAS_TIME
      struct timespec suspending;
      clock_gettime(CLOCK_MONOTONIC, &suspending);
      *seconds = suspending.tv_sec;
      *nanoseconds = suspending.tv_nsec;
#elif defined(__APPLE__) && HAS_TIME
      struct timespec suspending;
      clock_gettime(CLOCK_UPTIME_RAW, &suspending);
      *seconds = suspending.tv_sec;
      *nanoseconds = suspending.tv_nsec;
#elif defined(__wasi__) && HAS_TIME
      struct timespec suspending;
      clock_gettime(CLOCK_MONOTONIC, &suspending);
      *seconds = suspending.tv_sec;
      *nanoseconds = suspending.tv_nsec;
#elif (defined(__OpenBSD__) || defined(__FreeBSD__)) && HAS_TIME
      struct timespec suspending;
      clock_gettime(CLOCK_UPTIME, &suspending);
      *seconds = suspending.tv_sec;
      *nanoseconds = suspending.tv_nsec;
#elif defined(_WIN32)
      LARGE_INTEGER freq;
      QueryPerformanceFrequency(&freq);
      LARGE_INTEGER count;
      QueryPerformanceCounter(&count);
      *seconds = count.QuadPart / freq.QuadPart;
      if (freq.QuadPart < 1000000000) {
        *nanoseconds = 
            ((count.QuadPart % freq.QuadPart) * 1000000000) / freq.QuadPart;
      } else {
        *nanoseconds = 
            (count.QuadPart % freq.QuadPart) * (1000000000.0 / freq.QuadPart);
      }
#else
#error Missing platform suspending time definition
#endif
      return;
    }
  }
  abort(); // Invalid clock_id
}

SWIFT_EXPORT_FROM(swift_Concurrency)
SWIFT_CC(swift)
void swift_get_clock_res(
  long long *seconds,
  long long *nanoseconds,
  swift_clock_id clock_id) {
switch (clock_id) {
    case swift_clock_id_continuous: {
#if defined(__linux__) && HAS_TIME
      struct timespec continuous;
      clock_getres(CLOCK_BOOTTIME, &continuous);
      *seconds = continuous.tv_sec;
      *nanoseconds = continuous.tv_nsec;
#elif defined(__APPLE__) && HAS_TIME
      struct timespec continuous;
      clock_getres(CLOCK_MONOTONIC_RAW, &continuous);
      *seconds = continuous.tv_sec;
      *nanoseconds = continuous.tv_nsec;
#elif (defined(__OpenBSD__) || defined(__FreeBSD__) || defined(__wasi__)) && HAS_TIME
      struct timespec continuous;
      clock_getres(CLOCK_MONOTONIC, &continuous);
      *seconds = continuous.tv_sec;
      *nanoseconds = continuous.tv_nsec;
#elif defined(_WIN32)
      *seconds = 0;
      *nanoseconds = 1000;
#else
#error Missing platform continuous time definition
#endif
      return;
    }
    case swift_clock_id_suspending: {
      struct timespec suspending;
#if defined(__linux__) && HAS_TIME
      clock_getres(CLOCK_MONOTONIC_RAW, &suspending);
      *seconds = suspending.tv_sec;
      *nanoseconds = suspending.tv_nsec;
#elif defined(__APPLE__) && HAS_TIME
      clock_getres(CLOCK_UPTIME_RAW, &suspending);
      *seconds = suspending.tv_sec;
      *nanoseconds = suspending.tv_nsec;
#elif defined(__wasi__) && HAS_TIME
      clock_getres(CLOCK_MONOTONIC, &suspending);
      *seconds = suspending.tv_sec;
      *nanoseconds = suspending.tv_nsec;
#elif (defined(__OpenBSD__) || defined(__FreeBSD__)) && HAS_TIME
      clock_getres(CLOCK_UPTIME, &suspending);
      *seconds = suspending.tv_sec;
      *nanoseconds = suspending.tv_nsec;
#elif defined(_WIN32)
      *seconds = 0;
      *nanoseconds = 1000;
#else
#error Missing platform suspending time definition
#endif
      return;
    }
  }
  abort(); // Invalid clock_id
}
