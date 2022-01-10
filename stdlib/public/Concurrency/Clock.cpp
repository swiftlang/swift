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
      clock_gettime(CLOCK_MONOTONIC, &continuous);
      *seconds = continuous.tv_sec;
      *nanoseconds = continuous.tv_nsec;
#else
#error Missing platform continuous time definition
#endif
      break;
    }
    case swift_clock_id_realtime: {
#if HAS_TIME
      struct timespec realtime;
      clock_gettime(CLOCK_REALTIME, &realtime);
      *seconds = realtime.tv_sec;
      *nanoseconds = realtime.tv_nsec;
#else
#error Missing platform continuous time definition
#endif
      break;
    }
    case swift_clock_id_suspending: {
      struct timespec continuous;
#if defined(__linux__) && HAS_TIME
      clock_gettime(CLOCK_MONOTONIC_RAW, &continuous);
      *seconds = continuous.tv_sec;
      *nanoseconds = continuous.tv_nsec;
#elif defined(__APPLE__) && HAS_TIME
      clock_gettime(CLOCK_UPTIME_RAW, &continuous);
      *seconds = continuous.tv_sec;
      *nanoseconds = continuous.tv_nsec;
#else
#error Missing platform continuous time definition
#endif
      break;
    }
  }
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
      clock_getres(CLOCK_MONOTONIC, &continuous);
      *seconds = continuous.tv_sec;
      *nanoseconds = continuous.tv_nsec;
#else
#error Missing platform continuous time definition
#endif
      break;
    }
    case swift_clock_id_realtime: {
#if HAS_TIME
      struct timespec realtime;
      clock_getres(CLOCK_REALTIME, &realtime);
      *seconds = realtime.tv_sec;
      *nanoseconds = realtime.tv_nsec;
#else
#error Missing platform continuous time definition
#endif
      break;
    }
    case swift_clock_id_suspending: {
      struct timespec continuous;
#if defined(__linux__) && HAS_TIME
      clock_getres(CLOCK_MONOTONIC_RAW, &continuous);
      *seconds = continuous.tv_sec;
      *nanoseconds = continuous.tv_nsec;
#elif defined(__APPLE__) && HAS_TIME
      clock_gettime(CLOCK_UPTIME_RAW, &continuous);
      *seconds = continuous.tv_sec;
      *nanoseconds = continuous.tv_nsec;
#else
#error Missing platform continuous time definition
#endif
      break;
    }
  }
}
