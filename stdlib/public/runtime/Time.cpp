//===--- Time.cpp ---------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/Runtime/Once.h"

#if __has_include(<time.h>)
#define HAS_TIME 1
#include <time.h>
#endif 

#if __has_include(<System/mach/mach_time.h>)
#define HAS_MACH_GET_TIMES 1
#define HAS_MACH_TIME 1
#include <System/mach/mach_time.h>
#elif __has_include(<mach/mach_time.h>)
#define HAS_MACH_TIME 1
#include <mach/mach_time.h>
#endif

using namespace swift;

#if HAS_MACH_TIME
static struct mach_timebase_info timebaseInfo;
static swift_once_t computedInfo = SWIFT_ONCE_INIT;

static void _calculateTimebase(void *unused) {
  mach_timebase_info(&timebaseInfo);
}

static inline struct mach_timebase_info calculateTimebase(void) {
  swift_once(&computedInfo, _calculateTimebase, nullptr);
  return timebaseInfo;
}
#endif

SWIFT_RUNTIME_EXPORT
SWIFT_CC(swift)
void swift_get_time(
  unsigned long long *_Nullable absoluteNanoseconds,
  unsigned long long *_Nullable continuousNanoseconds,
  long long *_Nullable realtimeSeconds,
  long long *_Nullable realtimeNanoseconds) {
  
#if defined(__linux__) && HAS_TIME
  struct timespec monotonic;
  struct timespec uptime;
  struct timespec realtime;
  clock_gettime(CLOCK_MONOTONIC, &monotonic);
  clock_gettime(CLOCK_BOOTTIME, &uptime);
  clock_gettime(CLOCK_REALTIME, &realtime);

  if (absoluteNanoseconds) *absoluteNanoseconds = monotonic.tv_sec * 1000000000 + monotonic.tv_nsec;
  if (continuousNanoseconds) *continuousNanoseconds = uptime.tv_sec * 1000000000 + uptime.tv_nsec;
  if (realtimeSeconds) *realtimeSeconds = realtime.tv_sec;
  if (realtimeNanoseconds) *realtimeNanoseconds = realtime.tv_nsec;
  
#elif HAS_MACH_GET_TIMES
  struct mach_timebase_info info = calculateTimebase();
  
  uint64_t absolute;
  uint64_t continuous;
  struct timespec realtime;
  mach_get_times(&absolute, &continuous, &realtime);
  if (absoluteNanoseconds) *absoluteNanoseconds = (absolute * info.numer) / info.denom;
  if (continuousNanoseconds) *continuousNanoseconds = (continuous * info.numer) / info.denom;
  if (realtimeSeconds) *realtimeSeconds = realtime.tv_sec;
  if (realtimeNanoseconds) *realtimeNanoseconds = realtime.tv_nsec;
#elif defined(__APPLE__) && HAS_TIME
  struct timespec monotonic;
  struct timespec uptime;
  struct timespec realtime;
  clock_gettime(CLOCK_MONOTONIC, &monotonic);
  clock_gettime(CLOCK_UPTIME_RAW, &uptime);
  clock_gettime(CLOCK_REALTIME, &realtime);
  if (absoluteNanoseconds) *absoluteNanoseconds = uptime.tv_sec * 1000000000 + uptime.tv_nsec;
  if (continuousNanoseconds) *continuousNanoseconds = monotonic.tv_sec * 1000000000 + monotonic.tv_nsec;
  if (realtimeSeconds) *realtimeSeconds = realtime.tv_sec;
  if (realtimeNanoseconds) *realtimeNanoseconds = realtime.tv_nsec;
#else
#error Missing Time!
#endif
}