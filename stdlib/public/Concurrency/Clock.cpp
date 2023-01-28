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
#include "swift/Runtime/Once.h"

#include <time.h>
#if defined(_WIN32)
#define WIN32_LEAN_AND_MEAN
#define NOMINMAX
#include <Windows.h>
#include <realtimeapiset.h>
#endif

#include "Error.h"

using namespace swift;

SWIFT_EXPORT_FROM(swift_Concurrency)
SWIFT_CC(swift)
void swift_get_time(
  long long *seconds,
  long long *nanoseconds,
  swift_clock_id clock_id) {
  switch (clock_id) {
    case swift_clock_id_continuous: {
      struct timespec continuous;
#if defined(__linux__)
      clock_gettime(CLOCK_BOOTTIME, &continuous);
#elif defined(__APPLE__)
      clock_gettime(CLOCK_MONOTONIC_RAW, &continuous);
#elif (defined(__OpenBSD__) || defined(__FreeBSD__) || defined(__wasi__))
      clock_gettime(CLOCK_MONOTONIC, &continuous);
#elif defined(_WIN32)
      LARGE_INTEGER freq;
      QueryPerformanceFrequency(&freq);
      LARGE_INTEGER count;
      QueryPerformanceCounter(&count);
      // Divide count (number of ticks) by frequency (number of ticks per
      // second) to get the counter in seconds. We also need to multiply the
      // count by 1,000,000,000 to get nanosecond resolution. By multiplying
      // first, we maintain high precision. The resulting value is the tick
      // count in nanoseconds. Use 128-bit math to avoid overflowing.
      auto quadPart = static_cast<unsigned __int128>(count.QuadPart);
      auto ns = (quadPart * 1'000'000'000) / freq.QuadPart;
      continuous.tv_sec = ns / 1'000'000'000;
      continuous.tv_nsec = ns % 1'000'000'000;
#else
#error Missing platform continuous time definition
#endif
      *seconds = continuous.tv_sec;
      *nanoseconds = continuous.tv_nsec;
      return;
    }
    case swift_clock_id_suspending: {
      struct timespec suspending;
#if defined(__linux__)
      clock_gettime(CLOCK_MONOTONIC, &suspending);
#elif defined(__APPLE__)
      clock_gettime(CLOCK_UPTIME_RAW, &suspending);
#elif defined(__wasi__)
      clock_gettime(CLOCK_MONOTONIC, &suspending);
#elif (defined(__OpenBSD__) || defined(__FreeBSD__))
      clock_gettime(CLOCK_UPTIME, &suspending);
#elif defined(_WIN32)
      // QueryUnbiasedInterruptTimePrecise() was added in Windows 10 and is, as
      // the name suggests, more precise than QueryUnbiasedInterruptTime().
      // Unfortunately, the symbol is not listed in any .lib file in the SDK and
      // must be looked up dynamically at runtime even if our minimum deployment
      // target is Windows 10.
      typedef decltype(QueryUnbiasedInterruptTimePrecise) *QueryUITP_FP;
      static QueryUITP_FP queryUITP = nullptr;
      static swift::once_t onceToken;
      swift::once(onceToken, [] {
        if (HMODULE hKernelBase = GetModuleHandleW(L"KernelBase.dll")) {
          queryUITP = reinterpret_cast<QueryUITP_FP>(
            GetProcAddress(hKernelBase, "QueryUnbiasedInterruptTimePrecise")
          );
        }
      });

      // Call whichever API is available. Both output a value measured in 100ns
      // units. We must divide the output by 10,000,000 to get a value in
      // seconds and multiply the remainder by 100 to get nanoseconds.
      ULONGLONG unbiasedTime;
      if (queryUITP) {
        (* queryUITP)(&unbiasedTime);
      } else {
        // Fall back to the older, less precise API.
        (void)QueryUnbiasedInterruptTime(&unbiasedTime);
      }
      suspending.tv_sec = unbiasedTime / 10'000'000;
      suspending.tv_nsec = (unbiasedTime % 10'000'000) * 100;
#else
#error Missing platform suspending time definition
#endif
      *seconds = suspending.tv_sec;
      *nanoseconds = suspending.tv_nsec;
      return;
    }
  }
  swift_Concurrency_fatalError(0, "Fatal error: invalid clock ID %d\n",
                               clock_id);
}

SWIFT_EXPORT_FROM(swift_Concurrency)
SWIFT_CC(swift)
void swift_get_clock_res(
  long long *seconds,
  long long *nanoseconds,
  swift_clock_id clock_id) {
switch (clock_id) {
    case swift_clock_id_continuous: {
      struct timespec continuous;
#if defined(__linux__)
      clock_getres(CLOCK_BOOTTIME, &continuous);
#elif defined(__APPLE__)
      clock_getres(CLOCK_MONOTONIC_RAW, &continuous);
#elif (defined(__OpenBSD__) || defined(__FreeBSD__) || defined(__wasi__))
      clock_getres(CLOCK_MONOTONIC, &continuous);
#elif defined(_WIN32)
      LARGE_INTEGER freq;
      QueryPerformanceFrequency(&freq);
      continuous.tv_sec = 0;
      continuous.tv_nsec = 1'000'000'000 / freq.QuadPart;
#else
#error Missing platform continuous time definition
#endif
      *seconds = continuous.tv_sec;
      *nanoseconds = continuous.tv_nsec;
      return;
    }
    case swift_clock_id_suspending: {
      struct timespec suspending;
#if defined(__linux__)
      clock_getres(CLOCK_MONOTONIC_RAW, &suspending);
#elif defined(__APPLE__)
      clock_getres(CLOCK_UPTIME_RAW, &suspending);
#elif defined(__wasi__)
      clock_getres(CLOCK_MONOTONIC, &suspending);
#elif (defined(__OpenBSD__) || defined(__FreeBSD__))
      clock_getres(CLOCK_UPTIME, &suspending);
#elif defined(_WIN32)
      suspending.tv_sec = 0;
      suspending.tv_nsec = 100;
#else
#error Missing platform suspending time definition
#endif
      *seconds = suspending.tv_sec;
      *nanoseconds = suspending.tv_nsec;
      return;
    }
  }
  swift_Concurrency_fatalError(0, "Fatal error: invalid clock ID %d\n",
                               clock_id);
}
