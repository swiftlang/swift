///===--- ExecutorChecks.cpp - Static assertions to check struct layouts ---===///
///
/// This source file is part of the Swift.org open source project
///
/// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
/// Licensed under Apache License v2.0 with Runtime Library Exception
///
/// See https:///swift.org/LICENSE.txt for license information
/// See https:///swift.org/CONTRIBUTORS.txt for the list of Swift project authors
///
///===----------------------------------------------------------------------===///
///
/// This file is responsible for checking that the structures in ExecutorImpl.h
/// are laid out exactly the same as those in the ABI headers, and that the
/// values in InternalShims/ClockShims.h match the C library.
///
///===----------------------------------------------------------------------===///

#include "swift/Runtime/Concurrency.h"

#include "swift/ABI/Executor.h"
#include "swift/ABI/MetadataValues.h"
#include "swift/ABI/Task.h"

#include "ExecutorImpl.h"
#include "InternalShims/ClockShims.h"

#if !SWIFT_CONCURRENCY_EMBEDDED && !defined(_WIN32)
#include <cstddef>
#include <time.h>
#endif

// JobFlags
static_assert(sizeof(swift::JobFlags) == sizeof(SwiftJobFlags));

// JobKind
static_assert(sizeof(swift::JobKind) == sizeof(SwiftJobKind));
static_assert((SwiftJobKind)swift::JobKind::Task == SwiftTaskJobKind);
static_assert((SwiftJobKind)swift::JobKind::First_Reserved == SwiftFirstReservedJobKind);

// JobPriority
static_assert(sizeof(swift::JobPriority) == sizeof(SwiftJobPriority));
static_assert((SwiftJobPriority)swift::JobPriority::UserInteractive
              == SwiftUserInteractiveJobPriority);
static_assert((SwiftJobPriority)swift::JobPriority::UserInteractive
              == SwiftUserInteractiveJobPriority);
static_assert((SwiftJobPriority)swift::JobPriority::UserInitiated
              == SwiftUserInitiatedJobPriority);
static_assert((SwiftJobPriority)swift::JobPriority::Default
              == SwiftDefaultJobPriority);
static_assert((SwiftJobPriority)swift::JobPriority::Utility
              == SwiftUtilityJobPriority);
static_assert((SwiftJobPriority)swift::JobPriority::Background
              == SwiftBackgroundJobPriority);
static_assert((SwiftJobPriority)swift::JobPriority::Unspecified
              == SwiftUnspecifiedJobPriority);

// Job (has additional fields not exposed via SwiftJob)
static_assert(sizeof(swift::Job) >= sizeof(SwiftJob));

// SerialExecutorRef
static_assert(sizeof(swift::SerialExecutorRef) == sizeof(SwiftExecutorRef));

// swift_clock_id
static_assert((SwiftClockId)swift::swift_clock_id_continuous
              == SwiftContinuousClock);
static_assert((SwiftClockId)swift::swift_clock_id_suspending ==
              SwiftSuspendingClock);

// ClockShims.h, against the C library
#if !SWIFT_CONCURRENCY_EMBEDDED && !defined(_WIN32)
static_assert(sizeof(swift_concurrency_clockid_t) == sizeof(clockid_t));
static_assert(sizeof(swift_concurrency_timespec) == sizeof(struct timespec));
static_assert(offsetof(swift_concurrency_timespec, tv_sec) ==
              offsetof(struct timespec, tv_sec));
static_assert(offsetof(swift_concurrency_timespec, tv_nsec) ==
              offsetof(struct timespec, tv_nsec));
static_assert(sizeof(swift_concurrency_time_t) == sizeof(time_t));
#if defined(__wasi__)
// WASI clock IDs are addresses.
#elif defined(__APPLE__)
static_assert(SWIFT_CONCURRENCY_CONTINUOUS_CLOCK == CLOCK_MONOTONIC_RAW);
static_assert(SWIFT_CONCURRENCY_SUSPENDING_CLOCK == CLOCK_UPTIME_RAW);
static_assert(SWIFT_CONCURRENCY_SUSPENDING_RESOLUTION_CLOCK ==
              CLOCK_UPTIME_RAW);
#elif defined(__linux__)
static_assert(SWIFT_CONCURRENCY_CONTINUOUS_CLOCK == CLOCK_BOOTTIME);
static_assert(SWIFT_CONCURRENCY_SUSPENDING_CLOCK == CLOCK_MONOTONIC);
static_assert(SWIFT_CONCURRENCY_SUSPENDING_RESOLUTION_CLOCK ==
              CLOCK_MONOTONIC_RAW);
#elif defined(__FreeBSD__) || defined(__OpenBSD__)
static_assert(SWIFT_CONCURRENCY_CONTINUOUS_CLOCK == CLOCK_MONOTONIC);
static_assert(SWIFT_CONCURRENCY_SUSPENDING_CLOCK == CLOCK_UPTIME);
static_assert(SWIFT_CONCURRENCY_SUSPENDING_RESOLUTION_CLOCK == CLOCK_UPTIME);
#elif defined(__EMSCRIPTEN__) || defined(__CYGWIN__) || defined(__HAIKU__)
static_assert(SWIFT_CONCURRENCY_CONTINUOUS_CLOCK == CLOCK_MONOTONIC);
static_assert(SWIFT_CONCURRENCY_SUSPENDING_CLOCK == CLOCK_MONOTONIC);
static_assert(SWIFT_CONCURRENCY_SUSPENDING_RESOLUTION_CLOCK ==
              CLOCK_MONOTONIC);
#endif
#if !defined(__wasi__)
static_assert(SWIFT_CONCURRENCY_WALL_CLOCK == CLOCK_REALTIME);
#endif
#endif // !SWIFT_CONCURRENCY_EMBEDDED && !defined(_WIN32)
