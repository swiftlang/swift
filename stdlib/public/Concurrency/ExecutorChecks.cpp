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
/// are laid out exactly the same as those in the ABI headers.
///
///===----------------------------------------------------------------------===///

#include "swift/Runtime/Concurrency.h"

#include "swift/ABI/Executor.h"
#include "swift/ABI/MetadataValues.h"
#include "swift/ABI/Task.h"

#include "ExecutorImpl.h"

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
