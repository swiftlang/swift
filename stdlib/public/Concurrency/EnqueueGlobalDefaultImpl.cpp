//===--- EnqueueGlobalDefaultImpl.cpp - Default global enqueue impl ------===//
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
///
/// Default implementations of the optional functions in ExecutorImpl.h.
///
/// Each of these lives in an object file of its own, and this file must not
/// define anything else.  When the runtime is linked statically, an object
/// file in an archive is only pulled in to satisfy an undefined symbol, so an
/// executor that defines one of these functions itself overrides the default,
/// while an existing executor that doesn't simply picks it up.
///
//===----------------------------------------------------------------------===//

#include "swift/ABI/Task.h"
#include "swift/Runtime/Concurrency.h"
#include "ExecutorImpl.h"

using namespace swift;

/// By default, wrap the continuation in a job, which we can hand to the
/// executor's existing support for jobs with a deadline.
SWIFT_CC(swift)
void swift_task_enqueueGlobalContinuationWithDeadlineImpl(
    long long sec,
    long long nsec,
    long long tsec,
    long long tnsec,
    int clock,
    SwiftTimerContinuation *continuation,
    SwiftJobPriority priority) {
  auto *job = swift_task_createNullaryContinuationJob(
      priority, reinterpret_cast<AsyncTask *>(continuation));
  swift_task_enqueueGlobalWithDeadline(sec, nsec, tsec, tnsec, clock, job);
}
