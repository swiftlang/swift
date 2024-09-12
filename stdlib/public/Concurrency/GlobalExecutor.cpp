///===--- GlobalExecutor.cpp - Global concurrent executor ------------------===///
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
/// Routines related to the global concurrent execution service.
///
/// The execution side of Swift's concurrency model centers around
/// scheduling work onto various execution services ("executors").
/// Executors vary in several different dimensions:
///
/// First, executors may be exclusive or concurrent.  An exclusive
/// executor can only execute one job at once; a concurrent executor
/// can execute many.  Exclusive executors are usually used to achieve
/// some higher-level requirement, like exclusive access to some
/// resource or memory.  Concurrent executors are usually used to
/// manage a pool of threads and prevent the number of allocated
/// threads from growing without limit.
///
/// Second, executors may own dedicated threads, or they may schedule
/// work onto some underlying executor.  Dedicated threads can
/// improve the responsiveness of a subsystem *locally*, but they impose
/// substantial costs which can drive down performance *globally*
/// if not used carefully.  When an executor relies on running work
/// on its own dedicated threads, jobs that need to run briefly on
/// that executor may need to suspend and restart.  Dedicating threads
/// to an executor is a decision that should be made carefully
/// and holistically.
///
/// If most executors should not have dedicated threads, they must
/// be backed by some underlying executor, typically a concurrent
/// executor.  The purpose of most concurrent executors is to
/// manage threads and prevent excessive growth in the number
/// of threads.  Having multiple independent concurrent executors
/// with their own dedicated threads would undermine that.
/// Therefore, it is sensible to have a single, global executor
/// that will ultimately schedule most of the work in the system.
/// With that as a baseline, special needs can be recognized and
/// carved out from the global executor with its cooperation.
///
/// This file defines Swift's interface to that global executor.
///
/// The default implementation is backed by libdispatch, but there
/// may be good reasons to provide alternatives (e.g. when building
/// a single-threaded runtime).
///
///===----------------------------------------------------------------------===///

#include "../CompatibilityOverride/CompatibilityOverride.h"
#include "swift/Runtime/Concurrency.h"
#include "swift/Runtime/EnvironmentVariables.h"
#include "TaskPrivate.h"
#include "Error.h"
#include "ExecutorHooks.h"

using namespace swift;

extern "C" SWIFT_CC(swift)
    void _task_serialExecutor_checkIsolated(
        HeapObject *executor,
        const Metadata *selfType,
        const SerialExecutorWitnessTable *wtable);

SWIFT_CC(swift)
bool swift::swift_task_invokeSwiftCheckIsolated(SerialExecutorRef executor)
{
  if (!executor.hasSerialExecutorWitnessTable())
    return false;

  _task_serialExecutor_checkIsolated(
        executor.getIdentity(), swift_getObjectType(executor.getIdentity()),
        executor.getSerialExecutorWitnessTable());

  return true;
}

// Implemented in Swift because we need to obtain the user-defined flags on the executor ref.
//
// We could inline this with effort, though.
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
extern "C" SWIFT_CC(swift)
SerialExecutorRef _task_serialExecutor_getExecutorRef(
        HeapObject *executor, const Metadata *selfType,
        const SerialExecutorWitnessTable *wtable);
#pragma clang diagnostic pop

/// WARNING: This method is expected to CRASH in new runtimes, and cannot be
/// used to implement "log warnings" mode. We would need a new entry point to
/// implement a "only log warnings" actor isolation checking mode, and it would
/// no be able handle more complex situations, as `SerialExecutor.checkIsolated`
/// is able to (by calling into dispatchPrecondition on old runtimes).
SWIFT_CC(swift)
bool swift::swift_task_isOnExecutorImpl(HeapObject *executor,
                                        const Metadata *selfType,
                                        const SerialExecutorWitnessTable *wtable)
{
  auto executorRef = _task_serialExecutor_getExecutorRef(executor,
                                                         selfType,
                                                         wtable);
  return swift_task_isCurrentExecutor(executorRef);
}

bool swift::swift_executor_isComplexEquality(SerialExecutorRef ref) {
  return ref.isComplexEquality();
}

uint64_t swift::swift_task_getJobTaskId(Job *job) {
  if (auto task = dyn_cast<AsyncTask>(job)) {
    // TaskID is actually:
    //   32bits of Job's Id
    // + 32bits stored in the AsyncTask
    return task->getTaskId();
  } else {
    return job->getJobId();
  }
}

/*****************************************************************************/
/****************************** MAIN EXECUTOR  *******************************/
/*****************************************************************************/

bool SerialExecutorRef::isMainExecutor() const {
  return swift_task_isMainExecutor(*this);
}

#define OVERRIDE_GLOBAL_EXECUTOR COMPATIBILITY_OVERRIDE
#include COMPATIBILITY_OVERRIDE_INCLUDE_PATH
