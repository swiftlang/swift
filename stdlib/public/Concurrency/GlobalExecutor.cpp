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
#include "ExecutorImpl.h"

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

extern "C" bool _swift_task_invokeSwiftCheckIsolated_c(SwiftExecutorRef executor)
{
  return swift_task_invokeSwiftCheckIsolated(*reinterpret_cast<SerialExecutorRef *>(&executor));
}


extern "C" SWIFT_CC(swift)
int8_t _task_serialExecutor_isIsolatingCurrentContext(
    HeapObject *executor,
    const Metadata *selfType,
    const SerialExecutorWitnessTable *wtable);

SWIFT_CC(swift) int8_t
swift::swift_task_invokeSwiftIsIsolatingCurrentContext(SerialExecutorRef executor)
{
  if (!executor.hasSerialExecutorWitnessTable()) {
    return static_cast<int8_t>(IsIsolatingCurrentContextDecision::NotIsolated);
  }

  auto decision = _task_serialExecutor_isIsolatingCurrentContext(
        executor.getIdentity(), swift_getObjectType(executor.getIdentity()),
        executor.getSerialExecutorWitnessTable());

  return decision;
}

extern "C" int8_t
_swift_task_invokeSwiftIsIsolatingCurrentContext_c(SwiftExecutorRef executor)
{
  return
      static_cast<int8_t>(swift_task_invokeSwiftIsIsolatingCurrentContext(
      *reinterpret_cast<SerialExecutorRef *>(&executor)));
}

extern "C" void _swift_job_run_c(SwiftJob *job, SwiftExecutorRef executor)
{
  swift_job_run(reinterpret_cast<Job *>(job),
                *reinterpret_cast<SerialExecutorRef *>(&executor));
}

extern "C" SwiftTime swift_time_now(SwiftClockId clock)
{
  SwiftTime result;
  swift_get_time(&result.seconds, &result.nanoseconds, (swift_clock_id)clock);
  return result;
}

extern "C" SwiftTime swift_time_getResolution(SwiftClockId clock)
{
  SwiftTime result;
  swift_get_clock_res(&result.seconds, &result.nanoseconds,
                      (swift_clock_id)clock);
  return result;
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

extern "C" void *swift_job_alloc(SwiftJob *job, size_t size) {
  auto task = cast<AsyncTask>(reinterpret_cast<Job *>(job));
  return _swift_task_alloc_specific(task, size);
}

extern "C" void swift_job_dealloc(SwiftJob *job, void *ptr) {
  auto task = cast<AsyncTask>(reinterpret_cast<Job *>(job));
  return _swift_task_dealloc_specific(task, ptr);
}

IsIsolatingCurrentContextDecision
swift::getIsIsolatingCurrentContextDecisionFromInt(int8_t value) {
  switch (value) {
  case -1: return IsIsolatingCurrentContextDecision::Unknown;
  case 0: return IsIsolatingCurrentContextDecision::NotIsolated;
  case 1: return IsIsolatingCurrentContextDecision::Isolated;
  default:
    swift_Concurrency_fatalError(0, "Unexpected IsIsolatingCurrentContextDecision value");
  }
}

StringRef
swift::getIsIsolatingCurrentContextDecisionNameStr(IsIsolatingCurrentContextDecision decision) {
  switch (decision) {
  case IsIsolatingCurrentContextDecision::Unknown: return "Unknown";
  case IsIsolatingCurrentContextDecision::NotIsolated: return "NotIsolated";
  case IsIsolatingCurrentContextDecision::Isolated: return "Isolated";
  }
  swift_Concurrency_fatalError(0, "Unexpected IsIsolatingCurrentContextDecision value");
}

/*****************************************************************************/
/****************************** MAIN EXECUTOR  *******************************/
/*****************************************************************************/

bool SerialExecutorRef::isMainExecutor() const {
  return swift_task_isMainExecutor(*this);
}

extern "C" bool _swift_task_isMainExecutor_c(SwiftExecutorRef executor) {
  SerialExecutorRef ref = *reinterpret_cast<SerialExecutorRef *>(&executor);
  return swift_task_isMainExecutor(ref);
}

#define OVERRIDE_GLOBAL_EXECUTOR COMPATIBILITY_OVERRIDE
#include "../CompatibilityOverride/CompatibilityOverrideIncludePath.h"
