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
/// work onto some some underlying executor.  Dedicated threads can
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

#include "CompatibilityOverride.h"
#include "ConcurrencyRuntime.h"
#include "swift/Runtime/EnvironmentVariables.h"
#include "TaskPrivate.h"
#include "Error.h"

using namespace swift;

SWIFT_CC(swift)
void (*swift::swift_task_enqueueGlobal_hook)(
    Job *job, swift_task_enqueueGlobal_original original) = nullptr;

SWIFT_CC(swift)
void (*swift::swift_task_enqueueGlobalWithDelay_hook)(
    JobDelay delay, Job *job,
    swift_task_enqueueGlobalWithDelay_original original) = nullptr;

SWIFT_CC(swift)
void (*swift::swift_task_enqueueMainExecutor_hook)(
    Job *job, swift_task_enqueueMainExecutor_original original) = nullptr;

#if SWIFT_CONCURRENCY_COOPERATIVE_GLOBAL_EXECUTOR
#include "CooperativeGlobalExecutor.inc"
#elif SWIFT_CONCURRENCY_ENABLE_DISPATCH
#include "DispatchGlobalExecutor.inc"
#else
#include "NonDispatchGlobalExecutor.inc"
#endif

void swift::swift_task_enqueueGlobal(Job *job) {
  _swift_tsan_release(job);

  if (swift_task_enqueueGlobal_hook)
    swift_task_enqueueGlobal_hook(job, swift_task_enqueueGlobalImpl);
  else
    swift_task_enqueueGlobalImpl(job);
}

void swift::swift_task_enqueueGlobalWithDelay(JobDelay delay, Job *job) {
  if (swift_task_enqueueGlobalWithDelay_hook)
    swift_task_enqueueGlobalWithDelay_hook(
        delay, job, swift_task_enqueueGlobalWithDelayImpl);
  else
    swift_task_enqueueGlobalWithDelayImpl(delay, job);
}

void swift::swift_task_enqueueMainExecutor(Job *job) {
  if (swift_task_enqueueMainExecutor_hook)
    swift_task_enqueueMainExecutor_hook(job,
                                        swift_task_enqueueMainExecutorImpl);
  else
    swift_task_enqueueMainExecutorImpl(job);
}

ExecutorRef swift::swift_task_getMainExecutor() {
#if !SWIFT_CONCURRENCY_ENABLE_DISPATCH
  // FIXME: this isn't right for the non-cooperative environment
  return ExecutorRef::generic();
#else
  return ExecutorRef::forOrdinary(
           reinterpret_cast<HeapObject*>(&_dispatch_main_q),
           _swift_task_getDispatchQueueSerialExecutorWitnessTable());
#endif
}

bool ExecutorRef::isMainExecutor() const {
#if !SWIFT_CONCURRENCY_ENABLE_DISPATCH
  // FIXME: this isn't right for the non-cooperative environment
  return isGeneric();
#else
  return Identity == reinterpret_cast<HeapObject*>(&_dispatch_main_q);
#endif
}

#define OVERRIDE_GLOBAL_EXECUTOR COMPATIBILITY_OVERRIDE
#include COMPATIBILITY_OVERRIDE_INCLUDE_PATH
