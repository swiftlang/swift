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

#include "swift/Runtime/Concurrency.h"
#include "TaskPrivate.h"

#include <dispatch/dispatch.h>

using namespace swift;

SWIFT_CC(swift)
void (*swift::swift_task_enqueueGlobal_hook)(Job *job) = nullptr;

#if SWIFT_CONCURRENCY_COOPERATIVE_GLOBAL_EXECUTOR
static Job *JobQueue = nullptr;

/// Get the next-in-queue storage slot.
static Job *&nextInQueue(Job *cur) {
  return reinterpret_cast<Job*&>(cur->SchedulerPrivate);
}

/// Insert a job into the cooperative global queue.
static void insertIntoJobQueue(Job *newJob) {
  Job **position = &JobQueue;
  while (auto cur = *position) {
    // If we find a job with lower priority, insert here.
    if (cur->getPriority() < newJob->getPriority()) {
      nextInQueue(newJob) = cur;
      *position = newJob;
      return;
    }

    // Otherwise, keep advancing through the queue.
    position = &nextInQueue(cur);
  }
  nextInQueue(newJob) = nullptr;
  *position = newJob;
}

/// Claim the next job from the cooperative global queue.
static Job *claimNextFromJobQueue() {
  if (auto job = JobQueue) {
    JobQueue = nextInQueue(job);
    return job;
  }
  return nullptr;
}

void swift::donateThreadToGlobalExecutorUntil(bool (*condition)(void *),
                                              void *conditionContext) {
  while (!condition(conditionContext)) {
    auto job = claimNextFromJobQueue();
    if (!job) return;
    job->run(ExecutorRef::generic());
  }
}

#else

/// The function passed to dispatch_async_f to execute a job.
static void __swift_run_job(void *_job) {
  Job *job = (Job*) _job;
  job->run(ExecutorRef::generic());
}
#endif

void swift::swift_task_enqueueGlobal(Job *job) {
  assert(job && "no job provided");

  // If the hook is defined, use it.
  if (swift_task_enqueueGlobal_hook)
    return swift_task_enqueueGlobal_hook(job);

#if SWIFT_CONCURRENCY_COOPERATIVE_GLOBAL_EXECUTOR
  insertIntoJobQueue(job);
#else
  // We really want four things from the global execution service:
  //  - Enqueuing work should have minimal runtime and memory overhead.
  //  - Adding work should never result in an "explosion" where many
  //    more threads are created than the available cores.
  //  - Jobs should run on threads with an appropriate priority.
  //  - Thread priorities should temporarily elevatable to avoid
  //    priority inversions.
  //
  // Of these, the first two are the most important.  Many programs
  // do not rely on high-usage priority scheduling, and many priority
  // inversions can be avoided at a higher level (albeit with some
  // performance cost, e.g. by creating higher-priority tasks to run
  // critical sections that contend with high-priority work).  In
  // contrast, if the async feature adds too much overhead, or if
  // heavy use of it leads to thread explosions and memory exhaustion,
  // programmers will have no choice but to stop using it.  So if
  // goals are in conflict, it's best to focus on core properties over
  // priority-inversion avoidance.

  // We currently use Dispatch for our thread pool on all platforms.
  // Dispatch currently backs its serial queues with a global
  // concurrent queue that is prone to thread explosions when a flood
  // of jobs are added to it.  That problem does not apply equally
  // to the global concurrent queues returned by dispatch_get_global_queue,
  // which are not strictly CPU-limited but are at least much more
  // cautious about adding new threads.  We cannot safely elevate
  // the priorities of work added to this queue using Dispatch's public
  // API, but as discussed above, that is less important than avoiding
  // performance problems.
  dispatch_function_t dispatchFunction = &__swift_run_job;
  void *dispatchContext = job;

  JobPriority priority = job->getPriority();

  // TODO: cache this to avoid the extra call
  auto queue = dispatch_get_global_queue((dispatch_qos_class_t) priority,
                                         /*flags*/ 0);

  dispatch_async_f(queue, dispatchContext, dispatchFunction);
#endif
}
