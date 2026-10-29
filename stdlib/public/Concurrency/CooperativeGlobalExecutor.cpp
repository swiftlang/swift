///===--- CooperativeGlobalExecutor.inc ---------------------*- C++ -*--===///
///
/// This source file is part of the Swift.org open source project
///
/// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
/// Licensed under Apache License v2.0 with Runtime Library Exception
///
/// See https:///swift.org/LICENSE.txt for license information
/// See https:///swift.org/CONTRIBUTORS.txt for the list of Swift project authors
///
///===------------------------------------------------------------------===///
///
/// The implementation of the cooperative global executor.
///
/// This file is included into GlobalExecutor.cpp only when
/// the cooperative global executor is enabled.  It is expected to
/// declare the following functions:
///   swift_task_asyncMainDrainQueueImpl
///   swift_task_checkIsolatedImpl
///   swift_task_donateThreadToGlobalExecutorUntilImpl
///   swift_task_enqueueGlobalImpl
///   swift_task_enqueueMainExecutorImpl
///   swift_task_getMainExecutorImpl
///   swift_task_isMainExecutorImpl
/// as well as any cooperative-executor-specific functions in the runtime.
///
///===------------------------------------------------------------------===///

#include "swift/shims/Visibility.h"

#include "swift/Basic/PriorityQueue.h"

#include "ExecutorImpl.h"

using namespace swift;

namespace {

struct JobQueueTraits {
  static SwiftJob *&storage(SwiftJob *cur) {
    return reinterpret_cast<SwiftJob*&>(cur->schedulerPrivate[0]);
  }

  static SwiftJob *getNext(SwiftJob *job) {
    return storage(job);
  }
  static void setNext(SwiftJob *job, SwiftJob *next) {
    storage(job) = next;
  }
  enum { prioritiesCount = SwiftJobPriorityBucketCount };
  static int getPriorityIndex(SwiftJob *job) {
    return swift_priority_getBucketIndex(swift_job_getPriority(job));
  }
};
using JobPriorityQueue = PriorityQueue<SwiftJob*, JobQueueTraits>;

} // end anonymous namespace

static JobPriorityQueue JobQueue;

/// Insert a job into the cooperative global queue.
SWIFT_CC(swift)
void swift_task_enqueueGlobalImpl(SwiftJob *job) {
  assert(job && "no job provided");
  JobQueue.enqueue(job);
}

/// Enqueues a task on the main executor.
SWIFT_CC(swift)
void swift_task_enqueueMainExecutorImpl(SwiftJob *job) {
  // The cooperative executor does not distinguish between the main
  // queue and the global queue.
  swift_task_enqueueGlobalImpl(job);
}

SWIFT_CC(swift)
void swift_task_checkIsolatedImpl(SwiftExecutorRef executor) {
  swift_executor_invokeSwiftCheckIsolated(executor);
}

SWIFT_CC(swift)
int8_t swift_task_isIsolatingCurrentContextImpl(SwiftExecutorRef executor) {
  return swift_executor_invokeSwiftIsIsolatingCurrentContext(executor);
}

SWIFT_CC(swift) void
swift_task_donateThreadToGlobalExecutorUntilImpl(bool (*condition)(void *),
                                                 void *conditionContext) {
  while (!condition(conditionContext)) {
    auto job = JobQueue.dequeue();
    if (!job) return;
    swift_job_run(job, swift_executor_generic());
  }
}

SWIFT_RUNTIME_ATTRIBUTE_NORETURN SWIFT_CC(swift)
void swift_task_asyncMainDrainQueueImpl() {
  while (true) {
    auto job = JobQueue.dequeue();
    assert(job && "We should never run out of jobs on the async main queue.");
    swift_job_run(job, swift_executor_generic());
  }
}

SWIFT_CC(swift)
SwiftExecutorRef swift_task_getMainExecutorImpl() {
  return swift_executor_generic();
}

SWIFT_CC(swift)
bool swift_task_isMainExecutorImpl(SwiftExecutorRef executor) {
  return swift_executor_isGeneric(executor);
}
