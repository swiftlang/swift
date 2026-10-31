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
/// It defines the executor entrypoints declared in EmbeddedPlatform.h and
/// ExecutorImpl.h.
///
///===------------------------------------------------------------------===///

#include "swift/shims/Visibility.h"

#include "swift/Basic/PriorityQueue.h"
#include "swift/EmbeddedPlatform.h"

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
    return swift_priority_getBucketIndex(swift_job_getPriorityFromFlags(job));
  }
};
using JobPriorityQueue = PriorityQueue<SwiftJob*, JobQueueTraits>;

} // end anonymous namespace

static JobPriorityQueue JobQueue;

/// Insert a job into the cooperative global queue.
void _swift_concurrency_enqueueGlobal(SwiftJob *job) {
  assert(job && "no job provided");
  JobQueue.enqueue(job);
}

/// Enqueues a task on the main executor.
void _swift_concurrency_enqueueMainExecutor(SwiftJob *job) {
  // The cooperative executor does not distinguish between the main
  // queue and the global queue.
  _swift_concurrency_enqueueGlobal(job);
}

void _swift_concurrency_donateThreadToGlobalExecutorUntil(
    bool (*condition)(void *), void *conditionContext) {
  while (!condition(conditionContext)) {
    auto job = JobQueue.dequeue();
    if (!job) return;
    swift_job_runSynchronously(job, swift_executor_generic());
  }
}

void _swift_concurrency_asyncMainDrainQueue(void) {
  while (true) {
    auto job = JobQueue.dequeue();
    assert(job && "We should never run out of jobs on the async main queue.");
    swift_job_runSynchronously(job, swift_executor_generic());
  }
}

SwiftExecutorRef _swift_concurrency_getMainExecutor(void) {
  return swift_executor_generic();
}

bool _swift_concurrency_isMainExecutor(SwiftExecutorRef executor) {
  return swift_executor_isGeneric(executor);
}
