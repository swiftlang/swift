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
/// It defines the executor entrypoints declared in EmbeddedPlatform.h.
///
///===------------------------------------------------------------------===///

#include "swift/shims/Visibility.h"

#include "SwiftJobPriorityQueue.h"
#include "swift/EmbeddedPlatform.h"

using namespace swift;

static SwiftJobPriorityQueue JobQueue;

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
