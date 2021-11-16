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
///   swift_task_enqueueGlobalImpl
///   swift_task_enqueueGlobalWithDelayImpl
///   swift_task_enqueueMainExecutorImpl
/// as well as any cooperative-executor-specific functions in the runtime.
///
///===------------------------------------------------------------------===///

#include <chrono>
#include <thread>

static Job *JobQueue = nullptr;

class DelayedJob {
public:
  Job *job;
  unsigned long long when;
  DelayedJob *next;

  DelayedJob(Job *job, unsigned long long when) : job(job), when(when), next(nullptr) {}
};

static DelayedJob *DelayedJobQueue = nullptr;

/// Get the next-in-queue storage slot.
static Job *&nextInQueue(Job *cur) {
  return reinterpret_cast<Job*&>(cur->SchedulerPrivate[Job::NextWaitingTaskIndex]);
}

/// Insert a job into the cooperative global queue.
SWIFT_CC(swift)
static void swift_task_enqueueGlobalImpl(Job *job) {
  assert(job && "no job provided");

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

/// Enqueues a task on the main executor.
SWIFT_CC(swift)
static void swift_task_enqueueMainExecutorImpl(Job *job) {
  // The cooperative executor does not distinguish between the main
  // queue and the global queue.
  swift_task_enqueueGlobalImpl(job);
}

static unsigned long long currentNanos() {
  auto now = std::chrono::steady_clock::now();
  auto nowNanos = std::chrono::time_point_cast<std::chrono::nanoseconds>(now);
  auto value = std::chrono::duration_cast<std::chrono::nanoseconds>(nowNanos.time_since_epoch());
  return value.count();
}

/// Insert a job into the cooperative global queue with a delay.
SWIFT_CC(swift)
static void swift_task_enqueueGlobalWithDelayImpl(JobDelay delay,
                                                  Job *job) {
  assert(job && "no job provided");

  DelayedJob **position = &DelayedJobQueue;
  DelayedJob *newJob = new DelayedJob(job, currentNanos() + delay);

  while (auto cur = *position) {
    // If we find a job with lower priority, insert here.
    if (cur->when > newJob->when) {
      newJob->next = cur;
      *position = newJob;
      return;
    }

    // Otherwise, keep advancing through the queue.
    position = &cur->next;
  }
  *position = newJob;
}

/// Claim the next job from the cooperative global queue.
static Job *claimNextFromCooperativeGlobalQueue() {
  // Check delayed jobs first
  while (true) {
    if (auto delayedJob = DelayedJobQueue) {
      if (delayedJob->when < currentNanos()) {
        DelayedJobQueue = delayedJob->next;
        auto job = delayedJob->job;
        
        delete delayedJob;

        return job;
      }
    }
    if (auto job = JobQueue) {
      JobQueue = nextInQueue(job);
      return job;
    }
    // there are only delayed jobs left, but they are not ready,
    // so we sleep until the first one is
    if (auto delayedJob = DelayedJobQueue) {
      std::this_thread::sleep_for(std::chrono::nanoseconds(delayedJob->when - currentNanos()));
      continue;
    }
    return nullptr;
  }
}

void swift::
swift_task_donateThreadToGlobalExecutorUntil(bool (*condition)(void *),
                                             void *conditionContext) {
  while (!condition(conditionContext)) {
    auto job = claimNextFromCooperativeGlobalQueue();
    if (!job) return;
    swift_job_run(job, ExecutorRef::generic());
  }
}
