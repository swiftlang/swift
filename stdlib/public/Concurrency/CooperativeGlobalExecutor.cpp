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

#include <new>

#include "swift/Basic/PriorityQueue.h"
#include "swift/EmbeddedPlatform.h"
#include "swift/Runtime/Heap.h"

#ifndef NSEC_PER_SEC
# define NSEC_PER_SEC 1000000000ull
#endif

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

/// A deadline expressed as nanoseconds on the suspending clock — the same
/// monotonic timebase `swift_get_time(swift_clock_id_suspending, ...)`
/// reports.
using JobDeadline = long long;

static JobDeadline toNanos(__swift_int64_t seconds,
                           __swift_int64_t nanoseconds) {
  return seconds * static_cast<JobDeadline>(NSEC_PER_SEC) + nanoseconds;
}

/// Read the current time on the suspending clock and return it as a
/// nanosecond count.
static JobDeadline currentSuspendingNanos() {
  __swift_int64_t seconds, nanoseconds;
  _swift_clockSuspending_getTime(&seconds, &nanoseconds);
  return toNanos(seconds, nanoseconds);
}

template <bool = (sizeof(JobDeadline) <= sizeof(void*) &&
                  alignof(JobDeadline) <= alignof(void*))>
struct JobDeadlineStorage;

/// Specialization for when JobDeadline fits in schedulerPrivate.
template <>
struct JobDeadlineStorage<true> {
  static JobDeadline &storage(SwiftJob *job) {
    return reinterpret_cast<JobDeadline&>(job->schedulerPrivate[1]);
  }
  static JobDeadline get(SwiftJob *job) {
    return storage(job);
  }
  static void set(SwiftJob *job, JobDeadline deadline) {
    new(static_cast<void*>(&storage(job))) JobDeadline(deadline);
  }
  static void destroy(SwiftJob *job) {
    storage(job).~JobDeadline();
  }
};

/// Specialization for when JobDeadline doesn't fit in schedulerPrivate.
template <>
struct JobDeadlineStorage<false> {
  static JobDeadline *&storage(SwiftJob *job) {
    return reinterpret_cast<JobDeadline*&>(job->schedulerPrivate[1]);
  }
  static JobDeadline get(SwiftJob *job) {
    return *storage(job);
  }
  static void set(SwiftJob *job, JobDeadline deadline) {
    storage(job) = swift_cxx_newObject<JobDeadline>(deadline);
  }
  static void destroy(SwiftJob *job) {
    swift_cxx_deleteObject(storage(job));
  }
};

} // end anonymous namespace

static JobPriorityQueue JobQueue;
static SwiftJob *DelayedJobQueue = nullptr;

/// Insert a job into the cooperative global queue.
void _swift_task_enqueueGlobalImpl(SwiftJob *job) {
  assert(job && "no job provided");
  JobQueue.enqueue(job);
}

/// Enqueues a task on the main executor.
void _swift_task_enqueueMainExecutorImpl(SwiftJob *job) {
  // The cooperative executor does not distinguish between the main
  // queue and the global queue.
  _swift_task_enqueueGlobalImpl(job);
}

static void insertDelayedJob(SwiftJob *newJob, JobDeadline deadline) {
  SwiftJob **position = &DelayedJobQueue;
  while (auto cur = *position) {
    // If we find a job with a later deadline, insert here.
    // Note that we maintain FIFO order.
    if (deadline < JobDeadlineStorage<>::get(cur)) {
      JobQueueTraits::setNext(newJob, cur);
      *position = newJob;
      return;
    }

    // Otherwise, keep advancing through the queue.
    position = &JobQueueTraits::storage(cur);
  }
  JobQueueTraits::setNext(newJob, nullptr);
  *position = newJob;
}

void _swift_task_checkIsolatedImpl(SwiftExecutorRef executor) {
  swift_executor_invokeSwiftCheckIsolated(executor);
}

int8_t _swift_task_isIsolatingCurrentContextImpl(SwiftExecutorRef executor) {
  return swift_executor_invokeSwiftIsIsolatingCurrentContext(executor);
}

/// Insert a job into the cooperative global queue with a delay.
void _swift_task_enqueueGlobalWithDelayImpl(SwiftJobDelay delay,
                                            SwiftJob *newJob) {
  assert(newJob && "no job provided");

  auto deadline = currentSuspendingNanos() + static_cast<JobDeadline>(delay);
  JobDeadlineStorage<>::set(newJob, deadline);

  insertDelayedJob(newJob, deadline);
}

void _swift_task_enqueueGlobalWithContinuousDeadlineImpl(
    __swift_int64_t sec, __swift_int64_t nsec, __swift_int64_t tsec,
    __swift_int64_t tnsec, SwiftJob *newJob) {
  assert(newJob && "no job provided");

  // Compute how far away the deadline is on the continuous clock, then
  // re-anchor it to our internal suspending-clock timebase.
  __swift_int64_t nowSec, nowNsec;
  _swift_clockContinuous_getTime(&nowSec, &nowNsec);

  auto deadline = currentSuspendingNanos() + toNanos(sec, nsec) -
                  toNanos(nowSec, nowNsec);
  JobDeadlineStorage<>::set(newJob, deadline);

  insertDelayedJob(newJob, deadline);
}

void _swift_task_enqueueGlobalWithSuspendingDeadlineImpl(
    __swift_int64_t sec, __swift_int64_t nsec, __swift_int64_t tsec,
    __swift_int64_t tnsec, SwiftJob *newJob) {
  assert(newJob && "no job provided");

  auto deadline = toNanos(sec, nsec);
  JobDeadlineStorage<>::set(newJob, deadline);

  insertDelayedJob(newJob, deadline);
}

/// Recognize jobs in the delayed-jobs queue that are ready to execute
/// and move them to the primary queue.
static void recognizeReadyDelayedJobs() {
  // Process all the delayed jobs.
  auto nextDelayedJob = DelayedJobQueue;
  if (!nextDelayedJob) return;

  auto now = currentSuspendingNanos();

  // Pull jobs off of the delayed-jobs queue whose deadline has been
  // reached, and add them to the ready queue.
  while (nextDelayedJob &&
         JobDeadlineStorage<>::get(nextDelayedJob) <= now) {
    // Destroy the storage of the deadline in the job.
    JobDeadlineStorage<>::destroy(nextDelayedJob);

    auto next = JobQueueTraits::getNext(nextDelayedJob);
    JobQueue.enqueue(nextDelayedJob);
    nextDelayedJob = next;
  }
  DelayedJobQueue = nextDelayedJob;
}

static void sleepThisThreadUntil(JobDeadline deadline) {
  auto now = currentSuspendingNanos();
  auto remaining = deadline - now;
  // If the deadline is already in the past there's nothing to wait for.
  if (remaining <= 0)
    return;
  _swift_clock_sleep(remaining / static_cast<JobDeadline>(NSEC_PER_SEC),
                     remaining % static_cast<JobDeadline>(NSEC_PER_SEC));
}

/// Claim the next job from the cooperative global queue.
static SwiftJob *claimNextFromCooperativeGlobalQueue() {
  while (true) {
    // Move any delayed jobs that are now ready into the primary queue.
    recognizeReadyDelayedJobs();

    // If there's a job in the primary queue, run it.
    if (auto job = JobQueue.dequeue()) {
      return job;
    }

    // If there are only delayed jobs left, sleep until the next deadline.
    // TODO: should the donator have some say in this?
    if (auto delayedJob = DelayedJobQueue) {
      auto deadline = JobDeadlineStorage<>::get(delayedJob);
      sleepThisThreadUntil(deadline);
      continue;
    }

    return nullptr;
  }
}

void _swift_task_donateThreadToGlobalExecutorUntilImpl(
    bool (*condition)(void *), void *conditionContext) {
  while (!condition(conditionContext)) {
    auto job = claimNextFromCooperativeGlobalQueue();
    if (!job) return;
    swift_job_runSynchronously(job, swift_executor_generic());
  }
}

void _swift_task_asyncMainDrainQueueImpl(void) {
  while (true) {
    auto job = claimNextFromCooperativeGlobalQueue();
    assert(job && "We should never run out of jobs on the async main queue.");
    swift_job_runSynchronously(job, swift_executor_generic());
  }
}

SwiftExecutorRef _swift_task_getMainExecutorImpl(void) {
  return swift_executor_generic();
}

bool _swift_task_isMainExecutorImpl(SwiftExecutorRef executor) {
  return swift_executor_isGeneric(executor);
}
