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
///   swift_task_enqueueGlobalWithDeadlineImpl
///   swift_task_enqueueGlobalWithDelayImpl
///   swift_task_enqueueMainExecutorImpl
///   swift_task_getMainExecutorImpl
///   swift_task_isMainExecutorImpl
/// as well as any cooperative-executor-specific functions in the runtime.
///
///===------------------------------------------------------------------===///

#include "swift/shims/Visibility.h"

#include <chrono>
#ifndef SWIFT_THREADING_NONE
# include <thread>
#endif
#include <new>

#include <errno.h>
#include "swift/Basic/PriorityQueue.h"
#include "swift/Runtime/Heap.h"

#if __has_include(<time.h>)
# include <time.h>
#endif
#ifndef NSEC_PER_SEC
# define NSEC_PER_SEC 1000000000ull
#endif

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

using JobDeadline = std::chrono::time_point<std::chrono::steady_clock>;

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

SWIFT_CC(swift)
void swift_task_checkIsolatedImpl(SwiftExecutorRef executor) {
  swift_executor_invokeSwiftCheckIsolated(executor);
}

SWIFT_CC(swift)
int8_t swift_task_isIsolatingCurrentContextImpl(SwiftExecutorRef executor) {
  return swift_executor_invokeSwiftIsIsolatingCurrentContext(executor);
}

/// Insert a job into the cooperative global queue with a delay.
SWIFT_CC(swift)
void swift_task_enqueueGlobalWithDelayImpl(SwiftJobDelay delay,
                                           SwiftJob *newJob) {
  assert(newJob && "no job provided");

  auto deadline = std::chrono::steady_clock::now()
                + std::chrono::duration_cast<JobDeadline::duration>(
                    std::chrono::nanoseconds(delay));
  JobDeadlineStorage<>::set(newJob, deadline);

  insertDelayedJob(newJob, deadline);
}

SWIFT_CC(swift)
void swift_task_enqueueGlobalWithDeadlineImpl(long long sec,
                                              long long nsec,
                                              long long tsec,
                                              long long tnsec,
                                              int clock, SwiftJob *newJob) {
  assert(newJob && "no job provided");

  SwiftTime now = swift_time_now(clock);

  uint64_t delta = (sec - now.seconds) * NSEC_PER_SEC + nsec - now.nanoseconds;

  auto deadline = std::chrono::steady_clock::now()
                + std::chrono::duration_cast<JobDeadline::duration>(
                    std::chrono::nanoseconds(delta));
  JobDeadlineStorage<>::set(newJob, deadline);

  insertDelayedJob(newJob, deadline);
}

/// Recognize jobs in the delayed-jobs queue that are ready to execute
/// and move them to the primary queue.
static void recognizeReadyDelayedJobs() {
  // Process all the delayed jobs.
  auto nextDelayedJob = DelayedJobQueue;
  if (!nextDelayedJob) return;

  auto now = std::chrono::steady_clock::now();

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
#ifdef SWIFT_THREADING_NONE
  auto duration = deadline - std::chrono::steady_clock::now();
  // If the deadline is in the past, don't sleep with invalid negative value
  if (duration <= std::chrono::nanoseconds::zero()) {
    return;
  }
  auto sec = std::chrono::duration_cast<std::chrono::seconds>(duration);
  auto ns = std::chrono::duration_cast<std::chrono::nanoseconds>(duration - sec);

  struct timespec ts;
  ts.tv_sec = sec.count();
  ts.tv_nsec = ns.count();
  while (nanosleep(&ts, &ts) == -1 && errno == EINTR);
#else
  std::this_thread::sleep_until(deadline);
#endif
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

SWIFT_CC(swift) void
swift_task_donateThreadToGlobalExecutorUntilImpl(bool (*condition)(void *),
                                                 void *conditionContext) {
  while (!condition(conditionContext)) {
    auto job = claimNextFromCooperativeGlobalQueue();
    if (!job) return;
    swift_job_run(job, swift_executor_generic());
  }
}

SWIFT_RUNTIME_ATTRIBUTE_NORETURN SWIFT_CC(swift)
void swift_task_asyncMainDrainQueueImpl() {
  while (true) {
    auto job = claimNextFromCooperativeGlobalQueue();
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
