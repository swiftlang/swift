///===--- DispatchGlobalExecutor.inc ------------------------*- C++ -*--===///
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
/// The implementation of the global executor when using Dispatch.
///
/// This file is included into GlobalExecutor.cpp only when Dispatch
/// integration is enabled.  It is expected to define the following
/// functions:
///   swift_task_asyncMainDrainQueueImpl
///   swift_task_checkIsolatedImpl
///   swift_task_donateThreadToGlobalExecutorUntilImpl
///   swift_task_enqueueGlobalImpl
///   swift_task_enqueueGlobalWithDeadlineImpl
///   swift_task_enqueueGlobalWithDelayImpl
///   swift_task_enqueueMainExecutorImpl
///   swift_task_getMainExecutorImpl
///   swift_task_isMainExecutorImpl
/// as well as any Dispatch-specific functions for the runtime.
///
///===------------------------------------------------------------------===///

#include <atomic>
#include <cstddef>

#include "swift/Runtime/Concurrency.h"
#include "swift/Runtime/Heap.h"
#include "swift/Runtime/EnvironmentVariables.h"

#if SWIFT_CONCURRENCY_ENABLE_DISPATCH
#include "swift/Runtime/HeapObject.h"
#include <dispatch/dispatch.h>
#if defined(_WIN32)
#define WIN32_LEAN_AND_MEAN
#define NOMINMAX
#include <Windows.h>
// WORKAROUND #87548
#if defined(dispatch_STATIC)
#pragma comment(lib, "dispatch.lib")
#endif
#endif
#endif

#if defined(__APPLE__)
#if __has_include(<dispatch/private.h>)
#include <dispatch/private.h>
#else
/// Apple's libdispatch always exports this, but the public SDK doesn't declare
/// it.
extern "C" void dispatch_async_swift_job(dispatch_queue_t queue, void *obj,
                                        dispatch_qos_class_t qos);
#endif
#endif

#include "Error.h"
#include "ExecutorImpl.h"
#include "TaskPrivate.h"

#ifndef NSEC_PER_SEC
#define NSEC_PER_SEC 1000000000ull
#endif

using namespace swift;

/// The function passed to dispatch_async_f to execute a job.
static void __swift_run_job(void *_job) {
  SwiftJob *job = (SwiftJob*) _job;
  auto metadata =
      reinterpret_cast<const DispatchClassMetadata *>(job->metadata);
  metadata->VTableInvoke(job, nullptr, 0);
}

/// Enqueue a Job onto a dispatch queue.
static void dispatchEnqueue(dispatch_queue_t queue, SwiftJob *job,
                            dispatch_qos_class_t qos, void *executorQueue) {
  job->schedulerPrivate[Job::DispatchQueueIndex] = executorQueue;
#if defined(__APPLE__)
  // dispatch_async_swift_job is SPI available in macOS 12 and iOS 15, so older
  // deployment targets need the dispatch_async_f fallback below.
  if (__builtin_available(macOS 12.0, iOS 15.0, tvOS 15.0, watchOS 8.0, *)) {
    dispatch_async_swift_job(queue, job, qos);
    return;
  }
#endif
  dispatch_async_f(queue, job, __swift_run_job);
}

static constexpr size_t globalQueueCacheCount =
    static_cast<size_t>(JobPriority::UserInteractive) + 1;
static std::atomic<dispatch_queue_t> globalQueueCache[globalQueueCacheCount];

#if defined(__APPLE__)
static constexpr size_t dispatchQueueCooperativeFlag = 4;
#else
extern "C" void dispatch_queue_set_width(dispatch_queue_t dq, long width);
#endif

static dispatch_queue_t getGlobalQueue(SwiftJobPriority priority) {
  size_t numericPriority = static_cast<size_t>(priority);
  if (numericPriority >= globalQueueCacheCount)
    swift_Concurrency_fatalError(0, "invalid job priority %#zx", numericPriority);

  auto *ptr = &globalQueueCache[numericPriority];
  auto queue = ptr->load(std::memory_order_relaxed);
  if (SWIFT_LIKELY(queue))
    return queue;

#if !defined(__APPLE__)
  const int DISPATCH_QUEUE_WIDTH_MAX_LOGICAL_CPUS = -3;

  // Create a new cooperative concurrent queue and swap it in.
  dispatch_queue_attr_t newQueueAttr = dispatch_queue_attr_make_with_qos_class(
      DISPATCH_QUEUE_CONCURRENT, (dispatch_qos_class_t)priority, 0);
  dispatch_queue_t newQueue = dispatch_queue_create(
      "Swift global concurrent queue", newQueueAttr);
  dispatch_queue_set_width(newQueue, DISPATCH_QUEUE_WIDTH_MAX_LOGICAL_CPUS);

  if (!ptr->compare_exchange_strong(queue, newQueue,
                                    /*success*/ std::memory_order_release,
                                    /*failure*/ std::memory_order_acquire)) {
    dispatch_release(newQueue);
    return queue;
  }

  return newQueue;
#else
  // If we don't have a queue cached for this priority, cache it now. This may
  // race with other threads doing this at the same time for this priority, but
  // that's OK, they'll all end up writing the same value.
  if (runtime::environment::concurrencyEnableCooperativeQueues())
    queue = dispatch_get_global_queue((dispatch_qos_class_t)priority,
                                      dispatchQueueCooperativeFlag);
  // If dispatch doesn't support dispatchQueueCooperativeFlag, it will return
  // NULL. Fall back to a standard global queue.
  if (!queue)
    queue = dispatch_get_global_queue((dispatch_qos_class_t)priority,
                                      /*flags*/ 0);

  // Unconditionally store it back in the cache. If we raced with another
  // thread, we'll just overwrite the entry with the same value.
  ptr->store(queue, std::memory_order_relaxed);
#endif

  return queue;
}

// Get a queue suitable for dispatch_after. Use the cooperative queues on OS
// versions where they work with dispatch_after, and use a standard global
// queue where cooperative queues don't work.
static dispatch_queue_t getTimerQueue(SwiftJobPriority priority) {
  // On newer OSes, we can use the cooperative queues.
  if (__builtin_available(macOS 12.3, iOS 15.4, tvOS 15.4, watchOS 8.5, *))
    return getGlobalQueue(priority);

  // On older OSes, use a standard global queue.
  return dispatch_get_global_queue((dispatch_qos_class_t)priority, /*flags*/ 0);
}

extern "C" SWIFT_CC(swift)
void swift_dispatchEnqueueGlobal(SwiftJob *job) {
  assert(job && "no job provided");
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
  SwiftJobPriority priority = swift_job_getPriority(job);

  auto queue = getGlobalQueue(priority);

  dispatchEnqueue(queue, job, (dispatch_qos_class_t)priority,
                  DISPATCH_QUEUE_GLOBAL_EXECUTOR);
}

#define DISPATCH_UP_OR_MONOTONIC_TIME_MASK  (1ULL << 63)
#define DISPATCH_WALLTIME_MASK  (1ULL << 62)
#define DISPATCH_TIME_MAX_VALUE (DISPATCH_WALLTIME_MASK - 1)

/// State shared between a scheduled timer, its dispatch source's event
/// handler and (optionally) a cancellation token.
///
/// The job is "claimed" by atomically exchanging `job` with null: whoever
/// gets a non-null result owns the job and is responsible for releasing the
/// dispatch source, so exactly one of the timer firing and cancellation
/// can run or take the job.
///
/// This can't live in the job's allocator: it must outlive the job (it's
/// consulted after the job has run, at which point the task may have gone).
///
/// There are two references: one owned by the dispatch source (released by its
/// finalizer) and one owned by the token (released by
/// swift_dispatchReleaseSource).
struct swift_job_source {
  std::atomic<uint32_t> refcount;
  dispatch_source_t source;
  std::atomic<SwiftJob *> job;

  swift_job_source(dispatch_source_t source_, SwiftJob *job_)
    : refcount(2), source(source_), job(job_) {}
};

static void job_source_release(struct swift_job_source *jobSource) {
  if (jobSource->refcount.fetch_sub(1, std::memory_order_acq_rel) == 1)
    swift_cxx_deleteObject(jobSource);
}

/// The dispatch source's finalizer; releases the source's reference.
static void _swift_job_source_finalize(void *context) {
  job_source_release(static_cast<struct swift_job_source *>(context));
}

static void _swift_run_job_timer(struct swift_job_source *jobSource) {
  SwiftJob *job = jobSource->job.exchange(nullptr, std::memory_order_acq_rel);

  // If the job was taken by a cancellation, there is nothing to do; the
  // canceller will have released the source.
  if (!job)
    return;

  dispatch_release(jobSource->source);
  __swift_run_job(job);
}

#if defined(__i386__) || defined(__x86_64__) || !defined(__APPLE__)
#define TIME_UNIT_USES_NANOSECONDS 1
#else
#define TIME_UNIT_USES_NANOSECONDS 0
#endif

#if TIME_UNIT_USES_NANOSECONDS
// x86 currently implements mach time in nanoseconds
// this is NOT likely to change
static inline uint64_t
platform_time(uint64_t nsec) {
  return nsec;
}
#else
#define DISPATCH_USE_HOST_TIME 1
#if defined(__APPLE__)
#if defined(__arm__) || defined(__arm64__)
// Apple arm platforms currently use a fixed mach timebase of 125/3 (24 MHz)
static inline uint64_t
platform_time(uint64_t nsec) {
  if (!nsec) {
    return nsec;
  }
  if (nsec >= (uint64_t)INT64_MAX) {
    return INT64_MAX;
  }
  if (nsec >= UINT64_MAX / 3ull) {
    return (nsec / 125ull) * 3ull;
  } else {
    return (nsec * 3ull) / 125ull;
  }
}
#endif
#endif
#endif

static inline dispatch_time_t
clock_and_value_to_time(int clock, long long sec, long long nsec) {
  uint64_t deadline;
  if (sec < 0 || (sec == 0 && nsec < 0))
    deadline = 0;
  else if (__builtin_mul_overflow(sec, NSEC_PER_SEC, &deadline)
      || __builtin_add_overflow(nsec, deadline, &deadline)) {
    deadline = UINT64_MAX;
  }
  uint64_t value = platform_time((uint64_t)deadline);
  if (value >= DISPATCH_TIME_MAX_VALUE) {
    return DISPATCH_TIME_FOREVER;
  }
  switch (clock) {
  case swift_clock_id_suspending:
    return value;
  case swift_clock_id_continuous:
    return value | DISPATCH_UP_OR_MONOTONIC_TIME_MASK;
  case swift_clock_id_wall: {
    struct timespec ts = {
      .tv_sec = static_cast<decltype(ts.tv_sec)>(sec),
      .tv_nsec = static_cast<decltype(ts.tv_nsec)>(nsec)
    };
    return dispatch_walltime(&ts, 0);
  }
  }
  __builtin_unreachable();
}

static inline uint64_t seconds_nanos_to_nanos(long long sec, long long nsec) {
  uint64_t value;
  if (sec < 0 || (sec == 0 && nsec < 0))
    value = 0;
  else if (__builtin_mul_overflow(sec, NSEC_PER_SEC, &value)
           || __builtin_add_overflow(nsec, value, &value)) {
    value = UINT64_MAX;
  }
  return value;
}

extern "C" SWIFT_CC(swift)
void swift_dispatchEnqueueMain(SwiftJob *job);

extern "C" SWIFT_CC(swift)
uintptr_t swift_dispatchEnqueueWithDeadline(bool global,
                                            long long sec,
                                            long long nsec,
                                            long long tsec,
                                            long long tnsec,
                                            int clock, SwiftJob *job) {
  assert(job && "no job provided");

  SwiftJobPriority priority = swift_job_getPriority(job);

  dispatch_queue_t queue;

  if (global) {
    queue = getTimerQueue(priority);

    job->schedulerPrivate[SwiftJobDispatchQueueIndex] =
      DISPATCH_QUEUE_GLOBAL_EXECUTOR;
  } else {
    queue = dispatch_get_main_queue();

    job->schedulerPrivate[SwiftJobDispatchQueueIndex] = queue;
  }

  dispatch_time_t when = clock_and_value_to_time(clock, sec, nsec);

  // If the deadline has already passed there is nothing to wait for, and
  // creating, activating and firing a dispatch source would cost several
  // times as much as just running the job.  Enqueue it directly and return a
  // null token, which cancels as "already started".
  //
  // We don't do this for deadlines that are merely close: dispatch_after_f()
  // is no cheaper than a source for anything beyond about a microsecond, and
  // its coalescing makes sleeps of tens of microseconds take several times
  // longer than a source does.
  {
    long long nowSec, nowNsec;
    swift_get_time(&nowSec, &nowNsec, (swift_clock_id)clock);

    uint64_t now = seconds_nanos_to_nanos(nowSec, nowNsec);
    uint64_t deadline = seconds_nanos_to_nanos(sec, nsec);

    if (deadline <= now) {
      if (global)
        swift_dispatchEnqueueGlobal(job);
      else
        swift_dispatchEnqueueMain(job);
      return 0;
    }
  }

  uint64_t leeway;
  if (tnsec == -1) {
    // No tolerance was specified.  We used to call dispatch_after() in this
    // case, which sets the leeway according to the priority (5% for
    // interactive and above, 6.7% for default and above, 10% otherwise).  We
    // need a source so that we can cancel, and we have no way to ask Dispatch
    // for its idea of the priority, so use 6.7%.
    long long nowSec, nowNsec;
    swift_get_time(&nowSec, &nowNsec, (swift_clock_id)clock);

    uint64_t now = seconds_nanos_to_nanos(nowSec, nowNsec);
    uint64_t deadline = seconds_nanos_to_nanos(sec, nsec);

    uint64_t delta;
    if (__builtin_sub_overflow(deadline, now, &delta))
      delta = 0;

    leeway = delta / 15;
  } else if (tsec < 0 || (tsec == 0 && tnsec < 0)) {
    leeway = 0;
  } else if (__builtin_mul_overflow(tsec, NSEC_PER_SEC, &leeway)
             || __builtin_add_overflow(tnsec, leeway, &leeway)) {
    leeway = UINT64_MAX;
  }

  dispatch_source_t source =
    dispatch_source_create(DISPATCH_SOURCE_TYPE_TIMER, 0, 0, queue);
  dispatch_source_set_timer(source, when, DISPATCH_TIME_FOREVER, leeway);

  struct swift_job_source *jobSource =
    swift_cxx_newObject<struct swift_job_source>(source, job);

  dispatch_set_context(source, jobSource);
  dispatch_set_finalizer_f(source, &_swift_job_source_finalize);
  dispatch_source_set_event_handler_f(source,
    (dispatch_function_t)&_swift_run_job_timer);

  dispatch_activate(source);

  return reinterpret_cast<uintptr_t>(jobSource);
}

/// Cancel a job scheduled by swift_dispatchEnqueueWithDeadline().
///
/// Returns the job if it had not already been run (or taken), or null.
/// This does not release the caller's reference; call
/// swift_dispatchReleaseSource() for that.
extern "C" SWIFT_CC(swift)
SwiftJob *swift_dispatchCancel(uintptr_t token) {
  if (!token)
    return nullptr;  // The job was enqueued directly
  auto *jobSource = reinterpret_cast<struct swift_job_source *>(token);
  SwiftJob *job = jobSource->job.exchange(nullptr, std::memory_order_acq_rel);

  if (job) {
    dispatch_source_cancel(jobSource->source);
    dispatch_release(jobSource->source);
  }

  return job;
}

/// Release the reference to a scheduled job's state that was returned from
/// swift_dispatchEnqueueWithDeadline().
extern "C" SWIFT_CC(swift)
void swift_dispatchReleaseSource(uintptr_t token) {
  if (!token)
    return;
  job_source_release(reinterpret_cast<struct swift_job_source *>(token));
}

extern "C" SWIFT_CC(swift)
void swift_dispatchEnqueueMain(SwiftJob *job) {
  assert(job && "no job provided");

  SwiftJobPriority priority = swift_job_getPriority(job);

  // This is an inline function that compiles down to a pointer to a global.
  auto mainQueue = dispatch_get_main_queue();

  dispatchEnqueue(mainQueue, job, (dispatch_qos_class_t)priority, mainQueue);
}

void swift::swift_task_enqueueOnDispatchQueue(Job *job,
                                              HeapObject *_queue) {
  JobPriority priority = job->getPriority();
  auto queue = reinterpret_cast<dispatch_queue_t>(_queue);
  dispatchEnqueue(queue, (SwiftJob *)job, (dispatch_qos_class_t)priority, queue);
}
