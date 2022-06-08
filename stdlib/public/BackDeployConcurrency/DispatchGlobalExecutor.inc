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
///   swift_task_enqueueGlobalImpl
///   swift_task_enqueueGlobalWithDelayImpl
///   swift_task_enqueueMainExecutorImpl
/// as well as any Dispatch-specific functions for the runtime.
///
///===------------------------------------------------------------------===///

#if SWIFT_CONCURRENCY_ENABLE_DISPATCH
#include <dispatch/dispatch.h>

#if !defined(_WIN32)
#include <dlfcn.h>
#endif

#endif

// Ensure that Job's layout is compatible with what Dispatch expects.
// Note: MinimalDispatchObjectHeader just has the fields we care about, it is
// not complete and should not be used for anything other than these asserts.
struct MinimalDispatchObjectHeader {
  const void *VTable;
  int Opaque0;
  int Opaque1;
  void *Linkage;
};
static_assert(
    offsetof(Job, metadata) == offsetof(MinimalDispatchObjectHeader, VTable),
    "Job Metadata field must match location of Dispatch VTable field.");
static_assert(offsetof(Job, SchedulerPrivate[Job::DispatchLinkageIndex]) ==
                  offsetof(MinimalDispatchObjectHeader, Linkage),
              "Dispatch Linkage field must match Job "
              "SchedulerPrivate[DispatchLinkageIndex].");

/// The function passed to dispatch_async_f to execute a job.
static void __swift_run_job(void *_job) {
  Job *job = (Job*) _job;
  auto metadata =
      reinterpret_cast<const DispatchClassMetadata *>(job->metadata);
  metadata->VTableInvoke(job, nullptr, 0);
}

/// The type of a function pointer for enqueueing a Job object onto a dispatch
/// queue.
typedef void (*dispatchEnqueueFuncType)(dispatch_queue_t queue, void *obj,
                                        dispatch_qos_class_t qos);

/// Initialize dispatchEnqueueFunc and then call through to the proper
/// implementation.
static void initializeDispatchEnqueueFunc(dispatch_queue_t queue, void *obj,
                                          dispatch_qos_class_t qos);

/// A function pointer to the function used to enqueue a Job onto a dispatch
/// queue. Initially set to initializeDispatchEnqueueFunc, so that the first
/// call will initialize it. initializeDispatchEnqueueFunc sets it to point
/// either to dispatch_async_swift_job when it's available, otherwise to
/// dispatchEnqueueDispatchAsync.
static std::atomic<dispatchEnqueueFuncType> dispatchEnqueueFunc{
    initializeDispatchEnqueueFunc};

/// A small adapter that dispatches a Job onto a queue using dispatch_async_f.
static void dispatchEnqueueDispatchAsync(dispatch_queue_t queue, void *obj,
                                         dispatch_qos_class_t qos) {
  dispatch_async_f(queue, obj, __swift_run_job);
}

static void initializeDispatchEnqueueFunc(dispatch_queue_t queue, void *obj,
                                          dispatch_qos_class_t qos) {
  dispatchEnqueueFuncType func = nullptr;

  // Always fall back to plain dispatch_async_f on Windows for now, and
  // also for back-deployed concurrency.
#if !defined(_WIN32) && !defined(SWIFT_CONCURRENCY_BACK_DEPLOYMENT)
  if (runtime::environment::concurrencyEnableJobDispatchIntegration())
    func = reinterpret_cast<dispatchEnqueueFuncType>(
        dlsym(RTLD_NEXT, "dispatch_async_swift_job"));
#endif

  if (!func)
    func = dispatchEnqueueDispatchAsync;

  dispatchEnqueueFunc.store(func, std::memory_order_relaxed);

  func(queue, obj, qos);
}

/// Enqueue a Job onto a dispatch queue using dispatchEnqueueFunc.
static void dispatchEnqueue(dispatch_queue_t queue, Job *job,
                            dispatch_qos_class_t qos, void *executorQueue) {
  job->SchedulerPrivate[Job::DispatchQueueIndex] = executorQueue;
  dispatchEnqueueFunc.load(std::memory_order_relaxed)(queue, job, qos);
}

static constexpr size_t globalQueueCacheCount =
    static_cast<size_t>(JobPriority::UserInteractive) + 1;
static std::atomic<dispatch_queue_t> globalQueueCache[globalQueueCacheCount];

#if defined(SWIFT_CONCURRENCY_BACK_DEPLOYMENT) || !defined(__APPLE__)
extern "C" void dispatch_queue_set_width(dispatch_queue_t dq, long width);
#endif

static dispatch_queue_t getGlobalQueue(JobPriority priority) {
  size_t numericPriority = static_cast<size_t>(priority);
  if (numericPriority >= globalQueueCacheCount)
    swift_Concurrency_fatalError(0, "invalid job priority %#zx");

#ifdef SWIFT_CONCURRENCY_BACK_DEPLOYMENT
  std::memory_order loadOrder = std::memory_order_acquire;
#else
  std::memory_order loadOrder = std::memory_order_relaxed;
#endif

  auto *ptr = &globalQueueCache[numericPriority];
  auto queue = ptr->load(loadOrder);
  if (SWIFT_LIKELY(queue))
    return queue;

#if defined(SWIFT_CONCURRENCY_BACK_DEPLOYMENT) || !defined(__APPLE__)
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
  queue = dispatch_get_global_queue((dispatch_qos_class_t)priority,
                                    /*flags*/ 0);

  // Unconditionally store it back in the cache. If we raced with another
  // thread, we'll just overwrite the entry with the same value.
  ptr->store(queue, std::memory_order_relaxed);
#endif

  return queue;
}

SWIFT_CC(swift)
static void swift_task_enqueueGlobalImpl(Job *job) {
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
  JobPriority priority = job->getPriority();

  auto queue = getGlobalQueue(priority);

  dispatchEnqueue(queue, job, (dispatch_qos_class_t)priority,
                  DISPATCH_QUEUE_GLOBAL_EXECUTOR);
}


SWIFT_CC(swift)
static void swift_task_enqueueGlobalWithDelayImpl(JobDelay delay,
                                                  Job *job) {
  assert(job && "no job provided");

  dispatch_function_t dispatchFunction = &__swift_run_job;
  void *dispatchContext = job;

  JobPriority priority = job->getPriority();

  auto queue = getGlobalQueue(priority);

  job->SchedulerPrivate[Job::DispatchQueueIndex] =
      DISPATCH_QUEUE_GLOBAL_EXECUTOR;

  dispatch_time_t when = dispatch_time(DISPATCH_TIME_NOW, delay);
  dispatch_after_f(when, queue, dispatchContext, dispatchFunction);
}

SWIFT_CC(swift)
static void swift_task_enqueueMainExecutorImpl(Job *job) {
  assert(job && "no job provided");

  JobPriority priority = job->getPriority();

  // This is an inline function that compiles down to a pointer to a global.
  auto mainQueue = dispatch_get_main_queue();

  dispatchEnqueue(mainQueue, job, (dispatch_qos_class_t)priority, mainQueue);
}

void swift::swift_task_enqueueOnDispatchQueue(Job *job,
                                              HeapObject *_queue) {
  JobPriority priority = job->getPriority();
  auto queue = reinterpret_cast<dispatch_queue_t>(_queue);
  dispatchEnqueue(queue, job, (dispatch_qos_class_t)priority, queue);
}
