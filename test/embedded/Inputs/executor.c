/* A custom executor to test that we can build an external executor
   and use it with libswift_Concurrency.a. */

#include <swift/ExecutorImpl.h>

#include <assert.h>
#include <inttypes.h>
#include <stdio.h>
#include <stdarg.h>

#ifndef DEBUG_EXECUTOR
#define DEBUG_EXECUTOR 1
#endif

static void debug(const char *fmt, ...) {
#if DEBUG_EXECUTOR
  va_list val;

  va_start(val, fmt);
  vprintf(fmt, val);
  va_end(val);
#else
  (void)fmt;
#endif
}

// .. A max-heap to hold jobs in priority order ................................

/* This is an explicit binary heap threaded through the schedulerPrivate[]
   fields in the job. */

static SwiftJob *jobHeap = NULL;

static SwiftJob *job_left(SwiftJob *job) {
  return (SwiftJob *)(job->schedulerPrivate[0]);
}
static void job_setLeft(SwiftJob *job, SwiftJob *left) {
  job->schedulerPrivate[0] = left;
}
static SwiftJob *job_right(SwiftJob *job) {
  return (SwiftJob *)(job->schedulerPrivate[1]);
}
static void job_setRight(SwiftJob *job, SwiftJob *right) {
  job->schedulerPrivate[1] = right;
}

static SwiftJob *job_heap_fixup(SwiftJob *job) {
  SwiftJob *left = job_left(job);
  SwiftJob *right = job_right(job);

  SwiftJobPriority priority = swift_job_getPriority(job);
  SwiftJobPriority leftPriority = left ? swift_job_getPriority(left) : 0;
  SwiftJobPriority rightPriority = right ? swift_job_getPriority(right) : 0;

  SwiftJob *temp;

  if (left && leftPriority >= rightPriority && priority <= leftPriority) {
    job_setLeft(job, job_left(left));
    job_setRight(job, job_right(left));
    job_setRight(left, right);
    job_setLeft(left, job_heap_fixup(job));
    return left;
  }
  if (right && rightPriority > leftPriority && priority <= rightPriority) {
    job_setLeft(job, job_left(right));
    job_setRight(job, job_right(right));
    job_setLeft(right, left);
    job_setRight(right, job_heap_fixup(job));
    return right;
  }

  return job;
}

static void job_heap_push(SwiftJob *job) {
  job_setLeft(job, NULL);
  job_setRight(job, NULL);

  // If the heap is empty, this job is the top
  if (!jobHeap) {
    jobHeap = job;
    return;
  }

  // Otherwise, make the existing top node a child of this one, then fix the
  // heap condition.
  SwiftJobPriority jobPriority = swift_job_getPriority(job);

  job_setLeft(job, jobHeap);
  jobHeap = job_heap_fixup(job);
}

static SwiftJob *job_heap_pop(void) {
  if (!jobHeap)
    return NULL;

  SwiftJob *job = jobHeap;

  // The easy case: job has at most one child
  if (!job_left(job)) {
    jobHeap = job_right(job);
    return job;
  }
  if (!job_right(job)) {
    jobHeap = job_left(job);
    return job;
  }

  // Otherwise, find a job with no children
  SwiftJob *parent = NULL;
  SwiftJob *ptr = job;
  while (job_left(ptr) || job_right(ptr)) {
    parent = ptr;
    if (job_right(ptr))
      ptr = job_right(ptr);
    else
      ptr = job_left(ptr);
  }

  // Move it to the head of the queue
  if (job_right(parent) == ptr)
    job_setRight(parent, NULL);
  else
    job_setLeft(parent, NULL);

  job_setLeft(ptr, job_left(job));
  job_setRight(ptr, job_right(job));

  // And fix the heap condition
  jobHeap = job_heap_fixup(ptr);

  return job;
}

// .. A list of delayed jobs ...................................................

// One for each clock
static SwiftJob *delayQueues[2] = { NULL, NULL };

static SwiftJob *job_next(SwiftJob *job) {
  return (SwiftJob *)(job->schedulerPrivate[0]);
}

static SwiftJob **job_ptrToNext(SwiftJob *job) {
  return (SwiftJob **)&job->schedulerPrivate[0];
}

static void job_setNext(SwiftJob *job, SwiftJob *next) {
  job->schedulerPrivate[0] = next;
}

static uint64_t job_deadline(SwiftJob *job) {
  return (uint64_t)(job->schedulerPrivate[1]);
}

static void job_setDeadline(SwiftJob *job, uint64_t deadline) {
  job->schedulerPrivate[1] = (void *)(uintptr_t)deadline;
}

static void job_schedule(SwiftJob *job, SwiftClockId clock, uint64_t deadline) {
  assert(clock >= 1 && clock <= 2 && "clock out of range");

  job_setDeadline(job, deadline);

  SwiftJob **pos = &delayQueues[clock - 1];
  SwiftJob *ptr;
  while ((ptr = *pos) && deadline >= job_deadline(ptr)) {
    pos = job_ptrToNext(ptr);
  }

  job_setNext(job, ptr);
  *pos = job;
}

static uint64_t job_getTime(SwiftClockId clock) {
  SwiftTime now = swift_time_now(clock);
  return now.seconds * 1000000000ull + now.nanoseconds;
}

static void job_runTimers(void) {
  for (int clock = 0; clock < 2; ++clock) {
    uint64_t flatNow = job_getTime((SwiftClockId)(clock + 1));

    SwiftJob *job = delayQueues[clock];
    while (job && flatNow >= job_deadline(job)) {
      SwiftJob *next = job_next(job);
      debug("executor: job %d is ready\n", job);
      job_heap_push(job);
      job = next;
    }
    delayQueues[clock] = job;
  }
}

static bool job_haveDelayedJobs(void) {
  for (int clock = 0; clock < 2; ++clock) {
    if (delayQueues[clock])
      return true;
  }
  return false;
}

// Sleep for a specified number of nanoseconds
static void job_sleep(uint64_t ns) {
  if (ns == 0)
    return;

  debug("executor: sleeping for %"PRIu64" ns\n", ns);

  // We don't know how to do this "properly", so spin instead
  uint64_t flatNow = job_getTime(SwiftContinuousClock);
  uint64_t deadline = flatNow + ns;

  while (flatNow < deadline)
    flatNow = job_getTime(SwiftContinuousClock);
}

// Wait until the next timer is about to fire
static void job_wait(void) {
  uint64_t toSleep = ~(uint64_t)0;

  for (int clock = 0; clock < 2; ++clock) {
    uint64_t flatNow = job_getTime((SwiftClockId)(clock + 1));

    if (delayQueues[clock]) {
      uint64_t deadline = job_deadline(delayQueues[clock]);
      uint64_t delay = deadline - flatNow;
      if (delay < toSleep)
        toSleep = delay;
    }
  }

  job_sleep(toSleep);
}

// .. Main loop ................................................................

static SwiftJob *job_getNextJob() {
  while (true) {
    job_runTimers();

    SwiftJob *job = job_heap_pop();
    if (job) {
      return job;
    }

    if (!job_haveDelayedJobs())
      return NULL;

    job_wait();
  }

  return NULL;
}

// .. Interface functions ......................................................

/// Enqueue a job on the global executor.
SWIFT_CC(swift) void
swift_task_enqueueGlobalImpl(SwiftJob *job) {
  debug("executor: job %p enqueued\n", job);

  job_heap_push(job);
}

/// Enqueue a job on the global executor, with a specific delay before it
/// should execute.
SWIFT_CC(swift) void
swift_task_enqueueGlobalWithDelayImpl(SwiftJobDelay delay,
                                      SwiftJob *job) {
  SwiftTime now = swift_time_now(SwiftContinuousClock);
  uint64_t deadline = now.seconds * 1000000000ull + now.nanoseconds + delay;

  debug("executor: job %p scheduled with delay %llu ns\n", job, delay);

  job_schedule(job, SwiftContinuousClock, deadline);
}

/// Enqueue a job on the global executor, with a specific deadline before
/// which it must execute.
SWIFT_CC(swift)
void swift_task_enqueueGlobalWithDeadlineImpl(long long sec,
                                              long long nsec,
                                              long long tsec,
                                              long long tnsec,
                                              int clock,
                                              SwiftJob *job) {
  uint64_t deadline = sec * 1000000000ull + nsec;

  debug("executor: job %p scheduled with deadline %"PRIu64" on clock %d\n",
         job, deadline, clock);

  job_schedule(job, clock, deadline);
}

/// Enqueue a job on the main executor (which may or may not be the same as
/// the global executor).
SWIFT_CC(swift)
void swift_task_enqueueMainExecutorImpl(SwiftJob *job) {
  swift_task_enqueueGlobalImpl(job);
}

/// Assert that the specified executor is the current executor.
SWIFT_CC(swift)
void swift_task_checkIsolatedImpl(SwiftExecutorRef executor) {
  swift_executor_invokeSwiftCheckIsolated(executor);
}

/// Check if the specified executor is the current executor.
SWIFT_CC(swift)
int8_t swift_task_isIsolatingCurrentContextImpl(SwiftExecutorRef executor) {
  return swift_executor_invokeSwiftIsIsolatingCurrentContext(executor);
}

/// Get a reference to the main executor.
SWIFT_CC(swift)
SwiftExecutorRef swift_task_getMainExecutorImpl() {
  return swift_executor_generic();
}

/// Check if the specified executor is the main executor.
SWIFT_CC(swift)
bool swift_task_isMainExecutorImpl(SwiftExecutorRef executor) {
  return swift_executor_isGeneric(executor);
}

/// Drain the main executor's queue, processing jobs enqueued on it; this
/// should never return.
SWIFT_RUNTIME_ATTRIBUTE_NORETURN SWIFT_CC(swift) void
swift_task_asyncMainDrainQueueImpl() {
  debug("executor: running\n");
  while (true) {
    SwiftJob *job = job_getNextJob();
    assert(job && "We shouldn't run out of jobs here.");
    debug("executor: job %p running\n", job);
    swift_job_run(job, swift_executor_generic());
  }
}

/// Hand control of the current thread to the global executor until the
/// condition function returns `true`.  Support for this function is optional,
/// but you should assert or provide a dummy implementation if your executor
/// does not support it.
SWIFT_CC(swift) void
swift_task_donateThreadToGlobalExecutorUntilImpl(bool (*condition)(void *),
                                                 void *conditionContext) {
  debug("executor: running until condition\n");
  while (!condition(conditionContext)) {
    SwiftJob *job = job_getNextJob();
    if (!job)
      return;
    debug("executor: job %p running\n", job);
    swift_job_run(job, swift_executor_generic());
  }
  debug("executor: condition satisfied or no more jobs\n");
}

