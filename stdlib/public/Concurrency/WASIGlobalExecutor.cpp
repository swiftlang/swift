//===--- WASIGlobalExecutor.cpp - WebAssembly global executor -------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// The C half of the multithreaded executors for `wasm32-unknown-wasip1-threads`
// (the Swift half is WASIExecutor.swift).
//
// The single-threaded WebAssembly build uses the cooperative executor, which
// drains every job on the thread that started the program: `Task`, `TaskGroup`
// and `async let` therefore never run in parallel. The wasi-threads triple has
// shared memory, atomics and `wasi_thread_spawn`, so here the default executor
// is a pool of worker threads, and the main executor is a serial queue drained
// by the thread that runs the async `main`.
//
// Note that this is the one place where the Concurrency runtime itself spawns
// OS threads: on Darwin/Linux the pool belongs to libdispatch, which the WASI
// SDK does not have. Threads come from `pthread_create` (wasi-libc maps it to
// `wasi_thread_spawn`) with an explicit stack size, and are never joined.
//
// Entry points, all `SWIFT_CC(swift)` and declared in ExecutorBridge.swift:
//
//   * `swift_wasiEnqueueGlobal(job)` — hand a job to the pool.
//   * `swift_wasiEnqueueGlobalWithDelay(sec, nsec, clock, job)` — schedule a
//     job on the pool once a delay (on the given clock) elapses.
//   * `swift_wasiEnqueueMain(job)` — hand a job to the main-thread queue.
//   * `swift_wasiWaitForMainJob()` — block until the main queue has a job and
//     dequeue it; the Swift `WASIMainExecutor.run()` loop runs it with the
//     real main executor reference (executor tracking stays correct).
//   * `swift_wasiIsMainThread()` — the isolation probe behind
//     `WASIMainExecutor.isIsolatingCurrentContext()`.
//
// Both queues are guarded by a `swift::ConditionVariable` (which carries its
// own mutex), so a worker that resumes a `@MainActor` continuation can wake the
// blocked main thread — the cross-thread liveness the cooperative run loop
// cannot provide.
//
//===----------------------------------------------------------------------===//

#include "swift/shims/Visibility.h"

#include <chrono>
#include <climits>

#include <pthread.h>
#include <unistd.h>

#include "swift/Basic/PriorityQueue.h"
#include "swift/Runtime/Concurrency.h"
#include "swift/Runtime/EnvironmentVariables.h"
#include "swift/Threading/ConditionVariable.h"
#include "swift/Threading/Thread.h"

#include "Error.h"
#include "ExecutorImpl.h"

using namespace swift;

namespace {

// Intrusive linkage: reuse the job's first scheduler-private word as the "next"
// pointer, exactly as the cooperative and dispatch executors do.
struct JobQueueTraits {
  static SwiftJob *&storage(SwiftJob *cur) {
    return reinterpret_cast<SwiftJob *&>(cur->schedulerPrivate[0]);
  }

  static SwiftJob *getNext(SwiftJob *job) { return storage(job); }
  static void setNext(SwiftJob *job, SwiftJob *next) { storage(job) = next; }

  enum { prioritiesCount = SwiftJobPriorityBucketCount };
  static int getPriorityIndex(SwiftJob *job) {
    return swift_priority_getBucketIndex(swift_job_getPriority(job));
  }
};
using JobPriorityQueue = PriorityQueue<SwiftJob *, JobQueueTraits>;

/// A deadline expressed as nanoseconds on the suspending clock — the same
/// monotonic timebase `swift_get_time(swift_clock_id_suspending, ...)`
/// reports (matching CooperativeGlobalExecutor.cpp).
using JobDeadline = long long;

static constexpr JobDeadline NSEC_PER_SEC_LL = 1000000000ll;

/// Read the current time on the suspending clock as a nanosecond count.
static JobDeadline currentSuspendingNanos() {
  long long seconds, nanoseconds;
  swift_get_time(&seconds, &nanoseconds, swift_clock_id_suspending);
  return seconds * NSEC_PER_SEC_LL + nanoseconds;
}

/// `base + delta`, saturating instead of wrapping (a delay of centuries must
/// stay in the future, not fire immediately).
static JobDeadline saturatingAdd(JobDeadline base, JobDeadline delta) {
  if (delta > 0 && base > LLONG_MAX - delta)
    return LLONG_MAX;
  if (delta < 0 && base < LLONG_MIN - delta)
    return LLONG_MIN;
  return base + delta;
}

/// The deadline of a delayed job lives in its second scheduler-private word.
/// On wasm32 a pointer is 4 bytes while `JobDeadline` is 8, so it does not
/// fit inline and must be heap-indirected; the two specializations pick the
/// right strategy at compile time (matching CooperativeGlobalExecutor.cpp).
template <bool = (sizeof(JobDeadline) <= sizeof(void *) &&
                  alignof(JobDeadline) <= alignof(void *))>
struct JobDeadlineStorage;

/// The deadline fits in schedulerPrivate.
template <>
struct JobDeadlineStorage<true> {
  static JobDeadline &storage(SwiftJob *job) {
    return reinterpret_cast<JobDeadline &>(job->schedulerPrivate[1]);
  }
  static JobDeadline get(SwiftJob *job) { return storage(job); }
  static void set(SwiftJob *job, JobDeadline deadline) {
    storage(job) = deadline;
  }
  static void destroy(SwiftJob *) {}
};

/// The deadline does not fit in schedulerPrivate; store it out of line.
template <>
struct JobDeadlineStorage<false> {
  static JobDeadline *&storage(SwiftJob *job) {
    return reinterpret_cast<JobDeadline *&>(job->schedulerPrivate[1]);
  }
  static JobDeadline get(SwiftJob *job) { return *storage(job); }
  static void set(SwiftJob *job, JobDeadline deadline) {
    storage(job) = swift_cxx_newObject<JobDeadline>(deadline);
  }
  static void destroy(SwiftJob *job) { swift_cxx_deleteObject(storage(job)); }
};

/// Stack size for the pool's worker threads. wasi-libc's default pthread
/// stack is small (musl-derived), and wasm has no guard page: overflowing an
/// auxiliary thread's stack silently corrupts linear memory instead of
/// trapping. Swift async frames plus generic-metadata instantiation need
/// room, so ask for a few MiB — wasi-libc honours `pthread_attr_setstacksize`.
static constexpr size_t WorkerStackSize = 4 * 1024 * 1024;

/// Upper bound on the pool: each worker owns a stack and TLS block in linear
/// memory, spawned eagerly on the first enqueue, so a host that reports many
/// cores must not turn into that many wasm threads.
static constexpr unsigned MaxWorkerCount = 16;

/// The pool size: `SWIFT_WASI_EXECUTOR_THREADS` when set (>0); otherwise the
/// online CPU count when wasi-libc reports a real one, else 4. (wasi-libc does
/// implement `sysconf(_SC_NPROCESSORS_ONLN)`, but it returns a fixed value —
/// WASI has no CPU-count API to back it — so 0/1 is treated as "unknown".)
static unsigned workerCount() {
#if SWIFT_STDLIB_HAS_ENVIRON
  unsigned configured = runtime::environment::SWIFT_WASI_EXECUTOR_THREADS();
  if (configured > 0)
    return configured > MaxWorkerCount ? MaxWorkerCount : configured;
#endif
  long online = sysconf(_SC_NPROCESSORS_ONLN);
  unsigned n = online > 1 ? static_cast<unsigned>(online) : 4;
  return n > MaxWorkerCount ? MaxWorkerCount : n;
}

/// The default (global) executor: worker threads draining a shared, priority-
/// ordered ready queue, plus a deadline-ordered list of delayed jobs.
class WASIThreadPool {
  ConditionVariable Ready;     // guards everything below
  JobPriorityQueue ReadyQueue;
  SwiftJob *DelayedQueue = nullptr; // singly linked, earliest deadline first
  bool Started = false;

  void insertDelayed(SwiftJob *newJob, JobDeadline deadline) {
    SwiftJob **position = &DelayedQueue;
    while (auto cur = *position) {
      if (deadline < JobDeadlineStorage<>::get(cur)) {
        JobQueueTraits::setNext(newJob, cur);
        *position = newJob;
        return;
      }
      position = &JobQueueTraits::storage(cur);
    }
    JobQueueTraits::setNext(newJob, nullptr);
    *position = newJob;
  }

  /// Move every delayed job whose deadline has passed into the ready queue.
  /// Returns true (and the next unexpired deadline) if delayed jobs remain.
  bool promoteReadyDelayedJobs(JobDeadline &nextDeadline) {
    auto now = currentSuspendingNanos();
    while (DelayedQueue && JobDeadlineStorage<>::get(DelayedQueue) <= now) {
      auto job = DelayedQueue;
      DelayedQueue = JobQueueTraits::getNext(job);
      JobDeadlineStorage<>::destroy(job);
      ReadyQueue.enqueue(job);
    }
    if (DelayedQueue) {
      nextDeadline = JobDeadlineStorage<>::get(DelayedQueue);
      return true;
    }
    return false;
  }

  void workerLoop() {
    Ready.lock();
    while (true) {
      JobDeadline nextDeadline;
      bool haveDelayed = promoteReadyDelayedJobs(nextDeadline);

      if (auto job = ReadyQueue.dequeue()) {
        Ready.unlock();
        // Pool jobs run under the generic executor, exactly as the Dispatch
        // global queue's do; the main queue is what carries a real executor.
        swift_job_run(job, swift_executor_generic());
        Ready.lock();
        continue;
      }

      if (haveDelayed) {
        auto remaining = nextDeadline - currentSuspendingNanos();
        if (remaining > 0)
          Ready.wait(std::chrono::nanoseconds(remaining));
      } else {
        Ready.wait();
      }
    }
  }

  static void *workerEntry(void *self) {
    static_cast<WASIThreadPool *>(self)->workerLoop();
    return nullptr;
  }

  /// Spawn the workers (once, on the first enqueue). Precondition: locked.
  void startIfNeeded() {
    if (Started)
      return;
    Started = true;

    pthread_attr_t attr;
    pthread_attr_init(&attr);
    pthread_attr_setstacksize(&attr, WorkerStackSize);
    // Never joined: the workers are process-lifetime daemons.
    pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);

    unsigned n = workerCount();
    for (unsigned i = 0; i < n; ++i) {
      pthread_t thread;
      int error = pthread_create(&thread, &attr, workerEntry, this);
      if (error != 0) {
        // The host refused another thread (thread limit, out of memory). At
        // least one worker is needed for any job to ever run; beyond that,
        // run with the smaller pool.
        if (i == 0)
          swift_Concurrency_fatalError(
              0, "swift_wasiEnqueueGlobal: pthread_create failed (%d): the "
                 "WASI thread pool could not start\n", error);
        break;
      }
    }
    pthread_attr_destroy(&attr);
  }

public:
  void enqueue(SwiftJob *job) {
    Ready.lock();
    startIfNeeded();
    ReadyQueue.enqueue(job);
    Ready.unlock();
    Ready.signal();
  }

  void enqueueAfter(SwiftJob *job, JobDeadline deadline) {
    Ready.lock();
    startIfNeeded();
    JobDeadlineStorage<>::set(job, deadline);
    insertDelayed(job, deadline);
    Ready.unlock();
    // A new, possibly-earlier deadline changes how long a worker may sleep.
    Ready.broadcast();
  }
};

/// The main executor's queue: serial, drained by the thread that runs the
/// async `main` (see `WASIMainExecutor.run()`). Workers enqueue `@MainActor`
/// continuations here and wake the main thread.
class WASIMainQueue {
  ConditionVariable Ready;
  JobPriorityQueue Queue;

public:
  void enqueue(SwiftJob *job) {
    Ready.lock();
    Queue.enqueue(job);
    Ready.unlock();
    Ready.signal();
  }

  /// Block until a job is available, then hand it over.
  SwiftJob *waitForJob() {
    Ready.lock();
    SwiftJob *job;
    while (!(job = Queue.dequeue()))
      Ready.wait();
    Ready.unlock();
    return job;
  }
};

/// Both are process-lifetime and deliberately leaked: detached workers block
/// on the pool's condition variable, and the runtime's other executors do the
/// same; a static destructor at `exit()` would tear the primitives down under
/// threads that still use them.
WASIThreadPool &threadPool() {
  static WASIThreadPool *pool = new WASIThreadPool();
  return *pool;
}

WASIMainQueue &mainQueue() {
  static WASIMainQueue *queue = new WASIMainQueue();
  return *queue;
}

} // end anonymous namespace

extern "C" SWIFT_CC(swift) void swift_wasiEnqueueGlobal(SwiftJob *job) {
  assert(job && "no job provided");
  threadPool().enqueue(job);
}

extern "C" SWIFT_CC(swift)
void swift_wasiEnqueueGlobalWithDelay(long long sec, long long nsec, int clock,
                                      SwiftJob *job) {
  assert(job && "no job provided");
  // The delay was measured on `clock`; the queue's timebase is the suspending
  // clock. Both are monotonic and advance at the same rate while the program
  // runs, so a delay converts directly (the cooperative executor re-anchors
  // the same way).
  (void)clock;
  JobDeadline delta = sec > LLONG_MAX / NSEC_PER_SEC_LL
                          ? LLONG_MAX
                          : sec * NSEC_PER_SEC_LL;
  delta = saturatingAdd(delta, nsec);
  if (delta < 0)
    delta = 0;
  threadPool().enqueueAfter(job, saturatingAdd(currentSuspendingNanos(), delta));
}

extern "C" SWIFT_CC(swift) void swift_wasiEnqueueMain(SwiftJob *job) {
  assert(job && "no job provided");
  mainQueue().enqueue(job);
}

extern "C" SWIFT_CC(swift) SwiftJob *swift_wasiWaitForMainJob() {
  return mainQueue().waitForJob();
}

extern "C" SWIFT_CC(swift) bool swift_wasiIsMainThread() {
  return Thread::onMainThread();
}
