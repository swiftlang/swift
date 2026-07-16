//===--- WASIGlobalExecutor.cpp - WebAssembly global executor -------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// A multithreaded global executor for `wasm32-unknown-wasip1-threads`.
//
// The single-threaded WebAssembly build uses the cooperative executor, which
// drains every job on the thread that started the program: `Task`, `TaskGroup`
// and `async let` therefore never run in parallel. On the wasi-threads triple
// (shared memory + atomics + `thread_spawn`) we can do better.
//
// This file provides the C helpers that `PlatformExecutorWASI.swift` calls:
//
//   * `swift_wasiEnqueueGlobal`  - hand a job to a pool of worker threads.
//   * `swift_wasiEnqueueGlobalWithDelay` / `...WithDeadline` - schedule a job
//     to run on the pool once a deadline elapses.
//   * `swift_wasiEnqueueMain`    - hand a job to the main thread's queue.
//   * `swift_wasiDrainMain`      - run the main thread's queue forever (the
//     body of `MainExecutor.run()` for the async `main`).
//
// The main-thread queue and the concurrent pool each guard their job queue
// with a mutex and a condition variable, so a worker thread that resumes a
// `@MainActor` continuation can wake the blocked main thread (the liveness
// property the single-threaded cooperative run loop cannot provide across
// threads).
//
// The design mirrors DispatchGlobalExecutor.cpp (which backs the Swift
// executors on Darwin/Linux with libdispatch); here the backing is raw
// pthreads via the C++ standard threading facilities, which wasi-libc provides
// for the threads triple.
//
//===----------------------------------------------------------------------===//

#include "swift/shims/Visibility.h"

#include <chrono>
#include <condition_variable>
#include <mutex>
#include <thread>
#include <vector>

#include <errno.h>

#include "swift/Basic/PriorityQueue.h"
#include "swift/Runtime/Heap.h"

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

using JobDeadline = std::chrono::time_point<std::chrono::steady_clock>;

// The deadline of a delayed job is stashed in its second scheduler-private
// word. On wasm32 a pointer is 4 bytes while `JobDeadline` (a steady_clock
// time_point) is 8, so it does not fit inline and must be heap-indirected;
// the two specializations pick the right strategy at compile time (matching
// CooperativeGlobalExecutor.cpp).
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
    new (static_cast<void *>(&storage(job))) JobDeadline(deadline);
  }
  static void destroy(SwiftJob *job) { storage(job).~JobDeadline(); }
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

// A concurrent global executor: N worker threads draining a shared, priority-
// ordered ready queue, plus a deadline-ordered list of delayed jobs.
class WASIConcurrentExecutor {
  std::mutex Mutex;
  std::condition_variable Ready;
  JobPriorityQueue ReadyQueue;
  SwiftJob *DelayedQueue = nullptr; // singly linked, earliest deadline first
  std::vector<std::thread> Workers;
  bool Started = false;

  static unsigned workerCount() {
    unsigned n = std::thread::hardware_concurrency();
    return n ? n : 4;
  }

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

  // Move any delayed jobs whose deadline has passed into the ready queue.
  // Returns the next unexpired deadline, if the delayed list is non-empty.
  bool promoteReadyDelayedJobs(JobDeadline &nextDeadline) {
    auto now = std::chrono::steady_clock::now();
    while (DelayedQueue &&
           JobDeadlineStorage<>::get(DelayedQueue) <= now) {
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
    std::unique_lock<std::mutex> lock(Mutex);
    while (true) {
      JobDeadline nextDeadline;
      bool haveDelayed = promoteReadyDelayedJobs(nextDeadline);

      if (auto job = ReadyQueue.dequeue()) {
        lock.unlock();
        swift_job_run(job, swift_executor_generic());
        lock.lock();
        continue;
      }

      if (haveDelayed) {
        Ready.wait_until(lock, nextDeadline);
      } else {
        Ready.wait(lock);
      }
    }
  }

  void startIfNeeded() {
    if (Started)
      return;
    Started = true;
    unsigned n = workerCount();
    Workers.reserve(n);
    for (unsigned i = 0; i < n; ++i)
      Workers.emplace_back([this] { workerLoop(); });
  }

public:
  void enqueue(SwiftJob *job) {
    std::lock_guard<std::mutex> lock(Mutex);
    startIfNeeded();
    ReadyQueue.enqueue(job);
    Ready.notify_one();
  }

  void enqueueAfter(SwiftJob *job, JobDeadline deadline) {
    std::lock_guard<std::mutex> lock(Mutex);
    startIfNeeded();
    JobDeadlineStorage<>::set(job, deadline);
    insertDelayed(job, deadline);
    // A new, possibly-earlier deadline changes how long a worker should sleep.
    Ready.notify_all();
  }
};

// The main-thread executor: a single serial queue drained by the thread that
// runs the async `main`. Worker threads enqueue `@MainActor` continuations here
// and wake the main thread through `MainReady`.
class WASIMainExecutor {
  std::mutex Mutex;
  std::condition_variable MainReady;
  JobPriorityQueue Queue;

public:
  void enqueue(SwiftJob *job) {
    std::lock_guard<std::mutex> lock(Mutex);
    Queue.enqueue(job);
    MainReady.notify_one();
  }

  SWIFT_RUNTIME_ATTRIBUTE_NORETURN void drain() {
    std::unique_lock<std::mutex> lock(Mutex);
    while (true) {
      if (auto job = Queue.dequeue()) {
        lock.unlock();
        swift_job_run(job, swift_executor_generic());
        lock.lock();
        continue;
      }
      MainReady.wait(lock);
    }
  }
};

WASIConcurrentExecutor GlobalExecutor;
WASIMainExecutor MainExecutor;

JobDeadline deadlineFromNanoseconds(long long nanoseconds) {
  if (nanoseconds < 0)
    nanoseconds = 0;
  return std::chrono::steady_clock::now() +
         std::chrono::duration_cast<JobDeadline::duration>(
             std::chrono::nanoseconds(nanoseconds));
}

} // end anonymous namespace

extern "C" SWIFT_CC(swift) void swift_wasiEnqueueGlobal(SwiftJob *job) {
  assert(job && "no job provided");
  GlobalExecutor.enqueue(job);
}

extern "C" SWIFT_CC(swift)
void swift_wasiEnqueueGlobalWithDelay(long long delayNanoseconds,
                                      SwiftJob *job) {
  assert(job && "no job provided");
  GlobalExecutor.enqueueAfter(job, deadlineFromNanoseconds(delayNanoseconds));
}

extern "C" SWIFT_CC(swift)
void swift_wasiEnqueueGlobalWithDeadline(long long sec, long long nsec,
                                         long long /*tsec*/,
                                         long long /*tnsec*/, int clock,
                                         SwiftJob *job) {
  assert(job && "no job provided");
  SwiftTime now = swift_time_now(clock);
  long long delta =
      (sec - now.seconds) * 1000000000ll + nsec - now.nanoseconds;
  GlobalExecutor.enqueueAfter(job, deadlineFromNanoseconds(delta));
}

extern "C" SWIFT_CC(swift) void swift_wasiEnqueueMain(SwiftJob *job) {
  assert(job && "no job provided");
  MainExecutor.enqueue(job);
}

extern "C" SWIFT_RUNTIME_ATTRIBUTE_NORETURN SWIFT_CC(swift)
void swift_wasiDrainMain() {
  MainExecutor.drain();
}
