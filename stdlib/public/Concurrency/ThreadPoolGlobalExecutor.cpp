//===--- ThreadPoolGlobalExecutor.cpp - Embedded thread pool ----*- C++ -*-===//
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
// The executor entrypoints of EmbeddedPlatform.h for
// libswift_ConcurrencyThreadPoolExecutor.a. Main-actor jobs run on the thread
// that runs `async` main; other jobs run on worker threads.
//
//===----------------------------------------------------------------------===//

#include "swift/shims/Visibility.h"

#include "Error.h"
#include "SwiftJobPriorityQueue.h"
#include "swift/EmbeddedPlatform.h"
#include "swift/Runtime/Config.h"
#include "swift/Threading/Once.h"

using namespace swift;

// Defined in ThreadPoolMainExecutor.swift.
extern "C" SWIFT_CC(swift) SwiftExecutorRef
swift_threadPoolGetMainExecutor(void);

namespace {

struct JobQueue {
  void *mutex[EMBEDDED_SWIFT_MUTEX_NUM_WORDS];
  void *count[EMBEDDED_SWIFT_SEMAPHORE_NUM_WORDS];
  SwiftJobPriorityQueue jobs;

  void initialize() {
    _swift_mutex_init(mutex, SWIFT_MUTEX_NONE);
    _swift_sem_init(count, 0);
  }

  void push(SwiftJob *job, bool global) {
    job->schedulerPrivate[1] = global ? job : nullptr;
    _swift_mutex_lock(mutex);
    jobs.enqueue(job);
    _swift_mutex_unlock(mutex);
    _swift_sem_post(count);
  }

  SwiftJob *pop(bool &global) {
    _swift_sem_wait(count);
    _swift_mutex_lock(mutex);
    SwiftJob *job = jobs.dequeue();
    _swift_mutex_unlock(mutex);
    global = job->schedulerPrivate[1] != nullptr;
    return job;
  }
};

JobQueue WorkerQueue;
// Holds global jobs too while the pool has no worker.
JobQueue MainQueue;
swift::once_t QueuesToken;
swift::once_t WorkersToken;
__swift_size_t WorkerCount = 0;

void initializeQueues(void *) {
  WorkerQueue.initialize();
  MainQueue.initialize();
}

void *runWorker(void *) {
  while (true) {
    bool global;
    SwiftJob *job = WorkerQueue.pop(global);
    assert(global && "a main job reached the worker queue");
    swift_job_runSynchronously(job, swift_executor_generic());
  }
}

void startWorkers(void *) {
  __swift_size_t count = _swift_thread_getHardwareConcurrency();
  for (__swift_size_t i = 0; i < count; ++i) {
    if (!_swift_thread_create(runWorker, nullptr))
      break;
    ++WorkerCount;
  }
}

} // end anonymous namespace

void _swift_concurrency_enqueueGlobal(SwiftJob *job) {
  swift::once(QueuesToken, initializeQueues, nullptr);
  swift::once(WorkersToken, startWorkers, nullptr);
  if (WorkerCount == 0)
    MainQueue.push(job, /*global=*/true);
  else
    WorkerQueue.push(job, /*global=*/true);
}

void _swift_concurrency_enqueueMainExecutor(SwiftJob *job) {
  swift::once(QueuesToken, initializeQueues, nullptr);
  MainQueue.push(job, /*global=*/false);
}

extern "C" SWIFT_CC(swift) void swift_threadPoolEnqueueMain(SwiftJob *job) {
  _swift_concurrency_enqueueMainExecutor(job);
}

SwiftExecutorRef _swift_concurrency_getMainExecutor(void) {
  return swift_threadPoolGetMainExecutor();
}

bool _swift_concurrency_isMainExecutor(SwiftExecutorRef executor) {
  return swift_executor_getIdentity(executor) ==
         swift_executor_getIdentity(swift_threadPoolGetMainExecutor());
}

void _swift_concurrency_asyncMainDrainQueue(void) {
  swift::once(QueuesToken, initializeQueues, nullptr);
  SwiftExecutorRef mainExecutor = swift_threadPoolGetMainExecutor();
  while (true) {
    bool global;
    SwiftJob *job = MainQueue.pop(global);
    swift_job_runSynchronously(job, global ? swift_executor_generic()
                                           : mainExecutor);
  }
}

void _swift_concurrency_donateThreadToGlobalExecutorUntil(
    bool (*condition)(void *), void *conditionContext) {
  swift_Concurrency_fatalError(0, "Global executor does not support thread donation\n");
}
