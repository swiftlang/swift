//===--- TaskPrivate.h - Concurrency library internal interface -*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Internal functions for the concurrency library.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_CONCURRENCY_TASKPRIVATE_H
#define SWIFT_CONCURRENCY_TASKPRIVATE_H

#include "swift/Runtime/Concurrency.h"
#include "swift/ABI/Task.h"
#include "swift/ABI/Metadata.h"
#include "swift/Runtime/HeapObject.h"

namespace swift {

class AsyncTask;

/// Initialize the task-local allocator in the given task.
void _swift_task_alloc_initialize(AsyncTask *task);

/// Destroy the task-local allocator in the given task.
void _swift_task_alloc_destroy(AsyncTask *task);

#if defined(SWIFT_STDLIB_SINGLE_THREADED_RUNTIME)
#define SWIFT_CONCURRENCY_COOPERATIVE_GLOBAL_EXECUTOR 1
#elif !__APPLE__
// FIXME: this is a terrible workaround for our temporary
// inability to link libdispatch.
#define SWIFT_CONCURRENCY_COOPERATIVE_GLOBAL_EXECUTOR 1
#else
#define SWIFT_CONCURRENCY_COOPERATIVE_GLOBAL_EXECUTOR 0
#endif

#if SWIFT_CONCURRENCY_COOPERATIVE_GLOBAL_EXECUTOR
/// Donate this thread to the global executor until either the
/// given condition returns true or we've run out of cooperative
/// tasks to run.
void donateThreadToGlobalExecutorUntil(bool (*condition)(void*),
                                       void *context);
#endif

// ==== ------------------------------------------------------------------------
// TODO: this was moved from Task.cpp to share it with TaskGroup.cpp, where else better place to put it?

namespace {

/// An asynchronous context within a task that describes a general "Future".
/// task.
///
/// This type matches the ABI of a function `<T> () async throws -> T`, which
/// is the type used by `Task.runDetached` and `Task.group.add` to create
/// futures.
class TaskFutureWaitAsyncContext : public AsyncContext {
public:
  // Error result is always present.
  SwiftError *errorResult = nullptr;

  // No indirect results.

  TaskFutureWaitResult result;

  // FIXME: Currently, this is always here, but it isn't technically
  // necessary.
  void* Self;

  // Arguments.
  AsyncTask *task;

  using AsyncContext::AsyncContext;
};

}

/// Run the given task, providing it with the result of the future.
static void runTaskWithFutureResult(
    AsyncTask *waitingTask, ExecutorRef executor,
    AsyncTask::FutureFragment *futureFragment, bool hadErrorResult) {
  auto waitingTaskContext =
      static_cast<TaskFutureWaitAsyncContext *>(waitingTask->ResumeContext);

  waitingTaskContext->result.hadErrorResult = hadErrorResult;
  if (hadErrorResult) {
    waitingTaskContext->result.storage =
        reinterpret_cast<OpaqueValue *>(futureFragment->getError());
  } else {
    waitingTaskContext->result.storage = futureFragment->getStoragePtr();
  }

  // TODO: schedule this task on the executor rather than running it directly.
  waitingTask->run(executor);
}

/// Run the given task, providing it with the result of the future.
static void runTaskWithGroupPollResult(
    AsyncTask *waitingTask, ExecutorRef executor,
    AsyncTask::GroupFragment::GroupPollResult result) {
  auto waitingTaskContext =
      static_cast<TaskFutureWaitAsyncContext *>(waitingTask->ResumeContext);

  // Was it an error or successful return?
  waitingTaskContext->result.hadErrorResult =
      result.status == AsyncTask::GroupFragment::GroupPollStatus::Error;

  // Extract the stored value into the waiting task's result storage:
  switch (result.status) {
    case AsyncTask::GroupFragment::GroupPollStatus::Success:
      waitingTaskContext->result.storage = result.storage;
      break;
    case AsyncTask::GroupFragment::GroupPollStatus::Error:
      waitingTaskContext->result.storage =
        reinterpret_cast<OpaqueValue *>(result.storage);
      break;
    case AsyncTask::GroupFragment::GroupPollStatus::Empty:
      // return a `nil` here (as result of the `group.next()`)
      waitingTaskContext->result.storage = nullptr;
      break;
    case AsyncTask::GroupFragment::GroupPollStatus::Waiting:
      assert(false && "Must not attempt to run with a Waiting result.");
  }

  // TODO: schedule this task on the executor rather than running it directly.
  waitingTask->run(executor);

  // TODO: Not entirely sure when to release; we synchronously run the code above so we can't before
  // if we need to, release the now completed task so it can be destroyed
//  if (result.retainedTask) {
//    swift_release(result.retainedTask);
//  }
}

} // end namespace swift

#endif
