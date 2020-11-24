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

  // TODO: schedule this task on the executor rather than running it
  // directly.
  waitingTask->run(executor);
}

} // end namespace swift

#endif
