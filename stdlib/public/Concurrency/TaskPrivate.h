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
#include "swift/Runtime/Error.h"

namespace swift {

class AsyncTask;
class TaskGroup;

/// Initialize the task-local allocator in the given task.
void _swift_task_alloc_initialize(AsyncTask *task);

/// Destroy the task-local allocator in the given task.
void _swift_task_alloc_destroy(AsyncTask *task);

/// Given that we've already set the given executor as the active
/// executor, run the given job.  This does additional bookkeeping
/// related to the active task.
void runJobInExecutorContext(Job *job, ExecutorRef executor);

/// Clear the active task reference for the current thread.
void _swift_task_clearCurrent();

#if defined(SWIFT_STDLIB_SINGLE_THREADED_RUNTIME)
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

namespace {

/// An asynchronous context within a task that describes a general "Future".
///
/// This type matches the ABI of a function `<T> () async throws -> T`, which
/// is the type used by `Task.runDetached` and `Task.group.add` to create
/// futures.
class TaskFutureWaitAsyncContext : public AsyncContext {
public:
  // Error result is always present.
  SwiftError **errorResult = nullptr;

  OpaqueValue *successResultPointer;

  // FIXME: Currently, this is always here, but it isn't technically
  // necessary.
  void* Self;

  // Arguments.
  AsyncTask *task;

  // Only in swift_taskGroup_wait_next_throwing.
  TaskGroup *group;
  // Only in swift_taskGroup_wait_next_throwing.
  const Metadata *successType;

  using AsyncContext::AsyncContext;

  void fillWithSuccess(AsyncTask::FutureFragment *future, OpaqueValue *result) {
    fillWithSuccess(future->getStoragePtr(), future->getResultType(), result);
  }
  void fillWithSuccess(OpaqueValue *src, const Metadata *successType,
                       OpaqueValue *result) {
    successType->vw_initializeWithCopy(result, src);
  }

  void fillWithError(AsyncTask::FutureFragment *future) {
    fillWithError(future->getError());
  }
  void fillWithError(SwiftError *error) {
    *errorResult = error;
    swift_errorRetain(error);
  }
};

} // end anonymous namespace

} // end namespace swift

#endif
