//===--- Task.cpp - Task object and management ----------------------------===//
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
// Object management routines for asynchronous task objects.
//
//===----------------------------------------------------------------------===//

#include "swift/Runtime/Concurrency.h"
#include "swift/ABI/Task.h"
#include "swift/ABI/Metadata.h"
#include "swift/Runtime/HeapObject.h"
#include "TaskPrivate.h"

using namespace swift;

SWIFT_CC(swift)
static void destroySimpleTask(SWIFT_CONTEXT HeapObject *_obj) {
  auto obj = static_cast<AsyncTask*>(_obj);

  assert(!obj->isFuture());

  // The task execution itself should always hold a reference to it, so
  // if we get here, we know the task has finished running, which means
  // swift_task_complete should have been run, which will have torn down
  // the task-local allocator.  There's actually nothing else to clean up
  // here.

  free(obj);
}

/// Heap metadata for a simple asynchronous task that does not
/// include a future.
static FullMetadata<HeapMetadata> simpleTaskHeapMetadata = {
  {
    {
      &destroySimpleTask
    },
    {
      /*value witness table*/ nullptr
    }
  },
  {
    MetadataKind::SimpleTask
  }
};

/// The function that we put in the context of a simple task
/// (one with no future) to handle the final return.
SWIFT_CC(swift)
static void completeTask(AsyncTask *task, ExecutorRef executor,
                         AsyncContext *context) {
  // Tear down the task-local allocator immediately; there's no need
  // to wait for the object to be destroyed.
  _swift_task_alloc_destroy(task);

  // TODO: set something in the status?
  // TODO: notify the parent somehow?
  // TODO: remove this task from the child-task chain?
  // TODO: notify tasks waiting on the future?

  // Release the task, balancing the retain that a running task
  // has on itself.
  swift_release(task);
}

AsyncTaskAndContext
swift::swift_task_create(JobFlags flags, AsyncTask *parent,
                         const AsyncFunctionPointer<void()> *function) {
  return swift_task_create_f(flags, parent, function->Function.get(),
                             function->ExpectedContextSize);
}

AsyncTaskAndContext
swift::swift_task_create_f(JobFlags flags, AsyncTask *parent,
                           AsyncFunctionType<void()> *function,
                           size_t initialContextSize) {
  assert(!flags.task_isFuture() && "function doesn't support creating futures");
  assert((parent != nullptr) == flags.task_isChildTask());

  // Figure out the size of the header.
  size_t headerSize = sizeof(AsyncTask);
  if (parent) headerSize += sizeof(AsyncTask::ChildFragment);

  // Allocate the initial context together with the job.
  // This means that we never get rid of this allocation.
  size_t amountToAllocate = headerSize + initialContextSize;

  assert(amountToAllocate % MaximumAlignment == 0);

  void *allocation = malloc(amountToAllocate);

  AsyncContext *initialContext =
    reinterpret_cast<AsyncContext*>(
      reinterpret_cast<char*>(allocation) + headerSize);

  // Initialize the task so that resuming it will run the given
  // function on the initial context.
  AsyncTask *task =
    new(allocation) AsyncTask(&simpleTaskHeapMetadata, flags,
                              function, initialContext);

  // Initialize the child fragment if applicable.
  // TODO: propagate information from the parent?
  if (parent) {
    auto childFragment = task->childFragment();
    new (childFragment) AsyncTask::ChildFragment(parent);
  }

  // Configure the initial context.
  //
  // FIXME: if we store a null pointer here using the standard ABI for
  // signed null pointers, then we'll have to authenticate context pointers
  // as if they might be null, even though the only time they ever might
  // be is the final hop.  Store a signed null instead.
  initialContext->Parent = nullptr;
  initialContext->ResumeParent = &completeTask;
  initialContext->ResumeParentExecutor = ExecutorRef::noPreference();
  initialContext->Flags = AsyncContextKind::Ordinary;
  initialContext->Flags.setShouldNotDeallocateInCallee(true);

  // Initialize the task-local allocator.
  _swift_task_alloc_initialize(task);

  return {task, initialContext};
}
