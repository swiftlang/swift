//===--- TaskAlloc.cpp - Task-local stack allocator -----------------------===//
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
// A task-local allocator that obeys a stack discipline.
//
// Because allocation is task-local, and there's at most one thread
// running a task at once, no synchronization is required.
//
//===----------------------------------------------------------------------===//

#include "TaskPrivate.h"
#include "swift/Runtime/Concurrency.h"
#include "swift/Runtime/Debug.h"
#include <stdlib.h>
#include <vector>

using namespace swift;

namespace {

class TaskAllocator {
  // Just keep track of all allocations in a vector so that we can
  // verify stack discipline.  We should make sure the allocator
  // implementation strictly verifies allocation order at least
  // until we've stabilized the compiler implementation.
  std::vector<void*> Allocations;

public:
  void *alloc(size_t size) {
    void *ptr = malloc(size);
    Allocations.push_back(ptr);
    return ptr;
  }

  void dealloc(void *ptr) {
    if (Allocations.empty() || Allocations.back() != ptr)
      fatalError(0, "pointer was not the last allocation on this task");

    Allocations.pop_back();
    free(ptr);
  }
};

static_assert(sizeof(TaskAllocator) <= sizeof(AsyncTask::AllocatorPrivate),
              "task allocator must fit in allocator-private slot");

static_assert(alignof(TaskAllocator) <= alignof(decltype(AsyncTask::AllocatorPrivate)),
              "task allocator must not be more aligned than "
              "allocator-private slot");

} // end anonymous namespace

void swift::_swift_task_alloc_initialize(AsyncTask *task) {
  new (task->AllocatorPrivate) TaskAllocator();
}

static TaskAllocator &allocator(AsyncTask *task) {
  if (task)
    return reinterpret_cast<TaskAllocator &>(task->AllocatorPrivate);

  // FIXME: this fall-back shouldn't be necessary, but it's useful
  // for now, since the current execution tests aren't setting up a task
  // properly.
  static TaskAllocator global;
  return global;
}

void swift::_swift_task_alloc_destroy(AsyncTask *task) {
  allocator(task).~TaskAllocator();
}

void *swift::swift_task_alloc(AsyncTask *task, size_t size) {
  return allocator(task).alloc(size);
}

void swift::swift_task_dealloc(AsyncTask *task, void *ptr) {
  return allocator(task).dealloc(ptr);
}
