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
#include <stdlib.h>

using namespace swift;

namespace {

class TaskAllocator {
public:
  void *alloc(size_t size) {
    return malloc(size);
  }

  void dealloc(void *ptr) {
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
  return reinterpret_cast<TaskAllocator &>(task->AllocatorPrivate);
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
