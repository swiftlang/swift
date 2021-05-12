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

#include "swift/Runtime/Concurrency.h"
#include "swift/ABI/Task.h"
#include "TaskPrivate.h"
#include "Error.h"

#define SWIFT_FATAL_ERROR swift_Concurrency_fatalError
#include "../runtime/StackAllocator.h"
#include <stdlib.h>

using namespace swift;

namespace {

/// The size of an allocator slab.
///
/// TODO: find the optimal value by experiment.
static constexpr size_t SlabCapacity = 1024;

using TaskAllocator = StackAllocator<SlabCapacity>;

struct GlobalAllocator {
  TaskAllocator allocator;
  void *spaceForFirstSlab[64];

  GlobalAllocator() : allocator(spaceForFirstSlab, sizeof(spaceForFirstSlab)) {}
};

static_assert(alignof(TaskAllocator) <= alignof(decltype(AsyncTask::AllocatorPrivate)),
              "task allocator must not be more aligned than "
              "allocator-private slot");

} // end anonymous namespace

void swift::_swift_task_alloc_initialize(AsyncTask *task) {
  new (task->AllocatorPrivate) TaskAllocator();
}

void swift::_swift_task_alloc_initialize_with_slab(AsyncTask *task,
                                                   void *firstSlabBuffer,
                                                   size_t bufferCapacity) {
  new (task->AllocatorPrivate) TaskAllocator(firstSlabBuffer, bufferCapacity);
}

static TaskAllocator &allocator(AsyncTask *task) {
  if (task)
    return reinterpret_cast<TaskAllocator &>(task->AllocatorPrivate);

  // FIXME: this fall-back shouldn't be necessary, but it's useful
  // for now, since the current execution tests aren't setting up a task
  // properly.
  static GlobalAllocator global;
  return global.allocator;
}

void swift::_swift_task_alloc_destroy(AsyncTask *task) {
  allocator(task).~TaskAllocator();
}

void *swift::swift_task_alloc(size_t size) {
  return allocator(swift_task_getCurrent()).alloc(size);
}

void *swift::_swift_task_alloc_specific(AsyncTask *task, size_t size) {
  return allocator(task).alloc(size);
}

void swift::swift_task_dealloc(void *ptr) {
  allocator(swift_task_getCurrent()).dealloc(ptr);
}

void swift::_swift_task_dealloc_specific(AsyncTask *task, void *ptr) {
  allocator(task).dealloc(ptr);
}
