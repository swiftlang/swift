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

#include <stdlib.h>

using namespace swift;

namespace {

struct GlobalAllocator {
  TaskAllocator allocator;
  void *spaceForFirstSlab[64];

  GlobalAllocator() : allocator(spaceForFirstSlab, sizeof(spaceForFirstSlab)) {}
};

} // end anonymous namespace

static TaskAllocator &allocator(AsyncTask *task) {
  if (task)
    return task->Private.get().Allocator;

#if !SWIFT_CONCURRENCY_EMBEDDED
  // FIXME: this fall-back shouldn't be necessary, but it's useful
  // for now, since the current execution tests aren't setting up a task
  // properly.
  static GlobalAllocator global;
  return global.allocator;
#else
  fprintf(stderr, "global allocator fallback not available\n");
  abort();
#endif
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
