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
#include "swift/ABI/Task.h"
#include "swift/Runtime/Concurrency.h"
#include "swift/Runtime/Coro.h"

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
  puts("global allocator fallback not available\n");
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

static void *swift_task_alloc_thunk(size_t size) {
  return swift_task_alloc(size);
}

static void swift_task_dealloc_thunk(void *ptr) { swift_task_dealloc(ptr); }

void swift::swift_task_dealloc_through(void *ptr) {
  allocator(swift_task_getCurrent()).deallocThrough(ptr);
}

static CoroAllocator CoroTaskAllocatorImpl{
    CoroAllocatorFlags(/*CoroAllocatorKind::Async*/ 1), swift_task_alloc_thunk,
    swift_task_dealloc_thunk};

CoroAllocator *const swift::_swift_coro_task_allocator = &CoroTaskAllocatorImpl;

CoroAllocator *swift::swift_coro_getGlobalAllocator(CoroAllocatorFlags flags) {
  switch (flags.getKind()) {
  case CoroAllocatorKind::Sync:
    return nullptr;
  case CoroAllocatorKind::Async:
    return _swift_coro_task_allocator;
  case CoroAllocatorKind::Malloc:
    return _swift_coro_malloc_allocator;
  }
}

static CoroAllocator CoroMallocAllocatorImpl{
    CoroAllocatorFlags(/*CoroAllocatorKind::Malloc*/ 2), malloc, free};

CoroAllocator *const swift::_swift_coro_malloc_allocator =
    &CoroMallocAllocatorImpl;

void *swift::swift_coro_alloc(CoroAllocator *allocator, size_t size) {
  return allocator->allocate(size);
}

void swift::swift_coro_dealloc(CoroAllocator *allocator, void *ptr) {
  // Calls to swift_coro_dealloc are emitted in resume funclets for every
  // live-across dynamic allocation.  Whether such calls immediately deallocate
  // memory depends on the allocator.
  if (!allocator->shouldDeallocateImmediately()) {
    return;
  }
  allocator->deallocate(ptr);
}
