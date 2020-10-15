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
#include <stdlib.h>

using namespace swift;

void *swift::swift_task_alloc(AsyncTask *task, size_t size) {
  return malloc(size);
}

void swift::swift_task_dealloc(AsyncTask *task, void *ptr) {
  free(ptr);
}
