//===--- TaskRegistry.cpp - Global live-task registry --------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "TaskRegistry.h"
#include "TaskPrivate.h"
#include "swift/Threading/Mutex.h"
#include <cstdlib>

#if !SWIFT_CONCURRENCY_EMBEDDED

using namespace swift;

static LazyMutex RegistryLock;
TaskRegistryNode *swift::_swift_concurrency_task_registry = nullptr;

void swift::taskRegistryInsert(AsyncTask *task) {
  auto *node =
      static_cast<TaskRegistryNode *>(malloc(sizeof(TaskRegistryNode)));
  node->task = task;
  LazyMutex::ScopedLock guard(RegistryLock);
  node->next = _swift_concurrency_task_registry;
  _swift_concurrency_task_registry = node;
  SWIFT_TASK_DEBUG_LOG("TaskRegistry: inserted task %p id=%llu",
                       task, (unsigned long long)task->getTaskId());
}

void swift::taskRegistryRemove(AsyncTask *task) {
  LazyMutex::ScopedLock guard(RegistryLock);
  TaskRegistryNode **curr = &_swift_concurrency_task_registry;
  while (*curr) {
    if ((*curr)->task == task) {
      TaskRegistryNode *dead = *curr;
      *curr = dead->next;
      free(dead);
      SWIFT_TASK_DEBUG_LOG("TaskRegistry: removed task %p id=%llu",
                           task, (unsigned long long)task->getTaskId());
      return;
    }
    curr = &(*curr)->next;
  }
  swift_Concurrency_fatalError(0,
      "TaskRegistry: task %p destroyed without being registered\n", task);
}

#if SWIFT_STDLIB_TASK_REGISTRY_TESTING
size_t swift::swift_task_registryCount() {
  LazyMutex::ScopedLock guard(RegistryLock);
  size_t count = 0;
  for (auto *n = _swift_concurrency_task_registry; n; n = n->next)
    ++count;
  return count;
}
#endif

#endif // !SWIFT_CONCURRENCY_EMBEDDED
