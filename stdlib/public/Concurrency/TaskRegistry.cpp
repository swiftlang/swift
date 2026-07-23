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

#if !SWIFT_CONCURRENCY_EMBEDDED

using namespace swift;

static inline size_t registryShardIndex(uint64_t taskId) {
  return static_cast<size_t>(taskId) & (TaskRegistryShardCount - 1);
}

struct alignas(SWIFT_CACHE_LINE_SIZE) PaddedLazyMutex {
  LazyMutex mutex;
};

TaskRegistryShard swift::_swift_concurrency_task_registry[TaskRegistryShardCount] = {};
static PaddedLazyMutex ShardLocks[TaskRegistryShardCount];

void swift::taskRegistryInsert(AsyncTask *task) {
  auto shardIndex = registryShardIndex(task->getTaskId());
  auto &shardHead = _swift_concurrency_task_registry[shardIndex].head;
  
  LazyMutex::ScopedLock guard(ShardLocks[shardIndex].mutex);
  
  AsyncTask *head = shardHead.load(std::memory_order_relaxed);
  task->_private().registryNext.store(head, std::memory_order_relaxed);
  task->_private().registryPrev.store(nullptr, std::memory_order_relaxed);
  if (head) {
    head->_private().registryPrev.store(task, std::memory_order_relaxed);
  }
  shardHead.store(task, std::memory_order_relaxed);

  SWIFT_TASK_DEBUG_LOG("TaskRegistry: inserted task %p id=%llu",
                       task, (unsigned long long)task->getTaskId());
}

void swift::taskRegistryRemove(AsyncTask *task) {
  auto shardIndex = registryShardIndex(task->getTaskId());
  auto &shardHead = _swift_concurrency_task_registry[shardIndex].head;
  
  LazyMutex::ScopedLock guard(ShardLocks[shardIndex].mutex);
  
  AsyncTask *prev = task->_private().registryPrev.load(std::memory_order_relaxed);
  AsyncTask *next = task->_private().registryNext.load(std::memory_order_relaxed);

  if (prev) {
    prev->_private().registryNext.store(next, std::memory_order_relaxed);
  } else {
    shardHead.store(next, std::memory_order_relaxed);
  }

  if (next) {
    next->_private().registryPrev.store(prev, std::memory_order_relaxed);
  }

  task->_private().registryNext.store(nullptr, std::memory_order_relaxed);
  task->_private().registryPrev.store(nullptr, std::memory_order_relaxed);

  SWIFT_TASK_DEBUG_LOG("TaskRegistry: removed task %p id=%llu",
                       task, (unsigned long long)task->getTaskId());
}

size_t swift::__swift_concurrency_debug_task_registryCount() {
  size_t count = 0;
  for (size_t shardIndex = 0; shardIndex < TaskRegistryShardCount; ++shardIndex) {
    LazyMutex::ScopedLock guard(ShardLocks[shardIndex].mutex);
    for (auto *task = _swift_concurrency_task_registry[shardIndex].head.load(
             std::memory_order_relaxed);
         task;
         task = task->_private().registryNext.load(std::memory_order_relaxed)) {
      ++count;
    }
  }
  return count;
}

void swift::__swift_concurrency_debug_task_registryWalk(void (*callback)(void *, void *), void *context) {
  for (size_t shardIndex = 0; shardIndex < TaskRegistryShardCount; ++shardIndex) {
    LazyMutex::ScopedLock guard(ShardLocks[shardIndex].mutex);
    for (auto *task = _swift_concurrency_task_registry[shardIndex].head.load(
             std::memory_order_relaxed);
         task;
         task = task->_private().registryNext.load(std::memory_order_relaxed)) {
      callback(task, context);
    }
  }
}

void *swift::__swift_concurrency_debug_task_getShardHead(size_t shardIndex) {
  if (shardIndex >= TaskRegistryShardCount)
    return nullptr;
  return _swift_concurrency_task_registry[shardIndex].head.load(std::memory_order_relaxed);
}

void *swift::__swift_concurrency_debug_task_getTaskNext(void *task) {
  if (!task)
    return nullptr;
  return static_cast<AsyncTask *>(task)->_private().registryNext.load(std::memory_order_relaxed);
}

uint64_t swift::__swift_concurrency_debug_task_getId(void *task) {
  return static_cast<AsyncTask *>(task)->getTaskId();
}

#endif // !SWIFT_CONCURRENCY_EMBEDDED
