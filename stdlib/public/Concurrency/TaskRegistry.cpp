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

#if SWIFT_CONCURRENCY_ENABLE_TASK_REGISTRY

using namespace swift;

static inline size_t registryShardIndex(uint64_t taskId) {
  return static_cast<size_t>((taskId ^ (taskId >> 8)) &
                             (TaskRegistryShardCount - 1));
}

TaskRegistryShard
    swift::_swift_concurrency_task_registry[TaskRegistryShardCount] = {};

#if SWIFT_CONCURRENCY_ENABLE_TASK_REGISTRY

static inline bool isTaskRegistryEnabled() {
  static bool enabled = runtime::environment::concurrencyEnableTaskRegistry();
  return enabled;
}

void swift::taskRegistryInsert(AsyncTask *task) {
  if (!isTaskRegistryEnabled())
    return;

  auto shardIndex = registryShardIndex(task->getTaskId());
  auto &shard = _swift_concurrency_task_registry[shardIndex];

  LazyMutex::ScopedLock guard(shard.mutex);

  AsyncTask *head = shard.head;
  task->_private().registryNext = head;
  task->_private().registryPrev = nullptr;
  if (head) {
    head->_private().registryPrev = task;
  }
  shard.head = task;
  ++shard.count;

  SWIFT_TASK_DEBUG_LOG("TaskRegistry: inserted task %p id=%llu", task,
                       (unsigned long long)task->getTaskId());
}

void swift::taskRegistryRemove(AsyncTask *task) {
  if (!isTaskRegistryEnabled())
    return;

  auto shardIndex = registryShardIndex(task->getTaskId());
  auto &shard = _swift_concurrency_task_registry[shardIndex];

  LazyMutex::ScopedLock guard(shard.mutex);

  AsyncTask *prev = task->_private().registryPrev;
  AsyncTask *next = task->_private().registryNext;

  if (prev == nullptr && shard.head != task) {
    return; // Task was not in the registry.
  }

  if (prev) {
    prev->_private().registryNext = next;
  } else {
    shard.head = next;
  }

  if (next) {
    next->_private().registryPrev = prev;
  }

  // Do not clear registryNext so concurrent readers can continue traversal
  task->_private().registryPrev = nullptr;
  --shard.count;

  SWIFT_TASK_DEBUG_LOG("TaskRegistry: removed task %p id=%llu", task,
                       (unsigned long long)task->getTaskId());
}
#endif // SWIFT_CONCURRENCY_ENABLE_TASK_REGISTRY

SWIFT_EXPORT_FROM(swift_Concurrency) SWIFT_CC(swift)
size_t swift::_swift_concurrency_debug_task_registryCount() {
  size_t count = 0;
  for (size_t i = 0; i < TaskRegistryShardCount; ++i) {
    count += _swift_concurrency_task_registry[i].count;
  }
  return count;
}

SWIFT_EXPORT_FROM(swift_Concurrency) SWIFT_CC(swift)
void swift::_swift_concurrency_debug_task_registryWalk(void (*callback)(void *,
                                                                        void *),
                                                       void *context) {
  // Try to lock all shards to ensure crash-safety, but proceed even if lock
  // fails
  bool lockedShards[TaskRegistryShardCount] = {false};
  for (size_t i = 0; i < TaskRegistryShardCount; ++i) {
    lockedShards[i] = _swift_concurrency_task_registry[i].mutex.try_lock();
  }

  for (size_t i = 0; i < TaskRegistryShardCount; ++i) {
    if (!lockedShards[i])
      continue; // MUST SKIP UNLOCKED SHARDS

    size_t count = _swift_concurrency_task_registry[i].count;
    size_t limit = (count * 2) + 1000;
    size_t iterations = 0;

    for (auto *task = _swift_concurrency_task_registry[i].head;
         task;
         task = task->_private().registryNext) {
      if (++iterations > limit)
        break;
      if (registryShardIndex(task->getTaskId()) != i)
        break; // Prevent shard jumping
      callback(task, context);
    }
  }

  // Unlock acquired shards in reverse order
  for (size_t i = TaskRegistryShardCount; i > 0; --i) {
    if (lockedShards[i - 1]) {
      _swift_concurrency_task_registry[i - 1].mutex.unlock();
    }
  }
}

SWIFT_EXPORT_FROM(swift_Concurrency) SWIFT_CC(swift)
void *swift::_swift_concurrency_debug_task_getShardHead(size_t shardIndex) {
  if (shardIndex >= TaskRegistryShardCount)
    return nullptr;
  return _swift_concurrency_task_registry[shardIndex].head;
}

SWIFT_EXPORT_FROM(swift_Concurrency) SWIFT_CC(swift)
void *swift::_swift_concurrency_debug_task_getTaskNext(void *task) {
  if (!task)
    return nullptr;
  return static_cast<AsyncTask *>(task)->_private().registryNext;
}

SWIFT_EXPORT_FROM(swift_Concurrency) SWIFT_CC(swift)
uint64_t swift::_swift_concurrency_debug_task_getId(void *task) {
  return static_cast<AsyncTask *>(task)->getTaskId();
}

#endif // !SWIFT_CONCURRENCY_EMBEDDED
