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

#if !SWIFT_CONCURRENCY_EMBEDDED

using namespace swift;

static constexpr uintptr_t RegistryNextDeletedBit = 1;

static inline uintptr_t encodeRegistryNext(AsyncTask *task, bool deleted) {
  return reinterpret_cast<uintptr_t>(task) |
         (deleted ? RegistryNextDeletedBit : 0);
}

static inline AsyncTask *decodeRegistryNext(uintptr_t value) {
  return reinterpret_cast<AsyncTask *>(value & ~RegistryNextDeletedBit);
}

static inline size_t registryShardIndex(uint64_t taskId) {
  return static_cast<size_t>(taskId) & (TaskRegistryShardCount - 1);
}

static_assert((alignof(AsyncTask) & RegistryNextDeletedBit) == 0,
              "AsyncTask pointers must leave room for the registry mark bit");

std::atomic<AsyncTask *> swift::_swift_concurrency_task_registry[TaskRegistryShardCount] = {};

void swift::taskRegistryInsert(AsyncTask *task) {
  auto shardIndex = registryShardIndex(task->getTaskId());
  auto &shardHead = _swift_concurrency_task_registry[shardIndex];
  AsyncTask *head;
  do {
    head = shardHead.load(std::memory_order_acquire);
    task->_private().registryNext.store(encodeRegistryNext(head, false),
                                       std::memory_order_relaxed);
    task->_private().registryPrev.store(nullptr, std::memory_order_relaxed);
  } while (!shardHead.compare_exchange_weak(
      head, task, std::memory_order_release, std::memory_order_acquire));

  if (head) {
    head->_private().registryPrev.store(task, std::memory_order_relaxed);
  }

  SWIFT_TASK_DEBUG_LOG("TaskRegistry: inserted task %p id=%llu",
                       task, (unsigned long long)task->getTaskId());
}

void swift::taskRegistryRemove(AsyncTask *task) {
  auto shardIndex = registryShardIndex(task->getTaskId());
  auto &shardHead = _swift_concurrency_task_registry[shardIndex];
  uintptr_t next = task->_private().registryNext.fetch_or(
      RegistryNextDeletedBit, std::memory_order_release);
  AsyncTask *succ = decodeRegistryNext(next);

  while (true) {
    AsyncTask *prev = task->_private().registryPrev.load(
        std::memory_order_acquire);

    if (!prev) {
      AsyncTask *expected = task;
      if (shardHead.compare_exchange_strong(
              expected, succ, std::memory_order_release,
              std::memory_order_acquire)) {
        if (succ) {
          succ->_private().registryPrev.compare_exchange_strong(
              expected, nullptr, std::memory_order_release,
              std::memory_order_acquire);
        }
        SWIFT_TASK_DEBUG_LOG("TaskRegistry: removed task %p id=%llu",
                             task, (unsigned long long)task->getTaskId());
        return;
      }
      continue;
    }

    uintptr_t prevNext = prev->_private().registryNext.load(
        std::memory_order_acquire);
    if (decodeRegistryNext(prevNext) != task) {
      continue;
    }

    uintptr_t desired = encodeRegistryNext(succ, prevNext & RegistryNextDeletedBit);
    if (prev->_private().registryNext.compare_exchange_strong(
            prevNext, desired, std::memory_order_release,
            std::memory_order_acquire)) {
      if (succ) {
        succ->_private().registryPrev.compare_exchange_strong(
            task, prev, std::memory_order_release,
            std::memory_order_acquire);
      }
      SWIFT_TASK_DEBUG_LOG("TaskRegistry: removed task %p id=%llu",
                           task, (unsigned long long)task->getTaskId());
      return;
    }
  }
}

size_t swift::swift_task_registryCount() {
  size_t count = 0;
  for (size_t shardIndex = 0; shardIndex < TaskRegistryShardCount; ++shardIndex) {
    for (auto *task = _swift_concurrency_task_registry[shardIndex].load(
             std::memory_order_acquire);
         task;) {
      uintptr_t next = task->_private().registryNext.load(
          std::memory_order_acquire);
      if ((next & RegistryNextDeletedBit) == 0)
        ++count;
      task = decodeRegistryNext(next);
    }
  }
  return count;
}

#endif // !SWIFT_CONCURRENCY_EMBEDDED
