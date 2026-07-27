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
  return static_cast<size_t>((taskId ^ (taskId >> 8)) & (TaskRegistryShardCount - 1));
}

TaskRegistryShard swift::_swift_concurrency_task_registry[TaskRegistryShardCount] = {};

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
  
  AsyncTask *head = shard.head.load(std::memory_order_relaxed);
  task->_private().registryNext.store(head, std::memory_order_relaxed);
  task->_private().registryPrev.store(nullptr, std::memory_order_relaxed);
  if (head) {
    head->_private().registryPrev.store(task, std::memory_order_relaxed);
  }
  shard.head.store(task, std::memory_order_release);
  shard.count.fetch_add(1, std::memory_order_relaxed);

  SWIFT_TASK_DEBUG_LOG("TaskRegistry: inserted task %p id=%llu",
                       task, (unsigned long long)task->getTaskId());
}

void swift::taskRegistryRemove(AsyncTask *task) {
  if (!isTaskRegistryEnabled())
    return;

  auto shardIndex = registryShardIndex(task->getTaskId());
  auto &shard = _swift_concurrency_task_registry[shardIndex];
  
  LazyMutex::ScopedLock guard(shard.mutex);
  
  AsyncTask *prev = task->_private().registryPrev.load(std::memory_order_relaxed);
  AsyncTask *next = task->_private().registryNext.load(std::memory_order_relaxed);

  if (prev == nullptr && shard.head.load(std::memory_order_relaxed) != task) {
    return; // Task was not in the registry.
  }

  if (prev) {
    prev->_private().registryNext.store(next, std::memory_order_release);
  } else {
    shard.head.store(next, std::memory_order_release);
  }

  if (next) {
    next->_private().registryPrev.store(prev, std::memory_order_release);
  }

  // Do not clear registryNext so concurrent readers can continue traversal
  task->_private().registryPrev.store(nullptr, std::memory_order_relaxed);
  shard.count.fetch_sub(1, std::memory_order_relaxed);

  SWIFT_TASK_DEBUG_LOG("TaskRegistry: removed task %p id=%llu",
                       task, (unsigned long long)task->getTaskId());
}
#endif // SWIFT_CONCURRENCY_ENABLE_TASK_REGISTRY

SWIFT_EXPORT_FROM(swift_Concurrency) size_t swift::_swift_concurrency_debug_task_registryCount() {
  size_t count = 0;
  for (size_t i = 0; i < TaskRegistryShardCount; ++i) {
    count += _swift_concurrency_task_registry[i].count.load(std::memory_order_relaxed);
  }
  return count;
}

#if defined(__APPLE__)
#include <mach/mach.h>
#include <mach/mach_vm.h>
#elif defined(__linux__)
#include <sys/uio.h>
#include <unistd.h>
#elif defined(_WIN32)
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#endif

static AsyncTask *safeReadRegistryNext(AsyncTask *task) {
#if defined(__APPLE__)
  AsyncTask *next = nullptr;
  mach_vm_size_t size = sizeof(AsyncTask *);
  kern_return_t kr = mach_vm_read_overwrite(mach_task_self(), 
                                            (mach_vm_address_t)&task->_private().registryNext, 
                                            sizeof(AsyncTask *), 
                                            (mach_vm_address_t)&next, 
                                            &size);
  if (kr != KERN_SUCCESS) return nullptr;
  return next;
#elif defined(__linux__)
  AsyncTask *next = nullptr;
  struct iovec local[1];
  struct iovec remote[1];
  local[0].iov_base = &next;
  local[0].iov_len = sizeof(AsyncTask *);
  remote[0].iov_base = (void *)&task->_private().registryNext;
  remote[0].iov_len = sizeof(AsyncTask *);
  ssize_t result = process_vm_readv(getpid(), local, 1, remote, 1, 0);
  if (result != sizeof(AsyncTask *)) return nullptr;
  return next;
#elif defined(_WIN32)
  AsyncTask *next = nullptr;
  SIZE_T bytesRead = 0;
  if (!ReadProcessMemory(GetCurrentProcess(), (LPCVOID)&task->_private().registryNext, &next, sizeof(AsyncTask *), &bytesRead) || bytesRead != sizeof(AsyncTask *)) {
    return nullptr;
  }
  return next;
#else
  return task->_private().registryNext.load(std::memory_order_acquire);
#endif
}

SWIFT_EXPORT_FROM(swift_Concurrency) void swift::_swift_concurrency_debug_task_registryWalk(void (*callback)(void *, void *), void *context) {
  // Try to lock all shards to ensure crash-safety, but proceed even if lock fails
  bool lockedShards[TaskRegistryShardCount] = {false};
  for (size_t i = 0; i < TaskRegistryShardCount; ++i) {
    lockedShards[i] = _swift_concurrency_task_registry[i].mutex.try_lock();
  }

  for (size_t i = 0; i < TaskRegistryShardCount; ++i) {
    if (!lockedShards[i]) continue; // MUST SKIP UNLOCKED SHARDS

    size_t count = _swift_concurrency_task_registry[i].count.load(std::memory_order_relaxed);
    size_t limit = (count * 2) + 1000;
    size_t iterations = 0;

    for (auto *task = _swift_concurrency_task_registry[i].head.load(std::memory_order_acquire);
         task;
         task = safeReadRegistryNext(task)) {
      if (++iterations > limit) break;
      if (registryShardIndex(task->getTaskId()) != i) break; // Prevent shard jumping
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

SWIFT_EXPORT_FROM(swift_Concurrency) void *swift::_swift_concurrency_debug_task_getShardHead(size_t shardIndex) {
  if (shardIndex >= TaskRegistryShardCount)
    return nullptr;
  return _swift_concurrency_task_registry[shardIndex].head.load(std::memory_order_acquire);
}

SWIFT_EXPORT_FROM(swift_Concurrency) void *swift::_swift_concurrency_debug_task_getTaskNext(void *task) {
  if (!task)
    return nullptr;
  return safeReadRegistryNext(static_cast<AsyncTask *>(task));
}

SWIFT_EXPORT_FROM(swift_Concurrency) uint64_t swift::_swift_concurrency_debug_task_getId(void *task) {
  return static_cast<AsyncTask *>(task)->getTaskId();
}

#endif // !SWIFT_CONCURRENCY_EMBEDDED
