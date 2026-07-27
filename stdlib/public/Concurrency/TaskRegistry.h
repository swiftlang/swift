//===--- TaskRegistry.h - Global live-task registry -------------*- C++ -*-===//
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
//
// A global intrusive doubly-linked list of all live AsyncTask objects.
// Intended for debugger / tool enumeration.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_CONCURRENCY_TASKREGISTRY_H
#define SWIFT_CONCURRENCY_TASKREGISTRY_H

#ifndef SWIFT_CONCURRENCY_ENABLE_TASK_REGISTRY
#define SWIFT_CONCURRENCY_ENABLE_TASK_REGISTRY 1
#endif

#include "swift/ABI/Task.h"
#include "swift/Runtime/Config.h"
#include "swift/Threading/Mutex.h"
#include <atomic>

namespace swift {

#if defined(__APPLE__) && defined(__aarch64__)
#define SWIFT_CACHE_LINE_SIZE 128
#else
#define SWIFT_CACHE_LINE_SIZE 64
#endif

static constexpr size_t TaskRegistryShardCount = 64;

struct alignas(SWIFT_CACHE_LINE_SIZE) TaskRegistryShard {
  std::atomic<AsyncTask *> head{nullptr};
  LazyMutex mutex;
  std::atomic<size_t> count{0};
};

/// Head pointers for the global live-task registry shards.
/// Walk each shard via task->_private().registryNext.
SWIFT_EXPORT_FROM(swift_Concurrency)
TaskRegistryShard _swift_concurrency_task_registry[TaskRegistryShardCount];

/// Register a newly created task. Must be called after full initialization.
#if SWIFT_CONCURRENCY_ENABLE_TASK_REGISTRY
void taskRegistryInsert(AsyncTask *task);
#else
inline void taskRegistryInsert(AsyncTask *task) {}
#endif

/// Deregister a task. Must be called before swift_slowDealloc frees it.
#if SWIFT_CONCURRENCY_ENABLE_TASK_REGISTRY
void taskRegistryRemove(AsyncTask *task);
#else
inline void taskRegistryRemove(AsyncTask *task) {}
#endif

/// Returns the count of currently registered tasks. For testing and debugging.
SWIFT_EXPORT_FROM(swift_Concurrency) size_t _swift_concurrency_debug_task_registryCount();

SWIFT_EXPORT_FROM(swift_Concurrency)
void _swift_concurrency_debug_task_registryWalk(void (*callback)(void *, void *), void *context);

SWIFT_EXPORT_FROM(swift_Concurrency)
void *_swift_concurrency_debug_task_getShardHead(size_t shardIndex);

SWIFT_EXPORT_FROM(swift_Concurrency)
void *_swift_concurrency_debug_task_getTaskNext(void *task);

SWIFT_EXPORT_FROM(swift_Concurrency)
uint64_t _swift_concurrency_debug_task_getId(void *task);

} // namespace swift

#endif // SWIFT_CONCURRENCY_TASKREGISTRY_H
