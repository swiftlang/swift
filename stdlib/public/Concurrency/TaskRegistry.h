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

#include "swift/ABI/Task.h"
#include "swift/Runtime/Config.h"
#include "swift/Threading/Mutex.h"
#include <atomic>

namespace swift {

#if SWIFT_CONCURRENCY_ENABLE_TASK_REGISTRY

#if defined(__APPLE__) && defined(__aarch64__)
#define SWIFT_CACHE_LINE_SIZE 128
#else
#define SWIFT_CACHE_LINE_SIZE 64
#endif

static constexpr size_t TaskRegistryShardCount = 64;

struct alignas(SWIFT_CACHE_LINE_SIZE) TaskRegistryShard {
  AsyncTask * head{nullptr};
  LazyMutex mutex;
  size_t count{0};
};

/// Head pointers for the global live-task registry shards.
/// Walk each shard via task->_private().registryNext.
SWIFT_EXPORT_FROM(swift_Concurrency)
TaskRegistryShard _swift_concurrency_task_registry[TaskRegistryShardCount];

/// Register a newly created task. Must be called after full initialization.
void taskRegistryInsert(AsyncTask *task);

/// Deregister a task. Must be called before swift_slowDealloc frees it.
void taskRegistryRemove(AsyncTask *task);

/// Returns the count of currently registered tasks. For testing and debugging.
SWIFT_EXPORT_FROM(swift_Concurrency) SWIFT_CC(swift)
size_t _swift_concurrency_debug_task_registryCount();

SWIFT_EXPORT_FROM(swift_Concurrency) SWIFT_CC(swift)
void _swift_concurrency_debug_task_registryWalk(void (*callback)(void *, void *), void *context);

SWIFT_EXPORT_FROM(swift_Concurrency) SWIFT_CC(swift)
void *_swift_concurrency_debug_task_getShardHead(size_t shardIndex);

SWIFT_EXPORT_FROM(swift_Concurrency) SWIFT_CC(swift)
void *_swift_concurrency_debug_task_getTaskNext(void *task);

SWIFT_EXPORT_FROM(swift_Concurrency) SWIFT_CC(swift)
uint64_t _swift_concurrency_debug_task_getId(void *task);

#else // !SWIFT_CONCURRENCY_ENABLE_TASK_REGISTRY

inline void taskRegistryInsert(AsyncTask *task) {}
inline void taskRegistryRemove(AsyncTask *task) {}

#endif // SWIFT_CONCURRENCY_ENABLE_TASK_REGISTRY

} // namespace swift

#endif // SWIFT_CONCURRENCY_TASKREGISTRY_H
