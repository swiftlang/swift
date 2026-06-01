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
// A global singly-linked list of all live AsyncTask objects.
// Protected by a single LazyMutex; intended for debugger / tool enumeration.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_CONCURRENCY_TASKREGISTRY_H
#define SWIFT_CONCURRENCY_TASKREGISTRY_H

#include "swift/ABI/Task.h"
#include "swift/Runtime/Config.h"

namespace swift {

struct TaskRegistryNode {
  AsyncTask *task;
  TaskRegistryNode *next;
};

/// Head of the global live-task linked list.
/// Walk via node->next; node->task points to a live AsyncTask.
/// Guarded by the internal RegistryLock in TaskRegistry.cpp.
/// Exported so that LLDB and swift-inspect can locate the list without a
/// new runtime API.
SWIFT_EXPORT_FROM(swift_Concurrency)
extern TaskRegistryNode *_swift_concurrency_task_registry;

/// Register a newly created task. Must be called after full initialization.
void taskRegistryInsert(AsyncTask *task);

/// Deregister a task. Must be called before swift_slowDealloc frees it.
void taskRegistryRemove(AsyncTask *task);

/// Returns the count of currently registered tasks. For testing and debugging.
SWIFT_EXPORT_FROM(swift_Concurrency) size_t swift_task_registryCount();

} // namespace swift

#endif // SWIFT_CONCURRENCY_TASKREGISTRY_H
