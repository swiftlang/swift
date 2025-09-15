//===--- TaskGroup.h - ABI structures for task groups -00--------*- C++ -*-===//
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
// Swift ABI describing task groups.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_ABI_TASK_GROUP_H
#define SWIFT_ABI_TASK_GROUP_H

#include "swift/ABI/Task.h"
#include "swift/ABI/TaskStatus.h"
#include "swift/ABI/HeapObject.h"
#include "swift/Runtime/Concurrency.h"
#include "swift/Runtime/Config.h"
#include "swift/Basic/RelativePointer.h"
#include "swift/Basic/STLExtras.h"

namespace swift {

/// The task group is responsible for maintaining dynamically created child tasks.
class alignas(Alignment_TaskGroup) TaskGroup {
public:
  // These constructors do not initialize the group instance, and the
  // destructor does not destroy the group instance; you must call
  // swift_taskGroup_{initialize,destroy} yourself.
  constexpr TaskGroup()
    : PrivateData{} {}

  void *PrivateData[NumWords_TaskGroup];

  /// Upon a future task's completion, offer it to the task group it belongs to.
  void offer(AsyncTask *completed, AsyncContext *context);

  /// Checks the cancellation status of the group.
  bool isCancelled();

  /// Only mark the task group as cancelled, without performing the follow-up
  /// work of cancelling all the child tasks.
  ///
  /// Returns true if the group was already cancelled before this call.
  bool statusCancel();

  // Add a child task to the task group. Always called while holding the
  // status record lock of the task group's owning task.
  void addChildTask(AsyncTask *task);

  // Remove a child task from the task group. Always called while holding
  // the status record lock of the task group's owning task.
  void removeChildTask(AsyncTask *task);

  // Provide accessor for task group's status record
  TaskGroupTaskStatusRecord *getTaskRecord();

  /// The group is a `TaskGroup` that accumulates results.
  bool isAccumulatingResults() {
    return !isDiscardingResults();
  }

  /// The group is a `DiscardingTaskGroup` that discards results.
  bool isDiscardingResults();
};

} // end namespace swift

#endif // SWIFT_ABI_TASK_GROUP_H
