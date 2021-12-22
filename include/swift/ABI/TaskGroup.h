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

  // Add a child task to the group. Always called with the status record lock of
  // the parent task held
  void addChildTask(AsyncTask *task);

  // Provide accessor for task group's status record
  TaskGroupTaskStatusRecord *getTaskRecord();
};

} // end namespace swift

#endif // SWIFT_ABI_TASK_GROUP_H
