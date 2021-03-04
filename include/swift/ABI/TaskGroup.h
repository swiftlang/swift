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
#include "swift/ABI/HeapObject.h"
#include "swift/Runtime/Concurrency.h"
#include "swift/Runtime/Config.h"
#include "swift/Basic/RelativePointer.h"
#include "swift/Basic/STLExtras.h"

namespace swift {

using FutureAsyncSignature =
  AsyncSignature<void(void*), /*throws*/ true>;

/// The task group is responsible for maintaining dynamically created child tasks.
class alignas(Alignment_TaskGroup) TaskGroup {
public:
  // These constructors do not initialize the actor instance, and the
  // destructor does not destroy the actor instance; you must call
  // swift_taskGroup_{initialize,destroy} yourself.
  constexpr TaskGroup()
    : PrivateData{} {}

  void *PrivateData[NumWords_TaskGroup];

  /// Implements TaskGroup.add
  void add(JobPriority priorityOverride,
           FutureAsyncSignature::FunctionType *function);

  /// Implements a task group completion reporting back to its owning group.
  ///
  /// Upon a future task's completion, that task group child task will offer
  /// itself as `completed` to its parent group. This way the group can either
  /// immediately resume the (if present) on `next()` waiting task, or record
  /// the future result for later consumption whenever `next()` is awaited on.
  ///
  /// If other tasks have been offered but not yet next() consumed, this enqueues
  /// this completed result in an internal queue that will feed future next()
  /// invocations.
  void offer(AsyncTask *completed, AsyncContext *context, ExecutorRef executor);

};

} // end namespace swift

#endif
