///===--- ExecutorHooks.h - Executor hook implementations ------------------===///
///
/// This source file is part of the Swift.org open source project
///
/// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
/// Licensed under Apache License v2.0 with Runtime Library Exception
///
/// See https:///swift.org/LICENSE.txt for license information
/// See https:///swift.org/CONTRIBUTORS.txt for the list of Swift project authors
///
///===----------------------------------------------------------------------===///
///
/// Contains inlineable executor hook implementations; the hookable functions
/// need to be exposed from swift_Concurrency, but within swift_Concurrency
/// we should be able to inline them.
///
///===----------------------------------------------------------------------===///

#ifndef SWIFT_CONCURRENCY_EXECUTORHOOKS_H
#define SWIFT_CONCURRENCY_EXECUTORHOOKS_H

#include "swift/Runtime/Concurrency.h"
#include "TaskPrivate.h" // for _swift_tsan_release

namespace swift {

// -- Functions that executors must implement ----------------------------------

SWIFT_CC(swift) void swift_task_enqueueGlobalImpl(Job *job);
SWIFT_CC(swift) void swift_task_enqueueGlobalWithDelayImpl(JobDelay delay,
                                                           Job *job);
SWIFT_CC(swift) void swift_task_enqueueGlobalWithDeadlineImpl(long long sec,
                                                              long long nsec,
                                                              long long tsec,
                                                              long long tnsec,
                                                              int clock,
                                                              Job *job);
SWIFT_CC(swift) void swift_task_enqueueMainExecutorImpl(Job *job);
SWIFT_CC(swift) void swift_task_checkIsolatedImpl(SerialExecutorRef executor);
SWIFT_CC(swift) SerialExecutorRef swift_task_getMainExecutorImpl();
SWIFT_CC(swift) bool swift_task_isMainExecutorImpl(SerialExecutorRef executor);

// -- Legacy functions that do not need to be implemented any more -------------

// Implementation is in GlobalExecutor.cpp.
SWIFT_CC(swift) bool swift_task_isOnExecutorImpl(
  HeapObject *executor,
  const Metadata *selfType,
  const SerialExecutorWitnessTable *wtable
);

} // namespace swift

#endif // SWIFT_CONCURRENCY_EXECUTORHOOKS_H
