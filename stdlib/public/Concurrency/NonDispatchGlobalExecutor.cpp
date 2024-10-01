///===--- NonDispatchGlobalExecutor.inc ---------------------*- C++ -*--===///
///
/// This source file is part of the Swift.org open source project
///
/// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
/// Licensed under Apache License v2.0 with Runtime Library Exception
///
/// See https:///swift.org/LICENSE.txt for license information
/// See https:///swift.org/CONTRIBUTORS.txt for the list of Swift project authors
///
///===------------------------------------------------------------------===///
///
/// The implementation of the global executor when not using Dispatch but
/// also not using the cooperative global executor.  The general assumption
/// is that clients will be installing the appropriate hooks when all of
/// the functions here are called.
///
/// This file is included into GlobalExecutor.cpp only when both
/// Dispatch integration and the cooperative global executor are disabled.
/// It is expected to define the following functions:
///   swift_task_asyncMainDrainQueueImpl
///   swift_task_checkIsolatedImpl
///   swift_task_donateThreadToGlobalExecutorUntilImpl
///   swift_task_enqueueGlobalImpl
///   swift_task_enqueueGlobalWithDeadlineImpl
///   swift_task_enqueueGlobalWithDelayImpl
///   swift_task_enqueueMainExecutorImpl
///   swift_task_getMainExecutorImpl
///   swift_task_isMainExecutorImpl
///
///===------------------------------------------------------------------===///

#include "swift/Runtime/Debug.h"

#include "ExecutorImpl.h"

using namespace swift;

SWIFT_CC(swift)
void swift_task_enqueueGlobalImpl(SwiftJob *job) {
  assert(job && "no job provided");

  swift_reportError(0, "operation unsupported without libdispatch: "
                       "swift_task_enqueueGlobal");
}

SWIFT_CC(swift)
void swift_task_enqueueGlobalWithDelayImpl(SwiftJobDelay delay,
                                           SwiftJob *job) {
  assert(job && "no job provided");

  swift_reportError(0, "operation unsupported without libdispatch: "
                       "swift_task_enqueueGlobalWithDelay");
}

SWIFT_CC(swift)
void swift_task_enqueueGlobalWithDeadlineImpl(long long sec,
                                              long long nsec,
                                              long long tsec,
                                              long long tnsec,
                                              int clock, SwiftJob *job) {
  assert(job && "no job provided");

  swift_reportError(0, "operation unsupported without libdispatch: "
                       "swift_task_enqueueGlobalWithDeadline");
}

/// Enqueues a task on the main executor.
SWIFT_CC(swift)
void swift_task_enqueueMainExecutorImpl(SwiftJob *job) {
  assert(job && "no job provided");

  swift_reportError(0, "operation unsupported without libdispatch: "
                       "swift_task_enqueueMainExecutor");
}

SWIFT_CC(swift)
void swift_task_checkIsolatedImpl(SwiftExecutorRef executor) {
  swift_executor_invokeSwiftCheckIsolated(executor);
}

SWIFT_CC(swift)
SwiftExecutorRef swift_task_getMainExecutorImpl() {
  return swift_executor_generic();
}

SWIFT_CC(swift)
bool swift_task_isMainExecutorImpl(SwiftExecutorRef executor) {
  return swift_executor_isGeneric(executor);
}

SWIFT_RUNTIME_ATTRIBUTE_NORETURN SWIFT_CC(swift)
void swift_task_asyncMainDrainQueueImpll() {
  swift_reportError(0, "operation unsupported without libdispatch: "
                       "swift_task_asyncMainDrainQueue");
}

SWIFT_CC(swift) void
swift_task_donateThreadToGlobalExecutorUntilImpl(bool (*condition)(void *),
                                                 void *conditionContext) {
  swift_reportError(0, "operation unsupported: "
                    "swift_task_donateThreadToGlobalExecutorUntilImpl");
}
