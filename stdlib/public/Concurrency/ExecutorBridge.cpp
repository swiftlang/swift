//===--- ExecutorBridge.cpp - C++ side of executor bridge -----------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#if SWIFT_CONCURRENCY_USES_DISPATCH
#include <dispatch/dispatch.h>
#endif

#include "swift/Threading/Once.h"

#include "Error.h"
#include "ExecutorBridge.h"
#include "TaskPrivate.h"

using namespace swift;

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"

extern "C" SWIFT_CC(swift)
void _swift_exit(int result) {
  exit(result);
}

extern "C" SWIFT_CC(swift)
void swift_createDefaultExecutorsOnce() {
  static swift::once_t createExecutorsOnce;

  swift::once(createExecutorsOnce, swift_createDefaultExecutors);
}

#if SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
extern "C" SWIFT_CC(swift)
SerialExecutorRef swift_getMainExecutor() {
  return SerialExecutorRef::generic();
}
#endif

extern "C" SWIFT_CC(swift)
void _swift_task_checkIsolatedSwift(HeapObject *executor,
                                    const Metadata *executorType,
                                    const SerialExecutorWitnessTable *witnessTable);

extern "C" SWIFT_CC(swift)
uint8_t swift_job_getPriority(Job *job) {
  return (uint8_t)(job->getPriority());
}

extern "C" SWIFT_CC(swift)
uint8_t swift_job_getKind(Job *job) {
  return (uint8_t)(job->Flags.getKind());
}

extern "C" SWIFT_CC(swift)
void *swift_job_getExecutorPrivateData(Job *job) {
  return &job->SchedulerPrivate[0];
}

#if SWIFT_CONCURRENCY_USES_DISPATCH
extern "C" SWIFT_CC(swift) __attribute__((noreturn))
void swift_dispatchMain() {
  dispatch_main();
}

extern "C" SWIFT_CC(swift)
void swift_dispatchAssertMainQueue() {
  dispatch_assert_queue(dispatch_get_main_queue());
}

extern "C" SWIFT_CC(swift)
void *swift_getDispatchQueueForExecutor(SerialExecutorRef executor) {
  if (executor.getRawImplementation() == (uintptr_t)_swift_task_getDispatchQueueSerialExecutorWitnessTable()) {
    return executor.getIdentity();
  }
  return nullptr;
}

#endif // SWIFT_CONCURRENCY_USES_DISPATCH

#pragma clang diagnostic pop
