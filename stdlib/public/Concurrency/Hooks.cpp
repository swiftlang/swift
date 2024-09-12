///===--- Hooks.cpp - Concurrency hook variables --------------------------===///
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
/// Defines all of the hook variables.
///
///===----------------------------------------------------------------------===///

#include "swift/Runtime/Concurrency.h"
#include "swift/Runtime/HeapObject.h"
#include "swift/shims/Visibility.h"
#include "ExecutorHooks.h"

// Define all the hooks
#define SWIFT_CONCURRENCY_HOOK(returnType, name, ...)           \
  swift::name##_hook_t swift::name##_hook = nullptr
#define SWIFT_CONCURRENCY_HOOK0(returnType, name)               \
  swift::name##_hook_t swift::name##_hook = nullptr
#define SWIFT_CONCURRENCY_HOOK_OVERRIDE0(returnType, name)      \
  swift::name##_hook_t swift::name##_hook = nullptr

#include "swift/Runtime/ConcurrencyHooks.def"

// Define the external entry points
void
swift::swift_task_enqueueGlobal(Job *job) {
  _swift_tsan_release(job);

  concurrency::trace::job_enqueue_global(job);

  if (SWIFT_UNLIKELY(swift_task_enqueueGlobal_hook))
    swift_task_enqueueGlobal_hook(job, swift_task_enqueueGlobalImpl);
  else
    swift_task_enqueueGlobalImpl(job);
}

void
swift::swift_task_enqueueGlobalWithDelay(JobDelay delay, Job *job) {
  concurrency::trace::job_enqueue_global_with_delay(delay, job);

  if (SWIFT_UNLIKELY(swift_task_enqueueGlobalWithDelay_hook))
    swift_task_enqueueGlobalWithDelay_hook(
        delay, job, swift_task_enqueueGlobalWithDelayImpl);
  else
    swift_task_enqueueGlobalWithDelayImpl(delay, job);
}

void
swift::swift_task_enqueueGlobalWithDeadline(
    long long sec,
    long long nsec,
    long long tsec,
    long long tnsec,
    int clock, Job *job) {
  if (SWIFT_UNLIKELY(swift_task_enqueueGlobalWithDeadline_hook))
    swift_task_enqueueGlobalWithDeadline_hook(
        sec, nsec, tsec, tnsec, clock, job, swift_task_enqueueGlobalWithDeadlineImpl);
  else
    swift_task_enqueueGlobalWithDeadlineImpl(sec, nsec, tsec, tnsec, clock, job);
}

void
swift::swift_task_checkIsolated(SerialExecutorRef executor) {
  if (SWIFT_UNLIKELY(swift_task_checkIsolated_hook))
    swift_task_checkIsolated_hook(executor, swift_task_checkIsolatedImpl);
  else
    swift_task_checkIsolatedImpl(executor);
}

bool
swift::swift_task_isOnExecutor(HeapObject *executor,
                               const Metadata *selfType,
                               const SerialExecutorWitnessTable *wtable) {
  if (SWIFT_UNLIKELY(swift_task_isOnExecutor_hook))
    return swift_task_isOnExecutor_hook(
        executor, selfType, wtable, swift_task_isOnExecutorImpl);
  else
    return swift_task_isOnExecutorImpl(executor, selfType, wtable);
}

void
swift::swift_task_enqueueMainExecutor(Job *job) {
  concurrency::trace::job_enqueue_main_executor(job);
  if (SWIFT_UNLIKELY(swift_task_enqueueMainExecutor_hook))
    swift_task_enqueueMainExecutor_hook(job,
                                        swift_task_enqueueMainExecutorImpl);
  else
    swift_task_enqueueMainExecutorImpl(job);
}

swift::SerialExecutorRef
swift::swift_task_getMainExecutor() {
  if (SWIFT_UNLIKELY(swift_task_getMainExecutor_hook))
    return swift_task_getMainExecutor_hook(swift_task_getMainExecutorImpl);
  else
    return swift_task_getMainExecutorImpl();
}

bool
swift::swift_task_isMainExecutor(SerialExecutorRef executor) {
  if (SWIFT_UNLIKELY(swift_task_isMainExecutor_hook))
    return swift_task_isMainExecutor_hook(executor,
                                          swift_task_isMainExecutorImpl);
  else
    return swift_task_isMainExecutorImpl(executor);
}
