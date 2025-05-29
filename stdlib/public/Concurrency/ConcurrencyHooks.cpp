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

#include "swift/ABI/Task.h"
#include "swift/Runtime/Concurrency.h"
#include "swift/Runtime/HeapObject.h"
#include "swift/shims/Visibility.h"
#include "ExecutorImpl.h"
#include "TaskPrivate.h"

// Define all the hooks
#define SWIFT_CONCURRENCY_HOOK(returnType, name, ...)           \
  swift::name##_hook_t swift::name##_hook = nullptr
#define SWIFT_CONCURRENCY_HOOK0(returnType, name)               \
  swift::name##_hook_t swift::name##_hook = nullptr
#define SWIFT_CONCURRENCY_HOOK_OVERRIDE0(returnType, name)      \
  swift::name##_hook_t swift::name##_hook = nullptr

#include "swift/Runtime/ConcurrencyHooks.def"

using namespace swift;

// Define the external entry points; because the Impl functions use the C
// types from `ExecutorHooks.h`, we need to make an Orig version containing
// appropriate type casts in each case.

SWIFT_CC(swift) static void
swift_task_enqueueGlobalOrig(Job *job) {
  swift_task_enqueueGlobalImpl(reinterpret_cast<SwiftJob *>(job));
}

void
swift::swift_task_enqueueGlobal(Job *job) {
  _swift_tsan_release(job);

  concurrency::trace::job_enqueue_global(job);

  if (SWIFT_UNLIKELY(swift_task_enqueueGlobal_hook)) {
    swift_task_enqueueGlobal_hook(job, swift_task_enqueueGlobalOrig);
  } else {
    swift_task_enqueueGlobalOrig(job);
  }
}

SWIFT_CC(swift) static void
swift_task_enqueueGlobalWithDelayOrig(JobDelay delay, Job *job) {
  swift_task_enqueueGlobalWithDelayImpl(
      static_cast<SwiftJobDelay>(delay), reinterpret_cast<SwiftJob *>(job));
}

void
swift::swift_task_enqueueGlobalWithDelay(JobDelay delay, Job *job) {
  concurrency::trace::job_enqueue_global_with_delay(delay, job);

  if (SWIFT_UNLIKELY(swift_task_enqueueGlobalWithDelay_hook))
    swift_task_enqueueGlobalWithDelay_hook(
      delay, job, swift_task_enqueueGlobalWithDelayOrig);
  else
    swift_task_enqueueGlobalWithDelayOrig(delay, job);
}

SWIFT_CC(swift) static void
swift_task_enqueueGlobalWithDeadlineOrig(
    long long sec,
    long long nsec,
    long long tsec,
    long long tnsec,
    int clock, Job *job) {
  swift_task_enqueueGlobalWithDeadlineImpl(sec, nsec, tsec, tnsec, clock,
                                           reinterpret_cast<SwiftJob *>(job));
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
        sec, nsec, tsec, tnsec, clock, job,
        swift_task_enqueueGlobalWithDeadlineOrig);
  else
    swift_task_enqueueGlobalWithDeadlineOrig(sec, nsec, tsec, tnsec, clock, job);
}

SWIFT_CC(swift) static void
swift_task_checkIsolatedOrig(SerialExecutorRef executor) {
  swift_task_checkIsolatedImpl(*reinterpret_cast<SwiftExecutorRef *>(&executor));
}

void
swift::swift_task_checkIsolated(SerialExecutorRef executor) {
  if (SWIFT_UNLIKELY(swift_task_checkIsolated_hook))
    swift_task_checkIsolated_hook(executor, swift_task_checkIsolatedOrig);
  else
    swift_task_checkIsolatedOrig(executor);
}

SWIFT_CC(swift) static int8_t
swift_task_isIsolatingCurrentContextOrig(SerialExecutorRef executor) {
  return swift_task_isIsolatingCurrentContextImpl(
      *reinterpret_cast<SwiftExecutorRef *>(&executor));
}

int8_t
swift::swift_task_isIsolatingCurrentContext(SerialExecutorRef executor) {
  if (SWIFT_UNLIKELY(swift_task_isIsolatingCurrentContext_hook)) {
    return swift_task_isIsolatingCurrentContext_hook(
        executor, swift_task_isIsolatingCurrentContextOrig);
  } else {
    return swift_task_isIsolatingCurrentContextOrig(executor);
  }
}

// Implemented in Swift because we need to obtain the user-defined flags on the executor ref.
//
// We could inline this with effort, though.
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
extern "C" SWIFT_CC(swift)
swift::SerialExecutorRef _task_serialExecutor_getExecutorRef(
        swift::HeapObject *executor, const swift::Metadata *selfType,
        const swift::SerialExecutorWitnessTable *wtable);
#pragma clang diagnostic pop

/// WARNING: This method is expected to CRASH in new runtimes, and cannot be
/// used to implement "log warnings" mode. We would need a new entry point to
/// implement a "only log warnings" actor isolation checking mode, and it would
/// no be able handle more complex situations, as `SerialExecutor.checkIsolated`
/// is able to (by calling into dispatchPrecondition on old runtimes).
static SWIFT_CC(swift) bool
swift_task_isOnExecutorImpl(swift::HeapObject *executor,
                            const swift::Metadata *selfType,
                            const swift::SerialExecutorWitnessTable *wtable)
{
  auto executorRef = _task_serialExecutor_getExecutorRef(executor,
                                                         selfType,
                                                         wtable);
  return swift_task_isCurrentExecutor(executorRef);
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

SWIFT_CC(swift) static void
swift_task_enqueueMainExecutorOrig(Job *job) {
  swift_task_enqueueMainExecutorImpl(reinterpret_cast<SwiftJob *>(job));
}

void
swift::swift_task_enqueueMainExecutor(Job *job) {
  concurrency::trace::job_enqueue_main_executor(job);
  if (SWIFT_UNLIKELY(swift_task_enqueueMainExecutor_hook))
    swift_task_enqueueMainExecutor_hook(job, swift_task_enqueueMainExecutorOrig);
  else
    swift_task_enqueueMainExecutorOrig(job);
}

SWIFT_CC(swift) static swift::SerialExecutorRef
swift_task_getMainExecutorOrig() {
  auto ref = swift_task_getMainExecutorImpl();
  return *reinterpret_cast<SerialExecutorRef *>(&ref);
}

swift::SerialExecutorRef
swift::swift_task_getMainExecutor() {
  if (SWIFT_UNLIKELY(swift_task_getMainExecutor_hook))
    return swift_task_getMainExecutor_hook(swift_task_getMainExecutorOrig);
  else {
    return swift_task_getMainExecutorOrig();
  }
}

SWIFT_CC(swift) static bool
swift_task_isMainExecutorOrig(SerialExecutorRef executor) {
  return swift_task_isMainExecutorImpl(
           *reinterpret_cast<SwiftExecutorRef *>(&executor));
}

bool
swift::swift_task_isMainExecutor(SerialExecutorRef executor) {
  if (SWIFT_UNLIKELY(swift_task_isMainExecutor_hook))
    return swift_task_isMainExecutor_hook(
             executor, swift_task_isMainExecutorOrig);
  else
    return swift_task_isMainExecutorOrig(executor);
}

SWIFT_CC(swift) static void
swift_task_donateThreadToGlobalExecutorUntilOrig(bool (*condition)(void *),
                                                 void *context) {
  return swift_task_donateThreadToGlobalExecutorUntilImpl(condition, context);
}

void swift::
swift_task_donateThreadToGlobalExecutorUntil(bool (*condition)(void *),
                                             void *context) {
  if (SWIFT_UNLIKELY(swift_task_donateThreadToGlobalExecutorUntil_hook))
    return swift_task_donateThreadToGlobalExecutorUntil_hook(
              condition, context,
              swift_task_donateThreadToGlobalExecutorUntilOrig);
  else
    return swift_task_donateThreadToGlobalExecutorUntilOrig(condition, context);
}
