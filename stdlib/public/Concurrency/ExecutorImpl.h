///===--- ExecutorImpl.h - Global executor implementation interface --------===///
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
/// Contains the declarations you need to write a custom global
/// executor in plain C; this file intentionally does not include any
/// Swift ABI headers, because pulling those in necessitates use of
/// C++ and also pulls in a whole load of other things we don't need.
///
/// Note also that the global executor is expected to be statically
/// linked with swift_Concurrency, so we needn't worry about dynamic
/// linking from here.
///
///===----------------------------------------------------------------------===///

#ifndef SWIFT_CONCURRENCY_EXECUTORIMPL_H
#define SWIFT_CONCURRENCY_EXECUTORIMPL_H

#if !defined(__swift__) && __has_feature(ptrauth_calls)
#include <ptrauth.h>
#endif
#ifndef __ptrauth_objc_isa_pointer
#define __ptrauth_objc_isa_pointer
#endif

#include <inttypes.h>
#include <stdbool.h>
#include <stdlib.h>

#ifdef __cplusplus
extern "C" {
#endif

#ifndef SWIFT_CC
#define SWIFT_CC(x)     SWIFT_CC_##x
#define SWIFT_CC_swift  __attribute__((swiftcall))
#endif

#ifndef SWIFT_RUNTIME_ATTRIBUTE_NORETURN
#define SWIFT_RUNTIME_ATTRIBUTE_NORETURN __attribute__((noreturn))
#endif

// -- C versions of types executors might need ---------------------------------

/// Represents a Swift type
typedef struct SwiftHeapMetadata SwiftHeapMetadata;

/// Jobs have flags, which currently encode a kind and a priority
typedef uint32_t SwiftJobFlags;

typedef size_t SwiftJobKind;
enum {
  SwiftTaskJobKind = 0,

  // Job kinds >= 192 are private to the implementation
  SwiftFirstReservedJobKind = 192,
};

typedef size_t SwiftJobPriority;
enum {
  SwiftUserInteractiveJobPriority = 0x21, /* UI */
  SwiftUserInitiatedJobPriority   = 0x19, /* IN */
  SwiftDefaultJobPriority         = 0x15, /* DEF */
  SwiftUtilityJobPriority         = 0x11, /* UT */
  SwiftBackgroundJobPriority      = 0x09, /* BG */
  SwiftUnspecifiedJobPriority     = 0x00, /* UN */
};

enum { SwiftJobPriorityBucketCount = 5 };

static inline int swift_priority_getBucketIndex(SwiftJobPriority priority) {
  if (priority > SwiftUserInitiatedJobPriority)
    return 0;
  else if (priority > SwiftDefaultJobPriority)
    return 1;
  else if (priority > SwiftUtilityJobPriority)
    return 2;
  else if (priority > SwiftBackgroundJobPriority)
    return 3;
  else
    return 4;
}

/// Used by the Concurrency runtime to represent a job.  The `schedulerPrivate`
/// field may be freely used by the executor implementation.
typedef struct {
  SwiftHeapMetadata const *__ptrauth_objc_isa_pointer _Nonnull metadata;
  uintptr_t refCounts;
  void * _Nullable schedulerPrivate[2];
  SwiftJobFlags flags;
} __attribute__((aligned(2 * sizeof(void *)))) SwiftJob;

/// Indexes in the schedulerPrivate array
enum {
  SwiftJobNextWaitingTaskIndex = 0,

  // These are only relevant for the Dispatch executor
  SwiftJobDispatchHasLongObjectHeader = sizeof(void *) == sizeof(int),
  SwiftJobDispatchLinkageIndex = SwiftJobDispatchHasLongObjectHeader ? 1 : 0,
  SwiftJobDispatchQueueIndex = SwiftJobDispatchHasLongObjectHeader? 0 : 1
};

/// Get the kind of a job, by directly accessing the flags field.
static inline SwiftJobKind swift_job_getKind(SwiftJob * _Nonnull job) {
  return (SwiftJobKind)(job->flags & 0xff);
}

/// Get the priority of a job, by directly accessing the flags field.
static inline SwiftJobPriority swift_job_getPriority(SwiftJob * _Nonnull job) {
  return (SwiftJobPriority)((job->flags >> 8) & 0xff);
}

/// Get a pointer to the scheduler private data.
static inline void * _Nonnull * _Nonnull swift_job_getPrivateData(SwiftJob * _Nonnull job) {
  return &job->schedulerPrivate[0];
}

/// Allocate memory associated with a job.
void * _Nullable swift_job_alloc(SwiftJob * _Nonnull job, size_t size);

/// Release memory allocated using `swift_job_alloc()`.
void swift_job_dealloc(SwiftJob * _Nonnull job, void * _Nonnull ptr);

/// Swift's refcounted objects start with this header
typedef struct {
  SwiftHeapMetadata const *__ptrauth_objc_isa_pointer _Nonnull metadata;
} SwiftHeapObject;

/// A reference to an executor consists of two words; the first is a pointer
/// which may or may not be to a Swift heap object.
typedef struct {
  SwiftHeapObject * _Nullable identity;
  uintptr_t implementation;
} SwiftExecutorRef;

/// Indicates whether or not an executor can be compared by just looking at
/// the `identity` field.
typedef unsigned SwiftExecutorKind;
enum {
  SwiftExecutorOrdinaryKind = 0,
  SwiftExecutorComplexEqualityKind = 1
};

typedef struct SwiftExecutorWitnessTable SwiftExecutorWitnessTable;

/// Return the generic executor.
static inline SwiftExecutorRef swift_executor_generic(void) {
  return (SwiftExecutorRef){ NULL, 0 };
}

/// Return an ordinary executor with the specified identity and witness table.
static inline SwiftExecutorRef
swift_executor_ordinary(SwiftHeapObject * _Nullable identity,
                        SwiftExecutorWitnessTable * _Nullable witnessTable) {
  return (SwiftExecutorRef){ identity,
                             (uintptr_t)witnessTable
                             | SwiftExecutorOrdinaryKind };
}

/// Return a complex equality executor with the specified identity and
/// witness table.
static inline SwiftExecutorRef
swift_executor_complexEquality(SwiftHeapObject * _Nullable identity,
                               SwiftExecutorWitnessTable * _Nullable witnessTable) {
  return (SwiftExecutorRef){ identity,
                             (uintptr_t)witnessTable
                             | SwiftExecutorComplexEqualityKind };
}

/// Get the type of executor (ordinary vs complex equality).
static inline SwiftExecutorKind
swift_executor_getKind(SwiftExecutorRef executor) {
  const uintptr_t mask = ~(uintptr_t)(sizeof(void *) - 1);
  return executor.implementation & ~mask;
}

/// Return `true` if this is the generic executor.
static inline bool swift_executor_isGeneric(SwiftExecutorRef executor) {
  return executor.identity == NULL;
}

/// Return `true` if this executor is in fact the default actor.
static inline bool swift_executor_isDefaultActor(SwiftExecutorRef executor) {
  return !swift_executor_isGeneric(executor) && executor.implementation == 0;
}

/// Retrieve the identity of an executor.
static inline SwiftHeapObject * _Nullable
swift_executor_getIdentity(SwiftExecutorRef executor) {
  return executor.identity;
}

/// Test if an executor has a valid witness table.
static inline bool swift_executor_hasWitnessTable(SwiftExecutorRef executor) {
  return (!swift_executor_isGeneric(executor)
          && !swift_executor_isDefaultActor(executor));
}

/// Retrieve the witness table of an executor.
static inline const SwiftExecutorWitnessTable * _Nullable
swift_executor_getWitnessTable(SwiftExecutorRef executor) {
  const uintptr_t mask = ~(uintptr_t)(sizeof(void *) - 1);
  return (const SwiftExecutorWitnessTable *)(executor.implementation & mask);
}

/// Test if this is the main executor.
static inline bool swift_executor_isMain(SwiftExecutorRef executor) {
  extern bool _swift_task_isMainExecutor_c(SwiftExecutorRef executor);

  return _swift_task_isMainExecutor_c(executor);
}

/// Assert that we are isolated on the specified executor.
static inline bool
swift_executor_invokeSwiftCheckIsolated(SwiftExecutorRef executor) {
  extern bool _swift_task_invokeSwiftCheckIsolated_c(SwiftExecutorRef executor);

  return _swift_task_invokeSwiftCheckIsolated_c(executor);
}

/// Check if the current context is isolated by the specified executor.
///
/// The numeric values correspond to `swift::IsIsolatingCurrentContextDecision`.
///
/// Specifically ONLY `1` means "isolated", while smaller values mean not isolated or unknown.
/// See ``IsIsolatingCurrentContextDecision`` for details.
static inline int8_t
swift_executor_invokeSwiftIsIsolatingCurrentContext(SwiftExecutorRef executor) {
  extern int8_t _swift_task_invokeSwiftIsIsolatingCurrentContext_c(SwiftExecutorRef executor);

  return _swift_task_invokeSwiftIsIsolatingCurrentContext_c(executor);
}

/// Execute the specified job while running on the specified executor.
static inline void swift_job_run(SwiftJob * _Nonnull job,
                                 SwiftExecutorRef executor) {
  extern void _swift_job_run_c(SwiftJob * _Nonnull job,
                               SwiftExecutorRef executor);

  _swift_job_run_c(job, executor);
}

/// A delay in nanoseconds.
typedef unsigned long long SwiftJobDelay;

/// Names of clocks supported by the Swift time functions.
typedef int SwiftClockId;
enum {
  SwiftContinuousClock = 1,
  SwiftSuspendingClock = 2
};

/// An accurate timestamp, with *up to* nanosecond precision.
typedef struct {
  long long seconds;
  long long nanoseconds;
} SwiftTime;

/// Get the current time from the specified clock
extern SwiftTime swift_time_now(SwiftClockId clock);

/// Get the resolution of the specified clock
extern SwiftTime swift_time_getResolution(SwiftClockId clock);

// -- Functions that executors must implement ----------------------------------

/// Enqueue a job on the global executor.
SWIFT_CC(swift) void swift_task_enqueueGlobalImpl(SwiftJob * _Nonnull job);

/// Enqueue a job on the global executor, with a specific delay before it
/// should execute.
SWIFT_CC(swift) void swift_task_enqueueGlobalWithDelayImpl(SwiftJobDelay delay,
                                                           SwiftJob * _Nonnull job);

/// Enqueue a job on the global executor, with a specific deadline before
/// which it must execute.
SWIFT_CC(swift) void swift_task_enqueueGlobalWithDeadlineImpl(long long sec,
                                                              long long nsec,
                                                              long long tsec,
                                                              long long tnsec,
                                                              int clock,
                                                              SwiftJob * _Nonnull job);

/// Enqueue a job on the main executor (which may or may not be the same as
/// the global executor).
SWIFT_CC(swift) void swift_task_enqueueMainExecutorImpl(SwiftJob * _Nonnull job);

/// Assert that the specified executor is the current executor.
SWIFT_CC(swift) void swift_task_checkIsolatedImpl(SwiftExecutorRef executor);

/// Check if the specified executor isolates the current context.
SWIFT_CC(swift) int8_t
  swift_task_isIsolatingCurrentContextImpl(SwiftExecutorRef executor);

/// Get a reference to the main executor.
SWIFT_CC(swift) SwiftExecutorRef swift_task_getMainExecutorImpl(void);

/// Check if the specified executor is the main executor.
SWIFT_CC(swift) bool swift_task_isMainExecutorImpl(SwiftExecutorRef executor);

/// Drain the main executor's queue, processing jobs enqueued on it; this
/// should never return.
SWIFT_RUNTIME_ATTRIBUTE_NORETURN SWIFT_CC(swift) void
  swift_task_asyncMainDrainQueueImpl(void);

/// Hand control of the current thread to the global executor until the
/// condition function returns `true`.  Support for this function is optional,
/// but you should assert or provide a dummy implementation if your executor
/// does not support it.
SWIFT_CC(swift) void
  swift_task_donateThreadToGlobalExecutorUntilImpl(bool (* _Nonnull condition)(void * _Nullable),
                                                   void * _Nullable conditionContext);

#ifdef __cplusplus
} // extern "C"
#endif

#endif // SWIFT_CONCURRENCY_EXECUTORIMPL_H
