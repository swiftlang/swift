/*===--- ExecutorTypes.h - C types for jobs and executors ----------*- C -*-===*
 *
 * This source file is part of the Swift.org open source project
 *
 * Copyright (c) 2014 - 2026 Apple Inc. and the Swift project authors
 * Licensed under Apache License v2.0 with Runtime Library Exception
 *
 * See https://swift.org/LICENSE.txt for license information
 * See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
 *
 *===----------------------------------------------------------------------===*
 *
 * The C view of the jobs and executor references that the Swift Concurrency
 * runtime passes to a global executor. `EmbeddedPlatform.h` and
 * `ExecutorImpl.h` both include this file, which must compile without C
 * library headers.
 *
 *===----------------------------------------------------------------------===*/

#ifndef SWIFT_EXECUTOR_TYPES_H
#define SWIFT_EXECUTOR_TYPES_H

#if !defined(__has_feature)
#define __has_feature(x) 0
#endif

#if !defined(__swift__) && __has_feature(ptrauth_calls)
#include <ptrauth.h>
#endif

// This must match __ptrauth_swift_heap_object_metadata in
// swift/Runtime/Config.h, which this header cannot include
#if !defined(__swift__) && __has_feature(ptrauth_calls)
#define __ptrauth_swift_heap_object_metadata                                   \
  __ptrauth(ptrauth_key_process_independent_data, 1, 0x6AE1)
#else
#define __ptrauth_swift_heap_object_metadata
#endif

#include <stdbool.h>
#include <stddef.h>

#if __has_feature(nullability)
#define EMBEDDED_SWIFT_NONNULL _Nonnull
#define EMBEDDED_SWIFT_NULLABLE _Nullable
#else
#define EMBEDDED_SWIFT_NONNULL
#define EMBEDDED_SWIFT_NULLABLE
#endif

#ifdef __cplusplus
extern "C" {
#endif

/// Represents a Swift type
typedef struct SwiftHeapMetadata SwiftHeapMetadata;

/// Jobs have flags, which currently encode a kind and a priority
typedef __UINT32_TYPE__ SwiftJobFlags;

typedef __SIZE_TYPE__ SwiftJobKind;
enum {
  SwiftTaskJobKind = 0,

  // Job kinds >= 192 are private to the implementation
  SwiftFirstReservedJobKind = 192,
};

typedef __SIZE_TYPE__ SwiftJobPriority;
enum {
  SwiftUserInteractiveJobPriority = 0x21, /* UI */
  SwiftUserInitiatedJobPriority   = 0x19, /* IN */
  SwiftDefaultJobPriority         = 0x15, /* DEF */
  SwiftUtilityJobPriority         = 0x11, /* UT */
  SwiftBackgroundJobPriority      = 0x09, /* BG */
  SwiftUnspecifiedJobPriority     = 0x00, /* UN */
};

/// Used by the Concurrency runtime to represent a job.  The `schedulerPrivate`
/// field may be freely used by the executor implementation.
typedef struct {
  SwiftHeapMetadata const *__ptrauth_swift_heap_object_metadata
      EMBEDDED_SWIFT_NONNULL metadata;
  __UINTPTR_TYPE__ refCounts;
  void * EMBEDDED_SWIFT_NULLABLE schedulerPrivate[2];
  SwiftJobFlags flags;
} __attribute__((aligned(2 * sizeof(void *)))) SwiftJob;

/// Get the kind of a job, by directly accessing the flags field.
static inline SwiftJobKind
swift_job_getKindFromFlags(SwiftJob * EMBEDDED_SWIFT_NONNULL job) {
  return (SwiftJobKind)(job->flags & 0xff);
}

/// Get the priority of a job, by directly accessing the flags field.
static inline SwiftJobPriority
swift_job_getPriorityFromFlags(SwiftJob * EMBEDDED_SWIFT_NONNULL job) {
  return (SwiftJobPriority)((job->flags >> 8) & 0xff);
}

/// Get a pointer to the scheduler private data.
static inline void * EMBEDDED_SWIFT_NULLABLE * EMBEDDED_SWIFT_NONNULL
swift_job_getPrivateData(SwiftJob * EMBEDDED_SWIFT_NONNULL job) {
  return &job->schedulerPrivate[0];
}

/// Swift's refcounted objects start with this header
typedef struct {
  SwiftHeapMetadata const *__ptrauth_swift_heap_object_metadata
      EMBEDDED_SWIFT_NONNULL metadata;
} SwiftHeapObject;

/// A reference to an executor consists of two words; the first is a pointer
/// which may or may not be to a Swift heap object.
typedef struct {
  SwiftHeapObject * EMBEDDED_SWIFT_NULLABLE identity;
  __UINTPTR_TYPE__ implementation;
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
swift_executor_ordinary(
    SwiftHeapObject * EMBEDDED_SWIFT_NULLABLE identity,
    SwiftExecutorWitnessTable * EMBEDDED_SWIFT_NULLABLE witnessTable) {
  return (SwiftExecutorRef){ identity,
                             (__UINTPTR_TYPE__)witnessTable
                             | SwiftExecutorOrdinaryKind };
}

/// Return a complex equality executor with the specified identity and
/// witness table.
static inline SwiftExecutorRef
swift_executor_complexEquality(
    SwiftHeapObject * EMBEDDED_SWIFT_NULLABLE identity,
    SwiftExecutorWitnessTable * EMBEDDED_SWIFT_NULLABLE witnessTable) {
  return (SwiftExecutorRef){ identity,
                             (__UINTPTR_TYPE__)witnessTable
                             | SwiftExecutorComplexEqualityKind };
}

/// Get the type of executor (ordinary vs complex equality).
static inline SwiftExecutorKind
swift_executor_getKind(SwiftExecutorRef executor) {
  const __UINTPTR_TYPE__ mask = ~(__UINTPTR_TYPE__)(sizeof(void *) - 1);
  return (SwiftExecutorKind)(executor.implementation & ~mask);
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
static inline SwiftHeapObject * EMBEDDED_SWIFT_NULLABLE
swift_executor_getIdentity(SwiftExecutorRef executor) {
  return executor.identity;
}

/// Test if an executor has a valid witness table.
static inline bool swift_executor_hasWitnessTable(SwiftExecutorRef executor) {
  return (!swift_executor_isGeneric(executor)
          && !swift_executor_isDefaultActor(executor));
}

/// Retrieve the witness table of an executor.
static inline const SwiftExecutorWitnessTable * EMBEDDED_SWIFT_NULLABLE
swift_executor_getWitnessTable(SwiftExecutorRef executor) {
  const __UINTPTR_TYPE__ mask = ~(__UINTPTR_TYPE__)(sizeof(void *) - 1);
  return (const SwiftExecutorWitnessTable *)(executor.implementation & mask);
}

/// Runs the job on the executor. The Swift Concurrency runtime defines this
/// function, and executors call it through `swift_job_runSynchronously`.
void _swift_job_run_c(SwiftJob * EMBEDDED_SWIFT_NONNULL job,
                      SwiftExecutorRef executor);

/// Execute the specified job while running on the specified executor.
static inline void
swift_job_runSynchronously(SwiftJob * EMBEDDED_SWIFT_NONNULL job,
                           SwiftExecutorRef executor) {
  _swift_job_run_c(job, executor);
}

#ifdef __cplusplus
} // extern "C"
#endif

#endif // SWIFT_EXECUTOR_TYPES_H
