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
/// field may be freely used by the executor implementation.  Note that this
/// is just the header of a larger struct that is used in the Concurrency
/// runtime; you are not expected to allocate these in an executor, and you
/// should only access the fields exposed here.
typedef struct {
  SwiftHeapMetadata const *__ptrauth_objc_isa_pointer metadata;
  uintptr_t refCounts;
  void *schedulerPrivate[2];
  SwiftJobFlags flags;
} __attribute__((aligned(2 * sizeof(void *)))) SwiftJob;

/// Get the kind of a job, by directly accessing the flags field.
static inline SwiftJobKind swift_job_getKind(SwiftJob *job) {
  return (SwiftJobKind)(job->flags & 0xff);
}

/// Get the priority of a job, by directly accessing the flags field.
static inline SwiftJobPriority swift_job_getPriority(SwiftJob *job) {
  return (SwiftJobPriority)((job->flags >> 8) & 0xff);
}

/// Allocate memory associated with a job.  Memory is allocated using a
/// per-task allocator and will be disposed of automatically when the
/// task is cleaned up.
///
/// N.B. **Requires that the job kind is SwiftJobKindTask.**
///
/// Note also that the allocator used is a bump pointer allocator with
/// stack discipline.  This means that you must deallocate in the
/// opposite order to the order in which you allocate memory.
void *swift_job_alloc(SwiftJob *job, size_t size);

/// Release memory allocated using `swift_job_alloc()`.
///
/// Note that the allocator used here is a bump pointer allocator with
/// stack discipline; you can only deallocate the last extant allocation.
/// Attempting to dealloc anything other than the most recent extant
/// allocation will result in a fatal error.
///
/// TL/DR: You must deallocate in the opposite order to the order
///        in which you allocated memory.
void swift_job_dealloc(SwiftJob *job, void *ptr);

/// Swift's refcounted objects start with this header
typedef struct {
  SwiftHeapMetadata const *__ptrauth_objc_isa_pointer metadata;
} SwiftHeapObject;

/// A reference to an executor consists of two words; the first is a pointer
/// which may or may not be to a Swift heap object.
typedef struct {
  SwiftHeapObject *identity;
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
swift_executor_ordinary(SwiftHeapObject *identity,
                        SwiftExecutorWitnessTable *witnessTable) {
  return (SwiftExecutorRef){ identity,
                             (uintptr_t)witnessTable
                             | SwiftExecutorOrdinaryKind };
}

/// Return a complex equality executor with the specified identity and
/// witness table.
static inline SwiftExecutorRef
swift_executor_complexEquality(SwiftHeapObject *identity,
                               SwiftExecutorWitnessTable *witnessTable) {
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
static inline SwiftHeapObject *
swift_executor_getIdentity(SwiftExecutorRef executor) {
  return executor.identity;
}

/// Test if an executor has a valid witness table.
static inline bool swift_executor_hasWitnessTable(SwiftExecutorRef executor) {
  return (!swift_executor_isGeneric(executor)
          && !swift_executor_isDefaultActor(executor));
}

/// Retrieve the witness table of an executor.
static inline const SwiftExecutorWitnessTable *
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

/// Execute the specified job while running on the specified executor.
static inline void swift_job_run(SwiftJob *job, SwiftExecutorRef executor) {
  extern void _swift_job_run_c(SwiftJob *job, SwiftExecutorRef executor);

  _swift_job_run_c(job, executor);
}

/// Names of clocks supported by the Swift time functions.
typedef int SwiftClockId;
enum {
  SwiftContinuousClock = 1,
  SwiftSuspendingClock = 2
};

/// An accurate timestamp, with *up to* nanosecond precision.
typedef struct {
  int64_t seconds;
  uint64_t nanoseconds;
} SwiftTime;

/// Represents a duration.
typedef struct {
  int64_t seconds;
  uint64_t nanoseconds;
} SwiftDuration;

/// A tolerance is a kind of duration
typedef SwiftDuration SwiftTolerance;

#define SWIFT_TOLERANCE_UNSPECIFIED ((SwiftTolerance){ 0, ~(uint64_t)0 })

static inline bool swift_tolerance_isUnspecified(SwiftTolerance tolerance) {
  return tolerance.nanoseconds == ~(uint64_t)0;
}

/// Get the current time from the specified clock
extern SwiftTime swift_time_now(SwiftClockId clock);

/// Get the resolution of the specified clock
extern SwiftDuration swift_time_getResolution(SwiftClockId clock);

/// Add a duration to a time
static inline SwiftTime swift_time_add(SwiftTime time, SwiftDuration duration) {
  SwiftTime result;

  result.seconds = time.seconds + duration.seconds;
  result.nanoseconds = time.nanoseconds + duration.nanoseconds;
  if (result.nanoseconds >= 1000000000ull) {
    uint64_t extraSeconds = result.nanoseconds / 1000000000ull;
    result.nanoseconds %= 1000000000ull;
    result.seconds += extraSeconds;
  }

  return result;
}

/// Subtract two times to get a duration
static inline SwiftDuration swift_time_sub(SwiftTime lhs, SwiftTime rhs) {
  SwiftDuration result;

  result.seconds = lhs.seconds - rhs.seconds;
  if (lhs.nanoseconds < rhs.nanoseconds) {
    result.seconds -= 1;
    result.nanoseconds = 1000000000ull + lhs.nanoseconds - rhs.nanoseconds;
  } else {
    result.nanoseconds = lhs.nanoseconds - rhs.nanoseconds;
  }

  return result;
}

/// Convert a time to a nanosecond count
static inline int64_t swift_time_toNanoseconds(SwiftTime time) {
  return time.seconds * 1000000000ll + time.nanoseconds;
}

/// Convert a duration to a nanosecond count
static inline int64_t swift_duration_toNanoseconds(SwiftDuration duration) {
  return duration.seconds * 1000000000ll + duration.nanoseconds;
}

/// Convert a tolerance to a nanosecond count
static inline int64_t swift_tolerance_toNanoseconds(SwiftTolerance tolerance) {
  return tolerance.seconds * 1000000000ll + tolerance.nanoseconds;
}

// -- Functions that executors must implement ----------------------------------

// Your executor MUST provide definitions of ALL of these functions,
// however some implementations may be trivial or just assert.
//
// Example minimal implementations for some of the functions are given
// in the comments below.

/// Enqueue a job on the global executor.
SWIFT_CC(swift) void swift_task_enqueueGlobalImpl(SwiftJob *job);

/// Enqueue a job on the global executor after a specified delay in
/// nanoseconds.  The job may be run on whichever clock is most convenient,
/// though existing implementations effectively use the suspending clock.
///
/// - nanoseconds: The minimum time to wait before running the job.
/// - job:         The job to schedule.
///
/// This gets used to implement `Task.sleep(nanoseconds:)` and the deprecated
/// `Task.sleep(_ duration:)` method.  We don't build those on top of the newer
/// `swift_task_enqueueGlobalWithDeadlineImpl()` function (or equivalently the
/// `Task.sleep(until deadline:...)` or `Task.sleep(for duration:...)` methods
/// because doing so would mean doing extra calculations in the runtime, and
/// many back-ends are going to be able to sleep for a number of nanoseconds
/// without fetching the current time first.
///
/// Some executors may choose to share the underlying implementation between
/// this function and `swift_task_enqueueGlobalWithDeadlineImpl()`, for instance
/// by doing
///
///     SWIFT_CC(swift) void
///     swift_task_enqueueGlobalWithDelayImpl(
///       uint64_t nanoseconds,
///       SwiftJob *job
///     ) {
///       SwiftTime now = swift_time_now(SwiftSuspendingClock);
///       SwiftDuration toWait = { 0, nanoseconds };
///       SwiftTime deadline = swift_time_add(now, toWait);
///       swift_task_enqueueGlobalWithDeadlineImpl(deadline,
///                                                SWIFT_TOLERANCE_UNSPECIFIED,
///                                                SwiftSuspendingClock,
///                                                job);
///     }
SWIFT_CC(swift) void swift_task_enqueueGlobalWithDelayImpl(
  uint64_t nanoseconds,
  SwiftJob *job
);

/// Enqueue a job on the global executor at a specified time.
///
/// - deadline:  The time at which the task should execute.
/// - tolerance: The maximum extra delay acceptable before the task executes.
/// - clock:     The clock associated with the deadline timestamp.
/// - job:       The job to schedule.
///
/// This gets used to implement Task.sleep(until deadline:, tolerance:, clock:)
/// and Task.sleep(for duration:, tolerance:, clock:), as well as the sleep
/// functions on `ContinuousClock` and `SuspendingClock`.
///
/// Some executors may choose to share the underlying implementation between
/// this function and `swift_task_enqueueGlobalWithDelayImpl()`, for instance
/// by doing
///
///    SWIFT_CC(swift) void
///    swift_task_enqueueGlobalWithDeadlineImpl(
///      SwiftTime deadline,
///      SwiftTolerance tolerance,
///      SwiftClockId clock,
///      SwiftJob *job
///    ) {
///      SwiftTime now = swift_time_now(clock);
///      SwiftDuration toWait = swift_time_sub(deadline, now);
///      uint64_t nanoseconds = swift_duration_toNanoseconds(toWait);
///      swift_task_enqueueGlobalWithDelayImpl(nanoseconds, job);
///    }
SWIFT_CC(swift) void swift_task_enqueueGlobalWithDeadlineImpl(
  SwiftTime deadline,
  SwiftTolerance tolerance,
  SwiftClockId clock,
  SwiftJob *job
);

/// Enqueue a job on the main executor.
///
/// The main executor is the executor responsible for jobs that are @MainActor.
/// If your executor does not need support for @MainActor, you can implement
/// this as:
///
///     SWIFT_CC(swift)
///     void swift_task_enqueueMainExecutorImpl(SwiftJob *job) {
///       swift_enqueueGlobalImpl(job);
///     }
SWIFT_CC(swift) void swift_task_enqueueMainExecutorImpl(SwiftJob *job);

/// Assert that the specified executor is the current executor.
SWIFT_CC(swift) void swift_task_checkIsolatedImpl(SwiftExecutorRef executor);

/// Get a reference to the main executor.
///
/// The main executor is the executor responsible for jobs that are @MainActor.
/// If your executor does not need support for @MainActor, you can implement
/// this as:
///
///     SWIFT_CC(swift)
///     SwiftExecutorRef swift_task_getMainExecutorImpl(void) {
///       return swift_executor_generic();
///     }
SWIFT_CC(swift) SwiftExecutorRef swift_task_getMainExecutorImpl(void);

/// Check if the specified executor is the main executor.
///
/// The main executor is the executor responsible for jobs that are @MainActor.
/// If your executor does not need support for @MainActor, you can implement
/// this as:
///
///     SWIFT_CC(swift)
///     bool swift_task_isMainExecutorImpl(SwiftExecutorRef executor) {
///       return swift_executor_isGeneric(executor)
///     }
SWIFT_CC(swift) bool swift_task_isMainExecutorImpl(SwiftExecutorRef executor);

/// Drain the global executor's queue, processing jobs enqueued on it; this
/// should never return.  This is called by the async main entry point.
SWIFT_RUNTIME_ATTRIBUTE_NORETURN SWIFT_CC(swift) void
  swift_task_asyncMainDrainQueueImpl(void);

/// Hand control of the current thread to the global executor until the
/// condition function returns `true`.  This function is not directly used
/// by the runtime or the compiler, but may be useful in embedded applications;
/// you can provide a dummy implementation or just raise a fatal error if you
/// do not plan on using swift_task_donateThreadToGlobalExecutorUntil().
SWIFT_CC(swift) void
  swift_task_donateThreadToGlobalExecutorUntilImpl(bool (*condition)(void *),
                                                   void *conditionContext);

#ifdef __cplusplus
} // extern "C"
#endif

#endif // SWIFT_CONCURRENCY_EXECUTORIMPL_H
