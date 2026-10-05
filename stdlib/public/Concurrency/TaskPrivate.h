//===--- TaskPrivate.h - Concurrency library internal interface -*- C++ -*-===//
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
// Internal functions for the concurrency library.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_CONCURRENCY_TASKPRIVATE_H
#define SWIFT_CONCURRENCY_TASKPRIVATE_H

#include "Error.h"
#include "TaskLocal.h"
#include "Tracing.h"
#include "swift/ABI/Metadata.h"
#include "swift/ABI/Task.h"
#include "swift/ABI/TaskStatus.h"
#include "swift/Runtime/Atomic.h"
#include "swift/Runtime/Concurrency.h"
#include "swift/Runtime/DispatchShims.h"
#include "swift/Runtime/EnvironmentVariables.h"
#include "swift/Runtime/Error.h"
#include "swift/Runtime/Exclusivity.h"
#include "swift/Runtime/HeapObject.h"
#include "swift/Threading/Mutex.h"
#include "swift/Threading/Thread.h"
#include "swift/Threading/ThreadSanitizer.h"
#include <atomic>
#include <new>

#define SWIFT_FATAL_ERROR swift_Concurrency_fatalError
#include "../runtime/StackAllocator.h"

// In embedded builds, pure virtual methods are replaced with
// __builtin_unreachable() stubs to avoid pulling in __cxa_pure_virtual.
#if SWIFT_CONCURRENCY_EMBEDDED
#define SWIFT_CONCURRENCY_ABSTRACT(decl) decl { __builtin_unreachable(); }
#else
#define SWIFT_CONCURRENCY_ABSTRACT(decl) decl = 0
#endif

namespace swift {

// Set to 1 to enable helpful debug spew to stderr
// If this is enabled, tests with `swift_task_debug_log` requirement can run.
#if 0
#define SWIFT_TASK_DEBUG_LOG_ENABLED 1
#define SWIFT_TASK_DEBUG_LOG(fmt, ...)                                         \
  fprintf(stderr, "[%#lx] [%s:%d](%s) " fmt "\n",                              \
          (unsigned long)Thread::current().platformThreadId(), __FILE__,       \
          __LINE__, __FUNCTION__, __VA_ARGS__)
#else
#define SWIFT_TASK_DEBUG_LOG_ENABLED 0
#define SWIFT_TASK_DEBUG_LOG(fmt, ...) (void)0
#endif

class AsyncTask;
class TaskGroup;
class ActiveTaskStatus;

/// Allocate task-local memory on behalf of a specific task,
/// not necessarily the current one.  Generally this should only be
/// done on behalf of a child task.
void *_swift_task_alloc_specific(AsyncTask *task, size_t size);

/// deallocate task-local memory on behalf of a specific task,
/// not necessarily the current one.  Generally this should only be
/// done on behalf of a child task.
void _swift_task_dealloc_specific(AsyncTask *task, void *ptr);

/// Given that we've already set the right executor as the active
/// executor, run the given job.  This does additional bookkeeping
/// related to the active task. actor and executorIdentity are used for emitting
/// tracing around the run.
void runJobInEstablishedExecutorContext(Job *job,
                                        SerialExecutorRef serialExecutor,
                                        TaskExecutorRef taskExecutor);

/// Adopt the voucher stored in `task`. This removes the voucher from the task
/// and adopts it on the current thread.
void adoptTaskVoucher(AsyncTask *task);

/// Restore the voucher for `task`. This un-adopts the current thread's voucher
/// and stores it back into the task again.
void restoreTaskVoucher(AsyncTask *task);

/// Initialize the async let storage for the given async-let child task.
void asyncLet_addImpl(AsyncTask *task, AsyncLet *asyncLet,
                      bool didAllocateInParentTask);

/// Clear the active task reference for the current thread.
AsyncTask *_swift_task_clearCurrent();
/// Set the active task reference for the current thread.
AsyncTask *_swift_task_setCurrent(AsyncTask *newTask);

/// Options controlling `_swift_task_getCancellationScope`.
enum class GetCancellationScopeFlags : uintptr_t {
  None = 0,
  /// Look past any `TaskCancellationShieldRecord` encountered before the
  /// innermost scope. By default a shield installed above the innermost
  /// scope masks it and the search returns nullptr.
  IgnoringTaskCancellationShield = 1 << 0,
};

/// Return the innermost `TaskCancellationScopeRecord` in `task`'s records,
/// respecting shield masking, or nullptr if none is visible.
///
/// The caller must have already checked `HasTaskCancellationScope` on the
/// task's status; this function takes the record lock unconditionally.
///
/// A `TaskCancellationShieldRecord` seen before any cancellation scope
/// means the call site is inside a shield deeper than the innermost scope
/// - the search returns nullptr so callers observe the task "as-if" no
/// scope were installed, matching "as-if child task" semantics for
/// `withDeadline` etc. Pass `IgnoringTaskCancellationShield` to bypass
/// that masking.
class TaskCancellationScopeRecord;
TaskCancellationScopeRecord *
_swift_task_getCancellationScope(AsyncTask *task,
                                 GetCancellationScopeFlags flags =
                                     GetCancellationScopeFlags::None);

/// Cancel the task group and all the child tasks that belong to `group`.
///
/// The caller must guarantee that this is called while holding the owning
/// task's status record lock.
void _swift_taskGroup_cancel(TaskGroup *group, size_t reason);

/// Cancel the task group and all the child tasks that belong to `group`.
///
/// The caller must guarantee that this is called from the owning task.
void _swift_taskGroup_cancel_unlocked(TaskGroup *group,
                                      AsyncTask *owningTask,
                                      size_t reason);

/// Remove the given task from the given task group.
///
/// This is an internal API; clients outside of the TaskGroup implementation
/// should generally use a higher-level function.
void _swift_taskGroup_detachChild(TaskGroup *group, AsyncTask *child);

/// Tell TSan about an acquiring load
inline void _swift_tsan_acquire(void *addr) {
  swift::tsan::acquire(addr);
}
/// Tell TSan about a releasing store
inline void _swift_tsan_release(void *addr) {
  swift::tsan::release(addr);
}
/// Tell TSan about a consuming load
inline void _swift_tsan_consume(void *addr) {
  // TSan doesn't support consume, so pretend it's an acquire.
  //
  // Note that that means that TSan won't generate errors for non-dependent
  // reads, so this isn't entirely safe if you're relying solely on TSan to
  // spot bugs.
  swift::tsan::acquire(addr);
}

/// Special values used with DispatchQueueIndex to indicate the global and main
/// executors.
#define DISPATCH_QUEUE_GLOBAL_EXECUTOR (void *)1

#if SWIFT_CONCURRENCY_ENABLE_DISPATCH
inline SerialExecutorWitnessTable *
_swift_task_getDispatchQueueSerialExecutorWitnessTable() {
  extern SerialExecutorWitnessTable wtable
    SWIFT_ASM_LABEL_WITH_PREFIX("$ss17DispatchQueueShimCScfsWP");
  return &wtable;
}
#endif

// The task listed as argument is escalated to a new priority. Pass that
// information along to the executor that it is enqueued into.
SWIFT_CC(swift)
void
swift_executor_escalate(SerialExecutorRef executor, AsyncTask *task, JobPriority newPriority);

/*************** Methods for Status records manipulation ******************/

/// This function grabs the status record lock of the input task and invokes
/// `fn` while holding the StatusRecordLock of the input task.
///
/// Prefer to use the helper functions below when possible
/// because they can proactively use lockless algorithms. This
/// is only to be used when you *must* hold the lock for some
/// operation and there is no better way to accomplish that.
///
/// If the client of withStatusRecordLock has already loaded the status of the
/// task, they may pass it into this function to avoid a double-load.
///
/// The input `fn` is invoked once while holding the status record lock.
///
/// The optional `statusUpdate` is invoked while releasing the StatusRecordLock
/// from the ActiveTaskStatus so that callers may make additional modifications
/// to ActiveTaskStatus flags. `statusUpdate` can be called multiple times in a
/// RMW loop and so much be idempotent.
void withStatusRecordLock(
    AsyncTask *task, ActiveTaskStatus status,
    llvm::function_ref<void(ActiveTaskStatus)> fn,
    llvm::function_ref<void(ActiveTaskStatus, ActiveTaskStatus &)>
        statusUpdate = nullptr);

/// Add a status record to the input task.
///
/// Clients can optionally pass in the status of the task if they have already
/// done the load on it already or if they require the oldStatus on the task
/// prior to the atomic ActiveTaskStatus update that addStatusRecord will
/// perform. This status will be updated with the last status on the task prior
/// to updating it with the new status if the input function_ref allows so.
///
/// Clients can optionally pass in the status of the task if they have already
/// done the load on it or if they require the oldStatus on the task prior to
/// the atomic ActiveTaskStatus update that addStatusRecord will do. This status
/// will be updated with the last status on the task prior to updating it with
/// the new status if the input function_ref allows so.
///
/// This function also takes in a function_ref which is given the old
/// ActiveTaskStatus on the task and a reference to the new ActiveTaskStatus
/// that is to be set on the task that we are adding the record to.
///
/// This can be used by the function to
/// (1) to determine if the current status of the task permits adding the status
/// record
/// (2) modify the active task status that is to be set if needed
///
/// If the function_ref returns false, the status record is not added to the
/// task. This function_ref may be called multiple times and must be idempotent.
/// The new status passed to `fn` is freshly derived from the current status and
/// does not include modifications made by previous runs through the loop.
///
/// A convenience overload is also provided for clients who have not done the
/// load on the task status prior to adding the record
SWIFT_CC(swift)
bool addStatusRecord(AsyncTask *task, TaskStatusRecord *record,
     llvm::function_ref<bool(ActiveTaskStatus, ActiveTaskStatus&)> testAddRecord);

SWIFT_CC(swift)
bool addStatusRecord(AsyncTask *task, TaskStatusRecord *record,
     ActiveTaskStatus& taskStatus,
     llvm::function_ref<bool(ActiveTaskStatus, ActiveTaskStatus&)> testAddRecord);

/// Convenience functions for clients who want to just add a record to the task
/// on the current thread
SWIFT_CC(swift)
bool addStatusRecordToSelf(TaskStatusRecord *record,
     llvm::function_ref<bool(ActiveTaskStatus, ActiveTaskStatus&)> testAddRecord);

SWIFT_CC(swift)
bool addStatusRecordToSelf(TaskStatusRecord *record,  ActiveTaskStatus& taskStatus,
     llvm::function_ref<bool(ActiveTaskStatus, ActiveTaskStatus&)> testAddRecord);

/// Remove the status record from input task which may not be the current task.
/// This may be called asynchronously from the current task.  After this call
/// returns, the record's memory can be freely modified or deallocated.  The
/// record must be registered with the task. If it isn't, this function will
/// crash.
///
/// The given record need not be the last record added to
/// the task, but the operation may be less efficient if not.
///
/// This function also takes in a function_ref which is given the old
/// ActiveTaskStatus on the task and a reference to the new ActiveTaskStatus
/// that is to be set on the task that we are removing the record from. It may
/// modify the new ActiveTaskStatus that is to be set on the task. This function
/// may be called multiple times inside a RMW loop and must be therefore be
/// idempotent. The new status passed to `fn` is freshly derived from the
/// current status and does not include modifications made by previous runs
/// through the loop
SWIFT_CC(swift)
void removeStatusRecord(AsyncTask *task, TaskStatusRecord *record, ActiveTaskStatus& status,
     llvm::function_ref<void(ActiveTaskStatus, ActiveTaskStatus&)>fn = nullptr);

SWIFT_CC(swift)
void removeStatusRecord(AsyncTask *task, TaskStatusRecord *record,
     llvm::function_ref<void(ActiveTaskStatus, ActiveTaskStatus&)>fn = nullptr);

/// Similar to the above functions, attempt to remove record from
/// task. If condition returns false, record is considered invalid
/// and won't be attempted to be removed. It may be called multiple
/// times to ensure it is checked atomically with the removal.
SWIFT_CC(swift)
bool removeStatusRecordIf(
    AsyncTask *task, TaskStatusRecord *record, ActiveTaskStatus &status,
    llvm::function_ref<void(ActiveTaskStatus, ActiveTaskStatus &)> fn,
    llvm::function_ref<bool(ActiveTaskStatus)> condition);

/// Update the status record by scanning through all records and removing
/// those which match the condition. This can also be used to inspect
/// "remaining" records.
///
/// The `whenRemoved` function is called after a record indicated by
/// `condition` returning `true` was removed. It can be used to e.g.
/// deallocate or perform additional cleanup.
///
/// The `updateStatus` function can be used to update the status after all
/// records have been inspected. It may be invoked multiple times inside a RMW
/// loop, therefore must be idempotent.
SWIFT_CC(swift)
void removeStatusRecordWhere(
    AsyncTask *task,
    ActiveTaskStatus& status,
    llvm::function_ref<bool(ActiveTaskStatus, TaskStatusRecord*)> condition,
    llvm::function_ref<void(ActiveTaskStatus, ActiveTaskStatus&)>updateStatus = nullptr);

SWIFT_CC(swift)
void removeStatusRecordWhere(
    AsyncTask *task,
    llvm::function_ref<bool(ActiveTaskStatus, TaskStatusRecord*)> condition,
    llvm::function_ref<void(ActiveTaskStatus, ActiveTaskStatus&)>updateStatus = nullptr);

/// Remove and return a status record of the given type. This function removes a
/// singlw record, and leaves subsequent records as-is if there are any.
/// Returns `nullptr` if there are no matching records.
///
/// NOTE: When using this function with new record types, make sure to provide
/// an explicit instantiation in TaskStatus.cpp.
template <typename TaskStatusRecordT>
SWIFT_CC(swift)
TaskStatusRecordT* popStatusRecordOfType(AsyncTask *task);

/// Remove a status record from the current task. This must be called
/// synchronously with the task.
SWIFT_CC(swift)
void removeStatusRecordFromSelf(TaskStatusRecord *record, ActiveTaskStatus &status,
     llvm::function_ref<void(ActiveTaskStatus, ActiveTaskStatus&)>fn = nullptr);

SWIFT_CC(swift)
void removeStatusRecordFromSelf(TaskStatusRecord *record,
     llvm::function_ref<void(ActiveTaskStatus, ActiveTaskStatus&)>fn = nullptr);

/// Update the specified input status record while holding the status record
/// lock of the task. The status record must already be registered with the
/// task - if it isn't, this API provides no additional protections.
///
/// This function also takes in a function_ref which is given the old
/// ActiveTaskStatus on the task and a reference to the new ActiveTaskStatus
/// that is to be set on the task when we are unlocking the task status record
/// lock. It may modify the new ActiveTaskStatus that is to be set on the task.
/// This function may be called multiple times inside a RMW loop and must be
/// therefore be idempotent. The new status passed to `fn` is freshly derived
/// from the current status and does not include modifications made by previous
/// runs through the loop
SWIFT_CC(swift)
void updateStatusRecord(
    AsyncTask *task, TaskStatusRecord *record,
    llvm::function_ref<void(ActiveTaskStatus)> updateRecord,
    ActiveTaskStatus &status,
    llvm::function_ref<void(ActiveTaskStatus, ActiveTaskStatus &)> fn =
        nullptr);

/// A helper function for updating a new child task that is created with
/// information from the parent or the group that it was going to be added to.
SWIFT_CC(swift)
void updateNewChildWithParentAndGroupState(AsyncTask *child,
                                           ActiveTaskStatus parentStatus,
                                           TaskGroup *group);

// ==== ------------------------------------------------------------------------

namespace {

/// The layout of a context to call one of the following functions:
///
///   @_silgen_name("swift_task_future_wait")
///   func _taskFutureGet<T>(_ task: Builtin.NativeObject) async -> T
///
///   @_silgen_name("swift_task_future_wait_throwing")
///   func _taskFutureGetThrowing<T>(_ task: Builtin.NativeObject) async throws -> T
///
///   @_silgen_name("swift_asyncLet_get")
///   func _asyncLet_get<T>(_ task: Builtin.RawPointer) async -> T
///
///   @_silgen_name("swift_asyncLet_get_throwing")
///   func _asyncLet_get_throwing<T>(_ task: Builtin.RawPointer) async throws -> T
///
///   @_silgen_name("swift_taskGroup_wait_next_throwing")
///   func _taskGroupWaitNext<T>(group: Builtin.RawPointer) async throws -> T?
///
///   @_silgen_name("swift_taskGroup_waitAll")
///   func _taskGroupWaitAll<T>(
///     group: Builtin.RawPointer,
///     bodyError: Swift.Error?
///   ) async throws -> T?
///
class TaskFutureWaitAsyncContext : public AsyncContext {
public:
  // The ABI reserves three words of storage for these contexts, which
  // we currently use as follows.  These fields are not accessed by
  // generated code; they're purely internal to the runtime, and only
  // when the calling task actually suspends.
  //
  // (If you think three words is an odd choice, one of them used to be
  // the context flags.)
  SwiftError *errorResult;
  OpaqueValue *successResultPointer;
  void *_reserved;

  void fillWithSuccess(AsyncTask::FutureFragment *future) {
    OpaqueValue *src = future->getStoragePtr();
    OpaqueValue *result = successResultPointer;
    future->getResultType().vw_initializeWithCopy(result, src);
  }

  void fillWithError(AsyncTask::FutureFragment *future) {
    fillWithError(future->getError());
  }
  void fillWithError(SwiftError *error) {
    errorResult = error;
    swift_errorRetain(error);
  }
};

} // end anonymous namespace

/// The current state of a task's status records.
///
/// The runtime needs to track the following state about the task in within the
/// same atomic:
///
/// * The current status of the task (whether it is running, waiting in a queue,
/// waiting on another task to finish, etc.)
/// * The cancellation state
/// * The locked state of the status record
/// * The current maximum priority of the task
/// * The identity of the thread executing the task
/// * Pointer to head of TaskStatusRecords
///
/// It is important for all of this information to be in the same atomic so that
/// when the task's state changes, the information is visible to all threads that
/// may be modifying the task, allowing the algorithm to eventually converge.
///
/// Some changes to task state, like cancellation are local to the concurrency
/// runtime library but others like priority escalation require deeper integration
/// with the OS in order to have the intended side effects. On Darwin, Swift
/// Concurrency Tasks runs on dispatch's queues. As such, we need to use an
/// encoding of thread identity vended by libdispatch called dispatch_lock_t,
/// and a futex-style dispatch API in order to escalate the priority of a thread.
/// When the thread gives up running a task, the priority escalation that may
/// have been applied on the thread, is removed. Henceforth, the dispatch_lock_t
/// tracked in the ActiveTaskStatus will be called the ExecutionLock.
///
/// In order to keep the entire ActiveTaskStatus size to 2 words, the thread
/// identity is only tracked on platforms which can support 128 bit atomic
/// operations. The ActiveTaskStatus's layout has thus been changed to have the
/// following layout depending on the system configuration supported:
///
/// 32 bit systems with SWIFT_CONCURRENCY_ENABLE_PRIORITY_ESCALATION=1
///
///          Flags               Execution Lock           Unused           TaskStatusRecord *
/// |----------------------|----------------------|----------------------|-------------------|
///          32 bits                32 bits                32 bits              32 bits
///
/// 64 bit systems with SWIFT_CONCURRENCY_ENABLE_PRIORITY_ESCALATION=1
///
///         Flags              Execution Lock      TaskStatusRecord *
/// |----------------------|-------------------|----------------------|
///          32 bits                32 bits              64 bits
///
/// 32 bit systems with SWIFT_CONCURRENCY_ENABLE_PRIORITY_ESCALATION=0
///
///          Flags             TaskStatusRecord *
/// |----------------------|----------------------|
///          32 bits                32 bits
//
/// 64 bit systems with SWIFT_CONCURRENCY_ENABLE_PRIORITY_ESCALATION=0
///
///         Flags                  Unused            TaskStatusRecord *
/// |----------------------|----------------------|---------------------|
///         32 bits                 32 bits                64 bits
///
/// Note: This layout is intentional so that we can then do 64 bit atomics on
/// just the top 64 bits of atomic state, including escalation of a thread. We
/// only need to do double wide atomics if we need to reach for the
/// StatusRecord pointers and therefore have to update the flags at the same
/// time.
///
/// Size requirements:
///     On 64 bit systems or if SWIFT_CONCURRENCY_ENABLE_PRIORITY_ESCALATION=1,
///     the field is 16 bytes long.
///
///     Otherwise, it is 8 bytes long.
///
/// Alignment requirements:
///     On 64 bit systems or if SWIFT_CONCURRENCY_ENABLE_PRIORITY_ESCALATION=1,
///     this 16-byte field needs to be 16 byte aligned to be able to do aligned
///     atomic stores field.
///
///     On all other systems, it needs to be 8 byte aligned for the atomic
///     stores.
///
///     As a result of varying alignment needs, we've marked the class as
///     needing 2-word alignment but on arm64_32 with
///     SWIFT_CONCURRENCY_ENABLE_PRIORITY_ESCALATION=1, 16 byte alignment is
///     achieved through careful arrangement of the storage for this in the
///     AsyncTask::PrivateStorage. The additional alignment requirements are
///     enforced by static asserts below.
class alignas(2 * sizeof(void*)) ActiveTaskStatus {
  enum : uint32_t {
    /// The max priority of the task. This is always >= basePriority
    /// in the task. Currently, this stores an entire TaskPriority even
    /// though there are few valid values. If more bits are needed here
    /// in the future, this can be compressed to only the valid values.
    PriorityMask = 0xFF,

    /// Has the task been cancelled?
    IsCancelled = 0x100,

    /// Whether the task status is "locked", meaning that further
    /// accesses need to wait on the task status record lock. This is separate
    /// from the drain lock of the task.
    IsStatusRecordLocked = 0x200,

    /// Whether the running priority has been escalated above the previous max
    /// priority in the task status
    IsEscalated = 0x400,

#if !SWIFT_CONCURRENCY_ENABLE_PRIORITY_ESCALATION
    /// Whether the task is actively running. On systems which cannot track the
    /// identity of the drainer (see above), we use one bit in the flags to
    /// track
    /// whether or not task is running. Otherwise, the drain lock tells us
    /// whether or not it is running.
    IsRunning = 0x800,
#endif
    /// The Job portion of this Task is directly enqueued somewhere -
    /// either in the default executor pool, an actor, or another executor.
    ///
    /// Task is a subclass of Job so the Task object itself may be enqueued
    /// on an executor. A TaskStealer Job may also refer to a Task and
    /// invoke it. If this bit is set, the Task's Job is currently enqueued
    /// on an executor and may not be reused. If it is unset, the Task's
    /// Job is currently unused and may be mutated and enqueued elsewhere.
    ///
    /// This bit may be set or unset at any point in the Task's lifecycle and
    /// is orthgonal to if the Task has an EnqueuedOnExecutor dependency record.
    IsDirectlyEnqueued = 0x1000,
    /// Task has been completed.  This is purely used to enable an assertion
    /// that the task is completed when we destroy it.
    IsComplete = 0x2000,

    /// The task has a dependency and therefore, also a
    /// TaskDependencyStatusRecord in the status record list.
    HasTaskDependency = 0x4000,

    /// Whether the task has a task executor preference record stored.
    /// By storing this flag we can avoid taking the task record lock
    /// when running a task when we know there is no task executor preference
    /// to look for.
    ///
    /// If a task has a task executor preference, we must find the record and
    /// use the task executor preference when we'd otherwise be running on
    /// the generic global pool.
    HasTaskExecutorPreference = 0x8000,

    HasActiveTaskCancellationShield = 0x10000,

    // If this bit is set, there is more than one object (either this one
    // intrusively linked or an AsyncTaskStealer) that has the current
    // StealerExclusionValue. In order to run this Task, the exclusion value
    // must be incremented to invalidate those other objects. At that time,
    // this bit must also be reset. If this bit is not set, the only other
    // enqueued objects that may exist that refer to this Task have older
    // exclusion values so it is safe to run this Task without incrementing it.
    HasActiveStealers = 0x20000,
    // Swift Tasks typically don't need an additional retain while they
    // are enqueued. However, if a stealer is enqueued and the original
    // Task remains intrusively linked on a low priority queue, it may stay
    // there after the Task completes and is otherwise forgotten about by
    // anyone other than the executor. To ensure it is not disposed until
    // after it is finally dequeued, an additional retain is added when
    // the first stealer is enqueued which this bit tracks. The balancing
    // release happens when the intrusively linked Task is dequeued.
    HasRetainForIntrusiveLinkage = 0x40000,

    /// Whether the task has at least one `TaskCancellationScopeRecord` in its
    /// status record list. We use this to avoid scanning for a scope in
    /// records storage if we know for certain there is none. This matters
    /// because cancellation checking may happen on hot-paths, so it is worth
    /// the flag.
    HasTaskCancellationScope = 0x80000,

#if SWIFT_CONCURRENCY_ENABLE_PRIORITY_ESCALATION
    /// The Task's intrusive link or a Stealer may only run if its
    /// exclusion value is equal to this value. This number increases
    /// only when running a Task after it was escalated while it
    /// was dependent on the Dispatch default global executor.
    ///
    /// There are only 5 values of JobPriority and this value can only
    /// increase at most once for each priority transition so this
    /// could just use 3 bits (along with PriorityMask) if needed.
    StealerExclusionShift = 20,
    StealerExclusionMax = 0xF,
    StealerExclusionMask = StealerExclusionMax << StealerExclusionShift,
#endif

    /// Whether the task has at least one `TaskDeadlineStatusRecord` in its
    /// status record list. By storing this flag we can avoid taking the task
    /// record lock and walking the record chain when querying the innermost
    /// deadline (e.g. `Task.currentDeadline(for:)`), which is a hot path for
    /// code that composes with `withDeadline` but rarely actually installs
    /// one.
    HasDeadline = 0x1000000,

    /// Additional bit that qualifies `IsCancelled`: when set, the cancellation
    /// was caused by a deadline expiring (via `cancel(reason: .deadlineExpired)`).
    /// When unset while `IsCancelled` is set, the reason is `.unspecified`.
    /// Meaningless unless `IsCancelled` is set. First-cancel-wins on the reason;
    /// once set it is not modified by subsequent cancels.
    CancelReasonDeadlineExpired = 0x2000000,
  };

  /// Mirrors `CancellationError.Reason`'s raw values on the Swift side.
  /// Kept as an enum (rather than bare integers) so that adding a new
  /// reason forces a `-Wswitch` warning at the `withCancelled(size_t)`
  /// call site below.
  enum class CancellationReason : size_t {
    Unspecified = 0,
    DeadlineExpired = 1,
  };

  // Note: this structure is mirrored by ActiveTaskStatusWithEscalation and
  // ActiveTaskStatusWithoutEscalation in
  // include/swift/RemoteInspection/RuntimeInternals.h. Any changes to the layout here
  // must also be made there.
#if SWIFT_CONCURRENCY_ENABLE_PRIORITY_ESCALATION && SWIFT_POINTER_IS_4_BYTES
  uint32_t Flags;
  dispatch_lock_t ExecutionLock;
  LLVM_ATTRIBUTE_UNUSED uint32_t Unused = {};
#elif SWIFT_CONCURRENCY_ENABLE_PRIORITY_ESCALATION && SWIFT_POINTER_IS_8_BYTES
  uint32_t Flags;
  dispatch_lock_t ExecutionLock;
#elif !SWIFT_CONCURRENCY_ENABLE_PRIORITY_ESCALATION && SWIFT_POINTER_IS_4_BYTES
  uint32_t Flags;
#else /* !SWIFT_CONCURRENCY_ENABLE_PRIORITY_ESCALATION && SWIFT_POINTER_IS_8_BYTES */
  uint32_t Flags;
  LLVM_ATTRIBUTE_UNUSED uint32_t Unused = {};
#endif
  TaskStatusRecord *Record;

#if SWIFT_CONCURRENCY_ENABLE_PRIORITY_ESCALATION
  ActiveTaskStatus(TaskStatusRecord *record, uintptr_t flags)
    : Flags(flags), ExecutionLock(DLOCK_OWNER_NULL), Record(record) {}

  ActiveTaskStatus(TaskStatusRecord *record, uintptr_t flags, dispatch_lock_t executionLockValue)
    : Flags(flags), ExecutionLock(executionLockValue), Record(record) {}

#else
  ActiveTaskStatus(TaskStatusRecord *record, uintptr_t flags)
    : Flags(flags), Record(record) {}
#endif

  ActiveTaskStatus withFlags(uintptr_t flags) const {
#if SWIFT_CONCURRENCY_ENABLE_PRIORITY_ESCALATION
    return ActiveTaskStatus(Record, flags, ExecutionLock);
#else
    return ActiveTaskStatus(Record, flags);
#endif
  }

public:
#ifdef __GLIBCXX__
  /// We really don't want to provide this constructor, but in old
  /// versions of libstdc++, std::atomic<T>::load incorrectly requires
  /// the type to be default-constructible.
  ActiveTaskStatus() = default;
#endif

#if SWIFT_CONCURRENCY_ENABLE_PRIORITY_ESCALATION
  constexpr ActiveTaskStatus(JobPriority priority)
      : Flags(uintptr_t(priority)), ExecutionLock(DLOCK_OWNER_NULL), Record(nullptr) {}
#else
  constexpr ActiveTaskStatus(JobPriority priority)
      : Flags(uintptr_t(priority)), Record(nullptr) {}
#endif

  // Getters / setters for flags (in flag order)

  // Priority
  JobPriority getStoredPriority() const {
    return JobPriority(Flags & PriorityMask);
  }
  /// Creates a new active task status for a task with the specified priority
  /// and masks away any existing priority related flags on the task status. All
  /// other flags about the task are unmodified. This is only safe to use when
  /// we know that the task we're modifying is not running.
  ActiveTaskStatus withNewPriority(JobPriority priority) const {
#if SWIFT_CONCURRENCY_ENABLE_PRIORITY_ESCALATION
    assert(ExecutionLock == DLOCK_OWNER_NULL);
#endif
    return withFlags((Flags & ~PriorityMask) | uintptr_t(priority));
  }
  ActiveTaskStatus withEscalatedPriority(JobPriority priority) const {
    assert(priority > getStoredPriority());
    return withFlags((Flags & ~PriorityMask) | IsEscalated |
                     uintptr_t(priority));
  }

  // IsCancelled
  /// Is the task currently cancelled?
  /// This does take into account cancellation shields, i.e. while a shield is
  /// active this function will always return 'false'.
  bool isCancelled(bool ignoreShield = false) const {
    return (Flags & IsCancelled) &&
           (ignoreShield || !(Flags & HasActiveTaskCancellationShield));
  }
  bool isCancelledIgnoringShield() const { return Flags & IsCancelled; }
  ActiveTaskStatus withCancelled() const {
    return withFlags(Flags | IsCancelled);
  }
  ActiveTaskStatus withCancelled(size_t reason) const {
    // Reasons are set only once, when transitioning from not-cancelled to
    // cancelled. Callers should have checked `!isCancelled()` first.
    uintptr_t reasonBits = 0;
    switch (CancellationReason(reason)) {
    case CancellationReason::Unspecified:
      break;
    case CancellationReason::DeadlineExpired:
      reasonBits = uintptr_t(CancelReasonDeadlineExpired);
      break;
    }
    return withFlags(Flags | IsCancelled | reasonBits);
  }

  // CancellationReason
  /// Read the cancellation-reason bit. Meaningful only when `isCancelled()`
  /// (or `isCancelledIgnoringShield()`) returns true. Returns the raw
  /// value of `CancellationError.Reason`.
  ///
  /// The explicit `switch` below is exhaustive on purpose,
  /// so we revisit this func when adding more reasons.
  size_t getCancellationReason() const {
    CancellationReason reason = (Flags & CancelReasonDeadlineExpired) != 0
                                    ? CancellationReason::DeadlineExpired
                                    : CancellationReason::Unspecified;
    switch (reason) {
    case CancellationReason::Unspecified:
      return size_t(CancellationReason::Unspecified);
    case CancellationReason::DeadlineExpired:
      return size_t(CancellationReason::DeadlineExpired);
    }
  }

  // IsStatusRecordLocked
  /// Does some thread hold the status record lock?
  bool isStatusRecordLocked() const { return Flags & IsStatusRecordLocked; }
  ActiveTaskStatus withStatusRecordLocked() const {
    assert(!isStatusRecordLocked());
    return withFlags(Flags | IsStatusRecordLocked);
  }
  ActiveTaskStatus withoutStatusRecordLocked() const {
    assert(isStatusRecordLocked());
    return withFlags(Flags & ~IsStatusRecordLocked);
  }

  // IsEscalated
  bool isStoredPriorityEscalated() const { return Flags & IsEscalated; }
  ActiveTaskStatus withoutStoredPriorityEscalation() const {
#if SWIFT_CONCURRENCY_ENABLE_PRIORITY_ESCALATION
    return ActiveTaskStatus(Record, Flags & ~IsEscalated, ExecutionLock);
#else
    return ActiveTaskStatus(Record, Flags & ~IsEscalated);
#endif
  }

  // IsRunning
  /// Is the task currently running? Also known as whether it is drain locked.
  bool isRunning() const {
#if SWIFT_CONCURRENCY_ENABLE_PRIORITY_ESCALATION
    return dispatch_lock_is_locked(ExecutionLock);
#else
    return Flags & IsRunning;
#endif
  }
  ActiveTaskStatus withRunning(bool isRunning) const {
#if SWIFT_CONCURRENCY_ENABLE_PRIORITY_ESCALATION
  if (isRunning) {
    assert(!dispatch_lock_is_locked(ExecutionLock));
    return ActiveTaskStatus(Record, Flags, dispatch_lock_value_for_self());
  } else {
    return ActiveTaskStatus(Record, Flags, ExecutionLock & ~DLOCK_OWNER_MASK);
  }
#else
    return ActiveTaskStatus(Record, isRunning ? (Flags | IsRunning)
                                              : (Flags & ~IsRunning));
#endif
  }

  // IsDirectlyEnqueued
  bool isDirectlyEnqueued() const { return Flags & IsDirectlyEnqueued; }
  ActiveTaskStatus withIntrusivelyLinked() const {
    return withFlags(Flags | IsDirectlyEnqueued);
  }
  ActiveTaskStatus withoutIntrusivelyLinked() const {
    return withFlags(Flags & ~IsDirectlyEnqueued);
  }
  static bool atomicRemoveIntrusivelyLinked(char *storage) {
    // To do an atomic AND, we have to use the underlying storage and
    // cast it. This cast could be done in a simpler way since Flags
    // should always be at the start and should always be a uint32_t.
    // But to be more resilient, I get the offsets and types correctly
    auto *flags =
        reinterpret_cast<std::atomic<decltype(ActiveTaskStatus::Flags)> *>(
            storage + offsetof(ActiveTaskStatus, Flags));
    // Release memory ordering so that other threads can not decide to reuse
    // this Task's intrusive linkage before all of our earlier uses have
    // completed
    auto oldFlags =
        flags->fetch_and(~(IsDirectlyEnqueued | HasRetainForIntrusiveLinkage),
                         std::memory_order_release);

    // Tell the caller if it needs to do a release on the Task
    return (oldFlags & HasRetainForIntrusiveLinkage) != 0;
  }

  // IsComplete
  bool isComplete() const { return Flags & IsComplete; }
  ActiveTaskStatus withComplete() const {
    return withFlags(Flags | IsComplete);
  }

  // HasTaskDependency
  /// Is there a dependencyRecord in the linked list of status records?
  bool hasTaskDependency() const { return Flags & HasTaskDependency; }
  ActiveTaskStatus withTaskDependency() const {
    return withFlags(Flags | HasTaskDependency);
  }
  ActiveTaskStatus withoutTaskDependency() const {
    return withFlags(Flags & ~HasTaskDependency);
  }

  // HasTaskExecutorPreference
  /// Is there a task preference record in the linked list of status records?
  bool hasTaskExecutorPreference() const {
    return Flags & HasTaskExecutorPreference;
  }
  ActiveTaskStatus withTaskExecutorPreference() const {
    return withFlags(Flags | HasTaskExecutorPreference);
  }
  ActiveTaskStatus withoutTaskExecutorPreference() const {
    return withFlags(Flags & ~HasTaskExecutorPreference);
  }

  // HasActiveTaskCancellationShield
  bool hasCancellationShield() const {
    return Flags & HasActiveTaskCancellationShield;
  }
  ActiveTaskStatus withCancellationShield() const {
    return withFlags(Flags | HasActiveTaskCancellationShield);
  }
  ActiveTaskStatus withoutCancellationShield() const {
    return withFlags(Flags & ~HasActiveTaskCancellationShield);
  }

  // HasActiveStealers
  bool hasActiveStealers() const { return Flags & HasActiveStealers; }
  ActiveTaskStatus withActiveStealers() const {
    return withFlags(Flags | HasActiveStealers);
  }
  ActiveTaskStatus withoutActiveStealers() const {
    return withFlags(Flags & ~HasActiveStealers);
  }

  // HasRetainForIntrusiveLinkage
  bool hasRetainForIntrusiveLinkage() const {
    return Flags & HasRetainForIntrusiveLinkage;
  }
  ActiveTaskStatus withRetainForIntrusiveLinkage() const {
    return withFlags(Flags | HasRetainForIntrusiveLinkage);
  }
  ActiveTaskStatus withoutRetainForIntrusiveLinkage() const {
    return withFlags(Flags & ~HasRetainForIntrusiveLinkage);
  }

  // HasTaskCancellationScope
  /// Is there at least one `TaskCancellationScopeRecord` in the status records?
  bool hasTaskCancellationScope() const {
    return Flags & HasTaskCancellationScope;
  }
  ActiveTaskStatus withTaskCancellationScope() const {
    return withFlags(Flags | HasTaskCancellationScope);
  }
  ActiveTaskStatus withoutTaskCancellationScope() const {
    return withFlags(Flags & ~HasTaskCancellationScope);
  }

  // HasDeadline
  /// Is there at least one `TaskDeadlineStatusRecord` in the status records?
  bool hasDeadline() const {
    return Flags & HasDeadline;
  }
  ActiveTaskStatus withDeadline() const {
    return withFlags(Flags | HasDeadline);
  }
  ActiveTaskStatus withoutDeadline() const {
    return withFlags(Flags & ~HasDeadline);
  }

  // StealerExclusion
  AsyncTask::ExclusionValue getStealerExclusionValue() const {
#if SWIFT_CONCURRENCY_ENABLE_PRIORITY_ESCALATION
    return AsyncTask::ExclusionValue{static_cast<uint8_t>(
        (Flags & StealerExclusionMask) >> StealerExclusionShift)};
#else
    return {0};
#endif
  }
  ActiveTaskStatus withNextStealerExclusionValue() const {
#if SWIFT_CONCURRENCY_ENABLE_PRIORITY_ESCALATION
    auto currentStealerExclusionValue = getStealerExclusionValue().value;
    auto newStealerExclusionValue = currentStealerExclusionValue;
    if (__builtin_add_overflow(currentStealerExclusionValue, 1,
                               &newStealerExclusionValue) ||
        newStealerExclusionValue > StealerExclusionMax) {
      assert(false && "Somehow overflowed stealer exclusion value");
    }
    SWIFT_TASK_DEBUG_LOG("Updating exclusion value from %d to %d",
                         currentStealerExclusionValue,
                         newStealerExclusionValue);

    auto NewFlags = Flags & ~StealerExclusionMask;
    NewFlags |= (newStealerExclusionValue << StealerExclusionShift);
    return ActiveTaskStatus(Record, NewFlags, ExecutionLock);
#else
    return *this;
#endif
  }

  // Getters for ExecutionLock
#if SWIFT_CONCURRENCY_ENABLE_PRIORITY_ESCALATION
  static size_t executionLockOffset() {
    return offsetof(ActiveTaskStatus, ExecutionLock);
  }
#endif
  uint32_t currentExecutionLockOwner() const {
#if SWIFT_CONCURRENCY_ENABLE_PRIORITY_ESCALATION
    return dispatch_lock_owner(ExecutionLock);
#else
    return 0;
#endif
  }

  // Getters / Setters for Record

  /// Return the innermost cancellation record.  Code running
  /// asynchronously with this task should not access this record
  /// without having first locked it; see swift_taskCancel.
  TaskStatusRecord *getInnermostRecord() const {
    return Record;
  }
  ActiveTaskStatus withInnermostRecord(TaskStatusRecord *newRecord) {
#if SWIFT_CONCURRENCY_ENABLE_PRIORITY_ESCALATION
    return ActiveTaskStatus(newRecord, Flags, ExecutionLock);
#else
    return ActiveTaskStatus(newRecord, Flags);
#endif
  }

  static TaskStatusRecord *getStatusRecordParent(TaskStatusRecord *ptr);

  using record_iterator =
    LinkedListIterator<TaskStatusRecord, getStatusRecordParent>;
  llvm::iterator_range<record_iterator> records() const {
    return record_iterator::rangeBeginning(getInnermostRecord());
  }

  void traceStatusChanged(AsyncTask *task, ActiveTaskStatus oldStatus,
                          bool isStarting) {
    uint8_t maxPriority = static_cast<uint8_t>(getStoredPriority());
    bool cancelled = isCancelled();
    bool escalated = isStoredPriorityEscalated();
    bool running = isRunning();
    bool enqueued = isDirectlyEnqueued();
    bool wasRunning = oldStatus.isRunning();

    if (!isStarting &&
        maxPriority == static_cast<uint8_t>(oldStatus.getStoredPriority()) &&
        cancelled == oldStatus.isCancelled() &&
        escalated == oldStatus.isStoredPriorityEscalated() &&
        running == wasRunning && enqueued == oldStatus.isDirectlyEnqueued())
      return;

    concurrency::trace::task_status_changed(task, maxPriority, cancelled,
                                            escalated, isStarting, running,
                                            enqueued, wasRunning);
  }

  bool operator!=(const ActiveTaskStatus &other) const {
#if SWIFT_CONCURRENCY_ENABLE_PRIORITY_ESCALATION
    return Record != other.Record || Flags != other.Flags ||
           ExecutionLock != other.ExecutionLock;
#else
    return Record != other.Record || Flags != other.Flags;
#endif
  }
};

#if SWIFT_CONCURRENCY_ENABLE_PRIORITY_ESCALATION && SWIFT_POINTER_IS_4_BYTES
#define ACTIVE_TASK_STATUS_SIZE (4 * (sizeof(uintptr_t)))
#else
#define ACTIVE_TASK_STATUS_SIZE (2 * (sizeof(uintptr_t)))
#endif
static_assert(sizeof(ActiveTaskStatus) == ACTIVE_TASK_STATUS_SIZE,
  "ActiveTaskStatus is of incorrect size");

/// Global allocator that goes through swift_slowAlloc/swift_slowDealloc.
struct SwiftGlobalAllocator {
  void *allocateGlobal(size_t size, size_t alignMask) {
    return swift_slowAlloc(size, alignMask);
  }

  void deallocateGlobal(void* ptr, size_t size, size_t alignMask) {
    swift_slowDealloc(ptr, size, alignMask);
  }
};

/// Select the global allocator differently for Embedded Swift (where we always
/// want to go through swift_slow(Alloc|Dealloc)) or non-embedded (where we
/// continue using malloc/free).
#if SWIFT_CONCURRENCY_EMBEDDED
typedef SwiftGlobalAllocator TaskGlobalAllocator;
#else
typedef MallocFreeAllocator TaskGlobalAllocator;
#endif

struct TaskAllocatorConfiguration {
#if SWIFT_CONCURRENCY_EMBEDDED

  // Slab allocator is always enabled on embedded.
  bool enableSlabAllocator() { return true; }

#else

  enum class EnableState : uint8_t {
    Uninitialized,
    Enabled,
    Disabled,
  };

  static std::atomic<EnableState> enableState;

  bool enableSlabAllocator() {
    auto state = enableState.load(std::memory_order_relaxed);
    if (SWIFT_UNLIKELY(state == EnableState::Uninitialized)) {
      state = runtime::environment::concurrencyEnableTaskSlabAllocator()
                  ? EnableState::Enabled
                  : EnableState::Disabled;
      enableState.store(state, std::memory_order_relaxed);
    }

    return SWIFT_UNLIKELY(state == EnableState::Enabled);
  }

#endif // SWIFT_CONCURRENCY_EMBEDDED
};

/// The size of an allocator slab. We want the full allocation to fit into a
/// 1024-byte malloc quantum. We subtract off the slab header size, plus a
/// little extra to stay within our limits even when there's overhead from
/// malloc stack logging.
static constexpr size_t SlabCapacity =
    1024 - 8 -
  StackAllocator<0, nullptr, TaskAllocatorConfiguration, TaskGlobalAllocator>
    ::slabHeaderSize();
extern Metadata TaskAllocatorSlabMetadata;

using TaskAllocator = StackAllocator<SlabCapacity, &TaskAllocatorSlabMetadata,
                                     TaskAllocatorConfiguration,
                                     TaskGlobalAllocator>;

/// Private storage in an AsyncTask object.
struct AsyncTask::PrivateStorage {
  /// State inside the AsyncTask whose state is only managed by the exclusivity
  /// runtime in stdlibCore. We zero initialize to provide a safe initial value,
  /// but actually initialize its bit state to a const global provided by
  /// libswiftCore so that libswiftCore can control the layout of our initial
  /// state.
  uintptr_t ExclusivityAccessSet[2] = {0, 0};

  /// Storage for the ActiveTaskStatus. See doc for ActiveTaskStatus for size
  /// and alignment requirements.
  alignas(ActiveTaskStatus) char StatusStorage[sizeof(ActiveTaskStatus)];

  /// The allocator for the task stack.
  /// Currently 2 words + 4 bytes.
  TaskAllocator Allocator;

  // Four bytes of padding here (on 64-bit)

  /// Storage for task-local values.
  /// Currently one word.
  TaskLocal::Storage Local;

  /// The top 32 bits of the task ID. The bottom 32 bits are in Job::Id.
  uint32_t Id;

#if __SIZEOF_POINTER__ == 8
  AsyncTask::ExclusionValue LocalStealerExclusionValue = {0};
#endif
  // Three bytes of padding (on 64-bit)

  /// Base priority of Task - set only at creation time of task.
  /// Current max priority of task is ActiveTaskStatus.
  ///
  /// TODO (rokhinip): Only 8 bits of the full size_t are used. Change this into
  /// flagset thing so that remaining bits are available for other non-changing
  /// task status stuff
  JobPriority BasePriority;

  /// Pointer to the task status dependency record. This is allocated from the
  /// async task stack when it is needed.
  TaskDependencyStatusRecord *dependencyRecord = nullptr;

#if SWIFT_CONCURRENCY_ENABLE_TASK_REGISTRY
  /// Intrusive live-task registry links.
  AsyncTask * registryNext{nullptr};
  AsyncTask * registryPrev{nullptr};
#endif

  // The lock used to protect more complicated operations on the task status.
  RecursiveMutex statusLock;

#if __SIZEOF_POINTER__ == 4
  // See the comment on the declaration of this above
  AsyncTask::ExclusionValue LocalStealerExclusionValue = {0};
#endif
  // On 32-bit, there are 7 bytes remaining

  // Always create an async task with max priority in ActiveTaskStatus = base
  // priority. It will be updated later if needed.
  PrivateStorage(JobPriority basePri)
      : Local(TaskLocal::Storage()), BasePriority(basePri) {
    _status().store(ActiveTaskStatus(basePri), std::memory_order_relaxed);
  }

  PrivateStorage(JobPriority basePri, void *slab, size_t slabCapacity)
      : Allocator(slab, slabCapacity), Local(TaskLocal::Storage()),
          BasePriority(basePri) {
    _status().store(ActiveTaskStatus(basePri), std::memory_order_relaxed);
  }

  /// Called on the thread that was previously executing the task that we are
  /// now trying to complete.
  void complete(AsyncTask *task);

  // Destroy the opaque storage of the task
  void destroy() {
#ifndef NDEBUG
    auto oldStatus = _status().load(std::memory_order_relaxed);
    assert(oldStatus.isComplete());
#endif

    this->~PrivateStorage();
  }

  swift::atomic<ActiveTaskStatus> &_status() {
    return reinterpret_cast<swift::atomic<ActiveTaskStatus>&> (this->StatusStorage);
  }

  const swift::atomic<ActiveTaskStatus> &_status() const {
    return reinterpret_cast<const swift::atomic<ActiveTaskStatus>&> (this->StatusStorage);
  }
};

// It will be aligned to 2 words on all platforms. On arm64_32, we have an
// additional requirement where it is aligned to 4 words.
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Winvalid-offsetof"
static_assert(((offsetof(AsyncTask, Private) + offsetof(AsyncTask::PrivateStorage, StatusStorage)) % ACTIVE_TASK_STATUS_SIZE == 0),
   "StatusStorage is not aligned in the AsyncTask");
#pragma clang diagnostic pop
static_assert(sizeof(AsyncTask::PrivateStorage) <= sizeof(AsyncTask::OpaquePrivateStorage),
              "Task-private storage doesn't fit in reserved space");

inline AsyncTask::PrivateStorage &
AsyncTask::OpaquePrivateStorage::get() {
  return reinterpret_cast<PrivateStorage &>(*this);
}
inline const AsyncTask::PrivateStorage &
AsyncTask::OpaquePrivateStorage::get() const {
  return reinterpret_cast<const PrivateStorage &>(*this);
}
inline void AsyncTask::OpaquePrivateStorage::initialize(JobPriority basePri) {
  ::new (this) PrivateStorage(basePri);
}
inline void AsyncTask::OpaquePrivateStorage::initializeWithSlab(
    JobPriority basePri, void *slab, size_t slabCapacity) {
  ::new (this) PrivateStorage(basePri, slab, slabCapacity);
}

inline void AsyncTask::OpaquePrivateStorage::complete(AsyncTask *task) {
  get().complete(task);
}

inline void AsyncTask::OpaquePrivateStorage::destroy() {
  get().destroy();
}

inline AsyncTask::PrivateStorage &AsyncTask::_private() {
  return Private.get();
}
inline const AsyncTask::PrivateStorage &AsyncTask::_private() const {
  return Private.get();
}

/// A Job that acts as a proxy for a Task when the Task itself can't be
/// enqueued. Holds a reference to the Task and an exclusion value that causes
/// this Job to do nothing if it doesn't match what's in the Task's Status.
class AsyncTaskStealer : public Job {
public:
  AsyncTask *Task;
  AsyncTask::ExclusionValue ExclusionValue;

  AsyncTaskStealer(AsyncTask *task, JobPriority priority,
                   AsyncTask::ExclusionValue exclusionValue)
      : Job({JobKind::TaskStealer, priority}, &process), Task(task),
        ExclusionValue{exclusionValue} {
    swift_retain(Task);
  }

  // Implemented in Actor.cpp to make use of taskInvokeWithExclusionValue
  SWIFT_CC(swiftasync)
  static void process(Job *job);

  static bool classof(const Job *job) {
    return job->Flags.getKind() == JobKind::TaskStealer;
  }
};

/// When this task has a valid dependent on executor record, and
/// is holding the TaskStatusLock, this function will determine
/// and return the Job (either the Task itself or a fresh stealer)
/// that the caller should then enqueue on the target executor.
///
/// If the caller is transferring refcount to the child (i.e. the
/// regular enqueue path and not the escalation path), the caller MUST
/// enqueue the returned Job AFTER dropping the Task Status Lock. See
/// "Why the enqueue must happen after the lock is dropped" below.
///
/// Returns nullptr when there is nothing to enqueue
/// (currently only the async-let-during-escalation path).
///
/// This is an internal function needed to support regular enqueues, and
/// additional enqueues due to escalation while a Task is still enqueued
/// on an executor. Most external callers shoud not be using this function
/// and should instead call flagAsRunningFrom* on the Task instead.
///
/// This function was designed to be called by
/// swift_executor_escalate and flagAsAndEnqueueOnExecutor.
///
/// In those cases, we call this instead of swift_task_enqueue
/// directly for Tasks in case the Task is already
/// intrusively linked somewhere due to stealers.
///
/// Why the enqueue must happen after the lock is dropped
/// -----------------------------------------------------
/// The Task Status Lock is a hybrid lock: an "is-locked" bit in the
/// ActiveTaskStatus word plus a real mutex in the task's private storage.
/// On unlock, the bit is cleared via CAS *before* the mutex is released
/// (see withStatusRecordLock in TaskStatus.cpp). During that window,
/// any thread that observes the ActiveTaskStatus with the bit clear
/// is free to make forward progress on the Task using lock-free CAS
/// paths. If we called swift_task_enqueue while still under the lock,
/// another thread could pick the Task up off the target executor, run it
/// to completion, and attempt to dispose of it before this thread had
/// finished releasing the mutex, leaving us operating on a freed task.
/// Splitting "decide what to enqueue" (which requires the ActiveTaskStatus
/// to be quiescent) from the actual enqueue avoids that race — the
/// caller does the mutation under the lock and then hands the returned
/// Job off to swift_task_enqueue only after the lock is released.
///
/// The escalate path (swift_executor_escalate) is exempted
/// from having to drop the lock first because its caller must
/// hold a refcount on the Task for the duration of the escalate
/// call, so the Task cannot be destroyed out from under it.
///
/// This function has some important and complicated synchronization
/// requirements. It must be synchronized with marking the Task as running in
/// resumeRunningAfterFailedSuspend and tryStartRunning such that this function
/// may not be called once the Task has successfully been marked as running.
/// This is because once a Task is running, it must be escalated by escalating
/// the thread it is running on rather than by enqueuing a stealer. Thus
/// this function can only be called while there is an enqueued on executor
/// dependency record which points to the executor we are escalating here.
///
/// For the purposes of this comment, "enqueued" means that the Task
/// is "runnable" (has a dependent on executor dependency record) and
/// there is an object enqueued with the current exclusion value even
/// if there are objects with older exclusion values still enqueued.
///
/// The possible intrusive link of this Task and any stealers that have old
/// exclusion values will always fail resumeRunningAfterFailedSuspend regardless
/// of this function so there is sufficient synchronization between them.
///
/// flagAsAndEnqueueOnExecutor can only be called either on Task
/// initialization, a running Task, or a Task being resumed. These are
/// mutually exclusive between them by nature. * During Task initialization,
/// there are no stealers, thus there is trivial synchronization with
/// flagAsAndEnqueueOnExecutor * While running a Task (e.g. swapping executors),
/// because the Task is running, all stealers must have old exclusion values
/// because active stealers can't exist for Tasks without dependent on
/// executor dependency records * Resuming a suspended Task, the Task must
/// have a dependency record on something other than an executor so any
/// stealers must have an old exclusion value for the same reason as above
///
/// An important note though is that flagAsAndEnqueueOnExecutor adds
/// the dependent on executor record to the Task before calling this
/// function. As soon as it does so, swift_task_escalate may attempt to
/// call this function to add a stealer. To ensure that does not race,
/// flagAsAndEnqueueOnExecutor must take the Task Status Lock, add the
/// dependency record, then call this function to decide what to enqueue,
/// all before releasing the lock. swift_task_escalate calls all escalation
/// actions including the one that calls this function under the lock
/// so it can not call this function until flagAsAndEnqueueOnExecutor
/// has finished its work. Only the decision happens under the lock; the
/// actual swift_task_enqueue is performed by flagAsAndEnqueueOnExecutor
/// after the lock has been released, for the reason given above.
///
/// swift_executor_escalate is only called when there is a "dependent on
/// executor" dependency record so it only needs to synchronize with itself
/// and with flagAsRunningFrom*. It does this by holding the Task Status Lock
/// which mutually excludes itself. The flagAsRunningFrom* functions will
/// attempt to atomically remove the dependency record if there is no one else
/// trying to hold the lock. So if there is contention, it will attempt to
/// take the lock. This synchronizes access to the Task Status and dependency
/// record. If flagAsRunningFrom* wins, it will remove the dependency record
/// before swift_executor_escalate gets the lock so this function will never
/// be called. If it loses, then this function will run, decide on a stealer,
/// and update the state which flagAsRunningFrom* will then observe in it's
/// exclusive region (swift_executor_escalate then enqueues that stealer).
///
/// Once there are multiple active stealers, they will also have to synchronize
/// against each other in flagAsRunningFrom*. Two stealers will both use
/// atomics to synchronize if no one is holding / attempting to hold the
/// lock. They will check that their exclusion value is still the most recent
/// and check the bit that indicates if there are multiple stealers. If
/// they see that bit, they know that they may be racing with other stealers
/// so they clear the bit and update the exclusion value. Whoever wins
/// doing that is able to run and all other racing stealers notice that the
/// exclusion value is now different so they must fail to flag as running.
///
/// Any new calls to this function need to be careful to ensure that they
/// are only called in a way that synchronizes with all other uses, and to
/// enqueue the returned Job only after dropping the Task Status Lock unless
/// the caller holds a refcount on the Task for the duration of the enqueue
/// (as swift_executor_escalate does). It is likely possible to write this
/// function in such a way that it self-synchronizes with tryStartRunning
/// rather than relying on the Task Status Lock being held and this function
/// only being called if the Task is not yet running. However, that would
/// make this function failable. I leave that as an exercise for the future.
enum EnqueueFlags : uint32_t {
  EnqueueFlagsRegular = 0x0,
  EnqueueFlagsForEscalation = 0x1,
};
Job *swift_task_getSelfOrStealerForEnqueue(AsyncTask *task, EnqueueFlags flags);

/// Returns true if the task has any kind of task executor preference,
/// including an initial task executor preference (set during task creation).
inline bool AsyncTask::hasTaskExecutorPreferenceRecord() const {
  if (hasInitialTaskExecutorPreferenceRecord()) {
    // an "initial" setting is valid for the entire lifetime of a task,
    // so we don't need to check anything else; there definitely is at
    // least one preference record.
    return true;
  }

  auto oldStatus = _private()._status().load(std::memory_order_relaxed);
  return oldStatus.hasTaskExecutorPreference();
}

// READ ME: This is not a dead function! Do not remove it! This is a function
// that can be used when debugging locally to instrument when a task
// actually is dealloced.
inline void AsyncTask::flagAsDestroyed() {
  SWIFT_TASK_DEBUG_LOG("task destroyed %p", this);
}

// ==== Task Local Values -----------------------------------------------------

inline void AsyncTask::localValuePush(const HeapObject *key,
                                      /* +1 */ OpaqueValue *value,
                                      const Metadata *valueType) {
  _private().Local.pushValue(this, key, value, valueType);
}

inline OpaqueValue *AsyncTask::localValueGet(const HeapObject *key) {
  return _private().Local.getValue(this, key);
}

/// Returns true if storage has still more bindings.
inline bool AsyncTask::localValuePop() {
  return _private().Local.popValue(this);
}

// ==== Cancellation Shields --------------------------------------------------

// `cancellationShieldPush` / `cancellationShieldPop` are implemented in
// TaskStatus.cpp because they push/pop a `TaskCancellationShieldRecord`
// alongside the fast-path shield bit; the record makes shield-vs-scope
// ordering visible to the scope search in `_swift_task_getCancellationScope`.

} // end namespace swift

#endif
