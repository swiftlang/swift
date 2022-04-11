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
#include "Tracing.h"
#include "swift/ABI/Metadata.h"
#include "swift/ABI/Task.h"
#include "swift/Runtime/Atomic.h"
#include "swift/Runtime/Concurrency.h"
#include "swift/Runtime/DispatchShims.h"
#include "swift/Runtime/Error.h"
#include "swift/Runtime/Exclusivity.h"
#include "swift/Runtime/Heap.h"
#include "swift/Runtime/HeapObject.h"
#include <atomic>

#define SWIFT_FATAL_ERROR swift_Concurrency_fatalError
#include "../runtime/StackAllocator.h"

#if HAVE_PTHREAD_H
#include <pthread.h>
#endif
#if defined(_WIN32)
#define WIN32_LEAN_AND_MEAN
#define VC_EXTRA_LEAN
#define NOMINMAX
#include <Windows.h>
#endif

namespace swift {

// Set to 1 to enable helpful debug spew to stderr
// If this is enabled, tests with `swift_task_debug_log` requirement can run.
#if 0
#define SWIFT_TASK_DEBUG_LOG(fmt, ...)                                         \
  fprintf(stderr, "[%lu] [%s:%d](%s) " fmt "\n",                               \
          (unsigned long)_swift_get_thread_id(),                               \
          __FILE__, __LINE__, __FUNCTION__,                                    \
          __VA_ARGS__)

#if defined(_WIN32)
using ThreadID = decltype(GetCurrentThreadId());
#else
using ThreadID = decltype(pthread_self());
#endif

inline ThreadID _swift_get_thread_id() {
#if defined(_WIN32)
  return GetCurrentThreadId();
#else
  return pthread_self();
#endif
}

#else
#define SWIFT_TASK_DEBUG_LOG(fmt, ...) (void)0
#endif

class AsyncTask;
class TaskGroup;

/// Allocate task-local memory on behalf of a specific task,
/// not necessarily the current one.  Generally this should only be
/// done on behalf of a child task.
void *_swift_task_alloc_specific(AsyncTask *task, size_t size);

/// dellocate task-local memory on behalf of a specific task,
/// not necessarily the current one.  Generally this should only be
/// done on behalf of a child task.
void _swift_task_dealloc_specific(AsyncTask *task, void *ptr);

/// Given that we've already set the right executor as the active
/// executor, run the given job.  This does additional bookkeeping
/// related to the active task.
void runJobInEstablishedExecutorContext(Job *job);

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

/// release() establishes a happens-before relation with a preceding acquire()
/// on the same address.
void _swift_tsan_acquire(void *addr);
void _swift_tsan_release(void *addr);

/// Special values used with DispatchQueueIndex to indicate the global and main
/// executors.
#define DISPATCH_QUEUE_GLOBAL_EXECUTOR (void *)1

#if !defined(SWIFT_STDLIB_SINGLE_THREADED_RUNTIME)
inline SerialExecutorWitnessTable *
_swift_task_getDispatchQueueSerialExecutorWitnessTable() {
  extern SerialExecutorWitnessTable wtable
    SWIFT_ASM_LABEL_WITH_PREFIX("$ss17DispatchQueueShimCScfsWP");
  return &wtable;
}
#endif

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
///   @_silgen_name("swift_asyncLet_wait")
///   func _asyncLetGet<T>(_ task: Builtin.RawPointer) async -> T
///
///   @_silgen_name("swift_asyncLet_waitThrowing")
///   func _asyncLetGetThrowing<T>(_ task: Builtin.RawPointer) async throws -> T
///
///   @_silgen_name("swift_taskGroup_wait_next_throwing")
///   func _taskGroupWaitNext<T>(group: Builtin.RawPointer) async throws -> T?
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
    fillWithSuccess(future->getStoragePtr(), future->getResultType(),
                    successResultPointer);
  }
  void fillWithSuccess(OpaqueValue *src, const Metadata *successType,
                       OpaqueValue *result) {
    successType->vw_initializeWithCopy(result, src);
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
///          Flags               Exeuction Lock           Unused           TaskStatusRecord *
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
    /// The max priority of the task. This is always >= basePriority in the task
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
    /// identity of the drainer (see above), we use one bit in the flags to track
    /// whether or not task is running. Otherwise, the drain lock tells us
    /// whether or not it is running.
    IsRunning = 0x800,
#endif

    /// Task is enqueued somewhere - either in the cooperative pool, or in an
    /// actor. This bit is cleared when a starts running on a thread, suspends
    /// or is completed.
    IsEnqueued = 0x1000,

#ifndef NDEBUG
    /// Task has been completed.  This is purely used to enable an assertion
    /// that the task is completed when we destroy it.
    IsComplete = 0x2000,
#endif
  };

  // Note: this structure is mirrored by ActiveTaskStatusWithEscalation and
  // ActiveTaskStatusWithoutEscalation in
  // include/swift/Reflection/RuntimeInternals.h. Any changes to the layout here
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

  /// Is the task currently cancelled?
  bool isCancelled() const { return Flags & IsCancelled; }
  ActiveTaskStatus withCancelled() const {
#if SWIFT_CONCURRENCY_ENABLE_PRIORITY_ESCALATION
    return ActiveTaskStatus(Record, Flags | IsCancelled, ExecutionLock);
#else
    return ActiveTaskStatus(Record, Flags | IsCancelled);
#endif
  }

  bool isEnqueued() const { return Flags & IsEnqueued; }
  ActiveTaskStatus withEnqueued() const {
#if SWIFT_CONCURRENCY_ENABLE_PRIORITY_ESCALATION
    return ActiveTaskStatus(Record, Flags | IsEnqueued, ExecutionLock);
#else
    return ActiveTaskStatus(Record, Flags | IsEnqueued);
#endif
  }
  ActiveTaskStatus withoutEnqueued() const {
#if SWIFT_CONCURRENCY_ENABLE_PRIORITY_ESCALATION
    return ActiveTaskStatus(Record, Flags & ~IsEnqueued, ExecutionLock);
#else
    return ActiveTaskStatus(Record, Flags & ~IsEnqueued);
#endif
  }

  /// Is the task currently running? Also known as whether it is drain locked.
  /// IsRunning() = true implies isEnqueued()
  ///
  /// Eventually we'll track this with more specificity, like whether
  /// it's running on a specific thread, enqueued on a specific actor,
  /// etc.
  bool isRunning() const {
#if SWIFT_CONCURRENCY_ENABLE_PRIORITY_ESCALATION
  return dispatch_lock_is_locked(ExecutionLock);
#else
    return Flags & IsRunning;
#endif
  }

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

#ifndef NDEBUG
  bool isComplete() const {
    return Flags & IsComplete;
  }

  ActiveTaskStatus withComplete() const {
#if SWIFT_CONCURRENCY_ENABLE_PRIORITY_ESCALATION
    return ActiveTaskStatus(Record, Flags | IsComplete, ExecutionLock);
#else
    return ActiveTaskStatus(Record, Flags | IsComplete);
#endif
  }
#endif

  /// Is there a lock on the linked list of status records?
  bool isStatusRecordLocked() const { return Flags & IsStatusRecordLocked; }
  ActiveTaskStatus withLockingRecord(TaskStatusRecord *lockRecord) const {
    assert(!isStatusRecordLocked());
    assert(lockRecord->Parent == Record);
#if SWIFT_CONCURRENCY_ENABLE_PRIORITY_ESCALATION
    return ActiveTaskStatus(lockRecord, Flags | IsStatusRecordLocked, ExecutionLock);
#else
    return ActiveTaskStatus(lockRecord, Flags | IsStatusRecordLocked);
#endif
  }

  ActiveTaskStatus withoutLockingRecord() const {
    assert(isStatusRecordLocked());
    assert(Record->getKind() == TaskStatusRecordKind::Private_RecordLock);

    // Remove the lock record, and put the next one as the head
    auto newRecord = Record->Parent;
#if SWIFT_CONCURRENCY_ENABLE_PRIORITY_ESCALATION
    return ActiveTaskStatus(newRecord, Flags & ~IsStatusRecordLocked, ExecutionLock);
#else
    return ActiveTaskStatus(newRecord, Flags & ~IsStatusRecordLocked);
#endif
  }

  JobPriority getStoredPriority() const {
    return JobPriority(Flags & PriorityMask);
  }
  bool isStoredPriorityEscalated() const {
    return Flags & IsEscalated;
  }

  /// Creates a new active task status for a task with the specified priority
  /// and masks away any existing priority related flags on the task status. All
  /// other flags about the task are unmodified. This is only safe to use when
  /// we know that the task we're modifying is not running.
  ActiveTaskStatus withNewPriority(JobPriority priority) const {
#if SWIFT_CONCURRENCY_ENABLE_PRIORITY_ESCALATION
    assert(ExecutionLock == DLOCK_OWNER_NULL);
#endif
    assert(!isEnqueued());
    return ActiveTaskStatus(Record,
                            (Flags & ~PriorityMask) | uintptr_t(priority));
  }

  ActiveTaskStatus withEscalatedPriority(JobPriority priority) const {
    assert(priority > getStoredPriority());
#if SWIFT_CONCURRENCY_ENABLE_PRIORITY_ESCALATION
    return ActiveTaskStatus(Record,
                            (Flags & ~PriorityMask)
                               | IsEscalated | uintptr_t(priority), ExecutionLock);
#else
    return ActiveTaskStatus(Record, (Flags & ~PriorityMask) | IsEscalated | uintptr_t(priority));
#endif
  }

  ActiveTaskStatus withoutStoredPriorityEscalation() const {
#if SWIFT_CONCURRENCY_ENABLE_PRIORITY_ESCALATION
    return ActiveTaskStatus(Record, Flags & ~IsEscalated, ExecutionLock);
#else
    return ActiveTaskStatus(Record, Flags & ~IsEscalated);
#endif
  }

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

  void traceStatusChanged(AsyncTask *task) {
    concurrency::trace::task_status_changed(
        task, static_cast<uint8_t>(getStoredPriority()), isCancelled(),
        isStoredPriorityEscalated(), isRunning(), isEnqueued());
  }
};

#if SWIFT_CONCURRENCY_ENABLE_PRIORITY_ESCALATION && SWIFT_POINTER_IS_4_BYTES
#define ACTIVE_TASK_STATUS_SIZE (4 * (sizeof(uintptr_t)))
#else
#define ACTIVE_TASK_STATUS_SIZE (2 * (sizeof(uintptr_t)))
#endif
static_assert(sizeof(ActiveTaskStatus) == ACTIVE_TASK_STATUS_SIZE,
  "ActiveTaskStatus is of incorrect size");
#if !defined(_WIN64)
static_assert(sizeof(swift::atomic<ActiveTaskStatus>) == sizeof(std::atomic<ActiveTaskStatus>),
              "swift::atomic pads std::atomic, memory aliasing invariants violated");
#endif
static_assert(sizeof(swift::atomic<ActiveTaskStatus>) == sizeof(ActiveTaskStatus),
              "swift::atomic pads ActiveTaskStatus, memory aliasing invariants violated");

/// The size of an allocator slab. We want the full allocation to fit into a
/// 1024-byte malloc quantum. We subtract off the slab header size, plus a
/// little extra to stay within our limits even when there's overhead from
/// malloc stack logging.
static constexpr size_t SlabCapacity = 1024 - StackAllocator<0, nullptr>::slabHeaderSize() - 8;
extern Metadata TaskAllocatorSlabMetadata;

using TaskAllocator = StackAllocator<SlabCapacity, &TaskAllocatorSlabMetadata>;

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
  alignas(alignof(ActiveTaskStatus)) char StatusStorage[sizeof(ActiveTaskStatus)];

  /// The allocator for the task stack.
  /// Currently 2 words + 8 bytes.
  TaskAllocator Allocator;

  /// Storage for task-local values.
  /// Currently one word.
  TaskLocal::Storage Local;

  /// The top 32 bits of the task ID. The bottom 32 bits are in Job::Id.
  uint32_t Id;

  /// Base priority of Task - set only at creation time of task.
  /// Current max priority of task is ActiveTaskStatus.
  ///
  /// TODO (rokhinip): Only 8 bits of the full size_t are used. Change this into
  /// flagset thing so that remaining bits are available for other non-changing
  /// task status stuff
  JobPriority BasePriority;

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
  void complete(AsyncTask *task) {
    // Drain unlock the task and remove any overrides on thread as a
    // result of the task
    auto oldStatus = task->_private()._status().load(std::memory_order_relaxed);
    while (true) {
      // Task is completing, it shouldn't have any records and therefore
      // cannot be status record locked.
      assert(oldStatus.getInnermostRecord() == NULL);
      assert(!oldStatus.isStatusRecordLocked());

      assert(oldStatus.isRunning());

      // Remove drainer, enqueued and override bit if any
      auto newStatus = oldStatus.withRunning(false);
      newStatus = newStatus.withoutStoredPriorityEscalation();
      newStatus = newStatus.withoutEnqueued();
#ifndef NDEBUG
      newStatus = newStatus.withComplete();
#endif

      // This can fail since the task can still get concurrently cancelled or
      // escalated.
      if (task->_private()._status().compare_exchange_weak(oldStatus, newStatus,
              /* success */ std::memory_order_relaxed,
              /* failure */ std::memory_order_relaxed)) {
#if SWIFT_CONCURRENCY_ENABLE_PRIORITY_ESCALATION
        if (oldStatus.isStoredPriorityEscalated()) {
          SWIFT_TASK_DEBUG_LOG("[Override] Reset override %#x on thread from task %p", oldStatus.getStoredPriority(), this);
          swift_dispatch_lock_override_end((qos_class_t) oldStatus.getStoredPriority());
        }
#endif
        break;
      }
    }

    // Destroy and deallocate any remaining task local items.
    // We need to do this before we destroy the task local deallocator.
    Local.destroy(task);

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
static_assert(((offsetof(AsyncTask, Private) + offsetof(AsyncTask::PrivateStorage, StatusStorage)) % ACTIVE_TASK_STATUS_SIZE == 0),
   "StatusStorage is not aligned in the AsyncTask");
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
  new (this) PrivateStorage(basePri);
}
inline void AsyncTask::OpaquePrivateStorage::initializeWithSlab(
    JobPriority basePri, void *slab, size_t slabCapacity) {
  new (this) PrivateStorage(basePri, slab, slabCapacity);
}
inline void AsyncTask::OpaquePrivateStorage::complete(AsyncTask *task) {
  get().complete(task);
}
inline void AsyncTask::OpaquePrivateStorage::destroy() {
  // nothing else to do
}

inline AsyncTask::PrivateStorage &AsyncTask::_private() {
  return Private.get();
}
inline const AsyncTask::PrivateStorage &AsyncTask::_private() const {
  return Private.get();
}

inline bool AsyncTask::isCancelled() const {
  return _private()._status().load(std::memory_order_relaxed)
                          .isCancelled();
}

inline void AsyncTask::flagAsRunning() {
  SWIFT_TASK_DEBUG_LOG("%p->flagAsRunning()", this);

#if SWIFT_CONCURRENCY_ENABLE_PRIORITY_ESCALATION
  dispatch_thread_override_info_s threadOverrideInfo;
  threadOverrideInfo = swift_dispatch_thread_get_current_override_qos_floor();
  qos_class_t overrideFloor = threadOverrideInfo.override_qos_floor;
retry:;
#endif
  auto oldStatus = _private()._status().load(std::memory_order_relaxed);
  while (true) {
    // We can get here from being suspended or being enqueued
    assert(!oldStatus.isRunning());
    assert(!oldStatus.isComplete());

#if SWIFT_CONCURRENCY_ENABLE_PRIORITY_ESCALATION
    // Task's priority is greater than the thread's - do a self escalation
    qos_class_t maxTaskPriority = (qos_class_t) oldStatus.getStoredPriority();
    if (threadOverrideInfo.can_override && (maxTaskPriority > overrideFloor)) {
      SWIFT_TASK_DEBUG_LOG("[Override] Self-override thread with oq_floor %#x to match task %p's max priority %#x",
          overrideFloor, this, maxTaskPriority);

      (void) swift_dispatch_thread_override_self(maxTaskPriority);
      overrideFloor = maxTaskPriority;
      goto retry;
    }
#endif

    // Set self as executor and remove escalation bit if any - the task's
    // priority escalation has already been reflected on the thread.
    auto newStatus = oldStatus.withRunning(true);
    newStatus = newStatus.withoutStoredPriorityEscalation();
    newStatus = newStatus.withoutEnqueued();

    if (_private()._status().compare_exchange_weak(oldStatus, newStatus,
             /* success */ std::memory_order_relaxed,
             /* failure */ std::memory_order_relaxed)) {
      newStatus.traceStatusChanged(this);
      adoptTaskVoucher(this);
      swift_task_enterThreadLocalContext(
          (char *)&_private().ExclusivityAccessSet[0]);
      return;
    }
  }
}

/// TODO (rokhinip): We need the handoff of the thread to the next executor to be
/// done while the current thread still holds the voucher and the priority from the
/// task. Otherwise, if we reset the voucher and priority escalation too early, the
/// thread may be preempted immediately before we can finish the enqueue of the
/// high priority task to the next location. We will then have a priority inversion
/// of waiting for a low priority thread to enqueue a high priorty task.
///
/// In order to do this correctly, we need enqueue-ing of a task to the next
/// executor, to have a "hand-over-hand locking" type of behaviour - until the
/// enqueue completes to the new location, the original thread does not let go of
/// the task and the execution properties of the task. This involves rethinking
/// some of the enqueue logic and being able to handle races of a new thread
/// expecting to execute an enqueued task, while the task is still held onto by the
/// original enqueueing thread.
///
/// rdar://88366470 (Direct handoff behaviour when tasks switch executors)
inline void AsyncTask::flagAsAndEnqueueOnExecutor(ExecutorRef newExecutor) {

  SWIFT_TASK_DEBUG_LOG("%p->flagAsAndEnqueueOnExecutor()", this);
  auto oldStatus = _private()._status().load(std::memory_order_relaxed);
  auto newStatus = oldStatus;

  while (true) {
    // The task could either be running or suspended when it switches to an
    // enqueued state but it cannot be enqueued somewhere else
    assert(!oldStatus.isEnqueued());

    // If the task was running, drop execution lock and any override the thread
    // might have received as a result of executing it previously. Mark the task
    // as being enqueued
    newStatus = oldStatus.withRunning(false);
    newStatus = newStatus.withoutStoredPriorityEscalation();
    newStatus = newStatus.withEnqueued();

    if (_private()._status().compare_exchange_weak(oldStatus, newStatus,
            /* success */std::memory_order_relaxed,
            /* failure */std::memory_order_relaxed)) {
      break;
    }
  }
  newStatus.traceStatusChanged(this);

  if (oldStatus.isRunning()) {
#if SWIFT_CONCURRENCY_ENABLE_PRIORITY_ESCALATION
    // The thread was previously running the task, now that we aren't, we need
    // to remove any task escalation on the thread as a result of the task.
    if (oldStatus.isStoredPriorityEscalated()) {
      SWIFT_TASK_DEBUG_LOG("[Override] Reset override %#x on thread from task %p",
        oldStatus.getStoredPriority(), this);
      swift_dispatch_lock_override_end((qos_class_t) oldStatus.getStoredPriority());
    }
#endif
    swift_task_exitThreadLocalContext((char *)&_private().ExclusivityAccessSet[0]);
    restoreTaskVoucher(this);
  }

  // Set up task for enqueue to next location by setting the Job priority field
  Flags.setPriority(newStatus.getStoredPriority());
  concurrency::trace::task_flags_changed(
      this, static_cast<uint8_t>(Flags.getPriority()), Flags.task_isChildTask(),
      Flags.task_isFuture(), Flags.task_isGroupChildTask(),
      Flags.task_isAsyncLetTask());

  swift_task_enqueue(this, newExecutor);
}

inline void AsyncTask::flagAsSuspended() {
  SWIFT_TASK_DEBUG_LOG("%p->flagAsSuspended()", this);
  auto oldStatus = _private()._status().load(std::memory_order_relaxed);
  auto newStatus = oldStatus;
  while (true) {
    // We can only be suspended if we were previously running. See state
    // transitions listed out in Task.h
    assert(oldStatus.isRunning() && !oldStatus.isEnqueued());

    newStatus = oldStatus.withRunning(false);
    newStatus = newStatus.withoutStoredPriorityEscalation();

    if (_private()._status().compare_exchange_weak(oldStatus, newStatus,
            /* success */std::memory_order_relaxed,
            /* failure */std::memory_order_relaxed)) {
      break;
    }
  }
#if SWIFT_CONCURRENCY_ENABLE_PRIORITY_ESCALATION
  // Successfully dropped task drain lock, make sure to remove override on
  // thread due to task
  if (oldStatus.isStoredPriorityEscalated()) {
     SWIFT_TASK_DEBUG_LOG("[Override] Reset override %#x on thread from task %p",
       oldStatus.getStoredPriority(), this);
     swift_dispatch_lock_override_end((qos_class_t) oldStatus.getStoredPriority());
  }
#endif

  newStatus.traceStatusChanged(this);
  swift_task_exitThreadLocalContext((char *)&_private().ExclusivityAccessSet[0]);
  restoreTaskVoucher(this);
  return;
}

// READ ME: This is not a dead function! Do not remove it! This is a function
// that can be used when debugging locally to instrument when a task actually is
// dealloced.
inline void AsyncTask::flagAsCompleted() {
  SWIFT_TASK_DEBUG_LOG("task completed %p", this);
}

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

/*************** Methods for Status records manipulation ******************/

/// Remove a status record from a task.  After this call returns,
/// the record's memory can be freely modified or deallocated.
///
/// This must be called synchronously with the task.  The record must
/// be registered with the task or else this may crash.
///
/// The given record need not be the last record added to
/// the task, but the operation may be less efficient if not.
///
/// Returns false if the task has been cancelled.
SWIFT_CC(swift)
bool removeStatusRecord(TaskStatusRecord *record);

/// Add a status record to a task. This must be called synchronously with the
/// task.
///
/// This function also takes in a function_ref which is given the task status of
/// the task we're adding the record to, to determine if the current status of
/// the task permits adding the status record. This function_ref may be called
/// multiple times and must be idempotent.
SWIFT_CC(swift)
bool addStatusRecord(TaskStatusRecord *record,
                     llvm::function_ref<bool(ActiveTaskStatus)> testAddRecord);

/// A helper function for updating a new child task that is created with
/// information from the parent or the group that it was going to be added to.
SWIFT_CC(swift)
void updateNewChildWithParentAndGroupState(AsyncTask *child,
                                           ActiveTaskStatus parentStatus,
                                           TaskGroup *group);

} // end namespace swift

#endif
