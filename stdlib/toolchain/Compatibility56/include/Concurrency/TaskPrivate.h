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

#ifndef SWIFT_CONCURRENCY_TASKPRIVATE_BACKDEPLOY56_H
#define SWIFT_CONCURRENCY_TASKPRIVATE_BACKDEPLOY56_H

#include "Concurrency/Error.h"
#include "Concurrency/Task.h"
#include "Concurrency/TaskLocal.h"
#include "Runtime/Concurrency.h"
#include "swift/Runtime/Exclusivity.h"
#include "swift/Runtime/Error.h"

#define SWIFT_FATAL_ERROR swift_Concurrency_fatalError
#include "public/runtime/StackAllocator.h"

namespace swift {

#if 0
using ThreadID = decltype(pthread_self());

inline ThreadID _swift_get_thread_id() {
#if defined(_WIN32)
  return GetCurrentThreadId();
#else
  return pthread_self();
#endif
}

#define SWIFT_TASK_DEBUG_LOG(fmt, ...)                                         \
  fprintf(stderr, "[%lu] [%s:%d](%s) " fmt "\n",                               \
          (unsigned long)_swift_get_thread_id(),                               \
          __FILE__, __LINE__, __FUNCTION__,                                    \
          __VA_ARGS__)
#else
#define SWIFT_TASK_DEBUG_LOG(fmt ...) (void)0
#endif

/// Allocate task-local memory on behalf of a specific task,
/// not necessarily the current one.  Generally this should only be
/// done on behalf of a child task.
void *_swift_task_alloc_specific(AsyncTask *task, size_t size);

/// dellocate task-local memory on behalf of a specific task,
/// not necessarily the current one.  Generally this should only be
/// done on behalf of a child task.
void _swift_task_dealloc_specific(AsyncTask *task, void *ptr);

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
///   @_silgen_name("swift_taskGroup_wait_nextAll")
///   func _taskGroupWaitAll<T>(group: Builtin.RawPointer) async throws -> T?
///
class TaskFutureWaitAsyncContext : public AsyncContext {
public:
  SwiftError *errorResult;

  OpaqueValue *successResultPointer;

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

}

/// Adopt the voucher stored in `task`. This removes the voucher from the task
/// and adopts it on the current thread.
__attribute__((visibility("hidden")))
void adoptTaskVoucher(AsyncTask *task);

/// Restore the voucher for `task`. This un-adopts the current thread's voucher
/// and stores it back into the task again.
__attribute__((visibility("hidden")))
void restoreTaskVoucher(AsyncTask *task);

/// release() establishes a happens-before relation with a preceding acquire()
/// on the same address.
__attribute__((visibility("hidden")))
void _swift_tsan_acquire(void *addr);
__attribute__((visibility("hidden")))
void _swift_tsan_release(void *addr);

/// Clear the active task reference for the current thread.
__attribute__((visibility("hidden")))
AsyncTask *_swift_task_clearCurrent();

/// The current state of a task's status records.
class alignas(sizeof(void*) * 2) ActiveTaskStatus {
  enum : uintptr_t {
    /// The current running priority of the task.
    PriorityMask = 0xFF,

    /// Has the task been cancelled?
    IsCancelled = 0x100,

    /// Whether the task status is "locked", meaning that further
    /// accesses need to wait on the task status record lock
    IsLocked = 0x200,

    /// Whether the running priority has been escalated above the
    /// priority recorded in the Job header.
    IsEscalated = 0x400,

    /// Whether the task is actively running.
    /// We don't really need to be tracking this in the runtime right
    /// now, but we will need to eventually track enough information to
    /// escalate the thread that's running a task, so doing the stores
    /// necessary to maintain this gives us a more realistic baseline
    /// for performance.
    IsRunning = 0x800,
  };

  TaskStatusRecord *Record;
  uintptr_t Flags;

  ActiveTaskStatus(TaskStatusRecord *record, uintptr_t flags)
    : Record(record), Flags(flags) {}

public:
#ifdef __GLIBCXX__
  /// We really don't want to provide this constructor, but in old
  /// versions of libstdc++, std::atomic<T>::load incorrectly requires
  /// the type to be default-constructible.
  ActiveTaskStatus() = default;
#endif

  constexpr ActiveTaskStatus(JobFlags flags)
    : Record(nullptr), Flags(uintptr_t(flags.getPriority())) {}

  /// Is the task currently cancelled?
  bool isCancelled() const { return Flags & IsCancelled; }
  ActiveTaskStatus withCancelled() const {
    return ActiveTaskStatus(Record, Flags | IsCancelled);
  }

  /// Is the task currently running?
  /// Eventually we'll track this with more specificity, like whether
  /// it's running on a specific thread, enqueued on a specific actor,
  /// etc.
  bool isRunning() const { return Flags & IsRunning; }
  ActiveTaskStatus withRunning(bool isRunning) const {
    return ActiveTaskStatus(Record, isRunning ? (Flags | IsRunning)
                                              : (Flags & ~IsRunning));
  }

  /// Is there an active lock on the cancellation information?
  bool isLocked() const { return Flags & IsLocked; }
  ActiveTaskStatus withLockingRecord(TaskStatusRecord *lockRecord) const {
    assert(!isLocked());
    assert(lockRecord->Parent == Record);
    return ActiveTaskStatus(lockRecord, Flags | IsLocked);
  }

  JobPriority getStoredPriority() const {
    return JobPriority(Flags & PriorityMask);
  }
  bool isStoredPriorityEscalated() const {
    return Flags & IsEscalated;
  }
  ActiveTaskStatus withEscalatedPriority(JobPriority priority) const {
    assert(priority > getStoredPriority());
    return ActiveTaskStatus(Record,
                            (Flags & ~PriorityMask)
                               | IsEscalated | uintptr_t(priority));
  }
  ActiveTaskStatus withoutStoredPriorityEscalation() const {
    assert(isStoredPriorityEscalated());
    return ActiveTaskStatus(Record, Flags & ~IsEscalated);
  }

  /// Return the innermost cancellation record.  Code running
  /// asynchronously with this task should not access this record
  /// without having first locked it; see swift_taskCancel.
  TaskStatusRecord *getInnermostRecord() const {
    return Record;
  }
  ActiveTaskStatus withInnermostRecord(TaskStatusRecord *newRecord) {
    return ActiveTaskStatus(newRecord, Flags);
  }

  static TaskStatusRecord *getStatusRecordParent(TaskStatusRecord *ptr);

  using record_iterator =
    LinkedListIterator<TaskStatusRecord, getStatusRecordParent>;
  llvm::iterator_range<record_iterator> records() const {
    return record_iterator::rangeBeginning(getInnermostRecord());
  }
};

/// The size of an allocator slab.
static constexpr size_t SlabCapacity = 1000;
extern Metadata TaskAllocatorSlabMetadata;

using TaskAllocator = StackAllocator<SlabCapacity, &TaskAllocatorSlabMetadata>;

/// Private storage in an AsyncTask object.
struct AsyncTask::PrivateStorage {
  /// The currently-active information about cancellation.
  /// Currently two words.
  swift::atomic<ActiveTaskStatus> Status;

  /// The allocator for the task stack.
  /// Currently 2 words + 8 bytes.
  TaskAllocator Allocator;

  /// Storage for task-local values.
  /// Currently one word.
  TaskLocal::Storage Local;

  /// State inside the AsyncTask whose state is only managed by the exclusivity
  /// runtime in stdlibCore. We zero initialize to provide a safe initial value,
  /// but actually initialize its bit state to a const global provided by
  /// libswiftCore so that libswiftCore can control the layout of our initial
  /// state.
  uintptr_t ExclusivityAccessSet[2] = {0, 0};

  /// The top 32 bits of the task ID. The bottom 32 bits are in Job::Id.
  uint32_t Id;

  PrivateStorage(JobFlags flags)
      : Status(ActiveTaskStatus(flags)), Local(TaskLocal::Storage()) {}

  PrivateStorage(JobFlags flags, void *slab, size_t slabCapacity)
      : Status(ActiveTaskStatus(flags)), Allocator(slab, slabCapacity),
        Local(TaskLocal::Storage()) {}

  void complete(AsyncTask *task) {
    // Destroy and deallocate any remaining task local items.
    // We need to do this before we destroy the task local deallocator.
    Local.destroy(task);

    this->~PrivateStorage();
  }
};

static_assert(sizeof(AsyncTask::PrivateStorage)
                <= sizeof(AsyncTask::OpaquePrivateStorage) &&
              alignof(AsyncTask::PrivateStorage)
                <= alignof(AsyncTask::OpaquePrivateStorage),
              "Task-private storage doesn't fit in reserved space");

inline AsyncTask::PrivateStorage &
AsyncTask::OpaquePrivateStorage::get() {
  return reinterpret_cast<PrivateStorage &>(*this);
}
inline const AsyncTask::PrivateStorage &
AsyncTask::OpaquePrivateStorage::get() const {
  return reinterpret_cast<const PrivateStorage &>(*this);
}
inline void AsyncTask::OpaquePrivateStorage::initialize(AsyncTask *task) {
  new (this) PrivateStorage(task->Flags);
}
inline void
AsyncTask::OpaquePrivateStorage::initializeWithSlab(AsyncTask *task,
                                                    void *slab,
                                                    size_t slabCapacity) {
  new (this) PrivateStorage(task->Flags, slab, slabCapacity);
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
  return _private().Status.load(std::memory_order_relaxed)
                          .isCancelled();
}

inline void AsyncTask::flagAsRunning() {
  SWIFT_TASK_DEBUG_LOG("%p->flagAsRunning()", this);
  auto oldStatus = _private().Status.load(std::memory_order_relaxed);
  while (true) {
    assert(!oldStatus.isRunning());
    if (oldStatus.isLocked()) {
      flagAsRunning_slow();
      adoptTaskVoucher(this);
      swift_task_enterThreadLocalContextBackdeploy56(
          (char *)&_private().ExclusivityAccessSet[0]);
      return;
    }

    auto newStatus = oldStatus.withRunning(true);
    if (newStatus.isStoredPriorityEscalated()) {
      newStatus = newStatus.withoutStoredPriorityEscalation();
      Flags.setPriority(oldStatus.getStoredPriority());
    }

    if (_private().Status.compare_exchange_weak(oldStatus, newStatus,
                                                std::memory_order_relaxed,
                                                std::memory_order_relaxed)) {
      adoptTaskVoucher(this);
      swift_task_enterThreadLocalContextBackdeploy56(
          (char *)&_private().ExclusivityAccessSet[0]);
      return;
    }
  }
}

inline void AsyncTask::flagAsSuspended() {
  SWIFT_TASK_DEBUG_LOG("%p->flagAsSuspended()", this);
  auto oldStatus = _private().Status.load(std::memory_order_relaxed);
  while (true) {
    assert(oldStatus.isRunning());
    if (oldStatus.isLocked()) {
      flagAsSuspended_slow();
      swift_task_exitThreadLocalContextBackdeploy56(
          (char *)&_private().ExclusivityAccessSet[0]);
      restoreTaskVoucher(this);
      return;
    }

    auto newStatus = oldStatus.withRunning(false);
    if (newStatus.isStoredPriorityEscalated()) {
      newStatus = newStatus.withoutStoredPriorityEscalation();
      Flags.setPriority(oldStatus.getStoredPriority());
    }

    if (_private().Status.compare_exchange_weak(oldStatus, newStatus,
                                                std::memory_order_relaxed,
                                                std::memory_order_relaxed)) {
      swift_task_exitThreadLocalContextBackdeploy56(
          (char *)&_private().ExclusivityAccessSet[0]);
      restoreTaskVoucher(this);
      return;
    }
  }
}


} // namespace swift

#endif // SWIFT_CONCURRENCY_TASKPRIVATE_BACKDEPLOY56_H
