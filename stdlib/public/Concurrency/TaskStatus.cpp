//===--- TaskStatus.cpp - Asynchronous task status tracking ---------------===//
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
// Routines for maintaining and interacting with the current state of a
// task, including tracking child tasks, deadlines, and cancellation.
//
//===----------------------------------------------------------------------===//

#include "../CompatibilityOverride/CompatibilityOverride.h"
#include "swift/Runtime/Concurrency.h"
#include "swift/Runtime/Mutex.h"
#include "swift/ABI/TaskStatus.h"
#include <atomic>

using namespace swift;

/**************************************************************************/
/************************* RECORD LOCK MANAGEMENT *************************/
/**************************************************************************/

/// A lock used to protect management of task-specific status
/// record locks.
static StaticConditionVariable::StaticMutex StatusRecordLockLock;

namespace {

/// A lock record which can be used to protect a task's active
/// status records.
///
/// For the most part, the active task status records of a task are
/// only accessed by the task itself.  If that were always true,
/// no synchronization would be required to change them.  However,
/// cancellation and escalation can occur asynchronously, and they
/// must be able to inspect the status records without worrying about
/// their concurrent modification or destruction of the records.
/// Therefore, these operations freeze the active status records
/// for their duration.  They do this by (1) setting a bit in the
/// task's `Status` field state which says that the records are
/// locked and (2) creating a lock record as the new innermost
/// status record.  When the operation is complete, it removes this
/// record and clears the lock bit, then notifies the lock record that
/// the locking operation is complete.
///
/// When a task wants to change its active status record, but
/// it sees that the locked bit is set in the `Status` field, it
/// must acquire the global status-record lock, find this record
/// (which should be the innermost record), and wait for an unlock.
class StatusRecordLockRecord : public TaskStatusRecord {
  /// A lock held by the locking thread for the duration of some
  /// operation.  The real lock for the status record state is the
  /// isLocked() bit in the active state; this lock is just a
  /// mechanism to allow threads to wait for that lock.  This is
  /// rather unfortunately heavyweight, but we're willing make
  /// locking expensive if it makes a task's normal record
  /// manipulations as cheap as possible.
  Mutex LockingThreadLock;

  /// A condition variable that the locking thread waits for if
  /// there are active unlock waiters when it tries to unlock.
  ConditionVariable LockerQueue;

  // These fields are protected by StatusRecordLockLock,
  // not LockingThreadLock.

  /// The number of threads waiting for Locked to become false.
  size_t NumUnlockWaiters : CHAR_BIT * sizeof(size_t) - 1;

  /// True if the lock has been cleared.
  size_t Locked : 1;

public:
  StatusRecordLockRecord(TaskStatusRecord *parent)
    : TaskStatusRecord(TaskStatusRecordKind::Private_RecordLock, parent),
      NumUnlockWaiters(0), Locked(true) {
    // This is always initialized on the locking thread, and
    // the private lock starts off locked.
    LockingThreadLock.lock();
  }

  ~StatusRecordLockRecord() {
    // Unlock the lock before destroying it.
    if (Locked) LockingThreadLock.unlock();
  }

  /// Wait on the queue until there's an unlock.
  void
  waitForUnlock(StaticConditionVariable::StaticMutex::ScopedLock &globalLock) {
    assert(Locked);

    // Flag that we're waiting, then drop the global lock.
    NumUnlockWaiters++;
    {
      StaticConditionVariable::StaticMutex::ScopedUnlock globalUnlock(
          StatusRecordLockLock);

      // Attempt to acquire the locking-thread lock, thereby
      // waiting until the locking thread unlocks the record.
      {
        Mutex::ScopedLock acquirePrivateLock(LockingThreadLock);
      }

      // Now reacquire the global lock.
    }

    // The record should always be unlocked now.
    assert(!Locked);

    // Remove ourselves from the count, and if the count is zero,
    // wake the locking thread.
    NumUnlockWaiters--;
    if (NumUnlockWaiters == 0)
      LockerQueue.notifyAll();
  }

  /// Wake up any threads that were waiting for unlock.  Must be
  /// called by the locking thread.
  void unlock() {
    StaticConditionVariable::StaticMutex::ScopedLock globalLock(
        StatusRecordLockLock);
    assert(Locked);
    Locked = false;

    // Unlock the locking-thread lock, balancing out the lock()
    // call in the constructor.  This allows any unlock waiters
    // to wake up.
    LockingThreadLock.unlock();

    // As soon as we don't have any unlock waiters, we're done.
    while (NumUnlockWaiters) {
      // In the meantime, wait on the locker queue, temporarily
      // releasing the global lock.
      // FIXME: this is a priority inversion; we really want to
      // escalate the priority of the waiting threads.
      StatusRecordLockLock.wait(LockerQueue);
    }
  }

  static bool classof(const TaskStatusRecord *record) {
    return record->getKind() == TaskStatusRecordKind::Private_RecordLock;
  }
};

}

/// Wait for a task's status record lock to be unlocked.
///
/// When this function returns, `oldStatus` will have been updated
/// to the last value read and `isLocked()` will be false.
/// Of course, another thread may still be concurrently trying
/// to acquire the record lock.
static void waitForStatusRecordUnlock(AsyncTask *task,
                                      ActiveTaskStatus &oldStatus) {
  assert(oldStatus.isLocked());

  // Acquire the lock.
  StaticConditionVariable::StaticMutex::ScopedLock globalLock(
      StatusRecordLockLock);

  while (true) {
    // Check that oldStatus is still correct.
    oldStatus = task->Status.load(std::memory_order_acquire);
    if (!oldStatus.isLocked())
      return;

    // The innermost entry should be a record lock record; wait
    // for it to be unlocked.
    auto record = oldStatus.getInnermostRecord();
    auto recordLockRecord = cast<StatusRecordLockRecord>(record);
    recordLockRecord->waitForUnlock(globalLock);
  }
}

/// Acquire a task's status record lock and return the
/// previous value of its status record state.
///
/// If `forCancellation` is true, the cancelled bit will be set in the
/// state, and the lock will not be acquired if the task is already
/// cancelled or can be cancelled without the lock.  If this occurs,
/// `isCancelled()` will be true for the return value.
static ActiveTaskStatus
acquireStatusRecordLock(AsyncTask *task,
                        Optional<StatusRecordLockRecord> &recordLockRecord,
                        bool forCancellation) {
  auto loadOrdering = forCancellation
                        ? std::memory_order_acquire
                        : std::memory_order_relaxed;

  // Load the current state.  We can use relaxed loads if this isn't
  // for cancellation because (1) this operation should be synchronous
  // with the task, so the only thing that can modify it asynchronously
  // is a cancelling thread, and (2) we'll reload with acquire ordering
  // if a cancelling thread forces us to wait for an unlock.
  auto oldStatus = task->Status.load(loadOrdering);

  while (true) {
    // Cancellation should be idempotent: if the task has already
    // been cancelled (or is being cancelled concurrently), there
    // shouldn't be any need to do this work again.
    if (oldStatus.isCancelled() && forCancellation)
      return oldStatus;

    // If the old info says we're locked, wait for the lock to clear.
    if (oldStatus.isLocked()) {
      waitForStatusRecordUnlock(task, oldStatus);
      continue;
    }

    // If we're cancelling and the task has no active status records,
    // try to just set the cancelled bit and return.
    auto oldRecord = oldStatus.getInnermostRecord();
    if (!oldRecord && forCancellation) {
      ActiveTaskStatus newStatus(nullptr,
                                 /*cancelled*/ true,
                                 /*locked*/ false);
      if (task->Status.compare_exchange_weak(oldStatus, newStatus,
            /*success*/ std::memory_order_relaxed,
            /*failure*/ loadOrdering))
        return newStatus;

      // If that failed, just restart.
      continue;
    }

    // Make (or reconfigure) a lock record.
    if (!recordLockRecord) {
      recordLockRecord.emplace(oldRecord);
    } else {
      recordLockRecord->resetParent(oldRecord);
    }

    // Install the lock record as the active cancellation info, or
    // restart if that fails.
    bool newIsCancelled = forCancellation || oldStatus.isCancelled();
    ActiveTaskStatus newStatus(&*recordLockRecord,
                               /*cancelled*/ newIsCancelled,
                               /*locked*/ true);
    if (task->Status.compare_exchange_weak(oldStatus, newStatus,
           /*success*/ std::memory_order_release,
           /*failure*/ loadOrdering))
      return oldStatus;
  }
}

/// Release a task's status record lock that was previously
/// acquired on this thread.
static void releaseStatusRecordLock(AsyncTask *task,
                                    ActiveTaskStatus newStatus,
                     Optional<StatusRecordLockRecord> &recordLockRecord) {
  assert(!newStatus.isLocked());

  // We can just unconditionally store because nobody can be modifying
  // the state while we've locked it.  The task shouldn't depend
  // on memory-ordering with anything we've done, so we can use a
  // relaxed store.
  task->Status.store(newStatus, std::memory_order_relaxed);

  // Unlock the record lock.
  recordLockRecord->unlock();
}

/**************************************************************************/
/*************************** RECORD MANAGEMENT ****************************/
/**************************************************************************/

SWIFT_CC(swift)
static bool swift_task_addStatusRecordImpl(TaskStatusRecord *newRecord) {
  auto task = swift_task_getCurrent();

  // Load the current state.  We can use a relaxed load because we're
  // synchronous with the task.
  auto oldStatus = task->Status.load(std::memory_order_relaxed);

  while (true) {
    // Wait for any active lock to be released.
    if (oldStatus.isLocked())
      waitForStatusRecordUnlock(task, oldStatus);

    // Reset the parent of the new record.
    newRecord->resetParent(oldStatus.getInnermostRecord());

    // Set the record as the new innermost record.
    // We have to use a release on success to make the initialization of
    // the new record visible to the cancelling thread.
    ActiveTaskStatus newStatus(newRecord,
                               oldStatus.isCancelled(),
                               /*locked*/ false);
    if (task->Status.compare_exchange_weak(oldStatus, newStatus,
           /*success*/ std::memory_order_release,
           /*failure*/ std::memory_order_relaxed))
      return !oldStatus.isCancelled();
  }
}

SWIFT_CC(swift)
static bool swift_task_tryAddStatusRecordImpl(TaskStatusRecord *newRecord) {
  auto task = swift_task_getCurrent();

  // Load the current state.  We can use a relaxed load because we're
  // synchronous with the task.
  auto oldStatus = task->Status.load(std::memory_order_relaxed);

  while (true) {
    // If the old info is already cancelled, do nothing.
    if (oldStatus.isCancelled())
      return false;

    // Wait for any active lock to be released.
    if (oldStatus.isLocked()) {
      waitForStatusRecordUnlock(task, oldStatus);

      if (oldStatus.isCancelled())
        return false;
    }

    // Reset the parent of the new record.
    newRecord->resetParent(oldStatus.getInnermostRecord());

    // Set the record as the new innermost record.
    // We have to use a release on success to make the initialization of
    // the new record visible to the cancelling thread.
    ActiveTaskStatus newStatus(newRecord,
                               /*cancelled*/ false,
                               /*locked*/ false);
    if (task->Status.compare_exchange_weak(oldStatus, newStatus,
           /*success*/ std::memory_order_release,
           /*failure*/ std::memory_order_relaxed))
      return true;
  }
}

SWIFT_CC(swift)
static bool swift_task_removeStatusRecordImpl(TaskStatusRecord *record) {
  auto task = swift_task_getCurrent();

  // Load the current state.
  auto oldStatus = task->Status.load(std::memory_order_relaxed);

  while (true) {
    // Wait for any active lock to be released.
    if (oldStatus.isLocked())
      waitForStatusRecordUnlock(task, oldStatus);

    // If the record is the innermost record, try to just pop it off.
    if (oldStatus.getInnermostRecord() == record) {
      ActiveTaskStatus newStatus(record->getParent(),
                                 oldStatus.isCancelled(),
                                 /*locked*/ false);
      if (task->Status.compare_exchange_weak(oldStatus, newStatus,
             /*success*/ std::memory_order_release,
             /*failure*/ std::memory_order_relaxed)) {
        return !oldStatus.isCancelled();
      }

      // Otherwise, restart.
      continue;
    }

    // If the record is not the innermost record, we need to acquire the
    // record lock; there's no way to splice the record list safely with
    // a thread that's attempting to acquire the lock.
    break;
  }

  // Acquire the status record lock.
  Optional<StatusRecordLockRecord> recordLockRecord;
  oldStatus = acquireStatusRecordLock(task, recordLockRecord,
                                      /*forCancellation*/ false);
  assert(!oldStatus.isLocked());

  // We can't observe the record to be the innermost record here because
  // that would require some other thread to be concurrently structurally
  // changing the set of status records, but we're running
  // synchronously with the task.
  auto cur = oldStatus.getInnermostRecord();
  assert(cur != record);

  // Splice the record out.
  while (true) {
    auto next = cur->getParent();
    if (next == record) {
      cur->spliceParent(record->getParent());
      break;
    }
  }

  // Release the lock.  Since the record can't be the root, we don't
  // have to worry about replacing the root, and oldStatus is always
  // exactly what we want to restore.
  releaseStatusRecordLock(task, oldStatus, recordLockRecord);

  return !oldStatus.isCancelled();
}

SWIFT_CC(swift)
static bool swift_task_hasTaskGroupStatusRecordImpl() {
  auto task = swift_task_getCurrent();

  Optional<StatusRecordLockRecord> recordLockRecord;

  // Acquire the status record lock.
  auto oldStatus = acquireStatusRecordLock(task, recordLockRecord,
      /*forCancellation*/ false);
  assert(!oldStatus.isLocked());

  // Scan for the task group record within all the active records.
  auto foundTaskGroupRecord = false;
  for (auto record: oldStatus.records()) {
    if (record->getKind() == TaskStatusRecordKind::TaskGroup) {
      foundTaskGroupRecord = true;
      break; // out of the for loop
    }
  }

  // Release the status record lock, being sure to flag that
  // the task is now cancelled.
  ActiveTaskStatus cancelledStatus(oldStatus.getInnermostRecord(),
      /*cancelled*/ false, // FIXME: is this right, or must be the same as previous cancelled status?
      /*locked*/ false);
  releaseStatusRecordLock(task, cancelledStatus, recordLockRecord);

  return foundTaskGroupRecord;
}

/**************************************************************************/
/************************** CHILD TASK MANAGEMENT *************************/
/**************************************************************************/

// ==== Child tasks ------------------------------------------------------------
SWIFT_CC(swift)
static ChildTaskStatusRecord*
swift_task_attachChildImpl(AsyncTask *child) {
  void *allocation = malloc(sizeof(swift::ChildTaskStatusRecord));
  auto record = new (allocation) swift::ChildTaskStatusRecord(child);
  swift_task_addStatusRecord(record);
  return record;
}

SWIFT_CC(swift)
static void
swift_task_detachChildImpl(ChildTaskStatusRecord *record) {
  swift_task_removeStatusRecord(record);
}

/****************************** CANCELLATION ******************************/
/**************************************************************************/

/// Perform any cancellation actions required by the given record.
static void performCancellationAction(TaskStatusRecord *record) {
  switch (record->getKind()) {
  // Deadlines don't require any special support.
  case TaskStatusRecordKind::Deadline:
    return;

  // Child tasks need to be recursively cancelled.
  case TaskStatusRecordKind::ChildTask: {
    auto childRecord = cast<ChildTaskStatusRecord>(record);
    for (AsyncTask *child: childRecord->children())
      swift_task_cancel(child);
    return;
  }

  case TaskStatusRecordKind::TaskGroup: {
    auto childRecord = cast<TaskGroupTaskStatusRecord>(record);
    for (AsyncTask *child: childRecord->children())
      swift_task_cancel(child);
    return;
  }

  // Cancellation notifications need to be called.
  case TaskStatusRecordKind::CancellationNotification: {
    auto notification =
      cast<CancellationNotificationStatusRecord>(record);
    notification->run();
    return;
  }

  // Escalation notifications can be ignored.
  case TaskStatusRecordKind::EscalationNotification:
    return;

  // Record locks shouldn't be found this way, but they don't have
  // anything to do anyway.
  case TaskStatusRecordKind::Private_RecordLock:
    return;
  }

  // Other cases can fall through here and be ignored.
  // FIXME: allow dynamic extension/correction?
}

/// Perform any cancellation actions required by the given record.
static void performGroupCancellationAction(TaskStatusRecord *record) {
  switch (record->getKind()) {
  // We only need to cancel specific GroupChildTasks, not arbitrary child tasks.
  // A task may be parent to many tasks which are not part of a group after all.
  case TaskStatusRecordKind::ChildTask:
    return;

  case TaskStatusRecordKind::TaskGroup: {
    auto groupChildRecord = cast<TaskGroupTaskStatusRecord>(record);
    // Since a task can only be running a single task group at the same time,
    // we can always assume that the group record which we found is the one
    // we're intended to cancel child tasks for.
    //
    // A group enforces that tasks can not "escape" it, and as such once the group
    // returns, all its task have been completed.
    for (AsyncTask *child: groupChildRecord->children()) {
      swift_task_cancel(child);
    }
    return;
  }

  // All other kinds of records we handle the same way as in a normal cancellation
  case TaskStatusRecordKind::Deadline:
  case TaskStatusRecordKind::CancellationNotification:
  case TaskStatusRecordKind::EscalationNotification:
  case TaskStatusRecordKind::Private_RecordLock:
    performCancellationAction(record);
    return;
}

  // Other cases can fall through here and be ignored.
  // FIXME: allow dynamic extension/correction?
}

SWIFT_CC(swift)
static void swift_task_cancelImpl(AsyncTask *task) {
  Optional<StatusRecordLockRecord> recordLockRecord;

  // Acquire the status record lock.
  auto oldStatus = acquireStatusRecordLock(task, recordLockRecord,
                                           /*forCancellation*/ true);
  assert(!oldStatus.isLocked());

  // If we were already cancelled or were able to cancel without acquiring
  // the lock, there's nothing else to do.
  if (oldStatus.isCancelled()) {
    return;
  }

  // Otherwise, we've installed the lock record and are now the
  // locking thread.

  // Carry out the cancellation operations associated with all
  // the active records.
  for (auto cur: oldStatus.records()) {
    performCancellationAction(cur);
  }

  // Release the status record lock, being sure to flag that
  // the task is now cancelled.
  ActiveTaskStatus cancelledStatus(oldStatus.getInnermostRecord(),
                                   /*cancelled*/ true,
                                   /*locked*/ false);
  releaseStatusRecordLock(task, cancelledStatus, recordLockRecord);
}

SWIFT_CC(swift)
static void swift_task_cancel_group_child_tasksImpl(TaskGroup *group) {
  Optional<StatusRecordLockRecord> recordLockRecord;

  // Acquire the status record lock.
  //
  // We purposefully DO NOT make this a cancellation by itself.
  // We are cancelling the task group, and all tasks it contains.
  // We are NOT cancelling the entire parent task though.
  auto task = swift_task_getCurrent();
  auto oldStatus = acquireStatusRecordLock(task, recordLockRecord,
                                           /*forCancellation*/ false);
  // Carry out the cancellation operations associated with all
  // the active records.
  for (auto cur: oldStatus.records()) {
    performGroupCancellationAction(cur);
  }

  // Release the status record lock, being sure to flag that
  // the task is now cancelled.
  ActiveTaskStatus cancelledStatus(oldStatus.getInnermostRecord(),
                                   /*cancelled*/ oldStatus.isCancelled(),
                                   /*locked*/ false);
  releaseStatusRecordLock(task, cancelledStatus, recordLockRecord);
}

/**************************************************************************/
/******************************* ESCALATION *******************************/
/**************************************************************************/

/// Perform any escalation actions required by the given record.
static void performEscalationAction(TaskStatusRecord *record,
                                    JobPriority newPriority) {
  switch (record->getKind()) {
  // Deadlines don't require any special support.
  case TaskStatusRecordKind::Deadline:
    return;

  // Child tasks need to be recursively escalated.
  case TaskStatusRecordKind::ChildTask: {
    auto childRecord = cast<ChildTaskStatusRecord>(record);
    for (AsyncTask *child: childRecord->children())
      swift_task_escalate(child, newPriority);
    return;
  }
  case TaskStatusRecordKind::TaskGroup: {
    auto childRecord = cast<TaskGroupTaskStatusRecord>(record);
    for (AsyncTask *child: childRecord->children())
      swift_task_escalate(child, newPriority);
    return;
  }

  // Cancellation notifications can be ignore.
  case TaskStatusRecordKind::CancellationNotification:
    return;

  // Escalation notifications need to be called.
  case TaskStatusRecordKind::EscalationNotification:  {
    auto notification =
      cast<EscalationNotificationStatusRecord>(record);
    notification->run(newPriority);
    return;
  }

  // Record locks shouldn't be found this way, but they don't have
  // anything to do anyway.
  case TaskStatusRecordKind::Private_RecordLock:
    return;
  }

  // Other cases can fall through here and be ignored.
  // FIXME: allow dynamic extension/correction?
}

SWIFT_CC(swift)
JobPriority
static swift_task_escalateImpl(AsyncTask *task, JobPriority newPriority) {
  Optional<StatusRecordLockRecord> recordLockRecord;

  // Fast path: check that the task's priority is not already at least
  // as high as the target.  The task's priority can only be modified
  // under the status record lock; it's possible that the priority could
  // be getting simultaneously escalated, but it's okay for us to return
  // before that's complete.
  if (task->Flags.getPriority() >= newPriority)
    return task->Flags.getPriority();

  // Acquire the status record lock.
  auto oldStatus = acquireStatusRecordLock(task, recordLockRecord,
                                           /*forCancellation*/ false);
  assert(!oldStatus.isLocked());

  // Now that we have the task's status lock, check again that the
  // priority is still too low.
  auto priorityToReturn = task->Flags.getPriority();
  if (priorityToReturn < newPriority) {
    // Change the priority.
    task->Flags.setPriority(newPriority);
    priorityToReturn = newPriority;

    // TODO: attempt to escalate the thread running the task, if it's
    // currently running.  This probably requires the task to be enqueued
    // on a standard executor.

    // Perform escalation operations for all the status records.
    for (auto cur: oldStatus.records()) {
      performEscalationAction(cur, newPriority);
    }
  }

  // Release the status record lock, restoring the old status.
  releaseStatusRecordLock(task, oldStatus, recordLockRecord);

  return priorityToReturn;
}

/**************************************************************************/
/******************************** DEADLINE ********************************/
/**************************************************************************/
SWIFT_CC(swift)
static NearestTaskDeadline swift_task_getNearestDeadlineImpl(AsyncTask *task) {
  // We don't have to worry about the deadline records being
  // concurrently modified, so we can just walk the record chain,
  // ignoring the possibility of a concurrent cancelling task.

  // Load the current state.
  auto oldStatus = task->Status.load(std::memory_order_relaxed);

  NearestTaskDeadline result;

  // If it's already cancelled, we're done.
  if (oldStatus.isCancelled()) {
    result.ValueKind = NearestTaskDeadline::AlreadyCancelled;
    return result;
  }

  // If it's locked, wait for the lock; we can't safely step through
  // the RecordLockStatusRecord on a different thread.
  if (oldStatus.isLocked()) {
    waitForStatusRecordUnlock(task, oldStatus);
    assert(!oldStatus.isLocked());
  }

  // Walk all the records looking for deadlines.
  result.ValueKind = NearestTaskDeadline::None;
  for (const auto *record: oldStatus.records()) {
    auto deadlineRecord = dyn_cast<DeadlineStatusRecord>(record);
    if (!deadlineRecord) continue;
    auto recordDeadline = deadlineRecord->getDeadline();

    // If we already have a deadline, pick the earlier.
    if (result.ValueKind == NearestTaskDeadline::Active) {
      if (recordDeadline < result.Value)
        result.Value = recordDeadline;
    } else {
      result.Value = recordDeadline;
      result.ValueKind = NearestTaskDeadline::Active;
    }
  }
  return result;
}

#define OVERRIDE_TASK_STATUS COMPATIBILITY_OVERRIDE
#include COMPATIBILITY_OVERRIDE_INCLUDE_PATH
