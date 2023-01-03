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

#include "swift/ABI/TaskStatus.h"
#include "../CompatibilityOverride/CompatibilityOverride.h"
#include "TaskPrivate.h"
#include "swift/Runtime/AtomicWaitQueue.h"
#include "swift/Runtime/Concurrency.h"
#include "swift/Threading/Mutex.h"
#include "swift/Threading/Thread.h"
#include <atomic>

using namespace swift;

inline TaskStatusRecord *
ActiveTaskStatus::getStatusRecordParent(TaskStatusRecord *ptr) {
  return ptr->getParent();
}

/**************************************************************************/
/************************* RECORD LOCK MANAGEMENT *************************/
/**************************************************************************/

/// A lock used to protect management of task-specific status
/// record locks.
static LazyMutex StatusRecordLockLock;

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
/// Note that this status record lock only protects concurrent
/// modifications/access from *other* threads which may be trying to
/// inspect/modify a task's status records. A thread which already
/// owns the StatusRecordLock may recursively take it and modify
/// the status record list.
///
/// When a task wants to iterate task status records, but
/// it sees that the locked bit is set in the `Status` field, it
/// must acquire the global status-record lock, find this record
/// (which should be the innermost record), and wait for an unlock if
/// the the task is not the lock owner. If it already owns the
/// status record lock, it may proceed.
///
class StatusRecordLockRecord
    : public AtomicWaitQueue<StatusRecordLockRecord, LazyMutex>,
      public TaskStatusRecord {
  Thread Owner;
public:

  StatusRecordLockRecord(TaskStatusRecord *parent)
    : TaskStatusRecord(TaskStatusRecordKind::Private_RecordLock, parent) {
    // When we create a lock record, we always take it pre-locked
    Owner = Thread::current();
  }

  void updateForNewArguments(TaskStatusRecord *parent) {
    Parent = parent;
  }

  static bool classof(const TaskStatusRecord *record) {
    return record->getKind() == TaskStatusRecordKind::Private_RecordLock;
  }

  bool isStatusRecordLockedBySelf() {
    return Owner == Thread::current();
  }

};
}

/// If the status record is already self locked, returns true
///
/// If the status record is not self locked, then wait for the owner to unlock
/// before we return. We still have to retry getting the lock ourselves and may
/// still run into a race here.
static bool waitForStatusRecordUnlockIfNotSelfLocked(AsyncTask *task,
  ActiveTaskStatus &oldStatus) {
  // Acquire the lock.
  StatusRecordLockRecord::Waiter waiter(StatusRecordLockLock);

  while (true) {
    assert(oldStatus.isStatusRecordLocked());
    bool selfLocked = false;

    bool waited = waiter.tryReloadAndWait([&]() -> StatusRecordLockRecord* {
      // Check that oldStatus is still correct.
      oldStatus = task->_private()._status().load(std::memory_order_acquire);

      if (!oldStatus.isStatusRecordLocked())
        return nullptr;

      auto record = cast<StatusRecordLockRecord>(oldStatus.getInnermostRecord());
      if (record->isStatusRecordLockedBySelf()) {
        selfLocked = true;
        return nullptr;
      }
      return record;
    });

    if (!waited)
      return selfLocked;

    // Reload the status before trying to relock.
    oldStatus = task->_private()._status().load(std::memory_order_acquire);
    if (!oldStatus.isStatusRecordLocked())
      return false;
  }
}

/// Wait for a task's status record lock to be unlocked. This asserts that the
/// lock is not owned by self as that would result in a deadlock.
///
/// When this function returns, `oldStatus` will have been updated
/// to the last value read and `isStatusRecordLocked()` will be false.
/// Of course, another thread may still be concurrently trying
/// to acquire the record lock.
static void waitForStatusRecordUnlock(AsyncTask *task,
                                      ActiveTaskStatus &oldStatus) {
  // Acquire the lock.
  StatusRecordLockRecord::Waiter waiter(StatusRecordLockLock);

  while (true) {
    assert(oldStatus.isStatusRecordLocked());

    bool waited = waiter.tryReloadAndWait([&]() -> StatusRecordLockRecord* {
      // Check that oldStatus is still correct.
      oldStatus = task->_private()._status().load(std::memory_order_acquire);
      if (!oldStatus.isStatusRecordLocked())
        return nullptr;

      // The innermost entry should be a record lock record; Verify that we are
      // not the owner and then wait for it to be unlocked.
      auto record = cast<StatusRecordLockRecord>(oldStatus.getInnermostRecord());
      if (record->isStatusRecordLockedBySelf()) {
        swift_Concurrency_fatalError(0, "Waiting on a status record lock that is owned by self");
      }
      return record;
    });
    if (!waited)
      return;

    // Reload the status before trying to relock.
    oldStatus = task->_private()._status().load(std::memory_order_acquire);
    if (!oldStatus.isStatusRecordLocked())
      return;
  }
}

enum class LockContext {
  /// The lock is being acquired from within the running task.
  OnTask,

  /// The lock is being acquired asynchronously in order to read the
  /// status records for some other reason.
  OtherAsynchronous
};

// We can do this because a task can only add status records to itself. If we
// ever change addStatusRecord to allow it to add status records to a different
// task, we will need to revisit this.
static std::memory_order getLoadOrdering(LockContext lockContext) {
  return lockContext != LockContext::OnTask
                          ? std::memory_order_acquire
                          : std::memory_order_relaxed;
}

/// Makes sure to call the `fn` while holding the StatusRecordLock of the
/// input task.
///
/// If the client of withStatusRecordLock has already loaded the status of the
/// task with the right barriers, they may pass it into this function to avoid a
/// double-load.
///
/// The input `fn` is invoked once while holding the status record lock.
///
/// The optional `statusUpdate` is invoked while releasing the StatusRecordLock
/// from the ActiveTaskStatus so that callers may make additional modifications
/// to ActiveTaskStatus flags. `statusUpdate` can be called multiple times in a
/// RMW loop and so much be idempotent.
static bool withStatusRecordLock(AsyncTask *task,
    LockContext lockContext, ActiveTaskStatus status,
    llvm::function_ref<void(ActiveTaskStatus)>fn,
    llvm::function_ref<void(ActiveTaskStatus, ActiveTaskStatus&)> statusUpdate = nullptr) {

  StatusRecordLockRecord::Worker worker(StatusRecordLockLock);

  auto loadOrdering = getLoadOrdering(lockContext);

  TaskStatusRecord *oldRecord;
  StatusRecordLockRecord *lockingRecord;

  // Take the lock record
  bool installedLockRecord = false;
  while (true) {
    if (status.isStatusRecordLocked() &&
        waitForStatusRecordUnlockIfNotSelfLocked(task, status)) {
      // Top record is status record lock and we own it.
      SWIFT_TASK_DEBUG_LOG("[StatusRecordLock] Lock for task %p is already owned by thread", task);
      break;
    }

    // Make (or reconfigure) a lock record
    oldRecord = status.getInnermostRecord();
    lockingRecord = worker.createQueue(oldRecord);

    // Install the lock record as the top of the queue with a store release to
    // publish the contents of the StatusRecordLockRecord. We will read the
    // status with a load acquire which will pair with any store releases done
    // by other threads also trying to acquire the lock/add new status records.
    ActiveTaskStatus newStatus = status.withLockingRecord(lockingRecord);
    if (task->_private()._status().compare_exchange_weak(status, newStatus,
            /*success*/ std::memory_order_release,
            /*failure*/ loadOrdering)) {
      status = newStatus;

      status.traceStatusChanged(task);
      worker.flagQueueIsPublished(lockingRecord);
      installedLockRecord = true;

      // We've locked the status
      assert(worker.isWorkerThread());
      break;
    }
  }

  // Call the function.
  fn(status);

  // Release lock record if we installed it earlier and restore the old record
  // at the top.
  while (true) {
    auto newStatus = status;

    assert(status.isStatusRecordLocked());
    if (installedLockRecord) {
      newStatus = status.withoutLockingRecord();
    }

    // If the caller of the function wanted to modify something, let them.
    if (statusUpdate) {
      statusUpdate(status, newStatus);
    }

    if (task->_private()._status().compare_exchange_weak(status, newStatus,
            /*success*/ std::memory_order_relaxed,
            /*failure*/ std::memory_order_relaxed)) {
      newStatus.traceStatusChanged(task);
      break;
    }
  }

  if (installedLockRecord) {
    // Unblock any waiters.
    worker.finishAndUnpublishQueue([]{});
  }

  return true;
}

/// A convenience version of the above for contexts that haven't already
/// done the load.
template <class Fn>
static bool withStatusRecordLock(AsyncTask *task,
                                 LockContext lockContext,
                                 Fn &&fn) {
  auto loadOrdering = getLoadOrdering(lockContext);
  ActiveTaskStatus status =
    task->_private()._status().load(loadOrdering);
  if (loadOrdering == std::memory_order_acquire)
    _swift_tsan_acquire(task);
  return withStatusRecordLock(task, lockContext, status, [&](ActiveTaskStatus taskStatus) {
    fn(taskStatus);
  });
}

/**************************************************************************/
/*************************** RECORD MANAGEMENT ****************************/
/**************************************************************************/

SWIFT_CC(swift)
bool swift::addStatusRecord(
    TaskStatusRecord *newRecord,
    llvm::function_ref<bool(ActiveTaskStatus, ActiveTaskStatus&)> shouldAddRecord) {

  auto task = swift_task_getCurrent();
  // Load the current state. We can use a relaxed load because we're
  // synchronous with the task.
  auto oldStatus = task->_private()._status().load(std::memory_order_relaxed);

  while (true) {
    // Wait for any active lock to be released.
    if (oldStatus.isStatusRecordLocked()) {
      bool selfLocked = waitForStatusRecordUnlockIfNotSelfLocked(task, oldStatus);
      assert(!selfLocked);
    }

    // Reset the parent of the new record.
    newRecord->resetParent(oldStatus.getInnermostRecord());

    // Set the record as the new innermost record.
    ActiveTaskStatus newStatus = oldStatus.withInnermostRecord(newRecord);

    if (shouldAddRecord(oldStatus, newStatus)) {
      // We have to use a release on success to make the initialization of
      // the new record visible to an asynchronous thread trying to modify the
      // status records
      _swift_tsan_release(task);
      if (task->_private()._status().compare_exchange_weak(oldStatus, newStatus,
              /*success*/ std::memory_order_release,
              /*failure*/ std::memory_order_relaxed)) {
        newStatus.traceStatusChanged(task);
        return true;
      } else {
        // Retry
      }
    } else {
      return false;
    }
  }
}

static void removeStatusRecordLocked(ActiveTaskStatus status, TaskStatusRecord *record) {
  bool removedRecord = false;
  auto cur = status.getInnermostRecord();
  assert(cur->getKind() == TaskStatusRecordKind::Private_RecordLock);

  // Splice the record out.
  while (cur != nullptr) {
    auto next = cur->getParent();
    if (next == record) {
      cur->spliceParent(record->getParent());
      removedRecord = true;
      break;
    }
    cur = next;
  }
  assert(removedRecord);
}

// For when we are trying to remove a record and also optionally trying to
// modify some flags in the ActiveTaskStatus at the same time.
SWIFT_CC(swift)
void swift::removeStatusRecord(AsyncTask *task, TaskStatusRecord *record,
     llvm::function_ref<void(ActiveTaskStatus, ActiveTaskStatus&)>fn = nullptr) {

  SWIFT_TASK_DEBUG_LOG("remove status record = %p, from task = %p",
                       record, task);

  // Load the current state.
  auto &status = task->_private()._status();
  auto oldStatus = status.load(std::memory_order_relaxed);

  if (oldStatus.isStatusRecordLocked() &&
        waitForStatusRecordUnlockIfNotSelfLocked(task, oldStatus)) {
    SWIFT_TASK_DEBUG_LOG("[StatusRecordLock] Lock for task %p is already owned by thread", task);
    // Case 1: Top record is status record lock and this thread owns it.
    //
    // Since we have the lock, we can just remove the record we care about which
    // should be in the list somewhere and then modify the flags
    removeStatusRecordLocked(oldStatus, record);

    if (fn) {
      // Client wants to modify the flags on the status - do it in a loop to
      // make sure we handle other concurrent updates.
      while (true) {
        auto newStatus = oldStatus;
        fn(oldStatus, newStatus);

        // We should still remain status record locked no matter what since we
        // came in with the lock already
        assert(newStatus.isStatusRecordLocked());

        if (task->_private()._status().compare_exchange_weak(oldStatus, newStatus,
               /*success*/ std::memory_order_relaxed,
               /*failure*/ std::memory_order_relaxed)) {
          newStatus.traceStatusChanged(task);
          return;
        }
      }
    } else {
      // Client doesn't have any other flags to change on status, we came in
      // self-locked and leave with the lock held. No other flags changed.
      return;
    }
  }

  assert(!oldStatus.isStatusRecordLocked());

  while (true) {
    // We raced with some *other* thread which concurrently locked this task.
    if (oldStatus.isStatusRecordLocked()) {
      waitForStatusRecordUnlock(task, oldStatus);
    }

    // Case 2: No status record lock, see if the record we are trying to pop off
    // is the topmost record and if so, just get it out and modify the status
    // flags if needed
    if (oldStatus.getInnermostRecord() == record) {
      auto newStatus = oldStatus.withInnermostRecord(record->getParent());

      if (fn) {
        fn(oldStatus, newStatus);
      }

      if (task->_private()._status().compare_exchange_weak(oldStatus, newStatus,
             /*success*/ std::memory_order_relaxed,
             /*failure*/ std::memory_order_relaxed)) {
        newStatus.traceStatusChanged(task);
        return;
      }
      // Restart the loop again - someone else modified status concurrently
      continue;
    }

    // Case 3: If the record is not the innermost record, we need to acquire the
    // status record lock; there's no way to splice the record list safely
    // otherwise
    break;
  }

  auto lockContext = (swift_task_getCurrent() == task) ? LockContext::OnTask : LockContext::OtherAsynchronous;

  withStatusRecordLock(task, lockContext, oldStatus, [&](ActiveTaskStatus status) {
    removeStatusRecordLocked(status, record);
  }, fn);

}

// Convenience wrapper for when we don't care to make any modifications to the
// flags fields of the active task status when we are removing a task status
// record
SWIFT_CC(swift)
void swift::removeStatusRecord(TaskStatusRecord *record) {

  auto task = swift_task_getCurrent();
  SWIFT_TASK_DEBUG_LOG("remove status record = %p, from current task = %p",
                       record, task);
  return removeStatusRecord(task, record);
}

SWIFT_CC(swift)
static bool swift_task_hasTaskGroupStatusRecordImpl() {
  auto task = swift_task_getCurrent();

  // a group must be in a task, so if we're not in a task...
  // then, we certainly are not in a group either!
  if (!task)
    return false;

  bool foundTaskGroupRecord = false;
  withStatusRecordLock(task, LockContext::OnTask, [&](ActiveTaskStatus status) {
    // Scan for the task group record within all the active records.
    for (auto record: status.records()) {
      if (record->getKind() == TaskStatusRecordKind::TaskGroup) {
        foundTaskGroupRecord = true;
        return;
      }
    }
  });

  return foundTaskGroupRecord;
}

/**************************************************************************/
/************************** CHILD TASK MANAGEMENT *************************/
/**************************************************************************/

// ==== Child tasks ------------------------------------------------------------

/// Called in the path of linking a child into a parent/group synchronously with
/// the parent task.
//
/// When called to link a child into a parent directly, this does not hold the
/// parent's task status record lock. When called to link a child into a task
/// group, this holds the parent's task status record lock.
SWIFT_CC(swift)
void swift::updateNewChildWithParentAndGroupState(AsyncTask *child,
                                                  ActiveTaskStatus parentStatus,
                                                  TaskGroup *group) {
  // We can take the fast path of just modifying the ActiveTaskStatus in the
  // child task since we know that it won't have any task status records and
  // cannot be accessed by anyone else since it hasn't been linked in yet.
  // Avoids the extra logic in `swift_task_cancel` and `swift_task_escalate`
  auto oldChildTaskStatus =
      child->_private()._status().load(std::memory_order_relaxed);
  assert(oldChildTaskStatus.getInnermostRecord() == NULL);

  auto newChildTaskStatus = oldChildTaskStatus;

  if (parentStatus.isCancelled() || (group && group->isCancelled())) {
    newChildTaskStatus = newChildTaskStatus.withCancelled();
  }

  // Propagate max priority of parent to child task's active status
  JobPriority pri = parentStatus.getStoredPriority();
  newChildTaskStatus =
      newChildTaskStatus.withNewPriority(withUserInteractivePriorityDowngrade(pri));

  child->_private()._status().store(newChildTaskStatus, std::memory_order_relaxed);
}

SWIFT_CC(swift)
static void swift_taskGroup_attachChildImpl(TaskGroup *group,
                                            AsyncTask *child) {

  // We are always called from the context of the parent
  //
  // Acquire the status record lock of parent - we want to synchronize with
  // concurrent cancellation or escalation as we're adding new tasks to the
  // group.
  auto parent = child->childFragment()->getParent();
  assert(parent == swift_task_getCurrent());

  withStatusRecordLock(parent, LockContext::OnTask, [&](ActiveTaskStatus parentStatus) {
    group->addChildTask(child);

    // After getting parent's status record lock, do some sanity checks to
    // see if parent task or group has state changes that need to be
    // propagated to the child.
    //
    // This is the same logic that we would do if we were adding a child
    // task status record - see also asyncLet_addImpl. Since we attach a
    // child task to a TaskGroupRecord instead, we synchronize on the
    // parent's task status and then update the child.
    updateNewChildWithParentAndGroupState(child, parentStatus, group);
  });
}

void swift::_swift_taskGroup_detachChild(TaskGroup *group,
                                         AsyncTask *child) {
  // We are called synchronously from the perspective of the owning task.
  // That doesn't necessarily mean the owning task *is* the current task,
  // though, just that it's not concurrently running.
  auto parent = child->childFragment()->getParent();

  withStatusRecordLock(parent, LockContext::OnTask, [&](ActiveTaskStatus unused) {
    group->removeChildTask(child);
  });
}

/****************************** CANCELLATION ******************************/
/**************************************************************************/

/// Perform any cancellation actions required by the given record.
static void performCancellationAction(TaskStatusRecord *record) {
  switch (record->getKind()) {
  // Child tasks need to be recursively cancelled.
  case TaskStatusRecordKind::ChildTask: {
    auto childRecord = cast<ChildTaskStatusRecord>(record);
    for (AsyncTask *child: childRecord->children())
      swift_task_cancel(child);
    return;
  }

  // Task groups need their children to be cancelled.  Note that we do
  // not want to formally cancel the task group itself; that property is
  // under the synchronous control of the task that owns the group.
  case TaskStatusRecordKind::TaskGroup: {
    auto groupRecord = cast<TaskGroupTaskStatusRecord>(record);
    _swift_taskGroup_cancelAllChildren(groupRecord->getGroup());
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

SWIFT_CC(swift)
static void swift_task_cancelImpl(AsyncTask *task) {
  SWIFT_TASK_DEBUG_LOG("cancel task = %p", task);

  auto oldStatus = task->_private()._status().load(std::memory_order_relaxed);
  auto newStatus = oldStatus;
  while (true) {
    if (oldStatus.isCancelled()) {
      return;
    }

    // Set cancelled bit even if oldStatus.isStatusRecordLocked()
    newStatus = oldStatus.withCancelled();

    // Perform an acquire operation on success, which pairs with the release
    // operation in addStatusRecord. This ensures that the contents of the
    // status records are visible to this thread, as well as the contents of any
    // cancellation handlers and the data they access. We place this acquire
    // barrier here, because the subsequent call to `withStatusRecordLock` might
    // not have its own acquire barrier. We're calling the four-argument version
    // which relies on the caller to have performed the first load, and if the
    // compare_exchange operation in withStatusRecordLock succeeds the first
    // time, then it won't perform an acquire.
    if (task->_private()._status().compare_exchange_weak(oldStatus, newStatus,
            /*success*/ std::memory_order_acquire,
            /*failure*/ std::memory_order_relaxed)) {
      _swift_tsan_acquire(task);
      break;
    }
  }

  newStatus.traceStatusChanged(task);
  if (newStatus.getInnermostRecord() == NULL) {
     // No records, nothing to propagate
     return;
  }

  withStatusRecordLock(task, LockContext::OtherAsynchronous, newStatus, [&](ActiveTaskStatus status) {
    for (auto cur : status.records()) {
      // Some of the cancellation actions can cause us to recursively
      // modify this list that is being iterated. However, cancellation is
      // happening from outside of the task so we know that no new records will
      // be added since that's only possible while on task.
      performCancellationAction(cur);
    }
  });
}

/**************************************************************************/
/******************************* ESCALATION *******************************/
/**************************************************************************/

/// Perform any escalation actions required by the given record.
static void performEscalationAction(TaskStatusRecord *record,
                                    JobPriority newPriority) {
  switch (record->getKind()) {
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

  SWIFT_TASK_DEBUG_LOG("Escalating %p to %#zx priority", task, newPriority);
  auto oldStatus = task->_private()._status().load(std::memory_order_relaxed);
  auto newStatus = oldStatus;

  while (true) {
    // Fast path: check that the stored priority is already at least
    // as high as the desired priority.
    if (oldStatus.getStoredPriority() >= newPriority) {
      SWIFT_TASK_DEBUG_LOG("Task is already at %#zx priority", oldStatus.getStoredPriority());
      return oldStatus.getStoredPriority();
    }

    // Regardless of whether status record is locked or not, update the priority
    // and RO bit on the task status
    if (oldStatus.isRunning() || oldStatus.isEnqueued()) {
      newStatus = oldStatus.withEscalatedPriority(newPriority);
    } else {
      newStatus = oldStatus.withNewPriority(newPriority);
    }

    if (task->_private()._status().compare_exchange_weak(oldStatus, newStatus,
            /* success */ std::memory_order_relaxed,
            /* failure */ std::memory_order_relaxed)) {
      break;
    }
  }

#if SWIFT_CONCURRENCY_ENABLE_PRIORITY_ESCALATION
  if (newStatus.isRunning()) {
    // The task is running, escalate the thread that is running it.
    ActiveTaskStatus *taskStatus;
    dispatch_lock_t *executionLock;

    taskStatus = (ActiveTaskStatus *) &task->_private()._status();
    executionLock = (dispatch_lock_t *) ((char*)taskStatus + ActiveTaskStatus::executionLockOffset());

    SWIFT_TASK_DEBUG_LOG("[Override] Escalating %p which is running on %#x to %#x", task, newStatus.currentExecutionLockOwner(), newPriority);
    swift_dispatch_lock_override_start_with_debounce(executionLock, newStatus.currentExecutionLockOwner(), (qos_class_t) newPriority);
  } else if (newStatus.isEnqueued()) {
    //  Task is not running, it's enqueued somewhere waiting to be run
    //
    // TODO (rokhinip): Add a stealer to escalate the thread request for
    // the task. Still mark the task has having been escalated so that the
    // thread will self override when it starts draining the task
    //
    // TODO (rokhinip): Add a signpost to flag that this is a potential
    // priority inversion
    SWIFT_TASK_DEBUG_LOG("[Override] Escalating %p which is enqueued", task);
  } else {
    SWIFT_TASK_DEBUG_LOG("[Override] Escalating %p which is suspended to %#x", task, newPriority);
  }
#endif

  if (newStatus.getInnermostRecord() == NULL) {
    return newStatus.getStoredPriority();
  }

  withStatusRecordLock(task, LockContext::OtherAsynchronous, newStatus, [&](ActiveTaskStatus status) {
    // We know that none of the escalation actions will recursively
    // modify the task status record list by adding or removing task records
    for (auto cur: status.records()) {
      performEscalationAction(cur, newPriority);
    }
  });
  // TODO (rokhinip): If the task is awaiting on another task that is not a
  // child task, we need to escalate whoever we are already awaiting on
  //
  // rdar://88093007 (Task escalation does not propagate to a future that it is
  // waiting on)

  return newStatus.getStoredPriority();
}

#define OVERRIDE_TASK_STATUS COMPATIBILITY_OVERRIDE
#include COMPATIBILITY_OVERRIDE_INCLUDE_PATH
