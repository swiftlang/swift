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
/// When a task wants to iterate task status records, but
/// it sees that the locked bit is set in the `Status` field, it
/// must acquire the global status-record lock, find this record
/// (which should be the innermost record), and wait for an unlock.
class StatusRecordLockRecord
    : public AtomicWaitQueue<StatusRecordLockRecord, LazyMutex>,
      public TaskStatusRecord {
public:
  StatusRecordLockRecord(TaskStatusRecord *parent)
    : TaskStatusRecord(TaskStatusRecordKind::Private_RecordLock, parent) {
  }

  void updateForNewArguments(TaskStatusRecord *parent) {
    Parent = parent;
  }

  static bool classof(const TaskStatusRecord *record) {
    return record->getKind() == TaskStatusRecordKind::Private_RecordLock;
  }
};
}

/// Wait for a task's status record lock to be unlocked.
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

      // The innermost entry should be a record lock record; wait
      // for it to be unlocked.
      auto record = oldStatus.getInnermostRecord();
      return cast<StatusRecordLockRecord>(record);
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

static std::memory_order getLoadOrdering(LockContext lockContext) {
  return lockContext != LockContext::OnTask
                          ? std::memory_order_acquire
                          : std::memory_order_relaxed;
}

/// Call the given function while holding the task status record lock.
///
/// The value in `status` will be updated with the current status value
/// (ignoring the `TaskStatusLockRecord`) before calling the function,
/// and the value there will be written back into the task status after
/// calling the function.
template <class Fn>
static bool withStatusRecordLock(AsyncTask *task,
                                 LockContext lockContext,
                                 ActiveTaskStatus &status,
                                 Fn &&fn) {
  StatusRecordLockRecord::Worker worker(StatusRecordLockLock);

  auto loadOrdering = getLoadOrdering(lockContext);

  TaskStatusRecord *oldRecord;
  StatusRecordLockRecord *lockingRecord;

  // Take the lock record
  while (true) {
    // If the old info says we're locked, wait for the lock to clear.
    if (status.isStatusRecordLocked()) {
      waitForStatusRecordUnlock(task, status); // Will update status
      continue;
    }

    // Make (or reconfigure) a lock record.
    oldRecord = status.getInnermostRecord();
    lockingRecord = worker.createQueue(oldRecord);

    // Install the lock record as the top of the queue.
    ActiveTaskStatus newStatus = status.withLockingRecord(lockingRecord);
    if (task->_private()._status().compare_exchange_weak(status, newStatus,
            /*success*/ std::memory_order_release,
            /*failure*/ loadOrdering)) {
      status = newStatus;

      status.traceStatusChanged(task);
      worker.flagQueueIsPublished(lockingRecord);
      break;
    }
  }

  // We've locked the status
  assert(worker.isWorkerThread());

  // Call the function.
  std::forward<Fn>(fn)();

  // Release lock record, restore the old record at the top
  //
  // We may need to reload the status since other flags could have changed on it
  // while it is locked - namely cancelled bit, max priority, isEscalated.
  while (true) {
    assert(status.isStatusRecordLocked());
    auto newStatus = status.withoutLockingRecord();

    if (task->_private()._status().compare_exchange_weak(status, newStatus,
            /*success*/ std::memory_order_relaxed,
            /*failure*/ std::memory_order_relaxed)) {
      status.traceStatusChanged(task);
      break;
    }
  }

  // Unblock any waiters.
  worker.finishAndUnpublishQueue([]{});

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
  return withStatusRecordLock(task, lockContext, status, [&] {
    fn(status);
  });
}

/**************************************************************************/
/*************************** RECORD MANAGEMENT ****************************/
/**************************************************************************/

SWIFT_CC(swift)
bool swift::addStatusRecord(
    TaskStatusRecord *newRecord,
    llvm::function_ref<bool(ActiveTaskStatus status)> shouldAddRecord) {

  auto task = swift_task_getCurrent();
  // Load the current state. We can use a relaxed load because we're
  // synchronous with the task.
  auto oldStatus = task->_private()._status().load(std::memory_order_relaxed);

  while (true) {
    // Wait for any active lock to be released.
    if (oldStatus.isStatusRecordLocked())
      waitForStatusRecordUnlock(task, oldStatus);

    // Reset the parent of the new record.
    newRecord->resetParent(oldStatus.getInnermostRecord());

    // Set the record as the new innermost record.
    ActiveTaskStatus newStatus = oldStatus.withInnermostRecord(newRecord);

    if (shouldAddRecord(newStatus)) {
      // We have to use a release on success to make the initialization of
      // the new record visible to an asynchronous thread trying to modify the
      // status records
      _swift_tsan_release(task);
      if (task->_private()._status().compare_exchange_weak(oldStatus, newStatus,
              /*success*/ std::memory_order_release,
              /*failure*/ std::memory_order_relaxed)) {
        return true;
      } else {
        // Retry
      }
    } else {
      return false;
    }
  }
}

SWIFT_CC(swift)
bool swift::removeStatusRecord(TaskStatusRecord *record) {
  auto task = swift_task_getCurrent();
  SWIFT_TASK_DEBUG_LOG("remove status record = %p, from current task = %p",
                       record, task);

  // Load the current state.
  auto &status = task->_private()._status();
  auto oldStatus = status.load(std::memory_order_relaxed);

  while (true) {
    // Wait for any active lock to be released.
    if (oldStatus.isStatusRecordLocked())
      waitForStatusRecordUnlock(task, oldStatus);

    // If the record is the innermost record, try to just pop it off.
    if (oldStatus.getInnermostRecord() == record) {
      ActiveTaskStatus newStatus =
        oldStatus.withInnermostRecord(record->getParent());
      if (status.compare_exchange_weak(oldStatus, newStatus,
             /*success*/ std::memory_order_relaxed,
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
  withStatusRecordLock(task, LockContext::OnTask, oldStatus, [&] {
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
  });

  return !oldStatus.isCancelled();
}

SWIFT_CC(swift)
static bool swift_task_hasTaskGroupStatusRecordImpl() {
  auto task = swift_task_getCurrent();

  // a group must be in a task, so if we're not in a task...
  // then, we certainly are not in a group either!
  if (!task)
    return false;

  bool foundTaskGroupRecord = false;
  withStatusRecordLock(task, LockContext::OnTask,
                       [&](ActiveTaskStatus &status) {
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

  withStatusRecordLock(parent, LockContext::OnTask, [&](ActiveTaskStatus &parentStatus) {
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

  withStatusRecordLock(parent, LockContext::OnTask, [&](ActiveTaskStatus &parentStatus) {
    group->removeChildTask(child);
  });
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

  withStatusRecordLock(task, LockContext::OtherAsynchronous, newStatus, [&] {
    for (auto cur : newStatus.records()) {
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

  withStatusRecordLock(task, LockContext::OnTask, newStatus, [&] {
    // Perform escalation operations for all the status records.
    for (auto cur: newStatus.records()) {
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

/**************************************************************************/
/******************************** DEADLINE ********************************/
/**************************************************************************/
SWIFT_CC(swift)
static NearestTaskDeadline swift_task_getNearestDeadlineImpl(AsyncTask *task) {
  // We don't have to worry about the deadline records being
  // concurrently modified, so we can just walk the record chain,
  // ignoring the possibility of a concurrent cancelling task.

  // Load the current state.
  auto &status = task->_private()._status();
  auto oldStatus = status.load(std::memory_order_relaxed);

  NearestTaskDeadline result;

  // If it's already cancelled, we're done.
  if (oldStatus.isCancelled()) {
    result.ValueKind = NearestTaskDeadline::AlreadyCancelled;
    return result;
  }

  // If it's locked, wait for the lock; we can't safely step through
  // the RecordLockStatusRecord on a different thread.
  if (oldStatus.isStatusRecordLocked()) {
    waitForStatusRecordUnlock(task, oldStatus);
    assert(!oldStatus.isStatusRecordLocked());
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
