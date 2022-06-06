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

#include "CompatibilityOverride.h"
#include "ConcurrencyRuntime.h"
#include "swift/Runtime/Mutex.h"
#include "swift/Runtime/AtomicWaitQueue.h"
#include "TaskStatus.h"
#include "TaskPrivate.h"
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
static StaticMutex StatusRecordLockLock;

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
class StatusRecordLockRecord :
    public AtomicWaitQueue<StatusRecordLockRecord, StaticMutex>,
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
/// to the last value read and `isLocked()` will be false.
/// Of course, another thread may still be concurrently trying
/// to acquire the record lock.
static void waitForStatusRecordUnlock(AsyncTask *task,
                                      ActiveTaskStatus &oldStatus) {
  // Acquire the lock.
  StatusRecordLockRecord::Waiter waiter(StatusRecordLockLock);

  while (true) {
    assert(oldStatus.isLocked());

    bool waited = waiter.tryReloadAndWait([&]() -> StatusRecordLockRecord* {
      // Check that oldStatus is still correct.
      oldStatus = task->_private().Status.load(std::memory_order_acquire);
      if (!oldStatus.isLocked())
        return nullptr;

      // The innermost entry should be a record lock record; wait
      // for it to be unlocked.
      auto record = oldStatus.getInnermostRecord();
      return cast<StatusRecordLockRecord>(record);
    });
    if (!waited)
      return;

    // Reload the status before trying to relock.
    oldStatus = task->_private().Status.load(std::memory_order_acquire);
    if (!oldStatus.isLocked())
      return;
  }
}

enum class LockContext {
  /// The lock is being acquired from within the running task.
  OnTask,

  /// The lock is being acquired asynchronously in order to cancel the
  /// task.
  Cancellation,

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
///
/// As a special case, if `lockContext` is `Cancellation` and the task
/// is either already cancelled or can be cancelled without acquiring
/// the lock, then cancellation is performed, the lock is not taken,
/// and the function is not called.  `withStatusRecordLock` will return
/// false in this case, and `status` will still contain the updated
/// status value, for which `isCancelled()` will be true.
template <class Fn>
static bool withStatusRecordLock(AsyncTask *task,
                                 LockContext lockContext,
                                 ActiveTaskStatus &status,
                                 Fn &&fn) {
  StatusRecordLockRecord::Worker worker(StatusRecordLockLock);

  auto loadOrdering = getLoadOrdering(lockContext);
  bool forCancellation = lockContext == LockContext::Cancellation;

  // Load the current state.  We can use relaxed loads if this isn't
  // for cancellation because (1) this operation should be synchronous
  // with the task, so the only thing that can modify it asynchronously
  // is a cancelling thread, and (2) we'll reload with acquire ordering
  // if a cancelling thread forces us to wait for an unlock.

  while (true) {
    // Cancellation should be idempotent: if the task has already
    // been cancelled (or is being cancelled concurrently), there
    // shouldn't be any need to do this work again.
    if (status.isCancelled() && forCancellation)
      return false;

    // If the old info says we're locked, wait for the lock to clear.
    if (status.isLocked()) {
      waitForStatusRecordUnlock(task, status);
      continue;
    }

    // If we're cancelling and the task has no active status records,
    // try to just set the cancelled bit and return.
    auto oldRecord = status.getInnermostRecord();
    if (!oldRecord && forCancellation) {
      ActiveTaskStatus newStatus = status.withCancelled();
      if (task->_private().Status.compare_exchange_weak(status, newStatus,
            /*success*/ std::memory_order_relaxed,
            /*failure*/ loadOrdering)) {
        status = newStatus;
        return false;
      }

      // If that failed, just restart.
      continue;
    }

    // Make (or reconfigure) a lock record.
    auto recordLockRecord = worker.createQueue(oldRecord);

    // Install the lock record as the top of the queue.
    ActiveTaskStatus newStatus =
      status.withLockingRecord(recordLockRecord);
    if (forCancellation)
      newStatus = newStatus.withCancelled();
    if (task->_private().Status.compare_exchange_weak(status, newStatus,
           /*success*/ std::memory_order_release,
           /*failure*/ loadOrdering)) {

      // Update `status` for the purposes of the callback function.
      // Note that we don't include the record lock, but do need to
      // set the cancelled bit.
      if (forCancellation)
        status = status.withCancelled();

      worker.flagQueueIsPublished(recordLockRecord);
      break;
    }
  }

  assert(worker.isWorkerThread());

  // Call the function.
  std::forward<Fn>(fn)();

  // We can just unconditionally store because nobody can be modifying
  // the state while we've locked it.
  //
  // As a general matter, the task won't synchronize with anything we've
  // done here through the task status; it may not even realize we ever
  // acquired the lock.  If we need to change the state in a way that the
  // task will see, we need to do so in some other way, probably via
  // atomic objects in the task status records.  Because of this, we can
  // actually unpublish the lock with a relaxed store.
  assert(!status.isLocked());
  task->_private().Status.store(status,
                                /*success*/ std::memory_order_relaxed);

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
  ActiveTaskStatus status =
    task->_private().Status.load(getLoadOrdering(lockContext));
  return withStatusRecordLock(task, lockContext, status, [&] {
    fn(status);
  });
}

/**************************************************************************/
/*************************** RECORD MANAGEMENT ****************************/
/**************************************************************************/

SWIFT_CC(swift)
static bool swift_task_addStatusRecordImpl(TaskStatusRecord *newRecord) {
  auto task = swift_task_getCurrent();

  // Load the current state.  We can use a relaxed load because we're
  // synchronous with the task.
  auto oldStatus = task->_private().Status.load(std::memory_order_relaxed);

  while (true) {
    // Wait for any active lock to be released.
    if (oldStatus.isLocked())
      waitForStatusRecordUnlock(task, oldStatus);

    // Reset the parent of the new record.
    newRecord->resetParent(oldStatus.getInnermostRecord());

    // Set the record as the new innermost record.
    // We have to use a release on success to make the initialization of
    // the new record visible to the cancelling thread.
    ActiveTaskStatus newStatus = oldStatus.withInnermostRecord(newRecord);
    if (task->_private().Status.compare_exchange_weak(oldStatus, newStatus,
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
  auto oldStatus = task->_private().Status.load(std::memory_order_relaxed);

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
    ActiveTaskStatus newStatus = oldStatus.withInnermostRecord(newRecord);
    if (task->_private().Status.compare_exchange_weak(oldStatus, newStatus,
           /*success*/ std::memory_order_release,
           /*failure*/ std::memory_order_relaxed))
      return true;
  }
}

SWIFT_CC(swift)
static bool swift_task_removeStatusRecordImpl(TaskStatusRecord *record) {
  auto task = swift_task_getCurrent();
  SWIFT_TASK_DEBUG_LOG("remove status record = %p, from current task = %p",
                       record, task);

  // Load the current state.
  auto &status = task->_private().Status;
  auto oldStatus = status.load(std::memory_order_relaxed);

  while (true) {
    // Wait for any active lock to be released.
    if (oldStatus.isLocked())
      waitForStatusRecordUnlock(task, oldStatus);

    // If the record is the innermost record, try to just pop it off.
    if (oldStatus.getInnermostRecord() == record) {
      ActiveTaskStatus newStatus =
        oldStatus.withInnermostRecord(record->getParent());
      if (status.compare_exchange_weak(oldStatus, newStatus,
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
SWIFT_CC(swift)
static ChildTaskStatusRecord*
swift_task_attachChildImpl(AsyncTask *child) {
  void *allocation = malloc(sizeof(swift::ChildTaskStatusRecord));
  auto record = new (allocation) swift::ChildTaskStatusRecord(child);
  SWIFT_TASK_DEBUG_LOG("attach child task = %p, record = %p, to current task = %p",
                       child, record, swift_task_getCurrent());
  swift_task_addStatusRecord(record);
  return record;
}

SWIFT_CC(swift)
static void
swift_task_detachChildImpl(ChildTaskStatusRecord *record) {
  swift_task_removeStatusRecord(record);
}

SWIFT_CC(swift)
static void swift_taskGroup_attachChildImpl(TaskGroup *group,
                                            AsyncTask *child) {

  // We are always called from the context of the parent
  //
  // Acquire the status record lock of parent - we want to synchronize with
  // concurrent cancellation or escalation as we're adding new tasks to the
  // group.

  auto parent = swift_task_getCurrent();
  withStatusRecordLock(parent, LockContext::OnTask,
                       [&](ActiveTaskStatus &status) {
    group->addChildTask(child);
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

SWIFT_CC(swift)
static void swift_task_cancelImpl(AsyncTask *task) {
  SWIFT_TASK_DEBUG_LOG("cancel task = %p", task);

  // withStatusRecordLock has some special behavior for
  // LockContext::Cancellation; the function only gets called
  // when they don't apply.
  withStatusRecordLock(task, LockContext::Cancellation,
                       [&](ActiveTaskStatus &status) {
    assert(status.isCancelled());

    // Carry out the cancellation operations associated with all
    // the active records.
    for (auto cur: status.records()) {
      performCancellationAction(cur);
    }
  });
}

SWIFT_CC(swift)
static void swift_task_cancel_group_child_tasksImpl(TaskGroup *group) {
  // Acquire the status record lock.
  //
  // Guaranteed to be called from the context of the parent task that created
  // the task group once we have #40616
  auto task = swift_task_getCurrent();
  withStatusRecordLock(task, LockContext::OnTask,
                       [&](ActiveTaskStatus &status) {
    // We purposefully DO NOT make this a cancellation by itself.
    // We are cancelling the task group, and all tasks it contains.
    // We are NOT cancelling the entire parent task though.
    performCancellationAction(group->getTaskRecord());
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
  // Fast path: check that the stored priority is already at least
  // as high as the desired priority.
  auto status = task->_private().Status.load(std::memory_order_relaxed);
  if (status.getStoredPriority() >= newPriority)
    return status.getStoredPriority();

  withStatusRecordLock(task, LockContext::OtherAsynchronous, status, [&] {
    // Now that we have the task's status lock, check again that the
    // priority is still too low.
    if (status.getStoredPriority() >= newPriority)
      return;
    status = status.withEscalatedPriority(newPriority);

    // TODO: attempt to escalate the thread running the task, if it's
    // currently running.  This probably requires the task to be enqueued
    // on a standard executor.

    // Perform escalation operations for all the status records.
    for (auto cur: status.records()) {
      performEscalationAction(cur, newPriority);
    }
  });

  return status.getStoredPriority();
}

void AsyncTask::flagAsRunning_slow() {
  withStatusRecordLock(this, LockContext::OnTask,
                       [&](ActiveTaskStatus &status) {
    assert(!status.isRunning());

    status = status.withRunning(true);
    if (status.isStoredPriorityEscalated()) {
      status = status.withoutStoredPriorityEscalation();
      Flags.setPriority(status.getStoredPriority());
    }
  });
}

void AsyncTask::flagAsSuspended_slow() {
  withStatusRecordLock(this, LockContext::OnTask,
                       [&](ActiveTaskStatus &status) {
    assert(status.isRunning());

    status = status.withRunning(false);
    if (status.isStoredPriorityEscalated()) {
      status = status.withoutStoredPriorityEscalation();
      Flags.setPriority(status.getStoredPriority());
    }
  });
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
  auto &status = task->_private().Status;
  auto oldStatus = status.load(std::memory_order_relaxed);

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
