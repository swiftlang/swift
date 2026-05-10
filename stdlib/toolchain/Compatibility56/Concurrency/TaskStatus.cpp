#include "swift/Basic/Lazy.h"
#include "Concurrency/Task.h"
#include "Concurrency/TaskStatus.h"
#include "Concurrency/TaskPrivate.h"
#include "Concurrency/Threading/Mutex.h"
#include "Runtime/AtomicWaitQueue.h"
#include <atomic>
#include <dlfcn.h>

namespace swift {

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

JobPriority swift_task_escalateBackdeploy56(AsyncTask *task,
                                                   JobPriority newPriority) {
  const auto task_escalate =
      reinterpret_cast<JobPriority (*)(AsyncTask *, JobPriority)>(
          SWIFT_LAZY_CONSTANT(dlsym(RTLD_DEFAULT, "swift_task_escalate")));
  if (task_escalate)
    return task_escalate(task, newPriority);
  // We don't have swift_task_escalate, link against swift_Concurrency
  abort();
}

} // namespace swift
