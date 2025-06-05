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
#include "swift/Runtime/ExistentialContainer.h"
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

/// This function grabs the status record lock of the input task and invokes
/// `fn` while holding the StatusRecordLock of the input task.
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
static void withStatusRecordLock(
    AsyncTask *task, ActiveTaskStatus status,
    llvm::function_ref<void(ActiveTaskStatus)> fn,
    llvm::function_ref<void(ActiveTaskStatus, ActiveTaskStatus &)>
        statusUpdate = nullptr) {
  // We need to acquire the lock AND set the is-locked bit in the status so that
  // other threads attempting lockless operations can atomically check whether
  // another thread holds the lock. Various operations can be done with a
  // compare and swap if nothing holds the lock, but need to wait for the lock
  // to be released if something does hold it.
  //
  // Setting the bit can't be done atomically with acquiring the lock, but
  // that's OK. If we hold the lock but haven't yet set the bit, then we haven't
  // actually done any work and other threads can operate without violating our
  // assumptions. Once we swap in the is-locked bit and begin work, any
  // in-progress lockless work will fail its compare and swap operation, retry,
  // see that the is-locked bit is now set, and then wait for the lock.
  task->_private().statusLock.lock();

  bool alreadyLocked = false;

  // `status` was loaded before we acquired the lock. If its is-locked bit is
  // not set, then we know that this thread doesn't already hold the lock.
  // However, if the is-locked bit is set, then we don't know if this thread
  // held the lock or another thread did. In that case, we reload the status
  // after acquiring the lock. If the reloaded status still has the is-locked
  // bit set, then we know it's this thread. If it doesn't, then we know it was
  // a different thread.
  if (status.isStatusRecordLocked()) {
    status = task->_private()._status().load(std::memory_order_relaxed);
    alreadyLocked = status.isStatusRecordLocked();
  }

  // If it's already locked then this thread is the thread that locked it, and
  // we can leave that bit alone here.
  if (!alreadyLocked) {
    while (true) {
      ActiveTaskStatus newStatus = status.withStatusRecordLocked();

      // The caller may be reading a record that was locklessly emplaced. The
      // CONSUME here pairs with the release in addStatusRecord to ensure the
      // record's memory is visible to this thread.
      if (task->_private()._status().compare_exchange_weak(
              status, newStatus,
              /*success*/ SWIFT_MEMORY_ORDER_CONSUME,
              /*failure*/ std::memory_order_relaxed)) {
        status = newStatus;
        status.traceStatusChanged(task, false);
        break;
      }
    }
  }

  // We now hold the lock and we've set the bit (or the bit was already set).
  // Call the function to do the work.
  fn(status);

  while (true) {
    // If we were already locked and there's no status update, then we can skip
    // updating the status field.
    if (alreadyLocked && !statusUpdate)
      break;

    ActiveTaskStatus newStatus = status;

    // If we set the is-locked bit, clear it.
    if (!alreadyLocked) {
      newStatus = newStatus.withoutStatusRecordLocked();
    }

    // If the caller of the function wanted to modify something, let them.
    if (statusUpdate) {
      statusUpdate(status, newStatus);
    }

    // Records can only be read with the status lock held, so the lock's
    // memory barriers will ensure that a newly emplaced record will be
    // visible to any reader, and we can use relaxed ordering here.
    if (task->_private()._status().compare_exchange_weak(
            status, newStatus,
            /*success*/ std::memory_order_relaxed,
            /*failure*/ std::memory_order_relaxed)) {
      newStatus.traceStatusChanged(task, false);
      break;
    }
  }

  task->_private().statusLock.unlock();
}

/// A convenience version of the above for contexts that haven't already
/// done the load.
template <class Fn>
static void withStatusRecordLock(
    AsyncTask *task, Fn &&fn,
    llvm::function_ref<void(ActiveTaskStatus, ActiveTaskStatus &)>
        statusUpdate = nullptr) {
  ActiveTaskStatus status = task->_private()._status().load(std::memory_order_relaxed);
  withStatusRecordLock(
      task, status, [&](ActiveTaskStatus taskStatus) { fn(taskStatus); },
      /*statusUpdate=*/statusUpdate);
}

/**************************************************************************/
/*************************** RECORD MANAGEMENT ****************************/
/**************************************************************************/

SWIFT_CC(swift)
bool swift::addStatusRecord(AsyncTask *task, TaskStatusRecord *newRecord,
    ActiveTaskStatus& oldStatus,
    llvm::function_ref<bool(ActiveTaskStatus, ActiveTaskStatus&)> shouldAddRecord) {

  SWIFT_TASK_DEBUG_LOG("Adding %p record to task %p", newRecord, task);
  while (true) {
    if (oldStatus.isStatusRecordLocked()) {
      // If the record is locked, then acquire the lock and emplace the new
      // record with the lock held. We don't have any other work to do, so we
      // pass an empty function for `fn`, and give a `statusUpdate` that puts
      // the new record in place.
      bool addRecord = false;
      withStatusRecordLock(
          task, oldStatus, [](ActiveTaskStatus) {},
          [&](ActiveTaskStatus oldStatus, ActiveTaskStatus &newStatus) {
            // Reset the parent of the new record.
            newRecord->resetParent(newStatus.getInnermostRecord());
            ActiveTaskStatus modifiedStatus =
                newStatus.withInnermostRecord(newRecord);
            addRecord = shouldAddRecord(newStatus, modifiedStatus);
            if (addRecord)
              newStatus = modifiedStatus;
          });
      return addRecord;
    }

    // If the status record is not locked, try emplacing the new record without
    // locking.

    // Reset the parent of the new record.
    newRecord->resetParent(oldStatus.getInnermostRecord());

    ActiveTaskStatus newStatus = oldStatus.withInnermostRecord(newRecord);
    if (shouldAddRecord(oldStatus, newStatus)) {
      // We have to use a release on success to make the initialization of
      // the new record visible to another thread reading the new record.
      _swift_tsan_release(task);
      if (task->_private()._status().compare_exchange_weak(oldStatus, newStatus,
              /*success*/ std::memory_order_release,
              /*failure*/ std::memory_order_relaxed)) {
        newStatus.traceStatusChanged(task, false);
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
bool swift::addStatusRecord(AsyncTask *task, TaskStatusRecord *newRecord,
    llvm::function_ref<bool(ActiveTaskStatus, ActiveTaskStatus&)> shouldAddRecord) {
  auto oldStatus = task->_private()._status().load(std::memory_order_relaxed);

  return addStatusRecord(task, newRecord, oldStatus, shouldAddRecord);
}

SWIFT_CC(swift)
bool swift::addStatusRecordToSelf(TaskStatusRecord *record,
     llvm::function_ref<bool(ActiveTaskStatus, ActiveTaskStatus&)> testAddRecord) {
  return addStatusRecord(swift_task_getCurrent(), record, testAddRecord);
}

SWIFT_CC(swift)
bool swift::addStatusRecordToSelf(TaskStatusRecord *record, ActiveTaskStatus &status,
     llvm::function_ref<bool(ActiveTaskStatus, ActiveTaskStatus&)> testAddRecord) {
  return addStatusRecord(swift_task_getCurrent(), record, status, testAddRecord);
}

// Remove a status record that is not the innermost record. The status record
// lock must be held.
static void removeNonInnermostStatusRecordLocked(ActiveTaskStatus status,
                                                 TaskStatusRecord *record) {
  bool removedRecord = false;
  auto cur = status.getInnermostRecord();
  assert(status.isStatusRecordLocked());
  assert(cur != record);

  // Cut the record out.
  while (cur != nullptr) {
    auto next = cur->getParent();
    if (next == record) {
      cur->resetParent(record->getParent());
      removedRecord = true;
      break;
    }
    cur = next;
  }
  static_cast<void>(removedRecord);
  assert(removedRecord);
}

// For when we are trying to remove a record and also optionally trying to
// modify some flags in the ActiveTaskStatus at the same time.
SWIFT_CC(swift)
void swift::removeStatusRecord(AsyncTask *task, TaskStatusRecord *record,
     ActiveTaskStatus& oldStatus,
     llvm::function_ref<void(ActiveTaskStatus, ActiveTaskStatus&)>fn) {

  SWIFT_TASK_DEBUG_LOG("remove status record = %p, from task = %p",
                       record, task);

  while (true) {
    // If the record is locked, then either we wait for the lock or we own the
    // lock. Either way, acquire the status record lock and perform the removal.
    // If the record to be removed is not the innermost record, then we need to
    // acquire the lock to safely remove it.
    if (oldStatus.isStatusRecordLocked() ||
        oldStatus.getInnermostRecord() != record) {
      withStatusRecordLock(
          task, oldStatus,
          [&](ActiveTaskStatus lockedStatus) {
            // If the record is the innermost (always was, or became that way
            // while we waited) then we have to remove it in the status change
            // function, since changing the head of the list requires changing
            // the status. If it's not the innermost then we can remove it here.
            if (lockedStatus.getInnermostRecord() != record)
              removeNonInnermostStatusRecordLocked(lockedStatus, record);
          },
          [&](ActiveTaskStatus oldStatus, ActiveTaskStatus &newStatus) {
            // We're removing the innermost record, set a new status with the
            // record removed.
            if (newStatus.getInnermostRecord() == record)
              newStatus = newStatus.withInnermostRecord(record->getParent());

            if (fn) {
              fn(oldStatus, newStatus);
            }
          });
      return;
    }

    // Nobody holds the lock, and the record is the innermost record. Attempt to
    // remove it locklessly.
    auto newStatus = oldStatus.withInnermostRecord(record->getParent());

    if (fn) {
      fn(oldStatus, newStatus);
    }

    if (task->_private()._status().compare_exchange_weak(
            oldStatus, newStatus,
            /*success*/ std::memory_order_relaxed,
            /*failure*/ std::memory_order_relaxed)) {
      newStatus.traceStatusChanged(task, false);
      return;
    }

    // We failed to remove the record locklessly. Go back to the top and retry
    // removing it.
  }
}

// For when we are trying to remove a record and also optionally trying to
// modify some flags in the ActiveTaskStatus at the same time.
SWIFT_CC(swift)
void swift::removeStatusRecordWhere(
     AsyncTask *task,
     ActiveTaskStatus& oldStatus,
     llvm::function_ref<bool(ActiveTaskStatus, TaskStatusRecord*)> condition,
     llvm::function_ref<void(ActiveTaskStatus, ActiveTaskStatus&)> updateStatus) {
  assert(condition && "condition is required");
  SWIFT_TASK_DEBUG_LOG("remove status record where(), from task = %p",
                       task);

  // We're expected to look at the contents of the records, so we must hold the
  // status record lock to do this safely.
  bool removeInnermost = false;
  withStatusRecordLock(
      task, oldStatus,
      [&](ActiveTaskStatus status) {
        for (auto curr : status.records()) {
          // If the record is the innermost then we have to remove it in the
          // status change function, since changing the head of the list
          // requires changing the status. If it's not the innermost then we can
          // remove it here.
          //
          // We'll check the condition on the innermost record here so that we
          // only call it once on the innermost record, as callers expect it to
          // be called once per record, and the status update callback can be
          // called more than once. We hold the record lock throughout so we
          // know that the innermost record will be the same in the status
          // update callback as it was here.
          if (condition(status, curr)) {
            if (curr == status.getInnermostRecord())
              removeInnermost = true;
            else
              removeNonInnermostStatusRecordLocked(status, curr);
          }
        }
      },
      [&](ActiveTaskStatus oldStatus, ActiveTaskStatus &newStatus) {
        // Remove the innermost record here, if it's a match.
        if (removeInnermost) {
          auto *innermost = newStatus.getInnermostRecord();
          newStatus = newStatus.withInnermostRecord(innermost->getParent());
        }

        if (updateStatus) {
          updateStatus(oldStatus, newStatus);
        }
      });
}

template <typename TaskStatusRecordT>
SWIFT_CC(swift)
TaskStatusRecordT* swift::popStatusRecordOfType(AsyncTask *task) {
  TaskStatusRecordT *record = nullptr;
  bool alreadyRemovedRecord = false;
  removeStatusRecordWhere(task, [&](ActiveTaskStatus s, TaskStatusRecord *r) {
    if (alreadyRemovedRecord)
      return false;

    if (auto *match = dyn_cast<TaskStatusRecordT>(r)) {
      record = match;
      alreadyRemovedRecord = true;
      return true; // Remove this record.
    }

    return false;
  });
  return record;
}

// Convenience wrapper for when client hasn't already done the load of the
// status
SWIFT_CC(swift)
void swift::removeStatusRecord(AsyncTask *task, TaskStatusRecord *record,
     llvm::function_ref<void(ActiveTaskStatus, ActiveTaskStatus&)>fn) {
  auto oldStatus = task->_private()._status().load(std::memory_order_relaxed);
  return removeStatusRecord(task, record, oldStatus, fn);
}


SWIFT_CC(swift)
void swift::removeStatusRecordWhere(
    AsyncTask *task,
    llvm::function_ref<bool(ActiveTaskStatus, TaskStatusRecord*)> condition,
    llvm::function_ref<void(ActiveTaskStatus, ActiveTaskStatus&)>updateStatus) {
  auto status = task->_private()._status().load(std::memory_order_relaxed);
  return removeStatusRecordWhere(task, status, condition, updateStatus);
}

// Convenience wrapper for modifications on current task
SWIFT_CC(swift)
void swift::removeStatusRecordFromSelf(TaskStatusRecord *record, ActiveTaskStatus &status,
     llvm::function_ref<void(ActiveTaskStatus, ActiveTaskStatus&)>fn) {
  return removeStatusRecord(swift_task_getCurrent(), record, status, fn);
}

SWIFT_CC(swift)
void swift::removeStatusRecordFromSelf(TaskStatusRecord *record,
     llvm::function_ref<void(ActiveTaskStatus, ActiveTaskStatus&)>fn) {
  return removeStatusRecord(swift_task_getCurrent(), record, fn);
}

SWIFT_CC(swift)
void swift::updateStatusRecord(AsyncTask *task, TaskStatusRecord *record,
     llvm::function_ref<void()>updateRecord,
     ActiveTaskStatus& status,
     llvm::function_ref<void(ActiveTaskStatus, ActiveTaskStatus&)>fn) {

  SWIFT_TASK_DEBUG_LOG("Updating status record %p of task %p", record, task);
  withStatusRecordLock(task, status, [&](ActiveTaskStatus lockedStatus) {
#ifndef NDEBUG
    bool foundRecord = false;
    for (auto cur: lockedStatus.records()) {
      if (cur == record) {
        foundRecord = true;
        break;
      }
    }
    assert(foundRecord);
#endif
    updateRecord();
  }, fn);
}

SWIFT_CC(swift)
static bool swift_task_hasTaskGroupStatusRecordImpl() {
  auto task = swift_task_getCurrent();

  // a group must be in a task, so if we're not in a task...
  // then, we certainly are not in a group either!
  if (!task)
    return false;

  bool foundTaskGroupRecord = false;
  withStatusRecordLock(task, [&](ActiveTaskStatus status) {
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

///**************************************************************************/
///************************** TASK EXECUTORS ********************************/
///**************************************************************************/

TaskExecutorRef AsyncTask::getPreferredTaskExecutor(bool assumeHasRecord) {
  // We first check the executor preference status flag, in order to avoid
  // having to scan through the records of the task checking if there was
  // such record.
  //
  // This is an optimization in order to make the enqueue/run
  // path of a task avoid excessive work if a task had many records.
  if (!hasTaskExecutorPreferenceRecord()) {
    return TaskExecutorRef::undefined(); // "no executor preference"
  }

  TaskExecutorRef preference = TaskExecutorRef::undefined();
  withStatusRecordLock(this, [&](ActiveTaskStatus status) {
    for (auto record : status.records()) {
      if (record->getKind() == TaskStatusRecordKind::TaskExecutorPreference) {
        auto executorPreferenceRecord =
            cast<TaskExecutorPreferenceStatusRecord>(record);
        preference = executorPreferenceRecord->getPreferredExecutor();
        return;
      }
    }
  });

  return preference;
}

SWIFT_CC(swift)
static TaskExecutorRef
swift_task_getPreferredTaskExecutorImpl() {
  if (auto task = swift_task_getCurrent()) {
    return task->getPreferredTaskExecutor();
  }

  return TaskExecutorRef::undefined(); // "no executor preference"
}

SWIFT_CC(swift)
static TaskExecutorPreferenceStatusRecord *
swift_task_pushTaskExecutorPreferenceImpl(TaskExecutorRef taskExecutor) {
  auto task = swift_task_getCurrent();
  if (!task) {
    // we cannot push a preference if we're not in a task (including in
    // compatibility tests), so we return eagerly.
    return nullptr;
  }

  void *allocation = _swift_task_alloc_specific(
      task, sizeof(class TaskExecutorPreferenceStatusRecord));
  auto record =
      ::new (allocation) TaskExecutorPreferenceStatusRecord(
          taskExecutor,
          // we don't retain the executor by the task/record, because the "push"
          // is implemented as a scope which keeps the executor alive by itself
          // already, so we save the retain/release pair by the task doing it
          // as well. In contrast, unstructured task creation always retains
          // the executor.
          /*retainedExecutor=*/false);
  SWIFT_TASK_DEBUG_LOG("[TaskExecutorPreference] Create task executor "
                       "preference record %p for task:%p",
                       allocation, task);


  addStatusRecord(task, record,
                  [&](ActiveTaskStatus oldStatus, ActiveTaskStatus &newStatus) {
                    // We use a flag to mark a task executor preference is
                    // present in order to avoid looking for task executor
                    // records when running the task, and we know there is no
                    // task executor preference present.
                    newStatus = newStatus.withTaskExecutorPreference();

                    return true; // always add the record
                  });

  return record;
}

SWIFT_CC(swift)
static void swift_task_popTaskExecutorPreferenceImpl(
    TaskExecutorPreferenceStatusRecord *record) {
  SWIFT_TASK_DEBUG_LOG("[TaskExecutorPreference] Remove task executor "
                       "preference record %p from task:%p",
                       record, swift_task_getCurrent());
  // We keep count of how many records there are because if there is more than
  // one, it means the task status flag should still be "has task preference".
  int preferenceRecordsCount = 0;

  auto task = swift_task_getCurrent();
  if (!task)
    return;

  removeStatusRecordWhere(
      task,
      /*condition=*/[&](ActiveTaskStatus status, TaskStatusRecord *cur) {
        assert(status.hasTaskExecutorPreference() && "does not have record!");

          if (cur->getKind() == TaskStatusRecordKind::TaskExecutorPreference) {
            preferenceRecordsCount += 1;

            return preferenceRecordsCount == 1 &&
                   record == cast<TaskExecutorPreferenceStatusRecord>(cur);
          }
          return false;
      },
      /*updateStatus==*/[&](ActiveTaskStatus oldStatus, ActiveTaskStatus &newStatus) {
        if (preferenceRecordsCount == 1) {
          // if this was the last record removed, we flip the flag to off.
          assert(oldStatus.hasTaskExecutorPreference());
          newStatus = newStatus.withoutTaskExecutorPreference();
        }
      });

  swift_task_dealloc(record);
}

// Since the header would have incomplete declarations, we instead instantiate a concrete version of the function here
template SWIFT_CC(swift)
CancellationNotificationStatusRecord* swift::popStatusRecordOfType<CancellationNotificationStatusRecord>(AsyncTask *);

void AsyncTask::pushInitialTaskExecutorPreference(
    TaskExecutorRef preferredExecutor, bool owned) {
  void *allocation = _swift_task_alloc_specific(
      this, sizeof(class TaskExecutorPreferenceStatusRecord));
  auto record =
      ::new (allocation) TaskExecutorPreferenceStatusRecord(
          preferredExecutor, /*ownsExecutor=*/owned);
  SWIFT_TASK_DEBUG_LOG("[InitialTaskExecutorPreference] Create a task "
                       "preference record %p for task:%p",
                       record, this);

  addStatusRecord(this, record,
                  [&](ActiveTaskStatus oldStatus, ActiveTaskStatus &newStatus) {
                    // We use a flag to mark a task executor preference is
                    // present in order to avoid looking for task executor
                    // records when running the task, and we know there is no
                    // task executor preference present.
                    newStatus = newStatus.withTaskExecutorPreference();

                    return true;
                  });
}

// ONLY use this method while destroying task and removing the "initial"
// preference. In all other situations prefer a balanced "push / pop" pair of
// calls.
void AsyncTask::dropInitialTaskExecutorPreferenceRecord() {
  SWIFT_TASK_DEBUG_LOG("[InitialTaskExecutorPreference] Drop initial task "
                       "preference record from task:%p",
                       this);
  assert(hasInitialTaskExecutorPreferenceRecord());

  TaskExecutorPreferenceStatusRecord *record =
      popStatusRecordOfType<TaskExecutorPreferenceStatusRecord>(this);

  assert(record && "dropInitialTaskExecutorPreferenceRecord must only be "
                   "called when there is an executor preference record");

  if (record->hasRetainedExecutor()) {
    // Release the "initial" preferred task executor, because it was
    // specifically set in a Task initializer, which retained it.
    //
    // This should not be done for withTaskExecutorPreference executors,
    // however in that case, we would not enter this function here to clean up.
    //
    // NOTE: This MUST NOT assume that the object is a swift object (and use
    // swift_release), because a dispatch_queue_t conforms to TaskExecutor,
    // and may be passed in here; in which case swift_releasing it would be
    // incorrect.
    HeapObject *executorIdentityToRelease =
        record->getPreferredExecutor().getIdentity();
    SWIFT_TASK_DEBUG_LOG("Destroying executor %p", executorIdentityToRelease);
    swift_unknownObjectRelease(executorIdentityToRelease);
  }

  _swift_task_dealloc_specific(this, record);
}

/******************************************************************************/
/************************** TASK NAMING ***************************************/
/******************************************************************************/

void AsyncTask::pushInitialTaskName(const char* _taskName) {
  assert(_taskName && "Task name must not be null!");
  assert(hasInitialTaskNameRecord() && "Attempted pushing name but task has no initial task name flag!");

  void *allocation = _swift_task_alloc_specific(
      this, sizeof(class TaskNameStatusRecord));

  // TODO: Copy the string maybe into the same allocation at an offset or retain the swift string?
  auto taskNameLen = strlen(_taskName);
  char* taskNameCopy = reinterpret_cast<char*>(
      _swift_task_alloc_specific(this, taskNameLen + 1/*null terminator*/));
#if defined(_WIN32)
  static_cast<void>(strncpy_s(taskNameCopy, taskNameLen + 1,
                              _taskName, _TRUNCATE));
#else
  (void) strncpy(/*dst=*/taskNameCopy, /*src=*/_taskName, taskNameLen);
  taskNameCopy[taskNameLen] = '\0'; // make sure we null-terminate
#endif

  auto record =
      ::new (allocation) TaskNameStatusRecord(taskNameCopy);
  SWIFT_TASK_DEBUG_LOG("[TaskName] Create initial task name record %p "
                       "for task:%p, name:%s", record, this, taskNameCopy);

  addStatusRecord(this, record,
                  [&](ActiveTaskStatus oldStatus, ActiveTaskStatus &newStatus) {
                    return true; // always add the record
                  });
}

void AsyncTask::dropInitialTaskNameRecord() {
  if (!hasInitialTaskNameRecord()) {
    return;
  }

  SWIFT_TASK_DEBUG_LOG("[TaskName] Drop initial task name record for task:%p", this);
  TaskNameStatusRecord *record =
      popStatusRecordOfType<TaskNameStatusRecord>(this);
  assert(record &&
         "hasInitialTaskNameRecord is true but we did not find a name record");
  // Since we first allocated the record, and then the string copy, deallocate
  // in LIFO order.
  char *name = const_cast<char *>(record->getName());
  _swift_task_dealloc_specific(this, name);
  _swift_task_dealloc_specific(this, record);
}

const char*
AsyncTask::getTaskName() {
  // We first check the executor preference status flag, in order to avoid
  // having to scan through the records of the task checking if there was
  // such record.
  //
  // This is an optimization in order to make the enqueue/run
  // path of a task avoid excessive work if a task had many records.
  if (!hasInitialTaskNameRecord()) {
    return nullptr;
  }

  const char *data = nullptr;
  withStatusRecordLock(this, [&](ActiveTaskStatus status) {
    for (auto record : status.records()) {
      if (record->getKind() == TaskStatusRecordKind::TaskName) {
        auto nameRecord = cast<TaskNameStatusRecord>(record);
        data = nameRecord->getName();
        return;
      }
    }
  });

  return data;
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

  withStatusRecordLock(parent, [&](ActiveTaskStatus parentStatus) {
    group->addChildTask(child);

    // After getting parent's status record lock, do some soundness checks to
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

  withStatusRecordLock(parent, [&](ActiveTaskStatus unused) {
    group->removeChildTask(child);
  });
}

/// Cancel the task group and all the child tasks that belong to `group`.
///
/// The caller must guarantee that this is called while holding the owning
/// task's status record lock.
void swift::_swift_taskGroup_cancel(TaskGroup *group) {
  (void) group->statusCancel();

  // Because only the owning task of the task group can modify the
  // child list of a task group status record, and it can only do so
  // while holding the owning task's status record lock, we do not need
  // any additional synchronization within this function.
  for (auto childTask : group->getTaskRecord()->children())
    swift_task_cancel(childTask);
}

/// Cancel the task group and all the child tasks that belong to `group`.
///
/// The caller must guarantee that this is called from the owning task.
void swift::_swift_taskGroup_cancel_unlocked(TaskGroup *group,
                                                        AsyncTask *owningTask) {
  // Early out. If there are no children, there's nothing to do. We can safely
  // check this without locking, since this can only be concurrently mutated
  // from a child task. If there are no children then no more can be added.
  if (!group->getTaskRecord()->getFirstChild())
    return;

  withStatusRecordLock(owningTask, [&group](ActiveTaskStatus status) {
    _swift_taskGroup_cancel(group);
  });
}

/**************************************************************************/
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
    _swift_taskGroup_cancel(groupRecord->getGroup());
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

  // No cancellation action needs to be taken for dependency status records
  case TaskStatusRecordKind::TaskDependency:
    break;

  // Cancellation has no impact on executor preference.
  case TaskStatusRecordKind::TaskExecutorPreference:
    break;

  // Cancellation has no impact on task names.
  case TaskStatusRecordKind::TaskName:
    break;

  // This should never be found, but the compiler complains if we don't check.
  case TaskStatusRecordKind::First_Reserved:
    break;
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

    // consume here pairs with the release in addStatusRecord.
    if (task->_private()._status().compare_exchange_weak(oldStatus, newStatus,
            /*success*/ SWIFT_MEMORY_ORDER_CONSUME,
            /*failure*/ std::memory_order_relaxed)) {
      _swift_tsan_consume(task);
      break;
    }
  }

  newStatus.traceStatusChanged(task, false);
  if (newStatus.getInnermostRecord() == nullptr) {
     // No records, nothing to propagate
     return;
  }

  withStatusRecordLock(task, newStatus, [&](ActiveTaskStatus status) {
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
                                    JobPriority oldPriority,
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

  // Escalation notifications need to be called.
  case TaskStatusRecordKind::EscalationNotification:  {
    auto notification =
      cast<EscalationNotificationStatusRecord>(record);
    SWIFT_TASK_DEBUG_LOG("[Dependency] Trigger task escalation handler record %p, escalate from %#x to %#x",
                         record, oldPriority, newPriority);
    notification->run(oldPriority, newPriority);
    return;
  }

  case TaskStatusRecordKind::TaskDependency: {
    auto dependencyRecord = cast<TaskDependencyStatusRecord>(record);
    SWIFT_TASK_DEBUG_LOG("[Dependency] Escalating a task dependency record %p from %#x to %#x",
                    record, oldPriority, newPriority);
    dependencyRecord->performEscalationAction(oldPriority, newPriority);
    return;
  }

  // Cancellation notifications can be ignore.
  case TaskStatusRecordKind::CancellationNotification:
    return;
  /// Executor preference we can ignore.
  case TaskStatusRecordKind::TaskExecutorPreference:
    return;
  /// Task names don't matter to priority escalation.
  case TaskStatusRecordKind::TaskName:
    return;
  // This should never be found, but the compiler complains if we don't check.
  case TaskStatusRecordKind::First_Reserved:
    break;
  }

  // Other cases can fall through here and be ignored.
  // FIXME: allow dynamic extension/correction?
}

SWIFT_CC(swift)
JobPriority
static swift_task_escalateImpl(AsyncTask *task, JobPriority newPriority) {
  SWIFT_TASK_DEBUG_LOG("Escalating %p to %#zx priority", task, newPriority);
  auto oldStatus = task->_private()._status().load(std::memory_order_relaxed);
  auto oldPriority = oldStatus.getStoredPriority();
  auto newStatus = oldStatus;

  while (true) {
    // Ensure oldPriority is up to date if we retry the compare_exchange.
    oldPriority = oldStatus.getStoredPriority();

    // Fast path: check that the stored priority is already at least
    // as high as the desired priority.
    if (oldPriority >= newPriority) {
      SWIFT_TASK_DEBUG_LOG("Task is already at %#zx priority", oldPriority);
      return oldPriority;
    }

    if (oldStatus.isRunning() || oldStatus.isEnqueued()) {
      // Regardless of whether status record is locked or not, update the
      // priority and RO bit on the task status
      newStatus = oldStatus.withEscalatedPriority(newPriority);
    } else if (oldStatus.isComplete()) {
      // We raced with concurrent completion, nothing to escalate
      SWIFT_TASK_DEBUG_LOG("Escalated a task %p which had completed, do nothing", task);
      return oldStatus.getStoredPriority();
    } else {
      // Task is suspended.
      newStatus = oldStatus.withNewPriority(newPriority);
    }

    // consume here pairs with the release in addStatusRecord.
    if (task->_private()._status().compare_exchange_weak(oldStatus, newStatus,
            /* success */ SWIFT_MEMORY_ORDER_CONSUME,
            /* failure */ std::memory_order_relaxed)) {
      _swift_tsan_consume(task);
      break;
    }
  }

  if (newStatus.isRunning()) {
#if SWIFT_CONCURRENCY_ENABLE_PRIORITY_ESCALATION
    // The task is running, escalate the thread that is running it.
    ActiveTaskStatus *taskStatus;
    dispatch_lock_t *executionLock;

    taskStatus = (ActiveTaskStatus *) &task->_private()._status();
    executionLock = (dispatch_lock_t *) ((char*)taskStatus + ActiveTaskStatus::executionLockOffset());

    SWIFT_TASK_DEBUG_LOG("[Override] Escalating %p which is running on %#x from %#x to %#x",
                         task, newStatus.currentExecutionLockOwner(),
                         oldPriority, newPriority);
    swift_dispatch_lock_override_start_with_debounce(
        executionLock, newStatus.currentExecutionLockOwner(), (qos_class_t) newPriority);
#endif
  } else if (newStatus.isEnqueued()) {
    //  Task is not running, it's enqueued somewhere waiting to be run
    //
    // TODO (rokhinip): Add a stealer to escalate the thread request for
    //  the task. Still mark the task has having been escalated so that the
    //  thread will self override when it starts draining the task
    //
    // TODO (rokhinip): Add a signpost to flag that this is a potential
    //  priority inversion
    SWIFT_TASK_DEBUG_LOG("[Override] Escalating %p which is enqueued", task);

  }

  if (newStatus.getInnermostRecord() == NULL) {
    return newStatus.getStoredPriority();
  }

  SWIFT_TASK_DEBUG_LOG("[Override] Escalating %p which is suspended from %#x to %#x",
                       task, oldPriority, newPriority);
  // We must have at least one record - the task dependency one.
  assert(newStatus.getInnermostRecord() != NULL);

  withStatusRecordLock(task, newStatus, [&](ActiveTaskStatus status) {
    // We know that none of the escalation actions will recursively
    // modify the task status record list by adding or removing task records
    for (auto cur: status.records()) {
      performEscalationAction(cur, oldPriority, newPriority);
    }
  });

  return newStatus.getStoredPriority();
}

void TaskDependencyStatusRecord::performEscalationAction(
    JobPriority oldPriority, JobPriority newPriority) {
  switch (this->DependencyKind) {
    case WaitingOnTask:
      SWIFT_TASK_DEBUG_LOG("[Dependency] Escalate dependent task %p noted in %p record",
        this->DependentOn.Task, this);
      swift_task_escalate(this->DependentOn.Task, newPriority);
      break;
    case WaitingOnContinuation:
      // We can't do anything meaningful to escalate this since we don't know
      // who will resume the continuation
      SWIFT_TASK_DEBUG_LOG("[Dependency] Escalate dependent continuation %p noted in %p record -- do nothing",
        this->DependentOn.Continuation, this);
      break;
    case WaitingOnTaskGroup:
      // If a task is being escalated while waiting on a task group, the task
      // should also have a TaskGroupTaskStatusRecord and the escalation
      // action on that record should do the needful to propagate the
      // escalation to the child tasks. We can short-circuit here.
      SWIFT_TASK_DEBUG_LOG("[Dependency] Escalate dependent taskgroup %p noted in %p record -- do nothing",
        this->DependentOn.TaskGroup, this);
      break;
    case EnqueuedOnExecutor:
      SWIFT_TASK_DEBUG_LOG("[Dependency] Escalate dependent executor %p noted in %p record from %#x to %#x",
        this->DependentOn.Executor, this, oldPriority, newPriority);
      swift_executor_escalate(this->DependentOn.Executor, this->WaitingTask, newPriority);
      break;
  }
}

#define OVERRIDE_TASK_STATUS COMPATIBILITY_OVERRIDE
#include "../CompatibilityOverride/CompatibilityOverrideIncludePath.h"
