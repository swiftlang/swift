//===--- TaskGroup.cpp - Task Groups --------------------------------------===//
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
// Object management for child tasks that are children of a task group.
//
//===----------------------------------------------------------------------===//

#include "swift/Runtime/Concurrency.h"
#include "swift/ABI/Task.h"
#include "swift/ABI/TaskGroup.h"
#include "swift/ABI/Metadata.h"
#include "swift/Runtime/Mutex.h"
#include "swift/Runtime/HeapObject.h"
#include "TaskPrivate.h"
#include "TaskPrivate.h"
#include "AsyncCall.h"
#include "Debug.h"

#include <dispatch/dispatch.h>

#if !defined(_WIN32)
#include <dlfcn.h>
#endif

using namespace swift;
using TaskGroup = swift::TaskGroup;
using FutureFragment = AsyncTask::FutureFragment;

using ReadyQueueItem = TaskGroup::ReadyQueueItem;
using ReadyStatus = TaskGroup::ReadyStatus;
using PollResult = TaskGroup::PollResult;

// =============================================================================
// ==== create -----------------------------------------------------------------

TaskGroup* swift::swift_task_group_create(AsyncTask *task) {
  void *allocation = swift_task_alloc(task, sizeof(TaskGroup));

  // nasty trick, but we want to keep the record inside the group as we'll need
  // to remove it from the task as the group is destroyed, as well as interact
  // with it every time we add child tasks; so it is useful to pre-create it here
  // and store it in the group.
  //
  // The record won't be used by anyone until we're done constructing and setting
  // up the group anyway.
  void *recordAllocation = swift_task_alloc(task, sizeof(TaskGroupTaskStatusRecord));
  auto record = new (recordAllocation)
      TaskGroupTaskStatusRecord(reinterpret_cast<TaskGroup*>(allocation));

  TaskGroup *group = new (allocation) TaskGroup(record);
  fprintf(stderr, "[%s:%d] (%s): create group; task: %d, group:%d\n", __FILE__, __LINE__, __FUNCTION__, task, group);

  // ok, now that the group actually is initialized: attach it to the task
  swift_task_addStatusRecord(task, record);
  fprintf(stderr, "[%s:%d] (%s): attach GROUP, task record in parent [%d]; record:%d\n", __FILE__, __LINE__, __FUNCTION__, task, record);

  return group;
}

// =============================================================================
// ==== add / attachChild -----------------------------------------------------------------

void swift::swift_task_group_attachChild(TaskGroup *group,
                                         AsyncTask *parent, AsyncTask *child) {
  auto groupRecord = group->getTaskRecord();
  assert(groupRecord->getGroup() == group);
  fprintf(stderr, "[%s:%d] (%s): attach GROUP CHILD, task:%d, group record:%d, child:%d\n", __FILE__, __LINE__, __FUNCTION__,
          parent, groupRecord, child);
  return groupRecord->attachChild(child);
}

// =============================================================================
// ==== destroy ----------------------------------------------------------------

void swift::swift_task_group_destroy(AsyncTask *task, TaskGroup *group) {
  group->destroy(task);
}

void TaskGroup::destroy(AsyncTask *task) {
  // First, remove the group from the task and deallocate the record
  fprintf(stderr, "[%s:%d] (%s): detach GROUP, task:%d, group record:%d, group:%d\n", __FILE__, __LINE__, __FUNCTION__, task, Record, this);
  swift_task_removeStatusRecord(task, Record);
  swift_task_dealloc(task, Record);

  // TODO: need to release all waiters as well
//  auto waitHead = waitQueue.load(std::memory_order_acquire);
//  switch (waitHead.getStatus()) {
//    case TaskGroup::WaitStatus::Waiting:
//      assert(false && "destroying a task group that still has waiting tasks");
//  }

  mutex.lock(); // TODO: remove fragment lock, and use status for synchronization
  // Release all ready tasks which are kept retained, the group destroyed,
  // so no other task will ever await on them anymore;
  ReadyQueueItem item;
  bool taskDequeued = readyQueue.dequeue(item);
  while (taskDequeued) {
    swift_release(item.getTask());
    bool taskDequeued = readyQueue.dequeue(item);
  }
  mutex.unlock(); // TODO: remove fragment lock, and use status for synchronization

  // TODO: get the parent task, do we need to store it?
  swift_task_dealloc(task, this);
}

// =============================================================================
// ==== offer ------------------------------------------------------------------

void TaskGroup::offer(AsyncTask *completedTask, AsyncContext *context,
                      ExecutorRef completingExecutor) {
  assert(completedTask);
  assert(completedTask->isFuture());
  assert(completedTask->hasChildFragment());
  assert(completedTask->hasGroupChildFragment());
  assert(completedTask->groupChildFragment()->getGroup() == this);

  fprintf(stderr, "[%s:%d] (%s): offer %d\n", __FILE__, __LINE__, __FUNCTION__, completedTask);

  mutex.lock(); // TODO: remove fragment lock, and use status for synchronization

  // Immediately increment ready count and acquire the status
  // Examples:
  //   W:n R:0 P:3 -> W:n R:1 P:3 // no waiter, 2 more pending tasks
  //   W:n R:0 P:1 -> W:n R:1 P:1 // no waiter, no more pending tasks
  //   W:n R:0 P:1 -> W:y R:1 P:1 // complete immediately
  //   W:n R:0 P:1 -> W:y R:1 P:3 // complete immediately, 2 more pending tasks
  auto assumed = statusAddReadyAssumeAcquire();

  fprintf(stderr, "[%s:%d] (%s): offer %s\n", __FILE__, __LINE__, __FUNCTION__, assumed.to_string().c_str());

  // If an error was thrown, save it in the future fragment.
  auto futureContext = static_cast<FutureAsyncContext *>(context);
  bool hadErrorResult = false;
  if (auto errorObject = *futureContext->errorResult) {
    // instead we need to enqueue this result:
    hadErrorResult = true;
  }

  // ==== a) has waiting task, so let us complete it right away
  if (assumed.hasWaitingTask()) {
    auto waitingTask = waitQueue.load(std::memory_order_acquire);
    fprintf(stderr, "[%s:%d] (%s): has waiter! waiter:%d %s\n", __FILE__, __LINE__, __FUNCTION__, waitingTask, assumed.to_string().c_str());
    while (true) {
      fprintf(stderr, "[%s:%d] (%s): run waiting task directly!!!!!\n", __FILE__, __LINE__, __FUNCTION__);
      // ==== a) run waiting task directly -------------------------------------
      assert(assumed.hasWaitingTask());
      assert(assumed.pendingTasks() && "offered to group with no pending tasks!");
      // We are the "first" completed task to arrive,
      // and since there is a task waiting we immediately claim and complete it.
      if (waitQueue.compare_exchange_weak(
          waitingTask, nullptr,
          /*success*/ std::memory_order_release,
          /*failure*/ std::memory_order_acquire) &&
          statusCompletePendingReadyWaiting(assumed)) {
        fprintf(stderr, "[%s:%d] (%s): offer, claimed task!\n", __FILE__, __LINE__, __FUNCTION__);
        fprintf(stderr, "[%s:%d] (%s):        status now! assumed: %s\n", __FILE__, __LINE__, __FUNCTION__, assumed.to_string().c_str());
        fprintf(stderr, "[%s:%d] (%s):        status now!    load: %s\n", __FILE__, __LINE__, __FUNCTION__, statusLoadRelaxed().to_string().c_str());
        // Run the task.
        auto result = PollResult::get(
            completedTask, hadErrorResult, /*needsRelease*/ false);

        mutex.unlock(); // TODO: remove fragment lock, and use status for synchronization
        fprintf(stderr, "[%s:%d] (%s):        RUN  waiting:%d with result completed:%d\n", __FILE__, __LINE__, __FUNCTION__, waitingTask, completedTask);
        swift::runTaskWithPollResult(waitingTask, completingExecutor, result);
        return;
      } // else, try again

      assert(false && "why should this have to try again ever?"); // FIXME
    }
  }

  // ==== b) enqueue completion ------------------------------------------------
  //
  // else, no-one was waiting (yet), so we have to instead enqueue to the message
  // queue when a task polls during next() it will notice that we have a value
  // ready for it, and will process it immediately without suspending.
  assert(!waitQueue.load(std::memory_order_relaxed));

  // Retain the task while it is in the queue;
  // it must remain alive until the task group is alive.
  swift_retain(completedTask);
  auto readyItem = ReadyQueueItem::get(
      hadErrorResult ? ReadyStatus::Error : ReadyStatus::Success,
      completedTask
  );

  assert(completedTask == readyItem.getTask());
  assert(readyItem.getTask()->isFuture());
  fprintf(stderr, "[%s:%d] (%s): enqueue in ready queue %d\n", __FILE__, __LINE__, __FUNCTION__, completedTask);
  readyQueue.enqueue(readyItem);
  mutex.unlock(); // TODO: remove fragment lock, and use status for synchronization
  return;
}

// =============================================================================
// ==== group.next() implementation (wait_next and groupPoll) ------------------

SWIFT_CC(swiftasync)
void swift::swift_task_group_wait_next(
    AsyncTask *waitingTask,
    ExecutorRef executor,
    SWIFT_ASYNC_CONTEXT AsyncContext *rawContext) {
  waitingTask->ResumeTask = rawContext->ResumeParent;
  waitingTask->ResumeContext = rawContext;

  auto context = static_cast<TaskGroupNextWaitAsyncContext *>(rawContext);
  fprintf(stderr, "[%s:%d] (%s): context %d\n", __FILE__, __LINE__, __FUNCTION__, context);
  auto task = context->task;
  auto group = context->group;
  fprintf(stderr, "[%s:%d] (%s): task %d\n", __FILE__, __LINE__, __FUNCTION__, task);
  fprintf(stderr, "[%s:%d] (%s): waitingTask %d\n", __FILE__, __LINE__, __FUNCTION__, waitingTask);
  fprintf(stderr, "[%s:%d] (%s): group %d\n", __FILE__, __LINE__, __FUNCTION__, group);
  TaskGroup::PollResult polled = group->poll(waitingTask);
  fprintf(stderr, "[%s:%d] (%s): group polled: %d\n", __FILE__, __LINE__, __FUNCTION__, polled.status);

  if (polled.status == TaskGroup::GroupPollStatus::Waiting) {
    fprintf(stderr, "[%s:%d] (%s): group polled: WAITING\n", __FILE__, __LINE__, __FUNCTION__);
    // The waiting task has been queued on the channel,
    // there were pending tasks so it will be woken up eventually.
    return;
  }

  if (polled.status == TaskGroup::GroupPollStatus::Empty)
    fprintf(stderr, "[%s:%d] (%s): group polled: RUN EMPTY\n", __FILE__, __LINE__, __FUNCTION__);
  if (polled.status == TaskGroup::GroupPollStatus::Success)
    fprintf(stderr, "[%s:%d] (%s): group polled: RUN SUCCESS\n", __FILE__, __LINE__, __FUNCTION__);
  if (polled.status == TaskGroup::GroupPollStatus::Error)
    fprintf(stderr, "[%s:%d] (%s): group polled: RUN ERROR\n", __FILE__, __LINE__, __FUNCTION__);

  runTaskWithPollResult(waitingTask, executor, polled);
}

TaskGroup::PollResult TaskGroup::poll(AsyncTask *waitingTask) {
  mutex.lock(); // TODO: remove group lock, and use status for synchronization
  auto assumed = statusMarkWaitingAssumeAcquire();

  PollResult result;
  result.storage = nullptr;
  result.retainedTask = nullptr;

  fprintf(stderr, "[%s:%d] (%s): status %s\n", __FILE__, __LINE__, __FUNCTION__, assumed.to_string().c_str());

  // ==== 1) bail out early if no tasks are pending ----------------------------
  if (assumed.isEmpty()) {
    // No tasks in flight, we know no tasks were submitted before this poll
    // was issued, and if we parked here we'd potentially never be woken up.
    // Bail out and return `nil` from `group.next()`.
    fprintf(stderr, "[%s:%d] (%s): empty! assumed: %s\n", __FILE__, __LINE__, __FUNCTION__, assumed.to_string().c_str());
    statusRemoveWaiting();
    fprintf(stderr, "[%s:%d] (%s):      debug now: %s\n", __FILE__, __LINE__, __FUNCTION__, statusLoadRelaxed().to_string().c_str());
    result.status = TaskGroup::GroupPollStatus::Empty;
    mutex.unlock(); // TODO: remove group lock, and use status for synchronization
    return result;
  }

  auto waitHead = waitQueue.load(std::memory_order_acquire);

  // ==== 2) Ready task was polled, return with it immediately -----------------
  if (assumed.readyTasks()) {
    fprintf(stderr, "[%s:%d] (%s): there are ready tasks... %s\n", __FILE__, __LINE__, __FUNCTION__, statusLoadRelaxed().to_string().c_str());

    auto assumedStatus = assumed.status;
    auto newStatus = TaskGroup::GroupStatus{assumedStatus};
    if (status.compare_exchange_weak(
        assumedStatus, newStatus.completingPendingReadyWaiting().status,
        /*success*/ std::memory_order_relaxed,
        /*failure*/ std::memory_order_acquire)) {

      // Success! We are allowed to poll.
      ReadyQueueItem item;
      bool taskDequeued = readyQueue.dequeue(item);
      if (!taskDequeued) {
        result.status = TaskGroup::GroupPollStatus::Waiting;
        result.storage = nullptr;
        result.retainedTask = nullptr;
        mutex.unlock(); // TODO: remove group lock, and use status for synchronization
        return result;
      }

      assert(item.getTask()->isFuture());
      auto futureFragment = item.getTask()->futureFragment();

      // Store the task in the result, so after we're done processing it it may
      // be swift_release'd; we kept it alive while it was in the readyQueue by
      // an additional retain issued as we enqueued it there.
      result.retainedTask = item.getTask();
      fprintf(stderr, "[%s:%d] (%s): about to return: %d\n", __FILE__, __LINE__, __FUNCTION__, result.retainedTask);

      switch (item.getStatus()) {
        case ReadyStatus::Success:
          // Immediately return the polled value
          result.status = TaskGroup::GroupPollStatus::Success;
          result.storage = futureFragment->getStoragePtr();
          assert(result.retainedTask && "polled a task, it must be not null");
          mutex.unlock(); // TODO: remove fragment lock, and use status for synchronization
          return result;

        case ReadyStatus::Error:
          // Immediately return the polled value
          result.status = TaskGroup::GroupPollStatus::Error;
          result.storage =
              reinterpret_cast<OpaqueValue *>(futureFragment->getError());
          assert(result.retainedTask && "polled a task, it must be not null");
          mutex.unlock(); // TODO: remove fragment lock, and use status for synchronization
          return result;

        case ReadyStatus::Empty:
          result.status = TaskGroup::GroupPollStatus::Empty;
          result.storage = nullptr;
          result.retainedTask = nullptr;
          mutex.unlock(); // TODO: remove fragment lock, and use status for synchronization
          return result;
      }
      assert(false && "must return result when status compare-and-swap was successful");
    } // else, we failed status-cas (some other waiter claimed a ready pending task, try again)
  }

  // ==== 3) Add to wait queue -------------------------------------------------
  assert(assumed.readyTasks() == 0);
  while (true) {
    // Put the waiting task at the beginning of the wait queue.
    if (waitQueue.compare_exchange_weak(
        waitHead, waitingTask,
        /*success*/ std::memory_order_release,
        /*failure*/ std::memory_order_acquire)) {
      fprintf(stderr, "[%s:%d] (%s): added to wait queue\n", __FILE__, __LINE__, __FUNCTION__);
      mutex.unlock(); // TODO: remove fragment lock, and use status for synchronization
      // no ready tasks, so we must wait.
      result.status = TaskGroup::GroupPollStatus::Waiting;
      return result;
    } // else, try again
  }
  assert(false && "must successfully compare exchange the waiting task.");
}

// =============================================================================
// ==== isEmpty ----------------------------------------------------------------

bool swift::swift_task_group_is_empty(TaskGroup *group) {
  return group->isEmpty();
}

// =============================================================================
// ==== isCancelled ------------------------------------------------------------

bool swift::swift_task_group_is_cancelled(AsyncTask *task, TaskGroup *group) {
  return group->isCancelled();
}

// =============================================================================
// ==== cancelAll --------------------------------------------------------------

void swift::swift_task_group_cancel_all(AsyncTask *task, TaskGroup *group) {
  group->cancelAll(task);
}

bool TaskGroup::cancelAll(AsyncTask *task) {
  fprintf(stderr, "[%s:%d] (%s): cancel all child tasks %d\n", __FILE__, __LINE__, __FUNCTION__, task);

  // store the cancelled bit
  auto old = statusCancel();
  if (old.isCancelled()) {
    fprintf(stderr, "[%s:%d] (%s): already was cancelled, group:%d\n", __FILE__, __LINE__, __FUNCTION__, this);
    // already was cancelled previously, nothing to do?
    return false;
  }

//  // first time this group is being called cancelAll on, so we must cancel all tasks
//  if (this->isEmpty())
//    return true;

  fprintf(stderr, "[%s:%d] (%s): calling swift_task_cancel_group_child_tasks, group:%d\n", __FILE__, __LINE__, __FUNCTION__, this);
  // cancel all existing tasks within the group
  swift_task_cancel_group_child_tasks(task, this);
  return true;
}


// =============================================================================
// ==== internal ---------------------------------------------------------------

bool swift::swift_task_group_add_pending(TaskGroup *group) {
  return !group->statusAddPendingTaskRelaxed().isCancelled();
}
