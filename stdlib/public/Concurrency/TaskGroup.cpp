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
using GroupPollResult = TaskGroup::GroupPollResult;

// =============================================================================
// ==== create -----------------------------------------------------------------

TaskGroup* swift::swift_task_group_create(AsyncTask *task) {
  fprintf(stderr, "[%s:%d] (%s): task %d\n", __FILE__, __LINE__, __FUNCTION__, task);
  void *allocation = swift_task_alloc(task, sizeof(TaskGroup));
  TaskGroup *group = new (allocation) TaskGroup();
  fprintf(stderr, "[%s:%d] (%s): group %d\n", __FILE__, __LINE__, __FUNCTION__, group);
  return group;
}

// =============================================================================
// ==== destroy ----------------------------------------------------------------

void swift::swift_task_group_destroy(AsyncTask *task, TaskGroup *group) {
  group->destroy(task);
}

void TaskGroup::destroy(AsyncTask *task) {
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
                      ExecutorRef executor) {
  assert(completedTask);
  assert(completedTask->isFuture());
  assert(completedTask->hasChildFragment());
  assert(completedTask->hasGroupChildFragment());
  assert(completedTask->groupChildFragment()->getGroup() == this);

  mutex.lock(); // TODO: remove fragment lock, and use status for synchronization

  // Immediately increment ready count and acquire the status
  // Examples:
  //   R:0 P:1 W:0 -> R:1 P:1 W:0 //
  //   R:0 P:1 W:1 -> R:1 P:1 W:1 // complete immediately
  auto assumed = statusAddReadyTaskAcquire();

  // If an error was thrown, save it in the future fragment.
  auto futureContext = static_cast<FutureAsyncContext *>(context);
  bool hadErrorResult = false;
  if (auto errorObject = *futureContext->errorResult) {
    // instead we need to enqueue this result:
    hadErrorResult = true;
  }

  if (assumed.waitingTasks() == 0) {
    // ==== a) enqueue message -----------------------------------------------
    //
    // no-one was waiting (yet), so we have to instead enqueue to the message queue
    // when a task polls during next() it will notice that we have a value ready
    // for it, and will process it immediately without suspending.

    // Retain the task while it is in the queue;
    // it must remain alive until the task group is alive.
    swift_retain(completedTask);
    auto readyItem = ReadyQueueItem::get(
        hadErrorResult ? ReadyStatus::Error : ReadyStatus::Success,
        completedTask
    );

    assert(completedTask == readyItem.getTask());
    assert(readyItem.getTask()->isFuture());
    readyQueue.enqueue(readyItem);
    mutex.unlock(); // TODO: remove fragment lock, and use status for synchronization
    return;
  }

  while (true) {
    // Loop until we either:
    // a) no waiters available, and we enqueued the completed task to readyQueue
    // b) successfully claim a waiter to complete with this task
    assert(assumed.pendingTasks() && "offered to group with no pending tasks!");
    if (statusCompleteReadyPendingWaitingTasks(assumed)) {
        // ==== b) run waiter --------------------------------------------------
        // We are the "first" completed task to arrive, since old status had zero
        //
        // If old status had no tasks, it means we are the first to arrive,
        // and as such may directly get and signal the first waiting task.
        // We only signal *one* waiter and relink the waiter queue.
        auto waitHead = waitQueue.load(std::memory_order_acquire);
        while (auto waitingTask = waitHead.getTask()) {
          // Find the next waiting task.
          auto nextWaitingTask = waitingTask->getNextWaitingTask();
          auto nextWaitQueueItem = TaskGroup::WaitQueueItem::get(
              TaskGroup::WaitStatus::Waiting,
              nextWaitingTask
          );

          // Attempt to claim it, we are the future that is going to complete it.
          // TODO: there may be other futures trying to do the same right now? FIXME: not really because the status right?
          if (waitQueue.compare_exchange_weak(
              waitHead, nextWaitQueueItem,
              /*success*/ std::memory_order_release,
              /*failure*/ std::memory_order_acquire)) {
            // Run the task.
            auto result = GroupPollResult::get(
                completedTask, hadErrorResult, /*needsRelease*/ false);

            mutex.unlock(); // TODO: remove fragment lock, and use status for synchronization
            swift::runTaskWithGroupPollResult(waitingTask, executor, result);
            return;
          } else {
            waitingTask = waitHead.getTask();
          }
          // DO NOT move to the next task, one element is only signalled *once*.
          // E.g. if we somehow had two next() registered, each should get
          // individual elements, not the same element after all (!).
          //       Move to the next task.
          //      waitingTask = nextWaitingTask;

        }
    } // else, status-cas failed and we need to try again
  }

  llvm_unreachable("offer must successfully complete it's cas-loop enqueue!");
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
//  fprintf(stderr, "[%s:%d] (%s): context %d\n", __FILE__, __LINE__, __FUNCTION__, context);
  auto task = context->task;
  auto group = context->group;
//  fprintf(stderr, "[%s:%d] (%s): task %d\n", __FILE__, __LINE__, __FUNCTION__, task);
//  fprintf(stderr, "[%s:%d] (%s): waitingTask %d\n", __FILE__, __LINE__, __FUNCTION__, waitingTask);
//  fprintf(stderr, "[%s:%d] (%s): group %d\n", __FILE__, __LINE__, __FUNCTION__, group);
  GroupPollResult polled = group->poll(waitingTask);

  if (polled.status == TaskGroup::GroupPollStatus::Waiting) {
    // The waiting task has been queued on the channel,
    // there were pending tasks so it will be woken up eventually.
    return;
  }

  runTaskWithGroupPollResult(waitingTask, executor, polled);
}

TaskGroup::GroupPollResult TaskGroup::poll(AsyncTask *waitingTask) {
  auto fragment = this;
  fragment->mutex.lock(); // TODO: remove fragment lock, and use status for synchronization

  // immediately update the status counter
  auto assumed = fragment->statusAddWaitingTaskAcquire();

  GroupPollResult result;
  result.storage = nullptr;
  result.retainedTask = nullptr;

  // ==== 1) bail out early if no tasks are pending ----------------------------
  if (assumed.isEmpty()) {
    // 1) No tasks in flight, we know no tasks were submitted before this poll
    //    was issued, and if we parked here we'd potentially never be woken up.
    //    Bail out and return `nil` from `group.next()`.
    fragment->statusRemoveWaitingTask(); // "revert" our eager +1 we just did

    result.status = TaskGroup::GroupPollStatus::Empty;
    fragment->mutex.unlock(); // TODO: remove fragment lock, and use status for synchronization
    return result;
  }

  // ==== Add to wait queue ----------------------------------------------------
  while (true) {
    // Put the waiting task at the beginning of the wait queue.
    auto waitHead = fragment->waitQueue.load(std::memory_order_acquire);
    waitingTask->getNextWaitingTask() = waitHead.getTask();
    auto newWaitHead = TaskGroup::WaitQueueItem::get(
        TaskGroup::WaitStatus::Waiting, waitingTask);

    if (fragment->waitQueue.compare_exchange_weak(
        waitHead, newWaitHead,
        /*success*/ std::memory_order_release,
        /*failure*/ std::memory_order_acquire)) {
      // While usually the waiting task will be the group task,
      // we can attempt to escalate group task here. // TODO: does this make sense?
      // Note that we cannot escalate the specific future (child) task we'd
      // like to complete, since we don't know which one that might be.
      // swift_task_escalate(this, waitingTask->Flags.getPriority()); // FIXME!!!!!!!!!!
      result.status = TaskGroup::GroupPollStatus::Waiting;
      // return result;
      break;
    } // else, try again
  }

  // ==== 3) Ready task was polled, return with it immediately -----------------
  auto assumedStatus = assumed.status;
  while (assumed.readyTasks()) {
    auto newStatus = TaskGroup::GroupStatus{assumedStatus};
    if (fragment->status.compare_exchange_weak(
        assumedStatus, newStatus.completingReadyPendingWaitingTask().status,
        /*success*/ std::memory_order_relaxed,
        /*failure*/ std::memory_order_acquire)) {

      // Success! We are allowed to poll.
      ReadyQueueItem item;
      bool taskDequeued = fragment->readyQueue.dequeue(item);
      if (!taskDequeued) {
        result.status = TaskGroup::GroupPollStatus::Waiting;
        result.storage = nullptr;
        result.retainedTask = nullptr;
        fragment->mutex.unlock(); // TODO: remove fragment lock, and use status for synchronization
        return result;
      }

      assert(item.getTask()->isFuture());
      auto futureFragment = item.getTask()->futureFragment();

      // Store the task in the result, so after we're done processing it it may
      // be swift_release'd; we kept it alive while it was in the readyQueue by
      // an additional retain issued as we enqueued it there.
      result.retainedTask = item.getTask();

      switch (item.getStatus()) {
        case ReadyStatus::Success:
          // Immediately return the polled value
          result.status = TaskGroup::GroupPollStatus::Success;
          result.storage = futureFragment->getStoragePtr();
          assert(result.retainedTask && "polled a task, it must be not null");
          fragment->mutex.unlock(); // TODO: remove fragment lock, and use status for synchronization
          return result;

        case ReadyStatus::Error:
          // Immediately return the polled value
          result.status = TaskGroup::GroupPollStatus::Error;
          result.storage =
              reinterpret_cast<OpaqueValue *>(futureFragment->getError());
          assert(result.retainedTask && "polled a task, it must be not null");
          fragment->mutex.unlock(); // TODO: remove fragment lock, and use status for synchronization
          return result;

        case ReadyStatus::Empty:
          result.status = TaskGroup::GroupPollStatus::Empty;
          result.storage = nullptr;
          result.retainedTask = nullptr;
          fragment->mutex.unlock(); // TODO: remove fragment lock, and use status for synchronization
          return result;
      }
      assert(false && "must return result when status compare-and-swap was successful");
    } // else, we failed status-cas (some other waiter claimed a ready pending task, try again)
  } // no more ready tasks

  // no ready tasks, so we must wait.
  result.status = TaskGroup::GroupPollStatus::Waiting;
  fragment->mutex.unlock(); // TODO: remove fragment lock, and use status for synchronization
  return result;
}

// =============================================================================
// ==== isEmpty ----------------------------------------------------------------

bool swift::swift_task_group_is_empty(TaskGroup *group) {
  return group->isEmpty();
}

// =============================================================================
// ==== isEmpty ----------------------------------------------------------------

bool swift::swift_task_group_is_cancelled(TaskGroup *group) {
  return group->isCancelled();
}

// =============================================================================
// ==== cancelAll --------------------------------------------------------------

void swift::swift_task_group_cancel_all(AsyncTask *task, TaskGroup *group) {
  group->cancelAll(task);
}

bool TaskGroup::cancelAll(AsyncTask *task) {
  // store the cancelled bit
  auto old = status.fetch_or(GroupStatus::cancelled, std::memory_order_acquire);
  auto o = GroupStatus { old };

  if (o.isCancelled()) {
    // already was cancelled previously, nothing to do?
    return false;
  }

  // first time this group is being called cancelAll on, so we must cancel all tasks
  // TODO: iterate over all children and cancel them
}


// =============================================================================
// ==== internal ---------------------------------------------------------------

void swift::swift_task_group_add_pending(AsyncTask *pendingTask, TaskGroup *group) {
  group->statusAddPendingTaskRelaxed(pendingTask);
}
