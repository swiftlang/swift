//===--- TaskGroup.cpp - Task Group internal message channel ------------===//
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
// Object management for async child tasks that are children of a task group.
//
//===----------------------------------------------------------------------===//

#include "swift/Runtime/Concurrency.h"
#include "swift/ABI/Task.h"
#include "swift/ABI/Metadata.h"
#include "swift/Runtime/HeapObject.h"
#include "TaskPrivate.h"

using namespace swift;
using GroupFragment = AsyncTask::GroupFragment;
using FutureFragment = AsyncTask::FutureFragment;

using ReadyQueueItem = GroupFragment::ReadyQueueItem;
using ReadyStatus = GroupFragment::ReadyStatus;
using GroupPollResult = GroupFragment::GroupPollResult;

// =============================================================================
// ==== destroy ----------------------------------------------------------------

void GroupFragment::destroy() {
  // TODO: need to release all waiters as well
//  auto waitHead = waitQueue.load(std::memory_order_acquire);
//  switch (waitHead.getStatus()) {
//    case GroupFragment::WaitStatus::Waiting:
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
}

// =============================================================================
// ==== groupOffer -------------------------------------------------------------

void AsyncTask::groupOffer(AsyncTask *completedTask, AsyncContext *context,
                           ExecutorRef executor) {
  assert(completedTask);
  assert(completedTask->isFuture());
  assert(completedTask->hasChildFragment());
  assert(completedTask->childFragment()->getParent() == this);

  assert(isTaskGroup());
  auto fragment = groupFragment();
  fragment->mutex.lock(); // TODO: remove fragment lock, and use status for synchronization

  // Immediately increment ready count and acquire the status
  // Examples:
  //   R:0 P:1 W:0 -> R:1 P:1 W:0 //
  //   R:0 P:1 W:1 -> R:1 P:1 W:1 // complete immediately
  auto assumed = fragment->statusAddReadyTaskAcquire();

  // If an error was thrown, save it in the future fragment.
  auto futureContext = static_cast<FutureAsyncContext *>(context);
  bool hadErrorResult = false;
  if (auto errorObject = futureContext->errorResult) {
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
    fragment->readyQueue.enqueue(readyItem);
    fragment->mutex.unlock(); // TODO: remove fragment lock, and use status for synchronization
    return;
  }

  while (true) {
    // Loop until we either:
    // a) no waiters available, and we enqueued the completed task to readyQueue
    // b) successfully claim a waiter to complete with this task
    assert(assumed.pendingTasks() && "offered to group with no pending tasks!");
    if (fragment->statusCompleteReadyPendingWaitingTasks(assumed)) {
        // ==== b) run waiter --------------------------------------------------
        // We are the "first" completed task to arrive, since old status had zero
        //
        // If old status had no tasks, it means we are the first to arrive,
        // and as such may directly get and signal the first waiting task.
        // We only signal *one* waiter and relink the waiter queue.
        auto waitHead = fragment->waitQueue.load(std::memory_order_acquire);
        while (auto waitingTask = waitHead.getTask()) {
          // Find the next waiting task.
          auto nextWaitingTask = waitingTask->getNextWaitingTask();
          auto nextWaitQueueItem = GroupFragment::WaitQueueItem::get(
              GroupFragment::WaitStatus::Waiting,
              nextWaitingTask
          );

          // Attempt to claim it, we are the future that is going to complete it.
          // TODO: there may be other futures trying to do the same right now? FIXME: not really because the status right?
          if (fragment->waitQueue.compare_exchange_weak(
              waitHead, nextWaitQueueItem,
              /*success*/ std::memory_order_release,
              /*failure*/ std::memory_order_acquire)) {
            // Run the task.
            auto result = GroupPollResult::get(
                completedTask, hadErrorResult, /*needsRelease*/ false);

            fragment->mutex.unlock(); // TODO: remove fragment lock, and use status for synchronization
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

  llvm_unreachable("groupOffer must successfully complete it's cas-loop enqueue!");
}

// =============================================================================
// ==== group.next() implementation (wait_next and groupPoll) ------------------

void swift::swift_task_group_wait_next(
    AsyncTask *waitingTask,
    ExecutorRef executor,
    AsyncContext *rawContext) {
  waitingTask->ResumeTask = rawContext->ResumeParent;
  waitingTask->ResumeContext = rawContext;

  auto context = static_cast<TaskFutureWaitAsyncContext *>(rawContext);
  auto task = context->task;
  assert(task->isTaskGroup());

  GroupPollResult polled = task->groupPoll(waitingTask);
  if (polled.status == GroupFragment::GroupPollStatus::Waiting) {
    // The waiting task has been queued on the channel,
    // there were pending tasks so it will be woken up eventually.
    return;
  }

  runTaskWithGroupPollResult(waitingTask, executor, polled);
}

GroupFragment::GroupPollResult AsyncTask::groupPoll(AsyncTask *waitingTask) {
  assert(isTaskGroup());
  auto fragment = groupFragment();
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

    result.status = GroupFragment::GroupPollStatus::Empty;
    fragment->mutex.unlock(); // TODO: remove fragment lock, and use status for synchronization
    return result;
  }

  // ==== Add to wait queue ----------------------------------------------------
  while (true) {
    // Put the waiting task at the beginning of the wait queue.
    auto waitHead = fragment->waitQueue.load(std::memory_order_acquire);
    waitingTask->getNextWaitingTask() = waitHead.getTask();
    auto newWaitHead = GroupFragment::WaitQueueItem::get(
        GroupFragment::WaitStatus::Waiting, waitingTask);

    if (fragment->waitQueue.compare_exchange_weak(
        waitHead, newWaitHead,
        /*success*/ std::memory_order_release,
        /*failure*/ std::memory_order_acquire)) {
      // While usually the waiting task will be the group task,
      // we can attempt to escalate group task here. // TODO: does this make sense?
      // Note that we cannot escalate the specific future (child) task we'd
      // like to complete, since we don't know which one that might be.
      swift_task_escalate(this, waitingTask->Flags.getPriority());
      result.status = GroupFragment::GroupPollStatus::Waiting;
      // return result;
      break;
    } // else, try again
  }

  // ==== 3) Ready task was polled, return with it immediately -----------------
  auto assumedStatus = assumed.status;
  while (assumed.readyTasks()) {
    auto newStatus = GroupFragment::GroupStatus{assumedStatus};
    if (fragment->status.compare_exchange_weak(
        assumedStatus, newStatus.completingReadyPendingWaitingTask().status,
        /*success*/ std::memory_order_relaxed,
        /*failure*/ std::memory_order_acquire)) {

      // Success! We are allowed to poll.
      ReadyQueueItem item;
      bool taskDequeued = fragment->readyQueue.dequeue(item);
      if (!taskDequeued) {
        result.status = GroupFragment::GroupPollStatus::Waiting;
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
          result.status = GroupFragment::GroupPollStatus::Success;
          result.storage = futureFragment->getStoragePtr();
          assert(result.retainedTask && "polled a task, it must be not null");
          fragment->mutex.unlock(); // TODO: remove fragment lock, and use status for synchronization
          return result;

        case ReadyStatus::Error:
          // Immediately return the polled value
          result.status = GroupFragment::GroupPollStatus::Error;
          result.storage =
              reinterpret_cast<OpaqueValue *>(futureFragment->getError());
          assert(result.retainedTask && "polled a task, it must be not null");
          fragment->mutex.unlock(); // TODO: remove fragment lock, and use status for synchronization
          return result;

        case ReadyStatus::Empty:
          result.status = GroupFragment::GroupPollStatus::Empty;
          result.storage = nullptr;
          result.retainedTask = nullptr;
          fragment->mutex.unlock(); // TODO: remove fragment lock, and use status for synchronization
          return result;
      }
      assert(false && "must return result when status compare-and-swap was successful");
    } // else, we failed status-cas (some other waiter claimed a ready pending task, try again)
  } // no more ready tasks

  // no ready tasks, so we must wait.
  result.status = GroupFragment::GroupPollStatus::Waiting;
  fragment->mutex.unlock(); // TODO: remove fragment lock, and use status for synchronization
  return result;
}

// =============================================================================
// ==== isEmpty ----------------------------------------------------------------

bool swift::swift_task_group_is_empty(AsyncTask *task) {
  assert(task->isTaskGroup());
  return task->groupFragment()->isEmpty();
}

// =============================================================================
// ==== internal utils ---------------------------------------------------------

void swift::swift_task_group_add_pending(AsyncTask *task) {
  assert(task->isTaskGroup());
  task->groupFragment()->statusAddPendingTaskRelaxed();
}
