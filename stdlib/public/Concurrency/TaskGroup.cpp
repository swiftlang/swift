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
#include <iostream>
#include <pthread.h>
#include <stdio.h>

using namespace swift;
using GroupFragment = AsyncTask::GroupFragment;
using FutureFragment = AsyncTask::FutureFragment;

using ReadyQueueItem = GroupFragment::ReadyQueueItem;
using ReadyStatus = GroupFragment::ReadyQueueStatus;
using GroupPollResult = GroupFragment::GroupPollResult;

// =============================================================================
// ==== destroy ----------------------------------------------------------------

void
GroupFragment::destroy() {
  // FIXME: implement this properly
//  auto waitHead = waitQueue.load(std::memory_order_acquire);
//  switch (waitHead.getStatus()) {
//  case WaitStatus::Empty:
//    assert(false && "destroying a task that never completed");
//
//  case WaitStatus::Success:
//    resultType->vw_destroy(getStoragePtr());
//    break;
//
//  case WaitStatus::Error:
//    swift_unknownObjectRelease(reinterpret_cast<OpaqueValue *>(getError()));
//    break;
//  }
  // FIXME: all tasks still in the readyQueue must be: swift_release(...)
}

// =============================================================================
// ==== groupOffer -------------------------------------------------------------

void AsyncTask::groupOffer(AsyncTask *completedTask, AsyncContext *context,
                           ExecutorRef executor) {
  assert(completedTask);
  assert(completedTask->isFuture());
  assert(completedTask->hasChildFragment() &&
      "Attempted to offer non-child task! "
      "Only child tasks may offer their results to their parent.");
  assert(completedTask->childFragment()->getParent() == this);

  assert(isTaskGroup());
  auto fragment = groupFragment();
  auto status = fragment->statusAddReadyTaskLoad();

  // TODO: is this right? we should rather reuse the child I guess?
  // If an error was thrown, save it in the future fragment.
  auto futureContext = static_cast<FutureAsyncContext *>(context);
  bool hadErrorResult = false;
  if (auto errorObject = futureContext->errorResult) {
    // future does: fragment->getError() = errorObject;
    // instead we need to enqueue this result:
    hadErrorResult = true;
  }

  fprintf(stderr, "error: %s[%d %s:%d]: group swift completedTask: %d REF_COUNT: %d\n",
          __FUNCTION__, pthread_self(), __FILE__, __LINE__,
          completedTask, swift::swift_retainCount(completedTask));

  while (true) {
    // Loop until we either:
    // a) no waiters available, and we enqueued the completed task to readyQueue
    // b) successfully claim a waiter to complete with this task
    assert(status.pendingTasks() && "attempted to offer value, when no pending tasks remaining");
//    fprintf(stderr, "error: %s[%d %s:%d]: spin, taking P/W tasks %s \n",
//            __FUNCTION__, pthread_self(), __FILE__, __LINE__, status.to_string().c_str());
    if (status.waitingTasks() == 0) {
//      fprintf(stderr, "error: %s[%d %s:%d]: spin complete, zero waiting tasks; enqueue future; old status: %s\n",
//              __FUNCTION__, pthread_self(), __FILE__, __LINE__, status.to_string().c_str());
      // ==== a) enqueue message -----------------------------------------------
      //
      // no-one was waiting (yet), so we have to instead enqueue to the message queue
      // when a task polls during next() it will notice that we have a value ready
      // for it, and will process it immediately without suspending.

      // Retain the task while it is in the queue;
      // it must remain alive until the task group is alive.
      swift_retain(completedTask);
      fprintf(stderr, "error: %s[%d %s:%d]: group swift_retained completedTask: %d REF_COUNT: %d\n",
              __FUNCTION__, pthread_self(), __FILE__, __LINE__, completedTask, swift::swift_retainCount(completedTask));
      auto readyItem = ReadyQueueItem::get(
          hadErrorResult ? ReadyStatus::Error : ReadyStatus::Success,
          completedTask
      );

      assert(completedTask == readyItem.getTask());
      assert(readyItem.getTask()->isFuture());
      fragment->readyQueue.enqueue(readyItem);
      return;
    }

    if (fragment->statusCompleteReadyPendingWaitingTasks(status)) {
      fprintf(stderr, "error: %s[%d %s:%d]: spin complete, got waiter; value [%d], old status: %s \n",
              __FUNCTION__, pthread_self(), __FILE__, __LINE__,
              completedTask->futureFragment()->getStoragePtr(), status.to_string().c_str());
        fprintf(stderr, "error: %s[%d %s:%d]: waiting tasks: %d\n",
                __FUNCTION__, pthread_self(), __FILE__, __LINE__, status.waitingTasks());
        // ==== b) run waiter --------------------------------------------------
        // We are the "first" completed task to arrive, since old status had zero
        //
        // If old status had no tasks, it means we are the first to arrive,
        // and as such may directly get and signal the first waiting task.
        // We only signal *one* waiter and relink the waiter queue.
        auto waitHead = fragment->waitQueue.load(std::memory_order_acquire);
        while (auto waitingTask = waitHead.getTask()) { // FIXME: look hard at this loop, does it make sense?
          // Find the next waiting task.
          auto nextWaitingTask = waitingTask->getNextWaitingTask();
          auto nextWaitQueueItem = GroupFragment::WaitQueueItem::get(
              GroupFragment::WaitStatus::Executing, // TODO: waiting?
              nextWaitingTask
          );

          // Attempt to claim it, we are the future that is going to complete it.
          // TODO: there may be other futures trying to do the same right now? FIXME: not really because the status right?
          if (fragment->waitQueue.compare_exchange_weak(waitHead, nextWaitQueueItem,
              /*success*/ std::memory_order_release,
              /*failure*/ std::memory_order_acquire)) {
            // Run the task.
            auto result = GroupPollResult::get(
                completedTask, hadErrorResult, /*needsRelease*/ false);
            swift::runTaskWithGroupPollResult(waitingTask, executor, result);
            return;
          }

          // DO NOT move to the next task, one element is only signalled *once*.
          // E.g. if we somehow had two next() registered, each should get
          // individual elements, not the same element after all (!).
          //       Move to the next task.
          //      waitingTask = nextWaitingTask;
        }
    } // else, status-cas failed and we need to try again
  }
}

// =============================================================================
// ==== group.next() implementation (wait_next and groupPoll) ------------------

void swift::swift_task_group_wait_next(
    AsyncTask *waitingTask,
    ExecutorRef executor,
    AsyncContext *rawContext) {
  waitingTask->ResumeTask = rawContext->ResumeParent;
  waitingTask->ResumeContext = rawContext;
  fprintf(stderr, "error: %s[%d %s:%d]: invoked\n",
          __FUNCTION__, pthread_self(), __FILE__, __LINE__);

  // Wait on the future.
  auto context = static_cast<TaskFutureWaitAsyncContext *>(rawContext);
  auto task = context->task;
  // assert(task->isFuture()); // TODO: shall it also be a future?
  assert(task->isTaskGroup());

  GroupPollResult polled = task->groupPoll(waitingTask);
  fprintf(stderr, "error: %s[%d %s:%d]: polled storage: %d\n",
          __FUNCTION__, pthread_self(), __FILE__, __LINE__, polled);

  if (polled.status == GroupFragment::ChannelPollStatus::Waiting) {
    // The waiting task has been queued on the channel,
    // there were pending tasks so it will be woken up eventually.
    fprintf(stderr, "error: %s[%d %s:%d]: polled storage: %d -> WAITING\n",
            __FUNCTION__, pthread_self(), __FILE__, __LINE__, polled);
    return;
  }
  if (polled.status == GroupFragment::ChannelPollStatus::Empty) {
    fprintf(stderr, "error: %s[%d %s:%d]: polled storage: %d -> EMPTY\n",
            __FUNCTION__, pthread_self(), __FILE__, __LINE__, polled);
  }
  if (polled.status == GroupFragment::ChannelPollStatus::Success) {
    fprintf(stderr, "error: %s[%d %s:%d]: polled storage: %d -> Success\n",
            __FUNCTION__, pthread_self(), __FILE__, __LINE__, polled);
  }
  if (polled.status == GroupFragment::ChannelPollStatus::Error) {
    fprintf(stderr, "error: %s[%d %s:%d]: polled storage: %d -> Error\n",
            __FUNCTION__, pthread_self(), __FILE__, __LINE__, polled);
  }

  runTaskWithGroupPollResult(waitingTask, executor, polled);
}

GroupFragment::GroupPollResult
AsyncTask::groupPoll(AsyncTask *waitingTask) {
  assert(isTaskGroup());
  auto fragment = groupFragment();

  // immediately update the status counter
  auto assumed = fragment->statusAddWaitingTaskLoad();

//  fprintf(stderr, "error: %s[%d %s:%d]: assume status: %s\n",
//          __FUNCTION__, pthread_self(), __FILE__, __LINE__,
//          assumed.to_string().c_str());

  GroupPollResult result;
  result.storage = nullptr;
  result.retainedTask = nullptr;

  // ==== 1) bail out early if no tasks are pending ----------------------------
  if (assumed.isEmpty()) {
//    fprintf(stderr, "error: %s[%d %s:%d]: polled, isEmpty\n",
//            __FUNCTION__, pthread_self(), __FILE__, __LINE__);
    // 1) No tasks in flight, we know no tasks were submitted before this poll
    //    was issued, and if we parked here we'd potentially never be woken up.
    //    Bail out and return `nil` from `group.next()`.
    fragment->statusRemoveWaitingTask(); // "revert" our eager +1 we just did

    result.status = GroupFragment::ChannelPollStatus::Empty;
    return result;
  }

  // ==== Add to wait queue ----------------------------------------------------
  while (true) {
//    fprintf(stderr, "error: %s[%d %s:%d]: add waiting, spin...\n",
//            __FUNCTION__, pthread_self(), __FILE__, __LINE__);
    // Put the waiting task at the beginning of the wait queue.
    auto waitHead = fragment->waitQueue.load(std::memory_order_acquire);
    waitingTask->getNextWaitingTask() = waitHead.getTask();
    auto newWaitHead = GroupFragment::WaitQueueItem::get(
        GroupFragment::WaitStatus::Executing, waitingTask);
    if (fragment->waitQueue.compare_exchange_weak(
        waitHead, newWaitHead,
        /*success*/ std::memory_order_release,
        /*failure*/ std::memory_order_acquire)) {
      // While usually the waiting task will be the group task,
      // we can attempt to escalate group task here. // TODO: does this make sense?
      // Note that we cannot escalate the specific future (child) task we'd
      // like to complete, since we don't know which one that might be.
      swift_task_escalate(this, waitingTask->Flags.getPriority());

//      fprintf(stderr, "error: %s[%d %s:%d]: added waiting task. Waiting tasks: %d\n",
//              __FUNCTION__, pthread_self(), __FILE__, __LINE__,
//              (assumed.waitingTasks()));
      result.status = GroupFragment::ChannelPollStatus::Waiting;
      // return result;
      break;
    } // else, try again
  }

  fprintf(stderr, "error: %s[%d %s:%d]: Assume status: %s\n",
          __FUNCTION__, pthread_self(), __FILE__, __LINE__, assumed.to_string().c_str());

  // ==== 3) Ready task was polled, return with it immediately -----------------
  auto assumedStatus = assumed.status;
  while (assumed.readyTasks()) {
    fprintf(stderr, "error: %s[%d %s:%d]: attempt to dequeue, attempt cas assume status to be: %s\n",
            __FUNCTION__, pthread_self(), __FILE__, __LINE__,
            assumed.to_string().c_str());
  auto newStatus = GroupFragment::GroupStatus{ assumedStatus };
  if (fragment->status.compare_exchange_weak(
      assumedStatus, newStatus.completingReadyPendingWaitingTask().status,
        /*success*/ std::memory_order_relaxed,
        /*failure*/ std::memory_order_acquire)) {

    // Success! We are allowed to poll.
    fprintf(stderr, "error: %s[%d %s:%d]: Success! We are allowed to poll; status=%s\n",
            __FUNCTION__, pthread_self(), __FILE__, __LINE__, assumed.to_string().c_str());

    ReadyQueueItem item;
      bool taskDequeued = fragment->readyQueue.dequeue(item);
      fprintf(stderr, "error: %s[%d %s:%d]: dequeue, taskDequeued=%d\n",
              __FUNCTION__, pthread_self(), __FILE__, __LINE__, taskDequeued);

      if (!taskDequeued) {
        fprintf(stderr, "error: %s[%d %s:%d]: dequeue failed, taskDequeued=%d, status=%s\n",
                __FUNCTION__, pthread_self(), __FILE__, __LINE__,
                taskDequeued, GroupFragment::GroupStatus{assumedStatus}.to_string().c_str());
        result.status = GroupFragment::ChannelPollStatus::Waiting;
        result.storage = nullptr;
        result.retainedTask = nullptr;
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
          fprintf(stderr, "error: %s[%d %s:%d]: eagerly return success task\n",
                  __FUNCTION__, pthread_self(), __FILE__, __LINE__);
          fprintf(stderr, "error: %s[%d %s:%d]: status now DEFINITELY is: %s\n",
                  __FUNCTION__, pthread_self(), __FILE__, __LINE__, fragment->statusLoad().to_string().c_str());
          // great, we can immediately return the polled value
          result.status = GroupFragment::ChannelPollStatus::Success;
          result.storage = futureFragment->getStoragePtr();
          assert(result.retainedTask && "polled a task, it must be not null");
          return result;

        case ReadyStatus::Error:
          fprintf(stderr, "error: %s[%d %s:%d]: eagerly return error task, status now: %s\n",
                  __FUNCTION__, pthread_self(), __FILE__, __LINE__, fragment->statusLoad().to_string().c_str());
          // great, we can immediately return the polled value
          result.status = GroupFragment::ChannelPollStatus::Error;
          result.storage =
              reinterpret_cast<OpaqueValue *>(futureFragment->getError());
          assert(result.retainedTask && "polled a task, it must be not null");
          return result;

        case ReadyStatus::Empty:
          fprintf(stderr, "error: %s[%d %s:%d]: EMPTY! status now: \n",
                  __FUNCTION__, pthread_self(), __FILE__, __LINE__, fragment->statusLoad().to_string().c_str());
          result.status = GroupFragment::ChannelPollStatus::Empty;
          result.storage = nullptr;
          result.retainedTask = nullptr;
          return result;
      }
      assert(false && "must return result when status compare-and-swap was successful");
    } // else, we failed status-cas (some other waiter claimed a ready pending task, try again)
    fprintf(stderr, "error: %s[%d %s:%d]: cas failed, assume: %s\n",
            __FUNCTION__, pthread_self(), __FILE__, __LINE__,
            assumed.to_string().c_str());
  } // no more ready tasks

  fprintf(stderr, "error: %s[%d %s:%d]: no ready tasks, give up, must wait (status: %s)\n",
          __FUNCTION__, pthread_self(), __FILE__, __LINE__, assumed.to_string().c_str());


  fprintf(stderr, "error: %s[%d %s:%d]: no ready tasks, must wait\n",
          __FUNCTION__, pthread_self(), __FILE__, __LINE__);

  // no ready tasks, so we must wait.
  result.status = GroupFragment::ChannelPollStatus::Waiting;
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

void swift::swift_task_group_add_pending(AsyncTask *groupTask, AsyncTask *childTask) {
  assert(groupTask->isTaskGroup());
  auto status = groupTask->groupFragment()->statusAddPendingTaskLoad();
  fprintf(stderr, "error: %s[%d %s:%d]: add pending; groupTask: %d, childTask: %d -> %s\n",
          __FUNCTION__, pthread_self(), __FILE__, __LINE__, groupTask, childTask, status.to_string().c_str());
}

void swift::swift_task_print_ID(const char* name, const char* file, int line, AsyncTask *task) {
  fprintf(stderr, "error: swift_task_print_ID(%s)[%d %s:%d]: task: %d\n",
          name, pthread_self(), file, line, task);
}
