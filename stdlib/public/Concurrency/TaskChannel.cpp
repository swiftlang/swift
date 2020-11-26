//===--- TaskChannel.cpp - Task Group internal message channel ------------===//
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
#include<stdio.h>

using namespace swift;
using ChannelFragment = AsyncTask::ChannelFragment;
using FutureFragment = AsyncTask::FutureFragment;

using ReadyQueueItem = ChannelFragment::ReadyQueueItem;
using ReadyStatus = ChannelFragment::ReadyQueueStatus;
using ChannelPollResult = ChannelFragment::ChannelPollResult;

// ==== destroy ----------------------------------------------------------------

void
ChannelFragment::destroy() {
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
}

// ==== channelOffer -----------------------------------------------------------

void
AsyncTask::channelOffer(AsyncTask *completedTask, AsyncContext *context,
                       ExecutorRef executor) {
  assert(completedTask);
  assert(completedTask->isFuture());
  assert(completedTask->hasChildFragment() &&
      "Attempted to offer non-child task! "
      "Only child tasks may offer their results to their parent.");
  assert(completedTask->childFragment()->getParent() == this);

  assert(isChannel());
  auto fragment = channelFragment();

  auto oldStatus = fragment->statusCompletePendingTask();
  fprintf(stderr, "error: channelOffer[%d]: old status %s \n", __LINE__, oldStatus.to_string().c_str());

  // TODO: is this right? we should rather reuse the child I guess?
  // If an error was thrown, save it in the future fragment.
  auto futureContext = static_cast<FutureAsyncContext *>(context);
  bool hadErrorResult = false;
  if (auto errorObject = futureContext->errorResult) {
    // future does: fragment->getError() = errorObject;
    // instead we need to enqueue this result:
    hadErrorResult = true;
  }

  // TODO: cas check status?

  if (auto pendingTasks = oldStatus.pendingTasks()) {
    fprintf(stderr, "error: channelOffer[%d]: pending tasks: %d\n", __LINE__, pendingTasks);
    // ==== run waiter ---------------------------------------------------------
    // We are the "first" completed task to arrive, since old status had zero
    //
    // If old status had no tasks, it means we are the first to arrive,
    // and as such may directly get and signal the first waiting task.
    // We only signal *one* waiter and relink the waiter queue.
    auto waitHead = fragment->waitQueue.load(std::memory_order_acquire);
    while (auto waitingTask = waitHead.getTask()) { // FIXME: look hard at this loop, does it make sense?
      // Find the next waiting task.
      auto nextWaitingTask = waitingTask->getNextWaitingTask();
      auto nextWaitQueueItem = ChannelFragment::WaitQueueItem::get(
        ChannelFragment::WaitStatus::Executing, // TODO: waiting?
        nextWaitingTask
      );

      // Attempt to claim it, we are the future that is going to complete it.
      // TODO: there may be other futures trying to do the same right now? FIXME: not really because the status right?
      if (fragment->waitQueue.compare_exchange_weak(waitHead, nextWaitQueueItem,
          /*success*/ std::memory_order_release,
          /*failure*/ std::memory_order_acquire)) {
        // Run the task.
        auto completedFragment = completedTask->futureFragment();
        swift::runTaskWithFutureResult(waitingTask, executor,
            completedFragment, hadErrorResult);
        return;
      }

      // DO NOT move to the next task, one element is only signalled *once*.
      // E.g. if we somehow had two next() registered, each should get
      // individual elements, not the same element after all (!).
//       Move to the next task.
//      waitingTask = nextWaitingTask;
    }
  } else {
    fprintf(stderr, "error: channelOffer[%d]: pending tasks: 0, enqueue\n", __LINE__);
    // ==== enqueue message ----------------------------------------------------
    //
    // no-one was waiting (yet), so we have to instead enqueue to the message queue
    // when a task polls during next() it will notice that we have a value ready
    // for it, and will process it immediately without suspending.

    auto readyItem = ReadyQueueItem::get(
        hadErrorResult ? ReadyStatus::Error : ReadyStatus::Success,
        completedTask
    );
    // TODO: The enqueue performs a copy; so we pass a local value there but it is just a pointer
    // so this works out well I think? Originally enqueue was defined to take a reference.
    fragment->readyQueue.enqueue(readyItem);
    return;

//    // load the ready queue's head; it could be empty or be some other value,
//    // we don't really care right now, as we simply make it our new tail,
//    // and become the new head. // FIXME: this means we end up in reverse order (wrt completion)...
//    auto readyHead = fragment->readyQueue.load(std::memory_order_acquire);
//
//    while (true) {
//      completedTask->getNextChannelCompletedTask() = readyHead.getTask();
//      auto newReadyHead = ReadyQueueItem::get(
//          hadErrorResult ? ReadyStatus::Error : ReadyStatus::Success,
//          completedTask
//      );
//
//      if (fragment->readyQueue.compare_exchange_weak(
//          readyHead, newReadyHead,
//          /*success*/ std::memory_order_release,
//          /*failure*/ std::memory_order_acquire)) {
//        return;
//      } // else, try again
//    }
  }
}

// ==== pollChannel ------------------------------------------------------------

ChannelFragment::ChannelPollResult
AsyncTask::channelPoll(AsyncTask *waitingTask) {
  assert(isChannel());
  auto fragment = channelFragment();

  // immediately update the status counter
  auto status = fragment->statusAddWaitingTask();

  ChannelPollResult result;
  // FIXME: read status to know if tasks in flight or not.

  auto isEmpty = status.pendingTasks() == 0;
  if (isEmpty) {
    // 1) No tasks in flight, we know no tasks were submitted before this poll
    //    was issued, and if we parked here we'd potentially never be woken up.
    //    Bail out and return `nil` from `group.next()`.
    fragment->statusRemoveWaitingTask(); // "revert" our eager +1 we just did

    result.status = ChannelFragment::ChannelPollStatus::Empty;
    result.storage = nullptr;
    return result;
  }

  ReadyQueueItem item;
  if (!fragment->readyQueue.dequeue(item)) {
    assert(item.getStatus() == ReadyStatus::Empty);
    // TODO: order of wakeups! if this right or will cause us trouble if asyn lets are used for next?
    // 2) No ready tasks in the ready queue yet; we will have to wait/suspend
    //    until pending tasks complete.
    while (true) {
      // Put the waiting task at the beginning of the wait queue.
      auto waitHead = fragment->waitQueue.load(std::memory_order_acquire);
      waitingTask->getNextWaitingTask() = waitHead.getTask();
      auto newWaitHead = ChannelFragment::WaitQueueItem::get(
          ChannelFragment::WaitStatus::Executing, waitingTask);
      if (fragment->waitQueue.compare_exchange_weak(waitHead, newWaitHead,
          /*success*/ std::memory_order_release,
          /*failure*/ std::memory_order_acquire)) {
        // While usually the waiting task will be the group task,
        // we can attempt to escalate group task here. // TODO: does this make sense?
        // Note that we cannot escalate the specific future (child) task we'd
        // like to complete, since we don't know which one that might be.
        // TODO: we could speculatively pick some task and escalate it though that may have its own issues with fairness.
        swift_task_escalate(this, waitingTask->Flags.getPriority());

        fprintf(stderr, "error: added waiting task: %d\n", (status.waitingTasks() + 1));
        result.status = ChannelFragment::ChannelPollStatus::Waiting;
        return result;
      }
    }
  }

  assert(item.getTask()->isFuture());
  auto futureFragment = item.getTask()->futureFragment();
  switch (item.getStatus()) {
    case ReadyStatus::Success:
      // great, we can immediately return the polled value
      result.status = ChannelFragment::ChannelPollStatus::Success;
      result.storage = futureFragment->getStoragePtr();
      break;
    case ReadyStatus::Error:
      // great, we can immediately return the polled value
      result.status = ChannelFragment::ChannelPollStatus::Error;
      result.storage =
          reinterpret_cast<OpaqueValue *>(futureFragment->getError());
      break;
    case ReadyStatus::Empty:
      assert(false && "task status should never be Empty once successfully "
                      "polled a completed task from a task groups channel.");
  }
//  FutureFragment *futureFragment; // only used when Success or Failure
//  switch (item.getStatus()) {
//    case ReadyStatus::Empty:
//      // empty and no tasks in flight, bail out!
//      result.hadAnyResult = false;
//      result.hadErrorResult = false;
//      result.storage = nullptr;
//      return result;
//    case ReadyStatus::Empty:
//      // we will have to wait/suspend until pending tasks complete
//      shouldWait = true;
//      assert(false && "should wait not done yet.");
//      break;
//    case ReadyStatus::Success:
//      // great, we can immediately return the polled value
//      result.hadAnyResult = true; // FIXME: do we need this if we can just keep storage as null?
//      result.hadErrorResult = false;
//      assert(item.getTask()->isFuture());
//      futureFragment = item.getTask()->futureFragment();
//      result.storage = futureFragment->getStoragePtr();
//      break;
//    case ReadyStatus::Error:
//      // great, we can immediately return the polled value
//      result.hadAnyResult = true; // FIXME: do we need this if we can just keep storage as null?
//      result.hadErrorResult = true;
//      assert(item.getTask()->isFuture());
//      futureFragment = item.getTask()->futureFragment();
//      result.storage =
//          reinterpret_cast<OpaqueValue *>(futureFragment->getError());
//      break;
//  }

  return result;
}

void swift::swift_task_channel_poll(
    AsyncTask *waitingTask, ExecutorRef executor,
    AsyncContext *rawContext) {
  // Suspend the waiting task.
  waitingTask->ResumeTask = rawContext->ResumeParent;
  waitingTask->ResumeContext = rawContext;

  // Wait on the future.
  auto context = static_cast<TaskFutureWaitAsyncContext *>(rawContext);
  auto task = context->task;
  // assert(task->isFuture()); // TODO: shall it also be a future?
  assert(task->isChannel());

  ChannelPollResult polled = task->channelPoll(waitingTask);

  if (!polled.storage) {
    // The waiting task has been queued on the channel,
    // there were pending tasks so it will be woken up eventually.
    return;
  }

  runTaskWithChannelPollResult(waitingTask, executor, polled);
}

// ==== isEmpty ----------------------------------------------------------------

bool swift::swift_task_channel_is_empty(AsyncTask *task) {
  assert(task->isChannel());
  return task->channelFragment()->isEmpty();
}

void swift::swift_task_channel_add_pending(AsyncTask *channelTask, AsyncTask *childTask) {
  assert(channelTask->isChannel());
  channelTask->channelFragment()->statusAddPendingTask();
}
