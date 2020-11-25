//===--- Task.cpp - Task object and management ----------------------------===//
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
// Object management routines for asynchronous task objects.
//
//===----------------------------------------------------------------------===//

#include "swift/Runtime/Concurrency.h"
#include "swift/ABI/Task.h"
#include "swift/ABI/Metadata.h"
#include "swift/Runtime/HeapObject.h"
#include "TaskPrivate.h"

using namespace swift;
using ChannelFragment = AsyncTask::ChannelFragment;
using FutureFragment = AsyncTask::FutureFragment;

using ReadyQueueItem = ChannelFragment::ReadyQueueItem;
using ReadyQueueStatus = ChannelFragment::ReadyQueueStatus;
using ChannelPollResult = ChannelFragment::ChannelPollResult;

using WaitQueueItem = ChannelFragment::WaitQueueItem;
using WaitStatus = ChannelFragment::WaitStatus;

// ==== destroy ----------------------------------------------------------------

void
ChannelFragment::destroy() {
  // FIXME: implement this
//  auto waitHead = waitQueue.load(std::memory_order_acquire);
//  switch (waitHead.getStatus()) {
//  case Status::Empty:
//    assert(false && "destroying a task that never completed");
//
//  case Status::Success:
//    resultType->vw_destroy(getStoragePtr());
//    break;
//
//  case Status::Error:
//    swift_unknownObjectRelease(reinterpret_cast<OpaqueValue *>(getError()));
//    break;
//  }
}

// ==== channelOffer -----------------------------------------------------------

void
AsyncTask::channelOffer(AsyncTask *completedTask, AsyncContext *context,
                       ExecutorRef executor) {
  assert(completedTask);
  assert(completedTask->hasChildFragment() &&
      "Attempted to offer non-child task! "
      "Only child tasks may offer their results to their parent.");
  assert(completedTask->isFuture());

  assert(isChannel());
  auto fragment = channelFragment();

  // TODO: is this right? we should rather reuse the child I guess?
  // If an error was thrown, save it in the future fragment.
  auto futureContext = static_cast<FutureAsyncContext *>(context);
  bool hadErrorResult = false;
  if (auto errorObject = futureContext->errorResult) {
    // future does: fragment->getError() = errorObject;
    // instead we need to enqueue this result:
    hadErrorResult = true;
  }

//  // Update the wait queue, signaling that if there were any waiters,
//  // we'll have processed them by the end of this function.
//  auto newWaitHead = WaitQueueItem::get(
//      WaitStatus::Pending,
//      nullptr
//  );
//  auto waitHead = fragment->waitQueue.exchange(
//      newWaitHead, std::memory_order_acquire);

  auto waitHead = fragment->waitQueue.load(std::memory_order_acquire);
  if (auto waitingTask = waitHead.getTask()) {
    // ==== run waiters --------------------------------------------------------
    //
    // Some tasks were waiting, schedule them all on the executor.
    while (waitingTask) {
      // Find the next waiting task.
      auto nextWaitingTask = waitingTask->getNextWaitingTask();

      // Run the task.
      auto completedFragment = completedTask->futureFragment();
      swift::runTaskWithFutureResult(waitingTask, executor, completedFragment, hadErrorResult);

      // Move to the next task.
      waitingTask = nextWaitingTask;
    }
    // FIXME: have we cleared the queue?
    return;
  } else {
    // ==== enqueue message ----------------------------------------------------
    //
    // no-one was waiting (yet), so we have to instead enqueue to the message queue
    // when a task polls during next() it will notice that we have a value ready
    // for it, and will process it immediately without suspending.

    // load the ready queue's head; it could be empty or be some other value,
    // we don't really care right now, as we simply make it our new tail,
    // and become the new head. // FIXME: this means we end up in reverse order (wrt completion)...
    auto readyHead = fragment->readyQueue.load(std::memory_order_acquire);

    // TODO: replace with a "real" fifo queue, since we get the events in reverse
    //       order and just peeling off one event is quite a pain now.

    while (true) {
      completedTask->getNextChannelCompletedTask() = readyHead.getTask();
      auto newReadyHead = ReadyQueueItem::get(
          hadErrorResult ? ReadyQueueStatus::Error : ReadyQueueStatus::Success,
          completedTask
      );

      if (fragment->readyQueue.compare_exchange_weak(
          readyHead, newReadyHead,
          /*success*/ std::memory_order_release,
          /*failure*/ std::memory_order_acquire)) {
        return;
      } // else, try again
    }

    //    auto newReadyHead = ReadyQueueItem::get(
      //        hadErrorResult ? ReadyQueueStatus::Error : ReadyQueueStatus::Success,
      //        completedTask
      //    );
      //    auto readyHead = fragment->readyQueue.exchange(
      //        newReadyHead, std::memory_order_acquire);
      //    while (true) {
      //      // Put the completed task at the beginning of the message queue.
      //      waitingTask->getNextWaitingTask() = readyHead.getTask();
      //      if (fragment->readyQueue.compare_exchange_weak(
      //          readyHead, newReadyHead, std::memory_order_release,
      //          std::memory_order_acquire)) {
      //        // Escalate the priority of this task based on the priority
      //        // of the waiting task.
      //        swift_task_escalate(this, waitingTask->Flags.getPriority());
      //        return;
      //      }
      //    }
  }
}

//void AsyncTask::channelOffer(AsyncContext *context, ExecutorRef executor) {
//  assert(isChannel());
//  auto fragment = ChannelFragment();
//
//  // If an error was thrown, save it in the future fragment.
//  auto futureContext = static_cast<FutureAsyncContext *>(context);
//  bool hadErrorResult = false;
//  if (auto errorObject = futureContext->errorResult) {
//    fragment->getError() = errorObject;
//    hadErrorResult = true;
//  }
//
//  // Update the status to signal completion.
//  auto newWaitHead = WaitQueueItem::get(
//    hadErrorResult ? WaitStatus::Error : Status::Success,
//    nullptr
//  );
//  auto messageHead = fragment->waitQueue.exchange(newWaitHead, std::memory_order_acquire);
//  assert(messageHead.getStatus() == WaitStatus::Executing);
//
//  // Schedule every waiting task on the executor.
//  auto waitingTask = messageHead.getTask();
//  while (waitingTask) {
//    // Find the next waiting task.
//    auto nextWaitingTask = waitingTask->getNextWaitingTask();
//
//    // Run the task.
//    runTaskWithFutureResult(waitingTask, executor, fragment, hadErrorResult);
//
//    // Move to the next task.
//    waitingTask = nextWaitingTask;
//  }
//}

// ==== pollChannel ------------------------------------------------------------

ChannelPollResult
AsyncTask::channelPoll(AsyncTask *waitingTask) {
  assert(isChannel());
  auto fragment = channelFragment();

  auto readyQueueHead = fragment->readyQueue.load(std::memory_order_acquire);
  auto readyQueueHeadStatus = readyQueueHead.getStatus();
  auto shouldWait = false;
  switch (readyQueueHeadStatus) {
    case ReadyQueueStatus::Empty:
      // empty and no tasks in flight, bail out!
      ChannelPollResult result;
      result.hadAnyResult = false;
      return result;
    case ReadyQueueStatus::Pending:
      // we will have to wait/suspend until pending tasks complete
      shouldWait = true;
      break;
    case ReadyQueueStatus::Success:
    case ReadyQueueStatus::Error:
      // great, we can immediately return the polled value
      break;
  }

  if (shouldWait) {
    // ==== Enqueue task in the wait queue -------------------------------------
    assert(false && "IMPLEMENT ADDING TO WAITERS IN AsyncTask::channelPoll");
    // return ...;
  }

  // ==== Ready tasks in queue, poll one ----------------------------------------

//  while (true) {
//    switch (channelStatus) {
//    case ChannelFragment::ReadyQueueStatus::Empty:
//      // 1) No tasks in-flight or already complete, nothing to do.
//      return channelStatus;
//
//    default:
//      // The channel has completed tasks waiting to be pulled.
//      break;
//    }
//
//    auto messageHead = fragment->readyQueue.load(std::memory_order_acquire);
//    if (auto headTask = messageHead.getTask()) {
//      // 2) there is a completed message enqueued, we can pull it right away without suspending
//      return channelStatus;
//    }
//
//    // 3) Put the waiting task at the beginning of the wait queue.
//    auto waitHead = fragment->waitQueue.load(std::memory_order_acquire);
//    waitingTask->getNextWaitingTask() = waitHead.getTask();
//    auto newWaitHead = WaitQueueItem::get(WaitStatus::Pending, waitingTask);
//    if (fragment->waitQueue.compare_exchange_weak(waitHead, newWaitHead,
//            /*success*/ std::memory_order_release,
//            /*failure*/ std::memory_order_acquire)) {
//      // Escalate the priority of this task based on the priority
//      // of the waiting task.
//      swift_task_escalate(this, waitingTask->Flags.getPriority());
//      return ChannelFragment::ReadyQueueStatus::Empty; // FIXME: wrong
//    }
//  }
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

  auto polled = task->channelPoll(waitingTask);
//  if (!polled.hadAnyResult) {
//    return;
//  }

  runTaskWithChannelPollResult(waitingTask, executor, polled);
//  switch (polled.status) {
//  case ReadyQueueStatus::Empty:
//    // The waiting task has been queued on the future.
//    return;
//
//  case ReadyQueueStatus::Success:
//    // Run the task with a successful result.
//    // FIXME: Want to guarantee a tail call here
//    runTaskWithFutureResult(
//        waitingTask, executor, task->channelFragment(),
//        /*hadErrorResult=*/false);
//    return;
//
// case ReadyQueueStatus::Error:
//    // Run the task with an error result.
//    // FIXME: Want to guarantee a tail call here
//    runTaskWithFutureResult(
//        waitingTask, executor, task->channelFragment(),
//        /*hadErrorResult=*/true);
//    return;
//  }
}

// ==== isEmpty ----------------------------------------------------------------

bool swift::swift_task_channel_is_empty(AsyncTask *task) {
  assert(task->isChannel());
  return task->channelFragment()->isEmpty();
}
