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

// ==== Destroy ----------------------------------------------------------------

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
AsyncTask::channelOffer(AsyncTask *completed, AsyncContext *context,
                       ExecutorRef executor) {
  assert(completed->hasChildFragment() &&
      "Attempted to offer non-child task! "
      "Only child tasks may offer their results to their parent.");
  assert(completed->isFuture());

  assert(isChannel());
  auto fragment = channelFragment();

  using Status = ChannelFragment::Status;
  using ReadyQueueStatus = ChannelFragment::ReadyQueueStatus;
  using ReadyQueueItem = ChannelFragment::ReadyQueueItem;
  using WaitQueueItem = ChannelFragment::WaitQueueItem;

  // TODO: is this right? we should rather reuse the child I guess?
  // If an error was thrown, save it in the future fragment.
  auto futureContext = static_cast<FutureAsyncContext *>(context);
  bool hadErrorResult = false;
  if (auto errorObject = futureContext->errorResult) {
    // future does: fragment->getError() = errorObject;
    // instead we need to enqueue this result:
    hadErrorResult = true;
  }

  // Update the wait queue, signaling that if there were any waiters,
  // we'll have processed them by the end of this function.
  auto newWaitQueueHead = WaitQueueItem::get(
      Status::Pending,
      nullptr
  );
  auto waitQueueHead = fragment->waitQueue.exchange(
      newWaitQueueHead, std::memory_order_acquire);

  auto waitingTask = waitQueueHead.getTask();
  if (waitingTask == nullptr) {
    // ==== enqueue message ----------------------------------------------------
    //
    // no-one was waiting (yet), so we have to instead enqueue to the message queue
    // when a task polls during next() it will notice that we have a value ready
    // for it, and will process it immediately without suspending.
    auto newReadyQueueHead = ReadyQueueItem::get(
        ReadyQueueStatus::Empty,
        completed
    );
    auto readyQueueHead = fragment->readyQueue.exchange(
        newReadyQueueHead, std::memory_order_acquire);
    while (true) {
      // Put the completed task at the beginning of the message queue.
      waitingTask->getNextWaitingTask() = readyQueueHead.getTask();
      if (fragment->readyQueue.compare_exchange_weak(
          readyQueueHead, newReadyQueueHead, std::memory_order_release,
          std::memory_order_acquire)) {
        // Escalate the priority of this task based on the priority
        // of the waiting task.
        swift_task_escalate(this, waitingTask->Flags.getPriority());
        return;
      }
    }
  } else {
    // ==== run waiters --------------------------------------------------------
    //
    // Some tasks were waiting, schedule them all on the executor.
    while (waitingTask) {
      // Find the next waiting task.
      auto nextWaitingTask = waitingTask->getNextWaitingTask();

      // Run the task.
      auto completedFragment = completed->futureFragment();
      swift::runTaskWithFutureResult(waitingTask, executor, completedFragment, hadErrorResult);

      // Move to the next task.
      waitingTask = nextWaitingTask;
    }
    return;
  }
}


//void swift::swift_task_channel_offer(
//    AsyncTask *channelTask,
//    AsyncTask *completedTask) {
//  assert(channelTask->isChannel());
//  assert(completedTask->isFuture());
//}

// ==== pollChannel ------------------------------------------------------------

AsyncTask::ChannelFragment::ReadyQueueStatus
AsyncTask::channelPoll(AsyncTask *waitingTask) {
  using Status = ChannelFragment::Status;
  using WaitQueueItem = ChannelFragment::WaitQueueItem;
  using ReadyQueueItem = ChannelFragment::ReadyQueueItem;
  using ReadyQueueStatus = ChannelFragment::ReadyQueueStatus;

  assert(isChannel());
  auto fragment = channelFragment();

  auto readyQueueHead = fragment->readyQueue.load(std::memory_order_acquire);
  auto readyQueueHeadStatus = readyQueueHead.getStatus();
  auto shouldWait = false;
  switch (readyQueueHeadStatus) {
    case ReadyQueueStatus::Empty:
      // empty and no tasks in flight, bail out!
      return readyQueueHeadStatus;
    case ReadyQueueStatus::Pending:
      shouldWait = true;
      break;
    case ReadyQueueStatus::Success:
    case ReadyQueueStatus::Error:
      break;
  }

  if (shouldWait) {
    // ==== Enqueue task in the wait queue -------------------------------------
    assert(false && "IMPLEMENT ADDING TO WAITERS IN AsyncTask::channelPoll");
    // return ...;
  }

  // ==== Ready tasks in queue, poll one ----------------------------------------
  assert(false && "IMPLEMENT getting task AsyncTask::channelPoll");

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
//    auto waitQueueHead = fragment->waitQueue.load(std::memory_order_acquire);
//    waitingTask->getNextWaitingTask() = waitQueueHead.getTask();
//    auto newWaitHead = WaitQueueItem::get(Status::Pending, waitingTask);
//    if (fragment->waitQueue.compare_exchange_weak(waitQueueHead, newWaitHead,
//            /*success*/ std::memory_order_release,
//            /*failure*/ std::memory_order_acquire)) {
//      // Escalate the priority of this task based on the priority
//      // of the waiting task.
//      swift_task_escalate(this, waitingTask->Flags.getPriority());
//      return ChannelFragment::ReadyQueueStatus::Empty; // FIXME: wrong
//    }
//  }
}

//void AsyncTask::channelOffer(AsyncContext *context, ExecutorRef executor) {
//  using Status = ChannelFragment::Status;
//  using ReadyQueueItem = ChannelFragment::ReadyQueueItem;
//  using WaitQueueItem = ChannelFragment::WaitQueueItem;
//
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
//    hadErrorResult ? Status::Error : Status::Success,
//    nullptr
//  );
//  auto messageHead = fragment->waitQueue.exchange(newWaitHead, std::memory_order_acquire);
//  assert(messageHead.getStatus() == Status::Executing);
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

//AsyncTaskAndContext swift::swift_task_create_future(
//    JobFlags flags, AsyncTask *parent, const Metadata *futureResultType,
//    const AsyncFunctionPointer<void()> *function) {
//  return swift_task_create_future_f(
//      flags, parent, futureResultType, function->Function.get(),
//      function->ExpectedContextSize);
//}

void swift::swift_task_channel_poll(
    AsyncTask *waitingTask, ExecutorRef executor,
    AsyncContext *rawContext) {
  assert(false && "IMPLEMENT swift_task_channel_poll");
//  // Suspend the waiting task.
//  waitingTask->ResumeTask = rawContext->ResumeParent;
//  waitingTask->ResumeContext = rawContext;
//
//  // Wait on the future.
//  auto context = static_cast<TaskFutureWaitAsyncContext *>(rawContext);
//  auto task = context->task;
//  assert(task->isFuture());
//  assert(task->isChannel());
//  switch (task->pollChannel(waitingTask)) {
//  case ChannelFragment::ReadyQueueStatus::Empty:
//    // The waiting task has been queued on the future.
//    return;
//
//  case ChannelFragment::Status::Success:
//    // Run the task with a successful result.
//    // FIXME: Want to guarantee a tail call here
//    runTaskWithFutureResult(
//        waitingTask, executor, task->ChannelFragment(),
//        /*hadErrorResult=*/false);
//    return;
//
// case ChannelFragment::Status::Error:
//    // Run the task with an error result.
//    // FIXME: Want to guarantee a tail call here
//    runTaskWithFutureResult(
//        waitingTask, executor, task->ChannelFragment(),
//        /*hadErrorResult=*/true);
//    return;
//  }
}
