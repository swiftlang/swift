#include "Concurrency/Task.h"

#include "Concurrency/TaskPrivate.h"
#include "Concurrency/Error.h"
#include "Overrides.h"

using namespace swift;
using FutureFragment = AsyncTask::FutureFragment;
using TaskGroup = swift::TaskGroup;

Metadata swift::TaskAllocatorSlabMetadata;

FutureFragment::Status AsyncTask::waitFuture(AsyncTask *waitingTask,
                                             AsyncContext *waitingTaskContext,
                                             TaskContinuationFunction *resumeFn,
                                             AsyncContext *callerContext,
                                             OpaqueValue *result) {
  using Status = FutureFragment::Status;
  using WaitQueueItem = FutureFragment::WaitQueueItem;

  assert(isFuture());
  auto fragment = futureFragment();

  auto queueHead = fragment->waitQueue.load(std::memory_order_acquire);
  bool contextIntialized = false;
  while (true) {
    switch (queueHead.getStatus()) {
    case Status::Error:
    case Status::Success:
      SWIFT_TASK_DEBUG_LOG("task %p waiting on task %p, completed immediately",
                           waitingTask, this);
      _swift_tsan_acquire(static_cast<Job *>(this));
      if (contextIntialized) waitingTask->flagAsRunning();
      // The task is done; we don't need to wait.
      return queueHead.getStatus();

    case Status::Executing:
      SWIFT_TASK_DEBUG_LOG("task %p waiting on task %p, going to sleep",
                           waitingTask, this);
      _swift_tsan_release(static_cast<Job *>(waitingTask));
      // Task is not complete. We'll need to add ourselves to the queue.
      break;
    }

    if (!contextIntialized) {
      contextIntialized = true;
      auto context =
          reinterpret_cast<TaskFutureWaitAsyncContext *>(waitingTaskContext);
      context->errorResult = nullptr;
      context->successResultPointer = result;
      context->ResumeParent = resumeFn;
      context->Parent = callerContext;
      waitingTask->flagAsSuspended();
    }

    // Put the waiting task at the beginning of the wait queue.
    waitingTask->getNextWaitingTask() = queueHead.getTask();
    auto newQueueHead = WaitQueueItem::get(Status::Executing, waitingTask);
    if (fragment->waitQueue.compare_exchange_weak(
            queueHead, newQueueHead,
            /*success*/ std::memory_order_release,
            /*failure*/ std::memory_order_acquire)) {
      // Escalate the priority of this task based on the priority
      // of the waiting task.
      swift_task_escalateBackdeploy56(this, waitingTask->Flags.getPriority());
      _swift_task_clearCurrent();
      return FutureFragment::Status::Executing;
    }
  }
}

//===--- swift_task_future_wait -------------------------------------------===//

SWIFT_CC(swiftasync)
static void
task_future_wait_resume_adapter(SWIFT_ASYNC_CONTEXT AsyncContext *_context) {
  return _context->ResumeParent(_context->Parent);
}


#ifdef __ARM_ARCH_7K__
__attribute__((noinline))
SWIFT_CC(swiftasync) static void workaround_function_swift_task_future_waitImpl(
    OpaqueValue *result, SWIFT_ASYNC_CONTEXT AsyncContext *callerContext,
    AsyncTask *task, TaskContinuationFunction resumeFunction,
    AsyncContext *callContext) {
  // Make sure we don't eliminate calls to this function.
  asm volatile("" // Do nothing.
               :  // Output list, empty.
               : "r"(result), "r"(callerContext), "r"(task) // Input list.
               : // Clobber list, empty.
  );
  return;
}
#endif

void SWIFT_CC(swiftasync) swift::swift56override_swift_task_future_wait(
                                            OpaqueValue *result,
                                            SWIFT_ASYNC_CONTEXT AsyncContext *callerContext,
                                            AsyncTask *task,
                                            TaskContinuationFunction *resumeFn,
                                            AsyncContext *callContext,
                                            TaskFutureWait_t *original) {
  // Suspend the waiting task.
  auto waitingTask = swift_task_getCurrent();
  waitingTask->ResumeTask = task_future_wait_resume_adapter;
  waitingTask->ResumeContext = callContext;

  // Wait on the future.
  assert(task->isFuture());

  switch (task->waitFuture(waitingTask, callContext, resumeFn, callerContext,
                           result)) {
  case FutureFragment::Status::Executing:
    // The waiting task has been queued on the future.
#ifdef __ARM_ARCH_7K__
    return workaround_function_swift_task_future_waitImpl(
        result, callerContext, task, resumeFn, callContext);
#else
    return;
#endif

  case FutureFragment::Status::Success: {
    // Run the task with a successful result.
    auto future = task->futureFragment();
    future->getResultType()->vw_initializeWithCopy(result,
                                                   future->getStoragePtr());
    return resumeFn(callerContext);
  }

  case FutureFragment::Status::Error:
    swift_Concurrency_fatalError(0, "future reported an error, but wait cannot throw");
  }
}

//===--- swift_task_future_wait_throwing ----------------------------------===//

void SWIFT_CC(swiftasync) swift::swift56override_swift_task_future_wait_throwing(
                                            OpaqueValue *result,
                                            SWIFT_ASYNC_CONTEXT AsyncContext *callerContext,
                                            AsyncTask *task,
                                            ThrowingTaskFutureWaitContinuationFunction *resumeFunction,
                                            AsyncContext *callContext,
                                            TaskFutureWaitThrowing_t *original) {
  original(result, callerContext, task, resumeFunction, callContext);
}
