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
  SWIFT_TASK_DEBUG_LOG("compat 56 task task %p", this);
  using Status = FutureFragment::Status;
  using WaitQueueItem = FutureFragment::WaitQueueItem;

  assert(isFuture());
  auto fragment = futureFragment();

  auto queueHead = fragment->waitQueue.load(std::memory_order_acquire);
  bool contextInitialized = false;
  auto escalatedPriority = JobPriority::Unspecified;
  while (true) {
    switch (queueHead.getStatus()) {
    case Status::Error:
    case Status::Success:
      SWIFT_TASK_DEBUG_LOG("task(%d) %p waiting on task(%d) %p, completed immediately",
                           waitingTask.getJobId(), waitingTask,
                           this, this.getJobId());
      _swift_tsan_acquire(static_cast<Job *>(this));
      if (contextInitialized) waitingTask->flagAsRunning();
      // The task is done; we don't need to wait.
      return queueHead.getStatus();

    case Status::Executing:
      SWIFT_TASK_DEBUG_LOG("task(%d) %p waiting on task(%d) %p, going to sleep",
                           waitingTask, waitingTask.getJobId(),
                           this, this.getJobId());
      _swift_tsan_release(static_cast<Job *>(waitingTask));
      // Task is not complete. We'll need to add ourselves to the queue.
      break;
    }

    if (!contextInitialized) {
      contextInitialized = true;
      auto context =
          reinterpret_cast<TaskFutureWaitAsyncContext *>(waitingTaskContext);
      context->errorResult = nullptr;
      context->successResultPointer = result;
      context->ResumeParent = resumeFn;
      context->Parent = callerContext;
      waitingTask->flagAsSuspended();
    }

    // Escalate the blocking task to the priority of the waiting task.
    // FIXME: Also record that the waiting task is now waiting on the
    // blocking task so that escalators of the waiting task can propagate
    // the escalation to the blocking task.
    //
    // Recording this dependency is tricky because we need escalators
    // to be able to escalate without worrying about the blocking task
    // concurrently finishing, resuming the escalated task, and being
    // invalidated.  So we're not doing that yet.  In the meantime, we
    // do the best-effort alternative of escalating the blocking task
    // as a one-time deal to the current priority of the waiting task.
    // If the waiting task is escalated after this point, the priority
    // will not be escalated, but that's inevitable in the absence of
    // propagation during escalation.
    //
    // We have to do the escalation before we successfully enqueue the
    // waiting task on the blocking task's wait queue, because as soon as
    // we do, this thread is no longer blocking the resumption of the
    // waiting task, and so both the blocking task (which is retained
    // during the wait only from the waiting task's perspective) and the
    // waiting task (which can simply terminate) must be treat as
    // invalidated from this thread's perspective.
    //
    // When we do fix this bug to record the dependency, we will have to
    // do it before this escalation of the blocking task so that there
    // isn't a race where an escalation of the waiting task can fail
    // to propagate to the blocking task.  The correct priority to
    // escalate to is the priority we observe when we successfully record
    // the dependency; any later escalations will automatically propagate.
    //
    // If the blocking task finishes while we're doing this escalation,
    // the escalation will be innocuous.  The wasted effort is acceptable;
    // programmers should be encouraged to give tasks that will block
    // other tasks the correct priority to begin with.
    auto waitingStatus =
      waitingTask->_private().Status.load(std::memory_order_relaxed);
    if (waitingStatus.getStoredPriority() > escalatedPriority) {
      swift_task_escalateBackdeploy56(this, waitingStatus.getStoredPriority());
      escalatedPriority = waitingStatus.getStoredPriority();
    }

    // Put the waiting task at the beginning of the wait queue.
    waitingTask->getNextWaitingTask() = queueHead.getTask();
    auto newQueueHead = WaitQueueItem::get(Status::Executing, waitingTask);
    if (fragment->waitQueue.compare_exchange_weak(
            queueHead, newQueueHead,
            /*success*/ std::memory_order_release,
            /*failure*/ std::memory_order_acquire)) {
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

SWIFT_CC(swiftasync)
static void task_wait_throwing_resume_adapter(SWIFT_ASYNC_CONTEXT AsyncContext *_context) {

  auto context = static_cast<TaskFutureWaitAsyncContext *>(_context);
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wcast-function-type-mismatch"
  auto resumeWithError =
      reinterpret_cast<AsyncVoidClosureEntryPoint *>(context->ResumeParent);
#pragma clang diagnostic pop
  return resumeWithError(context->Parent, context->errorResult);
}

#ifdef __ARM_ARCH_7K__
__attribute__((noinline))
SWIFT_CC(swiftasync) static void workaround_function_swift_task_future_wait_throwingImpl(
    OpaqueValue *result, SWIFT_ASYNC_CONTEXT AsyncContext *callerContext,
    AsyncTask *task, ThrowingTaskFutureWaitContinuationFunction resumeFunction,
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

void SWIFT_CC(swiftasync) swift::swift56override_swift_task_future_wait_throwing(
                                            OpaqueValue *result,
                                            SWIFT_ASYNC_CONTEXT AsyncContext *callerContext,
                                            AsyncTask *task,
                                            ThrowingTaskFutureWaitContinuationFunction *resumeFunction,
                                            AsyncContext *callContext,
                                            TaskFutureWaitThrowing_t *original) {
  auto waitingTask = swift_task_getCurrent();
  // Suspend the waiting task.
  waitingTask->ResumeTask = task_wait_throwing_resume_adapter;
  waitingTask->ResumeContext = callContext;

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wcast-function-type-mismatch"
  auto resumeFn = reinterpret_cast<TaskContinuationFunction *>(resumeFunction);
#pragma clang diagnostic pop

  // Wait on the future.
  assert(task->isFuture());

  switch (task->waitFuture(waitingTask, callContext, resumeFn, callerContext,
                           result)) {
  case FutureFragment::Status::Executing:
    // The waiting task has been queued on the future.
#ifdef __ARM_ARCH_7K__
    return workaround_function_swift_task_future_wait_throwingImpl(
        result, callerContext, task, resumeFunction, callContext);
#else
    return;
#endif

  case FutureFragment::Status::Success: {
    auto future = task->futureFragment();
    future->getResultType()->vw_initializeWithCopy(result,
                                                   future->getStoragePtr());
    return resumeFunction(callerContext, nullptr /*error*/);
  }

  case FutureFragment::Status::Error: {
    // Run the task with an error result.
    auto future = task->futureFragment();
    auto error = future->getError();
    swift_errorRetain(error);
    return resumeFunction(callerContext, error);
  }
  }
}

//===--- swift_task_create_common -----------------------------------------===//

// NOTE: this function is currently only installed as an override on
// 64-bit targets.  The fix in it has been written to work correctly
// on any target, though, so if you need to use it for a more general
// fix, you should be able to just define and install it unconditionally.
#if __POINTER_WIDTH__ == 64

AsyncTaskAndContext SWIFT_CC(swift)
swift::swift56override_swift_task_create_common(
    size_t rawTaskCreateFlags,
    TaskOptionRecord *options,
    const Metadata *futureResultType,
    TaskContinuationFunction *function, void *closureContext,
    size_t initialContextSize,
    TaskCreateCommon_t *original) {

  // The <=5.6 versions of this function pointlessly initialize the
  // defunct Flags field in the initial context.  This initialization
  // is mostly harmless because the initial function has no expectations
  // about the non-header contents of the initial context on entry.
  // However, if the initial context doesn't include space for the Flags
  // field, and it ends up at the end of an allocation, this write can
  // go past the end of the allocation.
  //
  // The initial context is always at the end of the allocation for
  // Tasks that lack a preallocated buffer, i.e. any Task that is not
  // an async let.
  //
  // On 32-bit targets, the Flags field was at offset 8.  Since context
  // sizes are always rounded up to a multiple of MaximumAlignment,
  // initialContextSize is guaranteed to be >= 16, so the store to
  // Flags will always fall within it.  On 64-bit targets, however,
  // Flags was at offset 16.  We therefore need to ensure the initial
  // context is large enough for the unnecessary write to Flags.
  //
  // We could handle this in the compiler by ensuring that all
  // functions request at least 32 bytes of context, but that would
  // introduce a permanent overhead on thunks and other functions that
  // don't need any temporary scratch space.  We really only need to work
  // around this one store when creating tasks, and fortunately, that
  // always flows through this one function.  Since this hook receives
  // the initial function and context size directly instead of as an
  // async function pointer, it's painless for us to just change the
  // requested initial context size.
#if __POINTER_WIDTH__ == 64
  if (initialContextSize < 32) initialContextSize = 32;
#endif

  return original(rawTaskCreateFlags, options,
                  futureResultType, function, closureContext,
                  initialContextSize);
}

#endif
