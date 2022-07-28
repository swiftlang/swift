#include "Concurrency/Task.h"

#include "Concurrency/TaskPrivate.h"
#include "Concurrency/Error.h"
#include "Overrides.h"

using namespace swift;
using FutureFragment = AsyncTask::FutureFragment;
using TaskGroup = swift::TaskGroup;

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
  auto resumeWithError =
      reinterpret_cast<AsyncVoidClosureEntryPoint *>(context->ResumeParent);
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

  auto resumeFn = reinterpret_cast<TaskContinuationFunction *>(resumeFunction);

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
