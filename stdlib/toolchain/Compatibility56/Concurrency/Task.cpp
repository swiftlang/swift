#include "Concurrency/Task.h"

#include "Concurrency/TaskPrivate.h"
#include "Concurrency/Error.h"
#include "Overrides.h"

using namespace swift;
using FutureFragment = AsyncTask::FutureFragment;
using TaskGroup = swift::TaskGroup;

//===--- swift_task_future_wait -------------------------------------------===//

void SWIFT_CC(swiftasync) swift::swift56override_swift_task_future_wait(
                                            OpaqueValue *result,
                                            SWIFT_ASYNC_CONTEXT AsyncContext *callerContext,
                                            AsyncTask *task,
                                            TaskContinuationFunction *resumeFn,
                                            AsyncContext *callContext,
                                            TaskFutureWait_t *original) {
  original(result, callerContext, task, resumeFn, callContext);
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
