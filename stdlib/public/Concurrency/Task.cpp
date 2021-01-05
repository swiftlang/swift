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
#include "swift/Runtime/Mutex.h"
#include "swift/Runtime/HeapObject.h"
#include "TaskPrivate.h"
#include "AsyncCall.h"

using namespace swift;
using FutureFragment = AsyncTask::FutureFragment;
using GroupFragment = AsyncTask::GroupFragment;

void FutureFragment::destroy() {
  auto queueHead = waitQueue.load(std::memory_order_acquire);
  switch (queueHead.getStatus()) {
  case Status::Executing:
    assert(false && "destroying a task that never completed");

  case Status::Success:
    resultType->vw_destroy(getStoragePtr());
    break;

  case Status::Error:
    swift_unknownObjectRelease(reinterpret_cast<OpaqueValue *>(getError()));
    break;
  }
}

FutureFragment::Status AsyncTask::waitFuture(AsyncTask *waitingTask) {
  using Status = FutureFragment::Status;
  using WaitQueueItem = FutureFragment::WaitQueueItem;

  assert(isFuture());
  auto fragment = futureFragment();

  auto queueHead = fragment->waitQueue.load(std::memory_order_acquire);
  while (true) {
    switch (queueHead.getStatus()) {
    case Status::Error:
    case Status::Success:
      // The task is done; we don't need to wait.
      return queueHead.getStatus();

    case Status::Executing:
      // Task is now complete. We'll need to add ourselves to the queue.
      break;
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
      swift_task_escalate(this, waitingTask->Flags.getPriority());
      return FutureFragment::Status::Executing;
    }
  }
}

void AsyncTask::completeFuture(AsyncContext *context, ExecutorRef executor) {
  using Status = FutureFragment::Status;
  using WaitQueueItem = FutureFragment::WaitQueueItem;

  assert(isFuture());
  auto fragment = futureFragment();

  // If an error was thrown, save it in the future fragment.
  auto futureContext = static_cast<FutureAsyncContext *>(context);
  bool hadErrorResult = false;
  if (auto errorObject = futureContext->errorResult) {
    fragment->getError() = errorObject;
    hadErrorResult = true;
  }

  // Update the status to signal completion.
  auto newQueueHead = WaitQueueItem::get(
    hadErrorResult ? Status::Error : Status::Success,
    nullptr
  );
  auto queueHead = fragment->waitQueue.exchange(
      newQueueHead, std::memory_order_acquire);
  assert(queueHead.getStatus() == Status::Executing);

  // If this is task group child, notify the parent group about the completion.
  if (isTaskGroupChild()) {
    // then we must offer into the parent group that we completed,
    // so it may `next()` poll completed child tasks in completion order.
    auto parent = childFragment()->getParent();
    assert(parent->isTaskGroup());
    parent->groupOffer(this, context, executor);
  }

  // Schedule every waiting task on the executor.
  auto waitingTask = queueHead.getTask();
  while (waitingTask) {
    // Find the next waiting task.
    auto nextWaitingTask = waitingTask->getNextWaitingTask();

    // Run the task.
    runTaskWithFutureResult(waitingTask, executor, fragment, hadErrorResult);

    // Move to the next task.
    waitingTask = nextWaitingTask;
  }
}

SWIFT_CC(swift)
static void destroyTask(SWIFT_CONTEXT HeapObject *obj) {
  auto task = static_cast<AsyncTask*>(obj);

  // For a group, destroy the queues and results.
  if (task->isTaskGroup()) {
    task->groupFragment()->destroy();
  }

  // For a future, destroy the result.
  if (task->isFuture()) {
    task->futureFragment()->destroy();
  }

  // The task execution itself should always hold a reference to it, so
  // if we get here, we know the task has finished running, which means
  // swift_task_complete should have been run, which will have torn down
  // the task-local allocator.  There's actually nothing else to clean up
  // here.

  free(task);
}

/// Heap metadata for an asynchronous task.
static FullMetadata<HeapMetadata> taskHeapMetadata = {
  {
    {
      &destroyTask
    },
    {
      /*value witness table*/ nullptr
    }
  },
  {
    MetadataKind::Task
  }
};

/// The function that we put in the context of a simple task
/// to handle the final return.
SWIFT_CC(swift)
static void completeTask(AsyncTask *task, ExecutorRef executor,
                         AsyncContext *context) {
  // Tear down the task-local allocator immediately;
  // there's no need to wait for the object to be destroyed.
  _swift_task_alloc_destroy(task);

  // Complete the future.
  if (task->isFuture()) {
    task->completeFuture(context, executor);
  }

  // TODO: set something in the status?
  // TODO: notify the parent somehow?
  // TODO: remove this task from the child-task chain?

  // Release the task, balancing the retain that a running task has on itself.
  // If it was a group child task, it will remain until the group returns it.
  swift_release(task);
}

AsyncTaskAndContext
swift::swift_task_create(JobFlags flags, AsyncTask *parent,
        const ThinNullaryAsyncSignature::FunctionPointer *function) {
  return swift_task_create_f(flags, parent, function->Function.get(),
                             function->ExpectedContextSize);
}

AsyncTaskAndContext
swift::swift_task_create_f(JobFlags flags, AsyncTask *parent,
                ThinNullaryAsyncSignature::FunctionType *function,
                           size_t initialContextSize) {
  return swift_task_create_future_f(
      flags, parent, nullptr, function, initialContextSize);
}

AsyncTaskAndContext swift::swift_task_create_future(
    JobFlags flags, AsyncTask *parent, const Metadata *futureResultType,
    const FutureAsyncSignature::FunctionPointer *function) {
  return swift_task_create_future_f(
      flags, parent, futureResultType, function->Function.get(),
      function->ExpectedContextSize);
}

AsyncTaskAndContext swift::swift_task_create_future_f(
    JobFlags flags, AsyncTask *parent, const Metadata *futureResultType,
    FutureAsyncSignature::FunctionType *function, size_t initialContextSize) {
  assert((futureResultType != nullptr) == flags.task_isFuture());
  assert(!flags.task_isFuture() ||
         initialContextSize >= sizeof(FutureAsyncContext));
  assert((parent != nullptr) == flags.task_isChildTask());

  // Figure out the size of the header.
  size_t headerSize = sizeof(AsyncTask);

  if (parent) {
    headerSize += sizeof(AsyncTask::ChildFragment);
  }

  if (flags.task_isTaskGroup()) {
    headerSize += sizeof(AsyncTask::GroupFragment);
  }

  if (futureResultType) {
    headerSize += FutureFragment::fragmentSize(futureResultType);
  }

  headerSize = llvm::alignTo(headerSize, llvm::Align(alignof(AsyncContext)));

  // Allocate the initial context together with the job.
  // This means that we never get rid of this allocation.
  size_t amountToAllocate = headerSize + initialContextSize;

  assert(amountToAllocate % MaximumAlignment == 0);

  void *allocation = malloc(amountToAllocate);

  AsyncContext *initialContext =
    reinterpret_cast<AsyncContext*>(
      reinterpret_cast<char*>(allocation) + headerSize);

  // Initialize the task so that resuming it will run the given
  // function on the initial context.
  AsyncTask *task =
    new(allocation) AsyncTask(&taskHeapMetadata, flags,
                              function, initialContext);

  // Initialize the child fragment if applicable.
  // TODO: propagate information from the parent?
  if (parent) {
    auto childFragment = task->childFragment();
    new (childFragment) AsyncTask::ChildFragment(parent);
  }

  // Initialize the channel fragment if applicable.
  if (flags.task_isTaskGroup()) {
    auto groupFragment = task->groupFragment();
    new (groupFragment) GroupFragment();
  }

  // Initialize the future fragment if applicable.
  if (futureResultType) {
    assert(task->isFuture());
    auto futureFragment = task->futureFragment();
    new (futureFragment) FutureFragment(futureResultType);

    // Set up the context for the future so there is no error, and a successful
    // result will be written into the future fragment's storage.
    auto futureContext = static_cast<FutureAsyncContext *>(initialContext);
    futureContext->errorResult = nullptr;
    futureContext->indirectResult = futureFragment->getStoragePtr();
  }

  // Configure the initial context.
  //
  // FIXME: if we store a null pointer here using the standard ABI for
  // signed null pointers, then we'll have to authenticate context pointers
  // as if they might be null, even though the only time they ever might
  // be is the final hop.  Store a signed null instead.
  initialContext->Parent = nullptr;
  initialContext->ResumeParent = &completeTask;
  initialContext->ResumeParentExecutor = ExecutorRef::generic();
  initialContext->Flags = AsyncContextKind::Ordinary;
  initialContext->Flags.setShouldNotDeallocateInCallee(true);

  // Initialize the task-local allocator.
  // TODO: consider providing an initial pre-allocated first slab to the
  //       allocator.
  _swift_task_alloc_initialize(task);

  return {task, initialContext};
}

void swift::swift_task_future_wait(
    AsyncTask *waitingTask, ExecutorRef executor,
    AsyncContext *rawContext) {
  // Suspend the waiting task.
  waitingTask->ResumeTask = rawContext->ResumeParent;
  waitingTask->ResumeContext = rawContext;

  auto context = static_cast<TaskFutureWaitAsyncContext *>(rawContext);
  auto task = context->task;

  // Wait on the future.
  assert(task->isFuture());
  switch (task->waitFuture(waitingTask)) {
  case FutureFragment::Status::Executing:
    // The waiting task has been queued on the future.
    return;

  case FutureFragment::Status::Success:
    // Run the task with a successful result.
    // FIXME: Want to guarantee a tail call here
    runTaskWithFutureResult(
        waitingTask, executor, task->futureFragment(),
        /*hadErrorResult=*/false);
    return;

 case FutureFragment::Status::Error:
    // Run the task with an error result.
    // FIXME: Want to guarantee a tail call here
    runTaskWithFutureResult(
        waitingTask, executor, task->futureFragment(),
        /*hadErrorResult=*/true);
    return;
  }
}

namespace {
/// The header of a function context (closure captures) of
/// a thick async function with a non-null context.
struct ThickAsyncFunctionContext: HeapObject {
  uint32_t ExpectedContextSize;
};


#if SWIFT_CONCURRENCY_COOPERATIVE_GLOBAL_EXECUTOR

class RunAndBlockSemaphore {
  bool Finished = false;
public:
  void wait() {
    donateThreadToGlobalExecutorUntil([](void *context) {
      return *reinterpret_cast<bool*>(context);
    }, &Finished);

    assert(Finished && "ran out of tasks before we were signalled");
  }

  void signal() {
    Finished = true;
  }
};

#else

class RunAndBlockSemaphore {
  ConditionVariable Queue;
  ConditionVariable::Mutex Lock;
  bool Finished = false;
public:
  /// Wait for a signal.
  void wait() {
    Lock.withLockOrWait(Queue, [&] {
      return Finished;
    });
  }

  void signal() {
    Lock.withLockThenNotifyAll(Queue, [&]{
      Finished = true;
    });
  }
};

#endif

using RunAndBlockSignature =
  AsyncSignature<void(HeapObject*), /*throws*/ false>;
struct RunAndBlockContext: AsyncContext {
  const void *Function;
  HeapObject *FunctionContext;
  RunAndBlockSemaphore *Semaphore;
};
using RunAndBlockCalleeContext =
  AsyncCalleeContext<RunAndBlockContext, RunAndBlockSignature>;

} // end anonymous namespace

/// Second half of the runAndBlock async function.
SWIFT_CC(swiftasync)
static void runAndBlock_finish(AsyncTask *task, ExecutorRef executor,
                               AsyncContext *_context) {
  auto calleeContext = static_cast<RunAndBlockCalleeContext*>(_context);
  auto context = popAsyncContext(task, calleeContext);

  context->Semaphore->signal();

  return context->ResumeParent(task, executor, context);
}

/// First half of the runAndBlock async function.
SWIFT_CC(swiftasync)
static void runAndBlock_start(AsyncTask *task, ExecutorRef executor,
                              AsyncContext *_context) {
  auto callerContext = static_cast<RunAndBlockContext*>(_context);

  size_t calleeContextSize;
  RunAndBlockSignature::FunctionType *function;

  // If the function context is non-null, then the function pointer is
  // an ordinary function pointer.
  auto functionContext = callerContext->FunctionContext;
  if (functionContext) {
    function = reinterpret_cast<RunAndBlockSignature::FunctionType*>(
                   const_cast<void*>(callerContext->Function));
    calleeContextSize =
      static_cast<ThickAsyncFunctionContext*>(functionContext)
        ->ExpectedContextSize;

  // Otherwise, the function pointer is an async function pointer.
  } else {
    auto fnPtr = reinterpret_cast<const RunAndBlockSignature::FunctionPointer*>(
                   callerContext->Function);
    function = fnPtr->Function;
    calleeContextSize = fnPtr->ExpectedContextSize;
  }

  auto calleeContext =
    pushAsyncContext<RunAndBlockSignature>(task, executor, callerContext,
                                           calleeContextSize,
                                           &runAndBlock_finish,
                                           functionContext);
  return function(task, executor, calleeContext);
}

// TODO: Remove this hack.
void swift::swift_task_runAndBlockThread(const void *function,
                                         HeapObject *functionContext) {
  RunAndBlockSemaphore semaphore;

  // Set up a task that runs the runAndBlock async function above.
  auto pair = swift_task_create_f(JobFlags(JobKind::Task,
                                           JobPriority::Default),
                                  /*parent*/ nullptr,
                                  &runAndBlock_start,
                                  sizeof(RunAndBlockContext));
  auto context = static_cast<RunAndBlockContext*>(pair.InitialContext);
  context->Function = function;
  context->FunctionContext = functionContext;
  context->Semaphore = &semaphore;

  // Enqueue the task.
  swift_task_enqueueGlobal(pair.Task);

  // Wait until the task completes.
  semaphore.wait();
}

size_t swift::swift_task_getJobFlags(AsyncTask *task) {
  return task->Flags.getOpaqueValue();
}

namespace {
  
/// Structure that gets filled in when a task is suspended by `withUnsafeContinuation`.
struct AsyncContinuationContext {
  // These fields are unnecessary for resuming a continuation.
  void *Unused1;
  void *Unused2;
  // Storage slot for the error result, if any.
  SwiftError *ErrorResult;
  // Pointer to where to store a normal result.
  OpaqueValue *NormalResult;
  
  // Executor on which to resume execution.
  ExecutorRef ResumeExecutor;
};

static void resumeTaskAfterContinuation(AsyncTask *task,
                                        AsyncContinuationContext *context) {
  swift_task_enqueue(task, context->ResumeExecutor);
}

}

SWIFT_CC(swift)
void swift::swift_continuation_resume(/* +1 */ OpaqueValue *result,
                                      void *continuation,
                                      const Metadata *resumeType) {
  auto task = reinterpret_cast<AsyncTask*>(continuation);
  auto context = reinterpret_cast<AsyncContinuationContext*>(task->ResumeContext);
  resumeType->vw_initializeWithTake(context->NormalResult, result);
  
  resumeTaskAfterContinuation(task, context);
}

SWIFT_CC(swift)
void swift::swift_continuation_throwingResume(/* +1 */ OpaqueValue *result,
                                              void *continuation,
                                              const Metadata *resumeType) {
  return swift_continuation_resume(result, continuation, resumeType);
}


SWIFT_CC(swift)
void swift::swift_continuation_throwingResumeWithError(/* +1 */ SwiftError *error,
                                                       void *continuation,
                                                       const Metadata *resumeType) {
  auto task = reinterpret_cast<AsyncTask*>(continuation);
  auto context = reinterpret_cast<AsyncContinuationContext*>(task->ResumeContext);
  context->ErrorResult = error;
  
  resumeTaskAfterContinuation(task, context);
}

bool swift::swift_task_isCancelled(AsyncTask *task) {
  return task->isCancelled();
}
