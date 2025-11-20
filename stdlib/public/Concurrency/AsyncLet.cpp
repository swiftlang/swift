//===--- AsyncLet.h - async let object management -00------------*- C++ -*-===//
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

#include "../CompatibilityOverride/CompatibilityOverride.h"
#include "Debug.h"
#include "TaskPrivate.h"

#include "swift/ABI/AsyncLet.h"
#include "swift/ABI/Metadata.h"
#include "swift/ABI/Task.h"
#include "swift/ABI/TaskOptions.h"
#include "swift/Basic/Casting.h"
#include "swift/Runtime/Heap.h"
#include "swift/Runtime/HeapObject.h"
#include "swift/Threading/Mutex.h"
#include "llvm/ADT/PointerIntPair.h"

#if !defined(_WIN32) && !defined(__wasi__) && __has_include(<dlfcn.h>)
#include <dlfcn.h>
#endif

#include <new>

using namespace swift;

namespace {
class alignas(Alignment_AsyncLet) AsyncLetImpl: public ChildTaskStatusRecord {
public:
  // This is where we could define a Status or other types important for async-let

private:
  // Flags stored in the low bits of the task pointer.
  enum {
    /// Whether the result buffer (the `void*` passed to all the various
    /// runtime functions) has already been initialized. This implies that
    /// the async let task has completed (and without throwing).
    ///
    /// Note that the result buffer is currently a *copy* of the actual
    /// return value: we currently set up the task so that it evaluates
    /// into a future, then wait on that future the same way we would
    /// wait for an unstructured task. This is wasteful, since async let
    /// tasks always have a single waiter; there's no good reason not to
    /// evaluate the task directly into the result buffer and avoid the
    /// copy.
    HasResult = 1 << 0,

    /// Whether the task was allocated with the parent task's stack
    /// allocator. We normally try to use the allocation given to us by
    /// the compiler, but if that's not big enough, the runtime must
    /// allocate more memory.
    DidAllocateFromParentTask = 1 << 1,
  };

  /// The task that was kicked off to initialize this `async let`,
  /// and flags.
  llvm::PointerIntPair<AsyncTask *, 2, unsigned> taskAndFlags;

  /// Reserved space for a future_wait context frame, used during suspensions
  /// on the child task future.
  std::aligned_storage<sizeof(TaskFutureWaitAsyncContext),
                       alignof(TaskFutureWaitAsyncContext)>::type futureWaitContextStorage;

  friend class ::swift::AsyncTask;

public:
  explicit AsyncLetImpl(AsyncTask* task)
      : ChildTaskStatusRecord(task),
        taskAndFlags(task, 0) {
    assert(task->hasChildFragment() && "async let task must be a child task.");
  }

  /// Returns the task record representing this async let task.
  /// The record is stored in the parent task, and should be removed when the
  /// async let goes out of scope.
  ChildTaskStatusRecord *getTaskRecord() {
    return reinterpret_cast<ChildTaskStatusRecord *>(this);
  }

  AsyncTask *getTask() const {
    return taskAndFlags.getPointer();
  }

  bool hasResultInBuffer() const {
    return taskAndFlags.getInt() & HasResult;
  }

  void setHasResultInBuffer(bool value = true) {
    if (value)
      taskAndFlags.setInt(taskAndFlags.getInt() | HasResult);
    else
      taskAndFlags.setInt(taskAndFlags.getInt() & ~HasResult);
  }

  bool didAllocateFromParentTask() const {
    return taskAndFlags.getInt() & DidAllocateFromParentTask;
  }

  void setDidAllocateFromParentTask(bool value = true) {
    if (value)
      taskAndFlags.setInt(taskAndFlags.getInt() | DidAllocateFromParentTask);
    else
      taskAndFlags.setInt(taskAndFlags.getInt() & ~DidAllocateFromParentTask);
  }

  // The compiler preallocates a large fixed space for the `async let`, with the
  // intent that most of it be used for the child task context. The next two
  // methods return the address and size of that space.

  /// Return a pointer to the unused space within the async let block.
  void *getPreallocatedSpace() {
    return (void*)(this + 1);
  }

  /// Return the size of the unused space within the async let block.
  static constexpr size_t getSizeOfPreallocatedSpace() {
    return sizeof(AsyncLet) - sizeof(AsyncLetImpl);
  }

  TaskFutureWaitAsyncContext *getFutureContext() {
    return reinterpret_cast<TaskFutureWaitAsyncContext*>(&futureWaitContextStorage);
  }
}; // end AsyncLetImpl

} // end anonymous namespace


/******************************************************************************/
/************************* ASYNC LET IMPLEMENTATION ***************************/
/******************************************************************************/

static_assert(sizeof(AsyncLetImpl) <= sizeof(AsyncLet) &&
              alignof(AsyncLetImpl) <= alignof(AsyncLet),
              "AsyncLetImpl doesn't fit in AsyncLet");

static AsyncLetImpl *asImpl(AsyncLet *alet) {
  return reinterpret_cast<AsyncLetImpl*>(alet);
}

static AsyncLetImpl *asImpl(const AsyncLet *alet) {
  return reinterpret_cast<AsyncLetImpl*>(
      const_cast<AsyncLet*>(alet));
}

void swift::asyncLet_addImpl(AsyncTask *task, AsyncLet *asyncLet,
                             bool didAllocateInParentTask) {
  AsyncLetImpl *impl = ::new (asyncLet) AsyncLetImpl(task);
  impl->setDidAllocateFromParentTask(didAllocateInParentTask);

  auto record = impl->getTaskRecord();
  assert(impl == record && "the async-let IS the task record");

  // ok, now that the async let task actually is initialized: attach it to the
  // current task
  bool addedRecord = addStatusRecordToSelf(record,
      [&](ActiveTaskStatus parentStatus, ActiveTaskStatus& newStatus) {
    updateNewChildWithParentAndGroupState(task, parentStatus, NULL);
    return true;
  });
  (void)addedRecord;
  assert(addedRecord);
}

// =============================================================================
// ==== begin ------------------------------------------------------------------

SWIFT_CC(swift)
void swift::swift_asyncLet_begin(AsyncLet *alet,
                                 TaskOptionRecord *options,
                                 const Metadata *futureResultType,
                                 void *closureEntryPoint,
                                 HeapObject *closureContext,
                                 void *resultBuffer) {
  SWIFT_TASK_DEBUG_LOG("creating async let buffer of type %s at %p",
                       swift_getTypeName(futureResultType, true).data,
                       resultBuffer);

  auto flags = TaskCreateFlags();
#if SWIFT_CONCURRENCY_TASK_TO_THREAD_MODEL
  // In the task to thread model, we don't want tasks to start running on
  // separate threads - they will run in the context of the parent
  flags.setEnqueueJob(false);
#else
  flags.setEnqueueJob(true);
#endif


  AsyncLetWithBufferTaskOptionRecord asyncLetOptionRecord(alet, resultBuffer);
  asyncLetOptionRecord.Parent = options;

  swift_task_create(
      flags.getOpaqueValue(),
      &asyncLetOptionRecord,
      futureResultType,
      closureEntryPoint, closureContext);
}

// =============================================================================
// ==== get -------------------------------------------------------------------

SWIFT_CC(swiftasync)
static void swift_asyncLet_getImpl(SWIFT_ASYNC_CONTEXT AsyncContext *callerContext,
                                   AsyncLet *alet,
                                   void *resultBuffer,
                                   TaskContinuationFunction *resumeFunction,
                                   AsyncContext *callContext) {
  // Don't need to do anything if the result buffer is already populated.
  if (asImpl(alet)->hasResultInBuffer()) {
    return resumeFunction(callerContext);
  }

  // Mark the async let as having its result populated.
  // The only task that can ask this of the async let is the same parent task
  // that's currently executing, so we can set it now and tail-call future_wait,
  // since by the time we can call back it will be populated.
  asImpl(alet)->setHasResultInBuffer();
  swift_task_future_wait(reinterpret_cast<OpaqueValue*>(resultBuffer),
                         callerContext, alet->getTask(),
                         resumeFunction, callContext);
}

struct AsyncLetContinuationContext: AsyncContext {
  AsyncLet *alet;
  OpaqueValue *resultBuffer;
};

static_assert(sizeof(AsyncLetContinuationContext) <= sizeof(TaskFutureWaitAsyncContext),
              "compiler provides the same amount of context space to each");

SWIFT_CC(swiftasync)
static void _asyncLet_get_throwing_continuation(
        SWIFT_ASYNC_CONTEXT AsyncContext *callContext,
        SWIFT_CONTEXT void *error) {
  auto continuationContext = static_cast<AsyncLetContinuationContext*>(callContext);
  auto alet = continuationContext->alet;

  // If the future completed successfully, its result is now in the async let
  // buffer.
  if (!error) {
    asImpl(alet)->setHasResultInBuffer();
  }

  // Continue the caller's execution.
  auto throwingResume =
      function_cast<ThrowingTaskFutureWaitContinuationFunction*>(callContext->ResumeParent);
  return throwingResume(callContext->Parent, error);
}

SWIFT_CC(swiftasync)
static void swift_asyncLet_get_throwingImpl(
                    SWIFT_ASYNC_CONTEXT AsyncContext *callerContext,
                    AsyncLet *alet,
                    void *resultBuffer,
                    ThrowingTaskFutureWaitContinuationFunction *resumeFunction,
                    AsyncContext *callContext) {
  // Don't need to do anything if the result buffer is already populated.
  if (asImpl(alet)->hasResultInBuffer()) {
    return resumeFunction(callerContext, nullptr);
  }

  auto aletContext = static_cast<AsyncLetContinuationContext*>(callContext);
  aletContext->ResumeParent =
      function_cast<TaskContinuationFunction*>(resumeFunction);
  aletContext->Parent = callerContext;
  aletContext->alet = alet;
  auto futureContext = asImpl(alet)->getFutureContext();

  // Unlike the non-throwing variant, whether we end up with a result depends
  // on the success of the task. If we raise an error, then the result buffer
  // will not be populated. Save the async let binding so we can fetch it
  // after completion.
  return swift_task_future_wait_throwing(
                         reinterpret_cast<OpaqueValue*>(resultBuffer),
                         aletContext, alet->getTask(),
                         _asyncLet_get_throwing_continuation,
                         futureContext);
}

// =============================================================================
// ==== finish -----------------------------------------------------------------

SWIFT_CC(swiftasync)
// FIXME: noinline to work around an LLVM bug where the outliner breaks
// musttail.
SWIFT_NOINLINE
static void asyncLet_finish_after_task_completion(SWIFT_ASYNC_CONTEXT AsyncContext *callerContext,
                                                  AsyncLet *alet,
                                                  TaskContinuationFunction *resumeFunction,
                                                  AsyncContext *callContext,
                                                  SWIFT_CONTEXT void *error) {
  auto parentTask = swift_task_getCurrent();
  assert(parentTask && "async-let must have a parent task");

  auto task = alet->getTask();

  // Remove the child record from the parent task
  auto record = asImpl(alet)->getTaskRecord();
  removeStatusRecordFromSelf(record);

  // Destroy the task. Note that this destroys the copy of the result
  // (error or normal) in the task's future fragment.
  SWIFT_TASK_DEBUG_LOG("async let end of task %p, parent: %p", task, parentTask);
  task->~AsyncTask();

  // Deallocate the memory for the child task, if it was allocated with
  // the parent's stack allocator.
  if (alet->didAllocateFromParentTask()) {
    _swift_task_dealloc_specific(parentTask, (void*) task);
  }

  // Call the continuation function.
  return function_cast<ThrowingTaskFutureWaitContinuationFunction*>(resumeFunction)
    (callerContext, error);
}


SWIFT_CC(swiftasync)
static void _asyncLet_finish_continuation(
                    SWIFT_ASYNC_CONTEXT AsyncContext *callContext,
                    SWIFT_CONTEXT void *error) {
  // Retrieve the async let pointer from the context.
  auto continuationContext
    = reinterpret_cast<AsyncLetContinuationContext*>(callContext);
  auto alet = continuationContext->alet;
  auto resultBuffer = continuationContext->resultBuffer;

  // We waited for the task using swift_task_future_wait_throwing,
  // which means we've been passed a copy of the result in the future
  // fragment (either an error, if the task threw, or the result
  // in the result buffer). Destroy that copy now.

  // Destroy the error, or the result that was stored to the buffer.
  if (error) {
    #if SWIFT_CONCURRENCY_EMBEDDED
    swift_unreachable("untyped error used in embedded Swift");
    #else
    swift_errorRelease((SwiftError*)error);
    #endif
  } else {
    alet->getTask()->futureFragment()->getResultType().vw_destroy(resultBuffer);
  }

  // Clean up the async let now that the task has finished.
  return asyncLet_finish_after_task_completion(callContext->Parent,
                                               alet,
                                               callContext->ResumeParent,
                                               callContext,
                                               nullptr);
}

SWIFT_CC(swiftasync)
static void swift_asyncLet_finishImpl(SWIFT_ASYNC_CONTEXT AsyncContext *callerContext,
                                   AsyncLet *alet,
                                   void *resultBuffer,
                                   TaskContinuationFunction *resumeFunction,
                                   AsyncContext *callContext) {
  auto task = alet->getTask();

  // If the result buffer is already populated, then we just need to destroy
  // the value in it and then clean up the task.
  if (asImpl(alet)->hasResultInBuffer()) {
    task->futureFragment()->getResultType().vw_destroy(
                                  reinterpret_cast<OpaqueValue*>(resultBuffer));
    return asyncLet_finish_after_task_completion(callerContext,
                                                 alet,
                                                 resumeFunction,
                                                 callContext,
                                                 nullptr);
  }
  // Otherwise, cancel the task and let it finish first.
  swift_task_cancel(task);

  // Save the async let pointer in the context so we can clean it up once the
  // future completes.
  auto aletContext = static_cast<AsyncLetContinuationContext*>(callContext);
  aletContext->Parent = callerContext;
  aletContext->ResumeParent = resumeFunction;
  aletContext->alet = alet;
  aletContext->resultBuffer = reinterpret_cast<OpaqueValue*>(resultBuffer);
  auto futureContext = asImpl(alet)->getFutureContext();

  // TODO: It would be nice if we could await the future without having to
  // provide a buffer to store the value to, since we're going to dispose of
  // it anyway.
  return swift_task_future_wait_throwing(
                         reinterpret_cast<OpaqueValue*>(resultBuffer),
                         callContext, alet->getTask(),
                         _asyncLet_finish_continuation,
                         futureContext);
}

// =============================================================================
// ==== consume ----------------------------------------------------------------

SWIFT_CC(swiftasync)
static void _asyncLet_consume_continuation(
                                SWIFT_ASYNC_CONTEXT AsyncContext *callContext) {
  // Retrieve the async let pointer from the context.
  auto continuationContext
    = reinterpret_cast<AsyncLetContinuationContext*>(callContext);
  auto alet = continuationContext->alet;

  // Clean up the async let now that the task has finished.
  return asyncLet_finish_after_task_completion(callContext->Parent, alet,
                                               callContext->ResumeParent,
                                               callContext,
                                               nullptr);
}

SWIFT_CC(swiftasync)
static void swift_asyncLet_consumeImpl(SWIFT_ASYNC_CONTEXT AsyncContext *callerContext,
                                   AsyncLet *alet,
                                   void *resultBuffer,
                                   TaskContinuationFunction *resumeFunction,
                                   AsyncContext *callContext) {
  // If the result buffer is already populated, then we just need to clean up
  // the task.
  if (asImpl(alet)->hasResultInBuffer()) {
    return asyncLet_finish_after_task_completion(callerContext,
                                                 alet,
                                                 resumeFunction,
                                                 callContext,
                                                 nullptr);
  }

  // Save the async let pointer in the context so we can clean it up once the
  // future completes.
  auto aletContext = static_cast<AsyncLetContinuationContext*>(callContext);
  aletContext->Parent = callerContext;
  aletContext->ResumeParent = resumeFunction;
  aletContext->alet = alet;
  auto futureContext = asImpl(alet)->getFutureContext();

  // Await completion of the task. We'll destroy the task afterward.
  return swift_task_future_wait(
                         reinterpret_cast<OpaqueValue*>(resultBuffer),
                         callContext, alet->getTask(),
                         _asyncLet_consume_continuation,
                         futureContext);
}

SWIFT_CC(swiftasync)
static void _asyncLet_consume_throwing_continuation(
        SWIFT_ASYNC_CONTEXT AsyncContext *callContext,
        SWIFT_CONTEXT void *error) {
  // Get the async let pointer so we can destroy the task.
  auto continuationContext = static_cast<AsyncLetContinuationContext*>(callContext);
  auto alet = continuationContext->alet;

  return asyncLet_finish_after_task_completion(callContext->Parent,
                                               alet,
                                               callContext->ResumeParent,
                                               callContext,
                                               error);
}

SWIFT_CC(swiftasync)
static void swift_asyncLet_consume_throwingImpl(
                    SWIFT_ASYNC_CONTEXT AsyncContext *callerContext,
                    AsyncLet *alet,
                    void *resultBuffer,
                    ThrowingTaskFutureWaitContinuationFunction *resumeFunction,
                    AsyncContext *callContext) {
  // If the result buffer is already populated, we just need to clean up the
  // task.
  if (asImpl(alet)->hasResultInBuffer()) {
    return asyncLet_finish_after_task_completion(callerContext,
                   alet,
                   function_cast<TaskContinuationFunction*>(resumeFunction),
                   callContext,
                   nullptr);
  }

  auto aletContext = static_cast<AsyncLetContinuationContext*>(callContext);
  aletContext->ResumeParent =
      function_cast<TaskContinuationFunction*>(resumeFunction);
  aletContext->Parent = callerContext;
  aletContext->alet = alet;
  auto futureContext = asImpl(alet)->getFutureContext();

  // Unlike the non-throwing variant, whether we end up with a result depends
  // on the success of the task. If we raise an error, then the result buffer
  // will not be populated. Save the async let binding so we can fetch it
  // after completion.
  return swift_task_future_wait_throwing(
                         reinterpret_cast<OpaqueValue*>(resultBuffer),
                         aletContext, alet->getTask(),
                         _asyncLet_consume_throwing_continuation,
                         futureContext);
}

// =============================================================================
// ==== AsyncLet Implementation ------------------------------------------------

AsyncTask* AsyncLet::getTask() const {
  return asImpl(this)->getTask();
}

void *AsyncLet::getPreallocatedSpace() {
  return asImpl(this)->getPreallocatedSpace();
}

size_t AsyncLet::getSizeOfPreallocatedSpace() {
  return AsyncLetImpl::getSizeOfPreallocatedSpace();
}

bool AsyncLet::didAllocateFromParentTask() {
  return asImpl(this)->didAllocateFromParentTask();
}

void AsyncLet::setDidAllocateFromParentTask(bool value) {
  return asImpl(this)->setDidAllocateFromParentTask(value);
}

// =============================================================================

#define OVERRIDE_ASYNC_LET COMPATIBILITY_OVERRIDE
#include "../CompatibilityOverride/CompatibilityOverrideIncludePath.h"
