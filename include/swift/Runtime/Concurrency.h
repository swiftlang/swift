//===--- Concurrency.h - Runtime interface for concurrency ------*- C++ -*-===//
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
// The runtime interface for concurrency.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_RUNTIME_CONCURRENCY_H
#define SWIFT_RUNTIME_CONCURRENCY_H

#include "swift/ABI/TaskStatus.h"

namespace swift {
class DefaultActor;

struct SwiftError;

struct AsyncTaskAndContext {
  AsyncTask *Task;
  AsyncContext *InitialContext;
};

/// Create a task object with no future which will run the given
/// function.
///
/// The task is not yet scheduled.
///
/// If a parent task is provided, flags.task_hasChildFragment() must
/// be true, and this must be called synchronously with the parent.
/// The parent is responsible for creating a ChildTaskStatusRecord.
/// TODO: should we have a single runtime function for creating a task
/// and doing this child task status record management?
SWIFT_EXPORT_FROM(swift_Concurrency) SWIFT_CC(swift)
AsyncTaskAndContext swift_task_create(JobFlags flags,
                                      AsyncTask *parent,
                const ThinNullaryAsyncSignature::FunctionPointer *function);

/// Create a task object with no future which will run the given
/// function.
SWIFT_EXPORT_FROM(swift_Concurrency) SWIFT_CC(swift)
AsyncTaskAndContext swift_task_create_f(JobFlags flags,
                                        AsyncTask *parent,
                             ThinNullaryAsyncSignature::FunctionType *function,
                                        size_t initialContextSize);

/// Caution: not all future-initializing functions actually throw, so
/// this signature may be incorrect.
using FutureAsyncSignature =
  AsyncSignature<void(void*), /*throws*/ true>;

/// Create a task object with a future which will run the given
/// function.
///
/// The task is not yet scheduled.
///
/// If a parent task is provided, flags.task_hasChildFragment() must
/// be true, and this must be called synchronously with the parent.
/// The parent is responsible for creating a ChildTaskStatusRecord.
/// TODO: should we have a single runtime function for creating a task
/// and doing this child task status record management?
///
/// flags.task_isFuture must be set. \c futureResultType is the type
///
SWIFT_EXPORT_FROM(swift_Concurrency) SWIFT_CC(swift)
AsyncTaskAndContext swift_task_create_future(
    JobFlags flags, AsyncTask *parent, const Metadata *futureResultType,
    const FutureAsyncSignature::FunctionPointer *function);

/// Create a task object with a future which will run the given
/// function.
SWIFT_EXPORT_FROM(swift_Concurrency) SWIFT_CC(swift)
AsyncTaskAndContext swift_task_create_future_f(
    JobFlags flags, AsyncTask *parent, const Metadata *futureResultType,
    FutureAsyncSignature::FunctionType *function,
    size_t initialContextSize);

/// Allocate memory in a task.
///
/// This must be called synchronously with the task.
///
/// All allocations will be rounded to a multiple of MAX_ALIGNMENT.
SWIFT_EXPORT_FROM(swift_Concurrency) SWIFT_CC(swift)
void *swift_task_alloc(AsyncTask *task, size_t size);

/// Deallocate memory in a task.
///
/// The pointer provided must be the last pointer allocated on
/// this task that has not yet been deallocated; that is, memory
/// must be allocated and deallocated in a strict stack discipline.
SWIFT_EXPORT_FROM(swift_Concurrency) SWIFT_CC(swift)
void swift_task_dealloc(AsyncTask *task, void *ptr);

/// Cancel a task and all of its child tasks.
///
/// This can be called from any thread.
///
/// This has no effect if the task is already cancelled.
SWIFT_EXPORT_FROM(swift_Concurrency) SWIFT_CC(swift)
void swift_task_cancel(AsyncTask *task);

/// Escalate the priority of a task and all of its child tasks.
///
/// This can be called from any thread.
///
/// This has no effect if the task already has at least the given priority.
/// Returns the priority of the task.
SWIFT_EXPORT_FROM(swift_Concurrency) SWIFT_CC(swift)
JobPriority
swift_task_escalate(AsyncTask *task, JobPriority newPriority);

/// The result of waiting for a task future.
struct TaskFutureWaitResult {
  /// Whether the storage represents the error result vs. the successful
  /// result.
  bool hadErrorResult;

  /// Storage for the result of the future.
  ///
  /// When the future completed normally, this is a pointer to the storage
  /// of the result value, which lives inside the future task itself.
  ///
  /// When the future completed by throwing an error, this is the error
  /// object itself.
  OpaqueValue *storage;
};

using TaskFutureWaitSignature =
  AsyncSignature<TaskFutureWaitResult(AsyncTask *), /*throws*/ false>;

/// Wait for a future task to complete.
///
/// This can be called from any thread. Its Swift signature is
///
/// \code
/// func swift_task_future_wait(on task: Builtin.NativeObject) async
///     -> TaskFutureWaitResult
/// \endcode
SWIFT_EXPORT_FROM(swift_Concurrency) SWIFT_CC(swiftasync)
TaskFutureWaitSignature::FunctionType
swift_task_future_wait;

/// Wait for a readyQueue of a Channel to become non empty.
///
/// This can be called from any thread. Its Swift signature is
///
/// \code
/// func swift_task_group_wait_next(on groupTask: Builtin.NativeObject) async
///     -> (hadErrorResult: Bool, storage: UnsafeRawPointer?)
/// \endcode
SWIFT_EXPORT_FROM(swift_Concurrency) SWIFT_CC(swiftasync)
TaskFutureWaitSignature::FunctionType
swift_task_group_wait_next;

/// This can be called from any thread. Its Swift signature is
///
/// \code
/// func swift_task_group_add_pending(
///     _ groupTask: Builtin.NativeObject)
/// )
/// \endcode
SWIFT_EXPORT_FROM(swift_Concurrency) SWIFT_CC(swift)
void
swift_task_group_add_pending(AsyncTask *groupTask);

/// Check the readyQueue of a Channel, return true if it has no pending tasks.
///
/// This can be called from any thread. Its Swift signature is
///
/// \code
/// func swift_task_group_is_empty(on groupTask: Builtin.NativeObject) -> Bool
/// \endcode
SWIFT_EXPORT_FROM(swift_Concurrency) SWIFT_CC(swift)
bool
swift_task_group_is_empty(AsyncTask *task);

/// Add a status record to a task.  The record should not be
/// modified while it is registered with a task.
///
/// This must be called synchronously with the task.
///
/// If the task is already cancelled, returns `false` but still adds
/// the status record.
SWIFT_EXPORT_FROM(swift_Concurrency) SWIFT_CC(swift)
bool swift_task_addStatusRecord(AsyncTask *task,
                                TaskStatusRecord *record);

/// Add a status record to a task if the task has not already
/// been cancelled.   The record should not be modified while it is
/// registered with a task.
///
/// This must be called synchronously with the task.
///
/// If the task is already cancelled, returns `false` and does not
/// add the status record.
SWIFT_EXPORT_FROM(swift_Concurrency) SWIFT_CC(swift)
bool swift_task_tryAddStatusRecord(AsyncTask *task,
                                   TaskStatusRecord *record);

/// Remove a status record from a task.  After this call returns,
/// the record's memory can be freely modified or deallocated.
///
/// This must be called synchronously with the task.  The record must
/// be registered with the task or else this may crash.
///
/// The given record need not be the last record added to
/// the task, but the operation may be less efficient if not.
///s
/// Returns false if the task has been cancelled.
SWIFT_EXPORT_FROM(swift_Concurrency) SWIFT_CC(swift)
bool swift_task_removeStatusRecord(AsyncTask *task,
                                   TaskStatusRecord *record);

SWIFT_EXPORT_FROM(swift_Concurrency) SWIFT_CC(swift)
size_t swift_task_getJobFlags(AsyncTask* task);

SWIFT_EXPORT_FROM(swift_Concurrency) SWIFT_CC(swift)
bool swift_task_isCancelled(AsyncTask* task);

/// This should have the same representation as an enum like this:
///    enum NearestTaskDeadline {
///      case none
///      case alreadyCancelled
///      case active(TaskDeadline)
///    }
/// TODO: decide what this interface should really be.
struct NearestTaskDeadline {
  enum Kind : uint8_t {
    None,
    AlreadyCancelled,
    Active
  };

  TaskDeadline Value;
  Kind ValueKind;
};

/// Returns the nearest deadline that's been registered with this task.
///
/// This must be called synchronously with the task.
SWIFT_EXPORT_FROM(swift_Concurrency) SWIFT_CC(swift)
NearestTaskDeadline
swift_task_getNearestDeadline(AsyncTask *task);

/// Run the given async function and block the current thread until
/// it returns.  This is a hack added for testing purposes; eventually
/// top-level code will be an async context, and the need for this in
/// tests should go away.  We *definitely* do not want this to be part
/// of the standard feature set.
///
/// The argument is a `() async -> ()` function, whose ABI is currently
/// quite complex.  Eventually this should use a different convention;
/// that's rdar://72105841.
SWIFT_EXPORT_FROM(swift_Concurrency) SWIFT_CC(swift)
void swift_task_runAndBlockThread(const void *function,
                                  HeapObject *functionContext);

/// Switch the current task to a new executor if we aren't already
/// running on a compatible executor.
///
/// The resumption function pointer and continuation should be set
/// appropriately in the task.
///
/// Generally the compiler should inline a fast-path compatible-executor
/// check to avoid doing the suspension work.  This function should
/// generally be tail-called, as it may continue executing the task
/// synchronously if possible.
SWIFT_EXPORT_FROM(swift_Concurrency) SWIFT_CC(swiftasync)
void swift_task_switch(AsyncTask *task,
                       ExecutorRef currentExecutor,
                       ExecutorRef newExecutor);

/// Enqueue the given job to run asynchronously on the given executor.
///
/// The resumption function pointer and continuation should be set
/// appropriately in the task.
///
/// Generally you should call swift_task_switch to switch execution
/// synchronously when possible.
SWIFT_EXPORT_FROM(swift_Concurrency) SWIFT_CC(swift)
void swift_task_enqueue(Job *job, ExecutorRef executor);

/// Enqueue the given job to run asynchronously on the global
/// execution pool.
///
/// The resumption function pointer and continuation should be set
/// appropriately in the task.
///
/// Generally you should call swift_task_switch to switch execution
/// synchronously when possible.
SWIFT_EXPORT_FROM(swift_Concurrency) SWIFT_CC(swift)
void swift_task_enqueueGlobal(Job *job);

/// A hook to take over global enqueuing.
/// TODO: figure out a better abstraction plan than this.
SWIFT_EXPORT_FROM(swift_Concurrency) SWIFT_CC(swift)
void (*swift_task_enqueueGlobal_hook)(Job *job);

/// Initialize the runtime storage for a default actor.
SWIFT_EXPORT_FROM(swift_Concurrency) SWIFT_CC(swift)
void swift_defaultActor_initialize(DefaultActor *actor);

/// Destroy the runtime storage for a default actor.
SWIFT_EXPORT_FROM(swift_Concurrency) SWIFT_CC(swift)
void swift_defaultActor_destroy(DefaultActor *actor);

/// Enqueue a job on the default actor implementation.
///
/// The job must be ready to run.  Notably, if it's a task, that
/// means that the resumption function and context should have been
/// set appropriately.
///
/// Jobs are assumed to be "self-consuming": once it starts running,
/// the job memory is invalidated and the executor should not access it
/// again.
///
/// Jobs are generally expected to keep the actor alive during their
/// execution.
SWIFT_EXPORT_FROM(swift_Concurrency) SWIFT_CC(swift)
void swift_defaultActor_enqueue(Job *job, DefaultActor *actor);

/// Resume a task from its continuation, given a normal result value.
SWIFT_EXPORT_FROM(swift_Concurrency) SWIFT_CC(swift)
void swift_continuation_resume(/* +1 */ OpaqueValue *result,
                               void *continuation,
                               const Metadata *resumeType);

/// Resume a task from its throwing continuation, given a normal result value.
SWIFT_EXPORT_FROM(swift_Concurrency) SWIFT_CC(swift)
void swift_continuation_throwingResume(/* +1 */ OpaqueValue *result,
                                       void *continuation,
                                       const Metadata *resumeType);

/// Resume a task from its throwing continuation by throwing an error.
SWIFT_EXPORT_FROM(swift_Concurrency) SWIFT_CC(swift)
void swift_continuation_throwingResumeWithError(/* +1 */ SwiftError *error,
                                                void *continuation,
                                                const Metadata *resumeType);

}

#endif
