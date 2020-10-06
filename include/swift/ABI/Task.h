//===--- Task.h - ABI structures for asynchronous tasks ---------*- C++ -*-===//
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
// Swift ABI describing tasks.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_ABI_TASK_H
#define SWIFT_ABI_TASK_H

#include "swift/ABI/HeapObject.h"
#include "swift/ABI/MetadataValues.h"
#include "swift/Runtime/Config.h"
#include "swift/Basic/STLExtras.h"

namespace swift {

class AsyncTask;
class AsyncContext;
class Executor;
class Job;
class TaskStatusRecord;

/// An ExecutorRef isn't necessarily just a pointer to an executor
/// object; it may have other bits set.
using ExecutorRef = Executor *;

using JobInvokeFunction =
  SWIFT_CC(swift)
  void (Job *, ExecutorRef);

using TaskContinuationFunction =
  SWIFT_CC(swift)
  void (AsyncTask *, ExecutorRef, AsyncContext *);

/// A schedulable job.
class alignas(2 * alignof(void*)) Job {
public:
  // Reserved for the use of the scheduler.
  void *SchedulerPrivate[2];

  JobFlags Flags;

  // We use this union to avoid having to do a second indirect branch
  // when resuming an asynchronous task, which we expect will be the
  // common case.
  union {
    // A function to run a job that isn't an AsyncTask.
    JobInvokeFunction * __ptrauth_swift_job_invoke_function RunJob;

    // A function to resume an AsyncTask.
    TaskContinuationFunction * __ptrauth_swift_task_resume_function ResumeTask;
  };

  Job(JobFlags flags, JobInvokeFunction *invoke)
      : Flags(flags), RunJob(invoke) {
    assert(!isAsyncTask() && "wrong constructor for a task");
  }

  Job(JobFlags flags, TaskContinuationFunction *invoke)
      : Flags(flags), ResumeTask(invoke) {
    assert(isAsyncTask() && "wrong constructor for a non-task job");
  }

  bool isAsyncTask() const {
    return Flags.isAsyncTask();
  }

  /// Run this job.
  void run(Executor *currentExecutor);
};

// The compiler will eventually assume these.
static_assert(sizeof(Job) == 4 * sizeof(void*),
              "Job size is wrong");
static_assert(alignof(Job) == 2 * alignof(void*),
              "Job alignment is wrong");

/// The current state of a task's status records.
class ActiveTaskStatus {
  enum : uintptr_t {
    IsCancelled = 0x1,
    IsLocked = 0x2,
    RecordMask = ~uintptr_t(IsCancelled | IsLocked)
  };

  uintptr_t Value;

public:
  constexpr ActiveTaskStatus() : Value(0) {}
  ActiveTaskStatus(TaskStatusRecord *innermostRecord,
                   bool cancelled, bool locked)
    : Value(reinterpret_cast<uintptr_t>(innermostRecord)
                + (locked ? IsLocked : 0)
                + (cancelled ? IsCancelled : 0)) {}

  /// Is the task currently cancelled?
  bool isCancelled() const { return Value & IsCancelled; }

  /// Is there an active lock on the cancellation information?
  bool isLocked() const { return Value & IsLocked; }

  /// Return the innermost cancellation record.  Code running
  /// asynchronously with this task should not access this record
  /// without having first locked it; see swift_taskCancel.
  TaskStatusRecord *getInnermostRecord() const {
    return reinterpret_cast<TaskStatusRecord*>(Value & RecordMask);
  }

  static TaskStatusRecord *getStatusRecordParent(TaskStatusRecord *ptr);

  using record_iterator =
    LinkedListIterator<TaskStatusRecord, getStatusRecordParent>;
  llvm::iterator_range<record_iterator> records() const {
    return record_iterator::rangeBeginning(getInnermostRecord());
  }
};

/// An asynchronous task.  Tasks are the analogue of threads for
/// asynchronous functions: that is, they are a persistent identity
/// for the overall async computation.
class AsyncTask : public Job {
public:
  /// The context for resuming the job.  When a task is scheduled
  /// as a job, the next continuation should be installed as the
  /// ResumeTask pointer in the job header, with this serving as
  /// the context pointer.
  ///
  /// We can't protect the data in the context from being overwritten
  /// by attackers, but we can at least sign the context pointer to
  /// prevent it from being corrupted in flight.
  AsyncContext * __ptrauth_swift_task_resume_context ResumeContext;

  /// The currntly-active information about cancellation.
  std::atomic<ActiveTaskStatus> Status;

  /// Reserved for the use of the task-local stack allocator.
  void *AllocatorPrivate[4];

  AsyncTask(JobFlags flags, TaskContinuationFunction *run,
            AsyncContext *initialContext)
    : Job(flags, run),
      ResumeContext(initialContext),
      Status(ActiveTaskStatus()) {
    assert(flags.isAsyncTask());
  }

  void run(ExecutorRef currentExecutor) {
    ResumeTask(this, currentExecutor, ResumeContext);
  }

  /// Check whether this task has been cancelled.  Checking this is,
  /// of course, inherently race-prone on its own.
  bool isCancelled() const {
    return Status.load(std::memory_order_relaxed).isCancelled();
  }

  bool isHeapObject() const { return Flags.task_isHeapObject(); }
  HeapObject *heapObjectHeader() {
    assert(isHeapObject());
    return reinterpret_cast<HeapObject*>(this) - 1;
  }

  /// A fragment of an async task structure that happens to be a child task.
  class ChildFragment {
    /// The parent task of this task.
    AsyncTask *Parent;

    /// The next task in the singly-linked list of child tasks.
    /// The list must start in a `ChildTaskStatusRecord` registered
    /// with the parent task.
    /// Note that the parent task may have multiple such records.
    AsyncTask *NextChild = nullptr;

  public:
    AsyncTask *getParent() const {
      return Parent;
    }

    AsyncTask *getNextChild() const {
      return NextChild;
    }
  };

  bool hasChildFragment() const { return Flags.task_isChildTask(); }
  ChildFragment *childFragment() {
    assert(hasChildFragment());
    return reinterpret_cast<ChildFragment*>(this + 1);
  }

  // TODO: Future fragment

  static bool classof(const Job *job) {
    return job->isAsyncTask();
  }
};

// The compiler will eventually assume these.
static_assert(sizeof(AsyncTask) == 10 * sizeof(void*),
              "AsyncTask size is wrong");
static_assert(alignof(AsyncTask) == 2 * alignof(void*),
              "AsyncTask alignment is wrong");

inline void Job::run(ExecutorRef currentExecutor) {
  if (auto task = dyn_cast<AsyncTask>(this))
    task->run(currentExecutor);
  else
    RunJob(this, currentExecutor);
}

/// An asynchronous context within a task.  Generally contexts are
/// allocated using the task-local stack alloc/dealloc operations, but
/// there's no guarantee of that, and the ABI is designed to permit
/// contexts to be allocated within their caller's frame.
class alignas(MaximumAlignment) AsyncContext {
public:
  /// The parent context.
  AsyncContext * __ptrauth_swift_async_context_parent Parent;

  /// The function to call to resume running in the parent context.
  /// Generally this means a semantic return, but for some temporary
  /// translation contexts it might mean initiating a call.
  ///
  /// Eventually, the actual type here will depend on the types
  /// which need to be passed to the parent.  For now, arguments
  /// are always written into the context, and so the type is
  /// always the same.
  TaskContinuationFunction * __ptrauth_swift_async_context_resume
    ResumeParent;

  /// Flags describing this context.
  ///
  /// Note that this field is only 32 bits; any alignment padding
  /// following this on 64-bit platforms can be freely used by the
  /// function.  If the function is a yielding function, that padding
  /// is of course interrupted by the YieldToParent field.
  AsyncContextFlags Flags;

  // Fields following this point may not be valid in all instances
  // of AsyncContext.

  /// The function to call to temporarily resume running in the
  /// parent context temporarily.  Generally this means a semantic
  /// yield.  Requires Flags.hasYieldFunction().
  TaskContinuationFunction * __ptrauth_swift_async_context_yield
    YieldToParent;

  AsyncContext(AsyncContextFlags flags,
               TaskContinuationFunction *resumeParent,
               AsyncContext *parent)
    : Parent(parent), ResumeParent(resumeParent), Flags(flags) {}

  AsyncContext(AsyncContextFlags flags,
               TaskContinuationFunction *resumeParent,
               TaskContinuationFunction *yieldToParent,
               AsyncContext *parent)
    : Parent(parent), ResumeParent(resumeParent), Flags(flags),
      YieldToParent(yieldToParent) {}

  AsyncContext(const AsyncContext &) = delete;
  AsyncContext &operator=(const AsyncContext &) = delete;
};

} // end namespace swift

#endif
