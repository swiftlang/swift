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

#ifndef SWIFT_ABI_TASK_BACKDEPLOY56_H
#define SWIFT_ABI_TASK_BACKDEPLOY56_H

#include "Concurrency/TaskLocal.h"
#include "Executor.h"
#include "swift/ABI/HeapObject.h"
#include "swift/ABI/Metadata.h"
#include "swift/ABI/MetadataValues.h" // Maybe replace?

#include "swift/Runtime/Config.h"
#include "VoucherShims.h"
#include "swift/Basic/STLExtras.h"
#include <bitset>
#include <queue> // TODO: remove and replace with our own mpsc

namespace swift {
class AsyncTask;
class AsyncContext;
class Job;
struct OpaqueValue;
struct SwiftError;
class TaskStatusRecord;
class TaskOptionRecord;
class TaskGroup;

extern FullMetadata<DispatchClassMetadata> jobHeapMetadata;

/// A schedulable job.
class alignas(2 * alignof(void*)) Job :
  // For async-let tasks, the refcount bits are initialized as "immortal"
  // because such a task is allocated with the parent's stack allocator.
  public HeapObject {
public:
  // Indices into SchedulerPrivate, for use by the runtime.
  enum {
    /// The next waiting task link, an AsyncTask that is waiting on a future.
    NextWaitingTaskIndex = 0,

    // The Dispatch object header is one pointer and two ints, which is
    // equivalent to three pointers on 32-bit and two pointers 64-bit. Set the
    // indexes accordingly so that DispatchLinkageIndex points to where Dispatch
    // expects.
    DispatchHasLongObjectHeader = sizeof(void *) == sizeof(int),

    /// An opaque field used by Dispatch when enqueueing Jobs directly.
    DispatchLinkageIndex = DispatchHasLongObjectHeader ? 1 : 0,

    /// The dispatch queue being used when enqueueing a Job directly with
    /// Dispatch.
    DispatchQueueIndex = DispatchHasLongObjectHeader ? 0 : 1,
  };

  // Reserved for the use of the scheduler.
  void *SchedulerPrivate[2];

  JobFlags Flags;

  // Derived classes can use this to store a Job Id.
  uint32_t Id = 0;

  /// The voucher associated with the job. Note: this is currently unused on
  /// non-Darwin platforms, with stub implementations of the functions for
  /// consistency.
  voucher_t Voucher = nullptr;

  /// Reserved for future use.
  void *Reserved = nullptr;

  // We use this union to avoid having to do a second indirect branch
  // when resuming an asynchronous task, which we expect will be the
  // common case.
  union {
    // A function to run a job that isn't an AsyncTask.
    JobInvokeFunction * __ptrauth_swift_job_invoke_function RunJob;

    // A function to resume an AsyncTask.
    TaskContinuationFunction * __ptrauth_swift_task_resume_function ResumeTask;
  };

  Job(JobFlags flags, JobInvokeFunction *invoke,
      const HeapMetadata *metadata = &jobHeapMetadata)
      : HeapObject(metadata), Flags(flags), RunJob(invoke) {
    Voucher = voucher_copy();
    assert(!isAsyncTask() && "wrong constructor for a task");
  }

  Job(JobFlags flags, TaskContinuationFunction *invoke,
      const HeapMetadata *metadata = &jobHeapMetadata,
      bool captureCurrentVoucher = true)
      : HeapObject(metadata), Flags(flags), ResumeTask(invoke) {
    if (captureCurrentVoucher)
      Voucher = voucher_copy();
    assert(isAsyncTask() && "wrong constructor for a non-task job");
  }

  /// Create a job with "immortal" reference counts.
  /// Used for async let tasks.
  Job(JobFlags flags, TaskContinuationFunction *invoke,
      const HeapMetadata *metadata, InlineRefCounts::Immortal_t immortal,
      bool captureCurrentVoucher = true)
      : HeapObject(metadata, immortal), Flags(flags), ResumeTask(invoke) {
    if (captureCurrentVoucher)
      Voucher = voucher_copy();
    assert(isAsyncTask() && "wrong constructor for a non-task job");
  }

  ~Job() { swift_voucher_release(Voucher); }

  bool isAsyncTask() const {
    return Flags.isAsyncTask();
  }

  JobPriority getPriority() const {
    return Flags.getPriority();
  }

  /// Given that we've fully established the job context in the current
  /// thread, actually start running this job.  To establish the context
  /// correctly, call swift_job_run or runJobInExecutorContext.
  SWIFT_CC(swiftasync)
  void runInFullyEstablishedContext();

  /// Given that we've fully established the job context in the
  /// current thread, and that the job is a simple (non-task) job,
  /// actually start running this job.
  SWIFT_CC(swiftasync)
  void runSimpleInFullyEstablishedContext() {
    return RunJob(this); // 'return' forces tail call
  }
};

// The compiler will eventually assume these.
#if SWIFT_POINTER_IS_8_BYTES
static_assert(sizeof(Job) == 8 * sizeof(void*),
              "Job size is wrong");
#else
static_assert(sizeof(Job) == 10 * sizeof(void*),
              "Job size is wrong");
#endif
static_assert(alignof(Job) == 2 * alignof(void*),
              "Job alignment is wrong");

class NullaryContinuationJob : public Job {

private:
  AsyncTask* Task;
  AsyncTask* Continuation;

public:
  NullaryContinuationJob(AsyncTask *task, JobPriority priority, AsyncTask *continuation)
    : Job({JobKind::NullaryContinuation, priority}, &process),
      Task(task), Continuation(continuation) {}

  SWIFT_CC(swiftasync)
  static void process(Job *job);

  static bool classof(const Job *job) {
    return job->Flags.getKind() == JobKind::NullaryContinuation;
  }
};

/// An asynchronous task.  Tasks are the analogue of threads for
/// asynchronous functions: that is, they are a persistent identity
/// for the overall async computation.
///
/// ### Fragments
/// An AsyncTask may have the following fragments:
///
///    +--------------------------+
///    | childFragment?           |
///    | groupChildFragment?      |
///    | futureFragment?          |*
///    +--------------------------+
///
/// * The future fragment is dynamic in size, based on the future result type
///   it can hold, and thus must be the *last* fragment.
class AsyncTask : public Job {
public:
  // On 32-bit targets, there is a word of tail padding remaining
  // in Job, and ResumeContext will fit into that, at offset 28.
  // Private then has offset 32.
  // On 64-bit targets, there is no tail padding in Job, and so
  // ResumeContext has offset 48.  There is therefore another word
  // of reserved storage prior to Private (which needs to have
  // double-word alignment), which has offset 64.
  // We therefore converge and end up with 16 words of storage on
  // all platforms.

  /// The context for resuming the job.  When a task is scheduled
  /// as a job, the next continuation should be installed as the
  /// ResumeTask pointer in the job header, with this serving as
  /// the context pointer.
  ///
  /// We can't protect the data in the context from being overwritten
  /// by attackers, but we can at least sign the context pointer to
  /// prevent it from being corrupted in flight.
  AsyncContext * __ptrauth_swift_task_resume_context ResumeContext;

#if SWIFT_POINTER_IS_8_BYTES
  void *Reserved64;
#endif

  struct PrivateStorage;

  /// Private storage for the use of the runtime.
  struct alignas(2 * alignof(void*)) OpaquePrivateStorage {
    void *Storage[14];

    /// Initialize this storage during the creation of a task.
    void initialize(AsyncTask *task);
    void initializeWithSlab(AsyncTask *task,
                            void *slab, size_t slabCapacity);

    /// React to the completion of the enclosing task's execution.
    void complete(AsyncTask *task);

    /// React to the final destruction of the enclosing task.
    void destroy();

    PrivateStorage &get();
    const PrivateStorage &get() const;
  };
  /// Private storage for the use of the runtime.
  PrivateStorage &_private();
  const PrivateStorage &_private() const;

  OpaquePrivateStorage Private;

  /// Create a task.
  /// This does not initialize Private; callers must call
  /// Private.initialize separately.
  AsyncTask(const HeapMetadata *metadata, JobFlags flags,
            TaskContinuationFunction *run,
            AsyncContext *initialContext,
            bool captureCurrentVoucher)
    : Job(flags, run, metadata, captureCurrentVoucher),
      ResumeContext(initialContext) {
    assert(flags.isAsyncTask());
    setTaskId();
  }

  /// Create a task with "immortal" reference counts.
  /// Used for async let tasks.
  /// This does not initialize Private; callers must call
  /// Private.initialize separately.
  AsyncTask(const HeapMetadata *metadata, InlineRefCounts::Immortal_t immortal,
            JobFlags flags,
            TaskContinuationFunction *run,
            AsyncContext *initialContext,
            bool captureCurrentVoucher)
    : Job(flags, run, metadata, immortal, captureCurrentVoucher),
      ResumeContext(initialContext) {
    assert(flags.isAsyncTask());
    setTaskId();
  }

  ~AsyncTask();

  /// Set the task's ID field to the next task ID.
  void setTaskId();
  uint64_t getTaskId();

  /// Given that we've already fully established the job context
  /// in the current thread, start running this task.  To establish
  /// the job context correctly, call swift_job_run or
  /// runInExecutorContext.
  SWIFT_CC(swiftasync)
  void runInFullyEstablishedContext() {
    return ResumeTask(ResumeContext); // 'return' forces tail call
  }

  /// A task can have the following states:
  ///   * suspended: In this state, a task is considered not runnable
  ///   * enqueued: In this state, a task is considered runnable
  ///   * running on a thread
  ///   * completed
  ///
  /// The following state transitions are possible:
  ///       suspended -> enqueued
  ///       suspended -> running
  ///       enqueued -> running
  ///       running -> suspended
  ///       running -> completed
  ///       running -> enqueued
  ///
  /// The 4 methods below are how a task switches from one state to another.

  /// Flag that this task is now running.  This can update
  /// the priority stored in the job flags if the priority has been
  /// escalated.
  ///
  /// Generally this should be done immediately after updating
  /// ActiveTask.
  __attribute__((visibility("hidden")))
  void flagAsRunning();
  __attribute__((visibility("hidden")))
  void flagAsRunning_slow();

  /// Flag that this task is now suspended.  This can update the
  /// priority stored in the job flags if the priority has been
  /// escalated.  Generally this should be done immediately after
  /// clearing ActiveTask and immediately before enqueuing the task
  /// somewhere.  TODO: record where the task is enqueued if
  /// possible.
  __attribute__((visibility("hidden")))
  void flagAsSuspended();
  __attribute__((visibility("hidden")))
  void flagAsSuspended_slow();

  /// Flag that this task is now completed. This normally does not do anything
  /// but can be used to locally insert logging.
  __attribute__((visibility("hidden")))
  void flagAsCompleted();

  /// Check whether this task has been cancelled.
  /// Checking this is, of course, inherently race-prone on its own.
  __attribute__((visibility("hidden")))
  bool isCancelled() const;

  // ==== Task Local Values ----------------------------------------------------

  __attribute__((visibility("hidden")))
  void localValuePush(const HeapObject *key,
                      /* +1 */ OpaqueValue *value,
                      const Metadata *valueType);

  __attribute__((visibility("hidden")))
  OpaqueValue *localValueGet(const HeapObject *key);

  /// Returns true if storage has still more bindings.
  __attribute__((visibility("hidden")))
  bool localValuePop();

  // ==== Child Fragment -------------------------------------------------------

  /// A fragment of an async task structure that happens to be a child task.
  class ChildFragment {
    /// The parent task of this task.
    AsyncTask *Parent;

    // TODO: Document more how this is used from the `TaskGroupTaskStatusRecord`

    /// The next task in the singly-linked list of child tasks.
    /// The list must start in a `ChildTaskStatusRecord` registered
    /// with the parent task.
    ///
    /// Note that the parent task may have multiple such records.
    ///
    /// WARNING: Access can only be performed by the `Parent` of this task.
    AsyncTask *NextChild = nullptr;

  public:
    ChildFragment(AsyncTask *parent) : Parent(parent) {}

    AsyncTask *getParent() const {
      return Parent;
    }

    AsyncTask *getNextChild() const {
      return NextChild;
    }

    /// Set the `NextChild` to to the passed task.
    ///
    /// WARNING: This must ONLY be invoked from the parent of both
    /// (this and the passed-in) tasks for thread-safety reasons.
    void setNextChild(AsyncTask *task) {
      NextChild = task;
    }
  };

  bool hasChildFragment() const {
    return Flags.task_isChildTask();
  }

  ChildFragment *childFragment() {
    assert(hasChildFragment());

    auto offset = reinterpret_cast<char*>(this);
    offset += sizeof(AsyncTask);

    return reinterpret_cast<ChildFragment*>(offset);
  }

  // ==== TaskGroup Child ------------------------------------------------------

  /// A child task created by `group.add` is called a "task group child."
  /// Upon completion, in addition to the usual future notifying all its waiters,
  /// it must also `group->offer` itself to the group.
  ///
  /// This signalling is necessary to correctly implement the group's `next()`.
  class GroupChildFragment {
  private:
    TaskGroup* Group;

    friend class AsyncTask;
    friend class TaskGroup;

  public:
    explicit GroupChildFragment(TaskGroup *group)
        : Group(group) {}

    /// Return the group this task should offer into when it completes.
    TaskGroup* getGroup() {
      return Group;
    }
  };

  // Checks if task is a child of a TaskGroup task.
  //
  // A child task that is a group child knows that it's parent is a group
  // and therefore may `groupOffer` to it upon completion.
  bool hasGroupChildFragment() const { return Flags.task_isGroupChildTask(); }

  GroupChildFragment *groupChildFragment() {
    assert(hasGroupChildFragment());

    auto offset = reinterpret_cast<char*>(this);
    offset += sizeof(AsyncTask);
    if (hasChildFragment())
      offset += sizeof(ChildFragment);

    return reinterpret_cast<GroupChildFragment *>(offset);
  }

  // ==== Future ---------------------------------------------------------------

  class FutureFragment {
  public:
    /// Describes the status of the future.
    ///
    /// Futures always begin in the "Executing" state, and will always
    /// make a single state change to either Success or Error.
    enum class Status : uintptr_t {
      /// The future is executing or ready to execute. The storage
      /// is not accessible.
      Executing = 0,

      /// The future has completed with result (of type \c resultType).
      Success,

      /// The future has completed by throwing an error (an \c Error
      /// existential).
      Error,
    };

    /// An item within the wait queue, which includes the status and the
    /// head of the list of tasks.
    struct WaitQueueItem {
      /// Mask used for the low status bits in a wait queue item.
      static const uintptr_t statusMask = 0x03;

      uintptr_t storage;

      Status getStatus() const {
        return static_cast<Status>(storage & statusMask);
      }

      AsyncTask *getTask() const {
        return reinterpret_cast<AsyncTask *>(storage & ~statusMask);
      }

      static WaitQueueItem get(Status status, AsyncTask *task) {
        return WaitQueueItem{
          reinterpret_cast<uintptr_t>(task) | static_cast<uintptr_t>(status)};
      }
    };

  private:
    /// Queue containing all of the tasks that are waiting in `get()`.
    ///
    /// The low bits contain the status, the rest of the pointer is the
    /// AsyncTask.
    std::atomic<WaitQueueItem> waitQueue;

    /// The type of the result that will be produced by the future.
    const Metadata *resultType;

    SwiftError *error = nullptr;

    // Trailing storage for the result itself. The storage will be
    // uninitialized, contain an instance of \c resultType.

    friend class AsyncTask;

  public:
    explicit FutureFragment(const Metadata *resultType)
      : waitQueue(WaitQueueItem::get(Status::Executing, nullptr)),
        resultType(resultType) { }

    /// Destroy the storage associated with the future.
    void destroy();

    const Metadata *getResultType() const {
      return resultType;
    }

    /// Retrieve a pointer to the storage of the result.
    OpaqueValue *getStoragePtr() {
      // The result storage starts at the first aligned offset following
      // the fragment header.  This offset will agree with the abstract
      // calculation for `resultOffset` in the fragmentSize function below
      // because the entire task is aligned to at least the target
      // alignment (because it's aligned to MaxAlignment), which means
      // `this` must have the same value modulo that alignment as
      // `fragmentOffset` has in that function.
      char *fragmentAddr = reinterpret_cast<char *>(this);
      uintptr_t alignment = resultType->vw_alignment();
      char *resultAddr = fragmentAddr + sizeof(FutureFragment);
      uintptr_t unalignedResultAddrInt =
        reinterpret_cast<uintptr_t>(resultAddr);
      uintptr_t alignedResultAddrInt =
        (unalignedResultAddrInt + alignment - 1) & ~(alignment - 1);
      // We could just cast alignedResultAddrInt back to a pointer, but
      // doing pointer arithmetic is more strictly conformant and less
      // likely to annoy the optimizer.
      resultAddr += (alignedResultAddrInt - unalignedResultAddrInt);
      return reinterpret_cast<OpaqueValue *>(resultAddr);
    }

    /// Retrieve the error.
    SwiftError *&getError() { return error; }

    /// Determine the size of the future fragment given the result type
    /// of the future.
    static size_t fragmentSize(size_t fragmentOffset,
                               const Metadata *resultType) {
      assert((fragmentOffset & (alignof(FutureFragment) - 1)) == 0);
      size_t alignment = resultType->vw_alignment();
      size_t resultOffset = fragmentOffset + sizeof(FutureFragment);
      resultOffset = (resultOffset + alignment - 1) & ~(alignment - 1);
      size_t endOffset = resultOffset + resultType->vw_size();
      return (endOffset - fragmentOffset);
    }
  };

  bool isFuture() const { return Flags.task_isFuture(); }

  FutureFragment *futureFragment() {
    assert(isFuture());
    auto offset = reinterpret_cast<char*>(this);
    offset += sizeof(AsyncTask);
    if (hasChildFragment())
      offset += sizeof(ChildFragment);
    if (hasGroupChildFragment())
      offset += sizeof(GroupChildFragment);

    return reinterpret_cast<FutureFragment *>(offset);
  }

  /// Wait for this future to complete.
  ///
  /// \returns the status of the future. If this result is
  /// \c Executing, then \c waitingTask has been added to the
  /// wait queue and will be scheduled when the future completes. Otherwise,
  /// the future has completed and can be queried.
  /// The waiting task's async context will be initialized with the parameters if
  /// the current's task state is executing.
  __attribute__((visibility("hidden")))
  FutureFragment::Status waitFuture(AsyncTask *waitingTask,
                                    AsyncContext *waitingTaskContext,
                                    TaskContinuationFunction *resumeFn,
                                    AsyncContext *callerContext,
                                    OpaqueValue *result);

  /// Complete this future.
  ///
  /// Upon completion, any waiting tasks will be scheduled on the given
  /// executor.
  void completeFuture(AsyncContext *context);

  // ==== ----------------------------------------------------------------------

  static bool classof(const Job *job) {
    return job->isAsyncTask();
  }

private:
  /// Access the next waiting task, which establishes a singly linked list of
  /// tasks that are waiting on a future.
  AsyncTask *&getNextWaitingTask() {
    return reinterpret_cast<AsyncTask *&>(
        SchedulerPrivate[NextWaitingTaskIndex]);
  }
};

enum { NumWords_AsyncTask = 24 };

// The compiler will eventually assume these.
static_assert(sizeof(AsyncTask) == NumWords_AsyncTask * sizeof(void*),
              "AsyncTask size is wrong");
static_assert(alignof(AsyncTask) == 2 * alignof(void*),
              "AsyncTask alignment is wrong");
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Winvalid-offsetof"
// Libc hardcodes this offset to extract the TaskID
static_assert(offsetof(AsyncTask, Id) == 4 * sizeof(void *) + 4,
              "AsyncTask::Id offset is wrong");
#pragma clang diagnostic pop

SWIFT_CC(swiftasync)
inline void Job::runInFullyEstablishedContext() {
  if (auto task = dyn_cast<AsyncTask>(this))
    return task->runInFullyEstablishedContext(); // 'return' forces tail call
  else
    return runSimpleInFullyEstablishedContext(); // 'return' forces tail call
}

// ==== ------------------------------------------------------------------------

/// The Swift5.6 AsyncContextKind for the AsyncContext.
/// Note that these were removed in Swift5.7
/// (aca744b21165a20655502b563a6fa54c2c83efdf).
/// Kinds of async context.
enum class AsyncContextKind {
  /// An ordinary asynchronous function.
  Ordinary         = 0,

  /// A context which can yield to its caller.
  Yielding         = 1,

  /// A continuation context.
  Continuation     = 2,

  // Other kinds are reserved for interesting special
  // intermediate contexts.

  // Kinds >= 192 are private to the implementation.
  First_Reserved = 192
};


/// The Swift5.6 AsyncContextFlags for the AsyncContext.
/// Note that these were removed in Swift5.7
/// (aca744b21165a20655502b563a6fa54c2c83efdf).
/// Flags for async contexts.
class AsyncContextFlags : public FlagSet<uint32_t> {
public:
  enum {
    Kind                = 0,
    Kind_width          = 8,

    CanThrow            = 8,

    // Kind-specific flags should grow down from 31.

    Continuation_IsExecutorSwitchForced = 31,
  };

  explicit AsyncContextFlags(uint32_t bits) : FlagSet(bits) {}
  constexpr AsyncContextFlags() {}
  AsyncContextFlags(AsyncContextKind kind) {
    setKind(kind);
  }

  /// The kind of context this represents.
  FLAGSET_DEFINE_FIELD_ACCESSORS(Kind, Kind_width, AsyncContextKind,
                                 getKind, setKind)

  /// Whether this context is permitted to throw.
  FLAGSET_DEFINE_FLAG_ACCESSORS(CanThrow, canThrow, setCanThrow)

  /// See AsyncContinuationFlags::isExecutorSwitchForced.
  FLAGSET_DEFINE_FLAG_ACCESSORS(Continuation_IsExecutorSwitchForced,
                                continuation_isExecutorSwitchForced,
                                continuation_setIsExecutorSwitchForced)
};


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

  AsyncContext(AsyncContextFlags flags,
               TaskContinuationFunction *resumeParent,
               AsyncContext *parent)
    : Parent(parent), ResumeParent(resumeParent),
      Flags(flags) {}

  AsyncContext(const AsyncContext &) = delete;
  AsyncContext &operator=(const AsyncContext &) = delete;

  /// Perform a return from this context.
  ///
  /// Generally this should be tail-called.
  SWIFT_CC(swiftasync)
  void resumeParent() {
    // TODO: destroy context before returning?
    // FIXME: force tail call
    return ResumeParent(Parent);
  }
};

/// An async context that supports yielding.
class YieldingAsyncContext : public AsyncContext {
public:
  /// The function to call to temporarily resume running in the
  /// parent context.  Generally this means a semantic yield.
  TaskContinuationFunction * __ptrauth_swift_async_context_yield
    YieldToParent;

  YieldingAsyncContext(AsyncContextFlags flags,
                       TaskContinuationFunction *resumeParent,
                       TaskContinuationFunction *yieldToParent,
                       AsyncContext *parent)
    : AsyncContext(flags, resumeParent, parent),
      YieldToParent(yieldToParent) {}

  static bool classof(const AsyncContext *context) {
    return context->Flags.getKind() == AsyncContextKind::Yielding;
  }
};

/// An async context that can be resumed as a continuation.
class ContinuationAsyncContext : public AsyncContext {
public:
  /// An atomic object used to ensure that a continuation is not
  /// scheduled immediately during a resume if it hasn't yet been
  /// awaited by the function which set it up.
  std::atomic<ContinuationStatus> AwaitSynchronization;

  /// The error result value of the continuation.
  /// This should be null-initialized when setting up the continuation.
  /// Throwing resumers must overwrite this with a non-null value.
  SwiftError *ErrorResult;

  /// A pointer to the normal result value of the continuation.
  /// Normal resumers must initialize this before resuming.
  OpaqueValue *NormalResult;

  /// The executor that should be resumed to.
  ExecutorRef ResumeToExecutor;

  void setErrorResult(SwiftError *error) {
    ErrorResult = error;
  }

  bool isExecutorSwitchForced() const {
    return Flags.continuation_isExecutorSwitchForced();
  }

  static bool classof(const AsyncContext *context) {
    return context->Flags.getKind() == AsyncContextKind::Continuation;
  }
};

/// An asynchronous context within a task that describes a general "Future".
/// task.
///
/// This type matches the ABI of a function `<T> () async throws -> T`, which
/// is the type used by `detach` and `Task.group.add` to create
/// futures.
class FutureAsyncContext : public AsyncContext {
public:
  using AsyncContext::AsyncContext;
};

/// This matches the ABI of a closure `() async throws -> ()`
using AsyncVoidClosureEntryPoint =
  SWIFT_CC(swiftasync)
  void (SWIFT_ASYNC_CONTEXT AsyncContext *, SWIFT_CONTEXT void *);

/// This matches the ABI of a closure `<T>() async throws -> T`
using AsyncGenericClosureEntryPoint =
    SWIFT_CC(swiftasync)
    void(OpaqueValue *,
         SWIFT_ASYNC_CONTEXT AsyncContext *, SWIFT_CONTEXT void *);

/// This matches the ABI of the resume function of a closure
///  `() async throws -> ()`.
using AsyncVoidClosureResumeEntryPoint =
  SWIFT_CC(swiftasync)
  void(SWIFT_ASYNC_CONTEXT AsyncContext *, SWIFT_CONTEXT SwiftError *);

class AsyncContextPrefix {
public:
  // Async closure entry point adhering to compiler calling conv (e.g directly
  // passing the closure context instead of via the async context)
  AsyncVoidClosureEntryPoint *__ptrauth_swift_task_resume_function
      asyncEntryPoint;
  void *closureContext;
  SwiftError *errorResult;
};

/// Storage that is allocated before the AsyncContext to be used by an adapter
/// of Swift's async convention and the ResumeTask interface.
class FutureAsyncContextPrefix {
public:
  OpaqueValue *indirectResult;
  // Async closure entry point adhering to compiler calling conv (e.g directly
  // passing the closure context instead of via the async context)
  AsyncGenericClosureEntryPoint *__ptrauth_swift_task_resume_function
      asyncEntryPoint;
  void *closureContext;
  SwiftError *errorResult;
};

} // end namespace swift

#endif // SWIFT_ABI_TASK_BACKDEPLOY56_H
