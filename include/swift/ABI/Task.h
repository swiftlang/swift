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

#include "swift/Basic/RelativePointer.h"
#include "swift/ABI/HeapObject.h"
#include "swift/ABI/Metadata.h"
#include "swift/ABI/MetadataValues.h"
#include "swift/Runtime/Config.h"
#include "swift/Basic/STLExtras.h"

namespace swift {

class AsyncTask;
class AsyncContext;
class Executor;
class Job;
struct OpaqueValue;
struct SwiftError;
class TaskStatusRecord;

/// An ExecutorRef isn't necessarily just a pointer to an executor
/// object; it may have other bits set.
struct ExecutorRef {
  Executor *Pointer;

  /// Get an executor ref that represents a lack of preference about
  /// where execution resumes.  This is only valid in continuations,
  /// return contexts, and so on; it is not generally passed to
  /// executing functions.
  static ExecutorRef noPreference() {
    return { nullptr };
  }

  bool operator==(ExecutorRef other) const {
    return Pointer == other.Pointer;
  }
};

using JobInvokeFunction =
  SWIFT_CC(swift)
  void (Job *, ExecutorRef);

using TaskContinuationFunction =
  SWIFT_CC(swift)
  void (AsyncTask *, ExecutorRef, AsyncContext *);

template <class Fn>
struct AsyncFunctionTypeImpl;
template <class Result, class... Params>
struct AsyncFunctionTypeImpl<Result(Params...)> {
  // TODO: expand and include the arguments in the parameters.
  using type = TaskContinuationFunction;
};

template <class Fn>
using AsyncFunctionType = typename AsyncFunctionTypeImpl<Fn>::type;

/// A "function pointer" for an async function.
///
/// Eventually, this will always be signed with the data key
/// using a type-specific discriminator.
template <class FnType>
class AsyncFunctionPointer {
public:
  /// The function to run.
  RelativeDirectPointer<AsyncFunctionType<FnType>,
                        /*nullable*/ false,
                        int32_t> Function;

  /// The expected size of the context.
  uint32_t ExpectedContextSize;
};

/// A schedulable job.
class alignas(2 * alignof(void*)) Job {
protected:
  // Indices into SchedulerPrivate, for use by the runtime.
  enum {
    /// The next waiting task link, an AsyncTask that is waiting on a future.
    NextWaitingTaskIndex = 0,
  };

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

  JobPriority getPriority() const {
    return Flags.getPriority();
  }

  /// Run this job.
  void run(ExecutorRef currentExecutor);
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
class AsyncTask : public HeapObject, public Job {
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

  /// The currently-active information about cancellation.
  std::atomic<ActiveTaskStatus> Status;

  /// Reserved for the use of the task-local stack allocator.
  void *AllocatorPrivate[4];

  AsyncTask(const HeapMetadata *metadata, JobFlags flags,
            TaskContinuationFunction *run,
            AsyncContext *initialContext)
    : HeapObject(metadata), Job(flags, run),
      ResumeContext(initialContext),
      Status(ActiveTaskStatus()) {
    assert(flags.isAsyncTask());
  }

  void run(ExecutorRef currentExecutor) {
    ResumeTask(this, currentExecutor, ResumeContext);
  }

  /// Check whether this task has been cancelled.
  /// Checking this is, of course, inherently race-prone on its own.
  bool isCancelled() const {
    return Status.load(std::memory_order_relaxed).isCancelled();
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
    ChildFragment(AsyncTask *parent) : Parent(parent) {}

    AsyncTask *getParent() const {
      return Parent;
    }

    AsyncTask *getNextChild() const {
      return NextChild;
    }
  };

  // TODO: rename? all other functions are is... rather than has...Fragment
  bool hasChildFragment() const { return Flags.task_isChildTask(); }

  ChildFragment *childFragment() {
    assert(hasChildFragment());
    return reinterpret_cast<ChildFragment*>(this + 1);
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

    // Trailing storage for the result itself. The storage will be uninitialized,
    // contain an instance of \c resultType, or contain an an \c Error.

    friend class AsyncTask;

  public:
    explicit FutureFragment(const Metadata *resultType)
      : waitQueue(WaitQueueItem::get(Status::Executing, nullptr)),
        resultType(resultType) { }

    /// Destroy the storage associated with the future.
    void destroy();

    /// Retrieve a pointer to the storage of result.
    OpaqueValue *getStoragePtr() {
      return reinterpret_cast<OpaqueValue *>(
          reinterpret_cast<char *>(this) + storageOffset(resultType));
    }

    /// Retrieve the error.
    SwiftError *&getError() {
      return *reinterpret_cast<SwiftError **>(
           reinterpret_cast<char *>(this) + storageOffset(resultType));
    }

    /// Compute the offset of the storage from the base of the future
    /// fragment.
    static size_t storageOffset(const Metadata *resultType)  {
      size_t offset = sizeof(FutureFragment);
      size_t alignment =
          std::max(resultType->vw_alignment(), alignof(SwiftError *));
      return (offset + alignment - 1) & ~(alignment - 1);
    }

    /// Determine the size of the future fragment given a particular future
    /// result type.
    static size_t fragmentSize(const Metadata *resultType) {
      return storageOffset(resultType) +
          std::max(resultType->vw_size(), sizeof(SwiftError *));
    }
  };

  bool isFuture() const { return Flags.task_isFuture(); }

  FutureFragment *futureFragment() {
    assert(isFuture());
    if (hasChildFragment()) {
      return reinterpret_cast<FutureFragment *>(
          reinterpret_cast<ChildFragment*>(this + 1) + 1);
    }

    return reinterpret_cast<FutureFragment *>(this + 1);
  }

  /// Wait for this future to complete.
  ///
  /// \returns the status of the future. If this result is
  /// \c Executing, then \c waitingTask has been added to the
  /// wait queue and will be scheduled when the future completes. Otherwise,
  /// the future has completed and can be queried.
  FutureFragment::Status waitFuture(AsyncTask *waitingTask);

  /// Complete this future.
  ///
  /// Upon completion, any waiting tasks will be scheduled on the given
  /// executor.
  void completeFuture(AsyncContext *context, ExecutorRef executor);

  // ==== Channel --------------------------------------------------------------

  class ChannelFragment {
  public:
      /// Describes the status of the channel.
      enum class ReadyQueueStatus : uintptr_t {
          /// The channel is empty, no tasks are pending.
          /// Return immediately, there is no point in suspending.
          ///
          /// The storage is not accessible.
          Empty = 0,

          /// The channel has pending tasks
          ///
          /// The storage is not accessible.
          Pending = 1,

          /// The future has completed with result (of type \c resultType).
          Success = 2,

          /// The future has completed by throwing an error (an \c Error
          /// existential).
          Error = 3,

//          /// No tasks are ready, yet there exist pending tasks (they were
//          /// `add()`-ed and not completed yet). Suspend and await a wake-up
//          /// when any task completes.
//          Pending = 1,
//
//          /// At least one task is completed and waiting in the queue.
//          /// Pull immediately without suspending to obtain it.
//          Ready = 2,
      };

      /// Describes the status of the future.
      ///
      /// Futures always begin in the "Executing" state, and will always
      /// make a single state change to either Success or Error.
      enum class Status : uintptr_t {
          /// No-one is waiting.
          /// The storage is not accessible.
          Pending = 0,

          /// The future has completed with result (of type \c resultType).
          Success = 1,

          /// The future has completed by throwing an error (an \c Error
          /// existential).
          Error = 2,
      };

      /// An item within the message queue of a channel.
      struct ReadyQueueItem {
          /// Mask used for the low status bits in a message queue item.
          static const uintptr_t statusMask = 0x03;

          uintptr_t storage;

          ReadyQueueStatus getStatus() const {
            return static_cast<ReadyQueueStatus>(storage & statusMask);
          }

          AsyncTask *getTask() const {
            return reinterpret_cast<AsyncTask *>(storage & ~statusMask);
          }

          static ReadyQueueItem get(ReadyQueueStatus status, AsyncTask *task) {
            return ReadyQueueItem{
                reinterpret_cast<uintptr_t>(task) | static_cast<uintptr_t>(status)};
          }
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
      // TODO we likely can collapse these into one queue if we try hard enough

      /// Queue containing completed tasks offered into this channel.
      ///
      /// The low bits contain the status, the rest of the pointer is the
      /// AsyncTask.
      std::atomic<ReadyQueueItem> readyQueue;

      /// Queue containing all of the tasks that are waiting in `get()`.
      ///
      /// The low bits contain the status, the rest of the pointer is the
      /// AsyncTask.
      std::atomic<WaitQueueItem> waitQueue;

      /// The type of the result that will be produced by the channel.
      const Metadata *resultType;

      // FIXME: seems shady...?
      // Trailing storage for the result itself. The storage will be uninitialized.
      // Use the `readyQueue` to poll for values from the channel instead.
      friend class AsyncTask; // TODO: remove this?

  public:
      explicit ChannelFragment(const Metadata *resultType)
          : readyQueue(ReadyQueueItem::get(ReadyQueueStatus::Empty, nullptr)),
            waitQueue(WaitQueueItem::get(Status::Pending, nullptr)),
            resultType(resultType) { }

      /// Destroy the storage associated with the channel.
      void destroy();

//      /// Offer a completed task to the channel to enable polling for it.
//      void offer(AsyncTask *completed, AsyncContext *context, ExecutorRef executor);
//
//      /// Poll the channel for any
//      ChannelFragment::ReadyQueueStatus channelPoll(AsyncTask *waitingTask);

//      /// Retrieve a pointer to the storage of result.
//      OpaqueValue *getStoragePtr() {
//        return reinterpret_cast<OpaqueValue *>(
//            reinterpret_cast<char *>(this) + storageOffset(resultType));
//      }
//
//      /// Retrieve the error.
//      SwiftError *&getError() {
//        return *reinterpret_cast<SwiftError **>(
//            reinterpret_cast<char *>(this) + storageOffset(resultType));
//      }

//      /// Compute the offset of the storage from the base of the channel
//      /// fragment.
//      static size_t storageOffset(const Metadata *resultType)  {
//        size_t offset = sizeof(ChannelFragment);
//        size_t alignment =
//            std::max(resultType->vw_alignment(), alignof(SwiftError *));
//        return (offset + alignment - 1) & ~(alignment - 1);
//      }
//
//      /// Determine the size of the channel fragment given a particular channel
//      /// result type.
//      static size_t fragmentSize(const Metadata *resultType) {
//        return storageOffset(resultType) +
//               std::max(resultType->vw_size(), sizeof(SwiftError *));
//      }
  };

  bool isChannel() const { return Flags.task_isChannel(); }

  ChannelFragment *channelFragment() {
    assert(isChannel());
    // FIXME: isn't it also a future at the same time?
    if (hasChildFragment()) { // TODO: make sure those all are correct; we can have many fragments
      return reinterpret_cast<ChannelFragment *>(
          reinterpret_cast<ChildFragment*>(this + 1) + 1);
    }

    return reinterpret_cast<ChannelFragment *>(this + 1);
  }

  /// Offer result of a task into this channel.
  /// The value is enqueued at the end of the channel.
  ///
  /// Upon enqueue, any waiting tasks will be scheduled on the given executor. // TODO: not precisely right
  void channelOffer(AsyncTask *completed, AsyncContext *context, ExecutorRef executor);

  /// Wait for for channel to become non-empty.
  ///
  /// \returns the status of the queue.  TODO more docs
  ChannelFragment::ReadyQueueStatus channelPoll(AsyncTask *waitingTask);

  // ==== ----------------------------------------------------------------------

  bool isGroupChild() const { return Flags.task_isGroupChild(); }

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

// The compiler will eventually assume these.
static_assert(sizeof(AsyncTask) == 12 * sizeof(void*),
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

  /// The executor that the parent needs to be resumed on.
  ExecutorRef ResumeParentExecutor;

  /// Flags describing this context.
  ///
  /// Note that this field is only 32 bits; any alignment padding
  /// following this on 64-bit platforms can be freely used by the
  /// function.  If the function is a yielding function, that padding
  /// is of course interrupted by the YieldToParent field.
  AsyncContextFlags Flags;

  AsyncContext(AsyncContextFlags flags,
               TaskContinuationFunction *resumeParent,
               ExecutorRef resumeParentExecutor,
               AsyncContext *parent)
    : Parent(parent), ResumeParent(resumeParent),
      ResumeParentExecutor(resumeParentExecutor),
      Flags(flags) {}

  AsyncContext(const AsyncContext &) = delete;
  AsyncContext &operator=(const AsyncContext &) = delete;
};

/// An async context that supports yielding.
class YieldingAsyncContext : public AsyncContext {
public:
  /// The function to call to temporarily resume running in the
  /// parent context.  Generally this means a semantic yield.
  TaskContinuationFunction * __ptrauth_swift_async_context_yield
    YieldToParent;

  /// The executor that the parent context needs to be yielded to on.
  ExecutorRef YieldToParentExecutor;

  YieldingAsyncContext(AsyncContextFlags flags,
                       TaskContinuationFunction *resumeParent,
                       ExecutorRef resumeParentExecutor,
                       TaskContinuationFunction *yieldToParent,
                       ExecutorRef yieldToParentExecutor,
                       AsyncContext *parent)
    : AsyncContext(flags, resumeParent, resumeParentExecutor, parent),
      YieldToParent(yieldToParent),
      YieldToParentExecutor(yieldToParentExecutor) {}

  static bool classof(const AsyncContext *context) {
    return context->Flags.getKind() == AsyncContextKind::Yielding;
  }
};

/// An asynchronous context within a task that describes a general "Future".
/// task.
///
/// This type matches the ABI of a function `<T> () async throws -> T`, which
/// is the type used by `Task.runDetached` and `Task.group.add` to create
/// futures.
class FutureAsyncContext : public AsyncContext {
public:
  SwiftError *errorResult = nullptr;
  OpaqueValue *indirectResult;


  // TODO: this is to support "offer into queue on complete"
  AsyncContext *parentChannel = nullptr; // TODO: no idea if we need this or not

  using AsyncContext::AsyncContext;
};

} // end namespace swift

#endif
