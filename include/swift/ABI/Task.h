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
#include "swift/ABI/Executor.h"
#include "swift/ABI/HeapObject.h"
#include "swift/ABI/Metadata.h"
#include "swift/ABI/MetadataValues.h"
#include "swift/Runtime/Config.h"
#include "swift/Basic/STLExtras.h"
#include "bitset"
#include "string"
#include "queue"

namespace swift {
class AsyncTask;
class AsyncContext;
class Job;
struct OpaqueValue;
struct SwiftError;
class TaskStatusRecord;

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
///
/// ### Fragments
/// An AsyncTask may have the following fragments:
///
///    +------------------+
///    | childFragment?   |
///    | groupFragment?   |
///    | futureFragment?  |*
///    +------------------+
///
/// The future fragment is dynamic in size, based on the future result type
/// it can hold, and thus must be the *last* fragment.
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

  // TODO: rename? all other functions are `is...` rather than `has...Fragment`
  bool hasChildFragment() const { return Flags.task_isChildTask(); }

  ChildFragment *childFragment() {
    assert(hasChildFragment());
    return reinterpret_cast<ChildFragment*>(this + 1);
  }

  // ==== TaskGroup ------------------------------------------------------------

  class GroupFragment {
  public:
    /// Describes the status of the channel.
    enum class ReadyStatus : uintptr_t {
        /// The channel is empty, no tasks are pending.
        /// Return immediately, there is no point in suspending.
        ///
        /// The storage is not accessible.
        Empty = 0b00,

        /// The future has completed with result (of type \c resultType).
        Success = 0b10,

        /// The future has completed by throwing an error (an \c Error
        /// existential).
        Error = 0b11,
    };

    /// Describes the status of the waiting task that is suspended on `next()`.
    enum class WaitStatus : uintptr_t {
        Waiting = 0,
    };

    enum class GroupPollStatus : uintptr_t {
        /// The channel is known to be empty and we can immediately return nil.
        Empty = 0,

        /// The task has been enqueued to the channels wait queue.
        Waiting = 1,

        /// The task has completed with result (of type \c resultType).
        Success = 2,

        /// The task has completed by throwing an error (an \c Error
        /// existential).
        Error = 3,
    };

    /// The result of waiting on a Channel (TaskGroup).
    struct GroupPollResult {
        GroupPollStatus status; // TODO: pack it into storage pointer or not worth it?

        /// Storage for the result of the future.
        ///
        /// When the future completed normally, this is a pointer to the storage
        /// of the result value, which lives inside the future task itself.
        ///
        /// When the future completed by throwing an error, this is the error
        /// object itself.
        OpaqueValue *storage;

        /// Optional, the completed task that was polled out of the ready queue.
        ///
        /// # Important: swift_release
        /// If if a task is returned here, the task MUST be swift_release'd
        /// once we are done with it, to balance out the retain made before
        /// when the task was enqueued into the ready queue to keep it alive
        /// until a next() call eventually picks it up.
        AsyncTask *retainedTask;

        bool isStorageAccessible() {
          return status == GroupPollStatus::Success ||
              status == GroupPollStatus::Error ||
              status == GroupPollStatus::Empty;
        }

        static GroupPollResult get(AsyncTask *asyncTask, bool hadErrorResult,
                                   bool needsSwiftRelease) {
          auto fragment = asyncTask->futureFragment();
          return GroupPollResult{
              /*status*/ hadErrorResult ?
                  GroupFragment::GroupPollStatus::Error :
                  GroupFragment::GroupPollStatus::Success,
              /*storage*/ hadErrorResult ?
                  reinterpret_cast<OpaqueValue *>(fragment->getError()) :
                  fragment->getStoragePtr(),
              /*task*/ needsSwiftRelease ?
                  asyncTask :
                  nullptr
          };
        }
    };

    /// An item within the message queue of a channel.
    struct ReadyQueueItem {
        /// Mask used for the low status bits in a message queue item.
        static const uintptr_t statusMask = 0x03;

        uintptr_t storage;

        ReadyStatus getStatus() const {
          return static_cast<ReadyStatus>(storage & statusMask);
        }

        AsyncTask *getTask() const {
          return reinterpret_cast<AsyncTask *>(storage & ~statusMask);
        }

        static ReadyQueueItem get(ReadyStatus status, AsyncTask *task) {
          assert(task == nullptr || task->isFuture());
          return ReadyQueueItem{
              reinterpret_cast<uintptr_t>(task) | static_cast<uintptr_t>(status)};
        }
    };

    /// An item within the wait queue, which includes the status and the
    /// head of the list of tasks.
    struct WaitQueueItem { // TODO: reuse the future's wait queue instead?
        /// Mask used for the low status bits in a wait queue item.
        static const uintptr_t statusMask = 0x03;

        uintptr_t storage;

        WaitStatus getStatus() const {
          return static_cast<WaitStatus>(storage & statusMask);
        }

        AsyncTask *getTask() const {
          return reinterpret_cast<AsyncTask *>(storage & ~statusMask);
        }

        static WaitQueueItem get(WaitStatus status, AsyncTask *task) {
          return WaitQueueItem{
              reinterpret_cast<uintptr_t>(task) | static_cast<uintptr_t>(status)};
        }
    };

    struct GroupStatus {
        static const unsigned long maskReady      = 0x00FFFFF0000000000l;
        static const unsigned long oneReadyTask   = 0x00000010000000000l;

        static const unsigned long maskPending    = 0x0000000FFFFF00000l;
        static const unsigned long onePendingTask = 0x00000000000100000l;

        static const unsigned long maskWaiting    = 0x000000000000FFFFFl;
        static const unsigned long oneWaitingTask = 0x00000000000000001l;

        unsigned long status;

        unsigned int readyTasks() {
          return (status & maskReady) >> 40;
        }

        unsigned int pendingTasks() {
          return (status & maskPending) >> 20;
        }

        unsigned int waitingTasks() {
          return status & maskWaiting;
        }

        bool isEmpty() {
          return pendingTasks() == 0;
        }

        /// Status value decrementing the Ready, Pending and Waiting counters by one.
        GroupStatus completingReadyPendingWaitingTask() {
          assert(pendingTasks() > 0 && "can only complete waiting tasks when pending tasks available");
          assert(readyTasks() > 0 && "can only complete waiting tasks when ready tasks available");
          assert(waitingTasks() > 0 && "can only complete waiting tasks when waiting tasks available");
          return GroupStatus { status - oneReadyTask - oneWaitingTask - onePendingTask };
        }

        /// Pretty prints the status, as follows:
        /// GroupStatus{ P:{pending tasks} W:{waiting tasks} {binary repr} }
        std::string to_string() {
          std::string str;
          str.append("GroupStatus{ ");
          str.append("R:");
          str.append(std::to_string(readyTasks()));
          str.append(" P:");
          str.append(std::to_string(pendingTasks()));
          str.append(" W:");
          str.append(std::to_string(waitingTasks()));
          str.append(" " + std::bitset<64>(status).to_string());
          str.append(" }");
          return str;
        }

        /// Initially there are no waiting and no pending tasks.
        static const GroupStatus initial() {
          return GroupStatus { 0 };
        };
    };

    template<typename T>
    class NaiveQueue {
        std::queue<T> queue;

    public:
        NaiveQueue() = default;
        NaiveQueue(const NaiveQueue<T> &) = delete ;
        NaiveQueue& operator=(const NaiveQueue<T> &) = delete ;

        NaiveQueue(NaiveQueue<T>&& other) {
          queue = std::move(other.queue);
        }

        virtual ~NaiveQueue() { }

        bool dequeue(T &output) {
          if (queue.empty()) {
            return false;
          }
          output = queue.front();
          queue.pop();
          return true;
        }

        void enqueue(const T item) {
          queue.push(item);
        }

    };

  private:

    // TODO: move to lockless via the status atomic
    mutable std::mutex mutex;

    /// Used for queue management, counting number of waiting and ready tasks
    std::atomic<unsigned long> status;

    /// Queue containing completed tasks offered into this channel.
    ///
    /// The low bits contain the status, the rest of the pointer is the
    /// AsyncTask.
    NaiveQueue<ReadyQueueItem> readyQueue;
//     mpsc_queue_t<ReadyQueueItem> readyQueue; // TODO: can we get away with an MPSC queue here once actor executors land?

    /// Queue containing all of the tasks that are waiting in `get()`.
    ///
    /// A task group is also a future, and awaits on the group's result *itself*
    /// are enqueued on its future fragment.
    ///
    /// The low bits contain the status, the rest of the pointer is the
    /// AsyncTask.
    std::atomic<WaitQueueItem> waitQueue;

    friend class AsyncTask;

  public:
    explicit GroupFragment()
        : status(GroupStatus::initial().status),
          readyQueue(),
//          readyQueue(ReadyQueueItem::get(ReadyStatus::Empty, nullptr)),
          waitQueue(WaitQueueItem::get(WaitStatus::Waiting, nullptr)) {}

    /// Destroy the storage associated with the channel.
    void destroy();

    bool isEmpty() {
      auto oldStatus = GroupStatus { status.load(std::memory_order_relaxed) };
      return oldStatus.pendingTasks() == 0;
    }

    /// Returns *assumed* new status, including the just performed +1.
    GroupStatus statusAddReadyTaskAcquire() {
      auto old = status.fetch_add(GroupStatus::oneReadyTask, std::memory_order_acquire);
      auto s = GroupStatus {old + GroupStatus::oneReadyTask };
      assert(s.readyTasks() <= s.pendingTasks());
      return s;
    }

    /// Returns *assumed* new status, including the just performed +1.
    GroupStatus statusAddPendingTaskRelaxed() {
      auto old = status.fetch_add(GroupStatus::onePendingTask, std::memory_order_relaxed);
      return GroupStatus {old + GroupStatus::onePendingTask };
    }

    /// Returns *assumed* new status, including the just performed +1.
    GroupStatus statusAddWaitingTaskAcquire() {
      auto old = status.fetch_add(GroupStatus::oneWaitingTask, std::memory_order_acquire);
      return GroupStatus { old + GroupStatus::oneWaitingTask };
    }

    /// Remove waiting task, without taking any pending task.
    GroupStatus statusRemoveWaitingTask() {
      return GroupStatus {
          status.fetch_sub(GroupStatus::oneWaitingTask, std::memory_order_relaxed)
      };
    }

    /// Compare-and-set old status to a status derived from the old one,
    /// by simultaneously decrementing one Pending and one Waiting tasks.
    ///
    /// This is used to atomically perform a waiting task completion.
    bool statusCompleteReadyPendingWaitingTasks(GroupStatus& old) {
      return status.compare_exchange_weak(
          old.status, old.completingReadyPendingWaitingTask().status,
          /*success*/ std::memory_order_relaxed,
          /*failure*/ std::memory_order_relaxed);
    }

  };

  bool isTaskGroup() const { return Flags.task_isTaskGroup(); }

  GroupFragment *groupFragment() {
    assert(isTaskGroup());

    if (hasChildFragment()) {
      return reinterpret_cast<GroupFragment *>(
          reinterpret_cast<ChildFragment*>(this + 1) + 1);
    }

    return reinterpret_cast<GroupFragment *>(this + 1);
  }

  /// Offer result of a task into this channel.
  /// The value is enqueued at the end of the channel.
  void groupOffer(AsyncTask *completed, AsyncContext *context, ExecutorRef executor);

  /// Attempt to dequeue ready tasks and complete the waitingTask.
  ///
  /// If unable to complete the waiting task immediately (with an readily
  /// available completed task), either returns an `GroupPollStatus::Empty`
  /// result if it is known that no pending tasks in the group,
  /// or a `GroupPollStatus::Waiting` result if there are tasks in flight
  /// and the waitingTask eventually be woken up by a completion.
  GroupFragment::GroupPollResult groupPoll(AsyncTask *waitingTask);

  // ==== TaskGroup Child ------------------------------------------------------

  // Checks if task is a child of a TaskGroup task.
  //
  // A child task that is a group child knows that it's parent is a group
  // and therefore may `groupOffer` to it upon completion.
  bool isTaskGroupChild() {
    return hasChildFragment() && childFragment()->getParent()->isTaskGroup();
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

    auto offset = reinterpret_cast<uintptr_t>(this); // TODO: char* instead?
    offset += sizeof(AsyncTask);

    if (hasChildFragment()) {
      offset += sizeof(ChildFragment);
    }

    if (isTaskGroup()) {
      offset += sizeof(GroupFragment);
    }

    return reinterpret_cast<FutureFragment *>(offset);
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

  /// Perform a return from this context.
  ///
  /// Generally this should be tail-called.
  SWIFT_CC(swiftasync)
  void resumeParent(AsyncTask *task, ExecutorRef executor) {
    // TODO: destroy context before returning?
    // FIXME: force tail call
    return ResumeParent(task, executor, Parent);
  }
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

  using AsyncContext::AsyncContext;
};

} // end namespace swift

#endif
