//===--- TaskGroup.cpp - Task Groups --------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Object management for child tasks that are children of a task group.
//
//===----------------------------------------------------------------------===//

#include "../CompatibilityOverride/CompatibilityOverride.h"

#include "Debug.h"
#include "TaskGroupPrivate.h"
#include "TaskPrivate.h"
#include "bitset"
#include "queue" // TODO: remove and replace with usage of our mpsc queue
#include "string"
#include "swift/ABI/HeapObject.h"
#include "swift/ABI/Metadata.h"
#include "swift/ABI/Task.h"
#include "swift/ABI/TaskGroup.h"
#include "swift/Basic/RelativePointer.h"
#include "swift/Basic/STLExtras.h"
#include "swift/Runtime/Concurrency.h"
#include "swift/Runtime/Config.h"
#include "swift/Runtime/HeapObject.h"
#include "swift/Threading/Mutex.h"
#include <atomic>
#include <new>

#if !SWIFT_STDLIB_SINGLE_THREADED_CONCURRENCY
#include <mutex>
#endif

#include <assert.h>
#if SWIFT_CONCURRENCY_ENABLE_DISPATCH
#include <dispatch/dispatch.h>
#endif

#if !defined(_WIN32) && !defined(__wasi__) && __has_include(<dlfcn.h>)
#include <dlfcn.h>
#endif

using namespace swift;

#if 1
#define SWIFT_TASK_GROUP_DEBUG_LOG(group, fmt, ...) \
fprintf(stderr, "[%#lx] [%s:%d](%s) group(%p%s) " fmt "\n",             \
      (unsigned long)Thread::current().platformThreadId(),              \
      __FILE__, __LINE__, __FUNCTION__,                                 \
      group, group->isDiscardingResults() ? ",discardResults" : "",     \
      __VA_ARGS__)
#else
#define SWIFT_TASK_GROUP_DEBUG_LOG(group, fmt, ...) (void)0
#endif

using FutureFragment = AsyncTask::FutureFragment;

namespace {
class TaskStatusRecord;
struct TaskGroupStatus;

/******************************************************************************/
/*************************** TASK GROUP BASE **********************************/
/******************************************************************************/

class TaskGroupBase : public TaskGroupTaskStatusRecord {
protected:
#if SWIFT_STDLIB_SINGLE_THREADED_CONCURRENCY || SWIFT_CONCURRENCY_TASK_TO_THREAD_MODEL
  // Synchronization is simple here. In a single threaded mode, all swift tasks
  // run on a single thread so no coordination is needed. In a task-to-thread
  // model, only the parent task which created the task group can
  //
  //   (a) add child tasks to a group
  //   (b) run the child tasks
  //
  // So we shouldn't need to worry about coordinating between child tasks and
  // parents in a task group
  void lock() const {}
  void unlock() const {}
#else
  // TODO: move to lockless via the status atomic (make readyQueue an mpsc_queue_t<ReadyQueueItem>)
  mutable std::mutex mutex_;

  void lock() const { mutex_.lock(); }
  void unlock() const { mutex_.unlock(); }
#endif

  /// Used for queue management, counting number of waiting and ready tasks
  std::atomic<uint64_t> status;

  explicit TaskGroupBase(uint64_t initialStatus)
    : TaskGroupTaskStatusRecord(),
      status(initialStatus) {}

public:
  virtual ~TaskGroupBase() {}

  /// Describes the status of the group.
  enum class ReadyStatus : uintptr_t {
    /// The task group is empty, no tasks are pending.
    /// Return immediately, there is no point in suspending.
    ///
    /// The storage is not accessible.
    Empty = 0b00,

    /// A raw SwiftError is stored in the item's storage, rather than a Task with an Error inside.
    ///
    /// Only used by DiscardingTaskGroupBase.
    RawError = 0b01,

    /// The future has completed with result (of type \c resultType).
    ///
    /// Only used by AccumulatingTaskGroupBase.
    Success = 0b10,

    /// The future has completed by throwing an error (an \c Error existential).
    ///
    /// Only used by AccumulatingTaskGroupBase.
    Error = 0b11,
  };

  /// Status of a poll, i.e. is there a result we can return, or do we have to suspend.
  enum class PollStatus : uintptr_t {
    /// The group is known to be empty and we can immediately return nil.
    Empty = 0b00,

    /// The task has been enqueued to the groups wait queue.
    MustWait = 0b01,

    /// The task has completed with result (of type \c resultType).
    Success = 0b10,

    /// The task has completed by throwing an error (an \c Error existential).
    Error = 0b11,
  };

    /// The result of waiting on a task group.
    struct PollResult {
      PollStatus status; // TODO: pack it into storage pointer or not worth it?

      /// Storage for the result of the future.
      ///
      /// When the future completed normally, this is a pointer to the storage
      /// of the result value, which lives inside the future task itself.
      ///
      /// When the future completed by throwing an error, this is the error
      /// object itself.
      OpaqueValue *storage;

      const Metadata *successType;

      /// The completed task, if necessary to keep alive until consumed by next().
      ///
      /// # Important: swift_release
      /// If if a task is returned here, the task MUST be swift_released
      /// once we are done with it, to balance out the retain made before
      /// when the task was enqueued into the ready queue to keep it alive
      /// until a next() call eventually picks it up.
      AsyncTask *retainedTask;

      static PollResult get(AsyncTask *asyncTask, bool hadErrorResult) {
        auto fragment = asyncTask->futureFragment();
        return PollResult{
            /*status*/ hadErrorResult ?
                       PollStatus::Error :
                       PollStatus::Success,
            /*storage*/ hadErrorResult ?
                        reinterpret_cast<OpaqueValue *>(fragment->getError()) :
                        fragment->getStoragePtr(),
            /*successType*/fragment->getResultType(),
            /*task*/ asyncTask
        };
      }

      static PollResult getEmpty(const Metadata *successType) {
        return PollResult{
            /*status*/PollStatus::Empty,
            /*storage*/nullptr,
            /*successType*/successType,
            /*task*/nullptr
        };
      }

      static PollResult getError(SwiftError *error) {
        assert(error);
        return PollResult{
            /*status*/PollStatus::Error,
            /*storage*/reinterpret_cast<OpaqueValue *>(error),
            /*successType*/nullptr,
            /*task*/nullptr
        };
      }
    };


  /// Destroy the storage associated with the group.
  virtual void destroy() = 0;

  bool isAccumulatingResults() const {
    return !isDiscardingResults();
  }
  virtual bool isDiscardingResults() const = 0;

  /// Any TaskGroup always IS its own TaskRecord.
  /// This allows us to easily get the group while cancellation is propagated throughout the task tree.
  TaskGroupTaskStatusRecord *getTaskRecord() {
    return reinterpret_cast<TaskGroupTaskStatusRecord *>(this);
  }

  // ==== Queue operations ----------------------------------------------------

  /// Offer result of a task into this task group.
  ///
  /// If possible, and an existing task is already waiting on next(), this will
  /// schedule it immediately. If not, the result is enqueued and will be picked
  /// up whenever a task calls next() the next time.
  virtual void offer(AsyncTask *completed, AsyncContext *context) = 0;

  /// Attempt to park the `waitingTask` in the waiting queue.
  ///
  /// If unable to complete the waiting task immediately (with an readily
  /// available completed task), either returns an `PollStatus::Empty`
  /// result if it is known that there are no pending tasks in the group,
  /// or a `PollStatus::MustWait` result if there are tasks in flight
  /// and the waitingTask eventually be woken up by a completion.
  ///
  /// A `discardResults` TaskGroup is not able to wait on individual completions,
  /// instead, it can only await on "all pending tasks have been processed".
  ///
  /// There can be only at-most-one waiting task on a group at any given time,
  /// and the waiting task is expected to be the parent task in which the group
  /// body is running.
  virtual PollResult tryEnqueueWaitingTask(AsyncTask *waitingTask) = 0;

  // ==== Status manipulation -------------------------------------------------

  std::string statusString() const;

  bool isEmpty() const;

  uint64_t pendingTasks() const;

  /// Cancel the task group and all tasks within it.
  ///
  /// Returns `true` if this is the first time cancelling the group, false otherwise.
  bool isCancelled() const;

  /// Set waiting status bit.
  ///
  /// Returns *assumed* new status, including the just performed +1.
  TaskGroupStatus statusMarkWaitingAssumeAcquire();

  /// Remove waiting status bit.
  TaskGroupStatus statusRemoveWaitingRelease();


  /// Cancels the group and returns true if was already cancelled before.
  /// After this function returns, the group is guaranteed to be cancelled.
  ///
  /// Prefer calling cancelAll if the intent is to cancel the group and all of its children.
  ///
  /// \return true, if the group was already cancelled before, and false if it wasn't cancelled before (but now is).
  bool statusCancel();

  /// Cancel the group and all of its child tasks recursively.
  /// This also sets
  bool cancelAll();

  virtual TaskGroupStatus statusAddPendingTaskRelaxed(bool unconditionally) = 0;
};

/// The status of a task group.
///
/// Its exact structure depends on the type of group, and therefore a group must be passed to operations
/// which may be touching the 'ready' bits; Only an "accumulating" task group maintains the 'ready' count,
/// while all kinds of group use the 'pending' count (with varying width though).
///
/// Accumulating group status:
///     [1:cancelled][1:waiting][31:ready count][31:pending count]
/// Discarding group status:
///     [1:cancelled][1:waiting][62:pending count]
struct TaskGroupStatus {
  static const uint64_t cancelled               = 0b1000000000000000000000000000000000000000000000000000000000000000;
  static const uint64_t waiting                 = 0b0100000000000000000000000000000000000000000000000000000000000000;

  // 31 bits for ready tasks counter
  static const uint64_t maskReady               = 0b0011111111111111111111111111111110000000000000000000000000000000;
  static const uint64_t oneReadyTask            = 0b0000000000000000000000000000000010000000000000000000000000000000;

  // 31 bits for pending tasks counter, while accumulating results (default mode)
  static const uint64_t maskAccumulatingPending = 0b0000000000000000000000000000000001111111111111111111111111111111;
  // 62 bits for pending tasks counter, while discarding results (discardResults)
  static const uint64_t maskDiscardingPending   = 0b0011111111111111111111111111111111111111111111111111111111111111;
  static const uint64_t onePendingTask          = 0b0000000000000000000000000000000000000000000000000000000000000001;

  uint64_t status;

  bool isCancelled() {
    return (status & cancelled) > 0;
  }

  bool hasWaitingTask() {
    return (status & waiting) > 0;
  }

  unsigned int readyTasks(const TaskGroupBase* _Nonnull group) {
    assert(group->isAccumulatingResults()
           && "attempted to check ready tasks on group that does not accumulate results!");
    return (status & maskReady) >> 31;
  }

  uint64_t pendingTasks(const TaskGroupBase* _Nonnull group) {
    if (group->isAccumulatingResults()) {
      return (status & maskAccumulatingPending);
    } else {
      return (status & maskDiscardingPending);
    }
  }

  bool isEmpty(const TaskGroupBase *group) {
    return pendingTasks(group) == 0;
  }

  /// Status value decrementing the Ready, Pending and Waiting counters by one.
  TaskGroupStatus completingPendingReadyWaiting(const TaskGroupBase* _Nonnull group) {
    assert(pendingTasks(group) &&
           "can only complete waiting task when pending tasks available");
    assert(group->isDiscardingResults() || readyTasks(group) &&
                                           "can only complete waiting task when ready tasks available");
    assert(hasWaitingTask() &&
           "can only complete waiting task when waiting task available");
    uint64_t change = waiting + onePendingTask;
    // only while accumulating results does the status contain "ready" bits;
    // so if we're in "discard results" mode, we must not decrement the ready count,
    // as there is no ready count in the status.
    change += group->isAccumulatingResults() ? oneReadyTask : 0;
    return TaskGroupStatus{status - change};
  }

  TaskGroupStatus completingPendingReady(const TaskGroupBase* _Nonnull group) {
    assert(pendingTasks(group) &&
           "can only complete waiting task when pending tasks available");
    assert(group->isDiscardingResults() || readyTasks(group) &&
                                           "can only complete waiting task when ready tasks available");
    auto change = onePendingTask;
    change += group->isAccumulatingResults() ? oneReadyTask : 0;
    return TaskGroupStatus{status - change};
  }

  TaskGroupStatus asCancelled(bool cancel) {
    return TaskGroupStatus{status | (cancel ? cancelled : 0)};
  }

  /// Pretty prints the status, as follows:
  /// If accumulating results:
  ///     TaskGroupStatus{ C:{cancelled} W:{waiting task} R:{ready tasks} P:{pending tasks} {binary repr} }
  /// If discarding results:
  ///     TaskGroupStatus{ C:{cancelled} W:{waiting task} P:{pending tasks} {binary repr} }
  std::string to_string(const TaskGroupBase* _Nonnull group) {
    std::string str;
    str.append("TaskGroupStatus{ ");
    str.append("C:"); // cancelled
    str.append(isCancelled() ? "y" : "n");
    str.append(" W:"); // has waiting task
    str.append(hasWaitingTask() ? "y" : "n");
    if (group && group->isAccumulatingResults()) {
      str.append(" R:"); // ready
      str.append(std::to_string(readyTasks(group)));
    }
    str.append(" P:"); // pending
    str.append(std::to_string(pendingTasks(group)));
    str.append(" " + std::bitset<64>(status).to_string());
    str.append(" }");
    return str;
  }

  /// Initially there are no waiting and no pending tasks.
  static const TaskGroupStatus initial() {
    return TaskGroupStatus{0};
  };
};

bool TaskGroupBase::isCancelled() const {
  auto old = TaskGroupStatus{status.load(std::memory_order_relaxed)};
  return old.isCancelled();
}

std::string TaskGroupBase::statusString() const {
  auto old = TaskGroupStatus{status.load(std::memory_order_relaxed)};
  return old.to_string(this);
}

bool TaskGroupBase::isEmpty() const {
  auto oldStatus = TaskGroupStatus{status.load(std::memory_order_relaxed)};
  return oldStatus.pendingTasks(this) == 0;
}

uint64_t TaskGroupBase::pendingTasks() const {
  auto s = TaskGroupStatus{status.load(std::memory_order_relaxed)};
  return s.pendingTasks(this);
}

TaskGroupStatus TaskGroupBase::statusMarkWaitingAssumeAcquire() {
  auto old = status.fetch_or(TaskGroupStatus::waiting, std::memory_order_acquire);
  return TaskGroupStatus{old | TaskGroupStatus::waiting};
}

TaskGroupStatus TaskGroupBase::statusRemoveWaitingRelease() {
  auto old = status.fetch_and(~TaskGroupStatus::waiting,
                              std::memory_order_release);
  return TaskGroupStatus{old};
}

bool TaskGroupBase::statusCancel() {
  /// The cancelled bit is always the same, the first one, between all task group implementations:
  const uint64_t cancelled = TaskGroupStatus::cancelled;
  auto old = status.fetch_or(cancelled, std::memory_order_relaxed);

  // return if the status was already cancelled before we flipped it or not
  return old & cancelled;
}

/*****************************************************************************/
/************************** QUEUE IMPL ***************************************/
/*****************************************************************************/

template<typename T>
class NaiveTaskGroupQueue {
  std::queue <T> queue;

public:
  NaiveTaskGroupQueue() = default;

  NaiveTaskGroupQueue(const NaiveTaskGroupQueue<T> &) = delete;

  NaiveTaskGroupQueue &operator=(const NaiveTaskGroupQueue<T> &) = delete;

  NaiveTaskGroupQueue(NaiveTaskGroupQueue<T> &&other) {
    queue = std::move(other.queue);
  }

  virtual ~NaiveTaskGroupQueue() {}

  bool dequeue(T &output) {
    if (queue.empty()) {
      return false;
    }
    output = queue.front();
    queue.pop();
    return true;
  }

  bool isEmpty() const {
    return queue.empty();
  }

  void enqueue(const T item) {
    queue.push(item);
  }
};

/******************************************************************************/
/*************** ACCUMULATING (DEFAULT) TASK GROUP ****************************/
/******************************************************************************/

/// The default TaskGroup implementation, which accumulates results until they are consumed using `await next()`.
class AccumulatingTaskGroupBase: public TaskGroupBase {
public:

  /// An item within the message queue of a group.
  struct ReadyQueueItem {
    /// Mask used for the low status bits in a message queue item.
    static const uintptr_t statusMask = 0x03;

    uintptr_t storage;

    ReadyStatus getStatus() const {
      return static_cast<ReadyStatus>(storage & statusMask);
    }

    AsyncTask *getTask() const {
      assert(getStatus() != ReadyStatus::RawError && "storage did contain raw error pointer, not task!");
      return reinterpret_cast<AsyncTask *>(storage & ~statusMask);
    }

    SwiftError *getRawError() const {
      assert(getStatus() == ReadyStatus::RawError && "storage did not contain raw error pointer!");
      return reinterpret_cast<SwiftError *>(storage & ~statusMask);
    }

    static ReadyQueueItem get(ReadyStatus status, AsyncTask *task) {
      assert(task == nullptr || task->isFuture());
      return ReadyQueueItem{
        reinterpret_cast<uintptr_t>(task) | static_cast<uintptr_t>(status)};
    }

    static ReadyQueueItem getRawError(SwiftError *error) {
      return ReadyQueueItem{
        reinterpret_cast<uintptr_t>(error) | static_cast<uintptr_t>(ReadyStatus::RawError)};
    }
  };

  /// An item within the pending queue.
  struct PendingQueueItem {
    uintptr_t storage;

    AsyncTask *getTask() const {
      return reinterpret_cast<AsyncTask *>(storage);
    }

    static ReadyQueueItem get(AsyncTask *task) {
      assert(task == nullptr || task->isFuture());
      return ReadyQueueItem{reinterpret_cast<uintptr_t>(task)};
    }
  };

private:
  /// Queue containing completed tasks offered into this group.
  ///
  /// The low bits contain the status, the rest of the pointer is the
  /// AsyncTask.
  NaiveTaskGroupQueue<ReadyQueueItem> readyQueue;

  /// The task currently waiting on `group.next()`.  Since only the owning
  /// task can ever be waiting on a group, this is just either a reference
  /// to that task or null.
  std::atomic<AsyncTask *> waitQueue;

  const Metadata *successType;

  friend class ::swift::AsyncTask;

public:

  explicit AccumulatingTaskGroupBase(const Metadata *T)
    : TaskGroupBase(TaskGroupStatus::initial().status),
      readyQueue(),
      waitQueue(nullptr),
      successType(T) {}

  virtual void destroy() override;

  virtual bool isDiscardingResults() const override {
    return false;
  }

  /// Returns *assumed* new status.
  ///
  /// If the group is not accumulating results, the "ready" count does not exist,
  /// and this is just a plan load().
  TaskGroupStatus statusAddReadyAssumeAcquire() {
    auto old = status.fetch_add(TaskGroupStatus::oneReadyTask,
                                std::memory_order_acquire);
    auto s = TaskGroupStatus{old + TaskGroupStatus::oneReadyTask};
    assert(s.readyTasks(this) <= s.pendingTasks(this));
    return s;
  }

  /// Add a single pending task to the status counter.
  /// This is used to implement next() properly, as we need to know if there
  /// are pending tasks worth suspending/waiting for or not.
  ///
  /// Note that the group does *not* store child tasks at all, as they are
  /// stored in the `TaskGroupTaskStatusRecord` inside the current task, that
  /// is currently executing the group. Here we only need the counts of
  /// pending/ready tasks.
  ///
  /// If the `unconditionally` parameter is `true` the operation always successfully
  /// adds a pending task, even if the group is cancelled. If the unconditionally
  /// flag is `false`, the added pending count will be *reverted* before returning.
  /// This is because we will NOT add a task to a cancelled group, unless doing
  /// so unconditionally.
  ///
  /// Returns *assumed* new status, including the just performed +1.
  TaskGroupStatus statusAddPendingTaskRelaxed(bool unconditionally) {
    auto old = status.fetch_add(TaskGroupStatus::onePendingTask,
                                std::memory_order_relaxed);
    auto s = TaskGroupStatus{old + TaskGroupStatus::onePendingTask};

    if (!unconditionally && s.isCancelled()) {
      // revert that add, it was meaningless
      auto o = status.fetch_sub(TaskGroupStatus::onePendingTask,
                                std::memory_order_relaxed);
      s = TaskGroupStatus{o - TaskGroupStatus::onePendingTask};
    }

    return s;
  }

  TaskGroupStatus statusLoadRelaxed() {
    return TaskGroupStatus{status.load(std::memory_order_relaxed)};
  }

  /// Compare-and-set old status to a status derived from the old one,
  /// by simultaneously decrementing one Pending and one Waiting tasks.
  ///
  /// This is used to atomically perform a waiting task completion.
  bool statusCompletePendingReadyWaiting(TaskGroupStatus &old) {
    return status.compare_exchange_strong(
      old.status, old.completingPendingReadyWaiting(this).status,
      /*success*/ std::memory_order_relaxed,
      /*failure*/ std::memory_order_relaxed);
  }

  /// Decrement the pending status count.
  /// Returns the *assumed* new status, including the just performed -1.
  TaskGroupStatus statusCompletePendingAssumeRelease() {
    assert(this->isDiscardingResults()
           && "only a discardResults TaskGroup may use completePending, "
              "since it avoids updating the ready count, which other groups need.");
    auto old = status.fetch_sub(TaskGroupStatus::onePendingTask,
                                std::memory_order_release);
    assert(TaskGroupStatus{old}.pendingTasks(this) > 0 && "attempted to decrement pending count when it was 0 already");
    return TaskGroupStatus{old - TaskGroupStatus::onePendingTask};
  }

  virtual void offer(AsyncTask *completed, AsyncContext *context) override;

  /// Attempt to dequeue ready tasks and complete the waitingTask.
  ///
  /// If unable to complete the waiting task immediately (with an readily
  /// available completed task), either returns an `PollStatus::Empty`
  /// result if it is known that no pending tasks in the group,
  /// or a `PollStatus::MustWait` result if there are tasks in flight
  /// and the waitingTask eventually be woken up by a completion.
  PollResult poll(AsyncTask *waitingTask);

  /// Attempt to store the waiting task, though if there is no pending tasks to wait for,
  /// or we're ready to complete the waiting task immediately, the PollResult will inform about that.
  virtual PollResult tryEnqueueWaitingTask(AsyncTask *waitingTask) override;

private:
  // Enqueue the completed task onto ready queue if there are no waiting tasks yet
  void enqueueCompletedTask(AsyncTask *completedTask, bool hadErrorResult);

  /// Resume waiting task with result from `completedTask`
  void resumeWaitingTask(AsyncTask *completedTask, TaskGroupStatus &assumed, bool hadErrorResult);
};

/******************************************************************************/
/********************** DISCARDING TASK GROUP *********************************/
/******************************************************************************/

class DiscardingTaskGroupBase: public TaskGroupBase {
public:
  /// An item within the message queue of a group.
  struct ReadyQueueItem {
    /// Mask used for the low status bits in a message queue item.
    static const uintptr_t statusMask = 0x03;

    uintptr_t storage;

    ReadyStatus getStatus() const {
      return static_cast<ReadyStatus>(storage & statusMask);
    }

    AsyncTask *getTask() const {
      assert(getStatus() != ReadyStatus::RawError && "storage did contain raw error pointer, not task!");
      return reinterpret_cast<AsyncTask *>(storage & ~statusMask);
    }

    SwiftError *getRawError() const {
      assert(getStatus() == ReadyStatus::RawError && "storage did not contain raw error pointer!");
      return reinterpret_cast<SwiftError *>(storage & ~statusMask);
    }

    static ReadyQueueItem get(ReadyStatus status, AsyncTask *task) {
      assert(task == nullptr || task->isFuture());
      return ReadyQueueItem{
        reinterpret_cast<uintptr_t>(task) | static_cast<uintptr_t>(status)};
    }

    static ReadyQueueItem getRawError(SwiftError *error) {
      return ReadyQueueItem{
        reinterpret_cast<uintptr_t>(error) | static_cast<uintptr_t>(ReadyStatus::RawError)};
    }
  };

  /// An item within the pending queue.
  struct PendingQueueItem {
    uintptr_t storage;

    AsyncTask *getTask() const {
      return reinterpret_cast<AsyncTask *>(storage);
    }

    static ReadyQueueItem get(AsyncTask *task) {
      assert(task == nullptr || task->isFuture());
      return ReadyQueueItem{reinterpret_cast<uintptr_t>(task)};
    }
  };

private:
  /// Queue containing completed tasks offered into this group.
  ///
  /// The low bits contain the status, the rest of the pointer is the
  /// AsyncTask.
  NaiveTaskGroupQueue<ReadyQueueItem> readyQueue;

  /// The task currently waiting on `group.next()`.  Since only the owning
  /// task can ever be waiting on a group, this is just either a reference
  /// to that task or null.
  std::atomic<AsyncTask *> waitQueue;

  const Metadata *successType;

  friend class ::swift::AsyncTask;

public:

  explicit DiscardingTaskGroupBase(const Metadata *T)
    : TaskGroupBase(TaskGroupStatus::initial().status),
      readyQueue(),
      waitQueue(nullptr),
      successType(T) {}

  virtual void destroy() override;

  virtual bool isDiscardingResults() const override {
    return true;
  }


  /// Returns *assumed* new status, including the just performed +1.
  TaskGroupStatus statusMarkWaitingAssumeAcquire() {
    auto old = status.fetch_or(TaskGroupStatus::waiting, std::memory_order_acquire);
    return TaskGroupStatus{old | TaskGroupStatus::waiting};
  }

  TaskGroupStatus statusRemoveWaitingRelease() {
    auto old = status.fetch_and(~TaskGroupStatus::waiting,
                                std::memory_order_release);
    return TaskGroupStatus{old};
  }

  /// Returns *assumed* new status.
  TaskGroupStatus statusAddReadyAssumeAcquire(const DiscardingTaskGroupBase *group) {
    assert(group->isDiscardingResults());
    return TaskGroupStatus{status.load(std::memory_order_acquire)};
  }

  /// Add a single pending task to the status counter.
  /// This is used to implement next() properly, as we need to know if there
  /// are pending tasks worth suspending/waiting for or not.
  ///
  /// Note that the group does *not* store child tasks at all, as they are
  /// stored in the `TaskGroupTaskStatusRecord` inside the current task, that
  /// is currently executing the group. Here we only need the counts of
  /// pending/ready tasks.
  ///
  /// If the `unconditionally` parameter is `true` the operation always successfully
  /// adds a pending task, even if the group is cancelled. If the unconditionally
  /// flag is `false`, the added pending count will be *reverted* before returning.
  /// This is because we will NOT add a task to a cancelled group, unless doing
  /// so unconditionally.
  ///
  /// Returns *assumed* new status, including the just performed +1.
  TaskGroupStatus statusAddPendingTaskRelaxed(bool unconditionally) {
    auto old = status.fetch_add(TaskGroupStatus::onePendingTask,
                                std::memory_order_relaxed);
    auto s = TaskGroupStatus{old + TaskGroupStatus::onePendingTask};

    if (!unconditionally && s.isCancelled()) {
      // revert that add, it was meaningless
      auto o = status.fetch_sub(TaskGroupStatus::onePendingTask,
                                std::memory_order_relaxed);
      s = TaskGroupStatus{o - TaskGroupStatus::onePendingTask};
    }

    return s;
  }

  TaskGroupStatus statusLoadRelaxed() {
    return TaskGroupStatus{status.load(std::memory_order_relaxed)};
  }

  TaskGroupStatus statusLoadAcquire() {
    return TaskGroupStatus{status.load(std::memory_order_acquire)};
  }

  /// Compare-and-set old status to a status derived from the old one,
  /// by simultaneously decrementing one Pending and one Waiting tasks.
  ///
  /// This is used to atomically perform a waiting task completion.
  bool statusCompletePendingReadyWaiting(TaskGroupStatus &old) {
    return status.compare_exchange_strong(
      old.status, old.completingPendingReadyWaiting(this).status,
      /*success*/ std::memory_order_relaxed,
      /*failure*/ std::memory_order_relaxed);
  }

  /// Decrement the pending status count.
  /// Returns the *assumed* new status, including the just performed -1.
  TaskGroupStatus statusCompletePendingAssumeRelease() {
    assert(this->isDiscardingResults()
           && "only a discardResults TaskGroup may use completePending, "
              "since it avoids updating the ready count, which other groups need.");
    auto old = status.fetch_sub(TaskGroupStatus::onePendingTask,
                                std::memory_order_release);
    assert(TaskGroupStatus{old}.pendingTasks(this) > 0 && "attempted to decrement pending count when it was 0 already");
    return TaskGroupStatus{old - TaskGroupStatus::onePendingTask};
  }

  virtual void offer(AsyncTask *completed, AsyncContext *context) override;

  /// Attempt to dequeue ready tasks and complete the waitingTask.
  ///
  /// If unable to complete the waiting task immediately (with an readily
  /// available completed task), either returns an `PollStatus::Empty`
  /// result if it is known that no pending tasks in the group,
  /// or a `PollStatus::MustWait` result if there are tasks in flight
  /// and the waitingTask eventually be woken up by a completion.
  PollResult poll(AsyncTask *waitingTask);

  virtual PollResult tryEnqueueWaitingTask(AsyncTask *waitingTask) override;

  bool offerBodyError(SwiftError* _Nonnull bodyError);

private:
  // Enqueue the completed task onto ready queue if there are no waiting tasks yet
  void enqueueCompletedTask(AsyncTask *completedTask, bool hadErrorResult);

  /// Resume waiting task with result from `completedTask`
  void resumeWaitingTask(AsyncTask *completedTask, TaskGroupStatus &assumed, bool hadErrorResult);

  /// Resume waiting task with specified error
  void resumeWaitingTaskWithError(SwiftError *error, TaskGroupStatus &assumed);
};

} // end anonymous namespace

/******************************************************************************/
/************************ TASK GROUP PUBLIC API *******************************/
/******************************************************************************/

using ReadyQueueItem = AccumulatingTaskGroupBase::ReadyQueueItem;
using ReadyStatus = TaskGroupBase::ReadyStatus;
using PollResult = TaskGroupBase::PollResult;
using PollStatus = TaskGroupBase::PollStatus;

static_assert(sizeof(AccumulatingTaskGroupBase) <= sizeof(TaskGroup) &&
              alignof(AccumulatingTaskGroupBase) <= alignof(TaskGroup),
              "TaskGroupBase doesn't fit in TaskGroup");

static_assert(sizeof(DiscardingTaskGroupBase) <= sizeof(TaskGroup) &&
              alignof(DiscardingTaskGroupBase) <= alignof(TaskGroup),
              "DiscardingTaskGroupBase doesn't fit in TaskGroup");

static TaskGroupBase *asBaseImpl(TaskGroup *group) {
  return reinterpret_cast<TaskGroupBase*>(group);
}
static AccumulatingTaskGroupBase *asAccumulatingImpl(TaskGroup *group) {
  assert(group->isAccumulatingResults());
  return reinterpret_cast<AccumulatingTaskGroupBase*>(group);
}
static DiscardingTaskGroupBase *asDiscardingImpl(TaskGroup *group) {
  assert(group->isDiscardingResults());
  return reinterpret_cast<DiscardingTaskGroupBase*>(group);
}

static TaskGroup *asAbstract(TaskGroupBase *group) {
  return reinterpret_cast<TaskGroup*>(group);
}
static TaskGroup *asAbstract(AccumulatingTaskGroupBase *group) {
  return reinterpret_cast<TaskGroup*>(group);
}
static TaskGroup *asAbstract(DiscardingTaskGroupBase *group) {
  return reinterpret_cast<TaskGroup*>(group);
}

TaskGroupTaskStatusRecord *TaskGroup::getTaskRecord() {
  return asBaseImpl(this)->getTaskRecord();
}

bool TaskGroup::isDiscardingResults() {
  return asBaseImpl(this)->isDiscardingResults();
}

// =============================================================================
// ==== initialize -------------------------------------------------------------

// Initializes into the preallocated _group an actual TaskGroupBase.
SWIFT_CC(swift)
static void swift_taskGroup_initializeImpl(TaskGroup *group, const Metadata *T) {
  swift_taskGroup_initializeWithFlags(0, group, T);
}

// Initializes into the preallocated _group an actual instance.
SWIFT_CC(swift)
static void swift_taskGroup_initializeWithFlagsImpl(size_t rawGroupFlags,
                                                    TaskGroup *group, const Metadata *T) {

  TaskGroupFlags groupFlags(rawGroupFlags);
  SWIFT_TASK_DEBUG_LOG("(group(%p) create; flags: isDiscardingResults=%d",
                       group, groupFlags.isDiscardResults());

  TaskGroupBase *impl;
  if (groupFlags.isDiscardResults()) {
    impl = ::new(group) DiscardingTaskGroupBase(T);
  } else {
    impl = ::new(group) AccumulatingTaskGroupBase(T);
  }

  TaskGroupTaskStatusRecord *record = impl->getTaskRecord();
  // ok, now that the group actually is initialized: attach it to the task
  addStatusRecord(record, [&](ActiveTaskStatus parentStatus) {
    // If the task has already been cancelled, reflect that immediately in
    // the group's status.
    if (parentStatus.isCancelled()) {
      impl->statusCancel();
    }
    return true;
  });
}

// =============================================================================
// ==== child task management --------------------------------------------------

void TaskGroup::addChildTask(AsyncTask *child) {
  SWIFT_TASK_DEBUG_LOG("attach child task = %p to group = %p", child, this);

  // Add the child task to this task group.  The corresponding removal
  // won't happen until the parent task successfully polls for this child
  // task, either synchronously in poll (if a task is available
  // synchronously) or asynchronously in offer (otherwise).  In either
  // case, the work ends up being non-concurrent with the parent task.

  // The task status record lock is held during this operation, which
  // prevents us from racing with cancellation or escalation.  We don't
  // need to acquire the task group lock because the child list is only
  // accessed under the task status record lock.
  auto record = asBaseImpl(this)->getTaskRecord();
  record->attachChild(child);
}

void TaskGroup::removeChildTask(AsyncTask *child) {
  SWIFT_TASK_DEBUG_LOG("detach child task = %p from group = %p", child, this);

  auto groupRecord = asBaseImpl(this)->getTaskRecord();

  // The task status record lock is held during this operation, which
  // prevents us from racing with cancellation or escalation.  We don't
  // need to acquire the task group lock because the child list is only
  // accessed under the task status record lock.
  groupRecord->detachChild(child);
}

// =============================================================================
// ==== destroy ----------------------------------------------------------------

SWIFT_CC(swift)
static void swift_taskGroup_destroyImpl(TaskGroup *group) {
  asBaseImpl(group)->destroy();
}

void AccumulatingTaskGroupBase::destroy() {
#if SWIFT_TASK_DEBUG_LOG_ENABLED
  if (!this->isEmpty()) {
    auto status = this->statusLoadRelaxed();
    SWIFT_TASK_GROUP_DEBUG_LOG(this, "destroy, tasks .ready = %d, .pending = %llu",
                         status.readyTasks(this), status.pendingTasks(this));
  } else {
    SWIFT_TASK_DEBUG_LOG("destroying task group = %p", this);
  }
#endif
  assert(this->isEmpty() && "Attempted to destroy non-empty task group!");

  // First, remove the group from the task and deallocate the record
  removeStatusRecord(getTaskRecord());

  // No need to drain our queue here, as by the time we call destroy,
  // all tasks inside the group must have been awaited on already.
  // This is done in Swift's withTaskGroup function explicitly.

  // destroy the group's storage
  this->~AccumulatingTaskGroupBase();
}

void DiscardingTaskGroupBase::destroy() {
#if SWIFT_TASK_DEBUG_LOG_ENABLED
  if (!this->isEmpty()) {
    auto status = this->statusLoadRelaxed();
    SWIFT_TASK_GROUP_DEBUG_LOG(this, "destroy, tasks .ready = %d, .pending = %llu",
                         status.readyTasks(this), status.pendingTasks(this));
  } else {
    SWIFT_TASK_DEBUG_LOG("destroying discarding task group = %p", this);
  }
#endif
  assert(this->isEmpty() && "Attempted to destroy non-empty task group!");

  // First, remove the group from the task and deallocate the record
  removeStatusRecord(getTaskRecord());

  // No need to drain our queue here, as by the time we call destroy,
  // all tasks inside the group must have been awaited on already.
  // This is done in Swift's withTaskGroup function explicitly.

  // destroy the group's storage
  this->~DiscardingTaskGroupBase();
}

bool TaskGroup::isCancelled() {
  return asBaseImpl(this)->isCancelled();
}

// =============================================================================
// ==== offer ------------------------------------------------------------------

static void fillGroupNextErrorResult(TaskFutureWaitAsyncContext *context,
                                     SwiftError *error) {
  context->fillWithError(error);
}

static void fillGroupNextResult(TaskFutureWaitAsyncContext *context,
                                PollResult result) {
  /// Fill in the result value
  switch (result.status) {
  case PollStatus::MustWait:
    assert(false && "filling a waiting status?");
    return;

  case PollStatus::Error: {
    fillGroupNextErrorResult(context, reinterpret_cast<SwiftError *>(result.storage));
    return;
  }

  case PollStatus::Success: {
    // Initialize the result as an Optional<Success>.
    const Metadata *successType = result.successType;
    OpaqueValue *destPtr = context->successResultPointer;
    // TODO: figure out a way to try to optimistically take the
    // value out of the finished task's future, if there are no
    // remaining references to it.
    successType->vw_initializeWithCopy(destPtr, result.storage);
    successType->vw_storeEnumTagSinglePayload(destPtr, 0, 1);
    return;
  }

  case PollStatus::Empty: {
    // Initialize the result as a .none Optional<Success>.
    const Metadata *successType = result.successType;
    OpaqueValue *destPtr = context->successResultPointer;
    successType->vw_storeEnumTagSinglePayload(destPtr, 1, 1);
    return;
  }
  }
}

static void fillGroupNextNilResult(TaskFutureWaitAsyncContext *context,
                                   PollResult result) {
  // Initialize the result as a .none Optional<Success>.
  const Metadata *successType = result.successType;
  OpaqueValue *destPtr = context->successResultPointer;
  successType->vw_storeEnumTagSinglePayload(destPtr, 1, 1);
}

// TaskGroup is locked upon entry and exit
void AccumulatingTaskGroupBase::enqueueCompletedTask(AsyncTask *completedTask, bool hadErrorResult) {
  // Retain the task while it is in the queue; it must remain alive until
  // it is found by poll.  This retain will balanced by the release in poll.
  swift_retain(completedTask);

  auto readyItem = ReadyQueueItem::get(
      hadErrorResult ? ReadyStatus::Error : ReadyStatus::Success,
      completedTask
  );

  assert(completedTask == readyItem.getTask());
  assert(readyItem.getTask()->isFuture());
  readyQueue.enqueue(readyItem);
}

// TaskGroup is locked upon entry and exit
void DiscardingTaskGroupBase::enqueueCompletedTask(AsyncTask *completedTask, bool hadErrorResult) {
  if (hadErrorResult) {
    // we only store the FIRST error in discardResults mode
    if (readyQueue.isEmpty()) {
      SWIFT_TASK_GROUP_DEBUG_LOG(this, "store first error, completedTask:%p", completedTask);
      // continue handling as usual, which will perform the enqueue
    } else {
      SWIFT_TASK_GROUP_DEBUG_LOG(this, "discard error result, we already have an error stored, completedTask:%p", completedTask);
      // DO NOT RETAIN THE TASK.
      return;
    }
  } else {
    SWIFT_TASK_GROUP_DEBUG_LOG(this, "discard successful result, %p", completedTask);
    // DO NOT RETAIN THE TASK.
    // We know it is Void, so we don't need to store the result;
    // By releasing tasks eagerly we're able to keep "infinite" task groups,
    // running, that never consume their values. Even more-so,
    return;
  }

  // FIXME: de-duplicate this; Same logic as AccumulatingTaskGroupBase from here onwards:

  // Retain the task while it is in the queue; it must remain alive until
  // it is found by poll.  This retain will be balanced by the release in poll.
  swift_retain(completedTask);

  auto readyItem = ReadyQueueItem::get(
      hadErrorResult ? ReadyStatus::Error : ReadyStatus::Success,
      completedTask
  );

  assert(completedTask == readyItem.getTask());
  assert(readyItem.getTask()->isFuture());
  readyQueue.enqueue(readyItem);
}

void TaskGroup::offer(AsyncTask *completedTask, AsyncContext *context) {
  asBaseImpl(this)->offer(completedTask, context);
}

void AccumulatingTaskGroupBase::offer(AsyncTask *completedTask, AsyncContext *context) {
  assert(completedTask);
  assert(completedTask->isFuture());
  assert(completedTask->hasChildFragment());
  assert(completedTask->hasGroupChildFragment());
  assert(completedTask->groupChildFragment()->getGroup() == asAbstract(this));
  SWIFT_TASK_GROUP_DEBUG_LOG(this, "offer, completedTask:%p , status:%s", completedTask, statusString().c_str());

  // The current ownership convention is that we are *not* given ownership
  // of a retain on completedTask; we're called from the task completion
  // handler, and the task will release itself.  So if we need the task
  // to survive this call (e.g. because there isn't an immediate waiting
  // task), we will need to retain it, which we do in enqueueCompletedTask.
  // This is wasteful, and the task completion function should be fixed to
  // transfer ownership of a retain into this function, in which case we
  // will need to release in the other path.
  lock(); // TODO: remove fragment lock, and use status for synchronization

  // Immediately increment ready count and acquire the status
  //
  // NOTE: If the group is `discardResults` this becomes a plain load(),
  //       since there is no ready count to maintain.
  //
  // Examples:
  //   W:n R:0 P:3 -> W:n R:1 P:3 // no waiter, 2 more pending tasks
  //   W:n R:0 P:1 -> W:n R:1 P:1 // no waiter, no more pending tasks
  //   W:n R:0 P:1 -> W:y R:1 P:1 // complete immediately
  //   W:n R:0 P:1 -> W:y R:1 P:3 // complete immediately, 2 more pending tasks
  TaskGroupStatus assumed = statusAddReadyAssumeAcquire();

  auto asyncContextPrefix = reinterpret_cast<FutureAsyncContextPrefix *>(
      reinterpret_cast<char *>(context) - sizeof(FutureAsyncContextPrefix));
  bool hadErrorResult = false;
  auto errorObject = asyncContextPrefix->errorResult;
  if (errorObject) {
    // instead, we need to enqueue this result:
    hadErrorResult = true;
  }

  SWIFT_TASK_GROUP_DEBUG_LOG(this, "ready: %d, pending: %u",
                       assumed.readyTasks(this), assumed.pendingTasks(this));

  // ==== a) has waiting task, so let us complete it right away
  if (assumed.hasWaitingTask()) {
    resumeWaitingTask(completedTask, assumed, hadErrorResult);
    unlock(); // TODO: remove fragment lock, and use status for synchronization
    return;
  } else {
    // ==== b) enqueue completion ------------------------------------------------
    //
    // else, no-one was waiting (yet), so we have to instead enqueue to the message
    // queue when a task polls during next() it will notice that we have a value
    // ready for it, and will process it immediately without suspending.
    assert(!waitQueue.load(std::memory_order_relaxed));

    enqueueCompletedTask(completedTask, hadErrorResult);
    unlock(); // TODO: remove fragment lock, and use status for synchronization
  }
}

void DiscardingTaskGroupBase::offer(AsyncTask *completedTask, AsyncContext *context) {
  assert(completedTask);
  assert(completedTask->isFuture());
  assert(completedTask->hasChildFragment());
  assert(completedTask->hasGroupChildFragment());
  assert(completedTask->groupChildFragment()->getGroup() == asAbstract(this));
  SWIFT_TASK_GROUP_DEBUG_LOG(this, "offer, completedTask:%p , status:%s", completedTask, statusString().c_str());

  // The current ownership convention is that we are *not* given ownership
  // of a retain on completedTask; we're called from the task completion
  // handler, and the task will release itself.  So if we need the task
  // to survive this call (e.g. because there isn't an immediate waiting
  // task), we will need to retain it, which we do in enqueueCompletedTask.
  // This is wasteful, and the task completion function should be fixed to
  // transfer ownership of a retain into this function, in which case we
  // will need to release in the other path.
  lock(); // TODO: remove fragment lock, and use status for synchronization

  // Since we don't maintain ready counts in a discarding group, only load the status.
  TaskGroupStatus assumed = statusLoadAcquire();

  auto asyncContextPrefix = reinterpret_cast<FutureAsyncContextPrefix *>(
      reinterpret_cast<char *>(context) - sizeof(FutureAsyncContextPrefix));
  bool hadErrorResult = false;
  auto errorObject = asyncContextPrefix->errorResult;
  if (errorObject) {
    // instead, we need to enqueue this result:
    hadErrorResult = true;
  }

  /// If we're the last task we've been waiting for, and there is a waiting task on the group
  bool lastPendingTaskAndWaitingTask =
      assumed.pendingTasks(this) == 1 && assumed.hasWaitingTask();

  // Immediately decrement the pending count.
  // We can do this, since in this mode there is no ready count to keep track of,
  // and we immediately discard the result.
  SWIFT_TASK_GROUP_DEBUG_LOG(this, "discard result, hadError:%d, was pending:%llu",
                             hadErrorResult, assumed.pendingTasks(this));
  // If this was the last pending task, and there is a waiting task (from waitAll),
  // we must resume the task; but not otherwise. There cannot be any waiters on next()
  // while we're discarding results.
  if (lastPendingTaskAndWaitingTask) {
    ReadyQueueItem item;
    SWIFT_TASK_GROUP_DEBUG_LOG(this, "offer, offered last pending task, resume waiting task:%p",
                               waitQueue.load(std::memory_order_relaxed));
    if (readyQueue.dequeue(item)) {
      switch (item.getStatus()) {
        case ReadyStatus::RawError:
          resumeWaitingTaskWithError(item.getRawError(), assumed);
          break;
        case ReadyStatus::Error:
          resumeWaitingTask(item.getTask(), assumed, /*hadErrorResult=*/true);
          break;
        default:
          // FIXME: why can't we use llvm_unreachable here?
          assert(false && "only errors can be stored by a discarding task group, yet it wasn't an error!");
      }
    } else {
      resumeWaitingTask(completedTask, assumed, /*hadErrorResult=*/hadErrorResult);
    }
  } else {
    assert(!lastPendingTaskAndWaitingTask);
    if (hadErrorResult && readyQueue.isEmpty()) {
      // a discardResults throwing task group must retain the FIRST error it encounters.
      SWIFT_TASK_GROUP_DEBUG_LOG(this, "offer error, completedTask:%p", completedTask);
      enqueueCompletedTask(completedTask, /*hadErrorResult=*/hadErrorResult);
    } else {
      // we just are going to discard it.
      _swift_taskGroup_detachChild(asAbstract(this), completedTask);
    }

    auto afterComplete = statusCompletePendingAssumeRelease();
    SWIFT_TASK_GROUP_DEBUG_LOG(this, "offer, either more pending tasks, or no waiting task, status:%s",
                               afterComplete.to_string(this).c_str());
  }

  // Discarding results mode, immediately treats a child failure as group cancellation.
  // "All for one, one for all!" - any task failing must cause the group and all sibling tasks to be cancelled,
  // such that the discarding group can exit as soon as possible.
  if (hadErrorResult) {
    cancelAll();
  }

  unlock();
}

/// Must be called while holding the TaskGroup lock.
void AccumulatingTaskGroupBase::resumeWaitingTask(
    AsyncTask *completedTask,
    TaskGroupStatus &assumed,
    bool hadErrorResult) {
  auto waitingTask = waitQueue.load(std::memory_order_acquire);
  SWIFT_TASK_GROUP_DEBUG_LOG(this, "resume waiting task = %p, complete with = %p",
                       waitingTask, completedTask);
  while (true) {
    // ==== a) run waiting task directly -------------------------------------
    assert(assumed.hasWaitingTask());
    // assert(assumed.pendingTasks(this) && "offered to group with no pending tasks!");
    // We are the "first" completed task to arrive,
    // and since there is a task waiting we immediately claim and complete it.
    if (waitQueue.compare_exchange_strong(
        waitingTask, nullptr,
        /*success*/ std::memory_order_release,
        /*failure*/ std::memory_order_acquire)) {

#if SWIFT_CONCURRENCY_TASK_TO_THREAD_MODEL
      // In the task-to-thread model, child tasks are always actually
      // run synchronously on the parent task's thread.  For task groups
      // specifically, this means that poll() will pick a child task
      // that was added to the group and run it to completion as a
      // subroutine.  Therefore, when we enter offer(), we know that
      // the parent task is waiting and we can just return to it.

      // The task-to-thread logic in poll() currently expects the child
      // task to enqueue itself instead of just filling in the result in
      // the waiting task.  This is a little wasteful; there's no reason
      // we can't just have the parent task set itself up as a waiter.
      // But since it's what we're doing, we basically take the same
      // path as we would if there wasn't a waiter.
      enqueueCompletedTask(completedTask, hadErrorResult);
      return;

#else /* SWIFT_CONCURRENCY_TASK_TO_THREAD_MODEL */
      if (statusCompletePendingReadyWaiting(assumed)) {
        // Run the task.
        auto result = PollResult::get(completedTask, hadErrorResult);

        // Remove the child from the task group's running tasks list.
        // The parent task isn't currently running (we're about to wake
        // it up), so we're still synchronous with it.  We can safely
        // acquire our parent's status record lock here (which would
        // ordinarily run the risk of deadlock, since e.g. cancellation
        // does a parent -> child traversal while recursively holding
        // locks) because we know that the child task is completed and
        // we can't be holding its locks ourselves.
        _swift_taskGroup_detachChild(asAbstract(this), completedTask);

        auto waitingContext =
            static_cast<TaskFutureWaitAsyncContext *>(
                waitingTask->ResumeContext);

        fillGroupNextResult(waitingContext, result);

        _swift_tsan_acquire(static_cast<Job *>(waitingTask));
        // TODO: allow the caller to suggest an executor
        waitingTask->flagAsAndEnqueueOnExecutor(ExecutorRef::generic());
        return;
      } // else, try again
#endif
    }
  }
  llvm_unreachable("should have enqueued and returned.");
}

// FIXME: DUPLICATED!!!!!!
void DiscardingTaskGroupBase::resumeWaitingTask(
    AsyncTask *completedTask,
    TaskGroupStatus &assumed,
    bool hadErrorResult) {
  auto waitingTask = waitQueue.load(std::memory_order_acquire);
  SWIFT_TASK_GROUP_DEBUG_LOG(this, "resume waiting task = %p, complete with = %p",
                       waitingTask, completedTask);
  while (true) {
    // ==== a) run waiting task directly -------------------------------------
    assert(assumed.hasWaitingTask());
    // assert(assumed.pendingTasks(this) && "offered to group with no pending tasks!");
    // We are the "first" completed task to arrive,
    // and since there is a task waiting we immediately claim and complete it.
    if (waitQueue.compare_exchange_strong(
        waitingTask, nullptr,
        /*success*/ std::memory_order_release,
        /*failure*/ std::memory_order_acquire)) {

#if SWIFT_CONCURRENCY_TASK_TO_THREAD_MODEL
      // In the task-to-thread model, child tasks are always actually
      // run synchronously on the parent task's thread.  For task groups
      // specifically, this means that poll() will pick a child task
      // that was added to the group and run it to completion as a
      // subroutine.  Therefore, when we enter offer(), we know that
      // the parent task is waiting and we can just return to it.

      // The task-to-thread logic in poll() currently expects the child
      // task to enqueue itself instead of just filling in the result in
      // the waiting task.  This is a little wasteful; there's no reason
      // we can't just have the parent task set itself up as a waiter.
      // But since it's what we're doing, we basically take the same
      // path as we would if there wasn't a waiter.
      enqueueCompletedTask(completedTask, hadErrorResult);
      return;

#else /* SWIFT_CONCURRENCY_TASK_TO_THREAD_MODEL */
      if (statusCompletePendingReadyWaiting(assumed)) {
        // Run the task.
        auto result = PollResult::get(completedTask, hadErrorResult);

        // MOVED IT unlock(); // TODO: remove fragment lock, and use status for synchronization

        // Remove the child from the task group's running tasks list.
        // The parent task isn't currently running (we're about to wake
        // it up), so we're still synchronous with it.  We can safely
        // acquire our parent's status record lock here (which would
        // ordinarily run the risk of deadlock, since e.g. cancellation
        // does a parent -> child traversal while recursively holding
        // locks) because we know that the child task is completed and
        // we can't be holding its locks ourselves.
        _swift_taskGroup_detachChild(asAbstract(this), completedTask);

        auto waitingContext =
            static_cast<TaskFutureWaitAsyncContext *>(
                waitingTask->ResumeContext);

        fillGroupNextResult(waitingContext, result);

        _swift_tsan_acquire(static_cast<Job *>(waitingTask));
        // TODO: allow the caller to suggest an executor
        waitingTask->flagAsAndEnqueueOnExecutor(ExecutorRef::generic());
        return;
      } // else, try again
#endif
    }
  }
  llvm_unreachable("should have enqueued and returned.");
}

/// Must be called while holding the TaskGroup lock.
void DiscardingTaskGroupBase::resumeWaitingTaskWithError(
    SwiftError *error,
    TaskGroupStatus &assumed) {
  auto waitingTask = waitQueue.load(std::memory_order_acquire);
  SWIFT_TASK_GROUP_DEBUG_LOG(this, "resume waiting task = %p, with error = %p",
                       waitingTask, error);
  while (true) {
    // ==== a) run waiting task directly -------------------------------------
    assert(assumed.hasWaitingTask());
    // assert(assumed.pendingTasks(this) && "offered to group with no pending tasks!");
    // We are the "first" completed task to arrive,
    // and since there is a task waiting we immediately claim and complete it.
    if (waitQueue.compare_exchange_strong(
        waitingTask, nullptr,
        /*success*/ std::memory_order_release,
        /*failure*/ std::memory_order_acquire)) {

//#if SWIFT_CONCURRENCY_TASK_TO_THREAD_MODEL
//      // In the task-to-thread model, child tasks are always actually
//      // run synchronously on the parent task's thread.  For task groups
//      // specifically, this means that poll() will pick a child task
//      // that was added to the group and run it to completion as a
//      // subroutine.  Therefore, when we enter offer(), we know that
//      // the parent task is waiting and we can just return to it.
//
//      // The task-to-thread logic in poll() currently expects the child
//      // task to enqueue itself instead of just filling in the result in
//      // the waiting task.  This is a little wasteful; there's no reason
//      // we can't just have the parent task set itself up as a waiter.
//      // But since it's what we're doing, we basically take the same
//      // path as we would if there wasn't a waiter.
//      enqueueCompletedTask(completedTask, hadErrorResult);
//      return;
//
//#else /* SWIFT_CONCURRENCY_TASK_TO_THREAD_MODEL */
      if (statusCompletePendingReadyWaiting(assumed)) {
        // Run the task.
        auto result = PollResult::getError(error);

        auto waitingContext =
            static_cast<TaskFutureWaitAsyncContext *>(
                waitingTask->ResumeContext);

        fillGroupNextResult(waitingContext, result);

        _swift_tsan_acquire(static_cast<Job *>(waitingTask));
        // TODO: allow the caller to suggest an executor
        waitingTask->flagAsAndEnqueueOnExecutor(ExecutorRef::generic());
        return;
      } // else, try again
//#endif
    }
  }
  llvm_unreachable("should have enqueued and returned.");
}

SWIFT_CC(swiftasync)
static void
task_group_wait_resume_adapter(SWIFT_ASYNC_CONTEXT AsyncContext *_context) {

  auto context = static_cast<TaskFutureWaitAsyncContext *>(_context);
  auto resumeWithError =
      reinterpret_cast<AsyncVoidClosureResumeEntryPoint *>(context->ResumeParent);
  return resumeWithError(context->Parent, context->errorResult);
}

#ifdef __ARM_ARCH_7K__
__attribute__((noinline))
SWIFT_CC(swiftasync) static void workaround_function_swift_taskGroup_wait_next_throwingImpl(
    OpaqueValue *result, SWIFT_ASYNC_CONTEXT AsyncContext *callerContext,
    TaskGroup *_group,
    ThrowingTaskFutureWaitContinuationFunction resumeFunction,
    AsyncContext *callContext) {
  // Make sure we don't eliminate calls to this function.
  asm volatile("" // Do nothing.
               :  // Output list, empty.
               : "r"(result), "r"(callerContext), "r"(_group) // Input list.
               : // Clobber list, empty.
  );
  return;
}

__attribute__((noinline))
SWIFT_CC(swiftasync) static void workaround_function_swift_taskGroup_waitAllImpl(
    OpaqueValue *result, SWIFT_ASYNC_CONTEXT AsyncContext *callerContext,
    TaskGroup *_group,
    SwiftError *bodyError,
    ThrowingTaskFutureWaitContinuationFunction resumeFunction,
    AsyncContext *callContext) {
  // Make sure we don't eliminate calls to this function.
  asm volatile("" // Do nothing.
               :  // Output list, empty.
               : "r"(result), "r"(callerContext), "r"(_group) // Input list.
               : // Clobber list, empty.
  );
  return;
}
#endif

// =============================================================================
// ==== group.next() implementation (wait_next and groupPoll) ------------------

SWIFT_CC(swiftasync)
static void swift_taskGroup_wait_next_throwingImpl(
    OpaqueValue *resultPointer, SWIFT_ASYNC_CONTEXT AsyncContext *callerContext,
    TaskGroup *_group,
    ThrowingTaskFutureWaitContinuationFunction *resumeFunction,
    AsyncContext *rawContext) {
  auto waitingTask = swift_task_getCurrent();
  waitingTask->ResumeTask = task_group_wait_resume_adapter;
  waitingTask->ResumeContext = rawContext;

  auto context = static_cast<TaskFutureWaitAsyncContext *>(rawContext);
  context->ResumeParent =
      reinterpret_cast<TaskContinuationFunction *>(resumeFunction);
  context->Parent = callerContext;
  context->errorResult = nullptr;
  context->successResultPointer = resultPointer;

  auto group = asAccumulatingImpl(_group);
  assert(group && "swift_taskGroup_wait_next_throwing was passed context without group!");

  PollResult polled = group->poll(waitingTask);
  switch (polled.status) {
  case PollStatus::MustWait:
    SWIFT_TASK_DEBUG_LOG("poll group = %p, no ready tasks, waiting task = %p",
                         group, waitingTask);
    // The waiting task has been queued on the channel,
    // there were pending tasks so it will be woken up eventually.
#ifdef __ARM_ARCH_7K__
    return workaround_function_swift_taskGroup_wait_next_throwingImpl(
        resultPointer, callerContext, _group, resumeFunction, rawContext);
#else /* __ARM_ARCH_7K__ */
    return;
#endif /* __ARM_ARCH_7K__ */

  case PollStatus::Empty:
  case PollStatus::Error:
  case PollStatus::Success:
    SWIFT_TASK_GROUP_DEBUG_LOG(group, "poll, task = %p, ready task available = %p",
                         waitingTask, polled.retainedTask);
    fillGroupNextResult(context, polled);
    if (auto completedTask = polled.retainedTask) {
      // Remove the child from the task group's running tasks list.
      _swift_taskGroup_detachChild(asAbstract(group), completedTask);

      // Balance the retain done by enqueueCompletedTask.
      swift_release(completedTask);
    }

    return waitingTask->runInFullyEstablishedContext();
  }
}

PollResult AccumulatingTaskGroupBase::poll(AsyncTask *waitingTask) {
  SWIFT_TASK_GROUP_DEBUG_LOG(this, "poll, waitingTask:%p", waitingTask);
  lock(); // TODO: remove group lock, and use status for synchronization
  assert(isAccumulatingResults() &&
         "attempted to poll TaskGroup in discard-results mode!");

  PollResult result;
  result.storage = nullptr;
  result.successType = nullptr;
  result.retainedTask = nullptr;

  // Have we suspended the task?
  bool hasSuspended = false;
  bool haveRunOneChildTaskInline = false;

#if SWIFT_CONCURRENCY_TASK_TO_THREAD_MODEL
reevaluate_if_taskgroup_has_results:;
#endif
  auto assumed = statusMarkWaitingAssumeAcquire();
  if (haveRunOneChildTaskInline) {
    assert(assumed.readyTasks(this));
  }

  // ==== 1) bail out early if no tasks are pending ----------------------------
  if (assumed.isEmpty(this)) {
    SWIFT_TASK_DEBUG_LOG("poll group = %p, group is empty, no pending tasks", this);
    // No tasks in flight, we know no tasks were submitted before this poll
    // was issued, and if we parked here we'd potentially never be woken up.
    // Bail out and return `nil` from `group.next()`.
    statusRemoveWaitingRelease();
    result.status = PollStatus::Empty;
    result.successType = this->successType;
    unlock(); // TODO: remove group lock, and use status for synchronization
    return result;
  }

  auto waitHead = waitQueue.load(std::memory_order_acquire);

  // ==== 2) Ready task was polled, return with it immediately -----------------
  if (assumed.readyTasks(this)) {
    SWIFT_TASK_DEBUG_LOG("poll group = %p, tasks .ready = %d, .pending = %llu",
                         this, assumed.readyTasks(this), assumed.pendingTasks(this));

    auto assumedStatus = assumed.status;
    auto newStatus = TaskGroupStatus{assumedStatus};
    if (status.compare_exchange_strong(
        assumedStatus, newStatus.completingPendingReadyWaiting(this).status,
        /*success*/ std::memory_order_relaxed,
        /*failure*/ std::memory_order_acquire)) {

      // We're going back to running the task, so if we suspended before,
      // we need to flag it as running again.
      if (hasSuspended) {
        waitingTask->flagAsRunning();
      }

      // Success! We are allowed to poll.
      ReadyQueueItem item;
      bool taskDequeued = readyQueue.dequeue(item);
      assert(taskDequeued); (void) taskDequeued;

      auto futureFragment =
          item.getStatus() == ReadyStatus::RawError ?
          nullptr :
          item.getTask()->futureFragment();

      // Store the task in the result, so after we're done processing it may
      // be swift_release'd; we kept it alive while it was in the readyQueue by
      // an additional retain issued as we enqueued it there.

      // Note that the task was detached from the task group when it
      // completed, so we don't need to do that bit of record-keeping here.

      switch (item.getStatus()) {
        case ReadyStatus::Success:
          // Immediately return the polled value
          result.status = PollStatus::Success;
          result.storage = futureFragment->getStoragePtr();
          result.successType = futureFragment->getResultType();
          result.retainedTask = item.getTask();
          assert(result.retainedTask && "polled a task, it must be not null");
          _swift_tsan_acquire(static_cast<Job *>(result.retainedTask));
          unlock(); // TODO: remove fragment lock, and use status for synchronization
          return result;

        case ReadyStatus::Error:
          // Immediately return the polled value
          result.status = PollStatus::Error;
          result.storage =
              reinterpret_cast<OpaqueValue *>(futureFragment->getError());
          result.successType = nullptr;
          result.retainedTask = item.getTask();
          assert(result.retainedTask && "polled a task, it must be not null");
          _swift_tsan_acquire(static_cast<Job *>(result.retainedTask));
          unlock(); // TODO: remove fragment lock, and use status for synchronization
          return result;

        case ReadyStatus::RawError:
          // Immediately return the error stored
          assert(isDiscardingResults() && "raw errors are only stored in discarding results mode");
          result.status = PollStatus::Error;
          result.storage =
              reinterpret_cast<OpaqueValue *>(item.getRawError());
          result.successType = nullptr;
          result.retainedTask = nullptr;
          unlock(); // TODO: remove fragment lock, and use status for synchronization
          return result;

        case ReadyStatus::Empty:
          result.status = PollStatus::Empty;
          result.storage = nullptr;
          result.retainedTask = nullptr;
          result.successType = this->successType;
          unlock(); // TODO: remove fragment lock, and use status for synchronization
          return result;
      }
      assert(false && "must return result when status compare-and-swap was successful");
    } // else, we failed status-cas (some other waiter claimed a ready pending task, try again)
  }

  // ==== 3) Add to wait queue -------------------------------------------------
  assert(assumed.readyTasks(this) == 0);
  _swift_tsan_release(static_cast<Job *>(waitingTask));
  while (true) {
    if (!hasSuspended) {
      hasSuspended = true;
      waitingTask->flagAsSuspended();
    }
    // Put the waiting task at the beginning of the wait queue.
    if (waitQueue.compare_exchange_strong(
        waitHead, waitingTask,
        /*success*/ std::memory_order_release,
        /*failure*/ std::memory_order_acquire)) {
      unlock(); // TODO: remove fragment lock, and use status for synchronization
#if SWIFT_CONCURRENCY_TASK_TO_THREAD_MODEL
      // The logic here is paired with the logic in TaskGroupBase::offer. Once
      // we run the
      auto oldTask = _swift_task_clearCurrent();
      assert(oldTask == waitingTask);

      auto childTask = getTaskRecord()->getFirstChild();
      assert(childTask != NULL);

      SWIFT_TASK_DEBUG_LOG("[RunInline] Switching away from running %p to now running %p", oldTask, childTask);
      // Run the new task on the same thread now - this should run the new task to
      // completion. All swift tasks in task-to-thread model run on generic
      // executor
      swift_job_run(childTask, ExecutorRef::generic());
      haveRunOneChildTaskInline = true;

      SWIFT_TASK_DEBUG_LOG("[RunInline] Switching back from running %p to now running %p", childTask, oldTask);
      // We are back to being the parent task and now that we've run the child
      // task, we should reevaluate parent task
      _swift_task_setCurrent(oldTask);
      goto reevaluate_if_taskgroup_has_results;
#endif
      // no ready tasks, so we must wait.
      result.status = PollStatus::MustWait;
      _swift_task_clearCurrent();
      return result;
    } // else, try again
  }
}

// =============================================================================
// ==== _taskGroupWaitAll implementation ---------------------------------------

SWIFT_CC(swiftasync)
static void swift_taskGroup_waitAllImpl(
    OpaqueValue *resultPointer, SWIFT_ASYNC_CONTEXT AsyncContext *callerContext,
    TaskGroup *_group,
    SwiftError *bodyError,
    ThrowingTaskFutureWaitContinuationFunction *resumeFunction,
    AsyncContext *rawContext) {
  auto waitingTask = swift_task_getCurrent();
  waitingTask->ResumeTask = task_group_wait_resume_adapter;
  waitingTask->ResumeContext = rawContext;

  auto context = static_cast<TaskFutureWaitAsyncContext *>(rawContext);
  context->ResumeParent =
      reinterpret_cast<TaskContinuationFunction *>(resumeFunction);
  context->Parent = callerContext;
  context->errorResult = nullptr;
  context->successResultPointer = resultPointer;

  auto group = asBaseImpl(_group);
  SWIFT_TASK_GROUP_DEBUG_LOG(group, "waitAllImpl, waiting task = %p, bodyError = %p, status:%s",
                       waitingTask, bodyError, group->statusString().c_str());

  PollResult polled = group->tryEnqueueWaitingTask(waitingTask);
  switch (polled.status) {
    case PollStatus::MustWait:
    SWIFT_TASK_GROUP_DEBUG_LOG(group, "tryEnqueueWaitingTask MustWait, pending tasks exist, waiting task = %p",
                               waitingTask);
      if (bodyError && group->isDiscardingResults()) {
        auto discardingGroup = asDiscardingImpl(_group);
        bool storedBodyError = discardingGroup->offerBodyError(bodyError);
        if (storedBodyError) {
          SWIFT_TASK_GROUP_DEBUG_LOG(
              group, "tryEnqueueWaitingTask, stored error thrown by with...Group body, error = %p",
              bodyError);
        }
      }

      // The waiting task has been queued on the channel,
      // there were pending tasks so it will be woken up eventually.
#ifdef __ARM_ARCH_7K__
      return workaround_function_swift_taskGroup_waitAllImpl(
         resultPointer, callerContext, _group, bodyError, resumeFunction, rawContext);
#else /* __ARM_ARCH_7K__ */
      return;
#endif /* __ARM_ARCH_7K__ */

    case PollStatus::Error:
      SWIFT_TASK_GROUP_DEBUG_LOG(group, "tryEnqueueWaitingTask found error, waiting task = %p, status:%s",
                                 waitingTask, group->statusString().c_str());
      fillGroupNextResult(context, polled);
      if (auto completedTask = polled.retainedTask) {
        // Remove the child from the task group's running tasks list.
        _swift_taskGroup_detachChild(asAbstract(group), completedTask);

        // Balance the retain done by enqueueCompletedTask.
        swift_release(completedTask);
      }

      return waitingTask->runInFullyEstablishedContext();

    case PollStatus::Empty:
    case PollStatus::Success:
      /// Anything else than a "MustWait" can be treated as a successful poll.
      /// Only if there are in flight pending tasks do we need to wait after all.
      SWIFT_TASK_GROUP_DEBUG_LOG(group, "tryEnqueueWaitingTask %s, waiting task = %p, status:%s",
                                 polled.status == TaskGroupBase::PollStatus::Empty ? "empty" : "success",
                                 waitingTask, group->statusString().c_str());

      if (bodyError) {
        // None of the inner tasks have thrown, so we have to "re throw" the body error:
        fillGroupNextErrorResult(context, bodyError);
      } else {
        fillGroupNextNilResult(context, polled);
      }

      return waitingTask->runInFullyEstablishedContext();
  }
}

bool DiscardingTaskGroupBase::offerBodyError(SwiftError* _Nonnull bodyError) {
  lock(); // TODO: remove group lock, and use status for synchronization

  if (!readyQueue.isEmpty()) {
    // already other error stored, discard this one
    unlock();
    return false;
  }

  auto readyItem = ReadyQueueItem::getRawError(bodyError);
  readyQueue.enqueue(readyItem);
  unlock();

  return true;
}

PollResult DiscardingTaskGroupBase::tryEnqueueWaitingTask(AsyncTask *waitingTask) {
  SWIFT_TASK_GROUP_DEBUG_LOG(this, "tryEnqueueWaitingTask, status = %s", statusString().c_str());
  PollResult result = PollResult::getEmpty(this->successType);
  result.storage = nullptr;
  result.retainedTask = nullptr;

  // Have we suspended the task?
  bool hasSuspended = false;
  bool haveRunOneChildTaskInline = false;

  reevaluate_if_TaskGroup_has_results:;
  auto assumed = statusMarkWaitingAssumeAcquire();
  // ==== 1) bail out early if no tasks are pending ----------------------------
  if (assumed.isEmpty(this)) {
    SWIFT_TASK_DEBUG_LOG("group(%p) waitAll, is empty, no pending tasks", this);
    // No tasks in flight, we know no tasks were submitted before this poll
    // was issued, and if we parked here we'd potentially never be woken up.
    // Bail out and return `nil` from `group.next()`.
    statusRemoveWaitingRelease();
    return result;
  }

  lock(); // TODO: remove pool lock, and use status for synchronization
  auto waitHead = waitQueue.load(std::memory_order_acquire);

  // ==== 2) Add to wait queue -------------------------------------------------
  _swift_tsan_release(static_cast<Job *>(waitingTask));
  while (true) {
    if (!hasSuspended) {
      hasSuspended = true;
      waitingTask->flagAsSuspended();
    }
    // Put the waiting task at the beginning of the wait queue.
    if (waitQueue.compare_exchange_strong(
        waitHead, waitingTask,
        /*success*/ std::memory_order_release,
        /*failure*/ std::memory_order_acquire)) {
      unlock(); // TODO: remove fragment lock, and use status for synchronization
#if SWIFT_CONCURRENCY_TASK_TO_THREAD_MODEL
      // The logic here is paired with the logic in TaskGroupBase::offer. Once
      // we run the
      auto oldTask = _swift_task_clearCurrent();
      assert(oldTask == waitingTask);

      auto childTask = getTaskRecord()->getFirstChild();
      assert(childTask != NULL);

      SWIFT_TASK_DEBUG_LOG("[RunInline] Switching away from running %p to now running %p", oldTask, childTask);
      // Run the new task on the same thread now - this should run the new task to
      // completion. All swift tasks in task-to-thread model run on generic
      // executor
      swift_job_run(childTask, ExecutorRef::generic());
      haveRunOneChildTaskInline = true;

      SWIFT_TASK_DEBUG_LOG("[RunInline] Switching back from running %p to now running %p", childTask, oldTask);
      // We are back to being the parent task and now that we've run the child
      // task, we should reevaluate parent task
      _swift_task_setCurrent(oldTask);
      goto reevaluate_if_TaskGroup_has_results;
#endif
      // no ready tasks, so we must wait.
      result.status = PollStatus::MustWait;
      _swift_task_clearCurrent();
      return result;
    } // else, try again
  }
}

// FIXME: duplicated!!!!!!!!
PollResult AccumulatingTaskGroupBase::tryEnqueueWaitingTask(AsyncTask *waitingTask) {
  SWIFT_TASK_GROUP_DEBUG_LOG(this, "tryEnqueueWaitingTask, status = %s", statusString().c_str());
  PollResult result = PollResult::getEmpty(this->successType);
  result.storage = nullptr;
  result.retainedTask = nullptr;

  // Have we suspended the task?
  bool hasSuspended = false;
  bool haveRunOneChildTaskInline = false;

  reevaluate_if_TaskGroup_has_results:;
  auto assumed = statusMarkWaitingAssumeAcquire();
  // ==== 1) bail out early if no tasks are pending ----------------------------
  if (assumed.isEmpty(this)) {
    SWIFT_TASK_DEBUG_LOG("group(%p) waitAll, is empty, no pending tasks", this);
    // No tasks in flight, we know no tasks were submitted before this poll
    // was issued, and if we parked here we'd potentially never be woken up.
    // Bail out and return `nil` from `group.next()`.
    statusRemoveWaitingRelease();
    return result;
  }

  lock(); // TODO: remove pool lock, and use status for synchronization
  auto waitHead = waitQueue.load(std::memory_order_acquire);

  // ==== 2) Add to wait queue -------------------------------------------------
  _swift_tsan_release(static_cast<Job *>(waitingTask));
  while (true) {
    if (!hasSuspended) {
      hasSuspended = true;
      waitingTask->flagAsSuspended();
    }
    // Put the waiting task at the beginning of the wait queue.
    if (waitQueue.compare_exchange_strong(
        waitHead, waitingTask,
        /*success*/ std::memory_order_release,
        /*failure*/ std::memory_order_acquire)) {
      unlock(); // TODO: remove fragment lock, and use status for synchronization
#if SWIFT_CONCURRENCY_TASK_TO_THREAD_MODEL
      // The logic here is paired with the logic in TaskGroupBase::offer. Once
       // we run the
       auto oldTask = _swift_task_clearCurrent();
       assert(oldTask == waitingTask);

       auto childTask = getTaskRecord()->getFirstChild();
       assert(childTask != NULL);

       SWIFT_TASK_DEBUG_LOG("[RunInline] Switching away from running %p to now running %p", oldTask, childTask);
       // Run the new task on the same thread now - this should run the new task to
       // completion. All swift tasks in task-to-thread model run on generic
       // executor
       swift_job_run(childTask, ExecutorRef::generic());
       haveRunOneChildTaskInline = true;

       SWIFT_TASK_DEBUG_LOG("[RunInline] Switching back from running %p to now running %p", childTask, oldTask);
       // We are back to being the parent task and now that we've run the child
       // task, we should reevaluate parent task
       _swift_task_setCurrent(oldTask);
       goto reevaluate_if_TaskGroup_has_results;
#endif
      // no ready tasks, so we must wait.
      result.status = PollStatus::MustWait;
      _swift_task_clearCurrent();
      return result;
    } // else, try again
  }
}

// =============================================================================
// ==== Task Group status and flag checks  -------------------------------------

SWIFT_CC(swift)
static bool swift_taskGroup_isEmptyImpl(TaskGroup *group) {
  return asBaseImpl(group)->isEmpty();
}

SWIFT_CC(swift)
static bool swift_taskGroup_isCancelledImpl(TaskGroup *group) {
  return asBaseImpl(group)->isCancelled();
}

// =============================================================================
// ==== cancelAll --------------------------------------------------------------

SWIFT_CC(swift)
static void swift_taskGroup_cancelAllImpl(TaskGroup *group) {
  asBaseImpl(group)->cancelAll();
}

bool TaskGroupBase::cancelAll() {
  SWIFT_TASK_DEBUG_LOG("cancel all tasks in group = %p", this);

  // Flag the task group itself as cancelled.  If this was already
  // done, any existing child tasks should already have been cancelled,
  // and cancellation should automatically flow to any new child tasks,
  // so there's nothing else for us to do.
  auto wasCancelledBefore = statusCancel();
  if (wasCancelledBefore) {
    return false;
  }

  // Cancel all the child tasks.  TaskGroup is not a Sendable type,
  // so cancelAll() can only be called from the owning task.  This
  // satisfies the precondition on cancelAllChildren().
  _swift_taskGroup_cancelAllChildren(asAbstract(this));

  return true;
}

SWIFT_CC(swift)
static void swift_task_cancel_group_child_tasksImpl(TaskGroup *group) {
  // TaskGroup is not a Sendable type, and so this operation (which is not
  // currently exposed in the API) can only be called from the owning
  // task.  This satisfies the precondition on cancelAllChildren().
  _swift_taskGroup_cancelAllChildren(group);
}

/// Cancel all the children of the given task group.
///
/// The caller must guarantee that this is either called from the
/// owning task of the task group or while holding the owning task's
/// status record lock.
void swift::_swift_taskGroup_cancelAllChildren(TaskGroup *group) {
  // Because only the owning task of the task group can modify the
  // child list of a task group status record, and it can only do so
  // while holding the owning task's status record lock, we do not need
  // any additional synchronization within this function.
  for (auto childTask: group->getTaskRecord()->children())
    swift_task_cancel(childTask);
}

// =============================================================================
// ==== addPending -------------------------------------------------------------

SWIFT_CC(swift)
static bool swift_taskGroup_addPendingImpl(TaskGroup *_group, bool unconditionally) {
  auto group = asBaseImpl(_group);
  auto assumed = group->statusAddPendingTaskRelaxed(unconditionally);
  SWIFT_TASK_DEBUG_LOG("add pending %s to group(%p), tasks pending = %d",
                       unconditionally ? "unconditionally" : "",
                       group, assumed.pendingTasks(group));
  return !assumed.isCancelled();
}

#define OVERRIDE_TASK_GROUP COMPATIBILITY_OVERRIDE
#include COMPATIBILITY_OVERRIDE_INCLUDE_PATH
