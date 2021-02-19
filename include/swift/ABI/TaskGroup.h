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
// Swift ABI describing task groups.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_ABI_TASK_GROUP_H
#define SWIFT_ABI_TASK_GROUP_H

#include "swift/Basic/RelativePointer.h"
#include "swift/ABI/Executor.h"
#include "swift/ABI/HeapObject.h"
#include "swift/ABI/Metadata.h"
#include "swift/ABI/MetadataValues.h"
#include "swift/Runtime/Config.h"
#include "swift/Basic/STLExtras.h"
#include "Task.h"
#include "bitset"
#include "string"
#include "queue"

namespace swift {

  // ==== TaskGroup ------------------------------------------------------------

  class TaskGroup {
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

    enum class GroupPollStatus : uintptr_t {
        /// The channel is known to be empty and we can immediately return nil.
        Empty = 0,

        /// The task has been enqueued to the channels wait queue.
        MustWait = 1,

        /// The task has completed with result (of type \c resultType).
        Success = 2,

        /// The task has completed by throwing an error (an \c Error
        /// existential).
        Error = 3,
    };

    /// The result of waiting on a Channel (TaskGroup).
    struct PollResult {
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

        static PollResult get(AsyncTask *asyncTask, bool hadErrorResult,
                                   bool needsSwiftRelease) {
          auto fragment = asyncTask->futureFragment();
          return PollResult{
              /*status*/ hadErrorResult ?
                  TaskGroup::GroupPollStatus::Error :
                  TaskGroup::GroupPollStatus::Success,
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

    /// An item within the pending queue.
    struct PendingQueueItem {
        uintptr_t storage;

        AsyncTask *getTask() const {
          return reinterpret_cast<AsyncTask *>(storage);
        }

        static ReadyQueueItem get(AsyncTask *task) {
          assert(task == nullptr || task->isFuture());
          return ReadyQueueItem{ reinterpret_cast<uintptr_t>(task) };
        }
    };

    struct GroupStatus {
        static const uint64_t cancelled      = 0b1000000000000000000000000000000000000000000000000000000000000000;
        static const uint64_t waiting        = 0b0100000000000000000000000000000000000000000000000000000000000000;

        // 31 bits for ready tasks counter
        static const uint64_t maskReady      = 0b0011111111111111111111111111111110000000000000000000000000000000;
        static const uint64_t oneReadyTask   = 0b0000000000000000000000000000000010000000000000000000000000000000;

        // 31 bits for pending tasks counter
        static const uint64_t maskPending    = 0b0000000000000000000000000000000001111111111111111111111111111111;
        static const uint64_t onePendingTask = 0b0000000000000000000000000000000000000000000000000000000000000001;

        uint64_t status;

        bool isCancelled() {
          return (status & cancelled) > 0;
        }

        bool hasWaitingTask() {
          return (status & waiting) > 0;
        }

        unsigned int readyTasks() {
          return (status & maskReady) >> 31;
        }

        unsigned int pendingTasks() {
          return (status & maskPending);
        }

        bool isEmpty() {
          return pendingTasks() == 0;
        }

        /// Status value decrementing the Ready, Pending and Waiting counters by one.
        GroupStatus completingPendingReadyWaiting() {
          assert(pendingTasks() && "can only complete waiting task when pending tasks available");
          assert(readyTasks() && "can only complete waiting task when ready tasks available");
          assert(hasWaitingTask() && "can only complete waiting task when waiting task available");
          return GroupStatus{status - waiting - oneReadyTask - onePendingTask};
        }
        GroupStatus completingPendingReady() {
          assert(pendingTasks() && "can only complete waiting task when pending tasks available");
          assert(readyTasks() && "can only complete waiting task when ready tasks available");
          return GroupStatus{status - oneReadyTask - onePendingTask};
        }

        /// Pretty prints the status, as follows:
        /// GroupStatus{ P:{pending tasks} W:{waiting tasks} {binary repr} }
        std::string to_string() {
          std::string str;
          str.append("GroupStatus{ ");
          str.append("C:"); // cancelled
          str.append(isCancelled() ? "y " : "n ");
          str.append("W:"); // has waiting task
          str.append(hasWaitingTask() ? "y " : "n ");
          str.append("R:"); // ready
          str.append(std::to_string(readyTasks()));
          str.append(" P:"); // pending
          str.append(std::to_string(pendingTasks()));
          str.append(" " + std::bitset<64>(status).to_string());
          str.append(" }");
          return str;
        }

        /// Initially there are no waiting and no pending tasks.
        static const GroupStatus initial() {
          return GroupStatus{0};
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

//    // TODO: move to lockless via the status atomic
    mutable std::mutex mutex;

    /// Used for queue management, counting number of waiting and ready tasks
    std::atomic<uint64_t> status;

    /// TaskStatusRecord that is attached to the task running the group.
    ///
    /// Because we must remove it from the task as we exit/destroy the group,
    /// we have to keep this pointer here so we know which record to remove then.
    TaskGroupTaskStatusRecord* Record;

    /// Queue containing completed tasks offered into this channel.
    ///
    /// The low bits contain the status, the rest of the pointer is the
    /// AsyncTask.
    NaiveQueue<ReadyQueueItem> readyQueue;
//     mpsc_queue_t<ReadyQueueItem> readyQueue; // TODO: can we get away with an MPSC queue here once actor executors land?

    /// Single waiting `AsyncTask` currently waiting on `group.next()`,
    /// or `nullptr` if no task is currently waiting.
    std::atomic<AsyncTask*> waitQueue;

    friend class AsyncTask;

  public:
    explicit TaskGroup(TaskGroupTaskStatusRecord* record)
        : status(GroupStatus::initial().status),
          Record(record),
          readyQueue(),
//          readyQueue(ReadyQueueItem::get(ReadyStatus::Empty, nullptr)),
          waitQueue(nullptr) {}

    /// Destroy the storage associated with the channel.
    void destroy(AsyncTask *task);

    bool isEmpty() {
      auto oldStatus = GroupStatus { status.load(std::memory_order_relaxed) };
      return oldStatus.pendingTasks() == 0;
    }

    bool isCancelled() {
      auto oldStatus = GroupStatus { status.load(std::memory_order_relaxed) };
      return oldStatus.isCancelled();
    }

    TaskGroupTaskStatusRecord* getTaskRecord() const {
      return Record;
    }

    /// Cancel the task group and all tasks within it.
    ///
    /// Returns `true` if this is the first time cancelling the group, false otherwise.
    bool cancelAll(AsyncTask *task);

    GroupStatus statusCancel() {
      auto old = status.fetch_or(GroupStatus::cancelled, std::memory_order_relaxed);
      return GroupStatus { old };
    }
    /// Returns *assumed* new status, including the just performed +1.
    GroupStatus statusMarkWaitingAssumeAcquire() {
      auto old = status.fetch_or(GroupStatus::waiting, std::memory_order_acquire);
      return GroupStatus{old | GroupStatus::waiting};
    }
    GroupStatus statusRemoveWaiting() {
      auto old = status.fetch_and(~GroupStatus::waiting, std::memory_order_release);
      return GroupStatus{old};
    }

    /// Returns *assumed* new status, including the just performed +1.
    GroupStatus statusAddReadyAssumeAcquire() {
      auto old = status.fetch_add(GroupStatus::oneReadyTask, std::memory_order_acquire);
      auto s = GroupStatus {old + GroupStatus::oneReadyTask };
      assert(s.readyTasks() <= s.pendingTasks());
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
    /// Returns *assumed* new status, including the just performed +1.
    GroupStatus statusAddPendingTaskRelaxed() {
      auto old = status.fetch_add(GroupStatus::onePendingTask, std::memory_order_relaxed);
      auto s = GroupStatus {old + GroupStatus::onePendingTask };

      if (s.isCancelled()) {
        // revert that add, it was meaningless
        auto o = status.fetch_sub(GroupStatus::onePendingTask, std::memory_order_relaxed);
        s = GroupStatus {o - GroupStatus::onePendingTask };
      }

      return s;
    }

    GroupStatus statusLoadRelaxed() {
      return GroupStatus{status.load(std::memory_order_relaxed)};
    }

    /// Compare-and-set old status to a status derived from the old one,
    /// by simultaneously decrementing one Pending and one Waiting tasks.
    ///
    /// This is used to atomically perform a waiting task completion.
    bool statusCompletePendingReadyWaiting(GroupStatus& old) {
      return status.compare_exchange_weak(
          old.status, old.completingPendingReadyWaiting().status,
          /*success*/ std::memory_order_relaxed,
          /*failure*/ std::memory_order_relaxed);
    }
    bool statusCompletePendingReady(GroupStatus& old) {
      return status.compare_exchange_weak(
          old.status, old.completingPendingReady().status,
          /*success*/ std::memory_order_relaxed,
          /*failure*/ std::memory_order_relaxed);
    }


    /// Offer result of a task into this channel.
    /// The value is enqueued at the end of the channel.
    void offer(AsyncTask *completed, AsyncContext *context, ExecutorRef executor);

    /// Attempt to dequeue ready tasks and complete the waitingTask.
    ///
    /// If unable to complete the waiting task immediately (with an readily
    /// available completed task), either returns an `GroupPollStatus::Empty`
    /// result if it is known that no pending tasks in the group,
    /// or a `GroupPollStatus::MustWait` result if there are tasks in flight
    /// and the waitingTask eventually be woken up by a completion.
    TaskGroup::PollResult poll(AsyncTask *waitingTask);

  };

} // end namespace swift

#endif
