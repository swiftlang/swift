//===--- TaskPool.cpp - Task Pools --------------------------------------===//
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
#include "TaskPrivate.h"
#include "bitset"
#include "string"
#include "swift/ABI/HeapObject.h"
#include "swift/ABI/Metadata.h"
#include "swift/ABI/Task.h"
#include "swift/ABI/TaskPool.h"
#include "swift/Basic/RelativePointer.h"
#include "swift/Basic/STLExtras.h"
#include "swift/Runtime/Concurrency.h"
#include "swift/Runtime/Config.h"
#include "swift/Runtime/HeapObject.h"
#include "swift/Threading/Mutex.h"
#include <atomic>
#include <new>

#if defined(__APPLE__)
#include <asl.h>
#elif defined(__ANDROID__)
#include <android/log.h>
#endif

#if defined(_WIN32)
#include <io.h>
#endif

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

/******************************************************************************/
/*************************** TASK POOL ***************************************/
/******************************************************************************/

using FutureFragment = AsyncTask::FutureFragment;

namespace {
  class TaskStatusRecord;

  class TaskPoolImpl: public TaskPoolTaskStatusRecord {
  public:
    /// Describes the status of the group.
    enum class ReadyStatus : uintptr_t {
      /// The task pool is empty, no tasks are pending.
      /// Return immediately, there is no point in suspending.
      ///
      /// The storage is not accessible.
      Empty = 0b00,

      // not used: 0b01; same value as the PollStatus MustWait,
      //                 which does not make sense for the ReadyStatus

      /// The future has completed with result (of type \c resultType).
      Success = 0b10,

      /// The future has completed by throwing an error (an \c Error
      /// existential).
      Error = 0b11,
    };

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

    /// The result of waiting on the TaskPoolImpl.
    struct PollResult {
      PollStatus status;

      static PollResult get(AsyncTask *asyncTask) {
        // A TaskPool task is always Void, so we don't even have to collect the result from its future fragment.
        return PollResult{
            /*status*/ PollStatus::Success
        };
      }
    };

    const size_t TaskPoolMaximumPendingTasks = 0b0011111111111111111111111111111111111111111111111111111111111111;

    struct PoolStatus {
      static const uint64_t cancelled      = 0b1000000000000000000000000000000000000000000000000000000000000000;
      static const uint64_t waiting        = 0b0100000000000000000000000000000000000000000000000000000000000000;

      // 62 bits for pending tasks counter
      static const uint64_t maskPending    = 0b0011111111111111111111111111111111111111111111111111111111111111;
      static const uint64_t onePendingTask = 0b0000000000000000000000000000000000000000000000000000000000000001;

      uint64_t status;

      bool isCancelled() {
        return (status & cancelled) > 0;
      }

      bool hasWaitingTask() {
        return (status & waiting) > 0;
      }

      unsigned int pendingTasks() {
        return (status & maskPending);
      }

      bool isEmpty() {
        return pendingTasks() == 0;
      }

      /// Status value decrementing the Ready, Pending and Waiting counters by one.
      PoolStatus completingPendingWaiting() {
        assert(pendingTasks() &&
               "can only complete waiting task when pending tasks available");
        assert(hasWaitingTask() &&
               "can only complete waiting task when waiting task available");
        return PoolStatus{status - waiting - onePendingTask};
      }

      PoolStatus completingWaiting() {
        assert(hasWaitingTask() &&
               "must have waiting task to complete it");
        return PoolStatus{status - waiting};
      }

      /// Pretty prints the status, as follows:
      /// PoolStatus{ P:{pending tasks} W:{waiting tasks} {binary repr} }
      std::string to_string() {
        std::string str;
        str.append("PoolStatus{ ");
        str.append("C:"); // cancelled
        str.append(isCancelled() ? "y " : "n ");
        str.append("W:"); // has waiting task
        str.append(hasWaitingTask() ? "y " : "n ");
        str.append(" P:"); // pending
        str.append(std::to_string(pendingTasks()));
        str.append(" " + std::bitset<64>(status).to_string());
        str.append(" }");
        return str;
      }

      /// Initially there are no waiting and no pending tasks.
      static const PoolStatus initial() {
        return PoolStatus{0};
      };
    };

    // =============================================================================
    // ==== Checks -----------------------------------------------------------------

    static void _taskPool_reportPendingTaskOverflow(TaskPoolImpl *pool, uint64_t pendingCount) {

      char *message;
      swift_asprintf(
          &message,
          "error: task-pool: detected pending task overflow, in task pool %p! Pending task count: %llu",
          pool, pendingCount);

      if (_swift_shouldReportFatalErrorsToDebugger()) {
        RuntimeErrorDetails details = {
            .version = RuntimeErrorDetails::currentVersion,
            .errorType = "task-pool-violation",
            .currentStackDescription = "Task-pool exceeded supported pending task count",
            .framesToSkip = 1,
        };
        _swift_reportToDebugger(RuntimeErrorFlagFatal, message, &details);
      }

#if defined(_WIN32)
      #define STDERR_FILENO 2
  _write(STDERR_FILENO, message, strlen(message));
#else
      write(STDERR_FILENO, message, strlen(message));
#endif
#if defined(__APPLE__)
      asl_log(nullptr, nullptr, ASL_LEVEL_ERR, "%s", message);
#elif defined(__ANDROID__)
      __android_log_print(ANDROID_LOG_FATAL, "SwiftRuntime", "%s", message);
#endif

      free(message);
      abort();
    }

  private:
#if SWIFT_STDLIB_SINGLE_THREADED_CONCURRENCY || SWIFT_CONCURRENCY_TASK_TO_THREAD_MODEL
    // Synchronization is simple here. In a single threaded mode, all swift tasks
  // run on a single thread so no coordination is needed. In a task-to-thread
  // model, only the parent task which created the task pool can
  //
  //   (a) add child tasks to a pool
  //   (b) run the child tasks
  //
  // So we shouldn't need to worry about coordinating between child tasks and
  // parents in a task pool
  void lock() const {}
  void unlock() const {}
#else
    // TODO: move to lockless via the status atomic (make readyQueue an mpsc_queue_t<ReadyQueueItem>)
    mutable std::mutex mutex_;

    void lock() const { mutex_.lock(); }
    void unlock() const { mutex_.unlock(); }
#endif

    /// Used for queue management, counting number of waiting and ready tasks
    std::atomic <uint64_t> status;

    /// The task currently waiting on `group.next()`.  Since only the owning
    /// task can ever be waiting on a group, this is just either a reference
    /// to that task or null.
    std::atomic<AsyncTask *> waitQueue;

    friend class ::swift::AsyncTask;

  public:
    const Metadata *voidType;

    explicit TaskPoolImpl(const Metadata *T)
        : TaskPoolTaskStatusRecord(),
          status(PoolStatus::initial().status),
          waitQueue(nullptr),
          voidType(T)
    {}

    TaskPoolTaskStatusRecord *getTaskRecord() {
      return reinterpret_cast<TaskPoolTaskStatusRecord *>(this);
    }

    /// Destroy the storage associated with the group.
    void destroy();

    bool isEmpty() {
      return statusLoadRelaxed().pendingTasks() == 0;
    }

    bool isCancelled() {
      return statusLoadRelaxed().isCancelled();
    }

    /// Cancel the task pool and all tasks within it.
    ///
    /// Returns `true` if this is the first time cancelling the group, false otherwise.
    bool cancelAll();

    PoolStatus statusCancel() {
      auto old = status.fetch_or(PoolStatus::cancelled,
                                 std::memory_order_relaxed);
      return PoolStatus{old};
    }

    /// Returns *assumed* new status, including the just performed +1.
    PoolStatus statusMarkWaitingAssumeAcquire() {
      auto old = status.fetch_or(PoolStatus::waiting,
                                 std::memory_order_acquire);
      return PoolStatus{old | PoolStatus::waiting};
    }

    PoolStatus statusRemoveWaitingRelease() {
      auto old = status.fetch_and(~PoolStatus::waiting,
                                  std::memory_order_release);
      return PoolStatus{old};
    }

    /// Decrement the pending task count.
    /// Returns *assumed* new status, including the just performed +1.
    PoolStatus statusDecrementPendingAssumeAcquire() {
      auto old = status.fetch_sub(PoolStatus::onePendingTask,
                                  std::memory_order_acquire);
      assert(PoolStatus{old}.pendingTasks() > 0 && "attempted to decrement pending count when it was 0 already");
      return PoolStatus{old - PoolStatus::onePendingTask};
    }

    /// Increment the pending task count.
    ///
    /// Returns *assumed* new status, including the just performed -1.
    PoolStatus statusIncrementPendingAssumeAcquire() {
      auto old = status.fetch_add(PoolStatus::onePendingTask,
                                  std::memory_order_acquire);
      return PoolStatus{old + PoolStatus::onePendingTask};
    }

    /// Similar to decrementing the pending count, however does so 'relaxed'.
    /// Used to undo an optimistic increment, when the pool already is cancelled.
    ///
    /// Returns *assumed* new status, including the just performed -1.
    PoolStatus statusUndoIncrementPendingAssumeRelaxed() {
      auto o = status.fetch_sub(PoolStatus::onePendingTask,
                                std::memory_order_relaxed);
      return PoolStatus{o - PoolStatus::onePendingTask};
    }

    /// Add a single pending task to the status counter.
    /// This is used to implement next() properly, as we need to know if there
    /// are pending tasks worth suspending/waiting for or not.
    ///
    /// Note that the group does *not* store child tasks at all, as they are
    /// stored in the `TaskPoolTaskStatusRecord` inside the current task, that
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
    PoolStatus statusAddPendingTaskRelaxed(bool unconditionally) {
      auto assumed = statusIncrementPendingAssumeAcquire();

      if (assumed.pendingTasks() == TaskPoolMaximumPendingTasks) {
        _taskPool_reportPendingTaskOverflow(this, assumed.pendingTasks());
        abort(); // reportOverflow will abort();
      }

      if (!unconditionally && assumed.isCancelled()) {
        // revert that add, it was meaningless
        return statusUndoIncrementPendingAssumeRelaxed();
      }
      return assumed;
    }

    PoolStatus statusLoadRelaxed() {
      return PoolStatus{status.load(std::memory_order_relaxed)};
    }

    /// Offer result of a task into this task pool.
    ///
    /// Unlike a task group, result values are never stored and we immediately
    /// release the task after decrementing the `pending` count in the pool's status.
    ///
    /// If the TaskPool is currently "draining" tasks (i.e. its body has completed),
    /// there may be a `waiting` task. If so, and this is the last pending task,
    /// this offer will resume it, allowing the TaskPool to complete and destroy itself.
    void offer(AsyncTask *completed, AsyncContext *context);

    /// A `TaskPool` is not able to wait on individual completions,
    /// instead, it can only await on "all pending tasks have been processed".
    ///
    ///
    /// If unable to complete the waiting task immediately (with an readily
    /// available completed task), either returns an `PollStatus::Empty`
    /// result if it is known that no pending tasks in the group,
    /// or a `PollStatus::MustWait` result if there are tasks in flight
    /// and the waitingTask eventually be woken up by a completion.
    PollResult waitAll(AsyncTask *waitingTask);

  };

} // end anonymous namespace

/******************************************************************************/
/************************ TASK POOL IMPLEMENTATION ***************************/
/******************************************************************************/

using ReadyStatus = TaskPoolImpl::ReadyStatus;
using PollResult = TaskPoolImpl::PollResult;
using PollStatus = TaskPoolImpl::PollStatus;

static_assert(sizeof(TaskPoolImpl) <= sizeof(TaskPool) &&
              alignof(TaskPoolImpl) <= alignof(TaskPool),
              "TaskPoolImpl doesn't fit in TaskPool");

static TaskPoolImpl *asImpl(TaskPool *group) {
  return reinterpret_cast<TaskPoolImpl*>(group);
}

static TaskPool *asAbstract(TaskPoolImpl *group) {
  return reinterpret_cast<TaskPool*>(group);
}

TaskPoolTaskStatusRecord * TaskPool::getTaskRecord() {
  return asImpl(this)->getTaskRecord();
}

// =============================================================================
// ==== initialize -------------------------------------------------------------

// Initializes into the preallocated _pool an actual TaskPoolImpl.
SWIFT_CC(swift)
static void swift_taskPool_initializeImpl(TaskPool *pool, const Metadata *Void) {
  SWIFT_TASK_DEBUG_LOG("pool(%p) create", pool);

  TaskPoolImpl *impl = ::new (pool) TaskPoolImpl(Void);
  auto record = impl->getTaskRecord();
  assert(impl == record && "the pool IS the task record");

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

void TaskPool::addChildTask(AsyncTask *child) {
  SWIFT_TASK_DEBUG_LOG("attach child task = %p to pool = %p", child, this);

  // Add the child task to this task pool.
  //
  // The corresponding removal WILL happen concurrently and must be synchronized
  // using the task record lock of the child's parent. This is different from a
  // task group, where removals are non-concurrent to their parent!

  // Since calls to addChildTask must be holding the task status record lock,
  // we can proceed to attach the child without additional locking here.
  auto record = asImpl(this)->getTaskRecord();
  record->attachChild(child);
}

void TaskPool::removeChildTask(AsyncTask *child) {
  SWIFT_TASK_DEBUG_LOG("detach child task = %p from group = %p", child, this);

  auto record = asImpl(this)->getTaskRecord();

  // The task status record lock is held during this operation, which
  // prevents us from racing with cancellation or escalation.  We don't
  // need to acquire the task group lock because the child list is only
  // accessed under the task status record lock.
  record->detachChild(child);
}

// =============================================================================
// ==== destroy ----------------------------------------------------------------
SWIFT_CC(swift)
static void swift_taskPool_destroyImpl(TaskPool *group) {
  asImpl(group)->destroy();
}

void TaskPoolImpl::destroy() {
  SWIFT_TASK_DEBUG_LOG("destroying task group = %p", this);
  if (!this->isEmpty()) {
    auto status = this->statusLoadRelaxed();
    SWIFT_TASK_DEBUG_LOG("destroying task group = %p, .pending = %d",
                         this, status.pendingTasks());
  }
  assert(this->isEmpty() && "Attempted to destroy non-empty task group!");

  // First, remove the group from the task and deallocate the record
  removeStatusRecord(getTaskRecord());

  // No need to drain our queue here, as by the time we call destroy,
  // all tasks inside the group must have been awaited on already.
  // This is done in Swift's withTaskPool function explicitly.

  // destroy the group's storage
  this->~TaskPoolImpl();
}

// =============================================================================
// ==== offer ------------------------------------------------------------------

void TaskPool::offer(AsyncTask *completedTask, AsyncContext *context) {
  asImpl(this)->offer(completedTask, context);
}

bool TaskPool::isCancelled() {
  return asImpl(this)->isCancelled();
}

static void fillPoolNextVoidResult(TaskFutureWaitAsyncContext *context,
                                   const Metadata *voidType,
                                   PollResult result) {
  /// Fill in the result value
  switch (result.status) {
    case PollStatus::MustWait:
      assert(false && "filling a waiting status?");
      return;

    case PollStatus::Error: {
      assert(false && "cannot have errors");
      return;
    }

    case PollStatus::Success: {
      // Initialize the result as an Optional<Void>.
      // const Metadata *voidType = nullptr; // result.voidType; // FIXME: should be Void type
      OpaqueValue *destPtr = context->successResultPointer;
      // TODO: figure out a way to try to optimistically take the
      // value out of the finished task's future, if there are no
      // remaining references to it.
      voidType->vw_initializeWithCopy(destPtr, nullptr);
      voidType->vw_storeEnumTagSinglePayload(destPtr, 0, 1);
      return;
    }

    case PollStatus::Empty: {
      // Initialize the result as a nil Optional<Success>.
      // const Metadata *voidType = nullptr; // result.voidType; // FIXME: should be Void type
      OpaqueValue *destPtr = context->successResultPointer;
      voidType->vw_storeEnumTagSinglePayload(destPtr, 1, 1);
      return;
    }
  }
}

void TaskPoolImpl::offer(AsyncTask *completedTask, AsyncContext *context) {
  assert(completedTask);
  assert(completedTask->isFuture());
  assert(completedTask->hasChildFragment());
  assert(completedTask->hasPoolChildFragment());
  assert(completedTask->poolChildFragment()->getPool() == asAbstract(this));
  SWIFT_TASK_DEBUG_LOG("offer task %p to pool %p", completedTask, this);

  lock(); // TODO: remove fragment lock, and use status for synchronization

  // Immediately decrement the pending count; we do not keep track of "ready" tasks and never store them;
  // This is different from a task group, which has to keep the pending count and add +1 "ready" when offered to.
  auto assumed = statusDecrementPendingAssumeAcquire();
  SWIFT_TASK_DEBUG_LOG("pool(%p), remaining pending: %d", this, assumed.pendingTasks());

  auto asyncContextPrefix = reinterpret_cast<FutureAsyncContextPrefix *>(
      reinterpret_cast<char *>(context) - sizeof(FutureAsyncContextPrefix));

  SWIFT_TASK_DEBUG_LOG("pool(%p) child task=%p completed, detach", this, completedTask);
  /// We're offering concurrently to the parent task (which owns the pool), and must remove
  /// the child from the pool record's children container.
  /// The _swift_taskPool_detachChild will take the parent task lock to perform the child removal.
  _swift_taskPool_detachChild(asAbstract(this), completedTask);

  // ==== a) has waiting task.
  // A TaskPool only has a waiting task while terminating, and that task shall only be resumed once
  // all tasks have been processed. Only resume the waiting task if this was the last pending task.
  if (assumed.hasWaitingTask() && assumed.pendingTasks() == 0) {
    auto waitingTask = waitQueue.load(std::memory_order_acquire);
    SWIFT_TASK_DEBUG_LOG("group has waiting task = %p, complete with = %p",
                         waitingTask, completedTask);
    while (true) {
      // ==== a) run waiting task directly -------------------------------------
      assert(assumed.hasWaitingTask());
      assert(assumed.pendingTasks() && "offered to pool with no pending tasks!");
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
//        completeTask(completedTask);
        unlock(); // TODO: remove fragment lock, and use status for synchronization
        return;

#else /* SWIFT_CONCURRENCY_TASK_TO_THREAD_MODEL */
        // Run the task.
        auto result = PollResult::get(completedTask);

        unlock(); // TODO: remove fragment lock, and use status for synchronization

        // Remove the child from the task group's running tasks list.
        // The parent task isn't currently running (we're about to wake
        // it up), so we're still synchronous with it.  We can safely
        // acquire our parent's status record lock here (which would
        // ordinarily run the risk of deadlock, since e.g. cancellation
        // does a parent -> child traversal while recursively holding
        // locks) because we know that the child task is completed and
        // we can't be holding its locks ourselves.
        _swift_taskPool_detachChild(asAbstract(this), completedTask);

        auto waitingContext =
            static_cast<TaskFutureWaitAsyncContext *>(
                waitingTask->ResumeContext);

        fillPoolNextVoidResult(waitingContext, voidType, result);

        _swift_tsan_acquire(static_cast<Job *>(waitingTask));
        // TODO: allow the caller to suggest an executor
        waitingTask->flagAsAndEnqueueOnExecutor(ExecutorRef::generic());

        // completedTask will be released by the remainder of its
        // completion function.
        return;
#endif
      }
    }
    llvm_unreachable("should have enqueued and returned.");
  } else {
    // ==== b) enqueue completion ------------------------------------------------
    //
    // else, no-one was waiting (yet), so we have to instead enqueue to the message
    // queue when a task polls during next() it will notice that we have a value
    // ready for it, and will process it immediately without suspending.
    assert(!waitQueue.load(std::memory_order_relaxed));

    // completeTask(completedTask);
    unlock(); // TODO: remove fragment lock, and use status for synchronization
  }

  return;
}

SWIFT_CC(swiftasync)
static void
TASK_POOL_wait_resume_adapter(SWIFT_ASYNC_CONTEXT AsyncContext *_context) {

  auto context = static_cast<TaskFutureWaitAsyncContext *>(_context);
  auto resumeWithError =
      reinterpret_cast<AsyncVoidClosureResumeEntryPoint *>(context->ResumeParent);
  return resumeWithError(context->Parent, context->errorResult);
}

#ifdef __ARM_ARCH_7K__
__attribute__((noinline))
SWIFT_CC(swiftasync) static void workaround_function_swift_taskPool_waitAllImpl(
    OpaqueValue *result, SWIFT_ASYNC_CONTEXT AsyncContext *callerContext,
    TaskPool *_pool,
    ThrowingTaskFutureWaitContinuationFunction resumeFunction,
    AsyncContext *callContext) {
  // Make sure we don't eliminate calls to this function.
  asm volatile("" // Do nothing.
               :  // Output list, empty.
               : "r"(result), "r"(callerContext), "r"(_pool) // Input list.
               : // Clobber list, empty.
  );
  return;
}
#endif

// =============================================================================
// ==== pool.waitAll() implementation ------------------------------------------

SWIFT_CC(swiftasync)
static void swift_taskPool_waitAllImpl(
    OpaqueValue *resultPointer, SWIFT_ASYNC_CONTEXT AsyncContext *callerContext,
    TaskPool *_pool,
    ThrowingTaskFutureWaitContinuationFunction *resumeFunction,
    AsyncContext *rawContext) {
  auto waitingTask = swift_task_getCurrent();
  waitingTask->ResumeTask = TASK_POOL_wait_resume_adapter;
  waitingTask->ResumeContext = rawContext;

  auto context = static_cast<TaskFutureWaitAsyncContext *>(rawContext);
  context->ResumeParent =
      reinterpret_cast<TaskContinuationFunction *>(resumeFunction);
  context->Parent = callerContext;
  context->errorResult = nullptr;
  context->successResultPointer = resultPointer;

  auto pool = asImpl(_pool);
  assert(pool && "swift_taskPool_waitAll was passed context without pool!");

  SWIFT_TASK_DEBUG_LOG("pool(%p) waitAll, no ready tasks, waiting task = %p",
                       pool, waitingTask);

  PollResult polled = pool->waitAll(waitingTask);
  switch (polled.status) {
    case PollStatus::MustWait:
    SWIFT_TASK_DEBUG_LOG("pool(%p) poll, pending tasks exist, waiting task = %p",
                         pool, waitingTask);
      // The waiting task has been queued on the channel,
      // there were pending tasks so it will be woken up eventually.
#ifdef __ARM_ARCH_7K__
      return workaround_function_swift_taskPool_waitAllImpl(
        resultPointer, callerContext, _pool, resumeFunction, rawContext);
#else /* __ARM_ARCH_7K__ */
      return;
#endif /* __ARM_ARCH_7K__ */

    case PollStatus::Empty:
    case PollStatus::Error:
    case PollStatus::Success:
      /// Anything else than a "MustWait" can be treated as a successful poll.
      /// Only if there are in flight pending tasks do we need to wait after all.
    SWIFT_TASK_DEBUG_LOG("[pool:%p] poll successful, waiting task = %p", pool, waitingTask);
      fillPoolNextVoidResult(context, pool->voidType, polled);

      return waitingTask->runInFullyEstablishedContext();
  }
}

PollResult TaskPoolImpl::waitAll(AsyncTask *waitingTask) {
  SWIFT_TASK_DEBUG_LOG("[pool:%p], waitAll pending; status = %s", this, statusLoadRelaxed().to_string().c_str());
  PollResult result;

  // Have we suspended the task?
  bool hasSuspended = false;
  bool haveRunOneChildTaskInline = false;

  reevaluate_if_taskpool_has_results:;
  auto assumed = statusMarkWaitingAssumeAcquire();
  // ==== 1) bail out early if no tasks are pending ----------------------------
  if (assumed.isEmpty()) {
    SWIFT_TASK_DEBUG_LOG("[pool:%p] poll, is empty, no pending tasks", this);
    // No tasks in flight, we know no tasks were submitted before this poll
    // was issued, and if we parked here we'd potentially never be woken up.
    // Bail out and return `nil` from `group.next()`.
    statusRemoveWaitingRelease();
    result.status = PollStatus::Empty;
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
      // The logic here is paired with the logic in TaskPoolImpl::offer. Once
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
      goto reevaluate_if_taskpool_has_results;
#endif
      // no ready tasks, so we must wait.
      result.status = PollStatus::MustWait;
      _swift_task_clearCurrent();
      return result;
    } // else, try again
  }
}

// =============================================================================
// ==== isEmpty ----------------------------------------------------------------

SWIFT_CC(swift)
static bool swift_taskPool_isEmptyImpl(TaskPool *pool) {
  return asImpl(pool)->isEmpty();
}

// =============================================================================
// ==== isCancelled ------------------------------------------------------------

SWIFT_CC(swift)
static bool swift_taskPool_isCancelledImpl(TaskPool *pool) {
  return asImpl(pool)->isCancelled();
}

// =============================================================================
// ==== cancelAll --------------------------------------------------------------

SWIFT_CC(swift)
static void swift_taskPool_cancelAllImpl(TaskPool *pool) {
  asImpl(pool)->cancelAll();
}

bool TaskPoolImpl::cancelAll() {
  SWIFT_TASK_DEBUG_LOG("cancel all tasks in pool = %p", this);

  // Flag the task group itself as cancelled.  If this was already
  // done, any existing child tasks should already have been cancelled,
  // and cancellation should automatically flow to any new child tasks,
  // so there's nothing else for us to do.
  auto old = statusCancel();
  if (old.isCancelled()) {
    return false;
  }

  // Cancel all the child tasks.  TaskPool is not a Sendable type,
  // so cancelAll() can only be called from the owning task.  This
  // satisfies the precondition on cancelAllChildren().
  _swift_taskPool_cancelAllChildren(asAbstract(this));

  return true;
}

SWIFT_CC(swift)
static void swift_task_cancel_pool_child_tasksImpl(TaskPool *pool) {
  // TaskPool is not a Sendable type, and so this operation can
  // only be called from the owning task.
  // This satisfies the precondition on cancelAllChildren().
  _swift_taskPool_cancelAllChildren(pool);
}

/// Cancel all the children of the given task group.
///
/// The caller must guarantee that this is either called from the
/// owning task of the task group or while holding the owning task's
/// status record lock.
void swift::_swift_taskPool_cancelAllChildren(TaskPool *pool) {
  SWIFT_TASK_DEBUG_LOG("pool(%p) cancel all children tasks", pool);
  // Because only the owning task of the task group can modify the
  // child list of a task group status record, and it can only do so
  // while holding the owning task's status record lock, we do not need
  // any additional synchronization within this function.
  for (auto childTask: pool->getTaskRecord()->children())
    swift_task_cancel(childTask);
}

// =============================================================================
// ==== addPending -------------------------------------------------------------

SWIFT_CC(swift)
static bool swift_taskPool_addPendingImpl(TaskPool *pool, bool unconditionally) {
  auto assumed = asImpl(pool)->statusAddPendingTaskRelaxed(unconditionally);
  SWIFT_TASK_DEBUG_LOG("add pending %s to pool %p, tasks pending = %d",
                       unconditionally ? "unconditionally" : "",
                       pool, assumed.pendingTasks());
  return !assumed.isCancelled();
}

#define OVERRIDE_TASK_POOL COMPATIBILITY_OVERRIDE
#include COMPATIBILITY_OVERRIDE_INCLUDE_PATH
