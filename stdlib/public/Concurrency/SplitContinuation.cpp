//===--- SplitContinuation.cpp - Split continuation implementation ---------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// The runtime implementation of split continuations, whose create, await and
// resume operations can happen in different functions and on different threads.
//
//===----------------------------------------------------------------------===//

#include "TaskPrivate.h"
#include "Tracing.h"
#include "swift/ABI/Metadata.h"
#include "swift/ABI/Task.h"
#include "swift/Runtime/Concurrency.h"
#include "swift/Threading/ConditionVariable.h"
#include "llvm/Support/MathExtras.h"
#include <atomic>
#include <cstddef>

using namespace swift;

// A split continuation decouples the *create*, *await*, and *resume* operations
// of a continuation so they can happen in different functions and on different
// threads. Unlike swift_continuation_init/await/resume, which assume create and
// await are adjacent on one task with nothing in between, a split continuation:
//
//   * create (swift_continuation_initSplit): initializes the storage and the
//     context in the NumWords_SplitContinuation words the compiler allocated,
//     points the context at the result buffer the compiler also allocated, and
//     sets the state to Pending; does NOT bind a resume-point and does NOT
//     touch task execution state. The storage is shaped like the front of a
//     task, so one Continuation type can carry either flavour and a resume
//     works for both.
//   * await (swift_continuation_awaitSplit): binds the resume-point to the
//     *current* task, records the awaiting task on the context, and performs
//     the Pending->Awaited transition. The records of the cancellation and
//     escalation handlers are built in buffers in the waiting frame, around the
//     function and context the compiler stored in them, published in the same
//     status update that suspends the task, and removed by the task when it
//     resumes.
//   * resume: the regular swift_continuation_resume* entry points, and
//     swift_continuation_resumeOnExecutors, which read the marker word and
//     route to the split implementation. The resume works *by context* (the
//     value/error is stored into the context first), because
//     task->ResumeContext cannot be relied on: a resume may arrive before the
//     await. It enqueues the recorded awaiting task, or donates the resuming
//     thread if one was offered and it belongs to the executor the task
//     resumes on.

namespace {

/// The handler records that a wait published, which the task removes again
/// when it resumes. Null for a handler that isn't published. The records live
/// in buffers in the frame of the waiting function, which outlive the wait.
struct SplitContinuationHandlers {
  CancellationNotificationStatusRecord *Cancellation;
  EscalationNotificationStatusRecord *Escalation;
};

/// The fields of a split continuation that live in front of `ResumeContext`.
struct SplitContinuationPrefix {
  /// Distinguishes this from a task.
  uintptr_t Marker;

  /// The task awaiting the continuation, recorded at await time so that a
  /// resume-by-context can make it runnable. Null until then, which is also how
  /// we recognize a resume that races ahead of the await.
  AsyncTask *AwaitingTask;

  /// The handler records the wait published.
  SplitContinuationHandlers PublishedHandlers;
};

/// The storage backing a split continuation. A raw continuation referring to a
/// split continuation is the address of one of these. The compiler allocates
/// NumWords_SplitContinuation words for it in the frame of the function that
/// creates the continuation, followed by the context.
///
/// The layout makes a split continuation resumable by *any* code that can
/// resume a regular one, including code compiled before split continuations
/// existed. `ResumeContext` sits at exactly the offset at which an AsyncTask
/// keeps its own. Resumers only read the marker and `ResumeContext`, so the
/// other fields use the words in front of it that a task uses for its header.
///
/// AsyncTask isn't standard-layout, so `offsetof` on it warns; we rely on its
/// layout deliberately and assert it below.
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Winvalid-offsetof"
struct SplitContinuationStorage : SplitContinuationPrefix {
  /// Padding placing `ResumeContext` where a task keeps its own.
  char TaskPrefixPadding[offsetof(AsyncTask, ResumeContext) -
                         sizeof(SplitContinuationPrefix)];

  /// Points at the ContinuationAsyncContext that follows in this storage.
  AsyncContext * __ptrauth_swift_task_resume_context ResumeContext;
};

/// Written into `SplitContinuationStorage::Marker`, where a task keeps its
/// metadata pointer. No metadata pointer is all-ones.
static constexpr uintptr_t SplitContinuationMarker = ~(uintptr_t)0;

static_assert(offsetof(SplitContinuationStorage, Marker) == 0,
              "the marker must sit where a task keeps its metadata pointer");
static_assert(offsetof(SplitContinuationStorage, ResumeContext) ==
                  offsetof(AsyncTask, ResumeContext),
              "a split continuation must be loadable as if it were a "
              "task, so its ResumeContext must sit at the task's offset");
#pragma clang diagnostic pop

/// The context follows the storage, at the context's alignment.
static constexpr size_t splitContextOffset() {
  return (sizeof(SplitContinuationStorage) + alignof(ContinuationAsyncContext) -
          1) /
         alignof(ContinuationAsyncContext) * alignof(ContinuationAsyncContext);
}

// The compiler allocates these sizes in the frames of clients, so they must not
// grow once shipped.
static_assert(splitContextOffset() + sizeof(ContinuationAsyncContext) <=
                  NumWords_SplitContinuation * sizeof(void *),
              "a split continuation doesn't fit NumWords_SplitContinuation");
static_assert(alignof(ContinuationAsyncContext) <= Alignment_SplitContinuation,
              "a split continuation needs more than "
              "Alignment_SplitContinuation");
static_assert(sizeof(CancellationNotificationStatusRecord) <=
                  NumWords_TaskCancellationHandler * sizeof(void *),
              "a cancellation handler record doesn't fit "
              "NumWords_TaskCancellationHandler");
static_assert(sizeof(EscalationNotificationStatusRecord) <=
                  NumWords_TaskEscalationHandler * sizeof(void *),
              "an escalation handler record doesn't fit "
              "NumWords_TaskEscalationHandler");
static_assert(alignof(CancellationNotificationStatusRecord) <=
                  Alignment_TaskCancellationHandler,
              "a cancellation handler record needs more than "
              "Alignment_TaskCancellationHandler");
static_assert(alignof(EscalationNotificationStatusRecord) <=
                  Alignment_TaskEscalationHandler,
              "an escalation handler record needs more than "
              "Alignment_TaskEscalationHandler");

/// The function and context of a cancellation handler, which the compiler
/// stores in the record buffer before the await.
struct SplitContinuationCancellationHandler {
  void *Header[WordOffset_TaskCancellationHandlerFunction];
  CancellationNotificationStatusRecord::FunctionTypeWithReason
      *__ptrauth_swift_cancellation_notification_with_reason_function Function;
  void *Context;
};

/// The function and context of a priority escalation handler, which the
/// compiler stores in the record buffer before the await.
struct SplitContinuationEscalationHandler {
  void *Header[WordOffset_TaskEscalationHandlerFunction];
  EscalationNotificationStatusRecord::FunctionType
      *__ptrauth_swift_escalation_notification_function Function;
  void *Context;
};

static_assert(offsetof(SplitContinuationCancellationHandler, Context) ==
                  WordOffset_TaskCancellationHandlerContext * sizeof(void *),
              "the compiler stores the cancellation handler's context at "
              "WordOffset_TaskCancellationHandlerContext");
static_assert(offsetof(SplitContinuationEscalationHandler, Context) ==
                  WordOffset_TaskEscalationHandlerContext * sizeof(void *),
              "the compiler stores the escalation handler's context at "
              "WordOffset_TaskEscalationHandlerContext");
static_assert(sizeof(SplitContinuationCancellationHandler) <=
                  NumWords_TaskCancellationHandler * sizeof(void *),
              "the cancellation handler doesn't fit its record buffer");
static_assert(sizeof(SplitContinuationEscalationHandler) <=
                  NumWords_TaskEscalationHandler * sizeof(void *),
              "the escalation handler doesn't fit its record buffer");

/// Whether `continuation` is a split continuation rather than a task.
static bool isSplitContinuation(const void *continuation) {
  return *reinterpret_cast<const uintptr_t *>(continuation) ==
         SplitContinuationMarker;
}

/// The storage a split continuation refers to.
static SplitContinuationStorage *storageOfContinuation(const void *continuation) {
  return reinterpret_cast<SplitContinuationStorage *>(
      const_cast<void *>(continuation));
}

static ContinuationAsyncContext *
contextOfStorage(SplitContinuationStorage *storage) {
  return static_cast<ContinuationAsyncContext *>(storage->ResumeContext);
}

static SplitContinuationStorage *
storageOfContext(ContinuationAsyncContext *context) {
  return reinterpret_cast<SplitContinuationStorage *>(
      reinterpret_cast<char *>(context) - splitContextOffset());
}

} // end anonymous namespace

/// Perform the Pending/Awaited -> Resumed transition and, if a task was
/// awaiting, make it runnable. Mirrors resumeTaskAfterContinuation, but works
/// purely from the context (there is no task->ResumeContext to recover) and
/// tolerates the resume-before-await race where no awaiting task has been
/// recorded yet.
void swift::resumeSplitContinuation(
    ContinuationAsyncContext *context,
    const ContinuationResumingExecutors *resumingExecutors) {
  auto &sync = context->AwaitSynchronization;

  auto status = sync.load(std::memory_order_acquire);
  assert(status != ContinuationStatus::Resumed &&
         "continuation was already resumed");

  // Case 1: Pending -- nobody is awaiting yet. Record Resumed and return. A later
  // awaitSplit will observe Resumed and continue inline.  Note that no awaiting
  // task exists in this case, so we must do this *before* touching
  // context->AwaitingTask.
  if (status == ContinuationStatus::Pending &&
      sync.compare_exchange_strong(status, ContinuationStatus::Resumed,
                                   /*success*/ std::memory_order_release,
                                   /*failure*/ std::memory_order_acquire)) {
    return;
  }

  // Case 2: Awaited -- a task recorded itself at await time and suspended.
  assert(status == ContinuationStatus::Awaited &&
         "detected concurrent attempt to resume continuation");

  auto *storage = storageOfContext(context);
  auto task = storage->AwaitingTask;
  assert(task && "awaited split continuation has no awaiting task");

  // Make sure TSan knows that the resume call happens-before the task
  // restarting.
  _swift_tsan_release(static_cast<Job *>(task));

#if SWIFT_CONCURRENCY_TASK_TO_THREAD_MODEL
  assert(context->Cond != nullptr);
  sync.store(ContinuationStatus::Resumed, std::memory_order_relaxed);
  context->Cond->signal();
#else
  // Which executor the task resumes on is decided at *await* time, and the
  // resumer learns which executors it is on when it is handed the
  // continuation, which is necessarily earlier. So the two can disagree when
  // the awaiting side has moved isolation, or taken a task executor
  // preference, between being handed the continuation and awaiting it. That is
  // not a misuse: the offered thread simply cannot be used, and the task is
  // enqueued instead.
  makeResumedTaskRunnable(task, context->ResumeToExecutor, resumingExecutors);
#endif /* SWIFT_CONCURRENCY_TASK_TO_THREAD_MODEL */
}


SWIFT_CC(swift)
void swift::swift_continuation_initSplit(SplitContinuation *continuation,
                                         OpaqueValue *result) {
  // The compiler allocated the storage and the result buffer in the frame of
  // the function that creates the continuation. The result buffer has to exist
  // from now on rather than only in the awaiting frame, because a resume can
  // arrive before the await does and needs somewhere to write the value.
  auto *task = swift_task_getCurrent();
  assert(task && "creating a split continuation outside of a task");

  auto *storage = reinterpret_cast<SplitContinuationStorage *>(continuation);
  auto *context = reinterpret_cast<ContinuationAsyncContext *>(
      reinterpret_cast<char *>(storage) + splitContextOffset());

  // Initialize the fields we own. We deliberately do *not* run a constructor
  // (the base AsyncContext's Parent/ResumeParent are bound later, at await) and
  // do *not* write any task execution state.
  //
  // A split continuation is always awaited through the throwing builtin, so it
  // can always throw. The other two AsyncContinuationFlags a regular
  // continuation understands do not apply: `hasExecutorOverride` means "the
  // creator already chose ResumeToExecutor", which awaitSplit overwrites
  // unconditionally, and `isExecutorSwitchForced` means "hop on resume".
  context->Flags = ContinuationAsyncContext::FlagsType();
  context->Flags.setCanThrow(true);
  context->ErrorResult = nullptr;
  context->NormalResult = result;
  // Overwritten by the await.
  context->ResumeToExecutor = SerialExecutorRef::generic();
#if SWIFT_CONCURRENCY_TASK_TO_THREAD_MODEL
  context->Cond = nullptr;
#endif

  storage->Marker = SplitContinuationMarker;
  storage->AwaitingTask = nullptr;
  storage->PublishedHandlers = {};

  // Relaxed is fine: resumption must happen-after this call returns and the
  // continuation is handed to the executor.
  context->AwaitSynchronization.store(ContinuationStatus::Pending,
                                      std::memory_order_relaxed);

  storage->ResumeContext = context;

  concurrency::trace::task_continuation_init(task, context);
}

/// Remove the records that a wait published from `task`.
static void removeSplitContinuationHandlers(
    AsyncTask *task, SplitContinuationHandlers &published) {
  auto *cancellation = published.Cancellation;
  auto *escalation = published.Escalation;
  published = {};

  TaskStatusRecord *innermost = escalation;
  if (!innermost)
    innermost = cancellation;
  if (!innermost)
    return;
  TaskStatusRecord *outermost = cancellation;
  if (!outermost)
    outermost = escalation;

  // The task pushed nothing while it was suspended, so the records are
  // usually still the innermost ones and come off in a single update.
  auto status = task->_private()._status().load(std::memory_order_relaxed);
  while (!status.isStatusRecordLocked() &&
         status.getInnermostRecord() == innermost) {
    auto newStatus = status.withInnermostRecord(outermost->getParent());
    if (task->_private()._status().compare_exchange_weak(
            status, newStatus,
            /*success*/ std::memory_order_relaxed,
            /*failure*/ std::memory_order_relaxed)) {
      newStatus.traceStatusChanged(task, status, false);
      return;
    }
  }

  // Someone holds the lock, for example to run a handler. Wait for them.
  if (escalation)
    removeStatusRecord(task, escalation);
  if (cancellation)
    removeStatusRecord(task, cancellation);
}

/// The resume function of a task that waits with handlers: removes them before
/// the wait returns.
SWIFT_CC(swiftasync)
static void split_continuation_remove_handlers_resume_adapter(
    SWIFT_ASYNC_CONTEXT AsyncContext *_context) {
  auto *context = static_cast<ContinuationAsyncContext *>(_context);
  removeSplitContinuationHandlers(swift_task_getCurrent(),
                                  storageOfContext(context)->PublishedHandlers);
  return context->ResumeParent(context);
}

SWIFT_CC(swiftasync)
void swift::swift_continuation_awaitSplit(SplitContinuation *continuation,
                                          void *cancellationRecord,
                                          void *escalationRecord) {
  auto *storage =
      reinterpret_cast<SplitContinuationStorage *>(continuation);
  auto *context = contextOfStorage(storage);
  auto task = swift_task_getCurrent();
  assert(task && "awaiting a split continuation without a task");

  // Bind the resume-point to the current task. IRGen has already stored this
  // frame's Parent and ResumeParent into the context immediately before calling
  // us, so (re-)establish the task's resume state to point at them.
  task->ResumeContext = context;
  task->ResumeTask = context->ResumeParent;

  // Record where this task resumes: the executor it is suspending on. This
  // happens here rather than at create, because the task may change isolation
  // in between.
  context->ResumeToExecutor = getCurrentContinuationResumeExecutor();

  // Record ourselves as the awaiting task *before* transitioning to Awaited, so
  // that a racing resume that observes Awaited finds a valid task to enqueue.
  // The store is published by the release CAS below.
  storage->AwaitingTask = task;

  concurrency::trace::task_continuation_await(context);

  auto &sync = context->AwaitSynchronization;

  auto oldStatus = sync.load(std::memory_order_acquire);
  assert((oldStatus == ContinuationStatus::Pending ||
          oldStatus == ContinuationStatus::Resumed) &&
         "awaiting a corrupt or already-awaited continuation");

  // If the status is already Resumed (resume raced ahead), continue inline
  // without suspending. The handlers don't run, because the wait doesn't
  // suspend.
  if (oldStatus == ContinuationStatus::Resumed)
    return context->ResumeParent(context);

  // Build the records of the handlers in the buffers the compiler allocated
  // for them in the waiting frame, around the function and context it stored
  // there. We read them out first, because the record overwrites the buffer.
  CancellationNotificationStatusRecord *cancellation = nullptr;
  if (cancellationRecord) {
    auto *handler =
        static_cast<SplitContinuationCancellationHandler *>(cancellationRecord);
    auto *function = handler->Function;
    auto *functionContext = handler->Context;
    cancellation = ::new (cancellationRecord)
        CancellationNotificationStatusRecord(function, functionContext);
  }
  EscalationNotificationStatusRecord *escalation = nullptr;
  if (escalationRecord) {
    auto *handler =
        static_cast<SplitContinuationEscalationHandler *>(escalationRecord);
    auto *function = handler->Function;
    auto *functionContext = handler->Context;
    escalation = ::new (escalationRecord)
        EscalationNotificationStatusRecord(function, functionContext);
  }
  bool hasHandlers = cancellation || escalation;

  // The handlers run inline for what happened before their records are
  // published: the cancellation handler if the task is already cancelled, and
  // the escalation handler with the current priority. The records are then
  // published.
  //
  // The inline calls don't hold the status record lock. If the task is
  // escalated before its records are published, publishing fails and the
  // escalation handler runs inline again, so it can see `(p, p)` and then
  // `(p, p2)`. This avoids us needing to run the handlers while holding the
  // lock.
  std::optional<JobPriority> reportedPriority;

  auto runHandlersInline = [&] {
    if (cancellation) {
      if (auto reason = getObservedCancellationReason(task)) {
        cancellation->run(*reason);
        // Cancelled is a final state so we don't have to publish a handler
        // record if we already ran the handler.
        cancellation = nullptr;
      }
    }
    if (escalation) {
      auto priority = task->_private()
                          ._status()
                          .load(std::memory_order_relaxed)
                          .getStoredPriority();
      if (!reportedPriority) {
        escalation->run(priority, priority);
        reportedPriority = priority;
      } else if (*reportedPriority != priority) {
        escalation->run(*reportedPriority, priority);
        reportedPriority = priority;
      }
    }
  };

  // Link the records to publish between `above` and the innermost record of
  // `status`, or return false if `status` doesn't match with what the handlers
  // were told. Called inside the status update, so it must not run them.
  auto linkRecords = [&](const ActiveTaskStatus &status,
                         TaskStatusRecord *above) {
    if (cancellation && getObservedCancellationReason(status))
      return false;
    if (escalation && status.getStoredPriority() != *reportedPriority)
      return false;

    TaskStatusRecord *parent = status.getInnermostRecord();
    if (cancellation) {
      cancellation->resetParent(parent);
      parent = cancellation;
    }
    if (escalation) {
      escalation->resetParent(parent);
      parent = escalation;
    }
    if (above)
      above->resetParent(parent);
    return true;
  };

  // Publishing takes the status record lock if the cancellation record is still
  // to be published and the task has a cancellation scope, because a scope can
  // be cancelled concurrently without a change to the task's status.
  auto publishNeedsLock = [&] {
    return cancellation && task->_private()
                               ._status()
                               .load(std::memory_order_relaxed)
                               .hasTaskCancellationScope();
  };

  auto markPublished = [&] {
    storage->PublishedHandlers = {cancellation, escalation};
  };

#if SWIFT_CONCURRENCY_TASK_TO_THREAD_MODEL
  if (hasHandlers) {
    while (true) {
      runHandlersInline();
      TaskStatusRecord *innermost = escalation;
      if (!innermost)
        innermost = cancellation;
      if (!innermost)
        break;
      // addStatusRecord already made `innermost` the innermost record.
      auto link = [&](ActiveTaskStatus status, ActiveTaskStatus &) {
        return linkRecords(status, nullptr);
      };
      auto status = task->_private()._status().load(std::memory_order_relaxed);
      if (publishNeedsLock()
              ? addStatusRecordWithLock(task, innermost, status, link)
              : addStatusRecord(task, innermost, status, link)) {
        markPublished();
        break;
      }
    }
  }

  do {
  ConditionVariable Cond;
  context->Cond = &Cond;
#else
  // Flag the task as suspended on the continuation. Any handlers are published
  // in the same status update, and removed again when the task resumes.
  if (hasHandlers) {
    while (true) {
      runHandlersInline();
      if (!cancellation && !escalation) {
        task->flagAsSuspendedOnContinuation(context);
        break;
      }
      if (task->flagAsSuspendedOnContinuation(context, publishNeedsLock(),
                                              linkRecords)) {
        markPublished();
        task->ResumeTask = split_continuation_remove_handlers_resume_adapter;
        break;
      }
    }
  } else {
    task->flagAsSuspendedOnContinuation(context);
  }
#endif

  // Try to transition to Awaited.
  bool success =
    sync.compare_exchange_strong(oldStatus, ContinuationStatus::Awaited,
                                 /*success*/ std::memory_order_release,
                                 /*failure*/ std::memory_order_acquire);

  if (success) {
#if SWIFT_CONCURRENCY_TASK_TO_THREAD_MODEL
    Cond.lock();
    do {
      Cond.wait();
      oldStatus = sync.load(std::memory_order_relaxed);
    } while (oldStatus != ContinuationStatus::Resumed);
    Cond.unlock();
#else
    // Successfully suspended.
    _swift_task_clearCurrent();
    return;
#endif
  }

  // The CAS failed, which (given a strong exchange) means a resume raced and
  // won.
  assert(oldStatus == ContinuationStatus::Resumed &&
         "continuation was concurrently corrupted or awaited");

#if SWIFT_CONCURRENCY_TASK_TO_THREAD_MODEL
  } while (false);
#else
  task->resumeRunningAfterFailedSuspend();
#endif

  removeSplitContinuationHandlers(task, storage->PublishedHandlers);
  return context->ResumeParent(context);
}

ContinuationAsyncContext *swift::getSplitContinuationContext(void *continuation) {
  if (!isSplitContinuation(continuation))
    return nullptr;
  return contextOfStorage(storageOfContinuation(continuation));
}

