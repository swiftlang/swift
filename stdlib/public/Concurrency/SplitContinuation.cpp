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

#include "ExecutorTracking.h"
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
//   * create (swift_continuation_createSplit): allocates the continuation's
//     storage, its context, and the result storage, state Pending; does NOT
//     bind a resume-point and does NOT touch task execution state. Returns
//     the continuation. Its storage is shaped like the front of a task, so
//     one Continuation type can carry either flavour and a resume works for
//     both.
//   * await (swift_continuation_awaitSplit): binds the resume-point to the
//     *current* task, records the awaiting task on the context, publishes any
//     handler records together with the suspension, and performs the
//     Pending->Awaited transition.
//   * resume: the regular swift_continuation_resume* entry points, which read
//     the marker word and route to the split implementation. The resume works
//     *by context* (the value/error is stored into the context by the caller
//     first), because task->ResumeContext cannot be relied on: a resume may
//     arrive before the await. It enqueues the recorded awaiting task, or
//     donates the resuming thread if asked to and if that thread belongs to the
//     executor the task resumes on.

namespace {

/// The storage backing a split continuation. A raw continuation referring to a
/// split continuation is the address of one of these.
///
/// The layout makes a split continuation resumable by *any* code that can
/// resume a regular one, including code compiled before split continuations
/// existed. `ResumeContext` sits at exactly the offset at which an AsyncTask
/// keeps its own.
struct SplitContinuationStorage {
  /// Distinguishes this from a task.
  uintptr_t Marker;

  /// Padding placing `ResumeContext` where a task keeps its own.
  char TaskPrefixPadding[offsetof(AsyncTask, ResumeContext) - sizeof(uintptr_t)];

  /// Points at the ContinuationAsyncContext that follows in this allocation.
  void *ResumeContext;

  /// The task awaiting the continuation, recorded at await time so that a
  /// resume-by-context can make it runnable. Null until then, which is also how
  /// a resume that races ahead of the await is recognised.
  AsyncTask *AwaitingTask;

  /// Whether the resumer has said which executors it is running on, by calling
  /// swift_continuation_setResumingExecutors before the resume. A resumer that
  /// has not said is simply enqueued, as a regular continuation is.
  bool ResumingExecutorsKnown;

  /// The executors the caller of swift_continuation_setResumingExecutors said
  /// the resuming thread is running on. Only meaningful when
  /// `ResumingExecutorsKnown` is set, and compared against the executors the
  /// task will actually resume on to decide whether the thread may be donated.
  SerialExecutorRef ResumingSerialExecutor;
  TaskExecutorRef ResumingTaskExecutor;
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

/// The context follows the storage, at the context's alignment.
static size_t splitContextOffset() {
  return llvm::alignTo(sizeof(SplitContinuationStorage),
                       alignof(ContinuationAsyncContext));
}

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

/// Shared tail of the split resume entry points: perform the Pending/Awaited ->
/// Resumed transition and, if a task was awaiting, make it runnable. The result
/// value (or error) must already be stored in the context. Mirrors
/// resumeTaskAfterContinuation, but works purely from the context (there is no
/// task->ResumeContext to recover) and tolerates the resume-before-await race
/// where no awaiting task has been recorded yet.
static void
resumeSplitTaskAfterContinuation(ContinuationAsyncContext *context) {
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
  // A resumer that said which executors it is running on is offering its thread
  // to run the resumed task inline. The offer is taken only if that thread
  // belongs to the executor the task actually resumes on; otherwise, and when
  // no offer was made at all, the task is enqueued as usual.
  if (storage->ResumingExecutorsKnown) {
    auto resumeExecutor = context->ResumeToExecutor;

    // Which executor the task resumes on is decided at *await* time, and the
    // resumer learns which executors it is on when it is handed the
    // continuation, which is necessarily earlier. So the two can disagree when
    // the awaiting side may have moved isolation, or taken a task executor
    // preference, between being handed the continuation and awaiting it. A
    // disagreement is therefore not a misuse: the offered thread simply cannot
    // be used, and the task is enqueued instead.
    //
    // mustSwitchToRun already treats an undefined task executor as the default
    // executor on both sides, so a task that resumes with no isolation and no
    // task executor preference only takes the offer when what was offered is
    // itself the default. Never some other, unrelated executor the resumer
    // merely happened to be running on.
    auto preferredTaskExecutor = task->getPreferredTaskExecutor();
    if (!mustSwitchToRun(storage->ResumingSerialExecutor, resumeExecutor,
                         storage->ResumingTaskExecutor, preferredTaskExecutor)) {
      // Donate the current thread to the resumed task like
      // ExecutorJob.runSynchronously / swift_job_run, which establishes the
      // tracking context *before* flagAsRunning and save/restores the active
      // task.
      _swift_tsan_acquire(static_cast<Job *>(task));
      return swift_job_run(task, resumeExecutor);
    }
  }

  task->flagAsAndEnqueueOnExecutor(context->ResumeToExecutor);
#endif /* SWIFT_CONCURRENCY_TASK_TO_THREAD_MODEL */
}


SWIFT_CC(swift)
SplitContinuation *
swift::swift_continuation_createSplit(const Metadata *resultType) {
  // Task-allocate the continuation's storage, the context and trailing storage
  // for the result value. The result storage has to live here rather than in
  // the awaiting frame because a resume can arrive before the await does and
  // needs somewhere to write the value.
  assert(swift_task_getCurrent() &&
         "creating a split continuation outside of a task");

  size_t contextOffset = splitContextOffset();
  size_t resultAlign = resultType->vw_alignment();
  size_t resultSize = resultType->vw_size();
  size_t resultOffset =
      llvm::alignTo(contextOffset + sizeof(ContinuationAsyncContext),
                    resultAlign);
  // swift_task_alloc only guarantees the maximum standard alignment, so an
  // over-aligned result needs slack to align its storage within the allocation.
  size_t slack =
      resultAlign > alignof(std::max_align_t) ? resultAlign - 1 : 0;

  void *allocation = swift_task_alloc(resultOffset + slack + resultSize);
  auto *context = reinterpret_cast<ContinuationAsyncContext *>(
      static_cast<char *>(allocation) + contextOffset);

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
  context->NormalResult = reinterpret_cast<OpaqueValue *>(llvm::alignTo(
      reinterpret_cast<uintptr_t>(allocation) + resultOffset, resultAlign));
  // The fallback for an await that finds no executor tracking info to record.
  context->ResumeToExecutor = SerialExecutorRef::generic();
#if SWIFT_CONCURRENCY_TASK_TO_THREAD_MODEL
  context->Cond = nullptr;
#endif

  auto *storage = static_cast<SplitContinuationStorage *>(allocation);
  storage->Marker = SplitContinuationMarker;
  storage->AwaitingTask = nullptr;
  storage->ResumingExecutorsKnown = false;
  storage->ResumingSerialExecutor = SerialExecutorRef::generic();
  storage->ResumingTaskExecutor = TaskExecutorRef::undefined();

  // Relaxed is fine: resumption must happen-after this call returns and the
  // continuation is handed to the executor.
  context->AwaitSynchronization.store(ContinuationStatus::Pending,
                                      std::memory_order_relaxed);

  storage->ResumeContext = context;

  concurrency::trace::task_continuation_init(nullptr, context);

  return static_cast<SplitContinuation *>(allocation);
}


SWIFT_CC(swiftasync)
void swift::swift_continuation_awaitSplit(SplitContinuation *continuation) {
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

  // Record where this task resumes. Unlike a regular continuation, which
  // resumes to the generic executor and hops back to its isolation afterwards,
  // a split continuation records the task's actual executor: a synchronous
  // resume is checked against it, and donating a thread to a task that is
  // already on the right executor avoids the hop altogether.
  if (auto *tracking = ExecutorTrackingInfo::current()) {
    context->ResumeToExecutor = tracking->getActiveExecutor();
  }

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

  // If the status is already Resumed (resume raced ahead), continue inline.
  // Nothing was suspended, so the handler records are never published and
  // neither handler can fire.
  if (oldStatus == ContinuationStatus::Resumed) {
    return context->ResumeParent(context);
  }

#if SWIFT_CONCURRENCY_TASK_TO_THREAD_MODEL
  do {
  ConditionVariable Cond;
  context->Cond = &Cond;
#else
  // Flag the task as suspended on the continuation. Any cancellation or
  // priority-escalation handlers were installed before we got here, by the
  // regular per-handler entry points.
  task->flagAsSuspendedOnContinuation(context);
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

  return context->ResumeParent(context);
}

bool swift::tryResumeSplitContinuation(void *continuation, SwiftError *error) {
  if (!isSplitContinuation(continuation))
    return false;

  auto *context = contextOfStorage(storageOfContinuation(continuation));
  concurrency::trace::task_continuation_resume(context, error != nullptr);
  if (error)
    context->ErrorResult = error;
  resumeSplitTaskAfterContinuation(context);
  return true;
}

SWIFT_CC(swift)
void swift::swift_continuation_setResumingExecutors(
    SplitContinuation *continuation, SerialExecutorRef serialExecutor,
    TaskExecutorRef taskExecutor) {
  // Setting the resume executors is only supported on split continuations.
  if (!isSplitContinuation(continuation))
    return;

  // Written by the resuming thread before it resumes, and read only by the
  // resume itself, so no synchronization is needed.
  auto *storage = storageOfContinuation(continuation);
  storage->ResumingSerialExecutor = serialExecutor;
  storage->ResumingTaskExecutor = taskExecutor;
  storage->ResumingExecutorsKnown = true;
}

SWIFT_CC(swift)
void swift::swift_continuation_destroySplit(SplitContinuation *continuation) {
  // The await has resolved and taken the result out of the context, and the
  // resume-exactly-once contract guarantees the resume side is finished with the
  // context before the awaiting task runs again, so it is safe to free.
  swift_task_dealloc(continuation);
}

