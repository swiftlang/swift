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
#include "swift/Runtime/Error.h"
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
//     *current* task, records the awaiting task on the context, and performs
//     the Pending->Awaited transition. Cancellation and escalation handlers
//     are not part of this: ContinuationWaiter installs them beforehand with
//     the regular per-handler entry points.
//   * resume: the regular swift_continuation_resume* entry points, and
//     swift_continuation_resumeOnExecutors, which read the marker word and
//     route to the split implementation. The resume works *by context* (the
//     value/error is stored into the context first), because
//     task->ResumeContext cannot be relied on: a resume may arrive before the
//     await. It enqueues the recorded awaiting task, or donates the resuming
//     thread if one was offered and it belongs to the executor the task
//     resumes on.

namespace {

/// The storage backing a split continuation. A raw continuation referring to a
/// split continuation is the address of one of these.
///
/// It is a Job, of a kind of its own.  Resuming a continuation that has a Void
/// result is just what running such a job does, so a sleeping task can hand its
/// continuation straight to an executor as the job that wakes it, with no
/// stand-in job to allocate.  Executors must run the job, rather than resuming
/// the continuation themselves, since the job may have been cancelled, in which
/// case running it resumes the continuation by throwing.  All that running it
/// does is resume the continuation, which enqueues the awaiting task, so it is
/// a "resumption" job (see swift_job_isResumption), which a scheduler should
/// run as soon as it is due or cancelled rather than enqueue: its priority is
/// fixed here, and the task may have been escalated since.
///
/// The layout also makes a split continuation resumable by *any* code that can
/// resume a regular one, including code compiled before split continuations
/// existed.  `ResumeContext` sits at exactly the offset at which an AsyncTask
/// keeps its own, and a regular continuation is a task, so the job's kind is
/// enough to tell the two apart.
///
/// AsyncTask isn't standard-layout, so `offsetof` on it warns; we rely on its
/// layout deliberately and assert it below.
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Winvalid-offsetof"
struct SplitContinuationStorage : public Job {
  /// Points at the ContinuationAsyncContext that follows in this allocation.
  AsyncContext * __ptrauth_swift_task_resume_context ResumeContext;

  /// The task awaiting the continuation, recorded at await time so that a
  /// resume-by-context can make it runnable. Null until then, which is also how
  /// a resume that races ahead of the await is recognised.
  AsyncTask *AwaitingTask;

  /// Whether the continuation's result type is empty (that is, Void), which
  /// is what it takes to be able to resume it by running it as a job: a job
  /// has no way to supply a value.
  bool ResultIsEmpty;

  /// Decides, for a continuation that has been scheduled as a job, whether
  /// running the job resumes it normally or by throwing: null until one or the
  /// other happens, then either `Fired` (the job ran first) or the error that
  /// `swift_continuation_cancelScheduled` claimed it with, at +1.
  std::atomic<SwiftError *> Outcome;

  /// The value of `Outcome` once the job has run without being cancelled.
  static SwiftError *fired() { return reinterpret_cast<SwiftError *>(1); }

  SplitContinuationStorage(JobPriority priority, bool resultIsEmpty)
      : Job({JobKind::SplitContinuation, priority}, &process,
            jobHeapMetadataPtr, /*captureCurrentVoucher=*/false),
        ResumeContext(nullptr), AwaitingTask(nullptr),
        ResultIsEmpty(resultIsEmpty), Outcome(nullptr) {}

  /// Running the job resumes the continuation: normally, unless it was
  /// cancelled first, in which case by throwing the error it was cancelled
  /// with.
  SWIFT_CC(swiftasync)
  static void process(Job *job) {
    auto *storage = cast<SplitContinuationStorage>(job);
    if (!storage->ResultIsEmpty)
      swift_Concurrency_fatalError(
          0, "a split continuation can only be run as a job if its result is "
             "empty");
    auto *continuation = reinterpret_cast<AsyncTask *>(storage);
    SwiftError *error = storage->Outcome.exchange(fired(),
                                                  std::memory_order_acquire);
    if (error)
      swift_continuation_throwingResumeWithError(continuation, error);
    else
      swift_continuation_resume(continuation);
  }

  static bool classof(const Job *job) {
    return job->Flags.getKind() == JobKind::SplitContinuation;
  }
};

static_assert(offsetof(SplitContinuationStorage, ResumeContext) ==
                  offsetof(AsyncTask, ResumeContext),
              "a split continuation must be loadable as if it were a "
              "task, so its ResumeContext must sit at the task's offset");
#pragma clang diagnostic pop

/// The context follows the storage, at the context's alignment.
static size_t splitContextOffset() {
  return llvm::alignTo(sizeof(SplitContinuationStorage),
                       alignof(ContinuationAsyncContext));
}

/// Whether `continuation` is a split continuation rather than a task.  Either
/// way it is a job, so we can ask it what kind it is.
static bool isSplitContinuation(const void *continuation) {
  return isa<SplitContinuationStorage>(
      reinterpret_cast<const Job *>(continuation));
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
SplitContinuation *
swift::swift_continuation_createSplit(const Metadata *resultType) {
  // Task-allocate the continuation's storage, the context and trailing storage
  // for the result value. The result storage has to live here rather than in
  // the awaiting frame because a resume can arrive before the await does and
  // needs somewhere to write the value.
  auto *task = swift_task_getCurrent();
  assert(task && "creating a split continuation outside of a task");

  size_t contextOffset = splitContextOffset();
  size_t resultAlign = resultType->vw_alignment();
  size_t resultSize = resultType->vw_size();
  size_t resultOffset =
      llvm::alignTo(contextOffset + sizeof(ContinuationAsyncContext),
                    resultAlign);
  // swift_task_alloc only guarantees MaximumAlignment, so an over-aligned
  // result needs slack to align its storage within the allocation.
  size_t slack = resultAlign > MaximumAlignment ? resultAlign - 1 : 0;

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
  // Overwritten by the await.
  context->ResumeToExecutor = SerialExecutorRef::generic();
#if SWIFT_CONCURRENCY_TASK_TO_THREAD_MODEL
  context->Cond = nullptr;
#endif

  // The job takes the priority of the task that creates it, which is also
  // the task that will await it.  It doesn't capture a voucher, since the
  // great majority of split continuations are never scheduled as jobs, and the
  // task that is awaiting the continuation carries its own.
  //
  // Note that if the task's priority is later escalated, this job's priority
  // is not updated.  That is why schedulers should run it when it is due or
  // cancelled, rather than enqueue it: running it enqueues the task, at the
  // task's own priority.
  auto *storage = new (allocation) SplitContinuationStorage(
      swift_task_getCurrent()->getPriority(), resultSize == 0);

  // Relaxed is fine: resumption must happen-after this call returns and the
  // continuation is handed to the executor.
  context->AwaitSynchronization.store(ContinuationStatus::Pending,
                                      std::memory_order_relaxed);

  storage->ResumeContext = context;

  concurrency::trace::task_continuation_init(task, context);

  return reinterpret_cast<SplitContinuation *>(storage);
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
  // without suspending. Any handlers were already installed by the caller, so
  // they may have run regardless: a cancellation handler runs immediately if
  // the task was already cancelled, and an escalation handler is always called
  // once when it is installed.
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

ContinuationAsyncContext *swift::getSplitContinuationContext(void *continuation) {
  if (!isSplitContinuation(continuation))
    return nullptr;
  return contextOfStorage(storageOfContinuation(continuation));
}

SWIFT_CC(swift)
void swift::swift_continuation_destroySplit(SplitContinuation *continuation) {
  // The await has resolved and taken the result out of the context, and the
  // resume-exactly-once contract guarantees the resume side is finished with the
  // context before the awaiting task runs again, so it is safe to free.
  static_cast<Job *>(reinterpret_cast<SplitContinuationStorage *>(continuation))
      ->~Job();
  swift_task_dealloc(continuation);
}

SWIFT_CC(swift)
bool swift::swift_continuation_cancelScheduled(SplitContinuation *continuation,
                                               SwiftError *error) {
  auto *storage = reinterpret_cast<SplitContinuationStorage *>(continuation);
  SwiftError *expected = nullptr;
  // Release, so that the job sees the error that it is to throw.
  if (storage->Outcome.compare_exchange_strong(expected, error,
                                               std::memory_order_release,
                                               std::memory_order_relaxed))
    return true;

  // The job has already run, or another cancellation got there first.
  swift_errorRelease(error);
  return false;
}
