///===--- Actor.cpp - Standard actor implementation ------------------------===///
///
/// This source file is part of the Swift.org open source project
///
/// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
/// Licensed under Apache License v2.0 with Runtime Library Exception
///
/// See https:///swift.org/LICENSE.txt for license information
/// See https:///swift.org/CONTRIBUTORS.txt for the list of Swift project authors
///
///===----------------------------------------------------------------------===///
///
/// The default actor implementation for Swift actors, plus related
/// routines such as generic executor enqueuing and switching.
///
///===----------------------------------------------------------------------===///

#include "swift/Runtime/Concurrency.h"

#include "swift/Runtime/Atomic.h"
#include "swift/Runtime/Mutex.h"
#include "swift/Runtime/ThreadLocal.h"
#include "swift/ABI/Actor.h"
#include "llvm/ADT/PointerIntPair.h"

using namespace swift;

/// Should we yield the thread?
static bool shouldYieldThread() {
  // FIXME: system scheduler integration
  return false;
}

/*****************************************************************************/
/*********************** DEFAULT ACTOR IMPLEMENTATION ************************/
/*****************************************************************************/

namespace {

class DefaultActorImpl;

/// A job to process a default actor.  Allocated inline in the actor.
class ProcessInlineJob : public Job {
public:
  ProcessInlineJob(JobPriority priority)
    : Job({JobKind::DefaultActorInline, priority}, &process) {}

  SWIFT_CC(swiftasync)
  static void process(Job *job, ExecutorRef executor);

  static bool classof(const Job *job) {
    return job->Flags.getKind() == JobKind::DefaultActorInline;
  }
};

/// A job to process a default actor that's allocated separately from
/// the actor but doesn't need the override mechanics.
class ProcessOutOfLineJob : public Job {
  DefaultActorImpl *Actor;
public:
  ProcessOutOfLineJob(DefaultActorImpl *actor, JobPriority priority)
    : Job({JobKind::DefaultActorSeparate, priority}, &process),
      Actor(actor) {}

  SWIFT_CC(swiftasync)
  static void process(Job *job, ExecutorRef executor);

  static bool classof(const Job *job) {
    return job->Flags.getKind() == JobKind::DefaultActorSeparate;
  }
};

/// A job to process a default actor with a new priority; allocated
/// separately from the actor.
class ProcessOverrideJob;

/// Information about the currently-running processing job.
struct RunningJobInfo {
  enum KindType : uint8_t {
    Inline, Override, Other
  };
  KindType Kind;
  JobPriority Priority;
  ProcessOverrideJob *OverrideJob;

  bool wasInlineJob() const {
    return Kind == Inline;
  }

  static RunningJobInfo forOther(JobPriority priority) {
    return {Other, priority, nullptr};
  }
  static RunningJobInfo forInline(JobPriority priority) {
    return {Inline, priority, nullptr};
  }
  static RunningJobInfo forOverride(ProcessOverrideJob *job);

  void setAbandoned();
  void setRunning();
  bool waitForActivation();
};

class JobRef {
  enum : uintptr_t {
    NeedsPreprocessing = 0x1,
    IsOverride = 0x2,
    JobMask = ~uintptr_t(NeedsPreprocessing | IsOverride)
  };

  /// A Job* that may have one of the two bits above mangled into it.
  uintptr_t Value;

  JobRef(Job *job, unsigned flags)
    : Value(reinterpret_cast<uintptr_t>(job) | flags) {}
public:
  constexpr JobRef() : Value(0) {}

  /// Return a reference to a job that's been properly preprocessed.
  static JobRef getPreprocessed(Job *job) {
    /// We allow null pointers here.
    return { job, 0 };
  }

  /// Return a reference to a job that hasn't been preprocesssed yet.
  static JobRef getUnpreprocessed(Job *job) {
    assert(job && "passing a null job");
    return { job, NeedsPreprocessing };
  }

  /// Return a reference to an override job, which needs special
  /// treatment during preprocessing.
  static JobRef getOverride(ProcessOverrideJob *job);

  /// Is this a null reference?
  operator bool() const { return Value != 0; }

  /// Does this job need to be pre-processed before we can treat
  /// the job queue as a proper queue?
  bool needsPreprocessing() const {
    return Value & NeedsPreprocessing;
  }

  /// Is this an unpreprocessed override job?
  bool isOverride() const {
    return Value & IsOverride;
  }

  /// Given that this is an override job, return it.
  ProcessOverrideJob *getAsOverride() const {
    assert(isOverride());
    return reinterpret_cast<ProcessOverrideJob*>(Value & JobMask);
  }
  ProcessOverrideJob *getAsPreprocessedOverride() const;

  Job *getAsJob() const {
    assert(!isOverride());
    return reinterpret_cast<Job*>(Value & JobMask);
  }
  Job *getAsPreprocessedJob() const {
    assert(!isOverride() && !needsPreprocessing());
    return reinterpret_cast<Job*>(Value);
  }

  bool operator==(JobRef other) const {
    return Value == other.Value;
  }
  bool operator!=(JobRef other) const {
    return Value != other.Value;
  }
};

/// The default actor implementation.
///
/// Ownership of the actor is subtle.  Jobs are assumed to keep the actor
/// alive as long as they're executing on it; this allows us to avoid
/// retaining and releasing whenever threads are scheduled to run a job.
/// While jobs are enqueued on the actor, there is a conceptual shared
/// ownership of the currently-enqueued jobs which is passed around
/// between threads and processing jobs and managed using extra retains
/// and releases of the actor.  The basic invariant is as follows:
///
/// - Let R be 1 if there are jobs enqueued on the actor or if a job
///   is currently running on the actor; otherwise let R be 0.
/// - Let N be the number of active processing jobs for the actor.
/// - N >= R
/// - There are N - R extra retains of the actor.
///
/// We can think of this as there being one "owning" processing job
/// and K "extra" jobs.  If there is a processing job that is actively
/// running the actor, it is always the owning job; otherwise, any of
/// the N jobs may win the race to become the owning job.
///
/// We then have the following ownership rules:
///
/// - When we enqueue the first job on an actor, then R becomes 1, and
///   we must create a processing job so that N >= R.  We do not need to
///   retain the actor.
/// - When we create an extra job to process an actor (e.g. because of
///   priority overrides), N increases but R remains the same.  We must
///   retain the actor.
/// - When we start running an actor, our job definitively becomes the
///   owning job, but neither N nor R changes.  We do not need to retain
///   the actor.
/// - When we go to start running an actor and for whatever reason we
///   don't actually do so, we are eliminating an extra processing job,
///   and so N decreases but R remains the same.  We must release the
///   actor.
/// - When we are running an actor and give it up, and there are no
///   remaining jobs on it, then R becomes 0 and N decreases by 1.
///   We do not need to release the actor.
/// - When we are running an actor and give it up, and there are jobs
///   remaining on it, then R remains 1 but N is decreasing by 1.
///   We must either release the actor or create a new processing job
///   for it to maintain the balance.
class DefaultActorImpl : public HeapObject {
  enum class Status {
    /// The actor is not currently scheduled.  Completely redundant
    /// with the job list being empty.
    Idle,

    /// There is currently a job scheduled to process the actor at the
    /// stored max priority.
    Scheduled,

    /// There is currently a thread processing the actor at the stored
    /// max priority.
    Running
  };

  struct Flags : public FlagSet<size_t> {
    enum : size_t {
      Status_offset = 0,
      Status_width = 2,

      HasActiveInlineJob = 2,

      MaxPriority = 8,
      MaxPriority_width = JobFlags::Priority_width,

      // FIXME: add a reference to the running thread ID so that we
      // can boost priorities.
    };

    /// What is the current high-level status of this actor?
    FLAGSET_DEFINE_FIELD_ACCESSORS(Status_offset, Status_width, Status,
                                   getStatus, setStatus)

    /// Is there currently an active processing job allocated inline
    /// in the actor?
    FLAGSET_DEFINE_FLAG_ACCESSORS(HasActiveInlineJob,
                                  hasActiveInlineJob, setHasActiveInlineJob)

    /// What is the maximum priority of jobs that are currently running
    /// or enqueued on this actor?
    ///
    /// Note that the above isn't quite correct: we don't actually
    /// lower this after we finish processing higher-priority tasks.
    /// (Doing so introduces some subtleties around kicking off
    /// lower-priority processing jobs.)
    FLAGSET_DEFINE_FIELD_ACCESSORS(MaxPriority, MaxPriority_width,
                                   JobPriority,
                                   getMaxPriority, setMaxPriority)
  };

  /// This is designed to fit into two words, which can generally be
  /// done lock-free on all our supported platforms.
  struct alignas(2 * sizeof(void*)) State {
    JobRef FirstJob;
    struct Flags Flags;
  };

  swift::atomic<State> CurrentState;

  friend class ProcessInlineJob;
  union {
    ProcessInlineJob JobStorage;
  };

public:

  /// Properly construct an actor, except for the heap header.
  void initialize() {
    new (&CurrentState) std::atomic<State>(State{JobRef(), Flags()});
  }

  /// Properly destruct an actor, except for the heap header.
  void destroy() {
    assert(CurrentState.load(std::memory_order_relaxed).Flags.getStatus()
             == Status::Idle && "actor not idle during destruction?");
  }

  /// Add a job to this actor.
  void enqueue(Job *job);

  /// Take over running this actor in the current thread, if possible.
  bool tryAssumeThread(RunningJobInfo runner);

  /// Give up running this actor in the current thread.
  void giveUpThread(RunningJobInfo runner);

  /// Claim the next job off the actor or give it up.
  Job *claimNextJobOrGiveUp(bool actorIsOwned, RunningJobInfo runner);

private:
  /// Schedule an inline processing job.  This can generally only be
  /// done if we know nobody else is trying to do it at the same time,
  /// e.g. if this thread just sucessfully transitioned the actor from
  /// Idle to Scheduled.
  void scheduleNonOverrideProcessJob(JobPriority priority,
                                     bool hasActiveInlineJob);

  static DefaultActorImpl *fromInlineJob(Job *job) {
    assert(isa<ProcessInlineJob>(job));
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Winvalid-offsetof"
    return reinterpret_cast<DefaultActorImpl*>(
      reinterpret_cast<char*>(job) - offsetof(DefaultActorImpl, JobStorage));
#pragma clang diagnostic pop
  }

  class OverrideJobCache {
    ProcessOverrideJob *Job = nullptr;
    bool IsNeeded = false;
#ifndef NDEBUG
    bool WasCommitted = false;
#endif
  public:
    OverrideJobCache() = default;
    OverrideJobCache(const OverrideJobCache &) = delete;
    OverrideJobCache &operator=(const OverrideJobCache &) = delete;
    ~OverrideJobCache() {
      assert(WasCommitted && "didn't commit override job!");
    }

    void addToState(DefaultActorImpl *actor, State &newState);
    void setNotNeeded() { IsNeeded = false; }
    void commit();
  };
};

} /// end anonymous namespace

static_assert(sizeof(DefaultActorImpl) <= sizeof(DefaultActor) &&
              alignof(DefaultActorImpl) <= alignof(DefaultActor),
              "DefaultActorImpl doesn't fit in DefaultActor");

static DefaultActorImpl *asImpl(DefaultActor *actor) {
  return reinterpret_cast<DefaultActorImpl*>(actor);
}

static DefaultActor *asAbstract(DefaultActorImpl *actor) {
  return reinterpret_cast<DefaultActor*>(actor);
}

/*****************************************************************************/
/************************** DEFAULT ACTOR TRACKING ***************************/
/*****************************************************************************/

namespace {

enum Mode {
  /// Shadow any existing frame, leaving it untouched.
  ShadowExistingFrame,

  /// Update any existing frame if possible.
  UpdateExistingFrame
};

/// A little class for tracking whether there's a frame processing
/// default actors in the current thread.
///
/// The goal of this class is to encapsulate uses of the central variable.
/// We want to potentially use a more efficient access pattern than
/// ordinary thread-locals when that's available.
class DefaultActorProcessingFrame {
  using ValueType = llvm::PointerIntPair<DefaultActorImpl*, 1, bool>;

  /// The active default actor on the current thread, if any.
  /// This may still need to be tracked separately from the active
  /// executor, if/when we start tracking that in thread-local storage.
  static SWIFT_RUNTIME_DECLARE_THREAD_LOCAL(ValueType, ThreadLocalValue);

  ValueType SavedValue;
  bool IsNeeded;

public:
  /// Flag that this thread is processing the given actor (or null,
  /// for generic processing) and set up a processing frame if we
  /// don't already have one.
  DefaultActorProcessingFrame(DefaultActorImpl *actor, Mode mode) {
    // If we should shadow an existing frame, save any value that
    // it might have set.
    if (mode == ShadowExistingFrame) {
      SavedValue = ThreadLocalValue.get();
      IsNeeded = true;

    // If we should update an existing frame, just replace any value
    // that it might have set.
    } else {
      IsNeeded = !ThreadLocalValue.get().getInt();
      SavedValue = ValueType();
    }

    ThreadLocalValue.set(ValueType(actor, true));
  }

  DefaultActorProcessingFrame(const DefaultActorProcessingFrame &) = delete;
  DefaultActorProcessingFrame &operator=(
                              const DefaultActorProcessingFrame &) = delete;

  /// Return the currently active actor.
  DefaultActorImpl *getActiveActor() {
    return ThreadLocalValue.get().getPointer();
  }

  /// Exit the frame.  This isn't a destructor intentionally, because
  /// we need to be able to tail-call out of frames that might have
  /// optimistically made one of these.
  void exit() {
    ThreadLocalValue.set(SavedValue);
  }

  /// Return whether this frame was needed; if it was not, then it's
  /// okay to abandon it without calling exit().  This is only meaningful
  /// when constructed in the UpdateExistingFrame mode.
  bool isNeeded() {
    return IsNeeded;
  }
};

/// Define the thread-local.
SWIFT_RUNTIME_DECLARE_THREAD_LOCAL(
  DefaultActorProcessingFrame::ValueType,
  DefaultActorProcessingFrame::ThreadLocalValue);

} /// end anonymous namespace

/*****************************************************************************/
/*********************** DEFAULT ACTOR IMPLEMENTATION ************************/
/*****************************************************************************/

/// Given that a job is enqueued normally on a default actor, get/set
/// the next job in the actor's queue.
///
/// Note that this must not be used on the override jobs that can appear
/// in the queue; those jobs are not actually in the actor's queue (they're
/// on the global execution queues).  So the actor's actual queue flows
/// through the NextJob field on those objects rather than through
/// the SchedulerPrivate fields.
static JobRef getNextJobInQueue(Job *job) {
  return *reinterpret_cast<JobRef*>(job->SchedulerPrivate);
}
static void setNextJobInQueue(Job *job, JobRef next) {
  *reinterpret_cast<JobRef*>(job->SchedulerPrivate) = next;
}

/// Schedule a processing job that doesn't have to be an override job.
///
/// We can either do this with inline storage or heap-allocated.
/// To ues inline storage, we need to verify that the hasActiveInlineJob
/// flag is not set in the state and then successfully set it.  The
/// argument reports that this has happened correctly.
///
/// We should only schedule a non-override processing job at all if
/// we're transferring ownership of the jobs in it; see the ownership
/// comment on DefaultActorImpl.
void DefaultActorImpl::scheduleNonOverrideProcessJob(JobPriority priority,
                                                     bool hasActiveInlineJob) {
  Job *job;
  if (hasActiveInlineJob) {
    job = new ProcessOutOfLineJob(this, priority);
  } else {
    job = new (&JobStorage) ProcessInlineJob(priority);
  }
  swift_task_enqueueGlobal(job);
}


namespace {

/// A job to process a specific default actor at a higher priority than
/// it was previously running at.
///
/// When an override job is successfully registered with an actor
/// (not enqueued there), the thread processing the actor and the
/// thread processing the override job coordinate by each calling
/// one of a set of methods on the object.
class ProcessOverrideJob : public Job {
  DefaultActorImpl *Actor;

  ConditionVariable::Mutex Lock;
  ConditionVariable Queue;

  /// Has the actor made a decision about this job yet?
  bool IsResolvedByActor = false;

  /// Has the job made a decision about itself yet?
  bool IsResolvedByJob = false;

  /// Has this job been abandoned?
  bool IsAbandoned = false;

public:
  /// SchedulerPrivate in an override job is used for actually scheduling
  /// the job, so the actor queue goes through this instead.
  ///
  /// We also use this temporarily for the list of override jobs on
  /// the actor that we need to wake up.
  JobRef NextJob;

public:
  ProcessOverrideJob(DefaultActorImpl *actor, JobPriority priority,
                     JobRef nextJob)
    : Job({JobKind::DefaultActorOverride, priority}, &process),
      Actor(actor), NextJob(nextJob) {}

  DefaultActorImpl *getActor() const { return Actor; }

  /// Called by the job to notify the actor that the job has chosen
  /// to abandon its work.  This is irrevocable: the job is not going
  /// to have a thread behind it.
  ///
  /// This may delete the job or cause it to be deleted on another thread.
  void setAbandoned() {
    bool shouldDelete = false;
    Lock.withLock([&] {
      assert(!IsResolvedByJob && "job already resolved itself");
      IsResolvedByJob = true;
      IsAbandoned = true;
      shouldDelete = IsResolvedByJob && IsResolvedByActor;
    });
    if (shouldDelete) delete this;    
  }

  /// Called by the job to notify the actor that the job has successfully
  /// taken over the actor and is now running it.
  ///
  /// This may delete the job object or cause it to be deleted on
  /// another thread.
  void setRunning() {
    bool shouldDelete = false;
    Lock.withLock([&] {
      assert(!IsResolvedByJob && "job already resolved itself");
      IsResolvedByJob = true;
      shouldDelete = IsResolvedByJob && IsResolvedByActor;
    });
    if (shouldDelete) delete this;    
  }

  /// Called by the job to wait for the actor to resolve what the job
  /// should do.
  bool waitForActivation() {
    bool isActivated = false;
    Lock.withLockOrWait(Queue, [&] {
      assert(!IsResolvedByJob && "job already resolved itself");
      if (IsResolvedByActor) {
        isActivated = !IsAbandoned;
        IsResolvedByJob = true;
        return true;
      }
      return false;
    });
    delete this;
    return isActivated;
  }

  /// Called by the actor to notify this job that the actor thinks it
  /// should try to take over the actor.  It's okay if that doesn't
  /// succeed (as long as that's because some other job is going to
  /// take over).
  ///
  /// This may delete the job or cause it to be deleted on another
  /// thread.
  bool wakeAndActivate() {
    bool shouldDelete = false;
    bool mayHaveBeenActivated = false;
    Lock.withLockThenNotifyAll(Queue, [&] {
      assert(!IsResolvedByActor && "actor already resolved this sjob");
      IsResolvedByActor = true;
      mayHaveBeenActivated = IsResolvedByJob && !IsAbandoned;
      shouldDelete = IsResolvedByJob && IsResolvedByActor;
    });
    if (shouldDelete) delete this;
    return mayHaveBeenActivated;
  }

  /// Called by the actor to notify this job that the actor does not
  /// think it should try to take over the actor.  It's okay if the
  /// job successfully takes over the actor anyway.
  ///
  /// This may delete the job or cause it to be deleted on another
  /// thread.
  void wakeAndAbandon() {
    bool shouldDelete = false;
    Lock.withLockThenNotifyAll(Queue, [&] {
      assert(!IsResolvedByActor && "actor already resolved this job");
      IsResolvedByActor = true;
      IsAbandoned = true;
      shouldDelete = IsResolvedByJob && IsResolvedByActor;
    });
    if (shouldDelete) delete this;
  }

  SWIFT_CC(swiftasync)
  static void process(Job *job, ExecutorRef _executor);

  static bool classof(const Job *job) {
    return job->Flags.getKind() == JobKind::DefaultActorOverride;
  }
};

} /// end anonymous namespace

JobRef JobRef::getOverride(ProcessOverrideJob *job) {
  return JobRef(job, NeedsPreprocessing | IsOverride);
}
ProcessOverrideJob *JobRef::getAsPreprocessedOverride() const {
  return cast_or_null<ProcessOverrideJob>(getAsPreprocessedJob());
}
RunningJobInfo RunningJobInfo::forOverride(ProcessOverrideJob *job) {
  return {Override, job->getPriority(), job};
}

/// Flag that the current processing job has been abandoned
/// and will not be running the actor.
void RunningJobInfo::setAbandoned() {
  if (OverrideJob) {
    OverrideJob->setAbandoned();
    OverrideJob = nullptr;
  }
}

/// Flag that the current processing job is now running the actor.
void RunningJobInfo::setRunning() {
  if (OverrideJob) {
    OverrideJob->setRunning();
    OverrideJob = nullptr;
  }
}

/// Try to wait for the current processing job to be activated,
/// if that's possible.  It's okay to call this multiple times
/// (or to call setAbandoned/setRunning after it) as long as
/// it's all on a single value.
bool RunningJobInfo::waitForActivation() {
  if (Kind == Override) {
    // If we don't have an override job, it's because we've already
    // waited for activation successfully.
    if (!OverrideJob) return true;

    bool result = OverrideJob->waitForActivation();
    OverrideJob = nullptr;
    return result;
  }
  return false;
}

/// Wake all the overrides in the given list, activating the first
/// that exactly matches the target priority, if any.
static void wakeOverrides(ProcessOverrideJob *nextOverride,
                          Optional<JobPriority> targetPriority) {
  bool hasAlreadyActivated = false;
  while (nextOverride) {
    // We have to advance to the next override before we call one of
    // the wake methods because they can delete the job immediately
    // (and even if they don't, we'd still be racing with deletion).
    auto cur = nextOverride;
    nextOverride = cur->NextJob.getAsPreprocessedOverride();

    if (hasAlreadyActivated ||
        !targetPriority ||
        cur->getPriority() != *targetPriority)
      cur->wakeAndAbandon();
    else
      hasAlreadyActivated = cur->wakeAndActivate();
  }
}

/// Flag that an override job is needed and create it.
void DefaultActorImpl::OverrideJobCache::addToState(DefaultActorImpl *actor,
                                                    State &newState) {
  IsNeeded = true;
  auto newPriority = newState.Flags.getMaxPriority();
  auto nextJob = newState.FirstJob;
  if (Job) {
    Job->Flags.setPriority(newPriority);
    Job->NextJob = nextJob;
  } else {
    // Override jobs are always "extra" from the perspective of our
    // ownership rules and so require a retain of the actor.  We must
    // do this before changing the actor state because other jobs may
    // race to release the actor as soon as we change the actor state.
    swift_retain(actor);
    Job = new ProcessOverrideJob(actor, newPriority, nextJob);
  }
  newState.FirstJob = JobRef::getOverride(Job);
}

/// Schedule the override job if we created one and still need it.
/// If we created one but didn't end up needing it (which can happen
/// with a race to override), destroy it.
void DefaultActorImpl::OverrideJobCache::commit() {
#ifndef NDEBUG
  assert(!WasCommitted && "committing override job multiple timee");
  WasCommitted = true;
#endif

  if (Job) {
    if (IsNeeded) {
      swift_task_enqueueGlobal(Job);
    } else {
      swift_release(Job->getActor());
      delete Job;
    }
  }
}

/// Preprocess the prefix of the actor's queue that hasn't already
/// been preprocessed:
///
/// - Split the jobs into registered overrides and actual jobs.
/// - Append the actual jobs to any already-preprocessed job list.
///
/// The returned job should become the new root of the job queue
/// (or may be immediately dequeued, in which its successor should).
/// All of the jobs in this list are guaranteed to be non-override jobs.
static Job *preprocessQueue(JobRef first,
                            JobRef previousFirst,
                            Job *previousFirstNewJob,
                            ProcessOverrideJob *&overridesToWake) {
  assert(previousFirst || previousFirstNewJob == nullptr);

  if (!first.needsPreprocessing())
    return first.getAsPreprocessedJob();

  Job *firstNewJob = nullptr;

  while (first != previousFirst) {
    // If we find something that doesn't need preprocessing, it must've
    // been left by a previous queue-processing, which means that
    // this must be our first attempt to preprocess in this processing.
    // Just treat the queue from this point as a well-formed whole
    // to which we need to add any new items we might've just found.
    if (!first.needsPreprocessing()) {
      assert(!previousFirst && !previousFirstNewJob);
      previousFirstNewJob = first.getAsPreprocessedJob();
      break;
    }

    // If the job is an override, add it to the list of override jobs
    // that we need to wake up.  Note that the list of override jobs
    // flows through NextJob; we must not use getNextJobInQueue because
    // that touches queue-private state, and the override job is
    // not enqueued on the actor, merely registered with it.
    if (first.isOverride()) {
      auto overrideJob = first.getAsOverride();
      first = overrideJob->NextJob;
      overrideJob->NextJob = JobRef::getPreprocessed(overridesToWake);
      overridesToWake = overrideJob;
      continue;
    }

    // If the job isn't an override, add it to the front of the list of
    // jobs we're building up.  Note that this reverses the order of
    // jobs; since enqueue() always adds jobs to the front, reversing
    // the order effectively makes the actor queue FIFO, which is what
    // we want.
    // FIXME: but we should also sort by priority
    auto job = first.getAsJob();
    first = getNextJobInQueue(job);
    setNextJobInQueue(job, JobRef::getPreprocessed(firstNewJob));
    firstNewJob = job;
  }

  // If there are jobs already in the queue, put the new jobs at the end.
  if (!firstNewJob) {
    firstNewJob = previousFirstNewJob;
  } else if (previousFirstNewJob) {
    auto cur = previousFirstNewJob;
    while (true) {
      auto next = getNextJobInQueue(cur).getAsPreprocessedJob();
      if (!next) {
        setNextJobInQueue(cur, JobRef::getPreprocessed(firstNewJob));
        break;
      }
      cur = next;
    }
    firstNewJob = previousFirstNewJob;
  }

  return firstNewJob;
}

void DefaultActorImpl::giveUpThread(RunningJobInfo runner) {
  auto oldState = CurrentState.load(std::memory_order_acquire);
  assert(oldState.Flags.getStatus() == Status::Running);

  ProcessOverrideJob *overridesToWake = nullptr;
  auto firstNewJob = preprocessQueue(oldState.FirstJob, JobRef(), nullptr,
                                     overridesToWake);

  while (true) {
    State newState = oldState;
    newState.FirstJob = JobRef::getPreprocessed(firstNewJob);
    if (firstNewJob) {
      newState.Flags.setStatus(Status::Scheduled);
    } else {
      newState.Flags.setStatus(Status::Idle);
    }

    // If the runner was an inline job, it's no longer active.
    if (runner.wasInlineJob()) {
      newState.Flags.setHasActiveInlineJob(false);
    }

    bool hasMoreJobs = (bool) newState.FirstJob;
    bool hasOverrideAtNewPriority =
      (runner.Priority < oldState.Flags.getMaxPriority());
    bool hasActiveInlineJob = newState.Flags.hasActiveInlineJob();
    bool needsNewProcessJob = hasMoreJobs && !hasOverrideAtNewPriority;

    // If we want to create a new inline job below, be sure to claim that
    // in the new state.
    if (needsNewProcessJob && !hasActiveInlineJob) {
      newState.Flags.setHasActiveInlineJob(true);
    }

    auto firstPreprocessed = oldState.FirstJob;
    if (!CurrentState.compare_exchange_weak(oldState, newState,
                    /*success*/ std::memory_order_release,
                    /*failure*/ std::memory_order_acquire)) {
      // Preprocess any new queue items.
      firstNewJob = preprocessQueue(oldState.FirstJob,
                                    firstPreprocessed,
                                    firstNewJob,
                                    overridesToWake);

      // Try again.
      continue;
    }

    // The priority of the remaining work.
    auto newPriority = newState.Flags.getMaxPriority();

    // Wake any overrides.
    wakeOverrides(overridesToWake, newPriority);

    // This is the actor's owning job; per the ownership rules (see
    // the comment on DefaultActorImpl), if there are remaining
    // jobs, we need to balance out our ownership one way or another.
    // We also, of course, need to ensure that there's a thread that's
    // actually going to process the actor.
    if (hasMoreJobs) {
      // If we know that there's an override job at the new priority,
      // we can let it become the owning job.  We just need to release.
      if (hasOverrideAtNewPriority) {
        swift_release(this);

      // Otherwies, enqueue a job that will try to take over running
      // with the new priority.  This also ensures that there's a job
      // at that priority which will actually take over the actor.
      } else {
        scheduleNonOverrideProcessJob(newPriority, hasActiveInlineJob);
      }
    }

    return;
  }
}

/// Claim the next job on the actor or give it up forever.
///
/// The running thread doesn't need to already own the actor to do this.
/// It does need to be participating correctly in the ownership
/// scheme as a "processing job"; see the comment on DefaultActorImpl.
Job *DefaultActorImpl::claimNextJobOrGiveUp(bool actorIsOwned,
                                            RunningJobInfo runner) {
  auto oldState = CurrentState.load(std::memory_order_acquire);

  // The status had better be Running unless we're trying to acquire
  // our first job.
  assert(oldState.Flags.getStatus() == Status::Running ||
         !actorIsOwned);

  // If we don't yet own the actor, we need to try to claim the actor
  // first; we cannot safely access the queue memory yet because other
  // threads may concurrently be trying to do this.
  if (!actorIsOwned) {
    while (true) {
      // A helper function when the only change we need to try is to
      // update for an inline runner.
      auto tryUpdateForInlineRunner = [&]{
        if (!runner.wasInlineJob()) return true;

        auto newState = oldState;
        newState.Flags.setHasActiveInlineJob(false);
        return CurrentState.compare_exchange_weak(oldState, newState,
                            /*success*/ std::memory_order_relaxed,
                            /*failure*/ std::memory_order_acquire);
      };

      // If the actor is out of work, or its priority doesn't match our
      // priority, don't try to take over the actor.
      if (!oldState.FirstJob ||
          oldState.Flags.getMaxPriority() != runner.Priority) {

        // The only change we need here is inline-runner bookkeeping.
        if (!tryUpdateForInlineRunner())
          continue;

        // We're eliminating a processing thread; balance ownership.
        swift_release(this);
        runner.setAbandoned();
        return nullptr;
      }

      // If the actor is currently running, we'd need to wait for
      // it to stop.  We can do this if we're an override job;
      // otherwise we need to exit.
      if (oldState.Flags.getStatus() == Status::Running) {
        if (!runner.waitForActivation()) {
          // The only change we need here is inline-runner bookkeeping.
          if (!tryUpdateForInlineRunner())
            continue;

          swift_release(this);
          return nullptr;
        }

        // Fall through into the compare-exchange below, but anticipate
        // that the actor is now Scheduled instead of Running.
        oldState.Flags.setStatus(Status::Scheduled);
      }

      // Try to set the state as Running.
      assert(oldState.Flags.getStatus() == Status::Scheduled);
      auto newState = oldState;
      newState.Flags.setStatus(Status::Running);

      // Also do our inline-runner bookkeeping.
      if (runner.wasInlineJob())
        newState.Flags.setHasActiveInlineJob(false);

      if (!CurrentState.compare_exchange_weak(oldState, newState,
                              /*success*/ std::memory_order_relaxed,
                              /*failure*/ std::memory_order_acquire))
        continue;

      // If that succeeded, we can proceed to the main body.
      oldState = newState;
      runner.setRunning();
      break;
    }
  }

  assert(oldState.Flags.getStatus() == Status::Running);

  // We should have taken care of the inline-job bookkeeping now.
  assert(!oldState.Flags.hasActiveInlineJob() || !runner.wasInlineJob());

  // Okay, now it's safe to look at queue state.
  // Preprocess any queue items at the front of the queue.
  ProcessOverrideJob *overridesToWake = nullptr;
  auto newFirstJob = preprocessQueue(oldState.FirstJob, JobRef(),
                                     nullptr, overridesToWake);

  Optional<JobPriority> remainingJobPriority;
  while (true) {
    State newState = oldState;

    // If the priority we're currently running with is adqeuate for
    // all the remaining jobs, try to dequeue something.
    // FIXME: should this be an exact match in priority instead of
    // potentially running jobs with too high a priority?
    Job *jobToRun;
    if (oldState.Flags.getMaxPriority() <= runner.Priority &&
        newFirstJob) {
      jobToRun = newFirstJob;
      newState.FirstJob = getNextJobInQueue(newFirstJob);
      newState.Flags.setStatus(Status::Running);

    // Otherwise, we should give up the thread.
    } else {
      jobToRun = nullptr;
      newState.FirstJob = JobRef::getPreprocessed(newFirstJob);
      newState.Flags.setStatus(newFirstJob ? Status::Scheduled
                                           : Status::Idle);
    }

    // Try to update the queue.  The changes we've made to the queue
    // structure need to be made visible even if we aren't dequeuing
    // anything.
    auto firstPreprocessed = oldState.FirstJob;
    if (!CurrentState.compare_exchange_weak(oldState, newState,
                     /*success*/ std::memory_order_release,
                     /*failure*/ std::memory_order_acquire)) {
      // Preprocess any new queue items, which will have been formed
      // into a linked list leading to the last head we observed.
      // (The fact that that job may not be the head anymore doesn't
      // matter; we're looking for an exact match with that.)
      newFirstJob = preprocessQueue(oldState.FirstJob,
                                    firstPreprocessed,
                                    newFirstJob,
                                    overridesToWake);

      // Loop to retry updating the state.
      continue;
    }

    // We successfully updated the state.

    // If we're giving up the thread with jobs remaining, we need
    // to release the actor, and we should wake overrides with the
    // right priority.
    Optional<JobPriority> remainingJobPriority;
    if (!jobToRun && newFirstJob) {
      remainingJobPriority = newState.Flags.getMaxPriority();
    }

    // Wake the overrides.
    wakeOverrides(overridesToWake, remainingJobPriority);

    // Per the ownership rules (see the comment on DefaultActorImpl),
    // release the actor if we're giving up the thread with jobs
    // remaining.  We intentionally do this after wakeOverrides to
    // try to get the overrides running a little faster.
    if (remainingJobPriority)
      swift_release(this);

    return jobToRun;
  }
}

/// The primary function for processing an actor on a thread.  Start
/// processing the given default actor as the active default actor on
/// the current thread, and keep processing whatever actor we're
/// running when code returns back to us until we're not processing
/// any actors anymore.
static void processDefaultActor(DefaultActorImpl *currentActor,
                                RunningJobInfo runner) {
  // Register that we're processing a default actor in this frame.
  DefaultActorProcessingFrame frame(currentActor, ShadowExistingFrame);

  bool threadIsRunningActor = false;
  while (true) {
    assert(currentActor);

    // Immediately check if we've been asked to yield the thread.
    if (shouldYieldThread())
      break;

    // Claim another job from the current actor. 
    auto job = currentActor->claimNextJobOrGiveUp(threadIsRunningActor,
                                                  runner);

    // If we failed to claim a job, we have nothing to do.
    if (!job) {
      // We also gave up the actor as part of failing to claim it.
      // Make sure we don't try to give up the actor again.
      currentActor = nullptr;
      break;
    }

    // Run the job.
    job->run(ExecutorRef::forDefaultActor(asAbstract(currentActor)));

    // The current actor may have changed after the job.
    // If it's become nil, we have nothing to do.
    currentActor = frame.getActiveActor();
    if (!currentActor)
      break;

    // Otherwise, we know that we're running the actor on this thread.
    threadIsRunningActor = true;
  }

  frame.exit();

  // If we still have an active actor, we should give it up.
  if (currentActor)
    currentActor->giveUpThread(runner);
}

void ProcessInlineJob::process(Job *job, ExecutorRef _executor) {
  DefaultActorImpl *actor = DefaultActorImpl::fromInlineJob(job);

  // Pull the priority out of the job before we do anything that might
  // invalidate it.
  auto targetPriority = job->getPriority();
  auto runner = RunningJobInfo::forInline(targetPriority);

  // FIXME: force tail call
  return processDefaultActor(actor, runner);
}

void ProcessOutOfLineJob::process(Job *job, ExecutorRef _executor) {
  auto self = cast<ProcessOutOfLineJob>(job);
  DefaultActorImpl *actor = self->Actor;

  // Pull the priority out of the job before we do anything that might
  // invalidate it.
  auto targetPriority = job->getPriority();
  auto runner = RunningJobInfo::forOther(targetPriority);

  delete self;

  // FIXME: force tail call
  return processDefaultActor(actor, runner);
}

void ProcessOverrideJob::process(Job *job, ExecutorRef _executor) {
  auto self = cast<ProcessOverrideJob>(job);

  // Pull the actor and priority out of the job.
  auto actor = self->Actor;
  auto runner = RunningJobInfo::forOverride(self);

  // FIXME: force tail call
  return processDefaultActor(actor, runner);
}

void DefaultActorImpl::enqueue(Job *job) {
  auto oldState = CurrentState.load(std::memory_order_relaxed);

  OverrideJobCache overrideJob;

  while (true) {
    auto newState = oldState;

    // Put the job at the front of the job list (which will get
    // reversed during preprocessing).
    setNextJobInQueue(job, oldState.FirstJob);
    newState.FirstJob = JobRef::getUnpreprocessed(job);

    auto oldStatus = oldState.Flags.getStatus();
    bool wasIdle = oldStatus == Status::Idle;

    // Update the priority: the prriority of the job we're adding
    // if the actor was idle, or the max if not.  Only the running
    // thread can decrease the actor's priority once it's non-idle.
    // (But note that the job we enqueue can still observe a
    // lowered priority.)
    auto oldPriority = oldState.Flags.getMaxPriority();
    auto newPriority =
      wasIdle ? job->getPriority()
              : std::max(oldPriority, job->getPriority());
    newState.Flags.setMaxPriority(newPriority);

    // If we need an override job, create it (if necessary) and
    // register it with the queue.
    bool needsOverride = !wasIdle && newPriority != oldPriority;
    if (needsOverride) {
      overrideJob.addToState(this, newState);
    } else {
      overrideJob.setNotNeeded();
    }

    // If we don't need an override job, then we might be able to
    // create an inline job; flag that.
    bool hasActiveInlineJob = newState.Flags.hasActiveInlineJob();
    if (wasIdle && !hasActiveInlineJob)
      newState.Flags.setHasActiveInlineJob(true);

    // Make sure the status is at least Scheduled.  We'll actually
    // schedule the job below, if we succeed at this.
    if (wasIdle) {
      newState.Flags.setStatus(Status::Scheduled);
    }

    // Try the compare-exchange, and try again if it fails.
    if (!CurrentState.compare_exchange_weak(oldState, newState,
          /*success*/ std::memory_order_release,
          /*failure*/ std::memory_order_relaxed))
      continue;

    // Okay, we successfully updated the status.  Schedule a job to
    // process the actor if necessary.

    // Commit the override job if we created one.
    overrideJob.commit();

    // If the actor is currently idle, schedule it using the
    // invasive job.
    if (wasIdle) {
      assert(!needsOverride);
      scheduleNonOverrideProcessJob(newPriority, hasActiveInlineJob);
    }

    return;
  }
}

bool DefaultActorImpl::tryAssumeThread(RunningJobInfo runner) {
  // We have to load-acquire in order to properly order accesses to
  // the actor's state for the new task.
  auto oldState = CurrentState.load(std::memory_order_acquire);

  // If the actor is currently idle, try to mark it as running.
  while (oldState.Flags.getStatus() == Status::Idle) {
    assert(!oldState.FirstJob);
    auto newState = oldState;
    newState.Flags.setStatus(Status::Running);
    newState.Flags.setMaxPriority(runner.Priority);

    if (CurrentState.compare_exchange_weak(oldState, newState,
                              /*success*/ std::memory_order_relaxed,
                              /*failure*/ std::memory_order_acquire))
      return true;
  }

  return false;
}

void swift::swift_defaultActor_initialize(DefaultActor *_actor) {
  asImpl(_actor)->initialize();
}

void swift::swift_defaultActor_destroy(DefaultActor *_actor) {
  asImpl(_actor)->destroy();
}

void swift::swift_defaultActor_enqueue(Job *job, DefaultActor *_actor) {
  asImpl(_actor)->enqueue(job);
}

/*****************************************************************************/
/****************************** ACTOR SWITCHING ******************************/
/*****************************************************************************/

/// Can the current executor give up its thread?
static bool canGiveUpThreadForSwitch(ExecutorRef currentExecutor) {
  // We can certainly "give up" a generic executor to try to run
  // a task for an actor.
  if (currentExecutor.isGeneric())
    return true;

  // If the current executor is a default actor, we know how to make
  // it give up its thread.
  if (currentExecutor.isDefaultActor())
    return true;

  return false;
}

/// Tell the current executor to give up its thread, given that it
/// returned true from canGiveUpThreadForSwitch().
///
/// Note that we don't update DefaultActorProcessingFrame here; we'll
/// do that in runOnAssumedThread.
static void giveUpThreadForSwitch(ExecutorRef currentExecutor,
                                  RunningJobInfo runner) {
  if (currentExecutor.isGeneric())
    return;

  asImpl(currentExecutor.getDefaultActor())->giveUpThread(runner);
}

/// Try to assume control of the current thread for the given executor
/// in order to run the given job.
///
/// This doesn't actually run the job yet.
///
/// Note that we don't update DefaultActorProcessingFrame here; we'll
/// do that in runOnAssumedThread.
static bool tryAssumeThreadForSwitch(ExecutorRef newExecutor,
                                     RunningJobInfo runner) {
  // If the new executor is generic, we don't need to do anything.
  if (newExecutor.isGeneric()) {
    return true;
  }

  // If the new executor is a default actor, ask it to assume the thread.
  if (newExecutor.isDefaultActor()) {
    return asImpl(newExecutor.getDefaultActor())->tryAssumeThread(runner);
  }

  return false;
}

/// Given that we've assumed control of an executor on this thread,
/// run the given task on it.
SWIFT_CC(swiftasync)
static void runOnAssumedThread(AsyncTask *task, ExecutorRef newExecutor,
                               RunningJobInfo runner) {
  assert(newExecutor.isGeneric() || newExecutor.isDefaultActor());

  DefaultActorImpl *actor = newExecutor.isGeneric()
                              ? nullptr
                              : asImpl(newExecutor.getDefaultActor());

  // Set that this actor is now the active default actor on this thread,
  // and set up an actor-processing frame if there wasn't one already.
  DefaultActorProcessingFrame frame(actor, UpdateExistingFrame);

  // If one already existed, we should just tail-call the task; we don't
  // want these frames to potentially accumulate linearly.
  if (!frame.isNeeded()) {
    // FIXME: force tail call
    return task->run(newExecutor);
  }

  // Otherwise, run the new task.
  task->run(newExecutor);

  // Leave the processing frame, and give up the current actor if
  // we have one.
  //
  // In principle, we could execute more tasks here, but that's probably
  // not a reasonable thing to do in an assumed context rather than a
  // dedicated actor-processing job.
  actor = frame.getActiveActor();
  frame.exit();

  if (actor)
    actor->giveUpThread(runner);
}

void swift::swift_task_switch(AsyncTask *task, ExecutorRef currentExecutor,
                              ExecutorRef newExecutor) {
  assert(task && "no task provided");

  // If the current executor is compatible with running the new executor,
  // just continue running.
  if (!currentExecutor.mustSwitchToRun(newExecutor)) {
    // FIXME: force tail call
    return task->run(currentExecutor);
  }

  // Okay, we semantically need to switch.
  auto runner = RunningJobInfo::forOther(task->getPriority());

  // If the current executor can give up its thread, and the new executor
  // can take over a thread, try to do so; but don't do this if we've
  // been asked to yield the thread.
  if (canGiveUpThreadForSwitch(currentExecutor) &&
      !shouldYieldThread() &&
      tryAssumeThreadForSwitch(newExecutor, runner)) {
    giveUpThreadForSwitch(currentExecutor, runner);
    // FIXME: force tail call
    return runOnAssumedThread(task, newExecutor, runner);
  }

  // Otherwise, just asynchronously enqueue the task on the given
  // executor.
  swift_task_enqueue(task, newExecutor);
}

/*****************************************************************************/
/************************* GENERIC ACTOR INTERFACES **************************/
/*****************************************************************************/

void swift::swift_task_enqueue(Job *job, ExecutorRef executor) {
  assert(job && "no job provided");

  if (executor.isGeneric())
    return swift_task_enqueueGlobal(job);

  if (executor.isDefaultActor())
    return asImpl(executor.getDefaultActor())->enqueue(job);

  // Just assume it's actually a default actor that we haven't tagged
  // properly.
  // FIXME: call the general method.
  return asImpl(reinterpret_cast<DefaultActor*>(executor.getRawValue()))
    ->enqueue(job);
}
