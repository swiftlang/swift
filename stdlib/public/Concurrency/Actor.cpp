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
#include <atomic>

#ifdef _WIN32
// On Windows, an include below triggers an indirect include of minwindef.h
// which contains a definition of the `max` macro, generating an error in our
// use of std::max in this file. This define prevents those macros from being
// defined.
#define NOMINMAX
#endif

#include "../CompatibilityOverride/CompatibilityOverride.h"
#include "swift/Runtime/Atomic.h"
#include "swift/Runtime/AccessibleFunction.h"
#include "swift/Runtime/Casting.h"
#include "swift/Runtime/Once.h"
#include "swift/Runtime/Mutex.h"
#include "swift/Runtime/ThreadLocal.h"
#include "swift/Runtime/ThreadLocalStorage.h"
#include "swift/Runtime/DispatchShims.h"
#include "swift/ABI/Task.h"
#include "swift/ABI/Actor.h"
#include "swift/Basic/ListMerger.h"
#include "swift/Concurrency/Actor.h"
#ifdef SWIFT_CONCURRENCY_BACK_DEPLOYMENT
// All platforms where we care about back deployment have a known
// configurations.
#define HAVE_PTHREAD_H 1
#define SWIFT_OBJC_INTEROP 1
#endif
#include "llvm/ADT/PointerIntPair.h"
#include "TaskPrivate.h"
#include "VoucherSupport.h"

#if SWIFT_CONCURRENCY_ENABLE_DISPATCH
#include <dispatch/dispatch.h>
#endif

#if SWIFT_STDLIB_HAS_ASL
#include <asl.h>
#elif defined(__ANDROID__)
#include <android/log.h>
#endif

#if defined(__ELF__)
#include <unwind.h>
#endif

#if defined(__ELF__)
#include <sys/syscall.h>
#endif

#if defined(_POSIX_THREADS)
#include <pthread.h>

// Only use __has_include since HAVE_PTHREAD_NP_H is not provided.
#if __has_include(<pthread_np.h>)
#include <pthread_np.h>
#endif
#endif

#if defined(_WIN32)
#include <io.h>
#include <handleapi.h>
#include <processthreadsapi.h>
#endif

#if SWIFT_OBJC_INTEROP
extern "C" void *objc_autoreleasePoolPush();
extern "C" void objc_autoreleasePoolPop(void *);
#endif

using namespace swift;

/// Should we yield the thread?
static bool shouldYieldThread() {
  // return dispatch_swift_job_should_yield();
  return false;
}

/*****************************************************************************/
/******************************* TASK TRACKING ******************************/
/*****************************************************************************/

namespace {

/// An extremely silly class which exists to make pointer
/// default-initialization constexpr.
template <class T> struct Pointer {
  T *Value;
  constexpr Pointer() : Value(nullptr) {}
  constexpr Pointer(T *value) : Value(value) {}
  operator T *() const { return Value; }
  T *operator->() const { return Value; }
};

/// A class which encapsulates the information we track about
/// the current thread and active executor.
class ExecutorTrackingInfo {
  /// A thread-local variable pointing to the active tracking
  /// information about the current thread, if any.
  ///
  /// TODO: this is obviously runtime-internal and therefore not
  /// reasonable to make ABI. We might want to also provide a way
  /// for generated code to efficiently query the identity of the
  /// current executor, in order to do a cheap comparison to avoid
  /// doing all the work to suspend the task when we're already on
  /// the right executor. It would make sense for that to be a
  /// separate thread-local variable (or whatever is most efficient
  /// on the target platform).
  static SWIFT_RUNTIME_DECLARE_THREAD_LOCAL(
      Pointer<ExecutorTrackingInfo>, ActiveInfoInThread,
      SWIFT_CONCURRENCY_EXECUTOR_TRACKING_INFO_KEY);

  /// The active executor.
  ExecutorRef ActiveExecutor = ExecutorRef::generic();

  /// Whether this context allows switching.  Some contexts do not;
  /// for example, we do not allow switching from swift_job_run
  /// unless the passed-in executor is generic.
  bool AllowsSwitching = true;

  VoucherManager voucherManager;

  /// The tracking info that was active when this one was entered.
  ExecutorTrackingInfo *SavedInfo;

public:
  ExecutorTrackingInfo() = default;

  ExecutorTrackingInfo(const ExecutorTrackingInfo &) = delete;
  ExecutorTrackingInfo &operator=(const ExecutorTrackingInfo &) = delete;

  /// Unconditionally initialize a fresh tracking state on the
  /// current state, shadowing any previous tracking state.
  /// leave() must be called beforet the object goes out of scope.
  void enterAndShadow(ExecutorRef currentExecutor) {
    ActiveExecutor = currentExecutor;
    SavedInfo = ActiveInfoInThread.get();
    ActiveInfoInThread.set(this);
  }

  void swapToJob(Job *job) { voucherManager.swapToJob(job); }

  void restoreVoucher(AsyncTask *task) { voucherManager.restoreVoucher(task); }

  ExecutorRef getActiveExecutor() const {
    return ActiveExecutor;
  }

  void setActiveExecutor(ExecutorRef newExecutor) {
    ActiveExecutor = newExecutor;
  }


  bool allowsSwitching() const {
    return AllowsSwitching;
  }

  /// Disallow switching in this tracking context.  This should only
  /// be set on a new tracking info, before any jobs are run in it.
  void disallowSwitching() {
    AllowsSwitching = false;
  }

  static ExecutorTrackingInfo *current() {
    return ActiveInfoInThread.get();
  }

  void leave() {
    voucherManager.leave();
    ActiveInfoInThread.set(SavedInfo);
  }
};

class ActiveTask {
  /// A thread-local variable pointing to the active tracking
  /// information about the current thread, if any.
  static SWIFT_RUNTIME_DECLARE_THREAD_LOCAL(Pointer<AsyncTask>, Value,
                                            SWIFT_CONCURRENCY_TASK_KEY);

public:
  static void set(AsyncTask *task) { Value.set(task); }
  static AsyncTask *get() { return Value.get(); }
};

/// Define the thread-locals.
SWIFT_RUNTIME_DECLARE_THREAD_LOCAL(
  Pointer<AsyncTask>,
  ActiveTask::Value,
  SWIFT_CONCURRENCY_TASK_KEY);

SWIFT_RUNTIME_DECLARE_THREAD_LOCAL(
  Pointer<ExecutorTrackingInfo>,
  ExecutorTrackingInfo::ActiveInfoInThread,
  SWIFT_CONCURRENCY_EXECUTOR_TRACKING_INFO_KEY);

} // end anonymous namespace

void swift::runJobInEstablishedExecutorContext(Job *job) {
  _swift_tsan_acquire(job);

#if SWIFT_OBJC_INTEROP
  auto pool = objc_autoreleasePoolPush();
#endif

  if (auto task = dyn_cast<AsyncTask>(job)) {
    // Update the active task in the current thread.
    ActiveTask::set(task);

    // Update the task status to say that it's running on the
    // current thread.  If the task suspends somewhere, it should
    // update the task status appropriately; we don't need to update
    // it afterwards.
    task->flagAsRunning();

    task->runInFullyEstablishedContext();

    assert(ActiveTask::get() == nullptr &&
           "active task wasn't cleared before susspending?");
  } else {
    // There's no extra bookkeeping to do for simple jobs besides swapping in
    // the voucher.
    ExecutorTrackingInfo::current()->swapToJob(job);
    job->runSimpleInFullyEstablishedContext();
  }

#if SWIFT_OBJC_INTEROP
  objc_autoreleasePoolPop(pool);
#endif

  _swift_tsan_release(job);
}

void swift::adoptTaskVoucher(AsyncTask *task) {
  ExecutorTrackingInfo::current()->swapToJob(task);
}

void swift::restoreTaskVoucher(AsyncTask *task) {
  ExecutorTrackingInfo::current()->restoreVoucher(task);
}

SWIFT_CC(swift)
AsyncTask *swift::swift_task_getCurrent() {
  return ActiveTask::get();
}

AsyncTask *swift::_swift_task_clearCurrent() {
  auto task = ActiveTask::get();
  ActiveTask::set(nullptr);
  return task;
}

SWIFT_CC(swift)
static ExecutorRef swift_task_getCurrentExecutorImpl() {
  auto currentTracking = ExecutorTrackingInfo::current();
  auto result = (currentTracking ? currentTracking->getActiveExecutor()
                                 : ExecutorRef::generic());
  SWIFT_TASK_DEBUG_LOG("getting current executor %p", result.getIdentity());
  return result;
}

#if defined(_WIN32)
static HANDLE __initialPthread = INVALID_HANDLE_VALUE;
#endif

/// Determine whether we are currently executing on the main thread
/// independently of whether we know that we are on the main actor.
static bool isExecutingOnMainThread() {
#if SWIFT_STDLIB_SINGLE_THREADED_RUNTIME
  return true;
#elif defined(__linux__)
  return syscall(SYS_gettid) == getpid();
#elif defined(_WIN32)
  if (__initialPthread == INVALID_HANDLE_VALUE) {
    DuplicateHandle(GetCurrentProcess(), GetCurrentThread(),
                    GetCurrentProcess(), &__initialPthread, 0, FALSE,
                    DUPLICATE_SAME_ACCESS);
  }

  return __initialPthread == GetCurrentThread();
#elif defined(__wasi__)
  return true;
#else
  return pthread_main_np() == 1;
#endif
}

JobPriority swift::swift_task_getCurrentThreadPriority() {
#if SWIFT_STDLIB_SINGLE_THREADED_RUNTIME
  return JobPriority::UserInitiated;
#elif defined(__APPLE__)
  return static_cast<JobPriority>(qos_class_self());
#else
  if (isExecutingOnMainThread())
    return JobPriority::UserInitiated;

  return JobPriority::Unspecified;
#endif
}

SWIFT_CC(swift)
static bool swift_task_isCurrentExecutorImpl(ExecutorRef executor) {
  if (auto currentTracking = ExecutorTrackingInfo::current()) {
    return currentTracking->getActiveExecutor() == executor;
  }

  return executor.isMainExecutor() && isExecutingOnMainThread();
}

/// Logging level for unexpected executors:
/// 0 - no logging
/// 1 - warn on each instance
/// 2 - fatal error
static unsigned unexpectedExecutorLogLevel = 1;

static void checkUnexpectedExecutorLogLevel(void *context) {
#if SWIFT_STDLIB_HAS_ENVIRON
  const char *levelStr = getenv("SWIFT_UNEXPECTED_EXECUTOR_LOG_LEVEL");
  if (!levelStr)
    return;

  long level = strtol(levelStr, nullptr, 0);
  if (level >= 0 && level < 3)
    unexpectedExecutorLogLevel = level;
#endif // SWIFT_STDLIB_HAS_ENVIRON
}

SWIFT_CC(swift)
void swift::swift_task_reportUnexpectedExecutor(
    const unsigned char *file, uintptr_t fileLength, bool fileIsASCII,
    uintptr_t line, ExecutorRef executor) {
  // Make sure we have an appropriate log level.
  static swift_once_t logLevelToken;
  swift_once(&logLevelToken, checkUnexpectedExecutorLogLevel, nullptr);

  bool isFatalError = false;
  switch (unexpectedExecutorLogLevel) {
  case 0:
    return;

  case 1:
    isFatalError = false;
    break;

  case 2:
    isFatalError = true;
    break;
  }

  const char *functionIsolation;
  const char *whereExpected;
  if (executor.isMainExecutor()) {
    functionIsolation = "@MainActor function";
    whereExpected = "the main thread";
  } else {
    functionIsolation = "actor-isolated function";
    whereExpected = "the same actor";
  }

  char *message;
  swift_asprintf(
      &message,
      "%s: data race detected: %s at %.*s:%d was not called on %s\n",
      isFatalError ? "error" : "warning", functionIsolation,
      (int)fileLength, file, (int)line, whereExpected);

  if (_swift_shouldReportFatalErrorsToDebugger()) {
    RuntimeErrorDetails details = {
        .version = RuntimeErrorDetails::currentVersion,
        .errorType = "actor-isolation-violation",
        .currentStackDescription = "Actor-isolated function called from another thread",
        .framesToSkip = 1,
    };
    _swift_reportToDebugger(
        isFatalError ? RuntimeErrorFlagFatal : RuntimeErrorFlagNone, message,
        &details);
  }

#if defined(_WIN32)
#define STDERR_FILENO 2
  _write(STDERR_FILENO, message, strlen(message));
#else
  fputs(message, stderr);
  fflush(stderr);
#endif
#if SWIFT_STDLIB_HAS_ASL
  asl_log(nullptr, nullptr, ASL_LEVEL_ERR, "%s", message);
#elif defined(__ANDROID__)
  __android_log_print(ANDROID_LOG_FATAL, "SwiftRuntime", "%s", message);
#endif

  free(message);

  if (isFatalError)
    abort();
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
  static void process(Job *job);

  static bool classof(const Job *job) {
    return job->Flags.getKind() == JobKind::DefaultActorInline;
  }
};

/// A job to process a default actor that's allocated separately from
/// the actor.
class ProcessOutOfLineJob : public Job {
  DefaultActorImpl *Actor;
public:
  ProcessOutOfLineJob(DefaultActorImpl *actor, JobPriority priority)
    : Job({JobKind::DefaultActorSeparate, priority}, &process),
      Actor(actor) {}

  SWIFT_CC(swiftasync)
  static void process(Job *job);

  static bool classof(const Job *job) {
    return job->Flags.getKind() == JobKind::DefaultActorSeparate;
  }
};

/// Information about the currently-running processing job.
struct RunningJobInfo {
  enum KindType : uint8_t {
    Inline, Other
  };
  KindType Kind;

  bool wasInlineJob() const {
    return Kind == Inline;
  }

  static RunningJobInfo forOther() {
    return {Other};
  }
  static RunningJobInfo forInline() {
    return {Inline};
  }
};

class JobRef {
  enum : uintptr_t {
    NeedsPreprocessing = 0x1,
    JobMask = ~uintptr_t(NeedsPreprocessing)
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

  /// Is this a null reference?
  operator bool() const { return Value != 0; }

  /// Does this job need to be pre-processed before we can treat
  /// the job queue as a proper queue?
  bool needsPreprocessing() const {
    return Value & NeedsPreprocessing;
  }

  /// Is this an unprocessed message to the actor, rather than a job?
  bool isMessage() const {
    return false; // For now, we have no messages
  }

  Job *getAsJob() const {
    assert(!isMessage());
    return reinterpret_cast<Job*>(Value & JobMask);
  }
  Job *getAsPreprocessedJob() const {
    assert(!isMessage() && !needsPreprocessing());
    return reinterpret_cast<Job*>(Value);
  }

  /// Get the Job pointer with no preconditions on its type, for tracing.
  Job *getRawJob() const { return reinterpret_cast<Job *>(Value & JobMask); }

  bool operator==(JobRef other) const {
    return Value == other.Value;
  }
  bool operator!=(JobRef other) const {
    return Value != other.Value;
  }
};

/// TODO (rokhinip): The layout of the ActiveActorStatus seems to be broken in
/// arm64_32 with priority escalation support, disable this for now.
#if SWIFT_CONCURRENCY_ENABLE_PRIORITY_ESCALATION && SWIFT_POINTER_IS_4_BYTES
#define SWIFT_CONCURRENCY_ENABLE_PRIORITY_ESCALATION 0
#endif

/// Similar to the ActiveTaskStatus, this denotes the ActiveActorState for
/// tracking the atomic state of the actor
///
/// The runtime needs to track the following state about the actor within the
/// same atomic:
///
/// * The current status of the actor - scheduled, running, idle, etc
/// * The current maximum priority of the jobs enqueued in the actor
/// * The identity of the thread currently holding the actor lock
/// * Pointer to list of jobs enqueued in actor
///
/// It is important for all of this information to be in the same atomic so that
/// when the actor's state changes, the information is visible to all threads that
/// may be modifying the actor, allowing the algorithm to eventually converge.
///
/// In order to provide priority escalation support with actors, deeper integration is
/// required with the OS in order to have the intended side effects. On Darwin, Swift
/// Concurrency Tasks runs on dispatch's queues. As such, we need to use an
/// encoding of thread identity vended by libdispatch called dispatch_lock_t,
/// and a futex-style dispatch API in order to escalate the priority of a
/// thread. Henceforth, the dispatch_lock_t tracked in the ActiveActorStatus
/// will be called the DrainLock.
///
/// When a thread starts running on an actor, it's identity is recorded in the
/// ActiveActorStatus. This way, if a higher priority job is enqueued behind the
/// thread executing the actor, we can escalate the thread holding the actor
/// lock, thereby resolving the priority inversion. When a thread hops off of
/// the actor, any priority boosts it may have gotten as a result of contention
/// on the actor, is removed as well.
///
/// In order to keep the entire ActiveActorStatus size to 2 words, the thread
/// identity is only tracked on platforms which can support 128 bit atomic
/// operations. The ActiveActorStatus's layout has thus been changed to have the
/// following layout depending on the system configuration supported:
///
/// 32 bit systems with SWIFT_CONCURRENCY_ENABLE_PRIORITY_ESCALATION=1
///
///          Flags               Drain Lock               Unused                JobRef
/// |----------------------|----------------------|----------------------|-------------------|
///          32 bits                32 bits                32 bits              32 bits
///
/// 64 bit systems with SWIFT_CONCURRENCY_ENABLE_PRIORITY_ESCALATION=1
///
///         Flags                Drain Lock             JobRef
/// |----------------------|-------------------|----------------------|
///          32 bits                32 bits             64 bits
///
/// 32 bit systems with SWIFT_CONCURRENCY_ENABLE_PRIORITY_ESCALATION=0
///
///          Flags                  JobRef
/// |----------------------|----------------------|
///          32 bits                32 bits
//
/// 64 bit systems with SWIFT_CONCURRENCY_ENABLE_PRIORITY_ESCALATION=0
///
///         Flags                  Unused                 JobRef
/// |----------------------|----------------------|---------------------|
///         32 bits                 32 bits               64 bits
///
/// Size requirements:
///     On 64 bit systems or if SWIFT_CONCURRENCY_ENABLE_PRIORITY_ESCALATION=1,
///     the field is 16 bytes long.
///
///     Otherwise, it is 8 bytes long.
///
/// Alignment requirements:
///     On 64 bit systems or if SWIFT_CONCURRENCY_ENABLE_PRIORITY_ESCALATION=1,
///     this 16-byte field needs to be 16 byte aligned to be able to do aligned
///     atomic stores field.
///
///     On all other systems, it needs to be 8 byte aligned for the atomic
///     stores.
///
///     As a result of varying alignment needs, we've marked the class as
///     needing 2-word alignment but on arm64_32 with
///     SWIFT_CONCURRENCY_ENABLE_PRIORITY_ESCALATION=1, 16 byte alignment is
///     achieved through careful arrangement of the storage for this in the
///     DefaultActorImpl. The additional alignment requirements are
///     enforced by static asserts below.
class alignas(2 * sizeof(void *)) ActiveActorStatus
    : public swift::aligned_alloc<2 * sizeof(void *)> {
#if SWIFT_CONCURRENCY_ENABLE_PRIORITY_ESCALATION && SWIFT_POINTER_IS_4_BYTES
  uint32_t Flags;
  dispatch_lock_t DrainLock;
  LLVM_ATTRIBUTE_UNUSED uint32_t Unused = {};
#elif SWIFT_CONCURRENCY_ENABLE_PRIORITY_ESCALATION && SWIFT_POINTER_IS_8_BYTES
  uint32_t Flags;
  dispatch_lock_t DrainLock;
#elif !SWIFT_CONCURRENCY_ENABLE_PRIORITY_ESCALATION && SWIFT_POINTER_IS_4_BYTES
  uint32_t Flags;
#else /* !SWIFT_CONCURRENCY_ENABLE_PRIORITY_ESCALATION && SWIFT_POINTER_IS_8_BYTES */
  uint32_t Flags;
  LLVM_ATTRIBUTE_UNUSED uint32_t Unused = {};
#endif
  JobRef FirstJob;

#if SWIFT_CONCURRENCY_ENABLE_PRIORITY_ESCALATION
  ActiveActorStatus(uint32_t flags, dispatch_lock_t drainLockValue, JobRef job)
    : Flags(flags), DrainLock(drainLockValue), FirstJob(job) {}
#else
  ActiveActorStatus(uint32_t flags, JobRef job)
    : Flags(flags), FirstJob(job) {}
#endif

  uint32_t getActorState() const {
    return Flags & concurrency::ActorFlagConstants::ActorStateMask;
  }
  uint32_t setActorState(uint32_t state) const {
    return (Flags & ~concurrency::ActorFlagConstants::ActorStateMask) | state;
  }

public:

#if SWIFT_CONCURRENCY_ENABLE_PRIORITY_ESCALATION
  constexpr ActiveActorStatus()
      : Flags(), DrainLock(DLOCK_OWNER_NULL), FirstJob(JobRef()) {}
#else
  constexpr ActiveActorStatus()
      : Flags(), FirstJob(JobRef()) {}
#endif

  bool isDistributedRemote() const {
    return Flags & concurrency::ActorFlagConstants::DistributedRemote;
  }
  ActiveActorStatus withDistributedRemote() const {
#if SWIFT_CONCURRENCY_ENABLE_PRIORITY_ESCALATION
    return ActiveActorStatus(
        Flags | concurrency::ActorFlagConstants::DistributedRemote, DrainLock,
        FirstJob);
#else
    return ActiveActorStatus(
        Flags | concurrency::ActorFlagConstants::DistributedRemote, FirstJob);
#endif
  }

  bool isIdle() const {
    bool isIdle = (getActorState() == concurrency::ActorFlagConstants::Idle);
    if (isIdle) {
      assert(!FirstJob);
    }
    return isIdle;
  }
  ActiveActorStatus withIdle() const {
#if SWIFT_CONCURRENCY_ENABLE_PRIORITY_ESCALATION
    return ActiveActorStatus(
        setActorState(concurrency::ActorFlagConstants::Idle), DLOCK_OWNER_NULL,
        FirstJob);
#else
    return ActiveActorStatus(
        setActorState(concurrency::ActorFlagConstants::Idle), FirstJob);
#endif
  }

  bool isAnyRunning() const {
    uint32_t state = getActorState();
    return (state == concurrency::ActorFlagConstants::Running) ||
           (state ==
            concurrency::ActorFlagConstants::Zombie_ReadyForDeallocation);
  }
  bool isRunning() const {
    return getActorState() == concurrency::ActorFlagConstants::Running;
  }
  ActiveActorStatus withRunning() const {
#if SWIFT_CONCURRENCY_ENABLE_PRIORITY_ESCALATION
    return ActiveActorStatus(
        setActorState(concurrency::ActorFlagConstants::Running),
        dispatch_lock_value_for_self(), FirstJob);
#else
    return ActiveActorStatus(
        setActorState(concurrency::ActorFlagConstants::Running), FirstJob);
#endif
  }

  bool isScheduled() const {
    return getActorState() == concurrency::ActorFlagConstants::Scheduled;
  }

  ActiveActorStatus withScheduled() const {
#if SWIFT_CONCURRENCY_ENABLE_PRIORITY_ESCALATION
    return ActiveActorStatus(
        setActorState(concurrency::ActorFlagConstants::Scheduled),
        DLOCK_OWNER_NULL, FirstJob);
#else
    return ActiveActorStatus(
        setActorState(concurrency::ActorFlagConstants::Scheduled), FirstJob);
#endif
  }

  bool isZombie_ReadyForDeallocation() const {
    return getActorState() ==
           concurrency::ActorFlagConstants::Zombie_ReadyForDeallocation;
  }
  ActiveActorStatus withZombie_ReadyForDeallocation() const {
#if SWIFT_CONCURRENCY_ENABLE_PRIORITY_ESCALATION
    assert(dispatch_lock_owner(DrainLock) != DLOCK_OWNER_NULL);
    return ActiveActorStatus(
        setActorState(
            concurrency::ActorFlagConstants::Zombie_ReadyForDeallocation),
        DrainLock, FirstJob);
#else
    return ActiveActorStatus(
        setActorState(
            concurrency::ActorFlagConstants::Zombie_ReadyForDeallocation),
        FirstJob);
#endif
  }

  JobPriority getMaxPriority() const {
    return (
        JobPriority)((Flags & concurrency::ActorFlagConstants::PriorityMask) >>
                     concurrency::ActorFlagConstants::PriorityShift);
  }
  ActiveActorStatus withNewPriority(JobPriority priority) const {
    uint32_t flags =
        Flags & ~concurrency::ActorFlagConstants::PriorityAndOverrideMask;
    flags |=
        (uint32_t(priority) << concurrency::ActorFlagConstants::PriorityShift);
#if SWIFT_CONCURRENCY_ENABLE_PRIORITY_ESCALATION
    return ActiveActorStatus(flags, DrainLock, FirstJob);
#else
    return ActiveActorStatus(flags, FirstJob);
#endif
  }
  ActiveActorStatus resetPriority() const {
    return withNewPriority(JobPriority::Unspecified);
  }

  bool isMaxPriorityEscalated() const {
    return Flags & concurrency::ActorFlagConstants::IsPriorityEscalated;
  }
  ActiveActorStatus withEscalatedPriority(JobPriority priority) const {
    JobPriority currentPriority =
        JobPriority((Flags & concurrency::ActorFlagConstants::PriorityMask) >>
                    concurrency::ActorFlagConstants::PriorityShift);
    assert(priority > currentPriority);

    uint32_t flags =
        (Flags & ~concurrency::ActorFlagConstants::PriorityMask) |
        (uint32_t(priority) << concurrency::ActorFlagConstants::PriorityShift);
    flags |= concurrency::ActorFlagConstants::IsPriorityEscalated;
#if SWIFT_CONCURRENCY_ENABLE_PRIORITY_ESCALATION
    return ActiveActorStatus(flags, DrainLock, FirstJob);
#else
    return ActiveActorStatus(flags, FirstJob);
#endif
  }
  ActiveActorStatus withoutEscalatedPriority() const {
#if SWIFT_CONCURRENCY_ENABLE_PRIORITY_ESCALATION
    return ActiveActorStatus(
        Flags & ~concurrency::ActorFlagConstants::IsPriorityEscalated,
        DrainLock, FirstJob);
#else
    return ActiveActorStatus(
        Flags & ~concurrency::ActorFlagConstants::IsPriorityEscalated,
        FirstJob);
#endif
  }

  JobRef getFirstJob() const {
    return FirstJob;
  }
  ActiveActorStatus withFirstJob(JobRef firstJob) const {
#if SWIFT_CONCURRENCY_ENABLE_PRIORITY_ESCALATION
    return ActiveActorStatus(Flags, DrainLock, firstJob);
#else
    return ActiveActorStatus(Flags, firstJob);
#endif
  }

  uint32_t getOpaqueFlags() const {
    return Flags;
  }

  uint32_t currentDrainer() const {
#if SWIFT_CONCURRENCY_ENABLE_PRIORITY_ESCALATION
    return dispatch_lock_owner(DrainLock);
#else
    return 0;
#endif
  }

#if SWIFT_CONCURRENCY_ENABLE_PRIORITY_ESCALATION
  static size_t drainLockOffset() {
    return offsetof(ActiveActorStatus, DrainLock);
  }
#endif

  void traceStateChanged(HeapObject *actor) {
    // Convert our state to a consistent raw value. These values currently match
    // the enum values, but this explicit conversion provides room for change.
    uint8_t traceState = 255;
    switch (getActorState()) {
    case concurrency::ActorFlagConstants::Idle:
      traceState = 0;
      break;
    case concurrency::ActorFlagConstants::Scheduled:
      traceState = 1;
      break;
    case concurrency::ActorFlagConstants::Running:
      traceState = 2;
      break;
    case concurrency::ActorFlagConstants::Zombie_ReadyForDeallocation:
      traceState = 3;
      break;
    }
    concurrency::trace::actor_state_changed(
        actor, getFirstJob().getRawJob(), getFirstJob().needsPreprocessing(),
        traceState, isDistributedRemote(), isMaxPriorityEscalated(),
        static_cast<uint8_t>(getMaxPriority()));
  }
};

#if SWIFT_CONCURRENCY_ENABLE_PRIORITY_ESCALATION && SWIFT_POINTER_IS_4_BYTES
#define ACTIVE_ACTOR_STATUS_SIZE (4 * (sizeof(uintptr_t)))
#else
#define ACTIVE_ACTOR_STATUS_SIZE (2 * (sizeof(uintptr_t)))
#endif
static_assert(sizeof(ActiveActorStatus) == ACTIVE_ACTOR_STATUS_SIZE,
  "ActiveActorStatus is of incorrect size");

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
/// - Let N be the number of active processing jobs for the actor. N may be > 1
///   because we may have stealers for actors if we have to escalate the max
///   priority of the actor
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
/// (1) When we enqueue the first job on an actor, then R becomes 1, and
///     we must create a processing job so that N >= R.  We do not need to
///     retain the actor.
/// (2) When we create an extra job to process an actor (e.g. because of
///     priority overrides), N increases but R remains the same.  We must
///     retain the actor - ie stealer for actor has a reference on the actor
/// (3) When we start running an actor, our job definitively becomes the
///     owning job, but neither N nor R changes.  We do not need to retain
///     the actor.
/// (4) When we go to start running an actor and for whatever reason we
///     don't actually do so, we are eliminating an extra processing job,
///     and so N decreases but R remains the same.  We must release the
///     actor.
/// (5) When we are running an actor and give it up, and there are no
///     remaining jobs on it, then R becomes 0 and N decreases by 1.
///     We do not need to release the actor.
/// (6) When we are running an actor and give it up, and there are jobs
///     remaining on it, then R remains 1 but N is decreasing by 1.
///     We must either release the actor or create a new processing job
///     for it to maintain the balance.
///
/// The current behaviour of actors is such that we only use the inline
/// processing job to schedule the actor and not OOL jobs. As a result, the
/// subset of rules that currently apply are (1), (3), (5), (6).
class DefaultActorImpl : public HeapObject {
  friend class ProcessInlineJob;
  union {
    // When the ProcessInlineJob storage is initialized, its metadata pointer
    // will point to Job's metadata. When it isn't, the metadata pointer is
    // NULL. Use HeapObject to initialize the metadata pointer to NULL and allow
    // it to be checked without fully initializing the ProcessInlineJob.
    HeapObject JobStorageHeapObject{nullptr};

    ProcessInlineJob JobStorage;
  };
  // This field needs to be aligned to ACTIVE_ACTOR_STATUS but we need to
  // hide this from the compiler cause otherwise it adds a bunch of extra
  // padding to the structure. We will enforce this via static asserts.
  char StatusStorage[sizeof(ActiveActorStatus)];

public:
  /// Properly construct an actor, except for the heap header.
  void initialize(bool isDistributedRemote = false) {
    ActiveActorStatus status = ActiveActorStatus();
    if (isDistributedRemote) {
      status = status.withDistributedRemote();
    }
    _status().store(status, std::memory_order_relaxed);

    SWIFT_TASK_DEBUG_LOG("Creating default actor %p", this);
    JobStorageHeapObject.metadata = nullptr;
    concurrency::trace::actor_create(this);
  }

  /// Properly destruct an actor, except for the heap header.
  void destroy();

  /// Properly respond to the last release of a default actor.  Note
  /// that the actor will have been completely torn down by the time
  /// we reach this point.
  void deallocate();

  /// Try to lock the actor, if it is already running or scheduled, fail
  bool tryLock(bool asDrainer);

  /// Enqueue a job onto the actor. This typically means that the actor hit
  /// contention during the tryLock and so we're taking the slow path
  void enqueue(Job *job, JobPriority priority);

  /// Unlock an actor
  bool unlock(bool forceUnlock);

  // The calling thread must be holding the actor lock while calling this
  Job *drainOne();

  /// Check if the actor is actually a distributed *remote* actor.
  ///
  /// Note that a distributed *local* actor instance is the same as any other
  /// ordinary default (local) actor, and no special handling is needed for them.
  bool isDistributedRemote();

  swift::atomic<ActiveActorStatus> &_status() {
    return reinterpret_cast<swift::atomic<ActiveActorStatus>&> (this->StatusStorage);
  }

  const swift::atomic<ActiveActorStatus> &_status() const {
    return reinterpret_cast<const swift::atomic<ActiveActorStatus>&> (this->StatusStorage);
  }

  // Only for static assert use below, not for actual use otherwise
  static constexpr size_t offsetOfActiveActorStatus() {
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Winvalid-offsetof"
    return offsetof(DefaultActorImpl, StatusStorage);
#pragma clang diagnostic pop
  }

private:
#if SWIFT_CONCURRENCY_ENABLE_PRIORITY_ESCALATION
  dispatch_lock_t *drainLockAddr();
#endif

  void deallocateUnconditional();

  /// Schedule an inline processing job.  This can generally only be
  /// done if we know nobody else is trying to do it at the same time,
  /// e.g. if this thread just sucessfully transitioned the actor from
  /// Idle to Scheduled.
  void scheduleActorProcessJob(JobPriority priority,
                                     bool hasActiveInlineJob);

  static DefaultActorImpl *fromInlineJob(Job *job) {
    assert(isa<ProcessInlineJob>(job));
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Winvalid-offsetof"
    return reinterpret_cast<DefaultActorImpl*>(
      reinterpret_cast<char*>(job) - offsetof(DefaultActorImpl, JobStorage));
#pragma clang diagnostic pop
  }
};

} /// end anonymous namespace

static_assert(sizeof(DefaultActorImpl) <= sizeof(DefaultActor) &&
              alignof(DefaultActorImpl) <= alignof(DefaultActor),
              "DefaultActorImpl doesn't fit in DefaultActor");
static_assert(DefaultActorImpl::offsetOfActiveActorStatus() % ACTIVE_ACTOR_STATUS_SIZE == 0,
              "ActiveActorStatus is aligned to the right size");

static DefaultActorImpl *asImpl(DefaultActor *actor) {
  return reinterpret_cast<DefaultActorImpl*>(actor);
}

static DefaultActor *asAbstract(DefaultActorImpl *actor) {
  return reinterpret_cast<DefaultActor*>(actor);
}

/*****************************************************************************/
/******************** NEW DEFAULT ACTOR IMPLEMENTATION ***********************/
/*****************************************************************************/

/// Given that a job is enqueued normally on a default actor, get/set
/// the next job in the actor's queue.
static JobRef getNextJobInQueue(Job *job) {
  return *reinterpret_cast<JobRef*>(job->SchedulerPrivate);
}
static void setNextJobInQueue(Job *job, JobRef next) {
  *reinterpret_cast<JobRef*>(job->SchedulerPrivate) = next;
}

namespace {

struct JobQueueTraits {
  static Job *getNext(Job *job) {
    return getNextJobInQueue(job).getAsPreprocessedJob();
  }
  static void setNext(Job *job, Job *next) {
    setNextJobInQueue(job, JobRef::getPreprocessed(next));
  }
  static int compare(Job *lhs, Job *rhs) {
    return descendingPriorityOrder(lhs->getPriority(), rhs->getPriority());
  }
};

} // end anonymous namespace


// Called with the actor drain lock held
//
// This function is called when we hit a conflict between preprocessQueue and
// a concurrent enqueuer resulting in unprocessed jobs being queued up in the
// middle.
//
// We need to find the unprocessed jobs enqueued by the enqueuer and process
// them - We know that these unprocessed jobs must exist between the new head
// and the previous start. We can then process these jobs and merge them into
// the already processed list of jobs from the previous iteration of
// preprocessQueue
static Job *
preprocessQueue(JobRef unprocessedStart, JobRef unprocessedEnd, Job *existingProcessedJobsToMergeInto)
{
  assert(existingProcessedJobsToMergeInto != NULL);
  assert(unprocessedStart.needsPreprocessing());
  assert(unprocessedStart.getAsJob() != unprocessedEnd.getAsJob());

  // Build up a list of jobs we need to preprocess
  using ListMerger = swift::ListMerger<Job*, JobQueueTraits>;
  ListMerger jobsToProcess;

  // Get just the prefix list of unprocessed jobs
  auto current = unprocessedStart;
  while (current != unprocessedEnd) {
    assert(current.needsPreprocessing());
    // Advance current to next pointer and process current unprocessed job
    auto job = current.getAsJob();
    current = getNextJobInQueue(job);

    jobsToProcess.insertAtFront(job);
  }

  // Finish processing the unprocessed jobs
  Job *newProcessedJobs = jobsToProcess.release();
  assert(newProcessedJobs);

  ListMerger mergedList(existingProcessedJobsToMergeInto);
  mergedList.merge(newProcessedJobs);
  return mergedList.release();
}

// Called with the actor drain lock held.
//
// Preprocess the queue starting from the top
static Job *
preprocessQueue(JobRef start) {
  if (!start) {
    return NULL;
  }

  // Entire queue is well formed, no pre-processing needed
  if (!start.needsPreprocessing()) {
    return start.getAsPreprocessedJob();
  }

  // There exist some jobs which haven't been preprocessed

  // Build up a list of jobs we need to preprocess
  using ListMerger = swift::ListMerger<Job*, JobQueueTraits>;
  ListMerger jobsToProcess;

  Job *wellFormedListStart = NULL;

  auto current = start;
  while (current) {
    if (!current.needsPreprocessing()) {
      // We can assume that everything from here onwards as being well formed
      // and sorted
      wellFormedListStart = current.getAsPreprocessedJob();
      break;
    }

    // Advance current to next pointer and insert current fella to jobsToProcess
    // list
    auto job = current.getAsJob();
    current = getNextJobInQueue(job);

    jobsToProcess.insertAtFront(job);
  }

  // Finish processing the unprocessed jobs
  auto processedJobHead = jobsToProcess.release();
  assert(processedJobHead);

  Job *firstJob = NULL;
  if (wellFormedListStart) {
    // Merge it with already known well formed list if we have one.
    ListMerger mergedList(wellFormedListStart);
    mergedList.merge(processedJobHead);
    firstJob = mergedList.release();
  } else {
    // Nothing to merge with, just return the head we already have
    firstJob = processedJobHead;
  }

  return firstJob;
}

static void traceJobQueue(DefaultActorImpl *actor, Job *first) {
  concurrency::trace::actor_note_job_queue(actor, first, [](Job *job) {
    return getNextJobInQueue(job).getAsPreprocessedJob();
  });
}

static SWIFT_ATTRIBUTE_ALWAYS_INLINE void traceActorStateTransition(DefaultActorImpl *actor,
    ActiveActorStatus oldState, ActiveActorStatus newState) {

  SWIFT_TASK_DEBUG_LOG("Actor %p transitioned from %#x to %#x (%s)\n", actor,
                       oldState.getOpaqueFlags(), newState.getOpaqueFlags(),
                       __FUNCTION__);
  newState.traceStateChanged(actor);
}

void DefaultActorImpl::destroy() {
  auto oldState = _status().load(std::memory_order_relaxed);
  // Tasks on an actor are supposed to keep the actor alive until they start
  // running and we can only get here if ref count of the object = 0 which means
  // there should be no more tasks enqueued on the actor.
  assert(!oldState.getFirstJob() && "actor has queued jobs at destruction");

  if (oldState.isIdle()) {
      return;
  }
  assert(oldState.isRunning() && "actor scheduled but not running at destruction");
}

void DefaultActorImpl::deallocate() {
  // If we're running, mark ourselves as ready for deallocation but don't
  // deallocate yet. When we stop running the actor - at unlock() time - we'll
  // do the actual deallocation.
  //
  // If we're idle, just deallocate immediately
  auto oldState = _status().load(std::memory_order_relaxed);
  while (oldState.isRunning()) {
    auto newState = oldState.withZombie_ReadyForDeallocation();
    if (_status().compare_exchange_weak(oldState, newState,
                                           std::memory_order_relaxed,
                                           std::memory_order_relaxed))
      return;
  }

  assert(oldState.isIdle());
  deallocateUnconditional();
}

#if SWIFT_CONCURRENCY_ENABLE_PRIORITY_ESCALATION
dispatch_lock_t *DefaultActorImpl::drainLockAddr() {
  ActiveActorStatus *actorStatus = (ActiveActorStatus *) &this->StatusStorage;
  return (dispatch_lock_t *) (((char *) actorStatus) + ActiveActorStatus::drainLockOffset());
}
#endif

void DefaultActorImpl::deallocateUnconditional() {
  concurrency::trace::actor_deallocate(this);

  if (JobStorageHeapObject.metadata != nullptr)
    JobStorage.~ProcessInlineJob();
  auto metadata = cast<ClassMetadata>(this->metadata);
  swift_deallocObject(this, metadata->getInstanceSize(),
                      metadata->getInstanceAlignMask());
}

void DefaultActorImpl::scheduleActorProcessJob(JobPriority priority, bool useInlineJob) {
  Job *job;
  if (useInlineJob) {
    if (JobStorageHeapObject.metadata != nullptr)
      JobStorage.~ProcessInlineJob();
    job = new (&JobStorage) ProcessInlineJob(priority);
  } else {
    assert(false && "Should not be here - we don't have support for any OOL actor process jobs yet");
    // TODO (rokhinip): Don't we need to take a +1 per ref count rules specified?
    swift_retain(this);
    job = new ProcessOutOfLineJob(this, priority);
  }
  SWIFT_TASK_DEBUG_LOG(
      "Scheduling processing job %p for actor %p at priority %#zx", job, this,
      priority);
  swift_task_enqueueGlobal(job);
}


bool DefaultActorImpl::tryLock(bool asDrainer) {
#if SWIFT_CONCURRENCY_ENABLE_PRIORITY_ESCALATION
  SWIFT_TASK_DEBUG_LOG("Thread %#x attempting to jump onto %p, as drainer = %d", dispatch_lock_value_for_self(), this, asDrainer);
  dispatch_thread_override_info_s threadOverrideInfo;
  threadOverrideInfo = swift_dispatch_thread_get_current_override_qos_floor();
  qos_class_t overrideFloor = threadOverrideInfo.override_qos_floor;

retry:;
#else
  SWIFT_TASK_DEBUG_LOG("Thread attempting to jump onto %p, as drainer = %d", this, asDrainer);
#endif

  auto oldState = _status().load(std::memory_order_relaxed);
  while (true) {

    if (asDrainer) {
      // TODO (rokhinip): Once we have OOL process job support, this assert can
      // potentially fail due to a race with an actor stealer that might have
      // won the race and started running the actor
      assert(oldState.isScheduled());

#if SWIFT_CONCURRENCY_ENABLE_PRIORITY_ESCALATION
      // We only want to self override a thread if we are taking the actor lock
      // as a drainer because there might have been higher priority work
      // enqueued that might have escalated the max priority of the actor to be
      // higher than the original thread request.
      qos_class_t maxActorPriority = (qos_class_t) oldState.getMaxPriority();

      if (threadOverrideInfo.can_override && (maxActorPriority > overrideFloor)) {
        SWIFT_TASK_DEBUG_LOG("[Override] Self-override thread with oq_floor %#x to match max actor %p's priority %#x", overrideFloor, this, maxActorPriority);

        (void) swift_dispatch_thread_override_self(maxActorPriority);
        overrideFloor = maxActorPriority;
        goto retry;
      }
#endif
    } else {
      // We're trying to take the lock in an uncontended manner
      if (oldState.isRunning() || oldState.isScheduled()) {
        SWIFT_TASK_DEBUG_LOG("Failed to jump to %p in fast path", this);
        return false;
      }

      assert(oldState.getMaxPriority() == JobPriority::Unspecified);
      assert(!oldState.getFirstJob());
    }

    auto newState = oldState.withRunning();
    if (_status().compare_exchange_weak(oldState, newState,
                                 std::memory_order_relaxed,
                                 std::memory_order_relaxed)) {
      _swift_tsan_acquire(this);
      traceActorStateTransition(this, oldState, newState);
      return true;
    }
  }
}

void DefaultActorImpl::enqueue(Job *job, JobPriority priority) {
  // We can do relaxed loads here, we are just using the current head in the
  // atomic state and linking that into the new job we are inserting, we don't
  // need acquires
  SWIFT_TASK_DEBUG_LOG("Enqueueing job %p onto actor %p at priority %#zx", job,
                       this, priority);
  concurrency::trace::actor_enqueue(this, job);
  auto oldState = _status().load(std::memory_order_relaxed);
  while (true) {
    auto newState = oldState;

    // Link this into the queue in the atomic state
    JobRef currentHead = oldState.getFirstJob();
    setNextJobInQueue(job, currentHead);
    JobRef newHead = JobRef::getUnpreprocessed(job);

    newState = newState.withFirstJob(newHead);

    if (oldState.isIdle()) {
      // Someone gave up the actor lock after we failed fast path.
      // Schedule the actor
      newState = newState.withScheduled();
      newState = newState.withNewPriority(priority);
    } else {
      if (priority > oldState.getMaxPriority()) {
        newState = newState.withEscalatedPriority(priority);
      }
    }

    if (_status().compare_exchange_weak(oldState, newState,
                   /* success */ std::memory_order_release,
                   /* failure */ std::memory_order_relaxed)) {
      traceActorStateTransition(this, oldState, newState);

      if (!oldState.isScheduled() && newState.isScheduled()) {
        // We took responsibility to schedule the actor for the first time. See
        // also ownership rule (1)
        return scheduleActorProcessJob(newState.getMaxPriority(), true);
      }

#if SWIFT_CONCURRENCY_ENABLE_PRIORITY_ESCALATION
      if (oldState.getMaxPriority() != newState.getMaxPriority()) {
        if (newState.isRunning()) {
          // Actor is running on a thread, escalate the thread running it
          SWIFT_TASK_DEBUG_LOG("[Override] Escalating actor %p which is running on %#x to %#x priority", this, newState.currentDrainer(), priority);
          dispatch_lock_t *lockAddr = this->drainLockAddr();
          swift_dispatch_lock_override_start_with_debounce(lockAddr, newState.currentDrainer(),
                         (qos_class_t) priority);
        } else {
          // TODO (rokhinip): Actor is scheduled - we need to schedule a
          // stealer at the higher priority
          //
          // TODO (rokhinip): Add a signpost to flag that this is a potential
          // priority inversion
          SWIFT_TASK_DEBUG_LOG("[Override] Escalating actor %p which is enqueued", this);
        }
      }
#endif
      return;
    }
  }
}

/* This can be called in 2 ways:
 *    * force_unlock = true: Thread is following task and therefore wants to
 *    give up the current actor since task is switching away.
 *    * force_unlock = false: Thread is not necessarily following the task, it
 *    may want to follow actor if there is more work on it.
 *
 *    Returns true if we managed to unlock the actor, false if we didn't. If the
 *    actor has more jobs remaining on it during unlock, this method will
 *    schedule the actor
 */
bool DefaultActorImpl::unlock(bool forceUnlock)
{
  auto oldState = _status().load(std::memory_order_relaxed);
  SWIFT_TASK_DEBUG_LOG("Try unlock-ing actor %p with forceUnlock = %d", this, forceUnlock);

  _swift_tsan_release(this);
  while (true) {
    assert(oldState.isAnyRunning());
#if SWIFT_CONCURRENCY_ENABLE_PRIORITY_ESCALATION
    assert(dispatch_lock_is_locked_by_self(*(this->drainLockAddr())));
#endif

    if (oldState.isZombie_ReadyForDeallocation()) {
#if SWIFT_CONCURRENCY_ENABLE_PRIORITY_ESCALATION
      // Reset any override on this thread as a result of this thread running
      // the actor
      if (oldState.isMaxPriorityEscalated()) {
        swift_dispatch_lock_override_end((qos_class_t)oldState.getMaxPriority());
      }
#endif
      deallocateUnconditional();
      SWIFT_TASK_DEBUG_LOG("Unlock-ing actor %p succeeded with full deallocation", this);
      return true;
    }

    auto newState = oldState;
    if (oldState.getFirstJob()) {
      // There is work left to do, don't unlock the actor
      if (!forceUnlock) {
        SWIFT_TASK_DEBUG_LOG("Unlock-ing actor %p failed", this);
        return false;
      }
      // We need to schedule the actor - remove any escalation bits since we'll
      // schedule the actor at the max priority currently on it
      newState = newState.withScheduled();
      newState = newState.withoutEscalatedPriority();
    } else {
      // There is no work left to do - actor goes idle
      newState = newState.withIdle();
      newState = newState.resetPriority();
    }

    if (_status().compare_exchange_weak(oldState, newState,
                      /* success */ std::memory_order_relaxed,
                      /* failure */ std::memory_order_relaxed)) {
      traceActorStateTransition(this, oldState, newState);

      if (newState.isScheduled()) {
        // See ownership rule (6) in DefaultActorImpl
        assert(newState.getFirstJob());
        scheduleActorProcessJob(newState.getMaxPriority(), true);
      } else {
        // See ownership rule (5) in DefaultActorImpl
        SWIFT_TASK_DEBUG_LOG("Actor %p is idle now", this);
      }

#if SWIFT_CONCURRENCY_ENABLE_PRIORITY_ESCALATION
      // Reset any override on this thread as a result of this thread running
      // the actor. Only do this after we have reenqueued the actor
      if (oldState.isMaxPriorityEscalated()) {
        swift_dispatch_lock_override_end((qos_class_t) oldState.getMaxPriority());
      }
#endif
      return true;
    }
  }
}

// Called with actor lock held on current thread
Job * DefaultActorImpl::drainOne() {
  SWIFT_TASK_DEBUG_LOG("Draining one job from default actor %p", this);

  auto oldState = _status().load(std::memory_order_acquire);

  auto jobToPreprocessFrom = oldState.getFirstJob();
  Job *firstJob = preprocessQueue(jobToPreprocessFrom);
  traceJobQueue(this, firstJob);

  _swift_tsan_release(this);
  while (true) {
    assert(oldState.isAnyRunning());

    if (!firstJob) {
      // Nothing to drain, short circuit
      SWIFT_TASK_DEBUG_LOG("No jobs to drain on actor %p", this);
      return NULL;
    }

    auto newState = oldState;
    // Dequeue the first job and set up a new head
    newState = newState.withFirstJob(getNextJobInQueue(firstJob));
    if (_status().compare_exchange_weak(oldState, newState,
                            /* sucess */ std::memory_order_release,
                            /* failure */ std::memory_order_acquire)) {
      SWIFT_TASK_DEBUG_LOG("Drained first job %p from actor %p", firstJob, this);
      traceActorStateTransition(this, oldState, newState);
      concurrency::trace::actor_dequeue(this, firstJob);
      return firstJob;
    }

    // We failed the weak cmpxchg spuriously, go through loop again.
    if (oldState.getFirstJob().getAsJob() == jobToPreprocessFrom.getAsJob()) {
      continue;
    }

    // There were new items concurrently added to the queue. We need to
    // preprocess the newly added unprocessed items and merge them to the already
    // preprocessed list.
    //
    // The newly merged items that need to be preprocessed, are between the head
    // of the linked list, and the last job we did the previous preprocessQueue
    // on
    firstJob = preprocessQueue(oldState.getFirstJob(), jobToPreprocessFrom, firstJob);
    jobToPreprocessFrom = oldState.getFirstJob();
    traceJobQueue(this, firstJob);
  }
}

// Called from processing jobs which are created to drain an actor. We need to
// reason about this function together with swift_task_switch because threads
// can switch from "following actors" to "following tasks" and vice versa.
//
// The way this function works is as following:
//
// 1. We grab the actor lock for the actor we were scheduled to drain.
// 2. Drain the first task out of the actor and run it. Henceforth, the thread
// starts executing the task and following it around. It is no longer a
// processing job for the actor. Note that it will hold the actor lock until it
// needs to hop off the actor but conceptually, it is now a task executor as well.
// 3. When the thread needs to hop off the actor, it is done deep in the
// callstack of this function with an indirection through user code:
// defaultActorDrain -> runJobInEstablishedExecutorContext ->  user
// code -> a call to swift_task_switch to hop out of actor
// 4. This call to swift_task_switch() will attempt to hop off the existing
// actor and jump to the new one. There are 2 possible outcomes at that point:
//   (a) We were able to hop to the new actor and so thread continues executing
//   task. We then schedule a job for the old actor to continue if it has
//   pending work
//   (b) We were not able to take the fast path to the new actor, the task gets
//   enqueued onto the new actor it is going to, and the thread now follows the
//   actor we were trying to hop off. At this point, note that we don't give up
//   the actor lock in `swift_task_switchImpl` so we will come back to
//   defaultActorDrain with the actor lock still held.
//
// At the point of return from the job execution, we may not be holding the lock
// of the same actor that we had started off with, so we need to reevaluate what
// the current actor is
static void defaultActorDrain(DefaultActorImpl *actor) {
  SWIFT_TASK_DEBUG_LOG("Draining default actor %p", actor);
  DefaultActorImpl *currentActor = actor;

  bool actorLockAcquired = actor->tryLock(true);
  // We always must succeed in taking the actor lock that we are draining
  // because we don't have to compete with OOL jobs. See ownership rule (3)
  assert(actorLockAcquired);

  // Setup a TSD for tracking current execution info
  ExecutorTrackingInfo trackingInfo;
  trackingInfo.enterAndShadow(
    ExecutorRef::forDefaultActor(asAbstract(currentActor)));

  while (true) {
    if (shouldYieldThread()) {
      currentActor->unlock(true);
      break;
    }

    Job *job = currentActor->drainOne();
    if (job == NULL) {
      // No work left to do, try unlocking the actor. This may fail if there is
      // work concurrently enqueued in which case, we'd try again in the loop
      if (!currentActor->unlock(false)) {
        continue;
      }
      break;
    }

    // This thread is now going to follow the task on this actor. It may hop off
    // the actor
    runJobInEstablishedExecutorContext(job);

    // We could have come back from the job on a generic executor and not as
    // part of a default actor. If so, there is no more work left for us to do
    // here.
    auto currentExecutor = trackingInfo.getActiveExecutor();
    if (!currentExecutor.isDefaultActor()) {
      currentActor = nullptr;
      break;
    }
    currentActor = asImpl(currentExecutor.getDefaultActor());
  }

  // Leave the tracking info.
  trackingInfo.leave();

  // Balances with the retain taken in Process{Inline,OutOfLine}Job::process
  swift_release(actor);
}

SWIFT_CC(swift)
static void swift_job_runImpl(Job *job, ExecutorRef executor) {
  ExecutorTrackingInfo trackingInfo;

  // swift_job_run is a primary entrypoint for executors telling us to
  // run jobs.  Actor executors won't expect us to switch off them
  // during this operation.  But do allow switching if the executor
  // is generic.
  if (!executor.isGeneric()) trackingInfo.disallowSwitching();

  trackingInfo.enterAndShadow(executor);
  auto traceHandle = concurrency::trace::job_run_begin(job, &executor);

  SWIFT_TASK_DEBUG_LOG("job %p", job);
  runJobInEstablishedExecutorContext(job);

  concurrency::trace::job_run_end(&executor, traceHandle);
  trackingInfo.leave();

  // Give up the current executor if this is a switching context
  // (which, remember, only happens if we started out on a generic
  // executor) and we've switched to a default actor.
  auto currentExecutor = trackingInfo.getActiveExecutor();
  if (trackingInfo.allowsSwitching() && currentExecutor.isDefaultActor()) {
    asImpl(currentExecutor.getDefaultActor())->unlock(true);
  }
}

SWIFT_CC(swiftasync)
void ProcessInlineJob::process(Job *job) {
  DefaultActorImpl *actor = DefaultActorImpl::fromInlineJob(job);

  swift_retain(actor);
  return defaultActorDrain(actor); // 'return' forces tail call
}

// Currently unused
SWIFT_CC(swiftasync)
void ProcessOutOfLineJob::process(Job *job) {
  auto self = cast<ProcessOutOfLineJob>(job);
  DefaultActorImpl *actor = self->Actor;

  delete self;

  swift_retain(actor);
  return defaultActorDrain(actor); // 'return' forces tail call
}

void swift::swift_defaultActor_initialize(DefaultActor *_actor) {
  asImpl(_actor)->initialize();
}

void swift::swift_defaultActor_destroy(DefaultActor *_actor) {
  asImpl(_actor)->destroy();
}

void swift::swift_defaultActor_enqueue(Job *job, DefaultActor *_actor) {
  asImpl(_actor)->enqueue(job, job->getPriority());
}

void swift::swift_defaultActor_deallocate(DefaultActor *_actor) {
  asImpl(_actor)->deallocate();
}

static bool isDefaultActorClass(const ClassMetadata *metadata) {
  assert(metadata->isTypeMetadata());
  while (true) {
    // Trust the class descriptor if it says it's a default actor.
    if (metadata->getDescription()->isDefaultActor())
      return true;

    // Go to the superclass.
    metadata = metadata->Superclass;

    // If we run out of Swift classes, it's not a default actor.
    if (!metadata || !metadata->isTypeMetadata()) return false;
  }
}

void swift::swift_defaultActor_deallocateResilient(HeapObject *actor) {
  auto metadata = cast<ClassMetadata>(actor->metadata);
  if (isDefaultActorClass(metadata))
    return swift_defaultActor_deallocate(static_cast<DefaultActor*>(actor));

  swift_deallocObject(actor, metadata->getInstanceSize(),
                      metadata->getInstanceAlignMask());
}

/// FIXME: only exists for the quick-and-dirty MainActor implementation.
namespace swift {
  Metadata* MainActorMetadata = nullptr;
}

/*****************************************************************************/
/****************************** ACTOR SWITCHING ******************************/
/*****************************************************************************/

/// Can the current executor give up its thread?
static bool canGiveUpThreadForSwitch(ExecutorTrackingInfo *trackingInfo,
                                     ExecutorRef currentExecutor) {
  assert(trackingInfo || currentExecutor.isGeneric());

  // Some contexts don't allow switching at all.
  if (trackingInfo && !trackingInfo->allowsSwitching())
    return false;

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
static void giveUpThreadForSwitch(ExecutorRef currentExecutor) {
  if (currentExecutor.isGeneric()) {
    SWIFT_TASK_DEBUG_LOG("Giving up current generic executor %p",
                         currentExecutor.getIdentity());
    return;
  }

  asImpl(currentExecutor.getDefaultActor())->unlock(true);
}

/// Try to assume control of the current thread for the given executor
/// in order to run the given job.
///
/// This doesn't actually run the job yet.
///
/// Note that we don't update DefaultActorProcessingFrame here; we'll
/// do that in runOnAssumedThread.
static bool tryAssumeThreadForSwitch(ExecutorRef newExecutor) {
  // If the new executor is generic, we don't need to do anything.
  if (newExecutor.isGeneric()) {
    return true;
  }

  // If the new executor is a default actor, ask it to assume the thread.
  if (newExecutor.isDefaultActor()) {
    return asImpl(newExecutor.getDefaultActor())->tryLock(false);
  }

  return false;
}

/// Given that we've assumed control of an executor on this thread,
/// continue to run the given task on it.
SWIFT_CC(swiftasync)
static void runOnAssumedThread(AsyncTask *task, ExecutorRef executor,
                               ExecutorTrackingInfo *oldTracking) {
  // Note that this doesn't change the active task and so doesn't
  // need to either update ActiveTask or flagAsRunning/flagAsSuspended.

  // If there's alreaady tracking info set up, just change the executor
  // there and tail-call the task.  We don't want these frames to
  // potentially accumulate linearly.
  if (oldTracking) {
    oldTracking->setActiveExecutor(executor);

    return task->runInFullyEstablishedContext(); // 'return' forces tail call
  }

  // Otherwise, set up tracking info.
  ExecutorTrackingInfo trackingInfo;
  trackingInfo.enterAndShadow(executor);

  // Run the new task.
  task->runInFullyEstablishedContext();

  // Leave the tracking frame, and give up the current actor if
  // we have one.
  //
  // In principle, we could execute more tasks from the actor here, but
  // that's probably not a reasonable thing to do in an assumed context
  // rather than a dedicated actor-processing job.
  executor = trackingInfo.getActiveExecutor();
  trackingInfo.leave();

  SWIFT_TASK_DEBUG_LOG("leaving assumed thread, current executor is %p",
                       executor.getIdentity());

  if (executor.isDefaultActor())
    asImpl(executor.getDefaultActor())->unlock(true);
}

// TODO (rokhinip): Workaround rdar://88700717. To be removed with
// rdar://88711954
SWIFT_CC(swiftasync)
static void swift_task_switchImpl(SWIFT_ASYNC_CONTEXT AsyncContext *resumeContext,
                                  TaskContinuationFunction *resumeFunction,
                                  ExecutorRef newExecutor) SWIFT_OPTNONE {
  auto task = swift_task_getCurrent();
  assert(task && "no current task!");

  auto trackingInfo = ExecutorTrackingInfo::current();
  auto currentExecutor =
    (trackingInfo ? trackingInfo->getActiveExecutor()
                  : ExecutorRef::generic());
  SWIFT_TASK_DEBUG_LOG("Task %p trying to switch from executor %p to %p", task,
                       currentExecutor.getIdentity(),
                       newExecutor.getIdentity());

  // If the current executor is compatible with running the new executor,
  // we can just immediately continue running with the resume function
  // we were passed in.
  if (!currentExecutor.mustSwitchToRun(newExecutor)) {
    return resumeFunction(resumeContext); // 'return' forces tail call
  }

  // Park the task for simplicity instead of trying to thread the
  // initial resumption information into everything below.
  task->ResumeContext = resumeContext;
  task->ResumeTask = resumeFunction;

  // If the current executor can give up its thread, and the new executor
  // can take over a thread, try to do so; but don't do this if we've
  // been asked to yield the thread.
  if (canGiveUpThreadForSwitch(trackingInfo, currentExecutor) &&
      !shouldYieldThread() &&
      tryAssumeThreadForSwitch(newExecutor)) {
    SWIFT_TASK_DEBUG_LOG(
        "switch succeeded, task %p assumed thread for executor %p", task,
        newExecutor.getIdentity());
    giveUpThreadForSwitch(currentExecutor);
    // 'return' forces tail call
    return runOnAssumedThread(task, newExecutor, trackingInfo);
  }

  // Otherwise, just asynchronously enqueue the task on the given
  // executor.
  SWIFT_TASK_DEBUG_LOG("switch failed, task %p enqueued on executor %p", task,
                       newExecutor.getIdentity());

  task->flagAsAndEnqueueOnExecutor(newExecutor);
  _swift_task_clearCurrent();
}

/*****************************************************************************/
/************************* GENERIC ACTOR INTERFACES **************************/
/*****************************************************************************/

// Implemented in Swift to avoid some annoying hard-coding about
// SerialExecutor's protocol witness table.  We could inline this
// with effort, though.
extern "C" SWIFT_CC(swift)
void _swift_task_enqueueOnExecutor(Job *job, HeapObject *executor,
                                   const Metadata *selfType,
                                   const SerialExecutorWitnessTable *wtable);

SWIFT_CC(swift)
static void swift_task_enqueueImpl(Job *job, ExecutorRef executor) {
  SWIFT_TASK_DEBUG_LOG("enqueue job %p on executor %p", job,
                       executor.getIdentity());

  assert(job && "no job provided");

  _swift_tsan_release(job);

  if (executor.isGeneric())
    return swift_task_enqueueGlobal(job);

  if (executor.isDefaultActor())
    return asImpl(executor.getDefaultActor())->enqueue(job, job->getPriority());

  auto wtable = executor.getSerialExecutorWitnessTable();
  auto executorObject = executor.getIdentity();
  auto executorType = swift_getObjectType(executorObject);
  _swift_task_enqueueOnExecutor(job, executorObject, executorType, wtable);
}

#define OVERRIDE_ACTOR COMPATIBILITY_OVERRIDE
#include COMPATIBILITY_OVERRIDE_INCLUDE_PATH


/*****************************************************************************/
/***************************** DISTRIBUTED ACTOR *****************************/
/*****************************************************************************/

OpaqueValue*
swift::swift_distributedActor_remote_initialize(const Metadata *actorType) {
  auto *classMetadata = actorType->getClassObject();

  // TODO(distributed): make this allocation smaller
  // ==== Allocate the memory for the remote instance
  HeapObject *alloc = swift_allocObject(classMetadata,
                                        classMetadata->getInstanceSize(),
                                        classMetadata->getInstanceAlignMask());

  // TODO: remove this memset eventually, today we only do this to not have
  //       to modify the destructor logic, as releasing zeroes is no-op
  memset(alloc + 1, 0, classMetadata->getInstanceSize() - sizeof(HeapObject));

  // TODO(distributed): a remote one does not have to have the "real"
  //  default actor body, e.g. we don't need an executor at all; so
  //  we can allocate more efficiently and only share the flags/status field
  //  between the both memory representations
  // --- Currently we ride on the DefaultActorImpl to reuse the memory layout
  // of the flags etc. So initialize the default actor into the allocation.
  auto actor = asImpl(reinterpret_cast<DefaultActor*>(alloc));
  actor->initialize(/*remote*/true);
  assert(actor->isDistributedRemote());

  return reinterpret_cast<OpaqueValue*>(actor);
}

bool swift::swift_distributed_actor_is_remote(DefaultActor *_actor) {
  return asImpl(_actor)->isDistributedRemote();
}

bool DefaultActorImpl::isDistributedRemote() {
  auto state = _status().load(std::memory_order_relaxed);
  return state.isDistributedRemote();
}
