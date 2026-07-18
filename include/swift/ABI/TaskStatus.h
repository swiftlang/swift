//===--- TaskStatusRecord.h - Structures to track task status --*- C++ -*-===//
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
// Swift ABI describing "status records", the mechanism by which
// tasks track dynamic information about their child tasks, custom
// cancellation hooks, and other information which may need to be exposed
// asynchronously outside of the task.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_ABI_TASKSTATUS_H
#define SWIFT_ABI_TASKSTATUS_H

#include "swift/Basic/OptionSet.h"
#include "swift/ABI/MetadataValues.h"
#include "swift/ABI/Task.h"
#include "swift/ABI/Executor.h"
#include "swift/Runtime/HeapObject.h"

namespace swift {

/// The abstract base class for all status records.
///
/// TaskStatusRecords are typically allocated on the stack (possibly in the task
/// context), partially initialized, and then atomically added to the task with
/// `addStatusRecord`.
///
/// Status records can be added to or removed from  a task by itself or by other
/// threads. As a result, while registered with the task, a status record
/// should only be modified in ways that respect the possibility of asynchronous
/// access by a different thread.  In particular, the chain of status records
/// must not be disturbed. When the task leaves the scope that requires the
/// status record, the record can be unregistered from the task with
/// `removeStatusRecord`, at which point the memory can be returned to the
/// system.
class TaskStatusRecord {
public:
  TaskStatusRecordFlags Flags;
  TaskStatusRecord *Parent;

  TaskStatusRecord(TaskStatusRecordKind kind,
                   TaskStatusRecord *parent = nullptr)
      : Flags(kind) {
    getKind();
    resetParent(parent);
  }

  TaskStatusRecord(const TaskStatusRecord &) = delete;
  TaskStatusRecord &operator=(const TaskStatusRecord &) = delete;

  TaskStatusRecordKind getKind() const {
    return Flags.getKind();
  }

  TaskStatusRecord *getParent() const { return Parent; }

  /// Change the parent of this status record to the given record.
  void resetParent(TaskStatusRecord *newParent) {
    Parent = newParent;
  }
};

/// A status record which states that a task has one or
/// more active child tasks.
class ChildTaskStatusRecord : public TaskStatusRecord {
  AsyncTask *FirstChild;

public:
  ChildTaskStatusRecord(AsyncTask *child)
      : TaskStatusRecord(TaskStatusRecordKind::ChildTask), FirstChild(child) {}

  ChildTaskStatusRecord(AsyncTask *child, TaskStatusRecordKind kind)
      : TaskStatusRecord(kind), FirstChild(child) {
    assert(kind == TaskStatusRecordKind::ChildTask);
    assert(!child->hasGroupChildFragment() &&
           "Group child tasks must be tracked in their respective "
           "TaskGroupTaskStatusRecord, and not as independent "
           "ChildTaskStatusRecord "
           "records.");
  }

  /// Return the first child linked by this record.  This may be null;
  /// if not, it (and all of its successors) are guaranteed to satisfy
  /// `isChildTask()`.
  AsyncTask *getFirstChild() const { return FirstChild; }

  static AsyncTask *getNextChildTask(AsyncTask *task) {
    return task->childFragment()->getNextChild();
  }

  using child_iterator = LinkedListIterator<AsyncTask, getNextChildTask>;
  llvm::iterator_range<child_iterator> children() const {
    return child_iterator::rangeBeginning(getFirstChild());
  }

  static bool classof(const TaskStatusRecord *record) {
    return record->getKind() == TaskStatusRecordKind::ChildTask;
  }
};

/// A status record which states that a task has a task group.
///
/// A record always is an instance of a `TaskGroupBase` subclass.
///
/// This record holds references to all the non-completed children of
/// the task group.  It may also hold references to completed children
/// which have not yet been found by `next()`.
///
/// The child tasks are stored as an invasive doubly-linked list, starting
/// from `FirstChild` and continuing through the `NextChild` and `PrevChild`
/// pointers of all the linked children.
///
/// This list structure should only ever be modified:
/// - while holding the status record lock of the owning task, so that
///   asynchronous operations such as cancellation can walk the structure
///   without having to acquire a secondary lock, and
/// - synchronously with the owning task, so that the owning task doesn't
///   have to acquire the status record lock just to walk the structure
///   itself.
///
/// When the group exits, it may simply remove this single record from the task
/// running it, as it has guaranteed that the tasks have already completed.
///
/// Group child tasks DO NOT have their own `ChildTaskStatusRecord` entries,
/// and are only tracked by their respective `TaskGroupTaskStatusRecord`.
class TaskGroupTaskStatusRecord : public TaskStatusRecord {
  // FirstChild may be read concurrently to check for the presence of children,
  // so it needs to be atomic. The pointer is never dereferenced in that case,
  // so we can universally use memory_order_relaxed on it.
  std::atomic<AsyncTask *> FirstChild;
  AsyncTask *LastChild;

public:
  TaskGroupTaskStatusRecord()
      : TaskStatusRecord(TaskStatusRecordKind::TaskGroup),
        FirstChild(nullptr),
        LastChild(nullptr) {}

  TaskGroupTaskStatusRecord(AsyncTask *child)
      : TaskStatusRecord(TaskStatusRecordKind::TaskGroup),
        FirstChild(child),
        LastChild(child) {
    assert(!LastChild || !LastChild->childFragment()->getNextChild());
  }

  /// Get the task group this record is associated with.
  TaskGroup *getGroup();

  /// Return the first child linked by this record.  This may be null;
  /// if not, it (and all of its successors) are guaranteed to satisfy
  /// `isChildTask()`.
  AsyncTask *getFirstChild() const {
    return FirstChild.load(std::memory_order_relaxed);
  }

  /// Attach the passed in `child` task to this group.
  void attachChild(AsyncTask *child) {
    assert(child->hasGroupChildFragment());
    assert(child->groupChildFragment()->getGroup() == getGroup());

    auto oldLastChild = LastChild;
    LastChild = child;

    // Set the prev pointer of the new child to the old last child.
    child->childFragment()->setPrevChild(oldLastChild);

    if (!getFirstChild()) {
      // This is the first child we ever attach, so store it as FirstChild.
      FirstChild.store(child, std::memory_order_relaxed);
      return;
    }

    oldLastChild->childFragment()->setNextChild(child);
  }

  void detachChild(AsyncTask *child) {
    assert(child && "cannot remove a null child from group");

    auto *childFragment = child->childFragment();
    auto *prev = childFragment->getPrevChild();
    auto *next = childFragment->getNextChild();

    if (prev) {
      prev->childFragment()->setNextChild(next);
    } else {
      // child is the first in the list.
      FirstChild.store(next, std::memory_order_relaxed);
    }

    if (next) {
      next->childFragment()->setPrevChild(prev);
    } else {
      // child is the last in the list.
      LastChild = prev;
    }

    childFragment->setPrevChild(nullptr);
    childFragment->setNextChild(nullptr);
  }

  static AsyncTask *getNextChildTask(AsyncTask *task) {
    return task->childFragment()->getNextChild();
  }

  using child_iterator = LinkedListIterator<AsyncTask, getNextChildTask>;
  llvm::iterator_range<child_iterator> children() const {
    return child_iterator::rangeBeginning(getFirstChild());
  }

  static bool classof(const TaskStatusRecord *record) {
    return record->getKind() == TaskStatusRecordKind::TaskGroup;
  }
};

/// A cancellation record which states that a task has an arbitrary
/// function that needs to be called if the task is cancelled.
///
/// The end of any call to the function will be ordered before the
/// end of a call to unregister this record from the task.  That is,
/// code may call `removeStatusRecord` and freely
/// assume after it returns that this function will not be
/// subsequently used.
class CancellationNotificationStatusRecord : public TaskStatusRecord {
public:
  using FunctionType = SWIFT_CC(swift) void(SWIFT_CONTEXT void *);

private:
  FunctionType *__ptrauth_swift_cancellation_notification_function Function;
  void *Argument;

  /// FIXME: Investigate whether this at-most-once flag is actually needed.
  std::atomic<bool> Fired{false};

public:
  CancellationNotificationStatusRecord(FunctionType *fn, void *arg)
      : TaskStatusRecord(TaskStatusRecordKind::CancellationNotification),
        Function(fn), Argument(arg) {}

  void run() {
    bool expected = false;
    if (!Fired.compare_exchange_strong(expected, true,
                                       std::memory_order_relaxed))
      return;
    Function(Argument);
  }

  static bool classof(const TaskStatusRecord *record) {
    return record->getKind() == TaskStatusRecordKind::CancellationNotification;
  }
};

/// A status record which says that a task has an arbitrary
/// function that needs to be called if the task's priority is escalated.
///
/// The end of any call to the function will be ordered before the
/// end of a call to unregister this record from the task.  That is,
/// code may call `removeStatusRecord` and freely
/// assume after it returns that this function will not be
/// subsequently used.
class EscalationNotificationStatusRecord : public TaskStatusRecord {
public:
  using FunctionType = SWIFT_CC(swift) void(uint8_t, uint8_t, SWIFT_CONTEXT void *);

private:
  FunctionType *__ptrauth_swift_escalation_notification_function Function;
  void *Argument;

public:
  EscalationNotificationStatusRecord(FunctionType *fn, void *arg)
      : TaskStatusRecord(TaskStatusRecordKind::EscalationNotification),
        Function(fn), Argument(arg) {
  }

  void run(JobPriority oldPriority, JobPriority newPriority) {
    Function(
      static_cast<size_t>(oldPriority),
      static_cast<size_t>(newPriority),
      Argument);
  }

  static bool classof(const TaskStatusRecord *record) {
    return record->getKind() == TaskStatusRecordKind::EscalationNotification;
  }
};

/// This record signifies that the task has an executor preference.
/// This preference may be added or removed at runtime, e.g. when multiple
/// `_withTaskExecutor { ... }` blocks are nested, they add more executor
/// preferences.
///
/// Any number of these preferences may be present at runtime, and the
/// innermost preference takes priority.
class TaskExecutorPreferenceStatusRecord : public TaskStatusRecord {
private:
  enum class Flags : uint8_t {
    /// The executor was retained during this task's creation,
    /// and therefore must be released when this task completes.
    ///
    /// The only tasks which need to manually retain/release the task executor
    /// are those which cannot structurally guarantee its lifetime. E.g. an async
    /// let does not need to do so, because it structurally always will end
    /// before/// we leave the scope in which it was defined -- and such scope
    /// must have been keeping alive the executor.
    HasRetainedExecutor = 1 << 0
  };
  OptionSet<Flags> flags;
  const TaskExecutorRef Preferred;

public:
  TaskExecutorPreferenceStatusRecord(TaskExecutorRef executor, bool retainedExecutor)
      : TaskStatusRecord(TaskStatusRecordKind::TaskExecutorPreference),
        Preferred(executor) {
    if (retainedExecutor) {
      flags = Flags::HasRetainedExecutor;
    }
  }

  TaskExecutorRef getPreferredExecutor() { return Preferred; }

  bool hasRetainedExecutor() const {
    return flags.contains(Flags::HasRetainedExecutor);
  }

  static bool classof(const TaskStatusRecord *record) {
    return record->getKind() == TaskStatusRecordKind::TaskExecutorPreference;
  }
};

// Deprecated: TaskNameStatusRecord was used Swift 6.4 (including)
// to store the initial task name of a task; However we later optimized
// this to avoid the ready-for-mutable-operations record infrastructure
// in order to facilitate quicker task name lookups by tools like lldb, spindump etc.
//
//    class TaskNameStatusRecord : public TaskStatusRecord {
//      const char *Name;
//    };

// This record is allocated for a task to record what it is dependent on before
// the task can make progress again.
class TaskDependencyStatusRecord : public TaskStatusRecord {
  // A word sized storage which references what this task is waiting for. Note
  // that this is different from the waitQueue in the future fragment of a task
  // since that denotes all the tasks which this specific task, will unblock.
  //
  // This field is only really pointing to something valid when the
  // ActiveTaskStatus specifies that the task is suspended or enqueued. It can
  // be accessed asynchronous to the task during escalation which will therefore
  // require the task status record lock for synchronization.
  //
  // When a task has TaskDependencyStatusRecord in the status record list, it
  // must be the innermost status record, barring the status record lock which
  // could be taken while this record is present.
  //
  // The type of thing we are waiting on, is specified in the enum below
  union Dependent {
    constexpr Dependent(): Task(nullptr) {}

    // This task is suspended waiting on another task. This could be an async
    // let child task or it could be another unstructured task.
    AsyncTask *Task;

    // This task is suspended waiting on its continuation to be resumed. The
    // ContinuationAsyncContext here belongs to this task itself and so we just
    // stash the pointer here (no +1 or anything taken)
    ContinuationAsyncContext *Continuation;

    // This task is suspended waiting on the child tasks in the task group to
    // return with results. Only the task which created the task group can
    // create this dependency and be suspended waiting for the group - as a
    // result, it is guaranteed to always have a reference to the task group for
    // the duration of the wait. We do not need to take an additional +1 on this
    // task group in this dependency record.
    TaskGroup *TaskGroup;

    // The task is enqueued waiting on an executor. It could be any kind of
    // executor - the generic executor, the default actor's executor, or an
    // actor with a custom executor.
    //
    // This information is helpful to know *where* a task is enqueued into
    // (potentially intrusively), so that the appropriate escalation effect
    // (which may be different for each type of executor) can happen if a task
    // is escalated while enqueued.
    SerialExecutorRef Executor;
  } DependentOn;

  // Enum specifying the type of dependency this task has
  enum DependencyKind {
    WaitingOnTask = 1,
    WaitingOnContinuation,
    WaitingOnTaskGroup,

    EnqueuedOnExecutor,
  } DependencyKind;

  // When the dependency kind is waiting on Task, this pointer contains
  // the next link in the wait queue of the Task it is waiting on. This
  // pointer should only be used through the wait queue's functions.
  AsyncTask *NextWaitingTask;

public:
  TaskDependencyStatusRecord(AsyncTask *task)
      : TaskStatusRecord(TaskStatusRecordKind::TaskDependency),
        DependencyKind(WaitingOnTask) {
    DependentOn.Task = task;
  }

  TaskDependencyStatusRecord(ContinuationAsyncContext *context)
      : TaskStatusRecord(TaskStatusRecordKind::TaskDependency),
        DependencyKind(WaitingOnContinuation) {
    DependentOn.Continuation = context;
  }

  TaskDependencyStatusRecord(TaskGroup *taskGroup)
      : TaskStatusRecord(TaskStatusRecordKind::TaskDependency),
        DependencyKind(WaitingOnTaskGroup) {
    DependentOn.TaskGroup = taskGroup;
  }

  TaskDependencyStatusRecord(SerialExecutorRef executor)
      : TaskStatusRecord(TaskStatusRecordKind::TaskDependency),
        DependencyKind(EnqueuedOnExecutor) {
    DependentOn.Executor = executor;
  }

  static bool classof(const TaskStatusRecord *record) {
    return record->getKind() == TaskStatusRecordKind::TaskDependency;
  }

  void updateDependencyToEnqueuedOn(SerialExecutorRef executor) {
    DependencyKind = EnqueuedOnExecutor;
    DependentOn.Executor = executor;
  }

  void performEscalationAction(AsyncTask *task, JobPriority oldPriority,
                               JobPriority newPriority);

  // Assumes that this record is of kind WaitingOnTask
  AsyncTask *&getNextWaitingTask();
};

/// A status record that represents a task deadline. Multiple deadlines may
/// be installed on the same task, however they may differ by clock identity.
///
/// The innermost record for any given clock identity always is the "nearest"
/// deadline, because by construction we only install deadlines which are
/// more narrow than an already existing one. I.e. a search for nearest
/// deadline can always stop at first matching record.
class TaskDeadlineStatusRecord : public TaskStatusRecord {
  /// Clock type.
  const Metadata *ClockType;

  /// Metatype of the Clock.Instant (where Clock is our ClockType).
  const Metadata *InstantType;

  /// True iff the record is the first task in this entire task hierarchy,
  /// including any parent tasks. This allows short-circuting lookups, and
  /// resetting the HasDeadline flag used for avoiding slow-path deadline
  /// lookups in `isCancelled`.
  bool IsOutermostDeadline;

  // Trailing tail-allocated storage (uninitialized in this class body):
  //   [pad to ClockType.vw_alignment()]
  //   C bytes                            <- getClockStorage()
  //   [pad to InstantType.vw_alignment()]
  //   C.Instant bytes                    <- getInstantStorage()

public:
  TaskDeadlineStatusRecord(const Metadata *clockType,
                           const Metadata *instantType,
                           bool isOutermostDeadline)
      : TaskStatusRecord(TaskStatusRecordKind::Deadline),
        ClockType(clockType), InstantType(instantType),
        IsOutermostDeadline(isOutermostDeadline) {}

  const Metadata *getClockType() const { return ClockType; }
  const Metadata *getInstantType() const { return InstantType; }
  bool isOutermostDeadline() const { return IsOutermostDeadline; }
  void setOutermostDeadline(bool value) { IsOutermostDeadline = value; }

  /// Offset from the base of the record where the clock value lives.
  static size_t clockOffset(const Metadata *clockType) {
    size_t align = clockType->vw_alignment();
    return (sizeof(TaskDeadlineStatusRecord) + align - 1) & ~(align - 1);
  }

  /// Offset from the base of the record where the deadline instant lives.
  static size_t instantOffset(const Metadata *clockType,
                              const Metadata *instantType) {
    size_t off = clockOffset(clockType) + clockType->vw_size();
    size_t align = instantType->vw_alignment();
    return (off + align - 1) & ~(align - 1);
  }

  /// Total allocation size for a record with the given generic types.
  static size_t recordSize(const Metadata *clockType,
                           const Metadata *instantType) {
    return instantOffset(clockType, instantType) + instantType->vw_size();
  }

  OpaqueValue *getClockStorage() {
    return reinterpret_cast<OpaqueValue *>(
        reinterpret_cast<char *>(this) + clockOffset(ClockType));
  }
  OpaqueValue *getInstantStorage() {
    return reinterpret_cast<OpaqueValue *>(
        reinterpret_cast<char *>(this) + instantOffset(ClockType, InstantType));
  }

  static bool classof(const TaskStatusRecord *record) {
    return record->getKind() == TaskStatusRecordKind::Deadline;
  }
};

/// A status record which represents a scoped cancellation domain that is
/// independent of whole-task cancellation. Cancelling the scope does not set
/// the task's own cancellation flag; only handlers registered _inside_ the
/// scope's dynamic extent are fired.
class TaskCancellationScopeRecord : public TaskStatusRecord {
  /// The task that installed this scope.
  AsyncTask *OwningTask;

  /// Packed state: bit 0 is the cancelled flag; the remaining bits hold
  /// the cancellation reason (matching the `size_t reason` ABI used by
  /// `swift_task_cancelWithReason`). Written once at cancellation time
  /// as a single atomic store so readers observe a consistent pair.
  std::atomic<uintptr_t> State{0};

  static constexpr uintptr_t CancelledBit = 1;

public:
  explicit TaskCancellationScopeRecord(AsyncTask *owningTask)
      : TaskStatusRecord(TaskStatusRecordKind::TaskCancellationScope),
        OwningTask(owningTask) {}

  AsyncTask *getOwningTask() const { return OwningTask; }
  bool isCancelled() const {
    return (State.load(std::memory_order_relaxed) & CancelledBit) != 0;
  }
  size_t getReason() const {
    return State.load(std::memory_order_relaxed) >> 1;
  }
  void cancel(size_t reason = 0) {
    State.store((static_cast<uintptr_t>(reason) << 1) | CancelledBit,
                std::memory_order_relaxed);
  }

  static bool classof(const TaskStatusRecord *record) {
    return record->getKind() == TaskStatusRecordKind::TaskCancellationScope;
  }
};

/// A status record representing an active `withTaskCancellationShield` block.
/// Its position in the LIFO chain relative to any `TaskCancellationScopeRecord`
/// determines whether a scope's cancellation is masked at a given call site.
class TaskCancellationShieldRecord : public TaskStatusRecord {
public:
  TaskCancellationShieldRecord()
      : TaskStatusRecord(TaskStatusRecordKind::CancellationShield) {}

  static bool classof(const TaskStatusRecord *record) {
    return record->getKind() == TaskStatusRecordKind::CancellationShield;
  }
};

} // end namespace swift

#endif
