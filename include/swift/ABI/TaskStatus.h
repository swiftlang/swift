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

#include "swift/ABI/Task.h"
#include "swift/ABI/MetadataValues.h"

namespace swift {

/// The abstract base class for all status records.
///
/// TaskStatusRecords are typically allocated on the stack (possibly
/// in the task context), partially initialized, and then atomically
/// added to the task with `swift_task_addTaskStatusRecord`.  While
/// registered with the task, a status record should only be 
/// modified in ways that respect the possibility of asynchronous
/// access by a cancelling thread.  In particular, the chain of
/// status records must not be disturbed.  When the task leaves
/// the scope that requires the status record, the record can
/// be unregistered from the task with `swift_task_removeStatusRecord`,
/// at which point the memory can be returned to the system.
class TaskStatusRecord {
public:
  TaskStatusRecordFlags Flags;
  TaskStatusRecord *Parent;

  TaskStatusRecord(TaskStatusRecordKind kind,
                   TaskStatusRecord *parent = nullptr)
      : Flags(kind) {
    resetParent(parent);
  }

  TaskStatusRecord(const TaskStatusRecord &) = delete;
  TaskStatusRecord &operator=(const TaskStatusRecord &) = delete;

  TaskStatusRecordKind getKind() const {
    return Flags.getKind();
  }

  TaskStatusRecord *getParent() const {
    return Parent;
  }

  /// Change the parent of this unregistered status record to the
  /// given record.
  ///
  /// This should be used when the record has been previously initialized
  /// without knowing what the true parent is.  If we decide to cache
  /// important information (e.g. the earliest timeout) in the innermost
  /// status record, this is the method that should fill that in
  /// from the parent.
  void resetParent(TaskStatusRecord *newParent) {
    Parent = newParent;
    // TODO: cache
  }

  /// Splice a record out of the status-record chain.
  ///
  /// Unlike resetParent, this assumes that it's just removing one or
  /// more records from the chain and that there's no need to do any
  /// extra cache manipulation.
  void spliceParent(TaskStatusRecord *newParent) {
    Parent = newParent;
  }
};

/// A deadline for the task.  If this is reached, the task will be
/// automatically cancelled.  The deadline can also be queried and used
/// in other ways.
struct TaskDeadline {
  // FIXME: I don't really know what this should look like right now.
  // It's probably target-specific.
  uint64_t Value;

  bool operator==(const TaskDeadline &other) const {
    return Value == other.Value;
  }
  bool operator<(const TaskDeadline &other) const {
    return Value < other.Value;
  }
};

/// A status record which states that there's an active deadline
/// within the task.
class DeadlineStatusRecord : public TaskStatusRecord {
  TaskDeadline Deadline;
public:
  DeadlineStatusRecord(TaskDeadline deadline)
    : TaskStatusRecord(TaskStatusRecordKind::Deadline),
      Deadline(deadline) {}

  TaskDeadline getDeadline() const {
    return Deadline;
  }

  static bool classof(const TaskStatusRecord *record) {
    return record->getKind() == TaskStatusRecordKind::Deadline;
  }
};

/// A status record which states that a task has one or
/// more active child tasks.
class ChildTaskStatusRecord : public TaskStatusRecord {
  AsyncTask *FirstChild;

public:
  ChildTaskStatusRecord(AsyncTask *child)
    : TaskStatusRecord(TaskStatusRecordKind::ChildTask),
      FirstChild(child) {}

  ChildTaskStatusRecord(AsyncTask *child, TaskStatusRecordKind kind)
    : TaskStatusRecord(kind),
      FirstChild(child) {
    assert(kind == TaskStatusRecordKind::ChildTask);
    assert(!child->hasGroupChildFragment() &&
      "Group child tasks must be tracked in their respective "
      "TaskGroupTaskStatusRecord, and not as independent ChildTaskStatusRecord "
      "records.");
  }

  /// Return the first child linked by this record.  This may be null;
  /// if not, it (and all of its successors) are guaranteed to satisfy
  /// `isChildTask()`.
  AsyncTask *getFirstChild() const {
    return FirstChild;
  }

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
/// A record always is a specific `TaskGroupImpl`.
///
/// The child tasks are stored as an invasive single-linked list, starting
/// from `FirstChild` and continuing through the `NextChild` pointers of all
/// the linked children.
///
/// All children of the specific `Group` are stored "by" this record,
/// so that they may be cancelled when this task becomes cancelled.
///
/// When the group exits, it may simply remove this single record from the task
/// running it. As it has guaranteed that the tasks have already completed.
///
/// Group child tasks DO NOT have their own `ChildTaskStatusRecord` entries,
/// and are only tracked by their respective `TaskGroupTaskStatusRecord`.
class TaskGroupTaskStatusRecord : public TaskStatusRecord {
  AsyncTask *FirstChild;
public:
  TaskGroupTaskStatusRecord()
    : TaskStatusRecord(TaskStatusRecordKind::TaskGroup),
      FirstChild(nullptr) {}

  TaskGroupTaskStatusRecord(AsyncTask *child)
    : TaskStatusRecord(TaskStatusRecordKind::TaskGroup),
      FirstChild(child) {}

  TaskGroup* getGroup() {
    return reinterpret_cast<TaskGroup *>(this);
  }

  /// Return the first child linked by this record.  This may be null;
  /// if not, it (and all of its successors) are guaranteed to satisfy
  /// `isChildTask()`.
  AsyncTask *getFirstChild() const {
    return FirstChild;
  }

  /// Attach the passed in `child` task to this group.
  void attachChild(AsyncTask *child) {
    assert(child->groupChildFragment());
    assert(child->hasGroupChildFragment());
    assert(child->groupChildFragment()->getGroup() == getGroup());

    if (!FirstChild) {
      // This is the first child we ever attach, so store it as FirstChild.
      FirstChild = child;
      return;
    }

    // We need to traverse the siblings to find the last one and add the child there.
    // FIXME: just set prepend to the current head, no need to traverse.

    auto cur = FirstChild;
    while (cur) {
      // no need to check hasChildFragment, all tasks we store here have them.
      auto fragment = cur->childFragment();
      if (auto next = fragment->getNextChild()) {
        cur = next;
      } else {
        // we're done searching and `cur` is the last
        break;
      }
    }

    cur->childFragment()->setNextChild(child);
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
/// code may call `swift_task_removeStatusRecord` and freely
/// assume after it returns that this function will not be
/// subsequently used.
class CancellationNotificationStatusRecord : public TaskStatusRecord {
public:
  using FunctionType = SWIFT_CC(swift) void (SWIFT_CONTEXT void *);

private:
  FunctionType * __ptrauth_swift_cancellation_notification_function Function;
  void *Argument;

public:
  CancellationNotificationStatusRecord(FunctionType *fn, void *arg)
    : TaskStatusRecord(TaskStatusRecordKind::CancellationNotification),
      Function(fn), Argument(arg) {}

  void run() {
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
/// code may call `swift_task_removeStatusRecord` and freely
/// assume after it returns that this function will not be
/// subsequently used.
class EscalationNotificationStatusRecord : public TaskStatusRecord {
public:
  using FunctionType = void (void *, JobPriority);

private:
  FunctionType * __ptrauth_swift_escalation_notification_function Function;
  void *Argument;

public:
  EscalationNotificationStatusRecord(FunctionType *fn, void *arg)
    : TaskStatusRecord(TaskStatusRecordKind::EscalationNotification),
      Function(fn), Argument(arg) {}

  void run(JobPriority newPriority) {
    Function(Argument, newPriority);
  }

  static bool classof(const TaskStatusRecord *record) {
    return record->getKind() == TaskStatusRecordKind::EscalationNotification;
  }
};

} // end namespace swift

#endif
