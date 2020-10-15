//===--- Concurrency.h - Runtime interface for concurrency ------*- C++ -*-===//
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
// The runtime interface for concurrency.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_RUNTIME_CONCURRENCY_H
#define SWIFT_RUNTIME_CONCURRENCY_H

#include "swift/ABI/TaskStatus.h"

namespace swift {

/// Cancel a task and all of its child tasks.
///
/// This can be called from any thread.
///
/// This has no effect if the task is already cancelled.
SWIFT_EXPORT_FROM(swift_Concurrency) SWIFT_CC(swift)
void swift_task_cancel(AsyncTask *task);

/// Escalate the priority of a task and all of its child tasks.
///
/// This can be called from any thread.
///
/// This has no effect if the task already has at least the given priority.
/// Returns the priority of the task.
SWIFT_EXPORT_FROM(swift_Concurrency) SWIFT_CC(swift)
JobPriority
swift_task_escalate(AsyncTask *task, JobPriority newPriority);

/// Add a status record to a task.  The record should not be
/// modified while it is registered with a task.
///
/// This must be called synchronously with the task.
///
/// If the task is already cancelled, returns `false` but still adds
/// the status record.
SWIFT_EXPORT_FROM(swift_Concurrency) SWIFT_CC(swift)
bool swift_task_addStatusRecord(AsyncTask *task,
                                TaskStatusRecord *record);

/// Add a status record to a task if the task has not already
/// been cancelled.   The record should not be modified while it is
/// registered with a task.
///
/// This must be called synchronously with the task.
///
/// If the task is already cancelled, returns `false` and does not
/// add the status record.
SWIFT_EXPORT_FROM(swift_Concurrency) SWIFT_CC(swift)
bool swift_task_tryAddStatusRecord(AsyncTask *task,
                                   TaskStatusRecord *record);

/// Remove a status record from a task.  After this call returns,
/// the record's memory can be freely modified or deallocated.
///
/// This must be called synchronously with the task.  The record must
/// be registered with the task or else this may crash.
///
/// The given record need not be the last record added to
/// the task, but the operation may be less efficient if not.
///s
/// Returns false if the task has been cancelled.
SWIFT_EXPORT_FROM(swift_Concurrency) SWIFT_CC(swift)
bool swift_task_removeStatusRecord(AsyncTask *task,
                                   TaskStatusRecord *record);

/// This should have the same representation as an enum like this:
///    enum NearestTaskDeadline {
///      case none
///      case alreadyCancelled
///      case active(TaskDeadline)
///    }
/// TODO: decide what this interface should really be.
struct NearestTaskDeadline {
  enum Kind : uint8_t {
    None,
    AlreadyCancelled,
    Active
  };

  TaskDeadline Value;
  Kind ValueKind;
};

/// Returns the nearest deadline that's been registered with this task.
///
/// This must be called synchronously with the task.
SWIFT_EXPORT_FROM(swift_Concurrency) SWIFT_CC(swift)
NearestTaskDeadline
swift_task_getNearestDeadline(AsyncTask *task);

}

#endif
