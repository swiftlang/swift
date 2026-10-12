//===--- ExecutorTracking.h - Concurrency library internal ------*- C++ -*-===//
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

#ifndef SWIFT_CONCURRENCY_EXECUTOR_TRACKING_H
#define SWIFT_CONCURRENCY_EXECUTOR_TRACKING_H

#include "VoucherSupport.h"
#include "swift/ABI/Task.h"
#include "swift/Threading/ThreadLocalStorage.h"

namespace swift {

/// The task executor of an execution context: none, the default executor, or a
/// task executor the job was run with explicitly. See
/// docs/ConcurrencyRuntimeExecution.md.
///
/// The default executor is its own case, so that running a job doesn't need
/// to look up the default executor. An explicit task executor can still be the
/// default executor, for example a custom default executor that runs its jobs
/// with runSynchronously(on:) and itself.
class ContextTaskExecutor {
  TaskExecutorRef Explicit = TaskExecutorRef::undefined();
  bool IsDefault = false;

  ContextTaskExecutor(TaskExecutorRef explicitTaskExecutor, bool isDefault)
      : Explicit(explicitTaskExecutor), IsDefault(isDefault) {}

public:
  /// No task executor.
  ContextTaskExecutor() = default;

  /// The task executor of the context a job is run in with `serialExecutor`
  /// and `taskExecutor`, which is undefined if the executor passed none. A job
  /// run with no serial executor and no task executor runs on a thread of the
  /// default executor.
  static ContextTaskExecutor forJob(SerialExecutorRef serialExecutor,
                                    TaskExecutorRef taskExecutor) {
    if (taskExecutor.isDefined())
      return ContextTaskExecutor(taskExecutor, false);
    return ContextTaskExecutor(TaskExecutorRef::undefined(),
                               serialExecutor.isGeneric());
  }

  /// Whether this is the default executor as its own case.
  bool isDefault() const { return IsDefault; }

  /// The task executor the job was run with explicitly, or undefined for none
  /// and for the default executor as its own case.
  TaskExecutorRef getExplicit() const { return Explicit; }
};

/// The per-thread record of the execution context a job runs in. See
/// docs/ConcurrencyRuntimeExecution.md.
///
/// A record holds the base context, which is the context the job was started
/// in, and the current serial executor. The runtime changes the current serial
/// executor during a job when it locks or unlocks a default actor on top of the
/// base. The task executor never changes during a job, so the current and the
/// base context share it.
class ExecutorTrackingInfo {
  /// The current record of this thread, if any.
  static SWIFT_THREAD_LOCAL_TYPE(TLSPointer<ExecutorTrackingInfo>,
                                 tls_key::concurrency_executor_tracking_info)
      CurrentInThread;

  /// The serial executor of the current context.
  SerialExecutorRef CurrentSerialExecutor = SerialExecutorRef::generic();

  /// The serial executor of the base context.
  SerialExecutorRef BaseSerialExecutor = SerialExecutorRef::generic();

  /// The task executor of the context.
  ContextTaskExecutor TaskExecutor;

  VoucherManager voucherManager;

  /// The record that was current before this one.
  ExecutorTrackingInfo *SavedInfo;

public:
  ExecutorTrackingInfo() = default;

  ExecutorTrackingInfo(const ExecutorTrackingInfo &) = delete;
  ExecutorTrackingInfo &operator=(const ExecutorTrackingInfo &) = delete;

  /// Make this the current record, with (serialExecutor, taskExecutor) as its
  /// current and base context. leave() must be called before the object goes
  /// out of scope.
  void enter(SerialExecutorRef serialExecutor,
             ContextTaskExecutor taskExecutor) {
    CurrentSerialExecutor = serialExecutor;
    BaseSerialExecutor = serialExecutor;
    TaskExecutor = taskExecutor;
    SavedInfo = CurrentInThread.get();
    CurrentInThread.set(this);
  }

  /// Make this the current record for a default actor that was locked on top
  /// of the current context. The record keeps the base context and the task
  /// executor of the record that was current before. Without such a record,
  /// the base context is (none, none). leave() must be called before the
  /// object goes out of scope.
  void enterLockedDefaultActor(SerialExecutorRef defaultActor) {
    assert(defaultActor.isDefaultActor());
    SavedInfo = CurrentInThread.get();
    CurrentSerialExecutor = defaultActor;
    BaseSerialExecutor =
        SavedInfo ? SavedInfo->BaseSerialExecutor : SerialExecutorRef::generic();
    TaskExecutor = SavedInfo ? SavedInfo->TaskExecutor : ContextTaskExecutor();
    CurrentInThread.set(this);
  }

  void swapToJob(Job *job) { voucherManager.swapToJob(job); }

  void restoreVoucher(AsyncTask *task) { voucherManager.restoreVoucher(task); }

  SerialExecutorRef getCurrentSerialExecutor() const {
    return CurrentSerialExecutor;
  }
  void setCurrentSerialExecutor(SerialExecutorRef serialExecutor) {
    CurrentSerialExecutor = serialExecutor;
  }

  SerialExecutorRef getBaseSerialExecutor() const { return BaseSerialExecutor; }

  ContextTaskExecutor getTaskExecutor() const { return TaskExecutor; }

  static ExecutorTrackingInfo *current() { return CurrentInThread.get(); }

  void leave() {
    voucherManager.leave();
    CurrentInThread.set(SavedInfo);
  }
};

} // namespace swift

#endif // SWIFT_CONCURRENCY_EXECUTOR_TRACKING_H
