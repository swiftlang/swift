//===--- TaskTrackingPrivate.h --------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_STDLIB_CONCURRENCY_TASKTRACKINGPRIVATE_H
#define SWIFT_STDLIB_CONCURRENCY_TASKTRACKINGPRIVATE_H

#include "VoucherSupport.h"
#include "swift/ABI/Actor.h"
#include "swift/ABI/Task.h"
#include "swift/Threading/ThreadLocalStorage.h"

namespace swift {
namespace tasktracking {

/// An extremely silly class which exists to make pointer
/// default-initialization constexpr.
template <class T>
struct Pointer {
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
  static SWIFT_THREAD_LOCAL_TYPE(Pointer<ExecutorTrackingInfo>,
                                 tls_key::concurrency_executor_tracking_info)
      ActiveInfoInThread;

  /// The active executor.
  SerialExecutorRef ActiveExecutor = SerialExecutorRef::generic();

  /// The current task executor, if present, otherwise `undefined`.
  /// The task executor should be used to execute code when the active executor
  /// is `generic`.
  TaskExecutorRef TaskExecutor = TaskExecutorRef::undefined();

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
  /// leave() must be called before the object goes out of scope.
  void enterAndShadow(SerialExecutorRef currentExecutor,
                      TaskExecutorRef taskExecutor) {
    ActiveExecutor = currentExecutor;
    TaskExecutor = taskExecutor;
    SavedInfo = ActiveInfoInThread.get();
    ActiveInfoInThread.set(this);
  }

  void swapToJob(Job *job) { voucherManager.swapToJob(job); }

  void restoreVoucher(AsyncTask *task) { voucherManager.restoreVoucher(task); }

  SerialExecutorRef getActiveExecutor() const { return ActiveExecutor; }
  void setActiveExecutor(SerialExecutorRef newExecutor) {
    ActiveExecutor = newExecutor;
  }

  TaskExecutorRef getTaskExecutor() const { return TaskExecutor; }

  void setTaskExecutor(TaskExecutorRef newExecutor) {
    TaskExecutor = newExecutor;
  }

  bool allowsSwitching() const { return AllowsSwitching; }

  /// Disallow switching in this tracking context.  This should only
  /// be set on a new tracking info, before any jobs are run in it.
  void disallowSwitching() { AllowsSwitching = false; }

  static ExecutorTrackingInfo *current() { return ActiveInfoInThread.get(); }

  void leave() {
    voucherManager.leave();
    ActiveInfoInThread.set(SavedInfo);
  }
};

} // namespace tasktracking
} // namespace swift

#endif // SWIFT_STDLIB_CONCURRENCY_TASKTRACKINGPRIVATE_H
