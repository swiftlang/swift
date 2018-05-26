//===--- TaskQueue.inc - Default serial TaskQueue ---------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
///
/// \file
/// \brief This file contains a platform-agnostic implementation of TaskQueue
/// using the functions from llvm/Support/Program.h.
///
/// \note The default implementation of TaskQueue does not support parallel
/// execution, nor does it support output buffering. As a result,
/// platform-specific implementations should be preferred.
///
//===----------------------------------------------------------------------===//

#include "swift/Basic/TaskQueue.h"

#include "swift/Basic/LLVM.h"

using namespace llvm::sys;

namespace swift {
namespace sys {

class Task {
public:
  /// The path to the executable which this Task will execute.
  const char *ExecPath;

  /// Any arguments which should be passed during execution.
  ArrayRef<const char *> Args;

  /// The environment which should be used during execution. If empty,
  /// the current process's environment will be used instead.
  ArrayRef<const char *> Env;

  /// Context associated with this Task.
  void *Context;

  Task(const char *ExecPath, ArrayRef<const char *> Args,
       ArrayRef<const char *> Env = llvm::None, void *Context = nullptr)
      : ExecPath(ExecPath), Args(Args), Env(Env), Context(Context) {}
};

} // end namespace sys
} // end namespace swift

bool TaskQueue::supportsBufferingOutput() {
  // The default implementation does not support buffering output.
  return false;
}

bool TaskQueue::supportsParallelExecution() {
  // The default implementation does not support parallel execution.
  return false;
}

unsigned TaskQueue::getNumberOfParallelTasks() const {
  // The default implementation does not support parallel execution.
  return 1;
}

void TaskQueue::addTask(const char *ExecPath, ArrayRef<const char *> Args,
                        ArrayRef<const char *> Env, void *Context,
                        bool SeparateErrors) {
  // This implementation of TaskQueue ignores SeparateErrors.
  // We need to reference SeparateErrors to avoid warnings, though.
  (void)SeparateErrors;
  std::unique_ptr<Task> T(new Task(ExecPath, Args, Env, Context));
  QueuedTasks.push(std::move(T));
}

bool TaskQueue::execute(TaskBeganCallback Began, TaskFinishedCallback Finished,
                        TaskSignalledCallback Signalled) {
  bool ContinueExecution = true;

  // This implementation of TaskQueue doesn't support parallel execution.
  // We need to reference NumberOfParallelTasks to avoid warnings, though.
  (void)NumberOfParallelTasks;

  while (!QueuedTasks.empty() && ContinueExecution) {
    std::unique_ptr<Task> T(QueuedTasks.front().release());
    QueuedTasks.pop();

    SmallVector<const char *, 128> Argv;
    Argv.push_back(T->ExecPath);
    Argv.append(T->Args.begin(), T->Args.end());
    Argv.push_back(0);

    const char *const *envp = T->Env.empty() ? nullptr : T->Env.data();

    bool ExecutionFailed = false;
    ProcessInfo PI = ExecuteNoWait(T->ExecPath, Argv.data(),
                                   (const char **)envp,
                                   /*redirects*/None, /*memoryLimit*/0,
                                   /*ErrMsg*/nullptr, &ExecutionFailed);
    if (ExecutionFailed) {
      return true;
    }

    if (Began) {
      Began(PI.Pid, T->Context);
    }

    std::string ErrMsg;
    PI = Wait(PI, 0, true, &ErrMsg);
    int ReturnCode = PI.ReturnCode;
    if (ReturnCode == -2) {
      // Wait() returning a return code of -2 indicates the process received
      // a signal during execution.
      if (Signalled) {
        TaskFinishedResponse Response =
            Signalled(PI.Pid, ErrMsg, StringRef(), StringRef(), T->Context, None, TaskProcessInformation(PI.Pid));
        ContinueExecution = Response != TaskFinishedResponse::StopExecution;
      } else {
        // If we don't have a Signalled callback, unconditionally stop.
        ContinueExecution = false;
      }
    } else {
      // Wait() returned a normal return code, so just indicate that the task
      // finished.
      if (Finished) {
        TaskFinishedResponse Response = Finished(PI.Pid, PI.ReturnCode,
        StringRef(), StringRef(), TaskProcessInformation(PI.Pid), T->Context);
        ContinueExecution = Response != TaskFinishedResponse::StopExecution;
      } else if (PI.ReturnCode != 0) {
        ContinueExecution = false;
      }
    }
  }

  return !ContinueExecution;
}
