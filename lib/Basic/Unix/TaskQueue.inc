//===--- Unix/TaskQueue.inc - Unix-specific TaskQueue -----------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/Basic/TaskQueue.h"

#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/DenseMap.h"

#include <string>
#include <cerrno>

#if HAVE_POSIX_SPAWN
#include <spawn.h>
#endif

#if HAVE_UNISTD_H
#include <unistd.h>
#endif

#if !defined(__APPLE__)
extern char **environ;
#else
#include <crt_externs.h> // for _NSGetEnviron
#endif

namespace swift {
namespace sys {

class Task {
  /// The path to the executable which this Task will execute.
  const char *ExecPath;

  /// Any arguments which should be passed during execution.
  ArrayRef<const char *> Args;

  /// The environment which will be used during execution. If empty, uses
  /// this process's environment.
  ArrayRef<const char *> Env;

  /// Context which should be associated with this task.
  void *Context;

  /// A pipe for reading output from the child process.
  int Pipe;

  /// The current state of the Task.
  enum {
    Preparing,
    Executing,
    Finished
  } State;

  /// Once the Task has finished, this contains the buffered output of the Task.
  std::string Output;

public:
  Task(const char *ExecPath, ArrayRef<const char *> Args,
       ArrayRef<const char *> Env, void *Context)
      : ExecPath(ExecPath), Args(Args), Env(Env), Context(Context),
        State(Preparing) {
    assert((Env.empty() || Env.back() == nullptr) &&
           "Env must either be empty or null-terminated!");
  }

  const char *getExecPath() const { return ExecPath; }
  ArrayRef<const char *> getArgs() const { return Args; }
  StringRef getOutput() const { return Output; }
  void *getContext() const { return Context; }

  /// \brief Begins execution of this Task.
  /// \returns the Task's pid on success, or 0 on failure
  pid_t execute();

  /// \brief Performs any post-execution work for this Task, such as reading
  /// piped output and closing the pipe.
  void finishExecution();
};

} // end namespace sys
} // end namespace swift

pid_t Task::execute() {
  assert(State < Executing && "This Task cannot be executed twice!");
  State = Executing;

  // Construct argv.
  SmallVector<const char *, 128> Argv;
  Argv.push_back(ExecPath);
  Argv.append(Args.begin(), Args.end());
  Argv.push_back(0); // argv is expected to be null-terminated.

  // Set up the pipe.
  int FullPipe[2];
  pipe(FullPipe);
  Pipe = FullPipe[0];

  pid_t Pid;

  // Get the environment to pass down to the subtask.
  const char *const *envp = Env.empty() ? nullptr : Env.data();
  if (!envp) {
#if __APPLE__
    envp = *_NSGetEnviron();
#else
    envp = environ;
#endif
  }

  const char **argvp = Argv.data();

#if HAVE_POSIX_SPAWN
  posix_spawn_file_actions_t FileActions;
  posix_spawn_file_actions_init(&FileActions);

  posix_spawn_file_actions_adddup2(&FileActions, FullPipe[1], STDOUT_FILENO);
  posix_spawn_file_actions_adddup2(&FileActions, STDOUT_FILENO, STDERR_FILENO);
  posix_spawn_file_actions_addclose(&FileActions, FullPipe[0]);

  // Spawn the subtask.
  int spawnErr = posix_spawn(&Pid, ExecPath, &FileActions, nullptr,
                             const_cast<char **>(argvp),
                             const_cast<char **>(envp));

  posix_spawn_file_actions_destroy(&FileActions);
  close(FullPipe[1]);

  if (spawnErr != 0 || Pid == 0) {
    close(FullPipe[0]);
    State = Finished;
    return 0;
  }
#else
  Pid = fork();
  switch (Pid) {
  case -1: {
    close(FullPipe[0]);
    State = Finished;
    Pid = 0;
    break;
  }
  case 0: {
    // Child process: Execute the program.
    dup2(FullPipe[1], STDOUT_FILENO);
    dup2(STDOUT_FILENO, STDERR_FILENO);
    close(FullPipe[0]);
    execve(ExecPath, const_cast<char **>(argvp), const_cast<char **>(envp));

    // If the execve() failed, we should exit. Follow Unix protocol and
    // return 127 if the executable was not found, and 126 otherwise.
    // Use _exit rather than exit so that atexit functions and static
    // object destructors cloned from the parent process aren't
    // redundantly run, and so that any data buffered in stdio buffers
    // cloned from the parent aren't redundantly written out.
    _exit(errno == ENOENT ? 127 : 126);
  }
  default:
    // Parent process: Break out of the switch to do our processing.
    break;
  }

  close(FullPipe[1]);
#endif
  
  return Pid;
}

void Task::finishExecution() {
  assert(State == Executing &&
         "This Task must be executing to finish execution!");

  State = Finished;

  // Read the output of the command, so we can use it later.
  char outputBuffer[1024];
  ssize_t readBytes = 0;
  while ((readBytes = read(Pipe, outputBuffer, sizeof(outputBuffer))) != 0) {
    Output.append(outputBuffer, readBytes);
  }

  close(Pipe);
}

bool TaskQueue::supportsBufferingOutput() {
  // The Unix implementation supports buffering output.
  return true;
}

bool TaskQueue::supportsParallelExecution() {
  // The Unix implementation supports parallel execution.
  return true;
}

void TaskQueue::addTask(const char *ExecPath, ArrayRef<const char *> Args,
                        ArrayRef<const char *> Env, void *Context) {
  std::unique_ptr<Task> T(new Task(ExecPath, Args, Env, Context));
  QueuedTasks.push(std::move(T));
}

bool TaskQueue::execute(TaskBeganCallback Began,
                        TaskFinishedCallback Finished) {
  llvm::DenseMap<pid_t, std::unique_ptr<Task>> ExecutingTasks;

  bool SubtaskFailed = false;

  unsigned MaxNumberOfParallelTasks = NumberOfParallelTasks;
  // TODO: add support for choosing a better default value for
  // MaxNumberOfParallelTasks if NumberOfParallelTasks is 0. (Optimally, this
  // should choose a value > 1 tailored to the current system.)
  if (MaxNumberOfParallelTasks == 0)
    MaxNumberOfParallelTasks = 1;

  while ((!QueuedTasks.empty() && !SubtaskFailed) ||
         !ExecutingTasks.empty()) {
    // Enqueue additional tasks, if we have additional tasks, we aren't
    // already at the parallel limit, and no earlier subtasks have failed.
    while (!SubtaskFailed && !QueuedTasks.empty() &&
           ExecutingTasks.size() < MaxNumberOfParallelTasks) {
      std::unique_ptr<Task> T(QueuedTasks.front().release());
      QueuedTasks.pop();
      pid_t Pid = T->execute();
      if (Pid == 0) {
        return true;
      }
      if (Began) {
        Began(Pid, T->getContext());
      }
      ExecutingTasks[Pid] = std::move(T);
    }

    // Wait for the next subtask to finish execution.
    int Status;
    pid_t Pid = waitpid(-1, &Status, 0);
    assert(Pid != 0 && "We do not pass WNOHANG, so we should always get a pid");

    if (Pid < 0) {
      int err = errno;
      switch (err) {
      case ECHILD:
        // There was no child to wait for, which is an error.
        break;
      case EINVAL:
        // We passed invalid arguments to waitpid, which is an error.
        break;
      case EINTR:
        // The call to waitpid was interrupted; continue so we wait again.
        continue;
      }
      return true;
    }

    auto iter = ExecutingTasks.find(Pid);
    if (iter != ExecutingTasks.end()) {
      Task &T = *iter->second;

      T.finishExecution();

      int Result = 0;
      if (WIFEXITED(Status)) {
        Result = WEXITSTATUS(Status);

        if (Finished &&
            Finished(Pid, Result, T.getOutput(), T.getContext()) ==
            TaskFinishedResponse::StopExecution) {
          // If we have a TaskFinishedCallback, only set SubtaskFailed to true
          // if the callback returns StopExecution.
          SubtaskFailed = true;
        } else if (Result != 0) {
          // Since we don't have a TaskFinishedCallback, treat a subtask which
          // returned a nonzero exit code as having failed.
          SubtaskFailed = true;
        }
      }

      // We can't reuse the existing iterator, since ExecutingTasks may change
      // due to the call to Finished. Instead, erase the Pid directly.
      ExecutingTasks.erase(Pid);
    }
    
  }

  return SubtaskFailed;
}
