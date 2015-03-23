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
#include "llvm/ADT/DenseSet.h"
#include "llvm/Support/ErrorHandling.h"

#include <string>
#include <cerrno>

#if HAVE_POSIX_SPAWN
#include <spawn.h>
#endif

#if HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <poll.h>
#include <sys/types.h>
#include <sys/wait.h>

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

  /// The pid of this Task when executing.
  pid_t Pid;

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
        Pid(-1), Pipe(-1), State(Preparing) {
    assert((Env.empty() || Env.back() == nullptr) &&
           "Env must either be empty or null-terminated!");
  }

  const char *getExecPath() const { return ExecPath; }
  ArrayRef<const char *> getArgs() const { return Args; }
  StringRef getOutput() const { return Output; }
  void *getContext() const { return Context; }
  pid_t getPid() const { return Pid; }
  int getPipe() const { return Pipe; }

  /// \brief Begins execution of this Task.
  /// \returns true on error, false on success
  bool execute();

  /// \brief Reads data from the pipe, if any is available.
  /// \returns true on error, false on success
  bool readFromPipe();

  /// \brief Performs any post-execution work for this Task, such as reading
  /// piped output and closing the pipe.
  void finishExecution();
};

} // end namespace sys
} // end namespace swift

bool Task::execute() {
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
    return true;
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

  if (Pid == 0)
    return true;
#endif
  
  return false;
}

bool Task::readFromPipe() {
  char outputBuffer[1024];
  ssize_t readBytes = 0;
  while ((readBytes = read(Pipe, outputBuffer, sizeof(outputBuffer))) != 0) {
    if (readBytes < 0) {
      if (errno == EINTR)
        // read() was interrupted, so try again.
        continue;
      return true;
    }

    Output.append(outputBuffer, readBytes);
  }

  return false;
}

void Task::finishExecution() {
  assert(State == Executing &&
         "This Task must be executing to finish execution!");

  State = Finished;

  // Read the output of the command, so we can use it later.
  readFromPipe();

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

unsigned TaskQueue::getNumberOfParallelTasks() const {
  // TODO: add support for choosing a better default value for
  // MaxNumberOfParallelTasks if NumberOfParallelTasks is 0. (Optimally, this
  // should choose a value > 1 tailored to the current system.)
  return NumberOfParallelTasks > 0 ? NumberOfParallelTasks : 1;
}

void TaskQueue::addTask(const char *ExecPath, ArrayRef<const char *> Args,
                        ArrayRef<const char *> Env, void *Context) {
  std::unique_ptr<Task> T(new Task(ExecPath, Args, Env, Context));
  QueuedTasks.push(std::move(T));
}

bool TaskQueue::execute(TaskBeganCallback Began, TaskFinishedCallback Finished,
                        TaskSignalledCallback Signalled) {
  typedef llvm::DenseMap<pid_t, std::unique_ptr<Task>> PidToTaskMap;

  // Stores the current executing Tasks, organized by pid.
  PidToTaskMap ExecutingTasks;

  // Maintains the current fds we're checking with poll.
  std::vector<struct pollfd> PollFds;

  bool SubtaskFailed = false;

  unsigned MaxNumberOfParallelTasks = getNumberOfParallelTasks();

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
      if (T->execute())
        return true;

      pid_t Pid = T->getPid();

      if (Began) {
        Began(Pid, T->getContext());
      }

      PollFds.push_back({ T->getPipe(), POLLIN | POLLPRI | POLLHUP, 0 });
      ExecutingTasks[Pid] = std::move(T);
    }

    assert(PollFds.size() > 0 &&
           "We should only call poll() if we have fds to watch!");
    int ReadyFdCount = poll(PollFds.data(), PollFds.size(), -1);
    if (ReadyFdCount == -1) {
      // Recover from error, if possible.
      if (errno == EAGAIN || errno == EINTR)
        continue;
      return true;
    }

    // Holds all fds which have finished during this loop iteration.
    std::vector<int> FinishedFds;

    for (struct pollfd &fd : PollFds) {
      if (fd.revents & POLLIN || fd.revents & POLLPRI || fd.revents & POLLHUP ||
          fd.revents & POLLERR) {
        // An event which we care about occurred. Find the appropriate Task.
        auto predicate = [&fd] (PidToTaskMap::value_type &value) -> bool {
          return value.second->getPipe() == fd.fd;
        };

        auto iter = std::find_if(ExecutingTasks.begin(), ExecutingTasks.end(),
                                 predicate);
        assert(iter != ExecutingTasks.end() &&
               "All outstanding fds must be associated with an executing Task");
        Task &T = *iter->second;
        if (fd.revents & POLLIN || fd.revents & POLLPRI) {
          // There's data available to read.
          T.readFromPipe();
        }

        if (fd.revents & POLLHUP || fd.revents & POLLERR) {
          // This fd was "hung up" or had an error, so we need to wait for the
          // Task and then clean up.
          pid_t Pid;
          int Status;
          do {
            Status = 0;
            Pid = waitpid(T.getPid(), &Status, 0);
            assert(Pid != 0 &&
                   "We do not pass WNOHANG, so we should always get a pid");
            if (Pid < 0 && (errno == ECHILD || errno == EINVAL))
              return true;
          } while (Pid < 0);

          assert(Pid == T.getPid() &&
                 "We asked to wait for this Task, but we got another Pid!");

          T.finishExecution();

          if (WIFEXITED(Status)) {
            int Result = WEXITSTATUS(Status);

            if (Finished) {
              // If we have a TaskFinishedCallback, only set SubtaskFailed to
              // true if the callback returns StopExecution.
              SubtaskFailed = Finished(T.getPid(), Result, T.getOutput(),
                                       T.getContext()) ==
                  TaskFinishedResponse::StopExecution;
            } else if (Result != 0) {
              // Since we don't have a TaskFinishedCallback, treat a subtask
              // which returned a nonzero exit code as having failed.
              SubtaskFailed = true;
            }
          } else if (WIFSIGNALED(Status)) {
            // The process exited due to a signal.
            int Signal = WTERMSIG(Status);

            StringRef ErrorMsg = strsignal(Signal);

            if (Signalled) {
              TaskFinishedResponse Response = Signalled(T.getPid(), ErrorMsg,
                                                        T.getOutput(),
                                                        T.getContext());
              if (Response == TaskFinishedResponse::StopExecution)
                // If we have a TaskCrashedCallback, only set SubtaskFailed to
                // true if the callback returns StopExecution.
                SubtaskFailed = true;
            } else {
              // Since we don't have a TaskCrashedCallback, treat a crashing
              // subtask as having failed.
              SubtaskFailed = true;
            }
          }

          ExecutingTasks.erase(Pid);
          FinishedFds.push_back(fd.fd);
        }
      } else if (fd.revents & POLLNVAL) {
        // We passed an invalid fd; this should never happen,
        // since we always mark fds as finished after calling
        // Task::finishExecution() (which closes the Task's fd).
        llvm_unreachable("Asked poll() to watch a closed fd");
      }

      fd.revents = 0;
    }

    // Remove any fds which we've closed from PollFds.
    for (int fd : FinishedFds) {
      auto predicate = [&fd] (struct pollfd &i) {
        return i.fd == fd;
      };

      auto iter = std::find_if(PollFds.begin(), PollFds.end(), predicate);
      assert(iter != PollFds.end() && "The finished fd must be in PollFds!");
      PollFds.erase(iter);
    }
  }

  return SubtaskFailed;
}
