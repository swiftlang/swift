//===--- TaskQueueTest.cpp  tests -----------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/Basic/TaskQueue.h"

#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/StringRef.h"
#include "gtest/gtest.h"

#include <algorithm>
#include <chrono>
#include <condition_variable>
#include <mutex>
#include <thread>

#if __is_target_os(darwin) || __is_target_os(linux)
#include <signal.h>

using namespace swift::sys;

TEST(TaskQueueTest, LargeOutput) {
  TaskQueue TQ(1);

  EXPECT_TRUE(TQ.supportsBufferingOutput());
  EXPECT_TRUE(TQ.supportsParallelExecution());
  EXPECT_EQ(1U, TQ.getNumberOfParallelTasks());

  int OutputSize = 0;
  auto TaskFinished = [&](ProcessId Pid, int Result, llvm::StringRef Output,
                         llvm::StringRef Errors, TaskProcessInformation ProcInfo,
                         void *Context) -> TaskFinishedResponse {
    EXPECT_EQ(0, Result);
    OutputSize = Output.size();
    return TaskFinishedResponse::ContinueExecution;
  };

  const char *Args[] = {"-c", "seq 1 100 | while read i; do echo 'This is line number $i with some additional content to make it longer than usual'; done", nullptr};
  TQ.addTask("/bin/sh", Args, llvm::ArrayRef<const char *>(), nullptr, false);

  EXPECT_FALSE(TQ.execute(nullptr, TaskFinished, nullptr));
  EXPECT_GT(OutputSize, 1024U) << "Should produce at least 1KB";
}

TEST(TaskQueueTest, ErrorHandling) {
  TaskQueue TQ(1);
  int ReceivedResult = 0;

  auto TaskFinished = [&](ProcessId Pid, int Result, llvm::StringRef Output,
                         llvm::StringRef Errors, TaskProcessInformation ProcInfo,
                         void *Context) -> TaskFinishedResponse {
    ReceivedResult = Result;
    return TaskFinishedResponse::ContinueExecution;
  };

  const char *Args[] = {"false", nullptr};
  TQ.addTask("/usr/bin/false", Args, llvm::ArrayRef<const char *>(), nullptr, false);

  EXPECT_FALSE(TQ.execute(nullptr, TaskFinished, nullptr));
  EXPECT_NE(0, ReceivedResult);
}

TEST(TaskQueueTest, SeparateErrorStream) {
  TaskQueue TQ(1);
  std::string StdoutContent, StderrContent;

  auto TaskFinished = [&](ProcessId Pid, int Result, llvm::StringRef Output,
                         llvm::StringRef Errors, TaskProcessInformation ProcInfo,
                         void *Context) -> TaskFinishedResponse {
    StdoutContent = Output.str();
    StderrContent = Errors.str();
    return TaskFinishedResponse::ContinueExecution;
  };

  const char *Args[] = {"-c", "echo 'stdout message'; echo 'stderr message' >&2", nullptr};
  TQ.addTask("/bin/sh", Args, llvm::ArrayRef<const char *>(), nullptr, true);

  EXPECT_FALSE(TQ.execute(nullptr, TaskFinished, nullptr));
  EXPECT_NE(StdoutContent.find("stdout message"), std::string::npos);
  EXPECT_NE(StderrContent.find("stderr message"), std::string::npos);
}

TEST(TaskQueueTest, TaskSignalHandling) {
  TaskQueue TQ(1);

  bool TaskSignalled = false;
  int ReceivedSignal = 0;
  ProcessId ChildPid = 0;
  std::mutex PidMutex;
  std::condition_variable PidCv;

  auto TaskBegan = [&](ProcessId Pid, void *Context) {
    {
      std::lock_guard<std::mutex> lock(PidMutex);
      ChildPid = Pid;
    }
    PidCv.notify_one();
  };

  auto TaskSignalledCallback = [&](ProcessId Pid, llvm::StringRef ErrorMsg,
                                  llvm::StringRef Output, llvm::StringRef Errors,
                                  void *Context, std::optional<int> Signal,
                                  TaskProcessInformation ProcInfo) -> TaskFinishedResponse {
    TaskSignalled = true;
    if (Signal.has_value()) {
      ReceivedSignal = Signal.value();
    }
    return TaskFinishedResponse::ContinueExecution;
  };

  const char *Args[] = {"-c", "sleep 10", nullptr};
  TQ.addTask("/bin/sh", Args, llvm::ArrayRef<const char *>(), nullptr, false);

  // Start execution in a separate thread and kill the process
  std::thread executor([&] {
    TQ.execute(TaskBegan, nullptr, TaskSignalledCallback);
  });

  // Wait for the task to actually start and get its PID
  {
    std::unique_lock<std::mutex> lock(PidMutex);
    PidCv.wait_for(lock, std::chrono::seconds(5), [&] { return ChildPid > 0; });
  }

  if (ChildPid > 0) {
    EXPECT_EQ(0, kill(ChildPid, SIGTERM)) << "Should kill the specific child process we spawned";
  }

  executor.join();

  EXPECT_TRUE(TaskSignalled);
  EXPECT_EQ(SIGTERM, ReceivedSignal);
}

TEST(TaskQueueTest, HighConcurrency) {
  TaskQueue TQ(10);

  int TasksCompleted = 0;
  bool AnyTaskFailed = false;
  std::mutex CompletionMutex;

  auto TaskFinished = [&](ProcessId Pid, int Result, llvm::StringRef Output,
                         llvm::StringRef Errors, TaskProcessInformation ProcInfo,
                         void *Context) -> TaskFinishedResponse {
    std::lock_guard<std::mutex> lock(CompletionMutex);
    TasksCompleted++;
    if (Result != 0) {
      AnyTaskFailed = true;
    } else {
      EXPECT_EQ("test", Output.rtrim().str());
    }
    return TaskFinishedResponse::ContinueExecution;
  };

  const char *Args[] = {"test", nullptr};
  for (int i = 0; i < 50; i++) {
    TQ.addTask("/bin/echo", Args, llvm::ArrayRef<const char *>(), nullptr, false);
  }

  bool ExecutionFailed = TQ.execute(nullptr, TaskFinished, nullptr);

  EXPECT_FALSE(ExecutionFailed);
  EXPECT_EQ(50, TasksCompleted);
  EXPECT_FALSE(AnyTaskFailed);
  EXPECT_EQ(10U, TQ.getNumberOfParallelTasks());
}

TEST(TaskQueueTest, TaskBeganCallback) {
  TaskQueue TQ(2);

  std::vector<ProcessId> StartedTasks;
  std::vector<ProcessId> FinishedTasks;
  std::mutex TaskMutex;

  auto TaskBegan = [&](ProcessId Pid, void *Context) {
    std::lock_guard<std::mutex> lock(TaskMutex);
    StartedTasks.push_back(Pid);
  };

  auto TaskFinished = [&](ProcessId Pid, int Result, llvm::StringRef Output,
                         llvm::StringRef Errors, TaskProcessInformation ProcInfo,
                         void *Context) -> TaskFinishedResponse {
    std::lock_guard<std::mutex> lock(TaskMutex);
    FinishedTasks.push_back(Pid);
    return TaskFinishedResponse::ContinueExecution;
  };

  const char *Args[] = {"echo", "test", nullptr};
  for (int i = 0; i < 3; i++) {
    TQ.addTask("/bin/echo", Args, llvm::ArrayRef<const char *>(), nullptr, false);
  }

  bool ExecutionFailed = TQ.execute(TaskBegan, TaskFinished, nullptr);

  EXPECT_FALSE(ExecutionFailed);
  EXPECT_EQ(3U, StartedTasks.size());
  EXPECT_EQ(3U, FinishedTasks.size());

  std::sort(StartedTasks.begin(), StartedTasks.end());
  std::sort(FinishedTasks.begin(), FinishedTasks.end());
  EXPECT_EQ(StartedTasks, FinishedTasks);
}

TEST(TaskQueueTest, StopExecutionOnFailure) {
  TaskQueue TQ(2);

  int TasksCompleted = 0;
  int TasksStarted = 0;

  auto TaskBegan = [&](ProcessId Pid, void *Context) {
    TasksStarted++;
  };

  auto TaskFinished = [&](ProcessId Pid, int Result, llvm::StringRef Output,
                         llvm::StringRef Errors, TaskProcessInformation ProcInfo,
                         void *Context) -> TaskFinishedResponse {
    TasksCompleted++;
    // Stop execution after the first task completes
    if (TasksCompleted >= 1) {
      return TaskFinishedResponse::StopExecution;
    }
    return TaskFinishedResponse::ContinueExecution;
  };

  const char *Args[] = {"test", nullptr};
  for (int i = 0; i < 10; i++) {
    TQ.addTask("/bin/echo", Args, llvm::ArrayRef<const char *>(), nullptr, false);
  }

  bool ExecutionFailed = TQ.execute(TaskBegan, TaskFinished, nullptr);

  EXPECT_TRUE(ExecutionFailed);
  EXPECT_LE(TasksStarted, 2U) << "Should have started up to 2 tasks";
  EXPECT_LT(TasksCompleted, 10) << "Should have completed fewer than 10 tasks";
  EXPECT_GE(TasksCompleted, 1) << "At least one task should have completed (the one that triggered StopExecution)";
}

TEST(TaskQueueTest, DummyTaskQueueBasicOperation) {
  DummyTaskQueue TQ(2);

  int TasksStarted = 0;
  int TasksCompleted = 0;
  int Context1 = 42;
  int Context2 = 100;
  std::vector<void*> ReceivedContexts;

  auto TaskBegan = [&](ProcessId Pid, void *Context) {
    TasksStarted++;
    ReceivedContexts.push_back(Context);
  };

  auto TaskFinished = [&](ProcessId Pid, int Result, llvm::StringRef Output,
                         llvm::StringRef Errors, TaskProcessInformation ProcInfo,
                         void *Context) -> TaskFinishedResponse {
    TasksCompleted++;
    EXPECT_EQ(0, Result);
    EXPECT_EQ("Output placeholder\n", Output.str());
    EXPECT_NE(nullptr, Context);
    if (Context == &Context1) {
      EXPECT_EQ(42, *static_cast<int*>(Context));
    } else if (Context == &Context2) {
      EXPECT_EQ(100, *static_cast<int*>(Context));
    }
    return TaskFinishedResponse::ContinueExecution;
  };

  const char *Args[] = {"test", nullptr};
  TQ.addTask("/dummy/path1", Args, llvm::ArrayRef<const char *>(), &Context1, false);
  TQ.addTask("/dummy/path2", Args, llvm::ArrayRef<const char *>(), &Context2, false);

  bool ExecutionFailed = TQ.execute(TaskBegan, TaskFinished, nullptr);

  EXPECT_FALSE(ExecutionFailed);
  EXPECT_EQ(2, TasksStarted);
  EXPECT_EQ(2, TasksCompleted);
  EXPECT_EQ(&Context1, ReceivedContexts[0]);
  EXPECT_EQ(&Context2, ReceivedContexts[1]);
}

TEST(TaskQueueTest, DummyTaskQueueSeparateErrors) {
  DummyTaskQueue TQ(1);

  bool TaskExecuted = false;
  std::string ReceivedOutput;
  std::string ReceivedErrors;

  auto TaskFinished = [&](ProcessId Pid, int Result, llvm::StringRef Output,
                         llvm::StringRef Errors, TaskProcessInformation ProcInfo,
                         void *Context) -> TaskFinishedResponse {
    TaskExecuted = true;
    ReceivedOutput = Output.str();
    ReceivedErrors = Errors.str();
    return TaskFinishedResponse::ContinueExecution;
  };

  const char *Args[] = {"test", nullptr};
  TQ.addTask("/dummy/path", Args, llvm::ArrayRef<const char *>(), nullptr, true);

  bool ExecutionFailed = TQ.execute(nullptr, TaskFinished, nullptr);

  EXPECT_FALSE(ExecutionFailed);
  EXPECT_TRUE(TaskExecuted);
  EXPECT_EQ("Output placeholder\n", ReceivedOutput);
  EXPECT_EQ("Error placeholder\n", ReceivedErrors);
}

#endif
