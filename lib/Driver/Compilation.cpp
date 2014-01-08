//===--- Compilation.cpp - Compilation Task Data Structure ----------------===//
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

#include "swift/Driver/Compilation.h"

#include "swift/Basic/TaskQueue.h"
#include "swift/Driver/Job.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/Option/ArgList.h"
#include "llvm/Support/raw_ostream.h"

using namespace swift;
using namespace swift::sys;
using namespace swift::driver;
using namespace llvm::opt;

Compilation::Compilation(const Driver &D, const ToolChain &DefaultToolChain,
                         std::unique_ptr<InputArgList> InputArgs,
                         std::unique_ptr<DerivedArgList> TranslatedArgs,
                         unsigned NumberOfParallelCommands)
  : TheDriver(D), DefaultToolChain(DefaultToolChain), Jobs(new JobList),
    InputArgs(std::move(InputArgs)), TranslatedArgs(std::move(TranslatedArgs)),
    NumberOfParallelCommands(NumberOfParallelCommands) {
};

Compilation::~Compilation() = default;

void Compilation::addJob(Job *J) {
  Jobs->addJob(J);
}

int Compilation::performJobsInList(const JobList &JL) {
  // Create a TaskQueue for execution, if possible.
  TaskQueue TQ(NumberOfParallelCommands);

  // Store pointers to the Commands which have already been scheduled
  // to execute.
  llvm::DenseSet<const Command *> ScheduledCommands;

  // Set up scheduleCommandIfNecessary and supportsBufferingOutput.
  auto scheduleCommandIfNecessary = [&] (const Command *Cmd) {
    if (ScheduledCommands.insert(Cmd).second) {
      TQ.addTask(Cmd->getExecutable(), Cmd->getArguments(), llvm::None,
                 (void *)Cmd);
    }
  };

  // Perform all inputs to the Jobs in our JobList, and schedule any Commands
  // which we know need to execute.
  for (const Job *J : JL) {
    if (const Command *Cmd = dyn_cast<Command>(J)) {
      int result = performJobsInList(Cmd->getInputs());
      if (result != 0)
        return result;

      // TODO: replace with a real check once available.
      bool needsToExecute = true;
      if (needsToExecute)
        scheduleCommandIfNecessary(Cmd);
    } else if (const JobList *List = dyn_cast<JobList>(J)) {
      int result = performJobsInList(*List);
      if (result != 0)
        return result;
    } else {
      llvm_unreachable("Unknown Job class!");
    }
  }

  int Result = 0;

  // Set up a callback which will be called immediately after a task has
  // started. This callback may be used to provide output indicating that the
  // task began.
  auto taskBegan = [] (ProcessId Pid, void *Context) {
    // TODO: properly handle task began.
    const Command *BeganCmd = (const Command *)Context;

    // TODO: add support for controlling whether command lines are printed
    // when execution begins.
    llvm::errs() << BeganCmd->getExecutable();
    for (const char *Arg : BeganCmd->getArguments()) {
      llvm::errs() << ' ' << Arg;
    }
    llvm::errs() << '\n';
  };

  // Set up a callback which will be called immediately after a task has
  // finished execution. This callback should determine if execution should
  // continue (if execution should stop, this callback should return true), and
  // it should also schedule any additional commands which we now know need
  // to run.
  auto taskFinished = [&] (ProcessId Pid, int ReturnCode, StringRef Output,
                           void *Context) -> TaskFinishedResponse {
    // First, send the buffered output to stderr, though only if we support
    // getting buffered output.
    if (TaskQueue::supportsBufferingOutput())
      llvm::errs() << Output;

    if (ReturnCode != 0) {
      // The task failed, so return true without performing any further
      // dependency analysis.

      // Store this task's ReturnCode as our Result if we haven't stored
      // anything yet.
      if (Result == 0)
        Result = ReturnCode;

      return TaskFinishedResponse::StopExecution;
    }

    // When a task finishes, we need to reevaluate the other Commands in our
    // JobList.

    // TODO: use the Command which just finished to evaluate other Commands.
    const Command *FinishedCmd = (const Command *)Context;
    for (const Job *J : JL) {
      if (const Command *Cmd = dyn_cast<Command>(J)) {
        // TODO: replace with a real check once available.
        bool needsToExecute = Cmd != FinishedCmd;
        if (needsToExecute)
          scheduleCommandIfNecessary(Cmd);
      } else {
        assert(isa<JobList>(J) && "Unknown Job class!");
      }
    }

    return TaskFinishedResponse::ContinueExecution;
  };

  // Ask the TaskQueue to execute.
  TQ.execute(taskBegan, taskFinished);

  return Result;
}

int Compilation::performJobs() {
  if (!TaskQueue::supportsParallelExecution() && NumberOfParallelCommands > 1) {
    // TODO: emit diagnostic
    llvm::errs() << "warning: parallel execution not supported; "
                 << "falling back to serial execution\n";
  }
  return performJobsInList(*Jobs);
}
