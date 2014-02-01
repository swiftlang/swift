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

#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/DiagnosticsDriver.h"
#include "swift/Basic/Program.h"
#include "swift/Basic/TaskQueue.h"
#include "swift/Driver/Driver.h"
#include "swift/Driver/Job.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/Option/ArgList.h"
#include "llvm/Support/raw_ostream.h"

using namespace swift;
using namespace swift::sys;
using namespace swift::driver;
using namespace llvm::opt;

Compilation::Compilation(const Driver &D, const ToolChain &DefaultToolChain,
                         DiagnosticEngine &Diags, OutputLevel Level,
                         std::unique_ptr<InputArgList> InputArgs,
                         std::unique_ptr<DerivedArgList> TranslatedArgs,
                         unsigned NumberOfParallelCommands,
                         bool SkipTaskExecution)
  : TheDriver(D), DefaultToolChain(DefaultToolChain), Diags(Diags),
    Level(Level), Jobs(new JobList), InputArgs(std::move(InputArgs)),
    TranslatedArgs(std::move(TranslatedArgs)),
    NumberOfParallelCommands(NumberOfParallelCommands),
    SkipTaskExecution(SkipTaskExecution) {
};

Compilation::~Compilation() = default;

void Compilation::addJob(Job *J) {
  Jobs->addJob(J);
}

typedef llvm::DenseSet<const Command *> CommandSet;

static bool allJobsInListHaveFinished(const JobList &JL,
                                      const CommandSet &FinishedCommands) {
  for (const Job *J : JL) {
    if (const Command *Cmd = dyn_cast<Command>(J)) {
      if (!FinishedCommands.count(Cmd))
        return false;
    } else if (const JobList *List = dyn_cast<JobList>(J)) {
      if (!allJobsInListHaveFinished(*List, FinishedCommands))
        return false;
    } else {
      llvm_unreachable("Unknown Job class!");
    }
  }
  return true;
}

int Compilation::performJobsInList(const JobList &JL,
                                   CommandSet &ScheduledCommands,
                                   CommandSet &FinishedCommands) {
  // Create a TaskQueue for execution.
  std::unique_ptr<TaskQueue> TQ;
  if (SkipTaskExecution)
    TQ.reset(new DummyTaskQueue(NumberOfParallelCommands));
  else
    TQ.reset(new TaskQueue(NumberOfParallelCommands));

  // Set up scheduleCommandIfNecessaryAndPossible.
  // This will only schedule the given command if it has not been scheduled
  // and if all of its inputs are in FinishedCommands.
  auto scheduleCommandIfNecessaryAndPossible = [&] (const Command *Cmd) {
    if (!ScheduledCommands.count(Cmd)) {
      if (allJobsInListHaveFinished(Cmd->getInputs(), FinishedCommands)) {
        ScheduledCommands.insert(Cmd);
        TQ->addTask(Cmd->getExecutable(), Cmd->getArguments(), llvm::None,
                    (void *)Cmd);
      }
    }
  };

  // Set up handleCommandWhichDoesNotNeedToExecute.
  // This will mark the Command as both scheduled and finished, which meets the
  // definitions of Commands which should be in those sets.
  auto handleCommandWhichDoesNotNeedToExecute = [&] (const Command *Cmd) {
    ScheduledCommands.insert(Cmd);
    FinishedCommands.insert(Cmd);
  };

  // Perform all inputs to the Jobs in our JobList, and schedule any Commands
  // which we know need to execute.
  for (const Job *J : JL) {
    if (const Command *Cmd = dyn_cast<Command>(J)) {
      int res = performJobsInList(Cmd->getInputs(), ScheduledCommands,
                                  FinishedCommands);
      if (res != 0)
        return res;

      // TODO: replace with a real check once available.
      bool needsToExecute = true;
      if (needsToExecute)
        scheduleCommandIfNecessaryAndPossible(Cmd);
      else
        handleCommandWhichDoesNotNeedToExecute(Cmd);
    } else if (const JobList *List = dyn_cast<JobList>(J)) {
      int res = performJobsInList(*List, ScheduledCommands, FinishedCommands);
      if (res != 0)
        return res;
    } else {
      llvm_unreachable("Unknown Job class!");
    }
  }

  int Result = 0;

  // Set up a callback which will be called immediately after a task has
  // started. This callback may be used to provide output indicating that the
  // task began.
  auto taskBegan = [this] (ProcessId Pid, void *Context) {
    // TODO: properly handle task began.
    const Command *BeganCmd = (const Command *)Context;

    // For verbose output, print out each command as it begins execution.
    if (Level == OutputLevel::Verbose)
      BeganCmd->printCommandLine(llvm::errs());
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
    FinishedCommands.insert(FinishedCmd);
    for (const Job *J : JL) {
      if (const Command *Cmd = dyn_cast<Command>(J)) {
        // TODO: replace with a real check once available.
        if (Cmd != FinishedCmd) {
          bool needsToExecute = true;
          if (needsToExecute)
            scheduleCommandIfNecessaryAndPossible(Cmd);
          else
            handleCommandWhichDoesNotNeedToExecute(Cmd);
        }
      } else {
        assert(isa<JobList>(J) && "Unknown Job class!");
      }
    }

    return TaskFinishedResponse::ContinueExecution;
  };

  // Ask the TaskQueue to execute.
  TQ->execute(taskBegan, taskFinished);

  return Result;
}

static const Command *getOnlyCommandInList(const JobList *List) {
  if (List->size() != 1)
    return nullptr;

  const Job *J = List->getJobs()[0];
  if (const Command *Cmd = dyn_cast<Command>(J)) {
    if (Cmd->getInputs().empty())
      return Cmd;
    else
      return nullptr;
  } else if (const JobList *JL = dyn_cast<JobList>(J)) {
    return getOnlyCommandInList(JL);
  } else {
    llvm_unreachable("Unknown Job class!");
  }
}

static int performSingleCommand(const Command *Cmd) {
  assert(Cmd->getInputs().empty() &&
         "This can only be used to run a single Command with no inputs");
  
  SmallVector<const char *, 128> Argv;
  Argv.push_back(Cmd->getExecutable());
  Argv.append(Cmd->getArguments().begin(), Cmd->getArguments().end());
  Argv.push_back(0);

  const char *ExecPath = Cmd->getExecutable();
  const char **argv = Argv.data();

  return ExecuteInPlace(ExecPath, argv);
}

int Compilation::performJobs() {
  // TODO: determine if an option requires buffered output; right now, none do
  bool RequiresBufferedOutput = false;
  if (!RequiresBufferedOutput) {
    const Command *OnlyCmd = getOnlyCommandInList(Jobs.get());
    if (OnlyCmd)
      return performSingleCommand(OnlyCmd);
  }

  if (!TaskQueue::supportsParallelExecution() && NumberOfParallelCommands > 1) {
    Diags.diagnose(SourceLoc(), diag::warning_parallel_execution_not_supported);
  }
  
  // Set up a set for storing all Commands which have been scheduled for
  // execution (whether or not they've finished execution),
  // or which have been determined that they don't need to run.
  CommandSet ScheduledCommands;

  // Set up a set for storing all Commands which have finished execution or
  // which have been determined that they don't need to run.
  CommandSet FinishedCommands;

  return performJobsInList(*Jobs, ScheduledCommands, FinishedCommands);
}
