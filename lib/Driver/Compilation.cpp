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
#include "swift/Basic/Fallthrough.h"
#include "swift/Basic/Program.h"
#include "swift/Basic/TaskQueue.h"
#include "swift/Basic/Version.h"
#include "swift/Driver/Action.h"
#include "swift/Driver/DependencyGraph.h"
#include "swift/Driver/Driver.h"
#include "swift/Driver/Job.h"
#include "swift/Driver/ParseableOutput.h"
#include "swift/Driver/Tool.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/ADT/TinyPtrVector.h"
#include "llvm/Option/Arg.h"
#include "llvm/Option/ArgList.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/YAMLParser.h"

using namespace swift;
using namespace swift::sys;
using namespace swift::driver;
using namespace llvm::opt;

Compilation::Compilation(const Driver &D, const ToolChain &DefaultToolChain,
                         DiagnosticEngine &Diags, OutputLevel Level,
                         std::unique_ptr<InputArgList> InputArgs,
                         std::unique_ptr<DerivedArgList> TranslatedArgs,
                         StringRef ArgsHash, llvm::sys::TimeValue StartTime,
                         unsigned NumberOfParallelCommands,
                         bool EnableIncrementalBuild,
                         bool SkipTaskExecution,
                         bool SaveTemps)
  : TheDriver(D), DefaultToolChain(DefaultToolChain), Diags(Diags),
    Level(Level), InputArgs(std::move(InputArgs)),
    TranslatedArgs(std::move(TranslatedArgs)), ArgsHash(ArgsHash),
    BuildStartTime(StartTime),
    NumberOfParallelCommands(NumberOfParallelCommands),
    SkipTaskExecution(SkipTaskExecution),
    EnableIncrementalBuild(EnableIncrementalBuild),
    SaveTemps(SaveTemps) {
};

using CommandSet = llvm::SmallPtrSet<const Job *, 16>;

namespace {
  struct PerformJobsState {
    /// All jobs which have been scheduled for execution (whether or not
    /// they've finished execution), or which have been determined that they
    /// don't need to run.
    CommandSet ScheduledCommands;

    /// All jobs which have finished execution or which have been determined
    /// that they don't need to run.
    CommandSet FinishedCommands;

    /// A map from a Job to the commands it is known to be blocking.
    ///
    /// The blocked jobs should be scheduled as soon as possible.
    llvm::SmallDenseMap<const Job *, TinyPtrVector<const Job *>, 16>
        BlockingCommands;

    /// A map from commands that didn't get to run to whether or not they affect
    /// downstream commands.
    ///
    /// Only intended for source files.
    llvm::SmallDenseMap<const Job *, bool, 16> UnfinishedCommands;
  };
}

Compilation::~Compilation() = default;

Job *Compilation::addJob(std::unique_ptr<Job> J) {
  Job *result = J.get();
  Jobs.emplace_back(std::move(J));
  return result;
}

static const Job *findUnfinishedJob(ArrayRef<const Job *> JL,
                                    const CommandSet &FinishedCommands) {
  for (const Job *Cmd : JL) {
    if (!FinishedCommands.count(Cmd))
      return Cmd;
  }
  return nullptr;
}

static void writeCompilationRecord(StringRef path, StringRef argsHash,
                                   llvm::sys::TimeValue buildTime,
                                   const PerformJobsState &endState) {

  llvm::SmallDenseMap<const llvm::opt::Arg *, CompileJobAction::InputInfo, 16>
      inputs;

  for (auto &entry : endState.UnfinishedCommands) {
    for (auto *action : entry.first->getSource().getInputs()) {
      auto inputFile = cast<InputAction>(action);

      CompileJobAction::InputInfo info;
      info.previousModTime = entry.first->getInputModTime();
      info.status = entry.second ?
          CompileJobAction::InputInfo::NeedsCascadingBuild :
          CompileJobAction::InputInfo::NeedsNonCascadingBuild;
      inputs[&inputFile->getInputArg()] = info;
    }
  }

  for (const Job *entry : endState.FinishedCommands) {
    const auto *compileAction = dyn_cast<CompileJobAction>(&entry->getSource());
    if (!compileAction)
      continue;

    for (auto *action : compileAction->getInputs()) {
      auto inputFile = cast<InputAction>(action);

      CompileJobAction::InputInfo info;
      info.previousModTime = entry->getInputModTime();
      info.status = CompileJobAction::InputInfo::UpToDate;
      inputs[&inputFile->getInputArg()] = info;
    }
  }

  std::error_code error;
  llvm::raw_fd_ostream out(path, error, llvm::sys::fs::F_None);
  if (out.has_error()) {
    // FIXME: How should we report this error?
    out.clear_error();
    return;
  }

  auto writeTimeValue = [](llvm::raw_ostream &out, llvm::sys::TimeValue time) {
    out << "[" << time.seconds() << ", " << time.nanoseconds() << "]";
  };

  out << "version: \"" << llvm::yaml::escape(version::getSwiftFullVersion())
      << "\"\n";
  out << "options: \"" << llvm::yaml::escape(argsHash) << "\"\n";
  out << "build_time: ";
  writeTimeValue(out, buildTime);
  out << "\n";
  out << "inputs: \n";

  for (auto &entry : inputs) {
    out << "\t" << llvm::yaml::escape(entry.first->getValue()) << ": ";

    switch (entry.second.status) {
    case CompileJobAction::InputInfo::UpToDate:
      break;
    case CompileJobAction::InputInfo::NewlyAdded:
    case CompileJobAction::InputInfo::NeedsCascadingBuild:
      out << "!dirty ";
      break;
    case CompileJobAction::InputInfo::NeedsNonCascadingBuild:
      out << "!private ";
      break;
    }

    writeTimeValue(out, entry.second.previousModTime);
    out << "\n";
  }
}

int Compilation::performJobsImpl() {
  // Create a TaskQueue for execution.
  std::unique_ptr<TaskQueue> TQ;
  if (SkipTaskExecution)
    TQ.reset(new DummyTaskQueue(NumberOfParallelCommands));
  else
    TQ.reset(new TaskQueue(NumberOfParallelCommands));

  PerformJobsState State;

  using DependencyGraph = DependencyGraph<const Job *>;
  DependencyGraph DepGraph;
  SmallPtrSet<const Job *, 16> DeferredCommands;
  SmallVector<const Job *, 16> InitialOutOfDateCommands;

  DependencyGraph::MarkTracer ActualIncrementalTracer;
  DependencyGraph::MarkTracer *IncrementalTracer = nullptr;
  if (ShowIncrementalBuildDecisions)
    IncrementalTracer = &ActualIncrementalTracer;

  auto noteBuilding = [&] (const Job *cmd, StringRef reason) {
    if (!ShowIncrementalBuildDecisions)
      return;
    if (State.ScheduledCommands.count(cmd))
      return;
    llvm::outs() << "Queuing "
                 << llvm::sys::path::filename(cmd->getOutput().getBaseInput(0))
                 << " " << reason << "\n";
    IncrementalTracer->printPath(llvm::outs(), cmd,
                                 [](raw_ostream &out, const Job *base) {
      out << llvm::sys::path::filename(base->getOutput().getBaseInput(0));
    });
  };

  // Set up scheduleCommandIfNecessaryAndPossible.
  // This will only schedule the given command if it has not been scheduled
  // and if all of its inputs are in FinishedCommands.
  auto scheduleCommandIfNecessaryAndPossible = [&] (const Job *Cmd) {
    if (State.ScheduledCommands.count(Cmd))
      return;

    if (auto Blocking = findUnfinishedJob(Cmd->getInputs(),
                                          State.FinishedCommands)) {
      State.BlockingCommands[Blocking].push_back(Cmd);
      return;
    }

    State.ScheduledCommands.insert(Cmd);
    TQ->addTask(Cmd->getExecutable(), Cmd->getArguments(), llvm::None,
                (void *)Cmd);
  };

  // When a task finishes, we need to reevaluate the other commands that
  // might have been blocked.
  auto markFinished = [&] (const Job *Cmd) {
    State.FinishedCommands.insert(Cmd);

    auto BlockedIter = State.BlockingCommands.find(Cmd);
    if (BlockedIter != State.BlockingCommands.end()) {
      auto AllBlocked = std::move(BlockedIter->second);
      State.BlockingCommands.erase(BlockedIter);
      for (auto *Blocked : AllBlocked)
        scheduleCommandIfNecessaryAndPossible(Blocked);
    }
  };

  // Schedule all jobs we can.
  for (const Job *Cmd : getJobs()) {
    if (!getIncrementalBuildEnabled()) {
      scheduleCommandIfNecessaryAndPossible(Cmd);
      continue;
    }

    // Try to load the dependencies file for this job. If there isn't one, we
    // always have to run the job, but it doesn't affect any other jobs. If
    // there should be one but it's not present or can't be loaded, we have to
    // run all the jobs.
    // FIXME: We can probably do better here!
    Job::Condition Condition = Job::Condition::Always;
    StringRef DependenciesFile =
      Cmd->getOutput().getAdditionalOutputForType(types::TY_SwiftDeps);
    if (!DependenciesFile.empty()) {
      if (Cmd->getCondition() == Job::Condition::NewlyAdded) {
        DepGraph.addIndependentNode(Cmd);
      } else {
        switch (DepGraph.loadFromPath(Cmd, DependenciesFile)) {
        case DependencyGraphImpl::LoadResult::HadError:
          disableIncrementalBuild();
          for (const Job *Cmd : DeferredCommands)
            scheduleCommandIfNecessaryAndPossible(Cmd);
          DeferredCommands.clear();
          break;
        case DependencyGraphImpl::LoadResult::UpToDate:
          Condition = Cmd->getCondition();
          break;
        case DependencyGraphImpl::LoadResult::AffectsDownstream:
          llvm_unreachable("we haven't marked anything in this graph yet");
        }
      }
    }

    switch (Condition) {
    case Job::Condition::Always:
      if (getIncrementalBuildEnabled() && !DependenciesFile.empty()) {
        InitialOutOfDateCommands.push_back(Cmd);
        DepGraph.markIntransitive(Cmd);
      }
      SWIFT_FALLTHROUGH;
    case Job::Condition::RunWithoutCascading:
      noteBuilding(Cmd, "(initial)");
      scheduleCommandIfNecessaryAndPossible(Cmd);
      break;
    case Job::Condition::CheckDependencies:
      DeferredCommands.insert(Cmd);
      break;
    case Job::Condition::NewlyAdded:
      llvm_unreachable("handled above");
    }
  }

  if (getIncrementalBuildEnabled()) {
    SmallVector<const Job *, 16> AdditionalOutOfDateCommands;

    // We scheduled all of the files that have actually changed. Now add the
    // files that haven't changed, so that they'll get built in parallel if
    // possible and after the first set of files if it's not.
    for (auto *Cmd : InitialOutOfDateCommands) {
      DepGraph.markTransitive(AdditionalOutOfDateCommands, Cmd,
                              IncrementalTracer);
    }

    for (auto *transitiveCmd : AdditionalOutOfDateCommands)
      noteBuilding(transitiveCmd, "because of the initial set:");
    size_t firstSize = AdditionalOutOfDateCommands.size();

    // Check all cross-module dependencies as well.
    for (StringRef dependency : DepGraph.getExternalDependencies()) {
      llvm::sys::fs::file_status depStatus;
      if (!llvm::sys::fs::status(dependency, depStatus))
        if (depStatus.getLastModificationTime() < LastBuildTime)
          continue;

      // If the dependency has been modified since the oldest built file,
      // or if we can't stat it for some reason (perhaps it's been deleted?),
      // trigger rebuilds through the dependency graph.
      DepGraph.markExternal(AdditionalOutOfDateCommands, dependency);
    }

    for (auto *externalCmd :
            llvm::makeArrayRef(AdditionalOutOfDateCommands).slice(firstSize)) {
      noteBuilding(externalCmd, "because of external dependencies");
    }

    for (auto *AdditionalCmd : AdditionalOutOfDateCommands) {
      if (!DeferredCommands.count(AdditionalCmd))
        continue;
      scheduleCommandIfNecessaryAndPossible(AdditionalCmd);
      DeferredCommands.erase(AdditionalCmd);
    }
  }

  int Result = 0;

  // Set up a callback which will be called immediately after a task has
  // started. This callback may be used to provide output indicating that the
  // task began.
  auto taskBegan = [this] (ProcessId Pid, void *Context) {
    // TODO: properly handle task began.
    const Job *BeganCmd = (const Job *)Context;

    // For verbose output, print out each command as it begins execution.
    if (Level == OutputLevel::Verbose)
      BeganCmd->printCommandLine(llvm::errs());
    else if (Level == OutputLevel::Parseable)
      parseable_output::emitBeganMessage(llvm::errs(), *BeganCmd, Pid);
  };

  // Set up a callback which will be called immediately after a task has
  // finished execution. This callback should determine if execution should
  // continue (if execution should stop, this callback should return true), and
  // it should also schedule any additional commands which we now know need
  // to run.
  auto taskFinished = [&] (ProcessId Pid, int ReturnCode, StringRef Output,
                           void *Context) -> TaskFinishedResponse {
    const Job *FinishedCmd = (const Job *)Context;

    if (Level == OutputLevel::Parseable) {
      // Parseable output was requested.
      parseable_output::emitFinishedMessage(llvm::errs(), *FinishedCmd, Pid,
                                            ReturnCode, Output);
    } else {
      // Otherwise, send the buffered output to stderr, though only if we
      // support getting buffered output.
      if (TaskQueue::supportsBufferingOutput())
        llvm::errs() << Output;
    }

    if (ReturnCode != 0) {
      // The task failed, so return true without performing any further
      // dependency analysis.

      // Store this task's ReturnCode as our Result if we haven't stored
      // anything yet.
      if (Result == 0)
        Result = ReturnCode;

      if (!FinishedCmd->getCreator().hasGoodDiagnostics() || ReturnCode != 1)
        Diags.diagnose(SourceLoc(), diag::error_command_failed,
                       FinishedCmd->getCreator().getNameForDiagnostics(),
                       ReturnCode);

      return ContinueBuildingAfterErrors ? 
          TaskFinishedResponse::ContinueExecution :
          TaskFinishedResponse::StopExecution;
    }

    // When a task finishes, we need to reevaluate the other commands that
    // might have been blocked.
    markFinished(FinishedCmd);

    // In order to handle both old dependencies that have disappeared and new
    // dependencies that have arisen, we need to reload the dependency file.
    if (getIncrementalBuildEnabled()) {
      const CommandOutput &Output = FinishedCmd->getOutput();
      StringRef DependenciesFile =
        Output.getAdditionalOutputForType(types::TY_SwiftDeps);
      if (!DependenciesFile.empty()) {
        SmallVector<const Job *, 16> Dependents;
        bool wasCascading = DepGraph.isMarked(FinishedCmd);

        switch (DepGraph.loadFromPath(FinishedCmd, DependenciesFile)) {
        case DependencyGraphImpl::LoadResult::HadError:
          disableIncrementalBuild();
          for (const Job *Cmd : DeferredCommands)
            scheduleCommandIfNecessaryAndPossible(Cmd);
          DeferredCommands.clear();
          Dependents.clear();
          break;
        case DependencyGraphImpl::LoadResult::UpToDate:
          if (!wasCascading)
            break;
          SWIFT_FALLTHROUGH;
        case DependencyGraphImpl::LoadResult::AffectsDownstream:
          DepGraph.markTransitive(Dependents, FinishedCmd);
          break;
        }

        for (const Job *Cmd : Dependents) {
          DeferredCommands.erase(Cmd);
          noteBuilding(Cmd, "because of dependencies discovered later");
          scheduleCommandIfNecessaryAndPossible(Cmd);
        }
      }
    }

    return TaskFinishedResponse::ContinueExecution;
  };

  auto taskSignalled = [&] (ProcessId Pid, StringRef ErrorMsg, StringRef Output,
                            void *Context) -> TaskFinishedResponse {
    const Job *SignalledCmd = (const Job *)Context;

    if (Level == OutputLevel::Parseable) {
      // Parseable output was requested.
      parseable_output::emitSignalledMessage(llvm::errs(), *SignalledCmd, Pid,
                                             ErrorMsg, Output);
    } else {
      // Otherwise, send the buffered output to stderr, though only if we
      // support getting buffered output.
      if (TaskQueue::supportsBufferingOutput())
        llvm::errs() << Output;
    }

    if (!ErrorMsg.empty())
      Diags.diagnose(SourceLoc(), diag::error_unable_to_execute_command,
                     ErrorMsg);

    Diags.diagnose(SourceLoc(), diag::error_command_signalled,
                   SignalledCmd->getCreator().getNameForDiagnostics());

    // Since the task signalled, so unconditionally set result to -2.
    Result = -2;

    return TaskFinishedResponse::StopExecution;
  };

  do {
    // Ask the TaskQueue to execute.
    TQ->execute(taskBegan, taskFinished, taskSignalled);

    // Mark all remaining deferred commands as skipped.
    for (const Job *Cmd : DeferredCommands) {
      if (Level == OutputLevel::Parseable) {
        // Provide output indicating this command was skipped if parseable output
        // was requested.
        parseable_output::emitSkippedMessage(llvm::errs(), *Cmd);
      }

      State.ScheduledCommands.insert(Cmd);
      markFinished(Cmd);
    }

    // ...which may allow us to go on and do later tasks.
  } while (Result == 0 && TQ->hasRemainingTasks());

  if (Result == 0) {
    assert(State.BlockingCommands.empty() &&
           "some blocking commands never finished properly");
  } else {
    // Make sure we record any files that still need to be rebuilt.
    for (const Job *Cmd : getJobs()) {
      // Skip files that don't use dependency analysis.
      StringRef DependenciesFile =
          Cmd->getOutput().getAdditionalOutputForType(types::TY_SwiftDeps);
      if (DependenciesFile.empty())
        continue;

      // Don't worry about commands that finished or weren't going to run.
      if (State.FinishedCommands.count(Cmd))
        continue;
      if (!State.ScheduledCommands.count(Cmd))
        continue;

      bool isCascading = true;
      if (getIncrementalBuildEnabled())
        isCascading = DepGraph.isMarked(Cmd);
      State.UnfinishedCommands.insert({Cmd, isCascading});
    }
  }

  if (!CompilationRecordPath.empty() && !SkipTaskExecution) {
    writeCompilationRecord(CompilationRecordPath, ArgsHash, BuildStartTime,
                           State);
  }

  return Result;
}

int Compilation::performSingleCommand(const Job *Cmd) {
  assert(Cmd->getInputs().empty() &&
         "This can only be used to run a single command with no inputs");

  switch (Cmd->getCondition()) {
  case Job::Condition::CheckDependencies:
    return 0;
  case Job::Condition::RunWithoutCascading:
  case Job::Condition::Always:
  case Job::Condition::NewlyAdded:
    break;
  }

  if (Level == OutputLevel::Verbose)
    Cmd->printCommandLine(llvm::errs());

  SmallVector<const char *, 128> Argv;
  Argv.push_back(Cmd->getExecutable());
  Argv.append(Cmd->getArguments().begin(), Cmd->getArguments().end());
  Argv.push_back(0);

  const char *ExecPath = Cmd->getExecutable();
  const char **argv = Argv.data();

  return ExecuteInPlace(ExecPath, argv);
}

int Compilation::performJobs() {
  // If we don't have to do any cleanup work, just exec the subprocess.
  if (Level < OutputLevel::Parseable &&
      (SaveTemps || TempFilePaths.empty()) &&
      CompilationRecordPath.empty() &&
      Jobs.size() == 1) {
    return performSingleCommand(Jobs.front().get());
  }

  if (!TaskQueue::supportsParallelExecution() && NumberOfParallelCommands > 1) {
    Diags.diagnose(SourceLoc(), diag::warning_parallel_execution_not_supported);
  }

  int result = performJobsImpl();

  if (!SaveTemps) {
    // FIXME: Do we want to be deleting temporaries even when a child process
    // crashes?
    for (auto &path : TempFilePaths) {
      // Ignore the error code for removing temporary files.
      (void)llvm::sys::fs::remove(path);
    }
  }

  return result;
}
