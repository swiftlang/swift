//===--- Compilation.cpp - Compilation Task Data Structure ----------------===//
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

#include "swift/Driver/Compilation.h"

#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/DiagnosticsDriver.h"
#include "swift/Basic/Program.h"
#include "swift/Basic/TaskQueue.h"
#include "swift/Basic/Version.h"
#include "swift/Basic/type_traits.h"
#include "swift/Driver/Action.h"
#include "swift/Driver/DependencyGraph.h"
#include "swift/Driver/Driver.h"
#include "swift/Driver/Job.h"
#include "swift/Driver/ParseableOutput.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/MapVector.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/ADT/TinyPtrVector.h"
#include "llvm/Option/Arg.h"
#include "llvm/Option/ArgList.h"
#include "llvm/Support/Chrono.h"
#include "llvm/Support/Compiler.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/Timer.h"
#include "llvm/Support/YAMLParser.h"

#include "CompilationRecord.h"

using namespace swift;
using namespace swift::sys;
using namespace swift::driver;
using namespace llvm::opt;

struct LogJob {
  const Job *j;
  LogJob(const Job *j) : j(j) {}
};

struct LogJobArray {
  const ArrayRef<const Job *> js;
  LogJobArray(const ArrayRef<const Job *> js) : js(js) {}
};

struct LogJobSet {
  const SmallPtrSetImpl<const Job*> &js;
  LogJobSet(const SmallPtrSetImpl<const Job*> &js) : js(js) {}
};

llvm::raw_ostream &operator<<(llvm::raw_ostream &os, const LogJob &lj) {
  lj.j->printSummary(os);
  return os;
}

llvm::raw_ostream &operator<<(llvm::raw_ostream &os, const LogJobArray &ljs) {
  os << "[";
  interleave(ljs.js,
             [&](Job const *j) { os << LogJob(j); },
             [&]() { os << ' '; });
  os << "]";
  return os;
}

llvm::raw_ostream &operator<<(llvm::raw_ostream &os, const LogJobSet &ljs) {
  os << "{";
  interleave(ljs.js,
             [&](Job const *j) { os << LogJob(j); },
             [&]() { os << ' '; });
  os << "}";
  return os;
}


Compilation::Compilation(DiagnosticEngine &Diags, OutputLevel Level,
                         std::unique_ptr<InputArgList> InputArgs,
                         std::unique_ptr<DerivedArgList> TranslatedArgs,
                         InputFileList InputsWithTypes,
                         StringRef ArgsHash, llvm::sys::TimePoint<> StartTime,
                         unsigned NumberOfParallelCommands,
                         bool EnableIncrementalBuild,
                         bool SkipTaskExecution,
                         bool SaveTemps,
                         bool ShowDriverTimeCompilation)
  : Diags(Diags), Level(Level), RawInputArgs(std::move(InputArgs)),
    TranslatedArgs(std::move(TranslatedArgs)), 
    InputFilesWithTypes(std::move(InputsWithTypes)), ArgsHash(ArgsHash),
    BuildStartTime(StartTime),
    NumberOfParallelCommands(NumberOfParallelCommands),
    SkipTaskExecution(SkipTaskExecution),
    EnableIncrementalBuild(EnableIncrementalBuild),
    SaveTemps(SaveTemps),
    ShowDriverTimeCompilation(ShowDriverTimeCompilation) {
};

static bool writeFilelistIfNecessary(const Job *job, DiagnosticEngine &diags);

using CommandSet = llvm::SmallPtrSet<const Job *, 16>;


using InputInfoMap = llvm::SmallMapVector<const llvm::opt::Arg *,
                                          CompileJobAction::InputInfo, 16>;

namespace swift {
namespace driver {
  class PerformJobsState {

    /// The containing Compilation object.
    Compilation &Comp;

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

    /// Jobs that incremental-mode has decided it can skip.
    CommandSet DeferredCommands;

    /// Jobs in the initial set with Condition::Always, or lacking existing
    /// .swiftdeps files.
    SmallVector<const Job *, 16> InitialOutOfDateCommands;

    /// Dependency graph for deciding which jobs are dirty (need running)
    /// or clean (can be skipped).
    using DependencyGraph = DependencyGraph<const Job *>;
    DependencyGraph DepGraph;

    /// Helper for tracing the propagation of marks in the graph.
    DependencyGraph::MarkTracer ActualIncrementalTracer;
    DependencyGraph::MarkTracer *IncrementalTracer = nullptr;

    /// TaskQueue for execution.
    std::unique_ptr<TaskQueue> TQ;

    /// Cumulative result of PerformJobs(), accumulated from subprocesses.
    int Result = EXIT_SUCCESS;

    /// Timers for monitoring execution time of subprocesses.
    llvm::TimerGroup DriverTimerGroup {"driver", "Driver Compilation Time"};
    llvm::SmallDenseMap<const Job *, std::unique_ptr<llvm::Timer>, 16>
    DriverTimers;

    void noteBuilding(const Job *cmd, StringRef reason) {
      if (!Comp.ShowIncrementalBuildDecisions)
        return;
      if (ScheduledCommands.count(cmd))
        return;
      llvm::outs() << "Queuing " << reason << ": " << LogJob(cmd) << "\n";
      IncrementalTracer->printPath(
        llvm::outs(), cmd, [](raw_ostream &out, const Job *base) {
          out << llvm::sys::path::filename(base->getOutput().getBaseInput(0));
        });
    }

    const Job *findUnfinishedJob(ArrayRef<const Job *> JL) {
      for (const Job *Cmd : JL) {
        if (!FinishedCommands.count(Cmd))
          return Cmd;
      }
      return nullptr;
    }

    /// Schedule the given Job if it has not been scheduled and if all of
    /// its inputs are in FinishedCommands.
    void scheduleCommandIfNecessaryAndPossible(const Job *Cmd) {
      if (ScheduledCommands.count(Cmd)) {
        if (Comp.ShowJobLifecycle) {
          llvm::outs() << "Already scheduled: " << LogJob(Cmd) << "\n";
        }
        return;
      }

      if (auto Blocking = findUnfinishedJob(Cmd->getInputs())) {
        BlockingCommands[Blocking].push_back(Cmd);
        if (Comp.ShowJobLifecycle) {
          llvm::outs() << "Blocked by: " << LogJob(Blocking)
                       << ", now blocking jobs: "
                       << LogJobArray(BlockingCommands[Blocking]) << "\n";
        }
        return;
      }

      // FIXME: Failing here should not take down the whole process.
      bool success = writeFilelistIfNecessary(Cmd, Comp.Diags);
      assert(success && "failed to write filelist");
      (void)success;

      assert(Cmd->getExtraEnvironment().empty() &&
             "not implemented for compilations with multiple jobs");
      ScheduledCommands.insert(Cmd);
      if (Comp.ShowJobLifecycle)
        llvm::outs() << "Added to TaskQueue: " << LogJob(Cmd) << "\n";
      TQ->addTask(Cmd->getExecutable(), Cmd->getArguments(), llvm::None,
                  (void *)Cmd);
    }

    /// When a task finishes, check other Jobs that may be blocked.
    void markFinished(const Job *Cmd, bool Skipped=false) {
      if (Comp.ShowJobLifecycle) {
        llvm::outs() << "Job "
                     << (Skipped ? "skipped" : "finished")
                     << ": " << LogJob(Cmd) << "\n";
      }
      FinishedCommands.insert(Cmd);

      auto BlockedIter = BlockingCommands.find(Cmd);
      if (BlockedIter != BlockingCommands.end()) {
        auto AllBlocked = std::move(BlockedIter->second);
        if (Comp.ShowJobLifecycle) {
          llvm::outs() << "Scheduling maybe-unblocked jobs: "
                       << LogJobArray(AllBlocked) << "\n";
        }
        BlockingCommands.erase(BlockedIter);
        for (auto *Blocked : AllBlocked)
          scheduleCommandIfNecessaryAndPossible(Blocked);
      }
    }

    /// Callback which will be called immediately after a task has started. This
    /// callback may be used to provide output indicating that the task began.
    void taskBegan(ProcessId Pid, void *Context) {
      // TODO: properly handle task began.
      const Job *BeganCmd = (const Job *)Context;

      if (Comp.ShowDriverTimeCompilation) {
        llvm::SmallString<128> TimerName;
        llvm::raw_svector_ostream OS(TimerName);
        OS << LogJob(BeganCmd);
        DriverTimers.insert({
            BeganCmd,
              std::unique_ptr<llvm::Timer>(
                new llvm::Timer("task", OS.str(), DriverTimerGroup))
              });
        DriverTimers[BeganCmd]->startTimer();
      }

      // For verbose output, print out each command as it begins execution.
      if (Comp.Level == OutputLevel::Verbose)
        BeganCmd->printCommandLine(llvm::errs());
      else if (Comp.Level == OutputLevel::Parseable)
        parseable_output::emitBeganMessage(llvm::errs(), *BeganCmd, Pid);
    }

    void
    dependencyLoadFailed(StringRef DependenciesFile, bool Warn=true) {
      if (Warn && Comp.ShowIncrementalBuildDecisions)
        Comp.Diags.diagnose(SourceLoc(),
                            diag::warn_unable_to_load_dependencies,
                            DependenciesFile);
      Comp.disableIncrementalBuild();
      for (const Job *Cmd : DeferredCommands)
        scheduleCommandIfNecessaryAndPossible(Cmd);
      DeferredCommands.clear();
    }

    /// Helper that reloads a job's .swiftdeps file after the job exits, and
    /// re-runs transitive marking to ensure everything is properly invalidated
    /// by any new dependency edges introduced by it.
    template <unsigned N>
    void reloadAndRemarkDeps(const Job *FinishedCmd,
                             int ReturnCode,
                             SmallVector<const Job *, N> &Dependents) {
      const CommandOutput &Output = FinishedCmd->getOutput();
      StringRef DependenciesFile =
        Output.getAdditionalOutputForType(types::TY_SwiftDeps);

      if (DependenciesFile.empty()) {
        // If this job doesn't track dependencies, it must always be run.
        // Note: In theory CheckDependencies makes sense as well (for a leaf
        // node in the dependency graph), and maybe even NewlyAdded (for very
        // coarse dependencies that always affect downstream nodes), but we're
        // not using either of those right now, and this logic should probably
        // be revisited when we are.
        assert(FinishedCmd->getCondition() == Job::Condition::Always);
      } else {
        // If we have a dependency file /and/ the frontend task exited normally,
        // we can be discerning about what downstream files to rebuild.
        if (ReturnCode == EXIT_SUCCESS || ReturnCode == EXIT_FAILURE) {
          bool wasCascading = DepGraph.isMarked(FinishedCmd);

          switch (DepGraph.loadFromPath(FinishedCmd, DependenciesFile)) {
          case DependencyGraphImpl::LoadResult::HadError:
            if (ReturnCode == EXIT_SUCCESS) {
              dependencyLoadFailed(DependenciesFile);
              Dependents.clear();
            } // else, let the next build handle it.
            break;
          case DependencyGraphImpl::LoadResult::UpToDate:
            if (!wasCascading)
              break;
            LLVM_FALLTHROUGH;
          case DependencyGraphImpl::LoadResult::AffectsDownstream:
            DepGraph.markTransitive(Dependents, FinishedCmd,
                                    IncrementalTracer);
            break;
          }
        } else {
          // If there's an abnormal exit (a crash), assume the worst.
          switch (FinishedCmd->getCondition()) {
          case Job::Condition::NewlyAdded:
            // The job won't be treated as newly added next time. Conservatively
            // mark it as affecting other jobs, because some of them may have
            // completed already.
            DepGraph.markTransitive(Dependents, FinishedCmd,
                                    IncrementalTracer);
            break;
          case Job::Condition::Always:
            // Any incremental task that shows up here has already been marked;
            // we didn't need to wait for it to finish to start downstream
            // tasks.
            assert(DepGraph.isMarked(FinishedCmd));
            break;
          case Job::Condition::RunWithoutCascading:
            // If this file changed, it might have been a non-cascading change
            // and it might not. Unfortunately, the interface hash has been
            // updated or compromised, so we don't actually know anymore; we
            // have to conservatively assume the changes could affect other
            // files.
            DepGraph.markTransitive(Dependents, FinishedCmd,
                                    IncrementalTracer);
            break;
          case Job::Condition::CheckDependencies:
            // If the only reason we're running this is because something else
            // changed, then we can trust the dependency graph as to whether
            // it's a cascading or non-cascading change. That is, if whatever
            // /caused/ the error isn't supposed to affect other files, and
            // whatever /fixes/ the error isn't supposed to affect other files,
            // then there's no need to recompile any other inputs. If either of
            // those are false, we /do/ need to recompile other inputs.
            break;
          }
        }
      }
    }

    /// Callback which will be called immediately after a task has finished
    /// execution. Determines if execution should continue, and also schedule
    /// any additional Jobs which we now know we need to run.
    TaskFinishedResponse
    taskFinished(ProcessId Pid, int ReturnCode, StringRef Output,
                 StringRef Errors, void *Context,
                 Optional<ResourceStats> Resources) {
      const Job *FinishedCmd = (const Job *)Context;

      if (Comp.ShowDriverTimeCompilation) {
        DriverTimers[FinishedCmd]->stopTimer();
      }

      if (Comp.Level == OutputLevel::Parseable) {
        // Parseable output was requested.
        parseable_output::emitFinishedMessage(llvm::errs(), *FinishedCmd, Pid,
                                              ReturnCode, Output, Resources);
      } else {
        // Otherwise, send the buffered output to stderr, though only if we
        // support getting buffered output.
        if (TaskQueue::supportsBufferingOutput())
          llvm::errs() << Output;
      }

      // In order to handle both old dependencies that have disappeared and new
      // dependencies that have arisen, we need to reload the dependency file.
      // Do this whether or not the build succeeded.
      SmallVector<const Job *, 16> Dependents;
      if (Comp.getIncrementalBuildEnabled()) {
        reloadAndRemarkDeps(FinishedCmd, ReturnCode, Dependents);
      }

      if (ReturnCode != EXIT_SUCCESS) {
        // The task failed, so return true without performing any further
        // dependency analysis.

        // Store this task's ReturnCode as our Result if we haven't stored
        // anything yet.
        if (Result == EXIT_SUCCESS)
          Result = ReturnCode;

        if (!isa<CompileJobAction>(FinishedCmd->getSource()) ||
            ReturnCode != EXIT_FAILURE) {
          Comp.Diags.diagnose(SourceLoc(), diag::error_command_failed,
                              FinishedCmd->getSource().getClassName(),
                              ReturnCode);
        }

        return Comp.ContinueBuildingAfterErrors ?
          TaskFinishedResponse::ContinueExecution :
          TaskFinishedResponse::StopExecution;
      }

      // When a task finishes, we need to reevaluate the other commands that
      // might have been blocked.
      markFinished(FinishedCmd);

      for (const Job *Cmd : Dependents) {
        DeferredCommands.erase(Cmd);
        noteBuilding(Cmd, "because of dependencies discovered later");
        scheduleCommandIfNecessaryAndPossible(Cmd);
      }

      return TaskFinishedResponse::ContinueExecution;
    }

    TaskFinishedResponse
    taskSignalled(ProcessId Pid, StringRef ErrorMsg, StringRef Output,
                  StringRef Errors, void *Context, Optional<int> Signal,
                  Optional<ResourceStats> Resources) {
      const Job *SignalledCmd = (const Job *)Context;

      if (Comp.ShowDriverTimeCompilation) {
        DriverTimers[SignalledCmd]->stopTimer();
      }

      if (Comp.Level == OutputLevel::Parseable) {
        // Parseable output was requested.
        parseable_output::emitSignalledMessage(llvm::errs(), *SignalledCmd,
                                               Pid, ErrorMsg, Output, Signal,
                                               Resources);
      } else {
        // Otherwise, send the buffered output to stderr, though only if we
        // support getting buffered output.
        if (TaskQueue::supportsBufferingOutput())
          llvm::errs() << Output;
      }

      if (!ErrorMsg.empty())
        Comp.Diags.diagnose(SourceLoc(), diag::error_unable_to_execute_command,
                            ErrorMsg);

      if (Signal.hasValue()) {
        Comp.Diags.diagnose(SourceLoc(), diag::error_command_signalled,
                            SignalledCmd->getSource().getClassName(),
                            Signal.getValue());
      } else {
        Comp.Diags.diagnose(SourceLoc(),
                            diag::error_command_signalled_without_signal_number,
                            SignalledCmd->getSource().getClassName());
      }

      // Since the task signalled, unconditionally set result to -2.
      Result = -2;

      return TaskFinishedResponse::StopExecution;
    }

  public:
    PerformJobsState(Compilation &Comp) : Comp(Comp) {
      if (Comp.SkipTaskExecution)
        TQ.reset(new DummyTaskQueue(Comp.NumberOfParallelCommands));
      else
        TQ.reset(new TaskQueue(Comp.NumberOfParallelCommands));
      if (Comp.ShowIncrementalBuildDecisions)
        IncrementalTracer = &ActualIncrementalTracer;
    }

    /// Schedule all jobs we can from the initial list provided by Compilation.
    void scheduleInitialJobs() {
      for (const Job *Cmd : Comp.getJobs()) {
        if (!Comp.getIncrementalBuildEnabled()) {
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
              dependencyLoadFailed(DependenciesFile, /*Warn=*/false);
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
          if (Comp.getIncrementalBuildEnabled() && !DependenciesFile.empty()) {
            InitialOutOfDateCommands.push_back(Cmd);
            DepGraph.markIntransitive(Cmd);
          }
          LLVM_FALLTHROUGH;
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
    }

    /// Schedule transitive closure of initial jobs, and external jobs.
    void scheduleAdditionalJobs() {
      if (Comp.getIncrementalBuildEnabled()) {
        SmallVector<const Job *, 16> AdditionalOutOfDateCommands;

        // We scheduled all of the files that have actually changed. Now add the
        // files that haven't changed, so that they'll get built in parallel if
        // possible and after the first set of files if it's not.
        for (auto *Cmd : InitialOutOfDateCommands) {
          DepGraph.markTransitive(AdditionalOutOfDateCommands, Cmd,
                                  IncrementalTracer);
        }

        for (auto *transitiveCmd : AdditionalOutOfDateCommands)
          noteBuilding(transitiveCmd, "because of the initial set");
        size_t firstSize = AdditionalOutOfDateCommands.size();

        // Check all cross-module dependencies as well.
        for (StringRef dependency : DepGraph.getExternalDependencies()) {
          llvm::sys::fs::file_status depStatus;
          if (!llvm::sys::fs::status(dependency, depStatus))
            if (depStatus.getLastModificationTime() < Comp.LastBuildTime)
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
    }

    void runTaskQueueToCompletion() {
      do {
        using namespace std::placeholders;
        // Ask the TaskQueue to execute.
        TQ->execute(std::bind(&PerformJobsState::taskBegan, this,
                              _1, _2),
                    std::bind(&PerformJobsState::taskFinished, this,
                              _1, _2, _3, _4, _5, _6),
                    std::bind(&PerformJobsState::taskSignalled, this,
                              _1, _2, _3, _4, _5, _6, _7));

        // Mark all remaining deferred commands as skipped.
        for (const Job *Cmd : DeferredCommands) {
          if (Comp.Level == OutputLevel::Parseable) {
            // Provide output indicating this command was skipped if parseable output
            // was requested.
            parseable_output::emitSkippedMessage(llvm::errs(), *Cmd);
          }

          ScheduledCommands.insert(Cmd);
          markFinished(Cmd, /*Skipped=*/true);
        }
        DeferredCommands.clear();

        // ...which may allow us to go on and do later tasks.
      } while (Result == 0 && TQ->hasRemainingTasks());
    }

    void checkUnfinishedJobs() {
      if (Result == 0) {
        assert(BlockingCommands.empty() &&
               "some blocking commands never finished properly");
      } else {
        // Make sure we record any files that still need to be rebuilt.
        for (const Job *Cmd : Comp.getJobs()) {
          // Skip files that don't use dependency analysis.
          StringRef DependenciesFile =
            Cmd->getOutput().getAdditionalOutputForType(types::TY_SwiftDeps);
          if (DependenciesFile.empty())
            continue;

          // Don't worry about commands that finished or weren't going to run.
          if (FinishedCommands.count(Cmd))
            continue;
          if (!ScheduledCommands.count(Cmd))
            continue;

          bool isCascading = true;
          if (Comp.getIncrementalBuildEnabled())
            isCascading = DepGraph.isMarked(Cmd);
          UnfinishedCommands.insert({Cmd, isCascading});
        }
      }
    }

    void populateInputInfoMap(InputInfoMap &inputs) const {
      for (auto &entry : UnfinishedCommands) {
        for (auto *action : entry.first->getSource().getInputs()) {
          auto inputFile = dyn_cast<InputAction>(action);
          if (!inputFile)
            continue;

          CompileJobAction::InputInfo info;
          info.previousModTime = entry.first->getInputModTime();
          info.status = entry.second ?
            CompileJobAction::InputInfo::NeedsCascadingBuild :
            CompileJobAction::InputInfo::NeedsNonCascadingBuild;
          inputs[&inputFile->getInputArg()] = info;
        }
      }

      for (const Job *entry : FinishedCommands) {
        const auto *compileAction = dyn_cast<CompileJobAction>(&entry->getSource());
        if (!compileAction)
          continue;

        for (auto *action : compileAction->getInputs()) {
          auto inputFile = dyn_cast<InputAction>(action);
          if (!inputFile)
            continue;

          CompileJobAction::InputInfo info;
          info.previousModTime = entry->getInputModTime();
          info.status = CompileJobAction::InputInfo::UpToDate;
          inputs[&inputFile->getInputArg()] = info;
        }
      }

      // Sort the entries by input order.
      static_assert(IsTriviallyCopyable<CompileJobAction::InputInfo>::value,
                    "llvm::array_pod_sort relies on trivially-copyable data");
      using InputInfoEntry = std::decay<decltype(inputs.front())>::type;
      llvm::array_pod_sort(inputs.begin(), inputs.end(),
                           [](const InputInfoEntry *lhs,
                              const InputInfoEntry *rhs) -> int {
                             auto lhsIndex = lhs->first->getIndex();
                             auto rhsIndex = rhs->first->getIndex();
                             return (lhsIndex < rhsIndex) ? -1 : (lhsIndex > rhsIndex) ? 1 : 0;
                           });
    }

    int getResult() {
      if (Result == 0)
        Result = Comp.Diags.hadAnyError();
      return Result;
    }
  };
} // driver
} // swift

Compilation::~Compilation() = default;

Job *Compilation::addJob(std::unique_ptr<Job> J) {
  Job *result = J.get();
  Jobs.emplace_back(std::move(J));
  return result;
}


static void checkForOutOfDateInputs(DiagnosticEngine &diags,
                                    const InputInfoMap &inputs) {
  for (const auto &inputPair : inputs) {
    auto recordedModTime = inputPair.second.previousModTime;
    if (recordedModTime == llvm::sys::TimePoint<>::max())
      continue;

    const char *input = inputPair.first->getValue();

    llvm::sys::fs::file_status inputStatus;
    if (auto statError = llvm::sys::fs::status(input, inputStatus)) {
      diags.diagnose(SourceLoc(), diag::warn_cannot_stat_input,
                     llvm::sys::path::filename(input), statError.message());
      continue;
    }

    if (recordedModTime != inputStatus.getLastModificationTime()) {
      diags.diagnose(SourceLoc(), diag::error_input_changed_during_build,
                     llvm::sys::path::filename(input));
    }
  }
}

static void writeCompilationRecord(StringRef path, StringRef argsHash,
                                   llvm::sys::TimePoint<> buildTime,
                                   const InputInfoMap &inputs) {
  // Before writing to the dependencies file path, preserve any previous file
  // that may have been there. No error handling -- this is just a nicety, it
  // doesn't matter if it fails.
  llvm::sys::fs::rename(path, path + "~");

  std::error_code error;
  llvm::raw_fd_ostream out(path, error, llvm::sys::fs::F_None);
  if (out.has_error()) {
    // FIXME: How should we report this error?
    out.clear_error();
    return;
  }

  auto writeTimeValue = [](llvm::raw_ostream &out,
                           llvm::sys::TimePoint<> time) {
    using namespace std::chrono;
    auto secs = time_point_cast<seconds>(time);
    time -= secs.time_since_epoch(); // remainder in nanoseconds
    out << "[" << secs.time_since_epoch().count()
        << ", " << time.time_since_epoch().count() << "]";
  };

  using compilation_record::TopLevelKey;
  // NB: We calculate effective version from getCurrentLanguageVersion()
  // here because any -swift-version argument is handled in the
  // argsHash that follows.
  out << compilation_record::getName(TopLevelKey::Version) << ": \""
      << llvm::yaml::escape(version::getSwiftFullVersion(
                              swift::version::Version::getCurrentLanguageVersion()))
      << "\"\n";
  out << compilation_record::getName(TopLevelKey::Options) << ": \""
      << llvm::yaml::escape(argsHash) << "\"\n";
  out << compilation_record::getName(TopLevelKey::BuildTime) << ": ";
  writeTimeValue(out, buildTime);
  out << "\n";
  out << compilation_record::getName(TopLevelKey::Inputs) << ":\n";

  for (auto &entry : inputs) {
    out << "  \"" << llvm::yaml::escape(entry.first->getValue()) << "\": ";

    using compilation_record::getIdentifierForInputInfoStatus;
    auto Name = getIdentifierForInputInfoStatus(entry.second.status);
    if (!Name.empty()) {
      out << Name << " ";
    }

    writeTimeValue(out, entry.second.previousModTime);
    out << "\n";
  }
}

static bool writeFilelistIfNecessary(const Job *job, DiagnosticEngine &diags) {
  FilelistInfo filelistInfo = job->getFilelistInfo();
  if (filelistInfo.path.empty())
    return true;

  std::error_code error;
  llvm::raw_fd_ostream out(filelistInfo.path, error, llvm::sys::fs::F_None);
  if (out.has_error()) {
    out.clear_error();
    diags.diagnose(SourceLoc(), diag::error_unable_to_make_temporary_file,
                   error.message());
    return false;
  }

  if (filelistInfo.whichFiles == FilelistInfo::Input) {
    // FIXME: Duplicated from ToolChains.cpp.
    for (const Job *input : job->getInputs()) {
      const CommandOutput &outputInfo = input->getOutput();
      if (outputInfo.getPrimaryOutputType() == filelistInfo.type) {
        for (auto &output : outputInfo.getPrimaryOutputFilenames())
          out << output << "\n";
      } else {
        auto &output = outputInfo.getAnyOutputForType(filelistInfo.type);
        if (!output.empty())
          out << output << "\n";
      }
    }
  } else {
    const CommandOutput &outputInfo = job->getOutput();
    assert(outputInfo.getPrimaryOutputType() == filelistInfo.type);
    for (auto &output : outputInfo.getPrimaryOutputFilenames())
      out << output << "\n";
  }

  return true;
}

int Compilation::performJobsImpl() {

  PerformJobsState State(*this);

  State.scheduleInitialJobs();
  State.scheduleAdditionalJobs();
  State.runTaskQueueToCompletion();
  State.checkUnfinishedJobs();

  if (!CompilationRecordPath.empty() && !SkipTaskExecution) {
    InputInfoMap InputInfo;
    State.populateInputInfoMap(InputInfo);
    checkForOutOfDateInputs(Diags, InputInfo);
    writeCompilationRecord(CompilationRecordPath, ArgsHash, BuildStartTime,
                           InputInfo);
  }

  return State.getResult();
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

  if (!writeFilelistIfNecessary(Cmd, Diags))
    return 1;

  if (Level == OutputLevel::Verbose)
    Cmd->printCommandLine(llvm::errs());

  SmallVector<const char *, 128> Argv;
  Argv.push_back(Cmd->getExecutable());
  Argv.append(Cmd->getArguments().begin(), Cmd->getArguments().end());
  Argv.push_back(nullptr);

  const char *ExecPath = Cmd->getExecutable();
  const char **argv = Argv.data();

  for (auto &envPair : Cmd->getExtraEnvironment()) {
#if defined(_MSC_VER)
    int envResult =_putenv_s(envPair.first, envPair.second);
#else
    int envResult = setenv(envPair.first, envPair.second, /*replacing=*/true);
#endif
    assert(envResult == 0 &&
          "expected environment variable to be set successfully");
    // Bail out early in release builds.
    if (envResult != 0) {
      return envResult;
    }
  }

  return ExecuteInPlace(ExecPath, argv);
}

static bool writeAllSourcesFile(DiagnosticEngine &diags, StringRef path,
                                ArrayRef<InputPair> inputFiles) {
  std::error_code error;
  llvm::raw_fd_ostream out(path, error, llvm::sys::fs::F_None);
  if (out.has_error()) {
    out.clear_error();
    diags.diagnose(SourceLoc(), diag::error_unable_to_make_temporary_file,
                   error.message());
    return false;
  }

  for (auto inputPair : inputFiles) {
    if (!types::isPartOfSwiftCompilation(inputPair.first))
      continue;
    out << inputPair.second->getValue() << "\n";
  }

  return true;
}

int Compilation::performJobs() {
  if (AllSourceFilesPath)
    if (!writeAllSourcesFile(Diags, AllSourceFilesPath, getInputFiles()))
      return EXIT_FAILURE;

  // If we don't have to do any cleanup work, just exec the subprocess.
  if (Level < OutputLevel::Parseable &&
      !ShowDriverTimeCompilation &&
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

const char *Compilation::getAllSourcesPath() const {
  if (!AllSourceFilesPath) {
    SmallString<128> Buffer;
    std::error_code EC =
        llvm::sys::fs::createTemporaryFile("sources", "", Buffer);
    if (EC) {
      Diags.diagnose(SourceLoc(),
                     diag::error_unable_to_make_temporary_file,
                     EC.message());
      // FIXME: This should not take down the entire process.
      llvm::report_fatal_error("unable to create list of input sources");
    }
    auto *mutableThis = const_cast<Compilation *>(this);
    mutableThis->addTemporaryFile(Buffer.str());
    mutableThis->AllSourceFilesPath = getArgs().MakeArgString(Buffer);
  }
  return AllSourceFilesPath;
}
