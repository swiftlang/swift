//===--- Compilation.cpp - Compilation Task Data Structure ----------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/Driver/Compilation.h"

#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/DiagnosticsDriver.h"
#include "swift/AST/FineGrainedDependencies.h"
#include "swift/AST/FineGrainedDependencyFormat.h"
#include "swift/Basic/Assertions.h"
#include "swift/Basic/OutputFileMap.h"
#include "swift/Basic/ParseableOutput.h"
#include "swift/Basic/Program.h"
#include "swift/Basic/STLExtras.h"
#include "swift/Basic/Statistic.h"
#include "swift/Basic/TaskQueue.h"
#include "swift/Basic/Version.h"
#include "swift/Basic/type_traits.h"
#include "swift/Driver/Action.h"
#include "swift/Driver/Driver.h"
#include "swift/Driver/Job.h"
#include "swift/Driver/ToolChain.h"
#include "swift/Option/Options.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/MapVector.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/ADT/TinyPtrVector.h"
#include "llvm/Option/Arg.h"
#include "llvm/Option/ArgList.h"
#include "llvm/Support/Chrono.h"
#include "llvm/Support/Compiler.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/Timer.h"
#include "llvm/Support/YAMLParser.h"
#include "llvm/Support/raw_ostream.h"

#include <fstream>
#include <signal.h>
#if defined(_WIN32)
#include <fcntl.h>
#include <io.h>
#endif

using namespace swift;
using namespace swift::sys;
using namespace swift::driver;
using namespace swift::parseable_output;
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

// clang-format off
Compilation::Compilation(DiagnosticEngine &Diags,
                         const ToolChain &TC,
                         OutputInfo const &OI,
                         OutputLevel Level,
                         std::unique_ptr<InputArgList> InputArgs,
                         std::unique_ptr<DerivedArgList> TranslatedArgs,
                         InputFileList InputsWithTypes,
                         size_t FilelistThreshold,
                         bool SaveTemps,
                         bool ShowDriverTimeCompilation,
                         std::unique_ptr<UnifiedStatsReporter> StatsReporter,
                         bool OnlyOneDependencyFile)
  : Diags(Diags), TheToolChain(TC),
    TheOutputInfo(OI),
    Level(Level),
    RawInputArgs(std::move(InputArgs)),
    TranslatedArgs(std::move(TranslatedArgs)),
    InputFilesWithTypes(std::move(InputsWithTypes)),
    SaveTemps(SaveTemps),
    ShowDriverTimeCompilation(ShowDriverTimeCompilation),
    Stats(std::move(StatsReporter)),
    FilelistThreshold(FilelistThreshold),
    OnlyOneDependencyFile(OnlyOneDependencyFile)    { }
// clang-format on

static bool writeFilelistIfNecessary(const Job *job, const ArgList &args,
                                     DiagnosticEngine &diags);

using CommandSetVector = llvm::SetVector<const Job*>;

namespace {
static DetailedTaskDescription
constructDetailedTaskDescription(const driver::Job &Cmd) {
  std::string Executable = Cmd.getExecutable();
  SmallVector<std::string, 16> Arguments;
  std::string CommandLine;
  SmallVector<CommandInput, 4> Inputs;
  SmallVector<OutputPair, 8> Outputs;
  for (const auto &A : Cmd.getArguments()) {
    Arguments.push_back(A);
  }
  llvm::raw_string_ostream wrapper(CommandLine);
  Cmd.printCommandLine(wrapper, "");
  wrapper.flush();

  for (const Action *A : Cmd.getSource().getInputs()) {
    if (const auto *IA = dyn_cast<InputAction>(A))
      Inputs.push_back(CommandInput(IA->getInputArg().getValue()));
  }

  for (const driver::Job *J : Cmd.getInputs()) {
    auto OutFiles = J->getOutput().getPrimaryOutputFilenames();
    if (const auto *BJAction = dyn_cast<BackendJobAction>(&Cmd.getSource())) {
      Inputs.push_back(CommandInput(OutFiles[BJAction->getInputIndex()]));
    } else {
      for (llvm::StringRef FileName : OutFiles) {
        Inputs.push_back(CommandInput(FileName));
      }
    }
  }

  // TODO: set up Outputs appropriately.
  file_types::ID PrimaryOutputType = Cmd.getOutput().getPrimaryOutputType();
  if (PrimaryOutputType != file_types::TY_Nothing) {
    for (llvm::StringRef OutputFileName :
         Cmd.getOutput().getPrimaryOutputFilenames()) {
      Outputs.push_back(OutputPair(PrimaryOutputType, OutputFileName.str()));
    }
  }
  file_types::forAllTypes([&](file_types::ID Ty) {
    for (auto Output : Cmd.getOutput().getAdditionalOutputsForType(Ty)) {
      Outputs.push_back(OutputPair(Ty, Output.str()));
    }
  });

  return DetailedTaskDescription{Executable, Arguments, CommandLine, Inputs,
                                 Outputs};
}
} // namespace

namespace swift {
namespace driver {
  class PerformJobsState {

    /// The containing Compilation object.
    Compilation &Comp;

    /// All jobs which have been scheduled for execution (whether or not
    /// they've finished execution), or which have been determined that they
    /// don't need to run.
    CommandSet ScheduledCommands;

    /// A temporary buffer to hold commands that were scheduled but haven't been
    /// added to the Task Queue yet
    CommandSetVector PendingExecution;

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

  private:
    /// TaskQueue for execution.
    std::unique_ptr<TaskQueue> TQ;

    /// Cumulative result of PerformJobs(), accumulated from subprocesses.
    int ResultCode = EXIT_SUCCESS;

    /// True if any Job crashed.
    bool AnyAbnormalExit = false;

    /// Timers for monitoring execution time of subprocesses.
    llvm::TimerGroup DriverTimerGroup {"driver", "Driver Compilation Time"};
    llvm::SmallDenseMap<const Job *, std::unique_ptr<llvm::Timer>, 16>
    DriverTimers;

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
        if (Comp.getShowJobLifecycle()) {
          llvm::outs() << "Already scheduled: " << LogJob(Cmd) << "\n";
        }
        return;
      }

      if (auto Blocking = findUnfinishedJob(Cmd->getInputs())) {
        BlockingCommands[Blocking].push_back(Cmd);
        if (Comp.getShowJobLifecycle()) {
          llvm::outs() << "Blocked by: " << LogJob(Blocking)
                       << ", now blocking jobs: "
                       << LogJobArray(BlockingCommands[Blocking]) << "\n";
        }
        return;
      }

      // Adding to scheduled means we've committed to its completion (not
      // distinguished from skipping). We never remove it once inserted.
      ScheduledCommands.insert(Cmd);

      // Adding to pending means it should be in the next round of additions to
      // the task queue; we remove Jobs from
      // PendingExecution once we hand them over to the TaskQueue.
      PendingExecution.insert(Cmd);
    }

    // Sort for ease of testing
    template <typename Jobs>
    void scheduleCommandsInSortedOrder(const Jobs &jobs) {
      llvm::SmallVector<const Job *, 16> sortedJobs;
      Comp.sortJobsToMatchCompilationInputs(jobs, sortedJobs);
      for (const Job *Cmd : sortedJobs)
        scheduleCommandIfNecessaryAndPossible(Cmd);
    }

    void addPendingJobToTaskQueue(const Job *Cmd) {
      // FIXME: Failing here should not take down the whole process.
      bool success =
          writeFilelistIfNecessary(Cmd, Comp.getArgs(), Comp.getDiags());
      assert(success && "failed to write filelist");
      (void)success;

      assert(Cmd->getExtraEnvironment().empty() &&
             "not implemented for compilations with multiple jobs");
      if (Comp.getShowJobLifecycle())
        llvm::outs() << "Added to TaskQueue: " << LogJob(Cmd) << "\n";
      TQ->addTask(Cmd->getExecutable(), Cmd->getArgumentsForTaskExecution(), {},
                  (void *)Cmd);
    }

    /// When a task finishes, check other Jobs that may be blocked.
    void markFinished(const Job *Cmd, bool Skipped=false) {
      if (Comp.getShowJobLifecycle()) {
        llvm::outs() << "Job "
                     << (Skipped ? "skipped" : "finished")
                     << ": " << LogJob(Cmd) << "\n";
      }
      FinishedCommands.insert(Cmd);
      if (auto *Stats = Comp.getStatsReporter()) {
          auto &D = Stats->getDriverCounters();
          if (Skipped)
            ++D.NumDriverJobsSkipped;
          else
            ++D.NumDriverJobsRun;
      }
      auto BlockedIter = BlockingCommands.find(Cmd);
      if (BlockedIter != BlockingCommands.end()) {
        auto AllBlocked = std::move(BlockedIter->second);
        if (Comp.getShowJobLifecycle()) {
          llvm::outs() << "Scheduling maybe-unblocked jobs: "
                       << LogJobArray(AllBlocked) << "\n";
        }
        BlockingCommands.erase(BlockedIter);
        scheduleCommandsInSortedOrder(AllBlocked);
      }
    }

    /// Callback which will be called immediately after a task has started. This
    /// callback may be used to provide output indicating that the task began.
    void taskBegan(ProcessId Pid, void *Context) {
      // TODO: properly handle task began.
      const Job *BeganCmd = (const Job *)Context;

      if (Comp.getShowDriverTimeCompilation()) {
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

      switch (Comp.getOutputLevel()) {
      case OutputLevel::Normal:
        break;
        // For command line or verbose output, print out each command as it
        // begins execution.
      case OutputLevel::PrintJobs:
        BeganCmd->printCommandLineAndEnvironment(llvm::outs());
        break;
      case OutputLevel::Verbose:
        BeganCmd->printCommandLine(llvm::errs());
        break;
      case OutputLevel::Parseable:
        BeganCmd->forEachContainedJobAndPID(Pid, [&](const Job *J, Job::PID P) {
          auto TascDesc = constructDetailedTaskDescription(*J);
          parseable_output::emitBeganMessage(llvm::errs(),
                                             J->getSource().getClassName(),
                                             TascDesc, P,
                                             TaskProcessInformation(Pid));
        });
        break;
      }
    }

    void
    emitParseableOutputForEachFinishedJob(ProcessId Pid, int ReturnCode,
                                          StringRef Output,
                                          const Job *FinishedCmd,
                                          TaskProcessInformation ProcInfo) {
      FinishedCmd->forEachContainedJobAndPID(Pid, [&](const Job *J,
                                                      Job::PID P) {
          parseable_output::emitFinishedMessage(llvm::errs(),
                                                J->getSource().getClassName(),
                                                Output.str(), ReturnCode,
                                                P, ProcInfo);
      });
    }

    /// Callback which will be called immediately after a task has finished
    /// execution. Determines if execution should continue, and also schedule
    /// any additional Jobs which we now know we need to run.
    TaskFinishedResponse taskFinished(ProcessId Pid, int ReturnCode,
                                      StringRef Output, StringRef Errors,
                                      TaskProcessInformation ProcInfo,
                                      void *Context) {
      const Job *const FinishedCmd = (const Job *)Context;

      if (Pid != llvm::sys::ProcessInfo::InvalidPid) {

        if (Comp.getShowDriverTimeCompilation()) {
          DriverTimers[FinishedCmd]->stopTimer();
        }
        processOutputOfFinishedProcess(Pid, ReturnCode, FinishedCmd, Output,
                                       ProcInfo);
      }

      if (Comp.getStatsReporter() && ProcInfo.getResourceUsage().has_value())
        Comp.getStatsReporter()->recordJobMaxRSS(
            ProcInfo.getResourceUsage()->Maxrss);

      if (ReturnCode != EXIT_SUCCESS)
        return taskFailed(FinishedCmd, ReturnCode);

      // When a task finishes, we need to reevaluate the other commands that
      // might have been blocked.
      markFinished(FinishedCmd);
      return TaskFinishedResponse::ContinueExecution;
    }

    TaskFinishedResponse taskFailed(const Job *FinishedCmd,
                                    const int ReturnCode) {
      // The task failed, so return true without performing any further
      // dependency analysis.

      // Store this task's ReturnCode as our Result if we haven't stored
      // anything yet.

      if (ResultCode == EXIT_SUCCESS)
        ResultCode = ReturnCode;

      if (!isa<CompileJobAction>(FinishedCmd->getSource()) ||
          ReturnCode != EXIT_FAILURE) {
        Comp.getDiags().diagnose(SourceLoc(), diag::error_command_failed,
                                 FinishedCmd->getSource().getClassName(),
                                 ReturnCode);
      }

      return Comp.getContinueBuildingAfterErrors()
                 ? TaskFinishedResponse::ContinueExecution
                 : TaskFinishedResponse::StopExecution;
    }

#if defined(_WIN32)
    struct FileBinaryModeRAII {
      FileBinaryModeRAII(FILE *F) : F(F) {
        PrevMode = _setmode(_fileno(F), _O_BINARY);
      }
      ~FileBinaryModeRAII() {
        _setmode(_fileno(F), PrevMode);
      }
      FILE *F;
      int PrevMode;
    };
#else
    struct FileBinaryModeRAII {
      FileBinaryModeRAII(FILE *) {}
    };
#endif

    void processOutputOfFinishedProcess(ProcessId Pid, int ReturnCode,
                                        const Job *const FinishedCmd,
                                        StringRef Output,
                                        TaskProcessInformation ProcInfo) {
      switch (Comp.getOutputLevel()) {
      case OutputLevel::PrintJobs:
        // Only print the jobs, not the outputs
        break;
      case OutputLevel::Normal:
      case OutputLevel::Verbose:
        // Send the buffered output to stderr, though only if we
        // support getting buffered output.
        if (TaskQueue::supportsBufferingOutput()) {
          // Temporarily change stderr to binary mode to avoid double
          // LF -> CR LF conversions on the outputs from child
          // processes, which have already this conversion appplied.
          // This makes a difference only for Windows.
          FileBinaryModeRAII F(stderr);
          llvm::errs() << Output;
        }
        break;
      case OutputLevel::Parseable:
        emitParseableOutputForEachFinishedJob(Pid, ReturnCode, Output,
                                              FinishedCmd, ProcInfo);
        break;
      }
    }

    TaskFinishedResponse taskSignalled(ProcessId Pid, StringRef ErrorMsg,
                                       StringRef Output, StringRef Errors,
                                       void *Context, std::optional<int> Signal,
                                       TaskProcessInformation ProcInfo) {
      const Job *SignalledCmd = (const Job *)Context;

      if (Comp.getShowDriverTimeCompilation()) {
        DriverTimers[SignalledCmd]->stopTimer();
      }

      if (Comp.getOutputLevel() == OutputLevel::Parseable) {
        // Parseable output was requested.
        SignalledCmd->forEachContainedJobAndPID(Pid, [&](const Job *J,
                                                         Job::PID P) {
          parseable_output::emitSignalledMessage(llvm::errs(),
                                                 J->getSource().getClassName(),
                                                 ErrorMsg, Output, Signal, P,
                                                 ProcInfo);
        });
      } else {
        // Otherwise, send the buffered output to stderr, though only if we
        // support getting buffered output.
        if (TaskQueue::supportsBufferingOutput())
          llvm::errs() << Output;
      }

      if (Comp.getStatsReporter() && ProcInfo.getResourceUsage().has_value())
        Comp.getStatsReporter()->recordJobMaxRSS(
            ProcInfo.getResourceUsage()->Maxrss);

      if (!ErrorMsg.empty())
        Comp.getDiags().diagnose(SourceLoc(),
                                 diag::error_unable_to_execute_command,
                                 ErrorMsg);

      if (Signal.has_value()) {
        Comp.getDiags().diagnose(SourceLoc(), diag::error_command_signalled,
                                 SignalledCmd->getSource().getClassName(),
                                 Signal.value());
      } else {
        Comp.getDiags()
            .diagnose(SourceLoc(),
                      diag::error_command_signalled_without_signal_number,
                      SignalledCmd->getSource().getClassName());
      }

      // Since the task signalled, unconditionally set result to -2.
      ResultCode = -2;
      AnyAbnormalExit = true;

      return TaskFinishedResponse::StopExecution;
    }

  public:
    PerformJobsState(Compilation &Comp, std::unique_ptr<TaskQueue> &&TaskQueue)
        : Comp(Comp),
          TQ(std::move(TaskQueue)) {}

    /// Schedule and run initial, additional, and single-file jobs.
    void runJobs() {
      scheduleJobsForNonIncrementalCompilation();
      addPendingJobsToTaskQueue();
      runTaskQueueToCompletion();
    }

  private:

    void scheduleJobsForNonIncrementalCompilation() {
      for (const Job *Cmd : Comp.getJobs())
        scheduleCommandIfNecessaryAndPossible(Cmd);
    }

    /// Insert all jobs in \p Cmds (of descriptive name \p Kind) to the \c
    /// TaskQueue, and clear \p Cmds.
    template <typename Container>
    void transferJobsToTaskQueue(Container &Cmds, StringRef Kind) {
      for (const Job *Cmd : Cmds) {
        if (Comp.getShowJobLifecycle())
          llvm::outs() << "Adding " << Kind
                       << " job to task queue: "
                       << LogJob(Cmd) << "\n";
        addPendingJobToTaskQueue(Cmd);
      }
      Cmds.clear();
    }

    void addPendingJobsToTaskQueue() {
      transferJobsToTaskQueue(PendingExecution, "standard");
      return;
    }

    void runTaskQueueToCompletion() {
      do {
        using namespace std::placeholders;
        // Ask the TaskQueue to execute.
        if (TQ->execute(std::bind(&PerformJobsState::taskBegan, this, _1, _2),
                        std::bind(&PerformJobsState::taskFinished, this, _1, _2,
                                  _3, _4, _5, _6),
                        std::bind(&PerformJobsState::taskSignalled, this, _1,
                                  _2, _3, _4, _5, _6, _7))) {
          if (ResultCode == EXIT_SUCCESS) {
            // FIXME: Error from task queue while Result == EXIT_SUCCESS most
            // likely means some fork/exec or posix_spawn failed; TaskQueue saw
            // "an error" at some stage before even calling us with a process
            // exit / signal (or else a poll failed); unfortunately the task
            // causing it was dropped on the floor and we have no way to recover
            // it here, so we report a very poor, generic error.
            Comp.getDiags().diagnose(SourceLoc(),
                                     diag::error_unable_to_execute_command,
                                     "<unknown>");
            ResultCode = -2;
            AnyAbnormalExit = true;
            return;
          }
        }

        // Returning without error from TaskQueue::execute should mean either an
        // empty TaskQueue or a failed subprocess.
        assert(!(ResultCode == 0 && TQ->hasRemainingTasks()));

        // Task-exit callbacks from TaskQueue::execute may have unblocked jobs,
        // which means there might be PendingExecution jobs to enqueue here. If
        // there are, we need to continue trying to make progress on the
        // TaskQueue before we start marking deferred jobs as skipped, below.
        if (!PendingExecution.empty() && ResultCode == 0) {
          addPendingJobsToTaskQueue();
          continue;
        }

        // It's possible that by marking some jobs as skipped, we unblocked
        // some jobs and thus have entries in PendingExecution again; push
        // those through to the TaskQueue.
        addPendingJobsToTaskQueue();

        // If we added jobs to the TaskQueue, and we are not in an error state,
        // we want to give the TaskQueue another run.
      } while (ResultCode == 0 && TQ->hasRemainingTasks());
    }

  public:
    Compilation::Result takeResult() && {
      if (ResultCode == 0)
        ResultCode = Comp.getDiags().hadAnyError();
      const bool hadAbnormalExit = hadAnyAbnormalExit();
      const auto resultCode = ResultCode;
      return Compilation::Result{hadAbnormalExit, resultCode};
    }

    bool hadAnyAbnormalExit() {
      return AnyAbnormalExit;
    }
  };
} // namespace driver
} // namespace swift

Compilation::~Compilation() = default;

Job *Compilation::addJob(std::unique_ptr<Job> J) {
  Job *result = J.get();
  Jobs.emplace_back(std::move(J));
  return result;
}

Job *Compilation::addExternalJob(std::unique_ptr<Job> J) {
  Job *result = J.get();
  ExternalJobs.emplace_back(std::move(J));
  return result;
}

static void writeInputJobsToFilelist(llvm::raw_fd_ostream &out, const Job *job,
                                     const file_types::ID infoType) {
  // FIXME: Duplicated from ToolChains.cpp.
  for (const Job *input : job->getInputs()) {
    const CommandOutput &outputInfo = input->getOutput();
    if (outputInfo.getPrimaryOutputType() == infoType) {
      for (auto &output : outputInfo.getPrimaryOutputFilenames())
        out << output << "\n";
    } else {
      auto output = outputInfo.getAnyOutputForType(infoType);
      if (!output.empty())
        out << output << "\n";
    }
  }
}
static void writeSourceInputActionsToFilelist(llvm::raw_fd_ostream &out,
                                              const Job *job,
                                              const ArgList &args) {
  // Ensure that -index-file-path works in conjunction with
  // -driver-use-filelists. It needs to be the only primary.
  if (Arg *A = args.getLastArg(options::OPT_index_file_path))
    out << A->getValue() << "\n";
  else {
    // The normal case for non-single-compile jobs.
    for (const Action *A : job->getSource().getInputs()) {
      // A could be a GeneratePCHJobAction
      if (!isa<InputAction>(A))
        continue;
      const auto *IA = cast<InputAction>(A);
      out << IA->getInputArg().getValue() << "\n";
    }
  }
}
static void writeOutputToFilelist(llvm::raw_fd_ostream &out, const Job *job,
                                  const file_types::ID infoType) {
  const CommandOutput &outputInfo = job->getOutput();
  assert(outputInfo.getPrimaryOutputType() == infoType);
  for (auto &output : outputInfo.getPrimaryOutputFilenames())
    out << output << "\n";
}
static void writeIndexUnitOutputPathsToFilelist(llvm::raw_fd_ostream &out,
                                                const Job *job) {
  const CommandOutput &outputInfo = job->getOutput();
  for (auto &output : outputInfo.getIndexUnitOutputFilenames())
    out << output << "\n";
}
static void writeSupplementaryOutputToFilelist(llvm::raw_fd_ostream &out,
                                               const Job *job) {
  job->getOutput().writeOutputFileMap(out);
}

static bool writeFilelistIfNecessary(const Job *job, const ArgList &args,
                                     DiagnosticEngine &diags) {
  bool ok = true;
  for (const FilelistInfo &filelistInfo : job->getFilelistInfos()) {
    if (filelistInfo.path.empty())
      return true;

    std::error_code error;
    llvm::raw_fd_ostream out(filelistInfo.path, error, llvm::sys::fs::OF_None);
    if (out.has_error()) {
      out.clear_error();
      diags.diagnose(SourceLoc(), diag::error_unable_to_make_temporary_file,
                     error.message());
      ok = false;
      continue;
    }

    switch (filelistInfo.whichFiles) {
    case FilelistInfo::WhichFiles::InputJobs:
      writeInputJobsToFilelist(out, job, filelistInfo.type);
      break;
    case FilelistInfo::WhichFiles::SourceInputActions:
      writeSourceInputActionsToFilelist(out, job, args);
      break;
    case FilelistInfo::WhichFiles::InputJobsAndSourceInputActions:
      writeInputJobsToFilelist(out, job, filelistInfo.type);
      writeSourceInputActionsToFilelist(out, job, args);
      break;
    case FilelistInfo::WhichFiles::Output:
      writeOutputToFilelist(out, job, filelistInfo.type);
      break;
    case FilelistInfo::WhichFiles::IndexUnitOutputPaths:
      writeIndexUnitOutputPathsToFilelist(out, job);
      break;
    case FilelistInfo::WhichFiles::SupplementaryOutput:
      writeSupplementaryOutputToFilelist(out, job);
      break;
    }
  }
  return ok;
}

Compilation::Result
Compilation::performJobsImpl(std::unique_ptr<TaskQueue> &&TQ) {
  PerformJobsState State(*this, std::move(TQ));
  State.runJobs();
  return std::move(State).takeResult();
}

Compilation::Result Compilation::performSingleCommand(const Job *Cmd) {
  assert(Cmd->getInputs().empty() &&
         "This can only be used to run a single command with no inputs");

  switch (Cmd->getCondition()) {
  case Job::Condition::CheckDependencies:
    return Compilation::Result::code(0);
  case Job::Condition::RunWithoutCascading:
  case Job::Condition::Always:
  case Job::Condition::NewlyAdded:
    break;
  }

  if (!writeFilelistIfNecessary(Cmd, *TranslatedArgs.get(), Diags))
    return Compilation::Result::code(1);

  switch (Level) {
  case OutputLevel::Normal:
  case OutputLevel::Parseable:
    break;
  case OutputLevel::PrintJobs:
    Cmd->printCommandLineAndEnvironment(llvm::outs());
    return Compilation::Result::code(0);
  case OutputLevel::Verbose:
    Cmd->printCommandLine(llvm::errs());
    break;
  }

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
      return Compilation::Result::code(envResult);
    }
  }

  const auto returnCode = ExecuteInPlace(ExecPath, argv);
  return Compilation::Result::code(returnCode);
}

static bool writeAllSourcesFile(DiagnosticEngine &diags, StringRef path,
                                ArrayRef<InputPair> inputFiles) {
  std::error_code error;
  llvm::raw_fd_ostream out(path, error, llvm::sys::fs::OF_None);
  if (out.has_error()) {
    out.clear_error();
    diags.diagnose(SourceLoc(), diag::error_unable_to_make_temporary_file,
                   error.message());
    return false;
  }

  for (auto inputPair : inputFiles) {
    if (!file_types::isPartOfSwiftCompilation(inputPair.first))
      continue;
    out << inputPair.second->getValue() << "\n";
  }

  return true;
}

Compilation::Result Compilation::performJobs(std::unique_ptr<TaskQueue> &&TQ) {
  if (AllSourceFilesPath)
    if (!writeAllSourcesFile(Diags, AllSourceFilesPath, getInputFiles()))
      return Compilation::Result::code(EXIT_FAILURE);

  // If we don't have to do any cleanup work, just exec the subprocess.
  if (Level < OutputLevel::Parseable &&
      !ShowDriverTimeCompilation &&
      (SaveTemps || TempFilePaths.empty()) &&
      Jobs.size() == 1) {
    return performSingleCommand(Jobs.front().get());
  }

  if (!TaskQueue::supportsParallelExecution() && TQ->getNumberOfParallelTasks() > 1) {
    Diags.diagnose(SourceLoc(), diag::warning_parallel_execution_not_supported);
  }

  auto result = performJobsImpl(std::move(TQ));

  if (!SaveTemps) {
    for (const auto &pathPair : TempFilePaths) {
      if (!result.hadAbnormalExit || pathPair.getValue() == PreserveOnSignal::No)
        (void)llvm::sys::fs::remove(pathPair.getKey());
    }
  }
  if (Stats)
    Stats->noteCurrentProcessExitStatus(result.exitCode);
  return result;
}

const char *Compilation::getAllSourcesPath() const {
  if (!AllSourceFilesPath) {
    SmallString<128> Buffer;
    std::error_code EC =
        llvm::sys::fs::createTemporaryFile("sources", "", Buffer);
    if (EC) {
      // Use the constructor that prints both the error code and the
      // description.
      // FIXME: This should not take down the entire process.
      auto error = llvm::make_error<llvm::StringError>(
          EC,
          "- unable to create list of input sources");
      llvm::report_fatal_error(std::move(error));
    }
    auto *mutableThis = const_cast<Compilation *>(this);
    mutableThis->addTemporaryFile(Buffer.str(), PreserveOnSignal::Yes);
    mutableThis->AllSourceFilesPath = getArgs().MakeArgString(Buffer);
  }
  return AllSourceFilesPath;
}

unsigned Compilation::countSwiftInputs() const {
  unsigned inputCount = 0;
  for (const auto &p : InputFilesWithTypes)
    if (p.first == file_types::TY_Swift)
      ++inputCount;
  return inputCount;
}

void Compilation::addDependencyPathOrCreateDummy(
    StringRef depPath, function_ref<void()> addDependencyPath) {

  if (!OnlyOneDependencyFile) {
    addDependencyPath();
    return;
  }
  if (!HaveAlreadyAddedDependencyPath) {
    addDependencyPath();
    HaveAlreadyAddedDependencyPath = true;
  } else if (!depPath.empty()) {
    // Create dummy empty file
    std::error_code EC;
    llvm::raw_fd_ostream(depPath, EC, llvm::sys::fs::OF_None);
  }
}

template <typename JobCollection>
void Compilation::sortJobsToMatchCompilationInputs(
    const JobCollection &unsortedJobs,
    SmallVectorImpl<const Job *> &sortedJobs) const {
  llvm::DenseMap<StringRef, const Job *> jobsByInput;
  for (const Job *J : unsortedJobs) {
    // Only worry about sorting compilation jobs
    if (const CompileJobAction *CJA =
            dyn_cast<CompileJobAction>(&J->getSource())) {
      const InputAction *IA = CJA->findSingleSwiftInput();
      jobsByInput.insert(std::make_pair(IA->getInputArg().getValue(), J));
    } else
      sortedJobs.push_back(J);
  }
  for (const InputPair &P : getInputFiles()) {
    auto I = jobsByInput.find(P.second->getValue());
    if (I != jobsByInput.end()) {
      sortedJobs.push_back(I->second);
    }
  }
}

template void
Compilation::sortJobsToMatchCompilationInputs<ArrayRef<const Job *>>(
    const ArrayRef<const Job *> &,
    SmallVectorImpl<const Job *> &sortedJobs) const;
