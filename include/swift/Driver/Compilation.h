//===--- Compilation.h - Compilation Task Data Structure --------*- C++ -*-===//
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
//
// TODO: Document me
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_DRIVER_COMPILATION_H
#define SWIFT_DRIVER_COMPILATION_H

#include "swift/Basic/ArrayRefView.h"
#include "swift/Basic/LLVM.h"
#include "swift/Basic/Statistic.h"
#include "swift/Driver/Driver.h"
#include "swift/Driver/Job.h"
#include "swift/Driver/Util.h"
#include "swift/Frontend/OutputFileMap.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/Chrono.h"

#include <memory>
#include <vector>

namespace llvm {
namespace opt {
  class InputArgList;
  class DerivedArgList;
}
}

namespace swift {
  class DiagnosticEngine;
  namespace sys {
    class TaskQueue;
  }

namespace driver {
  class Driver;
  class ToolChain;
  class OutputInfo;
  class PerformJobsState;

/// An enum providing different levels of output which should be produced
/// by a Compilation.
enum class OutputLevel {
  /// Indicates that normal output should be produced.
  Normal,
  
  /// Indicates that only jobs should be printed and not run. (-###)
  PrintJobs,

  /// Indicates that verbose output should be produced. (-v)
  Verbose,

  /// Indicates that parseable output should be produced.
  Parseable,
};

/// Indicates whether a temporary file should always be preserved if a part of
/// the compilation crashes.
enum class PreserveOnSignal : bool {
  No,
  Yes
};

class Compilation {
public:
  /// The filelist threshold value to pass to ensure file lists are never used
  static const size_t NEVER_USE_FILELIST = SIZE_MAX;

private:
  /// The DiagnosticEngine to which this Compilation should emit diagnostics.
  DiagnosticEngine &Diags;

  /// The ToolChain this Compilation was built with, that it may reuse to build
  /// subsequent BatchJobs.
  const ToolChain &TheToolChain;

  /// The OutputInfo, which the Compilation stores a copy of upon
  /// construction, and which it may use to build subsequent batch
  /// jobs itself.
  OutputInfo TheOutputInfo;

  /// The OutputLevel at which this Compilation should generate output.
  OutputLevel Level;

  /// The OutputFileMap describing the Compilation's outputs, populated both by
  /// the user-provided output file map (if it exists) and inference rules that
  /// derive otherwise-unspecified output filenames from context.
  OutputFileMap DerivedOutputFileMap;

  /// The Actions which were used to build the Jobs.
  ///
  /// This is mostly only here for lifetime management.
  SmallVector<std::unique_ptr<const Action>, 32> Actions;

  /// The Jobs which will be performed by this compilation.
  SmallVector<std::unique_ptr<const Job>, 32> Jobs;

  /// The original (untranslated) input argument list.
  ///
  /// This is only here for lifetime management. Any inspection of
  /// command-line arguments should use #getArgs().
  std::unique_ptr<llvm::opt::InputArgList> RawInputArgs;

  /// The translated input arg list.
  std::unique_ptr<llvm::opt::DerivedArgList> TranslatedArgs;

  /// A list of input files and their associated types.
  InputFileList InputFilesWithTypes;

  /// When non-null, a temporary file containing all input .swift files.
  /// Used for large compilations to avoid overflowing argv.
  const char *AllSourceFilesPath = nullptr;

  /// Temporary files that should be cleaned up after the compilation finishes.
  ///
  /// These apply whether the compilation succeeds or fails. If the
  llvm::StringMap<PreserveOnSignal> TempFilePaths;

  /// Write information about this compilation to this file.
  ///
  /// This is used for incremental builds.
  std::string CompilationRecordPath;

  /// A hash representing all the arguments that could trigger a full rebuild.
  std::string ArgsHash;

  /// When the build was started.
  ///
  /// This should be as close as possible to when the driver was invoked, since
  /// it's used as a lower bound.
  llvm::sys::TimePoint<> BuildStartTime;

  /// The time of the last build.
  ///
  /// If unknown, this will be some time in the past.
  llvm::sys::TimePoint<> LastBuildTime = llvm::sys::TimePoint<>::min();

  /// Indicates whether this Compilation should continue execution of subtasks
  /// even if they returned an error status.
  bool ContinueBuildingAfterErrors = false;

  /// Indicates whether tasks should only be executed if their output is out
  /// of date.
  bool EnableIncrementalBuild;

  /// When true, emit duplicated compilation record file whose filename is
  /// suffixed with '~moduleonly'.
  ///
  /// This compilation record is used by '-emit-module'-only incremental builds
  /// so that module-only builds do not affect compilation record file for
  /// normal builds, while module-only incremental builds are able to use
  /// artifacts of normal builds if they are already up to date.
  bool OutputCompilationRecordForModuleOnlyBuild = false;

  /// Indicates whether groups of parallel frontend jobs should be merged
  /// together and run in composite "batch jobs" when possible, to reduce
  /// redundant work.
  const bool EnableBatchMode;

  /// Provides a randomization seed to batch-mode partitioning, for debugging.
  const unsigned BatchSeed;

  /// Overrides parallelism level and \c BatchSizeLimit, sets exact
  /// count of batches, if in batch-mode.
  const Optional<unsigned> BatchCount;

  /// Overrides maximum batch size, if in batch-mode and not overridden
  /// by \c BatchCount.
  const Optional<unsigned> BatchSizeLimit;

  /// In order to test repartitioning, set to true if
  /// -driver-force-one-batch-repartition is present.
  const bool ForceOneBatchRepartition = false;

  /// True if temporary files should not be deleted.
  const bool SaveTemps;

  /// When true, dumps information on how long each compilation task took to
  /// execute.
  const bool ShowDriverTimeCompilation;

  /// When non-null, record various high-level counters to this.
  std::unique_ptr<UnifiedStatsReporter> Stats;

  /// When true, dumps information about why files are being scheduled to be
  /// rebuilt.
  bool ShowIncrementalBuildDecisions = false;

  /// When true, traces the lifecycle of each driver job. Provides finer
  /// detail than ShowIncrementalBuildDecisions.
  bool ShowJobLifecycle = false;

  /// When true, some frontend job has requested permission to pass
  /// -emit-loaded-module-trace, so no other job needs to do it.
  bool PassedEmitLoadedModuleTraceToFrontendJob = false;

  /// The limit for the number of files to pass on the command line. Beyond this
  /// limit filelists will be used.
  size_t FilelistThreshold;

  template <typename T>
  static T *unwrap(const std::unique_ptr<T> &p) {
    return p.get();
  }

  template <typename T>
  using UnwrappedArrayView =
      ArrayRefView<std::unique_ptr<T>, T *, Compilation::unwrap<T>>;

public:
  Compilation(DiagnosticEngine &Diags, const ToolChain &TC,
              OutputInfo const &OI,
              OutputLevel Level,
              std::unique_ptr<llvm::opt::InputArgList> InputArgs,
              std::unique_ptr<llvm::opt::DerivedArgList> TranslatedArgs,
              InputFileList InputsWithTypes,
              std::string CompilationRecordPath,
              bool OutputCompilationRecordForModuleOnlyBuild,
              StringRef ArgsHash, llvm::sys::TimePoint<> StartTime,
              llvm::sys::TimePoint<> LastBuildTime,
              size_t FilelistThreshold,
              bool EnableIncrementalBuild = false,
              bool EnableBatchMode = false,
              unsigned BatchSeed = 0,
              Optional<unsigned> BatchCount = None,
              Optional<unsigned> BatchSizeLimit = None,
              bool ForceOneBatchRepartition = false,
              bool SaveTemps = false,
              bool ShowDriverTimeCompilation = false,
              std::unique_ptr<UnifiedStatsReporter> Stats = nullptr);
  ~Compilation();

  ToolChain const &getToolChain() const {
    return TheToolChain;
  }

  OutputInfo const &getOutputInfo() const {
    return TheOutputInfo;
  }

  DiagnosticEngine &getDiags() const {
    return Diags;
  }

  UnwrappedArrayView<const Action> getActions() const {
    return llvm::makeArrayRef(Actions);
  }

  template <typename SpecificAction, typename... Args>
  SpecificAction *createAction(Args &&...args) {
    auto newAction = new SpecificAction(std::forward<Args>(args)...);
    Actions.emplace_back(newAction);
    return newAction;
  }

  UnwrappedArrayView<const Job> getJobs() const {
    return llvm::makeArrayRef(Jobs);
  }
  Job *addJob(std::unique_ptr<Job> J);

  void addTemporaryFile(StringRef file,
                        PreserveOnSignal preserve = PreserveOnSignal::No) {
    TempFilePaths[file] = preserve;
  }

  bool isTemporaryFile(StringRef file) {
    return TempFilePaths.count(file);
  }

  const llvm::opt::DerivedArgList &getArgs() const { return *TranslatedArgs; }
  ArrayRef<InputPair> getInputFiles() const { return InputFilesWithTypes; }

  OutputFileMap &getDerivedOutputFileMap() { return DerivedOutputFileMap; }
  const OutputFileMap &getDerivedOutputFileMap() const {
    return DerivedOutputFileMap;
  }

  bool getIncrementalBuildEnabled() const {
    return EnableIncrementalBuild;
  }
  void disableIncrementalBuild() {
    EnableIncrementalBuild = false;
  }

  bool getBatchModeEnabled() const {
    return EnableBatchMode;
  }

  bool getForceOneBatchRepartition() const { return ForceOneBatchRepartition; }

  bool getContinueBuildingAfterErrors() const {
    return ContinueBuildingAfterErrors;
  }
  void setContinueBuildingAfterErrors(bool Value = true) {
    ContinueBuildingAfterErrors = Value;
  }

  bool getShowIncrementalBuildDecisions() const {
    return ShowIncrementalBuildDecisions;
  }
  void setShowsIncrementalBuildDecisions(bool value = true) {
    ShowIncrementalBuildDecisions = value;
  }

  bool getShowJobLifecycle() const {
    return ShowJobLifecycle;
  }
  void setShowJobLifecycle(bool value = true) {
    ShowJobLifecycle = value;
  }

  bool getShowDriverTimeCompilation() const {
    return ShowDriverTimeCompilation;
  }

  size_t getFilelistThreshold() const {
    return FilelistThreshold;
  }

  UnifiedStatsReporter *getStatsReporter() const {
    return Stats.get();
  }

  OutputLevel getOutputLevel() const {
    return Level;
  }

  unsigned getBatchSeed() const {
    return BatchSeed;
  }

  llvm::sys::TimePoint<> getLastBuildTime() const {
    return LastBuildTime;
  }

  Optional<unsigned> getBatchCount() const {
    return BatchCount;
  }

  Optional<unsigned> getBatchSizeLimit() const {
    return BatchSizeLimit;
  }

  /// Requests the path to a file containing all input source files. This can
  /// be shared across jobs.
  ///
  /// If this is never called, the Compilation does not bother generating such
  /// a file.
  ///
  /// \sa types::isPartOfSwiftCompilation
  const char *getAllSourcesPath() const;

  /// Asks the Compilation to perform the Jobs which it knows about.
  ///
  /// \param TQ The TaskQueue used to schedule jobs for execution.
  ///
  /// \returns result code for the Compilation's Jobs; 0 indicates success and
  /// -2 indicates that one of the Compilation's Jobs crashed during execution
  int performJobs(std::unique_ptr<sys::TaskQueue> &&TQ);

  /// Returns whether the callee is permitted to pass -emit-loaded-module-trace
  /// to a frontend job.
  ///
  /// This only returns true once, because only one job should pass that
  /// argument.
  bool requestPermissionForFrontendToEmitLoadedModuleTrace() {
    if (PassedEmitLoadedModuleTraceToFrontendJob)
      // Someone else has already done it!
      return false;
    else {
      // We're the first and only (to execute this path).
      PassedEmitLoadedModuleTraceToFrontendJob = true;
      return true;
    }
  }

private:
  /// \brief Perform all jobs.
  ///
  /// \param[out] abnormalExit Set to true if any job exits abnormally (i.e.
  /// crashes).
  /// \param TQ The task queue on which jobs will be scheduled.
  ///
  /// \returns exit code of the first failed Job, or 0 on success. If a Job
  /// crashes during execution, a negative value will be returned.
  int performJobsImpl(bool &abnormalExit, std::unique_ptr<sys::TaskQueue> &&TQ);

  /// \brief Performs a single Job by executing in place, if possible.
  ///
  /// \param Cmd the Job which should be performed.
  ///
  /// \returns Typically, this function will not return, as the current process
  /// will no longer exist, or it will call exit() if the program was
  /// successfully executed. In the event of an error, this function will return
  /// a negative value indicating a failure to execute.
  int performSingleCommand(const Job *Cmd);
};

} // end namespace driver
} // end namespace swift

#endif
