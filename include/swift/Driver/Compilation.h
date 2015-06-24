//===--- Compilation.h - Compilation Task Data Structure --------*- C++ -*-===//
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
//
// TODO: Document me
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_DRIVER_COMPILATION_H
#define SWIFT_DRIVER_COMPILATION_H

#include "swift/Basic/LLVM.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/TimeValue.h"

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

namespace driver {
  class Driver;
  class Job;
  class JobList;
  class ToolChain;

/// An enum providing different levels of output which should be produced
/// by a Compilation.
enum class OutputLevel {
  /// Indicates that normal output should be produced.
  Normal,

  /// Indicates that verbose output should be produced. (-v)
  Verbose,

  /// Indicates that parseable output should be produced.
  Parseable,
};

class Compilation {
public:
  struct PerformJobsState;

private:
  /// The driver we were created by.
  const Driver &TheDriver;

  /// The default tool chain.
  const ToolChain &DefaultToolChain;

  /// The DiagnosticEngine to which this Compilation should emit diagnostics.
  DiagnosticEngine &Diags;

  /// The OutputLevel at which this Compilation should generate output.
  OutputLevel Level;

  /// The Jobs which will be performed by this compilation.
  std::unique_ptr<JobList> Jobs;

  /// The original (untranslated) input argument list.
  std::unique_ptr<llvm::opt::InputArgList> InputArgs;

  /// The translated input arg list.
  std::unique_ptr<llvm::opt::DerivedArgList> TranslatedArgs;

  /// Temporary files that should be cleaned up after the compilation finishes.
  ///
  /// These apply whether the compilation succeeds or fails.
  std::vector<std::string> TempFilePaths;

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
  llvm::sys::TimeValue BuildStartTime;

  /// The time of the last build.
  ///
  /// If unknown, this will be some time in the past.
  llvm::sys::TimeValue LastBuildTime = llvm::sys::TimeValue::MinTime();

  /// The number of commands which this compilation should attempt to run in
  /// parallel.
  unsigned NumberOfParallelCommands;

  /// Indicates whether this Compilation should use skip execution of
  /// subtasks during performJobs() by using a dummy TaskQueue.
  ///
  /// \note For testing purposes only; similar user-facing features should be
  /// implemented separately, as the dummy TaskQueue may provide faked output.
  bool SkipTaskExecution;

  /// Indicates whether this Compilation should continue execution of subtasks
  /// even if they returned an error status.
  bool ContinueBuildingAfterErrors = false;

  /// Indicates whether tasks should only be executed if their output is out
  /// of date.
  bool EnableIncrementalBuild;

  /// True if temporary files should not be deleted.
  bool SaveTemps;
  
public:
  Compilation(const Driver &D, const ToolChain &DefaultToolChain,
              DiagnosticEngine &Diags, OutputLevel Level,
              std::unique_ptr<llvm::opt::InputArgList> InputArgs,
              std::unique_ptr<llvm::opt::DerivedArgList> TranslatedArgs,
              StringRef ArgsHash, llvm::sys::TimeValue StartTime,
              unsigned NumberOfParallelCommands = 1,
              bool EnableIncrementalBuild = false,
              bool SkipTaskExecution = false,
              bool SaveTemps = false);
  ~Compilation();

  const Driver &getDriver() const { return TheDriver; }

  const ToolChain &getDefaultToolChain() const { return DefaultToolChain; }

  JobList &getJobs() const { return *Jobs; }
  void addJob(Job *J);
  void addTemporaryFile(StringRef file) {
    TempFilePaths.push_back(file.str());
  }

  bool isTemporaryFile(StringRef file) {
    // TODO: Use a set instead of a linear search.
    return std::find(TempFilePaths.begin(), TempFilePaths.end(), file) !=
             TempFilePaths.end();
  }

  const llvm::opt::InputArgList &getInputArgs() const { return *InputArgs; }

  const llvm::opt::DerivedArgList &getArgs() const { return *TranslatedArgs; }

  unsigned getNumberOfParallelCommands() const {
    return NumberOfParallelCommands;
  }

  bool getIncrementalBuildEnabled() const {
    return EnableIncrementalBuild;
  }
  void disableIncrementalBuild() {
    EnableIncrementalBuild = false;
  }
  
  bool getContinueBuildingAfterErrors() const {
    return ContinueBuildingAfterErrors;
  }
  void setContinueBuildingAfterErrors(bool Value = true) {
    ContinueBuildingAfterErrors = Value;
  }

  void setCompilationRecordPath(StringRef path) {
    assert(CompilationRecordPath.empty() && "already set");
    CompilationRecordPath = path;
  }

  void setLastBuildTime(llvm::sys::TimeValue time) {
    LastBuildTime = time;
  }

  /// Asks the Compilation to perform the Jobs which it knows about.
  /// \returns result code for the Compilation's Jobs; 0 indicates success and
  /// -2 indicates that one of the Compilation's Jobs crashed during execution
  int performJobs();

private:
  /// \brief Perform the Jobs in \p JL if necessary.
  ///
  /// \param JL the list of Jobs to perform
  /// \param State persistent state that crosses the entire compilation
  /// or which are known not to need to execute.
  ///
  /// \returns exit code of the first failed Job, or 0 on success. A return
  /// value of -2 indicates that a Job crashed during execution.
  int performJobsInList(const JobList &JL, PerformJobsState &State);

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
