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

#include "llvm/ADT/DenseSet.h"

#include <memory>

namespace llvm {
namespace opt {
  class InputArgList;
  class DerivedArgList;
}
}

namespace swift {
  class DiagnosticEngine;

namespace driver {
  class Command;
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
};

class Compilation {
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

  // The original (untranslated) input argument list.
  std::unique_ptr<llvm::opt::InputArgList> InputArgs;

  // The translated input arg list.
  std::unique_ptr<llvm::opt::DerivedArgList> TranslatedArgs;

  /// The number of commands which this compilation should attempt to run in
  /// parallel.
  unsigned NumberOfParallelCommands;

  /// \brief Indicates whether this Compilation should use skip execution of
  /// subtasks during performJobs() by using a dummy TaskQueue.
  ///
  /// \note For testing purposes only; similar user-facing features should be
  /// implemented separately, as the dummy TaskQueue may provide faked output.
  bool SkipTaskExecution;

public:
  Compilation(const Driver &D, const ToolChain &DefaultToolChain,
              DiagnosticEngine &Diags, OutputLevel Level,
              std::unique_ptr<llvm::opt::InputArgList> InputArgs,
              std::unique_ptr<llvm::opt::DerivedArgList> TranslatedArgs,
              unsigned NumberOfParallelCommands = 1,
              bool SkipTaskExecution = false);
  ~Compilation();

  const Driver &getDriver() const { return TheDriver; }

  const ToolChain &getDefaultToolChain() const { return DefaultToolChain; }

  JobList &getJobs() const { return *Jobs; }
  void addJob(Job *J);

  const llvm::opt::InputArgList &getInputArgs() const { return *InputArgs; }

  const llvm::opt::DerivedArgList &getArgs() const { return *TranslatedArgs; }

  unsigned getNumberOfParallelCommands() const {
    return NumberOfParallelCommands;
  }

  /// Asks the Compilation to perform the Jobs which it knows about.
  /// \returns result code for the Compilation's Jobs; 0 indicates success
  int performJobs();

private:
  /// \brief Perform the Jobs in \p JL if necessary.
  ///
  /// \param JL the list of Jobs to perform
  /// \param ScheduledCommands a set of Commands which have been previously
  /// scheduled
  /// \param FinishedCommands a set of Commands which have finished execution,
  /// or which are known not to need to execute.
  ///
  /// \returns exit code of the first failed Job, or 0 on success
  int performJobsInList(const JobList &JL,
                        llvm::DenseSet<const Command *> &ScheduledCommands,
                        llvm::DenseSet<const Command *> &FinishedCommands);
};

} // end namespace driver
} // end namespace swift

#endif
