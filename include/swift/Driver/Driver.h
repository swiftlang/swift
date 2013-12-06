//===-- Driver.h - Swift compiler driver -----------------------*- C++ -*--===//
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
// This file contains declarations of parts of the compiler driver.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_DRIVER_DRIVER_H
#define SWIFT_DRIVER_DRIVER_H

#include "swift/Basic/LLVM.h"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/Driver/Types.h"
#include "swift/Driver/Util.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/ADT/StringRef.h"

#include <string>
#include <memory>

namespace llvm {
namespace opt {
  class Arg;
  class ArgList;
  class OptTable;
  class InputArgList;
  class DerivedArgList;
}
}

namespace swift {
namespace driver {
  class Compilation;
  class Job;
  class JobList;
  class ToolChain;

class Driver {
  /// The default image name (e.g. "a.out")
  static const char *const DefaultImageName;
  
  std::unique_ptr<llvm::opt::OptTable> Opts;

  DiagnosticEngine &Diags;

  /// The name the driver was invoked as.
  std::string Name;

  /// The path the driver executable was in, as invoked from the command line.
  std::string Dir;

  /// The original path to the executable.
  std::string DriverExecutable;

  /// Default target triple.
  std::string DefaultTargetTriple;

  /// \brief Cache of all the ToolChains in use by the driver.
  ///
  /// This maps from the string representation of a triple to a ToolChain
  /// created targeting that triple. The driver owns all the ToolChain objects
  /// stored in it, and will clean them up when torn down.
  mutable llvm::StringMap<ToolChain *> ToolChains;

public:
  typedef std::pair<types::ID, const llvm::opt::Arg *> InputPair;
  typedef SmallVector<InputPair, 16> InputList;

  Driver(StringRef DriverExecutable,
         DiagnosticEngine &Diags);
  ~Driver();

  const llvm::opt::OptTable &getOpts() const { return *Opts; }

  const DiagnosticEngine &getDiags() const { return Diags; }

  const char *getSwiftProgramPath() const {
    return DriverExecutable.c_str();
  }

  /// Construct a compilation object for a command line argument vector.
  ///
  /// \return A Compilation, or nullptr if none was built for the given argument
  /// vector. A null return value does not necessarily indicate an error
  /// condition; the diagnostics should be queried to determine if an error
  /// occurred.
  std::unique_ptr<Compilation> buildCompilation(ArrayRef<const char *> Args);

  /// Parse the given list of strings into an InputArgList.
  llvm::opt::InputArgList *parseArgStrings(ArrayRef<const char *> Args);

  /// Translate the input arguments into a DerivedArgList.
  llvm::opt::DerivedArgList *translateInputArgs(
      const llvm::opt::InputArgList &ArgList) const;

  /// Construct the list of inputs and their types from the given arguments.
  ///
  /// \param TC The default host tool chain.
  /// \param Args The input arguments.
  /// \param[out] Inputs The list in which to store the resulting compilation
  /// inputs.
  void buildInputs(const ToolChain &TC, const llvm::opt::DerivedArgList &Args,
                   InputList &Inputs) const;

  /// Construct the list of Actions to perform for the given arguments,
  /// which are only done for a single architecture.
  ///
  /// \param TC the default host tool chain.
  /// \param Args The input arguments.
  /// \param Inputs The inputs for which Actions should be generated.
  /// \param[out] Actions The list in which to store the resulting Actions.
  void buildActions(const ToolChain &TC, const llvm::opt::DerivedArgList &Args,
                    const InputList &Inputs, ActionList &Actions) const;

  /// Add top-level Jobs to Compilation \p C for the given \p Actions.
  void buildJobs(Compilation &C, const ActionList &Actions) const;

  /// Create a Job for the given Action \p A, including creating any necessary
  /// input Jobs.
  ///
  /// \param C The Compilation which this Job will eventually be part of
  /// \param A The Action for which a Job should be created
  /// \param TC The tool chain which should be used to create the Job
  /// \param AtTopLevel indicates whether or not this is a top-level Job
  std::unique_ptr<Job> buildJobsForAction(const Compilation &C, const Action *A,
                                          const ToolChain &TC,
                                          bool AtTopLevel) const;

  /// Handle any arguments which should be treated before building actions or
  /// binding tools.
  ///
  /// \return Whether any compilation should be built for this invocation
  bool handleImmediateArgs(const llvm::opt::ArgList &Args, const ToolChain &TC);

  /// Print the list of Actions.
  void printActions(const ActionList &Actions) const;

  /// Print the list of Jobs.
  void printJobs(const JobList &Jobs) const;

  /// Print the driver version.
  void printVersion(const ToolChain &TC, raw_ostream &OS) const;

  /// Print the help text.
  ///
  /// \param ShowHidden Show hidden options.
  void printHelp(bool ShowHidden) const;

  /// Look up \p Name in the list of program search paths.
  ///
  /// \param TC The provided tool chain for additional information on
  /// directories to search.
  std::string getProgramPath(StringRef Name, const ToolChain &TC) const;

private:
  const ToolChain &getToolChain(const llvm::opt::ArgList &Args,
                                StringRef DarwinArchName = "") const;

  class OutputMode {
  public:
    /// The output type which should be used for compile actions.
    types::ID CompilerOutputType;

    /// Whether or not the output of compile actions should be linked together.
    bool ShouldLink;

    OutputMode(types::ID CompilerOutputType, bool ShouldLink)
      : CompilerOutputType(CompilerOutputType), ShouldLink(ShouldLink) {}
  };
  OutputMode getOutputMode(const llvm::opt::ArgList &Args) const;

};

} // end namespace driver
} // end namespace swift

#endif
