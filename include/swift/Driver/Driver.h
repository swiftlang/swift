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

#include "swift/AST/IRGenOptions.h"
#include "swift/Basic/LLVM.h"
#include "swift/Driver/Types.h"
#include "swift/Driver/Util.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/ADT/StringRef.h"

#include <functional>
#include <memory>
#include <string>

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
  class DiagnosticEngine;
namespace driver {
  class Compilation;
  class Job;
  class JobList;
  class OutputFileMap;
  class ToolChain;

/// \brief A class encapsulating information about the outputs the driver
/// is expected to generate.
class OutputInfo {
public:
  enum class Mode {
    /// A standard compilation, using multiple frontend invocations and
    /// -primary-file.
    StandardCompile,

    /// A compilation using a single frontend invocation without -primary-file.
    SingleCompile,

    /// Invoke the REPL
    REPL,

    /// Compile and execute the inputs immediately
    Immediate,

    /// Invoke swift-update with the compiler frontend options.
    UpdateCode,
  };

  /// The mode in which the driver should invoke the frontend.
  Mode CompilerMode = Mode::StandardCompile;

  /// The output type which should be used for compile actions.
  types::ID CompilerOutputType = types::ID::TY_INVALID;

  /// Describes if and how the output of compile actions should be
  /// linked together.
  LinkKind LinkAction = LinkKind::None;

  /// Returns true if the linker will be invoked at all.
  bool shouldLink() const { return LinkAction != LinkKind::None; }

  /// Whether or not the output should contain debug info.
  // FIXME: Eventually this should be replaced by dSYM generation.
  IRGenDebugInfoKind DebugInfoKind = IRGenDebugInfoKind::None;

  /// Whether or not the driver should generate a module.
  bool ShouldGenerateModule = false;

  /// Whether or not the driver should treat a generated module as a top-level
  /// output.
  bool ShouldTreatModuleAsTopLevelOutput = false;

  /// Whether the compiler picked the current module name, rather than the user.
  bool ModuleNameIsFallback = false;

  // Whether the driver should generate compiler fixits as source edits.
  bool ShouldGenerateFixitEdits = false;
  
  /// The number of threads for multi-threaded compilation.
  unsigned numThreads = 0;

  /// Returns true if multi-threading is enabled.
  bool isMultiThreading() const { return numThreads > 0; }
  
  /// The name of the module which we are building.
  std::string ModuleName;

  /// The path to the SDK against which to build.
  /// (If empty, this implies no SDK.)
  std::string SDKPath;
};

class Driver {
public:
  /// DriverKind determines how later arguments are parsed, as well as the
  /// allowable OutputInfo::Mode values.
  enum class DriverKind {
    Interactive,     // swift
    Batch,           // swiftc
    AutolinkExtract, // swift-autolink-extract
  };

  class InputInfoMap;

private:
  std::unique_ptr<llvm::opt::OptTable> Opts;

  DiagnosticEngine &Diags;

  /// The name the driver was invoked as.
  std::string Name;

  /// The original path to the executable.
  std::string DriverExecutable;

  DriverKind driverKind = DriverKind::Interactive;

  /// Default target triple.
  std::string DefaultTargetTriple;

  /// Indicates whether the driver should print bindings.
  bool DriverPrintBindings;

  /// Indicates whether the driver should suppress the "no input files" error.
  bool SuppressNoInputFilesError = false;

  /// Indicates whether the driver should check that the input files exist.
  bool CheckInputFilesExist = true;

  /// \brief Cache of all the ToolChains in use by the driver.
  ///
  /// This maps from the string representation of a triple to a ToolChain
  /// created targeting that triple. The driver owns all the ToolChain objects
  /// stored in it, and will clean them up when torn down.
  mutable llvm::StringMap<ToolChain *> ToolChains;

public:
  typedef std::pair<types::ID, const llvm::opt::Arg *> InputPair;
  typedef SmallVector<InputPair, 16> InputList;

  Driver(StringRef DriverExecutable, StringRef Name,
         ArrayRef<const char *> Args, DiagnosticEngine &Diags);
  ~Driver();

  const llvm::opt::OptTable &getOpts() const { return *Opts; }

  const DiagnosticEngine &getDiags() const { return Diags; }

  const std::string &getSwiftProgramPath() const {
    return DriverExecutable;
  }
  
  DriverKind getDriverKind() const { return driverKind; }
  
  ArrayRef<const char *> getArgsWithoutProgramNameAndDriverMode(
                                            ArrayRef<const char *> Args) const;

  bool getCheckInputFilesExist() const { return CheckInputFilesExist; }

  void setCheckInputFilesExist(bool Value) { CheckInputFilesExist = Value; }

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
  /// \param TC The current tool chain.
  /// \param Args The input arguments.
  /// \param[out] Inputs The list in which to store the resulting compilation
  /// inputs.
  void buildInputs(const ToolChain &TC, const llvm::opt::DerivedArgList &Args,
                   InputList &Inputs) const;

  /// Construct the OutputInfo for the driver from the given arguments.
  ///
  /// \param TC The current tool chain.
  /// \param Args The input arguments.
  /// \param Inputs The inputs to the driver.
  /// \param[out] OI The OutputInfo in which to store the resulting output
  /// information.
  void buildOutputInfo(const ToolChain &TC,
                       const llvm::opt::DerivedArgList &Args,
                       const InputList &Inputs, OutputInfo &OI) const;

  /// Construct the list of Actions to perform for the given arguments,
  /// which are only done for a single architecture.
  ///
  /// \param TC the default host tool chain.
  /// \param Args The input arguments.
  /// \param Inputs The inputs for which Actions should be generated.
  /// \param OI The OutputInfo for which Actions should be generated.
  /// \param OFM The OutputFileMap for the compilation; used to find any
  /// cross-build information.
  /// \param OutOfDateMap If present, information used to decide which files
  /// need to be rebuilt.
  /// \param[out] Actions The list in which to store the resulting Actions.
  void buildActions(const ToolChain &TC, const llvm::opt::DerivedArgList &Args,
                    const InputList &Inputs, const OutputInfo &OI,
                    const OutputFileMap *OFM, InputInfoMap *OutOfDateMap,
                    ActionList &Actions) const;

  /// Construct the OutputFileMap for the driver from the given arguments.
  std::unique_ptr<OutputFileMap>
  buildOutputFileMap(const llvm::opt::DerivedArgList &Args) const;

  /// Add top-level Jobs to Compilation \p C for the given \p Actions and
  /// OutputInfo.
  ///
  /// \param Actions The Actions for which Jobs should be generated.
  /// \param OI The OutputInfo for which Jobs should be generated
  /// \param OFM The OutputFileMap for which Jobs should be generated
  /// \param[out] C The Compilation to which Jobs should be added
  void buildJobs(const ActionList &Actions, const OutputInfo &OI,
                 const OutputFileMap *OFM, Compilation &C) const;

  /// A map for caching Jobs for a given Action/ToolChain pair
  using JobCacheMap =
    llvm::DenseMap<std::pair<const Action *, const ToolChain *>, Job *>;

  /// Create a Job for the given Action \p A, including creating any necessary
  /// input Jobs.
  ///
  /// \param C The Compilation which this Job will eventually be part of
  /// \param A The Action for which a Job should be created
  /// \param OI The OutputInfo for which a Job should be created
  /// \param OFM The OutputFileMap for which a Job should be created
  /// \param TC The tool chain which should be used to create the Job
  /// \param AtTopLevel indicates whether or not this is a top-level Job
  /// \param JobCache maps existing Action/ToolChain pairs to Jobs
  ///
  /// \returns a Job for the given Action/ToolChain pair
  Job *buildJobsForAction(Compilation &C, const Action *A,
                          const OutputInfo &OI, const OutputFileMap *OFM,
                          const ToolChain &TC, bool AtTopLevel,
                          JobCacheMap &JobCache) const;

  /// Handle any arguments which should be treated before building actions or
  /// binding tools.
  ///
  /// \return Whether any compilation should be built for this invocation
  bool handleImmediateArgs(const llvm::opt::ArgList &Args, const ToolChain &TC);

  /// Print the list of Actions.
  void printActions(const ActionList &Actions) const;

  /// Print the list of Jobs in a Compilation.
  void printJobs(const Compilation &C) const;

  /// Print the driver version.
  void printVersion(const ToolChain &TC, raw_ostream &OS) const;

  /// Print the help text.
  ///
  /// \param ShowHidden Show hidden options.
  void printHelp(bool ShowHidden) const;

private:
  const ToolChain *getToolChain(const llvm::opt::ArgList &Args,
                                StringRef DarwinArchName = "") const;

  /// Parse the driver kind.
  ///
  /// \param Args The arguments passed to the driver (excluding the path to the
  /// driver)
  void parseDriverKind(ArrayRef<const char *> Args);
};

} // end namespace driver
} // end namespace swift

#endif
