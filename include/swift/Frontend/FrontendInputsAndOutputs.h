//===--- FrontendInputs.h ---------------------------------------*- C++ -*-===//
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

#ifndef SWIFT_FRONTEND_FRONTENDINPUTS_H
#define SWIFT_FRONTEND_FRONTENDINPUTS_H

#include "swift/AST/Module.h"
#include "swift/Basic/PrimarySpecificPaths.h"
#include "swift/Basic/SupplementaryOutputPaths.h"
#include "swift/Frontend/InputFile.h"
#include "llvm/ADT/Hashing.h"
#include "llvm/ADT/MapVector.h"

#include <string>
#include <vector>

namespace llvm {
class MemoryBuffer;
}

namespace swift {

/// Information about all the inputs and outputs to the frontend.

class FrontendInputsAndOutputs {
  friend class ArgsToFrontendInputsConverter;

  std::vector<InputFile> AllInputs;

  llvm::StringMap<unsigned> PrimaryInputs;

  /// In Single-threaded WMO mode, all inputs are used
  /// both for importing and compiling.
  bool IsSingleThreadedWMO = false;

  /// Punt where needed to enable batch mode experiments.
  bool AreBatchModeChecksBypassed = false;

  SupplementaryOutputPaths SupplementaryOutputs;

public:
  bool areBatchModeChecksBypassed() const { return AreBatchModeChecksBypassed; }
  void setBypassBatchModeChecks(bool bbc) { AreBatchModeChecksBypassed = bbc; }

  const SupplementaryOutputPaths &supplementaryOutputs() const {
    return SupplementaryOutputs;
  }
  SupplementaryOutputPaths &supplementaryOutputs() {
    return SupplementaryOutputs;
  }

  /// When performing a compilation for zero or one primary input file,
  /// this will hold the PrimarySpecificPaths.
  /// In a future PR, each InputFile will hold its own PrimarySpecificPaths and
  /// this will go away.
  PrimarySpecificPaths PrimarySpecificPathsForAtMostOnePrimary;

  FrontendInputsAndOutputs() = default;
  FrontendInputsAndOutputs(const FrontendInputsAndOutputs &other);
  FrontendInputsAndOutputs &operator=(const FrontendInputsAndOutputs &other);

  // Whole-module-optimization (WMO) routines:

  // SingleThreadedWMO produces only main output file. In contrast,
  // multi-threaded WMO produces one main output per input, as single-file and
  // batch-mode do for each primary. Both WMO modes produce only one set of
  // supplementary outputs.

  bool isSingleThreadedWMO() const { return IsSingleThreadedWMO; }
  void setIsSingleThreadedWMO(bool istw) { IsSingleThreadedWMO = istw; }

  bool isWholeModule() const { return !hasPrimaryInputs(); }

  // Readers:

  // All inputs:

  ArrayRef<InputFile> getAllInputs() const { return AllInputs; }

  std::vector<std::string> getInputFilenames() const;

  unsigned inputCount() const { return AllInputs.size(); }

  bool hasInputs() const { return !AllInputs.empty(); }

  bool hasSingleInput() const { return inputCount() == 1; }

  const InputFile &firstInput() const { return AllInputs[0]; }
  InputFile &firstInput() { return AllInputs[0]; }

  const InputFile &lastInput() const { return AllInputs.back(); }

  StringRef getFilenameOfFirstInput() const;

  bool isReadingFromStdin() const;

  void forEachInput(llvm::function_ref<void(const InputFile &)> fn) const;

  // Primaries:

  const InputFile &firstPrimaryInput() const;
  const InputFile &lastPrimaryInput() const;

  void
  forEachPrimaryInput(llvm::function_ref<void(const InputFile &)> fn) const;

  unsigned primaryInputCount() const { return PrimaryInputs.size(); }

  // Primary count readers:

  bool hasUniquePrimaryInput() const { return primaryInputCount() == 1; }

  bool hasPrimaryInputs() const { return primaryInputCount() > 0; }

  bool hasMultiplePrimaryInputs() const { return primaryInputCount() > 1; }

  /// Fails an assertion if there is more than one primary input.
  /// Used in situations where only one primary input can be handled
  /// and where batch mode has not been implemented yet.
  void assertMustNotBeMoreThanOnePrimaryInput() const;

  /// Fails an assertion when there is more than one primary input unless
  /// the experimental -bypass-batch-mode-checks argument was passed to
  /// the front end.
  /// FIXME: When batch mode is complete, this function should be obsolete.
  void
  assertMustNotBeMoreThanOnePrimaryInputUnlessBatchModeChecksHaveBeenBypassed()
      const;

  // Count-dependend readers:

  /// \return the unique primary input, if one exists.
  const InputFile *getUniquePrimaryInput() const;

  const InputFile &getRequiredUniquePrimaryInput() const;

  /// \return the name of the unique primary input, or an empty StrinRef if
  /// there isn't one.
  StringRef getNameOfUniquePrimaryInputFile() const;

  /// Combines all primaries for stats reporter
  std::string getStatsFileMangledInputName() const;

  bool isInputPrimary(StringRef file) const;

  unsigned numberOfPrimaryInputsEndingWith(const char *extension) const;

  // Multi-facet readers

  // If we have exactly one input filename, and its extension is "bc" or "ll",
  // treat the input as LLVM_IR.
  bool shouldTreatAsLLVM() const;
  bool shouldTreatAsSIL() const;

  bool areAllNonPrimariesSIB() const;

  /// \return true for error
  bool verifyInputs(DiagnosticEngine &diags, bool treatAsSIL,
                    bool isREPLRequested, bool isNoneRequested) const;

  // Changing inputs

public:
  void clearInputs();
  void addInput(const InputFile &input);
  void addInputFile(StringRef file, llvm::MemoryBuffer *buffer = nullptr);
  void addPrimaryInputFile(StringRef file,
                           llvm::MemoryBuffer *buffer = nullptr);

  // Outputs

private:
  friend class ArgsToFrontendOptionsConverter;

  void
  setMainAndSupplementaryOutputs(ArrayRef<std::string> outputFiles,
                                 SupplementaryOutputPaths supplementaryOutputs);

public:
  unsigned countOfInputsProducingMainOutputs() const;

  bool hasInputsProducingMainOutputs() const {
    return countOfInputsProducingMainOutputs() != 0;
  }

  const InputFile &firstInputProducingOutput() const;
  const InputFile &lastInputProducingOutput() const;

  /// Under single-threaded WMO, we pretend that the first input
  /// generates the main output, even though it will include code
  /// generated from all of them.
  void forEachInputProducingAMainOutputFile(
      llvm::function_ref<void(const InputFile &)> fn) const;

  std::vector<std::string> copyOutputFilenames() const;

  void
  forEachOutputFilename(llvm::function_ref<void(const std::string &)> fn) const;

  /// Gets the name of the specified output filename.
  /// If multiple files are specified, the last one is returned.
  StringRef getSingleOutputFilename() const;

  bool isOutputFilenameStdout() const;
  bool isOutputFileDirectory() const;
  bool hasNamedOutputFile() const;

  // Supplementary outputs

  unsigned countOfFilesProducingSupplementaryOutput() const;

  void forEachInputProducingSupplementaryOutput(
      llvm::function_ref<void(const InputFile &)> fn) const;

  /// Assumes there is not more than one primary input file, if any.
  /// Otherwise, you would need to call getPrimarySpecificPathsForPrimary
  /// to tell it which primary input you wanted the outputs for.
  ///
  /// Must not be constructed on-the-fly because some parts of the compiler
  /// receive StringRefs to its components, so it must live as long as the
  /// compiler.
  PrimarySpecificPaths &getPrimarySpecificPathsForAtMostOnePrimary();

  PrimarySpecificPaths &getPrimarySpecificPathsForPrimary(StringRef filename);

  bool hasDependenciesPath() const;
  bool hasReferenceDependenciesPath() const;
  bool hasObjCHeaderOutputPath() const;
  bool hasLoadedModuleTracePath() const;
  bool hasModuleOutputPath() const;
  bool hasModuleDocOutputPath() const;
  bool hasTBDPath() const;

  bool hasDependencyTrackerPath() const;
};

} // namespace swift

#endif /* SWIFT_FRONTEND_FRONTENDINPUTS_H */
