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

class DiagnosticEngine;

/// Information about all the inputs and outputs to the frontend.

class FrontendInputsAndOutputs {
  friend class ArgsToFrontendInputsConverter;

  std::vector<InputFile> AllInputs;
  llvm::StringMap<unsigned> PrimaryInputsByName;
  std::vector<unsigned> PrimaryInputsInOrder;

  /// In Single-threaded WMO mode, all inputs are used
  /// both for importing and compiling.
  bool IsSingleThreadedWMO = false;

  /// Punt where needed to enable batch mode experiments.
  bool AreBatchModeChecksBypassed = false;

public:
  bool areBatchModeChecksBypassed() const { return AreBatchModeChecksBypassed; }
  void setBypassBatchModeChecks(bool bbc) { AreBatchModeChecksBypassed = bbc; }

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

  /// \return nullptr if not a primary input file.
  const InputFile *primaryInputNamed(StringRef name) const;

  unsigned inputCount() const { return AllInputs.size(); }

  bool hasInputs() const { return !AllInputs.empty(); }

  bool hasSingleInput() const { return inputCount() == 1; }

  const InputFile &firstInput() const { return AllInputs[0]; }
  InputFile &firstInput() { return AllInputs[0]; }

  const InputFile &lastInput() const { return AllInputs.back(); }

  const std::string &getFilenameOfFirstInput() const;

  bool isReadingFromStdin() const;

  /// If \p fn returns true, exits early and returns true.
  bool forEachInput(llvm::function_ref<bool(const InputFile &)> fn) const;

  // Primaries:

  const InputFile &firstPrimaryInput() const;
  const InputFile &lastPrimaryInput() const;

  /// If \p fn returns true, exit early and return true.
  bool
  forEachPrimaryInput(llvm::function_ref<bool(const InputFile &)> fn) const;

  unsigned primaryInputCount() const { return PrimaryInputsInOrder.size(); }

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

  /// FIXME: Should combine all primaries for the result
  /// instead of just answering "batch" if there is more than one primary.
  std::string getStatsFileMangledInputName() const;

  bool isInputPrimary(StringRef file) const;

  unsigned numberOfPrimaryInputsEndingWith(StringRef extension) const;

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

  void setMainAndSupplementaryOutputs(
      ArrayRef<std::string> outputFiles,
      ArrayRef<SupplementaryOutputPaths> supplementaryOutputs);

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
  ///
  /// If \p fn returns true, return early and return true.
  bool forEachInputProducingAMainOutputFile(
      llvm::function_ref<bool(const InputFile &)> fn) const;

  std::vector<std::string> copyOutputFilenames() const;

  void forEachOutputFilename(llvm::function_ref<void(StringRef)> fn) const;

  /// Gets the name of the specified output filename.
  /// If multiple files are specified, the last one is returned.
  std::string getSingleOutputFilename() const;

  bool isOutputFilenameStdout() const;
  bool isOutputFileDirectory() const;
  bool hasNamedOutputFile() const;

  // Supplementary outputs

  unsigned countOfFilesProducingSupplementaryOutput() const;

  /// If \p fn returns true, exit early and return true.
  bool forEachInputProducingSupplementaryOutput(
      llvm::function_ref<bool(const InputFile &)> fn) const;

  /// Assumes there is not more than one primary input file, if any.
  /// Otherwise, you would need to call getPrimarySpecificPathsForPrimary
  /// to tell it which primary input you wanted the outputs for.
  const PrimarySpecificPaths &
  getPrimarySpecificPathsForAtMostOnePrimary() const;

  const PrimarySpecificPaths &
      getPrimarySpecificPathsForPrimary(StringRef) const;

  bool hasSupplementaryOutputPath(
      llvm::function_ref<const std::string &(const SupplementaryOutputPaths &)>
          extractorFn) const;

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
