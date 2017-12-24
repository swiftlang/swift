//===--- FrontendInputsAndOutputs.h --------------------------------------*- C++
//-*-===//
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

#ifndef FrontendInputsAndOutputsAndOutputs_h
#define FrontendInputsAndOutputsAndOutputs_h

#include "swift/AST/Module.h"
#include "swift/Basic/InputFile.h"
#include "swift/Basic/OutputPaths.h"
#include "llvm/ADT/Hashing.h"
#include "llvm/ADT/MapVector.h"

#include <string>
#include <vector>

namespace llvm {
class MemoryBuffer;
}

namespace swift {

/// Information about all the inputsAndOutputs and outputs to the frontend.

class FrontendInputsAndOutputs {
  friend class ArgsToFrontendInputsConverter;

  std::vector<InputFile> AllFiles;

  typedef llvm::MapVector<StringRef, unsigned> InputFileMap;
  InputFileMap PrimaryInputs;

  /// In Single-threaded WMO mode, all inputs are used
  /// both for importing and compiling.
  /// Only one set of outputs (one OutputFilename, one OutputPaths) is produced.
  bool IsSingleThreadedWMO = false;

  /// The specified output files. If only a single outputfile is generated,
  /// the name of the last specified file is taken.
  std::vector<std::string> OutputFilenames;

public:
  FrontendInputsAndOutputs() = default;

  FrontendInputsAndOutputs(const FrontendInputsAndOutputs &other) {
    for (InputFile input : other.AllFiles)
      addInput(input);
    IsSingleThreadedWMO = other.IsSingleThreadedWMO;
    OutputFilenames = other.OutputFilenames;
  }

  FrontendInputsAndOutputs &operator=(const FrontendInputsAndOutputs &other) {
    clearInputs();
    for (InputFile input : other.AllFiles)
      addInput(input);
    IsSingleThreadedWMO = other.IsSingleThreadedWMO;
    OutputFilenames = other.OutputFilenames;
    return *this;
  }

  // WMO routines:

  //  SingleThreadedWMO mode needs only one of each output file for the entire
  //  invocation. WMO can get away with that because it doesn't even attempt to
  //  be incremental, and so it doesn't need per-file intermediates that
  //  wouldn't be generated otherwise.
  //
  //    (A few of the outputs might make more sense to be generated for every
  //    input—.d files in particular—but it wasn't natural when passing them on
  //    the command line, and it hasn't been critical. So right now there's only
  //    one of everything in WMO, always, except for the actual object files in
  //    threaded mode.)
  //
  // MultithreadedWMO needs only one set of supplementary outputs for the entire
  // invocation.

  bool isSingleThreadedWMO() const { return IsSingleThreadedWMO; }

  bool isWholeModule() const { return !hasPrimaryInputs(); }

  // Readers:

  // All inputs:

  std::vector<std::string> getInputFilenames() const;

  unsigned inputCount() const { return AllFiles.size(); }

  bool hasInputs() const { return !AllFiles.empty(); }

  bool hasSingleInput() const { return inputCount() == 1; }

  const InputFile &firstInput() const { return AllFiles[0]; }
  InputFile &firstInput() { return AllFiles[0]; }

  StringRef getFilenameOfFirstInput() const;

  bool isReadingFromStdin() const {
    return hasSingleInput() && getFilenameOfFirstInput() == "-";
  }

  void forEachInput(llvm::function_ref<void(const InputFile &)> fn) const;

  // Primaries:

  const InputFile &firstPrimaryInput() const;

  void
  forEachPrimaryInput(llvm::function_ref<void(const InputFile &)> fn) const;

  unsigned primaryInputCount() const { return PrimaryInputs.size(); }

  bool hasUniquePrimaryInput() const { return primaryInputCount() == 1; }

  bool hasPrimaryInputs() const { return primaryInputCount() > 0; }

  // FIXME: dmu fix uses / remove these when batch mode works
  void assertMustNotBeMoreThanOnePrimaryInput() const;

  // Outputs

  unsigned countOfFilesProducingOutput() const;

  const InputFile &firstInputProducingOutput() const;

  void forEachInputProducingOutput(
      llvm::function_ref<void(const InputFile &)> fn) const;

  unsigned countOfFilesProducingSupplementaryOutput() const;

  const InputFile &firstInputProducingSupplementaryOutput() const;

  void forEachInputProducingSupplementaryOutput(
      llvm::function_ref<void(const InputFile &)> fn) const;

  /// Return the unique primary input, if one exists.
  const InputFile *getUniquePrimaryInput() const;

  const InputFile &getRequiredUniquePrimaryInput() const;

  /// Return the name of the unique primary input, or an empty StringRef if
  /// there isn't one.
  StringRef getNameOfUniquePrimaryInputFile() const;

  bool isFilePrimary(StringRef file);

  unsigned numberOfPrimaryInputsEndingWith(const char *extension) const;

  std::vector<StringRef> getOutputFilenames() const;
  std::vector<std::string> copyOutputFilenames() const;

  void
  forEachOutputFilename(llvm::function_ref<void(const std::string)> fn) const;

  /// Gets the name of the specified output filename.
  /// If multiple files are specified, the last one is returned.
  StringRef getSingleOutputFilename() const;

  bool isOutputFilenameStdout() const;
  bool isOutputFileDirectory() const;
  bool hasNamedOutputFile() const;

  const std::string &getObjCHeaderOutputPath() const;
  const std::string &getModuleOutputPath() const;
  const std::string &getModuleDocOutputPath() const;
  const std::string &getDependenciesFilePath() const;
  const std::string &getReferenceDependenciesFilePath() const;
  const std::string &getSerializedDiagnosticsPath() const;
  const std::string &getLoadedModuleTracePath() const;
  const std::string &getTBDPath() const;

  const OutputPaths &supplementaryOutputPaths() const;

  // Queries

  // If we have exactly one input filename, and its extension is "bc" or "ll",
  // treat the input as LLVM_IR.
  bool shouldTreatAsLLVM() const;
  bool shouldTreatAsSIL() const;

  bool areAllNonPrimariesSIB() const;

  /// Return true for error
  bool verifyInputs(DiagnosticEngine &diags, bool treatAsSIL,
                    bool isREPLRequested, bool isNoneRequested) const;

public:
  void clearInputs();
  void addInput(const InputFile &input);
  void addInputFile(StringRef file, llvm::MemoryBuffer *buffer = nullptr);
  void addPrimaryInputFile(StringRef file,
                           llvm::MemoryBuffer *buffer = nullptr);

private:
  friend class ArgsToFrontendOptionsConverter;
  void setMainAndSupplementaryOutputs(
      ArrayRef<std::string> outputFiles,
      ArrayRef<const OutputPaths> supplementaryOutputs);

  void setIsSingleThreadedWMO(bool istw) { IsSingleThreadedWMO = istw; }
};

} // namespace swift

#endif /* FrontendInputsAndOutputsAndOutputs_h */
