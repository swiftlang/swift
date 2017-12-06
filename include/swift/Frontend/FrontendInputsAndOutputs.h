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

#ifndef FrontendInputsAndOutputs_h
#define FrontendInputsAndOutputs_h

#include "swift/AST/Module.h"
#include "swift/Basic/InputFile.h"
#include "swift/Frontend/FrontendInputsAndOutputs.h"
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

  std::vector<InputFile> AllFiles;
  typedef llvm::MapVector<StringRef, unsigned> InputFileMap;
  InputFileMap PrimaryInputs;
  bool IsSingleThreadedWMO = false;

public:
  FrontendInputsAndOutputs() = default;

  FrontendInputsAndOutputs(const FrontendInputsAndOutputs &other) {
    for (InputFile input : other.getAllInputs())
      addInput(input);
    IsSingleThreadedWMO = other.IsSingleThreadedWMO;
  }

  FrontendInputsAndOutputs &operator=(const FrontendInputsAndOutputs &other) {
    clearInputs();
    for (InputFile input : other.getAllInputs())
      addInput(input);
    IsSingleThreadedWMO = other.IsSingleThreadedWMO;
    return *this;
  }

  // Single-threaded WMO routines:

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
  const InputFile *getSingleThreadedWMOInput() const {
    return isSingleThreadedWMO() ? &AllFiles[0] : nullptr;
  }
  InputFile *getSingleThreadedWMOInput() {
    return isSingleThreadedWMO() ? &AllFiles[0] : nullptr;
  }
  const OutputPaths *getSingleThreadedWMOOutputs() const {
    return isSingleThreadedWMO() ? &AllFiles[0].outputs() : nullptr;
  }
  void setIsSingleThreadedWMO(bool istw) { IsSingleThreadedWMO = istw; }
  bool isSingleThreadedWMO() const { return IsSingleThreadedWMO; }

  // Readers:

  ArrayRef<InputFile> getAllInputs() const { return AllFiles; }
  std::vector<InputFile> &getAllInputs() { return AllFiles; }
  std::vector<InputFile *> getPointersToAllFiles() {
    std::vector<InputFile *> pointers;
    for (InputFile &input : getAllInputs()) {
      pointers.push_back(&input);
    }
    return pointers;
  }

  const InputFile &firstPrimary() const {
    assert(!PrimaryInputs.empty());
    return getAllInputs()[PrimaryInputs.front().second];
  }

  std::vector<std::string> getInputFilenames() const {
    std::vector<std::string> filenames;
    for (auto &input : getAllInputs()) {
      filenames.push_back(input.file());
    }
    return filenames;
  }

  unsigned inputCount() const { return getAllInputs().size(); }

  bool hasInputs() const { return !getAllInputs().empty(); }

  bool hasSingleInput() const { return inputCount() == 1; }

  StringRef getFilenameOfFirstInput() const {
    assert(hasInputs());
    const InputFile &inp = getAllInputs()[0];
    StringRef f = inp.file();
    assert(!f.empty());
    return f;
  }

  bool isReadingFromStdin() const {
    return hasSingleInput() && getFilenameOfFirstInput() == "-";
  }

  // If we have exactly one input filename, and its extension is "bc" or "ll",
  // treat the input as LLVM_IR.
  bool shouldTreatAsLLVM() const;

  // Primary input readers

  bool areAllNonPrimariesSIB() const;

public:
  unsigned countOfFilesProducingOutput() const {
    return hasPrimaries() ? primaryInputCount() : inputCount();
  }

  bool forEachInputProducingOutput(
      llvm::function_ref<bool(const InputFile &)> fn) const {
    return isSingleThreadedWMO()
               ? fn(*getSingleThreadedWMOInput())
               : hasPrimaries() ? forEachPrimaryInput(fn) : forEachInput(fn);
  }

  bool forEachInput(llvm::function_ref<bool(const InputFile &)> fn) const {
    for (const auto &file : getAllInputs())
      if (fn(file))
        return true;
    return false;
  }

  bool
  forEachPrimaryInput(llvm::function_ref<bool(const InputFile &)> fn) const {
    for (const auto p : PrimaryInputs)
      if (fn(getAllInputs()[p.second]))
        return true;
    return false;
  }

  bool forEachInputProducingOutput(llvm::function_ref<bool(InputFile &)> fn) {
    return isSingleThreadedWMO()
               ? fn(*getSingleThreadedWMOInput())
               : hasPrimaries() ? forEachPrimaryInput(fn) : forEachInput(fn);
  }

  bool forEachInput(llvm::function_ref<bool(InputFile &)> fn) {
    for (auto &file : getAllInputs())
      if (fn(file))
        return true;
    return false;
  }

  bool forEachPrimaryInput(llvm::function_ref<bool(InputFile &input)> fn) {
    for (auto p : PrimaryInputs)
      if (fn(getAllInputs()[p.second]))
        return true;
    return false;
  }

  unsigned primaryInputCount() const { return PrimaryInputs.size(); }

  // Primary count readers:

  bool hasUniquePrimaryInput() const { return primaryInputCount() == 1; }

  bool hasPrimaries() const { return primaryInputCount() > 0; }

  bool isWholeModule() const { return !hasPrimaries(); }

  // Count-dependend readers:

  /// Return the unique primary input, if one exists.
  const InputFile *getUniquePrimaryInput() const {
    assertMustNotBeMoreThanOnePrimaryInput();
    const auto b = PrimaryInputs.begin();
    return b == PrimaryInputs.end() ? nullptr : &getAllInputs()[b->second];
  }

  const InputFile &getRequiredUniquePrimaryInput() const {
    if (const auto *input = getUniquePrimaryInput())
      return *input;
    llvm_unreachable("No primary when one is required");
  }

  /// Return the name of the unique primary input, or an empty StringRef if
  /// there isn't one.
  StringRef preBatchGetNameOfUniquePrimaryInputFile() const {
    const auto *input = getUniquePrimaryInput();
    return input == nullptr ? StringRef() : input->file();
  }

  bool isFilePrimary(StringRef file) {
    const bool isPrimary = PrimaryInputs.count(file) != 0;
    return isPrimary;
  }

  unsigned numberOfPrimaryInputsEndingWith(const char *extension) const;

  // Multi-facet readers

  bool shouldTreatAsSIL() const;

  /// Return true for error
  bool verifyInputs(DiagnosticEngine &diags, bool treatAsSIL,
                    bool isREPLRequested, bool isNoneRequested) const;

  // Writers

  void addInputFile(StringRef file, llvm::MemoryBuffer *buffer = nullptr) {
    addInput(InputFile(file, false, buffer));
  }
  void addPrimaryInputFile(StringRef file,
                           llvm::MemoryBuffer *buffer = nullptr) {
    addInput(InputFile(file, true, buffer));
  }

  void addInput(const InputFile &input) {
    getAllInputs().push_back(input);
    if (input.isPrimary()) {
      // Take care to push a reference to the string in the InputFile stored in
      // AllFiles, NOT in the input parameter.
      PrimaryInputs.insert(std::make_pair(getAllInputs().back().file(),
                                          getAllInputs().size() - 1));
    }
  }

  void clearInputs() {
    AllFiles.clear();
    PrimaryInputs.clear();
  }

  // FIXME: dmu fix uses / remove these when batch mode works
  void assertMustNotBeMoreThanOnePrimaryInput() const {
    assert(primaryInputCount() < 2 &&
           "have not implemented >1 primary input yet");
  }

  const StringRef preBatchModeGetSingleOutputFilename() const {
    return preBatchModePathsForAtMostOnePrimary().OutputFilename;
  }
  const OutputPaths &preBatchModePathsForAtMostOnePrimary() const {
    assertMustNotBeMoreThanOnePrimaryInput();
    static OutputPaths empty;
    return hasPrimaries()
               ? getAllInputs()[PrimaryInputs.front().second].outputs()
               : isSingleThreadedWMO() ? *getSingleThreadedWMOOutputs()
                                       : getAllInputs().empty()
                                             ? empty
                                             : getAllInputs().front().outputs();
  }

  std::vector<std::string> preBatchModeOutputFilenames() const;

  const std::string &preBatchModeObjCHeaderOutputPath() const {
    return preBatchModePathsForAtMostOnePrimary().ObjCHeaderOutputPath;
  }

  const std::string &preBatchModeModuleOutputPath() const {
    return preBatchModePathsForAtMostOnePrimary().ModuleOutputPath;
  }

  const std::string &preBatchModeModuleDocOutputPath() const {
    return preBatchModePathsForAtMostOnePrimary().ModuleDocOutputPath;
  }

  const std::string &preBatchModeDependenciesFilePath() const {
    return preBatchModePathsForAtMostOnePrimary().DependenciesFilePath;
  }

  const std::string &preBatchModeReferenceDependenciesFilePath() const {
    return preBatchModePathsForAtMostOnePrimary().ReferenceDependenciesFilePath;
  }

  const std::string &preBatchModeSerializedDiagnosticsPath() const {
    return preBatchModePathsForAtMostOnePrimary().SerializedDiagnosticsPath;
  }

  const std::string &preBatchModeLoadedModuleTracePath() const {
    return preBatchModePathsForAtMostOnePrimary().LoadedModuleTracePath;
  }

  const std::string &preBatchModeTBDPath() const {
    return preBatchModePathsForAtMostOnePrimary().TBDPath;
  }
};

} // namespace swift

#endif /* FrontendInputsAndOutputs_h */
