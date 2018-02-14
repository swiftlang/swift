//===--- FrontendInputsAndOutputs.cpp -------------------------------------===//
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

#include "swift/Frontend/FrontendInputsAndOutputs.h"

#include "swift/AST/DiagnosticsFrontend.h"
#include "swift/Basic/PrimarySpecificPaths.h"
#include "swift/Frontend/FrontendOptions.h"
#include "swift/Option/Options.h"
#include "swift/Parse/Lexer.h"
#include "swift/Strings.h"
#include "llvm/Option/Arg.h"
#include "llvm/Option/ArgList.h"
#include "llvm/Option/Option.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/LineIterator.h"
#include "llvm/Support/Path.h"

using namespace swift;
using namespace llvm::opt;

// Constructors

FrontendInputsAndOutputs::FrontendInputsAndOutputs(
    const FrontendInputsAndOutputs &other) {
  for (InputFile input : other.AllInputs)
    addInput(input);
  IsSingleThreadedWMO = other.IsSingleThreadedWMO;
  SupplementaryOutputs = other.SupplementaryOutputs;
  PrimarySpecificPathsForAtMostOnePrimary =
      other.PrimarySpecificPathsForAtMostOnePrimary;
}

FrontendInputsAndOutputs &FrontendInputsAndOutputs::
operator=(const FrontendInputsAndOutputs &other) {
  clearInputs();
  for (InputFile input : other.AllInputs)
    addInput(input);
  IsSingleThreadedWMO = other.IsSingleThreadedWMO;
  SupplementaryOutputs = other.SupplementaryOutputs;
  PrimarySpecificPathsForAtMostOnePrimary =
      other.PrimarySpecificPathsForAtMostOnePrimary;
  return *this;
}

// All inputs:

std::vector<std::string> FrontendInputsAndOutputs::getInputFilenames() const {
  std::vector<std::string> filenames;
  for (auto &input : AllInputs) {
    filenames.push_back(input.file());
  }
  return filenames;
}

bool FrontendInputsAndOutputs::isReadingFromStdin() const {
  return hasSingleInput() && getFilenameOfFirstInput() == "-";
}

StringRef FrontendInputsAndOutputs::getFilenameOfFirstInput() const {
  assert(hasInputs());
  const InputFile &inp = AllInputs[0];
  StringRef f = inp.file();
  assert(!f.empty());
  return f;
}

void FrontendInputsAndOutputs::forEachInput(
    llvm::function_ref<void(const InputFile &)> fn) const {
  for (const InputFile &input : AllInputs)
    fn(input);
}

// Primaries:

const InputFile &FrontendInputsAndOutputs::firstPrimaryInput() const {
  assert(!PrimaryInputs.empty());
  for (const auto &f : AllInputs)
    if (f.isPrimary())
      return f;
  llvm_unreachable("no first primary?!");
}

const InputFile &FrontendInputsAndOutputs::lastPrimaryInput() const {
  assert(!PrimaryInputs.empty());
  for (const auto &f : reversed(AllInputs))
    if (f.isPrimary())
      return f;
  llvm_unreachable("no last primary?!");
}

void FrontendInputsAndOutputs::forEachPrimaryInput(
    llvm::function_ref<void(const InputFile &)> fn) const {
  for (auto &f : AllInputs)
    if (f.isPrimary())
      fn(f);
}

void FrontendInputsAndOutputs::assertMustNotBeMoreThanOnePrimaryInput() const {
  assert(!hasMultiplePrimaryInputs() &&
         "have not implemented >1 primary input yet");
}

void FrontendInputsAndOutputs::
    assertMustNotBeMoreThanOnePrimaryInputUnlessBatchModeChecksHaveBeenBypassed()
        const {
  if (!areBatchModeChecksBypassed())
    assertMustNotBeMoreThanOnePrimaryInput();
}

const InputFile *FrontendInputsAndOutputs::getUniquePrimaryInput() const {
  assertMustNotBeMoreThanOnePrimaryInput();
  const auto b = PrimaryInputs.begin();
  return b == PrimaryInputs.end() ? nullptr : &AllInputs[b->second];
}

const InputFile &
FrontendInputsAndOutputs::getRequiredUniquePrimaryInput() const {
  if (const auto *input = getUniquePrimaryInput())
    return *input;
  llvm_unreachable("No primary when one is required");
}

StringRef FrontendInputsAndOutputs::getNameOfUniquePrimaryInputFile() const {
  const auto *input = getUniquePrimaryInput();
  return input == nullptr ? StringRef() : input->file();
}

std::string FrontendInputsAndOutputs::getStatsFileMangledInputName() const {
  // FIXME: "batch" should probably be some concatenation of all the primary
  // input names, in order to keep the stats file names unique. (Or perhaps just
  // the first primary?)
  return isWholeModule()
             ? "all"
             : primaryInputCount() == 1 ? firstPrimaryInput().file() : "batch";
}

bool FrontendInputsAndOutputs::isInputPrimary(StringRef file) const {
  StringRef correctedFile =
      InputFile::convertBufferNameFromLLVM_getFileOrSTDIN_toSwiftConventions(
          file);
  auto iterator = PrimaryInputs.find(correctedFile);
  if (iterator == PrimaryInputs.end())
    return false;
  assert(AllInputs[iterator->second].isPrimary() &&
         "PrimaryInputs should only hold primaries");
  return true;
}

unsigned FrontendInputsAndOutputs::numberOfPrimaryInputsEndingWith(
    const char *extension) const {
  return count_if(
      PrimaryInputs, [&](const llvm::StringMapEntry<unsigned> &elem) -> bool {
        StringRef filename = AllInputs[elem.second].file();

        return llvm::sys::path::extension(filename).endswith(extension);
      });
}

// Input queries

bool FrontendInputsAndOutputs::shouldTreatAsLLVM() const {
  if (hasSingleInput()) {
    StringRef Input(getFilenameOfFirstInput());
    return llvm::sys::path::extension(Input).endswith(LLVM_BC_EXTENSION) ||
           llvm::sys::path::extension(Input).endswith(LLVM_IR_EXTENSION);
  }
  return false;
}

bool FrontendInputsAndOutputs::shouldTreatAsSIL() const {
  if (hasSingleInput()) {
    // If we have exactly one input filename, and its extension is "sil",
    // treat the input as SIL.
    StringRef Input(getFilenameOfFirstInput());
    return llvm::sys::path::extension(Input).endswith(SIL_EXTENSION);
  }
  // If we have one primary input and it's a filename with extension "sil",
  // treat the input as SIL.
  unsigned silPrimaryCount = numberOfPrimaryInputsEndingWith(SIL_EXTENSION);
  if (silPrimaryCount == 0)
    return false;
  if (silPrimaryCount == primaryInputCount()) {
    // Not clear what to do someday with multiple primaries
    assertMustNotBeMoreThanOnePrimaryInput();
    return true;
  }
  llvm_unreachable("Either all primaries or none must end with .sil");
}

bool FrontendInputsAndOutputs::areAllNonPrimariesSIB() const {
  for (const InputFile &input : AllInputs) {
    if (input.isPrimary())
      continue;
    if (!llvm::sys::path::extension(input.file()).endswith(SIB_EXTENSION)) {
      return false;
    }
  }
  return true;
}

bool FrontendInputsAndOutputs::verifyInputs(DiagnosticEngine &diags,
                                            bool treatAsSIL,
                                            bool isREPLRequested,
                                            bool isNoneRequested) const {
  if (isREPLRequested) {
    if (hasInputs()) {
      diags.diagnose(SourceLoc(), diag::error_repl_requires_no_input_files);
      return true;
    }
  } else if (treatAsSIL) {
    if (isWholeModule()) {
      if (inputCount() != 1) {
        diags.diagnose(SourceLoc(), diag::error_mode_requires_one_input_file);
        return true;
      }
    } else {
      assertMustNotBeMoreThanOnePrimaryInput();
      // If we have the SIL as our primary input, we can waive the one file
      // requirement as long as all the other inputs are SIBs.
      if (!areAllNonPrimariesSIB()) {
        diags.diagnose(SourceLoc(),
                       diag::error_mode_requires_one_sil_multi_sib);
        return true;
      }
    }
  } else if (!isNoneRequested && !hasInputs()) {
    diags.diagnose(SourceLoc(), diag::error_mode_requires_an_input_file);
    return true;
  }
  return false;
}

// Changing inputs

void FrontendInputsAndOutputs::clearInputs() {
  AllInputs.clear();
  PrimaryInputs.clear();
}

void FrontendInputsAndOutputs::addInput(const InputFile &input) {
  if (!input.file().empty() && input.isPrimary())
    PrimaryInputs.insert(std::make_pair(input.file(), AllInputs.size()));
  AllInputs.push_back(input);
}

void FrontendInputsAndOutputs::addInputFile(StringRef file,
                                            llvm::MemoryBuffer *buffer) {
  addInput(InputFile(file, false, buffer));
}

void FrontendInputsAndOutputs::addPrimaryInputFile(StringRef file,
                                                   llvm::MemoryBuffer *buffer) {
  addInput(InputFile(file, true, buffer));
}

// Outputs

unsigned FrontendInputsAndOutputs::countOfInputsProducingMainOutputs() const {
  return isSingleThreadedWMO()
             ? 1
             : hasPrimaryInputs() ? primaryInputCount() : inputCount();
}

const InputFile &FrontendInputsAndOutputs::firstInputProducingOutput() const {
  return isSingleThreadedWMO()
             ? firstInput()
             : hasPrimaryInputs() ? firstPrimaryInput() : firstInput();
}

const InputFile &FrontendInputsAndOutputs::lastInputProducingOutput() const {
  return isSingleThreadedWMO()
             ? firstInput()
             : hasPrimaryInputs() ? lastPrimaryInput() : lastInput();
}

void FrontendInputsAndOutputs::forEachInputProducingAMainOutputFile(
    llvm::function_ref<void(const InputFile &)> fn) const {
  isSingleThreadedWMO()
      ? fn(firstInput())
      : hasPrimaryInputs() ? forEachPrimaryInput(fn) : forEachInput(fn);
}

void FrontendInputsAndOutputs::setMainAndSupplementaryOutputs(
    ArrayRef<std::string> outputFiles,
    SupplementaryOutputPaths supplementaryOutputs) {
  if (hasPrimaryInputs()) {
    unsigned i = 0;
    for (auto index : indices(AllInputs)) {
      InputFile &f = AllInputs[index];
      if (f.isPrimary())
        f.setOutputFilename(outputFiles[i++]);
    }
  } else if (isSingleThreadedWMO()) {
    AllInputs[0].setOutputFilename(outputFiles[0]);
  } else {
    for (auto i : indices(AllInputs))
      AllInputs[i].setOutputFilename(outputFiles[i]);
  }
  SupplementaryOutputs = supplementaryOutputs;

  if (hasUniquePrimaryInput() || (hasInputs() && isWholeModule())) {
    // When batch mode is fully implemented, each InputFile will own
    // a PrimarySpecificPaths.
    PrimarySpecificPathsForAtMostOnePrimary.OutputFilename =
        getSingleOutputFilename();
    PrimarySpecificPathsForAtMostOnePrimary.MainInputFilenameForDebugInfo =
        hasInputsProducingMainOutputs() ? firstInputProducingOutput().file()
                                        : StringRef();
    PrimarySpecificPathsForAtMostOnePrimary.SupplementaryOutputs =
        supplementaryOutputs;
  }
}

std::vector<std::string> FrontendInputsAndOutputs::copyOutputFilenames() const {
  std::vector<std::string> outputs;
  forEachInputProducingAMainOutputFile([&](const InputFile &input) -> void {
    outputs.push_back(input.outputFilename());
  });
  return outputs;
}

void FrontendInputsAndOutputs::forEachOutputFilename(
    llvm::function_ref<void(const std::string &)> fn) const {
  forEachInputProducingAMainOutputFile(
      [&](const InputFile &input) -> void { fn(input.outputFilename()); });
}

StringRef FrontendInputsAndOutputs::getSingleOutputFilename() const {
  assertMustNotBeMoreThanOnePrimaryInputUnlessBatchModeChecksHaveBeenBypassed();
  return hasInputs() ? StringRef(lastInputProducingOutput().outputFilename())
                     : StringRef();
}

bool FrontendInputsAndOutputs::isOutputFilenameStdout() const {
  return getSingleOutputFilename() == "-";
}

bool FrontendInputsAndOutputs::isOutputFileDirectory() const {
  return hasNamedOutputFile() &&
         llvm::sys::fs::is_directory(getSingleOutputFilename());
}

bool FrontendInputsAndOutputs::hasNamedOutputFile() const {
  return hasInputs() && !isOutputFilenameStdout();
}

// Supplementary outputs

unsigned
FrontendInputsAndOutputs::countOfFilesProducingSupplementaryOutput() const {
  return hasPrimaryInputs() ? primaryInputCount() : hasInputs() ? 1 : 0;
}

void FrontendInputsAndOutputs::forEachInputProducingSupplementaryOutput(
    llvm::function_ref<void(const InputFile &)> fn) const {
  if (hasPrimaryInputs())
    forEachPrimaryInput(fn);
  else if (hasInputs())
    fn(firstInput());
}

bool FrontendInputsAndOutputs::hasDependenciesPath() const {
  return !supplementaryOutputs().DependenciesFilePath.empty();
}
bool FrontendInputsAndOutputs::hasReferenceDependenciesPath() const {
  return !supplementaryOutputs().ReferenceDependenciesFilePath.empty();
}
bool FrontendInputsAndOutputs::hasObjCHeaderOutputPath() const {
  return !supplementaryOutputs().ObjCHeaderOutputPath.empty();
}
bool FrontendInputsAndOutputs::hasLoadedModuleTracePath() const {
  return !supplementaryOutputs().LoadedModuleTracePath.empty();
}
bool FrontendInputsAndOutputs::hasModuleOutputPath() const {
  return !supplementaryOutputs().ModuleOutputPath.empty();
}
bool FrontendInputsAndOutputs::hasModuleDocOutputPath() const {
  return !supplementaryOutputs().ModuleDocOutputPath.empty();
}
bool FrontendInputsAndOutputs::hasTBDPath() const {
  return !supplementaryOutputs().TBDPath.empty();
}

bool FrontendInputsAndOutputs::hasDependencyTrackerPath() const {
  return hasDependenciesPath() || hasReferenceDependenciesPath() ||
         hasLoadedModuleTracePath();
}

PrimarySpecificPaths &
FrontendInputsAndOutputs::getPrimarySpecificPathsForAtMostOnePrimary() {
  return PrimarySpecificPathsForAtMostOnePrimary;
}

PrimarySpecificPaths &
FrontendInputsAndOutputs::getPrimarySpecificPathsForPrimary(
    StringRef filename) {
  return getPrimarySpecificPathsForAtMostOnePrimary(); // just a stub for now
}
