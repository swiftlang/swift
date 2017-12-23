//===--- FrontendInputsAndOutputs.cpp
//----------------------------------------------===//
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

#include "swift/Frontend/FrontendOptions.h"

#include "swift/AST/DiagnosticsFrontend.h"
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

unsigned FrontendInputsAndOutputs::numberOfPrimaryInputsEndingWith(
    const char *extension) const {
  return count_if(
      PrimaryInputs, [&](const std::pair<StringRef, unsigned> &elem) -> bool {
        StringRef filename = getAllInputs()[elem.second].file();
        return llvm::sys::path::extension(filename).endswith(extension);
      });
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
      // requirement as long as all the other inputsAndOutputs are SIBs.
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

bool FrontendInputsAndOutputs::areAllNonPrimariesSIB() const {
  for (const InputFile &input : getAllInputs()) {
    if (input.isPrimary())
      continue;
    if (!llvm::sys::path::extension(input.file()).endswith(SIB_EXTENSION)) {
      return false;
    }
  }
  return true;
}

InputFile &FrontendInputsAndOutputs::firstPrimaryInput() {
  assert(!PrimaryInputs.empty());
  return getAllInputs()[PrimaryInputs.front().second];
}
const InputFile &FrontendInputsAndOutputs::firstPrimaryInput() const {
  assert(!PrimaryInputs.empty());
  return getAllInputs()[PrimaryInputs.front().second];
}

std::vector<std::string> FrontendInputsAndOutputs::getInputFilenames() const {
  std::vector<std::string> filenames;
  for (auto &input : getAllInputs()) {
    filenames.push_back(input.file());
  }
  return filenames;
}

StringRef FrontendInputsAndOutputs::getFilenameOfFirstInput() const {
  assert(hasInputs());
  const InputFile &inp = firstInput();
  StringRef f = inp.file();
  assert(!f.empty());
  return f;
}

const InputFile *FrontendInputsAndOutputs::getUniquePrimaryInput() const {
  assertMustNotBeMoreThanOnePrimaryInput();
  const auto b = PrimaryInputs.begin();
  return b == PrimaryInputs.end() ? nullptr : &getAllInputs()[b->second];
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

bool FrontendInputsAndOutputs::isFilePrimary(StringRef file) {
  return PrimaryInputs.count(file) != 0;
}

void FrontendInputsAndOutputs::addInputFile(StringRef file,
                                            llvm::MemoryBuffer *buffer) {
  addInput(InputFile(file, false, buffer));
}
void FrontendInputsAndOutputs::addPrimaryInputFile(StringRef file,
                                                   llvm::MemoryBuffer *buffer) {
  addInput(InputFile(file, true, buffer));
}

void FrontendInputsAndOutputs::addInput(const InputFile &input) {
  getAllInputs().push_back(input);
  if (input.isPrimary()) {
    // Take care to push a reference to the string in the InputFile stored in
    // AllFiles, NOT in the input parameter.
    PrimaryInputs.insert(std::make_pair(getAllInputs().back().file(),
                                        getAllInputs().size() - 1));
  }
}

void FrontendInputsAndOutputs::clearInputs() {
  AllFiles.clear();
  PrimaryInputs.clear();
}

void FrontendInputsAndOutputs::assertMustNotBeMoreThanOnePrimaryInput() const {
  assert(primaryInputCount() < 2 &&
         "have not implemented >1 primary input yet");
}

unsigned FrontendInputsAndOutputs::countOfFilesProducingOutput() const {
  return hasPrimaryInputs() ? primaryInputCount() : inputCount();
}

const InputFile &FrontendInputsAndOutputs::firstInputProducingOutput() const {
  return isSingleThreadedWMO()
             ? *getSingleThreadedWMOInput()
             : hasPrimaryInputs() ? firstPrimaryInput() : firstInput();
}

bool FrontendInputsAndOutputs::forEachInputProducingOutput(
    llvm::function_ref<bool(const InputFile &, unsigned)> fn) const {
  return isSingleThreadedWMO()
             ? fn(*getSingleThreadedWMOInput(), 0)
             : hasPrimaryInputs() ? forEachPrimaryInput(fn) : forEachInput(fn);
}

bool FrontendInputsAndOutputs::forEachInput(
    llvm::function_ref<bool(const InputFile &, unsigned)> fn) const {
  for (auto i : indices(getAllInputs()))
    if (fn(getAllInputs()[i], i))
      return true;
  return false;
}

bool FrontendInputsAndOutputs::forEachPrimaryInput(
    llvm::function_ref<bool(const InputFile &, unsigned)> fn) const {
  unsigned i = 0;
  for (const auto p : PrimaryInputs)
    if (fn(getAllInputs()[p.second], i++))
      return true;
  return false;
}

InputFile &FrontendInputsAndOutputs::firstInputProducingOutput() {
  return isSingleThreadedWMO()
             ? *getSingleThreadedWMOInput()
             : hasPrimaryInputs() ? firstPrimaryInput() : firstInput();
}

bool FrontendInputsAndOutputs::forEachInputProducingOutput(
    llvm::function_ref<bool(InputFile &, unsigned)> fn) {
  return isSingleThreadedWMO()
             ? fn(*getSingleThreadedWMOInput(), 0)
             : hasPrimaryInputs() ? forEachPrimaryInput(fn) : forEachInput(fn);
}

bool FrontendInputsAndOutputs::forEachInput(
    llvm::function_ref<bool(InputFile &, unsigned)> fn) {
  for (auto i : indices(getAllInputs()))
    if (fn(getAllInputs()[i], i))
      return true;
  return false;
}

bool FrontendInputsAndOutputs::forEachPrimaryInput(
    llvm::function_ref<bool(InputFile &input, unsigned)> fn) {
  unsigned i = 0;
  for (auto p : PrimaryInputs)
    if (fn(getAllInputs()[p.second], i++))
      return true;
  return false;
}

bool FrontendInputsAndOutputs::isOutputFileDirectory() const {
  return hasNamedOutputFile() &&
         llvm::sys::fs::is_directory(getSingleOutputFilename());
}

std::vector<StringRef> FrontendInputsAndOutputs::getOutputFilenames() const {
  std::vector<StringRef> outputs;
  for (const std::string &s : OutputFilenames)
    outputs.push_back(s);
  return outputs;
}

std::vector<std::string> FrontendInputsAndOutputs::copyOutputFilenames() const {
  std::vector<std::string> outputs;
  for (const std::string s : OutputFilenames)
    outputs.push_back(s);
  return outputs;
}

void FrontendInputsAndOutputs::setOutputFilenames(
    ArrayRef<std::string> outputs) {
  assert(getOutputFilenames().empty() && "re-setting OutputFilenames");
  for (StringRef s : outputs)
    OutputFilenames.push_back(s);
}

/// Gets the name of the specified output filename.
/// If multiple files are specified, the last one is returned.
StringRef FrontendInputsAndOutputs::getSingleOutputFilename() const {
  if (OutputFilenames.size() >= 1)
    return OutputFilenames.back();
  return StringRef();
}
/// Sets a single filename as output filename.
void FrontendInputsAndOutputs::setSingleOutputFilename(
    const std::string &FileName) {
  OutputFilenames.clear();
  OutputFilenames.push_back(FileName);
}
void FrontendInputsAndOutputs::setOutputFilenameToStdout() {
  setSingleOutputFilename("-");
}
bool FrontendInputsAndOutputs::isOutputFilenameStdout() const {
  return getSingleOutputFilename() == "-";
}

bool FrontendInputsAndOutputs::hasNamedOutputFile() const {
  return !OutputFilenames.empty() && !isOutputFilenameStdout();
}

void FrontendInputsAndOutputs::forEachOutputFilename(
    llvm::function_ref<void(const std::string)> fn) const {
  for (const std::string &s : OutputFilenames)
    fn(s);
}
