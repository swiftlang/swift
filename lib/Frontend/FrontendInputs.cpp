//===--- FrontendInputs.cpp -------------------------------------*- C++ -*-===//
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

#include "swift/Frontend/FrontendInputs.h"

#include "swift/AST/DiagnosticsFrontend.h"
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

FrontendInputs::FrontendInputs(const FrontendInputs &other) {
  for (InputFile input : other.getAllFiles())
    addInput(input);
}

FrontendInputs &FrontendInputs::operator=(const FrontendInputs &other) {
  clearInputs();
  for (InputFile input : other.getAllFiles())
    addInput(input);
  return *this;
}

std::vector<std::string> FrontendInputs::getInputFilenames() const {
  std::vector<std::string> filenames;
  for (auto &input : getAllFiles()) {
    filenames.push_back(input.file());
  }
  return filenames;
}

bool FrontendInputs::isReadingFromStdin() const {
  return hasSingleInput() && getFilenameOfFirstInput() == "-";
}

void FrontendInputs::assertMustNotBeMoreThanOnePrimaryInput() const {
  assert(primaryInputCount() < 2 &&
         "have not implemented >1 primary input yet");
}

const InputFile *FrontendInputs::getUniquePrimaryInput() const {
  assertMustNotBeMoreThanOnePrimaryInput();
  const auto b = PrimaryInputs.begin();
  return b == PrimaryInputs.end() ? nullptr : &AllFiles[b->second];
}

const InputFile &FrontendInputs::getRequiredUniquePrimaryInput() const {
  if (const auto *input = getUniquePrimaryInput())
    return *input;
  llvm_unreachable("No primary when one is required");
}

StringRef FrontendInputs::getNameOfUniquePrimaryInputFile() const {
  const auto *input = getUniquePrimaryInput();
  return input == nullptr ? StringRef() : input->file();
}

bool FrontendInputs::isFilePrimary(StringRef file) const {
  auto iterator = PrimaryInputs.find(file);
  return iterator != PrimaryInputs.end() &&
         AllFiles[iterator->second].isPrimary();
}

StringRef FrontendInputs::getFilenameOfFirstInput() const {
  assert(hasInputs());
  const InputFile &inp = getAllFiles()[0];
  StringRef f = inp.file();
  assert(!f.empty());
  return f;
}

bool FrontendInputs::shouldTreatAsLLVM() const {
  if (hasSingleInput()) {
    StringRef Input(getFilenameOfFirstInput());
    return llvm::sys::path::extension(Input).endswith(LLVM_BC_EXTENSION) ||
           llvm::sys::path::extension(Input).endswith(LLVM_IR_EXTENSION);
  }
  return false;
}

bool FrontendInputs::shouldTreatAsSIL() const {
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

void FrontendInputs::addInput(const InputFile &input) {
  if (!input.file().empty() && input.isPrimary())
    PrimaryInputs.insert(std::make_pair(input.file(), AllFiles.size()));
  AllFiles.push_back(input);
}

unsigned
FrontendInputs::numberOfPrimaryInputsEndingWith(const char *extension) const {
  return count_if(
      PrimaryInputs, [&](const llvm::StringMapEntry<unsigned> &elem) -> bool {
        StringRef filename = AllFiles[elem.second].file();
        return llvm::sys::path::extension(filename).endswith(extension);
      });
}

bool FrontendInputs::verifyInputs(DiagnosticEngine &diags, bool treatAsSIL,
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

bool FrontendInputs::areAllNonPrimariesSIB() const {
  for (const InputFile &input : getAllFiles()) {
    if (input.isPrimary())
      continue;
    if (!llvm::sys::path::extension(input.file()).endswith(SIB_EXTENSION)) {
      return false;
    }
  }
  return true;
}
