//===--- ArgsToFrontendInputsConverter.cpp --------------------------------===//
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

#include "swift/Frontend/ArgsToFrontendInputsConverter.h"

#include "swift/AST/DiagnosticsFrontend.h"
#include "swift/Frontend/ArgsToFrontendOutputsConverter.h"
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

ArgsToFrontendInputsConverter::ArgsToFrontendInputsConverter(
    DiagnosticEngine &diags, const ArgList &args,
    FrontendInputsAndOutputs &inputsAndOutputs)
    : Diags(diags), Args(args), InputsAndOutputs(inputsAndOutputs),
      FilelistPathArg(args.getLastArg(options::OPT_filelist)),
      PrimaryFilelistPathArg(args.getLastArg(options::OPT_primary_filelist)) {}

bool ArgsToFrontendInputsConverter::convert() {
  if (enforceFilelistExclusion())
    return true;
  if (FilelistPathArg ? readInputFilesFromFilelist()
                      : readInputFilesFromCommandLine())
    return true;
  Optional<std::set<StringRef>> primaryFiles = readPrimaryFiles();
  if (!primaryFiles)
    return true;
  std::set<StringRef> unusedPrimaryFiles =
      createInputFilesConsumingPrimaries(*primaryFiles);

  if (checkForMissingPrimaryFiles(unusedPrimaryFiles))
    return true;

  // Must be set before iterating over inputs needing outputs.
  InputsAndOutputs.setIsSingleThreadedWMO(isSingleThreadedWMO());

  InputsAndOutputs.setBypassBatchModeChecks(
      Args.hasArg(options::OPT_bypass_batch_mode_checks));
  return false;
}

bool ArgsToFrontendInputsConverter::enforceFilelistExclusion() {
  if (Args.hasArg(options::OPT_INPUT) && FilelistPathArg) {
    Diags.diagnose(SourceLoc(),
                   diag::error_cannot_have_input_files_with_file_list);
    return true;
  }
  // The following is not strictly necessary, but the restriction makes
  // it easier to understand a given command line:
  if (Args.hasArg(options::OPT_primary_file) && PrimaryFilelistPathArg) {
    Diags.diagnose(
        SourceLoc(),
        diag::error_cannot_have_primary_files_with_primary_file_list);
    return true;
  }
  return false;
}

bool ArgsToFrontendInputsConverter::readInputFilesFromCommandLine() {
  bool hadDuplicates = false;
  for (const Arg *A :
       Args.filtered(options::OPT_INPUT, options::OPT_primary_file)) {
    hadDuplicates = addFile(A->getValue()) || hadDuplicates;
  }
  return false; // FIXME: Don't bail out for duplicates, too many tests depend
  // on it.
}

bool ArgsToFrontendInputsConverter::readInputFilesFromFilelist() {
  bool hadDuplicates = false;
  bool hadError =
      forAllFilesInFilelist(FilelistPathArg, [&](StringRef file) -> void {
        hadDuplicates = addFile(file) || hadDuplicates;
      });
  if (hadError)
    return true;
  return false; // FIXME: Don't bail out for duplicates, too many tests depend
                // on it.
}

bool ArgsToFrontendInputsConverter::forAllFilesInFilelist(
    Arg const *const pathArg, llvm::function_ref<void(StringRef)> fn) {
  if (!pathArg)
    return false;
  StringRef path = pathArg->getValue();
  llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>> filelistBufferOrError =
      llvm::MemoryBuffer::getFile(path);
  if (!filelistBufferOrError) {
    Diags.diagnose(SourceLoc(), diag::cannot_open_file, path,
                   filelistBufferOrError.getError().message());
    return true;
  }
  for (auto file :
       llvm::make_range(llvm::line_iterator(*filelistBufferOrError->get()),
                        llvm::line_iterator()))
    fn(file);
  BuffersToKeepAlive.push_back(std::move(*filelistBufferOrError));
  return false;
}

bool ArgsToFrontendInputsConverter::addFile(StringRef file) {
  if (Files.insert(file))
    return false;
  Diags.diagnose(SourceLoc(), diag::error_duplicate_input_file, file);
  return true;
}

Optional<std::set<StringRef>>
ArgsToFrontendInputsConverter::readPrimaryFiles() {
  std::set<StringRef> primaryFiles;
  for (const Arg *A : Args.filtered(options::OPT_primary_file))
    primaryFiles.insert(A->getValue());
  if (forAllFilesInFilelist(
          PrimaryFilelistPathArg,
          [&](StringRef file) -> void { primaryFiles.insert(file); }))
    return None;
  return primaryFiles;
}

std::set<StringRef>
ArgsToFrontendInputsConverter::createInputFilesConsumingPrimaries(
    std::set<StringRef> primaryFiles) {
  for (auto &file : Files) {
    bool isPrimary = primaryFiles.count(file) > 0;
    InputsAndOutputs.addInput(InputFile(file, isPrimary));
    if (isPrimary)
      primaryFiles.erase(file);
  }
  return primaryFiles;
}

bool ArgsToFrontendInputsConverter::checkForMissingPrimaryFiles(
    std::set<StringRef> primaryFiles) {
  for (auto &file : primaryFiles) {
    // Catch "swiftc -frontend -c -filelist foo -primary-file
    // some-file-not-in-foo".
    assert(FilelistPathArg && "Missing primary with no filelist");
    Diags.diagnose(SourceLoc(), diag::error_primary_file_not_found, file,
                   FilelistPathArg->getValue());
  }
  return !primaryFiles.empty();
}

bool ArgsToFrontendInputsConverter::isSingleThreadedWMO() const {
  Optional<std::vector<std::string>> userSuppliedNamesOrErr =
      OutputFilesComputer::getOutputFilenamesFromCommandLineOrFilelist(Args,
                                                                       Diags);
  return InputsAndOutputs.hasInputs() && !InputsAndOutputs.hasPrimaryInputs() &&
         userSuppliedNamesOrErr && userSuppliedNamesOrErr->size() == 1;
}
