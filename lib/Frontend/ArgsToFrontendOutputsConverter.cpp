//===--- ArgsToFrontendOutputsConverter.cpp -------------------------------===//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/Frontend/ArgsToFrontendOutputsConverter.h"

#include "swift/AST/DiagnosticsFrontend.h"
#include "swift/Basic/Platform.h"
#include "swift/Frontend/ArgsToFrontendInputsConverter.h"
#include "swift/Frontend/ArgsToFrontendOptionsConverter.h"
#include "swift/Frontend/Frontend.h"
#include "swift/Option/Options.h"
#include "swift/Option/SanitizerOptions.h"
#include "swift/Strings.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/Triple.h"
#include "llvm/Option/Arg.h"
#include "llvm/Option/ArgList.h"
#include "llvm/Option/Option.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/LineIterator.h"
#include "llvm/Support/Path.h"

using namespace swift;
using namespace llvm::opt;

Optional<std::vector<std::string>> ArgsToFrontendOutputsConverter::convert() {
  const auto requestedAction =
      ArgsToFrontendOptionsConverter::determineRequestedAction(Args);

  if (!FrontendOptions::doesActionProduceOutput(requestedAction))
    return std::vector<std::string>();

  if (auto ofc = OutputFilesComputer::create(Args, Diags, InputsAndOutputs))
    return ofc->computeOutputFiles();
  return None;
}

Optional<std::vector<std::string>>
ArgsToFrontendOutputsConverter::readOutputFileList(const StringRef filelistPath,
                                                   DiagnosticEngine &diags) {
  llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>> buffer =
      llvm::MemoryBuffer::getFile(filelistPath);
  if (!buffer) {
    diags.diagnose(SourceLoc(), diag::cannot_open_file, filelistPath,
                   buffer.getError().message());
    return None;
  }
  std::vector<std::string> outputFiles;
  for (StringRef line : make_range(llvm::line_iterator(*buffer.get()), {})) {
    outputFiles.push_back(line.str());
  }
  return outputFiles;
}

Optional<std::vector<std::string>>
OutputFilesComputer::getOutputFilenamesFromCommandLineOrFilelist(
    const ArgList &args, DiagnosticEngine &diags) {
  if (const Arg *A = args.getLastArg(options::OPT_output_filelist)) {
    assert(!args.hasArg(options::OPT_o) &&
           "don't use -o with -output-filelist");
    return ArgsToFrontendOutputsConverter::readOutputFileList(A->getValue(),
                                                              diags);
  }
  return args.getAllArgValues(options::OPT_o);
}

Optional<OutputFilesComputer>
OutputFilesComputer::create(const llvm::opt::ArgList &args,
                            DiagnosticEngine &diags,
                            const FrontendInputsAndOutputs &inputsAndOutputs) {
  Optional<std::vector<std::string>> outputArguments =
      getOutputFilenamesFromCommandLineOrFilelist(args, diags);
  if (!outputArguments)
    return None;
  const StringRef outputDirectoryArgument =
      outputArguments->size() == 1 &&
              llvm::sys::fs::is_directory(outputArguments->front())
          ? StringRef(outputArguments->front())
          : StringRef();
  ArrayRef<std::string> outputFileArguments =
      outputDirectoryArgument.empty() ? ArrayRef<std::string>(*outputArguments)
                                      : ArrayRef<std::string>();
  const StringRef firstInput = inputsAndOutputs.hasSingleInput()
                                   ? inputsAndOutputs.getFilenameOfFirstInput()
                                   : StringRef();
  const FrontendOptions::ActionType requestedAction =
      ArgsToFrontendOptionsConverter::determineRequestedAction(args);

  if (!outputFileArguments.empty() &&
      outputFileArguments.size() !=
          inputsAndOutputs.countOfInputsProducingMainOutputs()) {
    diags.diagnose(
        SourceLoc(),
        diag::error_if_any_output_files_are_specified_they_all_must_be);
    return None;
  }

  return OutputFilesComputer(
      args, diags, inputsAndOutputs, std::move(outputFileArguments),
      outputDirectoryArgument, firstInput, requestedAction,
      args.getLastArg(options::OPT_module_name),
      FrontendOptions::suffixForPrincipalOutputFileForAction(requestedAction),
      FrontendOptions::doesActionProduceTextualOutput(requestedAction));
}

OutputFilesComputer::OutputFilesComputer(
    const llvm::opt::ArgList &args, DiagnosticEngine &diags,
    const FrontendInputsAndOutputs &inputsAndOutputs,
    std::vector<std::string> outputFileArguments,
    const StringRef outputDirectoryArgument, const StringRef firstInput,
    const FrontendOptions::ActionType requestedAction,
    const llvm::opt::Arg *moduleNameArg, const StringRef suffix,
    const bool hasTextualOutput)
    : Args(args), Diags(diags), InputsAndOutputs(inputsAndOutputs),
      OutputFileArguments(outputFileArguments),
      OutputDirectoryArgument(outputDirectoryArgument), FirstInput(firstInput),
      RequestedAction(requestedAction), ModuleNameArg(moduleNameArg),
      Suffix(suffix), HasTextualOutput(hasTextualOutput) {}

Optional<std::vector<std::string>>
OutputFilesComputer::computeOutputFiles() const {
  std::vector<std::string> outputFiles;
  bool hadError = false;
  unsigned i = 0;
  InputsAndOutputs.forEachInputProducingAMainOutputFile(
      [&](const InputFile &input) -> void {

        StringRef outputArg = OutputFileArguments.empty()
                                  ? StringRef()
                                  : StringRef(OutputFileArguments[i++]);

        Optional<std::string> outputFile = computeOutputFile(outputArg, input);
        if (!outputFile) {
          hadError = true;
          return;
        }
        outputFiles.push_back(*outputFile);
      });
  return hadError ? None : Optional<std::vector<std::string>>(outputFiles);
}

Optional<std::string>
OutputFilesComputer::computeOutputFile(StringRef outputArg,
                                       const InputFile &input) const {
  if (!OutputDirectoryArgument.empty())
    return deriveOutputFileForDirectory(input);

  if (!outputArg.empty())
    return outputArg.str();

  return deriveOutputFileFromInput(input);
}

Optional<std::string>
OutputFilesComputer::deriveOutputFileFromInput(const InputFile &input) const {
  if (input.file() == "-" || HasTextualOutput)
    return std::string("-");

  std::string baseName = determineBaseNameOfOutput(input);
  if (baseName.empty()) {
    // Assuming FrontendOptions::doesActionProduceOutput(RequestedAction)
    Diags.diagnose(SourceLoc(), diag::error_no_output_filename_specified);
    return None;
  }
  return deriveOutputFileFromParts("", baseName);
}

Optional<std::string> OutputFilesComputer::deriveOutputFileForDirectory(
    const InputFile &input) const {
  std::string baseName = determineBaseNameOfOutput(input);
  if (baseName.empty()) {
    Diags.diagnose(SourceLoc(), diag::error_implicit_output_file_is_directory,
                   OutputDirectoryArgument);
    return None;
  }
  return deriveOutputFileFromParts(OutputDirectoryArgument, baseName);
}

std::string
OutputFilesComputer::determineBaseNameOfOutput(const InputFile &input) const {
  StringRef nameToStem =
      input.isPrimary()
          ? input.file()
          : ModuleNameArg ? ModuleNameArg->getValue() : FirstInput;
  return llvm::sys::path::stem(nameToStem).str();
}

std::string
OutputFilesComputer::deriveOutputFileFromParts(StringRef dir,
                                               StringRef base) const {
  assert(!base.empty());
  llvm::SmallString<128> path(dir);
  llvm::sys::path::append(path, base);
  llvm::sys::path::replace_extension(path, Suffix);
  return path.str();
}
