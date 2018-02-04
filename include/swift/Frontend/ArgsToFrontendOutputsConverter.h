//===--- ArgsToFrontendOutputsConverter.h -----------------------*- C++ -*-===//
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

#ifndef SWIFT_FRONTEND_ARGSTOFRONTENDOUTPUTSCONVERTER_H
#define SWIFT_FRONTEND_ARGSTOFRONTENDOUTPUTSCONVERTER_H

#include "swift/AST/DiagnosticConsumer.h"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/Basic/LLVM.h"
#include "swift/Frontend/FrontendOptions.h"
#include "swift/Frontend/SupplementaryOutputPaths.h"
#include "swift/Option/Options.h"
#include "llvm/Option/ArgList.h"

#include <vector>

namespace swift {

/// Given the command line arguments and information about the inputs,
/// Fill in all the information in FrontendInputsAndOutputs.

class ArgsToFrontendOutputsConverter {
  const llvm::opt::ArgList &Args;
  StringRef ModuleName;
  FrontendInputsAndOutputs &InputsAndOutputs;
  DiagnosticEngine &Diags;

public:
  ArgsToFrontendOutputsConverter(const llvm::opt::ArgList &args,
                                 StringRef moduleName,
                                 FrontendInputsAndOutputs &inputsAndOutputs,
                                 DiagnosticEngine &diags)
      : Args(args), ModuleName(moduleName), InputsAndOutputs(inputsAndOutputs),
        Diags(diags) {}

  bool convert(std::vector<std::string> &mainOutputs,
               SupplementaryOutputPaths &supplementaryOutputs);

  /// Try to read an output file list file.
  /// \returns `None` if it could not open the filelist.
  static Optional<std::vector<std::string>>
  readOutputFileList(StringRef filelistPath, DiagnosticEngine &diags);
};

class OutputFilesComputer {
  const llvm::opt::ArgList &Args;
  DiagnosticEngine &Diags;
  const FrontendInputsAndOutputs &InputsAndOutputs;
  const std::vector<std::string> OutputFileArguments;
  const std::string OutputDirectoryArgument;
  const StringRef FirstInput;
  const FrontendOptions::ActionType RequestedAction;
  const llvm::opt::Arg *const ModuleNameArg;
  const StringRef Suffix;
  const bool HasTextualOutput;

  OutputFilesComputer(const llvm::opt::ArgList &args, DiagnosticEngine &diags,
                      const FrontendInputsAndOutputs &inputsAndOutputs,
                      std::vector<std::string> outputFileArguments,
                      StringRef outputDirectoryArgument, StringRef firstInput,
                      FrontendOptions::ActionType requestedAction,
                      const llvm::opt::Arg *moduleNameArg, StringRef suffix,
                      bool hasTextualOutput);

public:
  static Optional<OutputFilesComputer>
  create(const llvm::opt::ArgList &args, DiagnosticEngine &diags,
         const FrontendInputsAndOutputs &inputsAndOutputs);

  /// \return the output filenames on the command line or in the output
  /// filelist. If there
  /// were neither -o's nor an output filelist, returns an empty vector.
  static Optional<std::vector<std::string>>
  getOutputFilenamesFromCommandLineOrFilelist(const llvm::opt::ArgList &args,
                                              DiagnosticEngine &diags);

  Optional<std::vector<std::string>> computeOutputFiles() const;

private:
  Optional<std::string> computeOutputFile(StringRef outputArg,
                                          const InputFile &input) const;

  /// \return the correct output filename when none was specified.
  ///
  /// Such an absence should only occur when invoking the frontend
  /// without the driver,
  /// because the driver will always pass -o with an appropriate filename
  /// if output is required for the requested action.
  Optional<std::string> deriveOutputFileFromInput(const InputFile &input) const;

  /// \return the correct output filename when a directory was specified.
  ///
  /// Such a specification should only occur when invoking the frontend
  /// directly, because the driver will always pass -o with an appropriate
  /// filename if output is required for the requested action.
  Optional<std::string>
  deriveOutputFileForDirectory(const InputFile &input) const;

  std::string determineBaseNameOfOutput(const InputFile &input) const;

  std::string deriveOutputFileFromParts(StringRef dir, StringRef base) const;
};

class SupplementaryOutputPathsComputer {
  const llvm::opt::ArgList &Args;
  DiagnosticEngine &Diags;
  const FrontendInputsAndOutputs &InputsAndOutputs;
  ArrayRef<std::string> OutputFiles;
  StringRef ModuleName;

  const FrontendOptions::ActionType RequestedAction;

public:
  SupplementaryOutputPathsComputer(
      const llvm::opt::ArgList &args, DiagnosticEngine &diags,
      const FrontendInputsAndOutputs &inputsAndOutputs,
      ArrayRef<std::string> outputFiles, StringRef moduleName);
  Optional<SupplementaryOutputPaths> computeOutputPaths() const;

private:
  /// \return None if error.
  Optional<std::vector<SupplementaryOutputPaths>>
  getSupplementaryOutputPathsFromArguments() const;

  /// Given an ID corresponding to supplementary output argument
  /// (e.g. -emit-module-path), collect all such paths, and ensure
  /// there are the right number of them.
  Optional<std::vector<std::string>>
  getSupplementaryFilenamesFromArguments(options::ID pathID) const;

  Optional<SupplementaryOutputPaths> computeOutputPathsForOneInput(
      StringRef outputFilename,
      const SupplementaryOutputPaths &pathsFromFilelists,
      const InputFile &) const;

  StringRef deriveDefaultSupplementaryOutputPathExcludingExtension(
      StringRef outputFilename, const InputFile &) const;

  /// \return empty string if no output file.
  std::string determineSupplementaryOutputFilename(
      options::ID emitOpt, std::string pathFromArgumentsOrFilelists,
      StringRef extension, StringRef mainOutputIfUsable,
      StringRef defaultSupplementaryOutputPathExcludingExtension) const;

  void deriveModulePathParameters(options::ID &emitOption,
                                  std::string &extension,
                                  std::string &mainOutputIfUsable) const;
};

} // namespace swift

#endif /* SWIFT_FRONTEND_ARGSTOFRONTENDOUTPUTSCONVERTER_H */
