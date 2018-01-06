//===--- ArgsToFrontendOutputsConverter.h
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

#ifndef ArgsToFrontendOutputsConverter_h
#define ArgsToFrontendOutputsConverter_h

#include "swift/AST/DiagnosticConsumer.h"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/Basic/SupplementaryOutputPaths.h"
#include "swift/Frontend/FrontendOptions.h"
#include "swift/Option/Options.h"
#include "llvm/Option/ArgList.h"

#include <vector>

using namespace swift;
using namespace llvm::opt;

namespace swift {

/// Given the command line arguments and information about the inputsAndOutputs,
/// Fill in all the information in FrontendInputsAndOutputs.

class ArgsToFrontendOutputsConverter {
  const ArgList &Args;
  StringRef ModuleName;
  // FIXME: dmu more functional, return this
  FrontendInputsAndOutputs &InputsAndOutputs;
  DiagnosticEngine &Diags;

public:
  ArgsToFrontendOutputsConverter(const ArgList &args, StringRef moduleName,
                                 FrontendInputsAndOutputs &inputsAndOutputs,
                                 DiagnosticEngine &diags)
      : Args(args), ModuleName(moduleName), InputsAndOutputs(inputsAndOutputs),
        Diags(diags) {}
  Optional<std::pair<std::vector<std::string>,
                     std::vector<SupplementaryOutputPaths>>>
  convert();

  static std::vector<std::string>
  readOutputFileList(const StringRef filelistPath, DiagnosticEngine &diags);

  static bool isOutputAUniqueOrdinaryFile(ArrayRef<std::string> outputs);
};

class OutputFilesComputer {
  const ArgList &Args;
  DiagnosticEngine &Diags;
  const FrontendInputsAndOutputs &InputsAndOutputs;

  const std::vector<std::string> OutputFileArguments;
  const StringRef OutputDirectoryArgument;
  const bool DoOutputFileArgumentsMatchInputs;
  const StringRef FirstInput;
  const FrontendOptions::ActionType RequestedAction;
  const Arg *const ModuleNameArg;
  const StringRef Suffix;
  const bool HasTextualOutput;

public:
  OutputFilesComputer(const ArgList &args, DiagnosticEngine &diags,
                      const FrontendInputsAndOutputs &inputsAndOutputs);

  /// Returns the output filenames on the command line or in the output
  /// filelist. If there
  /// were neither -o's nor an output filelist, returns an empty vector.
  static std::vector<std::string>
  getOutputFilenamesFromCommandLineOrFilelist(const ArgList &args,
                                              DiagnosticEngine &diags);

  Optional<std::vector<std::string>> computeOutputFiles() const;

private:
  Optional<std::string> computeOutputFile(StringRef outputArg,
                                          const InputFile &input) const;

  /// Determine the correct output filename when none was specified.
  ///
  /// Such an absence should only occur when invoking the frontend
  /// without the driver,
  /// because the driver will always pass -o with an appropriate filename
  /// if output is required for the requested action.
  Optional<std::string> deriveOutputFileFromInput(const InputFile &input) const;

  /// Determine the correct output filename when a directory was specified.
  ///
  /// Such a specification should only occur when invoking the frontend
  /// directly, because the driver will always pass -o with an appropriate
  /// filename if output is required for the requested action.
  Optional<std::string>
  deriveOutputFileForDirectory(const InputFile &input) const;

  std::string determineBaseNameOfOutput(const InputFile &input) const;

  std::string deriveOutputFileFromParts(StringRef dir, StringRef base) const;
};

class OutputPathsComputer {
  const ArgList &Args;
  DiagnosticEngine &Diags;
  const FrontendInputsAndOutputs &InputsAndOutputs;
  ArrayRef<std::string> OutputFiles;
  StringRef ModuleName;

  std::vector<SupplementaryOutputPaths> SupplementaryFilenamesFromFilelists;
  const FrontendOptions::ActionType RequestedAction;

public:
  OutputPathsComputer(const ArgList &args, DiagnosticEngine &diags,
                      const FrontendInputsAndOutputs &inputsAndOutputs,
                      ArrayRef<std::string> outputFiles, StringRef moduleName);
  Optional<std::vector<SupplementaryOutputPaths>>
  computeOutputPaths() const;

private:
  static std::vector<SupplementaryOutputPaths>
  getSupplementaryFilenamesFromFilelists(const ArgList &args,
                                         DiagnosticEngine &diags,
                                         unsigned inputCount);

  static Optional<std::vector<std::string>>
  readSupplementaryOutputFileList(const ArgList &args, DiagnosticEngine &diags,
                                  swift::options::ID id,
                                  unsigned requiredCount);

  Optional<SupplementaryOutputPaths> computeOutputPathsForOneInput(
      StringRef outputFilename,
      const SupplementaryOutputPaths &pathsFromFilelists,
      const InputFile &) const;

  StringRef deriveImplicitBasis(StringRef outputFilename,
                                const InputFile &) const;
  Optional<std::string> determineSupplementaryOutputFilename(
      options::ID pathOpt, options::ID emitOpt, StringRef pathFromFilelists,
      StringRef extension, StringRef mainOutputIfUsable,
      StringRef implicitBasis) const;

  void deriveModulePathParameters(options::ID &emitOption,
                                  std::string &extension,
                                  std::string &mainOutputIfUsable) const;
};

} // namespace swift

#endif /* ArgsToFrontendOutputsConverter_h */
