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
#include "swift/Basic/SupplementaryOutputPaths.h"
#include "swift/Frontend/FrontendOptions.h"
#include "swift/Option/Options.h"
#include "llvm/Option/ArgList.h"

#include <vector>

using namespace swift;
using namespace llvm::opt;

namespace swift {

/// Given the command line arguments and information about the inputs,
/// Fill in all the information in FrontendInputsAndOutputs.

class ArgsToFrontendOutputsConverter {
  const ArgList &Args;
  StringRef ModuleName;
  FrontendInputsAndOutputs &InputsAndOutputs;
  DiagnosticEngine &Diags;

public:
  ArgsToFrontendOutputsConverter(const ArgList &args, StringRef moduleName,
                                 FrontendInputsAndOutputs &inputsAndOutputs,
                                 DiagnosticEngine &diags)
      : Args(args), ModuleName(moduleName), InputsAndOutputs(inputsAndOutputs),
        Diags(diags) {}

  Optional<std::vector<std::string>> convert();

  /// None -> could not open
  static Optional<std::vector<std::string>>
  readOutputFileList(const StringRef filelistPath, DiagnosticEngine &diags);

  static bool isOutputAUniqueOrdinaryFile(ArrayRef<std::string> outputs);
};

class OutputFilesComputer {
  const ArgList &Args;
  DiagnosticEngine &Diags;
  const FrontendInputsAndOutputs &InputsAndOutputs;

  const Optional<std::vector<std::string>> OutputFileArguments;
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

  /// \return the output filenames on the command line or in the output
  /// filelist. If there
  /// were neither -o's nor an output filelist, \return an empty vector.
  static Optional<std::vector<std::string>>
  getOutputFilenamesFromCommandLineOrFilelist(const ArgList &args,
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

} // namespace swift

#endif /* SWIFT_FRONTEND_ARGSTOFRONTENDOUTPUTSCONVERTER_H */
