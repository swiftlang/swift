//===--- ArgsToFrontendOutputsConverter.cpp
//----------------------------------------------===////
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
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

bool ArgsToFrontendOutputsConverter::convert() {
  const auto requestedAction =
      ArgsToFrontendOptionsConverter::determineRequestedAction(Args);

  if (!FrontendOptions::doesActionProduceOutput(requestedAction))
    return false;

  Optional<std::vector<std::string>> outputFiles =
      OutputFilesComputer(Args, Diags, InputsAndOutputs).computeOutputFiles();
  if (!outputFiles)
    return true;
  Optional<std::vector<OutputPaths>> outputPaths =
      OutputPathsComputer(Args, Diags, InputsAndOutputs, *outputFiles)
          .computeOutputPaths();
  if (!outputPaths)
    return true;
  unsigned i = 0;
  return InputsAndOutputs.forEachInputProducingOutput(
      [&](InputFile &input) -> bool {
        input.setOutputs((*outputPaths)[i++]);
        return false;
      });
}

/// Try to read an output file list file.
std::vector<std::string>
ArgsToFrontendOutputsConverter::readOutputFileList(const StringRef filelistPath,
                                                   DiagnosticEngine &diags) {
  llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>> buffer =
      llvm::MemoryBuffer::getFile(filelistPath);
  if (!buffer) {
    diags.diagnose(SourceLoc(), diag::cannot_open_file, filelistPath,
                   buffer.getError().message());
  }
  std::vector<std::string> outputFiles;
  for (StringRef line : make_range(llvm::line_iterator(*buffer.get()), {})) {
    outputFiles.push_back(line.str());
  }
  return outputFiles;
}

bool ArgsToFrontendOutputsConverter::isOutputAUniqueOrdinaryFile(
    ArrayRef<std::string> outputs) {
  return outputs.size() == 1 && outputs[0] != "-" &&
         !llvm::sys::fs::is_directory(outputs[0]);
}

std::vector<std::string>
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

OutputFilesComputer::OutputFilesComputer(
    const ArgList &args, DiagnosticEngine &diags,
    const FrontendInputsAndOutputs &inputsAndOutputs)
    : Args(args), Diags(diags), InputsAndOutputs(inputsAndOutputs),
      OutputFileArguments(
          getOutputFilenamesFromCommandLineOrFilelist(Args, Diags)),
      OutputDirectoryArgument(
          OutputFileArguments.size() == 1 &&
                  llvm::sys::fs::is_directory(OutputFileArguments.front())
              ? StringRef(OutputFileArguments.front())
              : StringRef()),
      DoOutputFileArgumentsMatchInputs(
          OutputDirectoryArgument.empty() &&
          OutputFileArguments.size() ==
              InputsAndOutputs.countOfFilesProducingOutput()),
      FirstInput(InputsAndOutputs.hasSingleInput()
                     ? InputsAndOutputs.getFilenameOfFirstInput()
                     : StringRef()),
      RequestedAction(
          ArgsToFrontendOptionsConverter::determineRequestedAction(Args)),
      ModuleNameArg(Args.getLastArg(options::OPT_module_name)),
      Suffix(FrontendOptions::suffixForPrincipalOutputFileForAction(
          RequestedAction)),
      HasTextualOutput(
          FrontendOptions::doesActionProduceTextualOutput(RequestedAction)) {}

Optional<std::vector<std::string>>
OutputFilesComputer::computeOutputFiles() const {
  std::vector<std::string> outputFiles;

  unsigned i = 0;
  bool hadError = InputsAndOutputs.forEachInputProducingOutput(
      [&](InputFile &input) -> bool {

        StringRef outputArg = OutputFileArguments.empty()
                                  ? StringRef()
                                  : StringRef(OutputFileArguments[i++]);

        Optional<std::string> outputFile = computeOutputFile(outputArg, input);
        if (!outputFile)
          return true;
        outputFiles.push_back(*outputFile);
        return false;
      });
  if (hadError)
    return None;
  return outputFiles;
}

Optional<std::string>
OutputFilesComputer::computeOutputFile(StringRef outputArg,
                                       const InputFile &input) const {

  return outputArg.empty() ? deriveOutputFileFromInput(input)
                           : !OutputDirectoryArgument.empty()
                                 ? deriveOutputFileForDirectory(input)
                                 : outputArg.str();
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

//      // FIXME: dmu suppFilelistArgs should be const, but std::vector...
//      std::vector<OutputPaths> suppFilelistArgs =
//      getSupplementaryFilenamesFromFilelists();
//

//

//
//      return computeAllOutputs(Args, outputFileArgumentsForEveryInput,
//                               suppFilelistArgs, isSingleThreadedWMO,
//                               isOutputFilenameArgumentOneDirectory,
//                               doOutputFilenameArgumentsCorrespondToInputs,
//                               Opts.ModuleName, Diags, Opts.InputsAndOutputs);

// FIXME: dmu getSupplementaryFilenamesFromFilelists assumes same indices as
// inputs
//      std::vector<OutputPaths>
//      ArgsToFrontendOutputsConverter::getSupplementaryFilenamesFromFilelists()
//      {
//        const unsigned N = InputsAndOutputs.countOfFilesProducingOutput();
//
//        auto objCHeaderOutput = readSupplementaryOutputFileList(
//                                                                options::OPT_objCHeaderOutput_filelist,
//                                                                N);
//        auto moduleOutput =
//        readSupplementaryOutputFileList(options::OPT_moduleOutput_filelist,
//        N); auto moduleDocOutput =
//        readSupplementaryOutputFileList(options::OPT_moduleDocOutput_filelist,
//        N); auto dependenciesFile = readSupplementaryOutputFileList(
//                                                                options::OPT_dependenciesFile_filelist,
//                                                                N);
//        auto referenceDependenciesFile = readSupplementaryOutputFileList(
//                                                                         options::OPT_referenceDependenciesFile_filelist,
//                                                                         N);
//        auto serializedDiagnostics = readSupplementaryOutputFileList(
//                                                                     options::OPT_serializedDiagnostics_filelist,
//                                                                     N);
//        auto loadedModuleTrace = readSupplementaryOutputFileList(
//                                                                 options::OPT_loadedModuleTrace_filelist,
//                                                                 N);
//        auto TBD = readSupplementaryOutputFileList(options::OPT_TBD_filelist,
//        N);
//
//        std::vector<OutputPaths> result;
//
//        for (unsigned i = 0; i < N; ++i) {
//          result.push_back(
//                           OutputPaths(i, objCHeaderOutput, moduleOutput,
//                           moduleDocOutput,
//                                       dependenciesFile,
//                                       referenceDependenciesFile,
//                                       serializedDiagnostics,
//                                       loadedModuleTrace, TBD));
//        }
//        return result;
//      }

//      static bool computeAllOutputs(
//                                    const ArgList &args, ArrayRef<std::string>
//                                    outputFileArgumentsForEveryInput,
//                                    ArrayRef<OutputPaths> suppFileListArgs,
//                                    const bool isSingleThreadedWMO, const bool
//                                    isOutputFilenameArgumentOneDirectory,
//                                    const bool
//                                    doOutputFilenameArgumentsCorrespondToInputs,
//                                    StringRef moduleName, DiagnosticEngine
//                                    &diags, FrontendInputsAndOutputs &io) {
//
//        StringRef filenameOfFirstInput =
//        io.hasSingleInput() ? io.getFilenameOfFirstInput() : StringRef();
//
//        // FIXME: dmu Get rid of this someday.
//        StringRef outputFilenameOfFirstInput = computeOutputFilename(
//                                                                     args,
//                                                                     outputFileArgumentsForEveryInput,
//                                                                     outputFilenameArg,
//                                                                     isOutputFilenameArgumentOneDirectory,
//                                                                     doOutputFilenameArgumentsCorrespondToInputs,
//                                                                     filenameOfFirstInput,
//                                                                     input.file(),
//                                                                     input.isPrimary(),
//                                                                     diags);
//
//        unsigned i = 0;
//        io.setIsSingleThreadedWMO(isSingleThreadedWMO);
//        return io.forEachInputProducingOutput(
//                                              [&](InputFile &input) -> bool {
//                                              // ONLY ONCE IF WMONON??
//
//                                                StringRef ithOutputFilenameArg
//                                                =
//                                                outputFileArgumentsForEveryInput.empty()
//                                                ? StringRef()
//                                                :
//                                                StringRef(outputFileArgumentsForEveryInput[i]);
//
//                                                const OutputPaths
//                                                &ithSuppFileListArg =
//                                                suppFileListArgs[i];
//
//                                                Optional<OutputPaths>
//                                                outputPaths =
//                                                computeOutputsForOneInput(
//                                                                                                              args, outputFileArgumentsForEveryInput, ithOutputFilenameArg,
//                                                                                                              isOutputFilenameArgumentOneDirectory,
//                                                                                                              doOutputFilenameArgumentsCorrespondToInputs, filenameOfFirstInput,
//                                                                                                              outputFilenameOfFirstInput, input, moduleName, ithSuppFileListArg,
//                                                                                                              diags);
//
//                                                if (!outputPaths)
//                                                  return true;
//                                                input.setOutputs(*outputPaths);
//                                                ++i;
//                                                return false;
//                                              });
//      }
//      Optional<std::vector<std::string>>
//      FrontendArgsToOutputsConverter::readSupplementaryOutputFileList(swift::options::ID
//      id, unsigned N) {
//        Arg *A = Args.getLastArg(id);
//        if (!A)
//          return None;
//        auto r = readOutputFilelist(A->getValue(), Diags);
//        assert(r.size() == N);
//        return r;
//      }
