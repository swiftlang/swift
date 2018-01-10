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
#include "swift/Basic/SupplementaryOutputPaths.h"
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

Optional<std::pair<std::vector<std::string>,
                   std::vector<const SupplementaryOutputPaths>>>
ArgsToFrontendOutputsConverter::convert() {
  const auto requestedAction =
      ArgsToFrontendOptionsConverter::determineRequestedAction(Args);

  if (!FrontendOptions::doesActionProduceOutput(requestedAction))
    return std::make_pair(std::vector<std::string>(),
                          std::vector<const SupplementaryOutputPaths>());

  Optional<std::vector<std::string>> outputFiles =
      OutputFilesComputer(Args, Diags, InputsAndOutputs).computeOutputFiles();
  if (!outputFiles)
    return None;
  Optional<std::vector<const SupplementaryOutputPaths>> outputPaths =
      OutputPathsComputer(Args, Diags, InputsAndOutputs, *outputFiles,
                          ModuleName)
          .computeOutputPaths();
  if (!outputPaths)
    return None;

  return std::make_pair(*outputFiles, *outputPaths);
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

  bool hadError = false;
  unsigned i = 0;
  InputsAndOutputs.forEachInputProducingOutput(
      [&](const InputFile &input) -> void {

        StringRef outputArg = OutputFileArguments.empty()
                                  ? StringRef()
                                  : StringRef(OutputFileArguments[i]);
        ++i;
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

OutputPathsComputer::OutputPathsComputer(
    const ArgList &args, DiagnosticEngine &diags,
    const FrontendInputsAndOutputs &inputsAndOutputs,
    ArrayRef<std::string> outputFiles, StringRef moduleName)
    : Args(args), Diags(diags), InputsAndOutputs(inputsAndOutputs),
      OutputFiles(outputFiles), ModuleName(moduleName),
      SupplementaryFilenamesFromFilelists(
          getSupplementaryFilenamesFromFilelists(
              Args, Diags,
              InputsAndOutputs.countOfFilesProducingSupplementaryOutput())),
      RequestedAction(
          ArgsToFrontendOptionsConverter::determineRequestedAction(Args)) {}

Optional<std::vector<const SupplementaryOutputPaths>>
OutputPathsComputer::computeOutputPaths() const {
  std::vector<const SupplementaryOutputPaths> outputs;
  bool hadError = false;
  unsigned i = 0;
  InputsAndOutputs.forEachInputProducingSupplementaryOutput(
      [&](const InputFile &input) -> void {
        Optional<SupplementaryOutputPaths> outputPaths =
            computeOutputPathsForOneInput(
                OutputFiles[i], SupplementaryFilenamesFromFilelists[i], input);
        i++;
        if (!outputPaths)
          hadError = true;
        outputs.push_back(*outputPaths);
      });
  return hadError
             ? None
             : Optional<std::vector<const SupplementaryOutputPaths>>(outputs);
}

std::vector<SupplementaryOutputPaths>
OutputPathsComputer::getSupplementaryFilenamesFromFilelists(
    const ArgList &args, DiagnosticEngine &diags, const unsigned inputCount) {

  auto objCHeaderOutput = readSupplementaryOutputFileList(
      args, diags, options::OPT_objCHeaderOutput_filelist, inputCount);
  auto moduleOutput = readSupplementaryOutputFileList(
      args, diags, options::OPT_moduleOutput_filelist, inputCount);
  auto moduleDocOutput = readSupplementaryOutputFileList(
      args, diags, options::OPT_moduleDocOutput_filelist, inputCount);
  auto dependenciesFile = readSupplementaryOutputFileList(
      args, diags, options::OPT_dependenciesFile_filelist, inputCount);
  auto referenceDependenciesFile = readSupplementaryOutputFileList(
      args, diags, options::OPT_referenceDependenciesFile_filelist, inputCount);
  auto serializedDiagnostics = readSupplementaryOutputFileList(
      args, diags, options::OPT_serializedDiagnostics_filelist, inputCount);
  auto loadedModuleTrace = readSupplementaryOutputFileList(
      args, diags, options::OPT_loadedModuleTrace_filelist, inputCount);
  auto TBD = readSupplementaryOutputFileList(
      args, diags, options::OPT_TBD_filelist, inputCount);

  std::vector<SupplementaryOutputPaths> result;

  for (unsigned i = 0; i < inputCount; ++i) {
    result.push_back(SupplementaryOutputPaths(
        i, objCHeaderOutput, moduleOutput, moduleDocOutput, dependenciesFile,
        referenceDependenciesFile, serializedDiagnostics, loadedModuleTrace,
        TBD));
  }
  return result;
}

Optional<std::vector<std::string>>
OutputPathsComputer::readSupplementaryOutputFileList(const ArgList &args,
                                                     DiagnosticEngine &diags,
                                                     swift::options::ID id,
                                                     unsigned requiredCount) {
  Arg *A = args.getLastArg(id);
  if (!A)
    return None;
  auto r =
      ArgsToFrontendOutputsConverter::readOutputFileList(A->getValue(), diags);
  assert(r.size() == requiredCount);
  return r;
}

Optional<SupplementaryOutputPaths>
OutputPathsComputer::computeOutputPathsForOneInput(
    StringRef outputFile, const SupplementaryOutputPaths &pathsFromFilelists,
    const InputFile &input) const {
  StringRef implicitBasis = deriveImplicitBasis(outputFile, input);

  using namespace options;

  auto dependenciesFilePath = determineSupplementaryOutputFilename(
      OPT_emit_dependencies_path, OPT_emit_dependencies,
      pathsFromFilelists.DependenciesFilePath, "d", "", implicitBasis);
  if (!dependenciesFilePath)
    return None;

  auto referenceDependenciesFilePath = determineSupplementaryOutputFilename(
      OPT_emit_reference_dependencies_path, OPT_emit_reference_dependencies,
      pathsFromFilelists.ReferenceDependenciesFilePath, "swiftdeps", "",
      implicitBasis);
  if (!referenceDependenciesFilePath)
    return None;

  auto serializedDiagnosticsPath = determineSupplementaryOutputFilename(
      OPT_serialize_diagnostics_path, OPT_serialize_diagnostics,
      pathsFromFilelists.SerializedDiagnosticsPath, "dia", "", implicitBasis);

  if (!serializedDiagnosticsPath)
    return None;

  auto objCHeaderOutputPath = determineSupplementaryOutputFilename(
      OPT_emit_objc_header_path, OPT_emit_objc_header,
      pathsFromFilelists.ObjCHeaderOutputPath, "h", "", implicitBasis);
  if (!objCHeaderOutputPath)
    return None;

  auto loadedModuleTracePath = determineSupplementaryOutputFilename(
      OPT_emit_loaded_module_trace_path, OPT_emit_loaded_module_trace,
      pathsFromFilelists.LoadedModuleTracePath, "trace.json", "",
      implicitBasis);
  if (!loadedModuleTracePath)
    return None;

  auto tbdPath = determineSupplementaryOutputFilename(
      OPT_emit_tbd_path, OPT_emit_tbd, pathsFromFilelists.TBDPath, "tbd", "",
      implicitBasis);
  if (!tbdPath)
    return None;

  auto moduleDocOutputPath = determineSupplementaryOutputFilename(
      OPT_emit_module_doc_path, OPT_emit_module_doc,
      pathsFromFilelists.ModuleDocOutputPath, SERIALIZED_MODULE_DOC_EXTENSION,
      "", implicitBasis);
  if (!moduleDocOutputPath)
    return None;

  ID emitModuleOption;
  std::string moduleExtension;
  std::string mainOutputIfUsableForModule;
  deriveModulePathParameters(emitModuleOption, moduleExtension,
                             mainOutputIfUsableForModule);

  auto moduleOutputPath = determineSupplementaryOutputFilename(
      OPT_emit_module_path, emitModuleOption,
      pathsFromFilelists.ModuleOutputPath, moduleExtension,
      mainOutputIfUsableForModule, implicitBasis);
  if (!moduleOutputPath)
    return None;

  return SupplementaryOutputPaths(
      *objCHeaderOutputPath, *moduleOutputPath, *moduleDocOutputPath,
      *dependenciesFilePath, *referenceDependenciesFilePath,
      *serializedDiagnosticsPath, *loadedModuleTracePath, *tbdPath);
}

StringRef
OutputPathsComputer::deriveImplicitBasis(StringRef outputFilename,
                                         const InputFile &input) const {
  if (!outputFilename.empty() && outputFilename != "-")
    // Put the serialized diagnostics file next to the output file.
    return outputFilename;

  // If we have a primary input, so use that as the basis for the name of the
  // serialized diagnostics file, otherwise fall back on the
  // module name.
  if (input.isPrimary() && input.file() != "-")
    return llvm::sys::path::filename(input.file());

  return ModuleName;
}

Optional<std::string> OutputPathsComputer::determineSupplementaryOutputFilename(
    options::ID pathOpt, options::ID emitOpt, StringRef pathFromFilelists,
    StringRef extension, StringRef mainOutputIfUsable,
    StringRef implicitBasis) const {
  using namespace options;

  {
    const Arg *argWithPath = Args.getLastArg(pathOpt);
    if (argWithPath)
      Args.ClaimAllArgs(pathOpt);
    if (argWithPath && !pathFromFilelists.empty()) {
      // FIXME: dmu write out arg name and file list name
      Diags.diagnose(SourceLoc(),
                     diag::error_cannot_have_filelist_and_argument);
      return None; // FIXME: dmu bail on supp filelist and argument?
    }
    if (!pathFromFilelists.empty())
      return pathFromFilelists.str();
    if (argWithPath)
      return std::string(argWithPath->getValue());
  }

  if (!Args.hasArg(emitOpt))
    return std::string();

  if (!mainOutputIfUsable.empty()) {
    return mainOutputIfUsable.str();
  }

  llvm::SmallString<128> path(implicitBasis);
  llvm::sys::path::replace_extension(path, extension);
  return path.str().str();
};

void OutputPathsComputer::deriveModulePathParameters(
    options::ID &emitOption, std::string &extension,
    std::string &mainOutputIfUsable) const {

  bool isSIB = RequestedAction == FrontendOptions::ActionType::EmitSIB ||
               RequestedAction == FrontendOptions::ActionType::EmitSIBGen;

  emitOption = !isSIB ? options::OPT_emit_module
                      : RequestedAction == FrontendOptions::ActionType::EmitSIB
                            ? options::OPT_emit_sib
                            : options::OPT_emit_sibgen;

  bool canUseMainOutputForModule =
      RequestedAction == FrontendOptions::ActionType::MergeModules ||
      RequestedAction == FrontendOptions::ActionType::EmitModuleOnly || isSIB;

  extension = isSIB ? SIB_EXTENSION : SERIALIZED_MODULE_EXTENSION;

  mainOutputIfUsable =
      canUseMainOutputForModule && !OutputFiles.empty() ? OutputFiles[0] : "";
}
