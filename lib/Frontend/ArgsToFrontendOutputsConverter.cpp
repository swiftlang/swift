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

#include "ArgsToFrontendOutputsConverter.h"

#include "ArgsToFrontendInputsConverter.h"
#include "ArgsToFrontendOptionsConverter.h"
#include "swift/AST/DiagnosticsFrontend.h"
#include "swift/Basic/OutputFileMap.h"
#include "swift/Basic/Platform.h"
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

bool ArgsToFrontendOutputsConverter::convert(
    std::vector<std::string> &mainOutputs,
    std::vector<SupplementaryOutputPaths> &supplementaryOutputs) {

  Optional<OutputFilesComputer> ofc =
      OutputFilesComputer::create(Args, Diags, InputsAndOutputs);
  if (!ofc)
    return true;
  Optional<std::vector<std::string>> mains = ofc->computeOutputFiles();
  if (!mains)
    return true;

  Optional<std::vector<SupplementaryOutputPaths>> supplementaries =
      SupplementaryOutputPathsComputer(Args, Diags, InputsAndOutputs, *mains,
                                       ModuleName)
          .computeOutputPaths();
  if (!supplementaries)
    return true;

  mainOutputs = std::move(*mains);
  supplementaryOutputs = std::move(*supplementaries);
  return false;
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
  const StringRef firstInput =
      inputsAndOutputs.hasSingleInput()
          ? StringRef(inputsAndOutputs.getFilenameOfFirstInput())
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

  const file_types::ID outputType =
      FrontendOptions::formatForPrincipalOutputFileForAction(requestedAction);

  return OutputFilesComputer(
      diags, inputsAndOutputs, std::move(outputFileArguments),
      outputDirectoryArgument, firstInput, requestedAction,
      args.getLastArg(options::OPT_module_name),
      file_types::getExtension(outputType),
      FrontendOptions::doesActionProduceTextualOutput(requestedAction));
}

OutputFilesComputer::OutputFilesComputer(
    DiagnosticEngine &diags,
    const FrontendInputsAndOutputs &inputsAndOutputs,
    std::vector<std::string> outputFileArguments,
    const StringRef outputDirectoryArgument, const StringRef firstInput,
    const FrontendOptions::ActionType requestedAction,
    const llvm::opt::Arg *moduleNameArg, const StringRef suffix,
    const bool hasTextualOutput)
    : Diags(diags), InputsAndOutputs(inputsAndOutputs),
      OutputFileArguments(outputFileArguments),
      OutputDirectoryArgument(outputDirectoryArgument), FirstInput(firstInput),
      RequestedAction(requestedAction), ModuleNameArg(moduleNameArg),
      Suffix(suffix), HasTextualOutput(hasTextualOutput) {}

Optional<std::vector<std::string>>
OutputFilesComputer::computeOutputFiles() const {
  std::vector<std::string> outputFiles;
  unsigned i = 0;
  bool hadError = InputsAndOutputs.forEachInputProducingAMainOutputFile(
      [&](const InputFile &input) -> bool {

        StringRef outputArg = OutputFileArguments.empty()
                                  ? StringRef()
                                  : StringRef(OutputFileArguments[i++]);

        Optional<std::string> outputFile = computeOutputFile(outputArg, input);
        if (!outputFile)
          return true;
        outputFiles.push_back(*outputFile);
        return false;
      });
  return hadError ? None : Optional<std::vector<std::string>>(outputFiles);
}

Optional<std::string>
OutputFilesComputer::computeOutputFile(StringRef outputArg,
                                       const InputFile &input) const {
  // Return an empty string to signify no output.
  // The frontend does not currently produce a diagnostic
  // if a -o argument is present for such an action
  // for instance swiftc -frontend -o foo -interpret foo.swift
  if (!FrontendOptions::doesActionProduceOutput(RequestedAction))
    return std::string();

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
  std::string nameToStem =
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

SupplementaryOutputPathsComputer::SupplementaryOutputPathsComputer(
    const ArgList &args, DiagnosticEngine &diags,
    const FrontendInputsAndOutputs &inputsAndOutputs,
    ArrayRef<std::string> outputFiles, StringRef moduleName)
    : Args(args), Diags(diags), InputsAndOutputs(inputsAndOutputs),
      OutputFiles(outputFiles), ModuleName(moduleName),
      RequestedAction(
          ArgsToFrontendOptionsConverter::determineRequestedAction(Args)) {}

Optional<std::vector<SupplementaryOutputPaths>>
SupplementaryOutputPathsComputer::computeOutputPaths() const {
  Optional<std::vector<SupplementaryOutputPaths>> pathsFromUser =
      Args.hasArg(options::OPT_supplementary_output_file_map)
          ? readSupplementaryOutputFileMap()
          : getSupplementaryOutputPathsFromArguments();
  if (!pathsFromUser)
    return None;

  if (InputsAndOutputs.hasPrimaryInputs())
    assert(OutputFiles.size() == pathsFromUser->size());
  else if (InputsAndOutputs.isSingleThreadedWMO())
    assert(OutputFiles.size() == pathsFromUser->size() &&
           pathsFromUser->size() == 1);
  else {
    // Multi-threaded WMO is the exception
    assert(OutputFiles.size() == InputsAndOutputs.inputCount() &&
                   pathsFromUser->size() == InputsAndOutputs.hasInputs()
               ? 1
               : 0);
  }

  std::vector<SupplementaryOutputPaths> outputPaths;
  unsigned i = 0;
  bool hadError = InputsAndOutputs.forEachInputProducingSupplementaryOutput(
      [&](const InputFile &input) -> bool {
        if (auto suppPaths = computeOutputPathsForOneInput(
                OutputFiles[i], (*pathsFromUser)[i], input)) {
          ++i;
          outputPaths.push_back(*suppPaths);
          return false;
        }
        return true;
      });
  if (hadError)
    return None;
  return outputPaths;
}

Optional<std::vector<SupplementaryOutputPaths>>
SupplementaryOutputPathsComputer::getSupplementaryOutputPathsFromArguments()
    const {

  auto objCHeaderOutput = getSupplementaryFilenamesFromArguments(
      options::OPT_emit_objc_header_path);
  auto moduleOutput =
      getSupplementaryFilenamesFromArguments(options::OPT_emit_module_path);
  auto moduleDocOutput =
      getSupplementaryFilenamesFromArguments(options::OPT_emit_module_doc_path);
  auto dependenciesFile = getSupplementaryFilenamesFromArguments(
      options::OPT_emit_dependencies_path);
  auto referenceDependenciesFile = getSupplementaryFilenamesFromArguments(
      options::OPT_emit_reference_dependencies_path);
  auto serializedDiagnostics = getSupplementaryFilenamesFromArguments(
      options::OPT_serialize_diagnostics_path);
  auto fixItsOutput = getSupplementaryFilenamesFromArguments(
      options::OPT_emit_fixits_path);
  auto loadedModuleTrace = getSupplementaryFilenamesFromArguments(
      options::OPT_emit_loaded_module_trace_path);
  auto TBD = getSupplementaryFilenamesFromArguments(options::OPT_emit_tbd_path);
  auto moduleInterfaceOutput = getSupplementaryFilenamesFromArguments(
      options::OPT_emit_interface_path);

  if (!objCHeaderOutput || !moduleOutput || !moduleDocOutput ||
      !dependenciesFile || !referenceDependenciesFile ||
      !serializedDiagnostics || !fixItsOutput || !loadedModuleTrace || !TBD ||
      !moduleInterfaceOutput) {
    return None;
  }
  std::vector<SupplementaryOutputPaths> result;

  const unsigned N =
      InputsAndOutputs.countOfFilesProducingSupplementaryOutput();
  for (unsigned i = 0; i < N; ++i) {
    SupplementaryOutputPaths sop;
    sop.ObjCHeaderOutputPath = (*objCHeaderOutput)[i];
    sop.ModuleOutputPath = (*moduleOutput)[i];
    sop.ModuleDocOutputPath = (*moduleDocOutput)[i];
    sop.DependenciesFilePath = (*dependenciesFile)[i];
    sop.ReferenceDependenciesFilePath = (*referenceDependenciesFile)[i];
    sop.SerializedDiagnosticsPath = (*serializedDiagnostics)[i];
    sop.FixItsOutputPath = (*fixItsOutput)[i];
    sop.LoadedModuleTracePath = (*loadedModuleTrace)[i];
    sop.TBDPath = (*TBD)[i];
    sop.ModuleInterfaceOutputPath = (*moduleInterfaceOutput)[i];

    result.push_back(sop);
  }
  return result;
}

// Extend this routine for filelists if/when we have them.

Optional<std::vector<std::string>>
SupplementaryOutputPathsComputer::getSupplementaryFilenamesFromArguments(
    options::ID pathID) const {
  std::vector<std::string> paths = Args.getAllArgValues(pathID);

  const unsigned N =
      InputsAndOutputs.countOfFilesProducingSupplementaryOutput();

  if (paths.size() == N)
    return paths;

  if (paths.empty())
    return std::vector<std::string>(N, std::string());

  Diags.diagnose(SourceLoc(), diag::error_wrong_number_of_arguments,
                 Args.getLastArg(pathID)->getOption().getPrefixedName(), N,
                 paths.size());
  return None;
}

Optional<SupplementaryOutputPaths>
SupplementaryOutputPathsComputer::computeOutputPathsForOneInput(
    StringRef outputFile, const SupplementaryOutputPaths &pathsFromArguments,
    const InputFile &input) const {
  StringRef defaultSupplementaryOutputPathExcludingExtension =
      deriveDefaultSupplementaryOutputPathExcludingExtension(outputFile, input);

  using namespace options;

  auto dependenciesFilePath = determineSupplementaryOutputFilename(
      OPT_emit_dependencies, pathsFromArguments.DependenciesFilePath,
      file_types::TY_Dependencies, "",
      defaultSupplementaryOutputPathExcludingExtension);

  auto referenceDependenciesFilePath = determineSupplementaryOutputFilename(
      OPT_emit_reference_dependencies,
      pathsFromArguments.ReferenceDependenciesFilePath,
      file_types::TY_SwiftDeps, "",
      defaultSupplementaryOutputPathExcludingExtension);

  auto serializedDiagnosticsPath = determineSupplementaryOutputFilename(
      OPT_serialize_diagnostics, pathsFromArguments.SerializedDiagnosticsPath,
      file_types::TY_SerializedDiagnostics, "",
      defaultSupplementaryOutputPathExcludingExtension);

  // There is no non-path form of -emit-fixits-path
  auto fixItsOutputPath = pathsFromArguments.FixItsOutputPath;

  auto objcHeaderOutputPath = determineSupplementaryOutputFilename(
      OPT_emit_objc_header, pathsFromArguments.ObjCHeaderOutputPath,
      file_types::TY_ObjCHeader, "",
      defaultSupplementaryOutputPathExcludingExtension);

  auto loadedModuleTracePath = determineSupplementaryOutputFilename(
      OPT_emit_loaded_module_trace, pathsFromArguments.LoadedModuleTracePath,
      file_types::TY_ModuleTrace, "",
      defaultSupplementaryOutputPathExcludingExtension);

  auto tbdPath = determineSupplementaryOutputFilename(
      OPT_emit_tbd, pathsFromArguments.TBDPath, file_types::TY_TBD, "",
      defaultSupplementaryOutputPathExcludingExtension);

  auto moduleDocOutputPath = determineSupplementaryOutputFilename(
      OPT_emit_module_doc, pathsFromArguments.ModuleDocOutputPath,
      file_types::TY_SwiftModuleDocFile, "",
      defaultSupplementaryOutputPathExcludingExtension);

  // There is no non-path form of -emit-interface-path
  auto moduleInterfaceOutputPath = pathsFromArguments.ModuleInterfaceOutputPath;

  ID emitModuleOption;
  std::string moduleExtension;
  std::string mainOutputIfUsableForModule;
  deriveModulePathParameters(emitModuleOption, moduleExtension,
                             mainOutputIfUsableForModule);

  auto moduleOutputPath = determineSupplementaryOutputFilename(
      emitModuleOption, pathsFromArguments.ModuleOutputPath,
      file_types::TY_SwiftModuleFile, mainOutputIfUsableForModule,
      defaultSupplementaryOutputPathExcludingExtension);

  SupplementaryOutputPaths sop;
  sop.ObjCHeaderOutputPath = objcHeaderOutputPath;
  sop.ModuleOutputPath = moduleOutputPath;
  sop.ModuleDocOutputPath = moduleDocOutputPath;
  sop.DependenciesFilePath = dependenciesFilePath;
  sop.ReferenceDependenciesFilePath = referenceDependenciesFilePath;
  sop.SerializedDiagnosticsPath = serializedDiagnosticsPath;
  sop.FixItsOutputPath = fixItsOutputPath;
  sop.LoadedModuleTracePath = loadedModuleTracePath;
  sop.TBDPath = tbdPath;
  sop.ModuleInterfaceOutputPath = moduleInterfaceOutputPath;
  return sop;
}

StringRef SupplementaryOutputPathsComputer::
    deriveDefaultSupplementaryOutputPathExcludingExtension(
        StringRef outputFilename, const InputFile &input) const {
  // Put the supplementary output file next to the output file if possible.
  if (!outputFilename.empty() && outputFilename != "-")
    return outputFilename;

  if (input.isPrimary() && input.file() != "-")
    return llvm::sys::path::filename(input.file());

  return ModuleName;
}

std::string
SupplementaryOutputPathsComputer::determineSupplementaryOutputFilename(
    options::ID emitOpt, std::string pathFromArguments, file_types::ID type,
    StringRef mainOutputIfUsable,
    StringRef defaultSupplementaryOutputPathExcludingExtension) const {

  if (!pathFromArguments.empty())
    return pathFromArguments;

  if (!Args.hasArg(emitOpt))
    return std::string();

  if (!mainOutputIfUsable.empty()) {
    return mainOutputIfUsable.str();
  }

  llvm::SmallString<128> path(defaultSupplementaryOutputPathExcludingExtension);
  llvm::sys::path::replace_extension(path, file_types::getExtension(type));
  return path.str().str();
};

void SupplementaryOutputPathsComputer::deriveModulePathParameters(
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

  extension = file_types::getExtension(
      isSIB ? file_types::TY_SIB : file_types::TY_SwiftModuleFile);

  mainOutputIfUsable =
      canUseMainOutputForModule && !OutputFiles.empty() ? OutputFiles[0] : "";
}

static SupplementaryOutputPaths
createFromTypeToPathMap(const TypeToPathMap *map) {
  SupplementaryOutputPaths paths;
  if (!map)
    return paths;
  const std::pair<file_types::ID, std::string &> typesAndStrings[] = {
      {file_types::TY_ObjCHeader, paths.ObjCHeaderOutputPath},
      {file_types::TY_SwiftModuleFile, paths.ModuleOutputPath},
      {file_types::TY_SwiftModuleDocFile, paths.ModuleDocOutputPath},
      {file_types::TY_Dependencies, paths.DependenciesFilePath},
      {file_types::TY_SwiftDeps, paths.ReferenceDependenciesFilePath},
      {file_types::TY_SerializedDiagnostics, paths.SerializedDiagnosticsPath},
      {file_types::TY_ModuleTrace, paths.LoadedModuleTracePath},
      {file_types::TY_TBD, paths.TBDPath},
      {file_types::TY_SwiftModuleInterfaceFile,paths.ModuleInterfaceOutputPath}
  };
  for (const std::pair<file_types::ID, std::string &> &typeAndString :
       typesAndStrings) {
    auto const out = map->find(typeAndString.first);
    typeAndString.second = out == map->end() ? "" : out->second;
  }
  return paths;
}

Optional<std::vector<SupplementaryOutputPaths>>
SupplementaryOutputPathsComputer::readSupplementaryOutputFileMap() const {
  if (Arg *A = Args.getLastArg(options::OPT_emit_objc_header_path,
                               options::OPT_emit_module_path,
                               options::OPT_emit_module_doc_path,
                               options::OPT_emit_dependencies_path,
                               options::OPT_emit_reference_dependencies_path,
                               options::OPT_serialize_diagnostics_path,
                               options::OPT_emit_loaded_module_trace_path,
                               options::OPT_emit_tbd_path)) {
    Diags.diagnose(SourceLoc(),
                   diag::error_cannot_have_supplementary_outputs,
                   A->getSpelling(), "-supplementary-output-file-map");
    return None;
  }
  const StringRef supplementaryFileMapPath =
      Args.getLastArgValue(options::OPT_supplementary_output_file_map);
  llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>> buffer =
      llvm::MemoryBuffer::getFile(supplementaryFileMapPath);
  if (!buffer) {
    Diags.diagnose(SourceLoc(), diag::cannot_open_file,
                   supplementaryFileMapPath, buffer.getError().message());
    return None;
  }
  llvm::Expected<OutputFileMap> OFM =
      OutputFileMap::loadFromBuffer(std::move(buffer.get()), "");
  if (auto Err = OFM.takeError()) {
    Diags.diagnose(SourceLoc(),
                   diag::error_unable_to_load_supplementary_output_file_map,
                   supplementaryFileMapPath, llvm::toString(std::move(Err)));
    return None;
  }

  std::vector<SupplementaryOutputPaths> outputPaths;
  bool hadError = false;
  InputsAndOutputs.forEachInputProducingSupplementaryOutput(
      [&](const InputFile &input) -> bool {
        const TypeToPathMap *mapForInput =
            OFM->getOutputMapForInput(input.file());
        if (!mapForInput) {
          Diags.diagnose(
              SourceLoc(),
              diag::error_missing_entry_in_supplementary_output_file_map,
              supplementaryFileMapPath, input.file());
          hadError = true;
        }
        outputPaths.push_back(createFromTypeToPathMap(mapForInput));
        return false;
      });
  if (hadError)
    return None;

  return outputPaths;
}
