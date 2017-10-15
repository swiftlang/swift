//===--- FrontendOptions.cpp ----------------------------------------------===//
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

bool FrontendInputs::shouldTreatAsLLVM() const {
  if (hasUniqueInputFilename()) {
    StringRef Input(getFilenameOfFirstInput());
    return llvm::sys::path::extension(Input).endswith(LLVM_BC_EXTENSION) ||
           llvm::sys::path::extension(Input).endswith(LLVM_IR_EXTENSION);
  }
  return false;
}

StringRef FrontendInputs::baseNameOfOutput(const llvm::opt::ArgList &Args,
                                           StringRef ModuleName) const {
  StringRef pifn = primaryInputFilenameIfAny();
  if (!pifn.empty()) {
    return llvm::sys::path::stem(pifn);
  }
  bool UserSpecifiedModuleName = Args.getLastArg(options::OPT_module_name);
  if (!UserSpecifiedModuleName && hasUniqueInputFilename()) {
    return llvm::sys::path::stem(getFilenameOfFirstInput());
  }
  return ModuleName;
}

bool FrontendInputs::shouldTreatAsSIL() const {
  if (hasUniqueInputFilename()) {
    // If we have exactly one input filename, and its extension is "sil",
    // treat the input as SIL.
    StringRef Input(getFilenameOfFirstInput());
    return llvm::sys::path::extension(Input).endswith(SIL_EXTENSION);
  }
  StringRef Input = primaryInputFilenameIfAny();
  if (!Input.empty()) {
    // If we have a primary input and it's a filename with extension "sil",
    // treat the input as SIL.
    return llvm::sys::path::extension(Input).endswith(SIL_EXTENSION);
  }
  return false;
}

bool FrontendInputs::verifyInputs(DiagnosticEngine &Diags, bool TreatAsSIL,
                                  bool isREPLRequested,
                                  bool isNoneRequested) const {
  if (isREPLRequested) {
    if (hasInputFilenames()) {
      Diags.diagnose(SourceLoc(), diag::error_repl_requires_no_input_files);
      return true;
    }
  } else if (TreatAsSIL && hasPrimaryInput()) {
    // If we have the SIL as our primary input, we can waive the one file
    // requirement as long as all the other inputs are SIBs.
    for (unsigned i = 0, e = inputFilenameCount(); i != e; ++i) {
      if (i == getPrimaryInput()->Index)
        continue;

      StringRef File(getInputFilenames()[i]);
      if (!llvm::sys::path::extension(File).endswith(SIB_EXTENSION)) {
        Diags.diagnose(SourceLoc(),
                       diag::error_mode_requires_one_sil_multi_sib);
        return true;
      }
    }
  } else if (TreatAsSIL) {
    if (!hasUniqueInputFilename()) {
      Diags.diagnose(SourceLoc(), diag::error_mode_requires_one_input_file);
      return true;
    }
  } else if (!isNoneRequested) {
    if (!hasInputFilenames()) {
      Diags.diagnose(SourceLoc(), diag::error_mode_requires_an_input_file);
      return true;
    }
  }
  return false;
}

void FrontendInputs::transformInputFilenames(
    const llvm::function_ref<std::string(std::string)> &fn) {
  for (auto &InputFile : InputFilenames) {
    InputFile = fn(InputFile);
  }
}

void FrontendInputs::setInputFilenamesAndPrimaryInput(
    DiagnosticEngine &Diags, llvm::opt::ArgList &Args) {
  if (const Arg *filelistPath = Args.getLastArg(options::OPT_filelist)) {
    readInputFileList(Diags, Args, filelistPath);
    return;
  }
  for (const Arg *A :
       Args.filtered(options::OPT_INPUT, options::OPT_primary_file)) {
    if (A->getOption().matches(options::OPT_INPUT)) {
      addInputFilename(A->getValue());
    } else if (A->getOption().matches(options::OPT_primary_file)) {
      setPrimaryInput(SelectedInput(inputFilenameCount()));
      addInputFilename(A->getValue());
    } else {
      llvm_unreachable("Unknown input-related argument!");
    }
  }
}

/// Try to read an input file list file.
///
/// Returns false on error.
void FrontendInputs::readInputFileList(DiagnosticEngine &diags,
                                       llvm::opt::ArgList &Args,
                                       const llvm::opt::Arg *filelistPath) {
  llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>> buffer =
      llvm::MemoryBuffer::getFile(filelistPath->getValue());
  if (!buffer) {
    diags.diagnose(SourceLoc(), diag::cannot_open_file,
                   filelistPath->getValue(), buffer.getError().message());
    return;
  }

  const Arg *primaryFileArg = Args.getLastArg(options::OPT_primary_file);
  unsigned primaryFileIndex = 0;

  bool foundPrimaryFile = false;

  for (StringRef line : make_range(llvm::line_iterator(*buffer.get()), {})) {
    addInputFilename(line);

    if (foundPrimaryFile || primaryFileArg == nullptr)
      continue;
    if (line == primaryFileArg->getValue())
      foundPrimaryFile = true;
    else
      ++primaryFileIndex;
  }

  if (primaryFileArg && !foundPrimaryFile) {
    diags.diagnose(SourceLoc(), diag::error_primary_file_not_found,
                   primaryFileArg->getValue(), filelistPath->getValue());
    return;
  }

  if (primaryFileArg)
    setPrimaryInput(SelectedInput(primaryFileIndex));
  assert(!Args.hasArg(options::OPT_INPUT) && "mixing -filelist with inputs");
}

bool FrontendOptions::actionHasOutput() const {
  switch (RequestedAction) {
  case NoneAction:
  case Parse:
  case Typecheck:
  case DumpParse:
  case DumpAST:
  case EmitSyntax:
  case DumpInterfaceHash:
  case PrintAST:
  case DumpScopeMaps:
  case DumpTypeRefinementContexts:
    return false;
  case EmitPCH:
  case EmitSILGen:
  case EmitSIL:
  case EmitSIBGen:
  case EmitSIB:
  case EmitModuleOnly:
  case MergeModules:
    return true;
  case Immediate:
  case REPL:
    return false;
  case EmitAssembly:
  case EmitIR:
  case EmitBC:
  case EmitObject:
  case EmitImportedModules:
    return true;
  }
  llvm_unreachable("Unknown ActionType");
}

bool FrontendOptions::actionIsImmediate() const {
  switch (RequestedAction) {
  case NoneAction:
  case Parse:
  case Typecheck:
  case DumpParse:
  case DumpAST:
  case EmitSyntax:
  case DumpInterfaceHash:
  case PrintAST:
  case DumpScopeMaps:
  case DumpTypeRefinementContexts:
  case EmitPCH:
  case EmitSILGen:
  case EmitSIL:
  case EmitSIBGen:
  case EmitSIB:
  case EmitModuleOnly:
  case MergeModules:
    return false;
  case Immediate:
  case REPL:
    return true;
  case EmitAssembly:
  case EmitIR:
  case EmitBC:
  case EmitObject:
  case EmitImportedModules:
    return false;
  }
  llvm_unreachable("Unknown ActionType");
}

void FrontendOptions::forAllOutputPaths(
    std::function<void(const std::string &)> fn) const {
  if (RequestedAction != FrontendOptions::EmitModuleOnly &&
      RequestedAction != FrontendOptions::MergeModules) {
    for (const std::string &OutputFileName : OutputFilenames) {
      fn(OutputFileName);
    }
  }
  const std::string *outputs[] = {
    &ModuleOutputPath,
    &ModuleDocOutputPath,
    &ObjCHeaderOutputPath
  };
  for (const std::string *next : outputs) {
    if (!next->empty())
      fn(*next);
  }
}

void FrontendOptions::setModuleName(DiagnosticEngine &Diags,
                                    const llvm::opt::ArgList &Args) {
  const Arg *A = Args.getLastArg(options::OPT_module_name);
  if (A) {
    ModuleName = A->getValue();
  } else if (ModuleName.empty()) {
    // The user did not specify a module name, so determine a default fallback
    // based on other options.

    // Note: this code path will only be taken when running the frontend
    // directly; the driver should always pass -module-name when invoking the
    // frontend.
    ModuleName = determineFallbackModuleName();
  }

  if (Lexer::isIdentifier(ModuleName) &&
      (ModuleName != STDLIB_NAME || ParseStdlib)) {
    return;
  }
  if (!actionHasOutput() || isCompilingExactlyOneSwiftFile()) {
    ModuleName = "main";
    return;
  }
  auto DID = (ModuleName == STDLIB_NAME) ? diag::error_stdlib_module_name
                                         : diag::error_bad_module_name;
  Diags.diagnose(SourceLoc(), DID, ModuleName, A == nullptr);
  ModuleName = "__bad__";
}

StringRef FrontendOptions::originalPath() const {
  if (hasNamedOutputFile())
    // Put the serialized diagnostics file next to the output file.
    return getSingleOutputFilename();

  StringRef fn = Inputs.primaryInputFilenameIfAny();
  // If we have a primary input, so use that as the basis for the name of the
  // serialized diagnostics file, otherwise fall back on the
  // module name.
  return !fn.empty() ? llvm::sys::path::filename(fn) : StringRef(ModuleName);
}

StringRef FrontendOptions::determineFallbackModuleName() const {
  // Note: this code path will only be taken when running the frontend
  // directly; the driver should always pass -module-name when invoking the
  // frontend.
  if (RequestedAction == FrontendOptions::REPL) {
    // Default to a module named "REPL" if we're in REPL mode.
    return "REPL";
  }
  // In order to pass Driver/options.swift test must leave ModuleName empty
  if (!Inputs.hasInputFilenames()) {
    return StringRef();
  }
  StringRef OutputFilename = getSingleOutputFilename();
  bool useOutputFilename = isOutputFilePlainFile();
  return llvm::sys::path::stem(
      useOutputFilename ? OutputFilename
                        : StringRef(Inputs.getFilenameOfFirstInput()));
}

/// Try to read an output file list file.
static void readOutputFileList(DiagnosticEngine &diags,
                               std::vector<std::string> &outputFiles,
                               const llvm::opt::Arg *filelistPath) {
  llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>> buffer =
      llvm::MemoryBuffer::getFile(filelistPath->getValue());
  if (!buffer) {
    diags.diagnose(SourceLoc(), diag::cannot_open_file,
                   filelistPath->getValue(), buffer.getError().message());
  }
  for (StringRef line : make_range(llvm::line_iterator(*buffer.get()), {})) {
    outputFiles.push_back(line);
  }
}

void FrontendOptions::setOutputFileList(swift::DiagnosticEngine &Diags,
                                        const llvm::opt::ArgList &Args) {
  if (const Arg *A = Args.getLastArg(options::OPT_output_filelist)) {
    readOutputFileList(Diags, OutputFilenames, A);
    assert(!Args.hasArg(options::OPT_o) &&
           "don't use -o with -output-filelist");
  } else {
    OutputFilenames = Args.getAllArgValues(options::OPT_o);
  }
}

bool FrontendOptions::isOutputFileDirectory() const {
  return hasNamedOutputFile() &&
         llvm::sys::fs::is_directory(getSingleOutputFilename());
}

bool FrontendOptions::isOutputFilePlainFile() const {
  return hasNamedOutputFile() &&
         !llvm::sys::fs::is_directory(getSingleOutputFilename());
}
