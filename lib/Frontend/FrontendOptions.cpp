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
  // If we have one primary input and it's a filename with extension "sil",
  // treat the input as SIL.
  if (const auto fn = uniquePrimaryInputFilename()) {
    return llvm::sys::path::extension(fn.getValue()).endswith(SIL_EXTENSION);
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
      if (i == getOptionalPrimaryInput()->Index)
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


bool FrontendOptions::isOutputFileDirectory() const {
  return hasNamedOutputFile() &&
         llvm::sys::fs::is_directory(getSingleOutputFilename());
}

