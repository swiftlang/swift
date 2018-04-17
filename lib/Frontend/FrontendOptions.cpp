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

bool FrontendOptions::needsProperModuleName(ActionType action) {
  switch (action) {
  case ActionType::NoneAction:
  case ActionType::Parse:
  case ActionType::Typecheck:
  case ActionType::DumpParse:
  case ActionType::DumpAST:
  case ActionType::EmitSyntax:
  case ActionType::DumpInterfaceHash:
  case ActionType::PrintAST:
  case ActionType::DumpScopeMaps:
  case ActionType::DumpTypeRefinementContexts:
    return false;
  case ActionType::EmitPCH:
  case ActionType::EmitSILGen:
  case ActionType::EmitSIL:
  case ActionType::EmitSIBGen:
  case ActionType::EmitSIB:
  case ActionType::EmitModuleOnly:
  case ActionType::MergeModules:
    return true;
  case ActionType::Immediate:
  case ActionType::REPL:
    return false;
  case ActionType::EmitAssembly:
  case ActionType::EmitIR:
  case ActionType::EmitBC:
  case ActionType::EmitObject:
  case ActionType::EmitImportedModules:
    return true;
  }
  llvm_unreachable("Unknown ActionType");
}

bool FrontendOptions::isActionImmediate(ActionType action) {
  switch (action) {
  case ActionType::NoneAction:
  case ActionType::Parse:
  case ActionType::Typecheck:
  case ActionType::DumpParse:
  case ActionType::DumpAST:
  case ActionType::EmitSyntax:
  case ActionType::DumpInterfaceHash:
  case ActionType::PrintAST:
  case ActionType::DumpScopeMaps:
  case ActionType::DumpTypeRefinementContexts:
  case ActionType::EmitPCH:
  case ActionType::EmitSILGen:
  case ActionType::EmitSIL:
  case ActionType::EmitSIBGen:
  case ActionType::EmitSIB:
  case ActionType::EmitModuleOnly:
  case ActionType::MergeModules:
    return false;
  case ActionType::Immediate:
  case ActionType::REPL:
    return true;
  case ActionType::EmitAssembly:
  case ActionType::EmitIR:
  case ActionType::EmitBC:
  case ActionType::EmitObject:
  case ActionType::EmitImportedModules:
    return false;
  }
  llvm_unreachable("Unknown ActionType");
}

bool FrontendOptions::shouldActionOnlyParse(ActionType action) {
  switch (action) {
  case FrontendOptions::ActionType::Parse:
  case FrontendOptions::ActionType::DumpParse:
  case FrontendOptions::ActionType::EmitSyntax:
  case FrontendOptions::ActionType::DumpInterfaceHash:
  case FrontendOptions::ActionType::EmitImportedModules:
    return true;
  default:
    return false;
  }
}

void FrontendOptions::forAllOutputPaths(
    const InputFile &input, std::function<void(StringRef)> fn) const {
  if (RequestedAction != FrontendOptions::ActionType::EmitModuleOnly &&
      RequestedAction != FrontendOptions::ActionType::MergeModules) {
    if (InputsAndOutputs.isWholeModule())
      InputsAndOutputs.forEachOutputFilename(fn);
    else
      fn(input.outputFilename());
  }
  const SupplementaryOutputPaths &outs =
      input.getPrimarySpecificPaths().SupplementaryOutputs;
  const std::string *outputs[] = {&outs.ModuleOutputPath,
                                  &outs.ModuleDocOutputPath,
                                  &outs.ObjCHeaderOutputPath};
  for (const std::string *next : outputs) {
    if (!next->empty())
      fn(*next);
  }
}

StringRef
FrontendOptions::suffixForPrincipalOutputFileForAction(ActionType action) {
  switch (action) {
  case ActionType::NoneAction:
    return StringRef();

  case ActionType::Parse:
  case ActionType::Typecheck:
  case ActionType::DumpParse:
  case ActionType::DumpInterfaceHash:
  case ActionType::DumpAST:
  case ActionType::EmitSyntax:
  case ActionType::PrintAST:
  case ActionType::DumpScopeMaps:
  case ActionType::DumpTypeRefinementContexts:
    return StringRef();

  case ActionType::EmitPCH:
    return PCH_EXTENSION;

  case ActionType::EmitSILGen:
  case ActionType::EmitSIL:
    return SIL_EXTENSION;

  case ActionType::EmitSIBGen:
  case ActionType::EmitSIB:
    return SIB_EXTENSION;

  case ActionType::MergeModules:
  case ActionType::EmitModuleOnly:
    return SERIALIZED_MODULE_EXTENSION;

  case ActionType::Immediate:
  case ActionType::REPL:
    // These modes have no frontend-generated output.
    return StringRef();

  case ActionType::EmitAssembly:
    return "s";

  case ActionType::EmitIR:
    return "ll";

  case ActionType::EmitBC:
    return "bc";

  case ActionType::EmitObject:
    return "o";

  case ActionType::EmitImportedModules:
    return "importedmodules";
  }
}

bool FrontendOptions::canActionEmitDependencies(ActionType action) {
  switch (action) {
  case ActionType::NoneAction:
  case ActionType::DumpParse:
  case ActionType::DumpInterfaceHash:
  case ActionType::DumpAST:
  case ActionType::EmitSyntax:
  case ActionType::PrintAST:
  case ActionType::DumpScopeMaps:
  case ActionType::DumpTypeRefinementContexts:
  case ActionType::Immediate:
  case ActionType::REPL:
    return false;
  case ActionType::Parse:
  case ActionType::Typecheck:
  case ActionType::MergeModules:
  case ActionType::EmitModuleOnly:
  case ActionType::EmitPCH:
  case ActionType::EmitSILGen:
  case ActionType::EmitSIL:
  case ActionType::EmitSIBGen:
  case ActionType::EmitSIB:
  case ActionType::EmitIR:
  case ActionType::EmitBC:
  case ActionType::EmitAssembly:
  case ActionType::EmitObject:
  case ActionType::EmitImportedModules:
    return true;
  }
}

bool FrontendOptions::canActionEmitObjCHeader(ActionType action) {
  switch (action) {
  case ActionType::NoneAction:
  case ActionType::DumpParse:
  case ActionType::DumpInterfaceHash:
  case ActionType::DumpAST:
  case ActionType::EmitSyntax:
  case ActionType::PrintAST:
  case ActionType::EmitPCH:
  case ActionType::DumpScopeMaps:
  case ActionType::DumpTypeRefinementContexts:
  case ActionType::Immediate:
  case ActionType::REPL:
    return false;
  case ActionType::Parse:
  case ActionType::Typecheck:
  case ActionType::MergeModules:
  case ActionType::EmitModuleOnly:
  case ActionType::EmitSILGen:
  case ActionType::EmitSIL:
  case ActionType::EmitSIBGen:
  case ActionType::EmitSIB:
  case ActionType::EmitIR:
  case ActionType::EmitBC:
  case ActionType::EmitAssembly:
  case ActionType::EmitObject:
  case ActionType::EmitImportedModules:
    return true;
  }
}

bool FrontendOptions::canActionEmitLoadedModuleTrace(ActionType action) {
  switch (action) {
  case ActionType::NoneAction:
  case ActionType::Parse:
  case ActionType::DumpParse:
  case ActionType::DumpInterfaceHash:
  case ActionType::DumpAST:
  case ActionType::EmitSyntax:
  case ActionType::PrintAST:
  case ActionType::DumpScopeMaps:
  case ActionType::DumpTypeRefinementContexts:
  case ActionType::Immediate:
  case ActionType::REPL:
    return false;
  case ActionType::Typecheck:
  case ActionType::MergeModules:
  case ActionType::EmitModuleOnly:
  case ActionType::EmitPCH:
  case ActionType::EmitSILGen:
  case ActionType::EmitSIL:
  case ActionType::EmitSIBGen:
  case ActionType::EmitSIB:
  case ActionType::EmitIR:
  case ActionType::EmitBC:
  case ActionType::EmitAssembly:
  case ActionType::EmitObject:
  case ActionType::EmitImportedModules:
    return true;
  }
}

bool FrontendOptions::canActionEmitModule(ActionType action) {
  switch (action) {
  case ActionType::NoneAction:
  case ActionType::Parse:
  case ActionType::Typecheck:
  case ActionType::DumpParse:
  case ActionType::DumpInterfaceHash:
  case ActionType::DumpAST:
  case ActionType::EmitSyntax:
  case ActionType::PrintAST:
  case ActionType::EmitPCH:
  case ActionType::DumpScopeMaps:
  case ActionType::DumpTypeRefinementContexts:
  case ActionType::EmitSILGen:
  case ActionType::Immediate:
  case ActionType::REPL:
    return false;
  case ActionType::MergeModules:
  case ActionType::EmitModuleOnly:
  case ActionType::EmitSIL:
  case ActionType::EmitSIBGen:
  case ActionType::EmitSIB:
  case ActionType::EmitIR:
  case ActionType::EmitBC:
  case ActionType::EmitAssembly:
  case ActionType::EmitObject:
  case ActionType::EmitImportedModules:
    return true;
  }
}

bool FrontendOptions::canActionEmitModuleDoc(ActionType action) {
  return canActionEmitModule(action);
}

bool FrontendOptions::doesActionProduceOutput(ActionType action) {
  switch (action) {
  case ActionType::Parse:
  case ActionType::Typecheck:
  case ActionType::DumpParse:
  case ActionType::DumpAST:
  case ActionType::EmitSyntax:
  case ActionType::DumpInterfaceHash:
  case ActionType::PrintAST:
  case ActionType::DumpScopeMaps:
  case ActionType::DumpTypeRefinementContexts:
  case ActionType::EmitPCH:
  case ActionType::EmitSILGen:
  case ActionType::EmitSIL:
  case ActionType::EmitSIBGen:
  case ActionType::EmitSIB:
  case ActionType::EmitModuleOnly:
  case ActionType::EmitAssembly:
  case ActionType::EmitIR:
  case ActionType::EmitBC:
  case ActionType::EmitObject:
  case ActionType::EmitImportedModules:
  case ActionType::MergeModules:
    return true;

  case ActionType::NoneAction:
  case ActionType::Immediate:
  case ActionType::REPL:
    return false;
  }
  llvm_unreachable("Unknown ActionType");
}

bool FrontendOptions::doesActionProduceTextualOutput(ActionType action) {
  switch (action) {
  case ActionType::NoneAction:
  case ActionType::EmitPCH:
  case ActionType::EmitSIBGen:
  case ActionType::EmitSIB:
  case ActionType::MergeModules:
  case ActionType::EmitModuleOnly:
  case ActionType::EmitBC:
  case ActionType::EmitObject:
  case ActionType::Immediate:
  case ActionType::REPL:
    return false;

  case ActionType::Parse:
  case ActionType::Typecheck:
  case ActionType::DumpParse:
  case ActionType::DumpInterfaceHash:
  case ActionType::DumpAST:
  case ActionType::EmitSyntax:
  case ActionType::PrintAST:
  case ActionType::DumpScopeMaps:
  case ActionType::DumpTypeRefinementContexts:
  case ActionType::EmitImportedModules:
  case ActionType::EmitSILGen:
  case ActionType::EmitSIL:
  case ActionType::EmitAssembly:
  case ActionType::EmitIR:
    return true;
  }
}

const PrimarySpecificPaths &
FrontendOptions::getPrimarySpecificPathsForAtMostOnePrimary() const {
  return InputsAndOutputs.getPrimarySpecificPathsForAtMostOnePrimary();
}

const PrimarySpecificPaths &
FrontendOptions::getPrimarySpecificPathsForPrimary(StringRef filename) const {
  return InputsAndOutputs.getPrimarySpecificPathsForPrimary(filename);
}
