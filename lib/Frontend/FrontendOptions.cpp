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

void FrontendOptions::forAllOutputPathsForMakeDependencies(
    std::function<void(const std::string &)> fn) const {
  InputsAndOutputs.forEachInputProducingOutput([&](const InputFile &input,
                                                   unsigned) -> bool {
    if (RequestedAction != FrontendOptions::ActionType::EmitModuleOnly &&
        RequestedAction != FrontendOptions::ActionType::MergeModules &&
        !input.outputs().OutputFilename.empty()) {
      fn(input.outputs().OutputFilename);
    }
    const std::string *outputs[] = {
      &input.outputs().ModuleOutputPath,
      &input.outputs().ModuleDocOutputPath,
      &input.outputs().ObjCHeaderOutputPath
    };
    for (const std::string *next : outputs) {
      if (!next->empty())
        fn(*next);
    }
    return false;
  });
}


const char *
FrontendOptions::suffixForPrincipalOutputFileForAction(ActionType action) {
  switch (action) {
  case ActionType::NoneAction:
    return nullptr;

  case ActionType::Parse:
  case ActionType::Typecheck:
  case ActionType::DumpParse:
  case ActionType::DumpInterfaceHash:
  case ActionType::DumpAST:
  case ActionType::EmitSyntax:
  case ActionType::PrintAST:
  case ActionType::DumpScopeMaps:
  case ActionType::DumpTypeRefinementContexts:
    return nullptr;

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
    return nullptr;

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

bool FrontendOptions::hasUnusedDependenciesFilePath(
    const InputFile &input) const {
  return !input.outputs().DependenciesFilePath.empty() &&
         !canActionEmitDependencies(RequestedAction);
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

bool FrontendOptions::hasUnusedObjCHeaderOutputPath(
    const InputFile &input) const {
  return !input.outputs().ObjCHeaderOutputPath.empty() &&
         !canActionEmitHeader(RequestedAction);
}

bool FrontendOptions::canActionEmitHeader(ActionType action) {
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

bool FrontendOptions::hasUnusedLoadedModuleTracePath(
    const InputFile &input) const {
  return !input.outputs().LoadedModuleTracePath.empty() &&
         !canActionEmitLoadedModuleTrace(RequestedAction);
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

bool FrontendOptions::hasUnusedModuleOutputPath(const InputFile &input) const {
  return !input.outputs().ModuleOutputPath.empty() &&
         !canActionEmitModule(RequestedAction);
}

bool FrontendOptions::hasUnusedModuleDocOutputPath(
    const InputFile &input) const {
  return !input.outputs().ModuleDocOutputPath.empty() &&
         !canActionEmitModule(RequestedAction);
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
