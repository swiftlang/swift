//===--- SymbolGraphGen.cpp - Symbol Graph Generator Entry Point ----------===//
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

#include "swift/SymbolGraphGen/SymbolGraphGen.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/FileSystem.h"
#include "swift/AST/Import.h"
#include "swift/AST/Module.h"
#include "swift/AST/NameLookup.h"
#include "swift/Sema/IDETypeChecking.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/Support/JSON.h"
#include "llvm/Support/Path.h"

#include "SymbolGraphASTWalker.h"

using namespace swift;
using namespace symbolgraphgen;

namespace {
int serializeSymbolGraph(SymbolGraph &SG,
                         const SymbolGraphOptions &Options) {
  SmallString<256> FileName;
  FileName.append(SG.M.getNameStr());
  if (SG.ExtendedModule.has_value()) {
    FileName.push_back('@');
    FileName.append(SG.ExtendedModule.value()->getNameStr());
  } else if (SG.DeclaringModule.has_value()) {
    // Treat cross-import overlay modules as "extensions" of their declaring module
    FileName.push_back('@');
    FileName.append(SG.DeclaringModule.value()->getNameStr());
  }
  FileName.append(".symbols.json");

  SmallString<1024> OutputPath(Options.OutputDir);
  llvm::sys::path::append(OutputPath, FileName);

  return withOutputPath(
      SG.M.getASTContext().Diags, SG.M.getASTContext().getOutputBackend(),
      OutputPath, [&](raw_ostream &OS) {
        llvm::json::OStream J(OS, Options.PrettyPrint ? 2 : 0);
        SG.serialize(J);
        return false;
      });
}

} // end anonymous namespace

// MARK: - Main Entry Point

/// Emit a symbol graph JSON file for a `ModuleDecl`.
int symbolgraphgen::emitSymbolGraphForModule(
    ModuleDecl *M, const SymbolGraphOptions &Options) {
  ModuleDecl::ImportCollector importCollector(Options.MinimumAccessLevel);

  auto importFilter = [&Options](const ModuleDecl *module) {
      if (!module)
        return false;

      for (const auto &allowedModuleName : *Options.AllowedReexportedModules)
        if (allowedModuleName == module->getNameStr())
          return true;

       return false;
    };

  if (Options.AllowedReexportedModules.has_value())
    importCollector.importFilter = std::move(importFilter);

  SmallVector<Decl *, 64> ModuleDecls;
  swift::getTopLevelDeclsForDisplay(
      M, ModuleDecls, [&importCollector](ModuleDecl *M, SmallVectorImpl<Decl *> &results) {
        M->getDisplayDeclsRecursivelyAndImports(results, importCollector);
      });

  if (Options.PrintMessages)
    llvm::errs() << ModuleDecls.size()
        << " top-level declarations in this module.\n";

  SymbolGraphASTWalker Walker(*M, importCollector.imports,
                              importCollector.qualifiedImports, Options);

  for (auto *Decl : ModuleDecls) {
    Walker.walk(Decl);
  }

  if (Options.PrintMessages)
    llvm::errs()
      << "Found " << Walker.MainGraph.Nodes.size() << " symbols and "
      << Walker.MainGraph.Edges.size() << " relationships.\n";

  int Success = EXIT_SUCCESS;

  Success |= serializeSymbolGraph(Walker.MainGraph, Options);

  for (const auto &Entry : Walker.ExtendedModuleGraphs) {
    if (Entry.getValue()->empty()) {
      continue;
    }
    Success |= serializeSymbolGraph(*Entry.getValue(), Options);
  }

  return Success;
}

int symbolgraphgen::
printSymbolGraphForDecl(const ValueDecl *D, Type BaseTy,
                        bool InSynthesizedExtension,
                        const SymbolGraphOptions &Options,
                        llvm::raw_ostream &OS,
                        SmallVectorImpl<PathComponent> &ParentContexts,
                        SmallVectorImpl<FragmentInfo> &FragmentInfo) {
  if (!Symbol::supportsKind(D->getKind()))
    return EXIT_FAILURE;

  llvm::json::OStream JOS(OS, Options.PrettyPrint ? 2 : 0);
  ModuleDecl *MD = D->getModuleContext();
  SymbolGraphASTWalker Walker(*MD, Options);
  markup::MarkupContext MarkupCtx;
  SymbolGraph Graph(Walker, *MD, std::nullopt, MarkupCtx, std::nullopt,
                    /*IsForSingleNode=*/true);
  NominalTypeDecl *NTD = InSynthesizedExtension
      ? BaseTy->getAnyNominal()
      : nullptr;

  Symbol MySym(&Graph, D, NTD, BaseTy);
  MySym.getPathComponents(ParentContexts);
  MySym.getFragmentInfo(FragmentInfo);
  Graph.recordNode(MySym);
  Graph.serialize(JOS);
  return EXIT_SUCCESS;
}
