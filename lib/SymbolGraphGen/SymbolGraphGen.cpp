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

#include "llvm/Support/JSON.h"
#include "llvm/Support/Path.h"
#include "swift/SymbolGraphGen/SymbolGraphGen.h"

#include "SymbolGraphASTWalker.h"

using namespace swift;
using namespace symbolgraphgen;

namespace {
int serializeSymbolGraph(SymbolGraph &SG,
                         const SymbolGraphOptions &Options) {
  SmallString<256> FileName;
  if (SG.DeclaringModule.hasValue()) {
    // Save a cross-import overlay symbol graph as `MainModule@BystandingModule[@BystandingModule...]@OverlayModule.symbols.json`
    //
    // The overlay module's name is added as a disambiguator in case an overlay
    // declares multiple modules for the same set of imports.
    FileName.append(SG.DeclaringModule.getValue()->getNameStr());
    for (auto BystanderModule : SG.BystanderModules) {
      FileName.push_back('@');
      FileName.append(BystanderModule.str());
    }
    
    FileName.push_back('@');
    FileName.append(SG.M.getNameStr());
  } else {
    FileName.append(SG.M.getNameStr());
    
    if (SG.ExtendedModule.hasValue()) {
      FileName.push_back('@');
      FileName.append(SG.ExtendedModule.getValue()->getNameStr());
    }
  }
  FileName.append(".symbols.json");

  SmallString<1024> OutputPath(Options.OutputDir);
  llvm::sys::path::append(OutputPath, FileName);

  std::error_code Error;
  llvm::raw_fd_ostream OS(OutputPath, Error, llvm::sys::fs::FA_Write);
  if (Error) {
    llvm::errs() << "Couldn't open output file '" << OutputPath
        << " for writing: "
        << Error.message() << "\n";
    return EXIT_FAILURE;
  }

  llvm::json::OStream J(OS, Options.PrettyPrint ? 2 : 0);
  SG.serialize(J);
  return EXIT_SUCCESS;
}

} // end anonymous namespace

// MARK: - Main Entry Point

/// Emit a symbol graph JSON file for a `ModuleDecl`.
int
symbolgraphgen::emitSymbolGraphForModule(ModuleDecl *M,
                                         const SymbolGraphOptions &Options) {
  SymbolGraphASTWalker Walker(*M, Options);
  SmallVector<Decl *, 64> ModuleDecls;
  M->getDisplayDecls(ModuleDecls);

  llvm::errs() << ModuleDecls.size()
      << " top-level declarations in this module.\n";

  for (auto *Decl : ModuleDecls) {
    Walker.walk(Decl);
  }

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
                        llvm::raw_ostream &OS) {
  if (!Symbol::supportsKind(D->getKind()))
    return EXIT_FAILURE;

  llvm::json::OStream JOS(OS, Options.PrettyPrint ? 2 : 0);
  ModuleDecl *MD = D->getModuleContext();
  SymbolGraphASTWalker Walker(*MD, Options);
  markup::MarkupContext MarkupCtx;
  SymbolGraph Graph(Walker, *MD, None, MarkupCtx, None,
                    /*IsForSingleNode=*/true);
  NominalTypeDecl *NTD = InSynthesizedExtension
      ? BaseTy->getAnyNominal()
      : nullptr;

  Symbol MySym(&Graph, D, NTD, BaseTy);
  Graph.recordNode(MySym);
  Graph.serialize(JOS);
  return EXIT_SUCCESS;
}
