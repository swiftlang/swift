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
#include "swift/AST/Module.h"
#include "swift/SymbolGraphGen/SymbolGraphGen.h"

#include "SymbolGraphASTWalker.h"

using namespace swift;
using namespace symbolgraphgen;

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
    << "Found " << Walker.Graph.Nodes.size() << " symbols and "
    << Walker.Graph.Edges.size() << " relationships.\n";

  std::error_code Error;
  llvm::raw_fd_ostream OS(Options.OutputPath, Error, llvm::sys::fs::FA_Write);
  if (Error) {
    llvm::errs() << "Couldn't open output file for writing: "
        << Error.message() << "\n";
    return EXIT_FAILURE;
  }

  llvm::json::OStream J(OS, Options.PrettyPrint ? 2 : 0);
  Walker.Graph.serialize(Walker, J);

  return EXIT_SUCCESS;
}
