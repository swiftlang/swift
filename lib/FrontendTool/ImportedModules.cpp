//===--- ImportedModules.cpp -- generates the list of imported modules ----===//
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

#include "ImportedModules.h"
#include "swift/AST/Decl.h"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/DiagnosticsFrontend.h"
#include "swift/AST/Module.h"
#include "swift/Frontend/FrontendOptions.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/Support/FileSystem.h"

bool swift::emitImportedModules(DiagnosticEngine &diags, ModuleDecl *module,
                                const FrontendOptions &opts) {

  auto path = opts.getSingleOutputFilename();
  std::error_code EC;
  llvm::raw_fd_ostream out(path, EC, llvm::sys::fs::F_None);

  if (out.has_error() || EC) {
    diags.diagnose(SourceLoc(), diag::error_opening_output, path, EC.message());
    out.clear_error();
    return true;
  }

  llvm::SmallVector<Decl *, 32> Decls;
  module->getDisplayDecls(Decls);

  llvm::SetVector<Identifier> Modules;
  for (auto *D : Decls) {
    auto ID = dyn_cast<ImportDecl>(D);
    if (!ID)
      continue;

    auto accessPath = ID->getModulePath();
    // only the top-level name is needed (i.e. A in A.B.C)
    Modules.insert(accessPath[0].first);
  }

  for (auto name : Modules) {
    out << name << "\n";
  }
  return false;
}
