//===--- DWARFImporter.h - Import Clang Modules -----------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// \file This file implements support for loading Clang modules that were
//       reconstructed from DWARF into Swift.
//
//===----------------------------------------------------------------------===//
#ifndef SWIFT_DWARF_IMPORTER_H
#define SWIFT_DWARF_IMPORTER_H

#include "swift/AST/ClangModuleLoader.h"

namespace llvm {
}

namespace clang {
}

namespace swift {

/// Class that imports Clang modules into Swift, mapping directly
/// from Clang ASTs over to Swift ASTs.
class DWARFImporter final : public ClangModuleLoader {
  friend class ClangModuleUnit;

public:
  class Implementation;

private:
  Implementation &Impl;

  DWARFImporter(ASTContext &ctx, const ClangImporterOptions &clangImporterOpts,
                DependencyTracker *tracker);

public:
  /// Create a new DWARF importer that can import a Clang Modules from DWARF
  /// into the given ASTContext.
  ///
  /// \param ctx The ASTContext into which the module will be imported.
  /// The ASTContext's SearchPathOptions will be used for the DWARF importer.
  ///
  /// \param importerOpts The options to use for the DWARF importer.
  ///
  /// \param tracker The object tracking files this compilation depends on.
  ///
  /// \returns a new DWARF module importer, or null (with a diagnostic) if
  /// an error occurred.
  static std::unique_ptr<DWARFImporter>
  create(ASTContext &ctx,
         const ClangImporterOptions &importerOpts,
         DependencyTracker *tracker = nullptr);

  DWARFImporter(const DWARFImporter &) = delete;
  DWARFImporter(DWARFImporter &&) = delete;
  DWARFImporter &operator=(const DWARFImporter &) = delete;
  DWARFImporter &operator=(DWARFImporter &&) = delete;

  ~DWARFImporter();

  /// Check whether the module with a given name can be imported without
  /// importing it.
  ///
  /// Note that even if this check succeeds, errors may still occur if the
  /// module is loaded in full.
  bool canImportModule(std::pair<Identifier, SourceLoc> named) override;
  bool addSearchPath(StringRef newSearchPath, bool isFramework,
                     bool isSystem) override;
  ModuleDecl *
  loadModule(SourceLoc importLoc,
             ArrayRef<std::pair<Identifier, SourceLoc>> path) override;
  bool
  isInOverlayModuleForImportedModule(const DeclContext *overlayDC,
                                     const DeclContext *importedDC) override;
  void loadExtensions(NominalTypeDecl *nominal,
                      unsigned previousGeneration) override;
  void loadObjCMethods(
      ClassDecl *classDecl, ObjCSelector selector, bool isInstanceMethod,
      unsigned previousGeneration,
      llvm::TinyPtrVector<AbstractFunctionDecl *> &methods) override;
  ModuleDecl *getImportedHeaderModule() const override;
  void verifyAllModules() override;
  clang::ASTContext &getClangASTContext() const override;
  clang::Preprocessor &getClangPreprocessor() const override;
  clang::Sema &getClangSema() const override;
  const clang::CompilerInstance &getClangInstance() const override;

  void printStatistics() const override;
};

} // end namespace swift

#endif
