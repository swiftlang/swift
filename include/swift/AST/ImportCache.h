//===--- ImportCache.h - Caching the import graph ---------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines interfaces for querying the module import graph in an
// efficient manner.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_IMPORT_CACHE_H
#define SWIFT_AST_IMPORT_CACHE_H

#include "swift/AST/Module.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/FoldingSet.h"

namespace swift {
class DeclContext;

namespace namelookup {

/// An object describing a set of modules visible from a DeclContext.
///
/// This consists of two arrays of modules; the top-level imports and the
/// transitive imports.
///
/// The top-level imports contains all public imports of the parent module
/// of 'dc', together with any private imports in the source file containing
/// 'dc', if there is one.
///
/// The transitive imports contains all public imports reachable from the
/// set of top-level imports.
///
/// Both sets only contain unique elements. The top-level imports always
/// include the parent module of 'dc' explicitly.
///
/// The set of transitive imports does not contain any elements found in
/// the top-level imports.
///
/// The Swift standard library module is not included in either set unless
/// it was explicitly imported (or re-exported).
class ImportSet final :
    public llvm::FoldingSetNode,
    private llvm::TrailingObjects<ImportSet, ModuleDecl::ImportedModule> {
  friend TrailingObjects;
  friend class ImportCache;

  unsigned HasHeaderImportModule : 1;
  unsigned NumTopLevelImports : 31;
  unsigned NumTransitiveImports;

  ImportSet(bool hasHeaderImportModule,
            ArrayRef<ModuleDecl::ImportedModule> topLevelImports,
            ArrayRef<ModuleDecl::ImportedModule> transitiveImports);

  ImportSet(const ImportSet &) = delete;
  void operator=(const ImportSet &) = delete;

public:
  void Profile(llvm::FoldingSetNodeID &ID) {
    Profile(ID, getTopLevelImports());
  }
  static void Profile(
      llvm::FoldingSetNodeID &ID,
      ArrayRef<ModuleDecl::ImportedModule> topLevelImports);

  size_t numTrailingObjects(OverloadToken<ModuleDecl::ImportedModule>) const {
    return NumTopLevelImports + NumTransitiveImports;
  }

  /// This is a bit of a hack to make module name lookup work properly.
  /// If our import set includes the ClangImporter's special header import
  /// module, we have to check it first, before any other imported module.
  bool hasHeaderImportModule() const {
    return HasHeaderImportModule;
  }

  ArrayRef<ModuleDecl::ImportedModule> getTopLevelImports() const {
    return {getTrailingObjects<ModuleDecl::ImportedModule>(),
            NumTopLevelImports};
  }

  ArrayRef<ModuleDecl::ImportedModule> getTransitiveImports() const {
    return {getTrailingObjects<ModuleDecl::ImportedModule>() +
              NumTopLevelImports,
            NumTransitiveImports};
  }

  ArrayRef<ModuleDecl::ImportedModule> getAllImports() const {
      return {getTrailingObjects<ModuleDecl::ImportedModule>(),
              NumTopLevelImports + NumTransitiveImports};
  }
};

class alignas(ModuleDecl::ImportedModule) ImportCache {
  ImportCache(const ImportCache &) = delete;
  void operator=(const ImportCache &) = delete;

  llvm::FoldingSet<ImportSet> ImportSets;
  llvm::DenseMap<const DeclContext *, ImportSet *> ImportSetForDC;
  llvm::DenseMap<std::tuple<const ModuleDecl *,
                            const DeclContext *>,
                 ArrayRef<ModuleDecl::AccessPathTy>> VisibilityCache;
  llvm::DenseMap<std::tuple<const ModuleDecl *,
                            const ModuleDecl *,
                            const DeclContext *>,
                 ArrayRef<ModuleDecl::AccessPathTy>> ShadowCache;

  ModuleDecl::AccessPathTy EmptyAccessPath;

  ArrayRef<ModuleDecl::AccessPathTy> allocateArray(
      ASTContext &ctx,
      SmallVectorImpl<ModuleDecl::AccessPathTy> &results);

  ImportSet &getImportSet(ASTContext &ctx,
                          ArrayRef<ModuleDecl::ImportedModule> topLevelImports);

public:
  ImportCache() {}

  /// Returns an object descripting all modules transtiively imported
  /// from 'dc'.
  ImportSet &getImportSet(const DeclContext *dc);

  /// Returns all access paths into 'mod' that are visible from 'dc',
  /// including transitively, via re-exports.
  ArrayRef<ModuleDecl::AccessPathTy>
  getAllVisibleAccessPaths(const ModuleDecl *mod, const DeclContext *dc);

  bool isImportedBy(const ModuleDecl *mod,
                    const DeclContext *dc) {
    return !getAllVisibleAccessPaths(mod, dc).empty();
  }

  /// Determines if 'mod' is visible from 'dc' as a result of a scoped import.
  /// Note that if 'mod' was not imported from 'dc' at all, this also returns
  /// false.
  bool isScopedImport(const ModuleDecl *mod, DeclBaseName name,
                      const DeclContext *dc) {
    auto accessPaths = getAllVisibleAccessPaths(mod, dc);
    for (auto accessPath : accessPaths) {
      if (accessPath.empty())
        continue;
      if (ModuleDecl::matchesAccessPath(accessPath, name))
        return true;
    }

    return false;
  }

  /// Returns all access paths in 'mod' that are visible from 'dc' if we
  /// subtract imports of 'other'.
  ArrayRef<ModuleDecl::AccessPathTy>
  getAllAccessPathsNotShadowedBy(const ModuleDecl *mod,
                                 const ModuleDecl *other,
                                 const DeclContext *dc);

  /// Returns 'true' if a declaration named 'name' defined in 'other' shadows
  /// defined in 'mod', because no access paths can find 'name' in 'mod' from
  /// 'dc' if we ignore imports of 'other'.
  bool isShadowedBy(const ModuleDecl *mod,
                    const ModuleDecl *other,
                    DeclBaseName name,
                    const DeclContext *dc) {
     auto accessPaths = getAllAccessPathsNotShadowedBy(mod, other, dc);
     return llvm::none_of(accessPaths,
                          [&](ModuleDecl::AccessPathTy accessPath) {
                            return ModuleDecl::matchesAccessPath(accessPath, name);
                          });
  }

  /// Qualified lookup into types uses a slightly different check that does not
  /// take access paths into account.
  bool isShadowedBy(const ModuleDecl *mod,
                    const ModuleDecl *other,
                    const DeclContext *dc) {
    auto accessPaths = getAllAccessPathsNotShadowedBy(mod, other, dc);
    return accessPaths.empty();
  }

  /// This is a hack to cope with main file parsing and REPL parsing, where
  /// we can add ImportDecls after name binding.
  void clear() {
    ImportSetForDC.clear();
  }
};

ArrayRef<ModuleDecl::ImportedModule> getAllImports(const DeclContext *dc);

}  // namespace namelookup

}  // namespace swift

#endif
