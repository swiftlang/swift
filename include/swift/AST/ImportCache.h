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
    private llvm::TrailingObjects<ImportSet, ImportedModule> {
  friend TrailingObjects;
  friend class ImportCache;

  unsigned HasHeaderImportModule : 1;
  unsigned NumTopLevelImports : 31;
  unsigned NumTransitiveImports;
  unsigned NumTransitiveSwiftOnlyImports;

  ImportSet(bool hasHeaderImportModule,
            ArrayRef<ImportedModule> topLevelImports,
            ArrayRef<ImportedModule> transitiveImports,
            ArrayRef<ImportedModule> transitiveSwiftOnlyImports);

  ImportSet(const ImportSet &) = delete;
  void operator=(const ImportSet &) = delete;

public:
  void Profile(llvm::FoldingSetNodeID &ID) {
    Profile(ID, getTopLevelImports());
  }
  static void Profile(
      llvm::FoldingSetNodeID &ID,
      ArrayRef<ImportedModule> topLevelImports);

  size_t numTrailingObjects(OverloadToken<ImportedModule>) const {
    return NumTopLevelImports + NumTransitiveImports +
           NumTransitiveSwiftOnlyImports;
  }

  /// This is a bit of a hack to make module name lookup work properly.
  /// If our import set includes the ClangImporter's special header import
  /// module, we have to check it first, before any other imported module.
  bool hasHeaderImportModule() const {
    return HasHeaderImportModule;
  }

  ArrayRef<ImportedModule> getTopLevelImports() const {
    return getTrailingObjects(NumTopLevelImports);
  }

  ArrayRef<ImportedModule> getTransitiveImports() const {
    return {getTrailingObjects() + NumTopLevelImports, NumTransitiveImports};
  }

  ArrayRef<ImportedModule> getTransitiveSwiftOnlyImports() const {
    return {getTrailingObjects() + NumTopLevelImports + NumTransitiveImports,
            NumTransitiveSwiftOnlyImports};
  }

  ArrayRef<ImportedModule> getAllImports() const {
    return getTrailingObjects(NumTopLevelImports + NumTransitiveImports);
  }

  SWIFT_DEBUG_DUMP;
};

class alignas(ImportedModule) ImportCache {
  ImportCache(const ImportCache &) = delete;
  void operator=(const ImportCache &) = delete;

  llvm::FoldingSet<ImportSet> ImportSets;
  llvm::DenseMap<const DeclContext *, ImportSet *> ImportSetForDC;
  llvm::DenseMap<std::tuple<const ModuleDecl *,
                            const DeclContext *>,
                 ArrayRef<ImportPath::Access>> VisibilityCache;
  llvm::DenseMap<std::tuple<const ModuleDecl *,
                            const ModuleDecl *,
                            const DeclContext *>,
                 ArrayRef<ImportPath::Access>> ShadowCache;
  llvm::DenseMap<std::tuple<const ModuleDecl *,
                            const DeclContext *>,
                 bool> SwiftOnlyCache;
  llvm::DenseMap<const ModuleDecl *, ArrayRef<ModuleDecl *>> WeakCache;

  ImportPath::Access EmptyAccessPath;

  ArrayRef<ImportPath::Access> allocateArray(
      ASTContext &ctx,
      SmallVectorImpl<ImportPath::Access> &results);

  ArrayRef<ModuleDecl *> allocateArray(
      ASTContext &ctx,
      llvm::SetVector<ModuleDecl *> &results);

  ImportSet &getImportSet(ASTContext &ctx,
                          ArrayRef<ImportedModule> topLevelImports);

public:
  ImportCache() {}

  /// Returns an object descripting all modules transitively imported
  /// from 'dc'.
  ImportSet &getImportSet(const DeclContext *dc);

  /// Returns all access paths into 'mod' that are visible from 'dc',
  /// including transitively, via re-exports.
  ArrayRef<ImportPath::Access>
  getAllVisibleAccessPaths(const ModuleDecl *mod, const DeclContext *dc);

  bool isImportedBy(const ModuleDecl *mod,
                    const DeclContext *dc) {
    return !getAllVisibleAccessPaths(mod, dc).empty();
  }

  /// Is `mod` imported from `dc` via a purely Swift access path?
  /// Always returns false if `dc` is a non-Swift module and only takes
  /// into account re-exports declared from Swift modules for transitive imports.
  bool isImportedByViaSwiftOnly(const ModuleDecl *mod,
                                const DeclContext *dc);

  /// Returns all access paths in 'mod' that are visible from 'dc' if we
  /// subtract imports of 'other'.
  ArrayRef<ImportPath::Access>
  getAllAccessPathsNotShadowedBy(const ModuleDecl *mod,
                                 const ModuleDecl *other,
                                 const DeclContext *dc);

  /// Returns all weak-linked imported modules.
  ArrayRef<ModuleDecl *>
  getWeakImports(const ModuleDecl *mod);

  bool isWeakImportedBy(const ModuleDecl *mod,
                        const ModuleDecl *from);

  /// This is a hack to cope with main file parsing and REPL parsing, where
  /// we can add ImportDecls after import resolution.
  void clear() {
    ImportSetForDC.clear();
  }
};

ArrayRef<ImportedModule> getAllImports(const DeclContext *dc);

}  // namespace namelookup

}  // namespace swift

#endif
