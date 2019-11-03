//===--- ModuleNameLookup.cpp - Name lookup within a module ---------------===//
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

#include "swift/AST/ModuleNameLookup.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/ClangModuleLoader.h"
#include "swift/AST/ImportCache.h"
#include "swift/AST/NameLookup.h"
#include "llvm/Support/raw_ostream.h"

using namespace swift;
using namespace namelookup;

namespace {

/// Encapsulates the work done for a recursive qualified lookup into a module.
///
/// The \p LookupStrategy handles the non-recursive part of the lookup. It
/// must be a subclass of ModuleNameLookup.
template <typename LookupStrategy>
class ModuleNameLookup {
  ASTContext &ctx;
  const ResolutionKind resolutionKind;
  const bool respectAccessControl;

  LookupStrategy *getDerived() {
    static_assert(std::is_base_of<ModuleNameLookup<LookupStrategy>,
                                  LookupStrategy>::value,
                  "ModuleNameLookup is a CRTP class");
    return static_cast<LookupStrategy *>(this);
  }

public:
  ModuleNameLookup(ASTContext &ctx, ResolutionKind resolutionKind)
      : ctx(ctx),
        resolutionKind(resolutionKind),
        respectAccessControl(!ctx.isAccessControlDisabled()) {}

  /// Performs a qualified lookup into the given module and, if necessary, its
  /// reexports.
  ///
  /// The results are appended to \p decls.
  void lookupInModule(SmallVectorImpl<ValueDecl *> &decls,
                      const DeclContext *moduleOrFile,
                      ModuleDecl::AccessPathTy accessPath,
                      const DeclContext *moduleScopeContext);
};


/// Encapsulates the work done for a recursive qualified lookup into a module
/// by full name.
class LookupByName : public ModuleNameLookup<LookupByName> {
  using Super = ModuleNameLookup<LookupByName>;
  friend Super;

  const DeclName name;
  const NLKind lookupKind;

public:
  LookupByName(ASTContext &ctx, ResolutionKind resolutionKind,
               DeclName name, NLKind lookupKind)
    : Super(ctx, resolutionKind), name(name),
      lookupKind(lookupKind) {}

private:
  /// Returns whether it's okay to stop recursively searching imports, given 
  /// that we found something non-overloadable.
  static bool canReturnEarly() {
    return true;
  }

  void doLocalLookup(ModuleDecl *module, ModuleDecl::AccessPathTy path,
                     SmallVectorImpl<ValueDecl *> &localDecls) {
    // If this import is specific to some named decl ("import Swift.Int")
    // then filter out any lookups that don't match.
    if (!ModuleDecl::matchesAccessPath(path, name))
      return;
    module->lookupValue(name, lookupKind, localDecls);
  }
};

/// Encapsulates the work done for a recursive qualified lookup into a module
/// to find all visible decls.
class LookupVisibleDecls : public ModuleNameLookup<LookupVisibleDecls> {
  using Super = ModuleNameLookup<LookupVisibleDecls>;
  friend Super;

  const NLKind lookupKind;

public:
  LookupVisibleDecls(ASTContext &ctx, ResolutionKind resolutionKind,
                     NLKind lookupKind)
    : ModuleNameLookup(ctx, resolutionKind),
      lookupKind(lookupKind) {}

private:
  /// Returns whether it's okay to stop recursively searching imports, given
  /// that we found something non-overloadable.
  static bool canReturnEarly() {
    return false;
  }

  void doLocalLookup(ModuleDecl *module, ModuleDecl::AccessPathTy path,
                     SmallVectorImpl<ValueDecl *> &localDecls) {
    VectorDeclConsumer consumer(localDecls);
    module->lookupVisibleDecls(path, consumer, lookupKind);
  }
};

} // end anonymous namespace

template <typename LookupStrategy>
void ModuleNameLookup<LookupStrategy>::lookupInModule(
    SmallVectorImpl<ValueDecl *> &decls,
    const DeclContext *moduleOrFile,
    ModuleDecl::AccessPathTy accessPath,
    const DeclContext *moduleScopeContext) {
  assert(moduleOrFile->isModuleScopeContext());

  const size_t initialCount = decls.size();
  size_t currentCount = decls.size();

  auto updateNewDecls = [&](const DeclContext *moduleScopeContext) {
    if (decls.size() == currentCount)
      return;

    auto newEnd = std::remove_if(
      decls.begin() + currentCount, decls.end(),
      [&](ValueDecl *VD) {
        if (resolutionKind == ResolutionKind::TypesOnly && !isa<TypeDecl>(VD))
          return true;
        if (respectAccessControl && !VD->isAccessibleFrom(moduleScopeContext))
          return true;
        return false;
      });
    decls.erase(newEnd, decls.end());

    currentCount = decls.size();
  };

  // Do the lookup into the current module.
  auto *module = moduleOrFile->getParentModule();
  getDerived()->doLocalLookup(module, accessPath, decls);
  updateNewDecls(moduleScopeContext);

  bool canReturnEarly = (initialCount != decls.size() &&
                         getDerived()->canReturnEarly());
  if (canReturnEarly &&
      resolutionKind == ResolutionKind::Overloadable) {
    // If we only found top-level functions, keep looking, since we may
    // find additional overloads.
    if (std::all_of(decls.begin() + initialCount, decls.end(),
                    [](ValueDecl *VD) { return isa<FuncDecl>(VD); }))
      canReturnEarly = false;
  }

  // If needed, search for decls in re-exported modules as well.
  if (!canReturnEarly) {
    auto &imports = ctx.getImportCache().getImportSet(moduleOrFile);

    auto visitImport = [&](ModuleDecl::ImportedModule import,
                           const DeclContext *moduleScopeContext) {
      if (import.first.empty())
        import.first = accessPath;
      else if (!accessPath.empty() &&
               !ModuleDecl::isSameAccessPath(import.first, accessPath))
        return;

      getDerived()->doLocalLookup(import.second, import.first, decls);
      updateNewDecls(moduleScopeContext);
    };

    // If the ClangImporter's special header import module appears in the
    // import set, we must visit it first.
    ModuleDecl *headerImportModule = nullptr;
    if (imports.hasHeaderImportModule()) {
      if (auto *loader = ctx.getClangModuleLoader()) {
        headerImportModule = loader->getImportedHeaderModule();
        if (headerImportModule) {
          ModuleDecl::ImportedModule import({}, headerImportModule);
          visitImport(import, nullptr);
        }
      }
    }

    for (auto import : imports.getTopLevelImports()) {
      // A module appears in its own top-level import list; since we checked
      // it already, skip it.
      if (import.second == module)
        continue;

      // Skip the special import set module; we've already visited it.
      if (import.second == headerImportModule)
        continue;

      visitImport(import, moduleScopeContext);
    }

    for (auto import : imports.getTransitiveImports()) {
      // Skip the special import set module; we've already visited it.
      if (import.second == headerImportModule)
        continue;

      visitImport(import, nullptr);
    }
  }

  // Nothing more to do if we don't have ambiguous results.
  if (decls.size() - initialCount <= 1)
    return;

  // Remove duplicated declarations, which can happen when the same module is
  // imported with multiple access paths.
  llvm::SmallPtrSet<ValueDecl *, 4> knownDecls;
  decls.erase(std::remove_if(decls.begin() + initialCount, decls.end(),
                             [&](ValueDecl *d) -> bool {
                               return !knownDecls.insert(d).second;
                             }),
              decls.end());
}

void namelookup::lookupInModule(const DeclContext *moduleOrFile,
                                DeclName name,
                                SmallVectorImpl<ValueDecl *> &decls,
                                NLKind lookupKind,
                                ResolutionKind resolutionKind,
                                const DeclContext *moduleScopeContext) {
  assert(moduleScopeContext->isModuleScopeContext());

  auto &ctx = moduleOrFile->getASTContext();
  auto *stats = ctx.Stats;
  if (stats)
    stats->getFrontendCounters().NumLookupInModule++;

  FrontendStatsTracer tracer(stats, "lookup-in-module");

  LookupByName lookup(ctx, resolutionKind, name, lookupKind);
  lookup.lookupInModule(decls, moduleOrFile, {}, moduleScopeContext);
}

void namelookup::lookupVisibleDeclsInModule(
    const DeclContext *moduleOrFile,
    ModuleDecl::AccessPathTy accessPath,
    SmallVectorImpl<ValueDecl *> &decls,
    NLKind lookupKind,
    ResolutionKind resolutionKind,
    const DeclContext *moduleScopeContext) {
  assert(moduleScopeContext->isModuleScopeContext());
  auto &ctx = moduleOrFile->getASTContext();
  LookupVisibleDecls lookup(ctx, resolutionKind, lookupKind);
  lookup.lookupInModule(decls, moduleOrFile, accessPath, moduleScopeContext);
}

