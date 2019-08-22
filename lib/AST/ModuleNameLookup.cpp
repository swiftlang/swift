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
#include "swift/AST/LazyResolver.h"
#include "swift/AST/NameLookup.h"
#include "llvm/Support/raw_ostream.h"

using namespace swift;
using namespace namelookup;

namespace {

/// Encapsulates the work done for a recursive qualified lookup into a module.
///
/// The \p LookupStrategy handles the non-recursive part of the lookup, as well
/// as how to combine results from across modules (e.g. handling shadowing). It
/// must be a subclass of ModuleNameLookup.
template <typename LookupStrategy>
class ModuleNameLookup {
  /// The usable results of a lookup in a particular module may differ based on
  /// where the lookup is happening.
  using ModuleLookupCacheKey = std::pair<ModuleDecl::ImportedModule,
                                         const DeclContext * /*lookupScope*/>;
  using ModuleLookupCache = llvm::SmallDenseMap<ModuleLookupCacheKey,
                                                TinyPtrVector<ValueDecl *>, 32>;

  ModuleLookupCache cache;
  const ResolutionKind resolutionKind;
  const bool respectAccessControl;

  LookupStrategy *getDerived() {
    static_assert(std::is_base_of<ModuleNameLookup<LookupStrategy>,
                                  LookupStrategy>::value,
                  "ModuleNameLookup is a CRTP class");
    return static_cast<LookupStrategy *>(this);
  }

  /// After finding decls by name lookup, filter based on the given
  /// resolution kind and add them to \p results.
  ///
  /// \returns true if lookup is (locally) complete and does not need to recurse
  /// further.
  bool recordImportDecls(
      SmallVectorImpl<ValueDecl *> &results,
      ArrayRef<ValueDecl *> newDecls);

  /// Given a list of imports and an access path to limit by, perform a lookup
  /// into each of them and record the results in \p decls, filtering based on
  /// the given resolution kind.
  void collectLookupResultsFromImports(
      SmallVectorImpl<ValueDecl *> &decls,
      ArrayRef<ModuleDecl::ImportedModule> imports,
      ModuleDecl::AccessPathTy accessPath,
      const DeclContext *moduleScopeContext);

  /// Performs a qualified lookup into the given module and, if necessary, its
  /// reexports. The lookup into \p module itself will not check or update the
  /// lookup cache.
  ///
  /// The results are appended to \p decls.
  ///
  /// \returns The slice of \p decls that includes the newly-found
  /// declarations.
  ///
  /// \see lookupInModule
  ArrayRef<ValueDecl *> lookupInModuleUncached(
      SmallVectorImpl<ValueDecl *> &decls,
      ModuleDecl *module, ModuleDecl::AccessPathTy accessPath,
      const DeclContext *moduleScopeContext,
      ArrayRef<ModuleDecl::ImportedModule> extraImports);

public:
  ModuleNameLookup(ASTContext &ctx, ResolutionKind resolutionKind)
      : resolutionKind(resolutionKind),
        respectAccessControl(!ctx.isAccessControlDisabled()) {}

  /// Performs a qualified lookup into the given module and, if necessary, its
  /// reexports.
  ///
  /// The results are appended to \p decls.
  void lookupInModule(SmallVectorImpl<ValueDecl *> &decls,
                      ModuleDecl *module, ModuleDecl::AccessPathTy accessPath,
                      const DeclContext *moduleScopeContext,
                      ArrayRef<ModuleDecl::ImportedModule> extraImports = {});
};


/// Encapsulates the work done for a recursive qualified lookup into a module
/// by full name.
class LookupByName : public ModuleNameLookup<LookupByName> {
  using Super = ModuleNameLookup<LookupByName>;
  friend Super;
  friend class LookupVisibleDecls;

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
    module->lookupValue(path, name, lookupKind, localDecls);
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
bool ModuleNameLookup<LookupStrategy>::recordImportDecls(
    SmallVectorImpl<ValueDecl *> &results,
    ArrayRef<ValueDecl *> newDecls) {

  results.append(newDecls.begin(), newDecls.end());

  if (resolutionKind == ResolutionKind::Overloadable) {
    // If we only found top-level functions, keep looking, since we may
    // find additional overloads.
    if (llvm::all_of(newDecls,
                     [](ValueDecl *VD) { return isa<FuncDecl>(VD); }))
      return false;
  }

  return !newDecls.empty() && getDerived()->canReturnEarly();
}

template <typename LookupStrategy>
void ModuleNameLookup<LookupStrategy>::collectLookupResultsFromImports(
    SmallVectorImpl<ValueDecl *> &decls,
    ArrayRef<ModuleDecl::ImportedModule> reexports,
    ModuleDecl::AccessPathTy accessPath,
    const DeclContext *moduleScopeContext) {

  // Prefer scoped imports (those importing a specific name from a module, like
  // `import func Swift.max`) to whole-module imports.
  SmallVector<ValueDecl *, 8> unscopedValues;
  SmallVector<ValueDecl *, 8> scopedValues;
  for (auto next : reexports) {
    // Filter any whole-module imports, and skip specific-decl imports if the
    // import path doesn't match exactly.
    ModuleDecl::AccessPathTy combinedAccessPath;
    if (accessPath.empty()) {
      combinedAccessPath = next.first;
    } else if (!next.first.empty() &&
               !ModuleDecl::isSameAccessPath(next.first, accessPath)) {
      // If we ever allow importing non-top-level decls, it's possible the
      // rule above isn't what we want.
      assert(next.first.size() == 1 && "import of non-top-level decl");
      continue;
    } else {
      combinedAccessPath = accessPath;
    }

    auto &resultSet = next.first.empty() ? unscopedValues : scopedValues;
    lookupInModule(resultSet, next.second, combinedAccessPath,
                   moduleScopeContext);
  }

  // Add the results from scoped imports, then the results from unscoped
  // imports if needed.
  const bool canReturnEarly = recordImportDecls(decls, scopedValues);
  if (!canReturnEarly)
    (void)recordImportDecls(decls, unscopedValues);
}

template <typename LookupStrategy>
ArrayRef<ValueDecl *> ModuleNameLookup<LookupStrategy>::lookupInModuleUncached(
    SmallVectorImpl<ValueDecl *> &decls,
    ModuleDecl *module, ModuleDecl::AccessPathTy accessPath,
    const DeclContext *moduleScopeContext,
    ArrayRef<ModuleDecl::ImportedModule> extraImports) {

  // Do the lookup.
  SmallVector<ValueDecl *, 4> localDecls;
  getDerived()->doLocalLookup(module, accessPath, localDecls);

  llvm::erase_if(localDecls, [=](ValueDecl *VD) {
    if (resolutionKind == ResolutionKind::TypesOnly && !isa<TypeDecl>(VD))
      return true;
    if (respectAccessControl && !VD->isAccessibleFrom(moduleScopeContext))
      return true;
    return false;
  });

  // Record the decls by overload signature.
  const size_t initialCount = decls.size();
  const bool canReturnEarly = recordImportDecls(decls, localDecls);

  // If needed, search for decls in re-exported modules as well.
  if (!canReturnEarly) {
    SmallVector<ModuleDecl::ImportedModule, 8> reexports;
    module->getImportedModulesForLookup(reexports);
    assert(llvm::none_of(reexports,
                         [module](ModuleDecl::ImportedModule import) -> bool {
                           return import.second == nullptr || import.second == module;
                         }));
    reexports.append(extraImports.begin(), extraImports.end());

    // Special treatment based on the use site only applies to immediate
    // imports of the top-level module.
    // FIXME: It ought to apply to anything re-exported by those immediate
    // imports as well, since re-exports are supposed to be treated like part of
    // the module they're re-exported from.
    const DeclContext *moduleScopeContextForReexports = moduleScopeContext;
    if (moduleScopeContext && moduleScopeContext->getParentModule() != module)
      moduleScopeContextForReexports = nullptr;

    collectLookupResultsFromImports(decls, reexports, accessPath,
                                    moduleScopeContextForReexports);
  }

  // Remove duplicated declarations, which can happen when the same module is
  // imported indirectly through two intermediate modules.
  llvm::SmallPtrSet<ValueDecl *, 4> knownDecls;
  decls.erase(std::remove_if(decls.begin() + initialCount, decls.end(),
                             [&](ValueDecl *d) -> bool {
                               return !knownDecls.insert(d).second;
                             }),
              decls.end());

  return llvm::makeArrayRef(decls).slice(initialCount);
}

template <typename LookupStrategy>
void ModuleNameLookup<LookupStrategy>::lookupInModule(
    SmallVectorImpl<ValueDecl *> &decls,
    ModuleDecl *module, ModuleDecl::AccessPathTy accessPath,
    const DeclContext *moduleScopeContext,
    ArrayRef<ModuleDecl::ImportedModule> extraImports) {
  assert(module);
  assert(llvm::none_of(extraImports,
                       [](ModuleDecl::ImportedModule import) -> bool {
    return import.second == nullptr;
  }));

  const ModuleDecl::ImportedModule import{accessPath, module};
  const ModuleLookupCacheKey cacheKey{import, moduleScopeContext};

  {
    // Explicitly scope the cache lookup here, because the iterator won't be
    // valid after we do all the work to populate the cache.
    const auto iter = cache.find(cacheKey);
    if (iter != cache.end()) {
      decls.append(iter->second.begin(), iter->second.end());
      return;
    }
  }

  const ArrayRef<ValueDecl *> lookupResults =
      lookupInModuleUncached(decls, module, accessPath, moduleScopeContext,
                             extraImports);
  cache.try_emplace(cacheKey, lookupResults);
}

void namelookup::lookupInModule(const DeclContext *moduleOrFile,
                                ModuleDecl::AccessPathTy topAccessPath,
                                DeclName name,
                                SmallVectorImpl<ValueDecl *> &decls,
                                NLKind lookupKind,
                                ResolutionKind resolutionKind,
                                const DeclContext *moduleScopeContext) {
  assert(moduleScopeContext->isModuleScopeContext());

  auto *startModule = moduleOrFile->getParentModule();
  auto &ctx = startModule->getASTContext();
  auto *stats = ctx.Stats;
  if (stats)
    stats->getFrontendCounters().NumLookupInModule++;

  FrontendStatsTracer tracer(stats, "lookup-in-module");

  // Add private imports to the extra search list.
  SmallVector<ModuleDecl::ImportedModule, 8> extraImports;
  if (auto *file = dyn_cast<FileUnit>(moduleOrFile)) {
    ModuleDecl::ImportFilter importFilter;
    importFilter |= ModuleDecl::ImportFilterKind::Private;
    importFilter |= ModuleDecl::ImportFilterKind::ImplementationOnly;
    file->getImportedModules(extraImports, importFilter);
  }

  LookupByName lookup(ctx, resolutionKind, name, lookupKind);
  lookup.lookupInModule(decls, startModule, topAccessPath, moduleScopeContext,
                        extraImports);
}

void namelookup::lookupVisibleDeclsInModule(
    const DeclContext *moduleOrFile,
    ModuleDecl::AccessPathTy accessPath,
    SmallVectorImpl<ValueDecl *> &decls,
    NLKind lookupKind,
    ResolutionKind resolutionKind,
    const DeclContext *moduleScopeContext) {
  assert(moduleScopeContext->isModuleScopeContext());

  auto *startModule = moduleOrFile->getParentModule();
  auto &ctx = startModule->getASTContext();

  // Add private imports to the extra search list.
  SmallVector<ModuleDecl::ImportedModule, 8> extraImports;
  if (auto *file = dyn_cast<FileUnit>(moduleOrFile)) {
    ModuleDecl::ImportFilter importFilter;
    importFilter |= ModuleDecl::ImportFilterKind::Private;
    importFilter |= ModuleDecl::ImportFilterKind::ImplementationOnly;
    file->getImportedModules(extraImports, importFilter);
  }

  LookupVisibleDecls lookup(ctx, resolutionKind, lookupKind);
  lookup.lookupInModule(decls, startModule, accessPath, moduleScopeContext,
                        extraImports);
}

