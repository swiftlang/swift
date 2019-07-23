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

#include "swift/AST/NameLookup.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/LazyResolver.h"
#include "llvm/Support/raw_ostream.h"

using namespace swift;
using namespace namelookup;

/// Returns true if this particular ValueDecl is overloadable.
static bool isOverloadable(const ValueDecl *VD) {
  // FIXME: This is very suspect; it doesn't really match how the rest of the
  // compiler works anymore. (With extensions imported from different modules,
  // properties can have the same base names as methods.) How do we want
  // cross-module shadowing to work?
  return isa<FuncDecl>(VD) ||
         isa<ConstructorDecl>(VD) ||
         isa<SubscriptDecl>(VD);
}

namespace {

/// Encapsulates the work done for a recursive qualified lookup into a module.
///
/// The \p LookupStrategy handles the non-recursive part of the lookup, as well
/// as how to combine results from across modules (e.g. handling shadowing). It
/// must be a subclass of ModuleNameLookup.
template <typename LookupStrategy>
class ModuleNameLookup {
  /// An alias for the LookupStrategy subclass's nested OverloadSetTy whose
  /// resolution can be delayed until after the subclass type is
  /// considered "complete".
  template <bool CRTPWorkaround>
  using OverloadSetTy =
      typename std::enable_if<CRTPWorkaround,
                              LookupStrategy>::type::OverloadSetTy;

  /// The usable results of a lookup in a particular module may differ based on
  /// where the lookup is happening.
  using ModuleLookupCacheKey = std::pair<ModuleDecl::ImportedModule,
                                         const DeclContext * /*lookupScope*/>;
  using ModuleLookupCache = llvm::SmallDenseMap<ModuleLookupCacheKey,
                                                TinyPtrVector<ValueDecl *>, 32>;

  ModuleLookupCache cache;
  LazyResolver * const typeResolver;
  const ResolutionKind resolutionKind;
  const bool respectAccessControl;

  LookupStrategy *getDerived() {
    static_assert(std::is_base_of<ModuleNameLookup<LookupStrategy>,
                                  LookupStrategy>::value,
                  "ModuleNameLookup is a CRTP class");
    return static_cast<LookupStrategy *>(this);
  }

  /// After finding decls by name lookup, filter based on the given
  /// resolution kind and existing overload set and add them to \p results.
  ///
  /// \p overloads is updated based on the new declarations.
  ///
  /// \returns true if lookup is (locally) complete and does not need to recurse
  /// further.
  template <bool CRTPWorkaround = true>
  bool recordImportDecls(
      SmallVectorImpl<ValueDecl *> &results,
      ArrayRef<ValueDecl *> newDecls,
      OverloadSetTy<CRTPWorkaround> &overloads);

  /// Given a list of imports and an access path to limit by, perform a lookup
  /// into each of them and record the results in \p decls, filtering based on
  /// the given resolution kind and existing overload set.
  template <bool CRTPWorkaround = true>
  void collectLookupResultsFromImports(
      SmallVectorImpl<ValueDecl *> &decls,
      ArrayRef<ModuleDecl::ImportedModule> imports,
      ModuleDecl::AccessPathTy accessPath,
      const DeclContext *moduleScopeContext,
      OverloadSetTy<CRTPWorkaround> &overloads);

  /// Performs a qualified lookup into the given module and, if necessary, its
  /// reexports, observing proper shadowing rules. The lookup into \p module
  /// itself will not check or update the lookup cache.
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
  ModuleNameLookup(LazyResolver *typeResolver, ModuleDecl *M,
                   ResolutionKind resolutionKind)
      : typeResolver(typeResolver), resolutionKind(resolutionKind),
        respectAccessControl(!M->getASTContext().isAccessControlDisabled()) {}

  /// Performs a qualified lookup into the given module and, if necessary, its
  /// reexports, observing proper shadowing rules.
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
  LookupByName(LazyResolver *typeResolver, ModuleDecl *M,
               ResolutionKind resolutionKind, DeclName name, NLKind lookupKind)
    : Super(typeResolver, M, resolutionKind), name(name),
      lookupKind(lookupKind) {}

private:
  class SortCanType {
  public:
    bool operator()(CanType lhs, CanType rhs) const {
      return std::less<TypeBase *>()(lhs.getPointer(), rhs.getPointer());
    }
  };

  using OverloadSetTy = llvm::SmallSet<CanType, 4, SortCanType>;

  /// Does \p VD conflict with the \p overloads we've already seen?
  static bool isValidOverload(const OverloadSetTy &overloads,
                              const ValueDecl *VD) {
    if (!isOverloadable(VD))
      return overloads.empty();
    return !overloads.count(VD->getInterfaceType()->getCanonicalType());
  }

  /// Updates \p overloads with the types of the given decls.
  ///
  /// \returns true if all of the given decls are overloadable, false if not.
  static bool updateOverloadSet(OverloadSetTy &overloads,
                                ArrayRef<ValueDecl *> decls) {
    for (auto result : decls) {
      if (!isOverloadable(result))
        return false;
      if (!result->hasInterfaceType())
        continue;
      // FIXME: This relies on the interface type including argument labels.
      // FIXME: ...and it doesn't handle different generic requirements.
      overloads.insert(result->getInterfaceType()->getCanonicalType());
    }
    return true;
  }

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
  LookupVisibleDecls(LazyResolver *typeResolver, ModuleDecl *M,
                     ResolutionKind resolutionKind, NLKind lookupKind)
    : ModuleNameLookup(typeResolver, M, resolutionKind),
      lookupKind(lookupKind) {}

private:
  using OverloadSetEntry =
      std::pair<ResolutionKind, LookupByName::OverloadSetTy>;
  using OverloadSetTy = llvm::DenseMap<DeclBaseName, OverloadSetEntry>;
  static_assert(ResolutionKind() == ResolutionKind::Overloadable,
                "Entries in NamedCanTypeSet should be overloadable initially");

  /// Does \p VD conflict with the \p overloads we've already seen?
  static bool isValidOverload(OverloadSetTy &overloads, const ValueDecl *VD) {
    // Note: 'overloads' is not const because it's cheaper to create the empty
    // set value under this name once and check it repeatedly.
    const OverloadSetEntry &entry = overloads[VD->getBaseName()];
    if (entry.first != ResolutionKind::Overloadable)
      return false;
    return LookupByName::isValidOverload(entry.second, VD);
  }

  /// Updates \p overloads with the types of the given decls.
  ///
  /// \returns true, since there can always be more overloadable decls.
  static bool updateOverloadSet(OverloadSetTy &overloads,
                                ArrayRef<ValueDecl *> decls) {
    for (auto result : decls) {
      OverloadSetEntry &entry = overloads[result->getBaseName()];
      if (!isOverloadable(result)) {
        entry.first = ResolutionKind::Exact;
        entry.second.clear();
        continue;
      }

      if (!result->hasInterfaceType())
        continue;

      // FIXME: This relies on the interface type including argument labels.
      // FIXME: ...and it doesn't handle different generic requirements.
      entry.second.insert(result->getInterfaceType()->getCanonicalType());
    }
    return true;
  }

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
template <bool CRTPWorkaround>
bool ModuleNameLookup<LookupStrategy>::recordImportDecls(
    SmallVectorImpl<ValueDecl *> &results,
    ArrayRef<ValueDecl *> newDecls,
    OverloadSetTy<CRTPWorkaround> &overloads) {

  static_assert(
      std::is_same<decltype(overloads),
                   typename LookupStrategy::OverloadSetTy &>::value,
      "Template params should be inferred.");

  const size_t originalSize = results.size();

  switch (resolutionKind) {
  case ResolutionKind::Overloadable: {
    // Add new decls if they provide a new overload. Note that the new decls
    // may be ambiguous with respect to each other, just not any decls already
    // in the overload set.
    llvm::copy_if(newDecls, std::back_inserter(results),
                  [&](ValueDecl *result) -> bool {
      if (!result->hasInterfaceType()) {
        if (typeResolver) {
          typeResolver->resolveDeclSignature(result);
          if (result->isInvalid())
            return true;
        } else {
          return true;
        }
      }
      return getDerived()->isValidOverload(overloads, result);
    });

    // Update the overload set.
    bool stillOverloadable = getDerived()->updateOverloadSet(overloads,
                                                             newDecls);
    if (stillOverloadable)
      return false;
    break;
  }

  case ResolutionKind::Exact:
    // Add all decls. If they're ambiguous, they're ambiguous; if we got to this
    // point, the caller hasn't found anything that shadows these decls.
    results.append(newDecls.begin(), newDecls.end());
    break;

  case ResolutionKind::TypesOnly:
    // Add type decls only. If they're ambiguous, they're ambiguous; if we got
    // to this point, the caller hasn't found anything that shadows these decls.
    llvm::copy_if(newDecls, std::back_inserter(results),
                  [](const ValueDecl *VD) { return isa<TypeDecl>(VD); });
    break;
  }

  return results.size() != originalSize && getDerived()->canReturnEarly();
}

template <typename LookupStrategy>
template <bool CRTPWorkaround>
void ModuleNameLookup<LookupStrategy>::collectLookupResultsFromImports(
    SmallVectorImpl<ValueDecl *> &decls,
    ArrayRef<ModuleDecl::ImportedModule> reexports,
    ModuleDecl::AccessPathTy accessPath,
    const DeclContext *moduleScopeContext,
    OverloadSetTy<CRTPWorkaround> &overloads) {

  static_assert(
      std::is_same<decltype(overloads),
                   typename LookupStrategy::OverloadSetTy &>::value,
      "Template params should be inferred.");

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
  const bool canReturnEarly = recordImportDecls(decls, scopedValues, overloads);
  if (!canReturnEarly)
    (void)recordImportDecls(decls, unscopedValues, overloads);
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
  if (respectAccessControl) {
    llvm::erase_if(localDecls, [=](ValueDecl *VD) {
      return !VD->isAccessibleFrom(moduleScopeContext);
    });
  }

  // Record the decls by overload signature.
  const size_t initialCount = decls.size();
  typename LookupStrategy::OverloadSetTy overloads;
  const bool canReturnEarly = recordImportDecls(decls, localDecls, overloads);

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
                                    moduleScopeContextForReexports, overloads);
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

void namelookup::lookupInModule(ModuleDecl *startModule,
                                ModuleDecl::AccessPathTy topAccessPath,
                                DeclName name,
                                SmallVectorImpl<ValueDecl *> &decls,
                                NLKind lookupKind,
                                ResolutionKind resolutionKind,
                                LazyResolver *typeResolver,
                                const DeclContext *moduleScopeContext,
                                ArrayRef<ModuleDecl::ImportedModule> extraImports) {
  assert(moduleScopeContext && moduleScopeContext->isModuleScopeContext());
  LookupByName lookup(typeResolver, startModule, resolutionKind, name,
                      lookupKind);
  lookup.lookupInModule(decls, startModule, topAccessPath, moduleScopeContext,
                        extraImports);
}

void namelookup::lookupVisibleDeclsInModule(
    ModuleDecl *M,
    ModuleDecl::AccessPathTy accessPath,
    SmallVectorImpl<ValueDecl *> &decls,
    NLKind lookupKind,
    ResolutionKind resolutionKind,
    LazyResolver *typeResolver,
    const DeclContext *moduleScopeContext,
    ArrayRef<ModuleDecl::ImportedModule> extraImports) {
  assert(moduleScopeContext && moduleScopeContext->isModuleScopeContext());
  LookupVisibleDecls lookup(typeResolver, M, resolutionKind, lookupKind);
  lookup.lookupInModule(decls, M, accessPath, moduleScopeContext, extraImports);
}

