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

namespace {
  using ModuleLookupCache = llvm::SmallDenseMap<ModuleDecl::ImportedModule,
                                                TinyPtrVector<ValueDecl *>,
                                                32>;

  class SortCanType {
  public:
    bool operator()(CanType lhs, CanType rhs) const {
      return std::less<TypeBase *>()(lhs.getPointer(), rhs.getPointer());
    }
  };

  using CanTypeSet = llvm::SmallSet<CanType, 4, SortCanType>;
  using NamedCanTypeSet =
    llvm::DenseMap<DeclBaseName, std::pair<ResolutionKind, CanTypeSet>>;
  static_assert(ResolutionKind() == ResolutionKind::Overloadable,
                "Entries in NamedCanTypeSet should be overloadable initially");
} // end anonymous namespace


/// Returns true if this particular ValueDecl is overloadable.
static bool isOverloadable(const ValueDecl *VD) {
  return isa<FuncDecl>(VD) ||
         isa<ConstructorDecl>(VD) ||
         isa<SubscriptDecl>(VD);
}

static bool isValidOverload(CanTypeSet &overloads, const ValueDecl *VD) {
  if (!isOverloadable(VD))
    return overloads.empty();
  return !overloads.count(VD->getInterfaceType()->getCanonicalType());
}

static bool isValidOverload(NamedCanTypeSet &overloads, const ValueDecl *VD) {
  auto &entry = overloads[VD->getBaseName()];
  if (entry.first != ResolutionKind::Overloadable)
    return false;
  return isValidOverload(entry.second, VD);
}

/// Updates \p overloads with the types of the given decls.
///
/// \returns true if all of the given decls are overloadable, false if not.
static bool updateOverloadSet(CanTypeSet &overloads,
                              ArrayRef<ValueDecl *> decls) {
  for (auto result : decls) {
    if (!isOverloadable(result))
      return false;
    if (!result->hasInterfaceType())
      continue;
    overloads.insert(result->getInterfaceType()->getCanonicalType());
  }
  return true;
}

/// Updates \p overloads with the types of the given decls.
///
/// \returns true, since there can always be more overloadable decls.
static bool updateOverloadSet(NamedCanTypeSet &overloads,
                              ArrayRef<ValueDecl *> decls) {
  for (auto result : decls) {
    auto &entry = overloads[result->getBaseName()];
    if (!isOverloadable(result))
      entry.first = ResolutionKind::Exact;
    else if (!result->hasInterfaceType())
      continue;
    else
      entry.second.insert(result->getInterfaceType()->getCanonicalType());
  }
  return true;
}

/// After finding decls by name lookup, filter based on the given
/// resolution kind and existing overload set and add them to \p results.
template <typename OverloadSetTy>
static ResolutionKind recordImportDecls(LazyResolver *typeResolver,
                                        SmallVectorImpl<ValueDecl *> &results,
                                        ArrayRef<ValueDecl *> newDecls,
                                        OverloadSetTy &overloads,
                                        ResolutionKind resolutionKind) {
  switch (resolutionKind) {
  case ResolutionKind::Overloadable: {
    // Add new decls if they provide a new overload. Note that the new decls
    // may be ambiguous with respect to each other, just not any existing decls.
    std::copy_if(newDecls.begin(), newDecls.end(), std::back_inserter(results),
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
      return isValidOverload(overloads, result);
    });

    // Update the overload set.
    bool stillOverloadable = updateOverloadSet(overloads, newDecls);
    return stillOverloadable ? ResolutionKind::Overloadable
                             : ResolutionKind::Exact;
  }

  case ResolutionKind::Exact:
    // Add all decls. If they're ambiguous, they're ambiguous.
    results.append(newDecls.begin(), newDecls.end());
    return ResolutionKind::Exact;

  case ResolutionKind::TypesOnly:
    // Add type decls only. If they're ambiguous, they're ambiguous.
    std::copy_if(newDecls.begin(), newDecls.end(), std::back_inserter(results),
                 [](const ValueDecl *VD) { return isa<TypeDecl>(VD); });
    return ResolutionKind::TypesOnly;
  }

  llvm_unreachable("bad ResolutionKind");
}

/// Performs a qualified lookup into the given module and, if necessary, its
/// reexports, observing proper shadowing rules.
template <typename OverloadSetTy, typename CallbackTy>
static void lookupInModule(ModuleDecl *module, ModuleDecl::AccessPathTy accessPath,
                           SmallVectorImpl<ValueDecl *> &decls,
                           ResolutionKind resolutionKind, bool canReturnEarly,
                           LazyResolver *typeResolver,
                           ModuleLookupCache &cache,
                           const DeclContext *moduleScopeContext,
                           bool respectAccessControl,
                           ArrayRef<ModuleDecl::ImportedModule> extraImports,
                           CallbackTy callback) {
  assert(module);
  assert(std::none_of(extraImports.begin(), extraImports.end(),
                      [](ModuleDecl::ImportedModule import) -> bool {
    return !import.second;
  }));

  ModuleLookupCache::iterator iter;
  bool isNew;
  std::tie(iter, isNew) = cache.insert({{accessPath, module}, {}});
  if (!isNew) {
    decls.append(iter->second.begin(), iter->second.end());
    return;
  }

  size_t initialCount = decls.size();

  SmallVector<ValueDecl *, 4> localDecls;
  callback(module, accessPath, localDecls);
  if (respectAccessControl) {
    auto newEndIter = std::remove_if(localDecls.begin(), localDecls.end(),
                                    [=](ValueDecl *VD) {
      return !VD->isAccessibleFrom(moduleScopeContext);
    });
    localDecls.erase(newEndIter, localDecls.end());

    // This only applies to immediate imports of the top-level module.
    if (moduleScopeContext && moduleScopeContext->getParentModule() != module)
      moduleScopeContext = nullptr;
  }

  OverloadSetTy overloads;
  resolutionKind = recordImportDecls(typeResolver, decls, localDecls,
                                     overloads, resolutionKind);

  bool foundDecls = decls.size() > initialCount;
  if (!foundDecls || !canReturnEarly ||
      resolutionKind == ResolutionKind::Overloadable) {
    SmallVector<ModuleDecl::ImportedModule, 8> reexports;
    module->getImportedModulesForLookup(reexports);
    assert(std::none_of(reexports.begin(), reexports.end(),
                        [](ModuleDecl::ImportedModule import) -> bool {
      return !import.second;
    }));
    reexports.append(extraImports.begin(), extraImports.end());

    // Prefer scoped imports (import func Swift.max) to whole-module imports.
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
      lookupInModule<OverloadSetTy>(next.second, combinedAccessPath,
                                    resultSet, resolutionKind, canReturnEarly,
                                    typeResolver, cache, moduleScopeContext,
                                    respectAccessControl, {}, callback);
    }

    // Add the results from scoped imports.
    resolutionKind = recordImportDecls(typeResolver, decls, scopedValues,
                                       overloads, resolutionKind);

    // Add the results from unscoped imports.
    foundDecls = decls.size() > initialCount;
    if (!foundDecls || !canReturnEarly ||
        resolutionKind == ResolutionKind::Overloadable) {
      resolutionKind = recordImportDecls(typeResolver, decls, unscopedValues,
                                         overloads, resolutionKind);
    }
  }

  // Remove duplicated declarations.
  llvm::SmallPtrSet<ValueDecl *, 4> knownDecls;
  decls.erase(std::remove_if(decls.begin() + initialCount, decls.end(),
                             [&](ValueDecl *d) -> bool { 
                               return !knownDecls.insert(d).second;
                             }),
              decls.end());

  auto &cachedValues = cache[{accessPath, module}];
  cachedValues.insert(cachedValues.end(),
                      decls.begin() + initialCount,
                      decls.end());
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
  ModuleLookupCache cache;
  bool respectAccessControl = startModule->getASTContext().LangOpts
                                .EnableAccessControl;
  ::lookupInModule<CanTypeSet>(startModule, topAccessPath, decls,
                               resolutionKind, /*canReturnEarly=*/true,
                               typeResolver, cache, moduleScopeContext,
                               respectAccessControl, extraImports,
    [=](ModuleDecl *module, ModuleDecl::AccessPathTy path,
        SmallVectorImpl<ValueDecl *> &localDecls) {
      module->lookupValue(path, name, lookupKind, localDecls);
    }
  );
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
  ModuleLookupCache cache;
  bool respectAccessControl = M->getASTContext().LangOpts.EnableAccessControl;
  ::lookupInModule<NamedCanTypeSet>(M, accessPath, decls,
                                    resolutionKind, /*canReturnEarly=*/false,
                                    typeResolver, cache, moduleScopeContext,
                                    respectAccessControl, extraImports,
    [=](ModuleDecl *module, ModuleDecl::AccessPathTy path,
        SmallVectorImpl<ValueDecl *> &localDecls) {
      VectorDeclConsumer consumer(localDecls);
      module->lookupVisibleDecls(path, consumer, lookupKind);
    }
  );
}

