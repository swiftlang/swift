//===--- ModuleNameLookup.cpp - Name lookup within a module ----*- c++ -*--===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "ModuleNameLookup.h"
#include "swift/AST/AST.h"
#include "swift/AST/LazyResolver.h"
#include "llvm/Support/raw_ostream.h"

using namespace swift;
using namespace namelookup;

namespace {
  /// A cache used by lookupInModule().
  class ModuleLookupCache {
  public:
    using MapTy = llvm::SmallDenseMap<Module::ImportedModule,
                                      TinyPtrVector<ValueDecl *>,
                                      32>;
    MapTy Map;
    bool SearchedClangModule = false;
  };

  class SortCanType {
  public:
    bool operator()(CanType lhs, CanType rhs) const {
      return std::less<TypeBase *>()(lhs.getPointer(), rhs.getPointer());
    }
  };

  using CanTypeSet = llvm::SmallSet<CanType, 8, SortCanType>;
  using NamedCanTypeSet =
    llvm::DenseMap<Identifier, std::pair<ResolutionKind, CanTypeSet>>;
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
  if (overloads.count(VD->getType()->getCanonicalType()))
    return false;
  return true;
}

static bool isValidOverload(NamedCanTypeSet &overloads, const ValueDecl *VD) {
  auto &entry = overloads[VD->getName()];
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
    if (!result->hasType())
      continue;
    overloads.insert(result->getType()->getCanonicalType());
  }
  return true;
}

/// Updates \p overloads with the types of the given decls.
///
/// \returns true, since there can always be more overloadable decls.
static bool updateOverloadSet(NamedCanTypeSet &overloads,
                              ArrayRef<ValueDecl *> decls) {
  for (auto result : decls) {
    auto &entry = overloads[result->getName()];
    if (!isOverloadable(result))
      entry.first = ResolutionKind::Exact;
    else if (!result->hasType())
      continue;
    else
      entry.second.insert(result->getType()->getCanonicalType());
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
      if (!result->hasType()) {
        if (typeResolver)
          typeResolver->resolveDeclSignature(result);
        else
          return true;
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

}

/// Performs a qualified lookup into the given module and, if necessary, its
/// reexports, observing proper shadowing rules.
template <typename OverloadSetTy, typename CallbackTy>
static void lookupInModule(Module *module, Module::AccessPathTy accessPath,
                           SmallVectorImpl<ValueDecl *> &decls,
                           ResolutionKind resolutionKind, bool canReturnEarly,
                           LazyResolver *typeResolver,
                           ModuleLookupCache &cache,
                           ArrayRef<Module::ImportedModule> extraImports,
                           CallbackTy callback) {
  ModuleLookupCache::MapTy::iterator iter;
  bool isNew;
  std::tie(iter, isNew) = cache.Map.insert({{accessPath, module}, {}});
  if (!isNew) {
    decls.append(iter->second.begin(), iter->second.end());
    return;
  }

  size_t initialCount = decls.size();

  // Only perform unscoped searches once in Clang modules.
  // FIXME: This is a weird hack. ClangImporter should just filter the results
  // for us.
  bool isClangModule = false;
  if (accessPath.empty())
    isClangModule = module->getKind() == ModuleKind::Clang;

  SmallVector<ValueDecl *, 4> localDecls;
  if (!isClangModule || !cache.SearchedClangModule) {
    callback(module, accessPath, localDecls);
    if (isClangModule)
      cache.SearchedClangModule = true;
  }

  OverloadSetTy overloads;
  // FIXME: Pass TypeResolver down instead of getting it from the AST.
  resolutionKind = recordImportDecls(typeResolver, decls, localDecls,
                                     overloads, resolutionKind);

  bool foundDecls = decls.size() > initialCount;
  if (!foundDecls || !canReturnEarly ||
      resolutionKind == ResolutionKind::Overloadable) {
    SmallVector<Module::ImportedModule, 8> reexports;
    module->getImportedModules(reexports, false);
    reexports.append(extraImports.begin(), extraImports.end());

    // Prefer scoped imports (import def swift.max) to whole-module imports.
    SmallVector<ValueDecl *, 8> unscopedValues;
    SmallVector<ValueDecl *, 8> scopedValues;
    for (auto next : reexports) {
      // Filter any whole-module imports, and skip specific-decl imports if the
      // import path doesn't match exactly.
      Module::AccessPathTy combinedAccessPath;
      if (accessPath.empty()) {
        combinedAccessPath = next.first;
      } else if (!next.first.empty() &&
                 !Module::isSameAccessPath(next.first, accessPath)) {
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
                                    typeResolver, cache, {}, callback);
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

  std::sort(decls.begin() + initialCount, decls.end());
  auto afterUnique = std::unique(decls.begin() + initialCount, decls.end());
  decls.erase(afterUnique, decls.end());

  auto &cachedValues = cache.Map[{accessPath, module}];
  cachedValues.insert(cachedValues.end(),
                      decls.begin() + initialCount,
                      decls.end());
}

void namelookup::lookupInModule(Module *startModule,
                                Module::AccessPathTy topAccessPath,
                                Identifier name,
                                SmallVectorImpl<ValueDecl *> &decls,
                                NLKind lookupKind,
                                ResolutionKind resolutionKind,
                                LazyResolver *typeResolver,
                                ArrayRef<Module::ImportedModule> extraImports) {
  ModuleLookupCache cache;
  ::lookupInModule<CanTypeSet>(startModule, topAccessPath, decls,
                               resolutionKind, /*canReturnEarly=*/true,
                               typeResolver, cache, extraImports,
    [=](Module *module, Module::AccessPathTy path,
        SmallVectorImpl<ValueDecl *> &localDecls) {
      module->lookupValue(path, name, lookupKind, localDecls);
    }
  );
}

namespace {
class VectorDeclConsumer : public VisibleDeclConsumer {
public:
  SmallVectorImpl<ValueDecl *> &results;
  explicit VectorDeclConsumer(SmallVectorImpl<ValueDecl *> &decls)
    : results(decls) {}

  void foundDecl(ValueDecl *VD, DeclVisibilityKind Reason) override {
    results.push_back(VD);
  }
};
}

void namelookup::lookupVisibleDeclsInModule(
    Module *M,
    Module::AccessPathTy accessPath,
    SmallVectorImpl<ValueDecl *> &decls,
    NLKind lookupKind,
    ResolutionKind resolutionKind,
    LazyResolver *typeResolver,
    ArrayRef<Module::ImportedModule> extraImports) {
  ModuleLookupCache cache;
  ::lookupInModule<NamedCanTypeSet>(M, accessPath, decls,
                                    resolutionKind, /*canReturnEarly=*/false,
                                    typeResolver, cache, extraImports,
    [=](Module *module, Module::AccessPathTy path,
        SmallVectorImpl<ValueDecl *> &localDecls) {
      VectorDeclConsumer consumer(localDecls);
      module->lookupVisibleDecls(path, consumer, lookupKind);
    }
  );
}

