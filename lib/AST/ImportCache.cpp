//===--- ImportCache.cpp - Caching the import graph -------------*- C++ -*-===//
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

#include "llvm/ADT/DenseSet.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/ClangModuleLoader.h"
#include "swift/AST/FileUnit.h"
#include "swift/AST/ImportCache.h"
#include "swift/AST/Module.h"

using namespace swift;
using namespace namelookup;

ImportSet::ImportSet(bool hasHeaderImportModule,
                     ArrayRef<ModuleDecl::ImportedModule> topLevelImports,
                     ArrayRef<ModuleDecl::ImportedModule> transitiveImports)
  : HasHeaderImportModule(hasHeaderImportModule),
    NumTopLevelImports(topLevelImports.size()),
    NumTransitiveImports(transitiveImports.size()) {
  auto buffer = getTrailingObjects<ModuleDecl::ImportedModule>();
  std::uninitialized_copy(topLevelImports.begin(), topLevelImports.end(),
                          buffer);
  std::uninitialized_copy(transitiveImports.begin(), transitiveImports.end(),
                          buffer + topLevelImports.size());

#ifndef NDEBUG
  llvm::SmallDenseSet<ModuleDecl::ImportedModule, 8> unique;
  for (auto import : topLevelImports) {
    auto result = unique.insert(import).second;
    assert(result && "Duplicate imports in import set");
  }
  for (auto import : transitiveImports) {
    auto result = unique.insert(import).second;
    assert(result && "Duplicate imports in import set");
  }
#endif
}

void ImportSet::Profile(
    llvm::FoldingSetNodeID &ID,
    ArrayRef<ModuleDecl::ImportedModule> topLevelImports) {
  ID.AddInteger(topLevelImports.size());
  for (auto import : topLevelImports) {
    ID.AddInteger(import.accessPath.size());
    for (auto accessPathElt : import.accessPath) {
      ID.AddPointer(accessPathElt.Item.getAsOpaquePointer());
    }
    ID.AddPointer(import.importedModule);
  }
}

static void collectExports(ModuleDecl::ImportedModule next,
                           SmallVectorImpl<ModuleDecl::ImportedModule> &stack) {
  SmallVector<ModuleDecl::ImportedModule, 4> exports;
  next.importedModule->getImportedModulesForLookup(exports);
  for (auto exported : exports) {
    if (next.accessPath.empty())
      stack.push_back(exported);
    else if (exported.accessPath.empty()) {
      exported.accessPath = next.accessPath;
      stack.push_back(exported);
    } else if (next.accessPath.isSameAs(exported.accessPath)) {
      stack.push_back(exported);
    }
  }
}

ImportSet &
ImportCache::getImportSet(ASTContext &ctx,
                          ArrayRef<ModuleDecl::ImportedModule> imports) {
  bool hasHeaderImportModule = false;
  ModuleDecl *headerImportModule = nullptr;
  if (auto *loader = ctx.getClangModuleLoader())
    headerImportModule = loader->getImportedHeaderModule();

  SmallVector<ModuleDecl::ImportedModule, 4> topLevelImports;

  SmallVector<ModuleDecl::ImportedModule, 4> transitiveImports;
  llvm::SmallDenseSet<ModuleDecl::ImportedModule, 32> visited;

  for (auto next : imports) {
    if (!visited.insert(next).second)
      continue;

    topLevelImports.push_back(next);
    if (next.importedModule == headerImportModule)
      hasHeaderImportModule = true;
  }

  void *InsertPos = nullptr;

  llvm::FoldingSetNodeID ID;
  ImportSet::Profile(ID, topLevelImports);

  if (ImportSet *result = ImportSets.FindNodeOrInsertPos(ID, InsertPos)) {
    if (ctx.Stats)
      ++ctx.Stats->getFrontendCounters().ImportSetFoldHit;
    return *result;
  }

  if (ctx.Stats)
    ++ctx.Stats->getFrontendCounters().ImportSetFoldMiss;

  SmallVector<ModuleDecl::ImportedModule, 4> stack;
  for (auto next : topLevelImports) {
    collectExports(next, stack);
  }

  while (!stack.empty()) {
    auto next = stack.pop_back_val();

    if (!visited.insert(next).second)
      continue;

    transitiveImports.push_back(next);
    if (next.importedModule == headerImportModule)
      hasHeaderImportModule = true;

    collectExports(next, stack);
  }

  // Find the insert position again, in case the above traversal invalidated
  // the folding set via re-entrant calls to getImportSet() from
  // getImportedModulesForLookup().
  if (ImportSet *result = ImportSets.FindNodeOrInsertPos(ID, InsertPos))
    return *result;
  
  size_t bytes = ImportSet::totalSizeToAlloc<ModuleDecl::ImportedModule>(topLevelImports.size() + transitiveImports.size());
  void *mem = ctx.Allocate(bytes, alignof(ImportSet), AllocationArena::Permanent);

  auto *result = new (mem) ImportSet(hasHeaderImportModule,
                                     topLevelImports,
                                     transitiveImports);
  ImportSets.InsertNode(result, InsertPos);

  return *result;
}

ImportSet &ImportCache::getImportSet(const DeclContext *dc) {
  dc = dc->getModuleScopeContext();
  auto *file = dyn_cast<FileUnit>(dc);
  auto *mod = dc->getParentModule();
  if (!file)
    dc = mod;

  auto &ctx = mod->getASTContext();

  auto found = ImportSetForDC.find(dc);
  if (found != ImportSetForDC.end()) {
    if (ctx.Stats)
      ++ctx.Stats->getFrontendCounters().ImportSetCacheHit;
    return *found->second;
  }

  if (ctx.Stats)
    ++ctx.Stats->getFrontendCounters().ImportSetCacheMiss;

  SmallVector<ModuleDecl::ImportedModule, 4> imports;

  imports.emplace_back(
      ModuleDecl::ImportedModule{ImportPath::Access(), mod});

  if (file) {
    file->getImportedModules(imports,
                             {ModuleDecl::ImportFilterKind::Private,
                              ModuleDecl::ImportFilterKind::ImplementationOnly,
                              ModuleDecl::ImportFilterKind::SPIAccessControl});
  }

  auto &result = getImportSet(ctx, imports);
  ImportSetForDC[dc] = &result;

  return result;
}

ArrayRef<ImportPath::Access> ImportCache::allocateArray(
    ASTContext &ctx,
    SmallVectorImpl<ImportPath::Access> &results) {
  if (results.empty())
    return {};
  else if (results.size() == 1 && results[0].empty())
    return {&EmptyAccessPath, 1};
  else
    return ctx.AllocateCopy(results);
}

ArrayRef<ImportPath::Access>
ImportCache::getAllVisibleAccessPaths(const ModuleDecl *mod,
                                      const DeclContext *dc) {
  dc = dc->getModuleScopeContext();
  auto &ctx = mod->getASTContext();

  auto key = std::make_pair(mod, dc);
  auto found = VisibilityCache.find(key);
  if (found != VisibilityCache.end()) {
    if (ctx.Stats)
      ++ctx.Stats->getFrontendCounters().ModuleVisibilityCacheHit;
    return found->second;
  }

  if (ctx.Stats)
    ++ctx.Stats->getFrontendCounters().ModuleVisibilityCacheMiss;

  SmallVector<ImportPath::Access, 1> accessPaths;
  for (auto next : getImportSet(dc).getAllImports()) {
    // If we found 'mod', record the access path.
    if (next.importedModule == mod) {
      // Make sure the list of access paths is unique.
      if (!llvm::is_contained(accessPaths, next.accessPath))
        accessPaths.push_back(next.accessPath);
    }
  }

  auto result = allocateArray(ctx, accessPaths);
  VisibilityCache[key] = result;
  return result;
}

ArrayRef<ImportPath::Access>
ImportCache::getAllAccessPathsNotShadowedBy(const ModuleDecl *mod,
                                            const ModuleDecl *other,
                                            const DeclContext *dc) {
  dc = dc->getModuleScopeContext();
  auto *currentMod = dc->getParentModule();
  auto &ctx = currentMod->getASTContext();

  // Fast path.
  if (currentMod == other)
    return {};

  auto key = std::make_tuple(mod, other, dc);
  auto found = ShadowCache.find(key);
  if (found != ShadowCache.end()) {
    if (ctx.Stats)
      ++ctx.Stats->getFrontendCounters().ModuleShadowCacheHit;
    return found->second;
  }

  if (ctx.Stats)
    ++ctx.Stats->getFrontendCounters().ModuleShadowCacheMiss;

  SmallVector<ModuleDecl::ImportedModule, 4> stack;
  llvm::SmallDenseSet<ModuleDecl::ImportedModule, 32> visited;

  stack.emplace_back(
      ModuleDecl::ImportedModule{ImportPath::Access(), currentMod});

  if (auto *file = dyn_cast<FileUnit>(dc)) {
    file->getImportedModules(stack,
                             {ModuleDecl::ImportFilterKind::Private,
                              ModuleDecl::ImportFilterKind::ImplementationOnly,
                              ModuleDecl::ImportFilterKind::SPIAccessControl});
  }

  SmallVector<ImportPath::Access, 4> accessPaths;

  while (!stack.empty()) {
    auto next = stack.pop_back_val();

    // Don't visit a module more than once.
    if (!visited.insert(next).second)
      continue;

    // Don't visit the 'other' module's re-exports.
    if (next.importedModule == other)
      continue;

    // If we found 'mod' via some access path, remember the access
    // path.
    if (next.importedModule == mod) {
      // Make sure the list of access paths is unique.
      if (!llvm::is_contained(accessPaths, next.accessPath))
        accessPaths.push_back(next.accessPath);
    }

    collectExports(next, stack);
  }

  auto result = allocateArray(ctx, accessPaths);
  ShadowCache[key] = result;
  return result;
};

ArrayRef<ModuleDecl::ImportedModule>
swift::namelookup::getAllImports(const DeclContext *dc) {
  return dc->getASTContext().getImportCache().getImportSet(dc)
    .getAllImports();
}
