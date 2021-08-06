//===--- ModuleDependencies.h - Module Dependencies -------------*- C++ -*-===//
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
//
// This file implements data structures for capturing module dependencies.
//
//===----------------------------------------------------------------------===//
#include "swift/AST/ModuleDependencies.h"
#include "swift/AST/Decl.h"
#include "swift/AST/SourceFile.h"
using namespace swift;

ModuleDependenciesKind& operator++(ModuleDependenciesKind& e) {
  if (e == ModuleDependenciesKind::LastKind) {
    llvm_unreachable("Attempting to incrementa last enum value on ModuleDependenciesKind");
  }
  e = ModuleDependenciesKind(static_cast<std::underlying_type<ModuleDependenciesKind>::type>(e) + 1);
  return e;
}

ModuleDependenciesStorageBase::~ModuleDependenciesStorageBase() { }

bool ModuleDependencies::isSwiftModule() const {
  return isSwiftTextualModule() ||
         isSwiftBinaryModule() ||
         isSwiftPlaceholderModule();
}

bool ModuleDependencies::isSwiftTextualModule() const {
  return isa<SwiftTextualModuleDependenciesStorage>(storage.get());
}

bool ModuleDependencies::isSwiftBinaryModule() const {
  return isa<SwiftBinaryModuleDependencyStorage>(storage.get());
}

bool ModuleDependencies::isSwiftPlaceholderModule() const {
  return isa<SwiftPlaceholderModuleDependencyStorage>(storage.get());
}

bool ModuleDependencies::isClangModule() const {
  return isa<ClangModuleDependenciesStorage>(storage.get());
}

/// Retrieve the dependencies for a Swift module.
const SwiftTextualModuleDependenciesStorage *
ModuleDependencies::getAsSwiftTextualModule() const {
  return dyn_cast<SwiftTextualModuleDependenciesStorage>(storage.get());
}

/// Retrieve the dependencies for a binary Swift dependency module.
const SwiftBinaryModuleDependencyStorage *
ModuleDependencies::getAsSwiftBinaryModule() const {
  return dyn_cast<SwiftBinaryModuleDependencyStorage>(storage.get());
}

/// Retrieve the dependencies for a Clang module.
const ClangModuleDependenciesStorage *
ModuleDependencies::getAsClangModule() const {
  return dyn_cast<ClangModuleDependenciesStorage>(storage.get());
}

/// Retrieve the dependencies for a placeholder dependency module stub.
const SwiftPlaceholderModuleDependencyStorage *
ModuleDependencies::getAsPlaceholderDependencyModule() const {
  return dyn_cast<SwiftPlaceholderModuleDependencyStorage>(storage.get());
}

void ModuleDependencies::addModuleDependency(
    StringRef module, llvm::StringSet<> *alreadyAddedModules) {
  if (!alreadyAddedModules || alreadyAddedModules->insert(module).second)
    storage->moduleDependencies.push_back(module.str());
}

void ModuleDependencies::addModuleDependencies(
    const SourceFile &sf, llvm::StringSet<> &alreadyAddedModules) {
  // Add all of the module dependencies.
  SmallVector<Decl *, 32> decls;
  sf.getTopLevelDecls(decls);
  for (auto decl : decls) {
    auto importDecl = dyn_cast<ImportDecl>(decl);
    if (!importDecl)
      continue;

    addModuleDependency(importDecl->getModulePath(), &alreadyAddedModules);
  }

  auto fileName = sf.getFilename();
  if (fileName.empty())
    return;

  // If the storage is for an interface file, the only source file we
  // should see is that interface file.
  auto swiftStorage = cast<SwiftTextualModuleDependenciesStorage>(storage.get());
  if (swiftStorage->swiftInterfaceFile) {
    assert(fileName == *swiftStorage->swiftInterfaceFile);
    return;
  }

  // Otherwise, record the source file.
  swiftStorage->sourceFiles.push_back(fileName.str());
}

Optional<std::string> ModuleDependencies::getBridgingHeader() const {
  auto swiftStorage = cast<SwiftTextualModuleDependenciesStorage>(storage.get());
  return swiftStorage->bridgingHeaderFile;
}

void ModuleDependencies::addBridgingHeader(StringRef bridgingHeader) {
  auto swiftStorage = cast<SwiftTextualModuleDependenciesStorage>(storage.get());
  assert(!swiftStorage->bridgingHeaderFile);
  swiftStorage->bridgingHeaderFile = bridgingHeader.str();
}

/// Add source files that the bridging header depends on.
void ModuleDependencies::addBridgingSourceFile(StringRef bridgingSourceFile) {
  auto swiftStorage = cast<SwiftTextualModuleDependenciesStorage>(storage.get());
  swiftStorage->bridgingSourceFiles.push_back(bridgingSourceFile.str());
}

void ModuleDependencies::addSourceFile(StringRef sourceFile) {
  auto swiftStorage = cast<SwiftTextualModuleDependenciesStorage>(storage.get());
  swiftStorage->sourceFiles.push_back(sourceFile.str());
}

/// Add (Clang) module on which the bridging header depends.
void ModuleDependencies::addBridgingModuleDependency(
    StringRef module, llvm::StringSet<> &alreadyAddedModules) {
  auto swiftStorage = cast<SwiftTextualModuleDependenciesStorage>(storage.get());
  if (alreadyAddedModules.insert(module).second)
    swiftStorage->bridgingModuleDependencies.push_back(module.str());
}

llvm::StringMap<ModuleDependenciesVector> &
GlobalModuleDependenciesCache::getDependenciesMap(ModuleDependenciesKind kind) {
  auto it = ModuleDependenciesKindMap.find(kind);
  assert(it != ModuleDependenciesKindMap.end() && "invalid dependency kind");
  return it->second;
}

const llvm::StringMap<ModuleDependenciesVector> &
GlobalModuleDependenciesCache::getDependenciesMap(ModuleDependenciesKind kind) const {
  auto it = ModuleDependenciesKindMap.find(kind);
  assert(it != ModuleDependenciesKindMap.end() && "invalid dependency kind");
  return it->second;
}

static std::string moduleBasePath(const StringRef modulePath) {
  auto parent = llvm::sys::path::parent_path(modulePath);
  // If the modulePath is that of a .swiftinterface contained in a .swiftmodule,
  // disambiguate further to parent.
  if (llvm::sys::path::extension(parent) == ".swiftmodule") {
    parent = llvm::sys::path::parent_path(parent);
  }

  // If the module is a part of a framework, disambiguate to the framework's parent
  if (llvm::sys::path::filename(parent) == "Modules") {
    auto grandParent = llvm::sys::path::parent_path(parent);
    if (llvm::sys::path::extension(grandParent) == ".framework") {
      parent = llvm::sys::path::parent_path(grandParent);
    }
  }

  return parent.str();
}

static bool moduleContainedInImportPathSet(const StringRef modulePath,
                                           const llvm::StringSet<> &importPaths)
{
  return importPaths.contains(moduleBasePath(modulePath));
}

static bool moduleContainedInImportPathSet(const ModuleDependencies &module,
                                           const llvm::StringSet<> &importPaths)
{
  std::string modulePath = "";
  switch (module.getKind()) {
    case swift::ModuleDependenciesKind::SwiftTextual: {
      auto *swiftDep = module.getAsSwiftTextualModule();
      if (swiftDep->swiftInterfaceFile)
        modulePath = *(swiftDep->swiftInterfaceFile);
      else {
        // If we encountered a Swift textual dependency without an interface
        // file, we are seeing the main scan module itself. This means that
        // our search-path disambiguation is not necessary here.
        return true;
      }
      break;
    }
    case swift::ModuleDependenciesKind::SwiftBinary: {
      auto *swiftBinaryDep = module.getAsSwiftBinaryModule();
      modulePath = swiftBinaryDep->compiledModulePath;
      break;
    }
    case swift::ModuleDependenciesKind::Clang: {
      auto *clangDep = module.getAsClangModule();
      modulePath = clangDep->moduleMapFile;
      break;
    }
    case swift::ModuleDependenciesKind::SwiftPlaceholder: {
      // Placeholders are resolved as `true` because they are not associated with
      // any specific search path.
      return true;
    }
    default:
      llvm_unreachable("Unhandled dependency kind.");
  }

  if (moduleContainedInImportPathSet(modulePath, importPaths)) {
    return true;
  }
  return false;
}

GlobalModuleDependenciesCache::GlobalModuleDependenciesCache()  {
  for (auto kind = ModuleDependenciesKind::FirstKind;
       kind != ModuleDependenciesKind::LastKind; ++kind) {
    ModuleDependenciesKindMap.insert(
        {kind, llvm::StringMap<ModuleDependenciesVector>()});
  }

  ModuleDependenciesKindMap.insert(
      {ModuleDependenciesKind::SwiftBinary,
       llvm::StringMap<ModuleDependenciesVector>()});
  ModuleDependenciesKindMap.insert(
      {ModuleDependenciesKind::SwiftPlaceholder,
       llvm::StringMap<ModuleDependenciesVector>()});
  ModuleDependenciesKindMap.insert(
      {ModuleDependenciesKind::Clang,
       llvm::StringMap<ModuleDependenciesVector>()});
}

Optional<ModuleDependencies> GlobalModuleDependenciesCache::findDependencies(
    StringRef moduleName,
    ModuleLookupSpecifics details) const {
  if (!details.kind) {
    for (auto kind = ModuleDependenciesKind::FirstKind;
         kind != ModuleDependenciesKind::LastKind; ++kind) {
      auto dep = findDependencies(moduleName, {kind, details.currentSearchPaths});
      if (dep.hasValue())
        return dep.getValue();
    }
    return None;
  }

  const auto &map = getDependenciesMap(*details.kind);
  auto known = map.find(moduleName);
  if (known != map.end()) {
    assert(!known->second.empty());
    for (auto &dep : known->second) {
      if (moduleContainedInImportPathSet(dep, details.currentSearchPaths))
        return dep;
    }
    return None;
  }
  return None;
}

bool GlobalModuleDependenciesCache::hasDependencies(
    StringRef moduleName,
    ModuleLookupSpecifics details) const {
  return findDependencies(moduleName, details).hasValue();
}

Optional<ModuleDependenciesVector>
GlobalModuleDependenciesCache::findAllDependenciesIrrespectiveOfSearchPaths(
    StringRef moduleName, Optional<ModuleDependenciesKind> kind) const {
  if (!kind) {
    for (auto kind = ModuleDependenciesKind::FirstKind;
         kind != ModuleDependenciesKind::LastKind; ++kind) {
      auto deps = findAllDependenciesIrrespectiveOfSearchPaths(moduleName, kind);
      if (deps.hasValue())
        return deps.getValue();
    }
    return None;
  }

  const auto &map = getDependenciesMap(*kind);
  auto known = map.find(moduleName);
  if (known != map.end()) {
    assert(!known->second.empty());
    return known->second;
  }
  return None;
}

static std::string modulePathForVerification(const ModuleDependencies &module) {
  std::string existingModulePath = "";
  switch (module.getKind()) {
    case swift::ModuleDependenciesKind::SwiftTextual: {
      auto *swiftDep = module.getAsSwiftTextualModule();
      if (swiftDep->swiftInterfaceFile)
        existingModulePath = *(swiftDep->swiftInterfaceFile);
      break;
    }
    case swift::ModuleDependenciesKind::SwiftBinary: {
      auto *swiftBinaryDep = module.getAsSwiftBinaryModule();
      existingModulePath = swiftBinaryDep->compiledModulePath;
      break;
    }
    case swift::ModuleDependenciesKind::Clang: {
      auto *clangDep = module.getAsClangModule();
      existingModulePath = clangDep->moduleMapFile;
      break;
    }
    case swift::ModuleDependenciesKind::SwiftPlaceholder:
    default:
      llvm_unreachable("Unhandled dependency kind.");
  }
  return existingModulePath;
}

const ModuleDependencies* GlobalModuleDependenciesCache::recordDependencies(
    StringRef moduleName,
    ModuleDependencies dependencies) {
  auto kind = dependencies.getKind();
  auto &map = getDependenciesMap(kind);
  // Cache may already have a dependency for this module
  if (map.count(moduleName) != 0) {
    // Ensure that the existing dependencies objects are at a different path.
    // i.e. do not record duplicate dependencies.
    auto newModulePath = modulePathForVerification(dependencies);
    for (auto &existingDeps : map[moduleName]) {
      if (modulePathForVerification(existingDeps) == newModulePath)
        return &existingDeps;
    }

    map[moduleName].emplace_back(std::move(dependencies));
    return map[moduleName].end()-1;
  } else {
    map.insert({moduleName, ModuleDependenciesVector{std::move(dependencies)}});
    AllModules.push_back({moduleName.str(), kind});
    return &(map[moduleName].front());
  }
}

const ModuleDependencies* GlobalModuleDependenciesCache::updateDependencies(
    ModuleDependencyID moduleID, ModuleDependencies dependencies) {
  auto &map = getDependenciesMap(moduleID.second);
  auto known = map.find(moduleID.first);
  assert(known != map.end() && "Not yet added to map");
  assert(known->second.size() == 1 &&
         "Cannot update dependency with multiple candidates.");
  known->second[0] = std::move(dependencies);
  return &(known->second[0]);
}

llvm::StringMap<const ModuleDependencies*> &
ModuleDependenciesCache::getDependencyReferencesMap(ModuleDependenciesKind kind) {
  auto it = ModuleDependenciesKindMap.find(kind);
  assert(it != ModuleDependenciesKindMap.end() && "invalid dependency kind");
  return it->second;
}

const llvm::StringMap<const ModuleDependencies*> &
ModuleDependenciesCache::getDependencyReferencesMap(ModuleDependenciesKind kind) const {
  auto it = ModuleDependenciesKindMap.find(kind);
  assert(it != ModuleDependenciesKindMap.end() && "invalid dependency kind");
  return it->second;
}

ModuleDependenciesCache::ModuleDependenciesCache(GlobalModuleDependenciesCache &globalCache)
: globalCache(globalCache) {
  for (auto kind = ModuleDependenciesKind::FirstKind;
     kind != ModuleDependenciesKind::LastKind; ++kind) {
    ModuleDependenciesKindMap.insert(
                         {kind, llvm::StringMap<const ModuleDependencies *>()});
  }
}

Optional<const ModuleDependencies*> ModuleDependenciesCache::findDependencies(
    StringRef moduleName, Optional<ModuleDependenciesKind> kind) const {
  if (!kind) {
    for (auto kind = ModuleDependenciesKind::FirstKind;
         kind != ModuleDependenciesKind::LastKind; ++kind) {
      auto dep = findDependencies(moduleName, kind);
      if (dep.hasValue())
        return dep.getValue();
    }
    return None;
  }

  const auto &map = getDependencyReferencesMap(*kind);
  auto known = map.find(moduleName);
  if (known != map.end())
    return known->second;

  return None;
}

Optional<ModuleDependencies>
ModuleDependenciesCache::findDependencies(StringRef moduleName,
                                          ModuleLookupSpecifics details) const {
  // 1. Query the local scan results
  // 2. If no module found, query the global cache using the module details
  // lookup
  auto localResult = findDependencies(moduleName, details.kind);
  if (localResult.hasValue())
    return *(localResult.getValue());
  else
    return globalCache.findDependencies(moduleName, details);
}

bool ModuleDependenciesCache::hasDependencies(
    StringRef moduleName, ModuleLookupSpecifics details) const {
  return findDependencies(moduleName, details).hasValue();
}

void ModuleDependenciesCache::recordDependencies(
    StringRef moduleName, ModuleDependencies dependencies) {
  auto globalDepPtr = globalCache.recordDependencies(moduleName, dependencies);
  auto kind = globalDepPtr->getKind();
  auto &map = getDependencyReferencesMap(kind);
  assert(map.count(moduleName) == 0 && "Already added to map");
  map.insert({moduleName, globalDepPtr});
}

void ModuleDependenciesCache::updateDependencies(
    ModuleDependencyID moduleID, ModuleDependencies dependencies) {
  auto globalDepRef = globalCache.updateDependencies(moduleID, dependencies);
  auto &map = getDependencyReferencesMap(moduleID.second);
  auto known = map.find(moduleID.first);
  if (known != map.end())
    map.erase(known);
  map.insert({moduleID.first, globalDepRef});
}
