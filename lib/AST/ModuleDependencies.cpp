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

ModuleDependenciesStorageBase::~ModuleDependenciesStorageBase() {}

bool ModuleDependencies::isSwiftModule() const {
  return isSwiftInterfaceModule() || isSwiftSourceModule() ||
         isSwiftBinaryModule() || isSwiftPlaceholderModule();
}

ModuleDependenciesKind &operator++(ModuleDependenciesKind &e) {
  if (e == ModuleDependenciesKind::LastKind) {
    llvm_unreachable(
        "Attempting to increment last enum value on ModuleDependenciesKind");
  }
  e = ModuleDependenciesKind(
      static_cast<std::underlying_type<ModuleDependenciesKind>::type>(e) + 1);
  return e;
}

bool ModuleDependencies::isSwiftInterfaceModule() const {
  return isa<SwiftInterfaceModuleDependenciesStorage>(storage.get());
}

bool ModuleDependencies::isSwiftSourceModule() const {
  return isa<SwiftSourceModuleDependenciesStorage>(storage.get());
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

/// Retrieve the dependencies for a Swift textual interface module.
const SwiftInterfaceModuleDependenciesStorage *
ModuleDependencies::getAsSwiftInterfaceModule() const {
  return dyn_cast<SwiftInterfaceModuleDependenciesStorage>(storage.get());
}

const SwiftSourceModuleDependenciesStorage *
ModuleDependencies::getAsSwiftSourceModule() const {
  return dyn_cast<SwiftSourceModuleDependenciesStorage>(storage.get());
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

    ImportPath::Builder scratch;
    auto realPath = importDecl->getRealModulePath(scratch);
    addModuleDependency(realPath, &alreadyAddedModules);
  }

  auto fileName = sf.getFilename();
  if (fileName.empty())
    return;

  switch (getKind()) {
  case swift::ModuleDependenciesKind::SwiftInterface: {
    // If the storage is for an interface file, the only source file we
    // should see is that interface file.
    auto swiftInterfaceStorage =
        cast<SwiftInterfaceModuleDependenciesStorage>(storage.get());
    assert(fileName == swiftInterfaceStorage->swiftInterfaceFile);
    break;
  }
  case swift::ModuleDependenciesKind::SwiftSource: {
    // Otherwise, record the source file.
    auto swiftSourceStorage =
        cast<SwiftSourceModuleDependenciesStorage>(storage.get());
    swiftSourceStorage->sourceFiles.push_back(fileName.str());
    break;
  }
  default:
    llvm_unreachable("Unexpected dependency kind");
  }
}

Optional<std::string> ModuleDependencies::getBridgingHeader() const {
  switch (getKind()) {
  case swift::ModuleDependenciesKind::SwiftInterface: {
    auto swiftInterfaceStorage =
        cast<SwiftInterfaceModuleDependenciesStorage>(storage.get());
    return swiftInterfaceStorage->textualModuleDetails.bridgingHeaderFile;
  }
  case swift::ModuleDependenciesKind::SwiftSource: {
    auto swiftSourceStorage =
        cast<SwiftSourceModuleDependenciesStorage>(storage.get());
    return swiftSourceStorage->textualModuleDetails.bridgingHeaderFile;
  }
  default:
    llvm_unreachable("Unexpected dependency kind");
  }
}

void ModuleDependencies::addBridgingHeader(StringRef bridgingHeader) {
  switch (getKind()) {
  case swift::ModuleDependenciesKind::SwiftInterface: {
    auto swiftInterfaceStorage =
        cast<SwiftInterfaceModuleDependenciesStorage>(storage.get());
    assert(!swiftInterfaceStorage->textualModuleDetails.bridgingHeaderFile);
    swiftInterfaceStorage->textualModuleDetails.bridgingHeaderFile = bridgingHeader.str();
    break;
  }
  case swift::ModuleDependenciesKind::SwiftSource: {
    auto swiftSourceStorage =
        cast<SwiftSourceModuleDependenciesStorage>(storage.get());
    assert(!swiftSourceStorage->textualModuleDetails.bridgingHeaderFile);
    swiftSourceStorage->textualModuleDetails.bridgingHeaderFile = bridgingHeader.str();
    break;
  }
  default:
    llvm_unreachable("Unexpected dependency kind");
  }
}

/// Add source files that the bridging header depends on.
void ModuleDependencies::addBridgingSourceFile(StringRef bridgingSourceFile) {
  switch (getKind()) {
  case swift::ModuleDependenciesKind::SwiftInterface: {
    auto swiftInterfaceStorage =
        cast<SwiftInterfaceModuleDependenciesStorage>(storage.get());
    swiftInterfaceStorage->textualModuleDetails.bridgingSourceFiles.push_back(
        bridgingSourceFile.str());
    break;
  }
  case swift::ModuleDependenciesKind::SwiftSource: {
    auto swiftSourceStorage =
        cast<SwiftSourceModuleDependenciesStorage>(storage.get());
    swiftSourceStorage->textualModuleDetails.bridgingSourceFiles.push_back(bridgingSourceFile.str());
    break;
  }
  default:
    llvm_unreachable("Unexpected dependency kind");
  }
}

void ModuleDependencies::addSourceFile(StringRef sourceFile) {
  switch (getKind()) {
  case swift::ModuleDependenciesKind::SwiftSource: {
    auto swiftSourceStorage =
        cast<SwiftSourceModuleDependenciesStorage>(storage.get());
    swiftSourceStorage->sourceFiles.push_back(sourceFile.str());
    break;
  }
  default:
    llvm_unreachable("Unexpected dependency kind");
  }
}

/// Add (Clang) module on which the bridging header depends.
void ModuleDependencies::addBridgingModuleDependency(
    StringRef module, llvm::StringSet<> &alreadyAddedModules) {
  switch (getKind()) {
  case swift::ModuleDependenciesKind::SwiftInterface: {
    auto swiftInterfaceStorage =
        cast<SwiftInterfaceModuleDependenciesStorage>(storage.get());
    if (alreadyAddedModules.insert(module).second)
      swiftInterfaceStorage->textualModuleDetails.bridgingModuleDependencies.push_back(module.str());
    break;
  }
  case swift::ModuleDependenciesKind::SwiftSource: {
    auto swiftSourceStorage =
        cast<SwiftSourceModuleDependenciesStorage>(storage.get());
    if (alreadyAddedModules.insert(module).second)
      swiftSourceStorage->textualModuleDetails.bridgingModuleDependencies.push_back(module.str());
    break;
  }
  default:
    llvm_unreachable("Unexpected dependency kind");
  }
}

GlobalModuleDependenciesCache::TargetSpecificGlobalCacheState *
GlobalModuleDependenciesCache::getCurrentCache() const {
  assert(CurrentTriple.hasValue() &&
         "Global Module Dependencies Cache not configured with Triple.");
  return getCacheForTriple(CurrentTriple.getValue());
}

GlobalModuleDependenciesCache::TargetSpecificGlobalCacheState *
GlobalModuleDependenciesCache::getCacheForTriple(StringRef triple) const {
  auto targetSpecificCache = TargetSpecificCacheMap.find(triple);
  assert(targetSpecificCache != TargetSpecificCacheMap.end() &&
         "Global Module Dependencies Cache not configured with Triple-specific "
         "state.");
  return targetSpecificCache->getValue().get();
}

llvm::StringMap<ModuleDependenciesVector> &
GlobalModuleDependenciesCache::getDependenciesMap(ModuleDependenciesKind kind) {
  auto targetSpecificCache = getCurrentCache();
  auto it = targetSpecificCache->ModuleDependenciesMap.find(kind);
  assert(it != targetSpecificCache->ModuleDependenciesMap.end() &&
         "invalid dependency kind");
  return it->second;
}

const llvm::StringMap<ModuleDependenciesVector> &
GlobalModuleDependenciesCache::getDependenciesMap(
    ModuleDependenciesKind kind) const {
  auto targetSpecificCache = getCurrentCache();
  auto it = targetSpecificCache->ModuleDependenciesMap.find(kind);
  assert(it != targetSpecificCache->ModuleDependenciesMap.end() &&
         "invalid dependency kind");
  return it->second;
}

static std::string moduleBasePath(const StringRef modulePath) {
  auto parent = llvm::sys::path::parent_path(modulePath);
  // If the modulePath is that of a .swiftinterface contained in a .swiftmodule,
  // disambiguate further to parent.
  if (llvm::sys::path::extension(parent) == ".swiftmodule") {
    parent = llvm::sys::path::parent_path(parent);
  }

  // If the module is a part of a framework, disambiguate to the framework's
  // parent
  if (llvm::sys::path::filename(parent) == "Modules") {
    auto grandParent = llvm::sys::path::parent_path(parent);
    if (llvm::sys::path::extension(grandParent) == ".framework") {
      parent = llvm::sys::path::parent_path(grandParent);
    }
  }

  return parent.str();
}

static bool
moduleContainedInImportPathSet(const StringRef modulePath,
                               const llvm::StringSet<> &importPaths) {
  return importPaths.contains(moduleBasePath(modulePath));
}

static bool
moduleContainedInImportPathSet(const ModuleDependencies &module,
                               const llvm::StringSet<> &importPaths) {
  std::string modulePath = "";
  switch (module.getKind()) {
  case swift::ModuleDependenciesKind::SwiftInterface: {
    modulePath = module.getAsSwiftInterfaceModule()->swiftInterfaceFile;
    break;
  }
  case swift::ModuleDependenciesKind::SwiftSource:
    // We are seeing the main scan module itself. This means that
    // our search-path disambiguation is not necessary here.
    return true;
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

void GlobalModuleDependenciesCache::configureForTriple(std::string triple) {
  auto knownTriple = TargetSpecificCacheMap.find(triple);
  if (knownTriple != TargetSpecificCacheMap.end()) {
    // Set the current triple and leave the rest as-is
    CurrentTriple = triple;
  } else {
    // First time scanning with this triple, initialize target-specific state.
    std::unique_ptr<TargetSpecificGlobalCacheState> targetSpecificCache =
        std::make_unique<TargetSpecificGlobalCacheState>();
    for (auto kind = ModuleDependenciesKind::FirstKind;
         kind != ModuleDependenciesKind::LastKind; ++kind) {
      targetSpecificCache->ModuleDependenciesMap.insert(
          {kind, llvm::StringMap<ModuleDependenciesVector>()});
    }

    TargetSpecificCacheMap.insert({triple, std::move(targetSpecificCache)});
    CurrentTriple = triple;
    AllTriples.push_back(triple);
  }
}

Optional<ModuleDependencies> GlobalModuleDependenciesCache::findDependencies(
    StringRef moduleName, ModuleLookupSpecifics details) const {
  if (!details.kind) {
    for (auto kind = ModuleDependenciesKind::FirstKind;
         kind != ModuleDependenciesKind::LastKind; ++kind) {
      auto dep =
          findDependencies(moduleName, {kind, details.currentSearchPaths});
      if (dep.hasValue())
        return dep.getValue();
    }
    return None;
  }

  assert(details.kind.hasValue() && "Expected dependencies kind for lookup.");
  if (details.kind.getValue() == swift::ModuleDependenciesKind::SwiftSource) {
    return findSourceModuleDependency(moduleName);
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

Optional<ModuleDependencies>
GlobalModuleDependenciesCache::findSourceModuleDependency(
    StringRef moduleName) const {
  auto known = SwiftSourceModuleDependenciesMap.find(moduleName);
  if (known != SwiftSourceModuleDependenciesMap.end())
    return known->second;
  else
    return None;
}

bool GlobalModuleDependenciesCache::hasDependencies(
    StringRef moduleName, ModuleLookupSpecifics details) const {
  return findDependencies(moduleName, details).hasValue();
}

Optional<ModuleDependenciesVector>
GlobalModuleDependenciesCache::findAllDependenciesIrrespectiveOfSearchPaths(
    StringRef moduleName, Optional<ModuleDependenciesKind> kind) const {
  if (!kind) {
    for (auto kind = ModuleDependenciesKind::FirstKind;
         kind != ModuleDependenciesKind::LastKind; ++kind) {
      auto deps =
          findAllDependenciesIrrespectiveOfSearchPaths(moduleName, kind);
      if (deps.hasValue())
        return deps.getValue();
    }
    return None;
  }

  assert(kind.hasValue() && "Expected dependencies kind for lookup.");
  assert(kind.getValue() != swift::ModuleDependenciesKind::SwiftSource);

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
  case swift::ModuleDependenciesKind::SwiftInterface: {
    auto *swiftDep = module.getAsSwiftInterfaceModule();
    existingModulePath = swiftDep->swiftInterfaceFile;
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
  case swift::ModuleDependenciesKind::SwiftSource:
  case swift::ModuleDependenciesKind::SwiftPlaceholder:
  case swift::ModuleDependenciesKind::LastKind:
    llvm_unreachable("Unhandled dependency kind.");
  }
  return existingModulePath;
}

const ModuleDependencies *GlobalModuleDependenciesCache::recordDependencies(
    StringRef moduleName, ModuleDependencies dependencies) {
  auto kind = dependencies.getKind();
  // Source-based dependencies are recorded independently of the invocation's
  // target triple.
  if (kind == swift::ModuleDependenciesKind::SwiftSource) {
    assert(SwiftSourceModuleDependenciesMap.count(moduleName) == 0 &&
           "Attempting to record duplicate SwiftSource dependency.");
    SwiftSourceModuleDependenciesMap.insert(
        {moduleName, std::move(dependencies)});
    AllSourceModules.push_back({moduleName.str(), kind});
    return &(SwiftSourceModuleDependenciesMap.find(moduleName)->second);
  }

  // All other dependencies are recorded according to the target triple of the
  // scanning invocation that discovers them.
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
    return map[moduleName].end() - 1;
  } else {
    map.insert({moduleName, ModuleDependenciesVector{std::move(dependencies)}});
    getCurrentCache()->AllModules.push_back({moduleName.str(), kind});
    return &(map[moduleName].front());
  }
}

const ModuleDependencies *GlobalModuleDependenciesCache::updateDependencies(
    ModuleDependencyID moduleID, ModuleDependencies dependencies) {
  auto kind = dependencies.getKind();
  // Source-based dependencies
  if (kind == swift::ModuleDependenciesKind::SwiftSource) {
    assert(SwiftSourceModuleDependenciesMap.count(moduleID.first) == 1 &&
           "Attempting to update non-existing Swift Source dependency.");
    auto known = SwiftSourceModuleDependenciesMap.find(moduleID.first);
    known->second = std::move(dependencies);
    return &(known->second);
  }

  auto &map = getDependenciesMap(moduleID.second);
  auto known = map.find(moduleID.first);
  assert(known != map.end() && "Not yet added to map");
  assert(known->second.size() == 1 &&
         "Cannot update dependency with multiple candidates.");
  known->second[0] = std::move(dependencies);
  return &(known->second[0]);
}

llvm::StringMap<const ModuleDependencies *> &
ModuleDependenciesCache::getDependencyReferencesMap(
    ModuleDependenciesKind kind) {
  auto it = ModuleDependenciesMap.find(kind);
  assert(it != ModuleDependenciesMap.end() && "invalid dependency kind");
  return it->second;
}

const llvm::StringMap<const ModuleDependencies *> &
ModuleDependenciesCache::getDependencyReferencesMap(
    ModuleDependenciesKind kind) const {
  auto it = ModuleDependenciesMap.find(kind);
  assert(it != ModuleDependenciesMap.end() && "invalid dependency kind");
  return it->second;
}

ModuleDependenciesCache::ModuleDependenciesCache(
    GlobalModuleDependenciesCache &globalCache,
    StringRef mainModuleName)
    : globalCache(globalCache), mainModuleName(mainModuleName) {
  for (auto kind = ModuleDependenciesKind::FirstKind;
       kind != ModuleDependenciesKind::LastKind; ++kind) {
    ModuleDependenciesMap.insert(
        {kind, llvm::StringMap<const ModuleDependencies *>()});
  }
}

Optional<const ModuleDependencies *> ModuleDependenciesCache::findDependencies(
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
  auto dependenciesKind = dependencies.getKind();

  // The underlying Clang module needs to be cached in this invocation,
  // but should not make it to the global cache since it will look slightly
  // differently for clients of this module than it does for the module itself.
  const ModuleDependencies* recordedDependencies;
  if (moduleName == mainModuleName && dependencies.isClangModule()) {
    underlyingClangModuleDependency = std::make_unique<ModuleDependencies>(std::move(dependencies));
    recordedDependencies = underlyingClangModuleDependency.get();
  } else
    recordedDependencies = globalCache.recordDependencies(moduleName, dependencies);

  auto &map = getDependencyReferencesMap(dependenciesKind);
  assert(map.count(moduleName) == 0 && "Already added to map");
  map.insert({moduleName, recordedDependencies});
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
