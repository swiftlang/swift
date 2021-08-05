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

llvm::StringMap<ModuleDependenciesCache::ModuleDependenciesVector> &
ModuleDependenciesCache::getDependenciesMap(ModuleDependenciesKind kind) {
  switch (kind) {
  case ModuleDependenciesKind::SwiftTextual:
    return SwiftTextualModuleDependencies;
  case ModuleDependenciesKind::SwiftBinary:
    return SwiftBinaryModuleDependencies;
  case ModuleDependenciesKind::SwiftPlaceholder:
    return SwiftPlaceholderModuleDependencies;
  case ModuleDependenciesKind::Clang:
    return ClangModuleDependencies;
  }
  llvm_unreachable("invalid dependency kind");
}

const llvm::StringMap<ModuleDependenciesCache::ModuleDependenciesVector> &
ModuleDependenciesCache::getDependenciesMap(ModuleDependenciesKind kind) const {
  switch (kind) {
  case ModuleDependenciesKind::SwiftTextual:
    return SwiftTextualModuleDependencies;
  case ModuleDependenciesKind::SwiftBinary:
    return SwiftBinaryModuleDependencies;
  case ModuleDependenciesKind::SwiftPlaceholder:
    return SwiftPlaceholderModuleDependencies;
  case ModuleDependenciesKind::Clang:
    return ClangModuleDependencies;
  }
  llvm_unreachable("invalid dependency kind");
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
  if (auto *swiftDep = module.getAsSwiftTextualModule()) {
    if (swiftDep->swiftInterfaceFile)
      modulePath = *(swiftDep->swiftInterfaceFile);
    else {
      // If we encountered a Swift textual dependency without an interface
      // file, we are seeing the main scan module itself. This means that
      // our search-path disambiguation is not necessary here.
      return true;
    }
  } else if (auto *swiftBinaryDep = module.getAsSwiftBinaryModule()) {
    modulePath = swiftBinaryDep->compiledModulePath;
  } else if (auto *placehodlerDep = module.getAsPlaceholderDependencyModule()) {
    // Placeholders are resolved as `true` because they are not associated with
    // any specific search path.
    return true;
  } else if (auto *clangDep = module.getAsClangModule()) {
    modulePath = clangDep->moduleMapFile;
  }

  if (moduleContainedInImportPathSet(modulePath, importPaths)) {
    return true;
  }
  return false;
}

bool ModuleDependenciesCache::hasDependencies(
    StringRef moduleName,
    ModuleLookupSpecifics details) const {
  if (!details.kind) {
    return hasDependencies(moduleName,
                           {ModuleDependenciesKind::SwiftTextual,
                            details.currentSearchPaths}) ||
        hasDependencies(moduleName,
                        {ModuleDependenciesKind::SwiftBinary,
                         details.currentSearchPaths}) ||
        hasDependencies(moduleName,
                        {ModuleDependenciesKind::SwiftPlaceholder,
                         details.currentSearchPaths}) ||
        hasDependencies(moduleName,
                        {ModuleDependenciesKind::Clang,
                         details.currentSearchPaths});
  }

  const auto &map = getDependenciesMap(*details.kind);
  auto known = map.find(moduleName);
  if (known != map.end()) {
    assert(!known->second.empty());
    for (auto &dep : known->second) {
      if (moduleContainedInImportPathSet(dep, details.currentSearchPaths))
        return true;
    }
    return false;
  }
  return false;
}

Optional<ModuleDependencies> ModuleDependenciesCache::findDependencies(
    StringRef moduleName,
    ModuleLookupSpecifics details) const {
  if (!details.kind) {
    if (auto swiftTextualDep = findDependencies(
            moduleName,
            {ModuleDependenciesKind::SwiftTextual, details.currentSearchPaths}))
      return swiftTextualDep;
    else if (auto swiftBinaryDep = findDependencies(
            moduleName,
            {ModuleDependenciesKind::SwiftBinary, details.currentSearchPaths}))
      return swiftBinaryDep;
    else if (auto swiftPlaceholderDep = findDependencies(
            moduleName,
            {ModuleDependenciesKind::SwiftPlaceholder, details.currentSearchPaths}))
      return swiftPlaceholderDep;
    else
      return findDependencies(moduleName,
                              {ModuleDependenciesKind::Clang, details.currentSearchPaths});
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

Optional<ModuleDependenciesCache::ModuleDependenciesVector>
ModuleDependenciesCache::findAllDependenciesIrrespectiveOfSearchPaths(
    StringRef moduleName, Optional<ModuleDependenciesKind> kind) const {
  if (!kind) {
    if (auto swiftTextualDeps = findAllDependenciesIrrespectiveOfSearchPaths(
            moduleName, ModuleDependenciesKind::SwiftTextual))
      return swiftTextualDeps;
    else if (auto swiftBinaryDeps =
                 findAllDependenciesIrrespectiveOfSearchPaths(
                     moduleName, ModuleDependenciesKind::SwiftBinary))
      return swiftBinaryDeps;
    else if (auto swiftPlaceholderDeps =
                 findAllDependenciesIrrespectiveOfSearchPaths(
                     moduleName, ModuleDependenciesKind::SwiftPlaceholder))
      return swiftPlaceholderDeps;
    else
      return findAllDependenciesIrrespectiveOfSearchPaths(
          moduleName, ModuleDependenciesKind::Clang);
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
  if (auto *swiftDep = module.getAsSwiftTextualModule()) {
    if (swiftDep->swiftInterfaceFile)
      existingModulePath = *(swiftDep->swiftInterfaceFile);
  } else if (auto *swiftBinaryDep = module.getAsSwiftBinaryModule()) {
    existingModulePath = swiftBinaryDep->compiledModulePath;
  } else if (auto *clangDep = module.getAsClangModule()) {
    existingModulePath = clangDep->moduleMapFile;
  }
  return existingModulePath;
}

void ModuleDependenciesCache::recordDependencies(
    StringRef moduleName,
    ModuleDependencies dependencies) {
  auto kind = dependencies.getKind();
  auto &map = getDependenciesMap(kind);
  // Cache may already have a dependency for this module
  if (map.count(moduleName) != 0) {
    // Ensure that the existing dependencies objects are at a different path.
    // i.e. do not record duplicate dependencies.
    auto newModulePath = modulePathForVerification(dependencies);
    bool newEntry = true;
    for (auto &existingDeps : map[moduleName]) {
      if (modulePathForVerification(existingDeps) == newModulePath)
        newEntry = false;
    }
    if (newEntry)
      map[moduleName].emplace_back(std::move(dependencies));
  } else {
    map.insert({moduleName, ModuleDependenciesVector{std::move(dependencies)}});
    AllModules.push_back({moduleName.str(), kind});
  }
}

void ModuleDependenciesCache::updateDependencies(
    ModuleDependencyID moduleID, ModuleDependencies dependencies) {
  auto &map = getDependenciesMap(moduleID.second);
  auto known = map.find(moduleID.first);
  assert(known != map.end() && "Not yet added to map");
  assert(known->second.size() == 1 && "Cannot update dependency with multiple candidates.");
  known->second[0] = std::move(dependencies);
}
