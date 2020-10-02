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
  return isa<SwiftModuleDependenciesStorage>(storage.get());
}

bool ModuleDependencies::isPlaceholderSwiftModule() const {
  return isa<PlaceholderSwiftModuleDependencyStorage>(storage.get());
}

/// Retrieve the dependencies for a Swift module.
const SwiftModuleDependenciesStorage *
ModuleDependencies::getAsSwiftModule() const {
  return dyn_cast<SwiftModuleDependenciesStorage>(storage.get());
}

/// Retrieve the dependencies for a Clang module.
const ClangModuleDependenciesStorage *
ModuleDependencies::getAsClangModule() const {
  return dyn_cast<ClangModuleDependenciesStorage>(storage.get());
}

/// Retrieve the dependencies for a placeholder dependency module stub.
const PlaceholderSwiftModuleDependencyStorage *
ModuleDependencies::getAsPlaceholderDependencyModule() const {
  return dyn_cast<PlaceholderSwiftModuleDependencyStorage>(storage.get());
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

    addModuleDependency(importDecl->getModulePath().front().Item.str(),
                        &alreadyAddedModules);
  }

  auto fileName = sf.getFilename();
  if (fileName.empty())
    return;

  // If the storage is for an interface file, the only source file we
  // should see is that interface file.
  auto swiftStorage = cast<SwiftModuleDependenciesStorage>(storage.get());
  if (swiftStorage->swiftInterfaceFile) {
    assert(fileName == *swiftStorage->swiftInterfaceFile);
    return;
  }

  // Otherwise, record the source file.
  swiftStorage->sourceFiles.push_back(fileName.str());
}

Optional<std::string> ModuleDependencies::getBridgingHeader() const {
  auto swiftStorage = cast<SwiftModuleDependenciesStorage>(storage.get());
  return swiftStorage->bridgingHeaderFile;
}

void ModuleDependencies::addBridgingHeader(StringRef bridgingHeader) {
  auto swiftStorage = cast<SwiftModuleDependenciesStorage>(storage.get());
  assert(!swiftStorage->bridgingHeaderFile);
  swiftStorage->bridgingHeaderFile = bridgingHeader.str();
}

/// Add source files that the bridging header depends on.
void ModuleDependencies::addBridgingSourceFile(StringRef bridgingSourceFile) {
  auto swiftStorage = cast<SwiftModuleDependenciesStorage>(storage.get());
  swiftStorage->bridgingSourceFiles.push_back(bridgingSourceFile.str());
}

/// Add (Clang) module on which the bridging header depends.
void ModuleDependencies::addBridgingModuleDependency(
    StringRef module, llvm::StringSet<> &alreadyAddedModules) {
  auto swiftStorage = cast<SwiftModuleDependenciesStorage>(storage.get());
  if (alreadyAddedModules.insert(module).second)
    swiftStorage->bridgingModuleDependencies.push_back(module.str());
}

llvm::StringMap<ModuleDependencies> &
ModuleDependenciesCache::getDependenciesMap(ModuleDependenciesKind kind) {
  switch (kind) {
  case ModuleDependenciesKind::Swift:
    return SwiftModuleDependencies;
  case ModuleDependenciesKind::SwiftPlaceholder:
    return PlaceholderSwiftModuleDependencies;
  case ModuleDependenciesKind::Clang:
    return ClangModuleDependencies;
  }
  llvm_unreachable("invalid dependency kind");
}

const llvm::StringMap<ModuleDependencies> &
ModuleDependenciesCache::getDependenciesMap(ModuleDependenciesKind kind) const {
  switch (kind) {
  case ModuleDependenciesKind::Swift:
    return SwiftModuleDependencies;
  case ModuleDependenciesKind::SwiftPlaceholder:
    return PlaceholderSwiftModuleDependencies;
  case ModuleDependenciesKind::Clang:
    return ClangModuleDependencies;
  }
  llvm_unreachable("invalid dependency kind");
}

bool ModuleDependenciesCache::hasDependencies(
    StringRef moduleName,
    Optional<ModuleDependenciesKind> kind) const {
  if (!kind) {
    return hasDependencies(moduleName, ModuleDependenciesKind::Swift) ||
        hasDependencies(moduleName, ModuleDependenciesKind::SwiftPlaceholder) ||
        hasDependencies(moduleName, ModuleDependenciesKind::Clang);
  }

  const auto &map = getDependenciesMap(*kind);
  return map.count(moduleName) > 0;
}

Optional<ModuleDependencies> ModuleDependenciesCache::findDependencies(
    StringRef moduleName,
    Optional<ModuleDependenciesKind> kind) const {
  if (!kind) {
    if (auto swiftDep = findDependencies(
            moduleName, ModuleDependenciesKind::Swift))
      return swiftDep;
    else if (auto swiftPlaceholderDep = findDependencies(
            moduleName, ModuleDependenciesKind::SwiftPlaceholder))
      return swiftPlaceholderDep;
    else
      return findDependencies(moduleName, ModuleDependenciesKind::Clang);
  }

  const auto &map = getDependenciesMap(*kind);
  auto known = map.find(moduleName);
  if (known != map.end())
    return known->second;

  return None;
}

void ModuleDependenciesCache::recordDependencies(
    StringRef moduleName,
    ModuleDependencies dependencies,
    ModuleDependenciesKind kind) {
  auto &map = getDependenciesMap(kind);
  assert(map.count(moduleName) == 0 && "Already added to map");
  map.insert({moduleName, std::move(dependencies)});

  AllModules.push_back({moduleName.str(), kind});
}

void ModuleDependenciesCache::updateDependencies(
    ModuleDependencyID moduleID, ModuleDependencies dependencies) {
  auto &map = getDependenciesMap(moduleID.second);
  auto known = map.find(moduleID.first);
  assert(known != map.end() && "Not yet added to map");
  known->second = std::move(dependencies);
}
