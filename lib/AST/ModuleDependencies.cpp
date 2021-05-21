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

llvm::StringMap<ModuleDependencies> &
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

const llvm::StringMap<ModuleDependencies> &
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

bool ModuleDependenciesCache::hasDependencies(
    StringRef moduleName,
    Optional<ModuleDependenciesKind> kind) const {
  if (!kind) {
    return hasDependencies(moduleName, ModuleDependenciesKind::SwiftTextual) ||
        hasDependencies(moduleName, ModuleDependenciesKind::SwiftBinary) ||
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
    if (auto swiftTextualDep = findDependencies(
            moduleName, ModuleDependenciesKind::SwiftTextual))
      return swiftTextualDep;
    else if (auto swiftBinaryDep = findDependencies(
            moduleName, ModuleDependenciesKind::SwiftBinary))
      return swiftBinaryDep;
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
    ModuleDependencies dependencies) {
  auto kind = dependencies.getKind();
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
