//===--- ModuleDependencies.h - Module Dependencies -------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines data structures for capturing all of the source files
// and modules on which a given module depends, forming a graph of all of the
// modules that need to be present for a given module to be built.
//
//===----------------------------------------------------------------------===//
#ifndef SWIFT_AST_MODULE_DEPENDENCIES_H
#define SWIFT_AST_MODULE_DEPENDENCIES_H

#include "swift/Basic/LLVM.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/Optional.h"
#include "llvm/ADT/StringSet.h"
#include <string>
#include <vector>

namespace swift {

class ClangModuleDependenciesCacheImpl;
class SourceFile;

/// Which kind of module dependencies we are looking for.
enum class ModuleDependenciesKind : int8_t {
  Swift,
  Clang,
};

/// Base class for the variant storage of ModuleDependencies.
///
/// This class is mostly an implementation detail for \c ModuleDependencies.
class ModuleDependenciesStorageBase {
public:
  const bool isSwiftModule;

  ModuleDependenciesStorageBase(bool isSwiftModule,
                                const std::string &compiledModulePath)
      : isSwiftModule(isSwiftModule),
        compiledModulePath(compiledModulePath) { }

  virtual ModuleDependenciesStorageBase *clone() const = 0;

  virtual ~ModuleDependenciesStorageBase();

  /// The path to the compiled module file.
  const std::string compiledModulePath;

  /// The set of modules on which this module depends.
  std::vector<std::string> moduleDependencies;
};

/// Describes the dependencies of a Swift module.
///
/// This class is mostly an implementation detail for \c ModuleDependencies.
class SwiftModuleDependenciesStorage : public ModuleDependenciesStorageBase {
public:
  /// The Swift interface file, if it can be used to generate the module file.
  const Optional<std::string> swiftInterfaceFile;

  /// Bridging header file, if there is one.
  Optional<std::string> bridgingHeaderFile;

  /// Swift source files that are part of the Swift module, when known.
  std::vector<std::string> sourceFiles;

  /// Source files on which the bridging header depends.
  std::vector<std::string> bridgingSourceFiles;

  /// (Clang) modules on which the bridging header depends.
  std::vector<std::string> bridgingModuleDependencies;

  SwiftModuleDependenciesStorage(
      const std::string &compiledModulePath,
      const Optional<std::string> &swiftInterfaceFile
  ) : ModuleDependenciesStorageBase(/*isSwiftModule=*/true, compiledModulePath),
      swiftInterfaceFile(swiftInterfaceFile) { }

  ModuleDependenciesStorageBase *clone() const override {
    return new SwiftModuleDependenciesStorage(*this);
  }

  static bool classof(const ModuleDependenciesStorageBase *base) {
    return base->isSwiftModule;
  }
};

/// Describes the dependencies of a Clang module.
///
/// This class is mostly an implementation detail for \c ModuleDependencies.
class ClangModuleDependenciesStorage : public ModuleDependenciesStorageBase {
public:
  /// The module map file used to generate the Clang module.
  const std::string moduleMapFile;

  /// The context hash describing the configuration options for this module.
  const std::string contextHash;

  /// Partial (Clang) command line that can be used to build this module.
  const std::vector<std::string> nonPathCommandLine;

  /// The file dependencies
  const std::vector<std::string> fileDependencies;

  ClangModuleDependenciesStorage(
      const std::string &compiledModulePath,
      const std::string &moduleMapFile,
      const std::string &contextHash,
      const std::vector<std::string> &nonPathCommandLine,
      const std::vector<std::string> &fileDependencies
  ) : ModuleDependenciesStorageBase(/*isSwiftModule=*/false,
                                    compiledModulePath),
      moduleMapFile(moduleMapFile),
      contextHash(contextHash),
      nonPathCommandLine(nonPathCommandLine),
      fileDependencies(fileDependencies) { }

  ModuleDependenciesStorageBase *clone() const override {
    return new ClangModuleDependenciesStorage(*this);
  }

  static bool classof(const ModuleDependenciesStorageBase *base) {
    return !base->isSwiftModule;
  }
};

/// Describes the dependencies of a given module.
///
/// The dependencies of a module include all of the source files that go
/// into that module, as well as any modules that are directly imported
/// into the module.
class ModuleDependencies {
private:
  std::unique_ptr<ModuleDependenciesStorageBase> storage;

  ModuleDependencies(std::unique_ptr<ModuleDependenciesStorageBase> &&storage)
    : storage(std::move(storage)) { }

public:
  ModuleDependencies(const ModuleDependencies &other)
    : storage(other.storage->clone()) { }
  ModuleDependencies(ModuleDependencies &&other) = default;

  ModuleDependencies &operator=(const ModuleDependencies &other) {
    storage.reset(other.storage->clone());
    return *this;
  }

  ModuleDependencies &operator=(ModuleDependencies &&other) = default;

  /// Describe the module dependencies for a Swift module that can be
  /// built from a Swift interface file (\c .swiftinterface).
  static ModuleDependencies forSwiftInterface(
      const std::string &compiledModulePath,
      const std::string &swiftInterfaceFile) {
    return ModuleDependencies(
        std::make_unique<SwiftModuleDependenciesStorage>(
          compiledModulePath, swiftInterfaceFile));
  }

  /// Describe the module dependencies for a serialized or parsed Swift module.
  static ModuleDependencies forSwiftModule(
      const std::string &compiledModulePath) {
    return ModuleDependencies(
        std::make_unique<SwiftModuleDependenciesStorage>(
          compiledModulePath, None));
  }

  /// Describe the module dependencies for a Clang module that can be
  /// built from a module map and headers.
  static ModuleDependencies forClangModule(
      const std::string &compiledModulePath,
      const std::string &moduleMapFile,
      const std::string &contextHash,
      const std::vector<std::string> &nonPathCommandLine,
      const std::vector<std::string> &fileDependencies) {
    return ModuleDependencies(
        std::make_unique<ClangModuleDependenciesStorage>(
          compiledModulePath, moduleMapFile, contextHash, nonPathCommandLine,
          fileDependencies));
  }

  /// Retrieve the path to the compiled module.
  const std::string getCompiledModulePath() const {
    return storage->compiledModulePath;
  }

  /// Retrieve the module-level dependencies.
  ArrayRef<std::string> getModuleDependencies() const {
    return storage->moduleDependencies;
  }

  /// Whether the dependencies are for a Swift module.
  bool isSwiftModule() const;

  ModuleDependenciesKind getKind() const {
    return isSwiftModule() ? ModuleDependenciesKind::Swift
                           : ModuleDependenciesKind::Clang;
  }
  /// Retrieve the dependencies for a Swift module.
  const SwiftModuleDependenciesStorage *getAsSwiftModule() const;

  /// Retrieve the dependencies for a Clang module.
  const ClangModuleDependenciesStorage *getAsClangModule() const;

  /// Add a dependency on the given module, if it was not already in the set.
  void addModuleDependency(StringRef module,
                           llvm::StringSet<> &alreadyAddedModules);

  /// Add all of the module dependencies for the imports in the given source
  /// file to the set of module dependencies.
  void addModuleDependencies(const SourceFile &sf,
                             llvm::StringSet<> &alreadyAddedModules);

  /// Get the bridging header.
  Optional<std::string> getBridgingHeader() const;

  /// Add a bridging header to a Swift module's dependencies.
  void addBridgingHeader(StringRef bridgingHeader);

  /// Add source files that the bridging header depends on.
  void addBridgingSourceFile(StringRef bridgingSourceFile);

  /// Add (Clang) module on which the bridging header depends.
  void addBridgingModuleDependency(StringRef module,
                                   llvm::StringSet<> &alreadyAddedModules);
};

using ModuleDependencyID = std::pair<std::string, ModuleDependenciesKind>;

/// A cache describing the set of module dependencies that has been queried
/// thus far.
class ModuleDependenciesCache {
  /// All cached module dependencies, in the order in which they were
  /// encountered.
  std::vector<ModuleDependencyID> AllModules;

  /// Dependencies for Swift modules that have already been computed.
  llvm::StringMap<ModuleDependencies> SwiftModuleDependencies;

  /// Dependencies for Clang modules that have already been computed.
  llvm::StringMap<ModuleDependencies> ClangModuleDependencies;

  /// Additional information needed for Clang dependency scanning.
  ClangModuleDependenciesCacheImpl *clangImpl = nullptr;

  /// Function that will delete \c clangImpl properly.
  void (*clangImplDeleter)(ClangModuleDependenciesCacheImpl *) = nullptr;

  /// Free up the storage associated with the Clang implementation.
  void destroyClangImpl() {
    if (this->clangImplDeleter)
      this->clangImplDeleter(this->clangImpl);
  }

  /// Retrieve the dependencies map that corresponds to the given dependency
  /// kind.
  llvm::StringMap<ModuleDependencies> &getDependenciesMap(
      ModuleDependenciesKind kind);
  const llvm::StringMap<ModuleDependencies> &getDependenciesMap(
      ModuleDependenciesKind kind) const;

public:
  ModuleDependenciesCache() { }

  ModuleDependenciesCache(const ModuleDependenciesCache &) = delete;
  ModuleDependenciesCache &operator=(const ModuleDependenciesCache &) = delete;

  ~ModuleDependenciesCache() {
    destroyClangImpl();
  }

  /// Set the Clang-specific implementation data.
  void setClangImpl(
      ClangModuleDependenciesCacheImpl *clangImpl,
      void (*clangImplDeleter)(ClangModuleDependenciesCacheImpl *)) {
    destroyClangImpl();

    this->clangImpl = clangImpl;
    this->clangImplDeleter = clangImplDeleter;
  }

  /// Retrieve the Clang-specific implementation data;
  ClangModuleDependenciesCacheImpl *getClangImpl() const {
    return clangImpl;
  }

  /// Whether we have cached dependency information for the given module.
  bool hasDependencies(StringRef moduleName,
                       Optional<ModuleDependenciesKind> kind) const;

  /// Look for module dependencies for a module with the given name.
  ///
  /// \returns the cached result, or \c None if there is no cached entry.
  Optional<ModuleDependencies> findDependencies(
      StringRef moduleName,
      Optional<ModuleDependenciesKind> kind) const;

  /// Record dependencies for the given module.
  void recordDependencies(StringRef moduleName,
                          ModuleDependencies dependencies,
                          ModuleDependenciesKind kind);

  /// Update stored dependencies for the given module.
  void updateDependencies(ModuleDependencyID moduleID,
                          ModuleDependencies dependencies);

  /// Reference the list of all module dependencies.
  const std::vector<ModuleDependencyID> &getAllModules() const {
    return AllModules;
  }
};

}

#endif /* SWIFT_AST_MODULE_DEPENDENCIES_H */
