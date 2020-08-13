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
#include "llvm/ADT/Optional.h"
#include "llvm/ADT/StringSet.h"
#include <string>
#include <vector>

namespace swift {

class ClangModuleDependenciesCacheImpl;
class SourceFile;
class ASTContext;
class Identifier;

/// Which kind of module dependencies we are looking for.
enum class ModuleDependenciesKind : int8_t {
  Swift,
  // Placeholder dependencies are a kind of dependencies used only by the
  // dependency scanner. They are swift modules that the scanner will not be
  // able to locate in its search paths and which are the responsibility of the
  // scanner's client to ensure are provided.
  //
  // Placeholder dependencies will be specified in the scanner's output
  // dependency graph where it is the responsibility of the scanner's client to
  // ensure required post-processing takes place to "resolve" them. In order to
  // do so, the client (swift driver, or any other client build system) is
  // expected to have access to a full dependency graph of all placeholder
  // dependencies and be able to replace placeholder nodes in the dependency
  // graph with their full dependency trees, `uniquing` common dependency module
  // nodes in the process.
  //
  // One example where placeholder dependencies are employed is when using
  // SwiftPM in Explicit Module Build mode. SwiftPM constructs a build plan for
  // all targets ahead-of-time. When planning a build for a target that depends
  // on other targets, the dependency scanning action is not able to locate
  // dependency target modules, because they have not yet been built. Instead,
  // the build system treats them as placeholder dependencies and resolves them
  // with `actual` dependencies in a post-processing step once dependency graphs
  // of all targets, individually, have been computed.
  SwiftPlaceholder,
  Clang,
};

/// Base class for the variant storage of ModuleDependencies.
///
/// This class is mostly an implementation detail for \c ModuleDependencies.
class ModuleDependenciesStorageBase {
public:
  const ModuleDependenciesKind dependencyKind;

  ModuleDependenciesStorageBase(ModuleDependenciesKind dependencyKind,
                                const std::string &compiledModulePath)
      : dependencyKind(dependencyKind),
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

  /// Potentially ready-to-use compiled modules for the interface file.
  const std::vector<std::string> compiledModuleCandidates;

  /// The Swift frontend invocation arguments to build the Swift module from the
  /// interface.
  const std::vector<std::string> buildCommandLine;

  /// To build a PCM to be used by this Swift module, we need to append these
  /// arguments to the generic PCM build arguments reported from the dependency
  /// graph.
  const std::vector<std::string> extraPCMArgs;

  /// The hash value that will be used for the generated module
  const std::string contextHash;

  /// A flag that indicates this dependency is a framework
  const bool isFramework;

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
      const Optional<std::string> &swiftInterfaceFile,
      ArrayRef<std::string> compiledModuleCandidates,
      ArrayRef<StringRef> buildCommandLine,
      ArrayRef<StringRef> extraPCMArgs,
      StringRef contextHash,
      bool isFramework
  ) : ModuleDependenciesStorageBase(ModuleDependenciesKind::Swift,
                                    compiledModulePath),
      swiftInterfaceFile(swiftInterfaceFile),
      compiledModuleCandidates(compiledModuleCandidates.begin(),
                               compiledModuleCandidates.end()),
      buildCommandLine(buildCommandLine.begin(), buildCommandLine.end()),
      extraPCMArgs(extraPCMArgs.begin(), extraPCMArgs.end()),
      contextHash(contextHash), isFramework(isFramework) { }

  ModuleDependenciesStorageBase *clone() const override {
    return new SwiftModuleDependenciesStorage(*this);
  }

  static bool classof(const ModuleDependenciesStorageBase *base) {
    return base->dependencyKind == ModuleDependenciesKind::Swift;
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
  ) : ModuleDependenciesStorageBase(ModuleDependenciesKind::Clang,
                                    compiledModulePath),
      moduleMapFile(moduleMapFile),
      contextHash(contextHash),
      nonPathCommandLine(nonPathCommandLine),
      fileDependencies(fileDependencies) { }

  ModuleDependenciesStorageBase *clone() const override {
    return new ClangModuleDependenciesStorage(*this);
  }

  static bool classof(const ModuleDependenciesStorageBase *base) {
    return base->dependencyKind == ModuleDependenciesKind::Clang;
  }
};

/// Describes an placeholder Swift module dependency module stub.
///
/// This class is mostly an implementation detail for \c ModuleDependencies.
class PlaceholderSwiftModuleDependencyStorage : public ModuleDependenciesStorageBase {
public:
  PlaceholderSwiftModuleDependencyStorage(const std::string &compiledModulePath,
                                       const std::string &moduleDocPath,
                                       const std::string &sourceInfoPath)
      : ModuleDependenciesStorageBase(ModuleDependenciesKind::SwiftPlaceholder,
                                      compiledModulePath),
        moduleDocPath(moduleDocPath),
        sourceInfoPath(sourceInfoPath) {}

  ModuleDependenciesStorageBase *clone() const override {
    return new PlaceholderSwiftModuleDependencyStorage(*this);
  }

  /// The path to the .swiftModuleDoc file.
  const std::string moduleDocPath;

  /// The path to the .swiftSourceInfo file.
  const std::string sourceInfoPath;

  static bool classof(const ModuleDependenciesStorageBase *base) {
    return base->dependencyKind == ModuleDependenciesKind::SwiftPlaceholder;
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
      const std::string &swiftInterfaceFile,
      ArrayRef<std::string> compiledCandidates,
      ArrayRef<StringRef> buildCommands,
      ArrayRef<StringRef> extraPCMArgs,
      StringRef contextHash,
      bool isFramework) {
    std::string compiledModulePath;
    return ModuleDependencies(
        std::make_unique<SwiftModuleDependenciesStorage>(
          compiledModulePath, swiftInterfaceFile, compiledCandidates, buildCommands,
          extraPCMArgs, contextHash, isFramework));
  }

  /// Describe the module dependencies for a serialized or parsed Swift module.
  static ModuleDependencies forSwiftModule(
      const std::string &compiledModulePath, bool isFramework) {
    return ModuleDependencies(
        std::make_unique<SwiftModuleDependenciesStorage>(
          compiledModulePath, None, ArrayRef<std::string>(), ArrayRef<StringRef>(),
          ArrayRef<StringRef>(), StringRef(), isFramework));
  }

  /// Describe the main Swift module.
  static ModuleDependencies forMainSwiftModule(ArrayRef<StringRef> extraPCMArgs) {
    std::string compiledModulePath;
    return ModuleDependencies(
        std::make_unique<SwiftModuleDependenciesStorage>(
          compiledModulePath, None, ArrayRef<std::string>(),
          ArrayRef<StringRef>(), extraPCMArgs, StringRef(), false));
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

  /// Describe a placeholder dependency swift module.
  static ModuleDependencies forPlaceholderSwiftModuleStub(
      const std::string &compiledModulePath,
      const std::string &moduleDocPath,
      const std::string &sourceInfoPath) {
    return ModuleDependencies(
        std::make_unique<PlaceholderSwiftModuleDependencyStorage>(
          compiledModulePath, moduleDocPath, sourceInfoPath));
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

  /// Whether this represents a placeholder module stub
  bool isPlaceholderSwiftModule() const;

  ModuleDependenciesKind getKind() const {
    return storage->dependencyKind;
  }
  /// Retrieve the dependencies for a Swift module.
  const SwiftModuleDependenciesStorage *getAsSwiftModule() const;

  /// Retrieve the dependencies for a Clang module.
  const ClangModuleDependenciesStorage *getAsClangModule() const;

  /// Retrieve the dependencies for a placeholder dependency module stub.
  const PlaceholderSwiftModuleDependencyStorage *
  getAsPlaceholderDependencyModule() const;

  /// Add a dependency on the given module, if it was not already in the set.
  void addModuleDependency(StringRef module,
                           llvm::StringSet<> *alreadyAddedModules = nullptr);

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

  /// Collect a map from a secondary module name to a list of cross-import
  /// overlays, when this current module serves as the primary module.
  llvm::StringMap<llvm::SmallSetVector<Identifier, 4>>
  collectCrossImportOverlayNames(ASTContext &ctx, StringRef moduleName);
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

  /// Dependencies for Swift placeholder dependency modules.
  llvm::StringMap<ModuleDependencies> PlaceholderSwiftModuleDependencies;

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
