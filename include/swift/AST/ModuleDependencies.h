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
#include "swift/AST/Import.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/Optional.h"
#include "llvm/ADT/StringSet.h"
#include <string>
#include <vector>
#include <unordered_map>

namespace swift {

class ClangModuleDependenciesCacheImpl;
class SourceFile;
class ASTContext;
class Identifier;

/// Which kind of module dependencies we are looking for.
enum class ModuleDependenciesKind : int8_t {
  FirstKind,
  SwiftInterface = FirstKind,
  SwiftBinary,
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
  SwiftSource,
  LastKind = SwiftSource + 1
};

struct ModuleDependenciesKindHash {
  std::size_t operator()(ModuleDependenciesKind k) const {
    using UnderlyingType = std::underlying_type<ModuleDependenciesKind>::type;
    return std::hash<UnderlyingType>{}(static_cast<UnderlyingType>(k));
  }
};

/// Details of a given module used for dependency scanner cache queries.
struct ModuleLookupSpecifics {
  Optional<ModuleDependenciesKind> kind;
  llvm::StringSet<> currentSearchPaths;
};

/// Base class for the variant storage of ModuleDependencies.
///
/// This class is mostly an implementation detail for \c ModuleDependencies.
class ModuleDependenciesStorageBase {
public:
  const ModuleDependenciesKind dependencyKind;

  ModuleDependenciesStorageBase(ModuleDependenciesKind dependencyKind)
      : dependencyKind(dependencyKind) { }

  virtual ModuleDependenciesStorageBase *clone() const = 0;

  virtual ~ModuleDependenciesStorageBase();

  /// The set of modules on which this module depends.
  std::vector<std::string> moduleDependencies;
};

struct CommonSwiftTextualModuleDependencyDetails {
  CommonSwiftTextualModuleDependencyDetails(ArrayRef<StringRef> extraPCMArgs)
     : extraPCMArgs(extraPCMArgs.begin(), extraPCMArgs.end()) {}

  /// To build a PCM to be used by this Swift module, we need to append these
  /// arguments to the generic PCM build arguments reported from the dependency
  /// graph.
  const std::vector<std::string> extraPCMArgs;

  /// Bridging header file, if there is one.
  Optional<std::string> bridgingHeaderFile;

  /// Source files on which the bridging header depends.
  std::vector<std::string> bridgingSourceFiles;

  /// (Clang) modules on which the bridging header depends.
  std::vector<std::string> bridgingModuleDependencies;
};

/// Describes the dependencies of a Swift module described by an Swift interface file.
///
/// This class is mostly an implementation detail for \c ModuleDependencies.
class SwiftInterfaceModuleDependenciesStorage :
  public ModuleDependenciesStorageBase {
public:
  /// The Swift interface file to be used to generate the module file.
  const std::string swiftInterfaceFile;

  /// Potentially ready-to-use compiled modules for the interface file.
  const std::vector<std::string> compiledModuleCandidates;

  /// The Swift frontend invocation arguments to build the Swift module from the
  /// interface.
  const std::vector<std::string> buildCommandLine;

  /// The hash value that will be used for the generated module
  const std::string contextHash;

  /// A flag that indicates this dependency is a framework
  const bool isFramework;

  /// Details common to Swift textual (interface or source) modules
  CommonSwiftTextualModuleDependencyDetails textualModuleDetails;

  SwiftInterfaceModuleDependenciesStorage(
      const std::string swiftInterfaceFile,
      ArrayRef<std::string> compiledModuleCandidates,
      ArrayRef<StringRef> buildCommandLine,
      ArrayRef<StringRef> extraPCMArgs,
      StringRef contextHash,
      bool isFramework
  ) : ModuleDependenciesStorageBase(ModuleDependenciesKind::SwiftInterface),
      swiftInterfaceFile(swiftInterfaceFile),
      compiledModuleCandidates(compiledModuleCandidates.begin(),
                               compiledModuleCandidates.end()),
      buildCommandLine(buildCommandLine.begin(), buildCommandLine.end()),
      contextHash(contextHash), isFramework(isFramework),
      textualModuleDetails(extraPCMArgs)
      {}

  ModuleDependenciesStorageBase *clone() const override {
    return new SwiftInterfaceModuleDependenciesStorage(*this);
  }

  static bool classof(const ModuleDependenciesStorageBase *base) {
    return base->dependencyKind == ModuleDependenciesKind::SwiftInterface;
  }
};

/// Describes the dependencies of a Swift module
///
/// This class is mostly an implementation detail for \c ModuleDependencies.
class SwiftSourceModuleDependenciesStorage :
  public ModuleDependenciesStorageBase {
public:

  /// Swift source files that are part of the Swift module, when known.
  std::vector<std::string> sourceFiles;

  /// Details common to Swift textual (interface or source) modules
  CommonSwiftTextualModuleDependencyDetails textualModuleDetails;

  SwiftSourceModuleDependenciesStorage(
    ArrayRef<StringRef> extraPCMArgs
  ) : ModuleDependenciesStorageBase(ModuleDependenciesKind::SwiftSource),
      textualModuleDetails(extraPCMArgs) {}

  ModuleDependenciesStorageBase *clone() const override {
    return new SwiftSourceModuleDependenciesStorage(*this);
  }

  static bool classof(const ModuleDependenciesStorageBase *base) {
    return base->dependencyKind == ModuleDependenciesKind::SwiftSource;
  }
};

/// Describes the dependencies of a pre-built Swift module (with no .swiftinterface).
///
/// This class is mostly an implementation detail for \c ModuleDependencies.
class SwiftBinaryModuleDependencyStorage : public ModuleDependenciesStorageBase {
public:
  SwiftBinaryModuleDependencyStorage(const std::string &compiledModulePath,
                                     const std::string &moduleDocPath,
                                     const std::string &sourceInfoPath,
                                     const bool isFramework)
      : ModuleDependenciesStorageBase(ModuleDependenciesKind::SwiftBinary),
        compiledModulePath(compiledModulePath),
        moduleDocPath(moduleDocPath),
        sourceInfoPath(sourceInfoPath),
        isFramework(isFramework) {}

  ModuleDependenciesStorageBase *clone() const override {
    return new SwiftBinaryModuleDependencyStorage(*this);
  }

  /// The path to the .swiftmodule file.
  const std::string compiledModulePath;

  /// The path to the .swiftModuleDoc file.
  const std::string moduleDocPath;

  /// The path to the .swiftSourceInfo file.
  const std::string sourceInfoPath;

  /// A flag that indicates this dependency is a framework
  const bool isFramework;

  static bool classof(const ModuleDependenciesStorageBase *base) {
    return base->dependencyKind == ModuleDependenciesKind::SwiftBinary;
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
      const std::string &moduleMapFile,
      const std::string &contextHash,
      const std::vector<std::string> &nonPathCommandLine,
      const std::vector<std::string> &fileDependencies
  ) : ModuleDependenciesStorageBase(ModuleDependenciesKind::Clang),
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

class SwiftPlaceholderModuleDependencyStorage : public ModuleDependenciesStorageBase {
public:
  SwiftPlaceholderModuleDependencyStorage(const std::string &compiledModulePath,
                                          const std::string &moduleDocPath,
                                          const std::string &sourceInfoPath)
      : ModuleDependenciesStorageBase(ModuleDependenciesKind::SwiftPlaceholder),
        compiledModulePath(compiledModulePath),
        moduleDocPath(moduleDocPath),
        sourceInfoPath(sourceInfoPath) {}

  ModuleDependenciesStorageBase *clone() const override {
    return new SwiftPlaceholderModuleDependencyStorage(*this);
  }

  /// The path to the .swiftmodule file.
  const std::string compiledModulePath;

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
  static ModuleDependencies forSwiftInterfaceModule(
      std::string swiftInterfaceFile,
      ArrayRef<std::string> compiledCandidates,
      ArrayRef<StringRef> buildCommands,
      ArrayRef<StringRef> extraPCMArgs,
      StringRef contextHash,
      bool isFramework) {
    return ModuleDependencies(
        std::make_unique<SwiftInterfaceModuleDependenciesStorage>(
          swiftInterfaceFile, compiledCandidates, buildCommands,
          extraPCMArgs, contextHash, isFramework));
  }

  /// Describe the module dependencies for a serialized or parsed Swift module.
  static ModuleDependencies forSwiftBinaryModule(
      const std::string &compiledModulePath,
      const std::string &moduleDocPath,
      const std::string &sourceInfoPath,
      bool isFramework) {
    return ModuleDependencies(
        std::make_unique<SwiftBinaryModuleDependencyStorage>(
          compiledModulePath, moduleDocPath, sourceInfoPath, isFramework));
  }

  /// Describe the main Swift module.
  static ModuleDependencies forSwiftSourceModule(ArrayRef<StringRef> extraPCMArgs) {
    return ModuleDependencies(
        std::make_unique<SwiftSourceModuleDependenciesStorage>(extraPCMArgs));
  }

  /// Describe the module dependencies for a Clang module that can be
  /// built from a module map and headers.
  static ModuleDependencies forClangModule(
      const std::string &moduleMapFile,
      const std::string &contextHash,
      const std::vector<std::string> &nonPathCommandLine,
      const std::vector<std::string> &fileDependencies) {
    return ModuleDependencies(
        std::make_unique<ClangModuleDependenciesStorage>(
          moduleMapFile, contextHash, nonPathCommandLine, fileDependencies));
  }

  /// Describe a placeholder dependency swift module.
  static ModuleDependencies forPlaceholderSwiftModuleStub(
      const std::string &compiledModulePath,
      const std::string &moduleDocPath,
      const std::string &sourceInfoPath) {
    return ModuleDependencies(
        std::make_unique<SwiftPlaceholderModuleDependencyStorage>(
          compiledModulePath, moduleDocPath, sourceInfoPath));
  }

  /// Retrieve the module-level dependencies.
  ArrayRef<std::string> getModuleDependencies() const {
    return storage->moduleDependencies;
  }

  /// Whether the dependencies are for a Swift module: either Textual, Source, Binary, or Placeholder.
  bool isSwiftModule() const;

  /// Whether the dependencies are for a textual Swift module.
  bool isSwiftInterfaceModule() const;

  /// Whether the dependencies are for a textual Swift module.
  bool isSwiftSourceModule() const;

  /// Whether the dependencies are for a binary Swift module.
  bool isSwiftBinaryModule() const;

  /// Whether this represents a placeholder module stub
  bool isSwiftPlaceholderModule() const;

  /// Whether the dependencies are for a Clang module.
  bool isClangModule() const;

  ModuleDependenciesKind getKind() const {
    return storage->dependencyKind;
  }
  /// Retrieve the dependencies for a Swift textual-interface module.
  const SwiftInterfaceModuleDependenciesStorage *getAsSwiftInterfaceModule() const;

  /// Retrieve the dependencies for a Swift module.
  const SwiftSourceModuleDependenciesStorage *getAsSwiftSourceModule() const;

  /// Retrieve the dependencies for a binary Swift module.
  const SwiftBinaryModuleDependencyStorage *getAsSwiftBinaryModule() const;

  /// Retrieve the dependencies for a Clang module.
  const ClangModuleDependenciesStorage *getAsClangModule() const;

  /// Retrieve the dependencies for a placeholder dependency module stub.
  const SwiftPlaceholderModuleDependencyStorage *
    getAsPlaceholderDependencyModule() const;

  /// Add a dependency on the given module, if it was not already in the set.
  void addModuleDependency(StringRef module,
                           llvm::StringSet<> *alreadyAddedModules = nullptr);

  /// Add a dependency on the given module, if it was not already in the set.
  void addModuleDependency(ImportPath::Module module,
                           llvm::StringSet<> *alreadyAddedModules = nullptr) {
    addModuleDependency(module.front().Item.str(), alreadyAddedModules);
  }

  /// Add all of the module dependencies for the imports in the given source
  /// file to the set of module dependencies.
  void addModuleDependencies(const SourceFile &sf,
                             llvm::StringSet<> &alreadyAddedModules);

  /// Get the bridging header.
  Optional<std::string> getBridgingHeader() const;

  /// Add a bridging header to a Swift module's dependencies.
  void addBridgingHeader(StringRef bridgingHeader);

  /// Add source files
  void addSourceFile(StringRef sourceFile);

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
using ModuleDependenciesVector = llvm::SmallVector<ModuleDependencies, 1>;
using ModuleDependenciesKindMap =
    std::unordered_map<ModuleDependenciesKind,
                       llvm::StringMap<ModuleDependenciesVector>,
                       ModuleDependenciesKindHash>;
using ModuleDependenciesKindRefMap =
    std::unordered_map<ModuleDependenciesKind,
                       llvm::StringMap<const ModuleDependencies *>,
                       ModuleDependenciesKindHash>;

/// A cache describing the set of module dependencies that has been queried
/// thus far. This cache records/stores the actual Dependency values and can be
/// preserved across different scanning actions (e.g. in
/// `DependencyScanningTool`). It is not to be queried directly, but is rather
/// meant to be wrapped in an instance of `ModuleDependenciesCache`, responsible
/// for recording new dependencies and answering cache queries in a given scan.
/// Queries to this cache must be disambiguated with a set of search paths to
/// ensure that the returned cached dependency was one that can be found in the
/// current scanning action's filesystem view.
class GlobalModuleDependenciesCache {
  /// Global cache contents specific to a target-triple specified on a scanner invocation
  struct TargetSpecificGlobalCacheState {
    /// All cached module dependencies, in the order in which they were
    /// encountered.
    std::vector<ModuleDependencyID> AllModules;

    /// Dependencies for modules that have already been computed.
    /// This maps a dependency kind to a map of a module's name to a vector of Dependency objects,
    /// which correspond to instances of the same module that may have been found
    /// in different sets of search paths.
    ModuleDependenciesKindMap ModuleDependenciesMap;
  };

  /// All cached Swift source module dependencies, in the order in which they were encountered
  std::vector<ModuleDependencyID> AllSourceModules;

  /// Dependencies for all Swift source-based modules discovered. Each one is the main
  /// module of a prior invocation of the scanner.
  llvm::StringMap<ModuleDependencies> SwiftSourceModuleDependenciesMap;

  /// A map from a String representing the target triple of a scanner invocation to the corresponding
  /// cached dependencies discovered so far when using this triple.
  llvm::StringMap<std::unique_ptr<TargetSpecificGlobalCacheState>> TargetSpecificCacheMap;

  /// The current target triple cache configuration
  Optional<std::string> CurrentTriple;

  /// The triples used by scanners using this cache, in the order in which they were used
  std::vector<std::string> AllTriples;

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
  llvm::StringMap<ModuleDependenciesVector> &
  getDependenciesMap(ModuleDependenciesKind kind);
  const llvm::StringMap<ModuleDependenciesVector> &
  getDependenciesMap(ModuleDependenciesKind kind) const;

public:
  GlobalModuleDependenciesCache() {};
  GlobalModuleDependenciesCache(const GlobalModuleDependenciesCache &) = delete;
  GlobalModuleDependenciesCache &
  operator=(const GlobalModuleDependenciesCache &) = delete;

  virtual ~GlobalModuleDependenciesCache() { destroyClangImpl(); }

  void configureForTriple(std::string triple);

  const std::vector<std::string>& getAllTriples() const {
    return AllTriples;
  }

private:
  /// Enforce clients not being allowed to query this cache directly, it must be
  /// wrapped in an instance of `ModuleDependenciesCache`.
  friend class ModuleDependenciesCache;

  /// Set the Clang-specific implementation data.
  virtual void
  setClangImpl(ClangModuleDependenciesCacheImpl *clangImpl,
               void (*clangImplDeleter)(ClangModuleDependenciesCacheImpl *)) {
    destroyClangImpl();

    this->clangImpl = clangImpl;
    this->clangImplDeleter = clangImplDeleter;
  }

  /// Retrieve the Clang-specific implementation data;
  ClangModuleDependenciesCacheImpl *getClangImpl() const { return clangImpl; }

  /// Whether we have cached dependency information for the given module.
  bool hasDependencies(StringRef moduleName,
                       ModuleLookupSpecifics details) const;

  /// Look for module dependencies for a module with the given name given
  /// current search paths.
  ///
  /// \returns the cached result, or \c None if there is no cached entry.
  Optional<ModuleDependencies>
  findDependencies(StringRef moduleName, ModuleLookupSpecifics details) const;

  /// Return a pointer to the target-specific cache state of the current triple configuration.
  TargetSpecificGlobalCacheState* getCurrentCache() const;

  /// Return a pointer to the target-specific cache state of the specified triple configuration.
  TargetSpecificGlobalCacheState* getCacheForTriple(StringRef triple) const;

public:
  /// Look for module dependencies for a module with the given name.
  /// This method has a deliberately-obtuse name to indicate that it is not to
  /// be used for general queries.
  ///
  /// \returns the cached result, or \c None if there is no cached entry.
  Optional<ModuleDependenciesVector>
  findAllDependenciesIrrespectiveOfSearchPaths(
      StringRef moduleName, Optional<ModuleDependenciesKind> kind) const;

  /// Look for source-based module dependency details
  Optional<ModuleDependencies>
  findSourceModuleDependency(StringRef moduleName) const;

  /// Record dependencies for the given module.
  const ModuleDependencies *recordDependencies(StringRef moduleName,
                                               ModuleDependencies dependencies);

  /// Update stored dependencies for the given module.
  const ModuleDependencies *updateDependencies(ModuleDependencyID moduleID,
                                               ModuleDependencies dependencies);

  /// Reference the list of all module dependencies that are not source-based modules
  /// (i.e. interface dependencies, binary dependencies, clang dependencies).
  const std::vector<ModuleDependencyID> &getAllNonSourceModules(StringRef triple) const {
    auto targetSpecificCache = getCacheForTriple(triple);
    return targetSpecificCache->AllModules;
  }

  /// Return the list of all source-based modules discovered by this cache
  const std::vector<ModuleDependencyID> &getAllSourceModules() const {
    return AllSourceModules;
  }
};

/// This "local" dependencies cache persists only for the duration of a given
/// scanning action, and wraps an instance of a `GlobalModuleDependenciesCache`
/// which may carry cached scanning information from prior scanning actions.
/// This cache maintains a store of references to all dependencies found within
/// the current scanning action (with their values stored in the global Cache),
/// since these do not require clients to disambiguate them with search paths.
class ModuleDependenciesCache {
private:
  GlobalModuleDependenciesCache &globalCache;

  /// References to data in `globalCache` for dependencies accimulated during
  /// the current scanning action.
  ModuleDependenciesKindRefMap ModuleDependenciesMap;

  /// Retrieve the dependencies map that corresponds to the given dependency
  /// kind.
  llvm::StringMap<const ModuleDependencies *> &
  getDependencyReferencesMap(ModuleDependenciesKind kind);
  const llvm::StringMap<const ModuleDependencies *> &
  getDependencyReferencesMap(ModuleDependenciesKind kind) const;

  /// Local cache results lookup, only for modules which were discovered during
  /// the current scanner invocation.
  bool hasDependencies(StringRef moduleName,
                       Optional<ModuleDependenciesKind> kind) const;

  /// Local cache results lookup, only for modules which were discovered during
  /// the current scanner invocation.
  Optional<const ModuleDependencies *>
  findDependencies(StringRef moduleName,
                   Optional<ModuleDependenciesKind> kind) const;

public:
  ModuleDependenciesCache(GlobalModuleDependenciesCache &globalCache);
  ModuleDependenciesCache(const ModuleDependenciesCache &) = delete;
  ModuleDependenciesCache &operator=(const ModuleDependenciesCache &) = delete;
  virtual ~ModuleDependenciesCache() {}

public:
  /// Set the Clang-specific implementation data.
  void
  setClangImpl(ClangModuleDependenciesCacheImpl *clangImpl,
               void (*clangImplDeleter)(ClangModuleDependenciesCacheImpl *)) {
    globalCache.setClangImpl(clangImpl, clangImplDeleter);
  }

  /// Retrieve the Clang-specific implementation data;
  ClangModuleDependenciesCacheImpl *getClangImpl() const {
    return globalCache.getClangImpl();
  }

  /// Whether we have cached dependency information for the given module.
  bool hasDependencies(StringRef moduleName,
                       ModuleLookupSpecifics details) const;

  /// Look for module dependencies for a module with the given name given
  /// current search paths.
  ///
  /// \returns the cached result, or \c None if there is no cached entry.
  Optional<ModuleDependencies>
  findDependencies(StringRef moduleName, ModuleLookupSpecifics details) const;

  /// Look for module dependencies for a module with the given name.
  /// This method has a deliberately-obtuse name to indicate that it is not to
  /// be used for general queries.
  ///
  /// \returns the cached result, or \c None if there is no cached entry.
  Optional<ModuleDependenciesVector>
  findAllDependenciesIrrespectiveOfSearchPaths(
      StringRef moduleName, Optional<ModuleDependenciesKind> kind) const {
    return globalCache.findAllDependenciesIrrespectiveOfSearchPaths(moduleName,
                                                                    kind);
  }

  /// Record dependencies for the given module.
  void recordDependencies(StringRef moduleName,
                          ModuleDependencies dependencies);

  /// Update stored dependencies for the given module.
  void updateDependencies(ModuleDependencyID moduleID,
                          ModuleDependencies dependencies);

  const std::vector<ModuleDependencyID> &getAllSourceModules() const {
    return globalCache.getAllSourceModules();
  }
};

} // namespace swift

#endif /* SWIFT_AST_MODULE_DEPENDENCIES_H */
