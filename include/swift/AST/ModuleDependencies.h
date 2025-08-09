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

#include "swift/AST/Import.h"
#include "swift/AST/LinkLibrary.h"
#include "swift/Basic/Assertions.h"
#include "swift/Basic/CXXStdlibKind.h"
#include "swift/Basic/LLVM.h"
#include "swift/Serialization/Validation.h"
#include "clang/CAS/CASOptions.h"
#include "clang/Tooling/DependencyScanning/DependencyScanningService.h"
#include "clang/Tooling/DependencyScanning/ModuleDepCollector.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/IntrusiveRefCntPtr.h"
#include "llvm/ADT/StringSet.h"
#include "llvm/CAS/CachingOnDiskFileSystem.h"
#include "llvm/Support/Mutex.h"
#include <optional>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>

namespace swift {

class ClangModuleDependenciesCacheImpl;
class SourceFile;
class ASTContext;
class Identifier;
class CompilerInstance;
class IRGenOptions;
class CompilerInvocation;
class DiagnosticEngine;

/// Which kind of module dependencies we are looking for.
enum class ModuleDependencyKind : int8_t {
  FirstKind,
  // Textual Swift dependency
  SwiftInterface = FirstKind,
  // Binary module Swift dependency
  SwiftBinary,
  // Clang module dependency
  Clang,
  // Used to model the translation unit's source module
  SwiftSource,
  LastKind = SwiftSource + 1
};

/// This is used to idenfity a specific macro plugin dependency.
struct MacroPluginDependency {
  std::string LibraryPath;
  std::string ExecutablePath;
};

/// This is used to identify a specific module.
struct ModuleDependencyID {
  std::string ModuleName;
  ModuleDependencyKind Kind;
  bool operator==(const ModuleDependencyID &Other) const {
    return std::tie(ModuleName, Kind) == std::tie(Other.ModuleName, Other.Kind);
  }
  bool operator<(const ModuleDependencyID &Other) const {
    return std::tie(ModuleName, Kind) < std::tie(Other.ModuleName, Other.Kind);
  }
};

struct ModuleDependencyKindHash {
  std::size_t operator()(ModuleDependencyKind k) const {
    using UnderlyingType = std::underlying_type<ModuleDependencyKind>::type;
    return std::hash<UnderlyingType>{}(static_cast<UnderlyingType>(k));
  }
};
struct ModuleDependencyIDHash {
  std::size_t operator()(ModuleDependencyID id) const {
    return llvm::hash_combine(id.ModuleName, id.Kind);
  }
};

using ModuleDependencyIDSet =
    std::unordered_set<ModuleDependencyID, ModuleDependencyIDHash>;
using ModuleDependencyIDSetVector =
    llvm::SetVector<ModuleDependencyID, std::vector<ModuleDependencyID>,
                    std::set<ModuleDependencyID>>;

namespace dependencies {
std::string createEncodedModuleKindAndName(ModuleDependencyID id);
bool checkImportNotTautological(const ImportPath::Module, const SourceLoc,
                                const SourceFile &, bool);
void registerBackDeployLibraries(
    const IRGenOptions &IRGenOpts,
    std::function<void(const LinkLibrary &)> RegistrationCallback);
void registerCxxInteropLibraries(
    const llvm::Triple &Target, StringRef mainModuleName, bool hasStaticCxx,
    bool hasStaticCxxStdlib, CXXStdlibKind cxxStdlibKind,
    std::function<void(const LinkLibrary &)> RegistrationCallback);
} // namespace dependencies

struct ScannerImportStatementInfo {
  struct ImportDiagnosticLocationInfo {
    ImportDiagnosticLocationInfo() = delete;
    ImportDiagnosticLocationInfo(std::string bufferIdentifier,
                                 uint32_t lineNumber, uint32_t columnNumber)
        : bufferIdentifier(bufferIdentifier), lineNumber(lineNumber),
          columnNumber(columnNumber) {}
    std::string bufferIdentifier;
    uint32_t lineNumber;
    uint32_t columnNumber;
  };

  ScannerImportStatementInfo(std::string importIdentifier)
      : importIdentifier(importIdentifier), importLocations(),
        isExported(false), accessLevel(AccessLevel::Public) {}

  ScannerImportStatementInfo(std::string importIdentifier, bool isExported,
                             AccessLevel accessLevel)
      : importIdentifier(importIdentifier), importLocations(),
        isExported(isExported), accessLevel(accessLevel) {}

  ScannerImportStatementInfo(std::string importIdentifier, bool isExported,
                             AccessLevel accessLevel,
                             ImportDiagnosticLocationInfo location)
      : importIdentifier(importIdentifier), importLocations({location}),
        isExported(isExported), accessLevel(accessLevel) {}

  ScannerImportStatementInfo(std::string importIdentifier, bool isExported,
                             AccessLevel accessLevel,
                             SmallVector<ImportDiagnosticLocationInfo, 4> locations)
      : importIdentifier(importIdentifier), importLocations(locations),
        isExported(isExported), accessLevel(accessLevel) {}

  void addImportLocation(ImportDiagnosticLocationInfo location) {
    importLocations.push_back(location);
  }

  /// Imported module string. e.g. "Foo.Bar" in 'import Foo.Bar'
  std::string importIdentifier;
  /// Buffer, line & column number of the import statement
  SmallVector<ImportDiagnosticLocationInfo, 4> importLocations;
  /// Is this an @_exported import
  bool isExported;
  /// Access level of this dependency
  AccessLevel accessLevel;
};

/// Base class for the variant storage of ModuleDependencyInfo.
///
/// This class is mostly an implementation detail for \c ModuleDependencyInfo.
class ModuleDependencyInfoStorageBase {
public:
  const ModuleDependencyKind dependencyKind;

  ModuleDependencyInfoStorageBase(
      ModuleDependencyKind dependencyKind,
      ArrayRef<ScannerImportStatementInfo> moduleImports,
      ArrayRef<ScannerImportStatementInfo> optionalModuleImports,
      ArrayRef<LinkLibrary> linkLibraries, StringRef moduleCacheKey = "")
      : dependencyKind(dependencyKind), moduleImports(moduleImports),
        optionalModuleImports(optionalModuleImports),
        linkLibraries(linkLibraries), moduleCacheKey(moduleCacheKey.str()),
        finalized(false) {}

  virtual ModuleDependencyInfoStorageBase *clone() const = 0;

  virtual ~ModuleDependencyInfoStorageBase();

  /// The set of modules on which this module depends.
  std::vector<ScannerImportStatementInfo> moduleImports;

  /// The set of modules which constitute optional module
  /// dependencies for this module, such as `@_implementationOnly`
  /// or `internal` imports.
  std::vector<ScannerImportStatementInfo> optionalModuleImports;

  /// A collection of libraries that must be linked to
  /// use this module.
  std::vector<LinkLibrary> linkLibraries;

  /// All directly-imported Swift module dependencies.
  std::vector<ModuleDependencyID> importedSwiftModules;
  /// All directly-imported Clang module dependencies.
  std::vector<ModuleDependencyID> importedClangModules;
  /// All cross-import overlay module dependencies.
  std::vector<ModuleDependencyID> crossImportOverlayModules;
  /// All dependencies comprised of Swift overlay modules of direct and
  /// transitive Clang dependencies.
  std::vector<ModuleDependencyID> swiftOverlayDependencies;

  /// The cache key for the produced module.
  std::string moduleCacheKey;

  /// Auxiliary files that help to construct other dependencies (e.g.
  /// command-line), no need to be saved to reconstruct from cache.
  std::vector<std::string> auxiliaryFiles;

  /// The macro dependencies.
  std::map<std::string, MacroPluginDependency> macroDependencies;

  /// A list of Clang modules that are visible to this Swift module. This
  /// includes both direct Clang modules as well as transitive Clang
  /// module dependencies when they are exported
  llvm::StringSet<> visibleClangModules;

  /// ModuleDependencyInfo is finalized (with all transitive dependencies
  /// and inputs).
  bool finalized;
};

struct CommonSwiftTextualModuleDependencyDetails {
  CommonSwiftTextualModuleDependencyDetails(
      ArrayRef<StringRef> buildCommandLine,
      StringRef CASFileSystemRootID)
      : bridgingHeaderFile(std::nullopt),
        bridgingSourceFiles(), bridgingModuleDependencies(),
        buildCommandLine(buildCommandLine.begin(), buildCommandLine.end()),
        CASFileSystemRootID(CASFileSystemRootID) {}

  /// Bridging header file, if there is one.
  std::optional<std::string> bridgingHeaderFile;

  /// Source files on which the bridging header depends.
  std::vector<std::string> bridgingSourceFiles;

  /// (Clang) modules on which the bridging header depends.
  std::vector<ModuleDependencyID> bridgingModuleDependencies;

  /// The Swift frontend invocation arguments to build the Swift module from the
  /// interface.
  std::vector<std::string> buildCommandLine;

  /// CASID for the Root of CASFS. Empty if CAS is not used.
  std::string CASFileSystemRootID;

  /// CASID for the Root of bridgingHeaderClangIncludeTree. Empty if not used.
  std::string CASBridgingHeaderIncludeTreeRootID;
};

/// Describes the dependencies of a Swift module described by an Swift interface
/// file.
///
/// This class is mostly an implementation detail for \c ModuleDependencyInfo.
class SwiftInterfaceModuleDependenciesStorage
    : public ModuleDependencyInfoStorageBase {
public:
  /// Destination output path
  std::string moduleOutputPath;

  /// The Swift interface file to be used to generate the module file.
  const std::string swiftInterfaceFile;

  /// Potentially ready-to-use compiled modules for the interface file.
  const std::vector<std::string> compiledModuleCandidates;

  /// The hash value that will be used for the generated module
  std::string contextHash;

  /// A flag that indicates this dependency is a framework
  const bool isFramework;

  /// A flag that indicates this dependency is associated with a static archive
  const bool isStatic;

  /// Details common to Swift textual (interface or source) modules
  CommonSwiftTextualModuleDependencyDetails textualModuleDetails;

  /// The user module version of this textual module interface.
  const std::string userModuleVersion;

  SwiftInterfaceModuleDependenciesStorage(
      StringRef swiftInterfaceFile,
      ArrayRef<StringRef> compiledModuleCandidates,
      ArrayRef<ScannerImportStatementInfo> moduleImports,
      ArrayRef<ScannerImportStatementInfo> optionalModuleImports,
      ArrayRef<StringRef> buildCommandLine, ArrayRef<LinkLibrary> linkLibraries,
      bool isFramework, bool isStatic, StringRef RootID,
      StringRef moduleCacheKey, StringRef userModuleVersion)
      : ModuleDependencyInfoStorageBase(ModuleDependencyKind::SwiftInterface,
                                        moduleImports, optionalModuleImports,
                                        linkLibraries, moduleCacheKey),
        swiftInterfaceFile(swiftInterfaceFile),
        compiledModuleCandidates(compiledModuleCandidates.begin(),
                                 compiledModuleCandidates.end()),
        isFramework(isFramework), isStatic(isStatic),
        textualModuleDetails(buildCommandLine, RootID),
        userModuleVersion(userModuleVersion) {}

  ModuleDependencyInfoStorageBase *clone() const override {
    return new SwiftInterfaceModuleDependenciesStorage(*this);
  }

  static bool classof(const ModuleDependencyInfoStorageBase *base) {
    return base->dependencyKind == ModuleDependencyKind::SwiftInterface;
  }

  void updateCommandLine(const std::vector<std::string> &newCommandLine) {
    textualModuleDetails.buildCommandLine = newCommandLine;
  }
};

/// Describes the dependencies of a Swift module
///
/// This class is mostly an implementation detail for \c ModuleDependencyInfo.
class SwiftSourceModuleDependenciesStorage
    : public ModuleDependencyInfoStorageBase {
public:
  /// Swift source files that are part of the Swift module, when known.
  std::vector<std::string> sourceFiles;

  /// Details common to Swift textual (interface or source) modules
  CommonSwiftTextualModuleDependencyDetails textualModuleDetails;

  /// Collection of module imports that were detected to be `@Testable`
  llvm::StringSet<> testableImports;

  /// The Swift frontend invocation arguments to build bridging header.
  std::vector<std::string> bridgingHeaderBuildCommandLine;

  /// The chained bridging header path if used.
  std::string chainedBridgingHeaderPath;

  /// The chained bridging header source buffer if used.
  std::string chainedBridgingHeaderContent;

  SwiftSourceModuleDependenciesStorage(
      StringRef RootID, ArrayRef<StringRef> buildCommandLine,
      ArrayRef<ScannerImportStatementInfo> moduleImports,
      ArrayRef<ScannerImportStatementInfo> optionalModuleImports,
      ArrayRef<StringRef> bridgingHeaderBuildCommandLine)
      : ModuleDependencyInfoStorageBase(ModuleDependencyKind::SwiftSource,
                                        moduleImports, optionalModuleImports, {}),
        textualModuleDetails(buildCommandLine, RootID),
        testableImports(llvm::StringSet<>()),
        bridgingHeaderBuildCommandLine(bridgingHeaderBuildCommandLine.begin(),
                                       bridgingHeaderBuildCommandLine.end()) {}

  ModuleDependencyInfoStorageBase *clone() const override {
    return new SwiftSourceModuleDependenciesStorage(*this);
  }


  static bool classof(const ModuleDependencyInfoStorageBase *base) {
    return base->dependencyKind == ModuleDependencyKind::SwiftSource;
  }

  void updateCommandLine(const std::vector<std::string> &newCommandLine) {
    textualModuleDetails.buildCommandLine = newCommandLine;
  }

  void updateBridgingHeaderCommandLine(
      const std::vector<std::string> &newCommandLine) {
    bridgingHeaderBuildCommandLine = newCommandLine;
  }

  void addTestableImport(ImportPath::Module module) {
    testableImports.insert(module.front().Item.str());
  }

  void setChainedBridgingHeaderBuffer(StringRef path, StringRef buffer) {
    chainedBridgingHeaderPath = path.str();
    chainedBridgingHeaderContent = buffer.str();
  }
};

/// Describes the dependencies of a pre-built Swift module (with no
/// .swiftinterface).
///
/// This class is mostly an implementation detail for \c ModuleDependencyInfo.
class SwiftBinaryModuleDependencyStorage
    : public ModuleDependencyInfoStorageBase {
public:
  SwiftBinaryModuleDependencyStorage(
      StringRef compiledModulePath, StringRef moduleDocPath,
      StringRef sourceInfoPath,
      ArrayRef<ScannerImportStatementInfo> moduleImports,
      ArrayRef<ScannerImportStatementInfo> optionalModuleImports,
      ArrayRef<LinkLibrary> linkLibraries,
      ArrayRef<serialization::SearchPath> serializedSearchPaths,
      StringRef headerImport, StringRef definingModuleInterface,
      bool isFramework, bool isStatic, bool isBuiltWithCxxInterop,
      StringRef moduleCacheKey, StringRef userModuleVersion)
      : ModuleDependencyInfoStorageBase(ModuleDependencyKind::SwiftBinary,
                                        moduleImports, optionalModuleImports,
                                        linkLibraries, moduleCacheKey),
        compiledModulePath(compiledModulePath), moduleDocPath(moduleDocPath),
        sourceInfoPath(sourceInfoPath), headerImport(headerImport),
        definingModuleInterfacePath(definingModuleInterface),
        serializedSearchPaths(serializedSearchPaths),
        isFramework(isFramework), isStatic(isStatic),
        isBuiltWithCxxInterop(isBuiltWithCxxInterop),
        userModuleVersion(userModuleVersion) {}

  ModuleDependencyInfoStorageBase *clone() const override {
    return new SwiftBinaryModuleDependencyStorage(*this);
  }

  /// The path to the .swiftmodule file.
  const std::string compiledModulePath;

  /// The path to the .swiftModuleDoc file.
  const std::string moduleDocPath;

  /// The path to the .swiftSourceInfo file.
  const std::string sourceInfoPath;

  /// The path of the .h dependency of this module.
  const std::string headerImport;

  /// The path of the defining .swiftinterface that this
  /// binary .swiftmodule was built from, if one exists.
  const std::string definingModuleInterfacePath;

  /// Source files on which the header inputs depend.
  std::vector<std::string> headerSourceFiles;

  /// Search paths this module was built with which got serialized
  std::vector<serialization::SearchPath> serializedSearchPaths;

  /// (Clang) modules on which the header inputs depend.
  std::vector<ModuleDependencyID> headerModuleDependencies;

  /// A flag that indicates this dependency is a framework
  const bool isFramework;

  /// A flag that indicates this dependency is associated with a static archive
  const bool isStatic;

  /// A flag that indicates this dependency module was built
  /// with C++ interop enabled
  const bool isBuiltWithCxxInterop;

  /// The user module version of this binary module.
  const std::string userModuleVersion;

  /// Return the path to the defining .swiftinterface of this module
  /// of one was determined. Otherwise, return the .swiftmodule path
  /// itself.
  std::string getDefiningModulePath() const {
    if (definingModuleInterfacePath.empty())
      return compiledModulePath;
    return definingModuleInterfacePath;
  }

  static bool classof(const ModuleDependencyInfoStorageBase *base) {
    return base->dependencyKind == ModuleDependencyKind::SwiftBinary;
  }
};

/// Describes the dependencies of a Clang module.
///
/// This class is mostly an implementation detail for \c ModuleDependencyInfo.
class ClangModuleDependencyStorage : public ModuleDependencyInfoStorageBase {
public:
  /// Destination output path
  const std::string pcmOutputPath;

  /// Same as \c pcmOutputPath, but possibly prefix-mapped using clang's prefix
  /// mapper.
  const std::string mappedPCMPath;

  /// The module map file used to generate the Clang module.
  const std::string moduleMapFile;

  /// The context hash describing the configuration options for this module.
  const std::string contextHash;

  /// Partial (Clang) command line that can be used to build this module.
  std::vector<std::string> buildCommandLine;

  /// The file dependencies
  const std::vector<std::string> fileDependencies;

  /// CASID for the Root of CASFS. Empty if CAS is not used.
  std::string CASFileSystemRootID;

  /// CASID for the Root of ClangIncludeTree. Empty if not used.
  std::string CASClangIncludeTreeRootID;

  /// Whether this is a "system" module.
  bool IsSystem;

  ClangModuleDependencyStorage(StringRef pcmOutputPath, StringRef mappedPCMPath,
                               StringRef moduleMapFile, StringRef contextHash,
                               ArrayRef<std::string> buildCommandLine,
                               ArrayRef<std::string> fileDependencies,
                               ArrayRef<LinkLibrary> linkLibraries,
                               StringRef CASFileSystemRootID,
                               StringRef clangIncludeTreeRoot,
                               StringRef moduleCacheKey, bool IsSystem)
      : ModuleDependencyInfoStorageBase(ModuleDependencyKind::Clang,
                                        {}, {},
                                        linkLibraries, moduleCacheKey),
        pcmOutputPath(pcmOutputPath), mappedPCMPath(mappedPCMPath),
        moduleMapFile(moduleMapFile), contextHash(contextHash),
        buildCommandLine(buildCommandLine), fileDependencies(fileDependencies),
        CASFileSystemRootID(CASFileSystemRootID),
        CASClangIncludeTreeRootID(clangIncludeTreeRoot), IsSystem(IsSystem) {}

  ModuleDependencyInfoStorageBase *clone() const override {
    return new ClangModuleDependencyStorage(*this);
  }

  static bool classof(const ModuleDependencyInfoStorageBase *base) {
    return base->dependencyKind == ModuleDependencyKind::Clang;
  }

  void updateCommandLine(ArrayRef<std::string> newCommandLine) {
    buildCommandLine = newCommandLine;
  }
};

// MARK: Module Dependency Info
/// Describes the dependencies of a given module.
///
/// The dependencies of a module include all of the source files that go
/// into that module, as well as any modules that are directly imported
/// into the module.
class ModuleDependencyInfo {
private:
  std::unique_ptr<ModuleDependencyInfoStorageBase> storage;

  ModuleDependencyInfo(
      std::unique_ptr<ModuleDependencyInfoStorageBase> &&storage)
      : storage(std::move(storage)) {}

public:
  ModuleDependencyInfo() = default;
  ModuleDependencyInfo(const ModuleDependencyInfo &other)
      : storage(other.storage->clone()) {}
  ModuleDependencyInfo(ModuleDependencyInfo &&other) = default;

  ModuleDependencyInfo &operator=(const ModuleDependencyInfo &other) {
    storage.reset(other.storage->clone());
    return *this;
  }

  ModuleDependencyInfo &operator=(ModuleDependencyInfo &&other) = default;

  /// Describe the module dependencies for a Swift module that can be
  /// built from a Swift interface file (\c .swiftinterface).
  static ModuleDependencyInfo forSwiftInterfaceModule(
      StringRef swiftInterfaceFile, ArrayRef<StringRef> compiledCandidates,
      ArrayRef<StringRef> buildCommands,
      ArrayRef<ScannerImportStatementInfo> moduleImports,
      ArrayRef<ScannerImportStatementInfo> optionalModuleImports,
      ArrayRef<LinkLibrary> linkLibraries, bool isFramework, bool isStatic,
      StringRef CASFileSystemRootID, StringRef moduleCacheKey,
      StringRef userModuleVersion) {
    return ModuleDependencyInfo(
        std::make_unique<SwiftInterfaceModuleDependenciesStorage>(
            swiftInterfaceFile, compiledCandidates, moduleImports,
            optionalModuleImports, buildCommands, linkLibraries, isFramework,
            isStatic, CASFileSystemRootID, moduleCacheKey, userModuleVersion));
  }

  /// Describe the module dependencies for a serialized or parsed Swift module.
  static ModuleDependencyInfo forSwiftBinaryModule(
      StringRef compiledModulePath, StringRef moduleDocPath,
      StringRef sourceInfoPath,
      ArrayRef<ScannerImportStatementInfo> moduleImports,
      ArrayRef<ScannerImportStatementInfo> optionalModuleImports,
      ArrayRef<LinkLibrary> linkLibraries,
      ArrayRef<serialization::SearchPath> serializedSearchPaths,
      StringRef headerImport, StringRef definingModuleInterface,
      bool isFramework, bool isStatic, bool isBuiltWithCxxInterop,
      StringRef moduleCacheKey, StringRef userModuleVer) {
    return ModuleDependencyInfo(
        std::make_unique<SwiftBinaryModuleDependencyStorage>(
            compiledModulePath, moduleDocPath, sourceInfoPath, moduleImports,
            optionalModuleImports, linkLibraries, serializedSearchPaths,
            headerImport, definingModuleInterface,isFramework, isStatic,
            isBuiltWithCxxInterop, moduleCacheKey, userModuleVer));
  }

  /// Describe the main Swift module.
  static ModuleDependencyInfo
  forSwiftSourceModule(const std::string &CASFileSystemRootID,
                       ArrayRef<StringRef> buildCommands,
                       ArrayRef<ScannerImportStatementInfo> moduleImports,
                       ArrayRef<ScannerImportStatementInfo> optionalModuleImports,
                       ArrayRef<StringRef> bridgingHeaderBuildCommands) {
    return ModuleDependencyInfo(
        std::make_unique<SwiftSourceModuleDependenciesStorage>(
            CASFileSystemRootID, buildCommands, moduleImports,
            optionalModuleImports, bridgingHeaderBuildCommands));
  }

  static ModuleDependencyInfo
  forSwiftSourceModule() {
    return ModuleDependencyInfo(
        std::make_unique<SwiftSourceModuleDependenciesStorage>(
             StringRef(), ArrayRef<StringRef>(),
             ArrayRef<ScannerImportStatementInfo>(),
             ArrayRef<ScannerImportStatementInfo>(),
             ArrayRef<StringRef>()));
  }

  /// Describe the module dependencies for a Clang module that can be
  /// built from a module map and headers.
  static ModuleDependencyInfo forClangModule(
      StringRef pcmOutputPath, StringRef mappedPCMPath, StringRef moduleMapFile,
      StringRef contextHash, ArrayRef<std::string> nonPathCommandLine,
      ArrayRef<std::string> fileDependencies,
      ArrayRef<LinkLibrary> linkLibraries, StringRef CASFileSystemRootID,
      StringRef clangIncludeTreeRoot, StringRef moduleCacheKey, bool IsSystem) {
    return ModuleDependencyInfo(std::make_unique<ClangModuleDependencyStorage>(
        pcmOutputPath, mappedPCMPath, moduleMapFile, contextHash,
        nonPathCommandLine, fileDependencies, linkLibraries,
        CASFileSystemRootID, clangIncludeTreeRoot, moduleCacheKey, IsSystem));
  }

  /// Retrieve the module-level imports.
  ArrayRef<ScannerImportStatementInfo> getModuleImports() const {
    return storage->moduleImports;
  }

  /// Retrieve the module-level optional imports.
  ArrayRef<ScannerImportStatementInfo> getOptionalModuleImports() const {
    return storage->optionalModuleImports;
  }

  std::string getModuleCacheKey() const {
    return storage->moduleCacheKey;
  }

  void updateModuleCacheKey(const std::string &key) {
    storage->moduleCacheKey = key;
  }

  void
  setImportedSwiftDependencies(const ArrayRef<ModuleDependencyID> dependencyIDs) {
    assert(isSwiftModule());
    storage->importedSwiftModules.assign(dependencyIDs.begin(),
                                         dependencyIDs.end());
  }
  ArrayRef<ModuleDependencyID> getImportedSwiftDependencies() const {
    return storage->importedSwiftModules;
  }

  void
  setImportedClangDependencies(const ArrayRef<ModuleDependencyID> dependencyIDs) {
    storage->importedClangModules.assign(dependencyIDs.begin(),
                                         dependencyIDs.end());
  }
  ArrayRef<ModuleDependencyID> getImportedClangDependencies() const {
    return storage->importedClangModules;
  }

  void
  setHeaderClangDependencies(const ArrayRef<ModuleDependencyID> dependencyIDs) {
    assert(isSwiftModule());
    switch (getKind()) {
    case swift::ModuleDependencyKind::SwiftInterface: {
      auto swiftInterfaceStorage =
          cast<SwiftInterfaceModuleDependenciesStorage>(storage.get());
      swiftInterfaceStorage->textualModuleDetails.bridgingModuleDependencies.assign(dependencyIDs.begin(),
                                                                                    dependencyIDs.end());
      break;
    }
    case swift::ModuleDependencyKind::SwiftSource: {
      auto swiftSourceStorage =
          cast<SwiftSourceModuleDependenciesStorage>(storage.get());
      swiftSourceStorage->textualModuleDetails.bridgingModuleDependencies.assign(dependencyIDs.begin(),
                                                                                 dependencyIDs.end());
      break;
    }
    case swift::ModuleDependencyKind::SwiftBinary: {
      auto swiftBinaryStorage =
          cast<SwiftBinaryModuleDependencyStorage>(storage.get());
      swiftBinaryStorage->headerModuleDependencies.assign(dependencyIDs.begin(),
                                                          dependencyIDs.end());
      break;
    }
    default: {
      llvm_unreachable("Unexpected dependency kind");
    }
    }
  }
  ArrayRef<ModuleDependencyID> getHeaderClangDependencies() const {
    switch (getKind()) {
    case swift::ModuleDependencyKind::SwiftInterface: {
      auto swiftInterfaceStorage =
          cast<SwiftInterfaceModuleDependenciesStorage>(storage.get());
      return swiftInterfaceStorage->textualModuleDetails.bridgingModuleDependencies;
    }
    case swift::ModuleDependencyKind::SwiftSource: {
      auto swiftSourceStorage =
          cast<SwiftSourceModuleDependenciesStorage>(storage.get());
      return swiftSourceStorage->textualModuleDetails.bridgingModuleDependencies;
    }
    case swift::ModuleDependencyKind::SwiftBinary: {
      auto swiftBinaryStorage =
          cast<SwiftBinaryModuleDependencyStorage>(storage.get());
      return swiftBinaryStorage->headerModuleDependencies;
    }
    default:
      return {};
    }
  }

  void
  setSwiftOverlayDependencies(const ArrayRef<ModuleDependencyID> dependencyIDs) {
    assert(isSwiftModule());
    storage->swiftOverlayDependencies.assign(dependencyIDs.begin(),
                                             dependencyIDs.end());
  }
  ArrayRef<ModuleDependencyID> getSwiftOverlayDependencies() const {
    return storage->swiftOverlayDependencies;
  }

  void
  setCrossImportOverlayDependencies(const ArrayRef<ModuleDependencyID> dependencyIDs) {
    assert(isSwiftModule());
    storage->crossImportOverlayModules.assign(dependencyIDs.begin(),
                                              dependencyIDs.end());
  }
  ArrayRef<ModuleDependencyID> getCrossImportOverlayDependencies() const {
    return storage->crossImportOverlayModules;
  }

  ArrayRef<LinkLibrary> getLinkLibraries() const {
    return storage->linkLibraries;
  }

  void
  setLinkLibraries(const ArrayRef<LinkLibrary> linkLibraries) {
    storage->linkLibraries.assign(linkLibraries.begin(), linkLibraries.end());
  }

  ArrayRef<std::string> getAuxiliaryFiles() const {
    return storage->auxiliaryFiles;
  }

  bool isStaticLibrary() const {
    if (auto *detail = getAsSwiftInterfaceModule())
      return detail->isStatic;
    if (auto *detail = getAsSwiftBinaryModule())
      return detail->isStatic;
    return false;
  }

  ArrayRef<std::string> getHeaderInputSourceFiles() const {
    if (auto *detail = getAsSwiftInterfaceModule())
      return detail->textualModuleDetails.bridgingSourceFiles;
    if (auto *detail = getAsSwiftSourceModule())
      return detail->textualModuleDetails.bridgingSourceFiles;
    if (auto *detail = getAsSwiftBinaryModule())
      return detail->headerSourceFiles;
    return {};
  }

  ArrayRef<std::string> getCommandline() const {
    if (auto *detail = getAsClangModule())
      return detail->buildCommandLine;
    if (auto *detail = getAsSwiftInterfaceModule())
      return detail->textualModuleDetails.buildCommandLine;
    if (auto *detail = getAsSwiftSourceModule())
      return detail->textualModuleDetails.buildCommandLine;
    return {};
  }

  void updateCommandLine(const std::vector<std::string> &newCommandLine) {
    if (isSwiftInterfaceModule())
      return cast<SwiftInterfaceModuleDependenciesStorage>(storage.get())
          ->updateCommandLine(newCommandLine);
    if (isSwiftSourceModule())
      return cast<SwiftSourceModuleDependenciesStorage>(storage.get())
          ->updateCommandLine(newCommandLine);
    if (isClangModule())
      return cast<ClangModuleDependencyStorage>(storage.get())
          ->updateCommandLine(newCommandLine);
    llvm_unreachable("Unexpected type");
  }

  ArrayRef<std::string> getBridgingHeaderCommandline() const {
    if (auto *detail = getAsSwiftSourceModule())
      return detail->bridgingHeaderBuildCommandLine;
    return {};
  }

  void updateBridgingHeaderCommandLine(
      const std::vector<std::string> &newCommandLine) {
    if (isSwiftSourceModule())
      return cast<SwiftSourceModuleDependenciesStorage>(storage.get())
          ->updateBridgingHeaderCommandLine(newCommandLine);
    llvm_unreachable("Unexpected type");
  }

  void addAuxiliaryFile(const std::string &file) {
    storage->auxiliaryFiles.emplace_back(file);
  }

  void addMacroDependency(StringRef macroModuleName, StringRef libraryPath,
                          StringRef executablePath) {
    storage->macroDependencies.insert(
        {macroModuleName.str(), {libraryPath.str(), executablePath.str()}});
  }

  std::map<std::string, MacroPluginDependency> &getMacroDependencies() const {
    return storage->macroDependencies;
  }

  void updateCASFileSystemRootID(const std::string &rootID) {
    if (isSwiftInterfaceModule())
      cast<SwiftInterfaceModuleDependenciesStorage>(storage.get())
          ->textualModuleDetails.CASFileSystemRootID = rootID;
    else if (isSwiftSourceModule())
      cast<SwiftSourceModuleDependenciesStorage>(storage.get())
          ->textualModuleDetails.CASFileSystemRootID = rootID;
    else if (isClangModule())
      cast<ClangModuleDependencyStorage>(storage.get())->CASFileSystemRootID =
          rootID;
    else
      llvm_unreachable("Unexpected module dependency kind");
  }

  llvm::StringSet<> &getVisibleClangModules() const {
    return storage->visibleClangModules;
  }

  void
  addVisibleClangModules(const std::vector<std::string> &moduleNames) const {
    storage->visibleClangModules.insert(moduleNames.begin(),
                                        moduleNames.end());
  }

  /// Whether explicit input paths of all the module dependencies
  /// have been specified on the command-line recipe for this module.
  bool isFinalized() const { return storage->finalized; }
  void setIsFinalized(bool isFinalized) { storage->finalized = isFinalized; }

  /// For a Source dependency, register a `Testable` import
  void addTestableImport(ImportPath::Module module);

  /// Whether or not a queried module name is a `@Testable` import dependency
  /// of this module. Can only return `true` for Swift source modules.
  bool isTestableImport(StringRef moduleName) const;

  /// Whether the dependencies are for a Swift module: either Textual, Source,
  /// or Binary
  bool isSwiftModule() const;

  /// Whether the dependencies are for a textual interface Swift module or a
  /// Source Swift module.
  bool isTextualSwiftModule() const;

  /// Whether the dependencies are for a textual Swift module.
  bool isSwiftInterfaceModule() const;

  /// Whether the dependencies are for a textual Swift module.
  bool isSwiftSourceModule() const;

  /// Whether the dependencies are for a binary Swift module.
  bool isSwiftBinaryModule() const;

  /// Whether the dependencies are for a Clang module.
  bool isClangModule() const;

  ModuleDependencyKind getKind() const { return storage->dependencyKind; }

  /// Retrieve the dependencies for a Swift textual-interface module.
  const SwiftInterfaceModuleDependenciesStorage *
  getAsSwiftInterfaceModule() const;

  /// Retrieve the dependencies for a Swift module.
  const SwiftSourceModuleDependenciesStorage *getAsSwiftSourceModule() const;

  /// Retrieve the dependencies for a binary Swift module.
  const SwiftBinaryModuleDependencyStorage *getAsSwiftBinaryModule() const;

  /// Retrieve the dependencies for a Clang module.
  const ClangModuleDependencyStorage *getAsClangModule() const;

  /// Get the path to the module-defining file:
  /// `SwiftInterface`: Textual Interface path
  /// `SwiftBinary`: Binary module path
  /// `Clang`: Module map path
  std::string getModuleDefiningPath() const;

  /// Add a dependency on the given module, if it was not already in the set.
  void
  addOptionalModuleImport(StringRef module, bool isExported,
                          AccessLevel accessLevel,
                          llvm::StringSet<> *alreadyAddedModules = nullptr);

  /// Add all of the module imports in the given source
  /// file to the set of module imports.
  void addModuleImports(const SourceFile &sourceFile,
                        llvm::StringSet<> &alreadyAddedModules,
                        const SourceManager *sourceManager);

  /// Add a dependency on the given module, if it was not already in the set.
  void addModuleImport(ImportPath::Module module, bool isExported,
                       AccessLevel accessLevel,
                       llvm::StringSet<> *alreadyAddedModules = nullptr,
                       const SourceManager *sourceManager = nullptr,
                       SourceLoc sourceLocation = SourceLoc());

  /// Add a dependency on the given module, if it was not already in the set.
  void addModuleImport(StringRef module, bool isExported,
                       AccessLevel accessLevel,
                       llvm::StringSet<> *alreadyAddedModules = nullptr,
                       const SourceManager *sourceManager = nullptr,
                       SourceLoc sourceLocation = SourceLoc());

  /// Get the bridging header.
  std::optional<std::string> getBridgingHeader() const;

  /// Get CAS Filesystem RootID.
  std::optional<std::string> getCASFSRootID() const;

  /// Get Clang Include Tree ID.
  std::optional<std::string> getClangIncludeTree() const;

  /// Get bridging header Include Tree ID.
  std::optional<std::string> getBridgingHeaderIncludeTree() const;

  /// Get module output path.
  std::string getModuleOutputPath() const;

  /// Add a bridging header to a Swift module's dependencies.
  void addBridgingHeader(StringRef bridgingHeader);

  /// Add source files
  void addSourceFile(StringRef sourceFile);

  /// Add source files that the header input depends on.
  void setHeaderSourceFiles(const std::vector<std::string> &sourceFiles);

  /// Add bridging header include tree.
  void addBridgingHeaderIncludeTree(StringRef ID);

  /// Set the chained bridging header buffer.
  void setChainedBridgingHeaderBuffer(StringRef path, StringRef buffer);

  /// Set the output path and the context hash.
  void setOutputPathAndHash(StringRef outputPath, StringRef hash);

  /// Collect a map from a secondary module name to a list of cross-import
  /// overlays, when this current module serves as the primary module.
  llvm::StringMap<llvm::SmallSetVector<Identifier, 4>>
  collectCrossImportOverlayNames(
      ASTContext &ctx, StringRef moduleName,
      std::set<std::pair<std::string, std::string>> &overlayFiles) const;
};

using ModuleDependencyVector =
    llvm::SmallVector<std::pair<ModuleDependencyID, ModuleDependencyInfo>, 1>;
using ModuleNameToDependencyMap = llvm::StringMap<ModuleDependencyInfo>;
using ModuleDependenciesKindMap =
    std::unordered_map<ModuleDependencyKind, ModuleNameToDependencyMap,
                       ModuleDependencyKindHash>;

// MARK: SwiftDependencyScanningService
/// A carrier of state shared among possibly multiple invocations of the
/// dependency scanner.
class SwiftDependencyScanningService {
  /// The CASOption created the Scanning Service if used.
  std::optional<clang::CASOptions> CASOpts;

  /// The persistent Clang dependency scanner service
  std::optional<clang::tooling::dependencies::DependencyScanningService>
      ClangScanningService;

  /// Shared state mutual-exclusivity lock
  mutable llvm::sys::SmartMutex<true> ScanningServiceGlobalLock;

public:
  SwiftDependencyScanningService();
  SwiftDependencyScanningService(const SwiftDependencyScanningService &) =
      delete;
  SwiftDependencyScanningService &
  operator=(const SwiftDependencyScanningService &) = delete;
  virtual ~SwiftDependencyScanningService() {}

  /// Setup caching service.
  bool setupCachingDependencyScanningService(CompilerInstance &Instance);

private:
  /// Enforce clients not being allowed to query this cache directly, it must be
  /// wrapped in an instance of `ModuleDependenciesCache`.
  friend class ModuleDependenciesCache;
  friend class ModuleDependencyScanner;
  friend class ModuleDependencyScanningWorker;
};

// MARK: ModuleDependenciesCache
/// This "local" dependencies cache persists only for the duration of a given
/// scanning action, and maintains a store of references to all dependencies
/// found within the current scanning action.
class ModuleDependenciesCache {
private:
  /// Discovered dependencies
  ModuleDependenciesKindMap ModuleDependenciesMap;
  /// Set containing all of the Clang modules that have already been seen.
  llvm::DenseSet<clang::tooling::dependencies::ModuleID> alreadySeenClangModules;
  /// Name of the module under scan
  std::string mainScanModuleName;
  /// The context hash of the current scanning invocation
  std::string scannerContextHash;
  /// The timestamp of the beginning of the scanning query action
  /// using this cache
  const llvm::sys::TimePoint<> scanInitializationTime;

  /// Retrieve the dependencies map that corresponds to the given dependency
  /// kind.
  llvm::StringMap<const ModuleDependencyInfo *> &
  getDependencyReferencesMap(ModuleDependencyKind kind);
  const llvm::StringMap<const ModuleDependencyInfo *> &
  getDependencyReferencesMap(ModuleDependencyKind kind) const;

public:
  ModuleDependenciesCache(const std::string &mainScanModuleName,
                          const std::string &scanningContextHash);
  ModuleDependenciesCache(const ModuleDependenciesCache &) = delete;
  ModuleDependenciesCache &operator=(const ModuleDependenciesCache &) = delete;

public:
  /// Retrieve the dependencies map that corresponds to the given dependency
  /// kind.
  ModuleNameToDependencyMap &getDependenciesMap(ModuleDependencyKind kind);
  const ModuleNameToDependencyMap &getDependenciesMap(ModuleDependencyKind kind) const;

  /// Whether we have cached dependency information for the given module.
  bool hasDependency(const ModuleDependencyID &moduleID) const;
  /// Whether we have cached dependency information for the given module.
  bool hasDependency(StringRef moduleName,
                     std::optional<ModuleDependencyKind> kind) const;
  /// Whether we have cached dependency information for the given module Name.
  bool hasDependency(StringRef moduleName) const;
  /// Whether we have cached dependency information for the given Swift module.
  bool hasSwiftDependency(StringRef moduleName) const;

  const llvm::DenseSet<clang::tooling::dependencies::ModuleID> &
  getAlreadySeenClangModules() const {
    return alreadySeenClangModules;
  }
  void addSeenClangModule(clang::tooling::dependencies::ModuleID newModule) {
    alreadySeenClangModules.insert(newModule);
  }

  /// Query all dependencies
  ModuleDependencyIDSetVector
  getAllDependencies(const ModuleDependencyID &moduleID) const;

  /// Query all Clang module dependencies.
  ModuleDependencyIDSetVector
  getClangDependencies(const ModuleDependencyID &moduleID) const;

  /// Query all directly-imported Swift dependencies
  llvm::ArrayRef<ModuleDependencyID>
  getImportedSwiftDependencies(const ModuleDependencyID &moduleID) const;
  /// Query all directly-imported Clang dependencies
  llvm::ArrayRef<ModuleDependencyID>
  getImportedClangDependencies(const ModuleDependencyID &moduleID) const;
  /// Query all Clang module dependencies of this module's imported (bridging) header
  llvm::ArrayRef<ModuleDependencyID>
  getHeaderClangDependencies(const ModuleDependencyID &moduleID) const;
  /// Query Swift overlay dependencies
  llvm::ArrayRef<ModuleDependencyID>
  getSwiftOverlayDependencies(const ModuleDependencyID &moduleID) const;
  /// Query all cross-import overlay dependencies
  llvm::ArrayRef<ModuleDependencyID>
  getCrossImportOverlayDependencies(const ModuleDependencyID &moduleID) const;
  /// Query all visible Clang modules for a given Swift dependency
  llvm::StringSet<>&
  getVisibleClangModules(ModuleDependencyID moduleID) const;

  /// Look for module dependencies for a module with the given ID
  ///
  /// \returns the cached result, or \c None if there is no cached entry.
  std::optional<const ModuleDependencyInfo *>
  findDependency(const ModuleDependencyID moduleID) const;

  /// Look for module dependencies for a module with the given name
  ///
  /// \returns the cached result, or \c None if there is no cached entry.
  std::optional<const ModuleDependencyInfo *>
  findDependency(StringRef moduleName,
                 std::optional<ModuleDependencyKind> kind = std::nullopt) const;

  /// Look for Swift module dependencies for a module with the given name
  ///
  /// \returns the cached result, or \c None if there is no cached entry.
  std::optional<const ModuleDependencyInfo *>
  findSwiftDependency(StringRef moduleName) const;

  /// Look for known existing dependencies.
  ///
  /// \returns the cached result.
  const ModuleDependencyInfo &
  findKnownDependency(const ModuleDependencyID &moduleID) const;

  /// Record dependencies for the given module.
  void recordDependency(StringRef moduleName,
                        ModuleDependencyInfo dependencies);

  /// Record dependencies for the given collection of Clang modules.
  void recordClangDependencies(ModuleDependencyVector moduleDependencies,
                               DiagnosticEngine &diags);

  /// Update stored dependencies for the given module.
  void updateDependency(ModuleDependencyID moduleID,
                        ModuleDependencyInfo dependencyInfo);
  
  /// Remove a given dependency info from the cache.
  void removeDependency(ModuleDependencyID moduleID);

  /// Resolve this module's set of directly-imported Swift module
  /// dependencies
  void
  setImportedSwiftDependencies(ModuleDependencyID moduleID,
                               const ArrayRef<ModuleDependencyID> dependencyIDs);
  /// Resolve this module's set of directly-imported Clang module
  /// dependencies
  void
  setImportedClangDependencies(ModuleDependencyID moduleID,
                               const ArrayRef<ModuleDependencyID> dependencyIDs);
  /// Resolve this module's set of Swift module dependencies
  /// that are Swift overlays of Clang module dependencies.
  void
  setSwiftOverlayDependencies(ModuleDependencyID moduleID,
                              const ArrayRef<ModuleDependencyID> dependencyIDs);
  /// Resolve this Swift module's imported (bridging) header's
  /// Clang module dependencies
  void
  setHeaderClangDependencies(ModuleDependencyID moduleID,
                             const ArrayRef<ModuleDependencyID> dependencyIDs);
  /// Resolve this module's cross-import overlay dependencies
  void
  setCrossImportOverlayDependencies(ModuleDependencyID moduleID,
                                    const ArrayRef<ModuleDependencyID> dependencyIDs);
  /// Add to this module's set of visible Clang modules
  void
  addVisibleClangModules(ModuleDependencyID moduleID,
                         const std::vector<std::string> &moduleNames);

  StringRef getMainModuleName() const { return mainScanModuleName; }

private:
  friend class ModuleDependenciesCacheDeserializer;
  friend class ModuleDependenciesCacheSerializer;
};
} // namespace swift

namespace std {
template <>
struct hash<swift::ModuleDependencyID> {
  std::size_t operator()(const swift::ModuleDependencyID &id) const {
    return llvm::hash_combine(id.ModuleName, id.Kind);
  }
};
} // namespace std

#endif /* SWIFT_AST_MODULE_DEPENDENCIES_H */
