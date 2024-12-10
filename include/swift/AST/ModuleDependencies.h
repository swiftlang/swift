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
#include "swift/Basic/CXXStdlibKind.h"
#include "swift/Basic/LLVM.h"
#include "clang/CAS/CASOptions.h"
#include "clang/Tooling/DependencyScanning/DependencyScanningService.h"
#include "clang/Tooling/DependencyScanning/DependencyScanningTool.h"
#include "clang/Tooling/DependencyScanning/ModuleDepCollector.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/IntrusiveRefCntPtr.h"
#include "llvm/ADT/StringSet.h"
#include "llvm/CAS/CASProvidingFileSystem.h"
#include "llvm/CAS/CASReference.h"
#include "llvm/CAS/CachingOnDiskFileSystem.h"
#include "llvm/CAS/ObjectStore.h"
#include "llvm/Support/Error.h"
#include "llvm/Support/Mutex.h"
#include "llvm/Support/PrefixMapper.h"
#include "llvm/Support/VirtualFileSystem.h"
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
  LastKind = SwiftPlaceholder + 1
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
      : importLocations(), importIdentifier(importIdentifier) {}

  ScannerImportStatementInfo(std::string importIdentifier,
                             ImportDiagnosticLocationInfo location)
      : importLocations({location}), importIdentifier(importIdentifier) {}

  void addImportLocation(ImportDiagnosticLocationInfo location) {
    importLocations.push_back(location);
  }

  /// Buffer, line & column number of the import statement
  SmallVector<ImportDiagnosticLocationInfo, 4> importLocations;
  /// Imported module string. e.g. "Foo.Bar" in 'import Foo.Bar'
  std::string importIdentifier;
};

/// Base class for the variant storage of ModuleDependencyInfo.
///
/// This class is mostly an implementation detail for \c ModuleDependencyInfo.
class ModuleDependencyInfoStorageBase {
public:
  const ModuleDependencyKind dependencyKind;

  ModuleDependencyInfoStorageBase(ModuleDependencyKind dependencyKind,
                                  ArrayRef<LinkLibrary> linkLibraries,
                                  StringRef moduleCacheKey = "")
      : dependencyKind(dependencyKind), linkLibraries(linkLibraries),
        moduleCacheKey(moduleCacheKey.str()),
        finalized(false) {}

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

  /// ModuleDependencyInfo is finalized (with all transitive dependencies
  /// and inputs).
  bool finalized;
};

struct CommonSwiftTextualModuleDependencyDetails {
  CommonSwiftTextualModuleDependencyDetails(
      ArrayRef<StringRef> extraPCMArgs, ArrayRef<StringRef> buildCommandLine,
      StringRef CASFileSystemRootID)
      : extraPCMArgs(extraPCMArgs.begin(), extraPCMArgs.end()),
        bridgingHeaderFile(std::nullopt),
        bridgingSourceFiles(), bridgingModuleDependencies(),
        buildCommandLine(buildCommandLine.begin(), buildCommandLine.end()),
        CASFileSystemRootID(CASFileSystemRootID) {}

  /// To build a PCM to be used by this Swift module, we need to append these
  /// arguments to the generic PCM build arguments reported from the dependency
  /// graph.
  const std::vector<std::string> extraPCMArgs;

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
  const std::string moduleOutputPath;

  /// The Swift interface file to be used to generate the module file.
  const std::string swiftInterfaceFile;

  /// Potentially ready-to-use compiled modules for the interface file.
  const std::vector<std::string> compiledModuleCandidates;

  /// The hash value that will be used for the generated module
  const std::string contextHash;

  /// A flag that indicates this dependency is a framework
  const bool isFramework;

  /// A flag that indicates this dependency is associated with a static archive
  const bool isStatic;

  /// Details common to Swift textual (interface or source) modules
  CommonSwiftTextualModuleDependencyDetails textualModuleDetails;

  /// The user module version of this textual module interface.
  const std::string userModuleVersion;

  SwiftInterfaceModuleDependenciesStorage(
      StringRef moduleOutputPath, StringRef swiftInterfaceFile,
      ArrayRef<StringRef> compiledModuleCandidates,
      ArrayRef<StringRef> buildCommandLine, ArrayRef<LinkLibrary> linkLibraries,
      ArrayRef<StringRef> extraPCMArgs, StringRef contextHash, bool isFramework,
      bool isStatic, StringRef RootID, StringRef moduleCacheKey,
      StringRef userModuleVersion)
      : ModuleDependencyInfoStorageBase(ModuleDependencyKind::SwiftInterface,
                                        linkLibraries, moduleCacheKey),
        moduleOutputPath(moduleOutputPath),
        swiftInterfaceFile(swiftInterfaceFile),
        compiledModuleCandidates(compiledModuleCandidates.begin(),
                                 compiledModuleCandidates.end()),
        contextHash(contextHash), isFramework(isFramework), isStatic(isStatic),
        textualModuleDetails(extraPCMArgs, buildCommandLine, RootID),
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

  SwiftSourceModuleDependenciesStorage(
      StringRef RootID, ArrayRef<StringRef> buildCommandLine,
      ArrayRef<StringRef> bridgingHeaderBuildCommandLine,
      ArrayRef<StringRef> extraPCMArgs)
      : ModuleDependencyInfoStorageBase(ModuleDependencyKind::SwiftSource, {}),
        textualModuleDetails(extraPCMArgs, buildCommandLine, RootID),
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
      ArrayRef<LinkLibrary> linkLibraries, StringRef headerImport,
      StringRef definingModuleInterface, bool isFramework, bool isStatic,
      StringRef moduleCacheKey, StringRef userModuleVersion)
      : ModuleDependencyInfoStorageBase(ModuleDependencyKind::SwiftBinary,
                                        moduleImports, optionalModuleImports,
                                        linkLibraries, moduleCacheKey),
        compiledModulePath(compiledModulePath), moduleDocPath(moduleDocPath),
        sourceInfoPath(sourceInfoPath), headerImport(headerImport),
        definingModuleInterfacePath(definingModuleInterface),
        isFramework(isFramework), isStatic(isStatic),
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

  /// (Clang) modules on which the header inputs depend.
  std::vector<ModuleDependencyID> headerModuleDependencies;

  /// A flag that indicates this dependency is a framework
  const bool isFramework;

  /// A flag that indicates this dependency is associated with a static archive
  const bool isStatic;

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

  /// The swift-specific PCM arguments captured by this dependencies object
  /// as found by the scanning action that discovered it
  const std::vector<std::string> capturedPCMArgs;

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
                               ArrayRef<std::string> capturedPCMArgs,
                               ArrayRef<LinkLibrary> linkLibraries,
                               StringRef CASFileSystemRootID,
                               StringRef clangIncludeTreeRoot,
                               StringRef moduleCacheKey, bool IsSystem)
      : ModuleDependencyInfoStorageBase(ModuleDependencyKind::Clang,
                                        linkLibraries, moduleCacheKey),
        pcmOutputPath(pcmOutputPath), mappedPCMPath(mappedPCMPath),
        moduleMapFile(moduleMapFile), contextHash(contextHash),
        buildCommandLine(buildCommandLine), fileDependencies(fileDependencies),
        capturedPCMArgs(capturedPCMArgs),
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

/// Describes an placeholder Swift module dependency module stub.
///
/// This class is mostly an implementation detail for \c ModuleDependencyInfo.

class SwiftPlaceholderModuleDependencyStorage
    : public ModuleDependencyInfoStorageBase {
public:
  SwiftPlaceholderModuleDependencyStorage(StringRef compiledModulePath,
                                          StringRef moduleDocPath,
                                          StringRef sourceInfoPath)
      : ModuleDependencyInfoStorageBase(ModuleDependencyKind::SwiftPlaceholder,
                                        {}),
        compiledModulePath(compiledModulePath), moduleDocPath(moduleDocPath),
        sourceInfoPath(sourceInfoPath) {}

  ModuleDependencyInfoStorageBase *clone() const override {
    return new SwiftPlaceholderModuleDependencyStorage(*this);
  }

  /// The path to the .swiftmodule file.
  const std::string compiledModulePath;

  /// The path to the .swiftModuleDoc file.
  const std::string moduleDocPath;

  /// The path to the .swiftSourceInfo file.
  const std::string sourceInfoPath;

  static bool classof(const ModuleDependencyInfoStorageBase *base) {
    return base->dependencyKind == ModuleDependencyKind::SwiftPlaceholder;
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
      StringRef moduleOutputPath, StringRef swiftInterfaceFile,
      ArrayRef<StringRef> compiledCandidates, ArrayRef<StringRef> buildCommands,
      ArrayRef<LinkLibrary> linkLibraries, ArrayRef<StringRef> extraPCMArgs,
      StringRef contextHash, bool isFramework, bool isStatic,
      StringRef CASFileSystemRootID, StringRef moduleCacheKey,
      StringRef userModuleVersion) {
    return ModuleDependencyInfo(
        std::make_unique<SwiftInterfaceModuleDependenciesStorage>(
            moduleOutputPath, swiftInterfaceFile, compiledCandidates,
            buildCommands, linkLibraries, extraPCMArgs, contextHash,
            isFramework, isStatic, CASFileSystemRootID, moduleCacheKey,
            userModuleVersion));
  }

  /// Describe the module dependencies for a serialized or parsed Swift module.
  static ModuleDependencyInfo forSwiftBinaryModule(
      StringRef compiledModulePath, StringRef moduleDocPath,
      StringRef sourceInfoPath,
      ArrayRef<ScannerImportStatementInfo> moduleImports,
      ArrayRef<ScannerImportStatementInfo> optionalModuleImports,
      ArrayRef<LinkLibrary> linkLibraries, StringRef headerImport,
      StringRef definingModuleInterface, bool isFramework,
      bool isStatic, StringRef moduleCacheKey, StringRef userModuleVer) {
    return ModuleDependencyInfo(
        std::make_unique<SwiftBinaryModuleDependencyStorage>(
            compiledModulePath, moduleDocPath, sourceInfoPath, moduleImports,
            optionalModuleImports, linkLibraries, headerImport,
            definingModuleInterface,isFramework, isStatic, moduleCacheKey,
            userModuleVer));
  }

  /// Describe the main Swift module.
  static ModuleDependencyInfo
  forSwiftSourceModule(const std::string &CASFileSystemRootID,
                       ArrayRef<StringRef> buildCommands,
                       ArrayRef<StringRef> bridgingHeaderBuildCommands,
                       ArrayRef<StringRef> extraPCMArgs) {
    return ModuleDependencyInfo(
        std::make_unique<SwiftSourceModuleDependenciesStorage>(
            CASFileSystemRootID, buildCommands, bridgingHeaderBuildCommands,
            extraPCMArgs));
  }

  /// Describe the module dependencies for a Clang module that can be
  /// built from a module map and headers.
  static ModuleDependencyInfo forClangModule(
      StringRef pcmOutputPath, StringRef mappedPCMPath, StringRef moduleMapFile,
      StringRef contextHash, ArrayRef<std::string> nonPathCommandLine,
      ArrayRef<std::string> fileDependencies,
      ArrayRef<std::string> capturedPCMArgs,
      ArrayRef<LinkLibrary> linkLibraries, StringRef CASFileSystemRootID,
      StringRef clangIncludeTreeRoot, StringRef moduleCacheKey, bool IsSystem) {
    return ModuleDependencyInfo(std::make_unique<ClangModuleDependencyStorage>(
        pcmOutputPath, mappedPCMPath, moduleMapFile, contextHash,
        nonPathCommandLine, fileDependencies, capturedPCMArgs, linkLibraries,
        CASFileSystemRootID, clangIncludeTreeRoot, moduleCacheKey, IsSystem));
  }

  /// Describe a placeholder dependency swift module.
  static ModuleDependencyInfo
  forPlaceholderSwiftModuleStub(StringRef compiledModulePath,
                                StringRef moduleDocPath,
                                StringRef sourceInfoPath) {
    return ModuleDependencyInfo(
        std::make_unique<SwiftPlaceholderModuleDependencyStorage>(
            compiledModulePath, moduleDocPath, sourceInfoPath));
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
  const ArrayRef<ModuleDependencyID> getImportedSwiftDependencies() const {
    return storage->importedSwiftModules;
  }

  void
  setImportedClangDependencies(const ArrayRef<ModuleDependencyID> dependencyIDs) {
    storage->importedClangModules.assign(dependencyIDs.begin(),
                                         dependencyIDs.end());
  }
  const ArrayRef<ModuleDependencyID> getImportedClangDependencies() const {
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
  const ArrayRef<ModuleDependencyID> getHeaderClangDependencies() const {
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
  const ArrayRef<ModuleDependencyID> getSwiftOverlayDependencies() const {
    return storage->swiftOverlayDependencies;
  }

  void
  setCrossImportOverlayDependencies(const ArrayRef<ModuleDependencyID> dependencyIDs) {
    assert(isSwiftModule());
    storage->crossImportOverlayModules.assign(dependencyIDs.begin(),
                                              dependencyIDs.end());
  }
  const ArrayRef<ModuleDependencyID> getCrossImportOverlayDependencies() const {
    return storage->crossImportOverlayModules;
  }

  const ArrayRef<LinkLibrary> getLinkLibraries() const {
    return storage->linkLibraries;
  }

  void
  setLinkLibraries(const ArrayRef<LinkLibrary> linkLibraries) {
    storage->linkLibraries.assign(linkLibraries.begin(), linkLibraries.end());
  }

  bool isStaticLibrary() const {
    if (auto *detail = getAsSwiftInterfaceModule())
      return detail->isStatic;
    if (auto *detail = getAsSwiftBinaryModule())
      return detail->isStatic;
    return false;
  }

  const ArrayRef<std::string> getHeaderInputSourceFiles() const {
    if (auto *detail = getAsSwiftInterfaceModule())
      return detail->textualModuleDetails.bridgingSourceFiles;
    if (auto *detail = getAsSwiftSourceModule())
      return detail->textualModuleDetails.bridgingSourceFiles;
    if (auto *detail = getAsSwiftBinaryModule())
      return detail->headerSourceFiles;
    return {};
  }

  std::vector<std::string> getCommandline() const {
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

  std::vector<std::string> getBridgingHeaderCommandline() const {
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
      llvm_unreachable("Unexpected type");
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
  /// Binary, or Placeholder.
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

  /// Whether this represents a placeholder module stub
  bool isSwiftPlaceholderModule() const;

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

  /// Retrieve the dependencies for a placeholder dependency module stub.
  const SwiftPlaceholderModuleDependencyStorage *
  getAsPlaceholderDependencyModule() const;

  /// Add a dependency on the given module, if it was not already in the set.
  void
  addOptionalModuleImport(StringRef module,
                          llvm::StringSet<> *alreadyAddedModules = nullptr);

  /// Add all of the module imports in the given source
  /// file to the set of module imports.
  void addModuleImports(const SourceFile &sourceFile,
                        llvm::StringSet<> &alreadyAddedModules,
                        const SourceManager *sourceManager);

  /// Add a dependency on the given module, if it was not already in the set.
  void addModuleImport(ImportPath::Module module,
                       llvm::StringSet<> *alreadyAddedModules = nullptr,
                       const SourceManager *sourceManager = nullptr,
                       SourceLoc sourceLocation = SourceLoc());

  /// Add a dependency on the given module, if it was not already in the set.
  void addModuleImport(StringRef module,
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
  void addHeaderSourceFile(StringRef bridgingSourceFile);

  /// Add bridging header include tree.
  void addBridgingHeaderIncludeTree(StringRef ID);

  /// Collect a map from a secondary module name to a list of cross-import
  /// overlays, when this current module serves as the primary module.
  llvm::StringMap<llvm::SmallSetVector<Identifier, 4>>
  collectCrossImportOverlayNames(
      ASTContext &ctx, StringRef moduleName,
      std::vector<std::pair<std::string, std::string>> &overlayFiles) const;
};

using ModuleDependencyVector =
    llvm::SmallVector<std::pair<ModuleDependencyID, ModuleDependencyInfo>, 1>;
using ModuleNameToDependencyMap = llvm::StringMap<ModuleDependencyInfo>;
using ModuleDependenciesKindMap =
    std::unordered_map<ModuleDependencyKind, ModuleNameToDependencyMap,
                       ModuleDependencyKindHash>;

// MARK: SwiftDependencyTracker
/// Track swift dependency
class SwiftDependencyTracker {
public:
  SwiftDependencyTracker(llvm::cas::CachingOnDiskFileSystem &FS,
                         llvm::PrefixMapper *Mapper,
                         const CompilerInvocation &CI);

  void startTracking(bool includeCommonDeps = true);
  void trackFile(const Twine &path);
  llvm::Expected<llvm::cas::ObjectProxy> createTreeFromDependencies();

private:
  llvm::IntrusiveRefCntPtr<llvm::cas::CachingOnDiskFileSystem> FS;
  llvm::PrefixMapper *Mapper;

  struct FileEntry {
    llvm::cas::ObjectRef FileRef;
    size_t Size;

    FileEntry(llvm::cas::ObjectRef FileRef, size_t Size)
        : FileRef(FileRef), Size(Size) {}
  };
  llvm::StringMap<FileEntry> CommonFiles;
  std::map<std::string, FileEntry> TrackedFiles;
};

// MARK: SwiftDependencyScanningService
/// A carrier of state shared among possibly multiple invocations of the
/// dependency scanner.
class SwiftDependencyScanningService {
  /// The CASOption created the Scanning Service if used.
  std::optional<clang::CASOptions> CASOpts;

  /// The persistent Clang dependency scanner service
  std::optional<clang::tooling::dependencies::DependencyScanningService>
      ClangScanningService;

  /// CachingOnDiskFileSystem for dependency tracking.
  llvm::IntrusiveRefCntPtr<llvm::cas::CachingOnDiskFileSystem> CacheFS;

  /// If use clang include tree.
  bool UseClangIncludeTree = false;

  /// CAS ObjectStore Instance.
  std::shared_ptr<llvm::cas::ObjectStore> CAS;

  /// File prefix mapper.
  std::unique_ptr<llvm::PrefixMapper> Mapper;

  /// The global file system cache.
  std::optional<
      clang::tooling::dependencies::DependencyScanningFilesystemSharedCache>
      SharedFilesystemCache;

  /// Shared state mutual-exclusivity lock
  mutable llvm::sys::SmartMutex<true> ScanningServiceGlobalLock;

public:
  SwiftDependencyScanningService();
  SwiftDependencyScanningService(const SwiftDependencyScanningService &) =
      delete;
  SwiftDependencyScanningService &
  operator=(const SwiftDependencyScanningService &) = delete;
  virtual ~SwiftDependencyScanningService() {}

  /// Query the service's filesystem cache
  clang::tooling::dependencies::DependencyScanningFilesystemSharedCache &getSharedCache() {
    assert(SharedFilesystemCache && "Expected a shared cache");
    return *SharedFilesystemCache;
  }

  /// Query the service's filesystem cache
  clang::tooling::dependencies::DependencyScanningFilesystemSharedCache &
  getSharedFilesystemCache() {
    assert(SharedFilesystemCache && "Expected a shared cache");
    return *SharedFilesystemCache;
  }

  llvm::cas::CachingOnDiskFileSystem &getSharedCachingFS() const {
    assert(CacheFS && "Expect CachingOnDiskFileSystem");
    return *CacheFS;
  }

  llvm::cas::ObjectStore &getCAS() const {
    assert(CAS && "Expect CAS available");
    return *CAS;
  }

  std::optional<SwiftDependencyTracker>
  createSwiftDependencyTracker(const CompilerInvocation &CI) {
    if (!CacheFS)
      return std::nullopt;

    return SwiftDependencyTracker(*CacheFS, Mapper.get(), CI);
  }

  llvm::IntrusiveRefCntPtr<llvm::vfs::FileSystem> getClangScanningFS() const {
    if (UseClangIncludeTree)
      return llvm::cas::createCASProvidingFileSystem(
          CAS, llvm::vfs::createPhysicalFileSystem());

    if (CacheFS)
      return CacheFS->createProxyFS();

    return llvm::vfs::createPhysicalFileSystem();
  }

  bool hasPathMapping() const {
    return Mapper && !Mapper->getMappings().empty();
  }
  llvm::PrefixMapper *getPrefixMapper() const { return Mapper.get(); }
  std::string remapPath(StringRef Path) const {
    if (!Mapper)
      return Path.str();
    return Mapper->mapToString(Path);
  }

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
/// scanning action, and wraps an instance of a `SwiftDependencyScanningService`
/// which may carry cached scanning information from prior scanning actions.
/// This cache maintains a store of references to all dependencies found within
/// the current scanning action (with their values stored in the global Cache).
class ModuleDependenciesCache {
private:
  SwiftDependencyScanningService &globalScanningService;
  /// Discovered dependencies
  ModuleDependenciesKindMap ModuleDependenciesMap;
  /// Set containing all of the Clang modules that have already been seen.
  llvm::DenseSet<clang::tooling::dependencies::ModuleID> alreadySeenClangModules;
  /// Name of the module under scan
  std::string mainScanModuleName;
  /// The context hash of the current scanning invocation
  std::string scannerContextHash;
  /// The location of where the built modules will be output to
  std::string moduleOutputPath;

  /// Retrieve the dependencies map that corresponds to the given dependency
  /// kind.
  llvm::StringMap<const ModuleDependencyInfo *> &
  getDependencyReferencesMap(ModuleDependencyKind kind);
  const llvm::StringMap<const ModuleDependencyInfo *> &
  getDependencyReferencesMap(ModuleDependencyKind kind) const;

public:
  ModuleDependenciesCache(SwiftDependencyScanningService &globalScanningService,
                          std::string mainScanModuleName,
                          std::string moduleOutputPath,
                          std::string scanningContextHash);
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

  SwiftDependencyScanningService &getScanService() {
    return globalScanningService;
  }
  const SwiftDependencyScanningService &getScanService() const {
    return globalScanningService;
  }
  const llvm::DenseSet<clang::tooling::dependencies::ModuleID> &
  getAlreadySeenClangModules() const {
    return alreadySeenClangModules;
  }
  void addSeenClangModule(clang::tooling::dependencies::ModuleID newModule) {
    alreadySeenClangModules.insert(newModule);
  }
  std::string getModuleOutputPath() const { return moduleOutputPath; }

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

  /// Record dependencies for the given module collection.
  void recordDependencies(ModuleDependencyVector moduleDependencies);

  /// Update stored dependencies for the given module.
  void updateDependency(ModuleDependencyID moduleID,
                        ModuleDependencyInfo dependencyInfo);

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
