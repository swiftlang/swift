//===--- ModuleDependencyScanner.h - Import Swift modules ------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/AST/ASTContext.h"
#include "swift/AST/Identifier.h"
#include "swift/AST/ModuleDependencies.h"
#include "swift/Frontend/ModuleInterfaceLoader.h"
#include "swift/Serialization/ScanningLoaders.h"
#include "clang/Tooling/DependencyScanning/DependencyScanningTool.h"
#include "llvm/CAS/CASReference.h"
#include "llvm/CAS/CASFileSystem.h"
#include "llvm/Support/ThreadPool.h"

namespace swift {
class DependencyTracker;
}

namespace swift {

/// A callback to lookup module outputs for "-fmodule-file=", "-o" etc.
using LookupModuleOutputCallback = llvm::function_ref<std::string(
    const clang::tooling::dependencies::ModuleDeps &,
    clang::tooling::dependencies::ModuleOutputKind)>;
using RemapPathCallback = llvm::function_ref<std::string(StringRef)>;

/// A map from a module id to a collection of import statement infos.
using ImportStatementInfoMap =
    std::unordered_map<ModuleDependencyID,
                       std::vector<ScannerImportStatementInfo>>;

/// A map from a module ID to a collection of module IDs.
using ModuleIDToModuleIDSetVectorMap =
    std::unordered_map<ModuleDependencyID,
                       ModuleDependencyIDSetVector>;

using ModuleIDImportInfoPair =
    std::pair<ModuleDependencyID, ScannerImportStatementInfo>;

struct ScannerMetrics {
  /// Number of performed queries for a Swift dependency with a given name
  std::atomic<uint32_t> SwiftModuleQueries;
  /// Number of performed queries for a Clang dependency with a given name
  std::atomic<uint32_t> NamedClangModuleQueries;
  /// Number of discovered Clang module dependencies which are directly
  /// imported from a Swift module by-name
  std::atomic<uint32_t> RecordedNamedClangModuleDependencies;
};

class DependencyScannerDiagnosticReporter {
private:
  DependencyScannerDiagnosticReporter(DiagnosticEngine &Diagnostics,
                                      bool EmitScanRemarks);

  /// Diagnose scanner failure and attempt to reconstruct the dependency
  /// path from the main module to the missing dependency
  void diagnoseModuleNotFoundFailure(
      const ScannerImportStatementInfo &moduleImport,
      const ModuleDependenciesCache &cache,
      std::optional<ModuleDependencyID> dependencyOf,
      std::optional<std::pair<ModuleDependencyID, std::string>>
          resolvingSerializedSearchPath,
      std::optional<
          std::vector<SwiftModuleScannerQueryResult::IncompatibleCandidate>>
          foundIncompatibleCandidates = std::nullopt);

  /// Upon query failure, if incompatible binary module
  /// candidates were found, emit a failure diagnostic
  void diagnoseFailureOnOnlyIncompatibleCandidates(
      const ScannerImportStatementInfo &moduleImport,
      const std::vector<SwiftModuleScannerQueryResult::IncompatibleCandidate>
          &candidates,
      const ModuleDependenciesCache &cache,
      std::optional<ModuleDependencyID> dependencyOf);

  /// Emit warnings for each discovered binary Swift module
  /// which was incompatible with the current compilation
  /// when querying \c moduleName
  void warnOnIncompatibleCandidates(
      StringRef moduleName,
      const std::vector<SwiftModuleScannerQueryResult::IncompatibleCandidate>
          &candidates);

  /// If remark emission is enabled, increment the
  /// corresponding metric.
  void registerSwiftModuleQuery();
  void registerNamedClangModuleQuery();
  void registerNamedClangDependency();

  /// Emit various metrics about the current scannig action
  void emitScanMetrics(const ModuleDependenciesCache &cache) const;

  DiagnosticEngine &Diagnostics;
  bool EmitScanRemarks;
  std::unique_ptr<ScannerMetrics> ScanMetrics;
  std::unordered_set<std::string> ReportedMissing;
  // Restrict access to the parent scanner classes.
  friend class ModuleDependencyScanner;
  friend class ModuleDependencyScanningWorker;
};

/// A dependency scanning worker which performs filesystem lookup
/// of a named module dependency.
class ModuleDependencyScanningWorker {
public:
  ModuleDependencyScanningWorker(
      SwiftDependencyScanningService &globalScanningService,
      const CompilerInvocation &ScanCompilerInvocation,
      const SILOptions &SILOptions, ASTContext &ScanASTContext,
      DependencyTracker &DependencyTracker,
      std::shared_ptr<llvm::cas::ObjectStore> CAS,
      std::shared_ptr<llvm::cas::ActionCache> ActionCache,
      DependencyScannerDiagnosticReporter &DiagnosticReporter,
      llvm::PrefixMapper *mapper);

private:
  /// Initialize/finalize the clang compiler scanning tool.
  /// Behind the scenes, the clang scanning tool maintains
  /// a single clang compiler instance to perform all by-name
  /// dependency scans. initializeClangScanningTool() initializes
  /// the clang compiler instance, and returns an error if the
  /// initialization fails. Once successfully initialized,
  /// the same clang compiler instance is reused whenever
  /// scanFilesystemForClangModuleDependency is called,
  /// throughout the lifetime of the ModuleDependencyScanningWorker
  /// instance.
  llvm::Error initializeClangScanningTool();
  llvm::Error finalizeClangScanningTool();

  /// Query dependency information for a named Clang module
  ///
  /// \param moduleName moduel identifier for the query
  ///
  /// \param lookupModuleCallback a callback to compute a client-specific
  /// module-cache-relative output path for discovered Clang module dependencies.
  ///
  /// \param alreadySeenModules a set of module dependencies previously seen
  /// by the scanner, as to avoid processing them all over again
  ///
  /// \returns Clang dependency scanner's \c TranslationUnitDeps result
  std::optional<clang::tooling::dependencies::TranslationUnitDeps>
  scanFilesystemForClangModuleDependency(
      Identifier moduleName,
      LookupModuleOutputCallback lookupModuleCallback,
      const llvm::DenseSet<clang::tooling::dependencies::ModuleID>
          &alreadySeenModules);

  /// Query dependency information for header dependencies
  /// of a binary Swift module.
  ///
  /// \param moduleID the name of the Swift module whose dependency
  /// information will be augmented with information about the given
  /// textual header inputs.
  ///
  /// \param headerPath optional path to the header to be scanned.
  ///
  /// \param sourceBuffer optional in-memory buffer of a header to be scanned.
  ///
  /// \param lookupModuleCallback a callback to compute a client-specific
  /// module-cache-relative output path for discovered Clang module dependencies.
  ///
  /// \param alreadySeenModules a set of module dependencies previously seen
  /// by the scanner, as to avoid processing them all over again
  ///
  /// \returns Clang dependency scanner's \c TranslationUnitDeps result
  std::optional<clang::tooling::dependencies::TranslationUnitDeps>
  scanHeaderDependenciesOfSwiftModule(
      ModuleDependencyID moduleID, std::optional<StringRef> headerPath,
      std::optional<llvm::MemoryBufferRef> sourceBuffer,
      LookupModuleOutputCallback lookupModuleCallback,
      const llvm::DenseSet<clang::tooling::dependencies::ModuleID>
         &alreadySeenModules);

  /// Query dependency information for a named Swift module
  ///
  /// \param moduleName moduel identifier for the query
  ///
  /// \param isTestableImport a boolean flag which indicates whether
  /// this is an @testable dependency
  ///
  /// \returns a struct containing query results
  SwiftModuleScannerQueryResult scanFilesystemForSwiftModuleDependency(
      Identifier moduleName, bool isTestableImport = false);

  /// Store cache entry for include tree.
  llvm::Error
  createCacheKeyForEmbeddedHeader(std::string embeddedHeaderIncludeTree,
                                  std::string chainedHeaderIncludeTree);

  // Worker-specific instance of CompilerInvocation
  std::unique_ptr<CompilerInvocation> workerCompilerInvocation;
  // Worker-specific diagnostic engine
  std::unique_ptr<DiagnosticEngine> workerDiagnosticEngine;
  // Worker-specific instance of ASTContext
  std::unique_ptr<ASTContext> workerASTContext;
  // An AST delegate for interface scanning.
  std::unique_ptr<InterfaceSubContextDelegateImpl> scanningASTDelegate;
  // The Clang scanner tool used by this worker.
  clang::tooling::dependencies::DependencyScanningTool clangScanningTool;
  // Swift and Clang module loaders acting as scanners.
  std::unique_ptr<SwiftModuleScanner> swiftModuleScannerLoader;

  // CAS instance.
  std::shared_ptr<llvm::cas::ObjectStore> CAS;
  std::shared_ptr<llvm::cas::ActionCache> ActionCache;

  // The parent scanner's diagnostic reporter
  DependencyScannerDiagnosticReporter &diagnosticReporter;

  // Base command line invocation for clang scanner queries (both module and header)
  std::vector<std::string> clangScanningBaseCommandLineArgs;
  // Command line invocation for clang by-name module lookups
  std::vector<std::string> clangScanningModuleCommandLineArgs;
  // Clang-specific (-Xcc) command-line flags to include on
  // Swift module compilation commands
  std::vector<std::string> swiftModuleClangCC1CommandLineArgs;
  // Working directory for clang module lookup queries
  std::string clangScanningWorkingDirectoryPath;
  // Restrict access to the parent scanner class.
  friend class ModuleDependencyScanner;
};

// MARK: SwiftDependencyTracker
/// Track swift dependency
class SwiftDependencyTracker {
public:
  SwiftDependencyTracker(std::shared_ptr<llvm::cas::ObjectStore> CAS,
                         llvm::PrefixMapper *Mapper,
                         const CompilerInvocation &CI);
  
  void startTracking(bool includeCommonDeps = true);

  /// Track a file with path.
  /// \returns true if the file is tracked, false if the file doesn't exist.
  bool trackFile(const Twine &path);

  llvm::Expected<llvm::cas::ObjectProxy> createTreeFromDependencies();
  
private:
  llvm::IntrusiveRefCntPtr<llvm::vfs::FileSystem> FS;
  std::shared_ptr<llvm::cas::ObjectStore> CAS;
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

class ModuleDependencyScanner {
public:
  static llvm::ErrorOr<std::unique_ptr<ModuleDependencyScanner>>
  create(SwiftDependencyScanningService &service, CompilerInstance *instance,
         ModuleDependenciesCache &cache);

  ~ModuleDependencyScanner();

  /// Identify the scanner invocation's main module's dependencies
  llvm::ErrorOr<ModuleDependencyInfo>
  getMainModuleDependencyInfo(ModuleDecl *mainModule);

  /// Resolve module dependencies of the given module, computing a full
  /// transitive closure dependency graph.
  std::vector<ModuleDependencyID>
  performDependencyScan(ModuleDependencyID rootModuleID);

  /// How many filesystem lookups were performed by the scanner
  unsigned getNumLookups() { return NumLookups; }

  /// CAS Dependency Tracker.
  std::optional<SwiftDependencyTracker>
  createSwiftDependencyTracker(const CompilerInvocation &CI) {
    if (!CAS)
      return std::nullopt;

    return SwiftDependencyTracker(CAS, PrefixMapper.get(), CI);
  }

  /// PrefixMapper for scanner.
  bool hasPathMapping() const {
    return PrefixMapper && !PrefixMapper->getMappings().empty();
  }
  llvm::PrefixMapper *getPrefixMapper() const { return PrefixMapper.get(); }
  std::string remapPath(StringRef Path) const;

  /// CAS options.
  llvm::cas::ObjectStore &getCAS() const {
    assert(CAS && "Expect CAS available");
    return *CAS;
  }

  llvm::cas::CASBackedFileSystem &getSharedCachingFS() const {
    assert(CacheFS && "Expect CacheFS available");
    return *CacheFS;
  }

private:
  // Private methods that create, initialize and finalize the scanner.
  ModuleDependencyScanner(SwiftDependencyScanningService &ScanningService,
                          ModuleDependenciesCache &Cache,
                          const CompilerInvocation &ScanCompilerInvocation,
                          const SILOptions &SILOptions,
                          ASTContext &ScanASTContext,
                          DependencyTracker &DependencyTracker,
                          std::shared_ptr<llvm::cas::ObjectStore> CAS,
                          std::shared_ptr<llvm::cas::ActionCache> ActionCache,
                          DiagnosticEngine &Diagnostics, bool ParallelScan,
                          bool EmitScanRemarks);
  llvm::Error initializeWorkerClangScanningTool();
  llvm::Error finalizeWorkerClangScanningTool();

  /// Main routine that computes imported module dependency transitive
  /// closure for the given module.
  /// 1. Swift modules imported directly or via another Swift dependency
  /// 2. Clang modules imported directly or via a Swift dependency
  /// 3. Clang modules imported via textual header inputs to Swift modules
  /// (bridging headers)
  /// 4. Swift overlay modules of all of the transitively imported Clang modules
  /// that have one
  ModuleDependencyIDSetVector
  resolveImportedModuleDependencies(
      const ModuleDependencyID &rootModuleID);
  void resolveSwiftModuleDependencies(
      const ModuleDependencyID &rootModuleID,
      ModuleDependencyIDSetVector &discoveredSwiftModules);
  void resolveClangModuleDependencies(
      ArrayRef<ModuleDependencyID> swiftModules,
      ModuleDependencyIDSetVector &discoveredClangModules);
  void resolveHeaderDependencies(
      ArrayRef<ModuleDependencyID> swiftModules,
      ModuleDependencyIDSetVector &discoveredHeaderDependencyClangModules);
  void resolveSwiftOverlayDependencies(
      ArrayRef<ModuleDependencyID> swiftModules,
      ModuleDependencyIDSetVector &discoveredDependencies);

  /// Resolve all of a given module's imports to a Swift module, if one exists.
  void resolveSwiftImportsForModule(
      const ModuleDependencyID &moduleID,
      ModuleDependencyIDSetVector &importedSwiftDependencies);

  /// If a module has a bridging header or other header inputs, execute a
  /// dependency scan on it and record the dependencies.
  void resolveHeaderDependenciesForModule(
      const ModuleDependencyID &moduleID,
      ModuleDependencyIDSetVector &headerClangModuleDependencies);

  /// Resolve all module dependencies comprised of Swift overlays
  /// of this module's Clang module dependencies.
  void resolveSwiftOverlayDependenciesForModule(
      const ModuleDependencyID &moduleID,
      ModuleDependencyIDSetVector &swiftOverlayDependencies);

  /// Identify all cross-import overlay module dependencies of the
  /// source module under scan and apply an action for each.
  void resolveCrossImportOverlayDependencies(
      llvm::function_ref<void(ModuleDependencyID)> action);

  /// Perform Bridging Header Chaining.
  llvm::Error
  performBridgingHeaderChaining(const ModuleDependencyID &rootModuleID,
                                ModuleDependencyIDSetVector &allModules);

  /// Bridge Clang dependency scanner's dependency node
  /// to the Swift scanner's `ModuleDependencyInfo`.
  ModuleDependencyInfo
  bridgeClangModuleDependency(
      const clang::tooling::dependencies::ModuleDeps &clangDependency);

  /// Perform an operation utilizing one of the Scanning workers
  /// available to this scanner.
  template <typename Function, typename... Args>
  auto withDependencyScanningWorker(Function &&F, Args &&...ArgList);

  /// Determine cache-relative output path for a given Clang module
  std::string clangModuleOutputPathLookup(
      const clang::tooling::dependencies::ModuleDeps &clangDep,
      clang::tooling::dependencies::ModuleOutputKind moduleOutputKind) const;

  /// Use the scanner's ASTContext to construct an `Identifier`
  /// for a given module name.
  Identifier getModuleImportIdentifier(StringRef moduleName);

private:
  struct BatchClangModuleLookupResult {
    llvm::StringMap<clang::tooling::dependencies::ModuleDeps>
        discoveredDependencyInfos;
    llvm::StringMap<std::vector<std::string>> visibleModules;
  };

  /// For the provided collection of unresolved imports
  /// belonging to identified Swift dependnecies, execute a parallel
  /// query to the Clang dependency scanner for each import's module identifier.
  void performClangModuleLookup(
      const ImportStatementInfoMap &unresolvedImportsMap,
      const ImportStatementInfoMap &unresolvedOptionalImportsMap,
      BatchClangModuleLookupResult &result);

  /// Given a result of a batch Clang module dependency lookup,
  /// record its results in the cache:
  /// 1. Record all discovered Clang module dependency infos
  ///    in the \c cache.
  /// 1. Update the set of visible Clang modules from each Swift module
  ///    in the \c cache.
  /// 2. Update the total collection of all disovered clang modules
  ///    in \c allDiscoveredClangModules.
  /// 3. Record all import identifiers which the scan failed to resolve
  ///    in \c failedToResolveImports.
  /// 4. Update the set of resolved Clang dependencies for each Swift
  ///    module dependency in \c resolvedClangDependenciesMap.
  void processBatchClangModuleQueryResult(
      const BatchClangModuleLookupResult &lookupResult,
      const ImportStatementInfoMap &unresolvedImportsMap,
      const ImportStatementInfoMap &unresolvedOptionalImportsMap,
      ModuleDependencyIDSetVector &allDiscoveredClangModules,
      std::vector<ModuleIDImportInfoPair> &failedToResolveImports,
      ModuleIDToModuleIDSetVectorMap &resolvedClangDependenciesMap);

  /// Re-query some failed-to-resolve Clang imports from cache
  /// in chance they were brought in as transitive dependencies.
  void reQueryMissedModulesFromCache(
      const std::vector<ModuleIDImportInfoPair> &failedToResolveImports,
      ModuleIDToModuleIDSetVectorMap &resolvedClangDependenciesMap);

  /// Assuming the \c `moduleImport` failed to resolve,
  /// iterate over all binary Swift module dependencies with serialized
  /// search paths and attempt to diagnose if the failed-to-resolve module
  /// can be found on any of them. Returns the path containing
  /// the module, if one is found.
  std::optional<std::pair<ModuleDependencyID, std::string>>
  attemptToFindResolvingSerializedSearchPath(
      const ScannerImportStatementInfo &moduleImport);

private:
  const CompilerInvocation &ScanCompilerInvocation;
  ASTContext &ScanASTContext;
  DependencyScannerDiagnosticReporter ScanDiagnosticReporter;

  /// The location of where the explicitly-built modules will be output to
  std::string ModuleOutputPath;
  /// The location of where the explicitly-built SDK modules will be output to
  std::string SDKModuleOutputPath;
  
  /// Reference to a module dependency cache
  ModuleDependenciesCache &DependencyCache;

  /// The available pool of workers for filesystem module search
  unsigned NumThreads;
  std::list<std::unique_ptr<ModuleDependencyScanningWorker>> Workers;
  llvm::DefaultThreadPool ScanningThreadPool;
  // CAS instance.
  std::shared_ptr<llvm::cas::ObjectStore> CAS;
  std::shared_ptr<llvm::cas::ActionCache> ActionCache;
  /// File prefix mapper.
  std::unique_ptr<llvm::PrefixMapper> PrefixMapper;
  /// CAS file system for loading file content.
  llvm::IntrusiveRefCntPtr<llvm::cas::CASBackedFileSystem> CacheFS;
  /// Protect worker access.
  std::mutex WorkersLock;
  /// Count of filesystem queries performed
  std::atomic<unsigned> NumLookups = 0;
};

} // namespace swift
