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
#include "llvm/Support/ThreadPool.h"

namespace swift {
class DependencyTracker;
}

namespace swift {

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
      llvm::PrefixMapper *mapper, DiagnosticEngine &diags);

private:
  /// Retrieve the module dependencies for the Clang module with the given name.
  ClangModuleScannerQueryResult scanFilesystemForClangModuleDependency(
      Identifier moduleName,
      const llvm::DenseSet<clang::tooling::dependencies::ModuleID>
          &alreadySeenModules);

  /// Retrieve the module dependencies for the Swift module with the given name.
  SwiftModuleScannerQueryResult scanFilesystemForSwiftModuleDependency(
      Identifier moduleName, bool isTestableImport = false);

  /// Query dependency information for header dependencies
  /// of a binary Swift module.
  ///
  /// \param moduleID the name of the Swift module whose dependency
  /// information will be augmented with information about the given
  /// textual header inputs.
  ///
  /// \param headerPath the path to the header to be scanned.
  ///
  /// \param clangScanningTool The clang dependency scanner.
  ///
  /// \param cache The module dependencies cache to update, with information
  /// about new Clang modules discovered along the way.
  ///
  /// \returns \c true if an error occurred, \c false otherwise
  bool scanHeaderDependenciesOfSwiftModule(
      const ASTContext &ctx,
      ModuleDependencyID moduleID, std::optional<StringRef> headerPath,
      std::optional<llvm::MemoryBufferRef> sourceBuffer,
      ModuleDependenciesCache &cache,
      ModuleDependencyIDSetVector &headerClangModuleDependencies,
      std::vector<std::string> &headerFileInputs,
      std::vector<std::string> &bridgingHeaderCommandLine,
      std::vector<std::string> &visibleClangModules,
      std::optional<std::string> &includeTreeID);


  /// Store cache entry for include tree.
  llvm::Error
  createCacheKeyForEmbeddedHeader(std::string embeddedHeaderIncludeTree,
                                  std::string chainedHeaderIncludeTree);

  // Worker-specific instance of CompilerInvocation
  std::unique_ptr<CompilerInvocation> workerCompilerInvocation;
  // Worker-specific instance of ASTContext
  std::unique_ptr<ASTContext> workerASTContext;
  // An AST delegate for interface scanning.
  std::unique_ptr<InterfaceSubContextDelegateImpl> scanningASTDelegate;
  // The Clang scanner tool used by this worker.
  clang::tooling::dependencies::DependencyScanningTool clangScanningTool;
  // Swift and Clang module loaders acting as scanners.
  std::unique_ptr<SwiftModuleScanner> swiftModuleScannerLoader;

  /// The location of where the explicitly-built modules will be output to
  std::string moduleOutputPath;
  /// The location of where the explicitly-built SDK modules will be output to
  std::string sdkModuleOutputPath;

  // CAS instance.
  std::shared_ptr<llvm::cas::ObjectStore> CAS;
  std::shared_ptr<llvm::cas::ActionCache> ActionCache;
  /// File prefix mapper.
  llvm::PrefixMapper *PrefixMapper;

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
  void trackFile(const Twine &path);
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

class ModuleDependencyIssueReporter {
private:
  ModuleDependencyIssueReporter(DiagnosticEngine &Diagnostics)
      : Diagnostics(Diagnostics) {}

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

  DiagnosticEngine &Diagnostics;
  std::unordered_set<std::string> ReportedMissing;
  // Restrict access to the parent scanner class.
  friend class ModuleDependencyScanner;
};

class ModuleDependencyScanner {
public:
  ModuleDependencyScanner(SwiftDependencyScanningService &ScanningService,
                          const CompilerInvocation &ScanCompilerInvocation,
                          const SILOptions &SILOptions,
                          ASTContext &ScanASTContext,
                          DependencyTracker &DependencyTracker,
                          std::shared_ptr<llvm::cas::ObjectStore> CAS,
                          std::shared_ptr<llvm::cas::ActionCache> ActionCache,
                          DiagnosticEngine &diags, bool ParallelScan);

  /// Identify the scanner invocation's main module's dependencies
  llvm::ErrorOr<ModuleDependencyInfo>
  getMainModuleDependencyInfo(ModuleDecl *mainModule);

  /// Resolve module dependencies of the given module, computing a full
  /// transitive closure dependency graph.
  std::vector<ModuleDependencyID>
  performDependencyScan(ModuleDependencyID rootModuleID,
                        ModuleDependenciesCache &cache);

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
  std::string remapPath(StringRef Path) const {
    if (!PrefixMapper)
      return Path.str();
    return PrefixMapper->mapToString(Path);
  }

  /// CAS options.
  llvm::cas::ObjectStore &getCAS() const {
    assert(CAS && "Expect CAS available");
    return *CAS;
  }

  llvm::vfs::FileSystem &getSharedCachingFS() const {
    assert(CacheFS && "Expect CacheFS available");
    return *CacheFS;
  }

private:
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
      const ModuleDependencyID &rootModuleID, ModuleDependenciesCache &cache);
  void resolveSwiftModuleDependencies(
      const ModuleDependencyID &rootModuleID, ModuleDependenciesCache &cache,
      ModuleDependencyIDSetVector &discoveredSwiftModules);
  void resolveAllClangModuleDependencies(
      ArrayRef<ModuleDependencyID> swiftModules, ModuleDependenciesCache &cache,
      ModuleDependencyIDSetVector &discoveredClangModules);
  void resolveHeaderDependencies(
      ArrayRef<ModuleDependencyID> swiftModules, ModuleDependenciesCache &cache,
      ModuleDependencyIDSetVector &discoveredHeaderDependencyClangModules);
  void resolveSwiftOverlayDependencies(
      ArrayRef<ModuleDependencyID> swiftModules, ModuleDependenciesCache &cache,
      ModuleDependencyIDSetVector &discoveredDependencies);

  /// Resolve all of a given module's imports to a Swift module, if one exists.
  void resolveSwiftImportsForModule(
      const ModuleDependencyID &moduleID, ModuleDependenciesCache &cache,
      ModuleDependencyIDSetVector &importedSwiftDependencies);

  /// If a module has a bridging header or other header inputs, execute a
  /// dependency scan on it and record the dependencies.
  void resolveHeaderDependenciesForModule(
      const ModuleDependencyID &moduleID, ModuleDependenciesCache &cache,
      ModuleDependencyIDSetVector &headerClangModuleDependencies);

  /// Resolve all module dependencies comprised of Swift overlays
  /// of this module's Clang module dependencies.
  void resolveSwiftOverlayDependenciesForModule(
      const ModuleDependencyID &moduleID, ModuleDependenciesCache &cache,
      ModuleDependencyIDSetVector &swiftOverlayDependencies);

  /// Identify all cross-import overlay module dependencies of the
  /// source module under scan and apply an action for each.
  void resolveCrossImportOverlayDependencies(
      StringRef mainModuleName, ModuleDependenciesCache &cache,
      llvm::function_ref<void(ModuleDependencyID)> action);

  /// Perform Bridging Header Chaining.
  llvm::Error
  performBridgingHeaderChaining(const ModuleDependencyID &rootModuleID,
                                ModuleDependenciesCache &cache,
                                ModuleDependencyIDSetVector &allModules);

  /// Perform an operation utilizing one of the Scanning workers
  /// available to this scanner.
  template <typename Function, typename... Args>
  auto withDependencyScanningWorker(Function &&F, Args &&...ArgList);

  /// Use the scanner's ASTContext to construct an `Identifier`
  /// for a given module name.
  Identifier getModuleImportIdentifier(StringRef moduleName);

  /// Assuming the \c `moduleImport` failed to resolve,
  /// iterate over all binary Swift module dependencies with serialized
  /// search paths and attempt to diagnose if the failed-to-resolve module
  /// can be found on any of them. Returns the path containing
  /// the module, if one is found.
  std::optional<std::pair<ModuleDependencyID, std::string>>
  attemptToFindResolvingSerializedSearchPath(
      const ScannerImportStatementInfo &moduleImport,
      const ModuleDependenciesCache &cache);

private:
  const CompilerInvocation &ScanCompilerInvocation;
  ASTContext &ScanASTContext;
  ModuleDependencyIssueReporter IssueReporter;

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
  llvm::IntrusiveRefCntPtr<llvm::vfs::FileSystem> CacheFS;
  /// Protect worker access.
  std::mutex WorkersLock;
  /// Count of filesystem queries performed
  std::atomic<unsigned> NumLookups = 0;
};

} // namespace swift
