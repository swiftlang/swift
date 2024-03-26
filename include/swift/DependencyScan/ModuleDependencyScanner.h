//===--- ModuleDependencyScanner.h - Import Swift modules --------*- C++
//-*-===//
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
#include "swift/Serialization/SerializedModuleLoader.h"
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
      DependencyTracker &DependencyTracker, DiagnosticEngine &diags);

private:
  /// Retrieve the module dependencies for the module with the given name.
  ModuleDependencyVector
  scanFilesystemForModuleDependency(Identifier moduleName,
                                    const ModuleDependenciesCache &cache,
                                    bool isTestableImport = false);

  /// Retrieve the module dependencies for the Clang module with the given name.
  ModuleDependencyVector
  scanFilesystemForClangModuleDependency(Identifier moduleName,
                                         const ModuleDependenciesCache &cache);

  /// Retrieve the module dependencies for the Swift module with the given name.
  ModuleDependencyVector
  scanFilesystemForSwiftModuleDependency(Identifier moduleName,
                                         const ModuleDependenciesCache &cache);

  // An AST delegate for interface scanning.
  std::unique_ptr<InterfaceSubContextDelegateImpl> ScanningASTDelegate;
  // The Clang scanner tool used by this worker.
  clang::tooling::dependencies::DependencyScanningTool clangScanningTool;
  // Swift and Clang module loaders acting as scanners.
  std::unique_ptr<ModuleInterfaceLoader> swiftScannerModuleLoader;
  std::unique_ptr<ClangImporter> clangScannerModuleLoader;
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
                          DiagnosticEngine &diags, bool ParallelScan);

  /// Identify the scanner invocation's main module's dependencies
  llvm::ErrorOr<ModuleDependencyInfo>
  getMainModuleDependencyInfo(ModuleDecl *mainModule);

  /// Resolve module dependencies of the given module, computing a full
  /// transitive closure dependency graph.
  std::vector<ModuleDependencyID>
  getModuleDependencies(ModuleDependencyID moduleID,
                        ModuleDependenciesCache &cache);

  /// Query the module dependency info for the Clang module with the given name.
  /// Explicit by-name lookups are useful for batch mode scanning.
  std::optional<const ModuleDependencyInfo *>
  getNamedClangModuleDependencyInfo(StringRef moduleName,
                                    ModuleDependenciesCache &cache);

  /// Query the module dependency info for the Swift module with the given name.
  /// Explicit by-name lookups are useful for batch mode scanning.
  std::optional<const ModuleDependencyInfo *>
  getNamedSwiftModuleDependencyInfo(StringRef moduleName,
                                    ModuleDependenciesCache &cache);

private:
  /// Resolve the direct dependencies of the given module.
  std::vector<ModuleDependencyID>
  resolveDirectModuleDependencies(ModuleDependencyID moduleID,
                                  ModuleDependenciesCache &cache);

  /// Resolve imported module names of a given module to concrete
  /// modules. If `ParallelScan` is enabled, this operation is multithreaded.
  void
  resolveImportDependencies(const ModuleDependencyID &moduleID,
                            ModuleDependenciesCache &cache,
                            ModuleDependencyIDSetVector &directDependencies);

  /// If a module has a bridging header or other header inputs, execute a dependency scan
  /// on it and record the dependencies.
  void resolveHeaderDependencies(
      const ModuleDependencyID &moduleID, ModuleDependenciesCache &cache,
      std::vector<std::string> &allClangModules,
      llvm::StringSet<> &alreadyKnownModules,
      ModuleDependencyIDSetVector &directDependencies);

  /// Resolve all module dependencies comprised of Swift overlays
  /// of this module's Clang module dependencies.
  void resolveSwiftOverlayDependencies(
      const ModuleDependencyID &moduleID,
      const std::vector<std::string> &clangDependencies,
      ModuleDependenciesCache &cache,
      ModuleDependencyIDSetVector &swiftOverlayDependencies,
      ModuleDependencyIDSetVector &directDependencies);

  /// Identify all cross-import overlay modules of the specified
  /// dependency set and apply an action for each.
  void discoverCrossImportOverlayDependencies(
      StringRef mainModuleName, ArrayRef<ModuleDependencyID> allDependencies,
      ModuleDependenciesCache &cache,
      llvm::function_ref<void(ModuleDependencyID)> action);

  /// Perform an operation utilizing one of the Scanning workers
  /// available to this scanner.
  template <typename Function, typename... Args>
  auto withDependencyScanningWorker(Function &&F, Args &&...ArgList);

  Identifier getModuleImportIdentifier(StringRef moduleName);

private:
  const CompilerInvocation &ScanCompilerInvocation;
  ASTContext &ScanASTContext;
  DiagnosticEngine &Diagnostics;

  /// The available pool of workers for filesystem module search
  unsigned NumThreads;
  std::list<std::unique_ptr<ModuleDependencyScanningWorker>> Workers;
  llvm::ThreadPool ScanningThreadPool;
  /// Protect worker access.
  std::mutex WorkersLock;
};

} // namespace swift
