//===--- ModuleDependencyScanner.cpp - Compute module dependencies --------===//
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

#include "swift/AST/ASTContext.h"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/DiagnosticSuppression.h"
#include "swift/AST/DiagnosticsFrontend.h"
#include "swift/AST/DiagnosticsSema.h"
#include "swift/AST/ModuleDependencies.h"
#include "swift/AST/ModuleLoader.h"
#include "swift/AST/PluginLoader.h"
#include "swift/AST/SourceFile.h"
#include "swift/AST/TypeCheckRequests.h"
#include "swift/Basic/Assertions.h"
#include "swift/Basic/FileTypes.h"
#include "swift/Basic/PrettyStackTrace.h"
#include "swift/ClangImporter/ClangImporter.h"
#include "swift/DependencyScan/ModuleDependencyScanner.h"
#include "swift/Frontend/ModuleInterfaceLoader.h"
#include "swift/Serialization/SerializedModuleLoader.h"
#include "swift/Subsystems.h"
#include "llvm/ADT/IntrusiveRefCntPtr.h"
#include "llvm/ADT/SetOperations.h"
#include "llvm/CAS/CachingOnDiskFileSystem.h"
#include "llvm/Support/Error.h"
#include "llvm/Support/Threading.h"
#include "llvm/Support/VersionTuple.h"
#include "llvm/Support/VirtualFileSystem.h"
#include <algorithm>

using namespace swift;

static void findPath_dfs(ModuleDependencyID X, ModuleDependencyID Y,
                         ModuleDependencyIDSet &visited,
                         std::vector<ModuleDependencyID> &stack,
                         std::vector<ModuleDependencyID> &result,
                         const ModuleDependenciesCache &cache) {
  stack.push_back(X);
  if (X == Y) {
    copy(stack.begin(), stack.end(), std::back_inserter(result));
    return;
  }
  visited.insert(X);
  auto optionalNode = cache.findDependency(X);
  auto node = optionalNode.value();
  assert(optionalNode.has_value() && "Expected cache value for dependency.");
  for (const auto &dep : node->getModuleImports()) {
    std::optional<ModuleDependencyKind> lookupKind = std::nullopt;
    // Underlying Clang module needs an explicit lookup to avoid confusing it
    // with the parent Swift module.
    if ((dep.importIdentifier == X.ModuleName && node->isSwiftModule()) ||
        node->isClangModule())
      lookupKind = ModuleDependencyKind::Clang;

    auto optionalDepNode = cache.findDependency(dep.importIdentifier, lookupKind);
    if (!optionalDepNode.has_value())
      continue;
    auto depNode = optionalDepNode.value();
    auto depID = ModuleDependencyID{dep.importIdentifier, depNode->getKind()};
    if (!visited.count(depID)) {
      findPath_dfs(depID, Y, visited, stack, result, cache);
    }
  }
  stack.pop_back();
}

static std::vector<ModuleDependencyID>
findPathToDependency(ModuleDependencyID dependency,
                     const ModuleDependenciesCache &cache) {
  auto mainModuleDep = cache.findDependency(cache.getMainModuleName(),
                                            ModuleDependencyKind::SwiftSource);
  // We may be in a batch scan instance which does not have this dependency
  if (!mainModuleDep.has_value())
    return {};
  auto mainModuleID = ModuleDependencyID{cache.getMainModuleName().str(),
                                         ModuleDependencyKind::SwiftSource};
  auto visited = ModuleDependencyIDSet();
  auto stack = std::vector<ModuleDependencyID>();
  auto dependencyPath = std::vector<ModuleDependencyID>();
  findPath_dfs(mainModuleID, dependency, visited, stack, dependencyPath, cache);
  return dependencyPath;
}

// Diagnose scanner failure and attempt to reconstruct the dependency
// path from the main module to the missing dependency.
static void
diagnoseScannerFailure(const ScannerImportStatementInfo &moduleImport,
                       DiagnosticEngine &Diags,
                       const ModuleDependenciesCache &cache,
                       std::optional<ModuleDependencyID> dependencyOf) {
  SourceLoc importLoc = SourceLoc();
  if (!moduleImport.importLocations.empty()) {
    auto locInfo = moduleImport.importLocations[0];
    importLoc = Diags.SourceMgr.getLocFromExternalSource(locInfo.bufferIdentifier,
                                                         locInfo.lineNumber,
                                                         locInfo.columnNumber);
  }

  Diags.diagnose(importLoc, diag::dependency_scan_module_not_found,
                 moduleImport.importIdentifier);
  if (dependencyOf.has_value()) {
    auto path = findPathToDependency(dependencyOf.value(), cache);
    // We may fail to construct a path in some cases, such as a Swift overlay of
    // a Clang module dependnecy.
    if (path.empty())
      path = {dependencyOf.value()};

    for (auto it = path.rbegin(), end = path.rend(); it != end; ++it) {
      const auto &entry = *it;
      auto optionalEntryNode = cache.findDependency(entry);
      assert(optionalEntryNode.has_value());
      auto entryNode = optionalEntryNode.value();
      std::string moduleFilePath = "";
      bool isClang = false;
      switch (entryNode->getKind()) {
      case swift::ModuleDependencyKind::SwiftSource:
        Diags.diagnose(importLoc, diag::dependency_as_imported_by_main_module,
                       entry.ModuleName);
        continue;
      case swift::ModuleDependencyKind::SwiftInterface:
        moduleFilePath =
            entryNode->getAsSwiftInterfaceModule()->swiftInterfaceFile;
        break;
      case swift::ModuleDependencyKind::SwiftBinary:
        moduleFilePath =
            entryNode->getAsSwiftBinaryModule()->compiledModulePath;
        break;
      case swift::ModuleDependencyKind::SwiftPlaceholder:
        moduleFilePath =
            entryNode->getAsPlaceholderDependencyModule()->compiledModulePath;
        break;
      case swift::ModuleDependencyKind::Clang:
        moduleFilePath = entryNode->getAsClangModule()->moduleMapFile;
        isClang = true;
        break;
      default:
        llvm_unreachable("Unexpected dependency kind");
      }

      Diags.diagnose(importLoc, diag::dependency_as_imported_by,
                     entry.ModuleName, moduleFilePath, isClang);
    }
  }

  if (moduleImport.importLocations.size() > 1) {
    for (size_t i = 1; i < moduleImport.importLocations.size(); ++i) {
      auto locInfo = moduleImport.importLocations[i];
      auto importLoc = Diags.SourceMgr.getLocFromExternalSource(locInfo.bufferIdentifier,
                                                                locInfo.lineNumber,
                                                                locInfo.columnNumber);
      Diags.diagnose(importLoc, diag::unresolved_import_location);
    }
  }
}

static bool isSwiftDependencyKind(ModuleDependencyKind Kind) {
  return Kind == ModuleDependencyKind::SwiftInterface ||
         Kind == ModuleDependencyKind::SwiftSource ||
         Kind == ModuleDependencyKind::SwiftBinary ||
         Kind == ModuleDependencyKind::SwiftPlaceholder;
}

ModuleDependencyScanningWorker::ModuleDependencyScanningWorker(
    SwiftDependencyScanningService &globalScanningService,
    const CompilerInvocation &ScanCompilerInvocation,
    const SILOptions &SILOptions, ASTContext &ScanASTContext,
    swift::DependencyTracker &DependencyTracker, DiagnosticEngine &Diagnostics)
    : clangScanningTool(*globalScanningService.ClangScanningService,
                        globalScanningService.getClangScanningFS()) {
  // Create a scanner-specific Invocation and ASTContext.
  workerCompilerInvocation =
      std::make_unique<CompilerInvocation>(ScanCompilerInvocation);
  workerASTContext = std::unique_ptr<ASTContext>(
      ASTContext::get(workerCompilerInvocation->getLangOptions(),
                      workerCompilerInvocation->getTypeCheckerOptions(),
                      workerCompilerInvocation->getSILOptions(),
                      workerCompilerInvocation->getSearchPathOptions(),
                      workerCompilerInvocation->getClangImporterOptions(),
                      workerCompilerInvocation->getSymbolGraphOptions(),
                      workerCompilerInvocation->getCASOptions(),
                      ScanASTContext.SourceMgr, Diagnostics));
  auto loader = std::make_unique<PluginLoader>(
      *workerASTContext, /*DepTracker=*/nullptr,
      workerCompilerInvocation->getFrontendOptions().DisableSandbox);
  workerASTContext->setPluginLoader(std::move(loader));

  // Configure the interface scanning AST delegate.
  auto ClangModuleCachePath = getModuleCachePathFromClang(
      ScanASTContext.getClangModuleLoader()->getClangInstance());
  auto &FEOpts = workerCompilerInvocation->getFrontendOptions();
  scanningASTDelegate = std::make_unique<InterfaceSubContextDelegateImpl>(
      workerASTContext->SourceMgr, &workerASTContext->Diags,
      workerASTContext->SearchPathOpts, workerASTContext->LangOpts,
      workerASTContext->ClangImporterOpts, workerASTContext->CASOpts, FEOpts,
      /*buildModuleCacheDirIfAbsent*/ false, ClangModuleCachePath,
      FEOpts.PrebuiltModuleCachePath, FEOpts.BackupModuleInterfaceDir,
      FEOpts.SerializeModuleInterfaceDependencyHashes,
      FEOpts.shouldTrackSystemDependencies(), RequireOSSAModules_t(SILOptions));

  // Set up the ClangImporter.
  clangScannerModuleLoader = ClangImporter::create(
      *workerASTContext, workerCompilerInvocation->getPCHHash(),
      &DependencyTracker);
  if (!clangScannerModuleLoader)
    Diagnostics.diagnose(SourceLoc(), diag::error_clang_importer_create_fail);

  // Set up the Swift interface loader for Swift scanning.
  swiftScannerModuleLoader = ModuleInterfaceLoader::create(
      *workerASTContext,
      *static_cast<ModuleInterfaceCheckerImpl *>(
          ScanASTContext.getModuleInterfaceChecker()),
      &DependencyTracker,
      workerCompilerInvocation->getSearchPathOptions().ModuleLoadMode);
}

ModuleDependencyVector
ModuleDependencyScanningWorker::scanFilesystemForModuleDependency(
    Identifier moduleName, const ModuleDependenciesCache &cache,
    bool isTestableImport) {
  // First query a Swift module, otherwise lookup a Clang module
  ModuleDependencyVector moduleDependencies =
      swiftScannerModuleLoader->getModuleDependencies(
          moduleName, cache.getModuleOutputPath(),
          cache.getAlreadySeenClangModules(), clangScanningTool,
          *scanningASTDelegate, cache.getScanService().getPrefixMapper(),
          isTestableImport);

  if (moduleDependencies.empty())
    moduleDependencies = clangScannerModuleLoader->getModuleDependencies(
        moduleName, cache.getModuleOutputPath(),
        cache.getAlreadySeenClangModules(), clangScanningTool,
        *scanningASTDelegate, cache.getScanService().getPrefixMapper(),
        isTestableImport);

  return moduleDependencies;
}

ModuleDependencyVector
ModuleDependencyScanningWorker::scanFilesystemForSwiftModuleDependency(
    Identifier moduleName, const ModuleDependenciesCache &cache,
    bool isTestableImport) {
  return swiftScannerModuleLoader->getModuleDependencies(
      moduleName, cache.getModuleOutputPath(),
      cache.getAlreadySeenClangModules(), clangScanningTool,
      *scanningASTDelegate, cache.getScanService().getPrefixMapper(), isTestableImport);
}

ModuleDependencyVector
ModuleDependencyScanningWorker::scanFilesystemForClangModuleDependency(
    Identifier moduleName, const ModuleDependenciesCache &cache) {
  return clangScannerModuleLoader->getModuleDependencies(
      moduleName, cache.getModuleOutputPath(),
      cache.getAlreadySeenClangModules(), clangScanningTool,
      *scanningASTDelegate, cache.getScanService().getPrefixMapper(), false);
}

template <typename Function, typename... Args>
auto ModuleDependencyScanner::withDependencyScanningWorker(Function &&F,
                                                           Args &&...ArgList) {
  auto getWorker = [this]() -> std::unique_ptr<ModuleDependencyScanningWorker> {
    std::lock_guard<std::mutex> guard(WorkersLock);
    // If we have run out of workers, something has gone wrong as we must never
    // have the number of workers exceeding the size of the thread pool
    // requesting them.
    if (Workers.empty())
      swift_unreachable("Out of Swift dependency scanning workers.");

    // Otherwise, return from the back.
    auto result = std::move(Workers.back());
    Workers.pop_back();
    return result;
  };

  auto releaseWorker =
      [this](std::unique_ptr<ModuleDependencyScanningWorker> &&worker) {
        std::lock_guard<std::mutex> guard(WorkersLock);
        Workers.emplace_front(std::move(worker));
      };

  std::unique_ptr<ModuleDependencyScanningWorker> worker = getWorker();
  auto Task = std::bind(std::forward<Function>(F), worker.get(),
                        std::forward<Args>(ArgList)...);
  auto result = Task();
  releaseWorker(std::move(worker));
  return result;
}

Identifier ModuleDependencyScanner::getModuleImportIdentifier(StringRef moduleName) {
  return ScanASTContext.getIdentifier(moduleName);
}

ModuleDependencyScanner::ModuleDependencyScanner(
    SwiftDependencyScanningService &ScanningService,
    const CompilerInvocation &ScanCompilerInvocation,
    const SILOptions &SILOptions, ASTContext &ScanASTContext,
    swift::DependencyTracker &DependencyTracker, DiagnosticEngine &Diagnostics,
    bool ParallelScan)
    : ScanCompilerInvocation(ScanCompilerInvocation),
      ScanASTContext(ScanASTContext), Diagnostics(Diagnostics),
      NumThreads(ParallelScan
                     ? llvm::hardware_concurrency().compute_thread_count()
                     : 1),
      ScanningThreadPool(llvm::hardware_concurrency(NumThreads)) {
  // TODO: Make num threads configurable
  for (size_t i = 0; i < NumThreads; ++i)
    Workers.emplace_front(std::make_unique<ModuleDependencyScanningWorker>(
        ScanningService, ScanCompilerInvocation, SILOptions, ScanASTContext,
        DependencyTracker, Diagnostics));
}

/// Find all of the imported Clang modules starting with the given module name.
static void findAllImportedClangModules(StringRef moduleName,
                                        ModuleDependenciesCache &cache,
                                        std::vector<std::string> &allModules,
                                        llvm::StringSet<> &knownModules) {
  if (!knownModules.insert(moduleName).second)
    return;
  allModules.push_back(moduleName.str());
  auto moduleID = ModuleDependencyID{moduleName.str(),
                                     ModuleDependencyKind::Clang};
  auto optionalDependencies = cache.findDependency(moduleID);
  if (!optionalDependencies.has_value())
    return;

  for (const auto &dep : cache.getClangDependencies(moduleID))
    findAllImportedClangModules(dep.ModuleName, cache, allModules,
                                knownModules);
}

llvm::ErrorOr<ModuleDependencyInfo>
ModuleDependencyScanner::getMainModuleDependencyInfo(ModuleDecl *mainModule) {
  // Main module file name.
  auto newExt = file_types::getExtension(file_types::TY_SwiftModuleFile);
  llvm::SmallString<32> mainModulePath = mainModule->getName().str();
  llvm::sys::path::replace_extension(mainModulePath, newExt);

  std::string apinotesVer = (llvm::Twine("-fapinotes-swift-version=") +
                             ScanASTContext.LangOpts.EffectiveLanguageVersion
                                 .asAPINotesVersionString())
                                .str();

  // Compute the dependencies of the main module.
  std::vector<StringRef> ExtraPCMArgs = {"-Xcc", apinotesVer};
  if (!ScanASTContext.LangOpts.ClangTarget.has_value())
    ExtraPCMArgs.insert(
        ExtraPCMArgs.begin(),
        {"-Xcc", "-target", "-Xcc", ScanASTContext.LangOpts.Target.str()});

  auto clangImporter =
      static_cast<ClangImporter *>(ScanASTContext.getClangModuleLoader());
  std::vector<std::string> buildArgs;
  if (ScanASTContext.ClangImporterOpts.ClangImporterDirectCC1Scan) {
    buildArgs.push_back("-direct-clang-cc1-module-build");
    for (auto &arg : clangImporter->getSwiftExplicitModuleDirectCC1Args()) {
      buildArgs.push_back("-Xcc");
      buildArgs.push_back(arg);
    }
  }

  llvm::SmallVector<StringRef> buildCommands;
  buildCommands.reserve(buildArgs.size());
  llvm::for_each(buildArgs, [&](const std::string &arg) {
    buildCommands.emplace_back(arg);
  });

  auto mainDependencies = ModuleDependencyInfo::forSwiftSourceModule(
      {}, buildCommands, {}, ExtraPCMArgs);

  if (ScanASTContext.CASOpts.EnableCaching) {
    std::vector<std::string> clangDependencyFiles;
    clangImporter->addClangInvovcationDependencies(clangDependencyFiles);
    llvm::for_each(clangDependencyFiles, [&](std::string &file) {
      mainDependencies.addAuxiliaryFile(file);
    });
  }

  llvm::StringSet<> alreadyAddedModules;
  // Compute Implicit dependencies of the main module
  {
    const auto &importInfo = mainModule->getImplicitImportInfo();
    // Swift standard library.
    switch (importInfo.StdlibKind) {
    case ImplicitStdlibKind::None:
    case ImplicitStdlibKind::Builtin:
      break;

    case ImplicitStdlibKind::Stdlib:
      mainDependencies.addModuleImport("Swift", &alreadyAddedModules);
      break;
    }

    // Add any implicit module names.
    for (const auto &import : importInfo.AdditionalUnloadedImports) {
      mainDependencies.addModuleImport(import.module.getModulePath(),
                                       &alreadyAddedModules,
                                       &ScanASTContext.SourceMgr);
    }

    // Already-loaded, implicitly imported module names.
    for (const auto &import : importInfo.AdditionalImports) {
      mainDependencies.addModuleImport(
          import.module.importedModule->getNameStr(),
          &alreadyAddedModules);
    }

    // Add the bridging header.
    if (!importInfo.BridgingHeaderPath.empty()) {
      mainDependencies.addBridgingHeader(importInfo.BridgingHeaderPath);
    }

    // If we are to import the underlying Clang module of the same name,
    // add a dependency with the same name to trigger the search.
    if (importInfo.ShouldImportUnderlyingModule) {
      mainDependencies.addModuleImport(mainModule->getName().str(),
                                       &alreadyAddedModules);
    }

    // All modules specified with `-embed-tbd-for-module` are treated as
    // implicit dependnecies for this compilation since they are not guaranteed
    // to be impored in the source.
    for (const auto &tbdSymbolModule :
         ScanCompilerInvocation.getTBDGenOptions().embedSymbolsFromModules) {
      mainDependencies.addModuleImport(tbdSymbolModule,
                                       &alreadyAddedModules);
    }
  }

  // Add source-specified `import` dependencies
  {
    for (auto fileUnit : mainModule->getFiles()) {
      auto sourceFile = dyn_cast<SourceFile>(fileUnit);
      if (!sourceFile)
        continue;

      mainDependencies.addModuleImports(*sourceFile,
                                        alreadyAddedModules,
                                        &ScanASTContext.SourceMgr);
    }

    // Pass all the successful canImport checks from the ASTContext as part of
    // build command to main module to ensure frontend gets the same result.
    // This needs to happen after visiting all the top-level decls from all
    // SourceFiles.
    auto buildArgs = mainDependencies.getCommandline();
    mainModule->getASTContext().forEachCanImportVersionCheck(
        [&](StringRef moduleName, const llvm::VersionTuple &Version,
            const llvm::VersionTuple &UnderlyingVersion) {
          if (Version.empty() && UnderlyingVersion.empty()) {
            buildArgs.push_back("-module-can-import");
            buildArgs.push_back(moduleName.str());
          } else {
            buildArgs.push_back("-module-can-import-version");
            buildArgs.push_back(moduleName.str());
            buildArgs.push_back(Version.getAsString());
            buildArgs.push_back(UnderlyingVersion.getAsString());
          }
        });
    mainDependencies.updateCommandLine(buildArgs);
  }    

  return mainDependencies;
}

/// Retrieve the module dependencies for the Clang module with the given name.
std::optional<const ModuleDependencyInfo *>
ModuleDependencyScanner::getNamedClangModuleDependencyInfo(
    StringRef moduleName, ModuleDependenciesCache &cache,
    ModuleDependencyIDSetVector &discoveredClangModules) {
  // Check whether we've cached this result.
  auto moduleID = ModuleDependencyID{moduleName.str(),
                                     ModuleDependencyKind::Clang};
  if (auto found = cache.findDependency(moduleID)) {
    discoveredClangModules.insert(moduleID);
    auto directClangDeps = cache.getImportedClangDependencies(moduleID);
    ModuleDependencyIDSetVector reachableClangModules;
    reachableClangModules.insert(directClangDeps.begin(),
                                 directClangDeps.end());
    for (unsigned currentModuleIdx = 0;
         currentModuleIdx < reachableClangModules.size();
         ++currentModuleIdx) {
      auto moduleID = reachableClangModules[currentModuleIdx];
      auto dependencies =
        cache.findKnownDependency(moduleID).getImportedClangDependencies();
      reachableClangModules.insert(dependencies.begin(), dependencies.end());
    }
    discoveredClangModules.insert(reachableClangModules.begin(),
                                  reachableClangModules.end());
    return found;
  }

  // Otherwise perform filesystem scan
  auto moduleIdentifier = getModuleImportIdentifier(moduleName);
  auto moduleDependencies = withDependencyScanningWorker(
      [&cache, moduleIdentifier](ModuleDependencyScanningWorker *ScanningWorker) {
        return ScanningWorker->scanFilesystemForClangModuleDependency(
            moduleIdentifier, cache);
      });
  if (moduleDependencies.empty())
    return std::nullopt;
  
  for (const auto &dep : moduleDependencies)
    discoveredClangModules.insert(dep.first);

  cache.recordDependencies(moduleDependencies);
  return cache.findDependency(moduleDependencies[0].first);
}

/// Retrieve the module dependencies for the Swift module with the given name.
std::optional<const ModuleDependencyInfo *>
ModuleDependencyScanner::getNamedSwiftModuleDependencyInfo(
    StringRef moduleName, ModuleDependenciesCache &cache) {
  // Check whether we've cached this result.
  if (auto found =
          cache.findDependency(moduleName, ModuleDependencyKind::SwiftSource))
    return found;
  if (auto found = cache.findDependency(moduleName,
                                        ModuleDependencyKind::SwiftInterface))
    return found;
  if (auto found =
          cache.findDependency(moduleName, ModuleDependencyKind::SwiftBinary))
    return found;
  if (auto found = cache.findDependency(moduleName,
                                        ModuleDependencyKind::SwiftPlaceholder))
    return found;

  // Otherwise perform filesystem scan
  auto moduleIdentifier = getModuleImportIdentifier(moduleName);
  auto moduleDependencies = withDependencyScanningWorker(
      [&cache, moduleIdentifier](ModuleDependencyScanningWorker *ScanningWorker) {
        return ScanningWorker->scanFilesystemForSwiftModuleDependency(
            moduleIdentifier, cache);
      });
  if (moduleDependencies.empty())
    return std::nullopt;

  cache.recordDependencies(moduleDependencies);
  return cache.findDependency(moduleDependencies[0].first);
}

static void discoverCrossImportOverlayFiles(
    StringRef mainModuleName, ArrayRef<ModuleDependencyID> allDependencies,
    ModuleDependenciesCache &cache, ASTContext &scanASTContext,
    llvm::SetVector<Identifier> &newOverlays,
    std::vector<std::pair<std::string, std::string>> &overlayFiles) {
  for (auto moduleID : allDependencies) {
    auto moduleName = moduleID.ModuleName;
    // Do not look for overlays of main module under scan
    if (moduleName == mainModuleName)
      continue;

    auto dependencies = cache.findDependency(moduleName, moduleID.Kind).value();
    // Collect a map from secondary module name to cross-import overlay names.
    auto overlayMap = dependencies->collectCrossImportOverlayNames(
        scanASTContext, moduleName, overlayFiles);
    if (overlayMap.empty())
      continue;
    for (const auto &dependencyId : allDependencies) {
      auto moduleName = dependencyId.ModuleName;
      // Do not look for overlays of main module under scan
      if (moduleName == mainModuleName)
        continue;
      // check if any explicitly imported modules can serve as a
      // secondary module, and add the overlay names to the
      // dependencies list.
      for (auto overlayName : overlayMap[moduleName]) {
        if (overlayName.str() != mainModuleName &&
            std::find_if(allDependencies.begin(), allDependencies.end(),
                         [&](ModuleDependencyID Id) {
                           return moduleName == overlayName.str();
                         }) == allDependencies.end()) {
          newOverlays.insert(overlayName);
        }
      }
    }
  }
}

std::vector<ModuleDependencyID>
ModuleDependencyScanner::performDependencyScan(
    ModuleDependencyID rootModuleID, ModuleDependenciesCache &cache) {
  PrettyStackTraceStringAction trace("Performing dependency scan of: ",
                                     rootModuleID.ModuleName);
  // If scanning for an individual Clang module, simply resolve its imports
  if (rootModuleID.Kind == ModuleDependencyKind::Clang) {
    ModuleDependencyIDSetVector discoveredClangModules;
    resolveAllClangModuleDependencies({},
                                      cache, discoveredClangModules);
    return discoveredClangModules.takeVector();
  }
  
  // Resolve all, direct and transitive, imported module dependencies:
  // 1. Imported Swift modules
  // 2. Imported Clang modules
  // 3. Clang module dependencies of bridging headers
  // 4. Swift Overlay modules of imported Clang modules
  //    This may call into 'resolveImportedModuleDependencies'
  //    for the newly-added Swift overlay dependencies.
  ModuleDependencyIDSetVector allModules =
    resolveImportedModuleDependencies(rootModuleID, cache);

  // 5. Resolve cross-import overlays
  // This must only be done for the main source module, since textual and
  // binary Swift modules already encode their dependencies on cross-import overlays
  // with explicit imports.
  if (ScanCompilerInvocation.getLangOptions().EnableCrossImportOverlays)
    discoverCrossImportOverlayDependencies(
        rootModuleID.ModuleName, allModules.getArrayRef().slice(1), cache,
        [&](ModuleDependencyID id) { allModules.insert(id); });

  return allModules.takeVector();
}

ModuleDependencyIDSetVector
ModuleDependencyScanner::resolveImportedModuleDependencies(
    const ModuleDependencyID &rootModuleID, ModuleDependenciesCache &cache) {
  PrettyStackTraceStringAction trace("Resolving transitive closure of dependencies of: ",
                                     rootModuleID.ModuleName);
  ModuleDependencyIDSetVector allModules;

  // Resolve all imports for which a Swift module can be found,
  // transitively, starting at 'rootModuleID'.
  ModuleDependencyIDSetVector discoveredSwiftModules;
  resolveSwiftModuleDependencies(rootModuleID, cache, discoveredSwiftModules);
  allModules.insert(discoveredSwiftModules.begin(),
                    discoveredSwiftModules.end());
  
  ModuleDependencyIDSetVector discoveredClangModules;
  resolveAllClangModuleDependencies(discoveredSwiftModules.getArrayRef(),
                                    cache, discoveredClangModules);
  allModules.insert(discoveredClangModules.begin(),
                    discoveredClangModules.end());
  
  ModuleDependencyIDSetVector discoveredHeaderDependencyClangModules;
  resolveHeaderDependencies(discoveredSwiftModules.getArrayRef(), cache,
                            discoveredHeaderDependencyClangModules);
  allModules.insert(discoveredHeaderDependencyClangModules.begin(),
                    discoveredHeaderDependencyClangModules.end());
  
  ModuleDependencyIDSetVector discoveredSwiftOverlayDependencyModules;
  resolveSwiftOverlayDependencies(discoveredSwiftModules.getArrayRef(), cache,
                                  discoveredSwiftOverlayDependencyModules);
  allModules.insert(discoveredSwiftOverlayDependencyModules.begin(),
                    discoveredSwiftOverlayDependencyModules.end());

  return allModules;
}

void
ModuleDependencyScanner::resolveSwiftModuleDependencies(
    const ModuleDependencyID &rootModuleID, ModuleDependenciesCache &cache,
    ModuleDependencyIDSetVector &allDiscoveredSwiftModules) {
  PrettyStackTraceStringAction trace("Resolving transitive closure of Swift dependencies of: ",
                                     rootModuleID.ModuleName);
  // Clang modules cannot have Swift module dependencies
  if (!isSwiftDependencyKind(rootModuleID.Kind))
    return;

  allDiscoveredSwiftModules.insert(rootModuleID);
  for (unsigned currentModuleIdx = 0;
       currentModuleIdx < allDiscoveredSwiftModules.size();
       ++currentModuleIdx) {
    auto moduleID = allDiscoveredSwiftModules[currentModuleIdx];
    auto moduleDependencyInfo = cache.findKnownDependency(moduleID);

    // If this dependency module's Swift imports are already resolved,
    // we do not need to scan it.
    if (!moduleDependencyInfo.getImportedSwiftDependencies().empty()) {
      for (const auto &dep : moduleDependencyInfo.getImportedSwiftDependencies())
        allDiscoveredSwiftModules.insert(dep);
    } else {
      // Find the Swift dependencies of every module this module directly depends on.
      ModuleDependencyIDSetVector importedSwiftDependencies;
      resolveSwiftImportsForModule(moduleID, cache, importedSwiftDependencies);
      allDiscoveredSwiftModules.insert(importedSwiftDependencies.begin(),
                                       importedSwiftDependencies.end());
    }
  }
  return;
}

void
ModuleDependencyScanner::resolveAllClangModuleDependencies(
    ArrayRef<ModuleDependencyID> swiftModuleDependents,
    ModuleDependenciesCache &cache,
    ModuleDependencyIDSetVector &allDiscoveredClangModules) {
  // Gather all unresolved imports which must correspond to
  // Clang modules (since no Swift module for them was found).
  llvm::StringSet<> unresolvedImportIdentifiers;
  llvm::StringSet<> unresolvedOptionalImportIdentifiers;
  std::unordered_map<ModuleDependencyID, std::vector<ScannerImportStatementInfo>> unresolvedImportsMap;
  std::unordered_map<ModuleDependencyID, std::vector<ScannerImportStatementInfo>> unresolvedOptionalImportsMap;

  for (const auto &moduleID : swiftModuleDependents) {
    auto moduleDependencyInfo = cache.findKnownDependency(moduleID);
    auto unresolvedImports = &unresolvedImportsMap.emplace(moduleID, std::vector<ScannerImportStatementInfo>()).first->second;
    auto unresolvedOptionalImports = &unresolvedOptionalImportsMap.emplace(moduleID, std::vector<ScannerImportStatementInfo>()).first->second;

    // If we have already resolved Clang dependencies for this module,
    // then we have the entire dependency sub-graph already computed for
    // it and ready to be added to 'allDiscoveredClangModules' without
    // additional scanning.
    if (!moduleDependencyInfo.getImportedClangDependencies().empty()) {
      auto directClangDeps = cache.getImportedClangDependencies(moduleID);
      ModuleDependencyIDSetVector reachableClangModules;
      reachableClangModules.insert(directClangDeps.begin(),
                                   directClangDeps.end());
      for (unsigned currentModuleIdx = 0;
           currentModuleIdx < reachableClangModules.size();
           ++currentModuleIdx) {
        auto moduleID = reachableClangModules[currentModuleIdx];
        auto dependencies =
          cache.findKnownDependency(moduleID).getImportedClangDependencies();
        reachableClangModules.insert(dependencies.begin(), dependencies.end());
      }
      allDiscoveredClangModules.insert(reachableClangModules.begin(),
                                       reachableClangModules.end());
      continue;
    } else {
      // We need to query the Clang dependency scanner for this module's
      // unresolved imports
      auto moduleDependencyInfo = cache.findKnownDependency(moduleID);

      // Figure out which imports have already been resolved to module dependencies
      llvm::StringSet<> resolvedImportIdentifiers;
      for (const auto &resolvedDep : moduleDependencyInfo.getImportedSwiftDependencies())
        resolvedImportIdentifiers.insert(resolvedDep.ModuleName);

      for (const auto &depImport : moduleDependencyInfo.getModuleImports())
        if (!resolvedImportIdentifiers.contains(depImport.importIdentifier)) {
          unresolvedImports->push_back(depImport);
          unresolvedImportIdentifiers.insert(depImport.importIdentifier);
        }
      for (const auto &depImport : moduleDependencyInfo.getOptionalModuleImports())
        if (!resolvedImportIdentifiers.contains(depImport.importIdentifier)) {
          unresolvedOptionalImports->push_back(depImport);
          unresolvedOptionalImportIdentifiers.insert(depImport.importIdentifier);
        }
    }
  }

  // Prepare the module lookup result collection
  llvm::StringMap<std::optional<ModuleDependencyVector>> moduleLookupResult;
  for (const auto &unresolvedIdentifier : unresolvedImportIdentifiers)
    moduleLookupResult.insert(
        std::make_pair(unresolvedIdentifier.getKey(), std::nullopt));

  auto scanForClangModuleDependency =
      [this, &cache, &moduleLookupResult](Identifier moduleIdentifier) {
        auto moduleName = moduleIdentifier.str();
        {
          std::lock_guard<std::mutex> guard(WorkersLock);
          if (cache.hasDependency(moduleName, ModuleDependencyKind::Clang))
            return;
        }

        auto moduleDependencies = withDependencyScanningWorker(
            [&cache,
             moduleIdentifier](ModuleDependencyScanningWorker *ScanningWorker) {
              return ScanningWorker->scanFilesystemForClangModuleDependency(
                  moduleIdentifier, cache);
            });

        // Update the `moduleLookupResult` and cache all discovered dependencies
        // so that subsequent queries do not have to call into the scanner
        // if looking for a module that was discovered as a transitive dependency
        // in this scan.
        {
          std::lock_guard<std::mutex> guard(WorkersLock);
          moduleLookupResult.insert_or_assign(moduleName, moduleDependencies);
          if (!moduleDependencies.empty())
            cache.recordDependencies(moduleDependencies);
        }
      };

  // Enque asynchronous lookup tasks
  for (const auto &unresolvedIdentifier : unresolvedImportIdentifiers)
    ScanningThreadPool.async(
        scanForClangModuleDependency,
        getModuleImportIdentifier(unresolvedIdentifier.getKey()));
  for (const auto &unresolvedIdentifier : unresolvedOptionalImportIdentifiers)
    ScanningThreadPool.async(
        scanForClangModuleDependency,
        getModuleImportIdentifier(unresolvedIdentifier.getKey()));
  ScanningThreadPool.wait();

  // Use the computed scan results to update the dependency info
  for (const auto &moduleID : swiftModuleDependents) {
    std::vector<ScannerImportStatementInfo> failedToResolveImports;
    ModuleDependencyIDSetVector importedClangDependencies;
    auto recordResolvedClangModuleImport =
        [&moduleLookupResult, &importedClangDependencies,
         &allDiscoveredClangModules, moduleID,
         &failedToResolveImports](const ScannerImportStatementInfo &moduleImport,
                                  bool optionalImport) {
          auto lookupResult = moduleLookupResult[moduleImport.importIdentifier];
          // The imported module was found in the cache
          if (lookupResult == std::nullopt) {
            importedClangDependencies.insert(
                {moduleImport.importIdentifier, ModuleDependencyKind::Clang});
          } else {
            // Cache discovered module dependencies.
            if (!lookupResult.value().empty()) {
              importedClangDependencies.insert(
                  {moduleImport.importIdentifier, ModuleDependencyKind::Clang});
              // Add the full transitive dependency set
              for (const auto &dep : lookupResult.value())
                allDiscoveredClangModules.insert(dep.first);
            } else if (!optionalImport) {
              // Otherwise, we failed to resolve this dependency. We will try
              // again using the cache after all other imports have been resolved.
              // If that fails too, a scanning failure will be diagnosed.
              failedToResolveImports.push_back(moduleImport);
            }
          }
        };

    for (const auto &unresolvedImport : unresolvedImportsMap[moduleID])
      recordResolvedClangModuleImport(unresolvedImport, false);
    for (const auto &unresolvedImport : unresolvedOptionalImportsMap[moduleID])
      recordResolvedClangModuleImport(unresolvedImport, true);

    // It is possible that a specific import resolution failed because we are attempting to
    // resolve a module which can only be brought in via a modulemap of a
    // different Clang module dependency which is not otherwise on the current
    // search paths. For example, suppose we are scanning a `.swiftinterface` for
    // module `Foo`, which contains:
    // -----
    // @_exported import Foo
    // import Bar
    // ...
    // -----
    // Where `Foo` is the underlying Framework clang module whose .modulemap
    // defines an auxiliary module `Bar`. Because Foo is a framework, its
    // modulemap is under
    // `<some_framework_search_path>/Foo.framework/Modules/module.modulemap`.
    // Which means that lookup of `Bar` alone from Swift will not be able to
    // locate the module in it. However, the lookup of Foo will itself bring in
    // the auxiliary module becuase the Clang scanner instance scanning for clang
    // module Foo will be able to find it in the corresponding framework module's
    // modulemap and register it as a dependency which means it will be registered
    // with the scanner's cache in the step above. To handle such cases, we
    // first add all successfully-resolved modules and (for Clang modules) their
    // transitive dependencies to the cache, and then attempt to re-query imports
    // for which resolution originally failed from the cache. If this fails, then
    // the scanner genuinely failed to resolve this dependency.
    for (const auto &unresolvedImport : failedToResolveImports) {
      auto unresolvedModuleID = ModuleDependencyID{unresolvedImport.importIdentifier,
                                                   ModuleDependencyKind::Clang};
      auto optionalCachedModuleInfo =
        cache.findDependency(unresolvedModuleID);
      if (optionalCachedModuleInfo.has_value())
        importedClangDependencies.insert(unresolvedModuleID);
      else
        diagnoseScannerFailure(unresolvedImport, Diagnostics, cache, moduleID);
    }

    if (!importedClangDependencies.empty())
      cache.setImportedClangDependencies(moduleID, importedClangDependencies.takeVector());
  }

  return;
}

void
ModuleDependencyScanner::resolveHeaderDependencies(
    ArrayRef<ModuleDependencyID> allSwiftModules,
    ModuleDependenciesCache &cache,
    ModuleDependencyIDSetVector &allDiscoveredHeaderDependencyClangModules) {
  for (const auto &moduleID : allSwiftModules) {
    auto moduleDependencyInfo = cache.findKnownDependency(moduleID);
    if (!moduleDependencyInfo.getHeaderClangDependencies().empty()) {
      allDiscoveredHeaderDependencyClangModules.insert(moduleDependencyInfo.getImportedSwiftDependencies().begin(),
                                                       moduleDependencyInfo.getImportedSwiftDependencies().end());
    } else {
      ModuleDependencyIDSetVector headerClangModuleDependencies;
      resolveHeaderDependenciesForModule(moduleID, cache, headerClangModuleDependencies);
      allDiscoveredHeaderDependencyClangModules.insert(headerClangModuleDependencies.begin(),
                                                       headerClangModuleDependencies.end());
    }
  }
}

void
ModuleDependencyScanner::resolveSwiftOverlayDependencies(
    ArrayRef<ModuleDependencyID> allSwiftModules,
    ModuleDependenciesCache &cache,
    ModuleDependencyIDSetVector &allDiscoveredDependencies) {
  ModuleDependencyIDSetVector discoveredSwiftOverlays;
  for (const auto &moduleID : allSwiftModules) {
    auto moduleDependencyInfo = cache.findKnownDependency(moduleID);
    if (!moduleDependencyInfo.getSwiftOverlayDependencies().empty()) {
      allDiscoveredDependencies.insert(moduleDependencyInfo.getSwiftOverlayDependencies().begin(),
                                       moduleDependencyInfo.getSwiftOverlayDependencies().end());
    } else {
      ModuleDependencyIDSetVector swiftOverlayDependencies;
      resolveSwiftOverlayDependenciesForModule(moduleID, cache, swiftOverlayDependencies);
      discoveredSwiftOverlays.insert(swiftOverlayDependencies.begin(),
                                     swiftOverlayDependencies.end());
    }
  }

  // For each additional Swift overlay dependency, ensure we perform a full scan
  // in case it itself has unresolved module dependencies.
  for (const auto &overlayDepID : discoveredSwiftOverlays) {
    ModuleDependencyIDSetVector allNewModules =
      resolveImportedModuleDependencies(overlayDepID, cache);
    allDiscoveredDependencies.insert(allNewModules.begin(),
                                     allNewModules.end());
  }

  allDiscoveredDependencies.insert(discoveredSwiftOverlays.begin(),
                                   discoveredSwiftOverlays.end());
}

void ModuleDependencyScanner::resolveSwiftImportsForModule(
    const ModuleDependencyID &moduleID, ModuleDependenciesCache &cache,
    ModuleDependencyIDSetVector &importedSwiftDependencies) {
  PrettyStackTraceStringAction trace("Resolving Swift imports of: ",
                                     moduleID.ModuleName);
  if (!isSwiftDependencyKind(moduleID.Kind))
    return;

  auto moduleDependencyInfo = cache.findKnownDependency(moduleID);
  llvm::StringMap<std::optional<ModuleDependencyVector>> moduleLookupResult;
  for (const auto &dependsOn : moduleDependencyInfo.getModuleImports())
    moduleLookupResult.insert(
        std::make_pair(dependsOn.importIdentifier, std::nullopt));

  // A scanning task to query a module by-name. If the module already exists
  // in the cache, do nothing and return.
  auto scanForSwiftModuleDependency =
      [this, &cache, &moduleLookupResult](Identifier moduleIdentifier,
                                          bool isTestable) {
        auto moduleName = moduleIdentifier.str().str();
        // If this is already in the cache, no work to do here
        if (cache.hasSwiftDependency(moduleName))
          return;

        auto moduleDependencies = withDependencyScanningWorker(
            [&cache, moduleIdentifier,
             isTestable](ModuleDependencyScanningWorker *ScanningWorker) {
              return ScanningWorker->scanFilesystemForSwiftModuleDependency(
                  moduleIdentifier, cache, isTestable);
            });
        moduleLookupResult.insert_or_assign(moduleName, moduleDependencies);
      };

  // Enque asynchronous lookup tasks
  for (const auto &dependsOn : moduleDependencyInfo.getModuleImports()) {
    // Avoid querying the underlying Clang module here
    if (moduleID.ModuleName == dependsOn.importIdentifier)
      continue;
    ScanningThreadPool.async(
        scanForSwiftModuleDependency,
        getModuleImportIdentifier(dependsOn.importIdentifier),
        moduleDependencyInfo.isTestableImport(dependsOn.importIdentifier));
  }
  for (const auto &dependsOn :
       moduleDependencyInfo.getOptionalModuleImports()) {
    // Avoid querying the underlying Clang module here
    if (moduleID.ModuleName == dependsOn.importIdentifier)
      continue;
    ScanningThreadPool.async(
        scanForSwiftModuleDependency,
        getModuleImportIdentifier(dependsOn.importIdentifier),
        moduleDependencyInfo.isTestableImport(dependsOn.importIdentifier));
  }
  ScanningThreadPool.wait();

  auto recordResolvedModuleImport =
      [&cache, &moduleLookupResult, &importedSwiftDependencies,
       moduleID](const ScannerImportStatementInfo &moduleImport) {
        if (moduleID.ModuleName == moduleImport.importIdentifier)
          return;
        auto lookupResult = moduleLookupResult[moduleImport.importIdentifier];
        // The imported module was found in the cache
        if (lookupResult == std::nullopt) {
          auto cachedInfo = cache.findSwiftDependency(moduleImport.importIdentifier);
          if (cachedInfo.has_value())
            importedSwiftDependencies.insert(
              {moduleImport.importIdentifier, cachedInfo.value()->getKind()});
        } else {
          // Cache discovered module dependencies.
          if (!lookupResult.value().empty()) {
            cache.recordDependencies(lookupResult.value());
            importedSwiftDependencies.insert({moduleImport.importIdentifier,
                                              lookupResult.value()[0].first.Kind});
          }
        }
      };

  for (const auto &importInfo : moduleDependencyInfo.getModuleImports())
    recordResolvedModuleImport(importInfo);
  for (const auto &importInfo : moduleDependencyInfo.getOptionalModuleImports())
    recordResolvedModuleImport(importInfo);

  // Resolve the dependency info with Swift dependency module information.
  cache.setImportedSwiftDependencies(moduleID,
                                     importedSwiftDependencies.getArrayRef());
}

void ModuleDependencyScanner::resolveHeaderDependenciesForModule(
    const ModuleDependencyID &moduleID, ModuleDependenciesCache &cache,
    ModuleDependencyIDSetVector &headerClangModuleDependencies) {
  PrettyStackTraceStringAction trace("Resolving header dependencies of Swift module",
                                     moduleID.ModuleName);
  std::vector<std::string> allClangModules;
  llvm::StringSet<> alreadyKnownModules;
  auto moduleDependencyInfo = cache.findKnownDependency(moduleID);

  bool isTextualModuleWithABridgingHeader =
      moduleDependencyInfo.isTextualSwiftModule() &&
      moduleDependencyInfo.getBridgingHeader();
  bool isBinaryModuleWithHeaderInput =
      moduleDependencyInfo.isSwiftBinaryModule() &&
      !moduleDependencyInfo.getAsSwiftBinaryModule()->headerImport.empty();

  if (isTextualModuleWithABridgingHeader || isBinaryModuleWithHeaderInput) {
    withDependencyScanningWorker([&](ModuleDependencyScanningWorker
                                         *ScanningWorker) {
      auto clangImporter = static_cast<ClangImporter *>(
          ScanningWorker->clangScannerModuleLoader.get());

      std::vector<std::string> headerFileInputs;
      std::optional<std::string> includeTreeID;
      std::vector<std::string> bridgingHeaderCommandLine;
      auto headerScan = clangImporter->getHeaderDependencies(moduleID, ScanningWorker->clangScanningTool, cache,
                                                             headerClangModuleDependencies, headerFileInputs,
                                                             bridgingHeaderCommandLine, includeTreeID);
      if (!headerScan) {
        // Record direct header Clang dependencies
        cache.setHeaderClangDependencies(moduleID, headerClangModuleDependencies.getArrayRef());
        // Record include Tree ID
        if (includeTreeID.has_value())
          moduleDependencyInfo.addBridgingHeaderIncludeTree(*includeTreeID);
        // Record the bridging header command line
        if (isTextualModuleWithABridgingHeader)
          moduleDependencyInfo.updateBridgingHeaderCommandLine(bridgingHeaderCommandLine);
        for (const auto &headerInput : headerFileInputs)
          moduleDependencyInfo.addHeaderSourceFile(headerInput);
        // Update the dependency in the cache
        cache.updateDependency(moduleID, moduleDependencyInfo);
      } else {
        // Failure to scan header
      }
      return true;
    });
    cache.setHeaderClangDependencies(moduleID, headerClangModuleDependencies.getArrayRef());
  }
}

void ModuleDependencyScanner::resolveSwiftOverlayDependenciesForModule(
    const ModuleDependencyID &moduleID,
    ModuleDependenciesCache &cache,
    ModuleDependencyIDSetVector &swiftOverlayDependencies) {
  PrettyStackTraceStringAction trace("Resolving Swift Overlay dependencies of module",
                                     moduleID.ModuleName);
  std::vector<std::string> allClangDependencies;
  llvm::StringSet<> knownModules;

  // Find all of the discovered Clang modules that this module depends on.
  for (const auto &dep : cache.getClangDependencies(moduleID))
    findAllImportedClangModules(dep.ModuleName, cache, allClangDependencies,
                                knownModules);

  llvm::StringMap<std::optional<ModuleDependencyVector>>
      swiftOverlayLookupResult;
  for (const auto &clangDep : allClangDependencies)
    swiftOverlayLookupResult.insert(std::make_pair(clangDep, std::nullopt));

  // A scanning task to query a Swift module by-name. If the module already
  // exists in the cache, do nothing and return.
  auto scanForSwiftDependency = [this, &cache, &swiftOverlayLookupResult](
                                    Identifier moduleIdentifier) {
    auto moduleName = moduleIdentifier.str();
    if (cache.hasDependency(moduleName, ModuleDependencyKind::SwiftInterface) ||
        cache.hasDependency(moduleName, ModuleDependencyKind::SwiftBinary) ||
        cache.hasDependency(moduleName, ModuleDependencyKind::SwiftPlaceholder))
      return;

    auto moduleDependencies = withDependencyScanningWorker(
        [&cache,
         moduleIdentifier](ModuleDependencyScanningWorker *ScanningWorker) {
          return ScanningWorker->scanFilesystemForSwiftModuleDependency(
              moduleIdentifier, cache);
        });
    swiftOverlayLookupResult.insert_or_assign(moduleName, moduleDependencies);
  };

  // Enque asynchronous lookup tasks
  for (const auto &clangDep : allClangDependencies)
    ScanningThreadPool.async(scanForSwiftDependency, getModuleImportIdentifier(clangDep));
  ScanningThreadPool.wait();

  // Aggregate both previously-cached and freshly-scanned module results
  auto recordResult = [&cache, &swiftOverlayLookupResult,
                       &swiftOverlayDependencies,
                       moduleID](const std::string &moduleName) {
    auto lookupResult = swiftOverlayLookupResult[moduleName];
    if (moduleName != moduleID.ModuleName) {
      if (lookupResult == std::nullopt) {
        auto cachedInfo = cache.findSwiftDependency(moduleName);
        if (cachedInfo.has_value())
          swiftOverlayDependencies.insert(
            {moduleName, cachedInfo.value()->getKind()});
      } else {
        // Cache discovered module dependencies.
        cache.recordDependencies(lookupResult.value());
        if (!lookupResult.value().empty())
          swiftOverlayDependencies.insert({moduleName, lookupResult.value()[0].first.Kind});
      }
    }
  };
  for (const auto &clangDep : allClangDependencies)
    recordResult(clangDep);

  // C++ Interop requires additional handling
  if (ScanCompilerInvocation.getLangOptions().EnableCXXInterop) {
    for (const auto &clangDepName : allClangDependencies) {
      // If this Clang module is a part of the C++ stdlib, and we haven't loaded
      // the overlay for it so far, it is a split libc++ module (e.g.
      // std_vector). Load the CxxStdlib overlay explicitly.
      const auto &clangDepInfo =
          cache.findDependency(clangDepName, ModuleDependencyKind::Clang)
              .value()
              ->getAsClangModule();
      if (importer::isCxxStdModule(clangDepName, clangDepInfo->IsSystem) &&
          !swiftOverlayDependencies.contains(
              {clangDepName, ModuleDependencyKind::SwiftInterface}) &&
          !swiftOverlayDependencies.contains(
              {clangDepName, ModuleDependencyKind::SwiftBinary})) {
        ScanningThreadPool.async(
            scanForSwiftDependency,
            getModuleImportIdentifier(ScanASTContext.Id_CxxStdlib.str()));
        ScanningThreadPool.wait();
        recordResult(ScanASTContext.Id_CxxStdlib.str().str());
      }
    }
  }

  // Resolve the dependency info with Swift overlay dependency module information.
  cache.setSwiftOverlayDependencies(moduleID, swiftOverlayDependencies.getArrayRef());
}

void ModuleDependencyScanner::discoverCrossImportOverlayDependencies(
    StringRef mainModuleName, ArrayRef<ModuleDependencyID> allDependencies,
    ModuleDependenciesCache &cache,
    llvm::function_ref<void(ModuleDependencyID)> action) {
  // Modules explicitly imported. Only these can be secondary module.
  llvm::SetVector<Identifier> newOverlays;
  std::vector<std::pair<std::string, std::string>> overlayFiles;
  discoverCrossImportOverlayFiles(mainModuleName, allDependencies, cache,
                                  ScanASTContext, newOverlays, overlayFiles);

  // No new cross-import overlays are found, return.
  if (newOverlays.empty())
    return;

  // Construct a dummy main to resolve the newly discovered cross import
  // overlays.
  StringRef dummyMainName = "DummyMainModuleForResolvingCrossImportOverlays";
  auto dummyMainID = ModuleDependencyID{dummyMainName.str(),
                                        ModuleDependencyKind::SwiftSource};
  auto actualMainID = ModuleDependencyID{mainModuleName.str(),
                                         ModuleDependencyKind::SwiftSource};
  auto dummyMainDependencies =
      ModuleDependencyInfo::forSwiftSourceModule({}, {}, {}, {});
  std::for_each(newOverlays.begin(), newOverlays.end(),
                [&](Identifier modName) {
                  dummyMainDependencies.addModuleImport(modName.str());
                });

  // Record the dummy main module's direct dependencies. The dummy main module
  // only directly depend on these newly discovered overlay modules.
  if (cache.findDependency(dummyMainName, ModuleDependencyKind::SwiftSource)) {
    cache.updateDependency(dummyMainID, dummyMainDependencies);
  } else {
    cache.recordDependency(dummyMainName, dummyMainDependencies);
  }

  ModuleDependencyIDSetVector allModules =
    resolveImportedModuleDependencies(dummyMainID, cache);

  // Update main module's dependencies to include these new overlays.
  auto newOverlayDeps = cache.getAllDependencies(dummyMainID);
  cache.setCrossImportOverlayDependencies(actualMainID, newOverlayDeps.getArrayRef());

  // Update the command-line on the main module to
  // disable implicit cross-import overlay search.
  auto mainDep = cache.findKnownDependency(actualMainID);
  auto cmdCopy = mainDep.getCommandline();
  cmdCopy.push_back("-disable-cross-import-overlay-search");
  for (auto &entry : overlayFiles) {
    mainDep.addAuxiliaryFile(entry.second);
    cmdCopy.push_back("-swift-module-cross-import");
    cmdCopy.push_back(entry.first);
    auto overlayPath = cache.getScanService().remapPath(entry.second);
    cmdCopy.push_back(overlayPath);
  }
  mainDep.updateCommandLine(cmdCopy);
  cache.updateDependency(actualMainID, mainDep);

  // Report any discovered modules to the clients, which include all overlays
  // and their dependencies.
  std::for_each(/* +1 to exclude dummy main*/ allModules.begin() + 1,
                allModules.end(), action);
}
