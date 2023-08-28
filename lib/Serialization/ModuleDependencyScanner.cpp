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
#include "swift/AST/SourceFile.h"
#include "swift/AST/TypeCheckRequests.h"
#include "swift/Basic/FileTypes.h"
#include "swift/Basic/PrettyStackTrace.h"
#include "swift/Frontend/ModuleInterfaceLoader.h"
#include "swift/Serialization/SerializedModuleLoader.h"
#include "swift/Serialization/ModuleDependencyScanner.h"
#include "swift/Subsystems.h"
#include "llvm/ADT/IntrusiveRefCntPtr.h"
#include "llvm/ADT/SetOperations.h"
#include "llvm/CAS/CachingOnDiskFileSystem.h"
#include "llvm/Support/Error.h"
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
    llvm::Optional<ModuleDependencyKind> lookupKind = llvm::None;
    // Underlying Clang module needs an explicit lookup to avoid confusing it
    // with the parent Swift module.
    if ((dep == X.ModuleName && node->isSwiftModule()) || node->isClangModule())
      lookupKind = ModuleDependencyKind::Clang;

    auto optionalDepNode = cache.findDependency(dep, lookupKind);
    if (!optionalDepNode.has_value())
      continue;
    auto depNode = optionalDepNode.value();
    auto depID = ModuleDependencyID{dep, depNode->getKind()};
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
diagnoseScannerFailure(StringRef moduleName, DiagnosticEngine &Diags,
                       const ModuleDependenciesCache &cache,
                       llvm::Optional<ModuleDependencyID> dependencyOf) {
  Diags.diagnose(SourceLoc(), diag::dependency_scan_module_not_found,
                 moduleName);
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
        Diags.diagnose(SourceLoc(), diag::dependency_as_imported_by_main_module,
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

      // TODO: Consider turning the module file (interface or modulemap) into
      // the SourceLoc of this diagnostic instead. Ideally with the location of
      // the `import` statement that this dependency originates from.
      Diags.diagnose(SourceLoc(), diag::dependency_as_imported_by,
                     entry.ModuleName, moduleFilePath, isClang);
    }
  }
}

ModuleDependencyScanningWorker::ModuleDependencyScanningWorker(
    SwiftDependencyScanningService &globalScanningService,
    const CompilerInvocation &ScanCompilerInvocation,
    const SILOptions &SILOptions, ASTContext &ScanASTContext,
    swift::DependencyTracker &DependencyTracker, DiagnosticEngine &Diagnostics)
    : Diagnostics(Diagnostics), clangScanningTool(*globalScanningService.ClangScanningService,
                        globalScanningService.getClangScanningFS()) {
  // Configure the interface scanning AST delegate
  auto ClangModuleCachePath = getModuleCachePathFromClang(
      ScanASTContext.getClangModuleLoader()->getClangInstance());
  auto &FEOpts = ScanCompilerInvocation.getFrontendOptions();
  ModuleInterfaceLoaderOptions LoaderOpts(FEOpts);
  ScanningASTDelegate = std::make_unique<InterfaceSubContextDelegateImpl>(
      ScanASTContext.SourceMgr, &ScanASTContext.Diags,
      ScanASTContext.SearchPathOpts, ScanASTContext.LangOpts,
      ScanASTContext.ClangImporterOpts, LoaderOpts,
      /*buildModuleCacheDirIfAbsent*/ false, ClangModuleCachePath,
      FEOpts.PrebuiltModuleCachePath, FEOpts.BackupModuleInterfaceDir,
      FEOpts.SerializeModuleInterfaceDependencyHashes,
      FEOpts.shouldTrackSystemDependencies(), RequireOSSAModules_t(SILOptions));

  // Set up the Clang importer.
  clangScannerModuleLoader = ClangImporter::create(
      ScanASTContext, ScanCompilerInvocation.getPCHHash(), &DependencyTracker);
  if (!clangScannerModuleLoader)
    Diagnostics.diagnose(SourceLoc(), diag::error_clang_importer_create_fail);

  // Set up the Swift interface loader for Swift scanning.
  swiftScannerModuleLoader = ModuleInterfaceLoader::create(
      ScanASTContext,
      *static_cast<ModuleInterfaceCheckerImpl *>(
          ScanASTContext.getModuleInterfaceChecker()),
      &DependencyTracker, ModuleLoadingMode::OnlyInterface);
}

llvm::Optional<const ModuleDependencyInfo *>
ModuleDependencyScanningWorker::scanForModuleDependency(
    StringRef moduleName, ModuleDependenciesCache &cache,
    bool optionalDependencyLookup, bool isTestableImport,
    llvm::Optional<ModuleDependencyID> dependencyOf) {
  // Retrieve the dependencies for this module.
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
  if (auto found =
          cache.findDependency(moduleName, ModuleDependencyKind::Clang))
    return found;

  // First query a Swift module, otherwise lookup a Clang module
  ModuleDependencyVector moduleDependencies =
      swiftScannerModuleLoader->getModuleDependencies(
          moduleName, cache.getModuleOutputPath(),
          cache.getScanService().getCachingFS(),
          cache.getAlreadySeenClangModules(), clangScanningTool,
          *ScanningASTDelegate, isTestableImport);

  if (moduleDependencies.empty())
    moduleDependencies =
        clangScannerModuleLoader->getModuleDependencies(
            moduleName, cache.getModuleOutputPath(),
            cache.getScanService().getCachingFS(),
            cache.getAlreadySeenClangModules(),
            clangScanningTool,
            *ScanningASTDelegate, isTestableImport);

  if (moduleDependencies.empty()) {
    if (!optionalDependencyLookup)
      diagnoseScannerFailure(moduleName, Diagnostics, cache, dependencyOf);
    return llvm::None;
  }

  cache.recordDependencies(moduleDependencies);
  return cache.findDependency(moduleDependencies[0].first);
}

llvm::Optional<const ModuleDependencyInfo *>
ModuleDependencyScanningWorker::scanForSwiftModuleDependency(
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

  ModuleDependencyVector moduleDependencies =
      swiftScannerModuleLoader->getModuleDependencies(
          moduleName, cache.getModuleOutputPath(),
          cache.getScanService().getCachingFS(),
          cache.getAlreadySeenClangModules(), clangScanningTool,
          *ScanningASTDelegate, false);

  if (moduleDependencies.empty())
    return llvm::None;

  cache.recordDependencies(moduleDependencies);
  return cache.findDependency(moduleDependencies[0].first);
}

llvm::Optional<const ModuleDependencyInfo *>
ModuleDependencyScanningWorker::scanForClangModuleDependency(
    StringRef moduleName, ModuleDependenciesCache &cache) {
  // Check whether we've cached this result.
  if (auto found =
          cache.findDependency(moduleName, ModuleDependencyKind::Clang))
    return found;

  ModuleDependencyVector moduleDependencies =
      clangScannerModuleLoader->getModuleDependencies(
          moduleName, cache.getModuleOutputPath(),
          cache.getScanService().getCachingFS(),
          cache.getAlreadySeenClangModules(), clangScanningTool,
          *ScanningASTDelegate, false);

  if (moduleDependencies.empty())
    return llvm::None;

  cache.recordDependencies(moduleDependencies);
  return cache.findDependency(moduleDependencies[0].first);
}

ModuleDependencyScanner::ModuleDependencyScanner(
    SwiftDependencyScanningService &ScanningService,
    const CompilerInvocation &ScanCompilerInvocation,
    const SILOptions &SILOptions, ASTContext &ScanASTContext,
    swift::DependencyTracker &DependencyTracker, DiagnosticEngine &Diagnostics)
    : ScanCompilerInvocation(ScanCompilerInvocation),
      ScanASTContext(ScanASTContext), Diagnostics(Diagnostics),
      ScanningWorker(ScanningService, ScanCompilerInvocation, SILOptions,
                     ScanASTContext, DependencyTracker, Diagnostics) {}

std::vector<ModuleDependencyID>
ModuleDependencyScanner::resolveDependencies(ModuleDependencyID moduleID,
                                             ModuleDependenciesCache &cache) {
  ModuleDependencyIDSetVector allModules;
  allModules.insert(moduleID);
  for (unsigned currentModuleIdx = 0; currentModuleIdx < allModules.size();
       ++currentModuleIdx) {
    auto module = allModules[currentModuleIdx];
    auto discoveredModules = resolveModuleImports(module, cache);

    // Do not need to resolve Clang modules, as they come fully-resolved
    // from the Clang dependency scanner.
    for (const auto &moduleID : discoveredModules)
      if (moduleID.Kind != ModuleDependencyKind::Clang)
        allModules.insert(moduleID);

    allModules.insert(discoveredModules.begin(), discoveredModules.end());
  }

  // Resolve cross-import overlays.
  discoverCrossImportOverlayDependencies(
      moduleID.ModuleName, allModules.getArrayRef().slice(1), cache,
      [&](ModuleDependencyID id) { allModules.insert(id); });

  return allModules.takeVector();
}

/// Find all of the imported Clang modules starting with the given module name.
static void findAllImportedClangModules(StringRef moduleName,
                                        ModuleDependenciesCache &cache,
                                        std::vector<std::string> &allModules,
                                        llvm::StringSet<> &knownModules) {
  if (!knownModules.insert(moduleName).second)
    return;
  allModules.push_back(moduleName.str());
  auto optionalDependencies =
      cache.findDependency(moduleName, ModuleDependencyKind::Clang);
  if (!optionalDependencies.has_value())
    return;

  auto dependencies = optionalDependencies.value();
  for (const auto &dep : dependencies->getDirectModuleDependencies())
    findAllImportedClangModules(dep.ModuleName, cache, allModules,
                                knownModules);
  for (const auto &dep : dependencies->getSwiftOverlayDependencies())
    findAllImportedClangModules(dep.ModuleName, cache, allModules,
                                knownModules);
}

llvm::ErrorOr<ModuleDependencyInfo>
ModuleDependencyScanner::getMainModuleDependencyInfo(
    ModuleDecl *mainModule, llvm::Optional<SwiftDependencyTracker> tracker) {
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

  std::string rootID;
  if (tracker) {
    tracker->startTracking();
    for (auto fileUnit : mainModule->getFiles()) {
      auto sf = dyn_cast<SourceFile>(fileUnit);
      if (!sf)
        continue;
      tracker->trackFile(sf->getFilename());
    }
    tracker->addCommonSearchPathDeps(
        ScanCompilerInvocation.getSearchPathOptions());
    // Fetch some dependency files from clang importer.
    std::vector<std::string> clangDependencyFiles;
    auto clangImporter =
        static_cast<ClangImporter *>(ScanASTContext.getClangModuleLoader());
    clangImporter->addClangInvovcationDependencies(clangDependencyFiles);
    llvm::for_each(clangDependencyFiles,
                   [&](std::string &file) { tracker->trackFile(file); });

    auto root = tracker->createTreeFromDependencies();
    if (!root) {
      Diagnostics.diagnose(SourceLoc(), diag::error_cas,
                           toString(root.takeError()));
      return std::make_error_code(std::errc::io_error);
    }
    rootID = root->getID().toString();
  }

  auto mainDependencies =
      ModuleDependencyInfo::forSwiftSourceModule(rootID, {}, {}, ExtraPCMArgs);

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
                                       &alreadyAddedModules);
    }

    // Already-loaded, implicitly imported module names.
    for (const auto &import : importInfo.AdditionalImports) {
      mainDependencies.addModuleImport(
          import.module.importedModule->getNameStr(), &alreadyAddedModules);
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
      mainDependencies.addModuleImport(tbdSymbolModule, &alreadyAddedModules);
    }
  }

  // Add source-specified `import` dependencies
  {
    for (auto fileUnit : mainModule->getFiles()) {
      auto sf = dyn_cast<SourceFile>(fileUnit);
      if (!sf)
	continue;

      mainDependencies.addModuleImport(*sf, alreadyAddedModules);
    }
  }

  return mainDependencies;
}

/// Retrieve the module dependencies for the Clang module with the given name.
llvm::Optional<const ModuleDependencyInfo *>
ModuleDependencyScanner::scanForClangModuleDependency(StringRef moduleName,
                                                      ModuleDependenciesCache &cache) {
  return ScanningWorker.scanForClangModuleDependency(moduleName, cache);
}

/// Retrieve the module dependencies for the Swift module with the given name.
llvm::Optional<const ModuleDependencyInfo *>
ModuleDependencyScanner::scanForSwiftModuleDependency(StringRef moduleName,
                                                      ModuleDependenciesCache &cache) {
  return ScanningWorker.scanForSwiftModuleDependency(moduleName, cache);
}

std::vector<ModuleDependencyID>
ModuleDependencyScanner::resolveModuleImports(ModuleDependencyID moduleID,
                                              ModuleDependenciesCache &cache) {
  PrettyStackTraceStringAction trace("Resolving dependencies of: ",
                                     moduleID.ModuleName);
  auto optionalKnownDependencies = cache.findDependency(moduleID);
  assert(optionalKnownDependencies.has_value());
  auto knownDependencies = optionalKnownDependencies.value();

  // If this dependency has already been resolved, return the result.
  if (knownDependencies->isResolved() &&
      knownDependencies->getKind() != ModuleDependencyKind::SwiftSource)
    return cache.getAllDependencies(moduleID);

  auto isSwiftInterfaceOrSource = knownDependencies->isSwiftInterfaceModule() ||
                                  knownDependencies->isSwiftSourceModule();
  auto isSwift =
      isSwiftInterfaceOrSource || knownDependencies->isSwiftBinaryModule();
  // Find the dependencies of every module this module directly depends on.
  ModuleDependencyIDSetVector directDependencies;
  ModuleDependencyIDSetVector swiftOverlayDependencies;

  // ACTODO: The right granularity of parallelization is here!
  {
    for (auto dependsOn : knownDependencies->getModuleImports()) {
      // Figure out what kind of module we need.
      bool onlyClangModule = !isSwift || moduleID.ModuleName == dependsOn;
      bool isTestable = knownDependencies->isTestableImport(dependsOn);

      if (onlyClangModule) {
        if (auto found = ScanningWorker.scanForClangModuleDependency(dependsOn, cache))
          directDependencies.insert({dependsOn, ModuleDependencyKind::Clang});
      } else {
        if (auto found = ScanningWorker.scanForModuleDependency(
                dependsOn, cache, /* optionalDependencyLookup */ false,
                isTestable, moduleID))
          directDependencies.insert({dependsOn, found.value()->getKind()});
      }
    }

    // We may have a set of optional dependencies for this module, such as
    // `@_implementationOnly` imports of a `@Testable` import. Attempt to locate
    // those, but do not fail if they cannot be found.
    for (auto optionallyDependsOn :
         knownDependencies->getOptionalModuleImports()) {
      if (auto found = ScanningWorker.scanForModuleDependency(
              optionallyDependsOn, cache,
              /* optionalDependencyLookup */ true,
              /* isTestableDependency */ false, moduleID))
        directDependencies.insert(
            {optionallyDependsOn, found.value()->getKind()});
    }
  }

  if (isSwiftInterfaceOrSource) {
    // A record of all of the Clang modules referenced from this Swift module.
    std::vector<std::string> allClangModules;
    llvm::StringSet<> knownModules;
    auto clangImporter = static_cast<ClangImporter *>(
        ScanningWorker.clangScannerModuleLoader.get());

    // If the Swift module has a bridging header, add those dependencies.
    if (knownDependencies->getBridgingHeader()) {
      if (!clangImporter->addBridgingHeaderDependencies(
              moduleID, ScanningWorker.clangScanningTool, cache)) {
        // Grab the updated module dependencies.
        // FIXME: This is such a hack.
        knownDependencies = *cache.findDependency(moduleID);

        // Add the Clang modules referenced from the bridging header to the
        // set of Clang modules we know about.
        const std::vector<std::string> *bridgingModuleDependencies = nullptr;
        if (auto swiftDeps = knownDependencies->getAsSwiftInterfaceModule())
          bridgingModuleDependencies =
              &(swiftDeps->textualModuleDetails.bridgingModuleDependencies);
        else if (auto sourceDeps = knownDependencies->getAsSwiftSourceModule())
          bridgingModuleDependencies =
              &(sourceDeps->textualModuleDetails.bridgingModuleDependencies);

        assert(bridgingModuleDependencies);
        for (const auto &clangDep : *bridgingModuleDependencies) {
          /// TODO: separate this out of here as well into a separate entry in
          /// `CommonSwiftTextualModuleDependencyDetails`
          directDependencies.insert({clangDep, ModuleDependencyKind::Clang});
          findAllImportedClangModules(clangDep, cache, allClangModules,
                                      knownModules);
        }
      }
    }

    // Find all of the Clang modules this Swift module depends on.
    for (const auto &dep : directDependencies) {
      if (dep.Kind != ModuleDependencyKind::Clang)
        continue;

      findAllImportedClangModules(dep.ModuleName, cache, allClangModules,
                                  knownModules);
    }

    // Look for overlays for each of the Clang modules. The Swift module
    // directly depends on these.
    for (const auto &clangDep : allClangModules) {
      if (auto found = scanForSwiftModuleDependency(clangDep, cache)) {
        if (clangDep != moduleID.ModuleName) {
          swiftOverlayDependencies.insert({clangDep, found.value()->getKind()});
        }
      }
    }
  }

  // Resolve the dependnecy info
  cache.resolveDependencyImports(moduleID, directDependencies.getArrayRef());
  // Resolve swift Overlay dependencies
  if (!swiftOverlayDependencies.empty())
    cache.setSwiftOverlayDependencues(moduleID,
                                      swiftOverlayDependencies.getArrayRef());

  ModuleDependencyIDSetVector result = directDependencies;
  result.insert(swiftOverlayDependencies.begin(),
                swiftOverlayDependencies.end());
  return result.takeVector();
}

void ModuleDependencyScanner::discoverCrossImportOverlayDependencies(
    StringRef mainModuleName, ArrayRef<ModuleDependencyID> allDependencies,
    ModuleDependenciesCache &cache,
    llvm::function_ref<void(ModuleDependencyID)> action) {
  // Modules explicitly imported. Only these can be secondary module.
  llvm::SetVector<Identifier> newOverlays;
  for (auto dep : allDependencies) {
    auto moduleName = dep.ModuleName;

    // Do not look for overlays of main module under scan
    if (moduleName == mainModuleName)
      continue;

    auto dependencies = cache.findDependency(moduleName, dep.Kind).value();

    // Collect a map from secondary module name to cross-import overlay names.
    auto overlayMap = dependencies->collectCrossImportOverlayNames(
        ScanASTContext, moduleName);
    if (overlayMap.empty())
      continue;

    std::for_each(allDependencies.begin(), allDependencies.end(),
                  [&](ModuleDependencyID Id) {
                    auto moduleName = Id.ModuleName;
                    // Do not look for overlays of main module under scan
                    if (moduleName == mainModuleName)
                      return;
                    // check if any explicitly imported modules can serve as a
                    // secondary module, and add the overlay names to the
                    // dependencies list.
                    for (auto overlayName : overlayMap[moduleName]) {
                      if (overlayName.str() != mainModuleName &&
                          std::find_if(allDependencies.begin(),
                                       allDependencies.end(),
                                       [&](ModuleDependencyID Id) {
                                         return moduleName == overlayName.str();
                                       }) == allDependencies.end()) {
                        newOverlays.insert(overlayName);
                      }
                    }
                  });
  }
  // No new cross-import overlays are found, return.
  if (newOverlays.empty())
    return;
  // Construct a dummy main to resolve the newly discovered cross import
  // overlays.
  StringRef dummyMainName = "DummyMainModuleForResolvingCrossImportOverlays";
  auto dummyMainDependencies =
      ModuleDependencyInfo::forSwiftSourceModule({}, {}, {}, {});
  std::for_each(newOverlays.begin(), newOverlays.end(),
                [&](Identifier modName) {
                  dummyMainDependencies.addModuleImport(modName.str());
                });

  // Record the dummy main module's direct dependencies. The dummy main module
  // only directly depend on these newly discovered overlay modules.
  if (cache.findDependency(dummyMainName, ModuleDependencyKind::SwiftSource)) {
    cache.updateDependency(
        ModuleDependencyID{dummyMainName.str(),
                           ModuleDependencyKind::SwiftSource},
        dummyMainDependencies);
  } else {
    cache.recordDependency(dummyMainName, dummyMainDependencies);
  }

  ModuleDependencyIDSetVector allModules;

  // Seed the all module list from the dummy main module.
  allModules.insert({dummyMainName.str(), dummyMainDependencies.getKind()});

  // Explore the dependencies of every module.
  for (unsigned currentModuleIdx = 0; currentModuleIdx < allModules.size();
       ++currentModuleIdx) {
    auto module = allModules[currentModuleIdx];
    auto moduleDependnencyIDs = resolveModuleImports(module, cache);
    allModules.insert(moduleDependnencyIDs.begin(), moduleDependnencyIDs.end());
  }

  // Update main module's dependencies to include these new overlays.
  auto mainDep =
      *(cache.findDependency(mainModuleName, ModuleDependencyKind::SwiftSource)
            .value());
  std::for_each(/* +1 to exclude dummy main*/ allModules.begin() + 1,
                allModules.end(), [&](ModuleDependencyID dependencyID) {
                  mainDep.addModuleDependency(dependencyID);
                });
  cache.updateDependency(
      {mainModuleName.str(), ModuleDependencyKind::SwiftSource}, mainDep);

  // Report any discovered modules to the clients, which include all overlays
  // and their dependencies.
  std::for_each(/* +1 to exclude dummy main*/ allModules.begin() + 1,
                allModules.end(), action);
}

std::error_code SwiftModuleScanner::findModuleFilesInDirectory(
    ImportPath::Element ModuleID, const SerializedModuleBaseName &BaseName,
    SmallVectorImpl<char> *ModuleInterfacePath,
    SmallVectorImpl<char> *ModuleInterfaceSourcePath,
    std::unique_ptr<llvm::MemoryBuffer> *ModuleBuffer,
    std::unique_ptr<llvm::MemoryBuffer> *ModuleDocBuffer,
    std::unique_ptr<llvm::MemoryBuffer> *ModuleSourceInfoBuffer,
    bool skipBuildingInterface, bool IsFramework,
    bool isTestableDependencyLookup) {
  using namespace llvm::sys;

  auto &fs = *Ctx.SourceMgr.getFileSystem();

  auto ModPath = BaseName.getName(file_types::TY_SwiftModuleFile);
  auto InPath = BaseName.getName(file_types::TY_SwiftModuleInterfaceFile);

  if (LoadMode == ModuleLoadingMode::OnlySerialized || !fs.exists(InPath)) {
    if (fs.exists(ModPath)) {
      // The module file will be loaded directly.
      auto dependencies = scanModuleFile(ModPath, IsFramework);
      if (dependencies) {
        this->dependencies = std::move(dependencies.get());
        return std::error_code();
      }
      return dependencies.getError();
    } else {
      return std::make_error_code(std::errc::no_such_file_or_directory);
    }
  }
  assert(fs.exists(InPath));

  // Use the private interface file if exits.
  auto PrivateInPath =
      BaseName.getName(file_types::TY_PrivateSwiftModuleInterfaceFile);
  if (fs.exists(PrivateInPath)) {
    InPath = PrivateInPath;
  }
  auto dependencies =
      scanInterfaceFile(InPath, IsFramework, isTestableDependencyLookup);
  if (dependencies) {
    this->dependencies = std::move(dependencies.get());
    return std::error_code();
  }

  return dependencies.getError();
}

bool PlaceholderSwiftModuleScanner::findModule(
    ImportPath::Element moduleID, SmallVectorImpl<char> *moduleInterfacePath,
    SmallVectorImpl<char> *moduleInterfaceSourcePath,
    std::unique_ptr<llvm::MemoryBuffer> *moduleBuffer,
    std::unique_ptr<llvm::MemoryBuffer> *moduleDocBuffer,
    std::unique_ptr<llvm::MemoryBuffer> *moduleSourceInfoBuffer,
    bool skipBuildingInterface, bool isTestableDependencyLookup,
    bool &isFramework, bool &isSystemModule) {
  StringRef moduleName = Ctx.getRealModuleName(moduleID.Item).str();
  auto it = PlaceholderDependencyModuleMap.find(moduleName);
  if (it == PlaceholderDependencyModuleMap.end()) {
    return false;
  }
  auto &moduleInfo = it->getValue();
  auto dependencies = ModuleDependencyInfo::forPlaceholderSwiftModuleStub(
      moduleInfo.modulePath,
      moduleInfo.moduleDocPath.has_value() ? moduleInfo.moduleDocPath.value()
                                           : "",
      moduleInfo.moduleSourceInfoPath.has_value()
          ? moduleInfo.moduleSourceInfoPath.value()
          : "");
  this->dependencies = std::move(dependencies);
  return true;
}

void PlaceholderSwiftModuleScanner::parsePlaceholderModuleMap(
    StringRef fileName) {
  ExplicitModuleMapParser parser(Allocator);
  llvm::StringMap<ExplicitClangModuleInputInfo> ClangDependencyModuleMap;
  llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>> fileBufOrErr =
      llvm::MemoryBuffer::getFile(fileName);
  if (!fileBufOrErr) {
    Ctx.Diags.diagnose(SourceLoc(), diag::explicit_swift_module_map_missing,
                       fileName);
    return;
  }
  auto result = parser.parseSwiftExplicitModuleMap(
      (*fileBufOrErr)->getMemBufferRef(), PlaceholderDependencyModuleMap,
      ClangDependencyModuleMap);
  if (result == std::errc::invalid_argument) {
    Ctx.Diags.diagnose(SourceLoc(),
                       diag::placeholder_dependency_module_map_corrupted,
                       fileName);
  } else if (result == std::errc::no_such_file_or_directory) {
    Ctx.Diags.diagnose(
        SourceLoc(), diag::placeholder_dependency_module_map_missing, fileName);
  }
}

static std::vector<std::string> getCompiledCandidates(ASTContext &ctx,
                                                      StringRef moduleName,
                                                      StringRef interfacePath) {
  return ctx.getModuleInterfaceChecker()
      ->getCompiledModuleCandidatesForInterface(moduleName.str(),
                                                interfacePath);
}

llvm::ErrorOr<ModuleDependencyInfo>
SwiftModuleScanner::scanInterfaceFile(Twine moduleInterfacePath,
                                      bool isFramework, bool isTestableImport) {
  // Create a module filename.
  // FIXME: Query the module interface loader to determine an appropriate
  // name for the module, which includes an appropriate hash.
  auto newExt = file_types::getExtension(file_types::TY_SwiftModuleFile);
  auto realModuleName = Ctx.getRealModuleName(moduleName);
  llvm::SmallString<32> modulePath = realModuleName.str();
  llvm::sys::path::replace_extension(modulePath, newExt);
  llvm::Optional<ModuleDependencyInfo> Result;
  std::error_code code = astDelegate.runInSubContext(
      realModuleName.str(), moduleInterfacePath.str(), StringRef(), SourceLoc(),
      [&](ASTContext &Ctx, ModuleDecl *mainMod, ArrayRef<StringRef> BaseArgs,
          ArrayRef<StringRef> PCMArgs, StringRef Hash) {
        assert(mainMod);
        std::string InPath = moduleInterfacePath.str();
        auto compiledCandidates =
            getCompiledCandidates(Ctx, realModuleName.str(), InPath);
        std::vector<std::string> Args(BaseArgs.begin(), BaseArgs.end());

        // Add explicit Swift dependency compilation flags
        Args.push_back("-explicit-interface-module-build");
        Args.push_back("-disable-implicit-swift-modules");
        Args.push_back("-Xcc");
        Args.push_back("-fno-implicit-modules");
        Args.push_back("-Xcc");
        Args.push_back("-fno-implicit-module-maps");
        for (const auto &candidate : compiledCandidates) {
          Args.push_back("-candidate-module-file");
          Args.push_back(candidate);
        }

        // Compute the output path and add it to the command line
        SmallString<128> outputPathBase(moduleOutputPath);
        llvm::sys::path::append(
            outputPathBase,
            moduleName.str() + "-" + Hash + "." +
                file_types::getExtension(file_types::TY_SwiftModuleFile));
        Args.push_back("-o");
        Args.push_back(outputPathBase.str().str());

        // Open the interface file.
        auto &fs = *Ctx.SourceMgr.getFileSystem();
        auto interfaceBuf = fs.getBufferForFile(moduleInterfacePath);
        if (!interfaceBuf) {
          return interfaceBuf.getError();
        }

        // Create a source file.
        unsigned bufferID =
            Ctx.SourceMgr.addNewSourceBuffer(std::move(interfaceBuf.get()));
        auto moduleDecl = ModuleDecl::create(realModuleName, Ctx);

        SourceFile::ParsingOptions parsingOpts;
        auto sourceFile = new (Ctx) SourceFile(
            *moduleDecl, SourceFileKind::Interface, bufferID, parsingOpts);
        moduleDecl->addAuxiliaryFile(*sourceFile);

        std::string RootID;
        if (dependencyTracker) {
          dependencyTracker->startTracking();
          dependencyTracker->addCommonSearchPathDeps(Ctx.SearchPathOpts);
          std::vector<std::string> clangDependencyFiles;
          auto clangImporter =
              static_cast<ClangImporter *>(Ctx.getClangModuleLoader());
          clangImporter->addClangInvovcationDependencies(clangDependencyFiles);
          llvm::for_each(clangDependencyFiles, [&](std::string &file) {
            dependencyTracker->trackFile(file);
          });
          dependencyTracker->trackFile(moduleInterfacePath);
          auto RootOrError = dependencyTracker->createTreeFromDependencies();
          if (!RootOrError)
            return llvm::errorToErrorCode(RootOrError.takeError());
          RootID = RootOrError->getID().toString();
        }

        std::vector<StringRef> ArgsRefs(Args.begin(), Args.end());
        Result = ModuleDependencyInfo::forSwiftInterfaceModule(
            outputPathBase.str().str(), InPath, compiledCandidates, ArgsRefs,
            PCMArgs, Hash, isFramework, RootID, /*module-cache-key*/ "");

        // Walk the source file to find the import declarations.
        llvm::StringSet<> alreadyAddedModules;
        Result->addModuleImport(*sourceFile, alreadyAddedModules);

        // Collect implicitly imported modules in case they are not explicitly
        // printed in the interface file, e.g. SwiftOnoneSupport.
        auto &imInfo = mainMod->getImplicitImportInfo();
        for (auto import : imInfo.AdditionalUnloadedImports) {
          Result->addModuleImport(import.module.getModulePath(),
                                  &alreadyAddedModules);
        }

        // For a `@testable` direct dependency, read in the dependencies
        // from an adjacent binary module, for completeness.
        if (isTestableImport) {
          auto adjacentBinaryModule = std::find_if(
              compiledCandidates.begin(), compiledCandidates.end(),
              [moduleInterfacePath](const std::string &candidate) {
                return llvm::sys::path::parent_path(candidate) ==
                       llvm::sys::path::parent_path(moduleInterfacePath.str());
              });
          if (adjacentBinaryModule != compiledCandidates.end()) {
            // Required modules.
            auto adjacentBinaryModuleRequiredImports = getImportsOfModule(
                *adjacentBinaryModule, ModuleLoadingBehavior::Required,
                isFramework, isRequiredOSSAModules(), Ctx.LangOpts.SDKName,
                Ctx.LangOpts.PackageName, Ctx.SourceMgr.getFileSystem().get(),
                Ctx.SearchPathOpts.DeserializedPathRecoverer);
            if (!adjacentBinaryModuleRequiredImports)
              return adjacentBinaryModuleRequiredImports.getError();
            auto adjacentBinaryModuleRequiredModuleImports =
                (*adjacentBinaryModuleRequiredImports).moduleImports;
#ifndef NDEBUG
            //  Verify that the set of required modules read out from the binary
            //  module is a super-set of module imports identified in the
            //  textual interface.
            for (const auto &requiredImport : Result->getModuleImports()) {
              assert(
                  adjacentBinaryModuleRequiredModuleImports.contains(
                      requiredImport) &&
                  "Expected adjacent binary module's import set to contain all "
                  "textual interface imports.");
            }
#endif

            for (const auto &requiredImport :
                 adjacentBinaryModuleRequiredModuleImports)
              Result->addModuleImport(requiredImport.getKey(),
                                      &alreadyAddedModules);

            // Optional modules. Will be looked-up on a best-effort basis
            auto adjacentBinaryModuleOptionalImports = getImportsOfModule(
                *adjacentBinaryModule, ModuleLoadingBehavior::Optional,
                isFramework, isRequiredOSSAModules(), Ctx.LangOpts.SDKName,
                Ctx.LangOpts.PackageName, Ctx.SourceMgr.getFileSystem().get(),
                Ctx.SearchPathOpts.DeserializedPathRecoverer);
            if (!adjacentBinaryModuleOptionalImports)
              return adjacentBinaryModuleOptionalImports.getError();
            auto adjacentBinaryModuleOptionalModuleImports =
                (*adjacentBinaryModuleOptionalImports).moduleImports;
            for (const auto &optionalImport :
                 adjacentBinaryModuleOptionalModuleImports)
              Result->addOptionalModuleImport(optionalImport.getKey(),
                                              &alreadyAddedModules);
          }
        }

        return std::error_code();
      });

  if (code) {
    return code;
  }
  return *Result;
}

ModuleDependencyVector
SerializedModuleLoaderBase::getModuleDependencies(
    StringRef moduleName,  StringRef moduleOutputPath,
    llvm::IntrusiveRefCntPtr<llvm::cas::CachingOnDiskFileSystem> CacheFS,
    const llvm::DenseSet<clang::tooling::dependencies::ModuleID> &alreadySeenClangModules,
    clang::tooling::dependencies::DependencyScanningTool &clangScanningTool,
    InterfaceSubContextDelegate &delegate,
    bool isTestableDependencyLookup) {
  ImportPath::Module::Builder builder(Ctx, moduleName, /*separator=*/'.');
  auto modulePath = builder.get();
  auto moduleId = modulePath.front().Item;
  llvm::Optional<SwiftDependencyTracker> tracker = llvm::None;
  if (CacheFS)
    tracker = SwiftDependencyTracker(*CacheFS);

  // Instantiate dependency scanning "loaders".
  SmallVector<std::unique_ptr<SwiftModuleScanner>, 2> scanners;
  // Placeholder dependencies must be resolved first, to prevent the
  // ModuleDependencyScanner from first discovering artifacts of a previous
  // build. Such artifacts are captured as compiledModuleCandidates in the
  // dependency graph of the placeholder dependency module itself.
  // FIXME: submodules?
  scanners.push_back(std::make_unique<PlaceholderSwiftModuleScanner>(
      Ctx, LoadMode, moduleId,
      Ctx.SearchPathOpts.PlaceholderDependencyModuleMap, delegate,
      moduleOutputPath, tracker));
  scanners.push_back(std::make_unique<SwiftModuleScanner>(
      Ctx, LoadMode, moduleId, delegate, moduleOutputPath,
      SwiftModuleScanner::MDS_plain, tracker));

  // Check whether there is a module with this name that we can import.
  assert(isa<PlaceholderSwiftModuleScanner>(scanners[0].get()) &&
         "Expected PlaceholderSwiftModuleScanner as the first dependency "
         "scanner loader.");
  for (auto &scanner : scanners) {
    if (scanner->canImportModule(modulePath, nullptr,
                                 isTestableDependencyLookup)) {

      ModuleDependencyVector moduleDependnecies;
      moduleDependnecies.push_back(
          std::make_pair(ModuleDependencyID{moduleName.str(),
                                            scanner->dependencies->getKind()},
                         *(scanner->dependencies)));
      return moduleDependnecies;
    }
  }

  return {};
}
