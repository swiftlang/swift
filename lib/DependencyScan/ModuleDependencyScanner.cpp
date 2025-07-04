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
#include "swift/AST/DiagnosticsCommon.h"
#include "swift/AST/DiagnosticsFrontend.h"
#include "swift/AST/DiagnosticsSema.h"
#include "swift/AST/ModuleDependencies.h"
#include "swift/AST/ModuleLoader.h"
#include "swift/AST/PluginLoader.h"
#include "swift/AST/SourceFile.h"
#include "swift/AST/TypeCheckRequests.h"
#include "swift/Basic/Assertions.h"
#include "swift/Basic/Defer.h"
#include "swift/Basic/FileTypes.h"
#include "swift/Basic/PrettyStackTrace.h"
#include "swift/ClangImporter/ClangImporter.h"
#include "swift/DependencyScan/ModuleDependencyScanner.h"
#include "swift/Frontend/CompileJobCacheKey.h"
#include "swift/Frontend/ModuleInterfaceLoader.h"
#include "swift/Serialization/SerializedModuleLoader.h"
#include "swift/Subsystems.h"
#include "clang/Frontend/CompilerInstance.h"
#include "llvm/ADT/IntrusiveRefCntPtr.h"
#include "llvm/ADT/SetOperations.h"
#include "llvm/CAS/CachingOnDiskFileSystem.h"
#include "llvm/Support/Error.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/MemoryBufferRef.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/Threading.h"
#include "llvm/Support/VersionTuple.h"
#include "llvm/Support/VirtualFileSystem.h"
#include "llvm/Support/VirtualOutputBackend.h"
#include "llvm/Support/VirtualOutputBackends.h"
#include "llvm/Support/VirtualOutputFile.h"
#include "llvm/Support/raw_ostream.h"
#include <algorithm>
#include <optional>

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

static bool isSwiftDependencyKind(ModuleDependencyKind Kind) {
  return Kind == ModuleDependencyKind::SwiftInterface ||
         Kind == ModuleDependencyKind::SwiftSource ||
         Kind == ModuleDependencyKind::SwiftBinary ||
         Kind == ModuleDependencyKind::SwiftPlaceholder;
}

// The Swift compiler does not have a concept of a working directory.
// It is instead handled by the Swift driver by resolving relative paths
// according to the driver's notion of a working directory. On the other hand,
// Clang does have a concept working directory which may be specified on a
// Clang invocation with '-working-directory'. If so, it is crucial that we
// use this directory as an argument to the Clang scanner invocation below.
static std::string
computeClangWorkingDirectory(const std::vector<std::string> &commandLineArgs,
                             const ASTContext &ctx) {
  std::string workingDir;
  auto clangWorkingDirPos = std::find(
      commandLineArgs.rbegin(), commandLineArgs.rend(), "-working-directory");
  if (clangWorkingDirPos == commandLineArgs.rend())
    workingDir =
        ctx.SourceMgr.getFileSystem()->getCurrentWorkingDirectory().get();
  else {
    if (clangWorkingDirPos - 1 == commandLineArgs.rend()) {
      ctx.Diags.diagnose(SourceLoc(), diag::clang_dependency_scan_error,
                         "Missing '-working-directory' argument");
      workingDir =
          ctx.SourceMgr.getFileSystem()->getCurrentWorkingDirectory().get();
    } else
      workingDir = *(clangWorkingDirPos - 1);
  }
  return workingDir;
}

static std::string
moduleCacheRelativeLookupModuleOutput(const clang::tooling::dependencies::ModuleDeps &MD,
                                      clang::tooling::dependencies::ModuleOutputKind MOK,
                                      const StringRef moduleCachePath,
                                      const StringRef stableModuleCachePath,
                                      const StringRef runtimeResourcePath) {
  llvm::SmallString<128> outputPath(moduleCachePath);
  if (MD.IsInStableDirectories)
    outputPath = stableModuleCachePath;

  // FIXME: This is a hack to treat Clang modules defined in the compiler's
  // own resource directory as stable, when they are not reported as such
  // by the Clang scanner.
  if (!runtimeResourcePath.empty() &&
      hasPrefix(llvm::sys::path::begin(MD.ClangModuleMapFile),
                llvm::sys::path::end(MD.ClangModuleMapFile),
                llvm::sys::path::begin(runtimeResourcePath),
                llvm::sys::path::end(runtimeResourcePath)))
    outputPath = stableModuleCachePath;

  llvm::sys::path::append(outputPath, MD.ID.ModuleName + "-" + MD.ID.ContextHash);
  switch (MOK) {
  case clang::tooling::dependencies::ModuleOutputKind::ModuleFile:
    llvm::sys::path::replace_extension(
        outputPath, getExtension(swift::file_types::TY_ClangModuleFile));
    break;
  case clang::tooling::dependencies::ModuleOutputKind::DependencyFile:
    llvm::sys::path::replace_extension(
        outputPath, getExtension(swift::file_types::TY_Dependencies));
    break;
  case clang::tooling::dependencies::ModuleOutputKind::DependencyTargets:
    return MD.ID.ModuleName + "-" + MD.ID.ContextHash;
  case clang::tooling::dependencies::ModuleOutputKind::DiagnosticSerializationFile:
    llvm::sys::path::replace_extension(
        outputPath, getExtension(swift::file_types::TY_SerializedDiagnostics));
    break;
  }
  return outputPath.str().str();
}

static std::vector<std::string> inputSpecificClangScannerCommand(
    const std::vector<std::string> &baseCommandLineArgs,
    std::optional<StringRef> sourceFileName) {
  std::vector<std::string> result(baseCommandLineArgs.begin(),
                                  baseCommandLineArgs.end());
  auto sourceFilePos =
      std::find(result.begin(), result.end(), "<swift-imported-modules>");
  assert(sourceFilePos != result.end());
  if (sourceFileName.has_value())
    *sourceFilePos = sourceFileName->str();
  else
    result.erase(sourceFilePos);
  return result;
}

ModuleDependencyScanningWorker::ModuleDependencyScanningWorker(
    SwiftDependencyScanningService &globalScanningService,
    const CompilerInvocation &ScanCompilerInvocation,
    const SILOptions &SILOptions, ASTContext &ScanASTContext,
    swift::DependencyTracker &DependencyTracker, DiagnosticEngine &Diagnostics)
    : clangScanningTool(*globalScanningService.ClangScanningService,
                        globalScanningService.getClangScanningFS(ScanASTContext)) {
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
                      workerCompilerInvocation->getSerializationOptions(),
                      ScanASTContext.SourceMgr, Diagnostics));
  auto loader = std::make_unique<PluginLoader>(
      *workerASTContext, /*DepTracker=*/nullptr,
      workerCompilerInvocation->getFrontendOptions().CacheReplayPrefixMap,
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

  // Set up CAS instance.
  CAS = globalScanningService.CAS;
  ActionCache = globalScanningService.ActionCache;

  // Set up the required command-line arguments and working directory
  // configuration required for clang dependency scanner queries
  auto scanClangImporter =
      static_cast<ClangImporter *>(ScanASTContext.getClangModuleLoader());
  clangScanningBaseCommandLineArgs =
      scanClangImporter->getClangDepScanningInvocationArguments(ScanASTContext);
  clangScanningModuleCommandLineArgs = inputSpecificClangScannerCommand(
      clangScanningBaseCommandLineArgs, std::nullopt);
  clangScanningWorkingDirectoryPath = computeClangWorkingDirectory(
      clangScanningBaseCommandLineArgs, ScanASTContext);

  // Handle clang arguments. For caching build, all arguments are passed
  // with `-direct-clang-cc1-module-build`.
  if (ScanASTContext.ClangImporterOpts.ClangImporterDirectCC1Scan) {
    swiftModuleClangCC1CommandLineArgs.push_back("-direct-clang-cc1-module-build");
    for (auto &Arg : scanClangImporter->getSwiftExplicitModuleDirectCC1Args()) {
      swiftModuleClangCC1CommandLineArgs.push_back("-Xcc");
      swiftModuleClangCC1CommandLineArgs.push_back(Arg);
    }
  } else {
    swiftModuleClangCC1CommandLineArgs.push_back("-Xcc");
    swiftModuleClangCC1CommandLineArgs.push_back("-fno-implicit-modules");
    swiftModuleClangCC1CommandLineArgs.push_back("-Xcc");
    swiftModuleClangCC1CommandLineArgs.push_back("-fno-implicit-module-maps");
  }

  // Set up the Swift interface loader for Swift scanning.
  swiftScannerModuleLoader = ModuleInterfaceLoader::create(
      *workerASTContext,
      *static_cast<ModuleInterfaceCheckerImpl *>(
          ScanASTContext.getModuleInterfaceChecker()),
      &DependencyTracker,
      workerCompilerInvocation->getSearchPathOptions().ModuleLoadMode);
}

ModuleDependencyVector
ModuleDependencyScanningWorker::scanFilesystemForSwiftModuleDependency(
    Identifier moduleName, StringRef moduleOutputPath,
    StringRef sdkModuleOutputPath, llvm::PrefixMapper *prefixMapper,
    bool isTestableImport) {
  return swiftScannerModuleLoader->getModuleDependencies(
      moduleName, moduleOutputPath, sdkModuleOutputPath,
      {}, swiftModuleClangCC1CommandLineArgs, *scanningASTDelegate,
      prefixMapper, isTestableImport);
}

ModuleDependencyVector
ModuleDependencyScanningWorker::scanFilesystemForClangModuleDependency(
    Identifier moduleName, StringRef moduleOutputPath,
    StringRef sdkModuleOutputPath,
    const llvm::DenseSet<clang::tooling::dependencies::ModuleID>
        &alreadySeenModules,
    llvm::PrefixMapper *prefixMapper) {
  auto lookupModuleOutput =
      [this, moduleOutputPath, sdkModuleOutputPath](
          const clang::tooling::dependencies::ModuleDeps &MD,
          const clang::tooling::dependencies::ModuleOutputKind MOK)
      -> std::string {
    return moduleCacheRelativeLookupModuleOutput(
        MD, MOK, moduleOutputPath, sdkModuleOutputPath,
        workerASTContext->SearchPathOpts.RuntimeResourcePath);
  };

  auto clangModuleDependencies = clangScanningTool.getModuleDependencies(
      moduleName.str(), clangScanningModuleCommandLineArgs,
      clangScanningWorkingDirectoryPath,
      alreadySeenModules,
      lookupModuleOutput);
  if (!clangModuleDependencies) {
    auto errorStr = toString(clangModuleDependencies.takeError());
    // We ignore the "module 'foo' not found" error, the Swift dependency
    // scanner will report such an error only if all of the module loaders
    // fail as well.
    if (errorStr.find("fatal error: module '" + moduleName.str().str() +
                      "' not found") == std::string::npos)
      workerASTContext->Diags.diagnose(
          SourceLoc(), diag::clang_dependency_scan_error, errorStr);
    return {};
  }

  return ClangImporter::bridgeClangModuleDependencies(
      *workerASTContext, clangScanningTool, *clangModuleDependencies,
      moduleOutputPath, sdkModuleOutputPath, lookupModuleOutput,
      [&](StringRef path) {
        if (prefixMapper)
          return prefixMapper->mapToString(path);
        return path.str();
      });
}

bool ModuleDependencyScanningWorker::scanHeaderDependenciesOfSwiftModule(
    const ASTContext &ctx,
    ModuleDependencyID moduleID, std::optional<StringRef> headerPath,
    std::optional<llvm::MemoryBufferRef> sourceBuffer,
    ModuleDependenciesCache &cache,
    ModuleDependencyIDSetVector &headerClangModuleDependencies,
    std::vector<std::string> &headerFileInputs,
    std::vector<std::string> &bridgingHeaderCommandLine,
    std::optional<std::string> &includeTreeID) {
  // Scan the specified textual header file and collect its dependencies
  auto scanHeaderDependencies = [&]()
      -> llvm::Expected<clang::tooling::dependencies::TranslationUnitDeps> {
    auto moduleOutputPath = cache.getModuleOutputPath();
    auto sdkModuleOutputPath = cache.getSDKModuleOutputPath();
    auto lookupModuleOutput =
        [moduleOutputPath, sdkModuleOutputPath,
         &ctx](const clang::tooling::dependencies::ModuleDeps &MD,
               const clang::tooling::dependencies::ModuleOutputKind MOK)
        -> std::string {
      return moduleCacheRelativeLookupModuleOutput(
          MD, MOK, moduleOutputPath, sdkModuleOutputPath,
          ctx.SearchPathOpts.RuntimeResourcePath);
    };

    auto dependencies = clangScanningTool.getTranslationUnitDependencies(
        inputSpecificClangScannerCommand(clangScanningBaseCommandLineArgs, headerPath),
        clangScanningWorkingDirectoryPath,
        cache.getAlreadySeenClangModules(), lookupModuleOutput, sourceBuffer);
    if (!dependencies)
      return dependencies.takeError();

    // Record module dependencies for each new module we found.
    auto bridgedDeps = ClangImporter::bridgeClangModuleDependencies(
        ctx, clangScanningTool, dependencies->ModuleGraph, moduleOutputPath,
        sdkModuleOutputPath, lookupModuleOutput, [&cache](StringRef path) {
          return cache.getScanService().remapPath(path);
        });
    cache.recordDependencies(bridgedDeps, ctx.Diags);

    llvm::copy(dependencies->FileDeps, std::back_inserter(headerFileInputs));
    auto bridgedDependencyIDs =
        llvm::map_range(dependencies->ClangModuleDeps, [](auto &input) {
          return ModuleDependencyID{input.ModuleName,
                                    ModuleDependencyKind::Clang};
        });
    headerClangModuleDependencies.insert(bridgedDependencyIDs.begin(),
                                         bridgedDependencyIDs.end());
    return dependencies;
  };

  // - If a generated header is provided, scan the generated header.
  // - Textual module dependencies require us to process their bridging header.
  // - Binary module dependnecies may have arbitrary header inputs.
  auto clangModuleDependencies = scanHeaderDependencies();
  if (!clangModuleDependencies) {
    auto errorStr = toString(clangModuleDependencies.takeError());
    workerASTContext->Diags.diagnose(
            SourceLoc(), diag::clang_header_dependency_scan_error, errorStr);
    return true;
  }

  auto targetModuleInfo = cache.findKnownDependency(moduleID);
  if (!targetModuleInfo.isTextualSwiftModule())
    return false;

  if (auto TreeID = clangModuleDependencies->IncludeTreeID)
    includeTreeID = TreeID;
  ClangImporter::getBridgingHeaderOptions(ctx, *clangModuleDependencies,
                                          bridgingHeaderCommandLine);
  return false;
}

template <typename Function, typename... Args>
auto ModuleDependencyScanner::withDependencyScanningWorker(Function &&F,
                                                           Args &&...ArgList) {
  NumLookups++;
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

llvm::Error ModuleDependencyScanningWorker::createCacheKeyForEmbeddedHeader(
    std::string embeddedHeaderIncludeTree,
    std::string chainedHeaderIncludeTree) {
  assert(CAS && ActionCache && "CAS is not available");
  auto chained = CAS->parseID(chainedHeaderIncludeTree);
  if (!chained)
    return chained.takeError();
  auto chainedRef = CAS->getReference(*chained);
  if (!chainedRef)
    return llvm::createStringError("Chained IncludeTree missing");
  auto embedded = CAS->parseID(embeddedHeaderIncludeTree);
  if (!embedded)
    return embedded.takeError();
  auto key =
      ClangImporter::createEmbeddedBridgingHeaderCacheKey(*CAS, *chainedRef);
  if (!key)
    return key.takeError();

  return ActionCache->put(CAS->getID(*key), *embedded);
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
                                        const ModuleDependenciesCache &cache,
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

static std::set<ModuleDependencyID>
collectBinarySwiftDeps(const ModuleDependenciesCache &cache) {
  std::set<ModuleDependencyID> binarySwiftModuleDepIDs;
  auto binaryDepsMap = cache.getDependenciesMap(ModuleDependencyKind::SwiftBinary);
  for (const auto &binaryDepName : binaryDepsMap.keys())
    binarySwiftModuleDepIDs.insert(ModuleDependencyID{binaryDepName.str(),
                                                      ModuleDependencyKind::SwiftBinary});
  return binarySwiftModuleDepIDs;
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
       {}, buildCommands, {}, {}, {});

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
      mainDependencies.addModuleImport("Swift", /* isExported */false,
                                       &alreadyAddedModules);
      break;
    }

    // Add any implicit module names.
    for (const auto &import : importInfo.AdditionalUnloadedImports) {
      mainDependencies.addModuleImport(import.module.getModulePath(),
                                       import.options.contains(ImportFlags::Exported),
                                       &alreadyAddedModules,
                                       &ScanASTContext.SourceMgr);
    }

    // Already-loaded, implicitly imported module names.
    for (const auto &import : importInfo.AdditionalImports) {
      mainDependencies.addModuleImport(
          import.module.importedModule->getNameStr(),
          import.options.contains(ImportFlags::Exported),
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
                                       /* isExported */ true,
                                       &alreadyAddedModules);
    }

    // All modules specified with `-embed-tbd-for-module` are treated as
    // implicit dependnecies for this compilation since they are not guaranteed
    // to be impored in the source.
    for (const auto &tbdSymbolModule :
         ScanCompilerInvocation.getTBDGenOptions().embedSymbolsFromModules) {
      mainDependencies.addModuleImport(tbdSymbolModule,
                                       /* isExported */ false,
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
    std::vector<std::string> buildArgs = mainDependencies.getCommandline();
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

/// For the dependency set of the main module, discover all
/// cross-import overlays and their corresponding '.swiftcrossimport'
/// files. Cross-import overlay dependencies are required when
/// the two constituent modules are imported *from the same source file*,
/// directly or indirectly.
///
/// Given a complete module dependency graph in this stage of the scan,
/// the algorithm for discovering cross-import overlays is:
/// 1. For each source file of the module under scan construct a
///    set of module dependnecies only reachable from this source file.
/// 2. For each module set constructed in (1), perform pair-wise lookup
///    of cross import files for each pair of modules in the set.
///
/// Notably, if for some pair of modules 'A' and 'B' there exists
/// a cross-import overlay '_A_B', and these two modules are not reachable
/// from any single source file via direct or indirect imports, then
/// the cross-import overlay module is not required for compilation.
static void discoverCrossImportOverlayFiles(
    StringRef mainModuleName, ModuleDependenciesCache &cache,
    ASTContext &scanASTContext, llvm::SetVector<Identifier> &newOverlays,
    std::set<std::pair<std::string, std::string>> &overlayFiles) {
  auto mainModuleInfo = cache.findKnownDependency(ModuleDependencyID{
      mainModuleName.str(), ModuleDependencyKind::SwiftSource});

  llvm::StringMap<ModuleDependencyIDSet> perSourceFileDependencies;
  const ModuleDependencyIDSet mainModuleDirectSwiftDepsSet{
      mainModuleInfo.getImportedSwiftDependencies().begin(),
      mainModuleInfo.getImportedSwiftDependencies().end()};
  const ModuleDependencyIDSet mainModuleDirectClangDepsSet{
      mainModuleInfo.getImportedClangDependencies().begin(),
      mainModuleInfo.getImportedClangDependencies().end()};

  // A utility to map an import identifier to one of the
  // known resolved module dependencies
  auto getModuleIDForImportIdentifier =
      [](const std::string &importIdentifierStr,
         const ModuleDependencyIDSet &directSwiftDepsSet,
         const ModuleDependencyIDSet &directClangDepsSet) -> std::optional<ModuleDependencyID> {
    if (auto textualDepIt = directSwiftDepsSet.find(
            {importIdentifierStr, ModuleDependencyKind::SwiftInterface});
        textualDepIt != directSwiftDepsSet.end())
      return *textualDepIt;
    else if (auto binaryDepIt = directSwiftDepsSet.find(
                 {importIdentifierStr, ModuleDependencyKind::SwiftBinary});
             binaryDepIt != directSwiftDepsSet.end())
      return *binaryDepIt;
    else if (auto clangDepIt = directClangDepsSet.find(
                 {importIdentifierStr, ModuleDependencyKind::Clang});
             clangDepIt != directClangDepsSet.end())
      return *clangDepIt;
    else
      return std::nullopt;
  };

  // Collect the set of directly-imported module dependencies
  // for each source file in the source module under scan.
  for (const auto &import : mainModuleInfo.getModuleImports()) {
    auto importResolvedModuleID = getModuleIDForImportIdentifier(
        import.importIdentifier, mainModuleDirectSwiftDepsSet,
        mainModuleDirectClangDepsSet);
    if (importResolvedModuleID)
      for (const auto &importLocation : import.importLocations)
        perSourceFileDependencies[importLocation.bufferIdentifier].insert(
          *importResolvedModuleID);
  }

  // For each source-file, build a set of module dependencies of the
  // module under scan corresponding to a sub-graph of modules only reachable
  // from this source file's direct imports.
  for (auto &keyValPair : perSourceFileDependencies) {
    const auto &bufferIdentifier = keyValPair.getKey();
    auto &directDependencyIDs = keyValPair.second;
    SmallVector<ModuleDependencyID, 8> worklist{directDependencyIDs.begin(),
                                                directDependencyIDs.end()};
    while (!worklist.empty()) {
      auto moduleID = worklist.pop_back_val();
      perSourceFileDependencies[bufferIdentifier].insert(moduleID);
      if (isSwiftDependencyKind(moduleID.Kind)) {
        auto moduleInfo = cache.findKnownDependency(moduleID);
        if (llvm::any_of(moduleInfo.getModuleImports(),
                         [](const ScannerImportStatementInfo &importInfo) {
                           return importInfo.isExported;
                         })) {
          const ModuleDependencyIDSet directSwiftDepsSet{
              moduleInfo.getImportedSwiftDependencies().begin(),
              moduleInfo.getImportedSwiftDependencies().end()};
          const ModuleDependencyIDSet directClangDepsSet{
              moduleInfo.getImportedClangDependencies().begin(),
              moduleInfo.getImportedClangDependencies().end()};
          for (const auto &import : moduleInfo.getModuleImports()) {
            if (import.isExported) {
              auto importResolvedDepID = getModuleIDForImportIdentifier(
                  import.importIdentifier, directSwiftDepsSet,
                  directClangDepsSet);
              if (importResolvedDepID &&
                  !perSourceFileDependencies[bufferIdentifier].count(
                      *importResolvedDepID))
                worklist.push_back(*importResolvedDepID);
            }
          }
        }
      }
    }
  }

  // Within a provided set of module dependencies reachable via
  // direct imports from a given file, determine the available and required
  // cross-import overlays.
  auto discoverCrossImportOverlayFilesForModuleSet =
      [&mainModuleName, &cache, &scanASTContext, &newOverlays,
       &overlayFiles](const ModuleDependencyIDSet &inputDependencies) {
        for (auto moduleID : inputDependencies) {
          auto moduleName = moduleID.ModuleName;
          // Do not look for overlays of main module under scan
          if (moduleName == mainModuleName)
            continue;

          auto dependencies =
              cache.findDependency(moduleName, moduleID.Kind).value();
          // Collect a map from secondary module name to cross-import overlay
          // names.
          auto overlayMap = dependencies->collectCrossImportOverlayNames(
              scanASTContext, moduleName, overlayFiles);
          if (overlayMap.empty())
            continue;
          for (const auto &dependencyId : inputDependencies) {
            auto moduleName = dependencyId.ModuleName;
            // Do not look for overlays of main module under scan
            if (moduleName == mainModuleName)
              continue;
            // check if any explicitly imported modules can serve as a
            // secondary module, and add the overlay names to the
            // dependencies list.
            for (auto overlayName : overlayMap[moduleName]) {
              if (overlayName.str() != mainModuleName &&
                  std::find_if(inputDependencies.begin(),
                               inputDependencies.end(),
                               [&](ModuleDependencyID Id) {
                                 return moduleName == overlayName.str();
                               }) == inputDependencies.end()) {
                newOverlays.insert(overlayName);
              }
            }
          }
        }
      };

  for (const auto &keyValPair : perSourceFileDependencies)
    discoverCrossImportOverlayFilesForModuleSet(keyValPair.second);
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
    resolveCrossImportOverlayDependencies(
        rootModuleID.ModuleName, cache,
        [&](ModuleDependencyID id) { allModules.insert(id); });

  if (ScanCompilerInvocation.getSearchPathOptions().BridgingHeaderChaining) {
    auto err = performBridgingHeaderChaining(rootModuleID, cache, allModules);
    if (err)
      Diagnostics.diagnose(SourceLoc(), diag::error_scanner_extra,
                           toString(std::move(err)));
  }

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

void ModuleDependencyScanner::resolveAllClangModuleDependencies(
    ArrayRef<ModuleDependencyID> swiftModuleDependents,
    ModuleDependenciesCache &cache,
    ModuleDependencyIDSetVector &allDiscoveredClangModules) {
  // Gather all unresolved imports which must correspond to
  // Clang modules (since no Swift module for them was found).
  llvm::StringSet<> unresolvedImportIdentifiers;
  llvm::StringSet<> unresolvedOptionalImportIdentifiers;
  std::unordered_map<ModuleDependencyID,
                     std::vector<ScannerImportStatementInfo>>
      unresolvedImportsMap;
  std::unordered_map<ModuleDependencyID,
                     std::vector<ScannerImportStatementInfo>>
      unresolvedOptionalImportsMap;

  for (const auto &moduleID : swiftModuleDependents) {
    auto moduleDependencyInfo = cache.findKnownDependency(moduleID);
    auto unresolvedImports =
        &unresolvedImportsMap
             .emplace(moduleID, std::vector<ScannerImportStatementInfo>())
             .first->second;
    auto unresolvedOptionalImports =
        &unresolvedOptionalImportsMap
             .emplace(moduleID, std::vector<ScannerImportStatementInfo>())
             .first->second;

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
      llvm::StringSet<> resolvedImportIdentifiers;
      for (const auto &resolvedDep :
           moduleDependencyInfo.getImportedSwiftDependencies())
        resolvedImportIdentifiers.insert(resolvedDep.ModuleName);

      // When querying a *clang* module 'CxxStdlib' we must
      // instead expect a module called 'std'...
      auto addCanonicalClangModuleImport =
          [this](const ScannerImportStatementInfo &importInfo,
                 std::vector<ScannerImportStatementInfo> &unresolvedImports,
                 llvm::StringSet<> &unresolvedImportIdentifiers) {
            if (importInfo.importIdentifier ==
                ScanASTContext.Id_CxxStdlib.str()) {
              auto canonicalImportInfo = ScannerImportStatementInfo(
                  "std", importInfo.isExported, importInfo.importLocations);
              unresolvedImports.push_back(canonicalImportInfo);
              unresolvedImportIdentifiers.insert(
                  canonicalImportInfo.importIdentifier);
            } else {
              unresolvedImports.push_back(importInfo);
              unresolvedImportIdentifiers.insert(importInfo.importIdentifier);
            }
          };

      for (const auto &depImport : moduleDependencyInfo.getModuleImports())
        if (!resolvedImportIdentifiers.contains(depImport.importIdentifier))
          addCanonicalClangModuleImport(depImport, *unresolvedImports,
                                        unresolvedImportIdentifiers);
      for (const auto &depImport :
           moduleDependencyInfo.getOptionalModuleImports())
        if (!resolvedImportIdentifiers.contains(depImport.importIdentifier))
          addCanonicalClangModuleImport(depImport, *unresolvedOptionalImports,
                                        unresolvedOptionalImportIdentifiers);
    }
  }

  // Prepare the module lookup result collection
  llvm::StringMap<std::optional<ModuleDependencyVector>> moduleLookupResult;
  for (const auto &unresolvedIdentifier : unresolvedImportIdentifiers)
    moduleLookupResult.insert(
        std::make_pair(unresolvedIdentifier.getKey(), std::nullopt));

  // We need a copy of the shared already-seen module set, which will be shared amongst
  // all the workers. In `recordDependencies`, each worker will contribute its
  // results back to the shared set for future lookups.
  const llvm::DenseSet<clang::tooling::dependencies::ModuleID> seenClangModules =
      cache.getAlreadySeenClangModules();
  std::mutex cacheAccessLock;
  auto scanForClangModuleDependency =
      [this, &cache, &moduleLookupResult,
       &cacheAccessLock, &seenClangModules](Identifier moduleIdentifier) {
        auto moduleName = moduleIdentifier.str();
        {
          std::lock_guard<std::mutex> guard(cacheAccessLock);
          if (cache.hasDependency(moduleName, ModuleDependencyKind::Clang))
            return;
        }

        auto moduleDependencies = withDependencyScanningWorker(
            [&cache, &seenClangModules,
             moduleIdentifier](ModuleDependencyScanningWorker *ScanningWorker) {
              return ScanningWorker->scanFilesystemForClangModuleDependency(
                  moduleIdentifier, cache.getModuleOutputPath(),
                  cache.getSDKModuleOutputPath(), seenClangModules,
                  cache.getScanService().getPrefixMapper());
            });

        // Update the `moduleLookupResult` and cache all discovered dependencies
        // so that subsequent queries do not have to call into the scanner
        // if looking for a module that was discovered as a transitive dependency
        // in this scan.
        {
          std::lock_guard<std::mutex> guard(cacheAccessLock);
          moduleLookupResult.insert_or_assign(moduleName, moduleDependencies);
          if (!moduleDependencies.empty())
            cache.recordDependencies(moduleDependencies, Diagnostics);
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
        diagnoseScannerFailure(unresolvedImport, cache, moduleID);
    }

    if (!importedClangDependencies.empty())
      cache.setImportedClangDependencies(moduleID, importedClangDependencies.takeVector());
  }

  return;
}

void ModuleDependencyScanner::resolveHeaderDependencies(
    ArrayRef<ModuleDependencyID> allSwiftModules,
    ModuleDependenciesCache &cache,
    ModuleDependencyIDSetVector &allDiscoveredHeaderDependencyClangModules) {
  for (const auto &moduleID : allSwiftModules) {
    auto moduleDependencyInfo = cache.findKnownDependency(moduleID);
    if (!moduleDependencyInfo.getHeaderClangDependencies().empty()) {
      allDiscoveredHeaderDependencyClangModules.insert(
          moduleDependencyInfo.getHeaderClangDependencies().begin(),
          moduleDependencyInfo.getHeaderClangDependencies().end());
    } else {
      ModuleDependencyIDSetVector headerClangModuleDependencies;
      resolveHeaderDependenciesForModule(moduleID, cache,
                                         headerClangModuleDependencies);
      allDiscoveredHeaderDependencyClangModules.insert(
          headerClangModuleDependencies.begin(),
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
  std::mutex lookupResultLock;

  // A scanning task to query a module by-name. If the module already exists
  // in the cache, do nothing and return.
  auto scanForSwiftModuleDependency =
      [this, &cache, &lookupResultLock, &moduleLookupResult](Identifier moduleIdentifier,
                                                             bool isTestable) {
        auto moduleName = moduleIdentifier.str().str();
        {
          std::lock_guard<std::mutex> guard(lookupResultLock);
          if (cache.hasSwiftDependency(moduleName))
            return;
        }

        auto moduleDependencies = withDependencyScanningWorker(
            [&cache, moduleIdentifier,
             isTestable](ModuleDependencyScanningWorker *ScanningWorker) {
              return ScanningWorker->scanFilesystemForSwiftModuleDependency(
                  moduleIdentifier, cache.getModuleOutputPath(),
                  cache.getSDKModuleOutputPath(),
                  cache.getScanService().getPrefixMapper(), isTestable);
            });

        {
          std::lock_guard<std::mutex> guard(lookupResultLock);
          moduleLookupResult.insert_or_assign(moduleName, moduleDependencies);
        }
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
      [this, &cache, &moduleLookupResult, &importedSwiftDependencies,
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
            cache.recordDependencies(lookupResult.value(), Diagnostics);
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

  if (!isTextualModuleWithABridgingHeader && !isBinaryModuleWithHeaderInput)
    return;


  std::optional<std::string> headerPath;
  std::unique_ptr<llvm::MemoryBuffer> sourceBuffer;
  std::optional<llvm::MemoryBufferRef> sourceBufferRef;

  auto extractHeaderContent =
      [&](const SwiftBinaryModuleDependencyStorage &binaryMod)
      -> std::unique_ptr<llvm::MemoryBuffer> {
    auto header = binaryMod.headerImport;
    // Check to see if the header input exists on disk.
    auto FS = ScanASTContext.SourceMgr.getFileSystem();
    if (FS->exists(header))
      return nullptr;

    auto moduleBuf = FS->getBufferForFile(binaryMod.compiledModulePath);
    if (!moduleBuf)
      return nullptr;

    auto content = extractEmbeddedBridgingHeaderContent(std::move(*moduleBuf),
                                                        ScanASTContext);
    if (content.empty())
      return nullptr;

    return llvm::MemoryBuffer::getMemBufferCopy(content, header);
  };

  if (isBinaryModuleWithHeaderInput) {
    auto &binaryMod = *moduleDependencyInfo.getAsSwiftBinaryModule();
    if (auto embeddedHeader = extractHeaderContent(binaryMod)) {
      sourceBuffer = std::move(embeddedHeader);
      sourceBufferRef = sourceBuffer->getMemBufferRef();
    } else
      headerPath = binaryMod.headerImport;
  } else
    headerPath = *moduleDependencyInfo.getBridgingHeader();

  withDependencyScanningWorker(
      [&](ModuleDependencyScanningWorker *ScanningWorker) {
        std::vector<std::string> headerFileInputs;
        std::optional<std::string> includeTreeID;
        std::vector<std::string> bridgingHeaderCommandLine;
        auto headerScan = ScanningWorker->scanHeaderDependenciesOfSwiftModule(
            *ScanningWorker->workerASTContext,
            moduleID, headerPath, sourceBufferRef,
            cache,
            headerClangModuleDependencies, headerFileInputs,
            bridgingHeaderCommandLine, includeTreeID);
        if (!headerScan) {
          // Record direct header Clang dependencies
          cache.setHeaderClangDependencies(
              moduleID, headerClangModuleDependencies.getArrayRef());
          // Record include Tree ID
          if (includeTreeID)
            moduleDependencyInfo.addBridgingHeaderIncludeTree(*includeTreeID);
          // Record the bridging header command line
          if (isTextualModuleWithABridgingHeader)
            moduleDependencyInfo.updateBridgingHeaderCommandLine(
                bridgingHeaderCommandLine);
          moduleDependencyInfo.setHeaderSourceFiles(headerFileInputs);
          // Update the dependency in the cache
          cache.updateDependency(moduleID, moduleDependencyInfo);
        } else {
          // Failure to scan header
        }
        return true;
      });
  cache.setHeaderClangDependencies(moduleID,
                                   headerClangModuleDependencies.getArrayRef());
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
              moduleIdentifier, cache.getModuleOutputPath(),
              cache.getSDKModuleOutputPath(),
              cache.getScanService().getPrefixMapper());
        });
    swiftOverlayLookupResult.insert_or_assign(moduleName, moduleDependencies);
  };

  // Enque asynchronous lookup tasks
  for (const auto &clangDep : allClangDependencies)
    ScanningThreadPool.async(scanForSwiftDependency, getModuleImportIdentifier(clangDep));
  ScanningThreadPool.wait();

  // Aggregate both previously-cached and freshly-scanned module results
  auto recordResult = [this, &cache, &swiftOverlayLookupResult,
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
        cache.recordDependencies(lookupResult.value(), Diagnostics);
        if (!lookupResult.value().empty())
          swiftOverlayDependencies.insert({moduleName, lookupResult.value()[0].first.Kind});
      }
    }
  };
  for (const auto &clangDep : allClangDependencies)
    recordResult(clangDep);

  // C++ Interop requires additional handling
  bool lookupCxxStdLibOverlay = ScanCompilerInvocation.getLangOptions().EnableCXXInterop;
  if (lookupCxxStdLibOverlay && moduleID.Kind == ModuleDependencyKind::SwiftInterface) {
    const auto &moduleInfo = cache.findKnownDependency(moduleID);
    const auto commandLine = moduleInfo.getCommandline();
    // If the textual interface was built without C++ interop, do not query
    // the C++ Standard Library Swift overlay for its compilation.
    //
    // FIXME: We always declare the 'Darwin' module as formally having been built
    // without C++Interop, for compatibility with prior versions. Once we are certain
    // that we are only building against modules built with support of
    // '-formal-cxx-interoperability-mode', this hard-coded check should be removed.
    if (moduleID.ModuleName == "Darwin" ||
        llvm::find(commandLine, "-formal-cxx-interoperability-mode=off") !=
         commandLine.end())
      lookupCxxStdLibOverlay = false;
  }

  if (lookupCxxStdLibOverlay) {
    for (const auto &clangDepName : allClangDependencies) {
      // If this Clang module is a part of the C++ stdlib, and we haven't
      // loaded the overlay for it so far, it is a split libc++ module (e.g.
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
        scanForSwiftDependency(
            getModuleImportIdentifier(ScanASTContext.Id_CxxStdlib.str()));
        recordResult(ScanASTContext.Id_CxxStdlib.str().str());
        break;
      }
    }
  }

  // Resolve the dependency info with Swift overlay dependency module information.
  cache.setSwiftOverlayDependencies(moduleID, swiftOverlayDependencies.getArrayRef());
}

void ModuleDependencyScanner::resolveCrossImportOverlayDependencies(
    StringRef mainModuleName,
    ModuleDependenciesCache &cache,
    llvm::function_ref<void(ModuleDependencyID)> action) {
  // Modules explicitly imported. Only these can be secondary module.
  llvm::SetVector<Identifier> newOverlays;
  std::set<std::pair<std::string, std::string>> overlayFiles;
  discoverCrossImportOverlayFiles(mainModuleName, cache, ScanASTContext,
                                  newOverlays, overlayFiles);

  // No new cross-import overlays are found, return.
  if (newOverlays.empty())
    return;

  // Construct a dummy main to resolve the newly discovered cross import
  // overlays.
  StringRef dummyMainName = "MainModuleCrossImportOverlays";
  auto dummyMainID = ModuleDependencyID{dummyMainName.str(),
                                        ModuleDependencyKind::SwiftSource};
  auto actualMainID = ModuleDependencyID{mainModuleName.str(),
                                         ModuleDependencyKind::SwiftSource};
  auto dummyMainDependencies =
     ModuleDependencyInfo::forSwiftSourceModule();
  std::for_each(newOverlays.begin(), newOverlays.end(),
                [&](Identifier modName) {
                  dummyMainDependencies.addModuleImport(modName.str(),
                                                        /* isExported */ false);
                });

  // Record the dummy main module's direct dependencies. The dummy main module
  // only directly depend on these newly discovered overlay modules.
  if (cache.findDependency(dummyMainID))
    cache.updateDependency(dummyMainID, dummyMainDependencies);
  else
    cache.recordDependency(dummyMainName, dummyMainDependencies);

  ModuleDependencyIDSetVector allModules =
    resolveImportedModuleDependencies(dummyMainID, cache);

  // Update main module's dependencies to include these new overlays.
  auto newOverlayDeps = cache.getAllDependencies(dummyMainID);
  cache.setCrossImportOverlayDependencies(actualMainID, newOverlayDeps.getArrayRef());

  // Update the command-line on the main module to
  // disable implicit cross-import overlay search.
  auto mainDep = cache.findKnownDependency(actualMainID);
  std::vector<std::string> cmdCopy = mainDep.getCommandline();
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

llvm::Error ModuleDependencyScanner::performBridgingHeaderChaining(
    const ModuleDependencyID &rootModuleID, ModuleDependenciesCache &cache,
    ModuleDependencyIDSetVector &allModules) {
  if (rootModuleID.Kind != ModuleDependencyKind::SwiftSource)
    return llvm::Error::success();

  bool hasBridgingHeader = false;
  llvm::vfs::OnDiskOutputBackend outputBackend;

  SmallString<256> outputPath(
      ScanCompilerInvocation.getFrontendOptions().ScannerOutputDir);

  if (outputPath.empty())
    outputPath = "/<compiler-generated>";

  llvm::sys::path::append(
      outputPath, ScanCompilerInvocation.getFrontendOptions().ModuleName + "-" +
                      ScanCompilerInvocation.getModuleScanningHash() +
                      "-ChainedBridgingHeader.h");

  llvm::SmallString<256> sourceBuf;
  llvm::raw_svector_ostream outOS(sourceBuf);

  // Iterate through all the modules and collect all the bridging header
  // and chain them into a single file. The allModules list is in the order of
  // discover, thus providing stable ordering for a deterministic generated
  // buffer.
  auto FS = ScanASTContext.SourceMgr.getFileSystem();
  for (const auto &moduleID: allModules) {
    if (moduleID.Kind != ModuleDependencyKind::SwiftSource &&
        moduleID.Kind != ModuleDependencyKind::SwiftBinary)
      continue;

    auto moduleDependencyInfo = cache.findKnownDependency(moduleID);
    if (auto *binaryMod = moduleDependencyInfo.getAsSwiftBinaryModule()) {
      if (!binaryMod->headerImport.empty()) {
        hasBridgingHeader = true;
        if (FS->exists(binaryMod->headerImport)) {
          outOS << "#include \"" << binaryMod->headerImport << "\"\n";
        } else {
          // Extract the embedded bridging header
          auto moduleBuf = FS->getBufferForFile(binaryMod->compiledModulePath);
          if (!moduleBuf)
            return llvm::errorCodeToError(moduleBuf.getError());

          auto content = extractEmbeddedBridgingHeaderContent(
              std::move(*moduleBuf), ScanASTContext);
          if (content.empty())
            return llvm::createStringError("can't load embedded header from " +
                                           binaryMod->compiledModulePath);

          outOS << content << "\n";
        }
      }
    } else if (auto *srcMod = moduleDependencyInfo.getAsSwiftSourceModule()) {
      if (srcMod->textualModuleDetails.bridgingHeaderFile) {
        hasBridgingHeader = true;
        outOS << "#include \""
              << *srcMod->textualModuleDetails.bridgingHeaderFile << "\"\n";
      }
    }
  }

  if (!hasBridgingHeader)
    return llvm::Error::success();

  if (ScanCompilerInvocation.getFrontendOptions().WriteScannerOutput) {
    auto outFile = outputBackend.createFile(outputPath);
    if (!outFile)
      return outFile.takeError();
    *outFile << sourceBuf;
    if (auto err = outFile->keep())
      return err;
  }

  auto sourceBuffer =
      llvm::MemoryBuffer::getMemBufferCopy(sourceBuf, outputPath);
  // Scan and update the main module dependency.
  auto mainModuleDeps = cache.findKnownDependency(rootModuleID);
  ModuleDependencyIDSetVector headerClangModuleDependencies;
  std::optional<std::string> includeTreeID;
  auto err = withDependencyScanningWorker(
      [&](ModuleDependencyScanningWorker *ScanningWorker) -> llvm::Error {
        std::vector<std::string> headerFileInputs;
        std::vector<std::string> bridgingHeaderCommandLine;
        if (ScanningWorker->scanHeaderDependenciesOfSwiftModule(
                *ScanningWorker->workerASTContext,
                rootModuleID, /*headerPath=*/std::nullopt,
                sourceBuffer->getMemBufferRef(), cache,
                headerClangModuleDependencies, headerFileInputs,
                bridgingHeaderCommandLine, includeTreeID))
          return llvm::createStringError(
              "failed to scan generated bridging header " + outputPath);

        cache.setHeaderClangDependencies(
            rootModuleID, headerClangModuleDependencies.getArrayRef());
        // Record include Tree ID
        if (includeTreeID) {
          // Save the old include tree ID inside the CAS for lookup. Old include
          // tree can be used to create embedded header for the original
          // bridging header.
          if (auto embeddedHeaderIncludeTree =
                  mainModuleDeps.getBridgingHeaderIncludeTree()) {
            if (auto err = ScanningWorker->createCacheKeyForEmbeddedHeader(
                    *embeddedHeaderIncludeTree, *includeTreeID))
              return err;
          }
          mainModuleDeps.addBridgingHeaderIncludeTree(*includeTreeID);
        }
        mainModuleDeps.updateBridgingHeaderCommandLine(
            bridgingHeaderCommandLine);
        mainModuleDeps.setHeaderSourceFiles(headerFileInputs);
        mainModuleDeps.setChainedBridgingHeaderBuffer(
            outputPath, sourceBuffer->getBuffer());
        // Update the dependency in the cache
        cache.updateDependency(rootModuleID, mainModuleDeps);
        return llvm::Error::success();
      });

  if (err)
    return err;

  cache.setHeaderClangDependencies(rootModuleID,
                                   headerClangModuleDependencies.getArrayRef());

  llvm::for_each(
      headerClangModuleDependencies,
      [&allModules](const ModuleDependencyID &dep) { allModules.insert(dep); });

  return llvm::Error::success();
}

void ModuleDependencyScanner::diagnoseScannerFailure(
    const ScannerImportStatementInfo &moduleImport,
    const ModuleDependenciesCache &cache,
    std::optional<ModuleDependencyID> dependencyOf) {
  SourceLoc importLoc = SourceLoc();
  if (!moduleImport.importLocations.empty()) {
    auto locInfo = moduleImport.importLocations[0];
    importLoc = Diagnostics.SourceMgr.getLocFromExternalSource(
        locInfo.bufferIdentifier, locInfo.lineNumber, locInfo.columnNumber);
  }

  // Attempt to determine if any of the binary Swift module dependencies contain
  // serialized search paths where the missing module may be found. If yes,
  // emit a specialized diagnostic letting the user know which search path
  // is missing in current compilation.
  auto resolvedModuleDefiningPath = attemptToFindResolvingSerializedSearchPath(
      moduleImport, cache, importLoc);
  if (resolvedModuleDefiningPath) {
    Diagnostics.diagnose(
        importLoc,
        diag::dependency_scan_module_not_found_on_specified_search_paths,
        moduleImport.importIdentifier);
    Diagnostics.diagnose(importLoc, diag::inherited_search_path_resolves_module,
                         moduleImport.importIdentifier,
                         resolvedModuleDefiningPath->first.ModuleName,
                         resolvedModuleDefiningPath->second);
  } else
    Diagnostics.diagnose(importLoc, diag::dependency_scan_module_not_found,
                         moduleImport.importIdentifier);

  // Emit notes for every link in the dependency chain from the root
  // module-under-scan to the module whose import failed to resolve.
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
        Diagnostics.diagnose(importLoc,
                             diag::dependency_as_imported_by_main_module,
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

      Diagnostics.diagnose(importLoc, diag::dependency_as_imported_by,
                           entry.ModuleName, moduleFilePath, isClang);
    }
  }

  // Emit notes for every other location where the failed-to-resolve
  // module is imported.
  if (moduleImport.importLocations.size() > 1) {
    for (size_t i = 1; i < moduleImport.importLocations.size(); ++i) {
      auto locInfo = moduleImport.importLocations[i];
      auto importLoc = Diagnostics.SourceMgr.getLocFromExternalSource(
          locInfo.bufferIdentifier, locInfo.lineNumber, locInfo.columnNumber);
      Diagnostics.diagnose(importLoc, diag::unresolved_import_location);
    }
  }
}

static std::string getModuleDefiningPath(const ModuleDependencyInfo &info) {
  std::string path = "";
  switch (info.getKind()) {
  case swift::ModuleDependencyKind::SwiftInterface:
    path = info.getAsSwiftInterfaceModule()->swiftInterfaceFile;
    break;
  case swift::ModuleDependencyKind::SwiftBinary:
    path = info.getAsSwiftBinaryModule()->compiledModulePath;
    break;
  case swift::ModuleDependencyKind::SwiftPlaceholder:
    path = info.getAsPlaceholderDependencyModule()->compiledModulePath;
    break;
  case swift::ModuleDependencyKind::Clang:
    path = info.getAsClangModule()->moduleMapFile;
    break;
  case swift::ModuleDependencyKind::SwiftSource:
  default:
    llvm_unreachable("Unexpected dependency kind");
  }

  // Relative to the `module.modulemap` or `.swiftinterface` or `.swiftmodule`,
  // the defininig path is the parent directory of the file.
  return llvm::sys::path::parent_path(path).str();
}

std::optional<std::pair<ModuleDependencyID, std::string>>
ModuleDependencyScanner::attemptToFindResolvingSerializedSearchPath(
    const ScannerImportStatementInfo &moduleImport,
    const ModuleDependenciesCache &cache, const SourceLoc &importLoc) {
  std::set<ModuleDependencyID> binarySwiftModuleDepIDs =
      collectBinarySwiftDeps(cache);

  std::optional<std::pair<ModuleDependencyID, std::string>> result;
  for (const auto &binaryDepID : binarySwiftModuleDepIDs) {
    auto binaryModInfo =
        cache.findKnownDependency(binaryDepID).getAsSwiftBinaryModule();
    assert(binaryModInfo);
    if (binaryModInfo->serializedSearchPaths.empty())
      continue;

    // Note: this will permanently mutate this worker with additional search
    // paths. That's fine because we are diagnosing a scan failure here, but
    // worth being aware of.
    result = withDependencyScanningWorker(
        [&binaryModInfo, &moduleImport, &cache, this,
         &binaryDepID](ModuleDependencyScanningWorker *ScanningWorker)
            -> std::optional<std::pair<ModuleDependencyID, std::string>> {
          ModuleDependencyVector result;
          for (const auto &sp : binaryModInfo->serializedSearchPaths)
            ScanningWorker->workerASTContext->addSearchPath(
                sp.Path, sp.IsFramework, sp.IsSystem);

          result = ScanningWorker->scanFilesystemForSwiftModuleDependency(
              getModuleImportIdentifier(moduleImport.importIdentifier),
              cache.getModuleOutputPath(), cache.getSDKModuleOutputPath(),
              cache.getScanService().getPrefixMapper());
          if (!result.empty())
            return std::make_pair(binaryDepID,
                                  getModuleDefiningPath(result[0].second));

          result = ScanningWorker->scanFilesystemForClangModuleDependency(
              getModuleImportIdentifier(moduleImport.importIdentifier),
              cache.getModuleOutputPath(), cache.getSDKModuleOutputPath(), {},
              cache.getScanService().getPrefixMapper());
          if (!result.empty())
            return std::make_pair(binaryDepID,
                                  getModuleDefiningPath(result[0].second));
          return std::nullopt;
        });
    if (result)
      break;
  }

  return result;
}
