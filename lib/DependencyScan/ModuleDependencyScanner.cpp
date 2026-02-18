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

#include "swift/DependencyScan/ModuleDependencyScanner.h"
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
#include "swift/Basic/Statistic.h"
#include "swift/ClangImporter/ClangImporter.h"
#include "swift/Frontend/CompileJobCacheKey.h"
#include "swift/Frontend/ModuleInterfaceLoader.h"
#include "swift/Serialization/ScanningLoaders.h"
#include "swift/Serialization/SerializedModuleLoader.h"
#include "swift/Subsystems.h"
#include "clang/CAS/IncludeTree.h"
#include "clang/Frontend/CompilerInstance.h"
#include "llvm/ADT/IntrusiveRefCntPtr.h"
#include "llvm/ADT/SetOperations.h"
#include "llvm/CAS/CASProvidingFileSystem.h"
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

    auto optionalDepNode =
        cache.findDependency(dep.importIdentifier, lookupKind);
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
         Kind == ModuleDependencyKind::SwiftBinary;
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

std::string ModuleDependencyScanner::clangModuleOutputPathLookup(
    const clang::tooling::dependencies::ModuleDeps &clangDeps,
    clang::tooling::dependencies::ModuleOutputKind moduleOutputKind) const {
  llvm::SmallString<128> outputPath(ModuleOutputPath);
  if (clangDeps.IsInStableDirectories)
    outputPath = SDKModuleOutputPath;

  auto runtimeResourcePath = ScanASTContext.SearchPathOpts.RuntimeResourcePath;

  // FIXME: This is a hack to treat Clang modules defined in the compiler's
  // own resource directory as stable, when they are not reported as such
  // by the Clang scanner.
  if (!runtimeResourcePath.empty() &&
      hasPrefix(llvm::sys::path::begin(clangDeps.ClangModuleMapFile),
                llvm::sys::path::end(clangDeps.ClangModuleMapFile),
                llvm::sys::path::begin(runtimeResourcePath),
                llvm::sys::path::end(runtimeResourcePath)))
    outputPath = SDKModuleOutputPath;

  llvm::sys::path::append(outputPath, clangDeps.ID.ModuleName + "-" +
                                          clangDeps.ID.ContextHash);
  switch (moduleOutputKind) {
  case clang::tooling::dependencies::ModuleOutputKind::ModuleFile:
    llvm::sys::path::replace_extension(
        outputPath, getExtension(swift::file_types::TY_ClangModuleFile));
    break;
  case clang::tooling::dependencies::ModuleOutputKind::DependencyFile:
    llvm::sys::path::replace_extension(
        outputPath, getExtension(swift::file_types::TY_Dependencies));
    break;
  case clang::tooling::dependencies::ModuleOutputKind::DependencyTargets:
    return clangDeps.ID.ModuleName + "-" + clangDeps.ID.ContextHash;
  case clang::tooling::dependencies::ModuleOutputKind::
      DiagnosticSerializationFile:
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

static llvm::IntrusiveRefCntPtr<llvm::vfs::FileSystem>
getClangScanningFS(SwiftDependencyScanningService &service,
                   std::shared_ptr<llvm::cas::ObjectStore> cas,
                   ASTContext &ctx) {
  auto *importer = static_cast<ClangImporter *>(ctx.getClangModuleLoader());
  // Dependency scanner needs to create its own file system per worker.
  auto fs = ClangImporter::computeClangImporterFileSystem(
      ctx, importer->getClangFileMapping(),
      llvm::vfs::createPhysicalFileSystem(), true,
      [&](StringRef str) { return service.save(str); });

  if (cas)
    return llvm::cas::createCASProvidingFileSystem(cas, fs);
  return fs;
}

ModuleDependencyScanningWorker::ModuleDependencyScanningWorker(
    SwiftDependencyScanningService &globalScanningService,
    const CompilerInvocation &ScanCompilerInvocation,
    const SILOptions &SILOptions, ASTContext &ScanASTContext,
    swift::DependencyTracker &DependencyTracker,
    std::shared_ptr<llvm::cas::ObjectStore> CAS,
    std::shared_ptr<llvm::cas::ActionCache> ActionCache,
    DependencyScannerDiagnosticReporter &DiagnosticReporter,
    llvm::PrefixMapper *Mapper)
    : workerCompilerInvocation(
          std::make_unique<CompilerInvocation>(ScanCompilerInvocation)),
      clangScanningTool(
          *globalScanningService.ClangScanningService,
          getClangScanningFS(globalScanningService, CAS, ScanASTContext)),
      CAS(CAS), ActionCache(ActionCache),
      diagnosticReporter(DiagnosticReporter) {
  // Instantiate a worker-specific diagnostic engine and copy over
  // the scanner's diagnostic consumers (expected to be thread-safe).
  workerDiagnosticEngine = std::make_unique<DiagnosticEngine>(ScanASTContext.SourceMgr);
  for (auto &scannerDiagConsumer : DiagnosticReporter.Diagnostics.getConsumers())
    workerDiagnosticEngine->addConsumer(*scannerDiagConsumer);

  workerASTContext = std::unique_ptr<ASTContext>(
      ASTContext::get(workerCompilerInvocation->getLangOptions(),
                      workerCompilerInvocation->getTypeCheckerOptions(),
                      workerCompilerInvocation->getSILOptions(),
                      workerCompilerInvocation->getSearchPathOptions(),
                      workerCompilerInvocation->getClangImporterOptions(),
                      workerCompilerInvocation->getSymbolGraphOptions(),
                      workerCompilerInvocation->getCASOptions(),
                      workerCompilerInvocation->getSerializationOptions(),
                      ScanASTContext.SourceMgr, *workerDiagnosticEngine));

  scanningASTDelegate = std::make_unique<InterfaceSubContextDelegateImpl>(
      workerASTContext->SourceMgr, workerDiagnosticEngine.get(),
      workerASTContext->SearchPathOpts, workerASTContext->LangOpts,
      workerASTContext->ClangImporterOpts, workerASTContext->CASOpts,
      workerCompilerInvocation->getFrontendOptions(),
      /* buildModuleCacheDirIfAbsent */ false,
      getModuleCachePathFromClang(
          ScanASTContext.getClangModuleLoader()->getClangInstance()),
      workerCompilerInvocation->getFrontendOptions()
          .PrebuiltModuleCachePath,
      workerCompilerInvocation->getFrontendOptions()
          .BackupModuleInterfaceDir,
      workerCompilerInvocation->getFrontendOptions().CacheReplayPrefixMap,
      workerCompilerInvocation->getFrontendOptions()
          .SerializeModuleInterfaceDependencyHashes,
      workerCompilerInvocation->getFrontendOptions()
          .shouldTrackSystemDependencies()
      );

  auto loader = std::make_unique<PluginLoader>(
      *workerASTContext, /*DepTracker=*/nullptr,
      workerCompilerInvocation->getFrontendOptions().CacheReplayPrefixMap,
      workerCompilerInvocation->getFrontendOptions().DisableSandbox);
  workerASTContext->setPluginLoader(std::move(loader));

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
    swiftModuleClangCC1CommandLineArgs.push_back(
        "-direct-clang-cc1-module-build");
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

  // Set up the Swift module loader for Swift queries.
  swiftModuleScannerLoader = std::make_unique<SwiftModuleScanner>(
      *workerASTContext,
      workerCompilerInvocation->getSearchPathOptions().ModuleLoadMode,
      *scanningASTDelegate, swiftModuleClangCC1CommandLineArgs,
      workerCompilerInvocation->getSearchPathOptions().ExplicitSwiftModuleInputs);
}

llvm::Error ModuleDependencyScanningWorker::initializeClangScanningTool() {
  return clangScanningTool.initializeCompilerInstanceWithContext(
      clangScanningWorkingDirectoryPath, clangScanningModuleCommandLineArgs);
}

llvm::Error ModuleDependencyScanningWorker::finalizeClangScanningTool() {
  return clangScanningTool.finalizeCompilerInstanceWithContext();
}

SwiftModuleScannerQueryResult
ModuleDependencyScanningWorker::scanFilesystemForSwiftModuleDependency(
    Identifier moduleName, bool isTestableImport) {
  diagnosticReporter.registerSwiftModuleQuery();
  return swiftModuleScannerLoader->lookupSwiftModule(moduleName,
                                                     isTestableImport);
}

std::optional<clang::tooling::dependencies::TranslationUnitDeps>
ModuleDependencyScanningWorker::scanFilesystemForClangModuleDependency(
    Identifier moduleName, LookupModuleOutputCallback lookupModuleOutput,
    const llvm::DenseSet<clang::tooling::dependencies::ModuleID>
        &alreadySeenModules) {
  diagnosticReporter.registerNamedClangModuleQuery();
  auto clangModuleDependencies =
      clangScanningTool.computeDependenciesByNameWithContext(
          moduleName.str(), alreadySeenModules, lookupModuleOutput);
  if (!clangModuleDependencies) {
    llvm::handleAllErrors(
        clangModuleDependencies.takeError(),
        [this, &moduleName](const llvm::StringError &E) {
          auto &message = E.getMessage();
          if (message.find("fatal error: module '" + moduleName.str().str() +
                           "' not found") == std::string::npos)
            workerDiagnosticEngine->diagnose(
                SourceLoc(), diag::clang_dependency_scan_error, message);
        });
    return std::nullopt;
  }
  return clangModuleDependencies.get();
}

std::optional<clang::tooling::dependencies::TranslationUnitDeps>
ModuleDependencyScanningWorker::scanHeaderDependenciesOfSwiftModule(
    ModuleDependencyID moduleID,
    std::optional<StringRef> headerPath,
    std::optional<llvm::MemoryBufferRef> sourceBuffer,
    LookupModuleOutputCallback lookupModuleOutput,
    const llvm::DenseSet<clang::tooling::dependencies::ModuleID>
       &alreadySeenModules) {
  // Scan the specified textual header file and collect its dependencies
  auto scanHeaderDependencies = [&]()
      -> llvm::Expected<clang::tooling::dependencies::TranslationUnitDeps> {
    auto dependencies = clangScanningTool.getTranslationUnitDependencies(
        inputSpecificClangScannerCommand(clangScanningBaseCommandLineArgs,
                                         headerPath),
        clangScanningWorkingDirectoryPath, alreadySeenModules,
        lookupModuleOutput, sourceBuffer);
    if (!dependencies)
      return dependencies.takeError();
    return dependencies;
  };

  // - If a generated header is provided, scan the generated header.
  // - Textual module dependencies require us to process their bridging header.
  // - Binary module dependnecies may have arbitrary header inputs.
  auto clangModuleDependencies = scanHeaderDependencies();
  if (!clangModuleDependencies) {
    auto errorStr = toString(clangModuleDependencies.takeError());
    workerDiagnosticEngine->diagnose(
        SourceLoc(), diag::clang_header_dependency_scan_error, errorStr);
    return std::nullopt;
  }

  return *clangModuleDependencies;
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

Identifier
ModuleDependencyScanner::getModuleImportIdentifier(StringRef moduleName) {
  return ScanASTContext.getIdentifier(moduleName);
}

SwiftDependencyTracker::SwiftDependencyTracker(
    std::shared_ptr<llvm::cas::ObjectStore> CAS, llvm::PrefixMapper *Mapper,
    const CompilerInvocation &CI)
    : CAS(CAS), Mapper(Mapper) {
  auto &SearchPathOpts = CI.getSearchPathOptions();

  FS = llvm::cas::createCASProvidingFileSystem(
      CAS, llvm::vfs::createPhysicalFileSystem());

  auto addCommonFile = [&](StringRef path) {
    auto file = FS->openFileForRead(path);
    if (!file)
      return;
    auto status = (*file)->status();
    if (!status)
      return;

    auto casFile = dyn_cast<llvm::cas::CASBackedFile>(*file);
    if (!casFile)
      return;

    auto fileRef = casFile->getObjectRefForContent();
    std::string realPath = Mapper ? Mapper->mapToString(path) : path.str();
    CommonFiles.try_emplace(realPath, fileRef, (size_t)status->getSize());
  };

  // Add SDKSetting file.
  SmallString<256> SDKSettingPath;
  llvm::sys::path::append(SDKSettingPath, SearchPathOpts.getSDKPath(),
                          "SDKSettings.json");
  addCommonFile(SDKSettingPath);

  // Add VFSOverlay file.
  for (auto &Overlay: SearchPathOpts.VFSOverlayFiles)
    addCommonFile(Overlay);

  // Add blocklist file.
  for (auto &File: CI.getFrontendOptions().BlocklistConfigFilePaths)
    addCommonFile(File);

  // Add access notes.
  StringRef AccessNotePath = CI.getLangOptions().AccessNotesPath;
  if (!AccessNotePath.empty())
    addCommonFile(AccessNotePath);

  // const-gather-protocols-file
  StringRef ConstProtocolFile = SearchPathOpts.ConstGatherProtocolListFilePath;
  if (!ConstProtocolFile.empty())
    addCommonFile(ConstProtocolFile);
}

void SwiftDependencyTracker::startTracking(bool includeCommonDeps) {
  TrackedFiles.clear();
  if (includeCommonDeps) {
    for (auto &entry : CommonFiles)
      TrackedFiles.emplace(entry.first(), entry.second);
  }
}

bool SwiftDependencyTracker::trackFile(const Twine &path) {
  auto file = FS->openFileForRead(path);
  if (!file)
    return false;
  auto status = (*file)->status();
  if (!status)
    return false;
  auto CASFile = dyn_cast<llvm::cas::CASBackedFile>(*file);
  if (!CASFile)
    return false;
  auto fileRef = CASFile->getObjectRefForContent();
  std::string realPath =
      Mapper ? Mapper->mapToString(path.str()) : path.str();
  TrackedFiles.try_emplace(realPath, fileRef, (size_t)status->getSize());
  return true;
}

llvm::Expected<llvm::cas::ObjectProxy>
SwiftDependencyTracker::createTreeFromDependencies() {
  llvm::SmallVector<clang::cas::IncludeTree::FileList::FileEntry> Files;
  for (auto &file : TrackedFiles) {
    auto includeTreeFile = clang::cas::IncludeTree::File::create(
        *CAS, file.first, file.second.FileRef);
    if (!includeTreeFile) {
      return llvm::createStringError("CASFS createTree failed for " +
                                     file.first + ": " +
                                     toString(includeTreeFile.takeError()));
    }
    Files.push_back(
        {includeTreeFile->getRef(),
         (clang::cas::IncludeTree::FileList::FileSizeTy)file.second.Size});
  }

  auto includeTreeList =
      clang::cas::IncludeTree::FileList::create(*CAS, Files, {});
  if (!includeTreeList)
    return llvm::createStringError("casfs include-tree filelist error: " +
                                   toString(includeTreeList.takeError()));

  return *includeTreeList;
}

llvm::ErrorOr<std::unique_ptr<ModuleDependencyScanner>>
ModuleDependencyScanner::create(SwiftDependencyScanningService &service,
                                CompilerInstance *instance,
                                ModuleDependenciesCache &cache) {
  auto scanner =
      std::unique_ptr<ModuleDependencyScanner>(new ModuleDependencyScanner(
          service, cache, instance->getInvocation(), instance->getSILOptions(),
          instance->getASTContext(), *instance->getDependencyTracker(),
          instance->getSharedCASInstance(), instance->getSharedCacheInstance(),
          instance->getDiags(),
          instance->getInvocation().getFrontendOptions().ParallelDependencyScan,
          instance->getInvocation()
              .getFrontendOptions()
              .EmitDependencyScannerRemarks));

  auto initError = scanner->initializeWorkerClangScanningTool();

  if (initError) {
    llvm::handleAllErrors(
        std::move(initError), [&](const llvm::StringError &E) {
          instance->getDiags().diagnose(
              SourceLoc(), diag::clang_dependency_scan_error, E.getMessage());
        });
    return std::make_error_code(std::errc::invalid_argument);
  }

  return scanner;
}

ModuleDependencyScanner::ModuleDependencyScanner(
    SwiftDependencyScanningService &ScanningService,
    ModuleDependenciesCache &Cache,
    const CompilerInvocation &ScanCompilerInvocation,
    const SILOptions &SILOptions, ASTContext &ScanASTContext,
    swift::DependencyTracker &DependencyTracker,
    std::shared_ptr<llvm::cas::ObjectStore> CAS,
    std::shared_ptr<llvm::cas::ActionCache> ActionCache,
    DiagnosticEngine &Diagnostics, bool ParallelScan,
    bool EmitScanRemarks)
    : ScanCompilerInvocation(ScanCompilerInvocation),
      ScanASTContext(ScanASTContext),
      ScanDiagnosticReporter(Diagnostics, EmitScanRemarks),
      ModuleOutputPath(ScanCompilerInvocation.getFrontendOptions()
                       .ExplicitModulesOutputPath),
      SDKModuleOutputPath(ScanCompilerInvocation.getFrontendOptions()
                          .ExplicitSDKModulesOutputPath),
      DependencyCache(Cache),
      NumThreads(ParallelScan
                     ? llvm::hardware_concurrency().compute_thread_count()
                     : 1),
      ScanningThreadPool(llvm::hardware_concurrency(NumThreads)), CAS(CAS),
      ActionCache(ActionCache) {
  // Setup prefix mapping.
  auto &ScannerPrefixMapper =
      ScanCompilerInvocation.getSearchPathOptions().ScannerPrefixMapper;
  if (!ScannerPrefixMapper.empty()) {
    PrefixMapper = std::make_unique<llvm::PrefixMapper>();
    SmallVector<llvm::MappedPrefix, 4> Prefixes;
    llvm::MappedPrefix::transformPairs(ScannerPrefixMapper, Prefixes);
    PrefixMapper->addRange(Prefixes);
    PrefixMapper->sort();
  }

  if (CAS)
    CacheFS = cast<llvm::cas::CASBackedFileSystem>(
        llvm::cas::createCASProvidingFileSystem(
            CAS, ScanASTContext.SourceMgr.getFileSystem()));

  // TODO: Make num threads configurable
  for (size_t i = 0; i < NumThreads; ++i)
    Workers.emplace_front(std::make_unique<ModuleDependencyScanningWorker>(
        ScanningService, ScanCompilerInvocation, SILOptions, ScanASTContext,
        DependencyTracker, CAS, ActionCache, ScanDiagnosticReporter,
        PrefixMapper.get()));
}

ModuleDependencyScanner::~ModuleDependencyScanner() {
  auto finError = finalizeWorkerClangScanningTool();
  assert(!finError && "ClangScanningTool finalization must succeed.");
}

llvm::Error ModuleDependencyScanner::initializeWorkerClangScanningTool() {
  for (auto &W : Workers) {
    if (auto error = W->initializeClangScanningTool())
      return error;
  }
  return llvm::Error::success();
}

llvm::Error ModuleDependencyScanner::finalizeWorkerClangScanningTool() {
  for (auto &W : Workers) {
    if (auto error = W->finalizeClangScanningTool())
      return error;
  }
  return llvm::Error::success();
}

static std::set<ModuleDependencyID>
collectBinarySwiftDeps(const ModuleDependenciesCache &cache) {
  std::set<ModuleDependencyID> binarySwiftModuleDepIDs;
  auto binaryDepsMap =
      cache.getDependenciesMap(ModuleDependencyKind::SwiftBinary);
  for (const auto &binaryDepName : binaryDepsMap.keys())
    binarySwiftModuleDepIDs.insert(ModuleDependencyID{
        binaryDepName.str(), ModuleDependencyKind::SwiftBinary});
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

  auto mainDependencies =
      ModuleDependencyInfo::forSwiftSourceModule({}, buildCommands, {}, {}, {});

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
      mainDependencies.addModuleImport("Swift", /* isExported */ false,
                                       AccessLevel::Public,
                                       &alreadyAddedModules);
      break;
    }

    if (ScanASTContext.LangOpts.EnableCXXInterop) {
      StringRef mainModuleName = mainModule->getName().str();
      if (mainModuleName != CXX_MODULE_NAME)
        mainDependencies.addModuleImport(CXX_MODULE_NAME, /* isExported */ false,
                                         AccessLevel::Public,
                                         &alreadyAddedModules);
      if (llvm::none_of(llvm::ArrayRef<StringRef>{CXX_MODULE_NAME,
                            ScanASTContext.Id_CxxStdlib.str(), "std"},
                        [mainModuleName](StringRef Name) {
                          return mainModuleName == Name;
                        }))
        mainDependencies.addModuleImport(ScanASTContext.Id_CxxStdlib.str(),
                                         /* isExported */ false,
                                         AccessLevel::Public,
                                         &alreadyAddedModules);
    }

    // Add any implicit module names.
    for (const auto &import : importInfo.AdditionalUnloadedImports) {
      mainDependencies.addModuleImport(
          import.module.getModulePath(),
          import.options.contains(ImportFlags::Exported), import.accessLevel,
          &alreadyAddedModules, &ScanASTContext.SourceMgr);
    }

    // Already-loaded, implicitly imported module names.
    for (const auto &import : importInfo.AdditionalImports) {
      mainDependencies.addModuleImport(
          import.module.importedModule->getNameStr(),
          import.options.contains(ImportFlags::Exported), import.accessLevel,
          &alreadyAddedModules);
    }

    // Add the bridging header.
    if (!importInfo.BridgingHeaderPath.empty()) {
      mainDependencies.addBridgingHeader(importInfo.BridgingHeaderPath);
    }

    // If we are to import the underlying Clang module of the same name,
    // add a dependency with the same name to trigger the search.
    if (importInfo.ShouldImportUnderlyingModule) {
      mainDependencies.addModuleImport(
          mainModule->getName().str(),
          /* isExported */ true, AccessLevel::Public, &alreadyAddedModules);
    }

    // All modules specified with `-embed-tbd-for-module` are treated as
    // implicit dependnecies for this compilation since they are not guaranteed
    // to be impored in the source.
    for (const auto &tbdSymbolModule :
         ScanCompilerInvocation.getTBDGenOptions().embedSymbolsFromModules) {
      mainDependencies.addModuleImport(
          tbdSymbolModule,
          /* isExported */ false, AccessLevel::Public, &alreadyAddedModules);
    }
  }

  // Add source-specified `import` dependencies
  {
    for (auto fileUnit : mainModule->getFiles()) {
      auto sourceFile = dyn_cast<SourceFile>(fileUnit);
      if (!sourceFile)
        continue;

      mainDependencies.addModuleImports(*sourceFile, alreadyAddedModules,
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
    ModuleDependenciesCache &cache, ASTContext &scanASTContext,
    llvm::SetVector<Identifier> &newOverlays,
    std::set<std::pair<std::string, std::string>> &overlayFiles) {
  auto mainModuleID = ModuleDependencyID{cache.getMainModuleName().str(),
                                         ModuleDependencyKind::SwiftSource};
  auto mainModuleInfo = cache.findKnownDependency(mainModuleID);

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
         const ModuleDependencyIDSet &directClangDepsSet)
      -> std::optional<ModuleDependencyID> {
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
      [&mainModuleID, &cache, &scanASTContext, &newOverlays,
       &overlayFiles](const ModuleDependencyIDSet &inputDependencies) {
        for (auto moduleID : inputDependencies) {
          auto moduleName = moduleID.ModuleName;
          // Do not look for overlays of main module under scan
          if (moduleName == mainModuleID.ModuleName)
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
            if (moduleName == mainModuleID.ModuleName)
              continue;
            // check if any explicitly imported modules can serve as a
            // secondary module, and add the overlay names to the
            // dependencies list.
            for (auto overlayName : overlayMap[moduleName]) {
              if (overlayName.str() != mainModuleID.ModuleName &&
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
ModuleDependencyScanner::performDependencyScan(ModuleDependencyID rootModuleID) {
  PrettyStackTraceStringAction trace("Performing dependency scan of: ",
                                     rootModuleID.ModuleName);
  // If scanning for an individual Clang module, simply resolve its imports
  if (rootModuleID.Kind == ModuleDependencyKind::Clang) {
    ModuleDependencyIDSetVector discoveredClangModules;
    resolveClangModuleDependencies({}, discoveredClangModules);
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
    resolveImportedModuleDependencies(rootModuleID);

  // 5. Resolve cross-import overlays
  // This must only be done for the main source module, since textual and
  // binary Swift modules already encode their dependencies on cross-import
  // overlays with explicit imports.
  if (ScanCompilerInvocation.getLangOptions().EnableCrossImportOverlays)
    resolveCrossImportOverlayDependencies(
        [&](ModuleDependencyID id) { allModules.insert(id); });

  if (ScanCompilerInvocation.getSearchPathOptions().BridgingHeaderChaining) {
    auto err = performBridgingHeaderChaining(rootModuleID, allModules);
    if (err)
      ScanDiagnosticReporter.Diagnostics.diagnose(SourceLoc(),
                                         diag::error_scanner_extra,
                                         toString(std::move(err)));
  }

  ScanDiagnosticReporter.emitScanMetrics(DependencyCache);
  return allModules.takeVector();
}

ModuleDependencyIDSetVector
ModuleDependencyScanner::resolveImportedModuleDependencies(
    const ModuleDependencyID &rootModuleID) {
  ModuleDependencyIDSetVector allModules;
  PrettyStackTraceStringAction trace(
      "Resolving transitive closure of dependencies of: ",
      rootModuleID.ModuleName);

  // Resolve all imports for which a Swift module can be found,
  // transitively, starting at 'rootModuleID'.
  ModuleDependencyIDSetVector discoveredSwiftModules;
  resolveSwiftModuleDependencies(rootModuleID, discoveredSwiftModules);
  allModules.insert(discoveredSwiftModules.begin(),
                    discoveredSwiftModules.end());

  // Resolve all remaining unresolved imports for which no Swift
  // module could be found, assuming them to be Clang modules.
  // This operation is done by gathering all unresolved import
  // identifiers and querying them in-parallel to the Clang
  // dependency scanner.
  resolveClangModuleDependencies(discoveredSwiftModules.getArrayRef(),
                                 allModules);

  // For each discovered Swift module which was built with a
  // bridging header, scan the header for module dependencies.
  // This includes the source module bridging header.
  resolveHeaderDependencies(discoveredSwiftModules.getArrayRef(), allModules);

  // For each Swift module which imports Clang modules,
  // query whether all visible Clang dependencies from such imports
  // have a Swift overaly module.
  resolveSwiftOverlayDependencies(discoveredSwiftModules.getArrayRef(),
                                  allModules);

  return allModules;
}

void ModuleDependencyScanner::resolveSwiftModuleDependencies(
    const ModuleDependencyID &rootModuleID,
    ModuleDependencyIDSetVector &allDiscoveredSwiftModules) {
  PrettyStackTraceStringAction trace(
      "Resolving transitive closure of Swift dependencies of: ",
      rootModuleID.ModuleName);
  // Clang modules cannot have Swift module dependencies
  if (!isSwiftDependencyKind(rootModuleID.Kind))
    return;

  allDiscoveredSwiftModules.insert(rootModuleID);
  for (unsigned currentModuleIdx = 0;
       currentModuleIdx < allDiscoveredSwiftModules.size();
       ++currentModuleIdx) {
    auto moduleID = allDiscoveredSwiftModules[currentModuleIdx];
    auto moduleDependencyInfo = DependencyCache.findKnownDependency(moduleID);

    // If this dependency module's Swift imports are already resolved,
    // we do not need to scan it.
    if (!moduleDependencyInfo.getImportedSwiftDependencies().empty()) {
      for (const auto &dep :
           moduleDependencyInfo.getImportedSwiftDependencies())
        allDiscoveredSwiftModules.insert(dep);
    } else {
      // Find the Swift dependencies of every module this module directly
      // depends on.
      ModuleDependencyIDSetVector importedSwiftDependencies;
      resolveSwiftImportsForModule(moduleID, importedSwiftDependencies);
      allDiscoveredSwiftModules.insert(importedSwiftDependencies.begin(),
                                       importedSwiftDependencies.end());
    }
  }
  return;
}

static void findAllReachableClangModules(ModuleDependencyID moduleID,
                                         const ModuleDependenciesCache &cache,
                                         ModuleDependencyIDSetVector &reachableClangModules) {
  if (!reachableClangModules.insert(moduleID))
    return;
  for (const auto &depID : cache.getImportedClangDependencies(moduleID))
    findAllReachableClangModules(depID, cache, reachableClangModules);
}

static void gatherUnresolvedImports(
    ModuleDependenciesCache &cache, ASTContext &scanASTContext,
    ArrayRef<ModuleDependencyID> swiftModuleDependents,
    ModuleDependencyIDSetVector &allDiscoveredClangModules,
    ImportStatementInfoMap &unresolvedImportsMap,
    ImportStatementInfoMap &unresolvedOptionalImportsMap,
    ModuleIDToModuleIDSetVectorMap &resolvedClangDependenciesMap) {
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

    // If we have already fully resolved Clang dependencies for this module,
    // then we have the entire dependency sub-graph already computed for
    // it and ready to be added to 'allDiscoveredClangModules' without
    // additional scanning.
    if (!moduleDependencyInfo.getImportedClangDependencies().empty()) {
      auto directClangDeps = cache.getImportedClangDependencies(moduleID);
      ModuleDependencyIDSetVector reachableClangModules;
      for (const auto &depID : directClangDeps)
        findAllReachableClangModules(depID, cache, reachableClangModules);
      allDiscoveredClangModules.insert(
          reachableClangModules.getArrayRef().begin(),
          reachableClangModules.getArrayRef().end());
      continue;
    } else {
      llvm::StringSet<> resolvedImportIdentifiers;
      // Mark all import identifiers which were resolved to Swift dependencies
      // as resolved
      for (const auto &resolvedDep :
           moduleDependencyInfo.getImportedSwiftDependencies())
        resolvedImportIdentifiers.insert(resolvedDep.ModuleName);

      // Mark all import identifiers which can be fully resolved to a cached
      // Clang dependency (including visible modules) as resolved
      auto markCachedClangDependenciesResolved =
          [&cache, &moduleID, &resolvedImportIdentifiers,
           &resolvedClangDependenciesMap](
              ArrayRef<ScannerImportStatementInfo> imports) {
            for (const auto &import : imports) {
              auto &importIdentifier = import.importIdentifier;
              if (!resolvedImportIdentifiers.contains(importIdentifier) &&
                  cache.hasClangDependency(importIdentifier) &&
                  cache.hasVisibleClangModulesFromLookup(importIdentifier)) {
                resolvedImportIdentifiers.insert(importIdentifier);
                resolvedClangDependenciesMap[moduleID].insert(
                    {importIdentifier, ModuleDependencyKind::Clang});
              }
            }
          };
      markCachedClangDependenciesResolved(
          moduleDependencyInfo.getModuleImports());
      markCachedClangDependenciesResolved(
          moduleDependencyInfo.getOptionalModuleImports());

      // When querying a *clang* module 'CxxStdlib' we must
      // instead expect a module called 'std'...
      auto addCanonicalClangModuleImport =
          [&scanASTContext](
              const ScannerImportStatementInfo &importInfo,
              std::vector<ScannerImportStatementInfo> &unresolvedImports) {
            if (importInfo.importIdentifier ==
                scanASTContext.Id_CxxStdlib.str()) {
              auto canonicalImportInfo = ScannerImportStatementInfo(
                  "std", importInfo.isExported, importInfo.accessLevel,
                  importInfo.importLocations);
              unresolvedImports.push_back(canonicalImportInfo);
            } else {
              unresolvedImports.push_back(importInfo);
            }
          };

      // We need to query the Clang dependency scanner for all of this module's
      // unresolved imports
      for (const auto &depImport : moduleDependencyInfo.getModuleImports())
        if (!resolvedImportIdentifiers.contains(depImport.importIdentifier))
          addCanonicalClangModuleImport(depImport, *unresolvedImports);
      for (const auto &depImport :
           moduleDependencyInfo.getOptionalModuleImports())
        if (!resolvedImportIdentifiers.contains(depImport.importIdentifier))
          addCanonicalClangModuleImport(depImport, *unresolvedOptionalImports);
    }
  }
}

void ModuleDependencyScanner::reQueryMissedModulesFromCache(
    const std::vector<ModuleIDImportInfoPair> &failedToResolveImports,
    ModuleIDToModuleIDSetVectorMap &resolvedClangDependenciesMap) {
  // It is possible that a specific import resolution failed because we are
  // attempting to resolve a module which can only be brought in via a
  // modulemap of a different Clang module dependency which is not otherwise
  // on the current search paths. For example, suppose we are scanning a
  // `.swiftinterface` for module `Foo`, which contains:
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
  // the auxiliary module becuase the Clang scanner instance scanning for
  // clang module Foo will be able to find it in the corresponding framework
  // module's modulemap and register it as a dependency which means it will be
  // registered with the scanner's cache in the step above. To handle such
  // cases, we first add all successfully-resolved modules and (for Clang
  // modules) their transitive dependencies to the cache, and then attempt to
  // re-query imports for which resolution originally failed from the cache.
  // If this fails, then the scanner genuinely failed to resolve this
  // dependency.
  for (const auto &unresolvedImport : failedToResolveImports) {
    auto unresolvedModuleID = ModuleDependencyID{
        unresolvedImport.second.importIdentifier, ModuleDependencyKind::Clang};
    auto optionalCachedModuleInfo =
        DependencyCache.findDependency(unresolvedModuleID);
    if (optionalCachedModuleInfo) {
      resolvedClangDependenciesMap[unresolvedImport.first].insert(
          unresolvedModuleID);
      DependencyCache.setVisibleClangModulesFromLookup(
          unresolvedModuleID,
          {unresolvedImport.second.importIdentifier});
      ScanDiagnosticReporter.registerNamedClangDependency();
    } else {
      // Failed to resolve module dependency.
      ScanDiagnosticReporter.diagnoseModuleNotFoundFailure(
          unresolvedImport.second, DependencyCache, unresolvedImport.first,
          attemptToFindResolvingSerializedSearchPath(unresolvedImport.second));
    }
  }
}

void ModuleDependencyScanner::performClangModuleLookup(
    const ImportStatementInfoMap &unresolvedImportsMap,
    const ImportStatementInfoMap &unresolvedOptionalImportsMap,
    BatchClangModuleLookupResult &result) {
  auto seenClangModules = DependencyCache.getAlreadySeenClangModules();
  std::mutex resultAccessLock;
  auto scanForClangModuleDependency = [this, &result, &resultAccessLock,
                                       &seenClangModules](
                                          Identifier moduleIdentifier) {
    auto scanResult = withDependencyScanningWorker(
        [&](ModuleDependencyScanningWorker *ScanningWorker) {
          auto lookupModuleOutput = [this](const auto &cd, auto mok) -> auto {
            return clangModuleOutputPathLookup(cd, mok);
          };
          return ScanningWorker->scanFilesystemForClangModuleDependency(
              moduleIdentifier, lookupModuleOutput, seenClangModules);
        });
    {
      std::lock_guard<std::mutex> guard(resultAccessLock);
      if (scanResult) {
        llvm::for_each(scanResult->ModuleGraph, [&result](const auto &dep) {
          result.discoveredDependencyInfos.try_emplace(dep.ID.ModuleName, dep);
        });
        result.visibleModules.insert_or_assign(moduleIdentifier.str(),
                                               scanResult->VisibleModules);
      }
    }
  };

  // Enque asynchronous lookup tasks
  llvm::StringSet<> queriedIdentifiers;
  for (const auto &unresolvedImports : unresolvedImportsMap)
    for (const auto &unresolvedImportInfo : unresolvedImports.second)
      if (queriedIdentifiers.insert(unresolvedImportInfo.importIdentifier).second)
        ScanningThreadPool.async(
            scanForClangModuleDependency,
            getModuleImportIdentifier(unresolvedImportInfo.importIdentifier));

  for (const auto &unresolvedImports : unresolvedOptionalImportsMap)
    for (const auto &unresolvedImportInfo : unresolvedImports.second)
      if (queriedIdentifiers.insert(unresolvedImportInfo.importIdentifier).second)
        ScanningThreadPool.async(
            scanForClangModuleDependency,
            getModuleImportIdentifier(unresolvedImportInfo.importIdentifier));

  ScanningThreadPool.wait();
}

void ModuleDependencyScanner::processBatchClangModuleQueryResult(
    const BatchClangModuleLookupResult &lookupResult,
    const ImportStatementInfoMap &unresolvedImportsMap,
    const ImportStatementInfoMap &unresolvedOptionalImportsMap,
    ModuleDependencyIDSetVector &allDiscoveredClangModules,
    std::vector<ModuleIDImportInfoPair> &failedToResolveImports,
    ModuleIDToModuleIDSetVectorMap &resolvedClangDependenciesMap) {

  llvm::StringSet<> cachedNamedDependencies;
  auto recordResolvedClangModuleImport =
      [this, &lookupResult, &resolvedClangDependenciesMap,
       &allDiscoveredClangModules, &failedToResolveImports,
       &cachedNamedDependencies](
          const ModuleDependencyID &moduleID,
          const ScannerImportStatementInfo &moduleImport, bool optionalImport) {
        auto &moduleIdentifier = moduleImport.importIdentifier;
        auto dependencyID =
            ModuleDependencyID{moduleIdentifier, ModuleDependencyKind::Clang};

        // A successful named query will have returned a set of visible modules
        if (lookupResult.visibleModules.contains(moduleIdentifier)) {
          // If this dependency has already been recorded in the cache,
          // mark it as resolved for the current dependent and exit.
          if (!cachedNamedDependencies.insert(moduleIdentifier).second) {
            resolvedClangDependenciesMap[moduleID].insert(dependencyID);
            return;
          }

          ScanDiagnosticReporter.registerNamedClangDependency();
          DependencyCache.setVisibleClangModulesFromLookup(
              dependencyID, lookupResult.visibleModules.at(moduleIdentifier));
          resolvedClangDependenciesMap[moduleID].insert(dependencyID);

          // If this module was not seen before as a transitive dependency
          // of another lookup, record it into the cache
          if (lookupResult.discoveredDependencyInfos.contains(
                  moduleIdentifier)) {
            allDiscoveredClangModules.insert(dependencyID);
            DependencyCache.recordClangDependency(
                lookupResult.discoveredDependencyInfos.at(
                    moduleImport.importIdentifier),
                ScanASTContext.Diags, [this](auto &clangDep) {
                  return bridgeClangModuleDependency(clangDep);
                });
          } else {
            // If the query produced a set of visible modules but not
            // a `ModuleDeps` info for the queried module itself, then
            // it has to have been included in the set of already-seen
            // module dependencies from a prior query.
            assert(DependencyCache.hasClangDependency(moduleIdentifier));
          }
        } else if (!optionalImport) {
          // Otherwise, we failed to resolve this dependency. We will try
          // again using the cache after all other imports have been
          // resolved. If that fails too, a scanning failure will be
          // diagnosed.
          failedToResolveImports.push_back(
              std::make_pair(moduleID, moduleImport));
        }
      };

  for (const auto &moduleUnresolvedImports : unresolvedImportsMap)
    for (const auto &unresolvedImport : moduleUnresolvedImports.second)
      recordResolvedClangModuleImport(moduleUnresolvedImports.first,
                                      unresolvedImport, false);

  for (const auto &moduleUnresolvedImports : unresolvedOptionalImportsMap)
    for (const auto &unresolvedImport : moduleUnresolvedImports.second)
      recordResolvedClangModuleImport(moduleUnresolvedImports.first,
                                      unresolvedImport, true);

  // Use the computed scan results to record all transitive clang module
  // dependencies
  for (const auto &dep : lookupResult.discoveredDependencyInfos) {
    auto depID =
        ModuleDependencyID{dep.getKey().str(), ModuleDependencyKind::Clang};
    if (!allDiscoveredClangModules.contains(depID)) {
      DependencyCache.recordClangDependency(
          dep.second, ScanASTContext.Diags, [this](auto &clangDep) {
            return bridgeClangModuleDependency(clangDep);
          });
      allDiscoveredClangModules.insert(depID);
    }
  }
}

void ModuleDependencyScanner::resolveClangModuleDependencies(
    ArrayRef<ModuleDependencyID> swiftModuleDependents,
    ModuleDependencyIDSetVector &allDiscoveredClangModules) {
  // Gather all remaining unresolved imports which must correspond to
  // Clang modules (since no Swift module for them was found).
  ImportStatementInfoMap unresolvedImportsMap;
  ImportStatementInfoMap unresolvedOptionalImportsMap;
  ModuleIDToModuleIDSetVectorMap resolvedClangDependenciesMap;
  gatherUnresolvedImports(DependencyCache, ScanASTContext,
                          swiftModuleDependents, allDiscoveredClangModules,
                          unresolvedImportsMap, unresolvedOptionalImportsMap,
                          resolvedClangDependenciesMap);

  // Execute parallel lookup of all unresolved import
  // identifiers as Clang modules.
  BatchClangModuleLookupResult lookupResult;
  performClangModuleLookup(unresolvedImportsMap, unresolvedOptionalImportsMap,
                           lookupResult);

  // Use the computed scan results to record directly-queried clang module
  // dependencies.
  std::vector<ModuleIDImportInfoPair> failedToResolveImports;
  processBatchClangModuleQueryResult(
      lookupResult, unresolvedImportsMap, unresolvedOptionalImportsMap,
      allDiscoveredClangModules, failedToResolveImports,
      resolvedClangDependenciesMap);

  // Re-query some failed-to-resolve Clang imports from cache
  // in chance they were brought in as transitive dependencies.
  reQueryMissedModulesFromCache(failedToResolveImports,
                                resolvedClangDependenciesMap);

  // Record the computed result for each Swift module in the graph.
  for (const auto &moduleID : swiftModuleDependents) {
    auto resolvedDependencies =
        resolvedClangDependenciesMap[moduleID].getArrayRef();
    if (!resolvedDependencies.empty())
      DependencyCache.setImportedClangDependencies(moduleID,
                                                   resolvedDependencies);
  }
}

void ModuleDependencyScanner::resolveHeaderDependencies(
    ArrayRef<ModuleDependencyID> allSwiftModules,
    ModuleDependencyIDSetVector &allDiscoveredHeaderDependencyClangModules) {
  for (const auto &moduleID : allSwiftModules) {
    auto moduleDependencyInfo = DependencyCache.findKnownDependency(moduleID);
    if (!moduleDependencyInfo.getHeaderClangDependencies().empty()) {
      allDiscoveredHeaderDependencyClangModules.insert(
          moduleDependencyInfo.getHeaderClangDependencies().begin(),
          moduleDependencyInfo.getHeaderClangDependencies().end());
    } else {
      ModuleDependencyIDSetVector headerClangModuleDependencies;
      resolveHeaderDependenciesForModule(moduleID,
                                         headerClangModuleDependencies);
      allDiscoveredHeaderDependencyClangModules.insert(
          headerClangModuleDependencies.begin(),
          headerClangModuleDependencies.end());
    }
  }
}

void ModuleDependencyScanner::resolveSwiftOverlayDependencies(
    ArrayRef<ModuleDependencyID> allSwiftModules,
    ModuleDependencyIDSetVector &allDiscoveredDependencies) {
  std::string batchOverlayQueryModuleName =
      "_" + DependencyCache.getMainModuleName().str() + "-OverlayDependencies";

  ModuleDependencyIDSetVector discoveredSwiftOverlays;
  for (const auto &moduleID : allSwiftModules) {
    // Do not re-consider the supplied dummy module's Swift overlays,
    // if it is included in this list then we have already done so.
    if (moduleID.ModuleName == batchOverlayQueryModuleName)
      continue;

    auto moduleDependencyInfo = DependencyCache.findKnownDependency(moduleID);
    if (!moduleDependencyInfo.getSwiftOverlayDependencies().empty()) {
      allDiscoveredDependencies.insert(
          moduleDependencyInfo.getSwiftOverlayDependencies().begin(),
          moduleDependencyInfo.getSwiftOverlayDependencies().end());
    } else {
      ModuleDependencyIDSetVector swiftOverlayDependencies;
      resolveSwiftOverlayDependenciesForModule(moduleID,
                                               swiftOverlayDependencies);
      discoveredSwiftOverlays.insert(swiftOverlayDependencies.begin(),
                                     swiftOverlayDependencies.end());
    }
  }

  if (discoveredSwiftOverlays.empty())
    return;

  auto batchOverlayQueryModuleID = ModuleDependencyID{
      batchOverlayQueryModuleName, ModuleDependencyKind::SwiftSource};
  auto batchOverlayQueryModuleInfo =
      ModuleDependencyInfo::forSwiftSourceModule();
  // For each additional Swift overlay dependency, ensure we perform a full scan
  // in case it itself has unresolved module dependencies.
  llvm::for_each(discoveredSwiftOverlays, [&](ModuleDependencyID modID) {
    batchOverlayQueryModuleInfo.addModuleImport(modID.ModuleName, false,
                                                AccessLevel::Public);
  });
  // Record the dummy main module's direct dependencies. The dummy query module
  // only directly depend on these newly discovered overlay modules.
  if (DependencyCache.findDependency(batchOverlayQueryModuleID))
    DependencyCache.updateDependency(batchOverlayQueryModuleID,
                                     batchOverlayQueryModuleInfo);
  else
    DependencyCache.recordDependency(batchOverlayQueryModuleName,
                                     batchOverlayQueryModuleInfo);

  ModuleDependencyIDSetVector allNewModules =
      resolveImportedModuleDependencies(batchOverlayQueryModuleID);
  // Remove the dummy module
  allNewModules.remove(batchOverlayQueryModuleID);

  allDiscoveredDependencies.insert(allNewModules.begin(), allNewModules.end());
}

void ModuleDependencyScanner::resolveSwiftImportsForModule(
    const ModuleDependencyID &moduleID,
    ModuleDependencyIDSetVector &importedSwiftDependencies) {
  PrettyStackTraceStringAction trace("Resolving Swift imports of: ",
                                     moduleID.ModuleName);
  if (!isSwiftDependencyKind(moduleID.Kind))
    return;

  auto moduleDependencyInfo = DependencyCache.findKnownDependency(moduleID);
  llvm::StringMap<SwiftModuleScannerQueryResult> moduleLookupResult;
  std::mutex lookupResultLock;

  // A scanning task to query a module by-name. If the module already exists
  // in the cache, do nothing and return.
  auto scanForSwiftModuleDependency =
      [this, &lookupResultLock,
       &moduleLookupResult](Identifier moduleIdentifier, bool isTestable) {
        auto moduleDependencies = withDependencyScanningWorker(
            [moduleIdentifier,
             isTestable](ModuleDependencyScanningWorker *ScanningWorker) {
              return ScanningWorker->scanFilesystemForSwiftModuleDependency(
                  moduleIdentifier, isTestable);
            });
        {
          std::lock_guard<std::mutex> guard(lookupResultLock);
          moduleLookupResult.insert_or_assign(moduleIdentifier.str().str(),
                                              moduleDependencies);
        }
      };

  // Enque asynchronous lookup tasks
  for (const auto &dependsOn : moduleDependencyInfo.getModuleImports()) {
    // Avoid querying the underlying Clang module here
    if (moduleID.ModuleName == dependsOn.importIdentifier)
      continue;
    // Avoid querying Swift module dependencies previously looked up
    if (DependencyCache.hasQueriedSwiftDependency(dependsOn.importIdentifier))
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
      [this, &moduleLookupResult, &importedSwiftDependencies,
       moduleID](const ScannerImportStatementInfo &moduleImport) {
        if (moduleID.ModuleName == moduleImport.importIdentifier)
          return;
        auto lookupResult = moduleLookupResult[moduleImport.importIdentifier];

        // Query found module
        if (lookupResult.foundDependencyInfo) {
          DependencyCache.recordDependency(moduleImport.importIdentifier,
                                           *(lookupResult.foundDependencyInfo));
          importedSwiftDependencies.insert(
              {moduleImport.importIdentifier,
               lookupResult.foundDependencyInfo->getKind()});
          ScanDiagnosticReporter.warnOnIncompatibleCandidates(
              moduleImport.importIdentifier,
              lookupResult.incompatibleCandidates);
          // Module was resolved from a cache
        } else if (auto cachedInfo = DependencyCache.findSwiftDependency(
                       moduleImport.importIdentifier))
          importedSwiftDependencies.insert(
              {moduleImport.importIdentifier, cachedInfo.value()->getKind()});
        else {
          ScanDiagnosticReporter.diagnoseFailureOnOnlyIncompatibleCandidates(
                     moduleImport, lookupResult.incompatibleCandidates,
                     DependencyCache, std::nullopt);
          DependencyCache
            .recordFailedSwiftDependencyLookup(moduleImport.importIdentifier);
        }
      };

  for (const auto &importInfo : moduleDependencyInfo.getModuleImports())
    recordResolvedModuleImport(importInfo);
  for (const auto &importInfo : moduleDependencyInfo.getOptionalModuleImports())
    recordResolvedModuleImport(importInfo);

  // Resolve the dependency info with Swift dependency module information.
  DependencyCache.setImportedSwiftDependencies(
      moduleID, importedSwiftDependencies.getArrayRef());
}

void ModuleDependencyScanner::resolveHeaderDependenciesForModule(
    const ModuleDependencyID &moduleID,
    ModuleDependencyIDSetVector &headerClangModuleDependencies) {
  PrettyStackTraceStringAction trace(
      "Resolving header dependencies of Swift module", moduleID.ModuleName);
  std::vector<std::string> allClangModules;
  llvm::StringSet<> alreadyKnownModules;
  auto moduleDependencyInfo = DependencyCache.findKnownDependency(moduleID);

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

    return extractEmbeddedBridgingHeaderContent(std::move(*moduleBuf), header,
                                                ScanASTContext);
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
        std::vector<std::string> visibleClangModules;
        auto lookupModuleOutput = [this](const auto &cd, auto mok) -> auto {
          return clangModuleOutputPathLookup(cd, mok);
        };
        auto headerScanResult =
            ScanningWorker->scanHeaderDependenciesOfSwiftModule(
                moduleID, headerPath, sourceBufferRef, lookupModuleOutput,
                DependencyCache.getAlreadySeenClangModules());

        if (headerScanResult) {
          // Record module dependencies for each new module we found.
          DependencyCache.recordClangDependencies(
              headerScanResult->ModuleGraph, ScanASTContext.Diags,
              [this](auto &clangDep) {
                return bridgeClangModuleDependency(clangDep);
              });
          llvm::copy(headerScanResult->FileDeps,
                     std::back_inserter(headerFileInputs));
          auto bridgedDependencyIDs = llvm::map_range(
              headerScanResult->ClangModuleDeps, [](auto &input) {
                return ModuleDependencyID{input.ModuleName,
                                          ModuleDependencyKind::Clang};
              });
          headerClangModuleDependencies.insert(bridgedDependencyIDs.begin(),
                                               bridgedDependencyIDs.end());

          auto targetModuleInfo = DependencyCache.findKnownDependency(moduleID);
          if (targetModuleInfo.isTextualSwiftModule()) {
            if (auto TreeID = headerScanResult->IncludeTreeID)
              includeTreeID = TreeID;
            ClangImporter::getBridgingHeaderOptions(
                ScanASTContext, *headerScanResult, bridgingHeaderCommandLine);
          }

          // Record direct header Clang dependencies
          moduleDependencyInfo.setHeaderClangDependencies(
              headerClangModuleDependencies.getArrayRef());
          // Record include Tree ID
          if (includeTreeID)
            moduleDependencyInfo.addBridgingHeaderIncludeTree(*includeTreeID);
          // Record the bridging header command line
          if (isTextualModuleWithABridgingHeader)
            moduleDependencyInfo.updateBridgingHeaderCommandLine(
                bridgingHeaderCommandLine);
          moduleDependencyInfo.setHeaderSourceFiles(headerFileInputs);
          // Update the set of visible Clang modules
          moduleDependencyInfo.setHeaderVisibleClangModules(
              headerScanResult->VisibleModules);
          // Update the dependency in the cache
          DependencyCache.updateDependency(moduleID, moduleDependencyInfo);
        } else {
          // Failure to scan header
        }
        return true;
      });
}

void ModuleDependencyScanner::resolveSwiftOverlayDependenciesForModule(
    const ModuleDependencyID &moduleID,
    ModuleDependencyIDSetVector &swiftOverlayDependencies) {
  PrettyStackTraceStringAction trace(
      "Resolving Swift Overlay dependencies of module", moduleID.ModuleName);
  auto visibleClangDependencies =
      DependencyCache.getAllVisibleClangModules(moduleID);

  llvm::StringMap<SwiftModuleScannerQueryResult> swiftOverlayLookupResult;
  std::mutex lookupResultLock;

  // A scanning task to query a Swift module by-name. If the module already
  // exists in the cache, do nothing and return.
  auto scanForSwiftDependency =
      [this, &lookupResultLock,
       &swiftOverlayLookupResult](Identifier moduleIdentifier) {
        auto moduleDependencies = withDependencyScanningWorker(
            [moduleIdentifier](ModuleDependencyScanningWorker *ScanningWorker) {
              return ScanningWorker->scanFilesystemForSwiftModuleDependency(
                  moduleIdentifier, /* isTestableImport */ false);
            });
        {
          std::lock_guard<std::mutex> guard(lookupResultLock);
          swiftOverlayLookupResult.insert_or_assign(moduleIdentifier.str(),
                                                    moduleDependencies);
        }
      };

  // Enque asynchronous lookup tasks
  for (const auto &clangDep : visibleClangDependencies) {
    auto clangDepName = clangDep.getKey().str();
    // Avoid Swift overlay lookup for the underlying clang module of a known
    // Swift module. i.e. When computing set of Swift Overlay dependencies
    // for module 'A', which depends on a Clang module 'A', ensure we don't
    // lookup Swift module 'A' itself here.
    if (clangDepName == moduleID.ModuleName)
      continue;
    // Avoid querying Swift module dependencies previously looked up
    if (DependencyCache.hasQueriedSwiftDependency(clangDepName))
      continue;
    ScanningThreadPool.async(scanForSwiftDependency,
                             getModuleImportIdentifier(clangDepName));
  }
  ScanningThreadPool.wait();

  // Aggregate both previously-cached and freshly-scanned module results
  auto recordResult = [this, &swiftOverlayLookupResult,
                       &swiftOverlayDependencies,
                       moduleID](const std::string &moduleName) {
    auto lookupResult = swiftOverlayLookupResult[moduleName];
    if (moduleName != moduleID.ModuleName) {

      // Query found module
      if (lookupResult.foundDependencyInfo) {
        DependencyCache.recordDependency(moduleName,
                                         *(lookupResult.foundDependencyInfo));
        swiftOverlayDependencies.insert(
            {moduleName, lookupResult.foundDependencyInfo->getKind()});
        ScanDiagnosticReporter.warnOnIncompatibleCandidates(
            moduleName, lookupResult.incompatibleCandidates);
        // Module was resolved from a cache
      } else if (auto cachedInfo =
                     DependencyCache.findSwiftDependency(moduleName))
        swiftOverlayDependencies.insert(
            {moduleName, cachedInfo.value()->getKind()});
      else
        ScanDiagnosticReporter.diagnoseFailureOnOnlyIncompatibleCandidates(
            ScannerImportStatementInfo(moduleName),
            lookupResult.incompatibleCandidates, DependencyCache,
            std::nullopt);
    }
  };
  for (const auto &clangDep : visibleClangDependencies)
    recordResult(clangDep.getKey().str());

  // C++ Interop requires additional handling
  bool lookupCxxStdLibOverlay =
      ScanCompilerInvocation.getLangOptions().EnableCXXInterop;
  if (lookupCxxStdLibOverlay &&
      moduleID.Kind == ModuleDependencyKind::SwiftInterface) {
    const auto &moduleInfo = DependencyCache.findKnownDependency(moduleID);
    const auto commandLine = moduleInfo.getCommandline();
    // If the textual interface was built without C++ interop, do not query
    // the C++ Standard Library Swift overlay for its compilation.
    if (llvm::find(commandLine, "-formal-cxx-interoperability-mode=off") !=
        commandLine.end())
      lookupCxxStdLibOverlay = false;
  } else if (lookupCxxStdLibOverlay &&
             moduleID.Kind == ModuleDependencyKind::SwiftBinary) {
    const auto &moduleDetails =
      DependencyCache.findKnownDependency(moduleID).getAsSwiftBinaryModule();
    // If the binary module was built without C++ interop, do not query
    // the C++ Standard Library Swift overlay.
    if (!moduleDetails->isBuiltWithCxxInterop)
      lookupCxxStdLibOverlay = false;
  }

  // FIXME: We always declare the 'Darwin' module as formally having been built
  // without C++Interop, for compatibility with prior versions. Once we are certain
  // that we are only building against modules built with support of
  // '-formal-cxx-interoperability-mode', this hard-coded check should be removed.
  if (lookupCxxStdLibOverlay && moduleID.ModuleName == "Darwin")
    lookupCxxStdLibOverlay = false;

  if (lookupCxxStdLibOverlay) {
    for (const auto &clangDepNameEntry : visibleClangDependencies) {
      auto clangDepName = clangDepNameEntry.getKey().str();

      // If this Clang module is a part of the C++ stdlib, and we haven't
      // loaded the overlay for it so far, it is a split libc++ module (e.g.
      // std_vector). Load the CxxStdlib overlay explicitly.
      const auto &clangDepInfo =
        DependencyCache.findDependency(clangDepName,
                                       ModuleDependencyKind::Clang)
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

  // Resolve the dependency info with Swift overlay dependency module
  // information.
  DependencyCache.setSwiftOverlayDependencies(
      moduleID, swiftOverlayDependencies.getArrayRef());
}

void ModuleDependencyScanner::resolveCrossImportOverlayDependencies(
    llvm::function_ref<void(ModuleDependencyID)> action) {
  // Modules explicitly imported. Only these can be secondary module.
  llvm::SetVector<Identifier> newOverlays;
  std::set<std::pair<std::string, std::string>> overlayFiles;
  discoverCrossImportOverlayFiles(DependencyCache, ScanASTContext, newOverlays,
                                  overlayFiles);

  // No new cross-import overlays are found, return.
  if (newOverlays.empty())
    return;

  // Construct a dummy main to resolve the newly discovered cross import
  // overlays.
  std::string batchCrossImportQueryModuleName =
      "_" + DependencyCache.getMainModuleName().str() + "-CrossImportOverlays";
  auto queryModuleID = ModuleDependencyID{batchCrossImportQueryModuleName,
                                        ModuleDependencyKind::SwiftSource};
  auto mainModuleID = ModuleDependencyID{DependencyCache.getMainModuleName().str(),
                                         ModuleDependencyKind::SwiftSource};
  auto queryModuleInfo = ModuleDependencyInfo::forSwiftSourceModule();
  llvm::for_each(
      newOverlays, [&](Identifier modName) {
        queryModuleInfo.addModuleImport(
            modName.str(),
            /* isExported */ false,
            // TODO: What is the right access level for a cross-import overlay?
            AccessLevel::Public);
      });

  // Record the dummy query module's direct dependencies. The dummy query module
  // only directly depend on these newly discovered overlay modules.
  if (DependencyCache.findDependency(queryModuleID))
    DependencyCache.updateDependency(queryModuleID, queryModuleInfo);
  else
    DependencyCache.recordDependency(batchCrossImportQueryModuleName,
                                     queryModuleInfo);

  ModuleDependencyIDSetVector allModules =
      resolveImportedModuleDependencies(queryModuleID);

  // Update main module's dependencies to include these new overlays.
  DependencyCache.setCrossImportOverlayDependencies(
      mainModuleID, DependencyCache.getAllDependencies(queryModuleID));

  // Update the command-line on the main module to disable implicit
  // cross-import overlay search.
  auto mainModuleInfo = DependencyCache.findKnownDependency(mainModuleID);
  std::vector<std::string> cmdCopy = mainModuleInfo.getCommandline();
  cmdCopy.push_back("-disable-cross-import-overlay-search");
  for (auto &entry : overlayFiles) {
    mainModuleInfo.addAuxiliaryFile(entry.second);
    cmdCopy.push_back("-swift-module-cross-import");
    cmdCopy.push_back(entry.first);
    auto overlayPath = remapPath(entry.second);
    cmdCopy.push_back(overlayPath);
  }
  mainModuleInfo.updateCommandLine(cmdCopy);
  DependencyCache.updateDependency(mainModuleID, mainModuleInfo);

  // Report any discovered modules to the clients, which include all overlays
  // and their dependencies.
  std::for_each(/* +1 to exclude dummy main*/ allModules.begin() + 1,
                allModules.end(), action);
}

llvm::Error ModuleDependencyScanner::performBridgingHeaderChaining(
    const ModuleDependencyID &rootModuleID,
    ModuleDependencyIDSetVector &allModules) {
  if (rootModuleID.Kind != ModuleDependencyKind::SwiftSource)
    return llvm::Error::success();

  llvm::SmallString<256> chainedHeaderBuffer;
  llvm::raw_svector_ostream outOS(chainedHeaderBuffer);

  // If prefix mapping is used, don't try to import header since that will add
  // a path-relative component into dependency scanning and defeat the purpose
  // of prefix mapping. Additionally, if everything is prefix mapped, the
  // embedded header path is also prefix mapped, thus it can't be found anyway.
  bool useImportHeader = !hasPathMapping();
  auto FS = ScanASTContext.SourceMgr.getFileSystem();

  auto chainBridgingHeader = [&](StringRef moduleName, StringRef headerPath,
                                 StringRef binaryModulePath,
                                 bool useHeader) -> llvm::Error {
    if (useHeader) {
      if (auto buffer = FS->getBufferForFile(headerPath)) {
        outOS << "#include \"" << headerPath << "\"\n";
        return llvm::Error::success();
      }
    }

    if (binaryModulePath.empty())
      return llvm::createStringError("failed to load bridging header " +
                                     headerPath);

    // Extract the embedded bridging header
    auto moduleBuf = FS->getBufferForFile(binaryModulePath);
    if (!moduleBuf)
      return llvm::errorCodeToError(moduleBuf.getError());

    auto content = extractEmbeddedBridgingHeaderContent(
        std::move(*moduleBuf), /*headerPath=*/"", ScanASTContext);
    if (!content)
      return llvm::createStringError("can't load embedded header from " +
                                     binaryModulePath);

    outOS << "# 1 \"<module-" << moduleName
          << "-embedded-bridging-header>\" 1\n";
    outOS << content->getBuffer() << "\n";
    return llvm::Error::success();
  };

  // Iterate through all the modules and collect all the bridging header
  // and chain them into a single file. The allModules list is in the order of
  // discover, thus providing stable ordering for a deterministic generated
  // buffer.
  for (const auto &moduleID : allModules) {
    if (moduleID.Kind != ModuleDependencyKind::SwiftBinary)
      continue;

    auto moduleDependencyInfo = DependencyCache.findKnownDependency(moduleID);
    if (auto *binaryMod = moduleDependencyInfo.getAsSwiftBinaryModule()) {
      if (binaryMod->headerImport.empty())
        continue;

      if (auto E = chainBridgingHeader(
              moduleID.ModuleName, binaryMod->headerImport,
              binaryMod->compiledModulePath, useImportHeader))
        return E;
    }
  }

  // Handle bridging header in main module.
  auto mainModuleDeps = DependencyCache.findKnownDependency(rootModuleID);
  auto *mainModule = mainModuleDeps.getAsSwiftSourceModule();
  assert(mainModule && "expect main module to be a swift source module");
  std::unique_ptr<llvm::MemoryBuffer> sourceBuffer;
  bool needChainedHeader = !chainedHeaderBuffer.empty();
  if (!needChainedHeader) {
    // There is no bridging header chained from dependencies.
    // If main module also has no bridging header, ther is nothing to scan.
    if (!mainModule->textualModuleDetails.bridgingHeaderFile)
      return llvm::Error::success();

    // Otherwise, there is no chaining needed. Just use the bridging header from
    // main module.
    if (auto headerBuffer = FS->getBufferForFile(
            *mainModule->textualModuleDetails.bridgingHeaderFile))
      sourceBuffer = std::move(*headerBuffer);
    else
      return llvm::errorCodeToError(headerBuffer.getError());
  } else {
    // There are bridging header needed to be chained. Append the bridging
    // header from main module if needed and create use a new source buffer.
    if (mainModule->textualModuleDetails.bridgingHeaderFile) {
      if (auto E = chainBridgingHeader(
              rootModuleID.ModuleName,
              *mainModule->textualModuleDetails.bridgingHeaderFile, "",
              /*useHeader=*/true))
        return E;
    }

    SmallString<256> outputPath(
        ScanCompilerInvocation.getFrontendOptions().ScannerOutputDir);

    if (outputPath.empty())
      outputPath = "/<compiler-generated>";

    // Use the hash of the file content to differentiate the chained header.
    auto fileHash =
        llvm::toString(llvm::APInt(64, llvm::hash_value(chainedHeaderBuffer)),
                       36, /*Signed=*/false);
    llvm::sys::path::append(
        outputPath, ScanCompilerInvocation.getFrontendOptions().ModuleName +
                        "-" + fileHash + "-ChainedBridgingHeader.h");

    if (ScanCompilerInvocation.getFrontendOptions().WriteScannerOutput) {
      llvm::vfs::OnDiskOutputBackend outputBackend;
      auto outFile = outputBackend.createFile(outputPath);
      if (!outFile)
        return outFile.takeError();
      *outFile << chainedHeaderBuffer;
      if (auto err = outFile->keep())
        return err;
    }

    sourceBuffer =
        llvm::MemoryBuffer::getMemBufferCopy(chainedHeaderBuffer, outputPath);
  }

  // Scan and update the main module dependency.
  ModuleDependencyIDSetVector headerClangModuleDependencies;
  std::optional<std::string> includeTreeID;
  auto err = withDependencyScanningWorker(
      [&](ModuleDependencyScanningWorker *ScanningWorker) -> llvm::Error {
        std::vector<std::string> headerFileInputs;
        std::vector<std::string> bridgingHeaderCommandLine;
        std::vector<std::string> visibleClangModules;
        auto lookupModuleOutput = [this](const auto &cd, auto mok) -> auto {
          return clangModuleOutputPathLookup(cd, mok);
        };
        auto headerScanResult =
            ScanningWorker->scanHeaderDependenciesOfSwiftModule(
                rootModuleID, /*headerPath=*/std::nullopt,
                sourceBuffer->getMemBufferRef(), lookupModuleOutput,
                DependencyCache.getAlreadySeenClangModules());

        if (!headerScanResult)
          return llvm::createStringError(
              "failed to scan generated bridging header " +
              sourceBuffer->getBufferIdentifier());

        // Record module dependencies for each new module we found.
        DependencyCache.recordClangDependencies(
            headerScanResult->ModuleGraph, ScanASTContext.Diags,
            [this](auto &clangDep) {
              return bridgeClangModuleDependency(clangDep);
            });
        llvm::copy(headerScanResult->FileDeps,
                   std::back_inserter(headerFileInputs));
        auto bridgedDependencyIDs =
            llvm::map_range(headerScanResult->ClangModuleDeps, [](auto &input) {
              return ModuleDependencyID{input.ModuleName,
                                        ModuleDependencyKind::Clang};
            });
        headerClangModuleDependencies.insert(bridgedDependencyIDs.begin(),
                                             bridgedDependencyIDs.end());

        // Update visible module set
        if (auto TreeID = headerScanResult->IncludeTreeID)
          includeTreeID = TreeID;
        ClangImporter::getBridgingHeaderOptions(
            ScanASTContext, *headerScanResult, bridgingHeaderCommandLine);

        DependencyCache.setHeaderClangDependencies(
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
        if (needChainedHeader) {
          // As only the chained bridging header is scanned, the dependency will
          // not include the original bridging header passed by user. Fixup the
          // headerFileInputs to include original bridging header and not
          // include the generated header so build system can correctly computes
          // the dependencies.
          auto generated =
              llvm::find(headerFileInputs, sourceBuffer->getBufferIdentifier());
          if (generated != headerFileInputs.end()) {
            if (mainModule->textualModuleDetails.bridgingHeaderFile)
              *generated = *mainModule->textualModuleDetails.bridgingHeaderFile;
            else
              headerFileInputs.erase(generated);
          }
        }
        mainModuleDeps.setHeaderSourceFiles(headerFileInputs);
        if (needChainedHeader)
          mainModuleDeps.setChainedBridgingHeaderBuffer(
              sourceBuffer->getBufferIdentifier(), sourceBuffer->getBuffer());
        // Update the set of visible Clang modules
        mainModuleDeps.setHeaderVisibleClangModules(
            headerScanResult->VisibleModules);
        // Update the dependency in the cache
        DependencyCache.updateDependency(rootModuleID, mainModuleDeps);
        return llvm::Error::success();
      });

  if (err)
    return err;

  DependencyCache.setHeaderClangDependencies(
      rootModuleID, headerClangModuleDependencies.getArrayRef());

  llvm::for_each(
      headerClangModuleDependencies,
      [&allModules](const ModuleDependencyID &dep) { allModules.insert(dep); });

  return llvm::Error::success();
}

std::string ModuleDependencyScanner::remapPath(StringRef Path) const {
  if (!PrefixMapper)
    return Path.str();
  return PrefixMapper->mapToString(Path);
}

ModuleDependencyInfo ModuleDependencyScanner::bridgeClangModuleDependency(
    const clang::tooling::dependencies::ModuleDeps &clangModuleDep) {
  // File dependencies for this module.
  std::vector<std::string> fileDeps;
  clangModuleDep.forEachFileDep(
      [&fileDeps](StringRef fileDep) { fileDeps.emplace_back(fileDep); });

  std::vector<std::string> swiftArgs;
  auto addClangArg = [&](Twine arg) {
    swiftArgs.push_back("-Xcc");
    swiftArgs.push_back(arg.str());
  };

  // We are using Swift frontend mode.
  swiftArgs.push_back("-frontend");

  // Swift frontend action: -emit-pcm
  swiftArgs.push_back("-emit-pcm");
  swiftArgs.push_back("-module-name");
  swiftArgs.push_back(clangModuleDep.ID.ModuleName);

  auto pcmPath = clangModuleOutputPathLookup(
      clangModuleDep,
      clang::tooling::dependencies::ModuleOutputKind::ModuleFile);
  swiftArgs.push_back("-o");
  swiftArgs.push_back(pcmPath);

  // Ensure that the resulting PCM build invocation uses Clang frontend
  // directly
  swiftArgs.push_back("-direct-clang-cc1-module-build");

  // Swift frontend option for input file path (Foo.modulemap).
  swiftArgs.push_back(remapPath(clangModuleDep.ClangModuleMapFile));

  auto invocation = clangModuleDep.getUnderlyingCompilerInvocation();
  // Clear some options from clang scanner.
  invocation.getMutFrontendOpts().ModuleCacheKeys.clear();
  invocation.getMutFrontendOpts().PathPrefixMappings.clear();
  invocation.getMutFrontendOpts().OutputFile.clear();

  // Reset CASOptions since that should be coming from swift.
  invocation.getMutCASOpts() = clang::CASOptions();
  invocation.getMutFrontendOpts().CASIncludeTreeID.clear();

  if (!ScanASTContext.CASOpts.EnableCaching) {
    auto &overlayFiles = invocation.getMutHeaderSearchOpts().VFSOverlayFiles;

    // clang system overlay file is a virtual file that is not an actual file.
    auto clangSystemOverlayFile =
        ClangImporter::getClangSystemOverlayFile(ScanASTContext.SearchPathOpts);
    llvm::erase(overlayFiles, clangSystemOverlayFile);

    // FIXME: workaround for rdar://105684525: find the -ivfsoverlay option
    // from clang scanner and pass to swift.
    for (auto overlay : overlayFiles) {
      swiftArgs.push_back("-vfsoverlay");
      swiftArgs.push_back(overlay);
    }
  }

  // Pass the -sdk flag to make the system header VFS overlay finable
  // for the -direct-clang-cc1-module-build emit-pcm command on Windows.
  StringRef SDKPath = ScanASTContext.SearchPathOpts.getSDKPath();
  if (!SDKPath.empty()) {
    swiftArgs.push_back("-sdk");
    swiftArgs.push_back(SDKPath.str());
  }

  // Add args reported by the scanner.
  auto clangArgs = invocation.getCC1CommandLine();
  llvm::for_each(clangArgs, addClangArg);

  std::string IncludeTree =
      clangModuleDep.IncludeTreeID ? *clangModuleDep.IncludeTreeID : "";

  ScanASTContext.CASOpts.enumerateCASConfigurationFlags(
      [&](StringRef Arg) { swiftArgs.push_back(Arg.str()); });

  if (!IncludeTree.empty()) {
    swiftArgs.push_back("-clang-include-tree-root");
    swiftArgs.push_back(IncludeTree);
  }
  std::string mappedPCMPath = remapPath(pcmPath);

  std::vector<LinkLibrary> LinkLibraries;
  for (const auto &ll : clangModuleDep.LinkLibraries)
    LinkLibraries.emplace_back(ll.Library,
                               ll.IsFramework ? LibraryKind::Framework
                                              : LibraryKind::Library,
                               /*static=*/false);

  // Module-level dependencies.
  llvm::StringSet<> alreadyAddedModules;
  auto bridgedDependencyInfo = ModuleDependencyInfo::forClangModule(
      pcmPath, mappedPCMPath, clangModuleDep.ClangModuleMapFile,
      clangModuleDep.ID.ContextHash, swiftArgs, fileDeps, LinkLibraries,
      IncludeTree, /*module-cache-key*/ "", clangModuleDep.IsSystem);

  std::vector<ModuleDependencyID> directDependencyIDs;
  for (const auto &moduleName : clangModuleDep.ClangModuleDeps) {
    // FIXME: This assumes, conservatively, that all Clang module imports
    // are exported. We need to fix this once the clang scanner gains the
    // appropriate API to query this.
    bridgedDependencyInfo.addModuleImport(
        moduleName.ModuleName, /* isExported */ true, AccessLevel::Public,
        &alreadyAddedModules);
    // It is safe to assume that all dependencies of a Clang module are Clang
    // modules.
    directDependencyIDs.push_back(
        {moduleName.ModuleName, ModuleDependencyKind::Clang});
  }
  bridgedDependencyInfo.setImportedClangDependencies(directDependencyIDs);
  return bridgedDependencyInfo;
}

void DependencyScannerDiagnosticReporter::diagnoseModuleNotFoundFailure(
    const ScannerImportStatementInfo &moduleImport,
    const ModuleDependenciesCache &cache,
    std::optional<ModuleDependencyID> dependencyOf,
    std::optional<std::pair<ModuleDependencyID, std::string>>
        resolvingSerializedSearchPath,
    std::optional<
        std::vector<SwiftModuleScannerQueryResult::IncompatibleCandidate>>
        foundIncompatibleCandidates) {
  // Do not report the same failure multiple times. This can
  // happen, for example, when we first report that a valid Swift module is
  // missing and then look it up again as a Swift overlay of its underlying
  // Clang module.
  if (ReportedMissing.find(moduleImport.importIdentifier) !=
      ReportedMissing.end())
    return;

  SourceLoc importLoc = SourceLoc();
  if (!moduleImport.importLocations.empty()) {
    auto locInfo = moduleImport.importLocations[0];
    importLoc = Diagnostics.SourceMgr.getLocFromExternalSource(
        locInfo.bufferIdentifier, locInfo.lineNumber, locInfo.columnNumber);
  }

  // Emit the top-level error diagnostic, which is one of 3 possibilities:
  // 1. Module dependency can not be found but could be resolved if using search
  // paths serialized into some binary Swift module dependency
  //
  // 2. Swift Module dependency can not be found but we did find binary Swift
  // module candidates for this module which are not compatible with current
  // compilation
  //
  // 3. All other generic "module not found" cases
  if (resolvingSerializedSearchPath) {
    Diagnostics.diagnose(
        importLoc,
        diag::dependency_scan_module_not_found_on_specified_search_paths,
        moduleImport.importIdentifier);
    Diagnostics.diagnose(importLoc, diag::inherited_search_path_resolves_module,
                         moduleImport.importIdentifier,
                         resolvingSerializedSearchPath->first.ModuleName,
                         resolvingSerializedSearchPath->second);
  } else if (foundIncompatibleCandidates) {
    Diagnostics.diagnose(
        importLoc, diag::dependency_scan_compatible_swift_module_not_found,
        moduleImport.importIdentifier);
    for (const auto &candidate : *foundIncompatibleCandidates)
      Diagnostics.diagnose(importLoc,
                           diag::dependency_scan_incompatible_module_found,
                           candidate.path, candidate.incompatibilityReason);
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

  // Emit notes for every other known location where the failed-to-resolve
  // module is imported.
  if (moduleImport.importLocations.size() > 1) {
    for (size_t i = 1; i < moduleImport.importLocations.size(); ++i) {
      auto locInfo = moduleImport.importLocations[i];
      auto importLoc = Diagnostics.SourceMgr.getLocFromExternalSource(
          locInfo.bufferIdentifier, locInfo.lineNumber, locInfo.columnNumber);
      Diagnostics.diagnose(importLoc, diag::unresolved_import_location);
    }
  }

  ReportedMissing.insert(moduleImport.importIdentifier);
}

void DependencyScannerDiagnosticReporter::diagnoseFailureOnOnlyIncompatibleCandidates(
    const ScannerImportStatementInfo &moduleImport,
    const std::vector<SwiftModuleScannerQueryResult::IncompatibleCandidate>
        &candidates,
    const ModuleDependenciesCache &cache,
    std::optional<ModuleDependencyID> dependencyOf) {
  // If no incompatible candidates were discovered,
  // the dependency scanning failure will be caught downstream.
  if (candidates.empty())
    return;

  // FIXME: There are known cases where clients are relying on
  // loading the underlying Clang module in the presence of a Swift
  // module which is lacking the required target-specific variant,
  // such as MacCatalyst. Eventually, we should pursue making this
  // an error as well.
  if (llvm::all_of(candidates, [](auto &incompatibleCandidate) {
        return incompatibleCandidate.incompatibilityReason ==
            SwiftModuleScannerQueryResult::BUILT_FOR_INCOMPATIBLE_TARGET;
      })) {
    warnOnIncompatibleCandidates(moduleImport.importIdentifier, candidates);
    return;
  }

  diagnoseModuleNotFoundFailure(moduleImport, cache, dependencyOf,
                                /* resolvingSerializedSearchPath */ std::nullopt,
                                candidates);
}

DependencyScannerDiagnosticReporter::DependencyScannerDiagnosticReporter(
    DiagnosticEngine &Diagnostics, bool EmitScanRemarks)
    : Diagnostics(Diagnostics), EmitScanRemarks(EmitScanRemarks) {
  if (EmitScanRemarks)
    ScanMetrics = std::make_unique<ScannerMetrics>();
}

void DependencyScannerDiagnosticReporter::warnOnIncompatibleCandidates(
    StringRef moduleName,
    const std::vector<SwiftModuleScannerQueryResult::IncompatibleCandidate>
        &candidates) {
  // If the dependency was ultimately resolved to a different
  // binary module or a textual module, emit warnings about
  // having encountered incompatible binary modules.
  for (const auto &candidate : candidates)
    Diagnostics.diagnose(SourceLoc(), diag::dependency_scan_module_incompatible,
                         candidate.path, candidate.incompatibilityReason);
}

void DependencyScannerDiagnosticReporter::registerSwiftModuleQuery() {
  if (!EmitScanRemarks)
    return;
  assert(ScanMetrics);
  ScanMetrics->SwiftModuleQueries++;
}

void DependencyScannerDiagnosticReporter::registerNamedClangModuleQuery() {
  if (!EmitScanRemarks)
    return;
  assert(ScanMetrics);
  ScanMetrics->NamedClangModuleQueries++;
}

void DependencyScannerDiagnosticReporter::registerNamedClangDependency() {
  if (!EmitScanRemarks)
    return;
  assert(ScanMetrics);
  ScanMetrics->RecordedNamedClangModuleDependencies++;
}

void DependencyScannerDiagnosticReporter::emitScanMetrics(
    const ModuleDependenciesCache &cache) const {
  if (!EmitScanRemarks || !ScanMetrics)
    return;

  Diagnostics.diagnose(SourceLoc(), diag::dependency_scan_number_swift_queries,
                       ScanMetrics->SwiftModuleQueries);
  Diagnostics.diagnose(SourceLoc(),
                       diag::dependency_scan_number_named_clang_queries,
                       ScanMetrics->NamedClangModuleQueries);
  Diagnostics.diagnose(SourceLoc(),
                       diag::dependency_scan_number_named_clang_dependencies,
                       ScanMetrics->RecordedNamedClangModuleDependencies);
  Diagnostics.diagnose(SourceLoc(),
                       diag::dependency_scan_number_swift_dependencies,
                       cache.numberOfSwiftDependencies());
  Diagnostics.diagnose(SourceLoc(),
                       diag::dependency_scan_number_clang_dependencies,
                       cache.numberOfClangDependencies());
}

std::optional<std::pair<ModuleDependencyID, std::string>>
ModuleDependencyScanner::attemptToFindResolvingSerializedSearchPath(
    const ScannerImportStatementInfo &moduleImport) {
  std::set<ModuleDependencyID> binarySwiftModuleDepIDs =
      collectBinarySwiftDeps(DependencyCache);

  std::optional<std::pair<ModuleDependencyID, std::string>> result;
  for (const auto &binaryDepID : binarySwiftModuleDepIDs) {
    auto binaryModInfo = DependencyCache.findKnownDependency(binaryDepID)
                             .getAsSwiftBinaryModule();
    assert(binaryModInfo);
    if (binaryModInfo->serializedSearchPaths.empty())
      continue;

    // Note: this will permanently mutate this worker with additional search
    // paths. That's fine because we are diagnosing a scan failure here, but
    // worth being aware of.
    result = withDependencyScanningWorker(
        [&](ModuleDependencyScanningWorker *ScanningWorker)
            -> std::optional<std::pair<ModuleDependencyID, std::string>> {
          for (const auto &sp : binaryModInfo->serializedSearchPaths)
            ScanningWorker->workerASTContext->addSearchPath(
                sp.Path, sp.IsFramework, sp.IsSystem);

          auto importIdentifier =
              getModuleImportIdentifier(moduleImport.importIdentifier);
          SwiftModuleScannerQueryResult swiftResult =
              ScanningWorker->scanFilesystemForSwiftModuleDependency(
                  importIdentifier, /* isTestableImport */ false);
          if (swiftResult.foundDependencyInfo)
            return std::make_pair(
                binaryDepID,
                swiftResult.foundDependencyInfo->getModuleDefiningPath());

          auto clangResult =
              ScanningWorker->scanFilesystemForClangModuleDependency(
                  importIdentifier,
                  [this](const auto &cd, auto mok) -> std::string {
                    return clangModuleOutputPathLookup(cd, mok);
                  }, {});
          if (clangResult)
            return std::make_pair(
                binaryDepID, clangResult->ModuleGraph[0].ClangModuleMapFile);
          return std::nullopt;
        });
    if (result)
      break;
  }

  return result;
}
