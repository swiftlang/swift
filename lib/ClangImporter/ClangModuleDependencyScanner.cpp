//===--- ClangModuleDependencyScanner.cpp - Dependency Scanning -----------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file implements dependency scanning for Clang modules.
//
//===----------------------------------------------------------------------===//
#include "ImporterImpl.h"
#include "swift/AST/ModuleDependencies.h"
#include "swift/ClangImporter/ClangImporter.h"
#include "clang/Tooling/DependencyScanning/DependencyScanningService.h"
#include "clang/Tooling/DependencyScanning/DependencyScanningTool.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Signals.h"

using namespace swift;

using namespace clang::tooling;
using namespace clang::tooling::dependencies;

class swift::ClangModuleDependenciesCacheImpl {
  /// Cache the names of the files used for the "import hack" to compute module
  /// dependencies.
  /// FIXME: This should go away once Clang's dependency scanning library
  /// can scan by module name.
  llvm::StringMap<std::string> importHackFileCache;

public:
  /// Set containing all of the Clang modules that have already been seen.
  llvm::StringSet<> alreadySeen;

  DependencyScanningService service;

  DependencyScanningTool tool;

  ClangModuleDependenciesCacheImpl()
      : importHackFileCache(),
        service(ScanningMode::MinimizedSourcePreprocessing, ScanningOutputFormat::Full),
        tool(service) { }
  ~ClangModuleDependenciesCacheImpl();

  /// Retrieve the name of the file used for the "import hack" that is
  /// used to scan the dependencies of a Clang module.
  llvm::ErrorOr<StringRef> getImportHackFile(StringRef moduleName);
};

ClangModuleDependenciesCacheImpl::~ClangModuleDependenciesCacheImpl() {
  if (!importHackFileCache.empty()) {
    for (auto& it: importHackFileCache) {
      llvm::sys::fs::remove(it.second);
    }
  }
}

llvm::ErrorOr<StringRef> ClangModuleDependenciesCacheImpl::getImportHackFile(StringRef moduleName) {
  auto cacheIt = importHackFileCache.find(moduleName.str());
  if (cacheIt != importHackFileCache.end())
    return cacheIt->second;

  // Create a temporary file.
  int resultFD;
  SmallString<128> resultPath;
  if (auto error = llvm::sys::fs::createTemporaryFile(
          "import-hack-" + moduleName.str(), "c", resultFD, resultPath))
    return error;

  llvm::raw_fd_ostream out(resultFD, /*shouldClose=*/true);
  out << "#pragma clang module import " << moduleName.str() << ";\n";
  llvm::sys::RemoveFileOnSignal(resultPath);
  importHackFileCache.insert(std::make_pair(moduleName, resultPath.str().str()));
  return importHackFileCache[moduleName];
}

namespace {
  class SingleCommandCompilationDatabase : public CompilationDatabase {
  public:
    SingleCommandCompilationDatabase(CompileCommand Cmd)
        : Command(std::move(Cmd)) {}

    virtual std::vector<CompileCommand>
    getCompileCommands(StringRef FilePath) const override {
      return {Command};
    }

    virtual std::vector<CompileCommand> getAllCompileCommands() const override {
      return {Command};
    }

  private:
    CompileCommand Command;
  };
}

// Add search paths.
// Note: This is handled differently for the Clang importer itself, which
// adds search paths to Clang's data structures rather than to its
// command line.
static void addSearchPathInvocationArguments(
    std::vector<std::string> &invocationArgStrs, ASTContext &ctx) {
  SearchPathOptions &searchPathOpts = ctx.SearchPathOpts;
  for (const auto &framepath : searchPathOpts.FrameworkSearchPaths) {
    invocationArgStrs.push_back(framepath.IsSystem ? "-iframework" : "-F");
    invocationArgStrs.push_back(framepath.Path);
  }

  for (auto path : searchPathOpts.ImportSearchPaths) {
    invocationArgStrs.push_back("-I");
    invocationArgStrs.push_back(path);
  }
}

/// Create the command line for Clang dependency scanning.
static std::vector<std::string> getClangDepScanningInvocationArguments(
    ASTContext &ctx,
    StringRef sourceFileName) {
  std::vector<std::string> commandLineArgs;

  // Form the basic command line.
  commandLineArgs.push_back("clang");
  importer::getNormalInvocationArguments(commandLineArgs, ctx);
  importer::addCommonInvocationArguments(commandLineArgs, ctx);
  addSearchPathInvocationArguments(commandLineArgs, ctx);

  auto sourceFilePos = std::find(
      commandLineArgs.begin(), commandLineArgs.end(),
      "<swift-imported-modules>");
  assert(sourceFilePos != commandLineArgs.end());
  *sourceFilePos = sourceFileName.str();

  // HACK! Drop the -fmodule-format= argument and the one that
  // precedes it.
  {
    auto moduleFormatPos = std::find_if(commandLineArgs.begin(),
                                        commandLineArgs.end(),
                                        [](StringRef arg) {
      return arg.startswith("-fmodule-format=");
    });
    assert(moduleFormatPos != commandLineArgs.end());
    assert(moduleFormatPos != commandLineArgs.begin());
    commandLineArgs.erase(moduleFormatPos-1, moduleFormatPos+1);
  }

  // HACK: No -fsyntax-only here?
  {
    auto syntaxOnlyPos = std::find(commandLineArgs.begin(),
                                   commandLineArgs.end(),
                                   "-fsyntax-only");
    assert(syntaxOnlyPos != commandLineArgs.end());
    *syntaxOnlyPos = "-c";
  }

  // HACK: Stolen from ClangScanDeps.cpp
  commandLineArgs.push_back("-o");
  commandLineArgs.push_back("/dev/null");
  commandLineArgs.push_back("-M");
  commandLineArgs.push_back("-MT");
  commandLineArgs.push_back("import-hack.o");
  commandLineArgs.push_back("-Xclang");
  commandLineArgs.push_back("-Eonly");
  commandLineArgs.push_back("-Xclang");
  commandLineArgs.push_back("-sys-header-deps");
  commandLineArgs.push_back("-Wno-error");

  return commandLineArgs;
}

/// Get or create the Clang-specific
static ClangModuleDependenciesCacheImpl *getOrCreateClangImpl(
    ModuleDependenciesCache &cache) {
  auto clangImpl = cache.getClangImpl();
  if (!clangImpl) {
    clangImpl = new ClangModuleDependenciesCacheImpl();
    cache.setClangImpl(clangImpl,
                       [](ClangModuleDependenciesCacheImpl *ptr) {
      delete ptr;
    });
  }

  return clangImpl;
}

/// Record the module dependencies we found by scanning Clang modules into
/// the module dependencies cache.
void ClangImporter::recordModuleDependencies(
    ModuleDependenciesCache &cache,
    const FullDependenciesResult &clangDependencies) {
  struct ModuleInfo {
    std::string PCMPath;
    std::string ModuleMapPath;
  };
  auto ModuleCacheDir = swift::getModuleCachePathFromClang(getClangInstance());

  for (const auto &clangModuleDep : clangDependencies.DiscoveredModules) {
    // If we've already cached this information, we're done.
    if (cache.hasDependencies(clangModuleDep.ModuleName,
                              ModuleDependenciesKind::Clang))
      continue;

    // File dependencies for this module.
    std::vector<std::string> fileDeps;
    for (const auto &fileDep : clangModuleDep.FileDeps) {
      fileDeps.push_back(fileDep.getKey().str());
    }
    // Inherit all Clang driver args when creating the clang importer.
    ArrayRef<std::string> allArgs = Impl.ClangArgs;
    ClangImporterOptions Opts;

    // Ensure the arguments we collected is sufficient to create a Clang
    // invocation.
    assert(createClangInvocation(this, Opts, allArgs));

    std::vector<std::string> swiftArgs;
    // We are using Swift frontend mode.
    swiftArgs.push_back("-frontend");
    // We pass the entire argument list via -Xcc, so the invocation should
    // use extra clang options alone.
    swiftArgs.push_back("-only-use-extra-clang-opts");
    auto addClangArg = [&](StringRef arg) {
      swiftArgs.push_back("-Xcc");
      swiftArgs.push_back(arg.str());
    };
    // Add all args inheritted from creating the importer.
    auto It = allArgs.begin();
    while(It != allArgs.end()) {
      StringRef arg = *It;
      // Remove the -target arguments because we should use the target triple
      // from the depending Swift modules.
      if (arg == "-target") {
        It += 2;
      } else if (arg.startswith("-fapinotes-swift-version=")) {
        // Remove the apinotes version because we should use the language version
        // specified in the interface file.
        It += 1;
      } else {
        addClangArg(*It);
        ++ It;
      }
    }

    // Swift frontend action: -emit-pcm
    swiftArgs.push_back("-emit-pcm");
    swiftArgs.push_back("-module-name");
    swiftArgs.push_back(clangModuleDep.ModuleName);

    // Pass down search paths to the -emit-module action.
    // Unlike building Swift modules, we need to include all search paths to
    // the clang invocation to build PCMs because transitive headers can only
    // be found via search paths. Passing these headers as explicit inputs can
    // be quite challenging.
    for (auto &path: Impl.SwiftContext.SearchPathOpts.ImportSearchPaths) {
      addClangArg("-I" + path);
    }
    for (auto &path: Impl.SwiftContext.SearchPathOpts.FrameworkSearchPaths) {
      addClangArg((path.IsSystem ? "-Fsystem": "-F") + path.Path);
    }

    // Swift frontend option for input file path (Foo.modulemap).
    swiftArgs.push_back(clangModuleDep.ClangModuleMapFile);
    // Module-level dependencies.
    llvm::StringSet<> alreadyAddedModules;
    auto dependencies = ModuleDependencies::forClangModule(
        clangModuleDep.ImplicitModulePCMPath,
        clangModuleDep.ClangModuleMapFile,
        clangModuleDep.ContextHash,
        swiftArgs,
        fileDeps);
    for (const auto &moduleName : clangModuleDep.ClangModuleDeps) {
      dependencies.addModuleDependency(moduleName.ModuleName, &alreadyAddedModules);
    }

    cache.recordDependencies(clangModuleDep.ModuleName,
                             std::move(dependencies),
                             ModuleDependenciesKind::Clang);
  }
}

Optional<ModuleDependencies> ClangImporter::getModuleDependencies(
    StringRef moduleName, ModuleDependenciesCache &cache,
    InterfaceSubContextDelegate &delegate) {
  // Check whether there is already a cached result.
  if (auto found = cache.findDependencies(
          moduleName, ModuleDependenciesKind::Clang))
    return found;

  // Retrieve or create the shared state.
  auto clangImpl = getOrCreateClangImpl(cache);

  // HACK! Replace the module import buffer name with the source file hack.
  auto importHackFile = clangImpl->getImportHackFile(moduleName);
  if (!importHackFile) {
    // FIXME: Emit a diagnostic here.
    return None;
  }

  // Determine the command-line arguments for dependency scanning.
  auto &ctx = Impl.SwiftContext;
  std::vector<std::string> commandLineArgs =
    getClangDepScanningInvocationArguments(ctx, *importHackFile);

  std::string workingDir =
      ctx.SourceMgr.getFileSystem()->getCurrentWorkingDirectory().get();
  CompileCommand command(workingDir, *importHackFile, commandLineArgs, "-");
  SingleCommandCompilationDatabase database(command);

  auto clangDependencies = clangImpl->tool.getFullDependencies(
      database, workingDir, clangImpl->alreadySeen);
  if (!clangDependencies) {
    // FIXME: Route this to a normal diagnostic.
    llvm::logAllUnhandledErrors(clangDependencies.takeError(), llvm::errs());
    return None;
  }

  // Record module dependencies for each module we found.
  recordModuleDependencies(cache, *clangDependencies);
  return cache.findDependencies(moduleName, ModuleDependenciesKind::Clang);
}

bool ClangImporter::addBridgingHeaderDependencies(
    StringRef moduleName,
    ModuleDependenciesCache &cache) {
  auto targetModule = *cache.findDependencies(
      moduleName, ModuleDependenciesKind::Swift);

  // If we've already recorded bridging header dependencies, we're done.
  auto swiftDeps = targetModule.getAsSwiftModule();
  if (!swiftDeps->bridgingSourceFiles.empty() ||
      !swiftDeps->bridgingModuleDependencies.empty())
    return false;

  // Retrieve or create the shared state.
  auto clangImpl = getOrCreateClangImpl(cache);

  // Retrieve the bridging header.
  std::string bridgingHeader = *targetModule.getBridgingHeader();

  // Determine the command-line arguments for dependency scanning.
  auto &ctx = Impl.SwiftContext;
  std::vector<std::string> commandLineArgs =
    getClangDepScanningInvocationArguments(ctx, bridgingHeader);

  std::string workingDir =
      ctx.SourceMgr.getFileSystem()->getCurrentWorkingDirectory().get();
  CompileCommand command(workingDir, bridgingHeader, commandLineArgs, "-");
  SingleCommandCompilationDatabase database(command);

  auto clangDependencies = clangImpl->tool.getFullDependencies(
      database, workingDir, clangImpl->alreadySeen);

  if (!clangDependencies) {
    // FIXME: Route this to a normal diagnostic.
    llvm::logAllUnhandledErrors(clangDependencies.takeError(), llvm::errs());
    return true;
  }

  // Record module dependencies for each module we found.
  recordModuleDependencies(cache, *clangDependencies);

  // Record dependencies for the source files the bridging header includes.
  for (const auto &fileDep : clangDependencies->FullDeps.FileDeps)
    targetModule.addBridgingSourceFile(fileDep);

  // ... and all module dependencies.
  llvm::StringSet<> alreadyAddedModules;
  for (const auto &moduleDep : clangDependencies->FullDeps.ClangModuleDeps) {
    targetModule.addBridgingModuleDependency(
        moduleDep.ModuleName, alreadyAddedModules);
  }

  // Update the cache with the new information for the module.
  cache.updateDependencies(
     {moduleName.str(), ModuleDependenciesKind::Swift},
     std::move(targetModule));

  return false;
}
