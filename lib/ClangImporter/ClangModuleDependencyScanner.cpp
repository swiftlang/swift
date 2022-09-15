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
#include "swift/AST/DiagnosticsSema.h"
#include "swift/AST/ModuleDependencies.h"
#include "swift/Basic/SourceManager.h"
#include "swift/ClangImporter/ClangImporter.h"
#include "clang/Tooling/DependencyScanning/DependencyScanningService.h"
#include "clang/Tooling/DependencyScanning/DependencyScanningTool.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Signals.h"

using namespace swift;

using namespace clang::tooling;
using namespace clang::tooling::dependencies;

// Add search paths.
// Note: This is handled differently for the Clang importer itself, which
// adds search paths to Clang's data structures rather than to its
// command line.
static void addSearchPathInvocationArguments(
    std::vector<std::string> &invocationArgStrs, ASTContext &ctx) {
  SearchPathOptions &searchPathOpts = ctx.SearchPathOpts;
  for (const auto &framepath : searchPathOpts.getFrameworkSearchPaths()) {
    invocationArgStrs.push_back(framepath.IsSystem ? "-iframework" : "-F");
    invocationArgStrs.push_back(framepath.Path);
  }

  for (const auto &path : searchPathOpts.getImportSearchPaths()) {
    invocationArgStrs.push_back("-I");
    invocationArgStrs.push_back(path);
  }
}

/// Create the command line for Clang dependency scanning.
static std::vector<std::string> getClangDepScanningInvocationArguments(
    ASTContext &ctx,
    Optional<StringRef> sourceFileName = None) {
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
  if (sourceFileName.hasValue())
    *sourceFilePos = sourceFileName->str();
  else
    commandLineArgs.erase(sourceFilePos);

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

  return commandLineArgs;
}

/// Record the module dependencies we found by scanning Clang modules into
/// the module dependencies cache.
void ClangImporter::recordModuleDependencies(
    ModuleDependenciesCache &cache,
    const FullDependenciesResult &clangDependencies) {
  auto &ctx = Impl.SwiftContext;
  auto currentSwiftSearchPathSet = ctx.getAllModuleSearchPathsSet();

  // This scanner invocation's already-captured APINotes version
  std::vector<std::string> capturedPCMArgs = {
    "-Xcc",
    ("-fapinotes-swift-version=" +
     ctx.LangOpts.EffectiveLanguageVersion.asAPINotesVersionString())
  };

  for (const auto &clangModuleDep : clangDependencies.DiscoveredModules) {
    // If we've already cached this information, we're done.
    if (cache.hasDependencies(
                    clangModuleDep.ID.ModuleName,
                    {ModuleDependenciesKind::Clang, currentSwiftSearchPathSet}))
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
    assert(createClangInvocation(this, Opts, nullptr, allArgs));

    std::vector<std::string> swiftArgs;
    // We are using Swift frontend mode.
    swiftArgs.push_back("-frontend");
    // We pass the entire argument list via -Xcc, so the invocation should
    // use extra clang options alone.
    swiftArgs.push_back("-only-use-extra-clang-opts");
    auto addClangArg = [&](Twine arg) {
      swiftArgs.push_back("-Xcc");
      swiftArgs.push_back(arg.str());
    };
    auto addClangFrontendArg = [&](Twine arg) {
      addClangArg("-Xclang");
      addClangArg(arg);
    };

    // Add all args inherited from creating the importer.
    auto It = allArgs.begin();

    {
      StringRef arg = *It;
      if (arg == "clang" ||
          arg.endswith(llvm::sys::path::get_separator().str() + "clang")) {
        // Remove the initial path to clang executable argument, to avoid
        // treating it as an executable input to compilation. It is not needed
        // because the consumer of this command-line will invoke the emit-PCM
        // action via swift-frontend.
        It += 1;
      }
    }

    while(It != allArgs.end()) {
      StringRef arg = *It;
      // Remove the -target arguments because we should use the target triple
      // specified with `-clang-target` on the scanner invocation, or
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

    // Add the equivalent of the old `getAdditionalArgsWithoutModulePaths`.
    // TODO: Should we be passing all cc1 args (ie.
    // `getCanonicalCommandLineWithoutModulePaths`)?
    addClangFrontendArg("-fno-implicit-modules");
    addClangFrontendArg("-emit-module");
    addClangFrontendArg(Twine("-fmodule-name=") + clangModuleDep.ID.ModuleName);
    if (clangModuleDep.IsSystem)
      addClangFrontendArg("-fsystem-module");
    if (clangModuleDep.BuildInvocation.getLangOpts()->NeededByPCHOrCompilationUsesPCH)
      addClangFrontendArg("-fmodule-related-to-pch");

    // If the scanner is invoked with '-clang-target', ensure this is the target
    // used to build this PCM.
    if (Impl.SwiftContext.LangOpts.ClangTarget.hasValue()) {
      llvm::Triple triple = Impl.SwiftContext.LangOpts.ClangTarget.getValue();
      addClangArg("-target");
      addClangArg(triple.str());
    }

    // Swift frontend action: -emit-pcm
    swiftArgs.push_back("-emit-pcm");
    swiftArgs.push_back("-module-name");
    swiftArgs.push_back(clangModuleDep.ID.ModuleName);

    // Pass down search paths to the -emit-module action.
    // Unlike building Swift modules, we need to include all search paths to
    // the clang invocation to build PCMs because transitive headers can only
    // be found via search paths. Passing these headers as explicit inputs can
    // be quite challenging.
    for (const auto &path :
         Impl.SwiftContext.SearchPathOpts.getImportSearchPaths()) {
      addClangArg("-I" + path);
    }
    for (const auto &path :
         Impl.SwiftContext.SearchPathOpts.getFrameworkSearchPaths()) {
      addClangArg((path.IsSystem ? "-Fsystem": "-F") + path.Path);
    }

    // Swift frontend option for input file path (Foo.modulemap).
    swiftArgs.push_back(clangModuleDep.ClangModuleMapFile);
    // Module-level dependencies.
    llvm::StringSet<> alreadyAddedModules;
    auto dependencies = ModuleDependencies::forClangModule(
        clangModuleDep.ClangModuleMapFile,
        clangModuleDep.ID.ContextHash,
        swiftArgs,
        fileDeps,
        capturedPCMArgs);
    for (const auto &moduleName : clangModuleDep.ClangModuleDeps) {
      dependencies.addModuleDependency(moduleName.ModuleName, &alreadyAddedModules);
    }

    cache.recordDependencies(clangModuleDep.ID.ModuleName,
                             std::move(dependencies));
  }
}

Optional<ModuleDependencies> ClangImporter::getModuleDependencies(
    StringRef moduleName, ModuleDependenciesCache &cache,
    InterfaceSubContextDelegate &delegate) {
  auto &ctx = Impl.SwiftContext;
  auto currentSwiftSearchPathSet = ctx.getAllModuleSearchPathsSet();
  // Check whether there is already a cached result.
  if (auto found = cache.findDependencies(
          moduleName,
          {ModuleDependenciesKind::Clang, currentSwiftSearchPathSet}))
    return found;

  // Determine the command-line arguments for dependency scanning.
  std::vector<std::string> commandLineArgs =
    getClangDepScanningInvocationArguments(ctx);
  // The Swift compiler does not have a concept of a working directory.
  // It is instead handled by the Swift driver by resolving relative paths
  // according to the driver's notion of a working directory. On the other hand,
  // Clang does have a concept working directory which may be specified on this
  // Clang invocation with '-working-directory'. If so, it is crucial that we use
  // this directory as an argument to the Clang scanner invocation below.
  std::string workingDir;
  auto clangWorkingDirPos = std::find(commandLineArgs.rbegin(),
                                      commandLineArgs.rend(),
                                      "-working-directory");
  if (clangWorkingDirPos == commandLineArgs.rend())
    workingDir = ctx.SourceMgr.getFileSystem()->getCurrentWorkingDirectory().get();
  else {
    if (clangWorkingDirPos - 1 == commandLineArgs.rend()) {
      ctx.Diags.diagnose(SourceLoc(), diag::clang_dependency_scan_error, "Missing '-working-directory' argument");
      return None;
    }
    workingDir = *(clangWorkingDirPos - 1);
  }

  auto clangDependencies = cache.getClangScannerTool().getFullDependencies(
      commandLineArgs, workingDir, cache.getAlreadySeenClangModules(),
      moduleName);
  if (!clangDependencies) {
    auto errorStr = toString(clangDependencies.takeError());
    // We ignore the "module 'foo' not found" error, the Swift dependency
    // scanner will report such an error only if all of the module loaders
    // fail as well.
    if (errorStr.find("fatal error: module '" + moduleName.str() +
                      "' not found") == std::string::npos)
      ctx.Diags.diagnose(SourceLoc(), diag::clang_dependency_scan_error,
                         errorStr);
    return None;
  }

  // Record module dependencies for each module we found.
  recordModuleDependencies(cache, *clangDependencies);
            return cache.findDependencies(
                    moduleName,
                    {ModuleDependenciesKind::Clang, currentSwiftSearchPathSet});
}

bool ClangImporter::addBridgingHeaderDependencies(
    StringRef moduleName,
    ModuleDependenciesKind moduleKind,
    ModuleDependenciesCache &cache) {
  auto &ctx = Impl.SwiftContext;
  auto currentSwiftSearchPathSet = ctx.getAllModuleSearchPathsSet();
  
  auto targetModule = *cache.findDependencies(
              moduleName,
              {moduleKind,
               currentSwiftSearchPathSet});

  // If we've already recorded bridging header dependencies, we're done.
  if (auto swiftInterfaceDeps = targetModule.getAsSwiftInterfaceModule()) {
    if (!swiftInterfaceDeps->textualModuleDetails.bridgingSourceFiles.empty() ||
        !swiftInterfaceDeps->textualModuleDetails.bridgingModuleDependencies.empty())
      return false;
  } else if (auto swiftSourceDeps = targetModule.getAsSwiftSourceModule()) {
    if (!swiftSourceDeps->textualModuleDetails.bridgingSourceFiles.empty() ||
        !swiftSourceDeps->textualModuleDetails.bridgingModuleDependencies.empty())
      return false;
  } else {
    llvm_unreachable("Unexpected module dependency kind");
  }

  // Retrieve the bridging header.
  std::string bridgingHeader = *targetModule.getBridgingHeader();

  // Determine the command-line arguments for dependency scanning.
  std::vector<std::string> commandLineArgs =
    getClangDepScanningInvocationArguments(ctx, StringRef(bridgingHeader));
  std::string workingDir =
      ctx.SourceMgr.getFileSystem()->getCurrentWorkingDirectory().get();

  auto clangDependencies = cache.getClangScannerTool().getFullDependencies(
      commandLineArgs, workingDir, cache.getAlreadySeenClangModules());
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
     {moduleName.str(), moduleKind},
     std::move(targetModule));

  return false;
}
