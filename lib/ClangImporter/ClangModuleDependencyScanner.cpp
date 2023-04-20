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
#include "llvm/Support/Path.h"

using namespace swift;

using namespace clang::tooling;
using namespace clang::tooling::dependencies;

static std::string
moduleCacheRelativeLookupModuleOutput(const ModuleID &MID, ModuleOutputKind MOK,
                                      const std::string &moduleCachePathStr) {
  llvm::SmallString<128> outputPath(moduleCachePathStr);
  llvm::sys::path::append(outputPath, MID.ModuleName + "-" + MID.ContextHash);
  switch (MOK) {
  case ModuleOutputKind::ModuleFile:
    llvm::sys::path::replace_extension(
        outputPath, getExtension(swift::file_types::TY_ClangModuleFile));
    break;
  case ModuleOutputKind::DependencyFile:
    llvm::sys::path::replace_extension(
        outputPath, getExtension(swift::file_types::TY_Dependencies));
    break;
  case ModuleOutputKind::DependencyTargets:
    return MID.ModuleName + "-" + MID.ContextHash;
  case ModuleOutputKind::DiagnosticSerializationFile:
    llvm::sys::path::replace_extension(
        outputPath, getExtension(swift::file_types::TY_SerializedDiagnostics));
    break;
  }
  return outputPath.str().str();
}

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
  std::vector<std::string> commandLineArgs =
      ClangImporter::getClangArguments(ctx);
  addSearchPathInvocationArguments(commandLineArgs, ctx);

  auto sourceFilePos = std::find(
      commandLineArgs.begin(), commandLineArgs.end(),
      "<swift-imported-modules>");
  assert(sourceFilePos != commandLineArgs.end());
  if (sourceFileName.has_value())
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

  // The Clang modules produced by ClangImporter are always embedded in an
  // ObjectFilePCHContainer and contain -gmodules debug info.
  commandLineArgs.push_back("-gmodules");

  return commandLineArgs;
}

/// Record the module dependencies we found by scanning Clang modules into
/// the module dependencies cache.
void ClangImporter::recordModuleDependencies(
    ModuleDependenciesCache &cache,
    const ModuleDepsGraph &clangModuleDependencies) {
  auto &ctx = Impl.SwiftContext;

  // This scanner invocation's already-captured APINotes version
  std::vector<std::string> capturedPCMArgs = {
    "-Xcc",
    ("-fapinotes-swift-version=" +
     ctx.LangOpts.EffectiveLanguageVersion.asAPINotesVersionString())
  };

  for (const auto &clangModuleDep : clangModuleDependencies) {
    // If we've already cached this information, we're done.
    if (cache.hasDependency(
                    clangModuleDep.ID.ModuleName,
                    ModuleDependencyKind::Clang))
      continue;

    // File dependencies for this module.
    std::vector<std::string> fileDeps;
    for (const auto &fileDep : clangModuleDep.FileDeps) {
      fileDeps.push_back(fileDep.getKey().str());
    }

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

    // We pass the entire argument list via -Xcc, so the invocation should
    // use extra clang options alone.
    swiftArgs.push_back("-only-use-extra-clang-opts");

    auto pcmPath = moduleCacheRelativeLookupModuleOutput(
        clangModuleDep.ID, ModuleOutputKind::ModuleFile,
        getModuleCachePathFromClang(getClangInstance()));
    swiftArgs.push_back("-o");
    swiftArgs.push_back(pcmPath);

    // Ensure that the resulting PCM build invocation uses Clang frontend
    // directly
    swiftArgs.push_back("-direct-clang-cc1-module-build");

    // Swift frontend option for input file path (Foo.modulemap).
    swiftArgs.push_back(clangModuleDep.ClangModuleMapFile);

    // Add args reported by the scanner.
    llvm::for_each(clangModuleDep.BuildArguments, addClangArg);

    // Module-level dependencies.
    llvm::StringSet<> alreadyAddedModules;
    auto dependencies = ModuleDependencyInfo::forClangModule(
        pcmPath,
        clangModuleDep.ClangModuleMapFile,
        clangModuleDep.ID.ContextHash,
        swiftArgs,
        fileDeps,
        capturedPCMArgs);
    for (const auto &moduleName : clangModuleDep.ClangModuleDeps) {
      dependencies.addModuleImport(moduleName.ModuleName, &alreadyAddedModules);
      // It is safe to assume that all dependencies of a Clang module are Clang modules.
      // Doing this allows us to skip "resolving" Clang modules down the line.
      dependencies.addModuleDependency({moduleName.ModuleName, ModuleDependencyKind::Clang});
    }
    dependencies.setIsResolved(true);

    cache.recordDependency(clangModuleDep.ID.ModuleName,
                             std::move(dependencies));
  }
}

Optional<const ModuleDependencyInfo*> ClangImporter::getModuleDependencies(
    StringRef moduleName, ModuleDependenciesCache &cache,
    InterfaceSubContextDelegate &delegate, bool isTestableImport) {
  auto &ctx = Impl.SwiftContext;
  // Determine the command-line arguments for dependency scanning.
  std::vector<std::string> commandLineArgs =
      getClangDepScanningInvocationArguments(ctx);
  // The Swift compiler does not have a concept of a working directory.
  // It is instead handled by the Swift driver by resolving relative paths
  // according to the driver's notion of a working directory. On the other hand,
  // Clang does have a concept working directory which may be specified on this
  // Clang invocation with '-working-directory'. If so, it is crucial that we
  // use this directory as an argument to the Clang scanner invocation below.
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
      return None;
    }
    workingDir = *(clangWorkingDirPos - 1);
  }

  auto moduleCachePath = getModuleCachePathFromClang(getClangInstance());
  auto lookupModuleOutput =
      [moduleCachePath](const ModuleID &MID,
                        ModuleOutputKind MOK) -> std::string {
    return moduleCacheRelativeLookupModuleOutput(MID, MOK, moduleCachePath);
  };

  auto clangModuleDependencies =
      cache.getClangScannerTool().getModuleDependencies(
          moduleName, commandLineArgs, workingDir,
          cache.getAlreadySeenClangModules(), lookupModuleOutput);
  if (!clangModuleDependencies) {
    auto errorStr = toString(clangModuleDependencies.takeError());
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
  recordModuleDependencies(cache, *clangModuleDependencies);
  return cache.findDependency(moduleName, ModuleDependencyKind::Clang);
}

bool ClangImporter::addBridgingHeaderDependencies(
    StringRef moduleName, ModuleDependencyKind moduleKind,
    ModuleDependenciesCache &cache) {
  auto &ctx = Impl.SwiftContext;
  auto optionalTargetModule = cache.findDependency(moduleName, moduleKind);
  assert(optionalTargetModule.has_value());
  auto targetModule = *(optionalTargetModule.value());

  // If we've already recorded bridging header dependencies, we're done.
  if (auto swiftInterfaceDeps = targetModule.getAsSwiftInterfaceModule()) {
    if (!swiftInterfaceDeps->textualModuleDetails.bridgingSourceFiles.empty() ||
        !swiftInterfaceDeps->textualModuleDetails.bridgingModuleDependencies
             .empty())
      return false;
  } else if (auto swiftSourceDeps = targetModule.getAsSwiftSourceModule()) {
    if (!swiftSourceDeps->textualModuleDetails.bridgingSourceFiles.empty() ||
        !swiftSourceDeps->textualModuleDetails.bridgingModuleDependencies
             .empty())
      return false;
  } else {
    llvm_unreachable("Unexpected module dependency kind");
  }

  // Retrieve the bridging header.
  std::string bridgingHeader = *(targetModule.getBridgingHeader());

  // Determine the command-line arguments for dependency scanning.
  std::vector<std::string> commandLineArgs =
      getClangDepScanningInvocationArguments(ctx, StringRef(bridgingHeader));
  std::string workingDir =
      ctx.SourceMgr.getFileSystem()->getCurrentWorkingDirectory().get();

  auto moduleCachePath = getModuleCachePathFromClang(getClangInstance());
  auto lookupModuleOutput =
      [moduleCachePath](const ModuleID &MID,
                        ModuleOutputKind MOK) -> std::string {
    return moduleCacheRelativeLookupModuleOutput(MID, MOK, moduleCachePath);
  };

  auto clangModuleDependencies =
      cache.getClangScannerTool().getTranslationUnitDependencies(
          commandLineArgs, workingDir, cache.getAlreadySeenClangModules(),
          lookupModuleOutput);
  if (!clangModuleDependencies) {
    // FIXME: Route this to a normal diagnostic.
    llvm::logAllUnhandledErrors(clangModuleDependencies.takeError(), llvm::errs());
    return true;
  }

  // Record module dependencies for each module we found.
  recordModuleDependencies(cache, clangModuleDependencies->ModuleGraph);

  // Record dependencies for the source files the bridging header includes.
  for (const auto &fileDep : clangModuleDependencies->FileDeps)
    targetModule.addBridgingSourceFile(fileDep);

  // ... and all module dependencies.
  llvm::StringSet<> alreadyAddedModules;
  for (const auto &moduleDep : clangModuleDependencies->ModuleGraph) {
    targetModule.addBridgingModuleDependency(moduleDep.ID.ModuleName,
                                             alreadyAddedModules);
  }

  // Update the cache with the new information for the module.
  cache.updateDependency({moduleName.str(), moduleKind}, targetModule);

  return false;
}
