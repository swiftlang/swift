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
#include "swift/Basic/Assertions.h"
#include "clang/Basic/Diagnostic.h"
#include "clang/CAS/CASOptions.h"
#include "clang/Frontend/CompilerInvocation.h"
#include "clang/Frontend/FrontendOptions.h"
#include "clang/Tooling/DependencyScanning/DependencyScanningService.h"
#include "clang/Tooling/DependencyScanning/DependencyScanningTool.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/Support/Allocator.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Signals.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/StringSaver.h"

using namespace swift;

using namespace clang::tooling;
using namespace clang::tooling::dependencies;

static std::string
moduleCacheRelativeLookupModuleOutput(const ModuleID &MID, ModuleOutputKind MOK,
                                      const StringRef moduleCachePath) {
  llvm::SmallString<128> outputPath(moduleCachePath);
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

  for (const auto &arg : searchPathOpts.ScannerPrefixMapper) {
    std::string prefixMapArg = "-fdepscan-prefix-map=" + arg;
    invocationArgStrs.push_back(prefixMapArg);
  }
}

/// Create the command line for Clang dependency scanning.
static std::vector<std::string> getClangDepScanningInvocationArguments(
    ASTContext &ctx, std::optional<StringRef> sourceFileName = std::nullopt) {
  std::vector<std::string> commandLineArgs =
      ClangImporter::getClangDriverArguments(ctx);
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
      return arg.starts_with("-fmodule-format=");
    });
    assert(moduleFormatPos != commandLineArgs.end());
    assert(moduleFormatPos != commandLineArgs.begin());
    commandLineArgs.erase(moduleFormatPos-1, moduleFormatPos+1);
  }

  // Use `-fsyntax-only` to do dependency scanning and assert if not there.
  assert(std::find(commandLineArgs.begin(), commandLineArgs.end(),
                   "-fsyntax-only") != commandLineArgs.end() &&
         "missing -fsyntax-only option");

  // The Clang modules produced by ClangImporter are always embedded in an
  // ObjectFilePCHContainer and contain -gmodules debug info.
  commandLineArgs.push_back("-gmodules");

  // To use -gmodules we need to have a real path for the PCH; this option has
  // no effect if caching is disabled.
  commandLineArgs.push_back("-Xclang");
  commandLineArgs.push_back("-finclude-tree-preserve-pch-path");

  return commandLineArgs;
}

static std::unique_ptr<llvm::PrefixMapper>
getClangPrefixMapper(DependencyScanningTool &clangScanningTool,
                     ModuleDeps &clangModuleDep,
                     clang::CompilerInvocation &depsInvocation) {
  std::unique_ptr<llvm::PrefixMapper> Mapper;
  if (clangModuleDep.IncludeTreeID) {
    Mapper = std::make_unique<llvm::PrefixMapper>();
  } else if (clangModuleDep.CASFileSystemRootID) {
    assert(clangScanningTool.getCachingFileSystem());
    Mapper = std::make_unique<llvm::TreePathPrefixMapper>(
        clangScanningTool.getCachingFileSystem());
  }

  if (Mapper)
    DepscanPrefixMapping::configurePrefixMapper(depsInvocation, *Mapper);

  return Mapper;
}

ModuleDependencyVector ClangImporter::bridgeClangModuleDependencies(
    clang::tooling::dependencies::DependencyScanningTool &clangScanningTool,
    clang::tooling::dependencies::ModuleDepsGraph &clangDependencies,
    StringRef moduleOutputPath, RemapPathCallback callback) {
  const auto &ctx = Impl.SwiftContext;
  ModuleDependencyVector result;

  auto remapPath = [&](StringRef path) {
    if (callback)
      return callback(path);
    return path.str();
  };

  // This scanner invocation's already-captured APINotes version
  std::vector<std::string>
      capturedPCMArgs = {
          "-Xcc",
          ("-fapinotes-swift-version=" +
           ctx.LangOpts.EffectiveLanguageVersion.asAPINotesVersionString())};

  for (auto &clangModuleDep : clangDependencies) {
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

    auto pcmPath = moduleCacheRelativeLookupModuleOutput(
        clangModuleDep.ID, ModuleOutputKind::ModuleFile, moduleOutputPath);
    swiftArgs.push_back("-o");
    swiftArgs.push_back(pcmPath);

    // Ensure that the resulting PCM build invocation uses Clang frontend
    // directly
    swiftArgs.push_back("-direct-clang-cc1-module-build");

    // Swift frontend option for input file path (Foo.modulemap).
    swiftArgs.push_back(remapPath(clangModuleDep.ClangModuleMapFile));

    // Handle VFSOverlay. If include tree is used, there is no need for overlay.
    if (!ctx.ClangImporterOpts.UseClangIncludeTree) {
      for (auto &overlay : ctx.SearchPathOpts.VFSOverlayFiles) {
        swiftArgs.push_back("-vfsoverlay");
        swiftArgs.push_back(remapPath(overlay));
      }
    }

    // Add args reported by the scanner.

    // Round-trip clang args to canonicalize and clear the options that swift
    // compiler doesn't need.
    clang::CompilerInvocation depsInvocation;
    clang::DiagnosticsEngine clangDiags(new clang::DiagnosticIDs(),
                                        new clang::DiagnosticOptions(),
                                        new clang::IgnoringDiagConsumer());

    llvm::SmallVector<const char*> clangArgs;
    llvm::for_each(
        clangModuleDep.getBuildArguments(),
        [&](const std::string &Arg) { clangArgs.push_back(Arg.c_str()); });

    bool success = clang::CompilerInvocation::CreateFromArgs(
        depsInvocation, clangArgs, clangDiags);
    (void)success;
    assert(success && "clang option from dep scanner round trip failed");

    // Create a prefix mapper that matches clang's configuration.
    auto Mapper =
        getClangPrefixMapper(clangScanningTool, clangModuleDep, depsInvocation);

    // Clear the cache key for module. The module key is computed from clang
    // invocation, not swift invocation.
    depsInvocation.getFrontendOpts().ModuleCacheKeys.clear();
    depsInvocation.getFrontendOpts().PathPrefixMappings.clear();
    depsInvocation.getFrontendOpts().OutputFile.clear();

    // Reset CASOptions since that should be coming from swift.
    depsInvocation.getCASOpts() = clang::CASOptions();
    depsInvocation.getFrontendOpts().CASIncludeTreeID.clear();

    // FIXME: workaround for rdar://105684525: find the -ivfsoverlay option
    // from clang scanner and pass to swift.
    for (auto overlay : depsInvocation.getHeaderSearchOpts().VFSOverlayFiles) {
      if (llvm::is_contained(ctx.SearchPathOpts.VFSOverlayFiles, overlay))
        continue;
      swiftArgs.push_back("-vfsoverlay");
      swiftArgs.push_back(overlay);
    }

    llvm::BumpPtrAllocator allocator;
    llvm::StringSaver saver(allocator);
    clangArgs.clear();
    depsInvocation.generateCC1CommandLine(
        clangArgs,
        [&saver](const llvm::Twine &T) { return saver.save(T).data(); });

    llvm::for_each(clangArgs, addClangArg);

    // CASFileSystemRootID.
    std::string RootID = clangModuleDep.CASFileSystemRootID
                             ? clangModuleDep.CASFileSystemRootID->toString()
                             : "";

    std::string IncludeTree =
        clangModuleDep.IncludeTreeID ? *clangModuleDep.IncludeTreeID : "";

    ctx.CASOpts.enumerateCASConfigurationFlags(
        [&](StringRef Arg) { swiftArgs.push_back(Arg.str()); });

    if (!RootID.empty()) {
      swiftArgs.push_back("-no-clang-include-tree");
      swiftArgs.push_back("-cas-fs");
      swiftArgs.push_back(RootID);
    }

    if (!IncludeTree.empty()) {
      swiftArgs.push_back("-clang-include-tree-root");
      swiftArgs.push_back(IncludeTree);
    }

    std::string mappedPCMPath = pcmPath;
    if (Mapper)
      Mapper->mapInPlace(mappedPCMPath);

    std::vector<LinkLibrary> LinkLibraries;
    for (const auto &ll : clangModuleDep.LinkLibraries)
      LinkLibraries.push_back(
        {ll.Library,
         ll.IsFramework ? LibraryKind::Framework : LibraryKind::Library});

    // Module-level dependencies.
    llvm::StringSet<> alreadyAddedModules;
    auto dependencies = ModuleDependencyInfo::forClangModule(
        pcmPath, mappedPCMPath, clangModuleDep.ClangModuleMapFile,
        clangModuleDep.ID.ContextHash, swiftArgs, fileDeps, capturedPCMArgs,
        LinkLibraries, RootID, IncludeTree, /*module-cache-key*/ "",
        clangModuleDep.IsSystem);

    std::vector<ModuleDependencyID> directDependencyIDs;
    for (const auto &moduleName : clangModuleDep.ClangModuleDeps) {
      dependencies.addModuleImport(moduleName.ModuleName, &alreadyAddedModules);
      // It is safe to assume that all dependencies of a Clang module are Clang modules.
      directDependencyIDs.push_back({moduleName.ModuleName, ModuleDependencyKind::Clang});
    }
    dependencies.setImportedClangDependencies(directDependencyIDs);
    result.push_back(std::make_pair(ModuleDependencyID{clangModuleDep.ID.ModuleName,
                                                       ModuleDependencyKind::Clang},
                                    dependencies));
  }
  return result;
}

void ClangImporter::recordBridgingHeaderOptions(
    ModuleDependencyInfo &MDI,
    const clang::tooling::dependencies::TranslationUnitDeps &deps) {
  std::vector<std::string> swiftArgs;
  getBridgingHeaderOptions(deps, swiftArgs);
  MDI.updateBridgingHeaderCommandLine(swiftArgs);
}

void ClangImporter::getBridgingHeaderOptions(
    const clang::tooling::dependencies::TranslationUnitDeps &deps,
    std::vector<std::string> &swiftArgs) {
  auto &ctx = Impl.SwiftContext;

  auto addClangArg = [&](Twine arg) {
    swiftArgs.push_back("-Xcc");
    swiftArgs.push_back(arg.str());
  };

  // We are using Swift frontend mode.
  swiftArgs.push_back("-frontend");

  // Swift frontend action: -emit-pcm
  swiftArgs.push_back("-emit-pch");

  // Ensure that the resulting PCM build invocation uses Clang frontend
  // directly
  swiftArgs.push_back("-direct-clang-cc1-module-build");

  // Add args reported by the scanner.

  // Round-trip clang args to canonicalize and clear the options that swift
  // compiler doesn't need.
  clang::CompilerInvocation depsInvocation;
  clang::DiagnosticsEngine clangDiags(new clang::DiagnosticIDs(),
                                      new clang::DiagnosticOptions(),
                                      new clang::IgnoringDiagConsumer());

  llvm::SmallVector<const char *> clangArgs;
  llvm::for_each(deps.Commands[0].Arguments, [&](const std::string &Arg) {
    clangArgs.push_back(Arg.c_str());
  });

  bool success = clang::CompilerInvocation::CreateFromArgs(
      depsInvocation, clangArgs, clangDiags);
  (void)success;
  assert(success && "clang option from dep scanner round trip failed");

  // Clear the cache key for module. The module key is computed from clang
  // invocation, not swift invocation.
  depsInvocation.getFrontendOpts().ProgramAction =
      clang::frontend::ActionKind::GeneratePCH;
  depsInvocation.getFrontendOpts().ModuleCacheKeys.clear();
  depsInvocation.getFrontendOpts().PathPrefixMappings.clear();
  depsInvocation.getFrontendOpts().OutputFile.clear();

  llvm::BumpPtrAllocator allocator;
  llvm::StringSaver saver(allocator);
  clangArgs.clear();
  depsInvocation.generateCC1CommandLine(
      clangArgs,
      [&saver](const llvm::Twine &T) { return saver.save(T).data(); });

  llvm::for_each(clangArgs, addClangArg);

  ctx.CASOpts.enumerateCASConfigurationFlags(
      [&](StringRef Arg) { swiftArgs.push_back(Arg.str()); });

  if (auto Tree = deps.IncludeTreeID) {
    swiftArgs.push_back("-clang-include-tree-root");
    swiftArgs.push_back(*Tree);
  }
  if (auto CASFS = deps.CASFileSystemRootID) {
    swiftArgs.push_back("-no-clang-include-tree");
    swiftArgs.push_back("-cas-fs");
    swiftArgs.push_back(*CASFS);
  }
}

// The Swift compiler does not have a concept of a working directory.
// It is instead handled by the Swift driver by resolving relative paths
// according to the driver's notion of a working directory. On the other hand,
// Clang does have a concept working directory which may be specified on this
// Clang invocation with '-working-directory'. If so, it is crucial that we
// use this directory as an argument to the Clang scanner invocation below.
static std::optional<std::string>
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
      return std::nullopt;
    }
    workingDir = *(clangWorkingDirPos - 1);
  }
  return workingDir;
}

ModuleDependencyVector
ClangImporter::getModuleDependencies(Identifier moduleName,
                                     StringRef moduleOutputPath,
                                     const llvm::DenseSet<clang::tooling::dependencies::ModuleID> &alreadySeenClangModules,
                                     clang::tooling::dependencies::DependencyScanningTool &clangScanningTool,
                                     InterfaceSubContextDelegate &delegate,
                                     llvm::PrefixMapper *mapper,
                                     bool isTestableImport) {
  auto &ctx = Impl.SwiftContext;
  // Determine the command-line arguments for dependency scanning.
  std::vector<std::string> commandLineArgs =
      getClangDepScanningInvocationArguments(ctx);
  auto optionalWorkingDir = computeClangWorkingDirectory(commandLineArgs, ctx);
  if (!optionalWorkingDir) {
    ctx.Diags.diagnose(SourceLoc(), diag::clang_dependency_scan_error,
                       "Missing '-working-directory' argument");
    return {};
  }
  std::string workingDir = *optionalWorkingDir;

  auto lookupModuleOutput =
      [moduleOutputPath](const ModuleID &MID,
                        ModuleOutputKind MOK) -> std::string {
    return moduleCacheRelativeLookupModuleOutput(MID, MOK, moduleOutputPath);
  };

  auto clangModuleDependencies =
      clangScanningTool.getModuleDependencies(
          moduleName.str(), commandLineArgs, workingDir,
          alreadySeenClangModules, lookupModuleOutput);
  if (!clangModuleDependencies) {
    auto errorStr = toString(clangModuleDependencies.takeError());
    // We ignore the "module 'foo' not found" error, the Swift dependency
    // scanner will report such an error only if all of the module loaders
    // fail as well.
    if (errorStr.find("fatal error: module '" + moduleName.str().str() +
                      "' not found") == std::string::npos)
      ctx.Diags.diagnose(SourceLoc(), diag::clang_dependency_scan_error,
                         errorStr);
    return {};
  }

  return bridgeClangModuleDependencies(clangScanningTool,
                                       *clangModuleDependencies,
                                       moduleOutputPath, [&](StringRef path) {
                                         if (mapper)
                                           return mapper->mapToString(path);
                                         return path.str();
                                       });
}

bool ClangImporter::getHeaderDependencies(
    ModuleDependencyID moduleID,
    clang::tooling::dependencies::DependencyScanningTool &clangScanningTool,
    ModuleDependenciesCache &cache,
    ModuleDependencyIDSetVector &headerClangModuleDependencies,
    std::vector<std::string> &headerFileInputs,
    std::vector<std::string> &bridgingHeaderCommandLine,
    std::optional<std::string> &includeTreeID) {
  auto targetModuleInfo = cache.findKnownDependency(moduleID);
  // If we've already recorded bridging header dependencies, we're done.
  if (!targetModuleInfo.getHeaderClangDependencies().empty() ||
      !targetModuleInfo.getHeaderInputSourceFiles().empty())
    return false;

  // Scan the specified textual header file and collect its dependencies
  auto scanHeaderDependencies =
      [&](StringRef headerPath) -> llvm::Expected<TranslationUnitDeps> {
    auto &ctx = Impl.SwiftContext;
    std::vector<std::string> commandLineArgs =
        getClangDepScanningInvocationArguments(ctx, StringRef(headerPath));
    auto optionalWorkingDir =
        computeClangWorkingDirectory(commandLineArgs, ctx);
    if (!optionalWorkingDir) {
      ctx.Diags.diagnose(SourceLoc(), diag::clang_dependency_scan_error,
                         "Missing '-working-directory' argument");
      return llvm::errorCodeToError(
          std::error_code(errno, std::generic_category()));
    }
    std::string workingDir = *optionalWorkingDir;
    auto moduleOutputPath = cache.getModuleOutputPath();
    auto lookupModuleOutput =
        [moduleOutputPath](const ModuleID &MID,
                           ModuleOutputKind MOK) -> std::string {
      return moduleCacheRelativeLookupModuleOutput(MID, MOK, moduleOutputPath);
    };
    auto dependencies = clangScanningTool.getTranslationUnitDependencies(
        commandLineArgs, workingDir, cache.getAlreadySeenClangModules(),
        lookupModuleOutput);
    if (!dependencies)
      return dependencies.takeError();

    // Record module dependencies for each new module we found.
    auto bridgedDeps = bridgeClangModuleDependencies(
        clangScanningTool, dependencies->ModuleGraph,
        cache.getModuleOutputPath(),
        [&cache](StringRef path) {
          return cache.getScanService().remapPath(path);
        });
    cache.recordDependencies(bridgedDeps);

    llvm::copy(dependencies->FileDeps, std::back_inserter(headerFileInputs));
    auto bridgedDependencyIDs =
        llvm::map_range(dependencies->ClangModuleDeps, [](auto &input) {
          return ModuleDependencyID{input.ModuleName, ModuleDependencyKind::Clang};
        });
    headerClangModuleDependencies.insert(bridgedDependencyIDs.begin(),
                                         bridgedDependencyIDs.end());
    return dependencies;
  };

  // - Textual module dependencies require us to process their bridging header.
  // - Binary module dependnecies may have arbitrary header inputs.
  if (targetModuleInfo.isTextualSwiftModule() &&
      !targetModuleInfo.getBridgingHeader()->empty()) {
    auto clangModuleDependencies =
        scanHeaderDependencies(*targetModuleInfo.getBridgingHeader());
    if (!clangModuleDependencies) {
      // FIXME: Route this to a normal diagnostic.
      llvm::logAllUnhandledErrors(clangModuleDependencies.takeError(),
                                  llvm::errs());
      Impl.SwiftContext.Diags.diagnose(
          SourceLoc(), diag::clang_dependency_scan_error,
          "failed to scan bridging header dependencies");
      return true;
    }
    if (auto TreeID = clangModuleDependencies->IncludeTreeID)
      includeTreeID = TreeID;

    getBridgingHeaderOptions(*clangModuleDependencies, bridgingHeaderCommandLine);
  } else if (targetModuleInfo.isSwiftBinaryModule()) {
    auto swiftBinaryDeps = targetModuleInfo.getAsSwiftBinaryModule();
    if (!swiftBinaryDeps->headerImport.empty()) {
      auto clangModuleDependencies = scanHeaderDependencies(swiftBinaryDeps->headerImport);
      if (!clangModuleDependencies)
        return true;
    }
  }

  return false;
}
