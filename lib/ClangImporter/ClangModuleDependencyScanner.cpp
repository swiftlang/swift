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

static void addScannerPrefixMapperInvocationArguments(
    std::vector<std::string> &invocationArgStrs, ASTContext &ctx) {
  for (const auto &arg : ctx.SearchPathOpts.ScannerPrefixMapper) {
    invocationArgStrs.push_back("-fdepscan-prefix-map");
    invocationArgStrs.push_back(arg.first);
    invocationArgStrs.push_back(arg.second);
  }
}

/// Create the command line for Clang dependency scanning.
std::vector<std::string> ClangImporter::getClangDepScanningInvocationArguments(
    ASTContext &ctx) {
  std::vector<std::string> commandLineArgs = getClangDriverArguments(ctx);
  addScannerPrefixMapperInvocationArguments(commandLineArgs, ctx);

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

ModuleDependencyVector ClangImporter::bridgeClangModuleDependencies(
    const ASTContext &ctx,
    clang::tooling::dependencies::DependencyScanningTool &clangScanningTool,
    clang::tooling::dependencies::ModuleDepsGraph &clangDependencies,
    LookupModuleOutputCallback lookupModuleOutput,
    RemapPathCallback callback) {
  ModuleDependencyVector result;

  auto remapPath = [&](StringRef path) {
    if (callback)
      return callback(path);
    return path.str();
  };

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

    auto pcmPath = lookupModuleOutput(clangModuleDep,
                                      ModuleOutputKind::ModuleFile);
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

    // FIXME: workaround for rdar://105684525: find the -ivfsoverlay option
    // from clang scanner and pass to swift.
    if (!ctx.CASOpts.EnableCaching) {
      auto &overlayFiles = invocation.getMutHeaderSearchOpts().VFSOverlayFiles;
      for (auto overlay : overlayFiles) {
        if (llvm::is_contained(ctx.SearchPathOpts.VFSOverlayFiles, overlay))
          continue;
        swiftArgs.push_back("-vfsoverlay");
        swiftArgs.push_back(overlay);
      }
    }

    // Add args reported by the scanner.
    auto clangArgs = invocation.getCC1CommandLine();
    llvm::for_each(clangArgs, addClangArg);

    // CASFileSystemRootID.
    std::string RootID = clangModuleDep.CASFileSystemRootID
                             ? clangModuleDep.CASFileSystemRootID.value()
                             : "";

    std::string IncludeTree =
        clangModuleDep.IncludeTreeID ? *clangModuleDep.IncludeTreeID : "";

    ctx.CASOpts.enumerateCASConfigurationFlags(
        [&](StringRef Arg) { swiftArgs.push_back(Arg.str()); });

    if (!IncludeTree.empty()) {
      swiftArgs.push_back("-clang-include-tree-root");
      swiftArgs.push_back(IncludeTree);
    }
    std::string mappedPCMPath = remapPath(pcmPath);

    std::vector<LinkLibrary> LinkLibraries;
    for (const auto &ll : clangModuleDep.LinkLibraries)
      LinkLibraries.emplace_back(
          ll.Library,
          ll.IsFramework ? LibraryKind::Framework : LibraryKind::Library,
          /*static=*/false);

    // Module-level dependencies.
    llvm::StringSet<> alreadyAddedModules;
    auto dependencies = ModuleDependencyInfo::forClangModule(
        pcmPath, mappedPCMPath, clangModuleDep.ClangModuleMapFile,
        clangModuleDep.ID.ContextHash, swiftArgs, fileDeps,
        LinkLibraries, RootID, IncludeTree, /*module-cache-key*/ "",
        clangModuleDep.IsSystem);

    std::vector<ModuleDependencyID> directDependencyIDs;
    for (const auto &moduleName : clangModuleDep.ClangModuleDeps) {
      // FIXME: This assumes, conservatively, that all Clang module imports
      // are exported. We need to fix this once the clang scanner gains the appropriate
      // API to query this.
      dependencies.addModuleImport(moduleName.ModuleName, /* isExported */ true,
                                   AccessLevel::Public, &alreadyAddedModules);
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

void ClangImporter::getBridgingHeaderOptions(
    const ASTContext &ctx,
    const clang::tooling::dependencies::TranslationUnitDeps &deps,
    std::vector<std::string> &swiftArgs) {
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
  clang::DiagnosticOptions diagOpts;
  clang::DiagnosticsEngine clangDiags(new clang::DiagnosticIDs(), diagOpts,
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
}
