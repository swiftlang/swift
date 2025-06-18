//===--- ScanDependencies.cpp -- Scans the dependencies of a module -------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift-c/DependencyScan/DependencyScan.h"
#include "swift/AST/DiagnosticsCommon.h"
#include "swift/Basic/PrettyStackTrace.h"

#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/DiagnosticsDriver.h"
#include "swift/AST/DiagnosticsFrontend.h"
#include "swift/AST/DiagnosticsSema.h"
#include "swift/AST/FileSystem.h"
#include "swift/AST/Module.h"
#include "swift/AST/ModuleDependencies.h"
#include "swift/AST/ModuleLoader.h"
#include "swift/AST/SourceFile.h"
#include "swift/Basic/Assertions.h"
#include "swift/Basic/Defer.h"
#include "swift/Basic/FileTypes.h"
#include "swift/Basic/LLVM.h"
#include "swift/Basic/STLExtras.h"
#include "swift/ClangImporter/ClangImporter.h"
#include "swift/DependencyScan/DependencyScanImpl.h"
#include "swift/DependencyScan/DependencyScanJSON.h"
#include "swift/DependencyScan/DependencyScanningTool.h"
#include "swift/DependencyScan/ModuleDependencyScanner.h"
#include "swift/DependencyScan/ScanDependencies.h"
#include "swift/DependencyScan/SerializedModuleDependencyCacheFormat.h"
#include "swift/DependencyScan/StringUtils.h"
#include "swift/Frontend/CachingUtils.h"
#include "swift/Frontend/CompileJobCacheKey.h"
#include "swift/Frontend/CompileJobCacheResult.h"
#include "swift/Frontend/Frontend.h"
#include "swift/Frontend/FrontendOptions.h"
#include "swift/Frontend/ModuleInterfaceLoader.h"
#include "swift/Strings.h"
#include "clang/CAS/IncludeTree.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/SetOperations.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/StringSet.h"
#include "llvm/CAS/ActionCache.h"
#include "llvm/CAS/CASReference.h"
#include "llvm/CAS/ObjectStore.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Error.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/StringSaver.h"
#include "llvm/Support/VirtualOutputBackend.h"
#include "llvm/Support/YAMLParser.h"
#include "llvm/Support/YAMLTraits.h"
#include "llvm/Support/raw_ostream.h"
#include <algorithm>
#include <set>
#include <sstream>
#include <stack>
#include <string>

using namespace swift;
using namespace swift::dependencies;
using namespace swift::c_string_utils;
using namespace llvm::yaml;

namespace {

class ExplicitModuleDependencyResolver {
public:
  ExplicitModuleDependencyResolver(
      const ModuleDependencyID &moduleID, ModuleDependenciesCache &cache,
      CompilerInstance &instance, std::optional<SwiftDependencyTracker> tracker)
      : moduleID(moduleID), cache(cache), instance(instance),
        resolvingDepInfo(cache.findKnownDependency(moduleID)),
        tracker(std::move(tracker)) {
    // Copy commandline.
    commandline = resolvingDepInfo.getCommandline();
  }

  // Resolve the dependencies for the current moduleID. Return true on error.
  bool resolve(const std::set<ModuleDependencyID> &dependencies,
               std::optional<std::set<ModuleDependencyID>> bridgingHeaderDeps) {
    // If the dependency is already finalized, nothing needs to be done.
    if (resolvingDepInfo.isFinalized())
      return false;

    for (const auto &depModuleID : dependencies) {
      const auto &depInfo = cache.findKnownDependency(depModuleID);
      switch (depModuleID.Kind) {
      case swift::ModuleDependencyKind::SwiftInterface: {
        auto interfaceDepDetails = depInfo.getAsSwiftInterfaceModule();
        assert(interfaceDepDetails && "Expected Swift Interface dependency.");
        if (handleSwiftInterfaceModuleDependency(depModuleID,
                                                 *interfaceDepDetails))
          return true;
      } break;
      case swift::ModuleDependencyKind::SwiftBinary: {
        auto binaryDepDetails = depInfo.getAsSwiftBinaryModule();
        assert(binaryDepDetails && "Expected Swift Binary Module dependency.");
        if (handleSwiftBinaryModuleDependency(depModuleID, *binaryDepDetails))
          return true;
      } break;
      case swift::ModuleDependencyKind::Clang: {
        auto clangDepDetails = depInfo.getAsClangModule();
        assert(clangDepDetails && "Expected Clang Module dependency.");
        if (handleClangModuleDependency(depModuleID, *clangDepDetails))
          return true;
      } break;
      case swift::ModuleDependencyKind::SwiftSource: {
        auto sourceDepDetails = depInfo.getAsSwiftSourceModule();
        assert(sourceDepDetails && "Expected Swift Source Module dependency.");
        if (handleSwiftSourceModuleDependency(depModuleID, *sourceDepDetails))
          return true;
      } break;
      default:
        llvm_unreachable("Unhandled dependency kind.");
      }
    }

    // Update bridging header build command if there is a bridging header
    // dependency.
    if (addBridgingHeaderDeps(resolvingDepInfo))
      return true;

    if (bridgingHeaderDeps) {
      bridgingHeaderBuildCmd = resolvingDepInfo.getBridgingHeaderCommandline();
      for (auto bridgingDep : *bridgingHeaderDeps) {
        auto &dep = cache.findKnownDependency(bridgingDep);
        auto *clangDep = dep.getAsClangModule();
        assert(clangDep && "wrong module dependency kind");
        if (!clangDep->moduleCacheKey.empty()) {
          bridgingHeaderBuildCmd.push_back("-Xcc");
          bridgingHeaderBuildCmd.push_back("-fmodule-file-cache-key");
          bridgingHeaderBuildCmd.push_back("-Xcc");
          bridgingHeaderBuildCmd.push_back(clangDep->mappedPCMPath);
          bridgingHeaderBuildCmd.push_back("-Xcc");
          bridgingHeaderBuildCmd.push_back(clangDep->moduleCacheKey);
        }
      }
      addDeterministicCheckFlags(bridgingHeaderBuildCmd);
    }

    SwiftInterfaceModuleOutputPathResolution::ResultTy swiftInterfaceOutputPath;
    if (resolvingDepInfo.isSwiftInterfaceModule()) {
      pruneUnusedVFSOverlay(swiftInterfaceOutputPath);
      addSwiftInterfaceModuleOutputPathToCommandLine(swiftInterfaceOutputPath);
    }

    // Update the dependency in the cache with the modified command-line.
    if (resolvingDepInfo.isSwiftInterfaceModule() ||
        resolvingDepInfo.isClangModule()) {
      if (cache.getScanService().hasPathMapping())
        commandline =
            remapPathsFromCommandLine(commandline, [&](StringRef path) {
              return cache.getScanService().remapPath(path);
            });
      addDeterministicCheckFlags(commandline);
    }

    auto dependencyInfoCopy = resolvingDepInfo;
    if (finalize(dependencyInfoCopy, swiftInterfaceOutputPath))
      return true;

    dependencyInfoCopy.setIsFinalized(true);
    cache.updateDependency(moduleID, dependencyInfoCopy);
    return false;
  }

private:
  // Finalize the resolving dependency info.
  bool finalize(ModuleDependencyInfo &depInfo,
                const SwiftInterfaceModuleOutputPathResolution::ResultTy
                    &swiftInterfaceModuleOutputPath) {
    if (resolvingDepInfo.isSwiftInterfaceModule())
      depInfo.setOutputPathAndHash(
          swiftInterfaceModuleOutputPath.outputPath.str().str(),
          swiftInterfaceModuleOutputPath.hash.str());

    // Add macros.
    for (auto &macro : macros)
      depInfo.addMacroDependency(macro.first(), macro.second.LibraryPath,
                                 macro.second.ExecutablePath);

    bool needPathRemapping = instance.getInvocation()
                                 .getSearchPathOptions()
                                 .ResolvedPluginVerification &&
                             cache.getScanService().hasPathMapping();
    auto mapPath = [&](StringRef path) {
      if (!needPathRemapping)
        return path.str();

      return cache.getScanService().remapPath(path);
    };
    if (needPathRemapping)
      commandline.push_back("-resolved-plugin-verification");

    for (auto &macro : depInfo.getMacroDependencies()) {
      std::string arg = mapPath(macro.second.LibraryPath) + "#" +
                        mapPath(macro.second.ExecutablePath) + "#" +
                        macro.first;
      commandline.push_back("-load-resolved-plugin");
      commandline.push_back(arg);
    }

    // Update CAS dependencies.
    if (auto err = collectCASDependencies(depInfo))
      return err;

    if (!bridgingHeaderBuildCmd.empty())
      depInfo.updateBridgingHeaderCommandLine(bridgingHeaderBuildCmd);
    if (!resolvingDepInfo.isSwiftBinaryModule()) {
      depInfo.updateCommandLine(commandline);
      if (updateModuleCacheKey(depInfo))
        return true;
    }

    return false;
  }

  bool handleSwiftInterfaceModuleDependency(
      ModuleDependencyID depModuleID,
      const SwiftInterfaceModuleDependenciesStorage &interfaceDepDetails) {
    if (!resolvingDepInfo.isSwiftSourceModule()) {
      auto &path = interfaceDepDetails.moduleCacheKey.empty()
                       ? interfaceDepDetails.moduleOutputPath
                       : interfaceDepDetails.moduleCacheKey;
      commandline.push_back("-swift-module-file=" + depModuleID.ModuleName +
                            "=" + path);
    }
    addMacroDependencies(depModuleID, interfaceDepDetails);
    return false;
  }

  bool handleSwiftBinaryModuleDependency(
      ModuleDependencyID depModuleID,
      const SwiftBinaryModuleDependencyStorage &binaryDepDetails) {
    if (!resolvingDepInfo.isSwiftSourceModule()) {
      auto &path = binaryDepDetails.moduleCacheKey.empty()
                       ? binaryDepDetails.compiledModulePath
                       : binaryDepDetails.moduleCacheKey;
      commandline.push_back("-swift-module-file=" + depModuleID.ModuleName +
                            "=" + path);
      // If this binary module was built with a header, the header's module
      // dependencies must also specify a .modulemap to the compilation, in
      // order to resolve the header's own header include directives.
      for (const auto &bridgingHeaderDepID :
           binaryDepDetails.headerModuleDependencies) {
        auto optionalBridgingHeaderDepModuleInfo =
            cache.findKnownDependency(bridgingHeaderDepID);
        const auto bridgingHeaderDepModuleDetails =
            optionalBridgingHeaderDepModuleInfo.getAsClangModule();
        commandline.push_back("-Xcc");
        commandline.push_back(
            "-fmodule-map-file=" +
            cache.getScanService().remapPath(
                bridgingHeaderDepModuleDetails->moduleMapFile));
      }
    }
    addMacroDependencies(depModuleID, binaryDepDetails);
    return false;
  }

  bool handleClangModuleDependency(
      ModuleDependencyID depModuleID,
      const ClangModuleDependencyStorage &clangDepDetails) {
    if (!resolvingDepInfo.isSwiftSourceModule()) {
      if (!resolvingDepInfo.isClangModule()) {
        commandline.push_back("-Xcc");
        commandline.push_back("-fmodule-file=" + depModuleID.ModuleName + "=" +
                              clangDepDetails.mappedPCMPath);
      }
      if (!clangDepDetails.moduleCacheKey.empty()) {
        commandline.push_back("-Xcc");
        commandline.push_back("-fmodule-file-cache-key");
        commandline.push_back("-Xcc");
        commandline.push_back(clangDepDetails.mappedPCMPath);
        commandline.push_back("-Xcc");
        commandline.push_back(clangDepDetails.moduleCacheKey);
      }
    }

    // Collect CAS deppendencies from clang modules.
    if (!clangDepDetails.CASClangIncludeTreeRootID.empty()) {
      if (addIncludeTree(clangDepDetails.CASClangIncludeTreeRootID))
        return true;
    }

    collectUsedVFSOverlay(clangDepDetails);

    return false;
  }

  bool handleSwiftSourceModuleDependency(
      ModuleDependencyID depModuleID,
      const SwiftSourceModuleDependenciesStorage &sourceDepDetails) {
    addMacroDependencies(depModuleID, sourceDepDetails);
    return addBridgingHeaderDeps(sourceDepDetails);
  }

  bool addBridgingHeaderDeps(const ModuleDependencyInfo &depInfo) {
    auto sourceDepDetails = depInfo.getAsSwiftSourceModule();
    if (!sourceDepDetails)
      return false;

    return addBridgingHeaderDeps(*sourceDepDetails);
  }

  bool addBridgingHeaderDeps(
      const SwiftSourceModuleDependenciesStorage &sourceDepDetails) {
    if (sourceDepDetails.textualModuleDetails.CASBridgingHeaderIncludeTreeRootID
            .empty()) {
      if (!sourceDepDetails.textualModuleDetails.bridgingSourceFiles.empty()) {
        if (tracker) {
          tracker->startTracking(/*includeCommonDeps*/ false);
          for (auto &file :
               sourceDepDetails.textualModuleDetails.bridgingSourceFiles)
            tracker->trackFile(file);
          auto bridgeRoot = tracker->createTreeFromDependencies();
          if (!bridgeRoot)
            return diagnoseCASFSCreationError(bridgeRoot.takeError());

          fileListRefs.push_back(bridgeRoot->getRef());
        }
      }
    } else if (addIncludeTree(sourceDepDetails.textualModuleDetails
                                  .CASBridgingHeaderIncludeTreeRootID))
      return true;

    return false;
  };

  void addMacroDependencies(ModuleDependencyID moduleID,
                            const ModuleDependencyInfoStorageBase &dep) {
    auto directDeps = cache.getAllDependencies(this->moduleID);
    if (llvm::find(directDeps, moduleID) == directDeps.end())
      return;

    for (auto &entry : dep.macroDependencies)
      macros.insert({entry.first,
                     {entry.second.LibraryPath, entry.second.ExecutablePath}});
  }

  static bool isVFSOverlayFlag(StringRef arg) {
    return arg == "-ivfsoverlay" || arg == "-vfsoverlay";
  };
  static bool isXCCArg(StringRef arg) { return arg == "-Xcc"; };

  void
  collectUsedVFSOverlay(const ClangModuleDependencyStorage &clangDepDetails) {
    // true if the previous argument was the dash-option of an option pair
    bool getNext = false;
    for (const auto &A : clangDepDetails.buildCommandLine) {
      StringRef arg(A);
      if (isXCCArg(arg))
        continue;
      if (getNext) {
        getNext = false;
        usedVFSOverlayPaths.insert(arg);
      } else if (isVFSOverlayFlag(arg))
        getNext = true;
    }
  }

  void pruneUnusedVFSOverlay(
      SwiftInterfaceModuleOutputPathResolution::ResultTy &outputPath) {
    // Pruning of unused VFS overlay options for Clang dependencies is performed
    // by the Clang dependency scanner.
    if (moduleID.Kind == ModuleDependencyKind::Clang)
      return;

    // Prune the command line.
    std::vector<std::string> resolvedCommandLine;
    size_t skip = 0;
    for (auto it = commandline.begin(), end = commandline.end(); it != end;
         it++) {
      if (skip) {
        skip--;
        continue;
      }
      // If this VFS overlay was not used across any of the dependencies, skip
      // it.
      if ((it + 1) != end && isXCCArg(*it) && isVFSOverlayFlag(*(it + 1))) {
        assert(it + 2 != end); // Extra -Xcc
        assert(it + 3 != end); // Actual VFS overlay path argument
        if (!usedVFSOverlayPaths.contains(*(it + 3))) {
          skip = 3;
          continue;
        }
      }
      resolvedCommandLine.push_back(*it);
    }

    commandline = std::move(resolvedCommandLine);

    // Prune the clang impoter options. We do not need to deal with -Xcc because
    // these are clang options.
    const auto &CI = instance.getInvocation();

    SwiftInterfaceModuleOutputPathResolution::ArgListTy extraArgsList;
    const auto &clangImporterOptions =
        CI.getClangImporterOptions()
            .getReducedExtraArgsForSwiftModuleDependency();

    skip = 0;
    for (auto it = clangImporterOptions.begin(),
              end = clangImporterOptions.end();
         it != end; it++) {
      if (skip) {
        skip = 0;
        continue;
      }

      if ((it + 1) != end && isVFSOverlayFlag(*it)) {
        if (!usedVFSOverlayPaths.contains(*(it + 1))) {
          skip = 1;
          continue;
        }
      }

      extraArgsList.push_back(*it);
    }

    auto swiftTextualDeps = resolvingDepInfo.getAsSwiftInterfaceModule();
    auto &interfacePath = swiftTextualDeps->swiftInterfaceFile;
    auto sdkPath = instance.getASTContext().SearchPathOpts.getSDKPath();
    SwiftInterfaceModuleOutputPathResolution::setOutputPath(
        outputPath, moduleID.ModuleName, interfacePath, sdkPath, CI,
        extraArgsList);

    return;
  }

  void addSwiftInterfaceModuleOutputPathToCommandLine(
      const SwiftInterfaceModuleOutputPathResolution::ResultTy &outputPath) {
    StringRef outputName = outputPath.outputPath.str();

    commandline.push_back("-o");
    commandline.push_back(outputName.str());

    return;
  }

  bool collectCASDependencies(ModuleDependencyInfo &dependencyInfoCopy) {
    if (!instance.getInvocation().getCASOptions().EnableCaching)
      return false;

    // Collect CAS info from current resolving module.
    if (auto *sourceDep = resolvingDepInfo.getAsSwiftSourceModule()) {
      tracker->startTracking();
      llvm::for_each(sourceDep->sourceFiles, [this](const std::string &file) {
        tracker->trackFile(file);
      });
      llvm::for_each(
          sourceDep->auxiliaryFiles,
          [this](const std::string &file) { tracker->trackFile(file); });
      llvm::for_each(dependencyInfoCopy.getMacroDependencies(),
                     [this](const auto &entry) {
                       tracker->trackFile(entry.second.LibraryPath);
                     });
      auto root = tracker->createTreeFromDependencies();
      if (!root)
        return diagnoseCASFSCreationError(root.takeError());
      fileListRefs.push_back(root->getRef());
    } else if (auto *textualDep =
                   resolvingDepInfo.getAsSwiftInterfaceModule()) {
      tracker->startTracking();
      tracker->trackFile(textualDep->swiftInterfaceFile);
      llvm::for_each(
          textualDep->auxiliaryFiles,
          [this](const std::string &file) { tracker->trackFile(file); });
      llvm::for_each(dependencyInfoCopy.getMacroDependencies(),
                     [this](const auto &entry) {
                       tracker->trackFile(entry.second.LibraryPath);
                     });
      auto root = tracker->createTreeFromDependencies();
      if (!root)
        return diagnoseCASFSCreationError(root.takeError());
      fileListRefs.push_back(root->getRef());
    }

    // Update build command line.
    if (resolvingDepInfo.isSwiftInterfaceModule() ||
        resolvingDepInfo.isSwiftSourceModule()) {
      // Update with casfs option.
      if (computeCASFileSystem(dependencyInfoCopy))
        return true;
    }

    // Compute and update module cache key.
    if (auto *binaryDep = dependencyInfoCopy.getAsSwiftBinaryModule()) {
      if (setupBinaryCacheKey(binaryDep->compiledModulePath,
                              dependencyInfoCopy))
        return true;
    }
    return false;
  }

  bool updateModuleCacheKey(ModuleDependencyInfo &depInfo) {
    if (!instance.getInvocation().getCASOptions().EnableCaching)
      return false;

    auto &CAS = cache.getScanService().getCAS();
    auto commandLine = depInfo.getCommandline();
    std::vector<const char *> Args;
    if (commandLine.size() > 1)
      for (auto &c : ArrayRef<std::string>(commandLine).drop_front(1))
        Args.push_back(c.c_str());

    auto base = createCompileJobBaseCacheKey(CAS, Args);
    if (!base) {
      instance.getDiags().diagnose(SourceLoc(), diag::error_cache_key_creation,
                                   moduleID.ModuleName,
                                   toString(base.takeError()));
      return true;
    }

    // Module compilation commands always have only one input and the input
    // index is always 0.
    auto key = createCompileJobCacheKeyForOutput(CAS, *base, /*InputIndex=*/0);
    if (!key) {
      instance.getDiags().diagnose(SourceLoc(), diag::error_cache_key_creation,
                                   moduleID.ModuleName,
                                   toString(key.takeError()));
      return true;
    }

    depInfo.updateModuleCacheKey(CAS.getID(*key).toString());
    return false;
  }

  bool setupBinaryCacheKey(StringRef path, ModuleDependencyInfo &depInfo) {
    auto &CASFS = cache.getScanService().getSharedCachingFS();
    auto &CAS = cache.getScanService().getCAS();
    // For binary module, we need to make sure the lookup key is setup here in
    // action cache. We just use the CASID of the binary module itself as key.
    auto Ref = CASFS.getObjectRefForFileContent(path);
    if (!Ref) {
      instance.getDiags().diagnose(SourceLoc(), diag::error_cas_file_ref, path);
      return true;
    }
    assert(*Ref && "Binary module should be loaded into CASFS already");
    depInfo.updateModuleCacheKey(CAS.getID(**Ref).toString());

    swift::cas::CompileJobCacheResult::Builder Builder;
    Builder.addOutput(file_types::ID::TY_SwiftModuleFile, **Ref);
    auto Result = Builder.build(CAS);
    if (!Result) {
      instance.getDiags().diagnose(SourceLoc(), diag::error_cas,
                                   "adding binary module dependencies",
                                   toString(Result.takeError()));
      return true;
    }
    if (auto E = instance.getActionCache().put(CAS.getID(**Ref),
                                               CAS.getID(*Result))) {
      instance.getDiags().diagnose(
          SourceLoc(), diag::error_cas,
          "adding binary module dependencies cache entry",
          toString(std::move(E)));
      return true;
    }
    return false;
  }

  bool diagnoseCASFSCreationError(llvm::Error err) {
    if (!err)
      return false;

    instance.getDiags().diagnose(SourceLoc(), diag::error_cas_fs_creation,
                                 toString(std::move(err)));
    return true;
  }

  void addDeterministicCheckFlags(std::vector<std::string> &cmd) {
    // Propagate the deterministic check to explicit built module command.
    if (!instance.getInvocation().getFrontendOptions().DeterministicCheck)
      return;
    cmd.push_back("-enable-deterministic-check");
    cmd.push_back("-always-compile-output-files");
    // disable cache replay because that defeat the purpose of the check.
    if (instance.getInvocation().getCASOptions().EnableCaching)
      cmd.push_back("-cache-disable-replay");
  }

  bool addIncludeTree(StringRef includeTree) {
    auto &db = cache.getScanService().getCAS();
    auto casID = db.parseID(includeTree);
    if (!casID) {
      instance.getDiags().diagnose(SourceLoc(), diag::error_invalid_cas_id,
                                   includeTree, toString(casID.takeError()));
      return true;
    }
    auto ref = db.getReference(*casID);
    if (!ref) {
      instance.getDiags().diagnose(SourceLoc(), diag::error_load_input_from_cas,
                                   includeTree);
      return true;
    }

    auto root = clang::cas::IncludeTreeRoot::get(db, *ref);
    if (!root) {
      instance.getDiags().diagnose(SourceLoc(), diag::error_cas_malformed_input,
                                   includeTree, toString(root.takeError()));
      return true;
    }

    fileListRefs.push_back(root->getFileListRef());
    return false;
  }

  bool computeCASFileSystem(ModuleDependencyInfo &dependencyInfoCopy) {
    if (fileListRefs.empty())
      return false;

    auto &db = cache.getScanService().getCAS();
    auto casFS =
        clang::cas::IncludeTree::FileList::create(db, {}, fileListRefs);
    if (!casFS) {
      instance.getDiags().diagnose(SourceLoc(), diag::error_cas,
                                   "CAS IncludeTree FileList creation",
                                   toString(casFS.takeError()));
      return true;
    }

    auto casID = casFS->getID().toString();
    dependencyInfoCopy.updateCASFileSystemRootID(casID);
    commandline.push_back("-clang-include-tree-filelist");
    commandline.push_back(casID);
    return false;
  }

private:
  const ModuleDependencyID &moduleID;
  ModuleDependenciesCache &cache;
  CompilerInstance &instance;
  const ModuleDependencyInfo &resolvingDepInfo;

  std::optional<SwiftDependencyTracker> tracker;
  std::vector<llvm::cas::ObjectRef> fileListRefs;
  std::vector<std::string> commandline;
  std::vector<std::string> bridgingHeaderBuildCmd;
  llvm::StringMap<MacroPluginDependency> macros;
  llvm::StringSet<> usedVFSOverlayPaths;
};

static bool resolveExplicitModuleInputs(
    const ModuleDependencyID &moduleID,
    const std::set<ModuleDependencyID> &dependencies,
    ModuleDependenciesCache &cache, CompilerInstance &instance,
    std::optional<std::set<ModuleDependencyID>> bridgingHeaderDeps,
    std::optional<SwiftDependencyTracker> tracker) {
  ExplicitModuleDependencyResolver resolver(moduleID, cache, instance,
                                            std::move(tracker));
  return resolver.resolve(dependencies, bridgingHeaderDeps);
}

static bool writePrescanJSONToOutput(DiagnosticEngine &diags,
                                     llvm::vfs::OutputBackend &backend,
                                     StringRef path,
                                     swiftscan_import_set_t importSet) {
  return withOutputPath(diags, backend, path, [&](llvm::raw_pwrite_stream &os) {
    writePrescanJSON(os, importSet);
    return false;
  });
}

static bool writeJSONToOutput(DiagnosticEngine &diags,
                              llvm::vfs::OutputBackend &backend, StringRef path,
                              swiftscan_dependency_graph_t dependencies) {
  return withOutputPath(diags, backend, path, [&](llvm::raw_pwrite_stream &os) {
    writeJSON(os, dependencies);
    return false;
  });
}

static void
bridgeDependencyIDs(const ArrayRef<ModuleDependencyID> dependencies,
                    std::vector<std::string> &bridgedDependencyNames) {
  for (const auto &dep : dependencies) {
    std::string dependencyKindAndName;
    switch (dep.Kind) {
    case ModuleDependencyKind::SwiftInterface:
    case ModuleDependencyKind::SwiftSource:
      dependencyKindAndName = "swiftTextual";
      break;
    case ModuleDependencyKind::SwiftBinary:
      dependencyKindAndName = "swiftBinary";
      break;
    case ModuleDependencyKind::Clang:
      dependencyKindAndName = "clang";
      break;
    default:
      llvm_unreachable("Unhandled dependency kind.");
    }
    dependencyKindAndName += ":";
    dependencyKindAndName += dep.ModuleName;
    bridgedDependencyNames.push_back(dependencyKindAndName);
  }
}

static swiftscan_macro_dependency_set_t *createMacroDependencySet(
    const std::map<std::string, MacroPluginDependency> &macroDeps) {
  if (macroDeps.empty())
    return nullptr;

  swiftscan_macro_dependency_set_t *set = new swiftscan_macro_dependency_set_t;
  set->count = macroDeps.size();
  set->macro_dependencies = new swiftscan_macro_dependency_t[set->count];
  unsigned SI = 0;
  for (auto &entry : macroDeps) {
    set->macro_dependencies[SI] = new swiftscan_macro_dependency_s;
    set->macro_dependencies[SI]->module_name =
        create_clone(entry.first.c_str());
    set->macro_dependencies[SI]->library_path =
        create_clone(entry.second.LibraryPath.c_str());
    set->macro_dependencies[SI]->executable_path =
        create_clone(entry.second.ExecutablePath.c_str());
    ++SI;
  }
  return set;
}

static swiftscan_dependency_graph_t generateFullDependencyGraph(
    const CompilerInstance &instance,
    const DependencyScanDiagnosticCollector *diagnosticCollector,
    const ModuleDependenciesCache &cache,
    const ArrayRef<ModuleDependencyID> allModules) {
  if (allModules.empty()) {
    return nullptr;
  }

  std::string mainModuleName = allModules.front().ModuleName;
  swiftscan_dependency_set_t *dependencySet = new swiftscan_dependency_set_t;
  dependencySet->count = allModules.size();
  dependencySet->modules =
      new swiftscan_dependency_info_t[dependencySet->count];

  for (size_t i = 0; i < allModules.size(); ++i) {
    const auto &moduleID = allModules[i];
    auto &moduleDependencyInfo = cache.findKnownDependency(moduleID);
    // Collect all the required pieces to build a ModuleInfo
    auto swiftTextualDeps = moduleDependencyInfo.getAsSwiftInterfaceModule();
    auto swiftSourceDeps = moduleDependencyInfo.getAsSwiftSourceModule();
    auto swiftBinaryDeps = moduleDependencyInfo.getAsSwiftBinaryModule();
    auto clangDeps = moduleDependencyInfo.getAsClangModule();

    // ModulePath
    const char *modulePathSuffix =
        moduleDependencyInfo.isSwiftModule() ? ".swiftmodule" : ".pcm";
    std::string modulePath;
    if (swiftTextualDeps)
      modulePath = swiftTextualDeps->moduleOutputPath;
    else if (swiftBinaryDeps)
      modulePath = swiftBinaryDeps->compiledModulePath;
    else if (clangDeps)
      modulePath = clangDeps->pcmOutputPath;
    else
      modulePath = moduleID.ModuleName + modulePathSuffix;

    // SourceFiles
    std::vector<std::string> sourceFiles;
    if (swiftSourceDeps) {
      sourceFiles = swiftSourceDeps->sourceFiles;
    } else if (clangDeps) {
      sourceFiles = clangDeps->fileDependencies;
    }

    auto directDependencies = cache.getAllDependencies(moduleID);
    std::vector<std::string> clangHeaderDependencyNames;
    for (const auto &headerDepID :
         moduleDependencyInfo.getHeaderClangDependencies())
      clangHeaderDependencyNames.push_back(headerDepID.ModuleName);

    // Generate a swiftscan_clang_details_t object based on the dependency kind
    auto getModuleDetails = [&]() -> swiftscan_module_details_t {
      swiftscan_module_details_s *details = new swiftscan_module_details_s;
      if (swiftTextualDeps) {
        swiftscan_string_ref_t moduleInterfacePath =
            create_clone(swiftTextualDeps->swiftInterfaceFile.c_str());
        swiftscan_string_ref_t bridgingHeaderPath =
            swiftTextualDeps->textualModuleDetails.bridgingHeaderFile
                    .has_value()
                ? create_clone(
                      swiftTextualDeps->textualModuleDetails.bridgingHeaderFile
                          .value()
                          .c_str())
                : create_null();
        details->kind = SWIFTSCAN_DEPENDENCY_INFO_SWIFT_TEXTUAL;
        // Create an overlay dependencies set according to the output format
        std::vector<std::string> bridgedOverlayDependencyNames;
        bridgeDependencyIDs(swiftTextualDeps->swiftOverlayDependencies,
                            bridgedOverlayDependencyNames);

        details->swift_textual_details = {
            moduleInterfacePath,
            create_set(swiftTextualDeps->compiledModuleCandidates),
            bridgingHeaderPath,
            create_set(
                swiftTextualDeps->textualModuleDetails.bridgingSourceFiles),
            create_set(clangHeaderDependencyNames),
            create_set(bridgedOverlayDependencyNames),
            /*sourceImportedDependencies*/ create_set({}),
            create_set(swiftTextualDeps->textualModuleDetails.buildCommandLine),
            /*bridgingHeaderBuildCommand*/ create_set({}),
            create_clone(swiftTextualDeps->contextHash.c_str()),
            swiftTextualDeps->isFramework,
            swiftTextualDeps->isStatic,
            create_clone(swiftTextualDeps->textualModuleDetails
                             .CASFileSystemRootID.c_str()),
            create_clone(swiftTextualDeps->textualModuleDetails
                             .CASBridgingHeaderIncludeTreeRootID.c_str()),
            create_clone(swiftTextualDeps->moduleCacheKey.c_str()),
            createMacroDependencySet(swiftTextualDeps->macroDependencies),
            create_clone(swiftTextualDeps->userModuleVersion.c_str()),
            /*chained_bridging_header_path=*/create_clone(""),
            /*chained_bridging_header_content=*/create_clone("")};
      } else if (swiftSourceDeps) {
        swiftscan_string_ref_t moduleInterfacePath = create_null();
        swiftscan_string_ref_t bridgingHeaderPath =
            swiftSourceDeps->textualModuleDetails.bridgingHeaderFile.has_value()
                ? create_clone(
                      swiftSourceDeps->textualModuleDetails.bridgingHeaderFile
                          .value()
                          .c_str())
                : create_null();
        details->kind = SWIFTSCAN_DEPENDENCY_INFO_SWIFT_TEXTUAL;
        // Create an overlay dependencies set according to the output format
        std::vector<std::string> bridgedOverlayDependencyNames;
        bridgeDependencyIDs(swiftSourceDeps->swiftOverlayDependencies,
                            bridgedOverlayDependencyNames);

        // Create a set of directly-source-imported dependencies
        std::vector<ModuleDependencyID> sourceImportDependencies;
        std::copy(swiftSourceDeps->importedSwiftModules.begin(),
                  swiftSourceDeps->importedSwiftModules.end(),
                  std::back_inserter(sourceImportDependencies));
        std::copy(swiftSourceDeps->importedClangModules.begin(),
                  swiftSourceDeps->importedClangModules.end(),
                  std::back_inserter(sourceImportDependencies));
        std::vector<std::string> bridgedSourceImportedDependencyNames;
        bridgeDependencyIDs(sourceImportDependencies,
                            bridgedSourceImportedDependencyNames);

        details->swift_textual_details = {
            moduleInterfacePath, create_empty_set(), bridgingHeaderPath,
            create_set(
                swiftSourceDeps->textualModuleDetails.bridgingSourceFiles),
            create_set(clangHeaderDependencyNames),
            create_set(bridgedOverlayDependencyNames),
            create_set(bridgedSourceImportedDependencyNames),
            create_set(swiftSourceDeps->textualModuleDetails.buildCommandLine),
            create_set(swiftSourceDeps->bridgingHeaderBuildCommandLine),
            /*contextHash*/
            create_clone(
                instance.getInvocation().getModuleScanningHash().c_str()),
            /*isFramework*/ false,
            /*isStatic*/ false,
            /*CASFS*/
            create_clone(swiftSourceDeps->textualModuleDetails
                             .CASFileSystemRootID.c_str()),
            /*IncludeTree*/
            create_clone(swiftSourceDeps->textualModuleDetails
                             .CASBridgingHeaderIncludeTreeRootID.c_str()),
            /*CacheKey*/ create_clone(""),
            createMacroDependencySet(swiftSourceDeps->macroDependencies),
            /*userModuleVersion*/ create_clone(""),
            create_clone(swiftSourceDeps->chainedBridgingHeaderPath.c_str()),
            create_clone(
                swiftSourceDeps->chainedBridgingHeaderContent.c_str())};
      } else if (swiftBinaryDeps) {
        details->kind = SWIFTSCAN_DEPENDENCY_INFO_SWIFT_BINARY;
        // Create an overlay dependencies set according to the output format
        std::vector<std::string> bridgedOverlayDependencyNames;
        bridgeDependencyIDs(swiftBinaryDeps->swiftOverlayDependencies,
                            bridgedOverlayDependencyNames);
        details->swift_binary_details = {
            create_clone(swiftBinaryDeps->compiledModulePath.c_str()),
            create_clone(swiftBinaryDeps->moduleDocPath.c_str()),
            create_clone(swiftBinaryDeps->sourceInfoPath.c_str()),
            create_set(bridgedOverlayDependencyNames),
            create_clone(swiftBinaryDeps->headerImport.c_str()),
            create_set(clangHeaderDependencyNames),
            create_set(swiftBinaryDeps->headerSourceFiles),
            swiftBinaryDeps->isFramework,
            swiftBinaryDeps->isStatic,
            createMacroDependencySet(swiftBinaryDeps->macroDependencies),
            create_clone(swiftBinaryDeps->moduleCacheKey.c_str()),
            create_clone(swiftBinaryDeps->userModuleVersion.c_str())};
      } else {
        // Clang module details
        details->kind = SWIFTSCAN_DEPENDENCY_INFO_CLANG;
        details->clang_details = {
            create_clone(clangDeps->moduleMapFile.c_str()),
            create_clone(clangDeps->contextHash.c_str()),
            create_set(clangDeps->buildCommandLine),
            create_clone(clangDeps->CASFileSystemRootID.c_str()),
            create_clone(clangDeps->CASClangIncludeTreeRootID.c_str()),
            create_clone(clangDeps->moduleCacheKey.c_str())};
      }
      return details;
    };

    swiftscan_dependency_info_s *moduleInfo = new swiftscan_dependency_info_s;
    dependencySet->modules[i] = moduleInfo;

    std::string encodedModuleName = createEncodedModuleKindAndName(moduleID);
    auto ttt = create_clone(encodedModuleName.c_str());
    moduleInfo->module_name = ttt;
    moduleInfo->module_path = create_clone(modulePath.c_str());
    moduleInfo->source_files = create_set(sourceFiles);

    // Create a direct dependencies set according to the output format
    std::vector<std::string> bridgedDependencyNames;
    bridgeDependencyIDs(directDependencies.getArrayRef(),
                        bridgedDependencyNames);
    moduleInfo->direct_dependencies = create_set(bridgedDependencyNames);
    moduleInfo->details = getModuleDetails();

    // Create a link libraries set for this module
    auto linkLibraries = moduleDependencyInfo.getLinkLibraries();
    swiftscan_link_library_set_t *linkLibrarySet =
        new swiftscan_link_library_set_t;
    linkLibrarySet->count = linkLibraries.size();
    linkLibrarySet->link_libraries =
        new swiftscan_link_library_info_t[linkLibrarySet->count];
    for (size_t i = 0; i < linkLibraries.size(); ++i) {
      const auto &ll = linkLibraries[i];
      swiftscan_link_library_info_s *llInfo = new swiftscan_link_library_info_s;
      llInfo->name = create_clone(ll.getName().str().c_str());
      llInfo->isStatic = ll.isStaticLibrary();
      llInfo->isFramework = ll.getKind() == LibraryKind::Framework;
      llInfo->forceLoad = ll.shouldForceLoad();
      linkLibrarySet->link_libraries[i] = llInfo;
    }
    moduleInfo->link_libraries = linkLibrarySet;

    // Create source import infos set for this module
    auto imports = moduleDependencyInfo.getModuleImports();
    swiftscan_import_info_set_t *importInfoSet =
        new swiftscan_import_info_set_t;
    importInfoSet->count = imports.size();
    importInfoSet->imports = new swiftscan_import_info_t[importInfoSet->count];
    for (size_t i = 0; i < imports.size(); ++i) {
      const auto &ii = imports[i];
      swiftscan_import_info_s *iInfo = new swiftscan_import_info_s;
      iInfo->import_identifier = create_clone(ii.importIdentifier.c_str());
      iInfo->access_level =
          static_cast<swiftscan_access_level_t>(ii.accessLevel);

      const auto &sourceLocations = ii.importLocations;
      swiftscan_source_location_set_t *sourceLocSet =
          new swiftscan_source_location_set_t;
      sourceLocSet->count = sourceLocations.size();
      sourceLocSet->source_locations =
          new swiftscan_source_location_t[sourceLocSet->count];
      for (size_t j = 0; j < sourceLocations.size(); ++j) {
        const auto &sl = sourceLocations[j];
        swiftscan_source_location_s *slInfo = new swiftscan_source_location_s;
        slInfo->buffer_identifier = create_clone(sl.bufferIdentifier.c_str());
        slInfo->line_number = sl.lineNumber;
        slInfo->column_number = sl.columnNumber;
        sourceLocSet->source_locations[j] = slInfo;
      }
      iInfo->source_locations = sourceLocSet;
      importInfoSet->imports[i] = iInfo;
    }
    moduleInfo->imports = importInfoSet;
  }

  swiftscan_dependency_graph_t result = new swiftscan_dependency_graph_s;
  result->main_module_name = create_clone(mainModuleName.c_str());
  result->dependencies = dependencySet;
  result->diagnostics =
      diagnosticCollector
          ? mapCollectedDiagnosticsForOutput(diagnosticCollector)
          : nullptr;
  return result;
}

/// Implements a topological sort via recursion and reverse postorder DFS.
/// Does not bother handling cycles, relying on a DAG guarantee by the client.
static std::vector<ModuleDependencyID>
computeTopologicalSortOfExplicitDependencies(
    const std::vector<ModuleDependencyID> &allModules,
    const ModuleDependenciesCache &cache) {
  std::unordered_set<ModuleDependencyID> visited;
  std::vector<ModuleDependencyID> result;
  std::stack<ModuleDependencyID> stack;

  // Must be explicitly-typed to allow recursion
  std::function<void(const ModuleDependencyID &)> visit;
  visit = [&visit, &cache, &visited, &result,
           &stack](const ModuleDependencyID &moduleID) {
    // Mark this node as visited -- we are done if it already was.
    if (!visited.insert(moduleID).second)
      return;

    // Otherwise, visit each adjacent node.
    for (const auto &succID : cache.getAllDependencies(moduleID)) {
      // We don't worry if successor is already in this current stack,
      // since that would mean we have found a cycle, which should not
      // be possible because we checked for cycles earlier.
      stack.push(succID);
      visit(succID);
      auto top = stack.top();
      stack.pop();
      assert(top == succID);
    }

    // Add to the result.
    result.push_back(moduleID);
  };

  for (const auto &modID : allModules) {
    assert(stack.empty());
    stack.push(modID);
    visit(modID);
    auto top = stack.top();
    stack.pop();
    assert(top == modID);
  }

  std::reverse(result.begin(), result.end());
  return result;
}

/// For each module in the graph, compute a set of all its dependencies,
/// direct *and* transitive.
static std::unordered_map<ModuleDependencyID, std::set<ModuleDependencyID>>
computeTransitiveClosureOfExplicitDependencies(
    const std::vector<ModuleDependencyID> &topologicallySortedModuleList,
    const ModuleDependenciesCache &cache) {
  // The usage of an ordered ::set is important to ensure the
  // dependencies are listed in a deterministic order.
  std::unordered_map<ModuleDependencyID, std::set<ModuleDependencyID>> result;
  for (const auto &modID : topologicallySortedModuleList)
    result[modID] = {modID};

  // Traverse the set of modules in reverse topological order, assimilating
  // transitive closures
  for (auto it = topologicallySortedModuleList.rbegin(),
            end = topologicallySortedModuleList.rend();
       it != end; ++it) {
    const auto &modID = *it;
    auto &modReachableSet = result[modID];
    for (const auto &succID : cache.getAllDependencies(modID)) {
      const auto &succReachableSet = result[succID];
      llvm::set_union(modReachableSet, succReachableSet);
    }
  }
  // For ease of use down-the-line, remove the node's self from its set of
  // reachable nodes
  for (const auto &modID : topologicallySortedModuleList)
    result[modID].erase(modID);

  return result;
}

static std::set<ModuleDependencyID> computeBridgingHeaderTransitiveDependencies(
    const ModuleDependencyInfo &dep,
    const std::unordered_map<ModuleDependencyID, std::set<ModuleDependencyID>>
        &transitiveClosures,
    const ModuleDependenciesCache &cache) {
  std::set<ModuleDependencyID> result;
  if (!dep.isSwiftSourceModule())
    return result;

  for (auto &depID : dep.getHeaderClangDependencies()) {
    result.insert(depID);
    auto succDeps = transitiveClosures.find(depID);
    assert(succDeps != transitiveClosures.end() && "unknown dependency");
    llvm::set_union(result, succDeps->second);
  }

  return result;
}

static std::vector<ModuleDependencyID>
findClangDepPath(const ModuleDependencyID &from, const ModuleDependencyID &to,
                 const ModuleDependenciesCache &cache) {
  std::unordered_set<ModuleDependencyID> visited;
  std::vector<ModuleDependencyID> result;
  std::stack<ModuleDependencyID, std::vector<ModuleDependencyID>> stack;

  // Must be explicitly-typed to allow recursion
  std::function<void(const ModuleDependencyID &)> visit;

  visit = [&visit, &cache, &visited, &result, &stack,
           to](const ModuleDependencyID &moduleID) {
    if (!visited.insert(moduleID).second)
      return;

    if (moduleID == to) {
      // Copy stack contents to the result
      auto end = &stack.top() + 1;
      auto begin = end - stack.size();
      result.assign(begin, end);
      return;
    }

    // Otherwise, visit each child node.
    for (const auto &succID : cache.getAllDependencies(moduleID)) {
      stack.push(succID);
      visit(succID);
      stack.pop();
    }
  };

  stack.push(from);
  visit(from);
  return result;
}

static bool diagnoseCycle(const CompilerInstance &instance,
                          const ModuleDependenciesCache &cache,
                          ModuleDependencyID mainId) {
  ModuleDependencyIDSetVector openSet;
  ModuleDependencyIDSetVector closeSet;

  auto kindIsSwiftDependency = [&](const ModuleDependencyID &ID) {
    return ID.Kind == swift::ModuleDependencyKind::SwiftInterface ||
           ID.Kind == swift::ModuleDependencyKind::SwiftBinary ||
           ID.Kind == swift::ModuleDependencyKind::SwiftSource;
  };

  auto emitModulePath = [&](const std::vector<ModuleDependencyID> path,
                            llvm::SmallString<64> &buffer) {
    llvm::interleave(
        path,
        [&buffer](const ModuleDependencyID &id) {
          buffer.append(id.ModuleName);
          switch (id.Kind) {
          case swift::ModuleDependencyKind::SwiftSource:
            buffer.append(" (Source Target)");
            break;
          case swift::ModuleDependencyKind::SwiftInterface:
            buffer.append(".swiftinterface");
            break;
          case swift::ModuleDependencyKind::SwiftBinary:
            buffer.append(".swiftmodule");
            break;
          case swift::ModuleDependencyKind::Clang:
            buffer.append(".pcm");
            break;
          default:
            llvm::report_fatal_error(
                Twine("Invalid Module Dependency Kind in cycle: ") +
                id.ModuleName);
            break;
          }
        },
        [&buffer] { buffer.append(" -> "); });
  };

  auto emitCycleDiagnostic = [&](const ModuleDependencyID &sourceId,
                                 const ModuleDependencyID &sinkId) {
    auto startIt = std::find(openSet.begin(), openSet.end(), sourceId);
    assert(startIt != openSet.end());
    std::vector<ModuleDependencyID> cycleNodes(startIt, openSet.end());
    cycleNodes.push_back(sinkId);
    llvm::SmallString<64> errorBuffer;
    emitModulePath(cycleNodes, errorBuffer);
    instance.getASTContext().Diags.diagnose(
        SourceLoc(), diag::scanner_find_cycle, errorBuffer.str());

    // TODO: for (std::tuple<const ModuleDependencyID&, const
    // ModuleDependencyID&> v : cycleNodes | std::views::adjacent<2>)
    for (auto it = cycleNodes.begin(), end = cycleNodes.end(); it != end;
         it++) {
      if (it + 1 == cycleNodes.end())
        continue;

      const auto &thisID = *it;
      const auto &nextID = *(it + 1);
      if (kindIsSwiftDependency(thisID) && kindIsSwiftDependency(nextID) &&
          llvm::any_of(
              cache.getSwiftOverlayDependencies(thisID),
              [&](const ModuleDependencyID id) { return id == nextID; })) {
        llvm::SmallString<64> noteBuffer;
        auto clangDepPath = findClangDepPath(
            thisID,
            ModuleDependencyID{nextID.ModuleName, ModuleDependencyKind::Clang},
            cache);
        emitModulePath(clangDepPath, noteBuffer);
        instance.getASTContext().Diags.diagnose(
            SourceLoc(), diag::scanner_find_cycle_swift_overlay_path,
            thisID.ModuleName, nextID.ModuleName, noteBuffer.str());
      }
    }
  };

  // Start from the main module and check direct and overlay dependencies
  openSet.insert(mainId);
  while (!openSet.empty()) {
    auto lastOpen = openSet.back();
    auto beforeSize = openSet.size();
    assert(cache.findDependency(lastOpen).has_value() &&
           "Missing dependency info during cycle diagnosis.");
    for (const auto &depId : cache.getAllDependencies(lastOpen)) {
      if (closeSet.count(depId))
        continue;
      // Ensure we detect dependency of the Source target
      // on an existing Swift module with the same name
      if (kindIsSwiftDependency(depId) &&
          depId.ModuleName == mainId.ModuleName && openSet.contains(mainId)) {
        emitCycleDiagnostic(mainId, depId);
        return true;
      }
      if (openSet.insert(depId)) {
        break;
      } else {
        emitCycleDiagnostic(depId, depId);
        return true;
      }
    }
    // No new node added. We can close this node
    if (openSet.size() == beforeSize) {
      closeSet.insert(openSet.back());
      openSet.pop_back();
    } else {
      assert(openSet.size() == beforeSize + 1);
    }
  }
  assert(openSet.empty());
  closeSet.clear();
  return false;
}
} // namespace

bool swift::dependencies::scanDependencies(CompilerInstance &CI) {
  ASTContext &ctx = CI.getASTContext();
  std::string depGraphOutputPath =
      CI.getInvocation()
          .getFrontendOptions()
          .InputsAndOutputs.getSingleOutputFilename();
  // `-scan-dependencies` invocations use a single new instance
  // of a module cache
  SwiftDependencyScanningService *service =
      ctx.Allocate<SwiftDependencyScanningService>();
  ModuleDependenciesCache cache(*service,
                                CI.getMainModule()->getNameStr().str(),
                                CI.getInvocation().getModuleScanningHash());

  if (service->setupCachingDependencyScanningService(CI))
    return true;

  // Execute scan
  llvm::ErrorOr<swiftscan_dependency_graph_t> dependenciesOrErr =
      performModuleScan(CI, cache);

  if (dependenciesOrErr.getError())
    return true;
  auto dependencies = std::move(*dependenciesOrErr);

  if (writeJSONToOutput(ctx.Diags, CI.getOutputBackend(), depGraphOutputPath,
                        dependencies))
    return true;

  // This process succeeds regardless of whether any errors occurred.
  // FIXME: We shouldn't need this, but it's masking bugs in our scanning
  // logic where we don't create a fresh context when scanning Swift interfaces
  // that includes their own command-line flags.
  ctx.Diags.resetHadAnyError();
  return false;
}

bool swift::dependencies::prescanDependencies(CompilerInstance &instance) {
  ASTContext &Context = instance.getASTContext();
  const FrontendOptions &opts = instance.getInvocation().getFrontendOptions();
  std::string path = opts.InputsAndOutputs.getSingleOutputFilename();
  // `-scan-dependencies` invocations use a single new instance
  // of a module cache
  SwiftDependencyScanningService *singleUseService =
      Context.Allocate<SwiftDependencyScanningService>();
  ModuleDependenciesCache cache(
      *singleUseService, instance.getMainModule()->getNameStr().str(),
      instance.getInvocation().getModuleScanningHash());

  // Execute import prescan, and write JSON output to the output stream
  auto importSetOrErr = performModulePrescan(instance, cache);
  if (importSetOrErr.getError())
    return true;
  auto importSet = std::move(*importSetOrErr);

  // Serialize and output main module dependencies only and exit.
  if (writePrescanJSONToOutput(Context.Diags, instance.getOutputBackend(), path,
                               importSet))
    return true;

  // This process succeeds regardless of whether any errors occurred.
  // FIXME: We shouldn't need this, but it's masking bugs in our scanning
  // logic where we don't create a fresh context when scanning Swift interfaces
  // that includes their own command-line flags.
  Context.Diags.resetHadAnyError();
  return false;
}

std::string
swift::dependencies::createEncodedModuleKindAndName(ModuleDependencyID id) {
  switch (id.Kind) {
  case ModuleDependencyKind::SwiftInterface:
  case ModuleDependencyKind::SwiftSource:
    return "swiftTextual:" + id.ModuleName;
  case ModuleDependencyKind::SwiftBinary:
    return "swiftBinary:" + id.ModuleName;
  case ModuleDependencyKind::Clang:
    return "clang:" + id.ModuleName;
  default:
    llvm_unreachable("Unhandled dependency kind.");
  }
}

static bool resolveDependencyCommandLineArguments(
    CompilerInstance &instance, ModuleDependenciesCache &cache,
    const std::vector<ModuleDependencyID> &topoSortedModuleList) {
  auto moduleTransitiveClosures =
      computeTransitiveClosureOfExplicitDependencies(topoSortedModuleList,
                                                     cache);
  auto tracker = cache.getScanService().createSwiftDependencyTracker(
      instance.getInvocation());
  for (const auto &modID : llvm::reverse(topoSortedModuleList)) {
    auto dependencyClosure = moduleTransitiveClosures[modID];
    // For main module or binary modules, no command-line to resolve.
    // For Clang modules, their dependencies are resolved by the clang Scanner
    // itself for us.
    auto &deps = cache.findKnownDependency(modID);
    std::optional<std::set<ModuleDependencyID>> bridgingHeaderDeps;
    if (modID.Kind == ModuleDependencyKind::SwiftSource)
      bridgingHeaderDeps = computeBridgingHeaderTransitiveDependencies(
          deps, moduleTransitiveClosures, cache);

    if (resolveExplicitModuleInputs(modID, dependencyClosure, cache, instance,
                                    bridgingHeaderDeps, tracker))
      return true;
  }

  return false;
}

static void
updateDependencyTracker(CompilerInstance &instance,
                        ModuleDependenciesCache &cache,
                        const std::vector<ModuleDependencyID> &allModules) {
  if (auto depTracker = instance.getDependencyTracker()) {
    for (auto module : allModules) {
      auto optionalDeps = cache.findDependency(module);
      if (!optionalDeps.has_value())
        continue;
      auto deps = optionalDeps.value();

      if (auto swiftDeps = deps->getAsSwiftInterfaceModule()) {
        depTracker->addDependency(swiftDeps->swiftInterfaceFile,
                                  /*IsSystem=*/false);
        for (const auto &bridgingSourceFile :
             swiftDeps->textualModuleDetails.bridgingSourceFiles)
          depTracker->addDependency(bridgingSourceFile, /*IsSystem=*/false);
      } else if (auto swiftSourceDeps = deps->getAsSwiftSourceModule()) {
        for (const auto &sourceFile : swiftSourceDeps->sourceFiles)
          depTracker->addDependency(sourceFile, /*IsSystem=*/false);
        for (const auto &bridgingSourceFile :
             swiftSourceDeps->textualModuleDetails.bridgingSourceFiles)
          depTracker->addDependency(bridgingSourceFile, /*IsSystem=*/false);
      } else if (auto clangDeps = deps->getAsClangModule()) {
        if (!clangDeps->moduleMapFile.empty())
          depTracker->addDependency(clangDeps->moduleMapFile,
                                    /*IsSystem=*/false);
        for (const auto &sourceFile : clangDeps->fileDependencies)
          depTracker->addDependency(sourceFile, /*IsSystem=*/false);
      }
    }
  }
}

static void resolveImplicitLinkLibraries(const CompilerInstance &instance,
                                         ModuleDependenciesCache &cache) {
  auto langOpts = instance.getInvocation().getLangOptions();
  auto irGenOpts = instance.getInvocation().getIRGenOptions();
  auto mainModuleName = instance.getMainModule()->getNameStr();
  auto mainModuleID = ModuleDependencyID{mainModuleName.str(),
                                         ModuleDependencyKind::SwiftSource};
  auto mainModuleDepInfo = cache.findKnownDependency(mainModuleID);

  std::vector<LinkLibrary> linkLibraries;
  auto addLinkLibrary = [&linkLibraries](const LinkLibrary &ll) {
    linkLibraries.push_back(ll);
  };

  if (langOpts.EnableObjCInterop)
    addLinkLibrary(LinkLibrary{"objc", LibraryKind::Library, /*static=*/false});

  if (langOpts.EnableCXXInterop) {
    auto OptionalCxxDep = cache.findDependency(CXX_MODULE_NAME);
    auto OptionalCxxStdLibDep =
        cache.findDependency(instance.getASTContext().Id_CxxStdlib.str());
    bool hasStaticCxx =
        OptionalCxxDep.has_value() && OptionalCxxDep.value()->isStaticLibrary();
    bool hasStaticCxxStdlib = OptionalCxxStdLibDep.has_value() &&
                              OptionalCxxStdLibDep.value()->isStaticLibrary();
    registerCxxInteropLibraries(langOpts.Target, mainModuleName, hasStaticCxx,
                                hasStaticCxxStdlib, langOpts.CXXStdlib,
                                addLinkLibrary);
  }

  if (!irGenOpts.UseJIT && !langOpts.hasFeature(Feature::Embedded))
    registerBackDeployLibraries(irGenOpts, addLinkLibrary);

  mainModuleDepInfo.setLinkLibraries(linkLibraries);
  cache.updateDependency(mainModuleID, mainModuleDepInfo);
}

llvm::ErrorOr<swiftscan_dependency_graph_t>
swift::dependencies::performModuleScan(
    CompilerInstance &instance, ModuleDependenciesCache &cache,
    DependencyScanDiagnosticCollector *diagnosticCollector) {
  const ASTContext &ctx = instance.getASTContext();
  const FrontendOptions &opts = instance.getInvocation().getFrontendOptions();
  // Load the dependency cache if -reuse-dependency-scan-cache
  // is specified
  if (opts.ReuseDependencyScannerCache) {
    auto cachePath = opts.SerializedDependencyScannerCachePath;
    if (opts.EmitDependencyScannerCacheRemarks)
      ctx.Diags.diagnose(SourceLoc(), diag::remark_reuse_cache, cachePath);

    llvm::sys::TimePoint<> serializedCacheTimeStamp;
    bool loadFailure =
        module_dependency_cache_serialization::readInterModuleDependenciesCache(
            cachePath, cache, serializedCacheTimeStamp);
    if (opts.EmitDependencyScannerCacheRemarks && loadFailure)
      ctx.Diags.diagnose(SourceLoc(), diag::warn_scanner_deserialize_failed,
                         cachePath);

    if (!loadFailure && opts.ValidatePriorDependencyScannerCache) {
      auto mainModuleID =
          ModuleDependencyID{instance.getMainModule()->getNameStr().str(),
                             ModuleDependencyKind::SwiftSource};
      incremental::validateInterModuleDependenciesCache(
          mainModuleID, cache, serializedCacheTimeStamp,
          *instance.getSourceMgr().getFileSystem(), ctx.Diags,
          opts.EmitDependencyScannerCacheRemarks);
    }
  }

  auto scanner = ModuleDependencyScanner(
      cache.getScanService(), instance.getInvocation(),
      instance.getSILOptions(), instance.getASTContext(),
      *instance.getDependencyTracker(), instance.getDiags(),
      instance.getInvocation().getFrontendOptions().ParallelDependencyScan);

  // Identify imports of the main module and add an entry for it
  // to the dependency graph.
  auto mainModuleName = instance.getMainModule()->getNameStr();
  auto mainModuleID = ModuleDependencyID{mainModuleName.str(),
                                         ModuleDependencyKind::SwiftSource};
  if (!cache.hasDependency(mainModuleID))
    cache.recordDependency(mainModuleName, *scanner.getMainModuleDependencyInfo(
                                               instance.getMainModule()));

  // Perform the full module scan starting at the main module.
  auto allModules = scanner.performDependencyScan(mainModuleID, cache);
  if (diagnoseCycle(instance, cache, mainModuleID))
    return std::make_error_code(std::errc::not_supported);

  auto topologicallySortedModuleList =
      computeTopologicalSortOfExplicitDependencies(allModules, cache);

  resolveDependencyCommandLineArguments(instance, cache,
                                        topologicallySortedModuleList);
  resolveImplicitLinkLibraries(instance, cache);
  updateDependencyTracker(instance, cache, allModules);

  if (ctx.Stats)
    ctx.Stats->getFrontendCounters().NumDepScanFilesystemLookups =
        scanner.getNumLookups();

  // Serialize the dependency cache if -serialize-dependency-scan-cache
  // is specified
  if (opts.SerializeDependencyScannerCache) {
    auto savePath = opts.SerializedDependencyScannerCachePath;
    module_dependency_cache_serialization::writeInterModuleDependenciesCache(
        ctx.Diags, instance.getOutputBackend(), savePath, cache);
    if (opts.EmitDependencyScannerCacheRemarks)
      ctx.Diags.diagnose(SourceLoc(), diag::remark_save_cache, savePath);
  }

  return generateFullDependencyGraph(instance, diagnosticCollector, cache,
                                     topologicallySortedModuleList);
}

llvm::ErrorOr<swiftscan_import_set_t> swift::dependencies::performModulePrescan(
    CompilerInstance &instance, ModuleDependenciesCache &cache,
    DependencyScanDiagnosticCollector *diagnosticCollector) {
  // Setup the scanner
  auto scanner = ModuleDependencyScanner(
      cache.getScanService(), instance.getInvocation(),
      instance.getSILOptions(), instance.getASTContext(),
      *instance.getDependencyTracker(), instance.getDiags(),
      instance.getInvocation().getFrontendOptions().ParallelDependencyScan);
  // Execute import prescan, and write JSON output to the output stream
  auto mainDependencies =
      scanner.getMainModuleDependencyInfo(instance.getMainModule());
  if (!mainDependencies)
    return mainDependencies.getError();
  auto *importSet = new swiftscan_import_set_s;

  std::vector<std::string> importIdentifiers;
  importIdentifiers.reserve(mainDependencies->getModuleImports().size());
  llvm::transform(mainDependencies->getModuleImports(),
                  std::back_inserter(importIdentifiers),
                  [](const auto &importInfo) -> std::string {
                    return importInfo.importIdentifier;
                  });
  importSet->imports = create_set(importIdentifiers);
  importSet->diagnostics =
      diagnosticCollector
          ? mapCollectedDiagnosticsForOutput(diagnosticCollector)
          : nullptr;
  importSet->diagnostics =
      diagnosticCollector
          ? mapCollectedDiagnosticsForOutput(diagnosticCollector)
          : nullptr;
  return importSet;
}

void swift::dependencies::incremental::validateInterModuleDependenciesCache(
    const ModuleDependencyID &rootModuleID, ModuleDependenciesCache &cache,
    const llvm::sys::TimePoint<> &cacheTimeStamp, llvm::vfs::FileSystem &fs,
    DiagnosticEngine &diags, bool emitRemarks) {
  ModuleDependencyIDSet visited;
  ModuleDependencyIDSet modulesRequiringRescan;
  outOfDateModuleScan(rootModuleID, cache, cacheTimeStamp, fs, diags,
                      emitRemarks, visited, modulesRequiringRescan);
  for (const auto &outOfDateModID : modulesRequiringRescan)
    cache.removeDependency(outOfDateModID);

  // Regardless of invalidation, always re-scan main module.
  cache.removeDependency(rootModuleID);
}

void swift::dependencies::incremental::outOfDateModuleScan(
    const ModuleDependencyID &moduleID, const ModuleDependenciesCache &cache,
    const llvm::sys::TimePoint<> &cacheTimeStamp, llvm::vfs::FileSystem &fs,
    DiagnosticEngine &diags, bool emitRemarks, ModuleDependencyIDSet &visited,
    ModuleDependencyIDSet &modulesRequiringRescan) {
  // Visit the module's dependencies
  bool hasOutOfDateModuleDependency = false;
  for (const auto &depID : cache.getAllDependencies(moduleID)) {
    // If we have not already visited this module, recurse.
    if (visited.find(depID) == visited.end())
      outOfDateModuleScan(depID, cache, cacheTimeStamp, fs, diags, emitRemarks,
                          visited, modulesRequiringRescan);

    // Even if we're not revisiting a dependency, we must check if it's
    // already known to be out of date.
    hasOutOfDateModuleDependency |=
        (modulesRequiringRescan.find(depID) != modulesRequiringRescan.end());
  }

  if (hasOutOfDateModuleDependency) {
    if (emitRemarks)
      diags.diagnose(SourceLoc(), diag::remark_scanner_invalidate_upstream,
                     moduleID.ModuleName);
    modulesRequiringRescan.insert(moduleID);
  } else if (!verifyModuleDependencyUpToDate(moduleID, cache, cacheTimeStamp,
                                             fs, diags, emitRemarks))
    modulesRequiringRescan.insert(moduleID);

  visited.insert(moduleID);
}

bool swift::dependencies::incremental::verifyModuleDependencyUpToDate(
    const ModuleDependencyID &moduleID, const ModuleDependenciesCache &cache,
    const llvm::sys::TimePoint<> &cacheTimeStamp, llvm::vfs::FileSystem &fs,
    DiagnosticEngine &diags, bool emitRemarks) {
  const auto &moduleInfo = cache.findKnownDependency(moduleID);
  auto verifyInputOlderThanCacheTimeStamp = [&cacheTimeStamp, &fs, &diags,
                                             emitRemarks](StringRef moduleName,
                                                          StringRef inputPath) {
    llvm::sys::TimePoint<> inputModTime = llvm::sys::TimePoint<>::max();
    if (auto Status = fs.status(inputPath))
      inputModTime = Status->getLastModificationTime();
    if (inputModTime > cacheTimeStamp) {
      if (emitRemarks)
        diags.diagnose(SourceLoc(),
                       diag::remark_scanner_stale_result_invalidate, moduleName,
                       inputPath);
      return false;
    }
    return true;
  };

  auto verifyCASID = [&cache, &diags, emitRemarks](StringRef moduleName,
                                                   const std::string &casID) {
    if (!cache.getScanService().hasCAS()) {
      // If the wrong cache is passed.
      if (emitRemarks)
        diags.diagnose(SourceLoc(),
                       diag::remark_scanner_invalidate_configuration,
                       moduleName);
      return false;
    }
    auto &CAS = cache.getScanService().getCAS();
    auto ID = CAS.parseID(casID);
    if (!ID) {
      if (emitRemarks)
        diags.diagnose(SourceLoc(), diag::remark_scanner_invalidate_cas_error,
                       moduleName, toString(ID.takeError()));
      return false;
    }
    if (!CAS.getReference(*ID)) {
      if (emitRemarks)
        diags.diagnose(SourceLoc(), diag::remark_scanner_invalidate_missing_cas,
                       moduleName, casID);
      return false;
    }
    return true;
  };

  // Check CAS inputs exist
  if (const auto casID = moduleInfo.getClangIncludeTree())
    if (!verifyCASID(moduleID.ModuleName, *casID))
      return false;
  if (const auto casID = moduleInfo.getCASFSRootID())
    if (!verifyCASID(moduleID.ModuleName, *casID))
      return false;

  // Check interface file for Swift textual modules
  if (const auto &textualModuleDetails = moduleInfo.getAsSwiftInterfaceModule())
    if (!verifyInputOlderThanCacheTimeStamp(
            moduleID.ModuleName, textualModuleDetails->swiftInterfaceFile))
      return false;

  // Check binary module file for Swift binary-only modules
  if (const auto &binaryModuleDetails = moduleInfo.getAsSwiftBinaryModule())
    if (!verifyInputOlderThanCacheTimeStamp(
            moduleID.ModuleName, binaryModuleDetails->compiledModulePath))
      return false;

  // Check header input source files (bridging header etc.)
  for (const auto &headerInput : moduleInfo.getHeaderInputSourceFiles())
    if (!verifyInputOlderThanCacheTimeStamp(moduleID.ModuleName, headerInput))
      return false;

  // Auxiliary files
  for (const auto &auxInput : moduleInfo.getAuxiliaryFiles())
    if (!verifyInputOlderThanCacheTimeStamp(moduleID.ModuleName, auxInput))
      return false;

  // Check header/modulemap source files for a Clang dependency
  if (const auto &clangModuleDetails = moduleInfo.getAsClangModule())
    for (const auto &fileInput : clangModuleDetails->fileDependencies)
      if (!verifyInputOlderThanCacheTimeStamp(moduleID.ModuleName, fileInput))
        return false;

  return true;
}
