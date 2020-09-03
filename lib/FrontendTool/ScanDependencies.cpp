//===--- ScanDependencies.cpp -- Scans the dependencies of a module -------===//
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
#include "ScanDependencies.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/DiagnosticsFrontend.h"
#include "swift/AST/Module.h"
#include "swift/AST/ModuleDependencies.h"
#include "swift/AST/ModuleLoader.h"
#include "swift/AST/SourceFile.h"
#include "swift/ClangImporter/ClangImporter.h"
#include "swift/Basic/Defer.h"
#include "swift/Basic/LLVM.h"
#include "swift/Basic/STLExtras.h"
#include "swift/Frontend/Frontend.h"
#include "swift/Frontend/FrontendOptions.h"
#include "swift/Frontend/ModuleInterfaceLoader.h"
#include "swift/Strings.h"
#include "clang/Basic/Module.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/StringSet.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/StringSaver.h"
#include "llvm/Support/YAMLTraits.h"
#include "llvm/Support/YAMLParser.h"
#include <set>

using namespace swift;
using namespace llvm::yaml;

namespace {
struct BatchScanInput {
  StringRef moduleName;
  StringRef arguments;
  StringRef outputPath;
  bool isSwift;
};

static std::string getScalaNodeText(Node *N) {
  SmallString<32> Buffer;
  return cast<ScalarNode>(N)->getValue(Buffer).str();
}

/// Parse an entry like this, where the "platforms" key-value pair is optional:
///  {
///     "swiftModuleName": "Foo",
///     "arguments": "-target 10.15",
///     "output": "../Foo.json"
///  },
static bool parseBatchInputEntries(ASTContext &Ctx, llvm::StringSaver &saver,
                                   Node *Node, std::vector<BatchScanInput> &result) {
  auto *SN = cast<SequenceNode>(Node);
  if (!SN)
    return true;
  for (auto It = SN->begin(); It != SN->end(); ++It) {
    auto *MN = cast<MappingNode>(&*It);
    BatchScanInput entry;
    Optional<std::set<int8_t>> Platforms;
    for (auto &Pair: *MN) {
      auto Key = getScalaNodeText(Pair.getKey());
      auto* Value = Pair.getValue();
      if (Key == "clangModuleName") {
        entry.moduleName = saver.save(getScalaNodeText(Value));
        entry.isSwift = false;
      } else if (Key == "swiftModuleName") {
        entry.moduleName = saver.save(getScalaNodeText(Value));
        entry.isSwift = true;
      } else if (Key == "arguments") {
        entry.arguments = saver.save(getScalaNodeText(Value));
      } else if (Key == "output") {
        entry.outputPath = saver.save(getScalaNodeText(Value));
      } else {
        // Future proof.
        continue;
      }
    }
    if (entry.moduleName.empty())
      return true;
    if (entry.outputPath.empty())
      return true;
    result.emplace_back(std::move(entry));
  }
  return false;
}

static Optional<std::vector<BatchScanInput>>
parseBatchScanInputFile(ASTContext &ctx, StringRef batchInputPath,
                        llvm::StringSaver &saver) {
  assert(!batchInputPath.empty());
  namespace yaml = llvm::yaml;
  std::vector<BatchScanInput> result;

  // Load the input file.
  llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>> FileBufOrErr =
    llvm::MemoryBuffer::getFile(batchInputPath);
  if (!FileBufOrErr) {
    ctx.Diags.diagnose(SourceLoc(), diag::batch_scan_input_file_missing,
                       batchInputPath);
    return None;
  }
  StringRef Buffer = FileBufOrErr->get()->getBuffer();

  // Use a new source manager instead of the one from ASTContext because we
  // don't want the Json file to be persistent.
  SourceManager SM;
  yaml::Stream Stream(llvm::MemoryBufferRef(Buffer, batchInputPath),
                      SM.getLLVMSourceMgr());
  for (auto DI = Stream.begin(); DI != Stream.end(); ++ DI) {
    assert(DI != Stream.end() && "Failed to read a document");
    yaml::Node *N = DI->getRoot();
    assert(N && "Failed to find a root");
    if (parseBatchInputEntries(ctx, saver, N, result)) {
      ctx.Diags.diagnose(SourceLoc(), diag::batch_scan_input_file_corrupted,
                         batchInputPath);
      return None;
    }
  }
  return result;
}
}

/// Find all of the imported Clang modules starting with the given module name.
static void findAllImportedClangModules(ASTContext &ctx, StringRef moduleName,
                                        ModuleDependenciesCache &cache,
                                        std::vector<std::string> &allModules,
                                        llvm::StringSet<> &knownModules) {
  if (!knownModules.insert(moduleName).second)
    return;
  allModules.push_back(moduleName.str());

  auto dependencies = cache.findDependencies(
      moduleName, ModuleDependenciesKind::Clang);
  if (!dependencies)
    return;

  for (const auto &dep : dependencies->getModuleDependencies()) {
    findAllImportedClangModules(ctx, dep, cache, allModules, knownModules);
  }
}

/// Resolve the direct dependencies of the given module.
static std::vector<ModuleDependencyID> resolveDirectDependencies(
    CompilerInstance &instance, ModuleDependencyID module,
    ModuleDependenciesCache &cache,
    InterfaceSubContextDelegate &ASTDelegate) {
  auto &ctx = instance.getASTContext();
  auto knownDependencies = *cache.findDependencies(module.first, module.second);
  auto isSwift = knownDependencies.isSwiftModule();

  // Find the dependencies of every module this module directly depends on.
  std::vector<ModuleDependencyID> result;
  for (auto dependsOn : knownDependencies.getModuleDependencies()) {
    // Figure out what kind of module we need.
    bool onlyClangModule = !isSwift || module.first == dependsOn;

    // Retrieve the dependencies for this module.
    if (auto found = ctx.getModuleDependencies(
            dependsOn, onlyClangModule, cache, ASTDelegate)) {
      result.push_back({dependsOn, found->getKind()});
    }
  }

  if (isSwift) {
    // A record of all of the Clang modules referenced from this Swift module.
    std::vector<std::string> allClangModules;
    llvm::StringSet<> knownModules;

    // If the Swift module has a bridging header, add those dependencies.
    if (knownDependencies.getBridgingHeader()) {
      auto clangImporter =
          static_cast<ClangImporter *>(ctx.getClangModuleLoader());
      if (!clangImporter->addBridgingHeaderDependencies(module.first, cache)) {
        // Grab the updated module dependencies.
        // FIXME: This is such a hack.
        knownDependencies = *cache.findDependencies(module.first, module.second);

        // Add the Clang modules referenced from the bridging header to the
        // set of Clang modules we know about.
        auto swiftDeps = knownDependencies.getAsSwiftModule();
        for (const auto &clangDep : swiftDeps->bridgingModuleDependencies) {
          findAllImportedClangModules(ctx, clangDep, cache, allClangModules,
                                      knownModules);
        }
      }
    }

    // Find all of the Clang modules this Swift module depends on.
    for (const auto &dep : result) {
      if (dep.second != ModuleDependenciesKind::Clang)
        continue;

      findAllImportedClangModules(ctx, dep.first, cache, allClangModules,
                                  knownModules);
    }

    // Look for overlays for each of the Clang modules. The Swift module
    // directly depends on these.
    for (const auto &clangDep : allClangModules) {
      if (auto found = ctx.getModuleDependencies(
              clangDep, /*onlyClangModule=*/false, cache, ASTDelegate)) {
        // ASTContext::getModuleDependencies returns dependencies for a module with a given name.
        // This Clang module may have the same name as the Swift module we are resolving, so we
        // need to make sure we don't add a dependency from a Swift module to itself.
        if (found->getKind() == ModuleDependenciesKind::Swift && clangDep != module.first)
          result.push_back({clangDep, found->getKind()});
      }
    }
  }
  return result;
}

static void discoverCrosssImportOverlayDependencies(
    CompilerInstance &instance, StringRef mainModuleName,
    ArrayRef<ModuleDependencyID> allDependencies,
    ModuleDependenciesCache &cache, InterfaceSubContextDelegate &ASTDelegate,
    llvm::function_ref<void(ModuleDependencyID)> action) {
  // Modules explicitly imported. Only these can be secondary module.
  llvm::SetVector<Identifier> newOverlays;
  for (auto dep: allDependencies) {
    auto moduleName = dep.first;
    auto dependencies = *cache.findDependencies(moduleName, dep.second);
    // Collect a map from secondary module name to cross-import overlay names.
    auto overlayMap = dependencies.collectCrossImportOverlayNames(
      instance.getASTContext(), moduleName);
    if (overlayMap.empty())
      continue;
    std::for_each(allDependencies.begin(), allDependencies.end(),
                  [&](ModuleDependencyID Id) {
      // check if any explicitly imported modules can serve as a secondary
      // module, and add the overlay names to the dependencies list.
      for (auto overlayName: overlayMap[Id.first]) {
        if (std::find_if(allDependencies.begin(), allDependencies.end(),
            [&](ModuleDependencyID Id) { return Id.first == overlayName.str(); })
                == allDependencies.end()) {
          newOverlays.insert(overlayName);
        }
      }
    });
  }
  // No new cross-import overlays are found, return.
  if (newOverlays.empty())
    return;
  // Construct a dummy main to resolve the newly discovered cross import overlays.
  StringRef dummyMainName = "DummyMainModuleForResolvingCrossImportOverlays";
  auto dummyMainDependencies = ModuleDependencies::forMainSwiftModule({});

  // Update main module's dependencies to include these new overlays.
  auto mainDep = *cache.findDependencies(mainModuleName, ModuleDependenciesKind::Swift);
  std::for_each(newOverlays.begin(), newOverlays.end(), [&](Identifier modName) {
    dummyMainDependencies.addModuleDependency(modName.str());
    mainDep.addModuleDependency(modName.str());
  });
  cache.updateDependencies({mainModuleName.str(), ModuleDependenciesKind::Swift}, mainDep);

  // Record the dummy main module's direct dependencies. The dummy main module
  // only directly depend on these newly discovered overlay modules.
  cache.recordDependencies(dummyMainName, dummyMainDependencies,
                           ModuleDependenciesKind::Swift);
  llvm::SetVector<ModuleDependencyID, std::vector<ModuleDependencyID>,
                  std::set<ModuleDependencyID>> allModules;

  // Seed the all module list from the dummpy main module.
  allModules.insert({dummyMainName.str(), dummyMainDependencies.getKind()});

  // Explore the dependencies of every module.
  for (unsigned currentModuleIdx = 0;
       currentModuleIdx < allModules.size();
       ++currentModuleIdx) {
    auto module = allModules[currentModuleIdx];
    auto discoveredModules = resolveDirectDependencies(instance, module,
                                                       cache, ASTDelegate);
    allModules.insert(discoveredModules.begin(), discoveredModules.end());
  }
  // Report any discovered modules to the clients, which include all overlays
  // and their dependencies.
  std::for_each(/* +1 to exclude dummy main*/allModules.begin() + 1,
                allModules.end(), action);
}

/// Write a single JSON field.
namespace {
  template<typename T>
  void writeJSONSingleField(llvm::raw_ostream &out,
                            StringRef fieldName,
                            const T &value,
                            unsigned indentLevel,
                            bool trailingComma);

  /// Write a string value as JSON.
  void writeJSONValue(llvm::raw_ostream &out,
                      StringRef value,
                      unsigned indentLevel) {
    out << "\"";
    out.write_escaped(value);
    out << "\"";
  }

  /// Write a boolean value as JSON.
  void writeJSONValue(llvm::raw_ostream &out,
                      bool value,
                      unsigned indentLevel) {
    out.write_escaped(value ? "true" : "false");
  }

  /// Write a module identifier.
  void writeJSONValue(llvm::raw_ostream &out,
                      const ModuleDependencyID &module,
                      unsigned indentLevel) {
    out << "{\n";
    std::string moduleKind;
    if (module.second == ModuleDependenciesKind::Swift)
      moduleKind = "swift";
    else if (module.second == ModuleDependenciesKind::SwiftPlaceholder)
      moduleKind = "swiftPlaceholder";
    else
      moduleKind = "clang";

    writeJSONSingleField(
        out,
        moduleKind,
        module.first,
        indentLevel + 1,
        /*trailingComma=*/false);

    out.indent(indentLevel * 2);
    out << "}";
  }

  /// Write a JSON array.
  template<typename T>
  void writeJSONValue(llvm::raw_ostream &out,
                      ArrayRef<T> values,
                      unsigned indentLevel) {
    out << "[\n";

    for (const auto &value: values) {

      out.indent((indentLevel + 1) * 2);

      writeJSONValue(out, value, indentLevel + 1);

      if (&value != &values.back()) {
        out << ",";
      }
      out << "\n";
    }

    out.indent(indentLevel * 2);
    out << "]";
  }

  /// Write a JSON array.
  template<typename T>
  void writeJSONValue(llvm::raw_ostream &out,
                      const std::vector<T> &values,
                      unsigned indentLevel) {
    writeJSONValue(out, llvm::makeArrayRef(values), indentLevel);
  }

  /// Write a single JSON field.
  template<typename T>
  void writeJSONSingleField(llvm::raw_ostream &out,
                            StringRef fieldName,
                            const T &value,
                            unsigned indentLevel,
                            bool trailingComma) {
    out.indent(indentLevel * 2);
    writeJSONValue(out, fieldName, indentLevel);
    out << ": ";
    writeJSONValue(out, value, indentLevel);
    if (trailingComma)
      out << ",";
    out << "\n";
  }
}

static void writeJSON(llvm::raw_ostream &out,
                      CompilerInstance &instance,
                      ModuleDependenciesCache &cache,
                      InterfaceSubContextDelegate &ASTDelegate,
                      ArrayRef<ModuleDependencyID> allModules) {
  // Write out a JSON description of all of the dependencies.
  out << "{\n";
  SWIFT_DEFER {
    out << "}\n";
  };

  // Name of the main module.
  writeJSONSingleField(out, "mainModuleName", allModules.front().first,
                       /*indentLevel=*/1, /*trailingComma=*/true);

  // Write out all of the modules.
  out << "  \"modules\": [\n";
  SWIFT_DEFER {
    out << "  ]\n";
  };
  for (const auto &module : allModules) {
    auto directDependencies = resolveDirectDependencies(
      instance, ModuleDependencyID(module.first, module.second), cache,
      ASTDelegate);

    // Grab the completed module dependencies.
    auto moduleDeps = *cache.findDependencies(module.first, module.second);

    // The module we are describing.
    out.indent(2 * 2);
    writeJSONValue(out, module, 2);
    out << ",\n";

    out.indent(2 * 2);
    out << "{\n";

    auto externalSwiftDep = moduleDeps.getAsPlaceholderDependencyModule();
    auto swiftDeps = moduleDeps.getAsSwiftModule();
    auto clangDeps = moduleDeps.getAsClangModule();

    // Module path.
    const char *modulePathSuffix =
        moduleDeps.isSwiftModule() ? ".swiftmodule" : ".pcm";

    std::string modulePath = externalSwiftDep
                                 ? externalSwiftDep->compiledModulePath
                                 : module.first + modulePathSuffix;
    writeJSONSingleField(out, "modulePath", modulePath, /*indentLevel=*/3,
                         /*trailingComma=*/true);

    // Source files.
    if (swiftDeps) {
      writeJSONSingleField(out, "sourceFiles", swiftDeps->sourceFiles, 3,
                           /*trailingComma=*/true);
    } else if (clangDeps) {
      writeJSONSingleField(out, "sourceFiles", clangDeps->fileDependencies, 3,
                           /*trailingComma=*/true);
    }

    // Direct dependencies.
    if (swiftDeps || clangDeps)
      writeJSONSingleField(out, "directDependencies", directDependencies, 3,
                           /*trailingComma=*/true);

    // Swift and Clang-specific details.
    out.indent(3 * 2);
    out << "\"details\": {\n";
    out.indent(4 * 2);
    if (swiftDeps) {
      out << "\"swift\": {\n";

      /// Swift interface file, if any.
      if (swiftDeps->swiftInterfaceFile) {
        writeJSONSingleField(
            out, "moduleInterfacePath",
            *swiftDeps->swiftInterfaceFile, 5,
            /*trailingComma=*/true);
        writeJSONSingleField(out, "contextHash",
                             swiftDeps->contextHash, 5,
                             /*trailingComma=*/true);
        out.indent(5 * 2);
        out << "\"commandLine\": [\n";
        for (auto &arg :swiftDeps->buildCommandLine) {

          out.indent(6 * 2);
          out << "\"" << arg << "\"";
          if (&arg != &swiftDeps->buildCommandLine.back())
            out << ",";
          out << "\n";
        }
        out.indent(5 * 2);
        out << "],\n";
        out.indent(5 * 2);
        out << "\"compiledModuleCandidates\": [\n";
        for (auto &candidate: swiftDeps->compiledModuleCandidates) {
          out.indent(6 * 2);
          out << "\"" << candidate << "\"";
          if (&candidate != &swiftDeps->compiledModuleCandidates.back())
            out << ",";
          out << "\n";
        }
        out.indent(5 * 2);
        out << "],\n";
      } else if (!swiftDeps->compiledModulePath.empty()) {
        writeJSONSingleField(
            out, "compiledModulePath",
            swiftDeps->compiledModulePath, 5,
            /*trailingComma=*/false);
      }
      writeJSONSingleField(
          out, "isFramework",
          swiftDeps->isFramework, 5,
          /*trailingComma=*/!swiftDeps->extraPCMArgs.empty() ||
                           swiftDeps->bridgingHeaderFile.hasValue());
      if (!swiftDeps->extraPCMArgs.empty()) {
        out.indent(5 * 2);
        out << "\"extraPcmArgs\": [\n";
        for (auto &arg : swiftDeps->extraPCMArgs) {
          out.indent(6 * 2);
          out << "\"" << arg << "\"";
          if (&arg != &swiftDeps->extraPCMArgs.back())
            out << ",";
          out << "\n";
        }
        out.indent(5 * 2);
        out << (swiftDeps->bridgingHeaderFile.hasValue() ? "],\n" : "]\n");
      }
      /// Bridging header and its source file dependencies, if any.
      if (swiftDeps->bridgingHeaderFile) {
        out.indent(5 * 2);
        out << "\"bridgingHeader\": {\n";
        writeJSONSingleField(out, "path", *swiftDeps->bridgingHeaderFile, 6,
                             /*trailingComma=*/true);
        writeJSONSingleField(out, "sourceFiles", swiftDeps->bridgingSourceFiles,
                             6,
                             /*trailingComma=*/true);
        writeJSONSingleField(out, "moduleDependencies",
                             swiftDeps->bridgingModuleDependencies, 6,
                             /*trailingComma=*/false);
        out.indent(5 * 2);
        out << "}\n";
      }
    } else if (externalSwiftDep) {
      out << "\"swiftPlaceholder\": {\n";

      // Module doc file
      if (externalSwiftDep->moduleDocPath != "")
        writeJSONSingleField(out, "moduleDocPath",
                             externalSwiftDep->moduleDocPath,
                             /*indentLevel=*/5,
                             /*trailingComma=*/true);

      // Module Source Info file
      if (externalSwiftDep->moduleDocPath != "")
        writeJSONSingleField(out, "moduleSourceInfoPath",
                             externalSwiftDep->sourceInfoPath,
                             /*indentLevel=*/5,
                             /*trailingComma=*/true);
    } else {
      out << "\"clang\": {\n";

      // Module map file.
      writeJSONSingleField(out, "moduleMapPath", clangDeps->moduleMapFile, 5,
                           /*trailingComma=*/true);

      // Context hash.
      writeJSONSingleField(out, "contextHash", clangDeps->contextHash, 5,
                           /*trailingComma=*/true);

      // Command line.
      writeJSONSingleField(out, "commandLine", clangDeps->nonPathCommandLine, 5,
                           /*trailingComma=*/false);
    }

    out.indent(4 * 2);
    out << "}\n";
    out.indent(3 * 2);
    out << "}\n";
    out.indent(2 * 2);
    out << "}";

    if (&module != &allModules.back())
      out << ",";
    out << "\n";
  }
}

static bool diagnoseCycle(CompilerInstance &instance,
                          ModuleDependenciesCache &cache,
                          ModuleDependencyID mainId,
                          InterfaceSubContextDelegate &astDelegate) {
  llvm::SetVector<ModuleDependencyID, std::vector<ModuleDependencyID>,
                  std::set<ModuleDependencyID>> openSet;
  llvm::SetVector<ModuleDependencyID, std::vector<ModuleDependencyID>,
                  std::set<ModuleDependencyID>> closeSet;
  // Start from the main module.
  openSet.insert(mainId);
  while(!openSet.empty()) {
    auto &lastOpen = openSet.back();
    auto beforeSize = openSet.size();
    for (auto dep: resolveDirectDependencies(instance, lastOpen, cache,
                                             astDelegate)) {
      if (closeSet.count(dep))
        continue;
      if (openSet.insert(dep)) {
        break;
      } else {
        // Find a cycle, diagnose.
        auto startIt = std::find(openSet.begin(), openSet.end(), dep);
        assert(startIt != openSet.end());
        llvm::SmallString<64> buffer;
        for (auto it = startIt; it != openSet.end(); ++ it) {
          buffer.append(it->first);
          buffer.append(it->second == ModuleDependenciesKind::Swift?
                        ".swiftmodule": ".pcm");
          buffer.append(" -> ");
        }
        buffer.append(startIt->first);
        buffer.append(startIt->second == ModuleDependenciesKind::Swift?
                      ".swiftmodule": ".pcm");
        instance.getASTContext().Diags.diagnose(SourceLoc(),
                                                diag::scanner_find_cycle,
                                                buffer.str());
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
  return false;
}

static bool scanModuleDependencies(CompilerInstance &instance,
                                   StringRef moduleName,
                                   bool isClang,
                                   StringRef outputPath) {
  ASTContext &ctx = instance.getASTContext();
  auto &FEOpts = instance.getInvocation().getFrontendOptions();
  ModuleInterfaceLoaderOptions LoaderOpts(FEOpts);
  auto ModuleCachePath = getModuleCachePathFromClang(ctx
    .getClangModuleLoader()->getClangInstance());

  llvm::SetVector<ModuleDependencyID, std::vector<ModuleDependencyID>,
                  std::set<ModuleDependencyID>> allModules;
  // Create the module dependency cache.
  ModuleDependenciesCache cache;
  InterfaceSubContextDelegateImpl ASTDelegate(ctx.SourceMgr, ctx.Diags,
                                              ctx.SearchPathOpts, ctx.LangOpts,
                                              ctx.ClangImporterOpts,
                                              LoaderOpts,
                                              /*buildModuleCacheDirIfAbsent*/false,
                                              ModuleCachePath,
                                              FEOpts.PrebuiltModuleCachePath,
                                              FEOpts.SerializeModuleInterfaceDependencyHashes,
                                              FEOpts.shouldTrackSystemDependencies());
  Optional<ModuleDependencies> rootDeps;
  if (isClang) {
    // Loading the clang module using Clang importer.
    // This action will populate the cache with the main module's dependencies.
    rootDeps = ctx.getModuleDependencies(moduleName, /*IsClang*/true, cache,
                                         ASTDelegate);
  } else {
    // Loading the swift module's dependencies.
    rootDeps = ctx.getSwiftModuleDependencies(moduleName, cache, ASTDelegate);
  }
  if (!rootDeps.hasValue()) {
    // We cannot find the clang module, abort.
    return true;
  }
  // Add the main module.
  allModules.insert({moduleName.str(), isClang ? ModuleDependenciesKind::Clang:
    ModuleDependenciesKind::Swift});

  // Explore the dependencies of every module.
  for (unsigned currentModuleIdx = 0;
       currentModuleIdx < allModules.size();
       ++currentModuleIdx) {
    auto module = allModules[currentModuleIdx];
    auto discoveredModules =
        resolveDirectDependencies(instance, module, cache, ASTDelegate);
    allModules.insert(discoveredModules.begin(), discoveredModules.end());
  }
  // Write out the JSON description.
  std::error_code EC;
  llvm::raw_fd_ostream out(outputPath, EC, llvm::sys::fs::F_None);
  writeJSON(out, instance, cache, ASTDelegate, allModules.getArrayRef());
  return false;
}

bool swift::scanClangDependencies(CompilerInstance &instance) {
  return scanModuleDependencies(instance,
                                instance.getMainModule()->getNameStr(),
                                /*isClang*/true,
                                instance.getInvocation().getFrontendOptions()
                                  .InputsAndOutputs.getSingleOutputFilename());
}

bool swift::batchScanModuleDependencies(CompilerInstance &instance,
                                        llvm::StringRef batchInputFile) {
  const CompilerInvocation &invok = instance.getInvocation();

  (void)instance.getMainModule();
  llvm::BumpPtrAllocator alloc;
  llvm::StringSaver saver(alloc);
  auto results = parseBatchScanInputFile(instance.getASTContext(),
                                         batchInputFile, saver);
  if (!results.hasValue())
    return true;
  auto &diags = instance.getDiags();
  ForwardingDiagnosticConsumer FDC(diags);
  // Keep track of all compiler instances we have created.
  llvm::StringMap<std::unique_ptr<CompilerInstance>> subInstanceMap;
  for (auto &entry: *results) {
    CompilerInstance *pInstance = nullptr;
    if (entry.arguments.empty()) {
      // Use the compiler's instance if no arguments are specified.
      pInstance = &instance;
    } else if (subInstanceMap.count(entry.arguments)) {
      // Use the previously created instance if we've seen the arguments before.
      pInstance = subInstanceMap[entry.arguments].get();
    } else {
      // Create a new instance by the arguments and save it in the map.
      pInstance = subInstanceMap.insert({entry.arguments,
        std::make_unique<CompilerInstance>()}).first->getValue().get();
      SmallVector<const char*, 4> args;
      llvm::cl::TokenizeGNUCommandLine(entry.arguments, saver, args);
      CompilerInvocation subInvok = invok;
      pInstance->addDiagnosticConsumer(&FDC);
      if (subInvok.parseArgs(args, diags)) {
        instance.getDiags().diagnose(SourceLoc(), diag::scanner_arguments_invalid,
                                     entry.arguments);
        return true;
      }
      if (pInstance->setup(subInvok)) {
        instance.getDiags().diagnose(SourceLoc(), diag::scanner_arguments_invalid,
                                     entry.arguments);
        return true;
      }
    }
    assert(pInstance);
    // Scan using the chosen compiler instance.
    if (scanModuleDependencies(*pInstance, entry.moduleName, !entry.isSwift,
                               entry.outputPath)) {
      return true;
    }
  }
  return false;
}

bool swift::scanDependencies(CompilerInstance &instance) {
  ASTContext &Context = instance.getASTContext();
  ModuleDecl *mainModule = instance.getMainModule();
  const CompilerInvocation &invocation = instance.getInvocation();
  const FrontendOptions &opts = invocation.getFrontendOptions();

  std::string path = opts.InputsAndOutputs.getSingleOutputFilename();
  std::error_code EC;
  llvm::raw_fd_ostream out(path, EC, llvm::sys::fs::F_None);

  if (out.has_error() || EC) {
    Context.Diags.diagnose(SourceLoc(), diag::error_opening_output, path,
                           EC.message());
    out.clear_error();
    return true;
  }

  // Main module file name.
  auto newExt = file_types::getExtension(file_types::TY_SwiftModuleFile);
  llvm::SmallString<32> mainModulePath = mainModule->getName().str();
  llvm::sys::path::replace_extension(mainModulePath, newExt);

  std::string apinotesVer = (llvm::Twine("-fapinotes-swift-version=")
    + instance.getASTContext().LangOpts.EffectiveLanguageVersion
      .asAPINotesVersionString()).str();
  // Compute the dependencies of the main module.
  auto mainDependencies =
    ModuleDependencies::forMainSwiftModule({
      // ExtraPCMArgs
      "-Xcc", "-target", "-Xcc", instance.getASTContext().LangOpts.Target.str(),
      "-Xcc", apinotesVer
    });
  {
    llvm::StringSet<> alreadyAddedModules;
    for (auto fileUnit : mainModule->getFiles()) {
      auto sf = dyn_cast<SourceFile>(fileUnit);
      if (!sf)
        continue;

      mainDependencies.addModuleDependencies(*sf, alreadyAddedModules);
    }

    const auto &importInfo = mainModule->getImplicitImportInfo();

    // Swift standard library.
    switch (importInfo.StdlibKind) {
    case ImplicitStdlibKind::None:
    case ImplicitStdlibKind::Builtin:
      break;

    case ImplicitStdlibKind::Stdlib:
      mainDependencies.addModuleDependency("Swift", &alreadyAddedModules);
      break;
    }

    // Add any implicit module names.
    for (const auto &moduleName : importInfo.ModuleNames) {
      mainDependencies.addModuleDependency(moduleName.str(), &alreadyAddedModules);
    }

    // Already-loaded, implicitly imported module names.
    for (const auto &module : importInfo.AdditionalModules) {
      mainDependencies.addModuleDependency(module.first->getNameStr(), &alreadyAddedModules);
    }

    // Add the bridging header.
    if (!importInfo.BridgingHeaderPath.empty()) {
      mainDependencies.addBridgingHeader(importInfo.BridgingHeaderPath);
    }

    // If we are to import the underlying Clang module of the same name,
    // add a dependency with the same name to trigger the search.
    if (importInfo.ShouldImportUnderlyingModule) {
      mainDependencies.addModuleDependency(mainModule->getName().str(),
                                           &alreadyAddedModules);
    }
  }

  // Add the main module.
  StringRef mainModuleName = mainModule->getNameStr();
  llvm::SetVector<ModuleDependencyID, std::vector<ModuleDependencyID>,
                  std::set<ModuleDependencyID>> allModules;
  
  allModules.insert({mainModuleName.str(), mainDependencies.getKind()});

  // Create the module dependency cache.
  ModuleDependenciesCache cache;
  cache.recordDependencies(mainModuleName, std::move(mainDependencies),
                           ModuleDependenciesKind::Swift);

  auto &ctx = instance.getASTContext();
  auto ModuleCachePath = getModuleCachePathFromClang(ctx
    .getClangModuleLoader()->getClangInstance());
  auto &FEOpts = instance.getInvocation().getFrontendOptions();
  ModuleInterfaceLoaderOptions LoaderOpts(FEOpts);
  InterfaceSubContextDelegateImpl ASTDelegate(ctx.SourceMgr, ctx.Diags,
                                              ctx.SearchPathOpts, ctx.LangOpts,
                                              ctx.ClangImporterOpts,
                                              LoaderOpts,
                                              /*buildModuleCacheDirIfAbsent*/false,
                                              ModuleCachePath,
                                              FEOpts.PrebuiltModuleCachePath,
                                              FEOpts.SerializeModuleInterfaceDependencyHashes,
                                              FEOpts.shouldTrackSystemDependencies());

  // Explore the dependencies of every module.
  for (unsigned currentModuleIdx = 0;
       currentModuleIdx < allModules.size();
       ++currentModuleIdx) {
    auto module = allModules[currentModuleIdx];
    auto discoveredModules =
        resolveDirectDependencies(instance, module, cache, ASTDelegate);
    allModules.insert(discoveredModules.begin(), discoveredModules.end());
  }

  // We have all explicit imports now, resolve cross import overlays.
  discoverCrosssImportOverlayDependencies(instance, mainModuleName,
      /*All transitive dependencies*/allModules.getArrayRef().slice(1), cache,
      ASTDelegate, [&](ModuleDependencyID id) {
    allModules.insert(id);
  });

  // Dignose cycle in dependency graph.
  if (diagnoseCycle(instance, cache, /*MainModule*/allModules.front(), ASTDelegate))
    return true;

  // Write out the JSON description.
  writeJSON(out, instance, cache, ASTDelegate, allModules.getArrayRef());

  // Update the dependency tracker.
  if (auto depTracker = instance.getDependencyTracker()) {
    for (auto module : allModules) {
      auto deps = cache.findDependencies(module.first, module.second);
      if (!deps)
        continue;

      if (auto swiftDeps = deps->getAsSwiftModule()) {
        if (auto swiftInterfaceFile = swiftDeps->swiftInterfaceFile)
          depTracker->addDependency(*swiftInterfaceFile, /*IsSystem=*/false);
        for (const auto &sourceFile : swiftDeps->sourceFiles)
          depTracker->addDependency(sourceFile, /*IsSystem=*/false);
        for (const auto &bridgingSourceFile : swiftDeps->bridgingSourceFiles)
          depTracker->addDependency(bridgingSourceFile, /*IsSystem=*/false);
      } else if (auto clangDeps = deps->getAsClangModule()) {
        if (!clangDeps->moduleMapFile.empty())
          depTracker->addDependency(clangDeps->moduleMapFile, /*IsSystem=*/false);
        for (const auto &sourceFile : clangDeps->fileDependencies)
          depTracker->addDependency(sourceFile, /*IsSystem=*/false);
      }
    }
  }

  // This process succeeds regardless of whether any errors occurred.
  // FIXME: We shouldn't need this, but it's masking bugs in our scanning
  // logic where we don't create a fresh context when scanning Swift interfaces
  // that includes their own command-line flags.
  Context.Diags.resetHadAnyError();
  return false;
}
