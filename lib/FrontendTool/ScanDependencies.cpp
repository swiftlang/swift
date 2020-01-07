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
#include "swift/Basic/Defer.h"
#include "swift/Basic/LLVM.h"
#include "swift/Basic/STLExtras.h"
#include "swift/Frontend/FrontendOptions.h"
#include "clang/Basic/Module.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/StringSet.h"
#include "llvm/Support/FileSystem.h"
#include <set>

using namespace swift;

/// Find all of the imported Clang modules starting with the given module name.
static void findAllImportedClangModules(ASTContext &ctx, StringRef moduleName,
                                        ModuleDependenciesCache &cache,
                                        std::vector<std::string> &allModules,
                                        llvm::StringSet<> &knownModules) {
  if (!knownModules.insert(moduleName).second)
    return;
  allModules.push_back(moduleName);

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
    ASTContext &ctx, ModuleDependencyID module,
    ModuleDependenciesCache &cache) {
  auto knownDependencies = *cache.findDependencies(module.first, module.second);
  auto isSwift = knownDependencies.isSwiftModule();

  // Find the dependencies of every module this module directly depends on.
  std::vector<ModuleDependencyID> result;
  for (auto dependsOn : knownDependencies.getModuleDependencies()) {
    // Figure out what kind of module we need.
    bool onlyClangModule = !isSwift || module.first == dependsOn;

    // Retrieve the dependencies for this module.
    if (auto found = ctx.getModuleDependencies(
            dependsOn, onlyClangModule, cache)) {
      result.push_back({dependsOn, found->getKind()});
    }
  }

  // For a Swift module, look for overlays for each of the Clang modules it
  // depends on.
  if (isSwift) {
    // Find all of the Clang modules this Swift module depends on.
    std::vector<std::string> allClangModules;
    llvm::StringSet<> knownModules;
    for (const auto &dep : result) {
      if (dep.second != ModuleDependenciesKind::Clang)
        continue;

      findAllImportedClangModules(ctx, dep.first, cache, allClangModules,
                                  knownModules);
    }

    // Look for overlays for each of the Clang modules.
    for (const auto &clangDep : allClangModules) {
      if (auto found = ctx.getModuleDependencies(
              clangDep, /*onlyClangModule=*/false, cache)) {
        if (found->getKind() == ModuleDependenciesKind::Swift)
          result.push_back({clangDep, found->getKind()});
      }
    }
  }

  return result;
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

  /// Write a module identifier.
  void writeJSONValue(llvm::raw_ostream &out,
                      const ModuleDependencyID &module,
                      unsigned indentLevel) {
    out << "{\n";

    writeJSONSingleField(
        out,
        module.second == ModuleDependenciesKind::Swift ? "swift" : "clang",
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
                      ASTContext &ctx,
                      ModuleDependenciesCache &cache,
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
    auto moduleDeps = *cache.findDependencies(module.first, module.second);

    // The module we are describing.
    out.indent(2 * 2);
    writeJSONValue(out, module, 2);
    out << ",\n";

    out.indent(2 * 2);
    out << "{\n";

    // Module path.
    const char *modulePathSuffix =
        moduleDeps.isSwiftModule() ? ".swiftmodule" : ".pcm";
    std::string modulePath = module.first + modulePathSuffix;
    writeJSONSingleField(out, "modulePath", modulePath, /*indentLevel=*/3,
                         /*trailingComma=*/true);

    // Source files.
    auto swiftDeps = moduleDeps.getAsSwiftModule();
    auto clangDeps = moduleDeps.getAsClangModule();
    if (swiftDeps) {
      writeJSONSingleField(out, "sourceFiles", swiftDeps->sourceFiles, 3,
                     /*trailingComma=*/true);
    } else {
      writeJSONSingleField(out, "sourceFiles", clangDeps->fileDependencies, 3,
                           /*trailingComma=*/true);
    }

    // Direct dependencies.
    auto directDependencies = resolveDirectDependencies(
        ctx, ModuleDependencyID(module.first, module.second), cache);
    writeJSONSingleField(out, "directDependencies", directDependencies,
                         3, /*trailingComma=*/true);

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
      }

      /// Bridging header and its source file dependencies, if any.
      if (swiftDeps->bridgingHeaderFile) {
        writeJSONSingleField(out, "bridgingHeaderPath",
                             *swiftDeps->bridgingHeaderFile, 5,
                             /*trailingComma=*/true);
      }
      writeJSONSingleField(out, "bridgingSourceFiles",
                           swiftDeps->bridgingSourceFiles, 5,
                           /*trailingComma=*/false);
    } else {
      out << "\"clang\": {\n";

      // Module map file.
      writeJSONSingleField(out, "moduleMapPath",
                           clangDeps->moduleMapFile, 5,
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

bool swift::scanDependencies(ASTContext &Context, ModuleDecl *mainModule,
                             DependencyTracker *depTracker,
                             const FrontendOptions &opts) {

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

  // Compute the dependencies of the main module.
  auto mainDependencies =
      ModuleDependencies::forSwiftModule(mainModulePath.str());
  {
    llvm::StringSet<> alreadyAddedModules;
    for (auto fileUnit : mainModule->getFiles()) {
      auto sf = dyn_cast<SourceFile>(fileUnit);
      if (!sf)
        continue;

      mainDependencies.addModuleDependencies(*sf, alreadyAddedModules);
    }

    // Add the bridging header.
    StringRef implicitHeaderPath = opts.ImplicitObjCHeaderPath;
    if (!implicitHeaderPath.empty()) {
      mainDependencies.addBridgingHeader(implicitHeaderPath);
    }

    // If we are to import the underlying Clang module of the same name,
    // add a dependency with the same name to trigger the search.
    if (opts.ImportUnderlyingModule) {
      mainDependencies.addModuleDependency(mainModule->getName().str(),
                                           alreadyAddedModules);
    }
  }

  // Add the main module.
  StringRef mainModuleName = mainModule->getNameStr();
  llvm::SetVector<ModuleDependencyID, std::vector<ModuleDependencyID>,
                  std::set<ModuleDependencyID>> allModules;
  
  allModules.insert({mainModuleName, mainDependencies.getKind()});

  // Create the module dependency cache.
  ModuleDependenciesCache cache;
  cache.recordDependencies(mainModuleName, std::move(mainDependencies),
                           ModuleDependenciesKind::Swift);

  // Explore the dependencies of every module.
  for (unsigned currentModuleIdx = 0;
       currentModuleIdx < allModules.size();
       ++currentModuleIdx) {
    auto module = allModules[currentModuleIdx];
    auto discoveredModules =
        resolveDirectDependencies(Context, module, cache);
    allModules.insert(discoveredModules.begin(), discoveredModules.end());
  }

  // Write out the JSON description.
  writeJSON(out, Context, cache, allModules.getArrayRef());

  // Update the dependency tracker.
  if (depTracker) {
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
      } else {
        auto clangDeps = deps->getAsClangModule();
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
