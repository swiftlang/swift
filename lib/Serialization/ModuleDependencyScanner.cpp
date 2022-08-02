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
#include "swift/AST/DiagnosticSuppression.h"
#include "swift/AST/DiagnosticsFrontend.h"
#include "swift/AST/ModuleDependencies.h"
#include "swift/AST/SourceFile.h"
#include "swift/AST/TypeCheckRequests.h"
#include "swift/Basic/FileTypes.h"
#include "swift/Frontend/ModuleInterfaceLoader.h"
#include "swift/Serialization/SerializedModuleLoader.h"
#include "swift/Serialization/ModuleDependencyScanner.h"
#include "swift/Subsystems.h"
using namespace swift;
using llvm::ErrorOr;


std::error_code ModuleDependencyScanner::findModuleFilesInDirectory(
                                      ImportPath::Element ModuleID,
                                      const SerializedModuleBaseName &BaseName,
                                      SmallVectorImpl<char> *ModuleInterfacePath,
                                      std::unique_ptr<llvm::MemoryBuffer> *ModuleBuffer,
                                      std::unique_ptr<llvm::MemoryBuffer> *ModuleDocBuffer,
                                      std::unique_ptr<llvm::MemoryBuffer> *ModuleSourceInfoBuffer,
                                      bool skipBuildingInterface, bool IsFramework) {
  using namespace llvm::sys;

  auto &fs = *Ctx.SourceMgr.getFileSystem();

  auto ModPath = BaseName.getName(file_types::TY_SwiftModuleFile);
  auto InPath = BaseName.getName(file_types::TY_SwiftModuleInterfaceFile);

  if (LoadMode == ModuleLoadingMode::OnlySerialized || !fs.exists(InPath)) {
    if (fs.exists(ModPath)) {
      // The module file will be loaded directly.
      auto dependencies = scanModuleFile(ModPath);
      if (dependencies) {
        this->dependencies = std::move(dependencies.get());
        return std::error_code();
      }
      return dependencies.getError();
    } else {
      return std::make_error_code(std::errc::no_such_file_or_directory);
    }
  }
  assert(fs.exists(InPath));

  // Use the private interface file if exits.
  auto PrivateInPath =
  BaseName.getName(file_types::TY_PrivateSwiftModuleInterfaceFile);
  if (fs.exists(PrivateInPath)) {
    InPath = PrivateInPath;
  }
  auto dependencies = scanInterfaceFile(InPath, IsFramework);
  if (dependencies) {
    this->dependencies = std::move(dependencies.get());
    return std::error_code();
  }

  return dependencies.getError();
}

bool PlaceholderSwiftModuleScanner::findModule(
    ImportPath::Element moduleID, SmallVectorImpl<char> *moduleInterfacePath,
    std::unique_ptr<llvm::MemoryBuffer> *moduleBuffer,
    std::unique_ptr<llvm::MemoryBuffer> *moduleDocBuffer,
    std::unique_ptr<llvm::MemoryBuffer> *moduleSourceInfoBuffer,
    bool skipBuildingInterface, bool &isFramework, bool &isSystemModule) {
  StringRef moduleName = Ctx.getRealModuleName(moduleID.Item).str();
  auto it = PlaceholderDependencyModuleMap.find(moduleName);
  if (it == PlaceholderDependencyModuleMap.end()) {
    return false;
  }
  auto &moduleInfo = it->getValue();
  auto dependencies = ModuleDependencies::forPlaceholderSwiftModuleStub(
      moduleInfo.modulePath, moduleInfo.moduleDocPath,
      moduleInfo.moduleSourceInfoPath);
  this->dependencies = std::move(dependencies);
  return true;
}

static std::vector<std::string> getCompiledCandidates(ASTContext &ctx,
                                                      StringRef moduleName,
                                                      StringRef interfacePath) {
  return ctx.getModuleInterfaceChecker()->getCompiledModuleCandidatesForInterface(
      moduleName.str(), interfacePath);
}

ErrorOr<ModuleDependencies> ModuleDependencyScanner::scanInterfaceFile(
    Twine moduleInterfacePath, bool isFramework) {
  // Create a module filename.
  // FIXME: Query the module interface loader to determine an appropriate
  // name for the module, which includes an appropriate hash.
  auto newExt = file_types::getExtension(file_types::TY_SwiftModuleFile);
  auto realModuleName = Ctx.getRealModuleName(moduleName);
  llvm::SmallString<32> modulePath = realModuleName.str();
  llvm::sys::path::replace_extension(modulePath, newExt);
  Optional<ModuleDependencies> Result;
  std::error_code code =
    astDelegate.runInSubContext(realModuleName.str(),
                                              moduleInterfacePath.str(),
                                              StringRef(),
                                              SourceLoc(),
                                              false,
                [&](ASTContext &Ctx, ModuleDecl *mainMod,
                    ArrayRef<StringRef> Args,
                    ArrayRef<StringRef> PCMArgs, StringRef Hash) {
    assert(mainMod);
    std::string InPath = moduleInterfacePath.str();
    auto compiledCandidates = getCompiledCandidates(Ctx, realModuleName.str(),
                                                    InPath);
    Result = ModuleDependencies::forSwiftInterfaceModule(InPath,
                                                   compiledCandidates,
                                                   Args,
                                                   PCMArgs,
                                                   Hash,
                                                   isFramework);
    // Open the interface file.
    auto &fs = *Ctx.SourceMgr.getFileSystem();
    auto interfaceBuf = fs.getBufferForFile(moduleInterfacePath);
    if (!interfaceBuf) {
      return interfaceBuf.getError();
    }

    // Create a source file.
    unsigned bufferID = Ctx.SourceMgr.addNewSourceBuffer(std::move(interfaceBuf.get()));
    auto moduleDecl = ModuleDecl::create(realModuleName, Ctx);
    auto sourceFile = new (Ctx) SourceFile(
        *moduleDecl, SourceFileKind::Interface, bufferID);

    // Walk the source file to find the import declarations.
    llvm::StringSet<> alreadyAddedModules;
    Result->addModuleDependencies(*sourceFile, alreadyAddedModules);

    // Collect implicitly imported modules in case they are not explicitly
    // printed in the interface file, e.g. SwiftOnoneSupport.
    auto &imInfo = mainMod->getImplicitImportInfo();
    for (auto import: imInfo.AdditionalUnloadedImports) {
      Result->addModuleDependency(import.module.getModulePath(), &alreadyAddedModules);
    }
    return std::error_code();
  });

  if (code) {
    return code;
  }
  return *Result;
}

Optional<ModuleDependencies> SerializedModuleLoaderBase::getModuleDependencies(
    StringRef moduleName, ModuleDependenciesCache &cache,
    InterfaceSubContextDelegate &delegate) {
  auto currentSearchPathSet = Ctx.getAllModuleSearchPathsSet();

  // Check whether we've cached this result.
  if (auto found = cache.findDependencies(
           moduleName,
           {ModuleDependenciesKind::SwiftInterface, currentSearchPathSet}))
    return found;
  if (auto found = cache.findDependencies(
           moduleName,
           {ModuleDependenciesKind::SwiftSource, currentSearchPathSet}))
    return found;
  if (auto found = cache.findDependencies(
            moduleName,
            {ModuleDependenciesKind::SwiftBinary, currentSearchPathSet}))
    return found;
  if (auto found = cache.findDependencies(
            moduleName,
            {ModuleDependenciesKind::SwiftPlaceholder, currentSearchPathSet}))
    return found;

  ImportPath::Module::Builder builder(Ctx, moduleName, /*separator=*/'.');
  auto modulePath = builder.get();
  auto moduleId = modulePath.front().Item;
  // Instantiate dependency scanning "loaders".
  SmallVector<std::unique_ptr<ModuleDependencyScanner>, 2> scanners;
  // Placeholder dependencies must be resolved first, to prevent the
  // ModuleDependencyScanner from first discovering artifacts of a previous
  // build. Such artifacts are captured as compiledModuleCandidates in the
  // dependency graph of the placeholder dependency module itself.
  // FIXME: submodules?
  scanners.push_back(std::make_unique<PlaceholderSwiftModuleScanner>(
      Ctx, LoadMode, moduleId, Ctx.SearchPathOpts.PlaceholderDependencyModuleMap,
      delegate));
  scanners.push_back(std::make_unique<ModuleDependencyScanner>(
      Ctx, LoadMode, moduleId, delegate));

  // Check whether there is a module with this name that we can import.
  assert(isa<PlaceholderSwiftModuleScanner>(scanners[0].get()) &&
         "Expected PlaceholderSwiftModuleScanner as the first dependency scanner loader.");
  for (auto &scanner : scanners) {
    if (scanner->canImportModule(modulePath, llvm::VersionTuple(), false)) {
      // Record the dependencies.
      cache.recordDependencies(moduleName, *(scanner->dependencies));
      return std::move(scanner->dependencies);
    }
  }

  return None;
}
