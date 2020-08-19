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
#include "swift/Subsystems.h"
using namespace swift;
using llvm::ErrorOr;

namespace {

/// A module "loader" that looks for .swiftinterface and .swiftmodule files
/// for the purpose of determining dependencies, but does not attempt to
/// load the module files.
class ModuleDependencyScanner : public SerializedModuleLoaderBase {
  /// The module we're scanning dependencies of.
  Identifier moduleName;

  /// Scan the given interface file to determine dependencies.
  ErrorOr<ModuleDependencies> scanInterfaceFile(
      Twine moduleInterfacePath, bool isFramework);

  InterfaceSubContextDelegate &astDelegate;
public:
  Optional<ModuleDependencies> dependencies;

  /// Describes the kind of dependencies this scanner is able to identify
  ModuleDependenciesKind dependencyKind;

  ModuleDependencyScanner(
      ASTContext &ctx, ModuleLoadingMode LoadMode, Identifier moduleName,
      InterfaceSubContextDelegate &astDelegate,
      ModuleDependenciesKind dependencyKind = ModuleDependenciesKind::Swift)
      : SerializedModuleLoaderBase(ctx, nullptr, LoadMode,
                                   /*IgnoreSwiftSourceInfoFile=*/true),
        moduleName(moduleName), astDelegate(astDelegate),
        dependencyKind(dependencyKind) {}

  virtual std::error_code findModuleFilesInDirectory(
      AccessPathElem ModuleID,
      const SerializedModuleBaseName &BaseName,
      SmallVectorImpl<char> *ModuleInterfacePath,
      std::unique_ptr<llvm::MemoryBuffer> *ModuleBuffer,
      std::unique_ptr<llvm::MemoryBuffer> *ModuleDocBuffer,
      std::unique_ptr<llvm::MemoryBuffer> *ModuleSourceInfoBuffer,
      bool IsFramework) override {
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

  virtual void collectVisibleTopLevelModuleNames(
      SmallVectorImpl<Identifier> &names) const override {
    llvm_unreachable("Not used");
  }
};

/// A ModuleLoader that loads placeholder dependency module stubs specified in
/// -placeholder-dependency-module-map-file
/// This loader is used only in dependency scanning to inform the scanner that a
/// set of modules constitute placeholder dependencies that are not visible to the
/// scanner but will nevertheless be provided by the scanner's clients.
/// This "loader" will not attempt to load any module files.
class PlaceholderSwiftModuleScanner : public ModuleDependencyScanner {
  /// Scan the given placeholder module map
  void parsePlaceholderModuleMap(StringRef fileName) {
    ExplicitModuleMapParser parser(Allocator);
    auto result =
      parser.parseSwiftExplicitModuleMap(fileName, PlaceholderDependencyModuleMap);
    if (result == std::errc::invalid_argument) {
      Ctx.Diags.diagnose(SourceLoc(),
                         diag::placeholder_dependency_module_map_corrupted,
                         fileName);
    }
    else if (result == std::errc::no_such_file_or_directory) {
      Ctx.Diags.diagnose(SourceLoc(),
                         diag::placeholder_dependency_module_map_missing,
                         fileName);
    }
  }

  llvm::StringMap<ExplicitModuleInfo> PlaceholderDependencyModuleMap;
  llvm::BumpPtrAllocator Allocator;

public:
  PlaceholderSwiftModuleScanner(ASTContext &ctx, ModuleLoadingMode LoadMode,
                                Identifier moduleName,
                                StringRef PlaceholderDependencyModuleMap,
                                InterfaceSubContextDelegate &astDelegate)
      : ModuleDependencyScanner(ctx, LoadMode, moduleName, astDelegate,
                                ModuleDependenciesKind::SwiftPlaceholder) {

    // FIXME: Find a better place for this map to live, to avoid
    // doing the parsing on every module.
    if (!PlaceholderDependencyModuleMap.empty()) {
      parsePlaceholderModuleMap(PlaceholderDependencyModuleMap);
    }
  }

  std::error_code findModuleFilesInDirectory(
      AccessPathElem ModuleID, const SerializedModuleBaseName &BaseName,
      SmallVectorImpl<char> *ModuleInterfacePath,
      std::unique_ptr<llvm::MemoryBuffer> *ModuleBuffer,
      std::unique_ptr<llvm::MemoryBuffer> *ModuleDocBuffer,
      std::unique_ptr<llvm::MemoryBuffer> *ModuleSourceInfoBuffer,
      bool IsFramework) override {
    StringRef moduleName = ModuleID.Item.str();
    auto it = PlaceholderDependencyModuleMap.find(moduleName);
    // If no placeholder module stub path is given matches the name, return with an
    // error code.
    if (it == PlaceholderDependencyModuleMap.end()) {
      return std::make_error_code(std::errc::not_supported);
    }
    auto &moduleInfo = it->getValue();
    assert(!moduleInfo.moduleBuffer &&
           "Placeholder dependency module stubs cannot have an associated buffer");

    auto dependencies = ModuleDependencies::forPlaceholderSwiftModuleStub(
        moduleInfo.modulePath, moduleInfo.moduleDocPath,
        moduleInfo.moduleSourceInfoPath);
    this->dependencies = std::move(dependencies);
    return std::error_code{};
  }
};
} // namespace

static std::vector<std::string> getCompiledCandidates(ASTContext &ctx,
                                                      StringRef moduleName,
                                                      StringRef interfacePath) {
  return static_cast<SerializedModuleLoaderBase*>(ctx
    .getModuleInterfaceLoader())->getCompiledModuleCandidatesForInterface(
      moduleName.str(), interfacePath);
}

ErrorOr<ModuleDependencies> ModuleDependencyScanner::scanInterfaceFile(
    Twine moduleInterfacePath, bool isFramework) {
  // Create a module filename.
  // FIXME: Query the module interface loader to determine an appropriate
  // name for the module, which includes an appropriate hash.
  auto newExt = file_types::getExtension(file_types::TY_SwiftModuleFile);
  llvm::SmallString<32> modulePath = moduleName.str();
  llvm::sys::path::replace_extension(modulePath, newExt);
  Optional<ModuleDependencies> Result;
  std::error_code code =
    astDelegate.runInSubContext(moduleName.str(),
                                              moduleInterfacePath.str(),
                                              StringRef(),
                                              SourceLoc(),
                [&](ASTContext &Ctx, ModuleDecl *mainMod,
                    ArrayRef<StringRef> Args,
                    ArrayRef<StringRef> PCMArgs, StringRef Hash) {
    assert(mainMod);
    std::string InPath = moduleInterfacePath.str();
    auto compiledCandidates = getCompiledCandidates(Ctx, moduleName.str(),
                                                    InPath);
    Result = ModuleDependencies::forSwiftInterface(InPath,
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
    auto moduleDecl = ModuleDecl::create(moduleName, Ctx);
    auto sourceFile = new (Ctx) SourceFile(
        *moduleDecl, SourceFileKind::Interface, bufferID);

    // Walk the source file to find the import declarations.
    llvm::StringSet<> alreadyAddedModules;
    Result->addModuleDependencies(*sourceFile, alreadyAddedModules);

    // Collect implicitly imported modules in case they are not explicitly
    // printed in the interface file, e.g. SwiftOnoneSupport.
    auto &imInfo = mainMod->getImplicitImportInfo();
    for (auto name: imInfo.ModuleNames) {
      Result->addModuleDependency(name.str(), &alreadyAddedModules);
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
  // Check whether we've cached this result.
  if (auto found = cache.findDependencies(
          moduleName, ModuleDependenciesKind::Swift))
    return found;
  if (auto found =
          cache.findDependencies(moduleName, ModuleDependenciesKind::SwiftPlaceholder))
    return found;

  auto moduleId = Ctx.getIdentifier(moduleName);
  // Instantiate dependency scanning "loaders".
  SmallVector<std::unique_ptr<ModuleDependencyScanner>, 2> scanners;
  scanners.push_back(std::make_unique<ModuleDependencyScanner>(
      Ctx, LoadMode, moduleId, delegate));
  scanners.push_back(std::make_unique<PlaceholderSwiftModuleScanner>(
      Ctx, LoadMode, moduleId, Ctx.SearchPathOpts.PlaceholderDependencyModuleMap,
      delegate));

  // Check whether there is a module with this name that we can import.
  for (auto &scanner : scanners) {
    if (scanner->canImportModule({moduleId, SourceLoc()})) {
      // Record the dependencies.
      cache.recordDependencies(moduleName, *(scanner->dependencies),
                               scanner->dependencyKind);
      return std::move(scanner->dependencies);
    }
  }

  return None;
}
