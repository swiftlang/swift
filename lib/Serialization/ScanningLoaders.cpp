//===---------- ScanningLoaders.cpp - Compute module dependencies ---------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/Serialization/ScanningLoaders.h"
#include "ModuleFile.h"
#include "ModuleFileSharedCore.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/DiagnosticSuppression.h"
#include "swift/AST/DiagnosticsFrontend.h"
#include "swift/AST/DiagnosticsSema.h"
#include "swift/AST/ModuleDependencies.h"
#include "swift/AST/ModuleLoader.h"
#include "swift/AST/SourceFile.h"
#include "swift/AST/TypeCheckRequests.h"
#include "swift/Basic/Assertions.h"
#include "swift/Basic/Defer.h"
#include "swift/Basic/FileTypes.h"
#include "swift/Basic/PrettyStackTrace.h"
#include "swift/Frontend/ModuleInterfaceLoader.h"
#include "swift/Serialization/SerializedModuleLoader.h"
#include "swift/Subsystems.h"
#include "llvm/ADT/IntrusiveRefCntPtr.h"
#include "llvm/ADT/SetOperations.h"
#include "llvm/Support/PrefixMapper.h"
#include "llvm/Support/Threading.h"
#include "llvm/Support/VirtualFileSystem.h"
#include <algorithm>
#include <system_error>

using namespace swift;

std::error_code SwiftModuleScanner::findModuleFilesInDirectory(
    ImportPath::Element ModuleID, const SerializedModuleBaseName &BaseName,
    SmallVectorImpl<char> *ModuleInterfacePath,
    SmallVectorImpl<char> *ModuleInterfaceSourcePath,
    std::unique_ptr<llvm::MemoryBuffer> *ModuleBuffer,
    std::unique_ptr<llvm::MemoryBuffer> *ModuleDocBuffer,
    std::unique_ptr<llvm::MemoryBuffer> *ModuleSourceInfoBuffer,
    bool skipBuildingInterface, bool IsFramework,
    bool isTestableDependencyLookup) {
  using namespace llvm::sys;

  auto &fs = *Ctx.SourceMgr.getFileSystem();

  auto ModPath = BaseName.getName(file_types::TY_SwiftModuleFile);
  auto InPath = BaseName.findInterfacePath(fs, Ctx);

  // Lookup binary module if it is a testable lookup, or only binary module
  // lookup, or interface file does not exist.
  if (LoadMode == ModuleLoadingMode::OnlySerialized ||
      isTestableDependencyLookup || !InPath) {
    if (fs.exists(ModPath)) {
      // The module file will be loaded directly.
      auto dependencyInfo = scanBinaryModuleFile(
          ModuleID.Item, ModPath, IsFramework, isTestableDependencyLookup,
          /* isCandidateForTextualModule */ false);
      if (dependencyInfo) {
        this->foundDependencyInfo = std::move(dependencyInfo.get());
        return std::error_code();
      }
      return dependencyInfo.getError();
    }
    return std::make_error_code(std::errc::no_such_file_or_directory);
  }
  assert(InPath);

  auto dependencyInfo = scanInterfaceFile(ModuleID.Item, *InPath, IsFramework,
                                          isTestableDependencyLookup);
  if (dependencyInfo) {
    this->foundDependencyInfo = std::move(dependencyInfo.get());
    return std::error_code();
  }
  return dependencyInfo.getError();
}

static std::vector<std::string> getCompiledCandidates(ASTContext &ctx,
                                                      StringRef moduleName,
                                                      StringRef interfacePath) {
  return ctx.getModuleInterfaceChecker()
      ->getCompiledModuleCandidatesForInterface(moduleName.str(),
                                                interfacePath);
}

llvm::ErrorOr<ModuleDependencyInfo>
SwiftModuleScanner::scanInterfaceFile(Identifier moduleID,
                                      Twine moduleInterfacePath,
                                      bool isFramework, bool isTestableImport) {
  // Create a module filename.
  // FIXME: Query the module interface loader to determine an appropriate
  // name for the module, which includes an appropriate hash.
  auto newExt = file_types::getExtension(file_types::TY_SwiftModuleFile);
  auto realModuleName = Ctx.getRealModuleName(moduleID);
  StringRef sdkPath = Ctx.SearchPathOpts.getSDKPath();
  llvm::SmallString<32> modulePath = realModuleName.str();
  llvm::sys::path::replace_extension(modulePath, newExt);
  auto ScannerPackageName = Ctx.LangOpts.PackageName;
  std::optional<ModuleDependencyInfo> Result;
  std::error_code code = astDelegate.runInSubContext(
      realModuleName.str(), moduleInterfacePath.str(), sdkPath,
      Ctx.SearchPathOpts.getSysRoot(), StringRef(), SourceLoc(),
      [&](ASTContext &Ctx, ModuleDecl *mainMod, ArrayRef<StringRef> BaseArgs,
          StringRef Hash, StringRef UserModVer) {
        assert(mainMod);
        std::string InPath = moduleInterfacePath.str();
        auto compiledCandidates =
            getCompiledCandidates(Ctx, realModuleName.str(), InPath);
        if (!compiledCandidates.empty() &&
            Ctx.SearchPathOpts.ScannerModuleValidation) {
          assert(compiledCandidates.size() == 1 &&
                 "Should only have 1 candidate module");
          auto BinaryDep = scanBinaryModuleFile(
              moduleID, compiledCandidates[0], isFramework, isTestableImport,
              /* isCandidateForTextualModule */ true);
          if (BinaryDep) {
            Result = *BinaryDep;
            return std::error_code();
          }

          // If return no such file, just fallback to use interface.
          if (BinaryDep.getError() != std::errc::no_such_file_or_directory)
            return BinaryDep.getError();
        }

        std::vector<std::string> Args(BaseArgs.begin(), BaseArgs.end());
        // Add explicit Swift dependency compilation flags
        Args.push_back("-explicit-interface-module-build");
        Args.push_back("-disable-implicit-swift-modules");

        // Handle clang arguments. For caching build, all arguments are passed
        // with `-direct-clang-cc1-module-build`.
        llvm::append_range(Args, swiftModuleClangCC1CommandLineArgs);

        for (const auto &candidate : compiledCandidates) {
          Args.push_back("-candidate-module-file");
          Args.push_back(candidate);
        }

        // Open the interface file.
        auto &fs = *Ctx.SourceMgr.getFileSystem();
        auto interfaceBuf = fs.getBufferForFile(moduleInterfacePath);
        if (!interfaceBuf) {
          return interfaceBuf.getError();
        }

        // Create a source file.
        unsigned bufferID =
            Ctx.SourceMgr.addNewSourceBuffer(std::move(interfaceBuf.get()));
        auto moduleDecl = ModuleDecl::createEmpty(realModuleName, Ctx);

        SourceFile::ParsingOptions parsingOpts;
        auto sourceFile = new (Ctx) SourceFile(
            *moduleDecl, SourceFileKind::Interface, bufferID, parsingOpts);
        std::vector<StringRef> ArgsRefs(Args.begin(), Args.end());
        std::vector<StringRef> compiledCandidatesRefs(
            compiledCandidates.begin(), compiledCandidates.end());

        // If this interface specified '-autolink-force-load', add it to the
        // set of linked libraries for this module.
        std::vector<LinkLibrary> linkLibraries;
        if (llvm::find(ArgsRefs, "-autolink-force-load") != ArgsRefs.end()) {
          std::string linkName = realModuleName.str().str();
          auto linkNameArgIt = llvm::find(ArgsRefs, "-module-link-name");
          if (linkNameArgIt != ArgsRefs.end())
            linkName = *(linkNameArgIt + 1);
          linkLibraries.push_back(
              {linkName,
               isFramework ? LibraryKind::Framework : LibraryKind::Library,
               /*static=*/false, /*force_load=*/true});
        }
        bool isStatic = llvm::find(ArgsRefs, "-static") != ArgsRefs.end();

        Result = ModuleDependencyInfo::forSwiftInterfaceModule(
            InPath, compiledCandidatesRefs, ArgsRefs, {}, {}, linkLibraries,
            isFramework, isStatic, {}, /*module-cache-key*/ "", UserModVer);

        // Walk the source file to find the import declarations.
        llvm::StringSet<> alreadyAddedModules;
        Result->addModuleImports(*sourceFile, alreadyAddedModules,
                                 &Ctx.SourceMgr);

        // Collect implicitly imported modules in case they are not explicitly
        // printed in the interface file, e.g. SwiftOnoneSupport.
        auto &imInfo = mainMod->getImplicitImportInfo();
        for (auto import : imInfo.AdditionalUnloadedImports) {
          Result->addModuleImport(
              import.module.getModulePath(),
              import.options.contains(ImportFlags::Exported),
              import.accessLevel, &alreadyAddedModules, &Ctx.SourceMgr);
        }

        // If this is a dependency that belongs to the same package, and we have
        // not yet enabled Package Textual interfaces, scan the adjacent binary
        // module for package dependencies.
        if (!ScannerPackageName.empty() &&
            !Ctx.LangOpts.EnablePackageInterfaceLoad) {
          auto adjacentBinaryModule = std::find_if(
              compiledCandidates.begin(), compiledCandidates.end(),
              [moduleInterfacePath](const std::string &candidate) {
                return llvm::sys::path::parent_path(candidate) ==
                       llvm::sys::path::parent_path(moduleInterfacePath.str());
              });

          if (adjacentBinaryModule != compiledCandidates.end()) {
            auto adjacentBinaryModulePackageOnlyImports =
                getMatchingPackageOnlyImportsOfModule(
                    *adjacentBinaryModule, isFramework, isRequiredOSSAModules(),
                    Ctx.LangOpts.SDKName, Ctx.LangOpts.Target,
                    ScannerPackageName, Ctx.SourceMgr.getFileSystem().get(),
                    Ctx.SearchPathOpts.DeserializedPathRecoverer);

            if (!adjacentBinaryModulePackageOnlyImports)
              return adjacentBinaryModulePackageOnlyImports.getError();

            for (const auto &requiredImport :
                 *adjacentBinaryModulePackageOnlyImports)
              if (!alreadyAddedModules.contains(
                      requiredImport.importIdentifier))
                Result->addModuleImport(
                    requiredImport.importIdentifier, requiredImport.isExported,
                    requiredImport.accessLevel, &alreadyAddedModules);
          }
        }

        return std::error_code();
      });

  if (code) {
    return code;
  }
  return *Result;
}

llvm::ErrorOr<ModuleDependencyInfo> SwiftModuleScanner::scanBinaryModuleFile(
    Identifier moduleID, Twine binaryModulePath, bool isFramework,
    bool isTestableImport, bool isCandidateForTextualModule) {
  const std::string moduleDocPath;
  const std::string sourceInfoPath;

  // Read and valid module.
  auto moduleBuf =
      Ctx.SourceMgr.getFileSystem()->getBufferForFile(binaryModulePath);
  if (!moduleBuf)
    return moduleBuf.getError();

  std::shared_ptr<const ModuleFileSharedCore> loadedModuleFile;
  serialization::ValidationInfo loadInfo = ModuleFileSharedCore::load(
      "", "", std::move(moduleBuf.get()), nullptr, nullptr, isFramework,
      isRequiredOSSAModules(), Ctx.LangOpts.SDKName, Ctx.LangOpts.Target,
      Ctx.SearchPathOpts.DeserializedPathRecoverer, loadedModuleFile);

  if (Ctx.SearchPathOpts.ScannerModuleValidation) {
    // If failed to load, just ignore and return do not found.
    if (auto loadFailureReason = invalidModuleReason(loadInfo.status)) {
      // If no textual interface was found, then for this dependency
      // scanning query this was *the* module discovered, which means
      // it would be helpful to let the user know why the scanner
      // was not able to use it because the scan will ultimately fail to
      // resolve this dependency due to this incompatibility.
      incompatibleCandidates.push_back({binaryModulePath.str(),
                                        loadFailureReason.value()});
      return std::make_error_code(std::errc::no_such_file_or_directory);
    }

    if (isTestableImport && !loadedModuleFile->isTestable()) {
      incompatibleCandidates.push_back({binaryModulePath.str(),
                                        "module built without '-enable-testing'"});
      return std::make_error_code(std::errc::no_such_file_or_directory);
    }
  }

  auto binaryModuleImports =
      getImportsOfModule(*loadedModuleFile, ModuleLoadingBehavior::Required,
                         Ctx.LangOpts.PackageName, isTestableImport);

  // Lookup optional imports of this module also
  auto binaryModuleOptionalImports =
      getImportsOfModule(*loadedModuleFile, ModuleLoadingBehavior::Optional,
                         Ctx.LangOpts.PackageName, isTestableImport);

  std::vector<LinkLibrary> linkLibraries;
  {
    linkLibraries.reserve(loadedModuleFile->getLinkLibraries().size());
    llvm::copy(loadedModuleFile->getLinkLibraries(),
               std::back_inserter(linkLibraries));
    if (loadedModuleFile->isFramework())
      linkLibraries.emplace_back(loadedModuleFile->getName(),
                                 LibraryKind::Framework,
                                 loadedModuleFile->isStaticLibrary());
  }

  // Attempt to resolve the module's defining .swiftinterface path
  std::string definingModulePath =
      loadedModuleFile->resolveModuleDefiningFilePath(
          Ctx.SearchPathOpts.getSDKPath());

  std::string userModuleVer =
      loadedModuleFile->getUserModuleVersion().getAsString();
  std::vector<serialization::SearchPath> serializedSearchPaths;
  llvm::copy(loadedModuleFile->getSearchPaths(),
             std::back_inserter(serializedSearchPaths));

  // Map the set of dependencies over to the "module dependencies".
  auto dependencies = ModuleDependencyInfo::forSwiftBinaryModule(
      binaryModulePath.str(), moduleDocPath, sourceInfoPath,
      binaryModuleImports->moduleImports,
      binaryModuleOptionalImports->moduleImports, linkLibraries,
      serializedSearchPaths, binaryModuleImports->headerImport,
      definingModulePath, isFramework, loadedModuleFile->isStaticLibrary(),
      /*module-cache-key*/ "", userModuleVer);

  for (auto &macro : loadedModuleFile->getExternalMacros()) {
    auto deps =
        resolveMacroPlugin(macro, loadedModuleFile->getModulePackageName());
    if (!deps)
      continue;
    dependencies.addMacroDependency(macro.ModuleName, deps->LibraryPath,
                                    deps->ExecutablePath);
  }

  return std::move(dependencies);
}

SwiftModuleScannerQueryResult
SwiftModuleScanner::lookupSwiftModule(Identifier moduleName,
                                      bool isTestableImport) {
  // When we exit, ensure we clear dependencies discovered on this query
  SWIFT_DEFER {
    foundDependencyInfo = std::nullopt;
    incompatibleCandidates = {};
  };

  ImportPath::Module::Builder builder(moduleName);
  auto modulePath = builder.get();
  // Execute the check to determine whether there is a module with this name
  // that we can import. This check will populate the result fields if one is
  // found.
  canImportModule(modulePath, SourceLoc(), nullptr, isTestableImport);
  return SwiftModuleScannerQueryResult(
      std::move(foundDependencyInfo), std::move(incompatibleCandidates));
}
