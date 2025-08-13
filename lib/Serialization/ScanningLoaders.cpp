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
#include "swift/Basic/FileTypes.h"
#include "swift/Basic/PrettyStackTrace.h"
#include "swift/Frontend/ModuleInterfaceLoader.h"
#include "swift/Serialization/ScanningLoaders.h"
#include "swift/Serialization/SerializedModuleLoader.h"
#include "swift/Subsystems.h"
#include "llvm/ADT/IntrusiveRefCntPtr.h"
#include "llvm/ADT/SetOperations.h"
#include "llvm/CAS/CachingOnDiskFileSystem.h"
#include "llvm/Support/Error.h"
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
      auto dependencies =
          scanModuleFile(ModPath, IsFramework,
                         isTestableDependencyLookup,
                         /* isCandidateForTextualModule */ false);
      if (dependencies) {
        this->dependencies = std::move(dependencies.get());
        return std::error_code();
      }
      return dependencies.getError();
    }
    return std::make_error_code(std::errc::no_such_file_or_directory);
  }
  assert(InPath);

  auto dependencies =
      scanInterfaceFile(*InPath, IsFramework, isTestableDependencyLookup);
  if (dependencies) {
    this->dependencies = std::move(dependencies.get());
    return std::error_code();
  }
  return dependencies.getError();
}

bool SwiftModuleScanner::canImportModule(ImportPath::Module path, SourceLoc loc,
                                         ModuleVersionInfo *versionInfo,
                                         bool isTestableDependencyLookup) {
  if (path.hasSubmodule())
    return false;

  // Check explicitly-provided Swift modules with '-swift-module-file'
  ImportPath::Element mID = path.front();
  auto it =
      explicitSwiftModuleInputs.find(Ctx.getRealModuleName(mID.Item).str());
  if (it != explicitSwiftModuleInputs.end()) {
    auto dependencies = scanModuleFile(it->getValue(), /* IsFramework */ false,
                                       isTestableDependencyLookup,
                                       /* isCandidateForTextualModule */ false);
    if (dependencies) {
      this->dependencies = std::move(dependencies.get());
      return true;
    }
  }

  return SerializedModuleLoaderBase::canImportModule(
      path, loc, versionInfo, isTestableDependencyLookup);
}

bool PlaceholderSwiftModuleScanner::findModule(
    ImportPath::Element moduleID, SmallVectorImpl<char> *moduleInterfacePath,
    SmallVectorImpl<char> *moduleInterfaceSourcePath,
    std::unique_ptr<llvm::MemoryBuffer> *moduleBuffer,
    std::unique_ptr<llvm::MemoryBuffer> *moduleDocBuffer,
    std::unique_ptr<llvm::MemoryBuffer> *moduleSourceInfoBuffer,
    bool skipBuildingInterface, bool isTestableDependencyLookup,
    bool &isFramework, bool &isSystemModule) {
  StringRef moduleName = Ctx.getRealModuleName(moduleID.Item).str();
  auto it = PlaceholderDependencyModuleMap.find(moduleName);
  if (it == PlaceholderDependencyModuleMap.end()) {
    return false;
  }
  auto &moduleInfo = it->getValue();
  auto dependencies = ModuleDependencyInfo::forPlaceholderSwiftModuleStub(
      moduleInfo.modulePath,
      moduleInfo.moduleDocPath.has_value() ? moduleInfo.moduleDocPath.value()
                                           : "",
      moduleInfo.moduleSourceInfoPath.has_value()
          ? moduleInfo.moduleSourceInfoPath.value()
          : "");
  this->dependencies = std::move(dependencies);
  return true;
}

void PlaceholderSwiftModuleScanner::parsePlaceholderModuleMap(
    StringRef fileName) {
  ExplicitModuleMapParser parser(Allocator);
  llvm::StringMap<ExplicitClangModuleInputInfo> ClangDependencyModuleMap;
  llvm::StringMap<std::string> ModuleAliases;
  llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>> fileBufOrErr =
      llvm::MemoryBuffer::getFile(fileName);
  if (!fileBufOrErr) {
    Ctx.Diags.diagnose(SourceLoc(), diag::explicit_swift_module_map_missing,
                       fileName);
    return;
  }
  auto result = parser.parseSwiftExplicitModuleMap(
      (*fileBufOrErr)->getMemBufferRef(), PlaceholderDependencyModuleMap,
      ClangDependencyModuleMap, ModuleAliases);
  if (result == std::errc::invalid_argument) {
    Ctx.Diags.diagnose(SourceLoc(),
                       diag::placeholder_dependency_module_map_corrupted,
                       fileName);
  } else if (result == std::errc::no_such_file_or_directory) {
    Ctx.Diags.diagnose(
        SourceLoc(), diag::placeholder_dependency_module_map_missing, fileName);
  }
}

static std::vector<std::string> getCompiledCandidates(ASTContext &ctx,
                                                      StringRef moduleName,
                                                      StringRef interfacePath) {
  return ctx.getModuleInterfaceChecker()
      ->getCompiledModuleCandidatesForInterface(moduleName.str(),
                                                interfacePath);
}

llvm::ErrorOr<ModuleDependencyInfo>
SwiftModuleScanner::scanInterfaceFile(Twine moduleInterfacePath,
                                      bool isFramework, bool isTestableImport) {
  // Create a module filename.
  // FIXME: Query the module interface loader to determine an appropriate
  // name for the module, which includes an appropriate hash.
  auto newExt = file_types::getExtension(file_types::TY_SwiftModuleFile);
  auto realModuleName = Ctx.getRealModuleName(moduleName);
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
          auto BinaryDep = scanModuleFile(compiledCandidates[0],
                                          isFramework, isTestableImport,
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
        std::vector<StringRef> compiledCandidatesRefs(compiledCandidates.begin(),
                                                      compiledCandidates.end());

        // If this interface specified '-autolink-force-load', add it to the
        // set of linked libraries for this module.
        std::vector<LinkLibrary> linkLibraries;
        if (llvm::find(ArgsRefs, "-autolink-force-load") != ArgsRefs.end()) {
          std::string linkName = realModuleName.str().str();
          auto linkNameArgIt = llvm::find(ArgsRefs, "-module-link-name");
          if (linkNameArgIt != ArgsRefs.end())
            linkName = *(linkNameArgIt+1);
          linkLibraries.push_back({linkName,
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
          Result->addModuleImport(import.module.getModulePath(),
                                  import.options.contains(ImportFlags::Exported),
                                  import.accessLevel,
                                  &alreadyAddedModules, &Ctx.SourceMgr);
        }

        // If this is a dependency that belongs to the same package, and we have not yet enabled Package Textual interfaces,
        // scan the adjacent binary module for package dependencies.
        if (!ScannerPackageName.empty() &&
            !Ctx.LangOpts.EnablePackageInterfaceLoad) {
           auto adjacentBinaryModule = std::find_if(
               compiledCandidates.begin(), compiledCandidates.end(),
               [moduleInterfacePath](const std::string &candidate) {
                 return llvm::sys::path::parent_path(candidate) ==
                        llvm::sys::path::parent_path(moduleInterfacePath.str());
               });

           if (adjacentBinaryModule != compiledCandidates.end()) {
             auto adjacentBinaryModulePackageOnlyImports = getMatchingPackageOnlyImportsOfModule(
                  *adjacentBinaryModule, isFramework,
                  isRequiredOSSAModules(), Ctx.LangOpts.SDKName,
                  ScannerPackageName, Ctx.SourceMgr.getFileSystem().get(),
                  Ctx.SearchPathOpts.DeserializedPathRecoverer);

             if (!adjacentBinaryModulePackageOnlyImports)
               return adjacentBinaryModulePackageOnlyImports.getError();

             for (const auto &requiredImport : *adjacentBinaryModulePackageOnlyImports)
               if (!alreadyAddedModules.contains(requiredImport.importIdentifier))
                 Result->addModuleImport(requiredImport.importIdentifier,
                                         requiredImport.isExported,
                                         requiredImport.accessLevel,
                                         &alreadyAddedModules);
           }
         }

        return std::error_code();
      });

  if (code) {
    return code;
  }
  return *Result;
}

ModuleDependencyVector SerializedModuleLoaderBase::getModuleDependencies(
    Identifier moduleName, StringRef moduleOutputPath,
    StringRef sdkModuleOutputPath,
    const llvm::DenseSet<clang::tooling::dependencies::ModuleID>
        &alreadySeenClangModules,
    const std::vector<std::string> &swiftModuleClangCC1CommandLineArgs,
    const llvm::StringMap<std::string> &explicitSwiftModuleInputs,
    InterfaceSubContextDelegate &delegate, llvm::PrefixMapper *mapper,
    bool isTestableDependencyLookup) {
  ImportPath::Module::Builder builder(moduleName);
  auto modulePath = builder.get();
  auto moduleId = modulePath.front().Item;

  // Instantiate dependency scanning "loaders".
  SmallVector<std::unique_ptr<SwiftModuleScanner>, 2> scanners;
  // Placeholder dependencies must be resolved first, to prevent the
  // ModuleDependencyScanner from first discovering artifacts of a previous
  // build. Such artifacts are captured as compiledModuleCandidates in the
  // dependency graph of the placeholder dependency module itself.
  // FIXME: submodules?
  scanners.push_back(std::make_unique<PlaceholderSwiftModuleScanner>(
      Ctx, LoadMode, moduleId, Ctx.SearchPathOpts.PlaceholderDependencyModuleMap,
      delegate, moduleOutputPath, sdkModuleOutputPath));
  scanners.push_back(std::make_unique<SwiftModuleScanner>(
      Ctx, LoadMode, moduleId, delegate, moduleOutputPath, sdkModuleOutputPath,
      swiftModuleClangCC1CommandLineArgs, explicitSwiftModuleInputs,
      SwiftModuleScanner::MDS_plain));

  // Check whether there is a module with this name that we can import.
  assert(isa<PlaceholderSwiftModuleScanner>(scanners[0].get()) &&
         "Expected PlaceholderSwiftModuleScanner as the first dependency "
         "scanner loader.");
  for (auto &scanner : scanners) {
    if (scanner->canImportModule(modulePath, SourceLoc(), nullptr,
                                 isTestableDependencyLookup)) {

      ModuleDependencyVector moduleDependnecies;
      moduleDependnecies.push_back(
          std::make_pair(ModuleDependencyID{moduleName.str().str(),
                                            scanner->dependencies->getKind()},
                         *(scanner->dependencies)));
      return moduleDependnecies;
    }
  }

  return {};
}
