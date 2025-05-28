//===--- ModuleDependencies.h - Module Dependencies -------------*- C++ -*-===//
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
//
// This file implements data structures for capturing module dependencies.
//
//===----------------------------------------------------------------------===//
#include "swift/AST/ModuleDependencies.h"
#include "swift/AST/Decl.h"
#include "swift/AST/DiagnosticsFrontend.h"
#include "swift/AST/DiagnosticsSema.h"
#include "swift/AST/MacroDefinition.h"
#include "swift/AST/Module.h"
#include "swift/AST/PluginLoader.h"
#include "swift/AST/SourceFile.h"
#include "swift/Frontend/Frontend.h"
#include "swift/Strings.h"
#include "clang/CAS/IncludeTree.h"
#include "llvm/CAS/CASProvidingFileSystem.h"
#include "llvm/Config/config.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/PrefixMapper.h"
#include <system_error>
using namespace swift;

ModuleDependencyInfoStorageBase::~ModuleDependencyInfoStorageBase() {}

bool ModuleDependencyInfo::isSwiftModule() const {
  return isSwiftInterfaceModule() || isSwiftSourceModule() ||
         isSwiftBinaryModule();
}

bool ModuleDependencyInfo::isTextualSwiftModule() const {
  return isSwiftInterfaceModule() || isSwiftSourceModule();
}

namespace {
  ModuleDependencyKind &operator++(ModuleDependencyKind &e) {
    if (e == ModuleDependencyKind::LastKind) {
      llvm_unreachable(
                       "Attempting to increment last enum value on ModuleDependencyKind");
    }
    e = ModuleDependencyKind(
                             static_cast<std::underlying_type<ModuleDependencyKind>::type>(e) + 1);
    return e;
  }
}
bool ModuleDependencyInfo::isSwiftInterfaceModule() const {
  return isa<SwiftInterfaceModuleDependenciesStorage>(storage.get());
}

bool ModuleDependencyInfo::isSwiftSourceModule() const {
  return isa<SwiftSourceModuleDependenciesStorage>(storage.get());
}

bool ModuleDependencyInfo::isSwiftBinaryModule() const {
  return isa<SwiftBinaryModuleDependencyStorage>(storage.get());
}

bool ModuleDependencyInfo::isClangModule() const {
  return isa<ClangModuleDependencyStorage>(storage.get());
}

/// Retrieve the dependencies for a Swift textual interface module.
const SwiftInterfaceModuleDependenciesStorage *
ModuleDependencyInfo::getAsSwiftInterfaceModule() const {
  return dyn_cast<SwiftInterfaceModuleDependenciesStorage>(storage.get());
}

const SwiftSourceModuleDependenciesStorage *
ModuleDependencyInfo::getAsSwiftSourceModule() const {
  return dyn_cast<SwiftSourceModuleDependenciesStorage>(storage.get());
}

/// Retrieve the dependencies for a binary Swift dependency module.
const SwiftBinaryModuleDependencyStorage *
ModuleDependencyInfo::getAsSwiftBinaryModule() const {
  return dyn_cast<SwiftBinaryModuleDependencyStorage>(storage.get());
}

/// Retrieve the dependencies for a Clang module.
const ClangModuleDependencyStorage *
ModuleDependencyInfo::getAsClangModule() const {
  return dyn_cast<ClangModuleDependencyStorage>(storage.get());
}

void ModuleDependencyInfo::addTestableImport(ImportPath::Module module) {
  assert(getAsSwiftSourceModule() && "Expected source module for addTestableImport.");
  dyn_cast<SwiftSourceModuleDependenciesStorage>(storage.get())->addTestableImport(module);
}

bool ModuleDependencyInfo::isTestableImport(StringRef moduleName) const {
  if (auto swiftSourceDepStorage = getAsSwiftSourceModule())
    return swiftSourceDepStorage->testableImports.contains(moduleName);
  else
    return false;
}

void ModuleDependencyInfo::addOptionalModuleImport(
    StringRef module, bool isExported, AccessLevel accessLevel,
    llvm::StringSet<> *alreadyAddedModules) {

  if (alreadyAddedModules && alreadyAddedModules->contains(module)) {
    // Find a prior import of this module and add import location
    // and adjust whether or not this module is ever imported as exported
    // as well as the access level
    for (auto &existingImport : storage->optionalModuleImports) {
      if (existingImport.importIdentifier == module) {
        existingImport.isExported |= isExported;
        existingImport.accessLevel = std::max(existingImport.accessLevel,
                                              accessLevel);
        break;
      }
    }
  } else {
    if (alreadyAddedModules)
      alreadyAddedModules->insert(module);

    storage->optionalModuleImports.push_back(
         {module.str(), isExported, accessLevel});
  }
}

void ModuleDependencyInfo::addModuleImport(
    StringRef module, bool isExported, AccessLevel accessLevel,
    llvm::StringSet<> *alreadyAddedModules, const SourceManager *sourceManager,
    SourceLoc sourceLocation) {
  auto scannerImportLocToDiagnosticLocInfo =
      [&sourceManager](SourceLoc sourceLocation) {
        auto lineAndColumnNumbers =
            sourceManager->getLineAndColumnInBuffer(sourceLocation);
        return ScannerImportStatementInfo::ImportDiagnosticLocationInfo(
            sourceManager->getDisplayNameForLoc(sourceLocation).str(),
            lineAndColumnNumbers.first, lineAndColumnNumbers.second);
      };
  bool validSourceLocation = sourceManager && sourceLocation.isValid() &&
                             sourceManager->isOwning(sourceLocation);

  if (alreadyAddedModules && alreadyAddedModules->contains(module)) {
    // Find a prior import of this module and add import location
    // and adjust whether or not this module is ever imported as exported
    // as well as the access level
    for (auto &existingImport : storage->moduleImports) {
      if (existingImport.importIdentifier == module) {
        if (validSourceLocation) {
          existingImport.addImportLocation(
              scannerImportLocToDiagnosticLocInfo(sourceLocation));
        }
        existingImport.isExported |= isExported;
        existingImport.accessLevel = std::max(existingImport.accessLevel,
                                              accessLevel);
        break;
      }
    }
  } else {
    if (alreadyAddedModules)
      alreadyAddedModules->insert(module);

    if (validSourceLocation)
      storage->moduleImports.push_back(ScannerImportStatementInfo(
          module.str(), isExported, accessLevel,
          scannerImportLocToDiagnosticLocInfo(sourceLocation)));
    else
      storage->moduleImports.push_back(
          ScannerImportStatementInfo(module.str(), isExported, accessLevel));
  }
}

void ModuleDependencyInfo::addModuleImport(
    ImportPath::Module module, bool isExported, AccessLevel accessLevel,
    llvm::StringSet<> *alreadyAddedModules, const SourceManager *sourceManager,
    SourceLoc sourceLocation) {
  std::string ImportedModuleName = module.front().Item.str().str();
  auto submodulePath = module.getSubmodulePath();
  if (submodulePath.size() > 0 && !submodulePath[0].Item.empty()) {
    auto submoduleComponent = submodulePath[0];
    // Special case: a submodule named "Foo.Private" can be moved to a top-level
    // module named "Foo_Private". ClangImporter has special support for this.
    if (submoduleComponent.Item.str() == "Private")
      addOptionalModuleImport(ImportedModuleName + "_Private",
                              isExported, accessLevel,
                              alreadyAddedModules);
  }

  addModuleImport(ImportedModuleName, isExported, accessLevel,
                  alreadyAddedModules, sourceManager, sourceLocation);
}

void ModuleDependencyInfo::addModuleImports(
    const SourceFile &sourceFile, llvm::StringSet<> &alreadyAddedModules,
    const SourceManager *sourceManager) {
  // Add all of the module dependencies.
  SmallVector<Decl *, 32> decls;
  sourceFile.getTopLevelDecls(decls);
  for (auto decl : decls) {
    if (auto importDecl = dyn_cast<ImportDecl>(decl)) {
      ImportPath::Builder scratch;
      auto realPath = importDecl->getRealModulePath(scratch);

      // Explicit 'Builtin' import is not a part of the module's
      // dependency set, does not exist on the filesystem,
      // and is resolved within the compiler during compilation.
      SmallString<64> importedModuleName;
      realPath.getString(importedModuleName);
      if (importedModuleName == BUILTIN_NAME)
        continue;

      // Ignore/diagnose tautological imports akin to import resolution
      if (!swift::dependencies::checkImportNotTautological(
              realPath, importDecl->getLoc(), sourceFile,
              importDecl->isExported()))
        continue;

      addModuleImport(realPath, importDecl->isExported(),
                      importDecl->getAccessLevel(),
                      &alreadyAddedModules, sourceManager,
                      importDecl->getLoc());

      // Additionally, keep track of which dependencies of a Source
      // module are `@Testable`.
      if (getKind() == swift::ModuleDependencyKind::SwiftSource &&
          importDecl->isTestable())
        addTestableImport(realPath);
    } else if (auto macroDecl = dyn_cast<MacroDecl>(decl)) {
      auto macroDef = macroDecl->getDefinition();
      auto &ctx = macroDecl->getASTContext();
      if (macroDef.kind != MacroDefinition::Kind::External)
        continue;
      auto external = macroDef.getExternalMacro();
      PluginLoader &loader = ctx.getPluginLoader();
      auto &entry = loader.lookupPluginByModuleName(external.moduleName);
      if (entry.libraryPath.empty() && entry.executablePath.empty())
        continue;
      addMacroDependency(external.moduleName.str(), entry.libraryPath,
                         entry.executablePath);
    }
  }

  auto fileName = sourceFile.getFilename();
  if (fileName.empty())
    return;

  switch (getKind()) {
  case swift::ModuleDependencyKind::SwiftInterface: {
    // If the storage is for an interface file, the only source file we
    // should see is that interface file.
    assert(fileName ==
           cast<SwiftInterfaceModuleDependenciesStorage>(storage.get())->swiftInterfaceFile);
    break;
  }
  case swift::ModuleDependencyKind::SwiftSource: {
    // Otherwise, record the source file.
    auto swiftSourceStorage =
        cast<SwiftSourceModuleDependenciesStorage>(storage.get());
    swiftSourceStorage->sourceFiles.push_back(fileName.str());
    break;
  }
  default:
    llvm_unreachable("Unexpected dependency kind");
  }
}

std::optional<std::string> ModuleDependencyInfo::getBridgingHeader() const {
  switch (getKind()) {
  case swift::ModuleDependencyKind::SwiftInterface: {
    auto swiftInterfaceStorage =
        cast<SwiftInterfaceModuleDependenciesStorage>(storage.get());
    return swiftInterfaceStorage->textualModuleDetails.bridgingHeaderFile;
  }
  case swift::ModuleDependencyKind::SwiftSource: {
    auto swiftSourceStorage =
        cast<SwiftSourceModuleDependenciesStorage>(storage.get());
    return swiftSourceStorage->textualModuleDetails.bridgingHeaderFile;
  }
  default:
    return std::nullopt;
  }
}

std::optional<std::string> ModuleDependencyInfo::getCASFSRootID() const {
  std::string Root;
  switch (getKind()) {
  case swift::ModuleDependencyKind::SwiftInterface: {
    auto swiftInterfaceStorage =
        cast<SwiftInterfaceModuleDependenciesStorage>(storage.get());
    Root = swiftInterfaceStorage->textualModuleDetails.CASFileSystemRootID;
    break;
  }
  case swift::ModuleDependencyKind::SwiftSource: {
    auto swiftSourceStorage =
        cast<SwiftSourceModuleDependenciesStorage>(storage.get());
    Root = swiftSourceStorage->textualModuleDetails.CASFileSystemRootID;
    break;
  }
  case swift::ModuleDependencyKind::Clang: {
    auto clangModuleStorage = cast<ClangModuleDependencyStorage>(storage.get());
    Root = clangModuleStorage->CASFileSystemRootID;
    break;
  }
  default:
    return std::nullopt;
  }
  if (Root.empty())
    return std::nullopt;

  return Root;
}

std::optional<std::string> ModuleDependencyInfo::getClangIncludeTree() const {
  std::string Root;
  switch (getKind()) {
  case swift::ModuleDependencyKind::Clang: {
    auto clangModuleStorage = cast<ClangModuleDependencyStorage>(storage.get());
    Root = clangModuleStorage->CASClangIncludeTreeRootID;
    break;
  }
  default:
    return std::nullopt;
  }
  if (Root.empty())
    return std::nullopt;

  return Root;
}

std::optional<std::string>
ModuleDependencyInfo::getBridgingHeaderIncludeTree() const {
  std::string Root;
  switch (getKind()) {
  case swift::ModuleDependencyKind::SwiftInterface: {
    auto swiftInterfaceStorage =
        cast<SwiftInterfaceModuleDependenciesStorage>(storage.get());
    Root = swiftInterfaceStorage->textualModuleDetails
               .CASBridgingHeaderIncludeTreeRootID;
    break;
  }
  case swift::ModuleDependencyKind::SwiftSource: {
    auto swiftSourceStorage =
        cast<SwiftSourceModuleDependenciesStorage>(storage.get());
    Root = swiftSourceStorage->textualModuleDetails
               .CASBridgingHeaderIncludeTreeRootID;
    break;
  }
  default:
    return std::nullopt;
  }
  if (Root.empty())
    return std::nullopt;

  return Root;
}

std::string ModuleDependencyInfo::getModuleOutputPath() const {
  switch (getKind()) {
  case swift::ModuleDependencyKind::SwiftInterface: {
    auto swiftInterfaceStorage =
        cast<SwiftInterfaceModuleDependenciesStorage>(storage.get());
    return swiftInterfaceStorage->moduleOutputPath;
  }
  case swift::ModuleDependencyKind::SwiftSource: {
    return "<swiftmodule>";
  }
  case swift::ModuleDependencyKind::Clang: {
    auto clangModuleStorage = cast<ClangModuleDependencyStorage>(storage.get());
    return clangModuleStorage->pcmOutputPath;
  }
  case swift::ModuleDependencyKind::SwiftBinary: {
    auto swiftBinaryStorage =
        cast<SwiftBinaryModuleDependencyStorage>(storage.get());
    return swiftBinaryStorage->compiledModulePath;
  }
  default:
    llvm_unreachable("Unexpected dependency kind");
  }
}

void ModuleDependencyInfo::addBridgingHeader(StringRef bridgingHeader) {
  switch (getKind()) {
  case swift::ModuleDependencyKind::SwiftInterface: {
    auto swiftInterfaceStorage =
        cast<SwiftInterfaceModuleDependenciesStorage>(storage.get());
    assert(!swiftInterfaceStorage->textualModuleDetails.bridgingHeaderFile);
    swiftInterfaceStorage->textualModuleDetails.bridgingHeaderFile = bridgingHeader.str();
    break;
  }
  case swift::ModuleDependencyKind::SwiftSource: {
    auto swiftSourceStorage =
        cast<SwiftSourceModuleDependenciesStorage>(storage.get());
    assert(!swiftSourceStorage->textualModuleDetails.bridgingHeaderFile);
    swiftSourceStorage->textualModuleDetails.bridgingHeaderFile = bridgingHeader.str();
    break;
  }
  default:
    llvm_unreachable("Unexpected dependency kind");
  }
}

/// Add source files that the bridging header depends on.
void ModuleDependencyInfo::setHeaderSourceFiles(
    const std::vector<std::string> &files) {
  switch (getKind()) {
  case swift::ModuleDependencyKind::SwiftInterface: {
    auto swiftInterfaceStorage =
        cast<SwiftInterfaceModuleDependenciesStorage>(storage.get());
    swiftInterfaceStorage->textualModuleDetails.bridgingSourceFiles = files;
    break;
  }
  case swift::ModuleDependencyKind::SwiftSource: {
    auto swiftSourceStorage =
        cast<SwiftSourceModuleDependenciesStorage>(storage.get());
    swiftSourceStorage->textualModuleDetails.bridgingSourceFiles = files;
    break;
  }
  case swift::ModuleDependencyKind::SwiftBinary: {
    auto swiftBinaryStorage =
        cast<SwiftBinaryModuleDependencyStorage>(storage.get());
    swiftBinaryStorage->headerSourceFiles = files;
    break;
  }
  default:
    llvm_unreachable("Unexpected dependency kind");
  }
}

void ModuleDependencyInfo::addBridgingHeaderIncludeTree(StringRef ID) {
  switch (getKind()) {
  case swift::ModuleDependencyKind::SwiftInterface: {
    auto swiftInterfaceStorage =
        cast<SwiftInterfaceModuleDependenciesStorage>(storage.get());
    swiftInterfaceStorage->textualModuleDetails
        .CASBridgingHeaderIncludeTreeRootID = ID.str();
    break;
  }
  case swift::ModuleDependencyKind::SwiftSource: {
    auto swiftSourceStorage =
        cast<SwiftSourceModuleDependenciesStorage>(storage.get());
    swiftSourceStorage->textualModuleDetails
        .CASBridgingHeaderIncludeTreeRootID = ID.str();
    break;
  }
  default:
    llvm_unreachable("Unexpected dependency kind");
  }
}

void ModuleDependencyInfo::setChainedBridgingHeaderBuffer(StringRef path,
                                                          StringRef buffer) {
  switch (getKind()) {
  case swift::ModuleDependencyKind::SwiftSource: {
    auto swiftSourceStorage =
        cast<SwiftSourceModuleDependenciesStorage>(storage.get());
    swiftSourceStorage->setChainedBridgingHeaderBuffer(path, buffer);
    break;
  }
  default:
    llvm_unreachable("Unexpected dependency kind");
  }
}

void ModuleDependencyInfo::addSourceFile(StringRef sourceFile) {
  switch (getKind()) {
  case swift::ModuleDependencyKind::SwiftSource: {
    auto swiftSourceStorage =
        cast<SwiftSourceModuleDependenciesStorage>(storage.get());
    swiftSourceStorage->sourceFiles.push_back(sourceFile.str());
    break;
  }
  default:
    llvm_unreachable("Unexpected dependency kind");
  }
}

void ModuleDependencyInfo::setOutputPathAndHash(StringRef outputPath,
                                                StringRef hash) {
  switch (getKind()) {
  case swift::ModuleDependencyKind::SwiftInterface: {
    auto swiftInterfaceStorage =
        cast<SwiftInterfaceModuleDependenciesStorage>(storage.get());
    swiftInterfaceStorage->moduleOutputPath = outputPath.str();
    swiftInterfaceStorage->contextHash = hash.str();
    break;
  }
  default:
    llvm_unreachable("Unexpected dependency kind");
  }
}

SwiftDependencyScanningService::SwiftDependencyScanningService() {
  ClangScanningService.emplace(
      clang::tooling::dependencies::ScanningMode::DependencyDirectivesScan,
      clang::tooling::dependencies::ScanningOutputFormat::FullTree,
      clang::CASOptions(),
      /* CAS (llvm::cas::ObjectStore) */ nullptr,
      /* Cache (llvm::cas::ActionCache) */ nullptr,
      /* SharedFS */ nullptr,
      // ScanningOptimizations::Default excludes the current working
      // directory optimization. Clang needs to communicate with
      // the build system to handle the optimization safely.
      // Swift can handle the working directory optimizaiton
      // already so it is safe to turn on all optimizations.
      clang::tooling::dependencies::ScanningOptimizations::All);
  SharedFilesystemCache.emplace();
}

llvm::IntrusiveRefCntPtr<llvm::vfs::FileSystem>
SwiftDependencyScanningService::getClangScanningFS(ASTContext &ctx) const {
  llvm::IntrusiveRefCntPtr<llvm::vfs::FileSystem> baseFileSystem =
      llvm::vfs::createPhysicalFileSystem();
  ClangInvocationFileMapping fileMapping =
    applyClangInvocationMapping(ctx, nullptr, baseFileSystem, false);

  if (CAS)
    return llvm::cas::createCASProvidingFileSystem(CAS, baseFileSystem);
  return baseFileSystem;
}

bool
swift::dependencies::checkImportNotTautological(const ImportPath::Module modulePath,
                                                const SourceLoc importLoc,
                                                const SourceFile &SF,
                                                bool isExported) {
  if (modulePath.front().Item != SF.getParentModule()->getName() ||
      // Overlays use an @_exported self-import to load their clang module.
      isExported ||
      // Imports of your own submodules are allowed in cross-language libraries.
      modulePath.size() != 1 ||
      // SIL files self-import to get decls from the rest of the module.
      SF.Kind == SourceFileKind::SIL)
    return true;

  ASTContext &ctx = SF.getASTContext();

  StringRef filename = llvm::sys::path::filename(SF.getFilename());
  if (filename.empty())
    ctx.Diags.diagnose(importLoc, diag::sema_import_current_module,
                       modulePath.front().Item);
  else
    ctx.Diags.diagnose(importLoc, diag::sema_import_current_module_with_file,
                       filename, modulePath.front().Item);

  return false;
}

void swift::dependencies::registerCxxInteropLibraries(
    const llvm::Triple &Target, StringRef mainModuleName, bool hasStaticCxx,
    bool hasStaticCxxStdlib, CXXStdlibKind cxxStdlibKind,
    std::function<void(const LinkLibrary &)> RegistrationCallback) {

  switch (cxxStdlibKind) {
  case CXXStdlibKind::Libcxx:
    RegistrationCallback(
        LinkLibrary{"c++", LibraryKind::Library, /*static=*/false});
    break;
  case CXXStdlibKind::Libstdcxx:
    RegistrationCallback(
        LinkLibrary{"stdc++", LibraryKind::Library, /*static=*/false});
    break;
  case CXXStdlibKind::Msvcprt:
    // FIXME: should we be explicitly linking in msvcprt or will the module do
    // so?
    break;
  case CXXStdlibKind::Unknown:
    // FIXME: we should probably emit a warning or a note here.
    break;
  }

  // Do not try to link Cxx with itself.
  if (mainModuleName != CXX_MODULE_NAME)
    RegistrationCallback(
        LinkLibrary{"swiftCxx", LibraryKind::Library, hasStaticCxx});

  // Do not try to link CxxStdlib with the C++ standard library, Cxx or
  // itself.
  if (llvm::none_of(llvm::ArrayRef<StringRef>{CXX_MODULE_NAME, "CxxStdlib", "std"},
                    [mainModuleName](StringRef Name) {
                      return mainModuleName == Name;
                    })) {
    // Only link with CxxStdlib on platforms where the overlay is available.
    if (Target.isOSDarwin() || Target.isOSLinux() || Target.isOSWindows() ||
        Target.isOSFreeBSD())
      RegistrationCallback(LinkLibrary{"swiftCxxStdlib", LibraryKind::Library,
                                       hasStaticCxxStdlib});
  }
}

void
swift::dependencies::registerBackDeployLibraries(
    const IRGenOptions &IRGenOpts,
    std::function<void(const LinkLibrary&)> RegistrationCallback) {
  auto addBackDeployLib = [&](llvm::VersionTuple version,
                              StringRef libraryName, bool forceLoad) {
    std::optional<llvm::VersionTuple> compatibilityVersion;
    if (libraryName == "swiftCompatibilityDynamicReplacements") {
      compatibilityVersion = IRGenOpts.
          AutolinkRuntimeCompatibilityDynamicReplacementLibraryVersion;
    } else if (libraryName == "swiftCompatibilityConcurrency") {
      compatibilityVersion =
          IRGenOpts.AutolinkRuntimeCompatibilityConcurrencyLibraryVersion;
    } else {
      compatibilityVersion = IRGenOpts.
          AutolinkRuntimeCompatibilityLibraryVersion;
    }

    if (!compatibilityVersion)
      return;

    if (*compatibilityVersion > version)
      return;

    RegistrationCallback(
        {libraryName, LibraryKind::Library, /*static=*/true, forceLoad});
  };

#define BACK_DEPLOYMENT_LIB(Version, Filter, LibraryName, ForceLoad) \
    addBackDeployLib(llvm::VersionTuple Version, LibraryName, ForceLoad);
  #include "swift/Frontend/BackDeploymentLibs.def"
}

SwiftDependencyTracker::SwiftDependencyTracker(
    std::shared_ptr<llvm::cas::ObjectStore> CAS, llvm::PrefixMapper *Mapper,
    const CompilerInvocation &CI)
    : CAS(CAS), Mapper(Mapper) {
  auto &SearchPathOpts = CI.getSearchPathOptions();

  FS = llvm::cas::createCASProvidingFileSystem(
      CAS, llvm::vfs::createPhysicalFileSystem());

  auto addCommonFile = [&](StringRef path) {
    auto file = FS->openFileForRead(path);
    if (!file)
      return;
    auto status = (*file)->status();
    if (!status)
      return;
    auto fileRef = (*file)->getObjectRefForContent();
    if (!fileRef)
      return;

    std::string realPath = Mapper ? Mapper->mapToString(path) : path.str();
    CommonFiles.try_emplace(realPath, **fileRef, (size_t)status->getSize());
  };

  // Add SDKSetting file.
  SmallString<256> SDKSettingPath;
  llvm::sys::path::append(SDKSettingPath, SearchPathOpts.getSDKPath(),
                          "SDKSettings.json");
  addCommonFile(SDKSettingPath);

  // Add Legacy layout file.
  const std::vector<std::string> AllSupportedArches = {
      "arm64", "arm64e", "x86_64", "i386",
      "armv7", "armv7s", "armv7k", "arm64_32"};

  for (auto RuntimeLibPath : SearchPathOpts.RuntimeLibraryPaths) {
    std::error_code EC;
    for (auto &Arch : AllSupportedArches) {
      SmallString<256> LayoutFile(RuntimeLibPath);
      llvm::sys::path::append(LayoutFile, "layouts-" + Arch + ".yaml");
      addCommonFile(LayoutFile);
    }
  }

  // Add VFSOverlay file.
  for (auto &Overlay: SearchPathOpts.VFSOverlayFiles)
    addCommonFile(Overlay);

  // Add blocklist file.
  for (auto &File: CI.getFrontendOptions().BlocklistConfigFilePaths)
    addCommonFile(File);
}

void SwiftDependencyTracker::startTracking(bool includeCommonDeps) {
  TrackedFiles.clear();
  if (includeCommonDeps) {
    for (auto &entry : CommonFiles)
      TrackedFiles.emplace(entry.first(), entry.second);
  }
}

void SwiftDependencyTracker::trackFile(const Twine &path) {
  auto file = FS->openFileForRead(path);
  if (!file)
    return;
  auto status = (*file)->status();
  if (!status)
    return;
  auto fileRef = (*file)->getObjectRefForContent();
  if (!fileRef)
    return;
  std::string realPath =
      Mapper ? Mapper->mapToString(path.str()) : path.str();
  TrackedFiles.try_emplace(realPath, **fileRef, (size_t)status->getSize());
}

llvm::Expected<llvm::cas::ObjectProxy>
SwiftDependencyTracker::createTreeFromDependencies() {
  llvm::SmallVector<clang::cas::IncludeTree::FileList::FileEntry> Files;
  for (auto &file : TrackedFiles) {
    auto includeTreeFile = clang::cas::IncludeTree::File::create(
        *CAS, file.first, file.second.FileRef);
    if (!includeTreeFile) {
      return llvm::createStringError("CASFS createTree failed for " +
                                     file.first + ": " +
                                     toString(includeTreeFile.takeError()));
    }
    Files.push_back(
        {includeTreeFile->getRef(),
         (clang::cas::IncludeTree::FileList::FileSizeTy)file.second.Size});
  }

  auto includeTreeList =
      clang::cas::IncludeTree::FileList::create(*CAS, Files, {});
  if (!includeTreeList)
    return llvm::createStringError("casfs include-tree filelist error: " +
                                   toString(includeTreeList.takeError()));

  return *includeTreeList;
}

bool SwiftDependencyScanningService::setupCachingDependencyScanningService(
    CompilerInstance &Instance) {
  if (!Instance.getInvocation().getCASOptions().EnableCaching)
    return false;

  if (CASOpts) {
    // If CASOption matches, the service is initialized already.
    if (*CASOpts == Instance.getInvocation().getCASOptions().CASOpts)
      return false;

    // CASOption mismatch, return error.
    Instance.getDiags().diagnose(SourceLoc(), diag::error_cas_conflict_options);
    return true;
  }

  // Setup CAS.
  CASOpts = Instance.getInvocation().getCASOptions().CASOpts;
  CAS = Instance.getSharedCASInstance();
  ActionCache = Instance.getSharedCacheInstance();

  CacheFS = llvm::cas::createCASProvidingFileSystem(
      CAS, llvm::vfs::createPhysicalFileSystem());

  // Setup prefix mapping.
  auto &ScannerPrefixMapper =
      Instance.getInvocation().getSearchPathOptions().ScannerPrefixMapper;
  if (!ScannerPrefixMapper.empty()) {
    Mapper = std::make_unique<llvm::PrefixMapper>();
    SmallVector<llvm::MappedPrefix, 4> Prefixes;
    llvm::MappedPrefix::transformPairs(ScannerPrefixMapper,
                                       Prefixes);
    Mapper->addRange(Prefixes);
    Mapper->sort();
  }

  const clang::tooling::dependencies::ScanningOutputFormat ClangScanningFormat =
      clang::tooling::dependencies::ScanningOutputFormat::FullIncludeTree;

  ClangScanningService.emplace(
      clang::tooling::dependencies::ScanningMode::DependencyDirectivesScan,
      ClangScanningFormat, Instance.getInvocation().getCASOptions().CASOpts,
      Instance.getSharedCASInstance(), Instance.getSharedCacheInstance(),
      /*CachingOnDiskFileSystem=*/nullptr,
      // The current working directory optimization (off by default)
      // should not impact CAS. We set the optization to all to be
      // consistent with the non-CAS case.
      clang::tooling::dependencies::ScanningOptimizations::All);

  return false;
}

ModuleDependenciesCache::ModuleDependenciesCache(
    SwiftDependencyScanningService &globalScanningService,
    const std::string &mainScanModuleName, const std::string &moduleOutputPath,
    const std::string &sdkModuleOutputPath, const std::string &scannerContextHash)
    : globalScanningService(globalScanningService),
      mainScanModuleName(mainScanModuleName),
      scannerContextHash(scannerContextHash),
      moduleOutputPath(moduleOutputPath),
      sdkModuleOutputPath(sdkModuleOutputPath),
      scanInitializationTime(std::chrono::system_clock::now()) {
  for (auto kind = ModuleDependencyKind::FirstKind;
       kind != ModuleDependencyKind::LastKind; ++kind)
    ModuleDependenciesMap.insert({kind, ModuleNameToDependencyMap()});
}

const ModuleNameToDependencyMap &
ModuleDependenciesCache::getDependenciesMap(
    ModuleDependencyKind kind) const {
  auto it = ModuleDependenciesMap.find(kind);
  assert(it != ModuleDependenciesMap.end() &&
         "invalid dependency kind");
  return it->second;
}

ModuleNameToDependencyMap &
ModuleDependenciesCache::getDependenciesMap(
    ModuleDependencyKind kind) {
  auto it = ModuleDependenciesMap.find(kind);
  assert(it != ModuleDependenciesMap.end() &&
         "invalid dependency kind");
  return it->second;
}

std::optional<const ModuleDependencyInfo *>
ModuleDependenciesCache::findDependency(
    const ModuleDependencyID moduleID) const {
  return findDependency(moduleID.ModuleName, moduleID.Kind);
}

std::optional<const ModuleDependencyInfo *>
ModuleDependenciesCache::findDependency(
    StringRef moduleName, std::optional<ModuleDependencyKind> kind) const {
  if (!kind) {
    for (auto kind = ModuleDependencyKind::FirstKind;
         kind != ModuleDependencyKind::LastKind; ++kind) {
      auto dep = findDependency(moduleName, kind);
      if (dep.has_value())
        return dep.value();
    }
    return std::nullopt;
  }

  assert(kind.has_value() && "Expected dependencies kind for lookup.");
  std::optional<const ModuleDependencyInfo *> optionalDep = std::nullopt;
  const auto &map = getDependenciesMap(kind.value());
  auto known = map.find(moduleName);
  if (known != map.end())
    optionalDep = &(known->second);

  // During a scan, only produce the cached source module info for the current
  // module under scan.
  if (optionalDep.has_value()) {
    auto dep = optionalDep.value();
    if (dep->getAsSwiftSourceModule() &&
        moduleName != mainScanModuleName &&
        moduleName != "MainModuleCrossImportOverlays") {
      return std::nullopt;
    }
  }

  return optionalDep;
}

std::optional<const ModuleDependencyInfo *>
ModuleDependenciesCache::findSwiftDependency(StringRef moduleName) const {
  if (auto found = findDependency(moduleName, ModuleDependencyKind::SwiftInterface))
    return found;
  if (auto found = findDependency(moduleName, ModuleDependencyKind::SwiftBinary))
    return found;
  if (auto found = findDependency(moduleName, ModuleDependencyKind::SwiftSource))
    return found;
  return std::nullopt;
}

const ModuleDependencyInfo &ModuleDependenciesCache::findKnownDependency(
    const ModuleDependencyID &moduleID) const {
  
  auto dep = findDependency(moduleID);
  assert(dep && "dependency unknown");
  return **dep;
}

bool ModuleDependenciesCache::hasDependency(const ModuleDependencyID &moduleID) const {
  return hasDependency(moduleID.ModuleName, moduleID.Kind);
}

bool ModuleDependenciesCache::hasDependency(
    StringRef moduleName, std::optional<ModuleDependencyKind> kind) const {
  return findDependency(moduleName, kind).has_value();
}

bool ModuleDependenciesCache::hasDependency(StringRef moduleName) const {
  for (auto kind = ModuleDependencyKind::FirstKind;
       kind != ModuleDependencyKind::LastKind; ++kind)
    if (findDependency(moduleName, kind).has_value())
      return true;
  return false;
}

bool ModuleDependenciesCache::hasSwiftDependency(StringRef moduleName) const {
  return findSwiftDependency(moduleName).has_value();
}

void ModuleDependenciesCache::recordDependency(
    StringRef moduleName, ModuleDependencyInfo dependency) {
  auto dependenciesKind = dependency.getKind();
  auto &map = getDependenciesMap(dependenciesKind);
  map.insert({moduleName, dependency});
}

void ModuleDependenciesCache::recordDependencies(
    ModuleDependencyVector dependencies, DiagnosticEngine &diags) {
  for (const auto &dep : dependencies) {
    if (hasDependency(dep.first)) {
      if (dep.first.Kind == ModuleDependencyKind::Clang) {
        auto priorClangModuleDetails =
            findKnownDependency(dep.first).getAsClangModule();
        auto newClangModuleDetails = dep.second.getAsClangModule();
        auto priorContextHash = priorClangModuleDetails->contextHash;
        auto newContextHash = newClangModuleDetails->contextHash;
        if (priorContextHash != newContextHash) {
          // This situation means that within the same scanning action, Clang
          // Dependency Scanner has produced two different variants of the same
          // module. This is not supposed to happen, but we are currently
          // hunting down the rare cases where it does, seemingly due to
          // differences in Clang Scanner direct by-name queries and transitive
          // header lookup queries.
          //
          // Emit a failure diagnostic here that is hopefully more actionable
          // for the time being.
          diags.diagnose(SourceLoc(), diag::dependency_scan_unexpected_variant,
                         dep.first.ModuleName);
          diags.diagnose(
              SourceLoc(),
              diag::dependency_scan_unexpected_variant_context_hash_note,
              priorContextHash, newContextHash);
          diags.diagnose(
              SourceLoc(),
              diag::dependency_scan_unexpected_variant_module_map_note,
              priorClangModuleDetails->moduleMapFile,
              newClangModuleDetails->moduleMapFile);

          auto diagnoseExtraCommandLineFlags =
              [&diags](const ClangModuleDependencyStorage *checkModuleDetails,
                       const ClangModuleDependencyStorage *baseModuleDetails,
                       bool isNewlyDiscovered) -> void {
            std::unordered_set<std::string> baseCommandLineSet(
                baseModuleDetails->buildCommandLine.begin(),
                baseModuleDetails->buildCommandLine.end());
            for (const auto &checkArg : checkModuleDetails->buildCommandLine)
              if (baseCommandLineSet.find(checkArg) == baseCommandLineSet.end())
                diags.diagnose(
                    SourceLoc(),
                    diag::dependency_scan_unexpected_variant_extra_arg_note,
                    isNewlyDiscovered, checkArg);
          };
          diagnoseExtraCommandLineFlags(priorClangModuleDetails,
                                        newClangModuleDetails, true);
          diagnoseExtraCommandLineFlags(newClangModuleDetails,
                                        priorClangModuleDetails, false);
        }
      }
    } else
      recordDependency(dep.first.ModuleName, dep.second);

    if (dep.first.Kind == ModuleDependencyKind::Clang) {
      auto clangModuleDetails = dep.second.getAsClangModule();
      addSeenClangModule(clang::tooling::dependencies::ModuleID{
          dep.first.ModuleName, clangModuleDetails->contextHash});
    }
  }
}

void ModuleDependenciesCache::updateDependency(
    ModuleDependencyID moduleID, ModuleDependencyInfo dependencyInfo) {
  auto &map = getDependenciesMap(moduleID.Kind);
  assert(map.find(moduleID.ModuleName) != map.end() && "Not yet added to map");
  map.insert_or_assign(moduleID.ModuleName, std::move(dependencyInfo));
}

void ModuleDependenciesCache::removeDependency(ModuleDependencyID moduleID) {
  auto &map = getDependenciesMap(moduleID.Kind);
  map.erase(moduleID.ModuleName);
}

void
ModuleDependenciesCache::setImportedSwiftDependencies(ModuleDependencyID moduleID,
                                                      const ArrayRef<ModuleDependencyID> dependencyIDs) {
  auto dependencyInfo = findKnownDependency(moduleID);
  assert(dependencyInfo.getImportedSwiftDependencies().empty());
#ifndef NDEBUG
  for (const auto &depID : dependencyIDs)
    assert(depID.Kind != ModuleDependencyKind::Clang);
#endif
  // Copy the existing info to a mutable one we can then replace it with, after setting its overlay dependencies.
  auto updatedDependencyInfo = dependencyInfo;
  updatedDependencyInfo.setImportedSwiftDependencies(dependencyIDs);
  updateDependency(moduleID, updatedDependencyInfo);
}
void
ModuleDependenciesCache::setImportedClangDependencies(ModuleDependencyID moduleID,
                                                      const ArrayRef<ModuleDependencyID> dependencyIDs) {
  auto dependencyInfo = findKnownDependency(moduleID);
  assert(dependencyInfo.getImportedClangDependencies().empty());
#ifndef NDEBUG
  for (const auto &depID : dependencyIDs)
    assert(depID.Kind == ModuleDependencyKind::Clang);
#endif
  // Copy the existing info to a mutable one we can then replace it with, after setting its overlay dependencies.
  auto updatedDependencyInfo = dependencyInfo;
  updatedDependencyInfo.setImportedClangDependencies(dependencyIDs);
  updateDependency(moduleID, updatedDependencyInfo);
}
void
ModuleDependenciesCache::setHeaderClangDependencies(ModuleDependencyID moduleID,
                                                    const ArrayRef<ModuleDependencyID> dependencyIDs) {
  auto dependencyInfo = findKnownDependency(moduleID);
#ifndef NDEBUG
  for (const auto &depID : dependencyIDs)
    assert(depID.Kind == ModuleDependencyKind::Clang);
#endif
  // Copy the existing info to a mutable one we can then replace it with, after setting its overlay dependencies.
  auto updatedDependencyInfo = dependencyInfo;
  updatedDependencyInfo.setHeaderClangDependencies(dependencyIDs);
  updateDependency(moduleID, updatedDependencyInfo);
}
void ModuleDependenciesCache::setSwiftOverlayDependencies(ModuleDependencyID moduleID,
                                                          const ArrayRef<ModuleDependencyID> dependencyIDs) {
  auto dependencyInfo = findKnownDependency(moduleID);
  assert(dependencyInfo.getSwiftOverlayDependencies().empty());
#ifndef NDEBUG
  for (const auto &depID : dependencyIDs)
    assert(depID.Kind != ModuleDependencyKind::Clang);
#endif
  // Copy the existing info to a mutable one we can then replace it with, after setting its overlay dependencies.
  auto updatedDependencyInfo = dependencyInfo;
  updatedDependencyInfo.setSwiftOverlayDependencies(dependencyIDs);
  updateDependency(moduleID, updatedDependencyInfo);
}
void
ModuleDependenciesCache::setCrossImportOverlayDependencies(ModuleDependencyID moduleID,
                                                           const ArrayRef<ModuleDependencyID> dependencyIDs) {
  auto dependencyInfo = findKnownDependency(moduleID);
  assert(dependencyInfo.getCrossImportOverlayDependencies().empty());
  // Copy the existing info to a mutable one we can then replace it with, after setting its overlay dependencies.
  auto updatedDependencyInfo = dependencyInfo;
  updatedDependencyInfo.setCrossImportOverlayDependencies(dependencyIDs);
  updateDependency(moduleID, updatedDependencyInfo);
}

ModuleDependencyIDSetVector
ModuleDependenciesCache::getAllDependencies(const ModuleDependencyID &moduleID) const {
  const auto &moduleInfo = findKnownDependency(moduleID);
  ModuleDependencyIDSetVector result;
  if (moduleInfo.isSwiftModule()) {
    auto swiftImportedDepsRef = moduleInfo.getImportedSwiftDependencies();
    auto headerClangDepsRef = moduleInfo.getHeaderClangDependencies();
    auto overlayDependenciesRef = moduleInfo.getSwiftOverlayDependencies();
    result.insert(swiftImportedDepsRef.begin(),
                  swiftImportedDepsRef.end());
    result.insert(headerClangDepsRef.begin(),
                  headerClangDepsRef.end());
    result.insert(overlayDependenciesRef.begin(),
                  overlayDependenciesRef.end());
  }

  if (moduleInfo.isSwiftSourceModule()) {
    auto crossImportOverlayDepsRef = moduleInfo.getCrossImportOverlayDependencies();
    result.insert(crossImportOverlayDepsRef.begin(),
                  crossImportOverlayDepsRef.end());
  }

  auto clangImportedDepsRef = moduleInfo.getImportedClangDependencies();
  result.insert(clangImportedDepsRef.begin(),
                clangImportedDepsRef.end());

  return result;
}

ModuleDependencyIDSetVector
ModuleDependenciesCache::getClangDependencies(const ModuleDependencyID &moduleID) const {
  const auto &moduleInfo = findKnownDependency(moduleID);
  ModuleDependencyIDSetVector result;
  auto clangImportedDepsRef = moduleInfo.getImportedClangDependencies();
  result.insert(clangImportedDepsRef.begin(),
                clangImportedDepsRef.end());
  if (moduleInfo.isSwiftSourceModule() || moduleInfo.isSwiftBinaryModule()) {
    auto headerClangDepsRef = moduleInfo.getHeaderClangDependencies();
    result.insert(headerClangDepsRef.begin(),
                  headerClangDepsRef.end());
  }
  return result;
}

llvm::ArrayRef<ModuleDependencyID>
ModuleDependenciesCache::getImportedSwiftDependencies(const ModuleDependencyID &moduleID) const {
  const auto &moduleInfo = findKnownDependency(moduleID);
  assert(moduleInfo.isSwiftModule());
  return moduleInfo.getImportedSwiftDependencies();
}

llvm::ArrayRef<ModuleDependencyID>
ModuleDependenciesCache::getImportedClangDependencies(const ModuleDependencyID &moduleID) const {
  const auto &moduleInfo = findKnownDependency(moduleID);
  return moduleInfo.getImportedClangDependencies();
}

llvm::ArrayRef<ModuleDependencyID>
ModuleDependenciesCache::getHeaderClangDependencies(const ModuleDependencyID &moduleID) const {
  const auto &moduleInfo = findKnownDependency(moduleID);
  assert(moduleInfo.isSwiftModule());
  return moduleInfo.getHeaderClangDependencies();
}

llvm::ArrayRef<ModuleDependencyID>
ModuleDependenciesCache::getSwiftOverlayDependencies(const ModuleDependencyID &moduleID) const {
  const auto &moduleInfo = findKnownDependency(moduleID);
  assert(moduleInfo.isSwiftModule());
  return moduleInfo.getSwiftOverlayDependencies();
}

llvm::ArrayRef<ModuleDependencyID>
ModuleDependenciesCache::getCrossImportOverlayDependencies(const ModuleDependencyID &moduleID) const {
  const auto &moduleInfo = findKnownDependency(moduleID);
  assert(moduleInfo.isSwiftSourceModule());
  return moduleInfo.getCrossImportOverlayDependencies();
}

