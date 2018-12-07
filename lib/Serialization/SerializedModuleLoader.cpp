//===--- SerializedModuleLoader.cpp - Import Swift modules ----------------===//
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

#include "swift/Serialization/SerializedModuleLoader.h"
#include "swift/Serialization/ModuleFile.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/DiagnosticsSema.h"
#include "swift/Basic/Defer.h"
#include "swift/Basic/FileTypes.h"
#include "swift/Basic/STLExtras.h"
#include "swift/Basic/SourceManager.h"
#include "swift/Basic/Version.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/StringSet.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/Debug.h"
#include <system_error>

using namespace swift;
using swift::version::Version;

namespace {
} // end unnamed namespace

// Defined out-of-line so that we can see ~ModuleFile.
SerializedModuleLoaderBase::SerializedModuleLoaderBase(ASTContext &ctx,
                                                       DependencyTracker *tracker,
                                                       ModuleLoadingMode loadMode)
  : ModuleLoader(tracker), Ctx(ctx), LoadMode(loadMode) {}
SerializedModuleLoaderBase::~SerializedModuleLoaderBase() = default;

SerializedModuleLoader::~SerializedModuleLoader() = default;

std::error_code SerializedModuleLoaderBase::openModuleFiles(
    AccessPathElem ModuleID, StringRef DirName, StringRef ModuleFilename,
    StringRef ModuleDocFilename,
    std::unique_ptr<llvm::MemoryBuffer> *ModuleBuffer,
    std::unique_ptr<llvm::MemoryBuffer> *ModuleDocBuffer,
    llvm::SmallVectorImpl<char> &Scratch) {
  assert(((ModuleBuffer && ModuleDocBuffer) ||
          (!ModuleBuffer && !ModuleDocBuffer)) &&
         "Module and Module Doc buffer must both be initialized or NULL");

  llvm::vfs::FileSystem &FS = *Ctx.SourceMgr.getFileSystem();

  // Try to open the module file first.  If we fail, don't even look for the
  // module documentation file.
  Scratch.clear();
  llvm::sys::path::append(Scratch, DirName, ModuleFilename);
  // If there are no buffers to load into, simply check for the existence of
  // the module file.
  if (!(ModuleBuffer || ModuleDocBuffer)) {
    llvm::ErrorOr<llvm::vfs::Status> statResult = FS.status(Scratch);
    if (!statResult)
      return statResult.getError();
    if (!statResult->exists())
      return std::make_error_code(std::errc::no_such_file_or_directory);
    // FIXME: llvm::vfs::FileSystem doesn't give us information on whether or
    // not we can /read/ the file without actually trying to do so.
    return std::error_code();
  }

  llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>> ModuleOrErr =
      FS.getBufferForFile(StringRef(Scratch.data(), Scratch.size()));
  if (!ModuleOrErr)
    return ModuleOrErr.getError();

  // Try to open the module documentation file.  If it does not exist, ignore
  // the error.  However, pass though all other errors.
  Scratch.clear();
  llvm::sys::path::append(Scratch, DirName, ModuleDocFilename);
  llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>> ModuleDocOrErr =
      FS.getBufferForFile(StringRef(Scratch.data(), Scratch.size()));
  if (!ModuleDocOrErr &&
      ModuleDocOrErr.getError() != std::errc::no_such_file_or_directory) {
    return ModuleDocOrErr.getError();
  }

  *ModuleBuffer = std::move(ModuleOrErr.get());
  if (ModuleDocOrErr)
    *ModuleDocBuffer = std::move(ModuleDocOrErr.get());

  return std::error_code();
}

std::error_code SerializedModuleLoader::openModuleFiles(
    AccessPathElem ModuleID, StringRef DirName, StringRef ModuleFilename,
    StringRef ModuleDocFilename,
    std::unique_ptr<llvm::MemoryBuffer> *ModuleBuffer,
    std::unique_ptr<llvm::MemoryBuffer> *ModuleDocBuffer,
    llvm::SmallVectorImpl<char> &Scratch) {
  if (LoadMode == ModuleLoadingMode::OnlyParseable)
    return std::make_error_code(std::errc::not_supported);

  return SerializedModuleLoaderBase::openModuleFiles(ModuleID, DirName,
                                                     ModuleFilename,
                                                     ModuleDocFilename,
                                                     ModuleBuffer,
                                                     ModuleDocBuffer, Scratch);
}

bool SerializedModuleLoader::maybeDiagnoseArchitectureMismatch(
    SourceLoc sourceLocation, StringRef moduleName, StringRef archName,
    StringRef directoryPath) {
  llvm::vfs::FileSystem &fs = *Ctx.SourceMgr.getFileSystem();

  std::error_code errorCode;
  std::string foundArchs;
  for (llvm::vfs::directory_iterator directoryIterator =
           fs.dir_begin(directoryPath, errorCode), endIterator;
       directoryIterator != endIterator;
       directoryIterator.increment(errorCode)) {
    if (errorCode)
      return false;
    StringRef filePath = directoryIterator->path();
    StringRef extension = llvm::sys::path::extension(filePath);
    if (file_types::lookupTypeForExtension(extension) ==
          file_types::TY_SwiftModuleFile) {
      if (!foundArchs.empty())
        foundArchs += ", ";
      foundArchs += llvm::sys::path::stem(filePath).str();
    }
  }

  if (foundArchs.empty()) {
    // Maybe this swiftmodule directory only contains swiftinterfaces, or
    // maybe something else is going on. Regardless, we shouldn't emit a
    // possibly incorrect diagnostic.
    return false;
  }

  Ctx.Diags.diagnose(sourceLocation, diag::sema_no_import_arch, moduleName,
                     archName, foundArchs);
  return true;
}

static std::pair<llvm::SmallString<16>, llvm::SmallString<16>>
getArchSpecificModuleFileNames(StringRef archName) {
  llvm::SmallString<16> archFile, archDocFile;

  if (!archName.empty()) {
    archFile += archName;
    archFile += '.';
    archFile += file_types::getExtension(file_types::TY_SwiftModuleFile);

    archDocFile += archName;
    archDocFile += '.';
    archDocFile += file_types::getExtension(file_types::TY_SwiftModuleDocFile);
  }

  return {archFile, archDocFile};
}

bool
SerializedModuleLoaderBase::findModule(AccessPathElem moduleID,
           std::unique_ptr<llvm::MemoryBuffer> *moduleBuffer,
           std::unique_ptr<llvm::MemoryBuffer> *moduleDocBuffer,
           bool &isFramework) {
  llvm::SmallString<64> moduleName(moduleID.first.str());
  llvm::SmallString<64> moduleFilename(moduleName);
  moduleFilename += '.';
  moduleFilename += file_types::getExtension(file_types::TY_SwiftModuleFile);

  llvm::SmallString<64> moduleDocFilename(moduleID.first.str());
  moduleDocFilename += '.';
  moduleDocFilename +=
      file_types::getExtension(file_types::TY_SwiftModuleDocFile);

  StringRef archName = Ctx.LangOpts.Target.getArchName();
  auto archFileNames = getArchSpecificModuleFileNames(archName);

  // FIXME: We used to use "major architecture" names for these files---the
  // names checked in "#if arch(...)". Fall back to that name in the one case
  // where it's different from what Swift 4.2 supported: 32-bit ARM platforms.
  // We should be able to drop this once there's an Xcode that supports the
  // new names.
  StringRef alternateArchName;
  if (Ctx.LangOpts.Target.getArch() == llvm::Triple::ArchType::arm)
    alternateArchName = "arm";
  auto alternateArchFileNames =
      getArchSpecificModuleFileNames(alternateArchName);

  auto &fs = *Ctx.SourceMgr.getFileSystem();
  isFramework = false;

  // Declare some reusable buffers up front; if we end up spilling onto the
  // heap once, we're likely to do so again.
  llvm::SmallString<128> scratch;
  llvm::SmallString<128> currPath;
  for (auto path : Ctx.SearchPathOpts.ImportSearchPaths) {
    std::error_code result;

    currPath = path;
    llvm::sys::path::append(currPath, moduleFilename.str());
    llvm::ErrorOr<llvm::vfs::Status> statResult = fs.status(currPath);

    if (statResult && statResult->isDirectory()) {
      // A .swiftmodule directory contains architecture-specific files.
      result = openModuleFiles(moduleID, currPath,
                               archFileNames.first, archFileNames.second,
                               moduleBuffer, moduleDocBuffer,
                               scratch);

      if (result == std::errc::no_such_file_or_directory &&
          !alternateArchName.empty()) {
        result = openModuleFiles(moduleID, currPath,
                                 alternateArchFileNames.first,
                                 alternateArchFileNames.second,
                                 moduleBuffer, moduleDocBuffer,
                                 scratch);
      }

      if (result == std::errc::no_such_file_or_directory) {
        if (maybeDiagnoseArchitectureMismatch(moduleID.second, moduleName,
                                              archName, currPath)) {
          return false;
        }
      }

    } else {
      // We can't just return the error; the path we're looking for might not
      // be "Foo.swiftmodule".
      result = openModuleFiles(moduleID, path,
                               moduleFilename.str(), moduleDocFilename.str(),
                               moduleBuffer, moduleDocBuffer,
                               scratch);
    }
    if (!result)
      return true;
  }

  {
    llvm::SmallString<64> moduleFramework(moduleID.first.str());
    moduleFramework += ".framework";
    isFramework = true;

    auto tryFrameworkImport = [&](StringRef frameworkPath) -> bool {
      currPath = frameworkPath;
      llvm::sys::path::append(currPath, moduleFramework.str());

      // Check if the framework directory exists
      if (!fs.exists(currPath)) {
        return false;
      }

      // Frameworks always use architecture-specific files within a .swiftmodule
      // directory.
      llvm::sys::path::append(currPath, "Modules", moduleFilename.str());
      auto err = openModuleFiles(moduleID, currPath,
                                 archFileNames.first, archFileNames.second,
                                 moduleBuffer, moduleDocBuffer, scratch);

      if (err == std::errc::no_such_file_or_directory &&
          !alternateArchName.empty()) {
        err = openModuleFiles(moduleID, currPath,
                              alternateArchFileNames.first,
                              alternateArchFileNames.second,
                              moduleBuffer, moduleDocBuffer, scratch);
      }

      if (err == std::errc::no_such_file_or_directory) {
        if (maybeDiagnoseArchitectureMismatch(moduleID.second, moduleName,
                                              archName, currPath)) {
          return false;
        }
      }

      return !err;
    };

    for (const auto &framepath : Ctx.SearchPathOpts.FrameworkSearchPaths) {
      if (tryFrameworkImport(framepath.Path))
        return true;
    }

    if (Ctx.LangOpts.Target.isOSDarwin()) {
      // Apple platforms have extra implicit framework search paths:
      // $SDKROOT/System/Library/Frameworks/ and $SDKROOT/Library/Frameworks/
      scratch = Ctx.SearchPathOpts.SDKPath;
      llvm::sys::path::append(scratch, "System", "Library", "Frameworks");
      if (tryFrameworkImport(scratch))
        return true;

      scratch = Ctx.SearchPathOpts.SDKPath;
      llvm::sys::path::append(scratch, "Library", "Frameworks");
      if (tryFrameworkImport(scratch))
        return true;
    }
  }

  // If we're not allowed to look in the runtime library import path, stop.
  if (Ctx.SearchPathOpts.SkipRuntimeLibraryImportPath)
    return false;

  // Search the runtime import path.
  isFramework = false;
  return !openModuleFiles(moduleID, Ctx.SearchPathOpts.RuntimeLibraryImportPath,
                          moduleFilename.str(), moduleDocFilename.str(),
                          moduleBuffer, moduleDocBuffer, scratch);
}

static std::pair<StringRef, clang::VersionTuple>
getOSAndVersionForDiagnostics(const llvm::Triple &triple) {
  StringRef osName;
  unsigned major, minor, micro;
  if (triple.isMacOSX()) {
    // macOS triples represent their versions differently, so we have to use the
    // special accessor.
    triple.getMacOSXVersion(major, minor, micro);
    osName = swift::prettyPlatformString(PlatformKind::OSX);
  } else {
    triple.getOSVersion(major, minor, micro);
    if (triple.isWatchOS()) {
      osName = swift::prettyPlatformString(PlatformKind::watchOS);
    } else if (triple.isTvOS()) {
      assert(triple.isiOS() &&
             "LLVM treats tvOS as a kind of iOS, so tvOS is checked first");
      osName = swift::prettyPlatformString(PlatformKind::tvOS);
    } else if (triple.isiOS()) {
      osName = swift::prettyPlatformString(PlatformKind::iOS);
    } else {
      assert(!triple.isOSDarwin() && "unknown Apple OS");
      // Fallback to the LLVM triple name. This isn't great (it won't be
      // capitalized or anything), but it's better than nothing.
      osName = triple.getOSName();
    }
  }

  assert(!osName.empty());
  clang::VersionTuple version;
  if (micro != 0)
    version = clang::VersionTuple(major, minor, micro);
  else
    version = clang::VersionTuple(major, minor);
  return {osName, version};
}

FileUnit *SerializedModuleLoaderBase::loadAST(
    ModuleDecl &M, Optional<SourceLoc> diagLoc,
    std::unique_ptr<llvm::MemoryBuffer> moduleInputBuffer,
    std::unique_ptr<llvm::MemoryBuffer> moduleDocInputBuffer,
    bool isFramework) {
  assert(moduleInputBuffer);

  StringRef moduleBufferID = moduleInputBuffer->getBufferIdentifier();
  StringRef moduleDocBufferID;
  if (moduleDocInputBuffer)
    moduleDocBufferID = moduleDocInputBuffer->getBufferIdentifier();

  if (moduleInputBuffer->getBufferSize() % 4 != 0) {
    if (diagLoc)
      Ctx.Diags.diagnose(*diagLoc, diag::serialization_malformed_module,
                         moduleBufferID);
    return nullptr;
  }

  serialization::ExtendedValidationInfo extendedInfo;
  std::unique_ptr<ModuleFile> loadedModuleFile;
  serialization::ValidationInfo loadInfo =
      ModuleFile::load(std::move(moduleInputBuffer),
                       std::move(moduleDocInputBuffer),
                       isFramework, loadedModuleFile,
                       &extendedInfo);
  if (loadInfo.status == serialization::Status::Valid) {
    M.setResilienceStrategy(extendedInfo.getResilienceStrategy());

    // We've loaded the file. Now try to bring it into the AST.
    auto fileUnit = new (Ctx) SerializedASTFile(M, *loadedModuleFile,
                                                extendedInfo.isSIB());
    M.addFile(*fileUnit);
    if (extendedInfo.isTestable())
      M.setTestingEnabled();
    if (extendedInfo.arePrivateImportsEnabled())
      M.setPrivateImportsEnabled();

    auto diagLocOrInvalid = diagLoc.getValueOr(SourceLoc());
    loadInfo.status =
        loadedModuleFile->associateWithFileContext(fileUnit, diagLocOrInvalid);
    if (loadInfo.status == serialization::Status::Valid) {
      Ctx.bumpGeneration();
      LoadedModuleFiles.emplace_back(std::move(loadedModuleFile),
                                     Ctx.getCurrentGeneration());
      return fileUnit;
    }

    M.removeFile(*fileUnit);
  }

  // From here on is the failure path.

  // Even though the module failed to load, it's possible its contents include
  // a source buffer that need to survive because it's already been used for
  // diagnostics.
  if (auto orphanedBuffer = loadedModuleFile->takeBufferForDiagnostics())
    OrphanedMemoryBuffers.push_back(std::move(orphanedBuffer));

  if (diagLoc)
    serialization::diagnoseSerializedASTLoadFailure(
        Ctx, *diagLoc, loadInfo, extendedInfo, moduleBufferID,
        moduleDocBufferID, loadedModuleFile.get(), M.getName());
  return nullptr;
}

void swift::serialization::diagnoseSerializedASTLoadFailure(
    ASTContext &Ctx, SourceLoc diagLoc,
    const serialization::ValidationInfo &loadInfo,
    const serialization::ExtendedValidationInfo &extendedInfo,
    StringRef moduleBufferID, StringRef moduleDocBufferID,
    ModuleFile *loadedModuleFile, Identifier ModuleName) {
  auto diagnoseDifferentLanguageVersion = [&](StringRef shortVersion) -> bool {
    if (shortVersion.empty())
      return false;

    SmallString<32> versionBuf;
    llvm::raw_svector_ostream versionString(versionBuf);
    versionString << Version::getCurrentLanguageVersion();
    if (versionString.str() == shortVersion)
      return false;

    Ctx.Diags.diagnose(
        diagLoc, diag::serialization_module_language_version_mismatch,
        loadInfo.shortVersion, versionString.str(), moduleBufferID);
    return true;
  };

  switch (loadInfo.status) {
  case serialization::Status::Valid:
    llvm_unreachable("At this point we know loading has failed");

  case serialization::Status::FormatTooNew:
    if (diagnoseDifferentLanguageVersion(loadInfo.shortVersion))
      break;
    Ctx.Diags.diagnose(diagLoc, diag::serialization_module_too_new,
                       moduleBufferID);
    break;
  case serialization::Status::FormatTooOld:
    if (diagnoseDifferentLanguageVersion(loadInfo.shortVersion))
      break;
    Ctx.Diags.diagnose(diagLoc, diag::serialization_module_too_old, ModuleName,
                       moduleBufferID);
    break;
  case serialization::Status::Malformed:
    Ctx.Diags.diagnose(diagLoc, diag::serialization_malformed_module,
                       moduleBufferID);
    break;

  case serialization::Status::MalformedDocumentation:
    assert(!moduleDocBufferID.empty());
    Ctx.Diags.diagnose(diagLoc, diag::serialization_malformed_module,
                       moduleDocBufferID);
    break;

  case serialization::Status::MissingDependency: {
    // Figure out /which/ dependencies are missing.
    // FIXME: Dependencies should be de-duplicated at serialization time,
    // not now.
    llvm::StringSet<> duplicates;
    llvm::SmallVector<ModuleFile::Dependency, 4> missing;
    std::copy_if(
        loadedModuleFile->getDependencies().begin(),
        loadedModuleFile->getDependencies().end(), std::back_inserter(missing),
        [&duplicates](const ModuleFile::Dependency &dependency) -> bool {
          if (dependency.isLoaded() || dependency.isHeader())
            return false;
          return duplicates.insert(dependency.RawPath).second;
        });

    // FIXME: only show module part of RawAccessPath
    assert(!missing.empty() && "unknown missing dependency?");
    if (missing.size() == 1) {
      Ctx.Diags.diagnose(diagLoc, diag::serialization_missing_single_dependency,
                         missing.front().getPrettyPrintedPath());
    } else {
      llvm::SmallString<64> missingNames;
      missingNames += '\'';
      interleave(missing,
                 [&](const ModuleFile::Dependency &next) {
                   missingNames += next.getPrettyPrintedPath();
                 },
                 [&] { missingNames += "', '"; });
      missingNames += '\'';

      Ctx.Diags.diagnose(diagLoc, diag::serialization_missing_dependencies,
                         missingNames);
    }

    if (Ctx.SearchPathOpts.SDKPath.empty() &&
        llvm::Triple(llvm::sys::getProcessTriple()).isMacOSX()) {
      Ctx.Diags.diagnose(SourceLoc(), diag::sema_no_import_no_sdk);
      Ctx.Diags.diagnose(SourceLoc(), diag::sema_no_import_no_sdk_xcrun);
    }
    break;
  }

  case serialization::Status::CircularDependency: {
    auto circularDependencyIter =
        llvm::find_if(loadedModuleFile->getDependencies(),
                      [](const ModuleFile::Dependency &next) {
                        return !next.Import.second->hasResolvedImports();
                      });
    assert(circularDependencyIter !=
               loadedModuleFile->getDependencies().end() &&
           "circular dependency reported, but no module with unresolved "
           "imports found");

    // FIXME: We should include the path of the circularity as well, but that's
    // hard because we're discovering this /while/ resolving imports, which
    // means the problematic modules haven't been recorded yet.
    Ctx.Diags.diagnose(diagLoc, diag::serialization_circular_dependency,
                       circularDependencyIter->getPrettyPrintedPath(),
                       ModuleName);
    break;
  }

  case serialization::Status::MissingShadowedModule: {
    Ctx.Diags.diagnose(diagLoc, diag::serialization_missing_shadowed_module,
                       ModuleName);
    if (Ctx.SearchPathOpts.SDKPath.empty() &&
        llvm::Triple(llvm::sys::getProcessTriple()).isMacOSX()) {
      Ctx.Diags.diagnose(SourceLoc(), diag::sema_no_import_no_sdk);
      Ctx.Diags.diagnose(SourceLoc(), diag::sema_no_import_no_sdk_xcrun);
    }
    break;
  }

  case serialization::Status::FailedToLoadBridgingHeader:
    // We already emitted a diagnostic about the bridging header. Just emit
    // a generic message here.
    Ctx.Diags.diagnose(diagLoc, diag::serialization_load_failed, ModuleName);
    break;

  case serialization::Status::NameMismatch: {
    // FIXME: This doesn't handle a non-debugger REPL, which should also treat
    // this as a non-fatal error.
    auto diagKind = diag::serialization_name_mismatch;
    if (Ctx.LangOpts.DebuggerSupport)
      diagKind = diag::serialization_name_mismatch_repl;
    Ctx.Diags.diagnose(diagLoc, diagKind, loadInfo.name, ModuleName.str());
    break;
  }

  case serialization::Status::TargetIncompatible: {
    // FIXME: This doesn't handle a non-debugger REPL, which should also treat
    // this as a non-fatal error.
    auto diagKind = diag::serialization_target_incompatible;
    if (Ctx.LangOpts.DebuggerSupport)
      diagKind = diag::serialization_target_incompatible_repl;
    Ctx.Diags.diagnose(diagLoc, diagKind, ModuleName, loadInfo.targetTriple,
                       moduleBufferID);
    break;
  }

  case serialization::Status::TargetTooNew: {
    llvm::Triple moduleTarget(llvm::Triple::normalize(loadInfo.targetTriple));

    std::pair<StringRef, clang::VersionTuple> moduleOSInfo =
        getOSAndVersionForDiagnostics(moduleTarget);
    std::pair<StringRef, clang::VersionTuple> compilationOSInfo =
        getOSAndVersionForDiagnostics(Ctx.LangOpts.Target);

    // FIXME: This doesn't handle a non-debugger REPL, which should also treat
    // this as a non-fatal error.
    auto diagKind = diag::serialization_target_too_new;
    if (Ctx.LangOpts.DebuggerSupport)
      diagKind = diag::serialization_target_too_new_repl;
    Ctx.Diags.diagnose(diagLoc, diagKind, compilationOSInfo.first,
                       compilationOSInfo.second, ModuleName,
                       moduleOSInfo.second, moduleBufferID);
    break;
  }
  }
}

bool
SerializedModuleLoaderBase::canImportModule(std::pair<Identifier, SourceLoc> mID) {
  // First see if we find it in the registered memory buffers.
  if (!MemoryBuffers.empty()) {
    auto bufIter = MemoryBuffers.find(mID.first.str());
    if (bufIter != MemoryBuffers.end()) {
      return true;
    }
  }

  // Otherwise look on disk.
  bool isFramework = false;
  return findModule(mID, nullptr, nullptr, isFramework);
}

ModuleDecl *SerializedModuleLoaderBase::loadModule(SourceLoc importLoc,
                                                   ModuleDecl::AccessPathTy path) {
  // FIXME: Swift submodules?
  if (path.size() > 1)
    return nullptr;

  auto moduleID = path[0];
  bool isFramework = false;

  std::unique_ptr<llvm::MemoryBuffer> moduleInputBuffer;
  std::unique_ptr<llvm::MemoryBuffer> moduleDocInputBuffer;
  // First see if we find it in the registered memory buffers.
  if (!MemoryBuffers.empty()) {
    // FIXME: Right now this works only with access paths of length 1.
    // Once submodules are designed, this needs to support suffix
    // matching and a search path.
    auto bufIter = MemoryBuffers.find(moduleID.first.str());
    if (bufIter != MemoryBuffers.end()) {
      moduleInputBuffer = std::move(bufIter->second);
      MemoryBuffers.erase(bufIter);
    }
  }

  // Otherwise look on disk.
  if (!moduleInputBuffer) {
    if (!findModule(moduleID, &moduleInputBuffer, &moduleDocInputBuffer,
                    isFramework)) {
      return nullptr;
    }
    if (dependencyTracker)
      dependencyTracker->addDependency(moduleInputBuffer->getBufferIdentifier(),
                                       /*isSystem=*/false);
  }

  assert(moduleInputBuffer);

  auto M = ModuleDecl::create(moduleID.first, Ctx);
  Ctx.LoadedModules[moduleID.first] = M;
  SWIFT_DEFER { M->setHasResolvedImports(); };

  if (!loadAST(*M, moduleID.second, std::move(moduleInputBuffer),
               std::move(moduleDocInputBuffer), isFramework)) {
    M->setFailedToLoad();
  }

  return M;
}

void SerializedModuleLoaderBase::loadExtensions(NominalTypeDecl *nominal,
                                                unsigned previousGeneration) {
  for (auto &modulePair : LoadedModuleFiles) {
    if (modulePair.second <= previousGeneration)
      continue;
    modulePair.first->loadExtensions(nominal);
  }
}

void SerializedModuleLoaderBase::loadObjCMethods(
       ClassDecl *classDecl,
       ObjCSelector selector,
       bool isInstanceMethod,
       unsigned previousGeneration,
       llvm::TinyPtrVector<AbstractFunctionDecl *> &methods) {
  for (auto &modulePair : LoadedModuleFiles) {
    if (modulePair.second <= previousGeneration)
      continue;
    modulePair.first->loadObjCMethods(classDecl, selector, isInstanceMethod,
                                      methods);
  }
}

void SerializedModuleLoaderBase::verifyAllModules() {
#ifndef NDEBUG
  for (const LoadedModulePair &loaded : LoadedModuleFiles)
    loaded.first->verify();
#endif
}

//-----------------------------------------------------------------------------
// SerializedASTFile implementation
//-----------------------------------------------------------------------------

void SerializedASTFile::getImportedModules(
    SmallVectorImpl<ModuleDecl::ImportedModule> &imports,
    ModuleDecl::ImportFilter filter) const {
  File.getImportedModules(imports, filter);
}

void SerializedASTFile::collectLinkLibrariesFromImports(
    ModuleDecl::LinkLibraryCallback callback) const {
  llvm::SmallVector<ModuleDecl::ImportedModule, 8> Imports;
  File.getImportedModules(Imports, ModuleDecl::ImportFilter::All);

  for (auto Import : Imports)
    Import.second->collectLinkLibraries(callback);
}

void SerializedASTFile::collectLinkLibraries(
    ModuleDecl::LinkLibraryCallback callback) const {
  if (isSIB()) {
    collectLinkLibrariesFromImports(callback);
  } else {
    File.collectLinkLibraries(callback);
  }
}

bool SerializedASTFile::isSystemModule() const {
  if (auto Mod = File.getShadowedModule()) {
    return Mod->isSystemModule();
  }
  return false;
}

void SerializedASTFile::lookupValue(ModuleDecl::AccessPathTy accessPath,
                                    DeclName name, NLKind lookupKind,
                                    SmallVectorImpl<ValueDecl*> &results) const{
  if (!ModuleDecl::matchesAccessPath(accessPath, name))
    return;
  
  File.lookupValue(name, results);
}

TypeDecl *SerializedASTFile::lookupLocalType(llvm::StringRef MangledName) const{
  return File.lookupLocalType(MangledName);
}

TypeDecl *
SerializedASTFile::lookupNestedType(Identifier name,
                                    const NominalTypeDecl *parent) const {
  return File.lookupNestedType(name, parent);
}

OperatorDecl *SerializedASTFile::lookupOperator(Identifier name,
                                                DeclKind fixity) const {
  return File.lookupOperator(name, fixity);
}

PrecedenceGroupDecl *
SerializedASTFile::lookupPrecedenceGroup(Identifier name) const {
  return File.lookupPrecedenceGroup(name);
}

void SerializedASTFile::lookupVisibleDecls(ModuleDecl::AccessPathTy accessPath,
                                           VisibleDeclConsumer &consumer,
                                           NLKind lookupKind) const {
  File.lookupVisibleDecls(accessPath, consumer, lookupKind);
}

void SerializedASTFile::lookupClassMembers(ModuleDecl::AccessPathTy accessPath,
                                           VisibleDeclConsumer &consumer) const{
  File.lookupClassMembers(accessPath, consumer);
}

void
SerializedASTFile::lookupClassMember(ModuleDecl::AccessPathTy accessPath,
                                     DeclName name,
                                     SmallVectorImpl<ValueDecl*> &decls) const {
  File.lookupClassMember(accessPath, name, decls);
}

void SerializedASTFile::lookupObjCMethods(
       ObjCSelector selector,
       SmallVectorImpl<AbstractFunctionDecl *> &results) const {
  File.lookupObjCMethods(selector, results);
}

Optional<CommentInfo>
SerializedASTFile::getCommentForDecl(const Decl *D) const {
  return File.getCommentForDecl(D);
}

Optional<StringRef>
SerializedASTFile::getGroupNameForDecl(const Decl *D) const {
  return File.getGroupNameForDecl(D);
}


Optional<StringRef>
SerializedASTFile::getSourceFileNameForDecl(const Decl *D) const {
  return File.getSourceFileNameForDecl(D);
}

Optional<unsigned>
SerializedASTFile::getSourceOrderForDecl(const Decl *D) const {
  return File.getSourceOrderForDecl(D);
}

void
SerializedASTFile::collectAllGroups(std::vector<StringRef> &Names) const {
  File.collectAllGroups(Names);
};

Optional<StringRef>
SerializedASTFile::getGroupNameByUSR(StringRef USR) const {
  return File.getGroupNameByUSR(USR);
}

void
SerializedASTFile::getTopLevelDecls(SmallVectorImpl<Decl*> &results) const {
  File.getTopLevelDecls(results);
}

void SerializedASTFile::getPrecedenceGroups(
       SmallVectorImpl<PrecedenceGroupDecl*> &results) const {
  File.getPrecedenceGroups(results);
}

void
SerializedASTFile::getLocalTypeDecls(SmallVectorImpl<TypeDecl*> &results) const{
  File.getLocalTypeDecls(results);
}

void SerializedASTFile::getDisplayDecls(SmallVectorImpl<Decl*> &results) const {
  File.getDisplayDecls(results);
}

StringRef SerializedASTFile::getFilename() const {
  return File.getModuleFilename();
}

const clang::Module *SerializedASTFile::getUnderlyingClangModule() const {
  if (auto *ShadowedModule = File.getShadowedModule())
    return ShadowedModule->findUnderlyingClangModule();
  return nullptr;
}

Identifier
SerializedASTFile::getDiscriminatorForPrivateValue(const ValueDecl *D) const {
  Identifier discriminator = File.getDiscriminatorForPrivateValue(D);
  assert(!discriminator.empty() && "no discriminator found for value");
  return discriminator;
}
