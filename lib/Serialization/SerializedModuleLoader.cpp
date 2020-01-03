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
#include "ModuleFile.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/DiagnosticsSema.h"
#include "swift/Basic/Defer.h"
#include "swift/Basic/FileTypes.h"
#include "swift/Basic/Platform.h"
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

/// Apply \c body for each target-specific module file base name to search from
/// most to least desiable.
void forEachTargetModuleBasename(const ASTContext &Ctx,
                                 llvm::function_ref<void(StringRef)> body) {
  auto normalizedTarget = getTargetSpecificModuleTriple(Ctx.LangOpts.Target);
  body(normalizedTarget.str());

  // We used the un-normalized architecture as a target-specific
  // module name. Fall back to that behavior.
  body(Ctx.LangOpts.Target.getArchName());

  // FIXME: We used to use "major architecture" names for these files---the
  // names checked in "#if arch(...)". Fall back to that name in the one case
  // where it's different from what Swift 4.2 supported: 32-bit ARM platforms.
  // We should be able to drop this once there's an Xcode that supports the
  // new names.
  if (Ctx.LangOpts.Target.getArch() == llvm::Triple::ArchType::arm)
    body("arm");
}

enum class SearchPathKind {
  Import,
  Framework,
  RuntimeLibrary,
};

/// Apply \p body for each module search path in \p Ctx until \p body returns
/// non-None value. Returns the return value from \p body, or \c None.
Optional<bool> forEachModuleSearchPath(
    const ASTContext &Ctx,
    llvm::function_ref<Optional<bool>(StringRef, SearchPathKind, bool isSystem)>
        callback) {
  for (const auto &path : Ctx.SearchPathOpts.ImportSearchPaths)
    if (auto result =
            callback(path, SearchPathKind::Import, /*isSystem=*/false))
      return result;

  for (const auto &path : Ctx.SearchPathOpts.FrameworkSearchPaths)
    if (auto result =
            callback(path.Path, SearchPathKind::Framework, path.IsSystem))
      return result;

  // Apple platforms have extra implicit framework search paths:
  // $SDKROOT/System/Library/Frameworks/ and $SDKROOT/Library/Frameworks/.
  if (Ctx.LangOpts.Target.isOSDarwin()) {
    SmallString<128> scratch;
    scratch = Ctx.SearchPathOpts.SDKPath;
    llvm::sys::path::append(scratch, "System", "Library", "Frameworks");
    if (auto result =
            callback(scratch, SearchPathKind::Framework, /*isSystem=*/true))
      return result;

    scratch = Ctx.SearchPathOpts.SDKPath;
    llvm::sys::path::append(scratch, "Library", "Frameworks");
    if (auto result =
            callback(scratch, SearchPathKind::Framework, /*isSystem=*/true))
      return result;
  }

  for (auto importPath : Ctx.SearchPathOpts.RuntimeLibraryImportPaths) {
    if (auto result = callback(importPath, SearchPathKind::RuntimeLibrary,
                               /*isSystem=*/true))
      return result;
  }

  return None;
}
} // end unnamed namespace

// Defined out-of-line so that we can see ~ModuleFile.
SerializedModuleLoaderBase::SerializedModuleLoaderBase(
    ASTContext &ctx, DependencyTracker *tracker, ModuleLoadingMode loadMode,
    bool IgnoreSwiftSourceInfoFile)
    : ModuleLoader(tracker), Ctx(ctx), LoadMode(loadMode),
      IgnoreSwiftSourceInfoFile(IgnoreSwiftSourceInfoFile) {}

SerializedModuleLoaderBase::~SerializedModuleLoaderBase() = default;
SerializedModuleLoader::~SerializedModuleLoader() = default;
MemoryBufferSerializedModuleLoader::~MemoryBufferSerializedModuleLoader() =
    default;

void SerializedModuleLoaderBase::collectVisibleTopLevelModuleNamesImpl(
    SmallVectorImpl<Identifier> &names, StringRef extension) const {
  llvm::SmallString<16> moduleSuffix;
  moduleSuffix += '.';
  moduleSuffix += file_types::getExtension(file_types::TY_SwiftModuleFile);

  llvm::SmallString<16> suffix;
  suffix += '.';
  suffix += extension;

  SmallVector<SmallString<64>, 2> targetFiles;
  forEachTargetModuleBasename(Ctx, [&](StringRef targetName) {
    targetFiles.emplace_back(targetName);
    targetFiles.back() += suffix;
  });

  auto &fs = *Ctx.SourceMgr.getFileSystem();

  // Apply \p body for each directory entry in \p dirPath.
  auto forEachDirectoryEntryPath =
      [&](StringRef dirPath, llvm::function_ref<void(StringRef)> body) {
        std::error_code errorCode;
        llvm::vfs::directory_iterator DI = fs.dir_begin(dirPath, errorCode);
        llvm::vfs::directory_iterator End;
        for (; !errorCode && DI != End; DI.increment(errorCode))
          body(DI->path());
      };

  // Check whether target specific module file exists or not in given directory.
  // $PATH/{arch}.{extension}
  auto checkTargetFiles = [&](StringRef path) -> bool {
    llvm::SmallString<256> scratch;
    for (auto targetFile : targetFiles) {
      scratch.clear();
      llvm::sys::path::append(scratch, path, targetFile);
      // If {arch}.{extension} exists, consider it's visible. Technically, we
      // should check the file type, permission, format, etc., but it's too
      // heavy to do that for each files.
      if (fs.exists(scratch))
        return true;
    }
    return false;
  };

  forEachModuleSearchPath(Ctx, [&](StringRef searchPath, SearchPathKind Kind,
                                   bool isSystem) {
    switch (Kind) {
    case SearchPathKind::Import: {
      // Look for:
      // $PATH/{name}.swiftmodule/{arch}.{extension} or
      // $PATH/{name}.{extension}
      forEachDirectoryEntryPath(searchPath, [&](StringRef path) {
        auto pathExt = llvm::sys::path::extension(path);
        if (pathExt != moduleSuffix && pathExt != suffix)
          return;

        auto stat = fs.status(path);
        if (!stat)
          return;
        if (pathExt == moduleSuffix && stat->isDirectory()) {
          if (!checkTargetFiles(path))
            return;
        } else if (pathExt != suffix || stat->isDirectory()) {
          return;
        }
        // Extract module name.
        auto name = llvm::sys::path::filename(path).drop_back(pathExt.size());
        names.push_back(Ctx.getIdentifier(name));
      });
      return None;
    }
    case SearchPathKind::RuntimeLibrary: {
      // Look for:
      // (Darwin OS) $PATH/{name}.swiftmodule/{arch}.{extension}
      // (Other OS)  $PATH/{name}.{extension}
      bool requireTargetSpecificModule = Ctx.LangOpts.Target.isOSDarwin();
      forEachDirectoryEntryPath(searchPath, [&](StringRef path) {
        auto pathExt = llvm::sys::path::extension(path);
        if (requireTargetSpecificModule) {
          if (pathExt != moduleSuffix)
            return;
          if (!checkTargetFiles(path))
            return;
        } else {
          if (suffix != pathExt)
            return;
          auto stat = fs.status(path);
          if (!stat || stat->isDirectory())
            return;
        }
        // Extract module name.
        auto name = llvm::sys::path::filename(path).drop_back(pathExt.size());
        names.push_back(Ctx.getIdentifier(name));
      });
      return None;
    }
    case SearchPathKind::Framework: {
      // Look for:
      // $PATH/{name}.framework/Modules/{name}.swiftmodule/{arch}.{extension}
      forEachDirectoryEntryPath(searchPath, [&](StringRef path) {
        if (llvm::sys::path::extension(path) != ".framework")
          return;

        // Extract Framework name.
        auto name = llvm::sys::path::filename(path).drop_back(
            StringLiteral(".framework").size());

        SmallString<256> moduleDir;
        llvm::sys::path::append(moduleDir, path, "Modules",
                                name + moduleSuffix);
        if (!checkTargetFiles(moduleDir))
          return;

        names.push_back(Ctx.getIdentifier(name));
      });
      return None;
    }
    }
    llvm_unreachable("covered switch");
  });
}

void SerializedModuleLoader::collectVisibleTopLevelModuleNames(
    SmallVectorImpl<Identifier> &names) const {
  collectVisibleTopLevelModuleNamesImpl(
      names, file_types::getExtension(file_types::TY_SwiftModuleFile));
}

std::error_code SerializedModuleLoaderBase::openModuleDocFile(
  AccessPathElem ModuleID, StringRef ModuleDocPath,
  std::unique_ptr<llvm::MemoryBuffer> *ModuleDocBuffer) {

  if (!ModuleDocBuffer)
    return std::error_code();

  llvm::vfs::FileSystem &FS = *Ctx.SourceMgr.getFileSystem();

  // Try to open the module documentation file.  If it does not exist, ignore
  // the error.  However, pass though all other errors.
  llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>> ModuleDocOrErr =
    FS.getBufferForFile(ModuleDocPath);
  if (ModuleDocOrErr) {
    *ModuleDocBuffer = std::move(*ModuleDocOrErr);
  } else if (ModuleDocOrErr.getError() !=
               std::errc::no_such_file_or_directory) {
    return ModuleDocOrErr.getError();
  }

  return std::error_code();
}

void
SerializedModuleLoaderBase::openModuleSourceInfoFileIfPresent(
    AccessPathElem ModuleID,
    StringRef ModulePath,
    StringRef ModuleSourceInfoFilename,
    std::unique_ptr<llvm::MemoryBuffer> *ModuleSourceInfoBuffer) {
  if (!ModuleSourceInfoBuffer)
    return;
  llvm::vfs::FileSystem &FS = *Ctx.SourceMgr.getFileSystem();
  llvm::SmallString<128> PathWithoutProjectDir(ModulePath);
  llvm::sys::path::replace_extension(PathWithoutProjectDir,
                  file_types::getExtension(file_types::TY_SwiftSourceInfoFile));
  llvm::SmallString<128> PathWithProjectDir = PathWithoutProjectDir.str();
  StringRef FileName = llvm::sys::path::filename(PathWithoutProjectDir);
  llvm::sys::path::remove_filename(PathWithProjectDir);
  llvm::sys::path::append(PathWithProjectDir, "Project");
  llvm::sys::path::append(PathWithProjectDir, FileName);

  // Try to open the module source info file from the "Project" directory.
  // If it does not exist, ignore the error.
  if (auto ModuleSourceInfoOrErr = FS.getBufferForFile(PathWithProjectDir)) {
    *ModuleSourceInfoBuffer = std::move(*ModuleSourceInfoOrErr);
    return;
  }
  // Try to open the module source info file adjacent to the .swiftmodule file.
  if (auto ModuleSourceInfoOrErr = FS.getBufferForFile(PathWithoutProjectDir)) {
    *ModuleSourceInfoBuffer = std::move(*ModuleSourceInfoOrErr);
    return;
  }
}

std::error_code SerializedModuleLoaderBase::openModuleFiles(
    AccessPathElem ModuleID, StringRef ModulePath, StringRef ModuleDocPath,
    StringRef ModuleSourceInfoFileName,
    std::unique_ptr<llvm::MemoryBuffer> *ModuleBuffer,
    std::unique_ptr<llvm::MemoryBuffer> *ModuleDocBuffer,
    std::unique_ptr<llvm::MemoryBuffer> *ModuleSourceInfoBuffer) {
  assert(((ModuleBuffer && ModuleDocBuffer) ||
          (!ModuleBuffer && !ModuleDocBuffer)) &&
         "Module and Module Doc buffer must both be initialized or NULL");

  llvm::vfs::FileSystem &FS = *Ctx.SourceMgr.getFileSystem();

  // Try to open the module file first.  If we fail, don't even look for the
  // module documentation file.

  // If there are no buffers to load into, simply check for the existence of
  // the module file.
  if (!(ModuleBuffer || ModuleDocBuffer)) {
    llvm::ErrorOr<llvm::vfs::Status> statResult = FS.status(ModulePath);
    if (!statResult)
      return statResult.getError();
    if (!statResult->exists())
      return std::make_error_code(std::errc::no_such_file_or_directory);
    // FIXME: llvm::vfs::FileSystem doesn't give us information on whether or
    // not we can /read/ the file without actually trying to do so.
    return std::error_code();
  }

  llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>> ModuleOrErr =
      FS.getBufferForFile(ModulePath);
  if (!ModuleOrErr)
    return ModuleOrErr.getError();
  if (!IgnoreSwiftSourceInfoFile) {
    // Open .swiftsourceinfo file if it's present.
    openModuleSourceInfoFileIfPresent(ModuleID, ModulePath,
                                      ModuleSourceInfoFileName,
                                      ModuleSourceInfoBuffer);
  }
  auto ModuleDocErr =
    openModuleDocFile(ModuleID, ModuleDocPath, ModuleDocBuffer);
  if (ModuleDocErr)
    return ModuleDocErr;

  *ModuleBuffer = std::move(ModuleOrErr.get());

  return std::error_code();
}

std::error_code SerializedModuleLoader::findModuleFilesInDirectory(
    AccessPathElem ModuleID, StringRef DirPath, StringRef ModuleFilename,
    StringRef ModuleDocFilename, StringRef ModuleSourceInfoFileName,
    SmallVectorImpl<char> *ModuleInterfacePath,
    std::unique_ptr<llvm::MemoryBuffer> *ModuleBuffer,
    std::unique_ptr<llvm::MemoryBuffer> *ModuleDocBuffer,
    std::unique_ptr<llvm::MemoryBuffer> *ModuleSourceInfoBuffer) {
  if (LoadMode == ModuleLoadingMode::OnlyInterface)
    return std::make_error_code(std::errc::not_supported);

  llvm::SmallString<256> ModulePath{DirPath};
  llvm::sys::path::append(ModulePath, ModuleFilename);
  llvm::SmallString<256> ModuleDocPath{DirPath};
  llvm::sys::path::append(ModuleDocPath, ModuleDocFilename);
  return SerializedModuleLoaderBase::openModuleFiles(ModuleID,
                                                     ModulePath,
                                                     ModuleDocPath,
                                                     ModuleSourceInfoFileName,
                                                     ModuleBuffer,
                                                     ModuleDocBuffer,
                                                     ModuleSourceInfoBuffer);
}

bool SerializedModuleLoader::maybeDiagnoseTargetMismatch(
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

  Ctx.Diags.diagnose(sourceLocation, diag::sema_no_import_target, moduleName,
                     archName, foundArchs);
  return true;
}

struct ModuleFilenamePair {
  llvm::SmallString<64> module;
  llvm::SmallString<64> moduleDoc;
  llvm::SmallString<64> moduleSourceInfo;

  ModuleFilenamePair(StringRef baseName)
    : module(baseName), moduleDoc(baseName), moduleSourceInfo(baseName)
  {
    module += '.';
    module += file_types::getExtension(file_types::TY_SwiftModuleFile);

    moduleDoc += '.';
    moduleDoc += file_types::getExtension(file_types::TY_SwiftModuleDocFile);

    moduleSourceInfo += '.';
    moduleSourceInfo += file_types::getExtension(file_types::TY_SwiftSourceInfoFile);
  }
};

bool
SerializedModuleLoaderBase::findModule(AccessPathElem moduleID,
           SmallVectorImpl<char> *moduleInterfacePath,
           std::unique_ptr<llvm::MemoryBuffer> *moduleBuffer,
           std::unique_ptr<llvm::MemoryBuffer> *moduleDocBuffer,
           std::unique_ptr<llvm::MemoryBuffer> *moduleSourceInfoBuffer,
           bool &isFramework, bool &isSystemModule) {
  llvm::SmallString<64> moduleName(moduleID.first.str());
  ModuleFilenamePair fileNames(moduleName);

  SmallVector<ModuleFilenamePair, 4> targetFileNamePairs;
  SmallString<32> primaryTargetSpecificName;
  forEachTargetModuleBasename(Ctx, [&](StringRef targetName) {
    targetFileNamePairs.emplace_back(targetName);
    if (primaryTargetSpecificName.empty())
      primaryTargetSpecificName = targetName;
  });

  auto &fs = *Ctx.SourceMgr.getFileSystem();

  llvm::SmallString<256> currPath;

  /// Returns true if a target-specific module file was found, false if an error
  /// was diagnosed, or None if neither one happened and the search should
  /// continue.
  auto findTargetSpecificModuleFiles = [&]() -> Optional<bool> {
    for (const auto &targetFileNames : targetFileNamePairs) {
      auto result = findModuleFilesInDirectory(moduleID, currPath,
                        targetFileNames.module, targetFileNames.moduleDoc,
                        targetFileNames.moduleSourceInfo,
                        moduleInterfacePath,
                        moduleBuffer, moduleDocBuffer,
                        moduleSourceInfoBuffer);
      if (!result) {
        return true;
      } else if (result == std::errc::not_supported) {
        return false;
      } else if (result != std::errc::no_such_file_or_directory) {
        return None;
      }
    }

    // We can only get here if all targetFileNamePairs failed with
    // 'std::errc::no_such_file_or_directory'.
    if (maybeDiagnoseTargetMismatch(moduleID.second, moduleName,
                                    primaryTargetSpecificName, currPath)) {
      return false;
    } else {
      return None;
    }
  };

  auto result = forEachModuleSearchPath(
      Ctx,
      [&](StringRef path, SearchPathKind Kind,
          bool isSystem) -> Optional<bool> {
        currPath = path;
        isSystemModule = isSystem;

        switch (Kind) {
        case SearchPathKind::Import:
        case SearchPathKind::RuntimeLibrary: {
          isFramework = false;
          llvm::sys::path::append(currPath, fileNames.module.str());

          bool checkTargetSpecificModule;
          if (Kind == SearchPathKind::RuntimeLibrary) {
            // Apple platforms always use target-specific files within a
            // .swiftmodule directory for the stdlib; non-Apple platforms
            // always use single-architecture swiftmodules.
            checkTargetSpecificModule = Ctx.LangOpts.Target.isOSDarwin();
          } else {
            llvm::ErrorOr<llvm::vfs::Status> statResult = fs.status(currPath);
            // Even if stat fails, we can't just return the error; the path
            // we're looking for might not be "Foo.swiftmodule".
            checkTargetSpecificModule = statResult && statResult->isDirectory();
          }

          if (checkTargetSpecificModule)
            // A .swiftmodule directory contains architecture-specific files.
            return findTargetSpecificModuleFiles();

          auto result = findModuleFilesInDirectory(
              moduleID, path, fileNames.module.str(), fileNames.moduleDoc.str(),
              fileNames.moduleSourceInfo.str(), moduleInterfacePath,
              moduleBuffer, moduleDocBuffer, moduleSourceInfoBuffer);
          if (!result)
            return true;
          else if (result == std::errc::not_supported)
            return false;
          else
            return None;
        }
        case SearchPathKind::Framework: {
          isFramework = true;
          llvm::sys::path::append(currPath,
                                  moduleID.first.str() + ".framework");

          // Check if the framework directory exists.
          if (!fs.exists(currPath))
            return None;

          // Frameworks always use architecture-specific files within a
          // .swiftmodule directory.
          llvm::sys::path::append(currPath, "Modules", fileNames.module.str());
          return findTargetSpecificModuleFiles();
        }
        }
        llvm_unreachable("covered switch");
      });
  return result.getValueOr(false);
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
    StringRef moduleInterfacePath,
    std::unique_ptr<llvm::MemoryBuffer> moduleInputBuffer,
    std::unique_ptr<llvm::MemoryBuffer> moduleDocInputBuffer,
    std::unique_ptr<llvm::MemoryBuffer> moduleSourceInfoInputBuffer,
    bool isFramework, bool treatAsPartialModule) {
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
      ModuleFile::load(moduleInterfacePath,
                       std::move(moduleInputBuffer),
                       std::move(moduleDocInputBuffer),
                       std::move(moduleSourceInfoInputBuffer),
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
        loadedModuleFile->associateWithFileContext(fileUnit, diagLocOrInvalid,
                                                   treatAsPartialModule);

    // FIXME: This seems wrong. Overlay for system Clang module doesn't
    // necessarily mean it's "system" module. User can make their own overlay
    // in non-system directory.
    // Remove this block after we fix the test suite.
    if (auto shadowed = loadedModuleFile->getUnderlyingModule())
      if (shadowed->isSystemModule())
        M.setIsSystemModule(true);

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
          if (dependency.isLoaded() || dependency.isHeader() ||
              dependency.isImplementationOnly()) {
            return false;
          }
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
                        return next.isLoaded() &&
                               !next.Import.second->hasResolvedImports();
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

  case serialization::Status::MissingUnderlyingModule: {
    Ctx.Diags.diagnose(diagLoc, diag::serialization_missing_underlying_module,
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
    Ctx.Diags.diagnose(diagLoc, diag::serialization_load_failed,
                       ModuleName.str());
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

bool SerializedModuleLoaderBase::canImportModule(
    std::pair<Identifier, SourceLoc> mID) {
  // Look on disk.
  SmallVector<char, 0> *unusedModuleInterfacePath = nullptr;
  std::unique_ptr<llvm::MemoryBuffer> *unusedModuleBuffer = nullptr;
  std::unique_ptr<llvm::MemoryBuffer> *unusedModuleDocBuffer = nullptr;
  std::unique_ptr<llvm::MemoryBuffer> *unusedModuleSourceInfoBuffer = nullptr;
  bool isFramework = false;
  bool isSystemModule = false;
  return findModule(mID, unusedModuleInterfacePath, unusedModuleBuffer,
                    unusedModuleDocBuffer, unusedModuleSourceInfoBuffer,
                    isFramework, isSystemModule);
}

bool MemoryBufferSerializedModuleLoader::canImportModule(
    std::pair<Identifier, SourceLoc> mID) {
  // See if we find it in the registered memory buffers.
  return MemoryBuffers.count(mID.first.str());
}

ModuleDecl *
SerializedModuleLoaderBase::loadModule(SourceLoc importLoc,
                                       ModuleDecl::AccessPathTy path) {
  // FIXME: Swift submodules?
  if (path.size() > 1)
    return nullptr;

  auto moduleID = path[0];
  bool isFramework = false;
  bool isSystemModule = false;

  llvm::SmallString<256> moduleInterfacePath;
  std::unique_ptr<llvm::MemoryBuffer> moduleInputBuffer;
  std::unique_ptr<llvm::MemoryBuffer> moduleDocInputBuffer;
  std::unique_ptr<llvm::MemoryBuffer> moduleSourceInfoInputBuffer;

  // Look on disk.
  if (!findModule(moduleID, &moduleInterfacePath, &moduleInputBuffer,
                  &moduleDocInputBuffer, &moduleSourceInfoInputBuffer,
                  isFramework, isSystemModule)) {
    return nullptr;
  }
  if (dependencyTracker) {
    // Don't record cached artifacts as dependencies.
    StringRef DepPath = moduleInputBuffer->getBufferIdentifier();
    if (!isCached(DepPath)) {
      dependencyTracker->addDependency(DepPath, /*isSystem=*/false);
    }
  }

  assert(moduleInputBuffer);

  auto M = ModuleDecl::create(moduleID.first, Ctx);
  M->setIsSystemModule(isSystemModule);
  Ctx.LoadedModules[moduleID.first] = M;
  SWIFT_DEFER { M->setHasResolvedImports(); };

  StringRef moduleInterfacePathStr =
    Ctx.AllocateCopy(moduleInterfacePath.str());

  if (!loadAST(*M, moduleID.second, moduleInterfacePathStr,
               std::move(moduleInputBuffer), std::move(moduleDocInputBuffer),
               std::move(moduleSourceInfoInputBuffer),
               isFramework, /*treatAsPartialModule*/false)) {
    M->setFailedToLoad();
  }

  return M;
}

ModuleDecl *
MemoryBufferSerializedModuleLoader::loadModule(SourceLoc importLoc,
                                               ModuleDecl::AccessPathTy path) {
  // FIXME: Swift submodules?
  if (path.size() > 1)
    return nullptr;

  auto moduleID = path[0];

  // See if we find it in the registered memory buffers.

  // FIXME: Right now this works only with access paths of length 1.
  // Once submodules are designed, this needs to support suffix
  // matching and a search path.
  auto bufIter = MemoryBuffers.find(moduleID.first.str());
  if (bufIter == MemoryBuffers.end())
    return nullptr;

  bool isFramework = false;
  bool treatAsPartialModule = false;
  std::unique_ptr<llvm::MemoryBuffer> moduleInputBuffer;
  moduleInputBuffer = std::move(bufIter->second);
  MemoryBuffers.erase(bufIter);
  assert(moduleInputBuffer);

  auto *M = ModuleDecl::create(moduleID.first, Ctx);
  SWIFT_DEFER { M->setHasResolvedImports(); };

  if (!loadAST(*M, moduleID.second, /*moduleInterfacePath*/ "",
               std::move(moduleInputBuffer), {}, {},
               isFramework, treatAsPartialModule)) {
    return nullptr;
  }

  Ctx.LoadedModules[moduleID.first] = M;
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

std::error_code MemoryBufferSerializedModuleLoader::findModuleFilesInDirectory(
    AccessPathElem ModuleID, StringRef DirPath, StringRef ModuleFilename,
    StringRef ModuleDocFilename, StringRef ModuleSourceInfoFilename,
    SmallVectorImpl<char> *ModuleInterfacePath,
    std::unique_ptr<llvm::MemoryBuffer> *ModuleBuffer,
    std::unique_ptr<llvm::MemoryBuffer> *ModuleDocBuffer,
    std::unique_ptr<llvm::MemoryBuffer> *ModuleSourceInfoBuffer) {
  // This is a soft error instead of an llvm_unreachable because this API is
  // primarily used by LLDB which makes it more likely that unwitting changes to
  // the Swift compiler accidentally break the contract.
  assert(false && "not supported");
  return std::make_error_code(std::errc::not_supported);
}

bool MemoryBufferSerializedModuleLoader::maybeDiagnoseTargetMismatch(
    SourceLoc sourceLocation, StringRef moduleName, StringRef archName,
    StringRef directoryPath) {
  return false;
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
  ModuleDecl::ImportFilter ImportFilter;
  ImportFilter |= ModuleDecl::ImportFilterKind::Public;
  ImportFilter |= ModuleDecl::ImportFilterKind::Private;

  llvm::SmallVector<ModuleDecl::ImportedModule, 8> Imports;
  File.getImportedModules(Imports, ImportFilter);

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
  if (auto Mod = File.getUnderlyingModule()) {
    return Mod->isSystemModule();
  }
  return false;
}

void SerializedASTFile::lookupValue(DeclName name, NLKind lookupKind,
                                    SmallVectorImpl<ValueDecl*> &results) const{
  File.lookupValue(name, results);
}

StringRef
SerializedASTFile::getFilenameForPrivateDecl(const ValueDecl *decl) const {
  return File.FilenamesForPrivateValues.lookup(decl);
}

TypeDecl *SerializedASTFile::lookupLocalType(llvm::StringRef MangledName) const{
  return File.lookupLocalType(MangledName);
}

OpaqueTypeDecl *
SerializedASTFile::lookupOpaqueResultType(StringRef MangledName) {
  return File.lookupOpaqueResultType(MangledName);
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

Optional<BasicDeclLocs>
SerializedASTFile::getBasicLocsForDecl(const Decl *D) const {
  return File.getBasicDeclLocsForDecl(D);
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

void SerializedASTFile::getTopLevelDeclsWhereAttributesMatch(
              SmallVectorImpl<Decl*> &results,
              llvm::function_ref<bool(DeclAttributes)> matchAttributes) const {
  File.getTopLevelDecls(results, matchAttributes);
}

void SerializedASTFile::getPrecedenceGroups(
       SmallVectorImpl<PrecedenceGroupDecl*> &results) const {
  File.getPrecedenceGroups(results);
}

void
SerializedASTFile::getLocalTypeDecls(SmallVectorImpl<TypeDecl*> &results) const{
  File.getLocalTypeDecls(results);
}

void
SerializedASTFile::getOpaqueReturnTypeDecls(
                              SmallVectorImpl<OpaqueTypeDecl*> &results) const {
  File.getOpaqueReturnTypeDecls(results);
}

void
SerializedASTFile::getDisplayDecls(SmallVectorImpl<Decl*> &results) const {
  File.getDisplayDecls(results);
}

StringRef SerializedASTFile::getFilename() const {
  return File.getModuleFilename();
}

const clang::Module *SerializedASTFile::getUnderlyingClangModule() const {
  if (auto *UnderlyingModule = File.getUnderlyingModule())
    return UnderlyingModule->findUnderlyingClangModule();
  return nullptr;
}

Identifier
SerializedASTFile::getDiscriminatorForPrivateValue(const ValueDecl *D) const {
  Identifier discriminator = File.getDiscriminatorForPrivateValue(D);
  assert(!discriminator.empty() && "no discriminator found for value");
  return discriminator;
}
