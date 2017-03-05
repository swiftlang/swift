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
#include "swift/Strings.h"
#include "swift/AST/AST.h"
#include "swift/AST/DiagnosticsSema.h"
#include "swift/Basic/STLExtras.h"
#include "swift/Basic/SourceManager.h"
#include "swift/Basic/Version.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/Debug.h"
#include <system_error>

using namespace swift;

namespace {
typedef std::pair<Identifier, SourceLoc> AccessPathElem;
} // end unnamed namespace

// Defined out-of-line so that we can see ~ModuleFile.
SerializedModuleLoader::SerializedModuleLoader(ASTContext &ctx,
                                               DependencyTracker *tracker)
  : ModuleLoader(tracker), Ctx(ctx) {}
SerializedModuleLoader::~SerializedModuleLoader() = default;

static std::error_code
openModuleFiles(StringRef DirName, StringRef ModuleFilename,
                StringRef ModuleDocFilename,
                std::unique_ptr<llvm::MemoryBuffer> *ModuleBuffer,
                std::unique_ptr<llvm::MemoryBuffer> *ModuleDocBuffer,
                llvm::SmallVectorImpl<char> &Scratch) {
  assert(((ModuleBuffer && ModuleDocBuffer)
            || (!ModuleBuffer && !ModuleDocBuffer))
         && "Module and Module Doc buffer must both be initialized or NULL");
  // Try to open the module file first.  If we fail, don't even look for the
  // module documentation file.
  Scratch.clear();
  llvm::sys::path::append(Scratch, DirName, ModuleFilename);
  // If there are no buffers to load into, simply check for the existence of
  // the module file.
  if (!(ModuleBuffer || ModuleDocBuffer)) {
    return llvm::sys::fs::access(StringRef(Scratch.data(), Scratch.size()),
                                 llvm::sys::fs::AccessMode::Exist);
  }

  llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>> ModuleOrErr =
  llvm::MemoryBuffer::getFile(StringRef(Scratch.data(), Scratch.size()));
  if (!ModuleOrErr)
    return ModuleOrErr.getError();

  // Try to open the module documentation file.  If it does not exist, ignore
  // the error.  However, pass though all other errors.
  Scratch.clear();
  llvm::sys::path::append(Scratch, DirName, ModuleDocFilename);
  llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>> ModuleDocOrErr =
  llvm::MemoryBuffer::getFile(StringRef(Scratch.data(), Scratch.size()));
  if (!ModuleDocOrErr &&
      ModuleDocOrErr.getError() != std::errc::no_such_file_or_directory) {
    return ModuleDocOrErr.getError();
  }

  *ModuleBuffer = std::move(ModuleOrErr.get());
  if (ModuleDocOrErr)
    *ModuleDocBuffer = std::move(ModuleDocOrErr.get());

  return std::error_code();
}

static bool
findModule(ASTContext &ctx, AccessPathElem moduleID,
           std::unique_ptr<llvm::MemoryBuffer> *moduleBuffer,
           std::unique_ptr<llvm::MemoryBuffer> *moduleDocBuffer,
           bool &isFramework) {
  llvm::SmallString<64> moduleFilename(moduleID.first.str());
  moduleFilename += '.';
  moduleFilename += SERIALIZED_MODULE_EXTENSION;

  llvm::SmallString<64> moduleDocFilename(moduleID.first.str());
  moduleDocFilename += '.';
  moduleDocFilename += SERIALIZED_MODULE_DOC_EXTENSION;

  // FIXME: Which name should we be using here? Do we care about CPU subtypes?
  // FIXME: At the very least, don't hardcode "arch".
  llvm::SmallString<16> archFile{
      ctx.LangOpts.getPlatformConditionValue(PlatformConditionKind::Arch)};
  llvm::SmallString<16> archDocFile{archFile};
  if (!archFile.empty()) {
    archFile += '.';
    archFile += SERIALIZED_MODULE_EXTENSION;

    archDocFile += '.';
    archDocFile += SERIALIZED_MODULE_DOC_EXTENSION;
  }

  llvm::SmallString<128> scratch;
  llvm::SmallString<128> currPath;
  isFramework = false;
  for (auto path : ctx.SearchPathOpts.ImportSearchPaths) {
    auto err = openModuleFiles(path,
                               moduleFilename.str(), moduleDocFilename.str(),
                               moduleBuffer, moduleDocBuffer,
                               scratch);
    if (err == std::errc::is_a_directory) {
      currPath = path;
      llvm::sys::path::append(currPath, moduleFilename.str());
      err = openModuleFiles(currPath,
                            archFile.str(), archDocFile.str(),
                            moduleBuffer, moduleDocBuffer,
                            scratch);
    }
    if (!err)
      return true;
  }

  {
    llvm::SmallString<64> moduleFramework(moduleID.first.str());
    moduleFramework += ".framework";
    isFramework = true;

    for (const auto &framepath : ctx.SearchPathOpts.FrameworkSearchPaths) {
      currPath = framepath.Path;
      llvm::sys::path::append(currPath, moduleFramework.str(),
                              "Modules", moduleFilename.str());
      auto err = openModuleFiles(currPath,
                                 archFile.str(), archDocFile.str(),
                                 moduleBuffer, moduleDocBuffer,
                                 scratch);
      if (!err)
        return true;
    }
  }

  // If we're not allowed to look in the runtime library import path, stop.
  if (ctx.SearchPathOpts.SkipRuntimeLibraryImportPath)
    return false;

  // Search the runtime import path.
  isFramework = false;
  return !openModuleFiles(ctx.SearchPathOpts.RuntimeLibraryImportPath,
                          moduleFilename.str(), moduleDocFilename.str(),
                          moduleBuffer, moduleDocBuffer, scratch);
}

FileUnit *SerializedModuleLoader::loadAST(
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
    Ctx.bumpGeneration();

    M.setResilienceStrategy(extendedInfo.getResilienceStrategy());

    // We've loaded the file. Now try to bring it into the AST.
    auto fileUnit = new (Ctx) SerializedASTFile(M, *loadedModuleFile,
                                                extendedInfo.isSIB());
    M.addFile(*fileUnit);
    if (extendedInfo.isTestable())
      M.setTestingEnabled();

    auto diagLocOrInvalid = diagLoc.getValueOr(SourceLoc());
    loadInfo.status =
        loadedModuleFile->associateWithFileContext(fileUnit, diagLocOrInvalid);
    if (loadInfo.status == serialization::Status::Valid) {
      LoadedModuleFiles.emplace_back(std::move(loadedModuleFile),
                                     Ctx.getCurrentGeneration());
      return fileUnit;
    }

    M.removeFile(*fileUnit);
  }

  // This is the failure path. If we have a location, diagnose the issue.
  if (!diagLoc)
    return nullptr;

  auto diagnoseDifferentLanguageVersion = [&](StringRef shortVersion) -> bool {
    if (shortVersion.empty())
      return false;

    SmallString<32> versionBuf;
    llvm::raw_svector_ostream versionString(versionBuf);
    versionString << version::Version::getCurrentLanguageVersion();
    if (versionString.str() == shortVersion)
      return false;

    Ctx.Diags.diagnose(*diagLoc,
                       diag::serialization_module_language_version_mismatch,
                       loadInfo.shortVersion, versionString.str(),
                       moduleBufferID);
    return true;
  };

  switch (loadInfo.status) {
  case serialization::Status::Valid:
    llvm_unreachable("At this point we know loading has failed");

  case serialization::Status::FormatTooNew:
    if (diagnoseDifferentLanguageVersion(loadInfo.shortVersion))
      break;
    Ctx.Diags.diagnose(*diagLoc, diag::serialization_module_too_new,
                       moduleBufferID);
    break;
  case serialization::Status::FormatTooOld:
    if (diagnoseDifferentLanguageVersion(loadInfo.shortVersion))
      break;
    Ctx.Diags.diagnose(*diagLoc, diag::serialization_module_too_old,
                       M.getName(), moduleBufferID);
    break;
  case serialization::Status::Malformed:
    Ctx.Diags.diagnose(*diagLoc, diag::serialization_malformed_module,
                       moduleBufferID);
    break;

  case serialization::Status::MalformedDocumentation:
    assert(!moduleDocBufferID.empty());
    Ctx.Diags.diagnose(*diagLoc, diag::serialization_malformed_module,
                       moduleDocBufferID);
    break;

  case serialization::Status::MissingDependency: {
    // Figure out /which/ dependencies are missing.
    // FIXME: Dependencies should be de-duplicated at serialization time,
    // not now.
    llvm::StringMap<bool> duplicates;
    llvm::SmallVector<ModuleFile::Dependency, 4> missing;
    std::copy_if(loadedModuleFile->getDependencies().begin(),
                 loadedModuleFile->getDependencies().end(),
                 std::back_inserter(missing),
                 [&duplicates](const ModuleFile::Dependency &dependency)->bool {
      if (dependency.isLoaded() || dependency.isHeader())
        return false;
      bool &seen = duplicates[dependency.RawPath];
      if (seen)
        return false;
      seen = true;
      return true;
    });

    // FIXME: only show module part of RawAccessPath
    assert(!missing.empty() && "unknown missing dependency?");
    if (missing.size() == 1) {
      Ctx.Diags.diagnose(*diagLoc,diag::serialization_missing_single_dependency,
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

      Ctx.Diags.diagnose(*diagLoc, diag::serialization_missing_dependencies,
                         missingNames);
    }

    if (Ctx.SearchPathOpts.SDKPath.empty() &&
        llvm::Triple(llvm::sys::getProcessTriple()).isMacOSX()) {
      Ctx.Diags.diagnose(SourceLoc(), diag::sema_no_import_no_sdk);
      Ctx.Diags.diagnose(SourceLoc(), diag::sema_no_import_no_sdk_xcrun);
    }
    break;
  }

  case serialization::Status::MissingShadowedModule: {
    Ctx.Diags.diagnose(*diagLoc, diag::serialization_missing_shadowed_module,
                       M.getName());
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
    Ctx.Diags.diagnose(*diagLoc, diag::serialization_load_failed, M.getName());
    break;

  case serialization::Status::NameMismatch: {
    // FIXME: This doesn't handle a non-debugger REPL, which should also treat
    // this as a non-fatal error.
    auto diagKind = diag::serialization_name_mismatch;
    if (Ctx.LangOpts.DebuggerSupport)
      diagKind = diag::serialization_name_mismatch_repl;
    Ctx.Diags.diagnose(*diagLoc, diagKind,
                       loadInfo.name, M.getName());
    break;
  }

  case serialization::Status::TargetIncompatible: {
    // FIXME: This doesn't handle a non-debugger REPL, which should also treat
    // this as a non-fatal error.
    auto diagKind = diag::serialization_target_incompatible;
    if (Ctx.LangOpts.DebuggerSupport)
      diagKind = diag::serialization_target_incompatible_repl;
    Ctx.Diags.diagnose(*diagLoc, diagKind,
                       loadInfo.targetTriple, moduleBufferID);
    break;
  }

  case serialization::Status::TargetTooNew: {
    llvm::Triple moduleTarget(llvm::Triple::normalize(loadInfo.targetTriple));

    StringRef osName;
    unsigned major, minor, micro;
    if (moduleTarget.isMacOSX()) {
      osName = swift::prettyPlatformString(PlatformKind::OSX);
      moduleTarget.getMacOSXVersion(major, minor, micro);
    } else {
      osName = moduleTarget.getOSName();
      moduleTarget.getOSVersion(major, minor, micro);
    }

    // FIXME: This doesn't handle a non-debugger REPL, which should also treat
    // this as a non-fatal error.
    auto diagKind = diag::serialization_target_too_new;
    if (Ctx.LangOpts.DebuggerSupport)
      diagKind = diag::serialization_target_too_new_repl;
    Ctx.Diags.diagnose(*diagLoc, diagKind,
                       osName, major, minor, micro, moduleBufferID);
    break;
  }
  }

  return nullptr;
}

bool
SerializedModuleLoader::canImportModule(std::pair<Identifier, SourceLoc> mID) {
  // First see if we find it in the registered memory buffers.
  if (!MemoryBuffers.empty()) {
    auto bufIter = MemoryBuffers.find(mID.first.str());
    if (bufIter != MemoryBuffers.end()) {
      return true;
    }
  }

  // Otherwise look on disk.
  bool isFramework = false;
  return findModule(Ctx, mID, nullptr, nullptr, isFramework);
}

ModuleDecl *SerializedModuleLoader::loadModule(SourceLoc importLoc,
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
    if (!findModule(Ctx, moduleID, &moduleInputBuffer, &moduleDocInputBuffer,
                    isFramework)) {
      return nullptr;
    }

    addDependency(moduleInputBuffer->getBufferIdentifier());
  }

  assert(moduleInputBuffer);

  auto M = ModuleDecl::create(moduleID.first, Ctx);
  Ctx.LoadedModules[moduleID.first] = M;

  if (!loadAST(*M, moduleID.second, std::move(moduleInputBuffer),
               std::move(moduleDocInputBuffer), isFramework)) {
    M->setFailedToLoad();
  }

  return M;
}

void SerializedModuleLoader::loadExtensions(NominalTypeDecl *nominal,
                                            unsigned previousGeneration) {
  for (auto &modulePair : LoadedModuleFiles) {
    if (modulePair.second <= previousGeneration)
      continue;
    modulePair.first->loadExtensions(nominal);
  }
}

void SerializedModuleLoader::loadObjCMethods(
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

void SerializedModuleLoader::verifyAllModules() {
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
    if (File.getAssociatedModule()->getResilienceStrategy()
        == ResilienceStrategy::Fragile) {
      collectLinkLibrariesFromImports(callback);
    }
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

const clang::Module *SerializedASTFile::getUnderlyingClangModule() {
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
