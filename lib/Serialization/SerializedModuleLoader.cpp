//===--- SerializedModuleLoader.cpp - Import Swift modules ----------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/Serialization/SerializedModuleLoader.h"
#include "swift/Serialization/ModuleFile.h"
#include "swift/Strings.h"
#include "swift/AST/AST.h"
#include "swift/AST/DiagnosticsSema.h"
#include "swift/Basic/STLExtras.h"
#include "swift/Basic/SourceManager.h"
#include "llvm/ADT/SmallString.h"
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
                std::unique_ptr<llvm::MemoryBuffer> &ModuleBuffer,
                std::unique_ptr<llvm::MemoryBuffer> &ModuleDocBuffer,
                llvm::SmallVectorImpl<char> &Scratch) {
  // Try to open the module file first.  If we fail, don't even look for the
  // module documentation file.
  Scratch.clear();
  llvm::sys::path::append(Scratch, DirName, ModuleFilename);
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
  ModuleBuffer = std::move(ModuleOrErr.get());
  if (ModuleDocOrErr)
    ModuleDocBuffer = std::move(ModuleDocOrErr.get());
  return std::error_code();
}

static std::error_code
findModule(ASTContext &ctx, AccessPathElem moduleID,
           std::unique_ptr<llvm::MemoryBuffer> &moduleBuffer,
           std::unique_ptr<llvm::MemoryBuffer> &moduleDocBuffer,
           bool &isFramework) {
  llvm::SmallString<64> moduleFilename(moduleID.first.str());
  moduleFilename += '.';
  moduleFilename += SERIALIZED_MODULE_EXTENSION;

  llvm::SmallString<64> moduleDocFilename(moduleID.first.str());
  moduleDocFilename += '.';
  moduleDocFilename += SERIALIZED_MODULE_DOC_EXTENSION;

  // FIXME: Which name should we be using here? Do we care about CPU subtypes?
  // FIXME: At the very least, don't hardcode "arch".
  llvm::SmallString<16> archFile(ctx.LangOpts.getTargetConfigOption("arch"));
  llvm::SmallString<16> archDocFile(ctx.LangOpts.getTargetConfigOption("arch"));
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
    if (!err || err != std::errc::no_such_file_or_directory)
      return err;
  }

  {
    llvm::SmallString<64> moduleFramework(moduleID.first.str());
    moduleFramework += ".framework";
    isFramework = true;

    for (auto path : ctx.SearchPathOpts.FrameworkSearchPaths) {
      currPath = path;
      llvm::sys::path::append(currPath, moduleFramework.str(),
                              "Modules", moduleFilename.str());
      auto err = openModuleFiles(currPath,
                                 archFile.str(), archDocFile.str(),
                                 moduleBuffer, moduleDocBuffer,
                                 scratch);
      if (!err || err != std::errc::no_such_file_or_directory)
        return err;
    }
  }

  // If we're not allowed to look in the runtime library import path, stop.
  if (ctx.SearchPathOpts.SkipRuntimeLibraryImportPath)
    return std::make_error_code(std::errc::no_such_file_or_directory);

  // Search the runtime import path.
  isFramework = false;
  return openModuleFiles(ctx.SearchPathOpts.RuntimeLibraryImportPath,
                         moduleFilename.str(), moduleDocFilename.str(),
                         moduleBuffer, moduleDocBuffer, scratch);
}

FileUnit *SerializedModuleLoader::loadAST(
    Module &M, Optional<SourceLoc> diagLoc,
    std::unique_ptr<llvm::MemoryBuffer> moduleInputBuffer,
    std::unique_ptr<llvm::MemoryBuffer> moduleDocInputBuffer,
    bool isFramework) {
  assert(moduleInputBuffer);

  const char *moduleBufferID = moduleInputBuffer->getBufferIdentifier();
  const char *moduleDocBufferID = nullptr;
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
  serialization::Status err = ModuleFile::load(std::move(moduleInputBuffer),
                                               std::move(moduleDocInputBuffer),
                                               isFramework, loadedModuleFile,
                                               &extendedInfo);
  if (err == serialization::Status::Valid) {
    Ctx.bumpGeneration();

    // We've loaded the file. Now try to bring it into the AST.
    auto fileUnit = new (Ctx) SerializedASTFile(M, *loadedModuleFile,
                                                extendedInfo.isSIB());
    M.addFile(*fileUnit);
    if (extendedInfo.isTestable())
      M.setTestingEnabled();

    auto diagLocOrInvalid = diagLoc.getValueOr(SourceLoc());
    err = loadedModuleFile->associateWithFileContext(fileUnit,
                                                     diagLocOrInvalid);
    if (err == serialization::Status::Valid) {
      LoadedModuleFiles.emplace_back(std::move(loadedModuleFile),
                                     Ctx.getCurrentGeneration());
      return fileUnit;
    }

    M.removeFile(*fileUnit);
  }

  // This is the failure path. If we have a location, diagnose the issue.
  if (!diagLoc)
    return nullptr;

  switch (loadedModuleFile->getStatus()) {
  case serialization::Status::Valid:
    llvm_unreachable("At this point we know loading has failed");

  case serialization::Status::FormatTooNew:
    Ctx.Diags.diagnose(*diagLoc, diag::serialization_module_too_new,
                       moduleBufferID);
    break;
  case serialization::Status::FormatTooOld:
    Ctx.Diags.diagnose(*diagLoc, diag::serialization_module_too_old,
                       moduleBufferID);
    break;
  case serialization::Status::Malformed:
    Ctx.Diags.diagnose(*diagLoc, diag::serialization_malformed_module,
                       moduleBufferID);
    break;

  case serialization::Status::MalformedDocumentation:
    assert(moduleDocBufferID);
    Ctx.Diags.diagnose(*diagLoc, diag::serialization_malformed_module,
                       moduleDocBufferID ? moduleDocBufferID : "");
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

    if (Ctx.SearchPathOpts.SDKPath.empty()) {
      Ctx.Diags.diagnose(SourceLoc(), diag::sema_no_import_no_sdk);
      Ctx.Diags.diagnose(SourceLoc(), diag::sema_no_import_no_sdk_xcrun);
    }
    break;
  }

  case serialization::Status::MissingShadowedModule: {
    Ctx.Diags.diagnose(*diagLoc, diag::serialization_missing_shadowed_module,
                       M.Name);
    if (Ctx.SearchPathOpts.SDKPath.empty()) {
      Ctx.Diags.diagnose(SourceLoc(), diag::sema_no_import_no_sdk);
      Ctx.Diags.diagnose(SourceLoc(), diag::sema_no_import_no_sdk_xcrun);
    }
    break;
  }

  case serialization::Status::NameMismatch: {
    // FIXME: This doesn't handle a non-debugger REPL, which should also treat
    // this as a non-fatal error.
    auto diagKind = diag::serialization_name_mismatch;
    if (Ctx.LangOpts.DebuggerSupport)
      diagKind = diag::serialization_name_mismatch_repl;
    Ctx.Diags.diagnose(*diagLoc, diagKind,
                       loadedModuleFile->getModuleName(), M.Name);
    break;
  }

  case serialization::Status::TargetIncompatible: {
    // FIXME: This doesn't handle a non-debugger REPL, which should also treat
    // this as a non-fatal error.
    auto diagKind = diag::serialization_target_incompatible;
    if (Ctx.LangOpts.DebuggerSupport)
      diagKind = diag::serialization_target_incompatible_repl;
    Ctx.Diags.diagnose(*diagLoc, diagKind,
                       loadedModuleFile->getTargetTriple(), moduleBufferID);
    break;
  }

  case serialization::Status::TargetTooNew: {
    StringRef moduleTargetTriple = loadedModuleFile->getTargetTriple();
    llvm::Triple moduleTarget(llvm::Triple::normalize(moduleTargetTriple));

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

Module *SerializedModuleLoader::loadModule(SourceLoc importLoc,
                                           Module::AccessPathTy path) {
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
    if (std::error_code err = findModule(Ctx, moduleID, moduleInputBuffer,
                                         moduleDocInputBuffer,
                                         isFramework)) {
      if (err != std::errc::no_such_file_or_directory) {
        Ctx.Diags.diagnose(moduleID.second, diag::sema_opening_import,
                           moduleID.first, err.message());
      }

      return nullptr;
    }

    addDependency(moduleInputBuffer->getBufferIdentifier());
  }

  assert(moduleInputBuffer);

  auto M = Module::create(moduleID.first, Ctx);
  Ctx.LoadedModules[moduleID.first] = M;

  (void)loadAST(*M, moduleID.second, std::move(moduleInputBuffer),
                std::move(moduleDocInputBuffer), isFramework);
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
    SmallVectorImpl<Module::ImportedModule> &imports,
    Module::ImportFilter filter) const {
  File.getImportedModules(imports, filter);
}

void SerializedASTFile::collectLinkLibraries(
    Module::LinkLibraryCallback callback) const {
  if (isSIB()) {
    llvm::SmallVector<Module::ImportedModule, 8> Imports;
    File.getImportedModules(Imports, Module::ImportFilter::All);

    for (auto Import : Imports)
      Import.second->collectLinkLibraries(callback);
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

void SerializedASTFile::lookupValue(Module::AccessPathTy accessPath,
                                    DeclName name, NLKind lookupKind,
                                    SmallVectorImpl<ValueDecl*> &results) const{
  if (!Module::matchesAccessPath(accessPath, name))
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

void SerializedASTFile::lookupVisibleDecls(Module::AccessPathTy accessPath,
                                           VisibleDeclConsumer &consumer,
                                           NLKind lookupKind) const {
  File.lookupVisibleDecls(accessPath, consumer, lookupKind);
}

void SerializedASTFile::lookupClassMembers(Module::AccessPathTy accessPath,
                                           VisibleDeclConsumer &consumer) const{
  File.lookupClassMembers(accessPath, consumer);
}

void
SerializedASTFile::lookupClassMember(Module::AccessPathTy accessPath,
                                     DeclName name,
                                     SmallVectorImpl<ValueDecl*> &decls) const {
  File.lookupClassMember(accessPath, name, decls);
}

Optional<BriefAndRawComment>
SerializedASTFile::getCommentForDecl(const Decl *D) const {
  return File.getCommentForDecl(D);
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
  if (auto *ShadowedModule = getFile().getShadowedModule())
    return ShadowedModule->findUnderlyingClangModule();
  return nullptr;
}

Identifier
SerializedASTFile::getDiscriminatorForPrivateValue(const ValueDecl *D) const {
  Identifier discriminator = File.getDiscriminatorForPrivateValue(D);
  assert(!discriminator.empty() && "no discriminator found for value");
  return discriminator;
}
