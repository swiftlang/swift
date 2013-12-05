//===--- SerializedModuleLoader.cpp - Import Swift modules ------*- c++ -*-===//
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
#include "ModuleFile.h"
#include "swift/Subsystems.h"
#include "swift/AST/AST.h"
#include "swift/AST/Diagnostics.h"
#include "swift/Basic/STLExtras.h"
#include "swift/Basic/SourceManager.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/system_error.h"

using namespace swift;

namespace {
typedef std::pair<Identifier, SourceLoc> AccessPathElem;
}

// Defined out-of-line so that we can see ~ModuleFile.
SerializedModuleLoader::SerializedModuleLoader(ASTContext &ctx) : Ctx(ctx) {}
SerializedModuleLoader::~SerializedModuleLoader() = default;

StringRef SerializedModuleLoader::FailedImportModuleLoader::getModuleFilename(
    const Module *Module) {
  return cast<FailedImportModule>(Module)->ModuleFilename;
}

// FIXME: Copied from SourceLoader. Not bothering to fix until we decide that
// the source loader search path should be the same as the module loader search
// path.
static llvm::error_code findModule(ASTContext &ctx, AccessPathElem moduleID,
           llvm::OwningPtr<llvm::MemoryBuffer> &buffer){
  llvm::SmallString<64> moduleFilename(moduleID.first.str());
  moduleFilename += '.';
  moduleFilename += SERIALIZED_MODULE_EXTENSION;

  llvm::SmallString<128> inputFilename;

  // First, search in the directory corresponding to the import location.
  // FIXME: This screams for a proper FileManager abstraction.
  if (moduleID.second.isValid()) {
    unsigned currentBufferID =
        ctx.SourceMgr.findBufferContainingLoc(moduleID.second);
    const llvm::MemoryBuffer *importingBuffer
      = ctx.SourceMgr->getMemoryBuffer(currentBufferID);
    StringRef currentDirectory
      = llvm::sys::path::parent_path(importingBuffer->getBufferIdentifier());
    if (!currentDirectory.empty()) {
      inputFilename = currentDirectory;
      llvm::sys::path::append(inputFilename, moduleFilename.str());
      llvm::error_code err = llvm::MemoryBuffer::getFile(inputFilename.str(),
                                                         buffer);
      if (!err)
        return err;
    }
  }

  // Second, search in the current directory.
  llvm::error_code err = llvm::MemoryBuffer::getFile(moduleFilename.str(),
                                                     buffer);
  if (!err)
    return err;

  // If we fail, search each import search path.
  for (auto Path : ctx.ImportSearchPaths) {
    inputFilename = Path;
    llvm::sys::path::append(inputFilename, moduleFilename.str());
    err = llvm::MemoryBuffer::getFile(inputFilename.str(), buffer);
    if (!err)
      return err;
  }

  return err;
}

Module *SerializedModuleLoader::loadModule(SourceLoc importLoc,
                                           Module::AccessPathTy path) {
  // FIXME: Swift submodules?
  if (path.size() > 1)
    return nullptr;

  auto moduleID = path[0];

  llvm::OwningPtr<llvm::MemoryBuffer> inputFile;
  // First see if we find it in the registered memory buffers.
  if (!MemoryBuffers.empty()) {
    // FIXME: Right now this works only with access paths of length 1.
    // Once submodules are designed, this needs to support suffix
    // matching and a search path.
    llvm::SmallString<256> spath;
    for (auto el : path)
      llvm::sys::path::append(spath, el.first.str());

    auto bs = MemoryBuffers.find(spath.str());
    if (bs != MemoryBuffers.end())
      inputFile.reset(bs->second.release());
  }

  // Otherwise look on disk.
  if (!inputFile)
    if (llvm::error_code err = findModule(Ctx, moduleID, inputFile)) {
      if (err.value() != llvm::errc::no_such_file_or_directory) {
        Ctx.Diags.diagnose(moduleID.second, diag::sema_opening_import,
                           moduleID.first.str(), err.message());
      }

      return nullptr;
    }

  assert(inputFile);

  std::unique_ptr<ModuleFile> loadedModuleFile;
  ModuleStatus err = ModuleFile::load(std::move(inputFile), loadedModuleFile);
  switch (err) {
  case ModuleStatus::Valid:
    Ctx.bumpGeneration();
    break;
  case ModuleStatus::FormatTooNew:
    Ctx.Diags.diagnose(moduleID.second, diag::serialization_module_too_new);
    break;
  case ModuleStatus::Malformed:
    Ctx.Diags.diagnose(moduleID.second, diag::serialization_malformed_module);
    break;
  case ModuleStatus::MissingDependency:
    llvm_unreachable("dependencies haven't been loaded yet");
  }

  // Whether we succeed or fail, don't try to load this module again.
  Module *&moduleRef = Ctx.LoadedModules[moduleID.first.str()];

  if (err != ModuleStatus::Valid) {
    StringRef name = loadedModuleFile->getModuleFilename();
    moduleRef = new (Ctx) FailedImportModule(moduleID.first, err, name,
                                             Ctx, FailedImportLoader);
    return moduleRef;
  }

  auto TU = new (Ctx) TranslationUnit(moduleID.first, Ctx);
  auto fileUnit = new (Ctx) SerializedASTFile(*TU, *loadedModuleFile);
  TU->addFile(*fileUnit);
  moduleRef = TU;

  // moduleRef is no longer valid as soon as we start loading dependencies.

  if (loadedModuleFile->associateWithFileContext(fileUnit)) {
    LoadedModuleFiles.emplace_back(std::move(loadedModuleFile),
                                   Ctx.getCurrentGeneration());
    return TU;
  }

  // We failed to bring the module file into the AST.
  assert(loadedModuleFile->getStatus() == ModuleStatus::MissingDependency);

  SmallVector<ModuleFile::Dependency, 4> missing;
  std::copy_if(loadedModuleFile->getDependencies().begin(),
               loadedModuleFile->getDependencies().end(),
               std::back_inserter(missing),
               [](const ModuleFile::Dependency &dependency) {
    return !dependency.isLoaded();
  });

  // FIXME: only show module part of RawAccessPath
  assert(!missing.empty() && "unknown missing dependency?");
  if (missing.size() == 1) {
    Ctx.Diags.diagnose(moduleID.second,
                       diag::serialization_missing_single_dependency,
                       missing.front().RawAccessPath);
  } else {
    llvm::SmallString<64> missingNames;
    missingNames += '\'';
    interleave(missing,
               [&](const ModuleFile::Dependency &next) {
                 missingNames += next.RawAccessPath;
               },
               [&] { missingNames += "', '"; });
    missingNames += '\'';

    Ctx.Diags.diagnose(moduleID.second,
                       diag::serialization_missing_dependencies,
                       missingNames);
  }

  // Don't try to load this module again.
  Module *failedModule =
    new (Ctx) FailedImportModule(moduleID.first,
                                 loadedModuleFile->getStatus(),
                                 loadedModuleFile->getModuleFilename(),
                                 Ctx, FailedImportLoader);
  Ctx.LoadedModules[moduleID.first.str()] = failedModule;
  return failedModule;
}

void SerializedModuleLoader::loadExtensions(NominalTypeDecl *nominal,
                                            unsigned previousGeneration) {
  for (auto &modulePair : LoadedModuleFiles) {
    if (modulePair.second <= previousGeneration)
      continue;
    modulePair.first->loadExtensions(nominal);
  }
}

void
SerializedModuleLoader::loadDeclsConformingTo(KnownProtocolKind kind,
                                              unsigned previousGeneration) {
  for (auto &modulePair : LoadedModuleFiles) {
    if (modulePair.second <= previousGeneration)
      continue;
    modulePair.first->loadDeclsConformingTo(kind);
  }
}

//-----------------------------------------------------------------------------
// SerializedASTFile implementation
//-----------------------------------------------------------------------------

void SerializedASTFile::getImportedModules(
    SmallVectorImpl<Module::ImportedModule> &imports,
    bool includePrivate) const {
  File.getImportedModules(imports, includePrivate);
}

void SerializedASTFile::collectLinkLibraries(
    Module::LinkLibraryCallback callback) const {
  File.collectLinkLibraries(callback);
}

void SerializedASTFile::lookupValue(Module::AccessPathTy accessPath,
                                    Identifier name, NLKind lookupKind,
                                    SmallVectorImpl<ValueDecl*> &results) const{
  assert(accessPath.size() <= 1 && "can only refer to top-level decls");

  // If this import is specific to some named type or decl ("import swift.int")
  // then filter out any lookups that don't match.
  if (accessPath.size() == 1 && accessPath.front().first != name)
    return;

  File.lookupValue(name, results);
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
                                     Identifier name,
                                     SmallVectorImpl<ValueDecl*> &decls) const {
  File.lookupClassMember(accessPath, name, decls);
}


void SerializedASTFile::getTopLevelDecls(SmallVectorImpl<Decl*> &results) const{
  File.getTopLevelDecls(results);
}

StringRef SerializedASTFile::getFilename() const {
  return File.getModuleFilename();
}
