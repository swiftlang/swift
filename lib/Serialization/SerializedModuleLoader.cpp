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

// FIXME: Copied from SourceLoader. Not bothering to fix until we decide that
// the source loader search path should be the same as the module loader search
// path.
static llvm::error_code findModule(ASTContext &ctx, AccessPathElem moduleID,
                                   std::unique_ptr<llvm::MemoryBuffer> &buffer){
  llvm::SmallString<64> moduleFilename(moduleID.first.str());
  moduleFilename += '.';
  moduleFilename += SERIALIZED_MODULE_EXTENSION;

  llvm::SmallString<128> inputFilename;
  llvm::OwningPtr<llvm::MemoryBuffer> bufferRef;

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
                                                         bufferRef);
      if (!err) {
        buffer.reset(bufferRef.take());
        return err;
      }
    }
  }

  // Second, search in the current directory.
  llvm::error_code err = llvm::MemoryBuffer::getFile(moduleFilename.str(),
                                                     bufferRef);
  if (!err) {
    buffer.reset(bufferRef.take());
    return err;
  }

  // If we fail, search each import search path.
  for (auto Path : ctx.SearchPathOpts.ImportSearchPaths) {
    inputFilename = Path;
    llvm::sys::path::append(inputFilename, moduleFilename.str());
    err = llvm::MemoryBuffer::getFile(inputFilename.str(), bufferRef);
    if (!err) {
      buffer.reset(bufferRef.take());
      return err;
    }
  }

  // Search the runtime import path.
  inputFilename = ctx.SearchPathOpts.RuntimeImportPath;
  llvm::sys::path::append(inputFilename, moduleFilename.str());
  err = llvm::MemoryBuffer::getFile(inputFilename.str(), bufferRef);
  if (!err) {
    buffer.reset(bufferRef.take());
    return err;
  }

  // If we get here, we couldn't find the module, so return our most recent err.
  return err;
}

FileUnit *
SerializedModuleLoader::loadAST(Module &M, Optional<SourceLoc> diagLoc,
                                std::unique_ptr<llvm::MemoryBuffer> input) {
  assert(input);

  std::unique_ptr<ModuleFile> loadedModuleFile;
  ModuleStatus err = ModuleFile::load(std::move(input), loadedModuleFile);
  switch (err) {
  case ModuleStatus::Valid:
    Ctx.bumpGeneration();
    break;
  case ModuleStatus::FormatTooNew:
    if (diagLoc)
      Ctx.Diags.diagnose(*diagLoc, diag::serialization_module_too_new);
    return nullptr;
  case ModuleStatus::Malformed:
    if (diagLoc)
      Ctx.Diags.diagnose(*diagLoc, diag::serialization_malformed_module);
    return nullptr;
  case ModuleStatus::MissingDependency:
    llvm_unreachable("dependencies haven't been loaded yet");
  }

  // Create the FileUnit wrapper.
  auto fileUnit = new (Ctx) SerializedASTFile(M, *loadedModuleFile);
  M.addFile(*fileUnit);

  if (loadedModuleFile->associateWithFileContext(fileUnit)) {
    LoadedModuleFiles.emplace_back(std::move(loadedModuleFile),
                                   Ctx.getCurrentGeneration());
    return fileUnit;
  }

  // We failed to bring the module file into the AST.
  M.removeFile(*fileUnit);
  assert(loadedModuleFile->getStatus() == ModuleStatus::MissingDependency);

  if (!diagLoc)
    return nullptr;

  // Figure out /which/ dependencies are missing.
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
    Ctx.Diags.diagnose(*diagLoc, diag::serialization_missing_single_dependency,
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

    Ctx.Diags.diagnose(*diagLoc, diag::serialization_missing_dependencies,
                       missingNames);
  }

  return nullptr;
}

Module *SerializedModuleLoader::loadModule(SourceLoc importLoc,
                                           Module::AccessPathTy path) {
  // FIXME: Swift submodules?
  if (path.size() > 1)
    return nullptr;

  auto moduleID = path[0];

  std::unique_ptr<llvm::MemoryBuffer> inputFile;
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
  if (!inputFile) {
    if (llvm::error_code err = findModule(Ctx, moduleID, inputFile)) {
      if (err.value() != llvm::errc::no_such_file_or_directory) {
        Ctx.Diags.diagnose(moduleID.second, diag::sema_opening_import,
                           moduleID.first.str(), err.message());
      }

      return nullptr;
    }
  }

  assert(inputFile);

  auto M = new (Ctx) Module(moduleID.first, Ctx);
  Ctx.LoadedModules[moduleID.first.str()] = M;

  (void)loadAST(*M, moduleID.second, std::move(inputFile));
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

void
SerializedModuleLoader::loadDeclsConformingTo(KnownProtocolKind kind,
                                              unsigned previousGeneration) {
  for (auto &modulePair : LoadedModuleFiles) {
    if (modulePair.second <= previousGeneration)
      continue;
    modulePair.first->loadDeclsConformingTo(kind);
  }
}

bool
SerializedModuleLoader::isValidSerializedAST(const llvm::MemoryBuffer &input) {
  using serialization::SIGNATURE;
  StringRef signatureStr(reinterpret_cast<const char *>(SIGNATURE),
                         llvm::array_lengthof(SIGNATURE));
  return input.getBuffer().startswith(signatureStr);
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

void SerializedASTFile::getDisplayDecls(SmallVectorImpl<Decl*> &results) const {
  File.getDisplayDecls(results);
}

StringRef SerializedASTFile::getFilename() const {
  return File.getModuleFilename();
}
