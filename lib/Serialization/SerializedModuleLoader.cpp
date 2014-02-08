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
#include "swift/AST/DiagnosticsSema.h"
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

/// An adapter class that allows a std::unique_ptr to be used as an
/// llvm::OwningPtr.
template <typename T>
class OwningPtrAdapter {
  llvm::OwningPtr<T> Owner;
  std::unique_ptr<T> &Result;
public:
  OwningPtrAdapter(std::unique_ptr<T> &result) : Result(result) {
    Owner.reset(Result.release());
  }

  OwningPtrAdapter(const OwningPtrAdapter &other) = delete;
  OwningPtrAdapter &operator=(const OwningPtrAdapter &other) = delete;
  OwningPtrAdapter(OwningPtrAdapter &&other) = default;
  OwningPtrAdapter &operator=(OwningPtrAdapter &&other) = default;

  ~OwningPtrAdapter() {
    Result.reset(Owner.take());
  }
  operator llvm::OwningPtr<T> &() { return Owner; }
};

template <typename T>
OwningPtrAdapter<T> makeOwningPtrAdapter(std::unique_ptr<T> &result) {
  return result;
}

}

// Defined out-of-line so that we can see ~ModuleFile.
SerializedModuleLoader::SerializedModuleLoader(ASTContext &ctx) : Ctx(ctx) {}
SerializedModuleLoader::~SerializedModuleLoader() = default;

static llvm::error_code findModule(ASTContext &ctx, AccessPathElem moduleID,
                                   std::unique_ptr<llvm::MemoryBuffer> &buffer){
  llvm::SmallString<64> moduleFilename(moduleID.first.str());
  moduleFilename += '.';
  moduleFilename += SERIALIZED_MODULE_EXTENSION;

  llvm::SmallString<128> inputFilename;

  for (auto Path : ctx.SearchPathOpts.ImportSearchPaths) {
    inputFilename = Path;
    llvm::sys::path::append(inputFilename, moduleFilename.str());
    auto err = llvm::MemoryBuffer::getFile(inputFilename.str(),
                                           makeOwningPtrAdapter(buffer));
    if (!err || err.value() != llvm::errc::no_such_file_or_directory)
      return err;
  }

  // Search the runtime import path.
  inputFilename = ctx.SearchPathOpts.RuntimeLibraryImportPath;
  llvm::sys::path::append(inputFilename, moduleFilename.str());
  return llvm::MemoryBuffer::getFile(inputFilename.str(),
                                     makeOwningPtrAdapter(buffer));
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
  case ModuleStatus::MissingShadowedModule:
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
  assert(loadedModuleFile->getStatus() == ModuleStatus::MissingDependency ||
         loadedModuleFile->getStatus() == ModuleStatus::MissingShadowedModule);

  if (!diagLoc)
    return nullptr;

  if (loadedModuleFile->getStatus() == ModuleStatus::MissingShadowedModule) {
    Ctx.Diags.diagnose(*diagLoc, diag::serialization_missing_shadowed_module,
                       M.Name);
    if (Ctx.SearchPathOpts.SDKPath.empty()) {
      Ctx.Diags.diagnose(SourceLoc(), diag::sema_no_import_no_sdk);
      Ctx.Diags.diagnose(SourceLoc(), diag::sema_no_import_no_sdk_xcrun);
    }
    return nullptr;
  }

  // Figure out /which/ dependencies are missing.
  // FIXME: Dependencies should be de-duplicated at serialization time, not now.
  llvm::StringMap<bool> duplicates;
  llvm::SmallVector<ModuleFile::Dependency, 4> missing;
  std::copy_if(loadedModuleFile->getDependencies().begin(),
               loadedModuleFile->getDependencies().end(),
               std::back_inserter(missing),
               [&duplicates](const ModuleFile::Dependency &dependency) -> bool {
    if (dependency.isLoaded())
      return false;
    bool &seen = duplicates[dependency.RawAccessPath];
    if (seen)
      return false;
    seen = true;
    return true;
  });

  // FIXME: only show module part of RawAccessPath
  assert(!missing.empty() && "unknown missing dependency?");
  if (missing.size() == 1) {
    Ctx.Diags.diagnose(*diagLoc, diag::serialization_missing_single_dependency,
                       missing.begin()->RawAccessPath);
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

  if (Ctx.SearchPathOpts.SDKPath.empty()) {
    Ctx.Diags.diagnose(SourceLoc(), diag::sema_no_import_no_sdk);
    Ctx.Diags.diagnose(SourceLoc(), diag::sema_no_import_no_sdk_xcrun);
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

  auto M = Module::create(moduleID.first, Ctx);
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

bool SerializedModuleLoader::isSerializedAST(StringRef data) {
  using serialization::SIGNATURE;
  StringRef signatureStr(reinterpret_cast<const char *>(SIGNATURE),
                         llvm::array_lengthof(SIGNATURE));
  return data.startswith(signatureStr);
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
