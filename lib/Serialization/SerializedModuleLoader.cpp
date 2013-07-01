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
#include "swift/AST/Component.h"
#include "swift/AST/Diagnostics.h"
#include "swift/Basic/Interleave.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/system_error.h"

using namespace swift;

namespace {
class SerializedModule : public LoadedModule {
public:
  ModuleFile *File;

  SerializedModule(ASTContext &ctx, SerializedModuleLoader &owner,
                   Identifier name, Component *comp, ModuleFile *file)
    : LoadedModule(DeclContextKind::SerializedModule, name, comp, ctx, owner),
      File(file) {}

  static bool classof(const DeclContext *DC) {
    return DC->getContextKind() == DeclContextKind::SerializedModule;
  }
};

typedef std::pair<Identifier, SourceLoc> AccessPathElem;
}

// Defined out-of-line so that we can see ~ModuleFile.
SerializedModuleLoader::SerializedModuleLoader(ASTContext &ctx) : Ctx(ctx) {}
SerializedModuleLoader::~SerializedModuleLoader() = default;

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
  llvm::SMLoc rawLoc = moduleID.second.Value;
  int currentBufferID = ctx.SourceMgr.FindBufferContainingLoc(rawLoc);
  if (currentBufferID >= 0) {
    const llvm::MemoryBuffer *importingBuffer
      = ctx.SourceMgr.getBufferInfo(currentBufferID).Buffer;
    StringRef currentDirectory
      = llvm::sys::path::parent_path(importingBuffer->getBufferIdentifier());
    if (!currentDirectory.empty()) {
      inputFilename = currentDirectory;
      llvm::sys::path::append(inputFilename, moduleFilename.str());
      llvm::error_code err = llvm::MemoryBuffer::getFile(inputFilename, buffer);
      if (!err)
        return err;
    }
  }

  // Second, search in the current directory.
  llvm::error_code err = llvm::MemoryBuffer::getFile(moduleFilename, buffer);
  if (!err)
    return err;

  // If we fail, search each import search path.
  for (auto Path : ctx.ImportSearchPaths) {
    inputFilename = Path;
    llvm::sys::path::append(inputFilename, moduleFilename.str());
    err = llvm::MemoryBuffer::getFile(inputFilename, buffer);
    if (!err)
      return err;
  }

  return err;
}

static Module *makeTU(ASTContext &ctx, AccessPathElem moduleID,
                      ArrayRef<StringRef> inputPaths) {
  // FIXME: The kind of the TU should be read from the serialized file.
  Component *comp = new (ctx.Allocate<Component>(1)) Component();
  TranslationUnit *TU = new (ctx) TranslationUnit(moduleID.first, comp, ctx,
                                                  TranslationUnit::Library);

  TU->HasBuiltinModuleAccess = (moduleID.first.str() == "swift");
  performAutoImport(TU);

  ctx.LoadedModules[moduleID.first.str()] = TU;

  std::vector<unsigned> BufferIDs;
  for (auto &path : inputPaths) {
    // Open the input file.
    llvm::OwningPtr<llvm::MemoryBuffer> InputFile;
    if (llvm::MemoryBuffer::getFileOrSTDIN(path, InputFile))
      return nullptr;

    // Transfer ownership of the MemoryBuffer to the SourceMgr.
    // FIXME: include location
    llvm::SMLoc rawLoc = moduleID.second.Value;
    BufferIDs.push_back(ctx.SourceMgr.AddNewSourceBuffer(InputFile.take(),
                                                         rawLoc));
  }

  for (auto &BufferID : BufferIDs) {
    unsigned BufferOffset = 0;
    const llvm::MemoryBuffer *Buffer =
      ctx.SourceMgr.getMemoryBuffer(BufferID);
    do {
      parseIntoTranslationUnit(TU, BufferID, &BufferOffset, 0, /*SIL=*/nullptr);
    } while (BufferOffset != Buffer->getBufferSize());
  }

  performTypeChecking(TU);

  return TU;
}


Module *SerializedModuleLoader::loadModule(SourceLoc importLoc,
                                           Module::AccessPathTy path,
                                           bool isStdlibImport) {

  // FIXME: Swift submodules?
  if (path.size() > 1)
    return nullptr;

  auto moduleID = path[0];

  llvm::OwningPtr<llvm::MemoryBuffer> inputFile;
  if (llvm::error_code err = findModule(Ctx, moduleID, inputFile)) {
    if (err.value() != llvm::errc::no_such_file_or_directory) {
      Ctx.Diags.diagnose(moduleID.second, diag::sema_opening_import,
                         moduleID.first.str(), err.message());
    }

    return nullptr;
  }
  assert(inputFile);
  
  llvm::OwningPtr<ModuleFile> loadedModuleFile;
  ModuleStatus err = ModuleFile::load(std::move(inputFile), loadedModuleFile);
  switch (err) {
  case ModuleStatus::FallBackToTranslationUnit:
    return makeTU(Ctx, moduleID, loadedModuleFile->getInputSourcePaths());
  case ModuleStatus::Valid:
    Ctx.bumpGeneration();
    break;
  case ModuleStatus::FormatTooNew:
    Ctx.Diags.diagnose(moduleID.second, diag::serialization_module_too_new);
    loadedModuleFile.reset();
    break;
  case ModuleStatus::Malformed:
    Ctx.Diags.diagnose(moduleID.second, diag::serialization_malformed_module);
    loadedModuleFile.reset();
    break;
  case ModuleStatus::MissingDependency:
    llvm_unreachable("dependencies haven't been loaded yet");
  }

  auto comp = new (Ctx.Allocate<Component>(1)) Component();
  auto module = new (Ctx) SerializedModule(Ctx, *this, moduleID.first, comp,
                                           loadedModuleFile.get());

  if (loadedModuleFile) {
    bool success = loadedModuleFile->associateWithModule(module);
    if (success) {
      LoadedModuleFiles.push_back(std::move(loadedModuleFile));
    } else {
      assert(loadedModuleFile->getStatus() == ModuleStatus::MissingDependency);

      SmallVector<ModuleFile::Dependency, 4> missing;
      std::copy_if(loadedModuleFile->getDependencies().begin(),
                   loadedModuleFile->getDependencies().end(),
                   std::back_inserter(missing),
                   [](const ModuleFile::Dependency &dependency) {
        return dependency.Mod == nullptr;
      });

      assert(!missing.empty() && "unknown missing dependency?");
      if (missing.size() == 1) {
        Ctx.Diags.diagnose(moduleID.second,
                           diag::serialization_missing_single_dependency,
                           missing.front().Name);
      } else {
        llvm::SmallString<64> missingNames;
        missingNames += '\'';
        interleave(missing,
                   [&](const ModuleFile::Dependency &next) {
                     missingNames += next.Name;
                   },
                   [&] { missingNames += "', '"; });
        missingNames += '\'';

        Ctx.Diags.diagnose(moduleID.second,
                           diag::serialization_missing_dependencies,
                           missingNames);
      }
    }
  }

  // Whether we succeeded or failed, don't try to load this module again.
  Ctx.LoadedModules[moduleID.first.str()] = module;
  return module;
}

void SerializedModuleLoader::lookupValue(Module *module,
                                         Module::AccessPathTy accessPath,
                                         Identifier name, NLKind lookupKind,
                                         SmallVectorImpl<ValueDecl*> &results) {
  // FIXME: handle nested modules.
  if (!accessPath.empty())
    return;

  ModuleFile *moduleFile = cast<SerializedModule>(module)->File;
  if (!moduleFile)
    return;

  moduleFile->lookupValue(name, results);
}


void SerializedModuleLoader::loadExtensions(NominalTypeDecl *nominal,
                                            unsigned previousGeneration) {
  // FIXME: Look for extensions of the given nominal type within the
  // serialized module.
}
