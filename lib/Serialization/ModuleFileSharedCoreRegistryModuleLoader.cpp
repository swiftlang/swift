//===--- ModuleFileSharedCoreRegistryModuleLoader.cpp -----------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/Serialization/ModuleFileSharedCoreRegistryModuleLoader.h"

#include "ModuleFile.h"
#include "ModuleFileSharedCore.h"
#include "Serialization.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/ModuleLoader.h"

using namespace swift;

void ModuleFileSharedCoreRegistry::clear() { Storage.clear(); }

void ModuleFileSharedCoreRegistry::registerModule(ModuleDecl *M) {
  if (M->failedToLoad() || M->getFiles().empty())
    return;

  std::shared_ptr<const ModuleFileSharedCore> moduleCore;

  if (M->isNonSwiftModule()) {
    // TODO: Support clang modules.
    return;
  } else if (auto ASTFile =
                 dyn_cast<SerializedASTFile>(M->getFiles().front())) {
    moduleCore = ASTFile->File.getCore();
  } else {
    return;
  }

  //  llvm::errs() << "RegisterModule: " << M->getName() << "\n";
  Storage.insert({M->getName().str(), {moduleCore, M->isSystemModule()}});
}

ModuleFileSharedCoreRegistry::Value
ModuleFileSharedCoreRegistry::lookup(StringRef name) const {
  return Storage.lookup(name);
}

std::error_code
ModuleFileSharedCoreRegistryModuleLoader::findModuleFilesInDirectory(
    AccessPathElem ModuleID, const SerializedModuleBaseName &BaseName,
    SmallVectorImpl<char> *ModuleInterfacePath,
    std::unique_ptr<llvm::MemoryBuffer> *ModuleBuffer,
    std::unique_ptr<llvm::MemoryBuffer> *ModuleDocBuffer,
    std::unique_ptr<llvm::MemoryBuffer> *ModuleSourceInfoBuffer,
    bool IsFramework) {
  // This is a soft error instead of an llvm_unreachable because this API is
  // primarily used by LLDB which makes it more likely that unwitting changes to
  // the Swift compiler accidentally break the contract.
  assert(false && "not supported");
  return std::make_error_code(std::errc::not_supported);
}

bool ModuleFileSharedCoreRegistryModuleLoader::canImportModule(
    Located<Identifier> named) {
  assert(Registry);
  return bool(Registry->lookup(named.Item.str()).ModuleFileCore);
}

ModuleDecl *ModuleFileSharedCoreRegistryModuleLoader::loadModule(
    SourceLoc importLoc, ArrayRef<Located<Identifier>> path) {
  assert(Registry);

  // TODO: How to support submodules?
  if (path.size() > 1)
    return nullptr;

  const auto &moduleID = path[0];

  auto cached = Registry->lookup(moduleID.Item.str());
  if (!cached.ModuleFileCore)
    return nullptr;

  if (dependencyTracker)
    dependencyTracker->addDependency(cached.ModuleFileCore->getModuleFilename(),
                                     cached.IsSystemModule);

  auto *M = ModuleDecl::create(moduleID.Item, Ctx);
  M->setIsSystemModule(cached.IsSystemModule);
  Ctx.addLoadedModule(M);
  SWIFT_DEFER { M->setHasResolvedImports(); };

  std::unique_ptr<ModuleFile> loadedModuleFile =
      std::make_unique<ModuleFile>(cached.ModuleFileCore);
  FileUnit *file = nullptr;
  auto status = loadAST(*M, importLoc, loadedModuleFile, file);
  if (status == serialization::Status::Valid) {
    M->addFile(*file);
  } else {
    M->setFailedToLoad();
  }

  return M;
}
