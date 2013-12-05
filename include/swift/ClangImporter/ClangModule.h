//===--- ClangModule.h - An imported Clang module ---------------*- C++ -*-===//
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
//
// This file implements support for loading Clang modules into Swift.
//
//===----------------------------------------------------------------------===//
#ifndef SWIFT_CLANGIMPORTER_CLANGMODULE_H
#define SWIFT_CLANGIMPORTER_CLANGMODULE_H

#include "swift/AST/Module.h"

namespace clang {
  class Module;
}

namespace swift {

class ASTContext;
class ClangImporter;
class ModuleLoader;

/// \brief Represents a Clang module that has been imported into Swift.
class ClangModuleUnit final : public LoadedFile {
  ClangImporter &owner;
  clang::Module *clangModule;
  llvm::PointerIntPair<Module *, 1, bool> adapterModule;

  Module *getAdapterModule() const;

public:
  ClangModuleUnit(Module &M, ClangImporter &owner,
                  clang::Module *clangModule);

  /// \brief Retrieve the underlying Clang module.
  clang::Module *getClangModule() const { return clangModule; }

  /// Returns true if this is a top-level Clang module (not a submodule).
  bool isTopLevel() const;

  virtual void lookupValue(Module::AccessPathTy accessPath,
                           Identifier name, NLKind lookupKind,
                           SmallVectorImpl<ValueDecl*> &results) const override;

  virtual void lookupVisibleDecls(Module::AccessPathTy accessPath,
                                  VisibleDeclConsumer &consumer,
                                  NLKind lookupKind) const override;

  virtual void lookupClassMembers(Module::AccessPathTy accessPath,
                                  VisibleDeclConsumer &consumer) const override;

  virtual void
  lookupClassMember(Module::AccessPathTy accessPath, Identifier name,
                    SmallVectorImpl<ValueDecl*> &decls) const override;

  virtual void getTopLevelDecls(SmallVectorImpl<Decl*> &results) const override;

  virtual void
  getImportedModules(SmallVectorImpl<Module::ImportedModule> &imports,
                     bool includePrivate) const override;

  virtual void
  collectLinkLibraries(Module::LinkLibraryCallback callback) const override;

  virtual StringRef getFilename() const override;

  static bool classof(const FileUnit *file) {
    return file->getKind() == FileUnitKind::ClangModule;
  }
  static bool classof(const DeclContext *DC) {
    return isa<FileUnit>(DC) && classof(cast<FileUnit>(DC));
  }
};

}

#endif
