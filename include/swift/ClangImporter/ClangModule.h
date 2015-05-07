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
  class ASTContext;
  class Module;
}

namespace swift {

class ASTContext;
class ClangImporter;
class ModuleLoader;

/// \brief Represents a Clang module that has been imported into Swift.
class ClangModuleUnit final : public LoadedFile {
  ClangImporter &owner;
  const clang::Module *clangModule;
  llvm::PointerIntPair<ModuleDecl *, 1, bool> adapterModule;

  ~ClangModuleUnit() = default;

public:
  /// True if the given Module contains an imported Clang module unit.
  static bool hasClangModule(ModuleDecl *M);

  ClangModuleUnit(ModuleDecl &M, ClangImporter &owner,
                  const clang::Module *clangModule);

  /// \brief Retrieve the underlying Clang module.
  ///
  /// This will be null if the module unit represents the imported headers.
  const clang::Module *getClangModule() const { return clangModule; }

  /// Returns true if this is a top-level Clang module (not a submodule).
  bool isTopLevel() const;

  /// Returns the Swift module that overlays this Clang module.
  ModuleDecl *getAdapterModule() const;

  virtual bool isSystemModule() const override;

  virtual void lookupValue(ModuleDecl::AccessPathTy accessPath,
                           DeclName name, NLKind lookupKind,
                           SmallVectorImpl<ValueDecl*> &results) const override;

  virtual void lookupVisibleDecls(ModuleDecl::AccessPathTy accessPath,
                                  VisibleDeclConsumer &consumer,
                                  NLKind lookupKind) const override;

  virtual void lookupClassMembers(ModuleDecl::AccessPathTy accessPath,
                                  VisibleDeclConsumer &consumer) const override;

  virtual void
  lookupClassMember(ModuleDecl::AccessPathTy accessPath, DeclName name,
                    SmallVectorImpl<ValueDecl*> &decls) const override;

  virtual void getTopLevelDecls(SmallVectorImpl<Decl*> &results) const override;

  virtual void getDisplayDecls(SmallVectorImpl<Decl*> &results) const override;

  virtual void
  getImportedModules(SmallVectorImpl<ModuleDecl::ImportedModule> &imports,
                     ModuleDecl::ImportFilter filter) const override;

  virtual void
  collectLinkLibraries(ModuleDecl::LinkLibraryCallback callback) const override;

  Identifier
  getDiscriminatorForPrivateValue(const ValueDecl *D) const override {
    llvm_unreachable("no private decls in Clang modules");
  }

  virtual StringRef getFilename() const override;

  virtual const clang::Module *getUnderlyingClangModule() override {
    return getClangModule();
  }

  clang::ASTContext &getClangASTContext() const;

  static bool classof(const FileUnit *file) {
    return file->getKind() == FileUnitKind::ClangModule;
  }
  static bool classof(const DeclContext *DC) {
    return isa<FileUnit>(DC) && classof(cast<FileUnit>(DC));
  }
};

}

#endif
