//===--- SerializedModuleLoader.h - Import Swift modules --------*- c++ -*-===//
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

#ifndef SWIFT_SERIALIZATION_MODULELOADER_H
#define SWIFT_SERIALIZATION_MODULELOADER_H

#include "swift/AST/Module.h"
#include "swift/AST/ModuleLoader.h"
#include "llvm/Support/MemoryBuffer.h"

namespace swift {
class ModuleFile;

/// \brief Imports serialized Swift modules into an ASTContext.
class SerializedModuleLoader : public ModuleLoader {
private:
  ASTContext &Ctx;
  llvm::StringMap<std::unique_ptr<llvm::MemoryBuffer>> MemoryBuffers;
  /// A { module, generation # } pair.
  using LoadedModulePair = std::pair<std::unique_ptr<ModuleFile>, unsigned>;
  std::vector<LoadedModulePair> LoadedModuleFiles;

  explicit SerializedModuleLoader(ASTContext &ctx, DependencyTracker *tracker);

public:
  /// \brief Create a new importer that can load serialized Swift modules
  /// into the given ASTContext.
  static std::unique_ptr<SerializedModuleLoader>
  create(ASTContext &ctx, DependencyTracker *tracker = nullptr) {
    return std::unique_ptr<SerializedModuleLoader>{
      new SerializedModuleLoader(ctx, tracker)
    };
  }

  ~SerializedModuleLoader();

  SerializedModuleLoader(const SerializedModuleLoader &) = delete;
  SerializedModuleLoader(SerializedModuleLoader &&) = delete;
  SerializedModuleLoader &operator=(const SerializedModuleLoader &) = delete;
  SerializedModuleLoader &operator=(SerializedModuleLoader &&) = delete;

  /// \brief Import a module with the given module path.
  ///
  /// \param importLoc The location of the 'import' keyword.
  ///
  /// \param path A sequence of (identifier, location) pairs that denote
  /// the dotted module name to load, e.g., AppKit.NSWindow.
  ///
  /// \returns the module referenced, if it could be loaded. Otherwise,
  /// emits a diagnostic and returns a FailedImportModule object.
  virtual Module *
  loadModule(SourceLoc importLoc,
             ArrayRef<std::pair<Identifier, SourceLoc>> path) override;

  /// Attempt to load a serialized AST into the given module.
  ///
  /// If the AST cannot be loaded and \p diagLoc is present, a diagnostic is
  /// printed. (Note that \p diagLoc is allowed to be invalid.)
  FileUnit *loadAST(Module &M, Optional<SourceLoc> diagLoc,
                    std::unique_ptr<llvm::MemoryBuffer> moduleInputBuffer,
                    std::unique_ptr<llvm::MemoryBuffer> moduleDocInputBuffer,
                    bool isFramework = false);

  /// \brief Register a memory buffer that contains the serialized
  /// module for the given access path. This API is intended to be
  /// used by LLDB to add swiftmodules discovered in the __apple_ast
  /// section of a Mach-O file to the search path.
  /// FIXME: make this an actual access *path* once submodules are designed.
  void registerMemoryBuffer(StringRef AccessPath,
                            std::unique_ptr<llvm::MemoryBuffer> input) {
    MemoryBuffers[AccessPath] = std::move(input);
  }

  virtual void loadExtensions(NominalTypeDecl *nominal,
                              unsigned previousGeneration) override;

  virtual void loadObjCMethods(
                 ClassDecl *classDecl,
                 ObjCSelector selector,
                 bool isInstanceMethod,
                 unsigned previousGeneration,
                 llvm::TinyPtrVector<AbstractFunctionDecl *> &methods) override;

  virtual void verifyAllModules() override;
};

/// A file-unit loaded from a serialized AST file.
class SerializedASTFile final : public LoadedFile {
  friend class SerializedModuleLoader;
  friend class SerializedSILLoader;

  ModuleFile &File;
  bool IsSIB;

  ~SerializedASTFile() = default;

  SerializedASTFile(Module &M, ModuleFile &file, bool isSIB = false)
    : LoadedFile(FileUnitKind::SerializedAST, M), File(file), IsSIB(isSIB) {}

public:
  bool isSIB() const { return IsSIB; }

  virtual bool isSystemModule() const override;

  virtual void lookupValue(Module::AccessPathTy accessPath,
                           DeclName name, NLKind lookupKind,
                           SmallVectorImpl<ValueDecl*> &results) const override;

  virtual TypeDecl *lookupLocalType(StringRef MangledName) const override;

  virtual OperatorDecl *lookupOperator(Identifier name,
                                       DeclKind fixity) const override;

  virtual void lookupVisibleDecls(Module::AccessPathTy accessPath,
                                  VisibleDeclConsumer &consumer,
                                  NLKind lookupKind) const override;

  virtual void lookupClassMembers(Module::AccessPathTy accessPath,
                                  VisibleDeclConsumer &consumer) const override;

  virtual void
  lookupClassMember(Module::AccessPathTy accessPath, DeclName name,
                    SmallVectorImpl<ValueDecl*> &decls) const override;

  Optional<BriefAndRawComment> getCommentForDecl(const Decl *D) const override;

  virtual void getTopLevelDecls(SmallVectorImpl<Decl*> &results) const override;

  virtual void
  getLocalTypeDecls(SmallVectorImpl<TypeDecl*> &results) const override;

  virtual void getDisplayDecls(SmallVectorImpl<Decl*> &results) const override;

  virtual void
  getImportedModules(SmallVectorImpl<Module::ImportedModule> &imports,
                     Module::ImportFilter filter) const override;

  virtual void
  collectLinkLibraries(Module::LinkLibraryCallback callback) const override;

  Identifier getDiscriminatorForPrivateValue(const ValueDecl *D) const override;

  virtual StringRef getFilename() const override;

  ClassDecl *getMainClass() const override;

  bool hasEntryPoint() const override;

  virtual const clang::Module *getUnderlyingClangModule() override;

  static bool classof(const FileUnit *file) {
    return file->getKind() == FileUnitKind::SerializedAST;
  }
  static bool classof(const DeclContext *DC) {
    return isa<FileUnit>(DC) && classof(cast<FileUnit>(DC));
  }
};


} // end namespace swift

#endif
