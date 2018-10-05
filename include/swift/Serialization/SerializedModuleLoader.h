//===--- SerializedModuleLoader.h - Import Swift modules --------*- C++ -*-===//
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

#ifndef SWIFT_SERIALIZATION_MODULELOADER_H
#define SWIFT_SERIALIZATION_MODULELOADER_H

#include "swift/AST/Module.h"
#include "swift/AST/ModuleLoader.h"
#include "llvm/Support/MemoryBuffer.h"

namespace swift {
class ModuleFile;

/// Common functionality shared between \c SerializedModuleLoader and
/// \c ParseableInterfaceModuleLoader.
class SerializedModuleLoaderBase : public ModuleLoader {
  llvm::StringMap<std::unique_ptr<llvm::MemoryBuffer>> MemoryBuffers;
  /// A { module, generation # } pair.
  using LoadedModulePair = std::pair<std::unique_ptr<ModuleFile>, unsigned>;
  std::vector<LoadedModulePair> LoadedModuleFiles;

  SmallVector<std::unique_ptr<llvm::MemoryBuffer>, 2> OrphanedMemoryBuffers;

protected:
  ASTContext &Ctx;
  explicit SerializedModuleLoaderBase(ASTContext &ctx, DependencyTracker *tracker);

  using AccessPathElem = std::pair<Identifier, SourceLoc>;
  virtual bool findModule(AccessPathElem moduleID,
                          std::unique_ptr<llvm::MemoryBuffer> *moduleBuffer,
                          std::unique_ptr<llvm::MemoryBuffer> *moduleDocBuffer,
                          bool &isFramework);

  virtual std::error_code
  openModuleFiles(StringRef DirName, StringRef ModuleFilename,
                  StringRef ModuleDocFilename,
                  std::unique_ptr<llvm::MemoryBuffer> *ModuleBuffer,
                  std::unique_ptr<llvm::MemoryBuffer> *ModuleDocBuffer,
                  llvm::SmallVectorImpl<char> &Scratch);

public:
  virtual ~SerializedModuleLoaderBase();
  SerializedModuleLoaderBase(const SerializedModuleLoaderBase &) = delete;
  SerializedModuleLoaderBase(SerializedModuleLoaderBase &&) = delete;
  SerializedModuleLoaderBase &operator=(const SerializedModuleLoaderBase &) = delete;
  SerializedModuleLoaderBase &operator=(SerializedModuleLoaderBase &&) = delete;

  /// Attempt to load a serialized AST into the given module.
  ///
  /// If the AST cannot be loaded and \p diagLoc is present, a diagnostic is
  /// printed. (Note that \p diagLoc is allowed to be invalid.)
  FileUnit *loadAST(ModuleDecl &M, Optional<SourceLoc> diagLoc,
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

    /// \brief Check whether the module with a given name can be imported without
  /// importing it.
  ///
  /// Note that even if this check succeeds, errors may still occur if the
  /// module is loaded in full.
  virtual bool canImportModule(std::pair<Identifier, SourceLoc> named) override;

  /// \brief Import a module with the given module path.
  ///
  /// \param importLoc The location of the 'import' keyword.
  ///
  /// \param path A sequence of (identifier, location) pairs that denote
  /// the dotted module name to load, e.g., AppKit.NSWindow.
  ///
  /// \returns the module referenced, if it could be loaded. Otherwise,
  /// emits a diagnostic and returns a FailedImportModule object.
  virtual ModuleDecl *
  loadModule(SourceLoc importLoc,
             ArrayRef<std::pair<Identifier, SourceLoc>> path) override;


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

/// \brief Imports serialized Swift modules into an ASTContext.
class SerializedModuleLoader : public SerializedModuleLoaderBase {

  explicit SerializedModuleLoader(ASTContext &ctx, DependencyTracker *tracker)
    : SerializedModuleLoaderBase(ctx, tracker)
  {}

public:
  virtual ~SerializedModuleLoader();

  /// \brief Create a new importer that can load serialized Swift modules
  /// into the given ASTContext.
  static std::unique_ptr<SerializedModuleLoader>
  create(ASTContext &ctx, DependencyTracker *tracker = nullptr) {
    return std::unique_ptr<SerializedModuleLoader>{
      new SerializedModuleLoader(ctx, tracker)
    };
  }
};

/// A file-unit loaded from a serialized AST file.
class SerializedASTFile final : public LoadedFile {
  friend class SerializedModuleLoaderBase;
  friend class SerializedSILLoader;
  friend class ModuleFile;

  ModuleFile &File;
  bool IsSIB;

  ~SerializedASTFile() = default;

  SerializedASTFile(ModuleDecl &M, ModuleFile &file, bool isSIB = false)
    : LoadedFile(FileUnitKind::SerializedAST, M), File(file), IsSIB(isSIB) {}

  void
  collectLinkLibrariesFromImports(ModuleDecl::LinkLibraryCallback callback) const;

public:
  bool isSIB() const { return IsSIB; }

  /// Returns the language version that was used to compile the contents of this
  /// file.
  const version::Version &getLanguageVersionBuiltWith() const;

  virtual bool isSystemModule() const override;

  virtual void lookupValue(ModuleDecl::AccessPathTy accessPath,
                           DeclName name, NLKind lookupKind,
                           SmallVectorImpl<ValueDecl*> &results) const override;

  virtual TypeDecl *lookupLocalType(StringRef MangledName) const override;

  virtual TypeDecl *
  lookupNestedType(Identifier name,
                   const NominalTypeDecl *parent) const override;

  virtual OperatorDecl *lookupOperator(Identifier name,
                                       DeclKind fixity) const override;

  virtual PrecedenceGroupDecl *
  lookupPrecedenceGroup(Identifier name) const override;

  virtual void lookupVisibleDecls(ModuleDecl::AccessPathTy accessPath,
                                  VisibleDeclConsumer &consumer,
                                  NLKind lookupKind) const override;

  virtual void lookupClassMembers(ModuleDecl::AccessPathTy accessPath,
                                  VisibleDeclConsumer &consumer) const override;

  virtual void
  lookupClassMember(ModuleDecl::AccessPathTy accessPath, DeclName name,
                    SmallVectorImpl<ValueDecl*> &decls) const override;

  /// Find all Objective-C methods with the given selector.
  void lookupObjCMethods(
         ObjCSelector selector,
         SmallVectorImpl<AbstractFunctionDecl *> &results) const override;

  Optional<CommentInfo> getCommentForDecl(const Decl *D) const override;

  Optional<StringRef> getGroupNameForDecl(const Decl *D) const override;


  Optional<StringRef> getSourceFileNameForDecl(const Decl *D) const override;

  Optional<unsigned> getSourceOrderForDecl(const Decl *D) const override;

  Optional<StringRef> getGroupNameByUSR(StringRef USR) const override;

  void collectAllGroups(std::vector<StringRef> &Names) const override;

  virtual void getTopLevelDecls(SmallVectorImpl<Decl*> &results) const override;

  virtual void
  getPrecedenceGroups(SmallVectorImpl<PrecedenceGroupDecl*> &Results) const override;

  virtual void
  getLocalTypeDecls(SmallVectorImpl<TypeDecl*> &results) const override;

  virtual void getDisplayDecls(SmallVectorImpl<Decl*> &results) const override;

  virtual void
  getImportedModules(SmallVectorImpl<ModuleDecl::ImportedModule> &imports,
                     ModuleDecl::ImportFilter filter) const override;

  virtual void
  collectLinkLibraries(ModuleDecl::LinkLibraryCallback callback) const override;

  Identifier getDiscriminatorForPrivateValue(const ValueDecl *D) const override;

  virtual StringRef getFilename() const override;

  ClassDecl *getMainClass() const override;

  bool hasEntryPoint() const override;

  virtual const clang::Module *getUnderlyingClangModule() const override;

  virtual bool getAllGenericSignatures(
                   SmallVectorImpl<GenericSignature*> &genericSignatures)
                override;

  static bool classof(const FileUnit *file) {
    return file->getKind() == FileUnitKind::SerializedAST;
  }
  static bool classof(const DeclContext *DC) {
    return isa<FileUnit>(DC) && classof(cast<FileUnit>(DC));
  }
};


} // end namespace swift

#endif
