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

#include "swift/AST/FileUnit.h"
#include "swift/AST/Module.h"
#include "swift/AST/ModuleLoader.h"
#include "llvm/Support/MemoryBuffer.h"

namespace swift {
class ModuleFile;
class LazyResolver;

/// Spceifies how to load modules when both a module interface and serialized
/// AST are present, or whether to disallow one format or the other altogether.
enum class ModuleLoadingMode {
  PreferInterface,
  PreferSerialized,
  OnlyInterface,
  OnlySerialized
};

/// Common functionality shared between \c SerializedModuleLoader and
/// \c ModuleInterfaceLoader.
class SerializedModuleLoaderBase : public ModuleLoader {
  /// A { module, generation # } pair.
  using LoadedModulePair = std::pair<std::unique_ptr<ModuleFile>, unsigned>;
  std::vector<LoadedModulePair> LoadedModuleFiles;

  SmallVector<std::unique_ptr<llvm::MemoryBuffer>, 2> OrphanedMemoryBuffers;

protected:
  ASTContext &Ctx;
  ModuleLoadingMode LoadMode;
  SerializedModuleLoaderBase(ASTContext &ctx, DependencyTracker *tracker,
                             ModuleLoadingMode LoadMode);

  void collectVisibleTopLevelModuleNamesImpl(SmallVectorImpl<Identifier> &names,
                                             StringRef extension) const;

  using AccessPathElem = std::pair<Identifier, SourceLoc>;
  bool findModule(AccessPathElem moduleID,
                  std::unique_ptr<llvm::MemoryBuffer> *moduleBuffer,
                  std::unique_ptr<llvm::MemoryBuffer> *moduleDocBuffer,
                  bool &isFramework, bool &isSystemModule);

  /// Attempts to search the provided directory for a loadable serialized
  /// .swiftmodule with the provided `ModuleFilename`. Subclasses must
  /// override this method to perform their custom module lookup behavior.
  ///
  /// If such a module could not be loaded, the subclass must return a
  /// `std::error_code` indicating the failure. There are two specific error
  /// codes that will be treated specially:
  /// - `errc::no_such_file_or_directory`: The module loader will stop looking
  ///   for loadable modules and will diagnose the lookup failure.
  /// - `errc::not_supported`: The module loader will stop looking for loadable
  ///   modules and will defer to the remaining module loaders to look up this
  ///   module.
  virtual std::error_code findModuleFilesInDirectory(
      AccessPathElem ModuleID, StringRef DirPath, StringRef ModuleFilename,
      StringRef ModuleDocFilename,
      std::unique_ptr<llvm::MemoryBuffer> *ModuleBuffer,
      std::unique_ptr<llvm::MemoryBuffer> *ModuleDocBuffer) = 0;

  std::error_code
  openModuleFiles(AccessPathElem ModuleID,
                  StringRef ModulePath, StringRef ModuleDocPath,
                  std::unique_ptr<llvm::MemoryBuffer> *ModuleBuffer,
                  std::unique_ptr<llvm::MemoryBuffer> *ModuleDocBuffer);

  std::error_code
  openModuleDocFile(AccessPathElem ModuleID,
                    StringRef ModuleDocPath,
                    std::unique_ptr<llvm::MemoryBuffer> *ModuleDocBuffer);

  /// If the module loader subclass knows that all options have been tried for
  /// loading an architecture-specific file out of a swiftmodule bundle, try
  /// to list the architectures that \e are present.
  ///
  /// \returns true if an error diagnostic was emitted
  virtual bool maybeDiagnoseTargetMismatch(SourceLoc sourceLocation,
                                           StringRef moduleName,
                                           StringRef archName,
                                           StringRef directoryPath) {
    return false;
  }

  /// Determines if the provided path is a cached artifact for dependency
  /// tracking purposes.
  virtual bool isCached(StringRef DepPath) {
    return false;
  }

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
                    bool isFramework, bool treatAsPartialModule);

  /// Check whether the module with a given name can be imported without
  /// importing it.
  ///
  /// Note that even if this check succeeds, errors may still occur if the
  /// module is loaded in full.
  virtual bool canImportModule(std::pair<Identifier, SourceLoc> named) override;

  /// Import a module with the given module path.
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

/// Imports serialized Swift modules into an ASTContext.
class SerializedModuleLoader : public SerializedModuleLoaderBase {

  SerializedModuleLoader(ASTContext &ctx, DependencyTracker *tracker,
                         ModuleLoadingMode loadMode)
    : SerializedModuleLoaderBase(ctx, tracker, loadMode)
  {}

  std::error_code findModuleFilesInDirectory(
      AccessPathElem ModuleID, StringRef DirPath, StringRef ModuleFilename,
      StringRef ModuleDocFilename,
      std::unique_ptr<llvm::MemoryBuffer> *ModuleBuffer,
      std::unique_ptr<llvm::MemoryBuffer> *ModuleDocBuffer) override;

  bool maybeDiagnoseTargetMismatch(SourceLoc sourceLocation,
                                   StringRef moduleName,
                                   StringRef archName,
                                   StringRef directoryPath) override;

public:
  virtual ~SerializedModuleLoader();

  /// Append visible module names to \p names. Note that names are possibly
  /// duplicated, and not guaranteed to be ordered in any way.
  void collectVisibleTopLevelModuleNames(
      SmallVectorImpl<Identifier> &names) const override;

  /// Create a new importer that can load serialized Swift modules
  /// into the given ASTContext.
  static std::unique_ptr<SerializedModuleLoader>
  create(ASTContext &ctx, DependencyTracker *tracker = nullptr,
         ModuleLoadingMode loadMode = ModuleLoadingMode::PreferSerialized) {
    return std::unique_ptr<SerializedModuleLoader>{
      new SerializedModuleLoader(ctx, tracker, loadMode)
    };
  }
};

/// Imports serialized Swift modules from a MemoryBuffer into an ASTContext.
/// This interface is primarily used by LLDB.
class MemoryBufferSerializedModuleLoader : public SerializedModuleLoaderBase {
  llvm::StringMap<std::unique_ptr<llvm::MemoryBuffer>> MemoryBuffers;

  MemoryBufferSerializedModuleLoader(ASTContext &ctx,
                                     DependencyTracker *tracker,
                                     ModuleLoadingMode loadMode)
      : SerializedModuleLoaderBase(ctx, tracker, loadMode) {}

  std::error_code findModuleFilesInDirectory(
      AccessPathElem ModuleID, StringRef DirPath, StringRef ModuleFilename,
      StringRef ModuleDocFilename,
      std::unique_ptr<llvm::MemoryBuffer> *ModuleBuffer,
      std::unique_ptr<llvm::MemoryBuffer> *ModuleDocBuffer) override;

  bool maybeDiagnoseTargetMismatch(SourceLoc sourceLocation,
                                   StringRef moduleName,
                                   StringRef archName,
                                   StringRef directoryPath) override;

public:
  virtual ~MemoryBufferSerializedModuleLoader();

  bool canImportModule(std::pair<Identifier, SourceLoc> named) override;
  ModuleDecl *
  loadModule(SourceLoc importLoc,
             ArrayRef<std::pair<Identifier, SourceLoc>> path) override;

  /// Register a memory buffer that contains the serialized module for the given
  /// access path. This API is intended to be used by LLDB to add swiftmodules
  /// discovered in the __swift_ast section of a Mach-O file (or the .swift_ast
  /// section of an ELF file) to the search path.
  ///
  /// FIXME: make this an actual access *path* once submodules are designed.
  void registerMemoryBuffer(StringRef AccessPath,
                            std::unique_ptr<llvm::MemoryBuffer> input) {
    MemoryBuffers[AccessPath] = std::move(input);
  }

  void collectVisibleTopLevelModuleNames(
      SmallVectorImpl<Identifier> &names) const override {}

  /// Create a new importer that can load serialized Swift modules
  /// into the given ASTContext.
  static std::unique_ptr<MemoryBufferSerializedModuleLoader>
  create(ASTContext &ctx, DependencyTracker *tracker = nullptr,
         ModuleLoadingMode loadMode = ModuleLoadingMode::PreferSerialized) {
    return std::unique_ptr<MemoryBufferSerializedModuleLoader>{
        new MemoryBufferSerializedModuleLoader(ctx, tracker, loadMode)};
  }
};


/// A file-unit loaded from a serialized AST file.
class SerializedASTFile final : public LoadedFile {
  friend class SerializedModuleLoaderBase;
  friend class SerializedSILLoader;
  friend class ModuleFile;

  ModuleFile &File;

  bool IsSIB;

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

  virtual void lookupValue(DeclName name, NLKind lookupKind,
                           SmallVectorImpl<ValueDecl*> &results) const override;

  virtual StringRef
  getFilenameForPrivateDecl(const ValueDecl *decl) const override;

  virtual TypeDecl *lookupLocalType(StringRef MangledName) const override;
  
  virtual OpaqueTypeDecl *
  lookupOpaqueResultType(StringRef MangledName, LazyResolver *resolver) override;

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
  virtual void
  getOpaqueReturnTypeDecls(SmallVectorImpl<OpaqueTypeDecl*> &results) const override;

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
                   SmallVectorImpl<GenericSignature> &genericSignatures)
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
