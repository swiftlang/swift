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
#include <map>

namespace swift {
class ModuleFile;

/// Describes whether a loaded module can be used.
enum class ModuleStatus {
  /// The module is valid.
  Valid,

  /// The module file format is too old to be used by this version of the
  /// compiler.
  FormatTooOld,

  /// The module file format is too new to be used by this version of the
  /// compiler.
  FormatTooNew,

  /// The module file depends on another module that can't be loaded.
  MissingDependency,

  /// The module file is an overlay for a Clang module, which can't be found.
  MissingShadowedModule,

  /// The module file is malformed in some way.
  Malformed,

  /// The module documentation file is malformed in some way.
  MalformedDocumentation,

  /// The module file's name does not match the module it is being loaded into.
  NameMismatch
};

/// \brief Imports serialized Swift modules into an ASTContext.
class SerializedModuleLoader : public ModuleLoader {
private:
  ASTContext &Ctx;
  std::map<std::string, std::unique_ptr<llvm::MemoryBuffer> > MemoryBuffers;
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
    MemoryBuffers[AccessPath].reset(input.release());
  }

  virtual void loadExtensions(NominalTypeDecl *nominal,
                              unsigned previousGeneration) override;

  virtual void loadObjCMethods(
                 ClassDecl *classDecl,
                 ObjCSelector selector,
                 bool isInstanceMethod,
                 unsigned previousGeneration,
                 llvm::TinyPtrVector<AbstractFunctionDecl *> &methods);

  /// Returns true if the data looks like it contains a serialized AST.
  static bool isSerializedAST(StringRef data);

  /// \see validateSerializedAST()
  struct ValidationInfo {
    StringRef name;
    size_t bytes;
    ModuleStatus status;
  };

  /// Returns info about the serialized AST in the given data.
  ///
  /// If the returned status is anything but ModuleStatus::Valid, the
  /// serialized data cannot be loaded by this version of the compiler. If the
  /// returned size is non-zero, it's possible to skip over this module's data,
  /// in case there is more data in the buffer. The returned name, which may
  /// be empty, directly points into the given data buffer.
  ///
  /// Note that this does not actually try to load the module or validate any
  /// of its dependencies; it only checks that it /can/ be loaded.
  static ValidationInfo validateSerializedAST(StringRef data);

  virtual void verifyAllModules() override;
};

/// A file-unit loaded from a serialized AST file.
class SerializedASTFile final : public LoadedFile {
  friend class SerializedModuleLoader;
  friend class SerializedSILLoader;

  ModuleFile &File;

  ~SerializedASTFile() = default;

  SerializedASTFile(Module &M, ModuleFile &file)
    : LoadedFile(FileUnitKind::SerializedAST, M), File(file) {}

public:
  ModuleFile &getFile() const { return File; }

  virtual bool isSystemModule() const override;

  virtual void lookupValue(Module::AccessPathTy accessPath,
                           DeclName name, NLKind lookupKind,
                           SmallVectorImpl<ValueDecl*> &results) const override;

  virtual TypeDecl *lookupLocalType(llvm::StringRef FileHash,
                                    unsigned int LocalDiscriminator,
                                    llvm::StringRef Name) const override;

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

  virtual void getLocalTypeDecls(SmallVectorImpl<Decl *> &results) const override;

  virtual void getDisplayDecls(SmallVectorImpl<Decl*> &results) const override;

  virtual void
  getImportedModules(SmallVectorImpl<Module::ImportedModule> &imports,
                     Module::ImportFilter filter) const override;

  virtual void
  collectLinkLibraries(Module::LinkLibraryCallback callback) const override;

  Identifier getDiscriminatorForPrivateValue(const ValueDecl *D) const override;
  Identifier getDiscriminatorForLocalValue(const ValueDecl *D) const override;

  virtual StringRef getFilename() const override;

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
