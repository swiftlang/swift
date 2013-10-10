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

#include "swift/Basic/Dwarf.h"
#include "swift/AST/ModuleLoader.h"
#include "llvm/ADT/OwningPtr.h"
#include "llvm/Support/MemoryBuffer.h"
#include <map>

namespace swift {
class ASTContext;
class Module;
class ModuleFile;

/// \brief Imports serialized Swift modules into an ASTContext.
class SerializedModuleLoader : public ModuleLoader {
private:
  /// This is only used to pass as owner of FailedImportModules so that the
  /// rest of SerializedModuleLoader can assume that it is receiving valid
  /// modules.
  class FailedImportModuleLoader : public ModuleLoader {
    Module *loadModule(SourceLoc importLoc,
                     ArrayRef<std::pair<Identifier, SourceLoc>> path) override {
      return nullptr;
    }

    StringRef getModuleFilename(const Module *Module) override;
  };

  FailedImportModuleLoader FailedImportLoader;

  ASTContext &Ctx;
  std::map<std::string, std::unique_ptr<llvm::MemoryBuffer> > MemoryBuffers;
  /// A { module, generation # } pair.
  using LoadedModulePair = std::pair<std::unique_ptr<ModuleFile>, unsigned>;
  std::vector<LoadedModulePair> LoadedModuleFiles;

  explicit SerializedModuleLoader(ASTContext &ctx);

public:
  /// \brief Create a new importer that can load serialized Swift modules
  /// into the given ASTContext.
  static SerializedModuleLoader *create(ASTContext &ctx) {
    return new SerializedModuleLoader(ctx);
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
  loadModule(SourceLoc importLoc, Module::AccessPathTy path) override;

  /// \brief Register a memory buffer that contains the serialized
  /// module for the given access path. This API is intended to be
  /// used by LLDB to add swiftmodules discovered in the __apple_ast
  /// section of a Mach-O file to the search path.
  /// FIXME: make this an actual access *path* once submodules are designed.
  void registerMemoryBuffer(StringRef AccessPath,
                            std::unique_ptr<llvm::MemoryBuffer> input) {
    MemoryBuffers[AccessPath].reset(input.release());
  }

  /// \brief Look for declarations associated with the given name.
  ///
  /// \param module The module to search.
  ///
  /// \param accessPath The access path used to refer to the name within this
  /// (top-level) module.
  ///
  /// \param name The name we're searching for.
  ///
  /// \param lookupKind Whether we're performing qualified vs. unqualified
  /// lookup.
  ///
  /// \param result Will be populated with the results of name lookup.
  virtual void lookupValue(Module *module,
                           Module::AccessPathTy accessPath, Identifier name,
                           NLKind lookupKind,
                           SmallVectorImpl<ValueDecl*> &result) override;

  /// \brief Look for a declaration of the given operator.
  ///
  /// \returns The operator decl, or null if this module does not define the
  /// operator in question.
  virtual OperatorDecl *lookupOperator(Module *module, Identifier name,
                                       DeclKind fixity) override;

  virtual void getImportedModules(
    const Module *module,
    SmallVectorImpl<Module::ImportedModule> &imports,
    bool includePrivate) override;

  virtual void lookupVisibleDecls(const Module *module,
                                  Module::AccessPathTy accessPath,
                                  VisibleDeclConsumer &consumer,
                                  NLKind lookupKind) override;

  virtual void lookupClassMembers(const Module *module,
                                  Module::AccessPathTy accessPath,
                                  VisibleDeclConsumer &consumer) override;

  virtual void lookupClassMember(const Module *module,
                                 Module::AccessPathTy accessPath,
                                 Identifier name,
                                 SmallVectorImpl<ValueDecl*> &results) override;

  virtual void loadExtensions(NominalTypeDecl *nominal,
                              unsigned previousGeneration) override;

  virtual void loadDeclsConformingTo(KnownProtocolKind kind,
                                     unsigned previousGeneration) override;

  virtual void getLinkLibraries(const Module *module,
                                Module::LinkLibraryCallback callback) override;
  
  virtual void getTopLevelDecls(const Module *Module,
                                SmallVectorImpl<Decl*> &Results) override;

  virtual void getDisplayDecls(const Module *module,
                               SmallVectorImpl<Decl*> &results) override;

  StringRef getModuleFilename(const Module *Module) override;
};

/// Describes whether a loaded module can be used.
enum class ModuleStatus {
  /// The module is valid.
  Valid,

  /// The module file format is too new to be used by this version of the
  /// compiler.
  FormatTooNew,

  /// The module file depends on another module that can't be loaded.
  MissingDependency,

  /// The module file is malformed in some way.
  Malformed
};

/// Represents a module that failed to get imported.
class FailedImportModule : public LoadedModule {
public:
  const ModuleStatus Status;
  std::string ModuleFilename;

  FailedImportModule(Identifier Name, ModuleStatus Status,
                     StringRef ModuleFilename,
                     ASTContext &Ctx, ModuleLoader &Owner)
    : LoadedModule(ModuleKind::FailedImport, Name, std::string(), Ctx, Owner),
      Status(Status) {
    assert(Status != ModuleStatus::Valid && "module is valid ?");
  }

  static bool classof(const Module *M) {
    return M->getKind() == ModuleKind::FailedImport;
  }
  static bool classof(const DeclContext *DC) {
    return isa<Module>(DC) && classof(cast<Module>(DC));
  }
};

} // end namespace swift

#endif
