//===--- ModuleFile.h - Info about a loaded serialized module ---*- C++ -*-===//
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

#ifndef SWIFT_SERIALIZATION_MODULEFILE_H
#define SWIFT_SERIALIZATION_MODULEFILE_H

#include "ModuleFormat.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Identifier.h"
#include "swift/AST/KnownProtocols.h"
#include "swift/AST/LinkLibrary.h"
#include "swift/AST/Module.h"
#include "swift/AST/TypeLoc.h"
#include "swift/Serialization/SerializedModuleLoader.h"
#include "swift/Basic/Fixnum.h"
#include "swift/Basic/LLVM.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/OwningPtr.h"
#include "llvm/ADT/TinyPtrVector.h"
#include "llvm/Bitcode/BitstreamReader.h"

namespace llvm {
  class BitstreamCursor;
  class BitstreamReader;
  class MemoryBuffer;
}

// This template should eventually move to llvm/Support.
namespace clang {
  template <typename Info>
  class OnDiskChainedHashTable;
}

namespace swift {
class Pattern;
class ProtocolConformance;

/// A serialized module, along with the tools to access it.
class ModuleFile {
  /// A reference back to the AST representation of the file.
  FileUnit *FileContext = nullptr;

  /// The module shadowed by this module, if any.
  Module *ShadowedModule = nullptr;

  /// The module file data.
  llvm::OwningPtr<llvm::MemoryBuffer> InputFile;

  /// The reader attached to InputFile.
  llvm::BitstreamReader InputReader;

  /// The cursor used to lazily load things from the file.
  llvm::BitstreamCursor DeclTypeCursor;

  llvm::BitstreamCursor SILCursor;
  llvm::BitstreamCursor SILIndexCursor;

  /// The data blob containing all of the module's identifiers.
  StringRef IdentifierData;

  /// Paths to the source files used to build this module.
  SmallVector<StringRef, 4> SourcePaths;

public:
  /// Represents another module that has been imported as a dependency.
  class Dependency {
  public:
    Module::ImportedModule Import;
    StringRef RawAccessPath;
    bool IsExported;

    Dependency(StringRef path, bool Exported)
      : Import(), RawAccessPath(path), IsExported(Exported) {}

    bool isLoaded() const {
      return Import.second != nullptr;
    }
  };

private:
  /// All modules this module depends on.
  SmallVector<Dependency, 8> Dependencies;

  /// All of this module's link-time dependencies.
  SmallVector<LinkLibrary, 8> LinkLibraries;

public:
  template <typename T>
  class Serialized {
  private:
    using RawBitOffset = decltype(DeclTypeCursor.GetCurrentBitNo());

    using ImplTy = PointerUnion<T, serialization::BitOffset>;
    ImplTy Value;

  public:
    /*implicit*/ Serialized(serialization::BitOffset offset) : Value(offset) {}

    bool isComplete() const {
      return Value.template is<T>();
    }

    T get() const {
      return Value.template get<T>();
    }

    /*implicit*/ operator T() const {
      return get();
    }

    /*implicit*/ operator serialization::BitOffset() const {
      return Value.template get<serialization::BitOffset>();
    }

    /*implicit*/ operator RawBitOffset() const {
      return Value.template get<serialization::BitOffset>();
    }

    template <typename Derived>
    Serialized &operator=(Derived deserialized) {
      assert(!isComplete() || ImplTy(deserialized) == Value);
      Value = deserialized;
      return *this;
    }
  };

private:
  /// Decls referenced by this module.
  std::vector<Serialized<Decl*>> Decls;

  /// Types referenced by this module.
  std::vector<Serialized<Type>> Types;

  /// Represents an identifier that may or may not have been deserialized yet.
  ///
  /// If \c Offset is non-zero, the identifier has not been loaded yet.
  class SerializedIdentifier {
  public:
    Identifier Ident;
    serialization::BitOffset Offset;

    template <typename IntTy>
    /*implicit*/ SerializedIdentifier(IntTy rawOffset)
      : Offset(rawOffset) {}
  };

  /// Identifiers referenced by this module.
  std::vector<SerializedIdentifier> Identifiers;

  class DeclTableInfo;
  using SerializedDeclTable = clang::OnDiskChainedHashTable<DeclTableInfo>;

  std::unique_ptr<SerializedDeclTable> TopLevelDecls;
  std::unique_ptr<SerializedDeclTable> OperatorDecls;
  std::unique_ptr<SerializedDeclTable> ExtensionDecls;
  std::unique_ptr<SerializedDeclTable> ClassMembersByName;

  using DeclIDVector = SmallVector<serialization::DeclID, 4>;

  /// All adopters of compiler-known protocols in this module.
  DeclIDVector KnownProtocolAdopters[NumKnownProtocols];
  DeclIDVector EagerDeserializationDecls;

  /// Whether this module file can be used.
  ModuleStatus Status;

  /// Constructs an new module and validates it.
  ModuleFile(llvm::OwningPtr<llvm::MemoryBuffer> &&input);

  /// Convenience function for module loading.
  void error(ModuleStatus issue = ModuleStatus::Malformed) {
    assert(issue != ModuleStatus::Valid);
    Status = issue;
  }

  ASTContext &getContext() const {
    assert(FileContext && "no associated context yet");
    return FileContext->getParentModule()->Ctx;
  }

  /// Read an on-disk decl hash table stored in index_block::DeclListLayout
  /// format.
  std::unique_ptr<SerializedDeclTable>
  readDeclTable(ArrayRef<uint64_t> fields, StringRef blobData);

  /// Reads the known protocols block.
  bool readKnownProtocolsBlock(llvm::BitstreamCursor &cursor);

  /// Reads the index block, which contains global tables.
  ///
  /// Returns false if there was an error.
  bool readIndexBlock(llvm::BitstreamCursor &cursor);

  /// Recursively reads a pattern from \c DeclTypeCursor.
  ///
  /// If the record at the cursor is not a pattern, returns null.
  Pattern *maybeReadPattern();

  /// Read the underlying conformance for a specialized or inherited
  /// protocol conformance.
  ProtocolConformance *
  readUnderlyingConformance(ProtocolDecl *proto,
                            serialization::DeclID typeID,
                            serialization::IdentifierID moduleID,
                            llvm::BitstreamCursor &Cursor);

  /// Recursively reads a protocol conformance from \c DeclTypeCursor.
  ///
  /// The conformance will be newly-created; it's likely that it already exists
  /// in the AST, and will need to be canonicalized.
  ///
  /// If the record at the cursor is not a protocol conformance, returns
  /// Nothing. Note that a null pointer is a valid conformance value.
  Optional<std::pair<ProtocolDecl *, ProtocolConformance *>>
  maybeReadConformance(Type conformingType, llvm::BitstreamCursor &Cursor);

  /// Reads a generic param list from \c DeclTypeCursor.
  ///
  /// If the record at the cursor is not a generic param list, returns null
  /// without moving the cursor.
  GenericParamList *maybeReadGenericParams(DeclContext *DC);

  GenericParamList *maybeGetOrReadGenericParams(serialization::DeclID contextID,
                                                DeclContext *DC);

  /// Reads a set of requirements from \c DeclTypeCursor.
  void readGenericRequirements(SmallVectorImpl<Requirement> &requirements);

  /// Reads members of a DeclContext from \c DeclTypeCursor.
  ///
  /// The returned array is owned by the ASTContext.
  /// Returns Nothing if there is an error.
  ///
  /// Note: this destroys the cursor's position in the stream. Furthermore,
  /// because it reads from the cursor, it is not possible to reset the cursor
  /// after reading. Nothing should ever follow a DECL_CONTEXT block.
  Optional<MutableArrayRef<Decl *>> readMembers();

  /// Returns the decl context with the given ID, deserializing it if needed.
  DeclContext *getDeclContext(serialization::DeclID DID);

  /// Populates TopLevelIDs for name lookup.
  void buildTopLevelDeclMap();

public:
  /// Loads a module from the given memory buffer.
  ///
  /// \param input A memory buffer containing the serialized module data.
  ///              The created module takes ownership of the buffer, even if
  ///              there's an error in loading.
  /// \param[out] module The loaded module.
  /// \returns Whether the module was successfully loaded, or what went wrong
  ///          if it was not.
  static ModuleStatus load(llvm::OwningPtr<llvm::MemoryBuffer> &&input,
                           std::unique_ptr<ModuleFile> &module) {
    module.reset(new ModuleFile(std::move(input)));
    return module->getStatus();
  }

  // Out of line to avoid instantiation OnDiskChainedHashTable here.
  ~ModuleFile();

  /// Associates this module file with an AST module.
  ///
  /// Returns false if the association failed.
  bool associateWithFileContext(FileUnit *file);

  /// Checks whether this module can be used.
  ModuleStatus getStatus() const { return Status; }

  /// Returns paths to the source files that were used to build this module.
  ArrayRef<StringRef> getInputSourcePaths() const {
    assert(getStatus() == ModuleStatus::Valid);
    return SourcePaths;
  }

  /// Returns the list of modules this module depends on.
  ArrayRef<Dependency> getDependencies() const {
    return Dependencies;
  }

  /// Searches the module's top-level decls for the given identifier.
  void lookupValue(Identifier name, SmallVectorImpl<ValueDecl*> &results);

  /// Searches the module's operators for one with the given name and fixity.
  ///
  /// If none is found, returns null.
  OperatorDecl *lookupOperator(Identifier name, DeclKind fixity);

  /// Adds any imported modules to the given vector.
  ///
  /// Unless \p includePrivate is true, only re-exported modules are included.
  void getImportedModules(SmallVectorImpl<Module::ImportedModule> &results,
                          bool includePrivate);

  /// Reports all visible top-level members in this module.
  void lookupVisibleDecls(Module::AccessPathTy accessPath,
                          VisibleDeclConsumer &consumer,
                          NLKind lookupKind);

  /// Loads extensions for the given decl.
  ///
  /// Note that this may cause other decls to load as well.
  void loadExtensions(NominalTypeDecl *nominal);

  /// Loads decls that conform to the given protocol.
  ///
  /// Note that this may cause other decls to load as well.
  void loadDeclsConformingTo(KnownProtocolKind kind);
  
  /// Reports all class members in the module to the given consumer.
  ///
  /// This is intended for use with id-style lookup and code completion.
  void lookupClassMembers(Module::AccessPathTy accessPath,
                          VisibleDeclConsumer &consumer);

  /// Adds class members in the module with the given name to the given vector.
  ///
  /// This is intended for use with id-style lookup.
  void lookupClassMember(Module::AccessPathTy accessPath,
                         Identifier name,
                         SmallVectorImpl<ValueDecl*> &results);

  /// Reports all link-time dependencies.
  void collectLinkLibraries(Module::LinkLibraryCallback callback) const;
  
  /// Adds all top-level decls to the given vector.
  void getTopLevelDecls(SmallVectorImpl<Decl*> &Results);

  /// Adds all top-level decls to the given vector.
  ///
  /// This includes all decls that should be displayed to clients of the module.
  /// This can differ from \c getTopLevelDecls, e.g. it returns decls from a
  /// shadowed clang module.
  void getDisplayDecls(SmallVectorImpl<Decl*> &results);

  StringRef getModuleFilename() const {
    // FIXME: This seems fragile, maybe store the filename separately ?
    return InputFile->getBufferIdentifier();
  }

  llvm::BitstreamCursor getSILCursor() const {
    return SILCursor;
  }
  llvm::BitstreamCursor getSILIndexCursor() const {
    return SILIndexCursor;
  }

  /// Returns the type with the given ID, deserializing it if needed.
  Type getType(serialization::TypeID TID);

  /// Returns the identifier with the given ID, deserializing it if needed.
  Identifier getIdentifier(serialization::IdentifierID IID);

  /// Returns the decl with the given ID, deserializing it if needed.
  ///
  /// \param DID The ID for the decl within this module.
  /// \param ForcedContext Optional override for the decl context of certain
  ///                      kinds of decls, used to avoid re-entrant
  ///                      deserialization.
  /// \param DidRecord Optional callback, called at some point after the decl
  ///                  has been recorded in the decl table (but not necessarily
  ///                  completed).
  Decl *getDecl(serialization::DeclID DID,
                Optional<DeclContext *> ForcedContext = {},
                std::function<void(Decl*)> DidRecord = nullptr);

  /// Returns the appropriate module for the given ID.
  Module *getModule(serialization::ModuleID MID);

  /// Returns the appropriate module for the given name.
  ///
  /// If the name matches the name of the current module, a shadowed module
  /// is loaded instead.
  Module *getModule(Identifier name);

  /// Reads a substitution record from \c DeclTypeCursor.
  ///
  /// If the record at the cursor is not a substitution, returns Nothing.
  Optional<Substitution> maybeReadSubstitution(llvm::BitstreamCursor &Cursor);
};

/// A file-unit loaded from a serialized AST file.
class SerializedASTFile final : public LoadedFile {
public:
  ModuleFile &File;

  SerializedASTFile(TranslationUnit &TU, ModuleFile &file)
    : LoadedFile(FileUnitKind::SerializedAST, TU), File(file) {}

  virtual void lookupValue(Module::AccessPathTy accessPath,
                           Identifier name, NLKind lookupKind,
                           SmallVectorImpl<ValueDecl*> &results) const override;

  virtual OperatorDecl *lookupOperator(Identifier name,
                                       DeclKind fixity) const override;

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
    return file->getKind() == FileUnitKind::SerializedAST;
  }
  static bool classof(const DeclContext *DC) {
    return isa<FileUnit>(DC) && classof(cast<FileUnit>(DC));
  }
};

} // end namespace swift

#endif
