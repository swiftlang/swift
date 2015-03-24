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

#include "swift/AST/Decl.h"
#include "swift/AST/Identifier.h"
#include "swift/AST/LazyResolver.h"
#include "swift/AST/LinkLibrary.h"
#include "swift/AST/Module.h"
#include "swift/AST/RawComment.h"
#include "swift/AST/TypeLoc.h"
#include "swift/Serialization/ModuleFormat.h"
#include "swift/Serialization/Validation.h"
#include "swift/Basic/LLVM.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/Fixnum.h"
#include "llvm/ADT/TinyPtrVector.h"
#include "llvm/Bitcode/BitstreamReader.h"
#include "llvm/Support/MemoryBuffer.h"

namespace llvm {
  class BitstreamCursor;
  class BitstreamReader;
  class MemoryBuffer;
  template <typename Info> class OnDiskIterableChainedHashTable;
}

namespace swift {
class Pattern;
class ProtocolConformance;

/// A serialized module, along with the tools to access it.
class ModuleFile : public LazyMemberLoader {
  friend class SerializedASTFile;
  using Status = serialization::Status;

  /// A reference back to the AST representation of the file.
  FileUnit *FileContext = nullptr;

  /// The module shadowed by this module, if any.
  Module *ShadowedModule = nullptr;

  /// The module file data.
  std::unique_ptr<llvm::MemoryBuffer> ModuleInputBuffer;
  std::unique_ptr<llvm::MemoryBuffer> ModuleDocInputBuffer;

  /// The reader attached to \c ModuleInputBuffer.
  llvm::BitstreamReader ModuleInputReader;

  /// The reader attached to \c ModuleDocInputBuffer.
  llvm::BitstreamReader ModuleDocInputReader;

  /// The cursor used to lazily load things from the file.
  llvm::BitstreamCursor DeclTypeCursor;

  llvm::BitstreamCursor SILCursor;
  llvm::BitstreamCursor SILIndexCursor;

  /// The name of the module.
  StringRef Name;

  /// The target the module was built for.
  StringRef TargetTriple;

  /// The data blob containing all of the module's identifiers.
  StringRef IdentifierData;

public:
  /// Represents another module that has been imported as a dependency.
  class Dependency {
  public:
    Module::ImportedModule Import = {};
    const StringRef RawPath;

  private:
    const unsigned IsExported : 1;
    const unsigned IsHeader : 1;
    const unsigned IsScoped : 1;

    Dependency(StringRef path, bool isHeader, bool exported, bool isScoped)
      : RawPath(path), IsExported(exported), IsHeader(isHeader),
        IsScoped(isScoped) {}

  public:
    Dependency(StringRef path, bool exported, bool isScoped)
      : Dependency(path, false, exported, isScoped) {}

    static Dependency forHeader(StringRef headerPath, bool exported) {
      return Dependency(headerPath, true, exported, false);
    }

    bool isLoaded() const {
      return Import.second != nullptr;
    }

    bool isExported() const { return IsExported; }
    bool isHeader() const { return IsHeader; }
    bool isScoped() const { return IsScoped; }

    std::string getPrettyPrintedPath() const;
  };

private:
  /// All modules this module depends on.
  SmallVector<Dependency, 8> Dependencies;

  /// Search paths this module may provide.
  ///
  /// This is not intended for use by frameworks, but may show up in debug
  /// modules.
  std::vector<std::pair<StringRef, bool>> SearchPaths;

  /// Info for the (lone) imported header for this module.
  struct {
    off_t fileSize;
    time_t fileModTime;
    StringRef contents;
  } importedHeaderInfo = {};

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

    void unsafeOverwrite(T t) {
      Value = t;
    }
  };

  /// A class for holding a value that can be partially deserialized.
  ///
  /// This class assumes that "T()" is not a valid deserialized value.
  template <typename T>
  class PartiallySerialized {
  private:
    using RawBitOffset = decltype(DeclTypeCursor.GetCurrentBitNo());

    /// The deserialized value.
    T Value;

    /// The offset.
    serialization::BitOffset Offset;

    unsigned IsFullyDeserialized : 1;

  public:
    /*implicit*/ PartiallySerialized(serialization::BitOffset offset)
      : Value(), Offset(offset), IsFullyDeserialized(0) {}

    /*implicit*/ PartiallySerialized(RawBitOffset offset)
      : Value(), Offset(offset), IsFullyDeserialized(0) {}

    bool isDeserialized() const {
      return Value != T();
    }

    bool isFullyDeserialized() const {
      return isDeserialized() && IsFullyDeserialized;
    }

    serialization::BitOffset getOffset() const {
      assert(!isFullyDeserialized());
      return Offset;
    }

    T get() const {
      assert(isDeserialized());
      return Value;
    }

    void reset() {
      IsFullyDeserialized = 0;
      Value = T();
    }

    void set(T value, bool isFullyDeserialized) {
      assert(!isDeserialized() || Value == value);
      Value = value;
      IsFullyDeserialized = isFullyDeserialized;
    }
  };

private:
  /// Decls referenced by this module.
  std::vector<Serialized<Decl*>> Decls;

  /// DeclContexts referenced by this module.
  std::vector<Serialized<DeclContext*>> DeclContexts;

  /// Local DeclContexts referenced by this module.
  std::vector<Serialized<DeclContext*>> LocalDeclContexts;

  /// Normal protocol conformances referenced by this module.
  std::vector<Serialized<NormalProtocolConformance *>> NormalConformances;

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
  using SerializedDeclTable =
      llvm::OnDiskIterableChainedHashTable<DeclTableInfo>;

  class LocalDeclTableInfo;
  using SerializedLocalDeclTable =
      llvm::OnDiskIterableChainedHashTable<LocalDeclTableInfo>;

  std::unique_ptr<SerializedDeclTable> TopLevelDecls;
  std::unique_ptr<SerializedDeclTable> OperatorDecls;
  std::unique_ptr<SerializedDeclTable> ExtensionDecls;
  std::unique_ptr<SerializedDeclTable> ClassMembersByName;
  std::unique_ptr<SerializedDeclTable> OperatorMethodDecls;
  std::unique_ptr<SerializedLocalDeclTable> LocalTypeDecls;

  class ObjCMethodTableInfo;
  using SerializedObjCMethodTable =
    llvm::OnDiskIterableChainedHashTable<ObjCMethodTableInfo>;

  std::unique_ptr<SerializedObjCMethodTable> ObjCMethods;

  llvm::DenseMap<const ValueDecl *, Identifier> PrivateDiscriminatorsByValue;

  TinyPtrVector<Decl *> ImportDecls;

  using DeclIDVector = SmallVector<serialization::DeclID, 4>;

  DeclIDVector EagerDeserializationDecls;

  class DeclCommentTableInfo;
  using SerializedDeclCommentTable =
      llvm::OnDiskIterableChainedHashTable<DeclCommentTableInfo>;

  std::unique_ptr<SerializedDeclCommentTable> DeclCommentTable;

  struct {
    /// The decl ID of the main class in this module file, if it has one.
    unsigned EntryPointDeclID : 31;

    /// Whether or not this module file comes from a context that had a main
    /// entry point.
    unsigned HasEntryPoint : 1;

    /// Whether this module file comes from a framework.
    unsigned IsFramework : 1;

    /// Whether this module has a shadowed module that's part of its public
    /// interface.
    unsigned HasUnderlyingModule : 1;

    /// Whether or not ImportDecls is valid.
    unsigned ComputedImportDecls : 1;

    /// Whether this module file can be used, and what's wrong if not.
    unsigned Status : 4;

    // Explicitly pad out to the next word boundary.
    unsigned : 0;
  } Bits = {};
  static_assert(sizeof(Bits) <= 8, "The bit set should be small");

  void setStatus(Status status) {
    Bits.Status = static_cast<unsigned>(status);
    assert(status == getStatus() && "not enough bits for status");
  }

  void setEntryPointClassID(serialization::DeclID DID) {
    Bits.HasEntryPoint = true;
    Bits.EntryPointDeclID = DID;
    assert(Bits.EntryPointDeclID == DID && "not enough bits for DeclID");
  }

  /// Creates a new AST node to represent a deserialized decl.
  template <typename T, typename ...Args>
  T *createDecl(Args &&... args);

  /// Constructs an new module and validates it.
  ModuleFile(std::unique_ptr<llvm::MemoryBuffer> moduleInputBuffer,
             std::unique_ptr<llvm::MemoryBuffer> moduleDocInputBuffer,
             bool isFramework, serialization::ExtendedValidationInfo *extInfo);

public:
  /// Change the status of the current module. Default argument marks the module
  /// as being malformed.
  Status error(Status issue = Status::Malformed) {
    assert(issue != Status::Valid);
    assert((!FileContext || issue != Status::Malformed) &&
           "error deserializing an individual record");
    setStatus(issue);
    return getStatus();
  }

  ASTContext &getContext() const {
    assert(FileContext && "no associated context yet");
    return FileContext->getParentModule()->Ctx;
  }

  Module *getAssociatedModule() const {
    assert(FileContext && "no associated context yet");
    return FileContext->getParentModule();
  }

private:
  /// Read an on-disk decl hash table stored in index_block::DeclListLayout
  /// format.
  std::unique_ptr<SerializedDeclTable>
  readDeclTable(ArrayRef<uint64_t> fields, StringRef blobData);

  /// Read an on-disk local decl hash table stored in
  /// index_block::DeclListLayout format.
  std::unique_ptr<SerializedLocalDeclTable>
  readLocalDeclTable(ArrayRef<uint64_t> fields, StringRef blobData);

  /// Read an on-disk Objective-C method table stored in
  /// index_block::ObjCMethodTableLayout format.
  std::unique_ptr<ModuleFile::SerializedObjCMethodTable>
  readObjCMethodTable(ArrayRef<uint64_t> fields, StringRef blobData);

  /// Reads the index block, which contains global tables.
  ///
  /// Returns false if there was an error.
  bool readIndexBlock(llvm::BitstreamCursor &cursor);

  /// Read an on-disk decl hash table stored in
  /// \c comment_block::DeclCommentListLayout format.
  std::unique_ptr<SerializedDeclCommentTable>
  readDeclCommentTable(ArrayRef<uint64_t> fields, StringRef blobData);

  /// Reads the comment block, which contains USR to comment mappings.
  ///
  /// Returns false if there was an error.
  bool readCommentBlock(llvm::BitstreamCursor &cursor);

  /// Recursively reads a pattern from \c DeclTypeCursor.
  ///
  /// If the record at the cursor is not a pattern, returns null.
  Pattern *maybeReadPattern();

  GenericParamList *maybeGetOrReadGenericParams(serialization::DeclID contextID,
                                                DeclContext *DC,
                                                llvm::BitstreamCursor &Cursor);

  /// Reads a set of requirements from \c DeclTypeCursor.
  void readGenericRequirements(SmallVectorImpl<Requirement> &requirements);

  /// Populates the vector with members of a DeclContext from \c DeclTypeCursor.
  ///
  /// Returns true if there is an error.
  ///
  /// Note: this destroys the cursor's position in the stream. Furthermore,
  /// because it reads from the cursor, it is not possible to reset the cursor
  /// after reading. Nothing should ever follow a MEMBERS record.
  bool readMembers(SmallVectorImpl<Decl *> &Members);

  /// Resolves a cross-reference, starting from the given module.
  ///
  /// Note: this destroys the cursor's position in the stream. Furthermore,
  /// because it reads from the cursor, it is not possible to reset the cursor
  /// after reading. Nothing should ever follow an XREF record except
  /// XREF_PATH_PIECE records.
  Decl *resolveCrossReference(Module *M, uint32_t pathLen);

  /// Populates TopLevelIDs for name lookup.
  void buildTopLevelDeclMap();

  void configureStorage(AbstractStorageDecl *storage, unsigned rawStorageKind,
                        serialization::DeclID getter,
                        serialization::DeclID setter,
                        serialization::DeclID materializeForSet,
                        serialization::DeclID addressor,
                        serialization::DeclID mutableAddressor,
                        serialization::DeclID willSet,
                        serialization::DeclID didSet);

public:
  /// Loads a module from the given memory buffer.
  ///
  /// \param moduleInputBuffer A memory buffer containing the serialized module
  /// data. The created ModuleFile takes ownership of the buffer, even if
  /// there's an error in loading.
  /// \param moduleDocInputBuffer An optional memory buffer containing
  /// documentation data for the module. The created ModuleFile takes ownership
  /// of the buffer, even if there's an error in loading.
  /// \param isFramework If true, this is treated as a framework module for
  /// linking purposes.
  /// \param[out] theModule The loaded module.
  /// \param[out] extInfo Optionally, extra info serialized about the module.
  /// \returns Whether the module was successfully loaded, or what went wrong
  ///          if it was not.
  static Status
  load(std::unique_ptr<llvm::MemoryBuffer> moduleInputBuffer,
       std::unique_ptr<llvm::MemoryBuffer> moduleDocInputBuffer,
       bool isFramework, std::unique_ptr<ModuleFile> &theModule,
       serialization::ExtendedValidationInfo *extInfo = nullptr) {
    theModule.reset(new ModuleFile(std::move(moduleInputBuffer),
                                   std::move(moduleDocInputBuffer),
                                   isFramework, extInfo));
    return theModule->getStatus();
  }

  // Out of line to avoid instantiation OnDiskChainedHashTable here.
  ~ModuleFile();

  /// Associates this module file with an AST module.
  ///
  /// Returns any error that occurred during association, including validation
  /// that the module file is compatible with the module it's being loaded as.
  Status associateWithFileContext(FileUnit *file, SourceLoc diagLoc);

  /// Checks whether this module can be used.
  Status getStatus() const {
    return static_cast<Status>(Bits.Status);
  }

  /// Returns the list of modules this module depends on.
  ArrayRef<Dependency> getDependencies() const {
    return Dependencies;
  }

  /// The module shadowed by this module, if any.
  Module *getShadowedModule() const { return ShadowedModule; }

  /// Searches the module's top-level decls for the given identifier.
  void lookupValue(DeclName name, SmallVectorImpl<ValueDecl*> &results);

  /// Searches the module's local type decls for the given mangled name.
  TypeDecl *lookupLocalType(StringRef MangledName);

  /// Searches the module's operators for one with the given name and fixity.
  ///
  /// If none is found, returns null.
  OperatorDecl *lookupOperator(Identifier name, DeclKind fixity);

  /// Adds any imported modules to the given vector.
  void getImportedModules(SmallVectorImpl<Module::ImportedModule> &results,
                          Module::ImportFilter filter);

  void getImportDecls(SmallVectorImpl<Decl *> &Results);

  /// Reports all visible top-level members in this module.
  void lookupVisibleDecls(Module::AccessPathTy accessPath,
                          VisibleDeclConsumer &consumer,
                          NLKind lookupKind);

  /// Loads extensions for the given decl.
  ///
  /// Note that this may cause other decls to load as well.
  void loadExtensions(NominalTypeDecl *nominal);

  /// \brief Load the methods within the given class that that produce
  /// Objective-C class or instance methods with the given selector.
  ///
  /// \param classDecl The class in which we are searching for @objc methods.
  /// The search only considers this class and its extensions; not any
  /// superclasses.
  ///
  /// \param selector The selector to search for.
  ///
  /// \param isInstanceMethod Whether we are looking for an instance method
  /// (vs. a class method).
  ///
  /// \param methods The list of @objc methods in this class that have this
  /// selector and are instance/class methods as requested.
  void loadObjCMethods(ClassDecl *classDecl,
                       ObjCSelector selector,
                       bool isInstanceMethod,
                       llvm::TinyPtrVector<AbstractFunctionDecl *> &methods);

  /// Reports all class members in the module to the given consumer.
  ///
  /// This is intended for use with id-style lookup and code completion.
  void lookupClassMembers(Module::AccessPathTy accessPath,
                          VisibleDeclConsumer &consumer);

  /// Adds class members in the module with the given name to the given vector.
  ///
  /// This is intended for use with id-style lookup.
  void lookupClassMember(Module::AccessPathTy accessPath,
                         DeclName name,
                         SmallVectorImpl<ValueDecl*> &results);

  /// Reports all link-time dependencies.
  void collectLinkLibraries(Module::LinkLibraryCallback callback) const;

  /// Adds all top-level decls to the given vector.
  void getTopLevelDecls(SmallVectorImpl<Decl*> &Results);

  /// Adds all local type decls to the given vector.
  void getLocalTypeDecls(SmallVectorImpl<TypeDecl*> &Results);

  /// Adds all top-level decls to the given vector.
  ///
  /// This includes all decls that should be displayed to clients of the module.
  /// This can differ from \c getTopLevelDecls, e.g. it returns decls from a
  /// shadowed clang module.
  void getDisplayDecls(SmallVectorImpl<Decl*> &results);

  StringRef getModuleFilename() const {
    // FIXME: This seems fragile, maybe store the filename separately ?
    return ModuleInputBuffer->getBufferIdentifier();
  }

  /// Returns the module name as stored in the serialized data.
  StringRef getModuleName() const {
    return Name;
  }

  /// Returns the target triple the module was compiled for,
  /// as stored in the serialized data.
  StringRef getTargetTriple() const {
    return TargetTriple;
  }

  /// AST-verify imported decls.
  ///
  /// Has no effect in NDEBUG builds.
  void verify() const;

  virtual void loadAllMembers(Decl *D,
                              uint64_t contextData,
                              bool *ignored) override;

  virtual void
  loadAllConformances(const Decl *D, uint64_t contextData,
                      SmallVectorImpl<ProtocolConformance*> &Conforms) override;

  virtual TypeLoc loadAssociatedTypeDefault(const AssociatedTypeDecl *ATD,
                                            uint64_t contextData) override;

  Optional<BriefAndRawComment> getCommentForDecl(const Decl *D);
  Optional<BriefAndRawComment> getCommentForDeclByUSR(StringRef USR);

  Identifier getDiscriminatorForPrivateValue(const ValueDecl *D);

  // MARK: Deserialization interface

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
  Decl *getDecl(serialization::DeclID DID,
                Optional<DeclContext *> ForcedContext = None);

  /// Returns the decl context with the given ID, deserializing it if needed.
  DeclContext *getDeclContext(serialization::DeclContextID DID);

  /// Returns the local decl context with the given ID, deserializing it if needed.
  DeclContext *getLocalDeclContext(serialization::DeclContextID DID);

  /// Returns the appropriate module for the given ID.
  Module *getModule(serialization::ModuleID MID);

  /// Returns the appropriate module for the given name.
  ///
  /// If the name matches the name of the current module, a shadowed module
  /// is loaded instead.
  Module *getModule(ArrayRef<Identifier> name);

  /// Reads a substitution record from \c DeclTypeCursor.
  ///
  /// If the record at the cursor is not a substitution, returns None.
  Optional<Substitution> maybeReadSubstitution(llvm::BitstreamCursor &Cursor);

  /// Recursively reads a protocol conformance from the given cursor.
  ///
  /// Note that a null conformance is valid for archetypes.
  ProtocolConformance *readConformance(llvm::BitstreamCursor &Cursor);

  /// Read the given normal conformance from the current module file.
  NormalProtocolConformance *readNormalConformance(
                               serialization::NormalConformanceID id);

  /// Reads a generic param list from \c DeclTypeCursor.
  ///
  /// If the record at the cursor is not a generic param list, returns null
  /// without moving the cursor.
  GenericParamList *maybeReadGenericParams(DeclContext *DC,
                                     llvm::BitstreamCursor &Cursor,
                                     GenericParamList *outerParams = nullptr);
};

} // end namespace swift

#endif
