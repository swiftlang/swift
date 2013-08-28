//===--- Serialization.cpp - Read and write Swift modules -----------------===//
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

#include "swift/Subsystems.h"
#include "ModuleFormat.h"
#include "swift/AST/AST.h"
#include "swift/AST/Diagnostics.h"
#include "swift/AST/KnownProtocols.h"
#include "swift/AST/LinkLibrary.h"
#include "swift/Basic/STLExtras.h"
#include "swift/Basic/SourceManager.h"
#include "swift/Serialization/BCRecordLayout.h"

// This is a template-only header; eventually it should move to llvm/Support.
#include "clang/Basic/OnDiskHashTable.h"

#include "llvm/ADT/StringExtras.h"
#include "llvm/Bitcode/BitstreamWriter.h"
#include "llvm/Config/config.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/raw_ostream.h"
#include <array>
#include <queue>

using namespace swift;
using namespace swift::serialization;
using clang::OnDiskChainedHashTableGenerator;

namespace {
  typedef ArrayRef<unsigned> FileBufferIDs;

  /// Used to serialize the on-disk decl hash table.
  class DeclTableInfo {
  public:
    using key_type = Identifier;
    using key_type_ref = key_type;
    using data_type = SmallVector<std::pair<uint8_t, DeclID>, 4>;
    using data_type_ref = const data_type &;

    uint32_t ComputeHash(key_type_ref key) {
      assert(!key.empty());
      return llvm::HashString(key.str());
    }

    std::pair<unsigned, unsigned> EmitKeyDataLength(raw_ostream &out,
                                                    key_type_ref key,
                                                    data_type_ref data) {
      using namespace clang::io;
      uint32_t keyLength = key.str().size();
      uint32_t dataLength = (sizeof(DeclID) + 1) * data.size();
      Emit16(out, keyLength);
      Emit16(out, dataLength);
      return { keyLength, dataLength };
    }

    void EmitKey(raw_ostream &out, key_type_ref key, unsigned len) {
      out << key.str();
    }

    void EmitData(raw_ostream &out, key_type_ref key, data_type_ref data,
                  unsigned len) {
      static_assert(sizeof(DeclID) <= 32, "DeclID too large");
      using namespace clang::io;
      for (auto entry : data) {
        Emit8(out, entry.first);
        Emit32(out, entry.second);
      }
    }
  };

  class Serializer {
    SmallVector<char, 0> Buffer;
    llvm::BitstreamWriter Out{Buffer};

    /// A reusable buffer for emitting records.
    SmallVector<uint64_t, 64> ScratchRecord;

    /// The TranslationUnit currently being serialized.
    const TranslationUnit *TU = nullptr;

  public:    
    /// Stores a declaration or a type to be written to the AST file.
    ///
    /// Convenience wrapper around a PointerUnion.
    class DeclTypeUnion {
      using DataTy = llvm::PointerUnion<const Decl *, Type>;
      DataTy Data;

      explicit DeclTypeUnion(const void *val)
        : Data(DataTy::getFromOpaqueValue(const_cast<void *>(val))) {}

    public:
      /*implicit*/ DeclTypeUnion(const Decl *d)
        : Data(d) { }
      /*implicit*/ DeclTypeUnion(Type ty)
        : Data(ty) { }

      bool isDecl() const { return Data.is<const Decl *>(); }
      bool isType() const { return Data.is<Type>(); }

      Type getType() const { return Data.get<Type>(); }
      const Decl *getDecl() const { return Data.get<const Decl *>(); }

      const void *getOpaqueValue() const { return Data.getOpaqueValue(); }
      static DeclTypeUnion getFromOpaqueValue(void *opaqueVal) {
        return DeclTypeUnion(opaqueVal);
      }

      bool operator==(const DeclTypeUnion &other) const {
        return Data == other.Data;
      }
    };

  private:
    /// A map from Types and Decls to their serialized IDs.
    llvm::DenseMap<DeclTypeUnion, DeclID> DeclIDs;

    /// A map from Identifiers to their serialized IDs.
    llvm::DenseMap<Identifier, IdentifierID> IdentifierIDs;

    /// A map from generic parameter lists to the decls they come from.
    llvm::DenseMap<const GenericParamList *, const Decl *> GenericContexts;

  public:
    /// The in-memory representation of what will eventually be an on-disk hash
    /// table.
    using DeclTable = llvm::DenseMap<Identifier, DeclTableInfo::data_type>;

  private:
    /// A map from identifiers to methods and properties with the given name.
    ///
    /// This is used for id-style lookup.
    DeclTable ClassMembersByName;

    /// The queue of types and decls that need to be serialized.
    ///
    /// This is a queue and not simply a vector because serializing one
    /// decl-or-type might trigger the serialization of another one.
    std::queue<DeclTypeUnion> DeclsAndTypesToWrite;

    /// All identifiers that need to be serialized.
    std::vector<Identifier> IdentifiersToWrite;

    /// The abbreviation code for each record in the "decls-and-types" block.
    ///
    /// These are registered up front when entering the block, so they can be
    /// reused.
    std::array<unsigned, 256> DeclTypeAbbrCodes;

    /// The offset of each Decl in the bitstream, indexed by DeclID.
    std::vector<BitOffset> DeclOffsets;

    /// The offset of each Type in the bitstream, indexed by TypeID.
    std::vector<BitOffset> TypeOffsets;

    /// The offset of each Identifier in the identifier data block, indexed by
    /// IdentifierID.
    std::vector<CharOffset> IdentifierOffsets;

    SmallVector<DeclID, 2> KnownProtocolAdopters[NumKnownProtocols];
    SmallVector<DeclID, 2> EagerDeserializationDecls;

    /// The last assigned DeclID for decls from this module.
    DeclID LastDeclID = 0;

    /// The last assigned DeclID for types from this module.
    TypeID LastTypeID = 0;

    /// The last assigned IdentifierID for types from this module.
    IdentifierID LastIdentifierID = 0;

    /// True if this module does not fully represent the original source file.
    ///
    /// This is a bring-up hack and will eventually go away.
    bool ShouldFallBackToTranslationUnit = false;

    /// Returns the record code for serializing the given vector of offsets.
    ///
    /// This allows the offset-serialization code to be generic over all kinds
    /// of offsets.
    unsigned getOffsetRecordCode(const std::vector<BitOffset> &values) {
      if (&values == &DeclOffsets)
        return index_block::DECL_OFFSETS;
      if (&values == &TypeOffsets)
        return index_block::TYPE_OFFSETS;
      if (&values == &IdentifierOffsets)
        return index_block::IDENTIFIER_OFFSETS;
      llvm_unreachable("unknown offset kind");
    }

    /// Records the use of the given Decl.
    ///
    /// The Decl will be scheduled for serialization if necessary.
    ///
    /// \returns The ID for the given Decl in this module.
    DeclID addDeclRef(const Decl *D);

    /// Records the use of the given Type.
    ///
    /// The Type will be scheduled for serialization if necessary.
    ///
    /// \returns The ID for the given Type in this module.
    TypeID addTypeRef(Type ty);

    /// Records the use of the given Identifier.
    ///
    /// The Identifier will be scheduled for serialization if necessary.
    ///
    /// \returns The ID for the given Identifier in this module.
    IdentifierID addIdentifierRef(Identifier ident);

    /// Records the use of the given module.
    ///
    /// The module's name will be scheduled for serialization if necessary.
    ///
    /// \returns The ID for the identifier for the module's name, or 0 for the
    ///          builtin module.
    IdentifierID addModuleRef(const Module *M);

    /// Returns the declaration the given generic parameter list is associated
    /// with.
    const Decl *getGenericContext(const GenericParamList *paramList);

    /// Writes the BLOCKINFO block.
    void writeBlockInfoBlock();

    /// Writes the Swift module file header, BLOCKINFO block, and
    /// non-TU-specific metadata.
    void writeHeader();

    /// Writes the dependencies used to build this module: its imported
    /// modules and its source files.
    void writeInputFiles(const TranslationUnit *TU, FileBufferIDs inputFiles,
                         StringRef moduleLinkName);

    /// Writes the given pattern, recursively.
    void writePattern(const Pattern *pattern);

    /// Writes a generic parameter list.
    bool writeGenericParams(const GenericParamList *genericParams);

    /// Writes a list of generic substitutions.
    void writeSubstitutions(ArrayRef<Substitution> substitutions);

    /// Encode the underlying conformance of a generic or specialized
    /// conformance.
    ///
    /// \param conformance The conformance we're encoding.
    ///
    /// \param typeID Will be set to the "type ID" value to be stored
    /// in the parent record.
    ///
    /// \param moduleID Will be set to the "module ID" value to
    /// be stored in the parent record.
    ///
    /// \returns true if the underlying conformance will need to be written
    /// out as its own record following the parent record.
    bool encodeUnderlyingConformance(const ProtocolConformance *conformance,
                                     DeclID &typeID,
                                     IdentifierID &moduleID);

    /// Writes a protocol conformance.
    void writeConformance(const ProtocolDecl *protocol,
                          const ProtocolConformance *conformance,
                          const Decl *associatedDecl);

    /// Writes a list of protocol conformances.
    void writeConformances(ArrayRef<ProtocolDecl *> protocols,
                           ArrayRef<ProtocolConformance *> conformances,
                           const Decl *associatedDecl = nullptr);

    /// Writes an array of members for a decl context.
    ///
    /// \param members The decls within the context
    /// \param isClass True if the context could be a class context (class,
    ///        class extension, or protocol).
    void writeMembers(ArrayRef<Decl *> members, bool isClass);

    /// Writes a reference to a decl in another module.
    ///
    /// Returns false if the decl cannot be serialized without losing
    /// information.
    bool writeCrossReference(const Decl *D);

    /// Writes the given decl.
    ///
    /// Returns false if the decl cannot be serialized without losing
    /// information.
    bool writeDecl(const Decl *D);

    /// Writes the given type.
    ///
    /// Returns false if the type cannot be serialized without losing
    /// information.
    bool writeType(Type ty);

    /// Registers the abbreviation for the given decl or type layout.
    template <typename Layout>
    void registerDeclTypeAbbr() {
      using AbbrArrayTy = decltype(DeclTypeAbbrCodes);
      static_assert(Layout::Code <= std::tuple_size<AbbrArrayTy>::value,
                    "layout has invalid record code");
      DeclTypeAbbrCodes[Layout::Code] = Layout::emitAbbrev(Out);
    }

    /// Writes all decls and types in the DeclsToWrite queue.
    ///
    /// This will continue until the queue is empty, even if the items currently
    /// in the queue trigger the serialization of additional decls and/or types.
    void writeAllDeclsAndTypes();

    /// Writes all identifiers in the IdentifiersToWrite queue.
    ///
    /// This must be called after writeAllDeclsAndTypes(), since that may add
    /// additional identifiers to the pool.
    void writeAllIdentifiers();

    /// Writes the offsets for decls or types.
    void writeOffsets(const index_block::OffsetsLayout &Offsets,
                      const std::vector<BitOffset> &values);

    /// Top-level entry point for serializing a translation unit module.
    void writeTranslationUnit(const TranslationUnit *TU);

  public:
    Serializer() = default;

    /// Serialize a translation unit to the given stream.
    void writeToStream(raw_ostream &os, const TranslationUnit *TU,
                       FileBufferIDs inputFiles, StringRef moduleLinkName);
  };
} // end anonymous namespace

namespace llvm {
  template<> struct DenseMapInfo<Serializer::DeclTypeUnion> {
    using DeclTypeUnion = Serializer::DeclTypeUnion;
    static inline DeclTypeUnion getEmptyKey() { return nullptr; }
    static inline DeclTypeUnion getTombstoneKey() { return swift::Type(); }
    static unsigned getHashValue(const DeclTypeUnion &val) {
      return DenseMapInfo<const void *>::getHashValue(val.getOpaqueValue());
    }
    static bool isEqual(const DeclTypeUnion &lhs, const DeclTypeUnion &rhs) {
      return lhs == rhs;
    }
  };
}

static const Decl *getDeclForContext(const DeclContext *DC) {
  switch (DC->getContextKind()) {
  case DeclContextKind::TranslationUnit:
    // Use a null decl to represent the translation unit.
    // FIXME: multiple TUs within a module?
    return nullptr;
  case DeclContextKind::BuiltinModule:
    llvm_unreachable("builtins should be handled explicitly");
  case DeclContextKind::SerializedModule:
  case DeclContextKind::ClangModule:
    llvm_unreachable("shouldn't serialize decls from an imported module");
  case DeclContextKind::TopLevelCodeDecl:
    llvm_unreachable("shouldn't serialize the main module");
  case DeclContextKind::CapturingExpr: {
    // FIXME: What about default functions?
    assert(isa<FuncExpr>(DC) &&
           "shouldn't serialize decls from anonymous closures");
    auto FD = cast<FuncExpr>(DC)->getDecl();
    assert(FD && "shouldn't serialize decls from anonymous closures");
    return FD;
  }
  case DeclContextKind::NominalTypeDecl:
    return cast<NominalTypeDecl>(DC);
  case DeclContextKind::ExtensionDecl:
    return cast<ExtensionDecl>(DC);
  case DeclContextKind::ConstructorDecl:
    return cast<ConstructorDecl>(DC);
  case DeclContextKind::DestructorDecl:
    return cast<DestructorDecl>(DC);
  }
}

DeclID Serializer::addDeclRef(const Decl *D) {
  if (!D)
    return 0;

  DeclID &id = DeclIDs[D];
  if (id != 0)
    return id;

  // Record any generic parameters that come from this decl, so that we can use
  // the decl to refer to the parameters later.
  const GenericParamList *paramList = nullptr;
  switch (D->getKind()) {
  case DeclKind::Constructor:
    paramList = cast<ConstructorDecl>(D)->getGenericParams();
    break;
  case DeclKind::Func:
    paramList = cast<FuncDecl>(D)->getGenericParams();
    break;
  case DeclKind::Class:
  case DeclKind::Struct:
  case DeclKind::Union:
    paramList = cast<NominalTypeDecl>(D)->getGenericParams();
    break;
  default:
    break;
  }
  if (paramList)
    GenericContexts[paramList] = D;

  id = ++LastDeclID;
  DeclsAndTypesToWrite.push(D);
  return id;
}

TypeID Serializer::addTypeRef(Type ty) {
  if (!ty)
    return 0;

  TypeID &id = DeclIDs[ty];
  if (id != 0)
    return id;

  id = ++LastTypeID;
  DeclsAndTypesToWrite.push(ty);
  return id;
}

IdentifierID Serializer::addIdentifierRef(Identifier ident) {
  if (ident.empty())
    return 0;

  IdentifierID &id = IdentifierIDs[ident];
  if (id != 0)
    return id;

  id = ++LastIdentifierID;
  IdentifiersToWrite.push_back(ident);
  return id;
}

IdentifierID Serializer::addModuleRef(const Module *M) {
  assert(M != TU && "cannot form cross-reference to module being serialized");
  if (M == TU->Ctx.TheBuiltinModule)
    return 0;

  return addIdentifierRef(M->Name);
}

const Decl *Serializer::getGenericContext(const GenericParamList *paramList) {
  auto contextDecl = GenericContexts.lookup(paramList);
  assert(contextDecl && "Generic parameters not registered yet!");
  return contextDecl;
}

/// Record the name of a block.
static void emitBlockID(llvm::BitstreamWriter &out, unsigned ID,
                        StringRef name,
                        SmallVectorImpl<unsigned char> &nameBuffer) {
  SmallVector<unsigned, 1> idBuffer;
  idBuffer.push_back(ID);
  out.EmitRecord(llvm::bitc::BLOCKINFO_CODE_SETBID, idBuffer);

  // Emit the block name if present.
  if (name.empty())
    return;
  nameBuffer.resize(name.size());
  memcpy(nameBuffer.data(), name.data(), name.size());
  out.EmitRecord(llvm::bitc::BLOCKINFO_CODE_BLOCKNAME, nameBuffer);
}

/// Record the name of a record within a block.
static void emitRecordID(llvm::BitstreamWriter &out, unsigned ID,
                         StringRef name,
                         SmallVectorImpl<unsigned char> &nameBuffer) {
  assert(ID < 256 && "can't fit record ID in next to name");
  nameBuffer.resize(name.size()+1);
  nameBuffer[0] = ID;
  memcpy(nameBuffer.data()+1, name.data(), name.size());
  out.EmitRecord(llvm::bitc::BLOCKINFO_CODE_SETRECORDNAME, nameBuffer);
}

void Serializer::writeBlockInfoBlock() {
  BCBlockRAII restoreBlock(Out, llvm::bitc::BLOCKINFO_BLOCK_ID, 2);

  SmallVector<unsigned char, 64> nameBuffer;
#define BLOCK(X) emitBlockID(Out, X ## _ID, #X, nameBuffer)
#define RECORD(K, X) emitRecordID(Out, K::X, #X, nameBuffer)

  BLOCK(CONTROL_BLOCK);
  RECORD(control_block, METADATA);

  BLOCK(INPUT_BLOCK);
  RECORD(input_block, SOURCE_FILE);
  RECORD(input_block, IMPORTED_MODULE);
  RECORD(input_block, LINK_LIBRARY);

  BLOCK(DECLS_AND_TYPES_BLOCK);
  RECORD(decls_block, NAME_ALIAS_TYPE);
  RECORD(decls_block, GENERIC_TYPE_PARAM_TYPE);
  RECORD(decls_block, ASSOCIATED_TYPE_TYPE);
  RECORD(decls_block, DEPENDENT_MEMBER_TYPE);
  RECORD(decls_block, NOMINAL_TYPE);
  RECORD(decls_block, PAREN_TYPE);
  RECORD(decls_block, TUPLE_TYPE);
  RECORD(decls_block, TUPLE_TYPE_ELT);
  RECORD(decls_block, FUNCTION_TYPE);
  RECORD(decls_block, METATYPE_TYPE);
  RECORD(decls_block, LVALUE_TYPE);
  RECORD(decls_block, ARCHETYPE_TYPE);
  RECORD(decls_block, ARCHETYPE_NESTED_TYPE_NAMES);
  RECORD(decls_block, ARCHETYPE_NESTED_TYPES);
  RECORD(decls_block, PROTOCOL_COMPOSITION_TYPE);
  RECORD(decls_block, SUBSTITUTED_TYPE);
  RECORD(decls_block, BOUND_GENERIC_TYPE);
  RECORD(decls_block, BOUND_GENERIC_SUBSTITUTION);
  RECORD(decls_block, POLYMORPHIC_FUNCTION_TYPE);
  RECORD(decls_block, ARRAY_SLICE_TYPE);
  RECORD(decls_block, ARRAY_TYPE);
  RECORD(decls_block, REFERENCE_STORAGE_TYPE);
  RECORD(decls_block, UNBOUND_GENERIC_TYPE);
  RECORD(decls_block, OPTIONAL_TYPE);

  RECORD(decls_block, TYPE_ALIAS_DECL);
  RECORD(decls_block, GENERIC_TYPE_PARAM_DECL);
  RECORD(decls_block, ASSOCIATED_TYPE_DECL);
  RECORD(decls_block, STRUCT_DECL);
  RECORD(decls_block, CONSTRUCTOR_DECL);
  RECORD(decls_block, VAR_DECL);
  RECORD(decls_block, FUNC_DECL);
  RECORD(decls_block, PATTERN_BINDING_DECL);
  RECORD(decls_block, PROTOCOL_DECL);
  RECORD(decls_block, PREFIX_OPERATOR_DECL);
  RECORD(decls_block, POSTFIX_OPERATOR_DECL);
  RECORD(decls_block, INFIX_OPERATOR_DECL);
  RECORD(decls_block, CLASS_DECL);
  RECORD(decls_block, UNION_DECL);
  RECORD(decls_block, UNION_ELEMENT_DECL);
  RECORD(decls_block, SUBSCRIPT_DECL);
  RECORD(decls_block, EXTENSION_DECL);
  RECORD(decls_block, DESTRUCTOR_DECL);

  RECORD(decls_block, PAREN_PATTERN);
  RECORD(decls_block, TUPLE_PATTERN);
  RECORD(decls_block, TUPLE_PATTERN_ELT);
  RECORD(decls_block, NAMED_PATTERN);
  RECORD(decls_block, ANY_PATTERN);
  RECORD(decls_block, TYPED_PATTERN);

  RECORD(decls_block, GENERIC_PARAM_LIST);
  RECORD(decls_block, GENERIC_PARAM);
  RECORD(decls_block, GENERIC_REQUIREMENT);

  RECORD(decls_block, NO_CONFORMANCE);
  RECORD(decls_block, NORMAL_PROTOCOL_CONFORMANCE);
  RECORD(decls_block, SPECIALIZED_PROTOCOL_CONFORMANCE);
  RECORD(decls_block, INHERITED_PROTOCOL_CONFORMANCE);
  RECORD(decls_block, DECL_CONTEXT);
  RECORD(decls_block, XREF);

  BLOCK(IDENTIFIER_DATA_BLOCK);
  RECORD(identifier_block, IDENTIFIER_DATA);

  BLOCK(INDEX_BLOCK);
  RECORD(index_block, TYPE_OFFSETS);
  RECORD(index_block, DECL_OFFSETS);
  RECORD(index_block, IDENTIFIER_OFFSETS);
  RECORD(index_block, TOP_LEVEL_DECLS);
  RECORD(index_block, OPERATORS);
  RECORD(index_block, EXTENSIONS);
  RECORD(index_block, CLASS_MEMBERS);

  BLOCK(KNOWN_PROTOCOL_BLOCK);
#define PROTOCOL(Id) RECORD(index_block, Id);
#include "swift/AST/KnownProtocols.def"
  RECORD(index_block, FORCE_DESERIALIZATION);

  BLOCK(FALL_BACK_TO_TRANSLATION_UNIT);

#undef BLOCK
#undef RECORD
}

void Serializer::writeHeader() {
  writeBlockInfoBlock();

  {
    BCBlockRAII restoreBlock(Out, CONTROL_BLOCK_ID, 3);
    control_block::MetadataLayout Metadata(Out);

    // FIXME: put a real version in here.
#ifdef LLVM_VERSION_INFO
# define EXTRA_VERSION_STRING PACKAGE_STRING LLVM_VERSION_INFO
#else
# define EXTRA_VERSION_STRING PACKAGE_STRING
#endif
    Metadata.emit(ScratchRecord,
                  VERSION_MAJOR, VERSION_MINOR, EXTRA_VERSION_STRING);
#undef EXTRA_VERSION_STRING
  }
}

using ImportPathBlob = llvm::SmallString<64>;
void flattenImportPath(const Module::ImportedModule &import,
                       ImportPathBlob &out) {
  // FIXME: Submodules?
  out.append(import.second->Name.str());

  if (import.first.empty())
    return;

  out.push_back('\0');
  assert(import.first.size() == 1 && "can only handle top-level decl imports");
  auto accessPathElem = import.first.front();
  out.append(accessPathElem.first.str());
}

void Serializer::writeInputFiles(const TranslationUnit *TU,
                                 FileBufferIDs inputFiles,
                                 StringRef moduleLinkName) {
  BCBlockRAII restoreBlock(Out, INPUT_BLOCK_ID, 3);
  input_block::SourceFileLayout SourceFile(Out);
  input_block::ImportedModuleLayout ImportedModule(Out);
  input_block::LinkLibraryLayout LinkLibrary(Out);

  auto &sourceMgr = TU->Ctx.SourceMgr;
  for (auto bufferID : inputFiles) {
    // FIXME: We could really use a real FileManager here.
    auto buffer = sourceMgr->getMemoryBuffer(bufferID);
    llvm::SmallString<128> path(buffer->getBufferIdentifier());

    llvm::error_code err;
    err = llvm::sys::fs::make_absolute(path);
    if (err)
      continue;
    
    SourceFile.emit(ScratchRecord, path);
  }

  for (auto import : TU->getImports()) {
    if (import.first.second == TU->Ctx.TheBuiltinModule)
      continue;

    ImportPathBlob importPath;
    flattenImportPath(import.first, importPath);
    ImportedModule.emit(ScratchRecord, import.second, importPath);
  }

  if (!moduleLinkName.empty()) {
    LinkLibrary.emit(ScratchRecord, serialization::LibraryKind::Library,
                     moduleLinkName);
  }
}

/// Translate AST default argument kind to the Serialization enum values, which
/// are guaranteed to be stable.
static uint8_t getRawStableDefaultArgumentKind(swift::DefaultArgumentKind kind) {
  switch (kind) {
  case swift::DefaultArgumentKind::None:
    return serialization::DefaultArgumentKind::None;
  case swift::DefaultArgumentKind::Normal:
    return serialization::DefaultArgumentKind::Normal;
  case swift::DefaultArgumentKind::Column:
    return serialization::DefaultArgumentKind::Column;
  case swift::DefaultArgumentKind::File:
    return serialization::DefaultArgumentKind::File;
  case swift::DefaultArgumentKind::Line:
    return serialization::DefaultArgumentKind::Line;
  }
}

void Serializer::writePattern(const Pattern *pattern) {
  using namespace decls_block;

  assert(pattern && "null pattern");
  switch (pattern->getKind()) {
  case PatternKind::Paren: {
    unsigned abbrCode = DeclTypeAbbrCodes[ParenPatternLayout::Code];
    ParenPatternLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                   pattern->isImplicit());
    writePattern(cast<ParenPattern>(pattern)->getSubPattern());
    break;
  }
  case PatternKind::Tuple: {
    auto tuple = cast<TuplePattern>(pattern);

    unsigned abbrCode = DeclTypeAbbrCodes[TuplePatternLayout::Code];
    TuplePatternLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                   addTypeRef(tuple->getType()),
                                   tuple->getNumFields(), tuple->hasVararg(),
                                   tuple->isImplicit());

    abbrCode = DeclTypeAbbrCodes[TuplePatternEltLayout::Code];
    for (auto &elt : tuple->getFields()) {
      // FIXME: Default argument expressions?
      TuplePatternEltLayout::emitRecord(
        Out, ScratchRecord, abbrCode,
        getRawStableDefaultArgumentKind(elt.getDefaultArgKind()));
      writePattern(elt.getPattern());
    }
    break;
  }
  case PatternKind::Named: {
    auto named = cast<NamedPattern>(pattern);

    unsigned abbrCode = DeclTypeAbbrCodes[NamedPatternLayout::Code];
    NamedPatternLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                   addDeclRef(named->getDecl()),
                                   named->isImplicit());
    break;
  }
  case PatternKind::Any: {
    unsigned abbrCode = DeclTypeAbbrCodes[AnyPatternLayout::Code];
    AnyPatternLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                 addTypeRef(pattern->getType()),
                                 pattern->isImplicit());
    break;
  }
  case PatternKind::Typed: {
    auto typed = cast<TypedPattern>(pattern);

    unsigned abbrCode = DeclTypeAbbrCodes[TypedPatternLayout::Code];
    TypedPatternLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                   addTypeRef(typed->getType()),
                                   typed->isImplicit());
    writePattern(typed->getSubPattern());
    break;
  }
  case PatternKind::Isa: {
    auto isa = cast<IsaPattern>(pattern);
    
    unsigned abbrCode = DeclTypeAbbrCodes[IsaPatternLayout::Code];
    IsaPatternLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                 addTypeRef(isa->getCastTypeLoc().getType()),
                                 isa->isImplicit());
    break;
  }
  case PatternKind::NominalType: {
    auto nom = cast<NominalTypePattern>(pattern);

    unsigned abbrCode = DeclTypeAbbrCodes[NominalTypePatternLayout::Code];
    auto castTy = nom->getCastTypeLoc().getType();
    NominalTypePatternLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                         addTypeRef(castTy), nom->isImplicit());
    writePattern(nom->getSubPattern());
    break;
  }
  case PatternKind::UnionElement:
  case PatternKind::Expr:
    llvm_unreachable("FIXME: not implemented");

  case PatternKind::Var: {
    auto var = cast<VarPattern>(pattern);
    
    unsigned abbrCode = DeclTypeAbbrCodes[VarPatternLayout::Code];
    VarPatternLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                 var->isImplicit());
    writePattern(var->getSubPattern());
    break;
  }
  }
}

bool Serializer::writeGenericParams(const GenericParamList *genericParams) {
  using namespace decls_block;

  // Don't write anything if there are no generic params.
  if (!genericParams)
    return true;

  SmallVector<TypeID, 8> archetypeIDs;
  for (auto archetype : genericParams->getAllArchetypes())
    archetypeIDs.push_back(addTypeRef(archetype));

  unsigned abbrCode = DeclTypeAbbrCodes[GenericParamListLayout::Code];
  GenericParamListLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                     archetypeIDs);

  abbrCode = DeclTypeAbbrCodes[GenericParamLayout::Code];
  for (auto next : genericParams->getParams()) {
    GenericParamLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                   addDeclRef(next.getDecl()));
  }

  abbrCode = DeclTypeAbbrCodes[GenericRequirementLayout::Code];
  for (auto next : genericParams->getRequirements()) {
    switch (next.getKind()) {
    case RequirementKind::Conformance:
      GenericRequirementLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                           GenericRequirementKind::Conformance,
                                           addTypeRef(next.getSubject()),
                                           addTypeRef(next.getConstraint()));
      break;
    case RequirementKind::SameType:
      GenericRequirementLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                           GenericRequirementKind::SameType,
                                           addTypeRef(next.getFirstType()),
                                           addTypeRef(next.getSecondType()));
      break;
    }
  }

  return true;
}

bool
Serializer::encodeUnderlyingConformance(const ProtocolConformance *conformance,
                                        DeclID &typeID,
                                        IdentifierID &moduleID) {
  bool append = !isa<NormalProtocolConformance>(conformance);
  if (append) {
    // Encode the type in typeID. Set moduleID to 0 to indicate that the
    // underlying conformance will follow.
    typeID = addTypeRef(conformance->getType());
    moduleID = 0;
  } else {
    typeID = addDeclRef(conformance->getType()->getAnyNominal());
    assert(typeID && "Missing nominal type for specialized conformance");

    // '0' is a sentinel for a trailing underlying conformance record.
    // Use '1' to mean 'this module', and add 2 to any other module reference.
    if (conformance->getContainingModule() == TU)
      moduleID = 1;
    else
      moduleID = addModuleRef(conformance->getContainingModule()) + 2;
  }

  return append;
}

void
Serializer::writeConformance(const ProtocolDecl *protocol,
                             const ProtocolConformance *conformance,
                             const Decl *associatedDecl) {
  using namespace decls_block;

  if (!conformance) {
    unsigned abbrCode = DeclTypeAbbrCodes[NoConformanceLayout::Code];
    NoConformanceLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                    addDeclRef(protocol));
    return;
  }

  if (associatedDecl) {
    if (auto protoKind = protocol->getKnownProtocolKind()) {
      const NominalTypeDecl *nominal;
      if (auto extension = dyn_cast<ExtensionDecl>(associatedDecl))
        nominal = extension->getExtendedType()->getAnyNominal();
      else
        nominal = dyn_cast<NominalTypeDecl>(associatedDecl);

      auto index = static_cast<unsigned>(protoKind.getValue());
      KnownProtocolAdopters[index].push_back(addDeclRef(associatedDecl));
    }
  }

  switch (conformance->getKind()) {
  case ProtocolConformanceKind::Normal: {
    auto conf = cast<NormalProtocolConformance>(conformance);

    SmallVector<DeclID, 16> data;
    unsigned numValueWitnesses = 0;
    unsigned numTypeWitnesses = 0;
    unsigned numDefaultedDefinitions = 0;
    for (auto valueMapping : conf->getWitnesses()) {
      data.push_back(addDeclRef(valueMapping.first));
      data.push_back(addDeclRef(valueMapping.second.Decl));
      // The substitution records are serialized later.
      data.push_back(valueMapping.second.Substitutions.size());
      ++numValueWitnesses;
    }
    for (auto typeMapping : conf->getTypeWitnesses()) {
      data.push_back(addDeclRef(typeMapping.first));
      // The substitution record is serialized later.
      ++numTypeWitnesses;
    }
    for (auto defaulted : conf->getDefaultedDefinitions()) {
      data.push_back(addDeclRef(defaulted));
      ++numDefaultedDefinitions;
    }

    unsigned numInheritedConformances = conf->getInheritedConformances().size();
    unsigned abbrCode
      = DeclTypeAbbrCodes[NormalProtocolConformanceLayout::Code];
    NormalProtocolConformanceLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                                addDeclRef(protocol),
                                                numValueWitnesses,
                                                numTypeWitnesses,
                                                numInheritedConformances,
                                                numDefaultedDefinitions,
                                                data);

    // FIXME: Unfortunate to have to copy these.
    SmallVector<ProtocolDecl *, 8> inheritedProtos;
    SmallVector<ProtocolConformance *, 8> inheritedConformance;
    for (auto inheritedMapping : conf->getInheritedConformances()) {
      inheritedProtos.push_back(inheritedMapping.first);
      inheritedConformance.push_back(inheritedMapping.second);
    }
    writeConformances(inheritedProtos, inheritedConformance, associatedDecl);
    for (auto valueMapping : conf->getWitnesses())
      writeSubstitutions(valueMapping.second.Substitutions);
    for (auto typeMapping : conf->getTypeWitnesses())
      writeSubstitutions(typeMapping.second);

    break;
  }

  case ProtocolConformanceKind::Specialized: {
    auto conf = cast<SpecializedProtocolConformance>(conformance);
    SmallVector<DeclID, 16> data;
    unsigned numTypeWitnesses = 0;
    for (auto typeMapping : conf->getTypeWitnesses()) {
      data.push_back(addDeclRef(typeMapping.first));
      // The substitution record is serialized later.
      ++numTypeWitnesses;
    }
    auto substitutions = conf->getGenericSubstitutions();
    unsigned abbrCode
      = DeclTypeAbbrCodes[SpecializedProtocolConformanceLayout::Code];
    DeclID typeID;
    IdentifierID moduleID;

    bool appendGenericConformance
      = encodeUnderlyingConformance(conf->getGenericConformance(),
                                    typeID, moduleID);

    SpecializedProtocolConformanceLayout::emitRecord(Out, ScratchRecord,
                                                     abbrCode,
                                                     addDeclRef(protocol),
                                                     typeID,
                                                     moduleID,
                                                     numTypeWitnesses,
                                                     substitutions.size(),
                                                     data);
    writeSubstitutions(substitutions);
    for (auto typeMapping : conf->getTypeWitnesses())
      writeSubstitutions(typeMapping.second);

    if (appendGenericConformance) {
      writeConformance(protocol, conf->getGenericConformance(), nullptr);
    }
    break;
  }

  case ProtocolConformanceKind::Inherited: {
    auto conf = cast<InheritedProtocolConformance>(conformance);
    unsigned abbrCode
      = DeclTypeAbbrCodes[InheritedProtocolConformanceLayout::Code];
    DeclID typeID;
    IdentifierID moduleID;

    bool appendInheritedConformance
      = encodeUnderlyingConformance(conf->getInheritedConformance(),
                                    typeID, moduleID);

    InheritedProtocolConformanceLayout::emitRecord(Out, ScratchRecord,
                                                   abbrCode,
                                                   addDeclRef(protocol),
                                                   typeID,
                                                   moduleID);
    if (appendInheritedConformance) {
      writeConformance(protocol, conf->getInheritedConformance(), nullptr);
    }
    break;
  }
  }
}

void
Serializer::writeConformances(ArrayRef<ProtocolDecl *> protocols,
                              ArrayRef<ProtocolConformance *> conformances,
                              const Decl *associatedDecl) {
  using namespace decls_block;

  for_each(protocols, conformances,
           [&](const ProtocolDecl *proto, const ProtocolConformance *conf) {
    writeConformance(proto, conf, associatedDecl);
  });
}

void Serializer::writeSubstitutions(ArrayRef<Substitution> substitutions) {
  using namespace decls_block;
  auto abbrCode = DeclTypeAbbrCodes[BoundGenericSubstitutionLayout::Code];
  for (auto &sub : substitutions) {
    BoundGenericSubstitutionLayout::emitRecord(
      Out, ScratchRecord, abbrCode,
      addTypeRef(sub.Archetype),
      addTypeRef(sub.Replacement),
      sub.Archetype->getConformsTo().size());
    writeConformances(sub.Archetype->getConformsTo(), sub.Conformance);
  }
}


void Serializer::writeMembers(ArrayRef<Decl*> members, bool isClass) {
  using namespace decls_block;
  
  unsigned abbrCode = DeclTypeAbbrCodes[DeclContextLayout::Code];
  SmallVector<DeclID, 16> memberIDs;
  for (auto member : members) {
    DeclID memberID = addDeclRef(member);
    memberIDs.push_back(memberID);

    if (isClass) {
      if (auto VD = dyn_cast<ValueDecl>(member)) {
        if (VD->canBeAccessedByDynamicLookup()) {
          auto &list = ClassMembersByName[VD->getName()];
          list.push_back({getKindForTable(VD), memberID});
        }
      }
    }
  }
  DeclContextLayout::emitRecord(Out, ScratchRecord, abbrCode, memberIDs);
}

bool Serializer::writeCrossReference(const Decl *D) {
  using namespace decls_block;

  SmallVector<IdentifierID, 4> accessPath;
  XRefKind kind;
  TypeID typeID;

  if (auto value = dyn_cast<ValueDecl>(D)) {
    kind = XRefKind::SwiftValue;

    if (auto genericParam = dyn_cast<GenericTypeParamDecl>(D)) {
      kind = XRefKind::SwiftGenericParameter;
      typeID = genericParam->getIndex();
    }

    if (kind == XRefKind::SwiftValue) {
      accessPath.push_back(addIdentifierRef(value->getName()));

      // Make sure we don't create a self-referential type.
      Type ty = value->getType();
      if (ty->is<MetaTypeType>())
        ty = nullptr;
      typeID = addTypeRef(ty);
    }

  } else if (auto op = dyn_cast<OperatorDecl>(D)) {
    kind = XRefKind::SwiftOperator;
    accessPath.push_back(addIdentifierRef(op->getName()));
    typeID = getStableFixity(op->getKind());
  } else {
    llvm_unreachable("cannot cross-reference this kind of decl");
  }

  // Build up the access path by walking through parent DeclContexts.
  const DeclContext *DC;
  const ExtensionDecl *extension = nullptr;
  for (DC = D->getDeclContext(); !DC->isModuleContext(); DC = DC->getParent()) {
    if ((extension = dyn_cast<ExtensionDecl>(DC)))
      DC = extension->getExtendedType()->getNominalOrBoundGenericNominal();

    auto value = cast<ValueDecl>(getDeclForContext(DC));
    accessPath.push_back(addIdentifierRef(value->getName()));
  }

  accessPath.push_back(addModuleRef(cast<Module>(DC)));
  if (extension)
    accessPath.push_back(addModuleRef(extension->getModuleContext()));

  // Store the access path in forward order.
  std::reverse(accessPath.begin(), accessPath.end());

  unsigned abbrCode = DeclTypeAbbrCodes[XRefLayout::Code];
  XRefLayout::emitRecord(Out, ScratchRecord, abbrCode,
                         kind, typeID, !!extension, accessPath);

  return true;
}

/// Translate from the AST associativity enum to the Serialization enum
/// values, which are guaranteed to be stable.
static uint8_t getRawStableAssociativity(swift::Associativity assoc) {
  switch (assoc) {
  case swift::Associativity::Left:
    return serialization::Associativity::LeftAssociative;
  case swift::Associativity::Right:
    return serialization::Associativity::RightAssociative;
  case swift::Associativity::None:
    return serialization::Associativity::NonAssociative;
  }
}

bool Serializer::writeDecl(const Decl *D) {
  using namespace decls_block;

  assert(!D->isInvalid() && "cannot create a module with an invalid decl");
  Module *M = D->getModuleContext();
  if (M != TU)
    return writeCrossReference(D);

  assert(!D->hasClangNode() && "imported decls should use cross-references");

  switch (D->getKind()) {
  case DeclKind::Import:
    // FIXME: Do imported module names appear in the DeclContext of the
    // serialized module?
    return true;

  case DeclKind::Extension: {
    auto extension = cast<ExtensionDecl>(D);

    const Decl *DC = getDeclForContext(extension->getDeclContext());
    Type baseTy = extension->getExtendedType();

    unsigned abbrCode = DeclTypeAbbrCodes[ExtensionLayout::Code];
    ExtensionLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                addTypeRef(baseTy),
                                addDeclRef(DC),
                                extension->isImplicit());

    writeConformances(extension->getProtocols(), extension->getConformances(),
                      extension);

    bool isClassExtension = false;
    if (auto baseNominal = baseTy->getAnyNominal()) {
      isClassExtension = isa<ClassDecl>(baseNominal) ||
                         isa<ProtocolDecl>(baseNominal);
    }
    writeMembers(extension->getMembers(), isClassExtension);

    return true;
  }

  case DeclKind::PatternBinding: {
    auto binding = cast<PatternBindingDecl>(D);
    const Decl *DC = getDeclForContext(binding->getDeclContext());

    unsigned abbrCode = DeclTypeAbbrCodes[PatternBindingLayout::Code];
    PatternBindingLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                     addDeclRef(DC), binding->isImplicit());

    writePattern(binding->getPattern());
    // Ignore initializer; external clients don't need to know about it.

    return true;
  }

  case DeclKind::TopLevelCode:
    // Top-level code is ignored; external clients don't need to know about it.
    return true;

  case DeclKind::InfixOperator: {
    auto op = cast<InfixOperatorDecl>(D);

    const Decl *DC = getDeclForContext(op->getDeclContext());
    auto associativity = getRawStableAssociativity(op->getAssociativity());

    unsigned abbrCode = DeclTypeAbbrCodes[InfixOperatorLayout::Code];
    InfixOperatorLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                    addIdentifierRef(op->getName()),
                                    addDeclRef(DC),
                                    associativity,
                                    op->getPrecedence());
    return true;
  }
      
  case DeclKind::PrefixOperator: {
    auto op = cast<PrefixOperatorDecl>(D);

    const Decl *DC = getDeclForContext(op->getDeclContext());

    unsigned abbrCode = DeclTypeAbbrCodes[PrefixOperatorLayout::Code];
    PrefixOperatorLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                     addIdentifierRef(op->getName()),
                                     addDeclRef(DC));
    return true;
  }
    
  case DeclKind::PostfixOperator: {
    auto op = cast<PostfixOperatorDecl>(D);

    const Decl *DC = getDeclForContext(op->getDeclContext());

    unsigned abbrCode = DeclTypeAbbrCodes[PostfixOperatorLayout::Code];
    PostfixOperatorLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                      addIdentifierRef(op->getName()),
                                      addDeclRef(DC));
    return true;
  }

  case DeclKind::TypeAlias: {
    auto typeAlias = cast<TypeAliasDecl>(D);
    assert(!typeAlias->isObjC() && "ObjC typealias is not meaningful");

    // FIXME: Handle attributes.
    // FIXME: Do typealiases have any interesting attributes? Resilience?
    if (!typeAlias->getAttrs().empty())
      return false;

    const Decl *DC = getDeclForContext(typeAlias->getDeclContext());

    Type underlying;
    if (typeAlias->hasUnderlyingType())
      underlying = typeAlias->getUnderlyingType();

    unsigned abbrCode = DeclTypeAbbrCodes[TypeAliasLayout::Code];
    TypeAliasLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                addIdentifierRef(typeAlias->getName()),
                                addDeclRef(DC),
                                addTypeRef(underlying),
                                typeAlias->isImplicit());

    writeConformances(typeAlias->getProtocols(), typeAlias->getConformances(),
                      typeAlias);
    return true;
  }

  case DeclKind::GenericTypeParam: {
    auto genericParam = cast<GenericTypeParamDecl>(D);
    assert(!genericParam->isImplicit() && "Implicit generic parameter?");

    // FIXME: Handle attributes.
    // FIXME: Do typealiases have any interesting attributes? Resilience?
    if (!genericParam->getAttrs().empty())
      return false;

    const Decl *DC = getDeclForContext(genericParam->getDeclContext());

    unsigned abbrCode = DeclTypeAbbrCodes[GenericTypeParamDeclLayout::Code];
    GenericTypeParamDeclLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                addIdentifierRef(genericParam->getName()),
                                addDeclRef(DC),
                                genericParam->getDepth(),
                                genericParam->getIndex(),
                                addTypeRef(genericParam->getSuperclass()),
                                addTypeRef(genericParam->getArchetype()));

    writeConformances(genericParam->getProtocols(),
                      genericParam->getConformances(),
                      genericParam);
    return true;
  }

  case DeclKind::AssociatedType: {
    auto assocType = cast<AssociatedTypeDecl>(D);

    // FIXME: Handle attributes.
    // FIXME: Do typealiases have any interesting attributes? Resilience?
    if (!assocType->getAttrs().empty())
      return false;

    const Decl *DC = getDeclForContext(assocType->getDeclContext());

    unsigned abbrCode = DeclTypeAbbrCodes[AssociatedTypeDeclLayout::Code];
    AssociatedTypeDeclLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                         addIdentifierRef(assocType->getName()),
                                         addDeclRef(DC),
                                         addTypeRef(assocType->getSuperclass()),
                                         addTypeRef(assocType->getArchetype()),
                                         assocType->isImplicit());

    writeConformances(assocType->getProtocols(),
                      assocType->getConformances(),
                      assocType);
    return true;
  }

  case DeclKind::Struct: {
    auto theStruct = cast<StructDecl>(D);
    
    // FIXME: Handle attributes.
    if (!theStruct->getAttrs().empty())
      return false;

    const Decl *DC = getDeclForContext(theStruct->getDeclContext());

    unsigned abbrCode = DeclTypeAbbrCodes[StructLayout::Code];
    StructLayout::emitRecord(Out, ScratchRecord, abbrCode,
                             addIdentifierRef(theStruct->getName()),
                             addDeclRef(DC),
                             theStruct->isImplicit());

    writeGenericParams(theStruct->getGenericParams());
    writeConformances(theStruct->getProtocols(), theStruct->getConformances(),
                      theStruct);
    writeMembers(theStruct->getMembers(), false);
    return true;
  }

  case DeclKind::Union: {
    auto theUnion = cast<UnionDecl>(D);

    // FIXME: Handle attributes.
    if (!theUnion->getAttrs().empty())
      return false;

    const Decl *DC = getDeclForContext(theUnion->getDeclContext());

    unsigned abbrCode = DeclTypeAbbrCodes[UnionLayout::Code];
    UnionLayout::emitRecord(Out, ScratchRecord, abbrCode,
                            addIdentifierRef(theUnion->getName()),
                            addDeclRef(DC),
                            theUnion->isImplicit());

    writeGenericParams(theUnion->getGenericParams());
    writeConformances(theUnion->getProtocols(), theUnion->getConformances(),
                      theUnion);
    writeMembers(theUnion->getMembers(), false);
    return true;
  }

  case DeclKind::Class: {
    auto theClass = cast<ClassDecl>(D);

    DeclAttributes remainingAttrs = theClass->getAttrs();
    remainingAttrs.ObjC = false;

    if (!remainingAttrs.empty())
      return false;

    const Decl *DC = getDeclForContext(theClass->getDeclContext());

    unsigned abbrCode = DeclTypeAbbrCodes[ClassLayout::Code];
    ClassLayout::emitRecord(Out, ScratchRecord, abbrCode,
                            addIdentifierRef(theClass->getName()),
                            addDeclRef(DC),
                            theClass->isImplicit(),
                            theClass->isObjC(),
                            addTypeRef(theClass->getSuperclass()));

    writeGenericParams(theClass->getGenericParams());
    writeConformances(theClass->getProtocols(), theClass->getConformances(),
                      theClass);
    writeMembers(theClass->getMembers(), true);
    return true;
  }


  case DeclKind::Protocol: {
    auto proto = cast<ProtocolDecl>(D);
    
    DeclAttributes remainingAttrs = proto->getAttrs();
    remainingAttrs.ClassProtocol = false;
    remainingAttrs.ObjC = false;

    if (!remainingAttrs.empty())
      return false;

    assert(!proto->getGenericParams() && "protocols can't be generic");
    const Decl *DC = getDeclForContext(proto->getDeclContext());

    SmallVector<DeclID, 4> protocols;
    for (auto proto : proto->getProtocols())
      protocols.push_back(addDeclRef(proto));

    unsigned abbrCode = DeclTypeAbbrCodes[ProtocolLayout::Code];
    ProtocolLayout::emitRecord(Out, ScratchRecord, abbrCode,
                               addIdentifierRef(proto->getName()),
                               addDeclRef(DC),
                               proto->isImplicit(),
                               proto->getAttrs().isClassProtocol(),
                               proto->isObjC(),
                               protocols);

    writeMembers(proto->getMembers(), true);
    return true;
  }

  case DeclKind::Var: {
    auto var = cast<VarDecl>(D);

    DeclAttributes remainingAttrs = var->getAttrs();
    // FIXME: We need some representation of these for source fidelity.
    remainingAttrs.ObjC = false;
    remainingAttrs.IBOutlet = false;

    if (!remainingAttrs.empty())
      return false;

    const Decl *DC = getDeclForContext(var->getDeclContext());
    Type type = var->hasType() ? var->getType() : nullptr;

    unsigned abbrCode = DeclTypeAbbrCodes[VarLayout::Code];
    VarLayout::emitRecord(Out, ScratchRecord, abbrCode,
                          addIdentifierRef(var->getName()),
                          addDeclRef(DC),
                          var->isImplicit(),
                          var->isObjC(),
                          var->getAttrs().isIBOutlet(),
                          addTypeRef(type),
                          addDeclRef(var->getGetter()),
                          addDeclRef(var->getSetter()),
                          addDeclRef(var->getOverriddenDecl()));

    return true;
  }

  case DeclKind::Func: {
    auto fn = cast<FuncDecl>(D);

    DeclAttributes remainingAttrs = fn->getAttrs();
    // FIXME: We need some representation of these for source fidelity.
    remainingAttrs.ExplicitPrefix = false;
    remainingAttrs.ExplicitPostfix = false;
    remainingAttrs.ExplicitInfix = false;
    remainingAttrs.Assignment = false;
    remainingAttrs.Conversion = false;
    remainingAttrs.AsmName = {};
    remainingAttrs.NoReturn = false;
    remainingAttrs.Thin = false;
    remainingAttrs.ObjC = false;
    remainingAttrs.IBAction = false;

    if (!remainingAttrs.empty())
      return false;

    const Decl *DC = getDeclForContext(fn->getDeclContext());

    unsigned abbrCode = DeclTypeAbbrCodes[FuncLayout::Code];
    FuncLayout::emitRecord(Out, ScratchRecord, abbrCode,
                           addIdentifierRef(fn->getName()),
                           addDeclRef(DC),
                           fn->isImplicit(),
                           fn->isStatic(),
                           fn->getAttrs().isAssignment() ||
                             fn->getAttrs().isConversion(),
                           fn->isObjC(),
                           fn->getAttrs().isIBAction(),
                           addTypeRef(fn->getType()),
                           addDeclRef(fn->getOperatorDecl()),
                           addDeclRef(fn->getOverriddenDecl()),
                           fn->getAttrs().AsmName);

    writeGenericParams(fn->getGenericParams());

    // Write both argument and body parameters. This is important for proper
    // error messages with selector-style declarations.
    for (auto pattern : fn->getBody()->getArgParamPatterns())
      writePattern(pattern);
    for (auto pattern : fn->getBody()->getBodyParamPatterns())
      writePattern(pattern);

    if (fn->getAttrs().isConversion())
      EagerDeserializationDecls.push_back(addDeclRef(DC));

    return true;
  }

  case DeclKind::UnionElement: {
    auto elem = cast<UnionElementDecl>(D);

    // FIXME: Handle attributes.
    if (!elem->getAttrs().empty())
      return false;

    const Decl *DC = getDeclForContext(elem->getDeclContext());

    unsigned abbrCode = DeclTypeAbbrCodes[UnionElementLayout::Code];
    UnionElementLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                   addIdentifierRef(elem->getName()),
                                   addDeclRef(DC),
                                   addTypeRef(elem->getArgumentType()),
                                   addTypeRef(elem->getResultType()),
                                   addTypeRef(elem->getType()),
                                   elem->isImplicit());
    return true;
  }

  case DeclKind::Subscript: {
    auto subscript = cast<SubscriptDecl>(D);

    DeclAttributes remainingAttrs = subscript->getAttrs();
    remainingAttrs.ObjC = false;

    if (!remainingAttrs.empty())
      return false;

    const Decl *DC = getDeclForContext(subscript->getDeclContext());

    unsigned abbrCode = DeclTypeAbbrCodes[SubscriptLayout::Code];
    SubscriptLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                addDeclRef(DC),
                                subscript->isImplicit(),
                                subscript->isObjC(),
                                addTypeRef(subscript->getType()),
                                addTypeRef(subscript->getElementType()),
                                addDeclRef(subscript->getGetter()),
                                addDeclRef(subscript->getSetter()),
                                addDeclRef(subscript->getOverriddenDecl()));

    writePattern(subscript->getIndices());

    return true;
  }


  case DeclKind::Constructor: {
    auto ctor = cast<ConstructorDecl>(D);

    DeclAttributes remainingAttrs = ctor->getAttrs();
    remainingAttrs.ObjC = false;

    if (!remainingAttrs.empty())
      return false;

    // FIXME: Handle allocating constructors.
    // FIXME: Does this ever occur in Swift modules? If it's only used by the
    // importer, perhaps we don't need to worry about it here.
    if (ctor->getAllocSelfExpr())
      return false;

    const Decl *DC = getDeclForContext(ctor->getDeclContext());
    auto implicitSelf = ctor->getImplicitSelfDecl();

    unsigned abbrCode = DeclTypeAbbrCodes[ConstructorLayout::Code];
    ConstructorLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                  addDeclRef(DC),
                                  ctor->isImplicit(),
                                  ctor->isObjC(),
                                  addTypeRef(ctor->getType()),
                                  addDeclRef(implicitSelf));

    writeGenericParams(ctor->getGenericParams());
    writePattern(ctor->getArguments());

    return true;
  }

  case DeclKind::Destructor: {
    auto dtor = cast<DestructorDecl>(D);

    DeclAttributes remainingAttrs = dtor->getAttrs();
    remainingAttrs.ObjC = false;

    if (!remainingAttrs.empty())
      return false;

    const Decl *DC = getDeclForContext(dtor->getDeclContext());
    auto implicitSelf = dtor->getImplicitSelfDecl();

    unsigned abbrCode = DeclTypeAbbrCodes[DestructorLayout::Code];
    DestructorLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                 addDeclRef(DC),
                                 dtor->isImplicit(),
                                 dtor->isObjC(),
                                 addTypeRef(dtor->getType()),
                                 addDeclRef(implicitSelf));
    
    return true;
  }
  }
}

/// Translate from the AST calling convention enum to the Serialization enum
/// values, which are guaranteed to be stable.
static uint8_t getRawStableCC(swift::AbstractCC cc) {
  switch (cc) {
#define CASE(THE_CC) \
  case swift::AbstractCC::THE_CC: \
    return serialization::AbstractCC::THE_CC;
  CASE(C)
  CASE(ObjCMethod)
  CASE(Freestanding)
  CASE(Method)
#undef CASE
  }
}

/// Translate from the AST ownership enum to the Serialization enum
/// values, which are guaranteed to be stable.
static uint8_t getRawStableOwnership(swift::Ownership ownership) {
  switch (ownership) {
  case swift::Ownership::Strong:
    return serialization::Ownership::Strong;
  case swift::Ownership::Weak:
    return serialization::Ownership::Weak;
  case swift::Ownership::Unowned:
    return serialization::Ownership::Unowned;
  }
  llvm_unreachable("bad ownership kind");
}

bool Serializer::writeType(Type ty) {
  using namespace decls_block;

  switch (ty.getPointer()->getKind()) {
  case TypeKind::Error:
    llvm_unreachable("should not serialize an error type");

  case TypeKind::BuiltinInteger:
  case TypeKind::BuiltinFloat:
  case TypeKind::BuiltinRawPointer:
  case TypeKind::BuiltinObjectPointer:
  case TypeKind::BuiltinObjCPointer:
  case TypeKind::BuiltinVector:
    llvm_unreachable("should always be accessed through an implicit typealias");

  case TypeKind::NameAlias: {
    auto nameAlias = cast<NameAliasType>(ty.getPointer());
    const TypeAliasDecl *typeAlias = nameAlias->getDecl();

    unsigned abbrCode = DeclTypeAbbrCodes[NameAliasTypeLayout::Code];
    NameAliasTypeLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                    addDeclRef(typeAlias));
    return true;
  }

  case TypeKind::Paren: {
    auto parenTy = cast<ParenType>(ty.getPointer());

    unsigned abbrCode = DeclTypeAbbrCodes[ParenTypeLayout::Code];
    ParenTypeLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                addTypeRef(parenTy->getUnderlyingType()));
    return true;
  }

  case TypeKind::Tuple: {
    auto tupleTy = cast<TupleType>(ty.getPointer());

    unsigned abbrCode = DeclTypeAbbrCodes[TupleTypeLayout::Code];
    TupleTypeLayout::emitRecord(Out, ScratchRecord, abbrCode);

    abbrCode = DeclTypeAbbrCodes[TupleTypeEltLayout::Code];
    for (auto &elt : tupleTy->getFields()) {
      uint8_t rawDefaultArg
        = getRawStableDefaultArgumentKind(elt.getDefaultArgKind());
      TupleTypeEltLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                     addIdentifierRef(elt.getName()),
                                     addTypeRef(elt.getType()),
                                     rawDefaultArg,
                                     elt.isVararg());
    }

    return true;
  }

  case TypeKind::Struct:
  case TypeKind::Union:
  case TypeKind::Class:
  case TypeKind::Protocol: {
    auto nominalTy = cast<NominalType>(ty.getPointer());

    unsigned abbrCode = DeclTypeAbbrCodes[NominalTypeLayout::Code];
    NominalTypeLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                  addDeclRef(nominalTy->getDecl()),
                                  addTypeRef(nominalTy->getParent()));
    return true;
  }

  case TypeKind::MetaType: {
    auto metatypeTy = cast<MetaTypeType>(ty.getPointer());

    unsigned abbrCode = DeclTypeAbbrCodes[MetaTypeTypeLayout::Code];
    MetaTypeTypeLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                   addTypeRef(metatypeTy->getInstanceType()));
    return true;
  }

  case TypeKind::Module:
    llvm_unreachable("modules are currently not first-class values");

  case TypeKind::Archetype: {
    auto archetypeTy = cast<ArchetypeType>(ty.getPointer());

    TypeID indexOrParentID;
    if (archetypeTy->isPrimary())
      indexOrParentID = archetypeTy->getPrimaryIndex();
    else
      indexOrParentID = addTypeRef(archetypeTy->getParent());

    SmallVector<DeclID, 4> conformances;
    for (auto proto : archetypeTy->getConformsTo())
      conformances.push_back(addDeclRef(proto));

    unsigned abbrCode = DeclTypeAbbrCodes[ArchetypeTypeLayout::Code];
    ArchetypeTypeLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                    addIdentifierRef(archetypeTy->getName()),
                                    archetypeTy->isPrimary(),
                                    indexOrParentID,
                                    addDeclRef(archetypeTy->getAssocType()),
                                    addTypeRef(archetypeTy->getSuperclass()),
                                    conformances);

    SmallVector<IdentifierID, 4> nestedTypeNames;
    SmallVector<TypeID, 4> nestedTypes;
    for (auto next : archetypeTy->getNestedTypes()) {
      nestedTypeNames.push_back(addIdentifierRef(next.first));
      nestedTypes.push_back(addTypeRef(next.second));
    }

    abbrCode = DeclTypeAbbrCodes[ArchetypeNestedTypeNamesLayout::Code];
    ArchetypeNestedTypeNamesLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                               nestedTypeNames);

    abbrCode = DeclTypeAbbrCodes[ArchetypeNestedTypesLayout::Code];
    ArchetypeNestedTypesLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                           nestedTypes);

    return true;
  }

  case TypeKind::GenericTypeParam: {
    auto genericParam = cast<GenericTypeParamType>(ty.getPointer());
    unsigned abbrCode = DeclTypeAbbrCodes[GenericTypeParamTypeLayout::Code];
    GenericTypeParamTypeLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                           addDeclRef(genericParam->getDecl()));
    return true;
  }

  case TypeKind::AssociatedType: {
    auto assocType = cast<AssociatedTypeType>(ty.getPointer());
    unsigned abbrCode = DeclTypeAbbrCodes[AssociatedTypeTypeLayout::Code];
    AssociatedTypeTypeLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                         addDeclRef(assocType->getDecl()));
    return true;
  }

  case TypeKind::Substituted: {
    auto subTy = cast<SubstitutedType>(ty.getPointer());

    unsigned abbrCode = DeclTypeAbbrCodes[SubstitutedTypeLayout::Code];
    SubstitutedTypeLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                      addTypeRef(subTy->getOriginal()),
                                      addTypeRef(subTy->getReplacementType()));
    return true;
  }

  case TypeKind::DependentMember: {
    auto dependent = cast<DependentMemberType>(ty.getPointer());

    unsigned abbrCode = DeclTypeAbbrCodes[DependentMemberTypeLayout::Code];
    DependentMemberTypeLayout::emitRecord(
      Out, ScratchRecord, abbrCode,
      addTypeRef(dependent->getBase()),
      addIdentifierRef(dependent->getName()));
    return true;
  }

  case TypeKind::Function: {
    auto fnTy = cast<FunctionType>(ty.getPointer());

    unsigned abbrCode = DeclTypeAbbrCodes[FunctionTypeLayout::Code];
    FunctionTypeLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                   addTypeRef(fnTy->getInput()),
                                   addTypeRef(fnTy->getResult()),
                                   getRawStableCC(fnTy->getAbstractCC()),
                                   fnTy->isAutoClosure(),
                                   fnTy->isThin(),
                                   fnTy->isNoReturn(),
                                   fnTy->isBlock());

    return true;
  }

  case TypeKind::PolymorphicFunction: {
    auto fnTy = cast<PolymorphicFunctionType>(ty.getPointer());
    const Decl *genericContext = getGenericContext(&fnTy->getGenericParams());
    auto callingConvention = fnTy->getAbstractCC();

    unsigned abbrCode = DeclTypeAbbrCodes[PolymorphicFunctionTypeLayout::Code];
    PolymorphicFunctionTypeLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                              addTypeRef(fnTy->getInput()),
                                              addTypeRef(fnTy->getResult()),
                                              addDeclRef(genericContext),
                                              getRawStableCC(callingConvention),
                                              fnTy->isThin(),
                                              fnTy->isNoReturn());

    return true;
  }

  case TypeKind::Array: {
    auto arrayTy = cast<ArrayType>(ty.getPointer());

    Type base = arrayTy->getBaseType();

    unsigned abbrCode = DeclTypeAbbrCodes[ArrayTypeLayout::Code];
    ArrayTypeLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                     addTypeRef(base), arrayTy->getSize());
    return true;
  }

  case TypeKind::ArraySlice: {
    auto sliceTy = cast<ArraySliceType>(ty.getPointer());

    Type base = sliceTy->getBaseType();

    unsigned abbrCode = DeclTypeAbbrCodes[ArraySliceTypeLayout::Code];
    ArraySliceTypeLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                     addTypeRef(base));
    return true;
  }

  case TypeKind::Optional: {
    auto sliceTy = cast<OptionalType>(ty.getPointer());

    Type base = sliceTy->getBaseType();

    unsigned abbrCode = DeclTypeAbbrCodes[OptionalTypeLayout::Code];
    OptionalTypeLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                   addTypeRef(base));
    return true;
  }

  case TypeKind::ProtocolComposition: {
    auto composition = cast<ProtocolCompositionType>(ty.getPointer());

    SmallVector<TypeID, 4> protocols;
    for (auto proto : composition->getProtocols())
      protocols.push_back(addTypeRef(proto));

    unsigned abbrCode = DeclTypeAbbrCodes[ProtocolCompositionTypeLayout::Code];
    ProtocolCompositionTypeLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                              protocols);

    return true;
  }

  case TypeKind::LValue: {
    auto lValueTy = cast<LValueType>(ty.getPointer());

    unsigned abbrCode = DeclTypeAbbrCodes[LValueTypeLayout::Code];
    LValueTypeLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                 addTypeRef(lValueTy->getObjectType()),
                                 lValueTy->getQualifiers().isImplicit(),
                                 !lValueTy->getQualifiers().isSettable());
    return true;
  }

  case TypeKind::UnownedStorage:
  case TypeKind::WeakStorage: {
    auto refTy = cast<ReferenceStorageType>(ty.getPointer());

    unsigned abbrCode = DeclTypeAbbrCodes[ReferenceStorageTypeLayout::Code];
    auto stableOwnership = getRawStableOwnership(refTy->getOwnership());
    ReferenceStorageTypeLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                           stableOwnership,
                                  addTypeRef(refTy->getReferentType()));
    return true;
  }

  case TypeKind::UnboundGeneric: {
    auto generic = cast<UnboundGenericType>(ty.getPointer());

    unsigned abbrCode = DeclTypeAbbrCodes[UnboundGenericTypeLayout::Code];
    UnboundGenericTypeLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                         addDeclRef(generic->getDecl()),
                                         addTypeRef(generic->getParent()));
    return true;
  }

  case TypeKind::BoundGenericClass:
  case TypeKind::BoundGenericUnion:
  case TypeKind::BoundGenericStruct: {
    auto generic = cast<BoundGenericType>(ty.getPointer());

    SmallVector<TypeID, 8> genericArgIDs;
    for (auto next : generic->getGenericArgs())
      genericArgIDs.push_back(addTypeRef(next));

    // Get the substitutions.
    ArrayRef<Substitution> substitutions;
    if (generic->hasSubstitutions())
      substitutions = generic->getSubstitutions();

    unsigned abbrCode = DeclTypeAbbrCodes[BoundGenericTypeLayout::Code];
    BoundGenericTypeLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                       addDeclRef(generic->getDecl()),
                                       addTypeRef(generic->getParent()),
                                       substitutions.size(),
                                       genericArgIDs);

    writeSubstitutions(substitutions);

    return true;
  }

  case TypeKind::TypeVariable:
    llvm_unreachable("type variables should not escape the type checker");
  }
}

void Serializer::writeAllDeclsAndTypes() {
  BCBlockRAII restoreBlock(Out, DECLS_AND_TYPES_BLOCK_ID, 8);

  {
    using namespace decls_block;
    registerDeclTypeAbbr<NameAliasTypeLayout>();
    registerDeclTypeAbbr<GenericTypeParamDeclLayout>();
    registerDeclTypeAbbr<AssociatedTypeDeclLayout>();
    registerDeclTypeAbbr<NominalTypeLayout>();
    registerDeclTypeAbbr<ParenTypeLayout>();
    registerDeclTypeAbbr<TupleTypeLayout>();
    registerDeclTypeAbbr<TupleTypeEltLayout>();
    registerDeclTypeAbbr<FunctionTypeLayout>();
    registerDeclTypeAbbr<MetaTypeTypeLayout>();
    registerDeclTypeAbbr<LValueTypeLayout>();
    registerDeclTypeAbbr<ArchetypeTypeLayout>();
    registerDeclTypeAbbr<ArchetypeNestedTypeNamesLayout>();
    registerDeclTypeAbbr<ArchetypeNestedTypesLayout>();
    registerDeclTypeAbbr<ProtocolCompositionTypeLayout>();
    registerDeclTypeAbbr<SubstitutedTypeLayout>();
    registerDeclTypeAbbr<BoundGenericTypeLayout>();
    registerDeclTypeAbbr<BoundGenericSubstitutionLayout>();
    registerDeclTypeAbbr<PolymorphicFunctionTypeLayout>();
    registerDeclTypeAbbr<ArraySliceTypeLayout>();
    registerDeclTypeAbbr<ArrayTypeLayout>();
    registerDeclTypeAbbr<UnboundGenericTypeLayout>();
    registerDeclTypeAbbr<OptionalTypeLayout>();

    registerDeclTypeAbbr<TypeAliasLayout>();
    registerDeclTypeAbbr<GenericTypeParamTypeLayout>();
    registerDeclTypeAbbr<AssociatedTypeTypeLayout>();
    registerDeclTypeAbbr<DependentMemberTypeLayout>();
    registerDeclTypeAbbr<StructLayout>();
    registerDeclTypeAbbr<ConstructorLayout>();
    registerDeclTypeAbbr<VarLayout>();
    registerDeclTypeAbbr<FuncLayout>();
    registerDeclTypeAbbr<PatternBindingLayout>();
    registerDeclTypeAbbr<ProtocolLayout>();
    registerDeclTypeAbbr<PrefixOperatorLayout>();
    registerDeclTypeAbbr<PostfixOperatorLayout>();
    registerDeclTypeAbbr<InfixOperatorLayout>();
    registerDeclTypeAbbr<ClassLayout>();
    registerDeclTypeAbbr<UnionLayout>();
    registerDeclTypeAbbr<UnionElementLayout>();
    registerDeclTypeAbbr<SubscriptLayout>();
    registerDeclTypeAbbr<ExtensionLayout>();
    registerDeclTypeAbbr<DestructorLayout>();

    registerDeclTypeAbbr<ParenPatternLayout>();
    registerDeclTypeAbbr<TuplePatternLayout>();
    registerDeclTypeAbbr<TuplePatternEltLayout>();
    registerDeclTypeAbbr<NamedPatternLayout>();
    registerDeclTypeAbbr<AnyPatternLayout>();
    registerDeclTypeAbbr<TypedPatternLayout>();

    registerDeclTypeAbbr<GenericParamListLayout>();
    registerDeclTypeAbbr<GenericParamLayout>();
    registerDeclTypeAbbr<GenericRequirementLayout>();

    registerDeclTypeAbbr<NoConformanceLayout>();
    registerDeclTypeAbbr<NormalProtocolConformanceLayout>();
    registerDeclTypeAbbr<SpecializedProtocolConformanceLayout>();
    registerDeclTypeAbbr<InheritedProtocolConformanceLayout>();
    registerDeclTypeAbbr<DeclContextLayout>();
    registerDeclTypeAbbr<XRefLayout>();
  }

  while (!DeclsAndTypesToWrite.empty()) {
    DeclTypeUnion next = DeclsAndTypesToWrite.front();
    DeclsAndTypesToWrite.pop();

    DeclID id = DeclIDs[next];
    assert(id != 0 && "decl or type not referenced properly");
    (void)id;

    auto &offsets = next.isDecl() ? DeclOffsets : TypeOffsets;
    assert((id - 1) == offsets.size());
    
    offsets.push_back(Out.GetCurrentBitNo());

    // If we can't handle a decl or type, mark the module as incomplete.
    // FIXME: Eventually we should assert this.
    bool success = next.isDecl() ? writeDecl(next.getDecl())
                                 : writeType(next.getType());
    if (!success)
      ShouldFallBackToTranslationUnit = true;
  }
}

void Serializer::writeAllIdentifiers() {
  BCBlockRAII restoreBlock(Out, IDENTIFIER_DATA_BLOCK_ID, 3);
  identifier_block::IdentifierDataLayout IdentifierData(Out);

  llvm::SmallString<4096> stringData;

  // Make sure no identifier has an offset of 0.
  stringData.push_back('\0');

  for (Identifier ident : IdentifiersToWrite) {
    IdentifierOffsets.push_back(stringData.size());
    stringData.append(ident.get());
    stringData.push_back('\0');
  }

  IdentifierData.emit(ScratchRecord, stringData.str());
}

void Serializer::writeOffsets(const index_block::OffsetsLayout &Offsets,
                              const std::vector<BitOffset> &values) {
  Offsets.emit(ScratchRecord, getOffsetRecordCode(values), values);
}

/// Writes an in-memory decl table to an on-disk representation, using the
/// given layout.
static void writeDeclTable(const index_block::DeclListLayout &DeclList,
                           index_block::RecordKind kind,
                           const Serializer::DeclTable &table) {
  if (table.empty())
    return;

  SmallVector<uint64_t, 8> scratch;
  llvm::SmallString<4096> hashTableBlob;
  uint32_t tableOffset;
  {
    OnDiskChainedHashTableGenerator<DeclTableInfo> generator;
    for (auto &entry : table)
      generator.insert(entry.first, entry.second);

    llvm::raw_svector_ostream blobStream(hashTableBlob);
    // Make sure that no bucket is at offset 0
    clang::io::Emit32(blobStream, 0);
    tableOffset = generator.Emit(blobStream);
  }

  DeclList.emit(scratch, kind, tableOffset, hashTableBlob);
}

/// Translate from the AST known protocol enum to the Serialization enum
/// values, which are guaranteed to be stable.
static uint8_t getRawStableKnownProtocolKind(KnownProtocolKind kind) {
  switch (kind) {
#define PROTOCOL(Id) \
  case KnownProtocolKind::Id: return index_block::Id;
#include "swift/AST/KnownProtocols.def"
  }
}

/// Writes a list of decls known to conform to the given compiler-known
/// protocol.
static void
writeKnownProtocolList(const index_block::KnownProtocolLayout &AdopterList,
                       KnownProtocolKind kind, ArrayRef<DeclID> adopters) {
  if (adopters.empty())
    return;
                       
  SmallVector<uint32_t, 32> scratch;
  AdopterList.emit(scratch, getRawStableKnownProtocolKind(kind), adopters);
}

void Serializer::writeTranslationUnit(const TranslationUnit *TU) {
  assert(!this->TU && "already serializing a translation unit");
  this->TU = TU;

  DeclTable topLevelDecls, extensionDecls, operatorDecls;
  for (auto D : TU->Decls) {
    if (isa<ImportDecl>(D))
      continue;
    else if (auto VD = dyn_cast<ValueDecl>(D)) {
      if (VD->getName().empty())
        continue;
      topLevelDecls[VD->getName()]
        .push_back({ getKindForTable(D), addDeclRef(D) });

    } else if (auto ED = dyn_cast<ExtensionDecl>(D)) {
      Type extendedTy = ED->getExtendedType();
      const NominalTypeDecl *extendedNominal = extendedTy->getAnyNominal();
      extensionDecls[extendedNominal->getName()]
        .push_back({ getKindForTable(extendedNominal), addDeclRef(D) });

    } else if (auto OD = dyn_cast<OperatorDecl>(D)) {
      operatorDecls[OD->getName()]
        .push_back({ getStableFixity(OD->getKind()), addDeclRef(D) });
    }
  }

  writeAllDeclsAndTypes();
  writeAllIdentifiers();

  //

  {
    BCBlockRAII restoreBlock(Out, INDEX_BLOCK_ID, 3);

    index_block::OffsetsLayout Offsets(Out);
    writeOffsets(Offsets, DeclOffsets);
    writeOffsets(Offsets, TypeOffsets);
    writeOffsets(Offsets, IdentifierOffsets);

    index_block::DeclListLayout DeclList(Out);
    writeDeclTable(DeclList, index_block::TOP_LEVEL_DECLS, topLevelDecls);
    writeDeclTable(DeclList, index_block::OPERATORS, operatorDecls);
    writeDeclTable(DeclList, index_block::EXTENSIONS, extensionDecls);
    writeDeclTable(DeclList, index_block::CLASS_MEMBERS, ClassMembersByName);

    {
      BCBlockRAII subBlock(Out, KNOWN_PROTOCOL_BLOCK_ID, 3);
      index_block::KnownProtocolLayout AdopterList(Out);

      for (unsigned i = 0; i < NumKnownProtocols; ++i) {
        writeKnownProtocolList(AdopterList, static_cast<KnownProtocolKind>(i),
                               KnownProtocolAdopters[i]);
      }
      AdopterList.emit(ScratchRecord, index_block::FORCE_DESERIALIZATION,
                       EagerDeserializationDecls);
    }
  }

#ifndef NDEBUG
  this->TU = nullptr;
#endif
}

void Serializer::writeToStream(raw_ostream &os, const TranslationUnit *TU,
                               FileBufferIDs inputFiles,
                               StringRef moduleLinkName) {
  // Write the signature through the BitstreamWriter for alignment purposes.
  for (unsigned char byte : SIGNATURE)
    Out.Emit(byte, 8);

  writeHeader();
  writeInputFiles(TU, inputFiles, moduleLinkName);
  writeTranslationUnit(TU);

  if (ShouldFallBackToTranslationUnit)
    BCBlockRAII(Out, FALL_BACK_TO_TRANSLATION_UNIT_ID, 2);

  os.write(Buffer.data(), Buffer.size());
  os.flush();
  Buffer.clear();
}

void swift::serialize(const TranslationUnit *TU, const char *outputPath,
                      FileBufferIDs inputFiles, StringRef moduleLinkName) {
  std::string errorInfo;
  llvm::raw_fd_ostream out(outputPath, errorInfo,
                           llvm::sys::fs::F_Binary);

  if (out.has_error() || !errorInfo.empty()) {
    TU->Ctx.Diags.diagnose(SourceLoc(), diag::error_opening_output, outputPath,
                           errorInfo);
    out.clear_error();
    return;
  }

  serializeToStream(TU, out, inputFiles, moduleLinkName);
}

void swift::serializeToStream(const TranslationUnit *TU, raw_ostream &out,
                              FileBufferIDs inputFiles,
                              StringRef moduleLinkName) {
  Serializer S;
  S.writeToStream(out, TU, inputFiles, moduleLinkName);
}
