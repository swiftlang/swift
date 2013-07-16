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
#include "swift/Basic/STLExtras.h"
#include "swift/Serialization/BCRecordLayout.h"
#include "llvm/Bitcode/BitstreamWriter.h"
#include "llvm/Config/config.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/raw_ostream.h"
#include <array>
#include <queue>

using namespace swift;
using namespace swift::serialization;

namespace {
  typedef ArrayRef<unsigned> FileBufferIDs;

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
    void writeInputFiles(const TranslationUnit *TU, FileBufferIDs inputFiles);

    /// Writes the given pattern, recursively.
    void writePattern(const Pattern *pattern);

    /// Writes a generic parameter list.
    bool writeGenericParams(const GenericParamList *genericParams);

    /// Writes a list of protocol conformances.
    void writeConformances(ArrayRef<ProtocolDecl *> protocols,
                           ArrayRef<ProtocolConformance *> conformances);

    /// Writes an array of members for a decl context.
    void writeMembers(ArrayRef<Decl *> members);

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
                       FileBufferIDs inputFiles);
  };
} // end anonymous namespace

namespace llvm {
  template<> struct DenseMapInfo<Serializer::DeclTypeUnion> {
    using DeclTypeUnion = Serializer::DeclTypeUnion;
    static inline DeclTypeUnion getEmptyKey() { return nullptr; }
    static inline DeclTypeUnion getTombstoneKey() { return Type(); }
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
  case DeclKind::OneOf:
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

  BLOCK(DECLS_AND_TYPES_BLOCK);
  RECORD(decls_block, NAME_ALIAS_TYPE);
  RECORD(decls_block, NOMINAL_TYPE);
  RECORD(decls_block, PAREN_TYPE);
  RECORD(decls_block, TUPLE_TYPE);
  RECORD(decls_block, TUPLE_TYPE_ELT);
  RECORD(decls_block, IDENTIFIER_TYPE);
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

  RECORD(decls_block, TYPE_ALIAS_DECL);
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
  RECORD(decls_block, ONEOF_DECL);
  RECORD(decls_block, ONEOF_ELEMENT_DECL);
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
  RECORD(decls_block, PROTOCOL_CONFORMANCE);
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

void Serializer::writeInputFiles(const TranslationUnit *TU,
                                 FileBufferIDs inputFiles) {
  BCBlockRAII restoreBlock(Out, INPUT_BLOCK_ID, 3);
  input_block::SourceFileLayout SourceFile(Out);
  input_block::ImportedModuleLayout ImportedModule(Out);

  auto &sourceMgr = TU->Ctx.SourceMgr;
  for (auto bufferID : inputFiles) {
    // FIXME: We could really use a real FileManager here.
    auto buffer = sourceMgr.getMemoryBuffer(bufferID);
    llvm::SmallString<128> path(buffer->getBufferIdentifier());

    llvm::error_code err;
    err = llvm::sys::fs::make_absolute(path);
    if (err)
      continue;
    
    SourceFile.emit(ScratchRecord, path);
  }

  SmallVector<StringRef, 16> imported;
  for (auto &moduleEntry : TU->Ctx.LoadedModules) {
    if (moduleEntry.second == TU)
      continue;
    // FIXME: Submodules? Packages?
    imported.push_back(moduleEntry.second->Name.str());
  }
  // Arbitrarily sort by name.
  // FIXME: It would be more efficient to linearize the dependency graph, but
  // that's more difficult, especially with Clang modules in the mix. This is
  // at least deterministic.
  std::sort(imported.begin(), imported.end());
  for (auto name : imported)
    ImportedModule.emit(ScratchRecord, name);
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
    ParenPatternLayout::emitRecord(Out, ScratchRecord, abbrCode);
    writePattern(cast<ParenPattern>(pattern)->getSubPattern());
    break;
  }
  case PatternKind::Tuple: {
    auto tuple = cast<TuplePattern>(pattern);

    unsigned abbrCode = DeclTypeAbbrCodes[TuplePatternLayout::Code];
    TuplePatternLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                   addTypeRef(tuple->getType()),
                                   tuple->getNumFields(), tuple->hasVararg());

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
                                   addDeclRef(named->getDecl()));
    break;
  }
  case PatternKind::Any: {
    unsigned abbrCode = DeclTypeAbbrCodes[AnyPatternLayout::Code];
    AnyPatternLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                 addTypeRef(pattern->getType()));
    break;
  }
  case PatternKind::Typed: {
    auto typed = cast<TypedPattern>(pattern);

    unsigned abbrCode = DeclTypeAbbrCodes[TypedPatternLayout::Code];
    TypedPatternLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                   addTypeRef(typed->getType()));
    writePattern(typed->getSubPattern());
    break;
  }
  case PatternKind::Isa: {
    auto isa = cast<IsaPattern>(pattern);
    
    unsigned abbrCode = DeclTypeAbbrCodes[IsaPatternLayout::Code];
    IsaPatternLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                 addTypeRef(isa->getCastTypeLoc().getType()));
    break;
  }
  case PatternKind::NominalType: {
    auto nom = cast<NominalTypePattern>(pattern);

    unsigned abbrCode = DeclTypeAbbrCodes[NominalTypePatternLayout::Code];
    NominalTypePatternLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                 addTypeRef(nom->getCastTypeLoc().getType()));
    writePattern(nom->getSubPattern());
    break;
  }
  case PatternKind::OneOfElement:
  case PatternKind::Expr:
    llvm_unreachable("FIXME: not implemented");

  case PatternKind::Var: {
    auto var = cast<VarPattern>(pattern);
    
    unsigned abbrCode = DeclTypeAbbrCodes[VarPatternLayout::Code];
    VarPatternLayout::emitRecord(Out, ScratchRecord, abbrCode);
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

void
Serializer::writeConformances(ArrayRef<ProtocolDecl *> protocols,
                              ArrayRef<ProtocolConformance *> conformances) {
  using namespace decls_block;

  for_each(protocols, conformances,
           [&](const ProtocolDecl *proto, const ProtocolConformance *conf) {
    if (!conf) {
      unsigned abbrCode = DeclTypeAbbrCodes[NoConformanceLayout::Code];
      NoConformanceLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                      addDeclRef(proto));
      return;
    }

    SmallVector<DeclID, 16> data;
    for (auto valueMapping : conf->Mapping) {
      data.push_back(addDeclRef(valueMapping.first));
      data.push_back(addDeclRef(valueMapping.second));
    }
    for (auto typeMapping : conf->TypeMapping) {
      data.push_back(addTypeRef(typeMapping.first));
      data.push_back(addTypeRef(typeMapping.second));
    }
    for (auto defaulted : conf->DefaultedDefinitions) {
      data.push_back(addDeclRef(defaulted));
    }

    unsigned abbrCode = DeclTypeAbbrCodes[ProtocolConformanceLayout::Code];
    ProtocolConformanceLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                          addDeclRef(proto),
                                          conf->Mapping.size(),
                                          conf->TypeMapping.size(),
                                          conf->InheritedMapping.size(),
                                          conf->DefaultedDefinitions.size(),
                                          data);

    // FIXME: Unfortunate to have to copy these.
    SmallVector<ProtocolDecl *, 8> inheritedProtos;
    SmallVector<ProtocolConformance *, 8> inheritedConformance;
    for (auto inheritedMapping : conf->InheritedMapping) {
      inheritedProtos.push_back(inheritedMapping.first);
      inheritedConformance.push_back(inheritedMapping.second);
    }
    writeConformances(inheritedProtos, inheritedConformance);
  });
}


void Serializer::writeMembers(ArrayRef<Decl*> members) {
  using namespace decls_block;
  
  unsigned abbrCode = DeclTypeAbbrCodes[DeclContextLayout::Code];
  SmallVector<DeclID, 16> memberIDs;
  for (auto member : members)
    memberIDs.push_back(addDeclRef(member));
  DeclContextLayout::emitRecord(Out, ScratchRecord, abbrCode, memberIDs);
}

bool Serializer::writeCrossReference(const Decl *D) {
  using namespace decls_block;

  SmallVector<IdentifierID, 4> accessPath;
  XRefKind kind;
  TypeID typeID;

  if (auto value = dyn_cast<ValueDecl>(D)) {
    kind = XRefKind::SwiftValue;
    accessPath.push_back(addIdentifierRef(value->getName()));

    // Make sure we don't create a self-referential type.
    Type ty = value->getType();
    if (ty->is<MetaTypeType>())
      ty = nullptr;
    typeID = addTypeRef(ty);

  } else if (auto op = dyn_cast<OperatorDecl>(D)) {
    kind = XRefKind::SwiftOperator;
    accessPath.push_back(addIdentifierRef(op->getName()));

    switch (op->getKind()) {
      case DeclKind::InfixOperator:
        typeID = OperatorKind::Infix;
        break;
      case DeclKind::PrefixOperator:
        typeID = OperatorKind::Prefix;
        break;
      case DeclKind::PostfixOperator:
        typeID = OperatorKind::Postfix;
        break;
      default:
        llvm_unreachable("unknown operator kind");
    }
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
  if (extension) {
    assert(kind == XRefKind::SwiftValue);
    kind = XRefKind::SwiftExtensionValue;
    accessPath.push_back(addModuleRef(extension->getModuleContext()));
  }

  // Store the access path in forward order.
  std::reverse(accessPath.begin(), accessPath.end());

  unsigned abbrCode = DeclTypeAbbrCodes[XRefLayout::Code];
  XRefLayout::emitRecord(Out, ScratchRecord, abbrCode,
                         kind, typeID, accessPath);

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
  if (D->hasClangNode())
    return false;

  Module *M = D->getModuleContext();
  if (M != TU)
    return writeCrossReference(D);

  switch (D->getKind()) {
  case DeclKind::Import:
    // FIXME: Do imported module names appear in the DeclContext of the
    // serialized module?
    return true;

  case DeclKind::Extension: {
    auto extension = cast<ExtensionDecl>(D);

    const Decl *DC = getDeclForContext(extension->getDeclContext());

    SmallVector<TypeID, 4> inherited;
    for (auto parent : extension->getInherited())
      inherited.push_back(addTypeRef(parent.getType()));

    unsigned abbrCode = DeclTypeAbbrCodes[ExtensionLayout::Code];
    ExtensionLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                addTypeRef(extension->getExtendedType()),
                                addDeclRef(DC),
                                extension->isImplicit(),
                                inherited);

    writeConformances(extension->getProtocols(), extension->getConformances());
    writeMembers(extension->getMembers());
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

    SmallVector<TypeID, 4> inherited;
    for (auto parent : typeAlias->getInherited())
      inherited.push_back(addTypeRef(parent.getType()));

    unsigned abbrCode = DeclTypeAbbrCodes[TypeAliasLayout::Code];
    TypeAliasLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                addIdentifierRef(typeAlias->getName()),
                                addDeclRef(DC),
                                addTypeRef(underlying),
                                typeAlias->isGenericParameter(),
                                typeAlias->isImplicit(),
                                inherited);

    writeConformances(typeAlias->getProtocols(), typeAlias->getConformances());
    return true;
  }

  case DeclKind::Struct: {
    auto theStruct = cast<StructDecl>(D);
    
    // FIXME: Handle attributes.
    if (!theStruct->getAttrs().empty())
      return false;

    const Decl *DC = getDeclForContext(theStruct->getDeclContext());

    SmallVector<TypeID, 4> inherited;
    for (auto parent : theStruct->getInherited())
      inherited.push_back(addTypeRef(parent.getType()));

    unsigned abbrCode = DeclTypeAbbrCodes[StructLayout::Code];
    StructLayout::emitRecord(Out, ScratchRecord, abbrCode,
                             addIdentifierRef(theStruct->getName()),
                             addDeclRef(DC),
                             theStruct->isImplicit(),
                             inherited);

    writeGenericParams(theStruct->getGenericParams());
    writeConformances(theStruct->getProtocols(), theStruct->getConformances());
    writeMembers(theStruct->getMembers());
    return true;
  }

  case DeclKind::OneOf: {
    auto oneOf = cast<OneOfDecl>(D);

    // FIXME: Handle attributes.
    if (!oneOf->getAttrs().empty())
      return false;

    const Decl *DC = getDeclForContext(oneOf->getDeclContext());

    SmallVector<TypeID, 4> inherited;
    for (auto parent : oneOf->getInherited())
      inherited.push_back(addTypeRef(parent.getType()));

    unsigned abbrCode = DeclTypeAbbrCodes[OneOfLayout::Code];
    OneOfLayout::emitRecord(Out, ScratchRecord, abbrCode,
                            addIdentifierRef(oneOf->getName()),
                            addDeclRef(DC),
                            oneOf->isImplicit(),
                            inherited);

    writeGenericParams(oneOf->getGenericParams());
    writeConformances(oneOf->getProtocols(), oneOf->getConformances());
    writeMembers(oneOf->getMembers());
    return true;
  }

  case DeclKind::Class: {
    auto theClass = cast<ClassDecl>(D);

    // FIXME: Handle attributes.
    if (!theClass->getAttrs().empty())
      return false;

    const Decl *DC = getDeclForContext(theClass->getDeclContext());

    SmallVector<TypeID, 4> inherited;
    for (auto parent : theClass->getInherited())
      inherited.push_back(addTypeRef(parent.getType()));

    unsigned abbrCode = DeclTypeAbbrCodes[ClassLayout::Code];
    ClassLayout::emitRecord(Out, ScratchRecord, abbrCode,
                            addIdentifierRef(theClass->getName()),
                            addDeclRef(DC),
                            theClass->isImplicit(),
                            inherited);

    writeGenericParams(theClass->getGenericParams());
    writeConformances(theClass->getProtocols(), theClass->getConformances());
    writeMembers(theClass->getMembers());
    return true;
  }


  case DeclKind::Protocol: {
    auto proto = cast<ProtocolDecl>(D);
    
    // FIXME: Handle attributes.
    if (!proto->getAttrs().empty())
      return false;

    assert(!proto->getGenericParams() && "protocols can't be generic");
    const Decl *DC = getDeclForContext(proto->getDeclContext());

    SmallVector<TypeID, 4> inherited;
    for (auto parent : proto->getInherited())
      inherited.push_back(addTypeRef(parent.getType()));

    unsigned abbrCode = DeclTypeAbbrCodes[ProtocolLayout::Code];
    ProtocolLayout::emitRecord(Out, ScratchRecord, abbrCode,
                               addIdentifierRef(proto->getName()),
                               addDeclRef(DC),
                               proto->isImplicit(),
                               inherited);

    writeMembers(proto->getMembers());
    return true;
  }

  case DeclKind::Var: {
    auto var = cast<VarDecl>(D);

    // FIXME: Handle attributes.
    if (!var->getAttrs().empty())
      return false;

    const Decl *DC = getDeclForContext(var->getDeclContext());
    Type type = var->hasType() ? var->getType() : nullptr;

    unsigned abbrCode = DeclTypeAbbrCodes[VarLayout::Code];
    VarLayout::emitRecord(Out, ScratchRecord, abbrCode,
                          addIdentifierRef(var->getName()),
                          addDeclRef(DC),
                          var->isImplicit(),
                          addTypeRef(type),
                          addDeclRef(var->getGetter()),
                          addDeclRef(var->getSetter()),
                          addDeclRef(var->getOverriddenDecl()));

    return true;
  }

  case DeclKind::Func: {
    auto fn = cast<FuncDecl>(D);

    DeclAttributes attrs = fn->getAttrs();
    // FIXME: We need some representation of these for source fidelity.
    attrs.ExplicitPrefix = false;
    attrs.ExplicitPostfix = false;
    attrs.ExplicitInfix = false;
    attrs.Assignment = false;
    attrs.Conversion = false;
    attrs.AsmName = {};

    // FIXME: Handle other attributes.
    if (!attrs.empty())
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
                           addTypeRef(fn->getType()),
                           addDeclRef(fn->getOperatorDecl()),
                           addDeclRef(fn->getOverriddenDecl()),
                           attrs.AsmName);

    writeGenericParams(fn->getGenericParams());

    // Write both argument and body parameters. This is important for proper
    // error messages with selector-style declarations.
    for (auto pattern : fn->getBody()->getArgParamPatterns())
      writePattern(pattern);
    for (auto pattern : fn->getBody()->getBodyParamPatterns())
      writePattern(pattern);

    return true;
  }

  case DeclKind::OneOfElement: {
    auto elem = cast<OneOfElementDecl>(D);

    // FIXME: Handle attributes.
    if (!elem->getAttrs().empty())
      return false;

    const Decl *DC = getDeclForContext(elem->getDeclContext());

    unsigned abbrCode = DeclTypeAbbrCodes[OneOfElementLayout::Code];
    OneOfElementLayout::emitRecord(Out, ScratchRecord, abbrCode,
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

    // FIXME: Handle attributes.
    if (!subscript->getAttrs().empty())
      return false;

    const Decl *DC = getDeclForContext(subscript->getDeclContext());

    unsigned abbrCode = DeclTypeAbbrCodes[SubscriptLayout::Code];
    SubscriptLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                addDeclRef(DC),
                                subscript->isImplicit(),
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

    // FIXME: Handle attributes.
    if (!ctor->getAttrs().empty())
      return false;

    // FIXME: Handle allocating constructors.
    // FIXME: Does this ever occur in Swift modules? If it's only used by the
    // importer, perhaps we don't need to worry about it here.
    if (ctor->getAllocThisExpr())
      return false;

    const Decl *DC = getDeclForContext(ctor->getDeclContext());
    auto implicitThis = ctor->getImplicitThisDecl();

    unsigned abbrCode = DeclTypeAbbrCodes[ConstructorLayout::Code];
    ConstructorLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                  addDeclRef(DC),
                                  ctor->isImplicit(),
                                  addTypeRef(ctor->getType()),
                                  addDeclRef(implicitThis));

    writeGenericParams(ctor->getGenericParams());
    writePattern(ctor->getArguments());

    return true;
  }

  case DeclKind::Destructor: {
    auto dtor = cast<DestructorDecl>(D);

    // FIXME: Handle attributes.
    if (!dtor->getAttrs().empty())
      return false;

    const Decl *DC = getDeclForContext(dtor->getDeclContext());
    auto implicitThis = dtor->getImplicitThisDecl();

    unsigned abbrCode = DeclTypeAbbrCodes[DestructorLayout::Code];
    DestructorLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                 addDeclRef(DC),
                                 dtor->isImplicit(),
                                 addTypeRef(dtor->getType()),
                                 addDeclRef(implicitThis));
    
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
  case TypeKind::BuiltinOpaquePointer:
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

  case TypeKind::Identifier: {
    auto identTy = cast<IdentifierType>(ty.getPointer());

    unsigned abbrCode = DeclTypeAbbrCodes[IdentifierTypeLayout::Code];
    IdentifierTypeLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                     addTypeRef(identTy->getMappedType()));
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
      // FIXME: Handle initializers.
      TupleTypeEltLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                     addIdentifierRef(elt.getName()),
                                     addTypeRef(elt.getType()),
                                     elt.isVararg());
    }

    return true;
  }

  case TypeKind::Struct:
  case TypeKind::OneOf:
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

  case TypeKind::Substituted: {
    auto subTy = cast<SubstitutedType>(ty.getPointer());

    unsigned abbrCode = DeclTypeAbbrCodes[SubstitutedTypeLayout::Code];
    SubstitutedTypeLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                      addTypeRef(subTy->getOriginal()),
                                      addTypeRef(subTy->getReplacementType()));
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
                                              fnTy->isThin());

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
    Type impl = sliceTy->getImplementationType();

    unsigned abbrCode = DeclTypeAbbrCodes[ArraySliceTypeLayout::Code];
    ArraySliceTypeLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                     addTypeRef(base), addTypeRef(impl));
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

  case TypeKind::ReferenceStorage: {
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
  case TypeKind::BoundGenericOneOf:
  case TypeKind::BoundGenericStruct: {
    auto generic = cast<BoundGenericType>(ty.getPointer());

    SmallVector<TypeID, 8> genericArgIDs;
    for (auto next : generic->getGenericArgs())
      genericArgIDs.push_back(addTypeRef(next));

    unsigned abbrCode = DeclTypeAbbrCodes[BoundGenericTypeLayout::Code];
    BoundGenericTypeLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                       addDeclRef(generic->getDecl()),
                                       addTypeRef(generic->getParent()),
                                       genericArgIDs);

    if (generic->hasSubstitutions()) {
      abbrCode = DeclTypeAbbrCodes[BoundGenericSubstitutionLayout::Code];
      for (auto &sub : generic->getSubstitutions()) {
        BoundGenericSubstitutionLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                                   addTypeRef(sub.Archetype),
                                                   addTypeRef(sub.Replacement));
        writeConformances(sub.Archetype->getConformsTo(), sub.Conformance);
      }
    }

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
    registerDeclTypeAbbr<NominalTypeLayout>();
    registerDeclTypeAbbr<ParenTypeLayout>();
    registerDeclTypeAbbr<TupleTypeLayout>();
    registerDeclTypeAbbr<TupleTypeEltLayout>();
    registerDeclTypeAbbr<IdentifierTypeLayout>();
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

    registerDeclTypeAbbr<TypeAliasLayout>();
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
    registerDeclTypeAbbr<OneOfLayout>();
    registerDeclTypeAbbr<OneOfElementLayout>();
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
    registerDeclTypeAbbr<ProtocolConformanceLayout>();
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

void Serializer::writeTranslationUnit(const TranslationUnit *TU) {
  assert(!this->TU && "already serializing a translation unit");
  this->TU = TU;

  SmallVector<DeclID, 32> topLevelIDs;
  SmallVector<DeclID, 8> operatorIDs;
  for (auto D : TU->Decls) {
    if (isa<ImportDecl>(D))
      continue;
    else if (isa<ValueDecl>(D))
      topLevelIDs.push_back(addDeclRef(D));
    else if (isa<ExtensionDecl>(D))
      // FIXME: should have a lazy extension table
      topLevelIDs.push_back(addDeclRef(D));
    else if (isa<OperatorDecl>(D))
      operatorIDs.push_back(addDeclRef(D));
  }

  writeAllDeclsAndTypes();
  writeAllIdentifiers();

  {
    BCBlockRAII restoreBlock(Out, INDEX_BLOCK_ID, 3);
    index_block::OffsetsLayout Offsets(Out);

    writeOffsets(Offsets, DeclOffsets);
    writeOffsets(Offsets, TypeOffsets);
    writeOffsets(Offsets, IdentifierOffsets);

    index_block::DeclListLayout DeclList(Out);
    DeclList.emit(ScratchRecord, index_block::TOP_LEVEL_DECLS, topLevelIDs);
    DeclList.emit(ScratchRecord, index_block::OPERATORS, operatorIDs);
  }

#ifndef NDEBUG
  this->TU = nullptr;
#endif
}

void Serializer::writeToStream(raw_ostream &os, const TranslationUnit *TU,
                               FileBufferIDs inputFiles) {
  // Write the signature through the BitstreamWriter for alignment purposes.
  for (unsigned char byte : SIGNATURE)
    Out.Emit(byte, 8);

  writeHeader();
  writeInputFiles(TU, inputFiles);
  writeTranslationUnit(TU);

  if (ShouldFallBackToTranslationUnit)
    BCBlockRAII(Out, FALL_BACK_TO_TRANSLATION_UNIT_ID, 2);

  os.write(Buffer.data(), Buffer.size());
  os.flush();
  Buffer.clear();
}

void swift::serialize(const TranslationUnit *TU, const char *outputPath,
                      FileBufferIDs inputFiles) {
  std::string errorInfo;
  llvm::raw_fd_ostream out(outputPath, errorInfo,
                           llvm::sys::fs::F_Binary);

  if (out.has_error() || !errorInfo.empty()) {
    TU->Ctx.Diags.diagnose(SourceLoc(), diag::error_opening_output, outputPath,
                           errorInfo);
    out.clear_error();
    return;
  }

  Serializer S;
  S.writeToStream(out, TU, inputFiles);
}
