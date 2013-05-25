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
    llvm::BitstreamWriter Out;

    /// A reusable buffer for emitting records.
    SmallVector<uint64_t, 64> ScratchRecord;

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

    /// The queue of types and decls that need to be serialized.
    ///
    /// This is a queue and not simply a vector because serializing one
    /// decl-or-type might trigger the serialization of another one.
    std::queue<DeclTypeUnion> DeclsAndTypesToWrite;

    std::array<unsigned, 256> DeclTypeAbbrCodes;

    /// The offset of each Decl in the bitstream, indexed by DeclID.
    std::vector<BitOffset> DeclOffsets;

    /// The offset of each Type in the bitstream, indexed by DeclID.
    std::vector<BitOffset> TypeOffsets;

    /// The last assigned DeclID for decls from this module.
    DeclID LastDeclID;

    /// The last assigned DeclID for types from this module.
    TypeID LastTypeID;

    /// True if this module does not fully represent the original source file.
    ///
    /// This is a bring-up hack and will eventually go away.
    bool ShouldFallBackToTranslationUnit;

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

    /// Writes the BLOCKINFO block.
    void writeBlockInfoBlock();

    /// Writes the Swift module file header, BLOCKINFO block, and
    /// non-TU-specific metadata.
    void writeHeader();

    /// Writes the input file paths.
    void writeInputFiles(llvm::SourceMgr &sourceMgr, FileBufferIDs inputFiles);

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

    /// Writes the offsets for decls or types.
    void writeOffsets(const index_block::OffsetsLayout &Offsets,
                      DeclOrType whichOffsets);

    /// Top-level entry point for serializing a translation unit module.
    void writeTranslationUnit(const TranslationUnit *TU);

  public:
    // FIXME: Use real local offsets.
    Serializer()
      : Out(Buffer), LastDeclID(0), LastTypeID(0),
        ShouldFallBackToTranslationUnit(false) {
    }

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

DeclID Serializer::addDeclRef(const Decl *D) {
  if (!D)
    return 0;

  DeclID &id = DeclIDs[D];
  if (id != 0)
    return id;

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

  BLOCK(DECLS_AND_TYPES_BLOCK);
  RECORD(decls_block, BUILTIN_TYPE);
  RECORD(decls_block, NAME_ALIAS_TYPE);
  RECORD(decls_block, TYPE_ALIAS_DECL);
  RECORD(decls_block, NAME_HACK);

  BLOCK(INDEX_BLOCK);
  RECORD(index_block, TYPE_OFFSETS);
  RECORD(index_block, DECL_OFFSETS);

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

void Serializer::writeInputFiles(llvm::SourceMgr &sourceMgr,
                                 FileBufferIDs inputFiles) {
  BCBlockRAII restoreBlock(Out, INPUT_BLOCK_ID, 3);
  input_block::SourceFileLayout SourceFile(Out);

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
}

bool Serializer::writeDecl(const Decl *D) {
  using namespace decls_block;

  assert(!D->isInvalid() && "cannot create a module with an invalid decl");
  if (D->hasClangNode())
    return false;

  switch (D->getKind()) {
  case DeclKind::Import:
    // FIXME: Do imported module names appear in the DeclContext of the
    // serialized module?
    return true;

  case DeclKind::Extension:
  case DeclKind::PatternBinding:
  case DeclKind::TopLevelCode:
    return false;

  case DeclKind::InfixOperator:
  case DeclKind::PrefixOperator:
  case DeclKind::PostfixOperator:
    return false;

  case DeclKind::TypeAlias: {
    auto typeAlias = cast<TypeAliasDecl>(D);
    assert(!typeAlias->isObjC() && "ObjC typealias is not meaningful");

    // FIXME: Handle attributes.
    // FIXME: Do typealiases have any interesting attributes? Resilience?
    if (!typeAlias->getAttrs().empty())
      return false;

    Type underlying;
    if (typeAlias->hasUnderlyingType())
      underlying = typeAlias->getUnderlyingType();

    SmallVector<TypeID, 4> inherited;
    for (auto parent : typeAlias->getInherited())
      inherited.push_back(addTypeRef(parent.getType()));

    unsigned abbrCode = DeclTypeAbbrCodes[TypeAliasLayout::Code];
    TypeAliasLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                addTypeRef(underlying),
                                typeAlias->isGenericParameter(),
                                typeAlias->isImplicit(),
                                inherited);

    abbrCode = DeclTypeAbbrCodes[NameHackLayout::Code];
    NameHackLayout::emitRecord(Out, ScratchRecord, abbrCode,
                               typeAlias->getName().str());
    return true;
  }

  case DeclKind::OneOf:
  case DeclKind::Struct:
  case DeclKind::Class:
  case DeclKind::Protocol:
    return false;

  case DeclKind::Var:
  case DeclKind::Func:
  case DeclKind::OneOfElement:
  case DeclKind::Subscript:
  case DeclKind::Constructor:
  case DeclKind::Destructor:
    return false;
  }
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
    llvm_unreachable("should always be accessed through an implicit typealias");

  case TypeKind::UnstructuredUnresolved:
    return false;

  case TypeKind::NameAlias: {
    auto nameAlias = cast<NameAliasType>(ty.getPointer());
    const TypeAliasDecl *typeAlias = nameAlias->getDecl();

    // Short-circuit builtin typealiases by just serializing their names; we'll
    // look them up in the Builtin module upon deserialization.
    if (isa<BuiltinModule>(typeAlias->getModuleContext())) {
      // FIXME: Come up with a compact code for common builtins.
      unsigned abbrCode = DeclTypeAbbrCodes[BuiltinTypeLayout::Code];
      BuiltinTypeLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                    typeAlias->getName().str());
      return true;
    }

    unsigned abbrCode = DeclTypeAbbrCodes[NameAliasTypeLayout::Code];
    NameAliasTypeLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                    addDeclRef(nameAlias->getDecl()));
    return true;
  }

  case TypeKind::Identifier:
    // FIXME: this is very wrong!
    return writeType(cast<IdentifierType>(ty.getPointer())->getMappedType());

  case TypeKind::Paren:
    // FIXME: trivial.
    return false;

  case TypeKind::Tuple:
    return false;

  case TypeKind::OneOf:
  case TypeKind::Struct:
  case TypeKind::Class:
  case TypeKind::Protocol:
    return false;

  case TypeKind::MetaType:
    return false;

  case TypeKind::Module:
    return false;

  case TypeKind::Archetype:
  case TypeKind::DeducibleGenericParam:
    return false;

  case TypeKind::Substituted:
    return false;

  case TypeKind::Function:
  case TypeKind::PolymorphicFunction:
    return false;

  case TypeKind::Array:
  case TypeKind::ArraySlice:
    return false;

  case TypeKind::ProtocolComposition:
  case TypeKind::LValue:
  case TypeKind::UnboundGeneric:
    return false;

  case TypeKind::BoundGenericClass:
  case TypeKind::BoundGenericOneOf:
  case TypeKind::BoundGenericStruct:
    return false;

  case TypeKind::TypeVariable:
    return false;
  }
}

void Serializer::writeAllDeclsAndTypes() {
  BCBlockRAII restoreBlock(Out, DECLS_AND_TYPES_BLOCK_ID, 3);

  registerDeclTypeAbbr<decls_block::BuiltinTypeLayout>();
  registerDeclTypeAbbr<decls_block::NameAliasTypeLayout>();
  registerDeclTypeAbbr<decls_block::TypeAliasLayout>();
  registerDeclTypeAbbr<decls_block::NameHackLayout>();

  while (!DeclsAndTypesToWrite.empty()) {
    DeclTypeUnion next = DeclsAndTypesToWrite.front();
    DeclsAndTypesToWrite.pop();

    DeclID id = DeclIDs[next];
    assert(id != 0 && "decl or type not referenced properly");

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

void Serializer::writeOffsets(const index_block::OffsetsLayout &Offsets,
                              DeclOrType which) {
  if (which == DeclOrType::IsDecl)
    Offsets.emit(ScratchRecord, index_block::DECL_OFFSETS, DeclOffsets);
  else
    Offsets.emit(ScratchRecord, index_block::TYPE_OFFSETS, TypeOffsets);
}

void Serializer::writeTranslationUnit(const TranslationUnit *TU) {
  // We don't handle imported modules at all yet, not even the standard library.
  for (auto &M : TU->getImportedModules())
    if (!isa<BuiltinModule>(M.second))
      ShouldFallBackToTranslationUnit = true;

  for (auto D : TU->Decls) {
    if (isa<ImportDecl>(D))
      continue;
    (void)addDeclRef(D);
  }

  writeAllDeclsAndTypes();
  
  {
    BCBlockRAII restoreBlock(Out, INDEX_BLOCK_ID, 3);
    index_block::OffsetsLayout Offsets(Out);

    writeOffsets(Offsets, DeclOrType::IsDecl);
    writeOffsets(Offsets, DeclOrType::IsType);
  }
}

void Serializer::writeToStream(raw_ostream &os, const TranslationUnit *TU,
                               FileBufferIDs inputFiles) {
  // Write the signature through the BitstreamWriter for alignment purposes.
  for (unsigned char byte : SIGNATURE)
    Out.Emit(byte, 8);

  writeHeader();
  writeInputFiles(TU->Ctx.SourceMgr, inputFiles);
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
                           llvm::raw_fd_ostream::F_Binary);

  if (out.has_error() || !errorInfo.empty()) {
    TU->Ctx.Diags.diagnose(SourceLoc(), diag::error_opening_output, outputPath,
                           errorInfo);
    out.clear_error();
    return;
  }

  Serializer S;
  S.writeToStream(out, TU, inputFiles);
}
