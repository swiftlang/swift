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
#include "llvm/Config/Config.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/raw_ostream.h"
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
    /// Discriminator between Decls and Types.
    enum class DeclOrType {
      IsDecl,
      IsType
    };
    
    /// Stores a declaration or a type to be written to the AST file.
    class DeclTypeUnion {
      union {
        const Decl *D;
        Type T;
        const void *Opaque;
      };
      static_assert(sizeof(Type) == sizeof(void *),
                    "cannot use simple opaque union");

      DeclOrType Kind;
    public:
      /*implicit*/ DeclTypeUnion(const Decl *d)
        : D(d), Kind(DeclOrType::IsDecl) { }
      /*implicit*/ DeclTypeUnion(Type ty)
        : T(ty), Kind(DeclOrType::IsType) { }

      bool isDecl() const { return Kind == DeclOrType::IsDecl; }
      bool isType() const { return Kind == DeclOrType::IsType; }

      Type getType() const {
        assert(isType() && "Not a type!");
        return T;
      }

      const Decl *getDecl() const {
        assert(isDecl() && "Not a decl!");
        return D;
      }

      bool operator==(const DeclTypeUnion &other) const {
        return Kind == other.Kind && Opaque == other.Opaque;
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

    /// The offset of each Decl in the bitstream, indexed by DeclID.
    std::vector<BitOffset> DeclOffsets;

    /// The last assigned DeclID for decls from this module.
    DeclID LastDeclID;

    /// Records the use of the given Decl.
    ///
    /// The Decl will be scheduled for serialization if necessary.
    ///
    /// \returns The ID for the given Decl in this module.
    DeclID addDeclRef(const Decl *D);

    /// The offset of each Type in the bitstream, indexed by DeclID.
    std::vector<BitOffset> TypeOffsets;

    /// The last assigned DeclID for types from this module.
    DeclID LastTypeID;

    /// Records the use of the given Type.
    ///
    /// The Type will be scheduled for serialization if necessary.
    ///
    /// \returns The ID for the given Type in this module.
    DeclID addTypeRef(Type ty);

    /// Writes the BLOCKINFO block.
    void writeBlockInfoBlock();

    /// Writes the Swift module file header, BLOCKINFO block, and
    /// non-TU-specific metadata.
    void writeHeader();

    /// Writes the input file paths.
    void writeInputFiles(llvm::SourceMgr &sourceMgr, FileBufferIDs inputFiles);

    /// Writes all decls and types in the DeclsToWrite queue.
    ///
    /// This will continue until the queue is empty, even if the items currently
    /// in the queue trigger the serialization of additional decls and/or types.
    void writeAllDeclsAndTypes();

    /// Writes the offsets for decls or types.
    void writeOffsets(DeclOrType whichOffsets);

    /// Top-level entry point for serializing a translation unit module.
    void writeTranslationUnit(const TranslationUnit *TU);

  public:
    Serializer() : Out(Buffer), LastDeclID(0), LastTypeID(0) {}

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
      if (val.isType())
        return DenseMapInfo<Type>::getHashValue(val.getType());
      return DenseMapInfo<const Decl *>::getHashValue(val.getDecl());
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

DeclID Serializer::addTypeRef(Type ty) {
  if (!ty)
    return 0;

  DeclID &id = DeclIDs[ty];
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
  RECORD(decls_block, TYPEALIAS_DECL);
  RECORD(decls_block, NAME_HACK);

  BLOCK(INDEX_BLOCK);
  RECORD(index_block, TYPE_OFFSETS);
  RECORD(index_block, DECL_OFFSETS);

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

void Serializer::writeAllDeclsAndTypes() {
  BCBlockRAII restoreBlock(Out, DECLS_AND_TYPES_BLOCK_ID, 3);

  while (!DeclsAndTypesToWrite.empty()) {
    DeclTypeUnion next = DeclsAndTypesToWrite.front();
    DeclsAndTypesToWrite.pop();

    // FIXME: actually write the thing.

    DeclID id = DeclIDs[next];
    assert(id != 0 && "decl or type not referenced properly");

    auto &offsets = next.isDecl() ? DeclOffsets : TypeOffsets;
    assert(id == offsets.size());
    offsets.push_back(Out.GetCurrentBitNo());
  }
}

void Serializer::writeOffsets(DeclOrType which) {
  if (which == DeclOrType::IsDecl) {
    index_block::DeclOffsetsLayout Offsets(Out);
    // FIXME: Use a real start offset.
    Offsets.emit(ScratchRecord, 0, DeclOffsets);
  } else {
    index_block::TypeOffsetsLayout Offsets(Out);
    // FIXME: Use a real start offset.
    Offsets.emit(ScratchRecord, 0, TypeOffsets);
  }
}

void Serializer::writeTranslationUnit(const TranslationUnit *TU) {
  for (auto D : TU->Decls)
    (void)addDeclRef(D);

  writeAllDeclsAndTypes();
  
  {
    BCBlockRAII restoreBlock(Out, INDEX_BLOCK_ID, 3);

    writeOffsets(DeclOrType::IsDecl);
    writeOffsets(DeclOrType::IsType);
  }
}

void Serializer::writeToStream(raw_ostream &os, const TranslationUnit *TU,
                               FileBufferIDs inputFiles){
  // Write the signature through the BitstreamWriter for alignment purposes.
  for (unsigned char byte : SIGNATURE)
    Out.Emit(byte, 8);

  writeHeader();
  writeInputFiles(TU->Ctx.SourceMgr, inputFiles);
  writeTranslationUnit(TU);

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
