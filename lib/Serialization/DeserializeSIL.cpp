//===--- DeserializeSIL.cpp - Read SIL ------------------------------------===//
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

#include "DeserializeSIL.h"
#include "ModuleFile.h"
#include "SILFormat.h"
#include "swift/SIL/SILModule.h"
#include "swift/Serialization/BCReadingExtras.h"

// This is a template-only header; eventually it should move to llvm/Support.
#include "clang/Basic/OnDiskHashTable.h"

#include "llvm/ADT/StringExtras.h"

using namespace swift;
using namespace serialization;

/// Used to deserialize entries in the on-disk func hash table.
class SILDeserializer::FuncTableInfo {
public:
  using internal_key_type = StringRef;
  using external_key_type = Identifier;
  using data_type = DeclID;

  internal_key_type GetInternalKey(external_key_type ID) {
    return ID.str();
  }

  uint32_t ComputeHash(internal_key_type key) {
    return llvm::HashString(key);
  }

  static bool EqualKey(internal_key_type lhs, internal_key_type rhs) {
    return lhs == rhs;
  }

  static std::pair<unsigned, unsigned> ReadKeyDataLength(const uint8_t *&data) {
    using namespace clang::io;
    unsigned keyLength = ReadUnalignedLE16(data);
    unsigned dataLength = ReadUnalignedLE16(data);
    return { keyLength, dataLength };
  }

  static internal_key_type ReadKey(const uint8_t *data, unsigned length) {
    return StringRef(reinterpret_cast<const char *>(data), length);
  }

  static data_type ReadData(internal_key_type key, const uint8_t *data,
                            unsigned length) {
    using namespace clang::io;

    assert(length == 4 && "Expect a single DeclID.");
    data_type result = ReadUnalignedLE32(data);
    return result;
  }
};

SILDeserializer::SILDeserializer(ModuleFile *MF, SILModule &M) :
                                MF(MF), SILMod(M) {
  SILCursor = MF->getSILCursor();
  // Load any abbrev records at the start of the block.
  SILCursor.advance();
  SILIndexCursor = MF->getSILIndexCursor();

  llvm::BitstreamCursor cursor = SILIndexCursor;
  // Read SIL_FUNC_NAMES record and update FuncTable.
  auto next = cursor.advance();
  if (next.Kind == llvm::BitstreamEntry::EndBlock)
    return;
  SmallVector<uint64_t, 4> scratch;
  StringRef blobData;
  unsigned kind = cursor.readRecord(next.ID, scratch, &blobData);
  assert((next.Kind == llvm::BitstreamEntry::Record &&
          kind == sil_block::SIL_FUNC_NAMES) &&
         "Expect a SIL_FUNC_NAMES record.");
  FuncTable = readFuncTable(scratch, blobData);

  // Read SIL_FUNC_OFFSETS record and initialize Funcs.
  next = cursor.advance();
  scratch.clear();
  kind = cursor.readRecord(next.ID, scratch, &blobData);
  assert((next.Kind == llvm::BitstreamEntry::Record &&
          kind == sil_block::SIL_FUNC_OFFSETS) &&
         "Expect a SIL_FUNC_OFFSETS record.");
  Funcs.assign(scratch.begin(), scratch.end());
}

std::unique_ptr<SILDeserializer::SerializedFuncTable>
SILDeserializer::readFuncTable(ArrayRef<uint64_t> fields, StringRef blobData) {
  uint32_t tableOffset;
  sil_block::FuncListLayout::readRecord(fields, tableOffset);
  auto base = reinterpret_cast<const uint8_t *>(blobData.data());

  using OwnedTable = std::unique_ptr<SerializedFuncTable>;
  return OwnedTable(SerializedFuncTable::Create(base + tableOffset, base));
}

SILFunction *SILDeserializer::readSILFunction(DeclID FID) {
  if (FID == 0)
    return nullptr;
  assert(FID <= Funcs.size() && "invalid SILFunction ID");
  auto &funcOrOffset = Funcs[FID-1];

  if (funcOrOffset.isComplete())
    return funcOrOffset; 

  BCOffsetRAII restoreOffset(SILCursor);
  SILCursor.JumpToBit(funcOrOffset);
  auto entry = SILCursor.advance();

  SmallVector<uint64_t, 64> scratch;
  StringRef blobData;
  SILCursor.readRecord(entry.ID, scratch, &blobData);

  TypeID FuncTyID;
  unsigned Linkage;
  sil_block::SILFunctionLayout::readRecord(scratch, Linkage, FuncTyID);
  if (FuncTyID == 0)
    return nullptr;

  auto FnTy = TypeLoc::withoutLoc(MF->getType(FuncTyID));
  if (funcOrOffset.isComplete())
    return funcOrOffset;

  SILType SILTy = SILType::getPrimitiveType(FnTy.getType()->getCanonicalType(),
                                            SILValueCategory::Object);
  auto Fn = new (SILMod) SILFunction(SILMod, (SILLinkage)Linkage, "", SILTy);
  return Fn;
}

SILFunction *SILDeserializer::lookupSILFunction(Identifier name) {
  if (!FuncTable)
    return nullptr;
  auto iter = FuncTable->find(name);
  if (iter == FuncTable->end())
    return nullptr;

  auto Func = readSILFunction(*iter);
  return Func;
}

SILDeserializer::~SILDeserializer() = default;
