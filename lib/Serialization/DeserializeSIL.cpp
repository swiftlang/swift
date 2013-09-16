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
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILModule.h"
#include "swift/Serialization/BCReadingExtras.h"

// This is a template-only header; eventually it should move to llvm/Support.
#include "clang/Basic/OnDiskHashTable.h"

#include "llvm/ADT/StringExtras.h"
#include "llvm/Support/Debug.h"

using namespace swift;
using namespace swift::serialization;
using namespace swift::serialization::sil_block;

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
          kind == SIL_FUNC_NAMES) &&
         "Expect a SIL_FUNC_NAMES record.");
  FuncTable = readFuncTable(scratch, blobData);

  // Read SIL_FUNC_OFFSETS record and initialize Funcs.
  next = cursor.advance();
  scratch.clear();
  kind = cursor.readRecord(next.ID, scratch, &blobData);
  assert((next.Kind == llvm::BitstreamEntry::Record &&
          kind == SIL_FUNC_OFFSETS) &&
         "Expect a SIL_FUNC_OFFSETS record.");
  Funcs.assign(scratch.begin(), scratch.end());
}

std::unique_ptr<SILDeserializer::SerializedFuncTable>
SILDeserializer::readFuncTable(ArrayRef<uint64_t> fields, StringRef blobData) {
  uint32_t tableOffset;
  FuncListLayout::readRecord(fields, tableOffset);
  auto base = reinterpret_cast<const uint8_t *>(blobData.data());

  using OwnedTable = std::unique_ptr<SerializedFuncTable>;
  return OwnedTable(SerializedFuncTable::Create(base + tableOffset, base));
}

void SILDeserializer::setLocalValue(ValueBase *Value, ValueID Id) {
  LocalValues[Id] = Value;
}

SILValue SILDeserializer::getLocalValue(ValueID Id, unsigned ResultNum,
                                        SILType Type) {
  // Check to see if this is already defined.
  ValueBase *&Entry = LocalValues[Id];
  if (Entry) {
    // If this value is already defined, check it to make sure types match.
    SILType EntryTy = Entry->getType(ResultNum);
    assert(EntryTy == Type && "Value Type mismatch?");
    return SILValue(Entry, ResultNum);
  }
  // FIXME: handle forward references.
  return SILValue(nullptr, 0); 
}

/// Helper function to convert from Type to SILType.
static SILType getSILType(Type Ty, SILValueCategory Category) {
  auto TyLoc = TypeLoc::withoutLoc(Ty);
  return SILType::getPrimitiveType(TyLoc.getType()->getCanonicalType(),
                                   Category);
}

SILFunction *SILDeserializer::readSILFunction(DeclID FID, Identifier name) {
  LastValueID = 0;
  if (FID == 0)
    return nullptr;
  assert(FID <= Funcs.size() && "invalid SILFunction ID");
  auto &funcOrOffset = Funcs[FID-1];

  if (funcOrOffset.isComplete())
    return funcOrOffset; 

  BCOffsetRAII restoreOffset(SILCursor);
  SILCursor.JumpToBit(funcOrOffset);
  auto entry = SILCursor.advance();
  if (entry.Kind == llvm::BitstreamEntry::Error)
    return nullptr;

  SmallVector<uint64_t, 64> scratch;
  StringRef blobData;
  unsigned kind = SILCursor.readRecord(entry.ID, scratch, &blobData);
  assert(kind == SIL_FUNCTION && "expect a sil function");
  (void)kind;

  TypeID FuncTyID;
  unsigned Linkage;
  SILFunctionLayout::readRecord(scratch, Linkage, FuncTyID);
  if (FuncTyID == 0)
    return nullptr;

  auto Ty = MF->getType(FuncTyID);
  if (funcOrOffset.isComplete())
    return funcOrOffset;

  auto Fn = new (SILMod) SILFunction(SILMod, (SILLinkage)Linkage,
                                     name.str(),
                                     getSILType(Ty, SILValueCategory::Object));

  SILBasicBlock *CurrentBB = nullptr;

  // Fetch the next record.
  scratch.clear();
  entry = SILCursor.advance();
  if (entry.Kind == llvm::BitstreamEntry::EndBlock)
    // This function has no contents.
    return Fn;
  kind = SILCursor.readRecord(entry.ID, scratch);

  // Another SIL_FUNCTION record means the end of this SILFunction.
  while (kind != SIL_FUNCTION) {
    if (kind == SIL_BASIC_BLOCK)
      // Handle a SILBasicBlock record.
      CurrentBB = readSILBasicBlock(Fn, scratch);
    else {
      // Handle a SILInstruction record.
      if (readSILInstruction(CurrentBB, kind, scratch))
        return nullptr;
    }

    // Fetch the next record.
    scratch.clear();
    entry = SILCursor.advance();
    if (entry.Kind == llvm::BitstreamEntry::EndBlock)
      // EndBlock means the end of this SILFunction.
      return Fn;
    DEBUG(llvm::dbgs() << "Advance record ID " << entry.ID << "\n");
    kind = SILCursor.readRecord(entry.ID, scratch);
  }
  return Fn;
}

SILBasicBlock *SILDeserializer::readSILBasicBlock(SILFunction *Fn,
                                    SmallVectorImpl<uint64_t> &scratch) {
  ArrayRef<uint64_t> Args;
  SILBasicBlockLayout::readRecord(scratch, Args);

  // Args should be a list of pairs, the first number is a TypeID, the
  // second number is a ValueID.
  SILBasicBlock *CurrentBB = new (SILMod) SILBasicBlock(Fn);
  for (unsigned I = 0, E = Args.size(); I < E; I += 2) {
    TypeID TyID = Args[I];
    if (!TyID) return nullptr;
    ValueID ValId = Args[I+1];
    if (!ValId) return nullptr;

    auto ArgTy = MF->getType(TyID);
    auto Arg = new (SILMod) SILArgument(getSILType(ArgTy,
                                                   SILValueCategory::Object),
                                        CurrentBB);
    setLocalValue(Arg, ++LastValueID);
  }
  return CurrentBB;
}

bool SILDeserializer::readSILInstruction(SILBasicBlock *BB,
                          unsigned RecordKind,
                          SmallVectorImpl<uint64_t> &scratch) {
  // Return error if Basic Block is null.
  if (!BB)
    return true;

  SILBuilder Builder(BB);
  unsigned OpCode, TyCategory, ValResNum, ValResNum2;
  ValueID ValID, ValID2;
  TypeID TyID;
  SourceLoc SLoc;
  SILLocation Loc = SILFileLocation(SLoc);

  switch (RecordKind) {
  default:
    assert(0 && "Record kind for a SIL instruction is not supported.");
  case SIL_ONE_VALUE_ONE_OPERAND:
    SILOneValueOneOperandLayout::readRecord(scratch, OpCode,
                                            ValID, ValResNum, TyID, TyCategory,
                                            ValID2, ValResNum2);
    break;
  case SIL_ONE_TYPE:
    SILOneTypeLayout::readRecord(scratch, OpCode, TyID, TyCategory);
    break;
  case SIL_ONE_OPERAND:
    SILOneOperandLayout::readRecord(scratch, OpCode, TyID, TyCategory, ValID,
                                    ValResNum);
    break;
  case SIL_INST_TODO:
    SILInstTodoLayout::readRecord(scratch, OpCode);
    break;
  }

  ValueBase *ResultVal;
  switch ((ValueKind)OpCode) {
  default:
    DEBUG(llvm::dbgs() << "To be handled: " << OpCode << "\n");
    return true;
  case ValueKind::AllocBoxInst: {
    auto Ty = MF->getType(TyID);
    ResultVal = Builder.createAllocBox(Loc,
                            getSILType(Ty, (SILValueCategory)TyCategory));
    break;
  }
  case ValueKind::AllocStackInst: {
    auto Ty = MF->getType(TyID);
    ResultVal = Builder.createAllocStack(Loc,
                            getSILType(Ty, (SILValueCategory)TyCategory));
    break;
  }
  case ValueKind::DeallocStackInst: {
    auto Ty = MF->getType(TyID);
    ResultVal = Builder.createDeallocStack(Loc,
        getLocalValue(ValID, ValResNum,
                      getSILType(Ty, (SILValueCategory)TyCategory)));
    break;
  }
  case ValueKind::ReturnInst: {
    auto Ty = MF->getType(TyID); 
    ResultVal = Builder.createReturn(Loc,
        getLocalValue(ValID, ValResNum,
                      getSILType(Ty, (SILValueCategory)TyCategory)));
    break;
  }
  case ValueKind::StoreInst: {
    auto Ty = MF->getType(TyID);
    SILType addrType = getSILType(Ty, (SILValueCategory)TyCategory);
    SILType ValType = addrType.getObjectType();
    ResultVal = Builder.createStore(Loc,
                    getLocalValue(ValID, ValResNum, ValType),
                    getLocalValue(ValID2, ValResNum2, addrType));
    break;
  }
  }
  if (ResultVal->hasValue())
    setLocalValue(ResultVal, ++LastValueID);
  return false;
}

SILFunction *SILDeserializer::lookupSILFunction(Identifier name) {
  if (!FuncTable)
    return nullptr;
  auto iter = FuncTable->find(name);
  if (iter == FuncTable->end())
    return nullptr;

  auto Func = readSILFunction(*iter, name);
  return Func;
}

SILDeserializer::~SILDeserializer() = default;
