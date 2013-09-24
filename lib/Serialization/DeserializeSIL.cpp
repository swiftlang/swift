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
#include "swift/SIL/SILDebugScope.h"
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

SILDeserializer::SILDeserializer(ModuleFile *MF, SILModule &M,
                                 ASTContext &Ctx) :
                                MF(MF), SILMod(M), Ctx(Ctx) {
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
  ValueBase *&Entry = LocalValues[Id];
  if (Entry) {
    // If this value was already referenced, check it to make sure types match.
    assert(Entry->getTypes() != Value->getTypes() && "Value Type mismatch?");

    auto It = ForwardMRVLocalValues.find(Id);
    if (It != ForwardMRVLocalValues.end()) {
      // Take the information about the forward ref out of the map.
      std::vector<SILValue> Entries(std::move(It->second));

      // Remove the entries from the map.
      ForwardMRVLocalValues.erase(It);

      assert(Entries.size() <= Value->getTypes().size() &&
             "Value Type mismatch?");
      // Validate that any forward-referenced elements have the right type, and
      // RAUW them.
      for (unsigned i = 0, e = Entries.size(); i != e; ++i) {
        if (!Entries[i]) continue;

        assert(Entries[i]->getType(0) != Value->getType(i) &&
               "Value Type mismatch?");
        Entries[i].replaceAllUsesWith(SILValue(Value, i));
      }
    }
  }

  // Store it in our map.
  Entry = Value;
}

SILValue SILDeserializer::getLocalValue(ValueID Id, unsigned ResultNum,
                                        SILType Type) {
  // Check to see if this is already defined.
  ValueBase *&Entry = LocalValues[Id];
  if (Entry) {
    // If this value was already defined, check it to make sure types match.
    SILType EntryTy = Entry->getType(ResultNum);
    assert(EntryTy == Type && "Value Type mismatch?");
    (void)EntryTy;
    return SILValue(Entry, ResultNum);
  }

  // Otherwise, this is a forward reference.  Create a dummy node to represent
  // it until we see a real definition.
  std::vector<SILValue> &Placeholders = ForwardMRVLocalValues[Id];
  SourceLoc Loc;
  if (Placeholders.size() <= ResultNum)
    Placeholders.resize(ResultNum+1);

  if (!Placeholders[ResultNum])
    Placeholders[ResultNum] =
      new (SILMod) GlobalAddrInst(SILFileLocation(Loc), nullptr, Type);
  return Placeholders[ResultNum];
}

/// Return the SILBasicBlock of a given ID.
SILBasicBlock *SILDeserializer::getBBForDefinition(SILFunction *Fn,
                                                   unsigned ID) {
  SILBasicBlock *&BB = BlocksByID[ID];
  // If the block has never been named yet, just create it.
  if (BB == nullptr)
    return BB = new (SILMod) SILBasicBlock(Fn);

  // If it already exists, it was either a forward reference or a redefinition.
  // If it is a forward reference, it should be in our undefined set.
  if (!UndefinedBlocks.erase(BB)) {
    // If we have a redefinition, return a new BB to avoid inserting
    // instructions after the terminator.
    return new (SILMod) SILBasicBlock(Fn);
  }
  return BB;
}

/// Return the SILBasicBlock of a given ID.
SILBasicBlock *SILDeserializer::getBBForReference(SILFunction *Fn,
                                                  unsigned ID) {
  SILBasicBlock *&BB = BlocksByID[ID];
  if (BB != nullptr)
    return BB;

  // Otherwise, create it and remember that this is a forward reference
  BB = new (SILMod) SILBasicBlock(Fn);
  UndefinedBlocks[BB] = ID;
  return BB;
}

/// Helper function to convert from Type to SILType.
static SILType getSILType(Type Ty, SILValueCategory Category) {
  auto TyLoc = TypeLoc::withoutLoc(Ty);
  return SILType::getPrimitiveType(TyLoc.getType()->getCanonicalType(),
                                   Category);
}

SILFunction *SILDeserializer::readSILFunction(DeclID FID, SILFunction *InFunc) {
  LastValueID = 0;
  if (FID == 0)
    return nullptr;
  assert(FID <= Funcs.size() && "invalid SILFunction ID");
  auto &funcOrOffset = Funcs[FID-1];

  if (funcOrOffset.isComplete())
    return funcOrOffset; 

  BCOffsetRAII restoreOffset(SILCursor);
  SILCursor.JumpToBit(funcOrOffset);
  auto entry = SILCursor.advance(AF_DontPopBlockAtEnd);
  if (entry.Kind == llvm::BitstreamEntry::Error) {
    DEBUG(llvm::dbgs() << "Cursor advance error in readSILFunction.\n");
    return nullptr;
  }

  SmallVector<uint64_t, 64> scratch;
  StringRef blobData;
  unsigned kind = SILCursor.readRecord(entry.ID, scratch, &blobData);
  assert(kind == SIL_FUNCTION && "expect a sil function");
  (void)kind;

  TypeID FuncTyID;
  unsigned Linkage;
  SILFunctionLayout::readRecord(scratch, Linkage, FuncTyID);
  if (FuncTyID == 0) {
    DEBUG(llvm::dbgs() << "SILFunction typeID is 0.\n");
    return nullptr;
  }

  auto Ty = MF->getType(FuncTyID);

  // Verify that the types match up.
  if (InFunc->getLoweredType() != getSILType(Ty, SILValueCategory::Object)) {
    DEBUG(llvm::dbgs() << "SILFunction type mismatch.\n");
    return nullptr;
  }
  auto Fn = InFunc;
  // FIXME: what should we set the linkage to?
  Fn->setLinkage(SILLinkage::Deserialized);
  // FIXME: use the correct SILLocation from module.
  SourceLoc Loc;
  Fn->setLocation(SILFileLocation(Loc));
  Fn->setDebugScope(new (SILMod) SILDebugScope(Fn->getLocation()));
  SILBasicBlock *CurrentBB = nullptr;

  // Clear up at the beginning of each SILFunction.
  BasicBlockID = 0;
  BlocksByID.clear();
  UndefinedBlocks.clear();
  LastValueID = 0;
  LocalValues.clear();
  ForwardMRVLocalValues.clear();

  // Fetch the next record.
  scratch.clear();
  entry = SILCursor.advance(AF_DontPopBlockAtEnd);
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
      if (readSILInstruction(Fn, CurrentBB, kind, scratch)) {
        DEBUG(llvm::dbgs() << "readSILInstruction returns error.\n");
        return Fn;
      }
    }

    // Fetch the next record.
    scratch.clear();
    entry = SILCursor.advance(AF_DontPopBlockAtEnd);
    if (entry.Kind == llvm::BitstreamEntry::EndBlock)
      // EndBlock means the end of this SILFunction.
      return Fn;
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
  SILBasicBlock *CurrentBB = getBBForDefinition(Fn, BasicBlockID++);
  for (unsigned I = 0, E = Args.size(); I < E; I += 3) {
    TypeID TyID = Args[I];
    if (!TyID) return nullptr;
    ValueID ValId = Args[I+2];
    if (!ValId) return nullptr;

    auto ArgTy = MF->getType(TyID);
    auto Arg = new (SILMod) SILArgument(getSILType(ArgTy,
                                                   (SILValueCategory)Args[I+1]),
                                        CurrentBB);
    setLocalValue(Arg, ++LastValueID);
  }
  return CurrentBB;
}

/// Helper function to find the SILFunction given name and type.
static SILFunction *getFuncForReference(Identifier Name, SILType Ty,
                                        SILModule &SILMod) {
  // Check to see if we have a function by this name already.
  if (SILFunction *FnRef = SILMod.lookup(Name.str()))
    // FIXME: check for matching types.
    return FnRef;

  // If we didn't find a function, create a new one.
  SourceLoc Loc;
  auto Fn = new (SILMod) SILFunction(SILMod, SILLinkage::Internal,
                                     Name.str(), Ty, SILFileLocation(Loc));
  return Fn;
}

bool SILDeserializer::readSILInstruction(SILFunction *Fn, SILBasicBlock *BB,
                          unsigned RecordKind,
                          SmallVectorImpl<uint64_t> &scratch) {
  // Return error if Basic Block is null.
  if (!BB)
    return true;

  SILBuilder Builder(BB);
  unsigned OpCode, TyCategory, TyCategory2, ValResNum, ValResNum2, Attr;
  ValueID ValID, ValID2;
  TypeID TyID, TyID2;
  SourceLoc SLoc;
  ArrayRef<uint64_t> ListOfValues;
  SILLocation Loc = SILFileLocation(SLoc);

  switch (RecordKind) {
  default:
    assert(0 && "Record kind for a SIL instruction is not supported.");
  case SIL_ONE_VALUE_ONE_OPERAND:
    SILOneValueOneOperandLayout::readRecord(scratch, OpCode, Attr,
                                            ValID, ValResNum, TyID, TyCategory,
                                            ValID2, ValResNum2);
    break;
  case SIL_ONE_TYPE:
    SILOneTypeLayout::readRecord(scratch, OpCode, TyID, TyCategory);
    break;
  case SIL_ONE_OPERAND:
    SILOneOperandLayout::readRecord(scratch, OpCode, Attr,
                                    TyID, TyCategory, ValID, ValResNum);
    break;
  case SIL_ONE_TYPE_ONE_OPERAND:
    SILOneTypeOneOperandLayout::readRecord(scratch, OpCode, Attr,
                                           TyID, TyCategory,
                                           TyID2, TyCategory2,
                                           ValID, ValResNum);
    break;
  case SIL_ONE_TYPE_VALUES:
    SILOneTypeValuesLayout::readRecord(scratch, OpCode, TyID, TyCategory,
                                       ListOfValues);
    break;
  case SIL_TWO_OPERANDS:
    SILTwoOperandsLayout::readRecord(scratch, OpCode, Attr,
                                     TyID, TyCategory, ValID, ValResNum,
                                     TyID2, TyCategory2, ValID2, ValResNum2);
    break;
  case SIL_INST_APPLY: {
    unsigned IsPartial;
    SILInstApplyLayout::readRecord(scratch, IsPartial, Attr, TyID, TyCategory,
                                   ValID, ValResNum, ListOfValues);
    OpCode = (unsigned)(IsPartial ? ValueKind::PartialApplyInst :
                                    ValueKind::ApplyInst);
    break;
  }
  case SIL_INST_NO_OPERAND:
    SILInstNoOperandLayout::readRecord(scratch, OpCode);
    break;
  }

  ValueBase *ResultVal;
  switch ((ValueKind)OpCode) {
  case ValueKind::SILArgument:
    llvm_unreachable("not an instruction");

  case ValueKind::DeallocBoxInst:
  case ValueKind::InitExistentialInst:
  case ValueKind::InitExistentialRefInst:
  case ValueKind::ArchetypeMetatypeInst:
  case ValueKind::ClassMetatypeInst:
  case ValueKind::ProtocolMetatypeInst:
  case ValueKind::AllocArrayInst: {
    auto Ty = getSILType(MF->getType(TyID), (SILValueCategory)TyCategory);
    auto Ty2 = MF->getType(TyID2);
    SILValue operand = getLocalValue(ValID, ValResNum,
                         getSILType(Ty2, (SILValueCategory)TyCategory2));
    switch ((ValueKind)OpCode) {
    default: assert(0 && "Out of sync with parent switch");
    case ValueKind::ArchetypeMetatypeInst:
      ResultVal = Builder.createArchetypeMetatype(Loc, Ty, operand);
      break;
    case ValueKind::ClassMetatypeInst:
      ResultVal = Builder.createClassMetatype(Loc, Ty, operand);
      break;
    case ValueKind::ProtocolMetatypeInst:
      ResultVal = Builder.createProtocolMetatype(Loc, Ty, operand);
      break;
    case ValueKind::DeallocBoxInst:
      ResultVal = Builder.createDeallocBox(Loc, Ty, operand);
      break;
    case ValueKind::AllocArrayInst:
      ResultVal = Builder.createAllocArray(Loc, Ty, operand);
      break;
    case ValueKind::InitExistentialInst:
      // FIXME: Conformances in InitExistentialInst needs to be serialized.
      ResultVal = Builder.createInitExistential(Loc, operand, Ty,
                      ArrayRef<ProtocolConformance*>());
      break;
    case ValueKind::InitExistentialRefInst:
      // FIXME: Conformances in InitExistentialRefInst needs to be serialized.
      ResultVal = Builder.createInitExistentialRef(Loc, Ty, operand,
                      ArrayRef<ProtocolConformance*>());
      break;
    }
    break;
  }
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
  case ValueKind::AllocRefInst: {
    auto Ty = MF->getType(TyID);
    ResultVal = Builder.createAllocRef(Loc,
                            getSILType(Ty, (SILValueCategory)TyCategory));
    break;
  }
  case ValueKind::BuiltinZeroInst: {
    auto Ty = MF->getType(TyID);
    ResultVal = Builder.createBuiltinZero(Loc,
                            getSILType(Ty, (SILValueCategory)TyCategory));
    break;
  }
  case ValueKind::ApplyInst: {
    // Format: attributes such as transparent, the callee's type, a value for
    // the callee and a list of values for the arguments. Each value in the list
    // is represented with 2 IDs: ValueID and ValueResultNumber.
    auto Ty = MF->getType(TyID);
    SILType FnTy = getSILType(Ty, (SILValueCategory)TyCategory);
    SILFunctionTypeInfo *FTI = FnTy.getFunctionTypeInfo(SILMod);
    auto ArgTys = FTI->getInputTypes();

    assert((ArgTys.size() << 1) == ListOfValues.size() &&
           "Argument number mismatch in ApplyInst.");
    SmallVector<SILValue, 4> Args;
    for (unsigned I = 0, E = ListOfValues.size(); I < E; I += 2)
      Args.push_back(getLocalValue(ListOfValues[I], ListOfValues[I+1],
                                   ArgTys[I>>1]));
    bool Transparent = (Attr == 1);
    ResultVal = Builder.createApply(Loc, getLocalValue(ValID, ValResNum, FnTy),
                                    FTI->getResultType(), Args, Transparent);
    break;
  }
  case ValueKind::PartialApplyInst: {
    auto Ty = MF->getType(TyID);
    SILType FnTy = getSILType(Ty, (SILValueCategory)TyCategory);
    SILFunctionTypeInfo *FTI = FnTy.getFunctionTypeInfo(SILMod);
    auto ArgTys = FTI->getInputTypes();

    assert((ArgTys.size() << 1) >= ListOfValues.size() &&
           "Argument number mismatch in PartialApplyInst.");

    SmallVector<TupleTypeElt, 4> NewArgTypes;
    // Compute the result type of the partial_apply, based on which arguments
    // are getting applied.
    unsigned ArgNo = 0, NewArgCount = ArgTys.size() - (ListOfValues.size()>>1);
    while (ArgNo != NewArgCount)
      NewArgTypes.push_back(ArgTys[ArgNo++].getSwiftType());

    SILValue FnVal = getLocalValue(ValID, ValResNum, FnTy);
    SmallVector<SILValue, 4> Args;
    for (unsigned I = 0, E = ListOfValues.size(); I < E; I += 2)
      Args.push_back(getLocalValue(ListOfValues[I], ListOfValues[I+1],
                                   ArgTys[ArgNo++]));

    Type ArgTy = TupleType::get(NewArgTypes, Ctx);
    Type ResTy = FunctionType::get(ArgTy, FTI->getResultType().getSwiftType(),
                                   Ctx);

    // FIXME: Why the arbitrary order difference in IRBuilder type argument?
    ResultVal = Builder.createPartialApply(Loc, FnVal, Args,
                                           SILMod.Types.getLoweredType(ResTy));
    break;
  }
  case ValueKind::BuiltinFunctionRefInst: {
    // Format: FuncDecl and type. Use SILOneOperandLayout.
    auto Ty = MF->getType(TyID);
    ResultVal = Builder.createBuiltinFunctionRef(Loc,
                    cast<FuncDecl>(MF->getDecl(ValID)),
                    getSILType(Ty, (SILValueCategory)TyCategory));
    break;
  }
  case ValueKind::GlobalAddrInst: {
    // Format: VarDecl and type. Use SILOneOperandLayout.
    auto Ty = MF->getType(TyID);
    ResultVal = Builder.createGlobalAddr(Loc,
                    cast<VarDecl>(MF->getDecl(ValID)),
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
  case ValueKind::DeallocRefInst: {
    auto Ty = MF->getType(TyID);
    ResultVal = Builder.createDeallocRef(Loc,
        getLocalValue(ValID, ValResNum,
                      getSILType(Ty, (SILValueCategory)TyCategory)));
    break;
  }
  case ValueKind::FunctionRefInst: {
    auto Ty = MF->getType(TyID);
    Identifier FuncName = MF->getIdentifier(ValID); 
    ResultVal = Builder.createFunctionRef(Loc,
        getFuncForReference(FuncName,
                            getSILType(Ty, (SILValueCategory)TyCategory),
                            SILMod));
    break;
  }
  case ValueKind::IndexAddrInst: {
    auto Ty = MF->getType(TyID);
    auto Ty2 = MF->getType(TyID2);
    ResultVal = Builder.createIndexAddr(Loc,
        getLocalValue(ValID, ValResNum,
                      getSILType(Ty,  (SILValueCategory)TyCategory)),
        getLocalValue(ValID2, ValResNum2,
                      getSILType(Ty2,  (SILValueCategory)TyCategory2)));
    break;
  }
  case ValueKind::IndexRawPointerInst: {
    auto Ty = MF->getType(TyID);
    auto Ty2 = MF->getType(TyID2);
    ResultVal = Builder.createIndexRawPointer(Loc,
        getLocalValue(ValID, ValResNum,
                      getSILType(Ty,  (SILValueCategory)TyCategory)),
        getLocalValue(ValID2, ValResNum2,
                      getSILType(Ty2,  (SILValueCategory)TyCategory2)));
    break;
  }
  case ValueKind::UpcastExistentialInst: {
    auto Ty = MF->getType(TyID);
    auto Ty2 = MF->getType(TyID2);
    bool isTake = (Attr > 0);
    ResultVal = Builder.createUpcastExistential(Loc,
        getLocalValue(ValID, ValResNum,
                      getSILType(Ty,  (SILValueCategory)TyCategory)),
        getLocalValue(ValID2, ValResNum2,
                      getSILType(Ty2,  (SILValueCategory)TyCategory2)),
        IsTake_t(isTake));
    break;
  }
  case ValueKind::IntegerLiteralInst: {
    auto Ty = MF->getType(TyID);
    auto intTy = Ty->getAs<BuiltinIntegerType>();
    Identifier StringVal = MF->getIdentifier(ValID);
    // Build APInt from string.
    APInt value(intTy->getBitWidth(), StringVal.str(), 10);
    ResultVal = Builder.createIntegerLiteral(Loc,
        getSILType(Ty, (SILValueCategory)TyCategory),
        value);
    break;
  }
  case ValueKind::FloatLiteralInst: {
    auto Ty = MF->getType(TyID);
    auto floatTy = Ty->getAs<BuiltinFloatType>();
    Identifier StringVal = MF->getIdentifier(ValID);
    // Build APInt from string.
    APInt bits(floatTy->getBitWidth(), StringVal.str(), 16);
    if (bits.getBitWidth() != floatTy->getBitWidth())
      bits = bits.zextOrTrunc(floatTy->getBitWidth());

    APFloat value(floatTy->getAPFloatSemantics(), bits);

    ResultVal = Builder.createFloatLiteral(Loc,
        getSILType(Ty, (SILValueCategory)TyCategory),
        value);
    break;
  }
  case ValueKind::StringLiteralInst: {
    auto Ty = MF->getType(TyID);
    Identifier StringVal = MF->getIdentifier(ValID);
    ResultVal = Builder.createStringLiteral(Loc,
        getSILType(Ty, (SILValueCategory)TyCategory),
        StringVal.str());
    break;
  }
  case ValueKind::MarkFunctionEscapeInst: {
    // Format: a list of typed values. A typed value is expressed by 4 IDs:
    // TypeID, TypeCategory, ValueID, ValueResultNumber.
    SmallVector<SILValue, 4> OpList;
    for (unsigned I = 0, E = ListOfValues.size(); I < E; I += 4) {
      auto EltTy = MF->getType(ListOfValues[I]);
      OpList.push_back(
        getLocalValue(ListOfValues[I+2], ListOfValues[I+3],
                      getSILType(EltTy, (SILValueCategory)ListOfValues[I+1])));
    }
    ResultVal = Builder.createMarkFunctionEscape(Loc, OpList);
    break;
  }
  case ValueKind::MetatypeInst: {
    auto Ty = MF->getType(TyID);
    ResultVal = Builder.createMetatype(Loc,
                            getSILType(Ty, (SILValueCategory)TyCategory));
    break;
  }
  case ValueKind::ModuleInst: {
    // Has IdentifierID for the module reference. Use SILOneTypeLayout.
    auto Mod = MF->getModule(MF->getIdentifier(TyID));
    ResultVal = Builder.createModule(Loc,
                    getSILType(ModuleType::get(Mod), SILValueCategory::Object));
    break;
  }
  case ValueKind::ProjectExistentialInst:
  case ValueKind::ProjectExistentialRefInst: {
    auto Ty = MF->getType(TyID);
    auto Ty2 = MF->getType(TyID2);
    if ((ValueKind)OpCode == ValueKind::ProjectExistentialInst)
      ResultVal = Builder.createProjectExistential(Loc,
                      getLocalValue(ValID, ValResNum,
                           getSILType(Ty2, (SILValueCategory)TyCategory2)),
                      getSILType(Ty, (SILValueCategory)TyCategory));
    else
      ResultVal = Builder.createProjectExistentialRef(Loc,
                      getLocalValue(ValID, ValResNum,
                           getSILType(Ty2, (SILValueCategory)TyCategory2)),
                      getSILType(Ty, (SILValueCategory)TyCategory));
    break;
  }
  // Conversion instructions.
  case ValueKind::RefToObjectPointerInst:
  case ValueKind::UpcastInst:
  case ValueKind::CoerceInst:
  case ValueKind::AddressToPointerInst:
  case ValueKind::PointerToAddressInst:
  case ValueKind::ObjectPointerToRefInst:
  case ValueKind::RefToRawPointerInst:
  case ValueKind::RawPointerToRefInst:
  case ValueKind::RefToUnownedInst:
  case ValueKind::UnownedToRefInst:
  case ValueKind::ConvertCCInst:
  case ValueKind::ThinToThickFunctionInst:
  case ValueKind::BridgeToBlockInst:
  case ValueKind::ArchetypeRefToSuperInst:
  case ValueKind::ConvertFunctionInst:
  case ValueKind::UpcastExistentialRefInst: {
    auto Ty = MF->getType(TyID);
    auto Ty2 = MF->getType(TyID2);
    auto Val = getLocalValue(ValID, ValResNum,
                             getSILType(Ty2, (SILValueCategory)TyCategory2));
    auto SILTy = getSILType(Ty, (SILValueCategory)TyCategory);
    switch ((ValueKind)OpCode) {
    default: assert(0 && "Out of sync with parent switch");
    case ValueKind::UpcastInst:
      ResultVal = Builder.createUpcast(Loc, Val, SILTy);
      break;
    case ValueKind::CoerceInst:
      ResultVal = Builder.createCoerce(Loc, Val, SILTy);
      break;
    case ValueKind::AddressToPointerInst:
      ResultVal = Builder.createAddressToPointer(Loc, Val, SILTy);
      break;
    case ValueKind::RefToUnownedInst:
      ResultVal = Builder.createRefToUnowned(Loc, Val, SILTy);
      break;
    case ValueKind::UnownedToRefInst:
      ResultVal = Builder.createUnownedToRef(Loc, Val, SILTy);
      break;
    case ValueKind::PointerToAddressInst:
      ResultVal = Builder.createPointerToAddress(Loc, Val, SILTy);
      break;
    case ValueKind::RefToObjectPointerInst:
      ResultVal = Builder.createRefToObjectPointer(Loc, Val, SILTy);
      break;
    case ValueKind::RefToRawPointerInst:
      ResultVal = Builder.createRefToRawPointer(Loc, Val, SILTy);
      break;
    case ValueKind::RawPointerToRefInst:
      ResultVal = Builder.createRawPointerToRef(Loc, Val, SILTy);
      break;
    case ValueKind::ObjectPointerToRefInst:
      ResultVal = Builder.createObjectPointerToRef(Loc, Val, SILTy);
      break;
    case ValueKind::ConvertCCInst:
      ResultVal = Builder.createConvertCC(Loc, Val, SILTy);
      break;
    case ValueKind::ThinToThickFunctionInst:
      ResultVal = Builder.createThinToThickFunction(Loc, Val, SILTy);
      break;
    case ValueKind::BridgeToBlockInst:
      ResultVal = Builder.createBridgeToBlock(Loc, Val, SILTy);
      break;
    case ValueKind::ArchetypeRefToSuperInst:
      ResultVal = Builder.createArchetypeRefToSuper(Loc, Val, SILTy);
      break;
    case ValueKind::ConvertFunctionInst:
      ResultVal = Builder.createConvertFunction(Loc, Val, SILTy);
      break;
    case ValueKind::UpcastExistentialRefInst:
      ResultVal = Builder.createUpcastExistentialRef(Loc, Val, SILTy);
      break;
    }
    break;
  }
  // Checked Conversion instructions.
  case ValueKind::DowncastInst:
  case ValueKind::SuperToArchetypeRefInst:
  case ValueKind::DowncastArchetypeAddrInst:
  case ValueKind::DowncastArchetypeRefInst:
  case ValueKind::ProjectDowncastExistentialAddrInst:
  case ValueKind::DowncastExistentialRefInst: {
    SILValue Val = getLocalValue(ValID, ValResNum,
                       getSILType(MF->getType(TyID2),
                                  (SILValueCategory)TyCategory2));
    SILType Ty = getSILType(MF->getType(TyID), (SILValueCategory)TyCategory);
    CheckedCastMode Mode;
    if (Attr == (unsigned)CheckedCastMode::Unconditional)
      Mode = CheckedCastMode::Unconditional;
    else if (Attr == (unsigned)CheckedCastMode::Conditional)
      Mode = CheckedCastMode::Conditional;
    else
      llvm_unreachable("Not an valid CheckedCastMode");
    switch ((ValueKind)OpCode) {
    default: assert(0 && "Out of sync with parent switch");
    case ValueKind::DowncastInst:
      ResultVal = Builder.createDowncast(Loc, Val, Ty, Mode);
      break;
    case ValueKind::SuperToArchetypeRefInst:
      ResultVal = Builder.createSuperToArchetypeRef(Loc, Val, Ty, Mode);
      break;
    case ValueKind::DowncastArchetypeAddrInst:
      ResultVal = Builder.createDowncastArchetypeAddr(Loc, Val, Ty, Mode);
      break;
    case ValueKind::DowncastArchetypeRefInst:
      ResultVal = Builder.createDowncastArchetypeRef(Loc, Val, Ty, Mode);
      break;
    case ValueKind::ProjectDowncastExistentialAddrInst:
      ResultVal = Builder.createProjectDowncastExistentialAddr(Loc,
                                                         Val, Ty, Mode);
      break;
    case ValueKind::DowncastExistentialRefInst:
      ResultVal = Builder.createDowncastExistentialRef(Loc, Val, Ty, Mode);
      break;
    }
    break;
  }
#define UNARY_INSTRUCTION_HELPER(ID, CREATOR) \
  case ValueKind::ID##Inst:                   \
    ResultVal = Builder.CREATOR(Loc, getLocalValue(ValID, ValResNum,  \
                    getSILType(MF->getType(TyID),                     \
                               (SILValueCategory)TyCategory)));       \
    break;
#define UNARY_INSTRUCTION(ID) UNARY_INSTRUCTION_HELPER(ID, create##ID)
  UNARY_INSTRUCTION(CopyValue)
  UNARY_INSTRUCTION(DestroyValue)
  UNARY_INSTRUCTION(DeinitExistential)
  UNARY_INSTRUCTION(DestroyAddr)
  UNARY_INSTRUCTION(IsNonnull)
  UNARY_INSTRUCTION(Load)
  UNARY_INSTRUCTION(MarkUninitialized)
  UNARY_INSTRUCTION(Return)
  UNARY_INSTRUCTION_HELPER(StrongRetain, createStrongRetainInst)
  UNARY_INSTRUCTION_HELPER(StrongRelease, createStrongReleaseInst)
  UNARY_INSTRUCTION(StrongRetainAutoreleased)
  UNARY_INSTRUCTION(AutoreleaseReturn)
  UNARY_INSTRUCTION(StrongRetainUnowned)
  UNARY_INSTRUCTION(UnownedRetain)
  UNARY_INSTRUCTION(UnownedRelease)
#undef UNARY_INSTRUCTION
#undef UNARY_INSTRUCTION_HELPER

  case ValueKind::LoadWeakInst: {
    auto Ty = MF->getType(TyID);
    bool isTake = (Attr > 0);
    ResultVal = Builder.createLoadWeak(Loc,
        getLocalValue(ValID, ValResNum,
                      getSILType(Ty, (SILValueCategory)TyCategory)),
        IsTake_t(isTake));
    break;
  }
  case ValueKind::InitializeVarInst: {
    auto Ty = MF->getType(TyID);
    bool canDefault = (Attr > 0);
    ResultVal = Builder.createInitializeVar(Loc,
        getLocalValue(ValID, ValResNum,
                      getSILType(Ty, (SILValueCategory)TyCategory)),
        canDefault);
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
  case ValueKind::StoreWeakInst: {
    auto Ty = MF->getType(TyID);
    SILType addrType = getSILType(Ty, (SILValueCategory)TyCategory);
    auto refType = addrType.getAs<WeakStorageType>();
    auto ValType = SILType::getPrimitiveObjectType(refType.getReferentType());
    bool isInit = (Attr > 0);
    ResultVal = Builder.createStoreWeak(Loc,
                    getLocalValue(ValID, ValResNum, ValType),
                    getLocalValue(ValID2, ValResNum2, addrType),
                    IsInitialization_t(isInit));
    break;
  }
  case ValueKind::CopyAddrInst: {
    auto Ty = MF->getType(TyID);
    SILType addrType = getSILType(Ty, (SILValueCategory)TyCategory);
    auto refType = addrType.getAs<WeakStorageType>();
    auto ValType = SILType::getPrimitiveObjectType(refType.getReferentType());
    bool isInit = (Attr & 0x2) > 0;
    bool isTake = (Attr & 0x1) > 0;
    ResultVal = Builder.createCopyAddr(Loc,
                    getLocalValue(ValID, ValResNum, ValType),
                    getLocalValue(ValID2, ValResNum2, addrType),
                    IsTake_t(isTake),
                    IsInitialization_t(isInit));
    break;
  }
  case ValueKind::AssignInst: {
    auto Ty = MF->getType(TyID);
    SILType addrType = getSILType(Ty, (SILValueCategory)TyCategory);
    SILType ValType = addrType.getObjectType();
    ResultVal = Builder.createAssign(Loc,
                    getLocalValue(ValID, ValResNum, ValType),
                    getLocalValue(ValID2, ValResNum2, addrType));
    break;
  }
  case ValueKind::StructElementAddrInst: {
    // Use SILOneValueOneOperandLayout.
    VarDecl *Field = cast<VarDecl>(MF->getDecl(ValID));
    auto Ty = MF->getType(TyID);
    ResultVal = Builder.createStructElementAddr(Loc,
                    getLocalValue(ValID2, ValResNum2,
                                  getSILType(Ty, (SILValueCategory)TyCategory)),
                    Field,
                    getSILType(Field->getType(), SILValueCategory::Address));
    break;
  }
  case ValueKind::StructExtractInst: {
    // Use SILOneValueOneOperandLayout.
    VarDecl *Field = cast<VarDecl>(MF->getDecl(ValID));
    auto Ty = MF->getType(TyID);
    ResultVal = Builder.createStructExtract(Loc,
                    getLocalValue(ValID2, ValResNum2,
                                  getSILType(Ty, (SILValueCategory)TyCategory)),
                    Field,
                    getSILType(Field->getType(), SILValueCategory::Object));
    break;
  }
  case ValueKind::StructInst: {
    // Format: a type followed by a list of typed values. A typed value is
    // expressed by 4 IDs: TypeID, TypeCategory, ValueID, ValueResultNumber.
    auto Ty = MF->getType(TyID);
    SmallVector<SILValue, 4> OpList;
    for (unsigned I = 0, E = ListOfValues.size(); I < E; I += 4) {
      auto EltTy = MF->getType(ListOfValues[I]); 
      OpList.push_back(
        getLocalValue(ListOfValues[I+2], ListOfValues[I+3],
                      getSILType(EltTy, (SILValueCategory)ListOfValues[I+1])));
    }
    ResultVal = Builder.createStruct(Loc,
                    getSILType(Ty, (SILValueCategory)TyCategory),
                    OpList);
    break;
  }
  case ValueKind::TupleElementAddrInst:
  case ValueKind::TupleExtractInst: {
    // Use OneTypeOneOperand layout where the field number is stored in TypeID.
    auto Ty2 = MF->getType(TyID2);
    SILType ST = getSILType(Ty2, (SILValueCategory)TyCategory2);
    TupleType *TT = ST.getAs<TupleType>();

    auto ResultTy = TT->getFields()[TyID].getType();
    switch ((ValueKind)OpCode) {
    default: assert(0 && "Out of sync with parent switch");
    case ValueKind::TupleElementAddrInst:
      ResultVal = Builder.createTupleElementAddr(Loc,
                      getLocalValue(ValID, ValResNum, ST),
                      TyID, getSILType(ResultTy, SILValueCategory::Address));
      break;
    case ValueKind::TupleExtractInst:
      ResultVal = Builder.createTupleExtract(Loc,
                      getLocalValue(ValID, ValResNum, ST),
                      TyID,
                      getSILType(ResultTy, SILValueCategory::Object)).getDef();
      break;
    }
    break;
  }
  case ValueKind::TupleInst: {
    // Format: a type followed by a list of values. A value is expressed by
    // 2 IDs: ValueID, ValueResultNumber.
    auto Ty = MF->getType(TyID);
    TupleType *TT = Ty->getAs<TupleType>();
    assert(TT && "Type of a TupleInst should be TupleType");
    SmallVector<SILValue, 4> OpList;
    for (unsigned I = 0, E = ListOfValues.size(); I < E; I += 2) {
      Type EltTy = TT->getFields()[I >> 1].getType();
      OpList.push_back(
        getLocalValue(ListOfValues[I], ListOfValues[I+1],
                      getSILType(EltTy, SILValueCategory::Object)));
    }
    ResultVal = Builder.createTuple(Loc,
                    getSILType(Ty, (SILValueCategory)TyCategory),
                    OpList);
    break;
  }
  case ValueKind::BranchInst: {
    SmallVector<SILValue, 4> Args;
    for (unsigned I = 0, E = ListOfValues.size(); I < E; I += 4)
      Args.push_back(
        getLocalValue(ListOfValues[I+2], ListOfValues[I+3],
                      getSILType(MF->getType(ListOfValues[I]),
                                 (SILValueCategory)ListOfValues[I+1])));

    ResultVal = Builder.createBranch(Loc, getBBForReference(Fn, TyID),
                    Args);
    break;
  }
  case ValueKind::CondBranchInst: {
    // Format: condition, true basic block ID, a list of arguments, false basic
    // block ID, a list of arguments. Use SILOneTypeValuesLayout: the type is
    // for condition, the list has value for condition, true basic block ID,
    // false basic block ID, number of true arguments, and a list of true|false
    // arguments.
    SILValue Cond = getLocalValue(ListOfValues[0], ListOfValues[1],
                                  getSILType(MF->getType(TyID),
                                             (SILValueCategory)TyCategory));

    unsigned NumTrueArgs = ListOfValues[4];
    unsigned StartOfTrueArg = 5;
    unsigned StartOfFalseArg = StartOfTrueArg + 4*NumTrueArgs;
    SmallVector<SILValue, 4> TrueArgs;
    for (unsigned I = StartOfTrueArg, E = StartOfFalseArg; I < E; I += 4)
      TrueArgs.push_back(
        getLocalValue(ListOfValues[I+2], ListOfValues[I+3],
                      getSILType(MF->getType(ListOfValues[I]), 
                                 (SILValueCategory)ListOfValues[I+1])));

    SmallVector<SILValue, 4> FalseArgs;
    for (unsigned I = StartOfFalseArg, E = ListOfValues.size(); I < E; I += 4)
      FalseArgs.push_back(
        getLocalValue(ListOfValues[I+2], ListOfValues[I+3],
                      getSILType(MF->getType(ListOfValues[I]),
                                 (SILValueCategory)ListOfValues[I+1])));

    ResultVal = Builder.createCondBranch(Loc, Cond,
                    getBBForReference(Fn, ListOfValues[2]), TrueArgs,
                    getBBForReference(Fn, ListOfValues[3]), FalseArgs);
    break;
  }
  case ValueKind::SwitchEnumInst:
  case ValueKind::DestructiveSwitchEnumAddrInst: {
    // Format: condition, a list of cases (EnumElementDecl + Basic Block ID),
    // default basic block ID. Use SILOneTypeValuesLayout: the type is
    // for condition, the list has value for condition, hasDefault, default
    // basic block ID, a list of (DeclID, BasicBlock ID).
    SILValue Cond = getLocalValue(ListOfValues[0], ListOfValues[1],
                                  getSILType(MF->getType(TyID),
                                             (SILValueCategory)TyCategory));

    SILBasicBlock *DefaultBB = nullptr;
    if (ListOfValues[2])
      DefaultBB = getBBForReference(Fn, ListOfValues[3]);

    SmallVector<std::pair<EnumElementDecl*, SILBasicBlock*>, 4> CaseBBs;
    for (unsigned I = 4, E = ListOfValues.size(); I < E; I += 2) {
      CaseBBs.push_back( {cast<EnumElementDecl>(MF->getDecl(ListOfValues[I])),
                            getBBForReference(Fn, ListOfValues[I+1])} );
    }
    if ((ValueKind)OpCode == ValueKind::SwitchEnumInst)
      ResultVal = Builder.createSwitchEnum(Loc, Cond, DefaultBB, CaseBBs);
    else
      ResultVal = Builder.createDestructiveSwitchEnumAddr(Loc, Cond,
                      DefaultBB, CaseBBs);
    break;
  }
  case ValueKind::SwitchIntInst: {
    // Format: condition, a list of cases (APInt + Basic Block ID),
    // default basic block ID. Use SILOneTypeValuesLayout: the type is
    // for condition, the list contains value for condition, hasDefault, default
    // basic block ID, a list of (APInt(Identifier ID), BasicBlock ID).
    SILValue Cond = getLocalValue(ListOfValues[0], ListOfValues[1],
                                  getSILType(MF->getType(TyID),
                                             (SILValueCategory)TyCategory));

    SILBasicBlock *DefaultBB = nullptr;
    if (ListOfValues[2])
      DefaultBB = getBBForReference(Fn, ListOfValues[3]);

    SmallVector<std::pair<APInt, SILBasicBlock*>, 4> CaseBBs;
    for (unsigned I = 4, E = ListOfValues.size(); I < E; I += 2) {
      auto intTy = Cond.getType().getAs<BuiltinIntegerType>();
      // Build APInt from string.
      Identifier StringVal = MF->getIdentifier(ListOfValues[I]);
      APInt value(intTy->getBitWidth(), StringVal.str(), 10);
      CaseBBs.push_back( {value, getBBForReference(Fn, ListOfValues[I+1])} );
    }
    ResultVal = Builder.createSwitchInt(Loc, Cond, DefaultBB, CaseBBs);
    break;
  }
  case ValueKind::EnumInst: {
    // Format: a type, an operand and a decl ID. Use SILTwoOperandsLayout: type,
    // (DeclID + hasOperand), and an operand.
    SILValue Operand;
    if (ValResNum)
      Operand = getLocalValue(ValID2, ValResNum2,
                    getSILType(MF->getType(TyID2),
                               (SILValueCategory)TyCategory2));
    ResultVal = Builder.createEnum(Loc, Operand,
                                    cast<EnumElementDecl>(MF->getDecl(ValID)),
                                    getSILType(MF->getType(TyID),
                                               (SILValueCategory)TyCategory));
    break;
  }
  case ValueKind::EnumDataAddrInst: {
    // Use SILOneValueOneOperandLayout.
    EnumElementDecl *Elt = cast<EnumElementDecl>(MF->getDecl(ValID));
    auto OperandTy = MF->getType(TyID);
    auto ResultTy = OperandTy->getTypeOfMember(Elt->getModuleContext(),
                                               Elt,
                                               nullptr,
                                               Elt->getArgumentType());
    ResultVal = Builder.createEnumDataAddr(Loc,
                    getLocalValue(ValID2, ValResNum2,
                                  getSILType(OperandTy,
                                             (SILValueCategory)TyCategory)),
                    Elt,
                    getSILType(ResultTy, SILValueCategory::Address));
    break;
  }
  case ValueKind::InjectEnumAddrInst: {
    // Use SILOneValueOneOperandLayout.
    EnumElementDecl *Elt = cast<EnumElementDecl>(MF->getDecl(ValID));
    auto Ty = MF->getType(TyID);
    ResultVal = Builder.createInjectEnumAddr(Loc,
                    getLocalValue(ValID2, ValResNum2,
                                  getSILType(Ty, (SILValueCategory)TyCategory)),
                    Elt);
    break;
  }
  case ValueKind::RefElementAddrInst: {
    // Use SILOneValueOneOperandLayout.
    VarDecl *Field = cast<VarDecl>(MF->getDecl(ValID));
    auto OperandTy = MF->getType(TyID);
    ResultVal = Builder.createRefElementAddr(Loc,
                    getLocalValue(ValID2, ValResNum2,
                                  getSILType(OperandTy,
                                             (SILValueCategory)TyCategory)),
                    Field,
                    getSILType(Field->getType(), SILValueCategory::Address));
    break;
  }
  case ValueKind::ArchetypeMethodInst:
  case ValueKind::ProtocolMethodInst:
  case ValueKind::ClassMethodInst:
  case ValueKind::SuperMethodInst:
  case ValueKind::DynamicMethodInst: {
    // Format: a type, an operand and a SILDeclRef. Use SILOneTypeValuesLayout:
    // type, Attr, SILDeclRef (DeclID, Kind, uncurryLevel, IsObjC),
    // and an operand.
    assert(ListOfValues.size() >= 7 &&
           "Expect at least 7 numbers for MethodInst");
    SILDeclRef DRef(cast<ValueDecl>(MF->getDecl(ListOfValues[1])),
                    (SILDeclRef::Kind)ListOfValues[2],
                    ListOfValues[3], ListOfValues[4] > 0);
    SILType Ty = getSILType(MF->getType(TyID), (SILValueCategory)TyCategory);
    SILType operandTy = getSILType(MF->getType(ListOfValues[5]),
                                   (SILValueCategory)ListOfValues[6]);
    bool IsVolatile = ListOfValues[0] > 0;
    switch ((ValueKind)OpCode) {
    default: assert(0 && "Out of sync with parent switch");
    case ValueKind::ArchetypeMethodInst:
      ResultVal = Builder.createArchetypeMethod(Loc, Ty, DRef, 
                    operandTy, IsVolatile);
      break;
    case ValueKind::ProtocolMethodInst:
      ResultVal = Builder.createProtocolMethod(Loc,
                    getLocalValue(ListOfValues[7], ListOfValues[8], operandTy),
                    DRef, Ty, IsVolatile);
      break;
    case ValueKind::ClassMethodInst:
      ResultVal = Builder.createClassMethod(Loc,
                    getLocalValue(ListOfValues[7], ListOfValues[8], operandTy),
                    DRef, Ty, IsVolatile);
      break;
    case ValueKind::SuperMethodInst:
      ResultVal = Builder.createSuperMethod(Loc,
                    getLocalValue(ListOfValues[7], ListOfValues[8], operandTy),
                    DRef, Ty, IsVolatile);
      break;
    case ValueKind::DynamicMethodInst:
      ResultVal = Builder.createDynamicMethod(Loc,
                    getLocalValue(ListOfValues[7], ListOfValues[8], operandTy),
                    DRef, Ty, IsVolatile);
      break;
    }
    break;
  }
  case ValueKind::DynamicMethodBranchInst: {
    // Format: a typed value, a SILDeclRef, a BasicBlock ID for method,
    // a BasicBlock ID for no method. Use SILOneTypeValuesLayout.
    assert(ListOfValues.size() == 8 &&
           "Expect 8 numbers for DynamicMethodBranchInst");
    SILDeclRef DRef(cast<ValueDecl>(MF->getDecl(ListOfValues[2])),
                    (SILDeclRef::Kind)ListOfValues[3],
                    ListOfValues[4], ListOfValues[5] > 0);
    ResultVal = Builder.createDynamicMethodBranch(Loc,
                    getLocalValue(ListOfValues[0], ListOfValues[1],
                                  getSILType(MF->getType(TyID),
                                             (SILValueCategory)TyCategory)),
                    DRef, getBBForReference(Fn, ListOfValues[6]),
                    getBBForReference(Fn, ListOfValues[7]));
    break;
  }
  case ValueKind::SpecializeInst: {
    // Format: a typed value, a type, a list of substitutions (Archetype name,
    // Replacement type). Use SILOneTypeValuesLayout.
    assert(ListOfValues.size() == 5 && "Expect 5 numbers for SpecializeInst");
    unsigned NumSub = ListOfValues[4];
    // Read the substitutions.
    SmallVector<Substitution, 4> Substitutions;
    while (NumSub--) {
      auto sub = MF->maybeReadSubstitution(SILCursor);
      assert(sub.hasValue() && "Missing substitution?");
      Substitutions.push_back(*sub);
    }

    auto ValTy = MF->getType(ListOfValues[0]);
    assert(ValTy->is<PolymorphicFunctionType>() &&
           "Should be a polymorphic function");
    ResultVal = Builder.createSpecialize(Loc,
                  getLocalValue(ListOfValues[2], ListOfValues[3],
                                getSILType(ValTy,
                                           (SILValueCategory)ListOfValues[1])),
                  Ctx.AllocateCopy(Substitutions),
                  getSILType(MF->getType(TyID), (SILValueCategory)TyCategory));
    break;
  }
  case ValueKind::UnreachableInst: {
    ResultVal = Builder.createUnreachable(Loc);
    break;
  }
  }
  if (ResultVal->hasValue())
    setLocalValue(ResultVal, ++LastValueID);
  return false;
}

SILFunction *SILDeserializer::lookupSILFunction(SILFunction *InFunc) {
  Identifier name = Ctx.getIdentifier(InFunc->getName());
  if (!FuncTable)
    return nullptr;
  auto iter = FuncTable->find(name);
  if (iter == FuncTable->end())
    return nullptr;

  auto Func = readSILFunction(*iter, InFunc);
  if (Func)
    DEBUG(llvm::dbgs() << "Deserialize SIL:\n";
          Func->dump());
  return Func;
}

SILDeserializer::~SILDeserializer() = default;
