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
#include "swift/SIL/SILUndef.h"
#include "swift/Serialization/BCReadingExtras.h"

// This is a template-only header; eventually it should move to llvm/Support.
#include "clang/Basic/OnDiskHashTable.h"

#include "llvm/ADT/StringExtras.h"
#include "llvm/Support/Debug.h"

using namespace swift;
using namespace swift::serialization;
using namespace swift::serialization::sil_block;

static Optional<StringLiteralInst::Encoding>
fromStableStringEncoding(unsigned value) {
  switch (value) {
  case SIL_UTF8: return StringLiteralInst::Encoding::UTF8;
  case SIL_UTF16: return StringLiteralInst::Encoding::UTF16;
  default: return Nothing;
  }
}

static Optional<SILLinkage>
fromStableSILLinkage(unsigned value) {
  switch (value) {
  case SIL_LINKAGE_PUBLIC: return SILLinkage::Public;
  case SIL_LINKAGE_HIDDEN: return SILLinkage::Hidden;
  case SIL_LINKAGE_SHARED: return SILLinkage::Shared;
  case SIL_LINKAGE_PRIVATE: return SILLinkage::Private;
  case SIL_LINKAGE_PUBLIC_EXTERNAL: return SILLinkage::PublicExternal;
  case SIL_LINKAGE_HIDDEN_EXTERNAL: return SILLinkage::HiddenExternal;
  default: return Nothing;
  }
}

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
                                 ASTContext &Ctx,
                                 SerializedSILLoader::Callback *callback)
    : MF(MF), SILMod(M), Ctx(Ctx), Callback(callback) {

  SILCursor = MF->getSILCursor();
  SILIndexCursor = MF->getSILIndexCursor();
  // Early return if either sil block or sil index block does not exist.
  if (!SILCursor.getBitStreamReader() || !SILIndexCursor.getBitStreamReader())
    return;

  // Load any abbrev records at the start of the block.
  SILCursor.advance();

  llvm::BitstreamCursor cursor = SILIndexCursor;
  // We expect SIL_FUNC_NAMES first, then SIL_VTABLE_NAMES, then
  // SIL_GLOBALVAR_NAMES. But each one can be omitted if no entries exist
  // in the module file.
  unsigned kind = 0;
  while (kind != sil_index_block::SIL_GLOBALVAR_NAMES) {
    auto next = cursor.advance();
    if (next.Kind == llvm::BitstreamEntry::EndBlock)
      return;

    SmallVector<uint64_t, 4> scratch;
    StringRef blobData;
    unsigned prevKind = kind;
    kind = cursor.readRecord(next.ID, scratch, &blobData);
    assert((next.Kind == llvm::BitstreamEntry::Record &&
            kind > prevKind &&
            (kind == sil_index_block::SIL_FUNC_NAMES ||
             kind == sil_index_block::SIL_VTABLE_NAMES ||
             kind == sil_index_block::SIL_GLOBALVAR_NAMES)) &&
         "Expect SIL_FUNC_NAMES, SIL_VTABLE_NAMES, or SIL_GLOBALVAR_NAMES.");
    (void)prevKind;

    if (kind == sil_index_block::SIL_FUNC_NAMES)
      FuncTable = readFuncTable(scratch, blobData);
    else if (kind == sil_index_block::SIL_VTABLE_NAMES)
      VTableList = readFuncTable(scratch, blobData);
    else if (kind == sil_index_block::SIL_GLOBALVAR_NAMES)
      GlobalVarList = readFuncTable(scratch, blobData);

    // Read SIL_FUNC|VTABLE|GLOBALVAR_OFFSETS record.
    next = cursor.advance();
    scratch.clear();
    unsigned offKind = cursor.readRecord(next.ID, scratch, &blobData);
    (void)offKind;
    if (kind == sil_index_block::SIL_FUNC_NAMES) {
      assert((next.Kind == llvm::BitstreamEntry::Record &&
              offKind == sil_index_block::SIL_FUNC_OFFSETS) &&
             "Expect a SIL_FUNC_OFFSETS record.");
      Funcs.assign(scratch.begin(), scratch.end());
    } else if (kind == sil_index_block::SIL_VTABLE_NAMES) {
      assert((next.Kind == llvm::BitstreamEntry::Record &&
              offKind == sil_index_block::SIL_VTABLE_OFFSETS) &&
             "Expect a SIL_VTABLE_OFFSETS record.");
      VTables.assign(scratch.begin(), scratch.end());
    } else if (kind == sil_index_block::SIL_GLOBALVAR_NAMES) {
      assert((next.Kind == llvm::BitstreamEntry::Record &&
              offKind == sil_index_block::SIL_GLOBALVAR_OFFSETS) &&
             "Expect a SIL_GLOBALVAR_OFFSETS record.");
      GlobalVars.assign(scratch.begin(), scratch.end());
    }
  }
}

std::unique_ptr<SILDeserializer::SerializedFuncTable>
SILDeserializer::readFuncTable(ArrayRef<uint64_t> fields, StringRef blobData) {
  uint32_t tableOffset;
  sil_index_block::ListLayout::readRecord(fields, tableOffset);
  auto base = reinterpret_cast<const uint8_t *>(blobData.data());

  using OwnedTable = std::unique_ptr<SerializedFuncTable>;
  return OwnedTable(SerializedFuncTable::Create(base + tableOffset, base));
}

/// A high-level overview of how forward references work in serializer and
/// deserializer:
/// In serializer, we pre-assign a value ID in order, to each basic block
/// argument and each SILInstruction that has a value.
/// In deserializer, we use LocalValues to store the definitions and
/// ForwardMRVLocalValues for forward-referenced values (values that are
/// used but not yet defined). LocalValues are updated in setLocalValue where
/// the ID passed in assumes the same ordering as in serializer: in-order
/// for each basic block argument and each SILInstruction that has a value.
/// We update ForwardMRVLocalValues in getLocalValue and when a value is defined
/// in setLocalValue, the corresponding entry in ForwardMRVLocalValues will be
/// erased.
void SILDeserializer::setLocalValue(ValueBase *Value, ValueID Id) {
  ValueBase *&Entry = LocalValues[Id];
  assert(!Entry && "We should not redefine the same value.");

  auto It = ForwardMRVLocalValues.find(Id);
  if (It != ForwardMRVLocalValues.end()) {
    // Take the information about the forward ref out of the map.
    std::vector<SILValue> Entries = std::move(It->second);

    // Remove the entries from the map.
    ForwardMRVLocalValues.erase(It);

    assert(Entries.size() <= Value->getTypes().size() &&
           "Value Type mismatch?");
    // Validate that any forward-referenced elements have the right type, and
    // RAUW them.
    for (unsigned i = 0, e = Entries.size(); i != e; ++i) {
      if (!Entries[i]) continue;

      assert(Entries[i]->getType(0) == Value->getType(i) &&
             "Value Type mismatch?");
      Entries[i].replaceAllUsesWith(SILValue(Value, i));
    }
  }

  // Store it in our map.
  Entry = Value;
}

SILValue SILDeserializer::getLocalValue(ValueID Id, unsigned ResultNum,
                                        SILType Type) {
  if (Id == 0)
    return SILUndef::get(Type, &SILMod);

  // Check to see if this is already defined.
  ValueBase *Entry = LocalValues.lookup(Id);
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

/// Helper function to create a bogus SILFunction to appease error paths.
static SILFunction *createBogusSILFunction(SILModule &M,
                                           Identifier name,
                                           SILType type) {
  SourceLoc loc;
  return SILFunction::create(M, SILLinkage::Private, name.str(),
                             type.castTo<SILFunctionType>(),
                             SILFileLocation(loc));
}

/// Helper function to find a SILFunction, given its name and type.
SILFunction *SILDeserializer::getFuncForReference(Identifier name,
                                                  SILType type) {
  // Check to see if we have a function by this name already.
  SILFunction *fn = SILMod.lookUpFunction(name.str());
  if (!fn) {
    // Otherwise, look for a function with this name in the module.
    auto iter = FuncTable->find(name);
    if (iter != FuncTable->end()) {
      fn = readSILFunction(*iter, nullptr, name, /*declarationOnly*/ true);
    }
  }

  // FIXME: check for matching types.

  // Always return something of the right type.
  if (!fn) fn = createBogusSILFunction(SILMod, name, type);
  return fn;
}

/// Deserialize a SILFunction if it is not already deserialized. The input
/// SILFunction can either be an empty declaration or null. If it is an empty
/// declaration, we fill in the contents. If the input SILFunction is
/// null, we create a SILFunction.
SILFunction *SILDeserializer::readSILFunction(DeclID FID,
                                              SILFunction *existingFn,
                                              Identifier name,
                                              bool declarationOnly) {
  if (FID == 0)
    return nullptr;
  assert(FID <= Funcs.size() && "invalid SILFunction ID");

  auto &cacheEntry = Funcs[FID-1];
  if (cacheEntry.isFullyDeserialized() ||
      (cacheEntry.isDeserialized() && declarationOnly))
    return cacheEntry.get(); 

  BCOffsetRAII restoreOffset(SILCursor);
  SILCursor.JumpToBit(cacheEntry.getOffset());

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

  TypeID funcTyID;
  unsigned rawLinkage, isTransparent;
  SILFunctionLayout::readRecord(scratch, rawLinkage, isTransparent, funcTyID);

  if (funcTyID == 0) {
    DEBUG(llvm::dbgs() << "SILFunction typeID is 0.\n");
    return nullptr;
  }
  auto ty = getSILType(MF->getType(funcTyID), SILValueCategory::Object);
  if (!ty.is<SILFunctionType>()) {
    DEBUG(llvm::dbgs() << "not a function type for SILFunction\n");
    return nullptr;
  }

  auto linkage = fromStableSILLinkage(rawLinkage);
  if (!linkage) {
    DEBUG(llvm::dbgs() << "invalid linkage code " << rawLinkage
                       << " for SILFunction\n");
    return nullptr;
  }

  // If we weren't handed a function, check for an existing
  // declaration in the output module.
  if (!existingFn) existingFn = SILMod.lookUpFunction(name.str());
  auto fn = existingFn;

  // TODO: use the correct SILLocation from module.
  SILLocation loc = SILFileLocation(SourceLoc());

  // If we have an existing function, verify that the types match up.
  if (fn) {
    if (fn->getLoweredType() != ty) {
      DEBUG(llvm::dbgs() << "SILFunction type mismatch.\n");
      return nullptr;
    }

    // Don't override the transparency or linkage of a function with
    // an existing declaration.

  // Otherwise, create a new function.
  } else {
    fn = SILFunction::create(SILMod, linkage.getValue(), name.str(),
                             ty.castTo<SILFunctionType>(), loc);
    fn->setTransparent(IsTransparent_t(isTransparent == 1));

    if (Callback) Callback->didDeserialize(MF->getAssociatedModule(), fn);
  }

  assert(fn->empty() &&
         "SILFunction to be deserialized starts being empty.");

  fn->setBare(IsBare);
  if (!fn->hasLocation()) fn->setLocation(loc);

  // If the next entry is the end of the block, then this function has
  // no contents.
  entry = SILCursor.advance(AF_DontPopBlockAtEnd);
  if (entry.Kind == llvm::BitstreamEntry::EndBlock) {
    cacheEntry.set(fn, /*fully deserialized*/ true);
    return fn;
  }

  // Stop here if we're just supposed to parse a declaration.
  if (declarationOnly) {
    cacheEntry.set(fn, /*fully deserialized*/ false);
    return fn;
  }

  // Set the cache entry now in order to properly handle both forward
  // declarations and deserialization errors.
  cacheEntry.set(fn, /*fully deserialized*/ true);

  scratch.clear();
  kind = SILCursor.readRecord(entry.ID, scratch);

  SILBasicBlock *CurrentBB = nullptr;

  // Clear up at the beginning of each SILFunction.
  BasicBlockID = 0;
  BlocksByID.clear();
  UndefinedBlocks.clear();
  LastValueID = 0;
  LocalValues.clear();
  ForwardMRVLocalValues.clear();

  // Another SIL_FUNCTION record means the end of this SILFunction.
  // SIL_VTABLE or SIL_GLOBALVAR record also means the end of this SILFunction.
  while (kind != SIL_FUNCTION && kind != SIL_VTABLE && kind != SIL_GLOBALVAR) {
    if (kind == SIL_BASIC_BLOCK)
      // Handle a SILBasicBlock record.
      CurrentBB = readSILBasicBlock(fn, scratch);
    else {
      // Handle a SILInstruction record.
      if (readSILInstruction(fn, CurrentBB, kind, scratch)) {
        DEBUG(llvm::dbgs() << "readSILInstruction returns error.\n");
        return fn;
      }
    }

    // Fetch the next record.
    scratch.clear();
    entry = SILCursor.advance(AF_DontPopBlockAtEnd);

    // EndBlock means the end of this SILFunction.
    if (entry.Kind == llvm::BitstreamEntry::EndBlock)
      break;
    kind = SILCursor.readRecord(entry.ID, scratch);
  }

  if (Callback) Callback->didDeserializeBody(MF->getAssociatedModule(), fn);

  cacheEntry.set(fn, /*fully deserialized*/ true);
  return fn;
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

static CheckedCastKind getCheckedCastKind(unsigned Attr) {
  switch (Attr) {
  case (unsigned)CheckedCastKind::ArchetypeToArchetype:
  case (unsigned)CheckedCastKind::ArchetypeToConcrete:
  case (unsigned)CheckedCastKind::Downcast:
  case (unsigned)CheckedCastKind::ExistentialToArchetype:
  case (unsigned)CheckedCastKind::ExistentialToConcrete:
  case (unsigned)CheckedCastKind::SuperToArchetype:
    return (CheckedCastKind)Attr;
  default:
    llvm_unreachable("not a valid CheckedCastKind for SIL");
  }
}

/// Construct a SILDeclRef from ListOfValues.
static SILDeclRef getSILDeclRef(ModuleFile *MF,
                                ArrayRef<uint64_t> ListOfValues,
                                unsigned StartIdx) {
  assert(ListOfValues.size() >= StartIdx+4 &&
         "Expect 4 numbers for SILDeclRef");
  SILDeclRef DRef(cast<ValueDecl>(MF->getDecl(ListOfValues[StartIdx])),
                  (SILDeclRef::Kind)ListOfValues[StartIdx+1],
                  ListOfValues[StartIdx+2], ListOfValues[StartIdx+3] > 0);
  return DRef;
}

bool SILDeserializer::readSILInstruction(SILFunction *Fn, SILBasicBlock *BB,
                          unsigned RecordKind,
                          SmallVectorImpl<uint64_t> &scratch) {
  // Return error if Basic Block is null.
  if (!BB)
    return true;

  SILBuilder Builder(BB);
  unsigned OpCode, TyCategory, TyCategory2, ValResNum, ValResNum2, Attr,
          IsTransparent, NumSubs;
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
  case SIL_INST_CAST:
    SILInstCastLayout::readRecord(scratch, OpCode, Attr,
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
    SILInstApplyLayout::readRecord(scratch, IsPartial, IsTransparent, NumSubs,
                                   TyID, TyID2, ValID, ValResNum, ListOfValues);
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
  case ValueKind::SILUndef:
    llvm_unreachable("not an instruction");

#define ONETYPE_INST(ID)                      \
  case ValueKind::ID##Inst:                   \
    assert(RecordKind == SIL_ONE_TYPE && "Layout should be OneType.");         \
    ResultVal = Builder.create##ID(Loc,                                        \
                  getSILType(MF->getType(TyID), (SILValueCategory)TyCategory));\
    break;
  ONETYPE_INST(AllocBox)
  ONETYPE_INST(AllocRef)
  ONETYPE_INST(AllocStack)
  ONETYPE_INST(Metatype)
#undef ONETYPE_INST
#define ONETYPE_ONEOPERAND_INST(ID)           \
  case ValueKind::ID##Inst:                   \
    assert(RecordKind == SIL_ONE_TYPE_ONE_OPERAND &&       \
           "Layout should be OneTypeOneOperand.");         \
    ResultVal = Builder.create##ID(Loc,                    \
                  getSILType(MF->getType(TyID), (SILValueCategory)TyCategory), \
                  getLocalValue(ValID, ValResNum,                              \
                    getSILType(MF->getType(TyID2),                             \
                               (SILValueCategory)TyCategory2)));               \
    break;
  ONETYPE_ONEOPERAND_INST(DeallocBox)
  ONETYPE_ONEOPERAND_INST(ArchetypeMetatype)
  ONETYPE_ONEOPERAND_INST(ClassMetatype)
  ONETYPE_ONEOPERAND_INST(ProtocolMetatype)
  ONETYPE_ONEOPERAND_INST(AllocArray)
#undef ONETYPE_ONEOPERAND_INST
#define ONEOPERAND_ONETYPE_INST(ID)           \
  case ValueKind::ID##Inst:                   \
    assert(RecordKind == SIL_ONE_TYPE_ONE_OPERAND &&       \
           "Layout should be OneTypeOneOperand.");         \
    ResultVal = Builder.create##ID(Loc,                    \
                  getLocalValue(ValID, ValResNum,                              \
                    getSILType(MF->getType(TyID2),                             \
                               (SILValueCategory)TyCategory2)),                \
                  getSILType(MF->getType(TyID), (SILValueCategory)TyCategory));\
    break;
  ONEOPERAND_ONETYPE_INST(ProjectExistential)
  ONEOPERAND_ONETYPE_INST(ProjectExistentialRef)
  // Conversion instructions.
  ONEOPERAND_ONETYPE_INST(RefToObjectPointer)
  ONEOPERAND_ONETYPE_INST(Upcast)
  ONEOPERAND_ONETYPE_INST(Coerce)
  ONEOPERAND_ONETYPE_INST(AddressToPointer)
  ONEOPERAND_ONETYPE_INST(PointerToAddress)
  ONEOPERAND_ONETYPE_INST(ObjectPointerToRef)
  ONEOPERAND_ONETYPE_INST(RefToRawPointer)
  ONEOPERAND_ONETYPE_INST(RawPointerToRef)
  ONEOPERAND_ONETYPE_INST(RefToUnowned)
  ONEOPERAND_ONETYPE_INST(UnownedToRef)
  ONEOPERAND_ONETYPE_INST(ThinToThickFunction)
  ONEOPERAND_ONETYPE_INST(BridgeToBlock)
  ONEOPERAND_ONETYPE_INST(ArchetypeRefToSuper)
  ONEOPERAND_ONETYPE_INST(ConvertFunction)
  ONEOPERAND_ONETYPE_INST(UpcastExistentialRef)
#undef ONEOPERAND_ONETYPE_INST
  case ValueKind::InitExistentialInst:
  case ValueKind::InitExistentialRefInst: {
    auto Ty = getSILType(MF->getType(TyID), (SILValueCategory)TyCategory);
    auto Ty2 = MF->getType(TyID2);
    SILValue operand = getLocalValue(ValID, ValResNum,
                         getSILType(Ty2, (SILValueCategory)TyCategory2));
    switch ((ValueKind)OpCode) {
    default: assert(0 && "Out of sync with parent switch");
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
  case ValueKind::ApplyInst: {
    // Format: attributes such as transparent, the callee's type, a value for
    // the callee and a list of values for the arguments. Each value in the list
    // is represented with 2 IDs: ValueID and ValueResultNumber.
    auto Ty = MF->getType(TyID);
    auto Ty2 = MF->getType(TyID2);
    SILType FnTy = getSILType(Ty, SILValueCategory::Object);
    SILType SubstFnTy = getSILType(Ty2, SILValueCategory::Object);
    SILFunctionType *FTI = SubstFnTy.castTo<SILFunctionType>();
    auto ArgTys = FTI->getParameterSILTypes();

    assert((ArgTys.size() << 1) == ListOfValues.size() &&
           "Argument number mismatch in ApplyInst.");
    SmallVector<SILValue, 4> Args;
    for (unsigned I = 0, E = ListOfValues.size(); I < E; I += 2)
      Args.push_back(getLocalValue(ListOfValues[I], ListOfValues[I+1],
                                   ArgTys[I>>1]));
    bool Transparent = (bool)IsTransparent;
    unsigned NumSub = NumSubs;
    
    SmallVector<Substitution, 4> Substitutions;
    while (NumSub--) {
      auto sub = MF->maybeReadSubstitution(SILCursor);
      assert(sub.hasValue() && "missing substitution");
      Substitutions.push_back(*sub);
    }
    
    ResultVal = Builder.createApply(Loc, getLocalValue(ValID, ValResNum, FnTy),
                                    SubstFnTy,
                                    FTI->getResult().getSILType(),
                                    Substitutions, Args, Transparent);
    break;
  }
  case ValueKind::PartialApplyInst: {
    auto Ty = MF->getType(TyID);
    auto Ty2 = MF->getType(TyID2);
    SILType FnTy = getSILType(Ty, SILValueCategory::Object);
    SILType SubstFnTy = getSILType(Ty2, SILValueCategory::Object);
    SILFunctionType *FTI = SubstFnTy.castTo<SILFunctionType>();
    auto ArgTys = FTI->getParameterSILTypes();

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
    Type ResTy = FunctionType::get(ArgTy, FTI->getResult().getType());

    unsigned NumSub = NumSubs;
    SmallVector<Substitution, 4> Substitutions;
    while (NumSub--) {
      auto sub = MF->maybeReadSubstitution(SILCursor);
      assert(sub.hasValue() && "missing substitution");
      Substitutions.push_back(*sub);
    }
    
    // FIXME: Why the arbitrary order difference in IRBuilder type argument?
    ResultVal = Builder.createPartialApply(Loc, FnVal, SubstFnTy,
                                           Substitutions, Args,
                                           SILMod.Types.getLoweredType(ResTy));
    break;
  }
  case ValueKind::BuiltinFunctionRefInst: {
    // Format: FuncDecl and type. Use SILOneOperandLayout.
    auto Ty = MF->getType(TyID);
    ResultVal = Builder.createBuiltinFunctionRef(Loc, MF->getIdentifier(ValID),
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
  case ValueKind::SILGlobalAddrInst: {
    // Format: Name and type. Use SILOneOperandLayout.
    auto Ty = MF->getType(TyID);
    Identifier Name = MF->getIdentifier(ValID);

    // Find the global variable.
    SILGlobalVariable *g = readGlobalVar(Name);
    assert(g && "Can't deserialize global variable");
    assert(g->getLoweredType().getAddressType() ==
           getSILType(Ty, (SILValueCategory)TyCategory) &&
           "Type of a global variable does not match SILGlobalAddr.");
    (void)Ty;

    ResultVal = Builder.createSILGlobalAddr(Loc, g);
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
                            getSILType(Ty, (SILValueCategory)TyCategory)));
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
    APInt value(intTy->getGreatestWidth(), StringVal.str(), 10);
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
    Identifier StringVal = MF->getIdentifier(ValID);
    auto encoding = fromStableStringEncoding(Attr);
    if (!encoding) return true;
    ResultVal = Builder.createStringLiteral(Loc, StringVal.str(),
                                            encoding.getValue());
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
  // Checked Conversion instructions.
  case ValueKind::UnconditionalCheckedCastInst: {
    SILValue Val = getLocalValue(ValID, ValResNum,
                 getSILType(MF->getType(TyID2), (SILValueCategory)TyCategory2));
    SILType Ty = getSILType(MF->getType(TyID), (SILValueCategory)TyCategory);
    CheckedCastKind Kind;
    switch (Attr) {
    case (unsigned)CheckedCastKind::ArchetypeToArchetype:
    case (unsigned)CheckedCastKind::ArchetypeToConcrete:
    case (unsigned)CheckedCastKind::Downcast:
    case (unsigned)CheckedCastKind::ExistentialToArchetype:
    case (unsigned)CheckedCastKind::ExistentialToConcrete:
    case (unsigned)CheckedCastKind::SuperToArchetype:
      Kind = (CheckedCastKind)Attr;
      break;
        
    default:
      llvm_unreachable("not a valid CheckedCastKind for SIL");
    }
    ResultVal = Builder.createUnconditionalCheckedCast(Loc, Kind, Val, Ty);
    break;
  }
      
#define UNARY_INSTRUCTION(ID) \
  case ValueKind::ID##Inst:                   \
    assert(RecordKind == SIL_ONE_OPERAND &&            \
           "Layout should be OneOperand.");            \
    ResultVal = Builder.create##ID(Loc, getLocalValue(ValID, ValResNum,  \
                    getSILType(MF->getType(TyID),                        \
                               (SILValueCategory)TyCategory)));          \
    break;
  UNARY_INSTRUCTION(CondFail)
  UNARY_INSTRUCTION(CopyValue)
  UNARY_INSTRUCTION(DestroyValue)
  UNARY_INSTRUCTION(DeinitExistential)
  UNARY_INSTRUCTION(DestroyAddr)
  UNARY_INSTRUCTION(IsNonnull)
  UNARY_INSTRUCTION(Load)
  UNARY_INSTRUCTION(Return)
  UNARY_INSTRUCTION(StrongRetain)
  UNARY_INSTRUCTION(StrongRelease)
  UNARY_INSTRUCTION(StrongRetainAutoreleased)
  UNARY_INSTRUCTION(AutoreleaseReturn)
  UNARY_INSTRUCTION(StrongRetainUnowned)
  UNARY_INSTRUCTION(UnownedRetain)
  UNARY_INSTRUCTION(UnownedRelease)
#undef UNARY_INSTRUCTION

  case ValueKind::LoadWeakInst: {
    auto Ty = MF->getType(TyID);
    bool isTake = (Attr > 0);
    ResultVal = Builder.createLoadWeak(Loc,
        getLocalValue(ValID, ValResNum,
                      getSILType(Ty, (SILValueCategory)TyCategory)),
        IsTake_t(isTake));
    break;
  }
  case ValueKind::MarkUninitializedInst: {
    auto Ty = getSILType(MF->getType(TyID), (SILValueCategory)TyCategory);
    auto Kind = (MarkUninitializedInst::Kind)Attr;
    auto Val = getLocalValue(ValID, ValResNum, Ty);
    ResultVal = Builder.createMarkUninitialized(Loc, Val, Kind);
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
    bool isInit = (Attr & 0x2) > 0;
    bool isTake = (Attr & 0x1) > 0;
    ResultVal = Builder.createCopyAddr(Loc,
                    getLocalValue(ValID, ValResNum, addrType),
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
                                             getLocalValue(ValID, ValResNum,ST),
                                             TyID,
                                getSILType(ResultTy, SILValueCategory::Object));
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
  case ValueKind::SwitchEnumAddrInst: {
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
      ResultVal = Builder.createSwitchEnumAddr(Loc, Cond,
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
      APInt value(intTy->getGreatestWidth(), StringVal.str(), 10);
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
  case ValueKind::InitEnumDataAddrInst: {
    // Use SILOneValueOneOperandLayout.
    EnumElementDecl *Elt = cast<EnumElementDecl>(MF->getDecl(ValID));
    auto OperandTy = MF->getType(TyID);
    auto ResultTy = OperandTy->getTypeOfMember(Elt->getModuleContext(),
                                               Elt,
                                               nullptr,
                                               Elt->getArgumentType());
    ResultVal = Builder.createInitEnumDataAddr(Loc,
                    getLocalValue(ValID2, ValResNum2,
                                  getSILType(OperandTy,
                                             (SILValueCategory)TyCategory)),
                    Elt,
                    getSILType(ResultTy, SILValueCategory::Address));
    break;
  }
  case ValueKind::TakeEnumDataAddrInst: {
    // Use SILOneValueOneOperandLayout.
    EnumElementDecl *Elt = cast<EnumElementDecl>(MF->getDecl(ValID));
    auto OperandTy = MF->getType(TyID);
    auto ResultTy = OperandTy->getTypeOfMember(Elt->getModuleContext(),
                                               Elt,
                                               nullptr,
                                               Elt->getArgumentType());
    ResultVal = Builder.createTakeEnumDataAddr(Loc,
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
  case ValueKind::PeerMethodInst:
  case ValueKind::DynamicMethodInst: {
    // Format: a type, an operand and a SILDeclRef. Use SILOneTypeValuesLayout:
    // type, Attr, SILDeclRef (DeclID, Kind, uncurryLevel, IsObjC),
    // and an operand.
    // ArchetypeMethodInst is additionally optionally followed by a
    // ProtocolConformance record.
    assert(ListOfValues.size() >= 7 &&
           "Expect at least 7 numbers for MethodInst");
    SILDeclRef DRef = getSILDeclRef(MF, ListOfValues, 1);
    SILType Ty = getSILType(MF->getType(TyID), (SILValueCategory)TyCategory);
    SILType operandTy = getSILType(MF->getType(ListOfValues[5]),
                                   (SILValueCategory)ListOfValues[6]);
    bool IsVolatile = ListOfValues[0] > 0;
    
    switch ((ValueKind)OpCode) {
    default: assert(0 && "Out of sync with parent switch");
    case ValueKind::ArchetypeMethodInst: {
      auto conformancePair = MF->maybeReadConformance(Ty.getSwiftRValueType(),
                                                      SILCursor);
      ProtocolConformance *conformance
        = conformancePair ? conformancePair->second : nullptr;

      ResultVal = Builder.createArchetypeMethod(Loc, Ty,
                                                conformance, DRef,
                                                operandTy, IsVolatile);
      break;
    }
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
    case ValueKind::PeerMethodInst:
      ResultVal = Builder.createPeerMethod(Loc,
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
    SILDeclRef DRef = getSILDeclRef(MF, ListOfValues, 2);
    ResultVal = Builder.createDynamicMethodBranch(Loc,
                    getLocalValue(ListOfValues[0], ListOfValues[1],
                                  getSILType(MF->getType(TyID),
                                             (SILValueCategory)TyCategory)),
                    DRef, getBBForReference(Fn, ListOfValues[6]),
                    getBBForReference(Fn, ListOfValues[7]));
    break;
  }
  case ValueKind::CheckedCastBranchInst: {
    // Format: the cast kind, a typed value, a BasicBlock ID for success,
    // a BasicBlock ID for failure. Uses SILOneTypeValuesLayout.
    assert(ListOfValues.size() == 7 &&
           "expect 7 numbers for CheckedCastBranchInst");
    CheckedCastKind castKind = getCheckedCastKind(ListOfValues[0]);
    SILType opTy = getSILType(MF->getType(ListOfValues[3]),
                              (SILValueCategory)ListOfValues[4]);
    SILValue op = getLocalValue(ListOfValues[1], ListOfValues[2], opTy);
    SILType castTy = getSILType(MF->getType(TyID),
                                (SILValueCategory)TyCategory);
    auto *successBB = getBBForReference(Fn, ListOfValues[5]);
    auto *failureBB = getBBForReference(Fn, ListOfValues[6]);
    
    ResultVal = Builder.createCheckedCastBranch(Loc, castKind, op, castTy,
                                                successBB, failureBB);
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

  auto Func = readSILFunction(*iter, InFunc, name, /*declarationOnly*/ false);
  if (Func)
    DEBUG(llvm::dbgs() << "Deserialize SIL:\n";
          Func->dump());
  return Func;
}

SILFunction *SILDeserializer::lookupSILFunction(Identifier name) {
  if (!FuncTable)
    return nullptr;
  auto iter = FuncTable->find(name);
  if (iter == FuncTable->end())
    return nullptr;

  auto Func = readSILFunction(*iter, nullptr, name, /*declarationOnly*/ false);
  if (Func)
    DEBUG(llvm::dbgs() << "Deserialize SIL:\n";
          Func->dump());
  return Func;
}

SILGlobalVariable *SILDeserializer::readGlobalVar(Identifier Name) {
  if (!GlobalVarList)
    return nullptr;

  // Find Id for the given name.
  auto iter = GlobalVarList->find(Name);
  if (iter == GlobalVarList->end())
    return nullptr;
  auto VId = *iter;
  if (VId == 0)
    return nullptr;

  assert(VId <= GlobalVars.size() && "invalid GlobalVar ID");
  auto &globalVarOrOffset = GlobalVars[VId-1];
  if (globalVarOrOffset.isComplete())
    return globalVarOrOffset;

  BCOffsetRAII restoreOffset(SILCursor);
  SILCursor.JumpToBit(globalVarOrOffset);
  auto entry = SILCursor.advance(AF_DontPopBlockAtEnd);
  if (entry.Kind == llvm::BitstreamEntry::Error) {
    DEBUG(llvm::dbgs() << "Cursor advance error in readGlobalVar.\n");
    return nullptr;
  }

  SmallVector<uint64_t, 64> scratch;
  StringRef blobData;
  unsigned kind = SILCursor.readRecord(entry.ID, scratch, &blobData);
  assert(kind == SIL_GLOBALVAR && "expect a sil global var");
  (void)kind;

  TypeID TyID;
  unsigned rawLinkage;
  GlobalVarLayout::readRecord(scratch, rawLinkage, TyID);
  if (TyID == 0) {
    DEBUG(llvm::dbgs() << "SILGlobalVariable typeID is 0.\n");
    return nullptr;
  }

  auto linkage = fromStableSILLinkage(rawLinkage);
  if (!linkage) {
    DEBUG(llvm::dbgs() << "invalid linkage code " << rawLinkage
                       << " for SILGlobalVariable\n");
    return nullptr;
  }

  auto Ty = MF->getType(TyID);
  SILGlobalVariable *v = SILGlobalVariable::create(
                           SILMod, linkage.getValue(),
                           Name.str(), getSILType(Ty, SILValueCategory::Object));
  globalVarOrOffset = v;

  if (Callback) Callback->didDeserialize(MF->getAssociatedModule(), v);
  return v;
}

SILVTable *SILDeserializer::readVTable(DeclID VId) {
  if (VId == 0)
    return nullptr;
  assert(VId <= VTables.size() && "invalid VTable ID");
  auto &vTableOrOffset = VTables[VId-1];

  if (vTableOrOffset.isComplete())
    return vTableOrOffset;

  BCOffsetRAII restoreOffset(SILCursor);
  SILCursor.JumpToBit(vTableOrOffset);
  auto entry = SILCursor.advance(AF_DontPopBlockAtEnd);
  if (entry.Kind == llvm::BitstreamEntry::Error) {
    DEBUG(llvm::dbgs() << "Cursor advance error in readVTable.\n");
    return nullptr;
  }

  SmallVector<uint64_t, 64> scratch;
  StringRef blobData;
  unsigned kind = SILCursor.readRecord(entry.ID, scratch, &blobData);
  assert(kind == SIL_VTABLE && "expect a sil vtable");
  (void)kind;

  DeclID ClassID;
  VTableLayout::readRecord(scratch, ClassID);
  if (ClassID == 0) {
    DEBUG(llvm::dbgs() << "VTable classID is 0.\n");
    return nullptr;
  }

  ClassDecl *theClass = cast<ClassDecl>(MF->getDecl(ClassID));
  // Fetch the next record.
  scratch.clear();
  entry = SILCursor.advance(AF_DontPopBlockAtEnd);
  if (entry.Kind == llvm::BitstreamEntry::EndBlock)
    // This vtable has no contents.
    return nullptr;
  kind = SILCursor.readRecord(entry.ID, scratch);

  std::vector<SILVTable::Pair> vtableEntries;
  // Another SIL_VTABLE record means the end of this VTable.
  while (kind != SIL_VTABLE && kind != SIL_FUNCTION) {
    assert(kind == SIL_VTABLE_ENTRY &&
           "Content of Vtable should be in SIL_VTABLE_ENTRY.");
    ArrayRef<uint64_t> ListOfValues;
    DeclID NameID;
    VTableEntryLayout::readRecord(scratch, NameID, ListOfValues);
    SILFunction *Func = lookupSILFunction(MF->getIdentifier(NameID));
    if (Func)
      vtableEntries.emplace_back(getSILDeclRef(MF, ListOfValues, 0), Func);

    // Fetch the next record.
    scratch.clear();
    entry = SILCursor.advance(AF_DontPopBlockAtEnd);
    if (entry.Kind == llvm::BitstreamEntry::EndBlock)
      // EndBlock means the end of this VTable.
      break;
    kind = SILCursor.readRecord(entry.ID, scratch);
  }
  SILVTable *vT = SILVTable::create(SILMod, theClass, vtableEntries);
  vTableOrOffset = vT;

  if (Callback) Callback->didDeserialize(MF->getAssociatedModule(), vT);
  return vT;
}

SILVTable *SILDeserializer::lookupVTable(Identifier Name) {
  if (!VTableList)
    return nullptr;
  auto iter = VTableList->find(Name);
  if (iter == VTableList->end())
    return nullptr;

  auto VT = readVTable(*iter);
  return VT;
}

/// Deserialize all VTables inside the module and add them to SILMod.
void SILDeserializer::getAllVTables() {
  if (!VTableList)
    return;

  for (unsigned I = 0, E = VTables.size(); I < E; I++)
    readVTable(I+1);
}

SILDeserializer::~SILDeserializer() = default;
