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

#define DEBUG_TYPE "deserialize"
#include "DeserializeSIL.h"
#include "swift/Serialization/ModuleFile.h"
#include "SILFormat.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILDebugScope.h"
#include "swift/SIL/SILModule.h"
#include "swift/SIL/SILUndef.h"
#include "swift/Serialization/BCReadingExtras.h"

#include "llvm/ADT/Statistic.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/OnDiskHashTable.h"

using namespace swift;
using namespace swift::serialization;
using namespace swift::serialization::sil_block;
using namespace llvm::support;

STATISTIC(NumDeserializedFunc, "Number of deserialized SIL functions");

static Optional<StringLiteralInst::Encoding>
fromStableStringEncoding(unsigned value) {
  switch (value) {
  case SIL_UTF8: return StringLiteralInst::Encoding::UTF8;
  case SIL_UTF16: return StringLiteralInst::Encoding::UTF16;
  default: return None;
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
  case SIL_LINKAGE_SHARED_EXTERNAL: return SILLinkage::SharedExternal;
  case SIL_LINKAGE_PRIVATE_EXTERNAL: return SILLinkage::PrivateExternal;
  default: return None;
  }
}

/// Used to deserialize entries in the on-disk func hash table.
class SILDeserializer::FuncTableInfo {
public:
  using internal_key_type = StringRef;
  using external_key_type = StringRef;
  using data_type = DeclID;
  using hash_value_type = uint32_t;
  using offset_type = unsigned;

  internal_key_type GetInternalKey(external_key_type ID) { return ID; }

  external_key_type GetExternalKey(internal_key_type ID) { return ID; }

  hash_value_type ComputeHash(internal_key_type key) {
    return llvm::HashString(key);
  }

  static bool EqualKey(internal_key_type lhs, internal_key_type rhs) {
    return lhs == rhs;
  }

  static std::pair<unsigned, unsigned> ReadKeyDataLength(const uint8_t *&data) {
    unsigned keyLength = endian::readNext<uint16_t, little, unaligned>(data);
    unsigned dataLength = endian::readNext<uint16_t, little, unaligned>(data);
    return { keyLength, dataLength };
  }

  static internal_key_type ReadKey(const uint8_t *data, unsigned length) {
    return StringRef(reinterpret_cast<const char *>(data), length);
  }

  static data_type ReadData(internal_key_type key, const uint8_t *data,
                            unsigned length) {
    assert(length == 4 && "Expect a single DeclID.");
    data_type result = endian::readNext<uint32_t, little, unaligned>(data);
    return result;
  }
};

SILDeserializer::SILDeserializer(ModuleFile *MF, SILModule &M,
                                 SerializedSILLoader::Callback *callback)
    : MF(MF), SILMod(M), Callback(callback) {

  SILCursor = MF->getSILCursor();
  SILIndexCursor = MF->getSILIndexCursor();
  // Early return if either sil block or sil index block does not exist.
  if (!SILCursor.getBitStreamReader() || !SILIndexCursor.getBitStreamReader())
    return;

  // Load any abbrev records at the start of the block.
  SILCursor.advance();

  llvm::BitstreamCursor cursor = SILIndexCursor;
  // We expect SIL_FUNC_NAMES first, then SIL_VTABLE_NAMES, then
  // SIL_GLOBALVAR_NAMES, and SIL_WITNESSTABLE_NAMES. But each one can be
  // omitted if no entries exist in the module file.
  unsigned kind = 0;
  while (kind != sil_index_block::SIL_WITNESSTABLE_NAMES) {
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
             kind == sil_index_block::SIL_GLOBALVAR_NAMES ||
             kind == sil_index_block::SIL_WITNESSTABLE_NAMES)) &&
         "Expect SIL_FUNC_NAMES, SIL_VTABLE_NAMES, SIL_GLOBALVAR_NAMES or \
          SIL_WITNESSTABLE_NAMES.");
    (void)prevKind;

    if (kind == sil_index_block::SIL_FUNC_NAMES)
      FuncTable = readFuncTable(scratch, blobData);
    else if (kind == sil_index_block::SIL_VTABLE_NAMES)
      VTableList = readFuncTable(scratch, blobData);
    else if (kind == sil_index_block::SIL_GLOBALVAR_NAMES)
      GlobalVarList = readFuncTable(scratch, blobData);
    else if (kind == sil_index_block::SIL_WITNESSTABLE_NAMES)
      WitnessTableList = readFuncTable(scratch, blobData);

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
    } else if (kind == sil_index_block::SIL_WITNESSTABLE_NAMES) {
      assert((next.Kind == llvm::BitstreamEntry::Record &&
              offKind == sil_index_block::SIL_WITNESSTABLE_OFFSETS) &&
             "Expect a SIL_WITNESSTABLE_OFFSETS record.");
      WitnessTables.assign(scratch.begin(), scratch.end());
    }
  }
}

std::unique_ptr<SILDeserializer::SerializedFuncTable>
SILDeserializer::readFuncTable(ArrayRef<uint64_t> fields, StringRef blobData) {
  uint32_t tableOffset;
  sil_index_block::ListLayout::readRecord(fields, tableOffset);
  auto base = reinterpret_cast<const uint8_t *>(blobData.data());

  using OwnedTable = std::unique_ptr<SerializedFuncTable>;
  return OwnedTable(SerializedFuncTable::Create(base + tableOffset,
                                                base + sizeof(uint32_t), base));
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
      if (!Entries[i])
        continue;
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
      new (SILMod) GlobalAddrInst(SILFileLocation(Loc), Type);
  return Placeholders[ResultNum];
}

/// Return the SILBasicBlock of a given ID.
SILBasicBlock *SILDeserializer::getBBForDefinition(SILFunction *Fn,
                                                   SILBasicBlock *Prev,
                                                   unsigned ID) {
  SILBasicBlock *&BB = BlocksByID[ID];
  // If the block has never been named yet, just create it.
  if (BB == nullptr)
    return BB = new (SILMod) SILBasicBlock(Fn, Prev);

  // If it already exists, it was either a forward reference or a redefinition.
  // The latter should never happen.
  bool wasForwardReferenced = UndefinedBlocks.erase(BB);
  assert(wasForwardReferenced);
  (void)wasForwardReferenced;

  if (Prev)
    BB->moveAfter(Prev);
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
                                           StringRef name,
                                           SILType type) {
  SourceLoc loc;
  return SILFunction::create(M, SILLinkage::Private, name,
                             type.castTo<SILFunctionType>(),
                             nullptr, SILFileLocation(loc), IsNotBare,
                             IsNotTransparent, IsNotFragile, IsNotThunk,
                             SILFunction::NotRelevant);
}

/// Helper function to find a SILFunction, given its name and type.
SILFunction *SILDeserializer::getFuncForReference(StringRef name,
                                                  SILType type) {
  // Check to see if we have a function by this name already.
  SILFunction *fn = SILMod.lookUpFunction(name);
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

/// Helper function to find a SILFunction, given its name and type.
SILFunction *SILDeserializer::getFuncForReference(StringRef name) {
  // Check to see if we have a function by this name already.
  SILFunction *fn = SILMod.lookUpFunction(name);
  if (fn)
    return fn;

  // Otherwise, look for a function with this name in the module.
  auto iter = FuncTable->find(name);
  if (iter == FuncTable->end())
    return nullptr;

  return readSILFunction(*iter, nullptr, name, /*declarationOnly*/ true);
}

/// Helper function to find a SILGlobalVariable given its name. It first checks
/// in the module. If we can not find it in the module, we attempt to
/// deserialize it.
SILGlobalVariable *SILDeserializer::getGlobalForReference(StringRef name) {
  // Check to see if we have a global by this name already.
  if (SILGlobalVariable *g = SILMod.lookUpGlobalVariable(name))
    return g;

  // Otherwise, look for a global with this name in the module.
  return readGlobalVar(name);
}

/// Deserialize a SILFunction if it is not already deserialized. The input
/// SILFunction can either be an empty declaration or null. If it is an empty
/// declaration, we fill in the contents. If the input SILFunction is
/// null, we create a SILFunction.
SILFunction *SILDeserializer::readSILFunction(DeclID FID,
                                              SILFunction *existingFn,
                                              StringRef name,
                                              bool declarationOnly,
                                              bool errorIfEmptyBody) {
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
    MF->error();
    return nullptr;
  }

  SmallVector<uint64_t, 64> scratch;
  StringRef blobData;
  unsigned kind = SILCursor.readRecord(entry.ID, scratch, &blobData);
  assert(kind == SIL_FUNCTION && "expect a sil function");
  (void)kind;

  TypeID funcTyID;
  unsigned rawLinkage, isTransparent, isFragile, isThunk, isGlobal,
           inlineStrategy, effect;
  IdentifierID SemanticsID;
  // TODO: read fragile
  SILFunctionLayout::readRecord(scratch, rawLinkage,
                                isTransparent, isFragile, isThunk, isGlobal,
                                inlineStrategy, effect, funcTyID,
                                SemanticsID);

  if (funcTyID == 0) {
    DEBUG(llvm::dbgs() << "SILFunction typeID is 0.\n");
    MF->error();
    return nullptr;
  }
  auto ty = getSILType(MF->getType(funcTyID), SILValueCategory::Object);
  if (!ty.is<SILFunctionType>()) {
    DEBUG(llvm::dbgs() << "not a function type for SILFunction\n");
    MF->error();
    return nullptr;
  }

  auto linkage = fromStableSILLinkage(rawLinkage);
  if (!linkage) {
    DEBUG(llvm::dbgs() << "invalid linkage code " << rawLinkage
                       << " for SILFunction\n");
    MF->error();
    return nullptr;
  }

  // If we weren't handed a function, check for an existing
  // declaration in the output module.
  if (!existingFn) existingFn = SILMod.lookUpFunction(name);
  auto fn = existingFn;

  // TODO: use the correct SILLocation from module.
  SILLocation loc = SILFileLocation(SourceLoc());

  // If we have an existing function, verify that the types match up.
  if (fn) {
    if (fn->getLoweredType() != ty) {
      DEBUG(llvm::dbgs() << "SILFunction type mismatch.\n");
      MF->error();
      return nullptr;
    }

    if (isFragile)
      fn->setFragile(IsFragile);

    // Don't override the transparency or linkage of a function with
    // an existing declaration.

  // Otherwise, create a new function.
  } else {
    fn = SILFunction::create(SILMod, linkage.getValue(), name,
                             ty.castTo<SILFunctionType>(),
                             nullptr, loc,
                             IsNotBare, IsTransparent_t(isTransparent == 1),
                             IsFragile_t(isFragile == 1),
                             IsThunk_t(isThunk == 1), SILFunction::NotRelevant,
                             (Inline_t)inlineStrategy);
    fn->setGlobalInit(isGlobal == 1);
    fn->setEffectsKind((EffectsKind)effect);
    if (SemanticsID)
      fn->setSemanticsAttr(MF->getIdentifier(SemanticsID).str());

    if (Callback) Callback->didDeserialize(MF->getAssociatedModule(), fn);
  }

  assert(fn->empty() &&
         "SILFunction to be deserialized starts being empty.");

  fn->setBare(IsBare);
  if (!fn->hasLocation()) fn->setLocation(loc);

  SILDebugScope *DS = fn->getDebugScope();
  if (!DS) {
    DS= new (SILMod) SILDebugScope(loc, *fn);
    fn->setDebugScope(DS);
  }

  GenericParamList *contextParams = nullptr;
  if (!declarationOnly) {
    // We need to construct a linked list of GenericParamList. The outermost
    // list appears first in the module file. Force the declaration's context to
    // be the current module, not the original module associated with this
    // module file. A generic param decl at module scope cannot refer to another
    // module because we would have no way lookup the original module when
    // serializing a copy of the function. This comes up with generic
    // reabstraction thunks which have shared linkage.
    DeclContext *outerParamContext = SILMod.getSwiftModule();
    while(true) {
      // Params' OuterParameters will point to contextParams.
      auto *Params = MF->maybeReadGenericParams(outerParamContext,
                                                SILCursor, contextParams);
      if (!Params)
        break;
      // contextParams will point to the last deserialized list, which is the
      // innermost one.
      contextParams = Params;
    }
  }

  // If the next entry is the end of the block, then this function has
  // no contents.
  entry = SILCursor.advance(AF_DontPopBlockAtEnd);
  bool isEmptyFunction = (entry.Kind == llvm::BitstreamEntry::EndBlock);
  assert((!isEmptyFunction || !contextParams) &&
         "context params without body?!");

  // Remember this in our cache in case it's a recursive function.
  // Increase the reference count to keep it alive.
  bool isFullyDeserialized = (isEmptyFunction || !declarationOnly);
  if (cacheEntry.isDeserialized()) {
    assert(fn == cacheEntry.get() && "changing SIL function during deserialization!");
  } else {
    fn->incrementRefCount();
  }
  cacheEntry.set(fn, isFullyDeserialized);

  // Stop here if we have nothing else to do.
  if (isEmptyFunction || declarationOnly) {
    return fn;
  }

  NumDeserializedFunc++;
  scratch.clear();

  assert(!fn->getContextGenericParams()
         && "function already has context generic params?!");
  if (contextParams)
    fn->setContextGenericParams(contextParams);

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
  // SIL_VTABLE or SIL_GLOBALVAR or SIL_WITNESSTABLE record also means the end
  // of this SILFunction.
  while (kind != SIL_FUNCTION && kind != SIL_VTABLE && kind != SIL_GLOBALVAR &&
         kind != SIL_WITNESSTABLE) {
    if (kind == SIL_BASIC_BLOCK)
      // Handle a SILBasicBlock record.
      CurrentBB = readSILBasicBlock(fn, CurrentBB, scratch);
    else {
      // If CurrentBB is empty, just return fn. The code in readSILInstruction
      // assumes that such a situation means that fn is a declaration. Thus it
      // is using return false to mean two different things, error a failure
      // occured and this is a declaration. Work around that for now.
      if (!CurrentBB)
        return fn;

      // Handle a SILInstruction record.
      if (readSILInstruction(fn, CurrentBB, kind, scratch)) {
        DEBUG(llvm::dbgs() << "readSILInstruction returns error.\n");
        MF->error();
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

  // If fn is empty, we failed to deserialize its body. Return nullptr to signal
  // error.
  if (fn->empty() && errorIfEmptyBody)
    return nullptr;

  if (Callback)
    Callback->didDeserializeFunctionBody(MF->getAssociatedModule(), fn);

  return fn;
}

SILBasicBlock *SILDeserializer::readSILBasicBlock(SILFunction *Fn,
                                                  SILBasicBlock *Prev,
                                    SmallVectorImpl<uint64_t> &scratch) {
  ArrayRef<uint64_t> Args;
  SILBasicBlockLayout::readRecord(scratch, Args);

  // Args should be a list of pairs, the first number is a TypeID, the
  // second number is a ValueID.
  SILBasicBlock *CurrentBB = getBBForDefinition(Fn, Prev, BasicBlockID++);
  for (unsigned I = 0, E = Args.size(); I < E; I += 3) {
    TypeID TyID = Args[I];
    if (!TyID) return nullptr;
    ValueID ValId = Args[I+2];
    if (!ValId) return nullptr;

    auto ArgTy = MF->getType(TyID);
    auto Arg = new (SILMod) SILArgument(CurrentBB,
                                        getSILType(ArgTy,
                                                   (SILValueCategory)Args[I+1]));
    setLocalValue(Arg, ++LastValueID);
  }
  return CurrentBB;
}

static CastConsumptionKind getCastConsumptionKind(unsigned attr) {
  switch (attr) {
  case SIL_CAST_CONSUMPTION_TAKE_ALWAYS:
    return CastConsumptionKind::TakeAlways;
  case SIL_CAST_CONSUMPTION_TAKE_ON_SUCCESS:
    return CastConsumptionKind::TakeOnSuccess;
  case SIL_CAST_CONSUMPTION_COPY_ON_SUCCESS:
    return CastConsumptionKind::CopyOnSuccess;
  default:
    llvm_unreachable("not a valid CastConsumptionKind for SIL");
  }
}

/// Construct a SILDeclRef from ListOfValues.
static SILDeclRef getSILDeclRef(ModuleFile *MF,
                                ArrayRef<uint64_t> ListOfValues,
                                unsigned &NextIdx) {
  assert(ListOfValues.size() >= NextIdx+5 &&
         "Expect 5 numbers for SILDeclRef");
  SILDeclRef DRef(cast<ValueDecl>(MF->getDecl(ListOfValues[NextIdx])),
                  (SILDeclRef::Kind)ListOfValues[NextIdx+1],
                  (ResilienceExpansion)ListOfValues[NextIdx+2],
                  ListOfValues[NextIdx+3], ListOfValues[NextIdx+4] > 0);
  NextIdx += 5;
  return DRef;
}

bool SILDeserializer::readSILInstruction(SILFunction *Fn, SILBasicBlock *BB,
                                         unsigned RecordKind,
                                         SmallVectorImpl<uint64_t> &scratch) {
  // Return error if Basic Block is null.
  if (!BB)
    return true;

  SmallVector<SILInstruction*, 1> InsertedInsn;
  SILBuilder Builder(BB, &InsertedInsn);
  unsigned OpCode = 0, TyCategory = 0, TyCategory2 = 0, TyCategory3 = 0,
           ValResNum = 0, ValResNum2 = 0, Attr = 0,
           NumSubs = 0, NumConformances = 0;
  ValueID ValID, ValID2, ValID3;
  TypeID TyID, TyID2, TyID3;
  TypeID ConcreteTyID;
  DeclID ProtoID;
  ModuleID OwningModuleID;
  SourceLoc SLoc;
  ArrayRef<uint64_t> ListOfValues;
  SILLocation Loc = SILFileLocation(SLoc);

  switch (RecordKind) {
  default:
    llvm_unreachable("Record kind for a SIL instruction is not supported.");
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
  case SIL_INIT_EXISTENTIAL:
    SILInitExistentialLayout::readRecord(scratch, OpCode,
                                         TyID, TyCategory,
                                         TyID2, TyCategory2,
                                         ValID, ValResNum,
                                         ConcreteTyID,
                                         NumConformances);
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
    SILInstApplyLayout::readRecord(scratch, IsPartial, NumSubs,
                                   TyID, TyID2, ValID, ValResNum, ListOfValues);
    switch (IsPartial) {
    case 0:
      OpCode = (unsigned)ValueKind::ApplyInst;
      break;
    case 1:
      OpCode = (unsigned)ValueKind::PartialApplyInst;
      break;
    case 2:
      OpCode = (unsigned)ValueKind::BuiltinInst;
      break;
    case 3:
      OpCode = (unsigned)ValueKind::TryApplyInst;
      break;
        
    default:
      llvm_unreachable("unexpected apply inst kind");
    }
    break;
  }
  case SIL_INST_NO_OPERAND:
    SILInstNoOperandLayout::readRecord(scratch, OpCode);
    break;
  case SIL_INST_WITNESS_METHOD:
    SILInstWitnessMethodLayout::readRecord(
        scratch, TyID, TyCategory, Attr, TyID2, TyCategory2, TyID3,
        TyCategory3, ValID3, ListOfValues);
    OpCode = (unsigned)ValueKind::WitnessMethodInst;
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
  ONETYPE_INST(AllocStack)
  ONETYPE_INST(Metatype)
  ONETYPE_INST(NullClass)
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
  ONETYPE_ONEOPERAND_INST(ValueMetatype)
  ONETYPE_ONEOPERAND_INST(ExistentialMetatype)
  ONETYPE_ONEOPERAND_INST(AllocValueBuffer)
  ONETYPE_ONEOPERAND_INST(ProjectValueBuffer)
  ONETYPE_ONEOPERAND_INST(ProjectBox)
  ONETYPE_ONEOPERAND_INST(DeallocValueBuffer)
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
  ONEOPERAND_ONETYPE_INST(OpenExistentialAddr)
  ONEOPERAND_ONETYPE_INST(OpenExistentialRef)
  ONEOPERAND_ONETYPE_INST(OpenExistentialMetatype)
  ONEOPERAND_ONETYPE_INST(OpenExistentialBox)
  // Conversion instructions.
  ONEOPERAND_ONETYPE_INST(UncheckedRefCast)
  ONEOPERAND_ONETYPE_INST(UncheckedAddrCast)
  ONEOPERAND_ONETYPE_INST(UncheckedTrivialBitCast)
  ONEOPERAND_ONETYPE_INST(UncheckedRefBitCast)
  ONEOPERAND_ONETYPE_INST(BridgeObjectToRef)
  ONEOPERAND_ONETYPE_INST(BridgeObjectToWord)
  ONEOPERAND_ONETYPE_INST(Upcast)
  ONEOPERAND_ONETYPE_INST(AddressToPointer)
  ONEOPERAND_ONETYPE_INST(PointerToAddress)
  ONEOPERAND_ONETYPE_INST(RefToRawPointer)
  ONEOPERAND_ONETYPE_INST(RawPointerToRef)
  ONEOPERAND_ONETYPE_INST(RefToUnowned)
  ONEOPERAND_ONETYPE_INST(UnownedToRef)
  ONEOPERAND_ONETYPE_INST(RefToUnmanaged)
  ONEOPERAND_ONETYPE_INST(UnmanagedToRef)
  ONEOPERAND_ONETYPE_INST(ThinToThickFunction)
  ONEOPERAND_ONETYPE_INST(ThickToObjCMetatype)
  ONEOPERAND_ONETYPE_INST(ObjCToThickMetatype)
  ONEOPERAND_ONETYPE_INST(ObjCMetatypeToObject)
  ONEOPERAND_ONETYPE_INST(ObjCExistentialMetatypeToObject)
  ONEOPERAND_ONETYPE_INST(ConvertFunction)
  ONEOPERAND_ONETYPE_INST(ThinFunctionToPointer)
  ONEOPERAND_ONETYPE_INST(PointerToThinFunction)
  ONEOPERAND_ONETYPE_INST(ProjectBlockStorage)
#undef ONEOPERAND_ONETYPE_INST
  
  case ValueKind::DeallocExistentialBoxInst: {
    assert(RecordKind == SIL_ONE_TYPE_ONE_OPERAND &&
           "Layout should be OneTypeOneOperand.");
    ResultVal = Builder.createDeallocExistentialBox(Loc,
                  MF->getType(TyID)->getCanonicalType(),
                  getLocalValue(ValID, ValResNum,
                    getSILType(MF->getType(TyID2),
                               (SILValueCategory)TyCategory2)));
    break;

  }
  
  case ValueKind::RefToBridgeObjectInst: {
    auto RefTy = getSILType(MF->getType(TyID), (SILValueCategory)TyCategory);
    auto Ref = getLocalValue(ValID, ValResNum, RefTy);
    auto BitsTy = getSILType(MF->getType(TyID2), (SILValueCategory)TyCategory2);
    auto Bits = getLocalValue(ValID2, ValResNum2, BitsTy);
    
    ResultVal = Builder.createRefToBridgeObject(Loc, Ref, Bits);
    break;
  }

  case ValueKind::ObjCProtocolInst: {
    auto Ty = getSILType(MF->getType(TyID), (SILValueCategory)TyCategory);
    auto Proto = MF->getDecl(ValID);
    ResultVal = Builder.createObjCProtocol(Loc, cast<ProtocolDecl>(Proto), Ty);
    break;
  }

  case ValueKind::InitExistentialAddrInst:
  case ValueKind::InitExistentialMetatypeInst:
  case ValueKind::InitExistentialRefInst:
  case ValueKind::AllocExistentialBoxInst: {

    auto Ty = getSILType(MF->getType(TyID), (SILValueCategory)TyCategory);
    auto Ty2 = MF->getType(TyID2);
    CanType ConcreteTy;
    if ((ValueKind) OpCode != ValueKind::InitExistentialMetatypeInst)
      ConcreteTy = MF->getType(ConcreteTyID)->getCanonicalType();
    SILValue operand;
    if ((ValueKind) OpCode != ValueKind::AllocExistentialBoxInst)
      operand = getLocalValue(ValID, ValResNum,
                         getSILType(Ty2, (SILValueCategory)TyCategory2));

    SmallVector<ProtocolConformance*, 2> conformances;
    while (NumConformances--) {
      auto conformance = MF->readConformance(SILCursor);
      conformances.push_back(conformance);
    }

    auto ctxConformances = MF->getContext().AllocateCopy(conformances);

    switch ((ValueKind)OpCode) {
    default: llvm_unreachable("Out of sync with parent switch");
    case ValueKind::InitExistentialAddrInst:
      ResultVal = Builder.createInitExistentialAddr(Loc, operand,
                                                ConcreteTy,
                                                Ty,
                                                ctxConformances);
      break;
    case ValueKind::InitExistentialMetatypeInst:
      ResultVal = Builder.createInitExistentialMetatype(Loc, operand, Ty,
                                                        ctxConformances);
      break;
    case ValueKind::InitExistentialRefInst:
      ResultVal = Builder.createInitExistentialRef(Loc, Ty,
                                         ConcreteTy,
                                         operand,
                                         ctxConformances);
      break;
    case ValueKind::AllocExistentialBoxInst:
      ResultVal = Builder.createAllocExistentialBox(Loc, Ty, ConcreteTy,
                                  SILType::getPrimitiveAddressType(ConcreteTy),
                                  ctxConformances);
      break;
    }
    break;
  }
  case ValueKind::AllocRefInst:
    assert(RecordKind == SIL_ONE_TYPE_VALUES &&
           "Layout should be OneTypeValues.");
    assert(ListOfValues.size() >= 1 && "Not enough values");
    ResultVal = Builder.createAllocRef(
                  Loc,
                  getSILType(MF->getType(TyID), (SILValueCategory)TyCategory),
                  ListOfValues[0]);
    break;
  case ValueKind::AllocRefDynamicInst: {
    assert(RecordKind == SIL_ONE_TYPE_ONE_OPERAND &&
           "Layout should be OneTypeOneOperand.");
    bool isObjC = Attr & 0x01;
    ResultVal = Builder.createAllocRefDynamic(
                  Loc,
                  getLocalValue(ValID, ValResNum,
                                getSILType(MF->getType(TyID),
                                           (SILValueCategory)TyCategory)),
                  getSILType(MF->getType(TyID2), (SILValueCategory)TyCategory2),
                  isObjC);
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
                                    Substitutions, Args);
    break;
  }
  case ValueKind::TryApplyInst: {
    // Format: attributes such as transparent, the callee's type, a value for
    // the callee and a list of values for the arguments. Each value in the list
    // is represented with 2 IDs: ValueID and ValueResultNumber.  The final
    // two values in the list are the basic block identifiers.
    auto Ty = MF->getType(TyID);
    auto Ty2 = MF->getType(TyID2);
    SILType FnTy = getSILType(Ty, SILValueCategory::Object);
    SILType SubstFnTy = getSILType(Ty2, SILValueCategory::Object);

    SILBasicBlock *errorBB = getBBForReference(Fn, ListOfValues.back());
    ListOfValues = ListOfValues.drop_back();
    SILBasicBlock *normalBB = getBBForReference(Fn, ListOfValues.back());
    ListOfValues = ListOfValues.drop_back();

    SILFunctionType *FTI = SubstFnTy.castTo<SILFunctionType>();
    auto ArgTys = FTI->getParameterSILTypes();
    assert((ArgTys.size() << 1) == ListOfValues.size() &&
           "Argument number mismatch in ApplyInst.");
    SmallVector<SILValue, 4> Args;
    for (unsigned I = 0, E = ListOfValues.size(); I < E; I += 2)
      Args.push_back(getLocalValue(ListOfValues[I], ListOfValues[I+1],
                                   ArgTys[I>>1]));
    unsigned NumSub = NumSubs;

    SmallVector<Substitution, 4> Substitutions;
    while (NumSub--) {
      auto sub = MF->maybeReadSubstitution(SILCursor);
      assert(sub.hasValue() && "missing substitution");
      Substitutions.push_back(*sub);
    }

    ResultVal = Builder.createTryApply(Loc,
                                       getLocalValue(ValID, ValResNum, FnTy),
                                       SubstFnTy, Substitutions, Args,
                                       normalBB, errorBB);
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

    SILValue FnVal = getLocalValue(ValID, ValResNum, FnTy);
    SmallVector<SILValue, 4> Args;
    unsigned unappliedArgs = ArgTys.size() - (ListOfValues.size() >> 1);
    for (unsigned I = 0, E = ListOfValues.size(); I < E; I += 2)
      Args.push_back(getLocalValue(ListOfValues[I], ListOfValues[I+1],
                                   ArgTys[(I>>1) + unappliedArgs]));

    // Compute the result type of the partial_apply, based on which arguments
    // are getting applied.
    SILType closureTy
      = SILBuilder::getPartialApplyResultType(SubstFnTy,
                                              Args.size(),
                                              Fn->getModule(),
                                              {});

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
                                           closureTy);
    break;
  }
  case ValueKind::BuiltinInst: {
    auto ASTTy = MF->getType(TyID);
    auto ResultTy = getSILType(ASTTy, (SILValueCategory)(unsigned)TyID2);
    SmallVector<SILValue, 4> Args;
    for (unsigned i = 0, e = ListOfValues.size(); i < e; i += 4) {
      auto ArgASTTy = MF->getType(ListOfValues[i+2]);
      auto ArgTy = getSILType(ArgASTTy,
                              (SILValueCategory)(unsigned)ListOfValues[i+3]);
      Args.push_back(getLocalValue(ListOfValues[i], ListOfValues[i+1], ArgTy));
    }
    unsigned NumSub = NumSubs;
    SmallVector<Substitution, 4> Substitutions;
    while (NumSub--) {
      auto sub = MF->maybeReadSubstitution(SILCursor);
      assert(sub.hasValue() && "missing substitution");
      Substitutions.push_back(*sub);
    }
    Identifier Name = MF->getIdentifier(ValID);
    
    ResultVal = Builder.createBuiltin(Loc, Name, ResultTy, Substitutions,
                                      Args);
    break;
  }
  case ValueKind::GlobalAddrInst: {
    // Format: Name and type. Use SILOneOperandLayout.
    auto Ty = MF->getType(TyID);
    Identifier Name = MF->getIdentifier(ValID);

    // Find the global variable.
    SILGlobalVariable *g = getGlobalForReference(Name.str());
    assert(g && "Can't deserialize global variable");
    assert(g->getLoweredType().getAddressType() ==
           getSILType(Ty, (SILValueCategory)TyCategory) &&
           "Type of a global variable does not match GlobalAddr.");
    (void)Ty;

    ResultVal = Builder.createGlobalAddr(Loc, g);
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
        getFuncForReference(FuncName.str(),
                            getSILType(Ty, (SILValueCategory)TyCategory)));
    break;
  }
  case ValueKind::MarkDependenceInst: {
    auto Ty = MF->getType(TyID);
    auto Ty2 = MF->getType(TyID2);
    ResultVal = Builder.createMarkDependence(Loc,
        getLocalValue(ValID, ValResNum,
                      getSILType(Ty,  (SILValueCategory)TyCategory)),
        getLocalValue(ValID2, ValResNum2,
                      getSILType(Ty2,  (SILValueCategory)TyCategory2)));
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
    ResultVal = Builder.createUnconditionalCheckedCast(Loc, Val, Ty);
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
  UNARY_INSTRUCTION(RetainValue)
  UNARY_INSTRUCTION(ReleaseValue)
  UNARY_INSTRUCTION(AutoreleaseValue)
  UNARY_INSTRUCTION(DeinitExistentialAddr)
  UNARY_INSTRUCTION(DestroyAddr)
  UNARY_INSTRUCTION(IsNonnull)
  UNARY_INSTRUCTION(Load)
  UNARY_INSTRUCTION(Return)
  UNARY_INSTRUCTION(Throw)
  UNARY_INSTRUCTION(FixLifetime)
  UNARY_INSTRUCTION(CopyBlock)
  UNARY_INSTRUCTION(StrongPin)
  UNARY_INSTRUCTION(StrongUnpin)
  UNARY_INSTRUCTION(StrongRetain)
  UNARY_INSTRUCTION(StrongRelease)
  UNARY_INSTRUCTION(StrongRetainAutoreleased)
  UNARY_INSTRUCTION(AutoreleaseReturn)
  UNARY_INSTRUCTION(StrongRetainUnowned)
  UNARY_INSTRUCTION(UnownedRetain)
  UNARY_INSTRUCTION(UnownedRelease)
  UNARY_INSTRUCTION(IsUnique)
  UNARY_INSTRUCTION(IsUniqueOrPinned)
  UNARY_INSTRUCTION(DebugValue)
  UNARY_INSTRUCTION(DebugValueAddr)
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
  case ValueKind::StructElementAddrInst:
  case ValueKind::StructExtractInst: {
    // Use SILOneValueOneOperandLayout.
    VarDecl *Field = cast<VarDecl>(MF->getDecl(ValID));
    auto Ty = MF->getType(TyID);
    auto Val = getLocalValue(ValID2, ValResNum2,
                             getSILType(Ty, (SILValueCategory)TyCategory));
    auto ResultTy = Val.getType().getFieldType(Field, SILMod);
    if ((ValueKind)OpCode == ValueKind::StructElementAddrInst)
      ResultVal = Builder.createStructElementAddr(Loc, Val, Field,
                                                  ResultTy.getAddressType());
    else
      ResultVal = Builder.createStructExtract(Loc, Val, Field,
                                              ResultTy.getObjectType());
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

    auto ResultTy = TT->getElement(TyID).getType();
    switch ((ValueKind)OpCode) {
    default: llvm_unreachable("Out of sync with parent switch");
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
      Type EltTy = TT->getElement(I >> 1).getType();
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
  case ValueKind::SelectEnumInst:
  case ValueKind::SelectEnumAddrInst: {
    // Format: condition, a list of cases (EnumElementDecl + Value ID),
    // default value ID. Use SILOneTypeValuesLayout: the type is
    // for condition, the list has value for condition, result type,
    //   hasDefault, default
    // basic block ID, a list of (DeclID, BasicBlock ID).
    SILValue Cond = getLocalValue(ListOfValues[0], ListOfValues[1],
                                  getSILType(MF->getType(TyID),
                                             (SILValueCategory)TyCategory));

    Type ResultLoweredTy = MF->getType(ListOfValues[2]);
    SILValueCategory ResultCategory = (SILValueCategory)ListOfValues[3];
    SILType ResultTy = getSILType(ResultLoweredTy, ResultCategory);
    
    SILValue DefaultVal = nullptr;
    if (ListOfValues[4])
      DefaultVal = getLocalValue(ListOfValues[5], ListOfValues[6],
                                   ResultTy);

    SmallVector<std::pair<EnumElementDecl*, SILValue>, 4> CaseVals;
    for (unsigned I = 7, E = ListOfValues.size(); I < E; I += 3) {
      auto Value = getLocalValue(ListOfValues[I+1], ListOfValues[I+2],
                                 ResultTy);
      CaseVals.push_back({cast<EnumElementDecl>(MF->getDecl(ListOfValues[I])),
                         Value});
    }
    if ((ValueKind)OpCode == ValueKind::SelectEnumInst)
      ResultVal = Builder.createSelectEnum(Loc, Cond, ResultTy,
                                           DefaultVal, CaseVals);
    else
      ResultVal = Builder.createSelectEnumAddr(Loc, Cond, ResultTy,
                                               DefaultVal, CaseVals);
    break;
  }
  case ValueKind::SwitchValueInst: {
    // Format: condition, a list of cases (Value ID + Basic Block ID),
    // default basic block ID. Use SILOneTypeValuesLayout: the type is
    // for condition, the list contains value for condition, hasDefault, default
    // basic block ID, a list of (Value ID, BasicBlock ID).
    SILType ResultTy = getSILType(MF->getType(TyID),
                                             (SILValueCategory)TyCategory);
    SILValue Cond = getLocalValue(ListOfValues[0], ListOfValues[1],
                                  getSILType(MF->getType(TyID),
                                             (SILValueCategory)TyCategory));

    SILBasicBlock *DefaultBB = nullptr;
    if (ListOfValues[2])
      DefaultBB = getBBForReference(Fn, ListOfValues[3]);

    SmallVector<std::pair<SILValue, SILBasicBlock*>, 4> CaseBBs;
    for (unsigned I = 4, E = ListOfValues.size(); I < E; I += 3) {
      auto value = getLocalValue(ListOfValues[I], ListOfValues[I+1],
                                 ResultTy);
      CaseBBs.push_back( {value, getBBForReference(Fn, ListOfValues[I+2])} );
    }
    ResultVal = Builder.createSwitchValue(Loc, Cond, DefaultBB, CaseBBs);
    break;
  }
  case ValueKind::SelectValueInst: {
    // Format: condition, a list of cases (ValueID + Value ID),
    // default value ID. Use SILOneTypeValuesLayout: the type is
    // for condition, the list has value for condition, result type,
    // hasDefault, default,
    // basic block ID, a list of (Value ID, Value ID).
    SILValue Cond = getLocalValue(ListOfValues[0], ListOfValues[1],
                                  getSILType(MF->getType(TyID),
                                             (SILValueCategory)TyCategory));

    Type ResultLoweredTy = MF->getType(ListOfValues[2]);
    SILValueCategory ResultCategory = (SILValueCategory)ListOfValues[3];
    SILType ResultTy = getSILType(ResultLoweredTy, ResultCategory);

    SILValue DefaultVal = nullptr;
    if (ListOfValues[4])
      DefaultVal = getLocalValue(ListOfValues[5], ListOfValues[6],
                                 ResultTy);

    SmallVector<std::pair<SILValue, SILValue>, 4> CaseValuesAndResults;
    for (unsigned I = 7, E = ListOfValues.size(); I < E; I += 4) {
      auto CaseValue = getLocalValue(ListOfValues[I], ListOfValues[I+1],
                                     Cond.getType());
      auto Result = getLocalValue(ListOfValues[I+2], ListOfValues[I+3],
                                  ResultTy);
      CaseValuesAndResults.push_back({CaseValue, Result});
    }

    ResultVal = Builder.createSelectValue(Loc, Cond, ResultTy,
                                          DefaultVal, CaseValuesAndResults);
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
    SILType OperandTy = getSILType(MF->getType(TyID),
                                   (SILValueCategory) TyCategory);
    SILType ResultTy = OperandTy.getEnumElementType(Elt, SILMod);
    ResultVal = Builder.createInitEnumDataAddr(Loc,
                    getLocalValue(ValID2, ValResNum2, OperandTy),
                    Elt, ResultTy);
    break;
  }
  case ValueKind::UncheckedEnumDataInst: {
    // Use SILOneValueOneOperandLayout.
    EnumElementDecl *Elt = cast<EnumElementDecl>(MF->getDecl(ValID));
    SILType OperandTy = getSILType(MF->getType(TyID),
                                   (SILValueCategory) TyCategory);
    SILType ResultTy = OperandTy.getEnumElementType(Elt, SILMod);
    ResultVal = Builder.createUncheckedEnumData(Loc,
                    getLocalValue(ValID2, ValResNum2, OperandTy),
                    Elt, ResultTy);
    break;
  }
  case ValueKind::UncheckedTakeEnumDataAddrInst: {
    // Use SILOneValueOneOperandLayout.
    EnumElementDecl *Elt = cast<EnumElementDecl>(MF->getDecl(ValID));
    SILType OperandTy = getSILType(MF->getType(TyID),
                                   (SILValueCategory) TyCategory);
    SILType ResultTy = OperandTy.getEnumElementType(Elt, SILMod);
    ResultVal = Builder.createUncheckedTakeEnumDataAddr(Loc,
                    getLocalValue(ValID2, ValResNum2, OperandTy),
                    Elt, ResultTy);
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
    auto Ty = MF->getType(TyID);
    auto Val = getLocalValue(ValID2, ValResNum2,
                             getSILType(Ty, (SILValueCategory)TyCategory));
    auto ResultTy = Val.getType().getFieldType(Field, SILMod);
    ResultVal = Builder.createRefElementAddr(Loc, Val, Field,
                                             ResultTy);
    break;
  }
  case ValueKind::ClassMethodInst:
  case ValueKind::SuperMethodInst:
  case ValueKind::DynamicMethodInst: {
    // Format: a type, an operand and a SILDeclRef. Use SILOneTypeValuesLayout:
    // type, Attr, SILDeclRef (DeclID, Kind, uncurryLevel, IsObjC),
    // and an operand.
    unsigned NextValueIndex = 1;
    SILDeclRef DRef = getSILDeclRef(MF, ListOfValues, NextValueIndex);
    SILType Ty = getSILType(MF->getType(TyID), (SILValueCategory)TyCategory);
    assert(ListOfValues.size() >= NextValueIndex + 2 &&
           "Out of entries for MethodInst");
    SILType operandTy = getSILType(MF->getType(ListOfValues[NextValueIndex]),
                                   (SILValueCategory)ListOfValues[NextValueIndex+1]);
    NextValueIndex += 2;
    bool IsVolatile = ListOfValues[0] > 0;

    switch ((ValueKind)OpCode) {
    default: llvm_unreachable("Out of sync with parent switch");
    case ValueKind::ClassMethodInst:
      ResultVal = Builder.createClassMethod(Loc,
                    getLocalValue(ListOfValues[NextValueIndex],
                                  ListOfValues[NextValueIndex+1], operandTy),
                    DRef, Ty, IsVolatile);
      break;
    case ValueKind::SuperMethodInst:
      ResultVal = Builder.createSuperMethod(Loc,
                    getLocalValue(ListOfValues[NextValueIndex],
                                  ListOfValues[NextValueIndex+1], operandTy),
                    DRef, Ty, IsVolatile);
      break;
    case ValueKind::DynamicMethodInst:
      ResultVal = Builder.createDynamicMethod(Loc,
                    getLocalValue(ListOfValues[NextValueIndex],
                                  ListOfValues[NextValueIndex+1], operandTy),
                    DRef, Ty, IsVolatile);
      break;
    }
    break;
  }
  case ValueKind::WitnessMethodInst: {
    unsigned NextValueIndex = 0;
    SILDeclRef DRef = getSILDeclRef(MF, ListOfValues, NextValueIndex);
    assert(ListOfValues.size() >= NextValueIndex &&
           "Out of entries for MethodInst");

    CanType Ty = MF->getType(TyID)->getCanonicalType();
    SILType OperandTy = getSILType(MF->getType(TyID2),
                                   (SILValueCategory)TyCategory2);

    auto *Conformance = MF->readConformance(SILCursor);
    // Read the optional opened existential.
    SILValue ExistentialOperand;
    if (TyID3) {
      SILType ExistentialOperandTy =
          getSILType(MF->getType(TyID3), (SILValueCategory)TyCategory3);
      if (ValID3)
        ExistentialOperand = getLocalValue(ValID3, 0, ExistentialOperandTy);
    }
    ResultVal = Builder.createWitnessMethod(
        Loc, Ty, Conformance, DRef, OperandTy, ExistentialOperand, Attr);
    break;
  }
  case ValueKind::DynamicMethodBranchInst: {
    // Format: a typed value, a SILDeclRef, a BasicBlock ID for method,
    // a BasicBlock ID for no method. Use SILOneTypeValuesLayout.
    unsigned NextValueIndex = 2;
    SILDeclRef DRef = getSILDeclRef(MF, ListOfValues, NextValueIndex);
    assert(ListOfValues.size() == NextValueIndex + 2 &&
           "Wrong number of entries for DynamicMethodBranchInst");
    ResultVal = Builder.createDynamicMethodBranch(Loc,
                    getLocalValue(ListOfValues[0], ListOfValues[1],
                                  getSILType(MF->getType(TyID),
                                             (SILValueCategory)TyCategory)),
                    DRef, getBBForReference(Fn, ListOfValues[NextValueIndex]),
                    getBBForReference(Fn, ListOfValues[NextValueIndex+1]));
    break;
  }
  case ValueKind::CheckedCastBranchInst: {
    // Format: the cast kind, a typed value, a BasicBlock ID for success,
    // a BasicBlock ID for failure. Uses SILOneTypeValuesLayout.
    assert(ListOfValues.size() == 7 &&
           "expect 7 numbers for CheckedCastBranchInst");
    bool isExact = ListOfValues[0] != 0;
    SILType opTy = getSILType(MF->getType(ListOfValues[3]),
                              (SILValueCategory)ListOfValues[4]);
    SILValue op = getLocalValue(ListOfValues[1], ListOfValues[2], opTy);
    SILType castTy = getSILType(MF->getType(TyID),
                                (SILValueCategory)TyCategory);
    auto *successBB = getBBForReference(Fn, ListOfValues[5]);
    auto *failureBB = getBBForReference(Fn, ListOfValues[6]);

    ResultVal = Builder.createCheckedCastBranch(Loc, isExact, op, castTy,
                                                successBB, failureBB);
    break;
  }
  case ValueKind::UnconditionalCheckedCastAddrInst:
  case ValueKind::CheckedCastAddrBranchInst: {
    CastConsumptionKind consumption = getCastConsumptionKind(ListOfValues[0]);

    CanType sourceType = MF->getType(ListOfValues[1])->getCanonicalType();
    SILType srcAddrTy = getSILType(MF->getType(ListOfValues[4]),
                                   (SILValueCategory)ListOfValues[5]);
    SILValue src = getLocalValue(ListOfValues[2], ListOfValues[3], srcAddrTy);

    CanType targetType = MF->getType(ListOfValues[6])->getCanonicalType();
    SILType destAddrTy =
      getSILType(MF->getType(TyID), (SILValueCategory) TyCategory);
    SILValue dest = getLocalValue(ListOfValues[7], ListOfValues[8], destAddrTy);

    if (OpCode == (unsigned) ValueKind::UnconditionalCheckedCastAddrInst) {
      ResultVal = Builder.createUnconditionalCheckedCastAddr(Loc, consumption,
                                                             src, sourceType,
                                                             dest, targetType);
      break;
    }

    auto *successBB = getBBForReference(Fn, ListOfValues[9]);
    auto *failureBB = getBBForReference(Fn, ListOfValues[10]);
    ResultVal = Builder.createCheckedCastAddrBranch(Loc, consumption,
                                                    src, sourceType,
                                                    dest, targetType,
                                                    successBB, failureBB);
    break;
  }
  case ValueKind::InitBlockStorageHeaderInst: {
    assert(ListOfValues.size() == 6 &&
           "expected 6 values for InitBlockStorageHeader");
    SILType blockTy
      = getSILType(MF->getType(TyID), (SILValueCategory)TyCategory);

    SILType storageTy = getSILType(MF->getType(ListOfValues[2]),
                                   SILValueCategory::Address);
    SILValue storage
      = getLocalValue(ListOfValues[0], ListOfValues[1], storageTy);

    SILType invokeTy = getSILType(MF->getType(ListOfValues[5]),
                                  SILValueCategory::Object);
    SILValue invoke
      = getLocalValue(ListOfValues[3], ListOfValues[4], invokeTy);

    ResultVal = Builder.createInitBlockStorageHeader(Loc, storage, invoke,
                                                     blockTy);
    break;
  }
  case ValueKind::UnreachableInst: {
    ResultVal = Builder.createUnreachable(Loc);
    break;
  }
  }
  if (ResultVal->hasValue())
    setLocalValue(ResultVal, ++LastValueID);

  for (auto I : InsertedInsn)
    I->setDebugScope(I->getFunction()->getDebugScope());

  return false;
}

SILFunction *SILDeserializer::lookupSILFunction(SILFunction *InFunc) {
  StringRef name = InFunc->getName();
  if (!FuncTable)
    return nullptr;
  auto iter = FuncTable->find(name);
  if (iter == FuncTable->end())
    return nullptr;

  auto Func = readSILFunction(*iter, InFunc, name, /*declarationOnly*/ false);
  if (Func) {
    DEBUG(llvm::dbgs() << "Deserialize SIL:\n";
          Func->dump());
    assert(InFunc->getName() == Func->getName());
  }

  return Func;
}

SILFunction *SILDeserializer::lookupSILFunction(StringRef name) {
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

SILGlobalVariable *SILDeserializer::readGlobalVar(StringRef Name) {
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
  DeclID dID;
  unsigned rawLinkage, isFragile, IsDeclaration;
  GlobalVarLayout::readRecord(scratch, rawLinkage, isFragile, TyID, dID,
                              IsDeclaration);
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
                           SILMod, linkage.getValue(), (IsFragile_t)isFragile,
                           Name.str(), getSILType(Ty, SILValueCategory::Object),
                           None,
                           dID ? cast<VarDecl>(MF->getDecl(dID)): nullptr);
  globalVarOrOffset = v;
  v->setDeclaration(IsDeclaration);

  if (Callback) Callback->didDeserialize(MF->getAssociatedModule(), v);
  return v;
}

void SILDeserializer::getAllSILFunctions() {
  if (!FuncTable)
    return;

  for (auto KI = FuncTable->key_begin(), KE = FuncTable->key_end(); KI != KE;
       ++KI) {
    // Attempt to lookup our name from the output module. If we have a
    // definition already, don't do anything.
    if (SILFunction *F = SILMod.lookUpFunction(*KI))
      if (!F->isExternalDeclaration())
        continue;

    auto DI = FuncTable->find(*KI);
    assert(DI != FuncTable->end() && "There should never be a key without data.");

    SILFunction *fn = readSILFunction(*DI, nullptr, *KI, false,
                                      false/*errorIfEmptyBody*/);

    // Update linkage for global addressors to make it pass verifier.
    if (fn && fn->isGlobalInit() && fn->isExternalDeclaration() &&
        fn->getLinkage() == SILLinkage::Public)
      fn->setLinkage(SILLinkage::PublicExternal);
  }
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
  while (kind != SIL_VTABLE && kind != SIL_WITNESSTABLE &&
         kind != SIL_FUNCTION) {
    assert(kind == SIL_VTABLE_ENTRY &&
           "Content of Vtable should be in SIL_VTABLE_ENTRY.");
    ArrayRef<uint64_t> ListOfValues;
    DeclID NameID;
    VTableEntryLayout::readRecord(scratch, NameID, ListOfValues);
    SILFunction *Func = getFuncForReference(MF->getIdentifier(NameID).str());
    if (Func) {
      unsigned NextValueIndex = 0;
      vtableEntries.emplace_back(getSILDeclRef(MF, ListOfValues,
                                               NextValueIndex), Func);
    }

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
  auto iter = VTableList->find(Name.str());
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

SILWitnessTable *SILDeserializer::readWitnessTable(DeclID WId,
                                                   SILWitnessTable *existingWt,
                                                   bool declarationOnly) {
  if (WId == 0)
    return nullptr;
  assert(WId <= WitnessTables.size() && "invalid WitnessTable ID");

  auto &wTableOrOffset = WitnessTables[WId-1];

  if (wTableOrOffset.isFullyDeserialized() ||
      (wTableOrOffset.isDeserialized() && declarationOnly))
    return wTableOrOffset.get();

  BCOffsetRAII restoreOffset(SILCursor);
  SILCursor.JumpToBit(wTableOrOffset.getOffset());
  auto entry = SILCursor.advance(AF_DontPopBlockAtEnd);
  if (entry.Kind == llvm::BitstreamEntry::Error) {
    DEBUG(llvm::dbgs() << "Cursor advance error in readWitnessTable.\n");
    return nullptr;
  }

  SmallVector<uint64_t, 64> scratch;
  StringRef blobData;
  unsigned kind = SILCursor.readRecord(entry.ID, scratch, &blobData);
  assert(kind == SIL_WITNESSTABLE && "expect a sil witnesstable");
  (void)kind;

  unsigned RawLinkage;
  unsigned IsDeclaration;
  unsigned IsFragile;
  WitnessTableLayout::readRecord(scratch, RawLinkage, IsDeclaration, IsFragile);

  auto Linkage = fromStableSILLinkage(RawLinkage);
  if (!Linkage) {
    DEBUG(llvm::dbgs() << "invalid linkage code " << RawLinkage
                       << " for SILFunction\n");
    MF->error();
    return nullptr;
  }

  // Deserialize Conformance.
  auto theConformance = cast<NormalProtocolConformance>(
                          MF->readConformance(SILCursor));

  if (!existingWt)
    existingWt = SILMod.lookUpWitnessTable(theConformance, false).first;
  auto wT = existingWt;

  // If we have an existing witness table, verify that the conformance matches
  // up.
  if (wT) {
    if (wT->getConformance() != theConformance) {
      DEBUG(llvm::dbgs() << "Conformance mismatch.\n");
      MF->error();
      return nullptr;
    }

    // Don't override the linkage of a witness table with an existing
    // declaration.

  } else {
    // Otherwise, create a new witness table declaration.
    wT = SILWitnessTable::create(SILMod, *Linkage, theConformance);
    if (Callback)
      Callback->didDeserialize(MF->getAssociatedModule(), wT);
  }

  assert(wT->isDeclaration() && "Our witness table at this point must be a "
                                "declaration.");

  // If we are asked to just emit a declaration, return the declaration and say
  // that the witness table is not fully deserialized.
  if (IsDeclaration || declarationOnly) {
    wTableOrOffset.set(wT, /*fully deserialized*/ false);
    return wT;
  }

  // Fetch the next record.
  scratch.clear();
  entry = SILCursor.advance(AF_DontPopBlockAtEnd);
  if (entry.Kind == llvm::BitstreamEntry::EndBlock)
    return nullptr;
  kind = SILCursor.readRecord(entry.ID, scratch);

  std::vector<SILWitnessTable::Entry> witnessEntries;
  // Another SIL_WITNESSTABLE record means the end of this WitnessTable.
  while (kind != SIL_WITNESSTABLE && kind != SIL_FUNCTION) {
    if (kind == SIL_WITNESS_BASE_ENTRY) {
      DeclID protoId;
      WitnessBaseEntryLayout::readRecord(scratch, protoId);
      ProtocolDecl *proto = cast<ProtocolDecl>(MF->getDecl(protoId));
      auto conformance = MF->readConformance(SILCursor);
      witnessEntries.push_back(SILWitnessTable::BaseProtocolWitness{
        proto, conformance
      });
    } else if (kind == SIL_WITNESS_ASSOC_PROTOCOL) {
      DeclID assocId, protoId;
      WitnessAssocProtocolLayout::readRecord(scratch, assocId, protoId);
      ProtocolDecl *proto = cast<ProtocolDecl>(MF->getDecl(protoId));
      auto conformance = MF->readConformance(SILCursor);
      witnessEntries.push_back(SILWitnessTable::AssociatedTypeProtocolWitness{
        cast<AssociatedTypeDecl>(MF->getDecl(assocId)), proto, conformance
      });
    } else if (kind == SIL_WITNESS_ASSOC_ENTRY) {
      DeclID assocId;
      TypeID tyId;
      WitnessAssocEntryLayout::readRecord(scratch, assocId, tyId);
      AssociatedTypeDecl *assoc = cast<AssociatedTypeDecl>(MF->getDecl(assocId));
      witnessEntries.push_back(SILWitnessTable::AssociatedTypeWitness{
        assoc, MF->getType(tyId)->getCanonicalType()
      });
    } else {
      assert(kind == SIL_WITNESS_METHOD_ENTRY &&
             "Content of WitnessTable should be in SIL_WITNESS_METHOD_ENTRY.");
      ArrayRef<uint64_t> ListOfValues;
      DeclID NameID;
      WitnessMethodEntryLayout::readRecord(scratch, NameID, ListOfValues);
      SILFunction *Func = nullptr;
      if (NameID != 0) {
        Func = getFuncForReference(MF->getIdentifier(NameID).str());
      }
      if (Func || NameID == 0) {
        unsigned NextValueIndex = 0;
        witnessEntries.push_back(SILWitnessTable::MethodWitness{
          getSILDeclRef(MF, ListOfValues, NextValueIndex), Func
        });
      }
    }

    // Fetch the next record.
    scratch.clear();
    entry = SILCursor.advance(AF_DontPopBlockAtEnd);
    if (entry.Kind == llvm::BitstreamEntry::EndBlock)
      // EndBlock means the end of this WitnessTable.
      break;
    kind = SILCursor.readRecord(entry.ID, scratch);
  }

  wT->convertToDefinition(witnessEntries, IsFragile != 0);
  wTableOrOffset.set(wT, /*fully deserialized*/ true);
  if (Callback)
    Callback->didDeserializeWitnessTableEntries(MF->getAssociatedModule(), wT);
  return wT;
}

/// Deserialize all WitnessTables inside the module and add them to SILMod.
void SILDeserializer::getAllWitnessTables() {
  if (!WitnessTableList)
    return;
  for (unsigned I = 0, E = WitnessTables.size(); I < E; I++)
    readWitnessTable(I + 1, nullptr, false);
}

SILWitnessTable *
SILDeserializer::lookupWitnessTable(SILWitnessTable *existingWt) {
  assert(existingWt && "Can not deserialize a null witness table declaration.");
  assert(existingWt->isDeclaration() && "Can not deserialize a witness table "
                                        "definition.");

  // If we don't have a witness table list, we can't look anything up.
  if (!WitnessTableList)
    return nullptr;

  // Use the name of the given witness table to lookup the partially
  // deserialized value from the witness table list.
  auto iter = WitnessTableList->find(existingWt->getName());
  if (iter == WitnessTableList->end())
    return nullptr;

  // Attempt to read the witness table.
  auto Wt = readWitnessTable(*iter, existingWt, /*declarationOnly*/ false);
  if (Wt)
    DEBUG(llvm::dbgs() << "Deserialize SIL:\n"; Wt->dump());

  return Wt;
}

SILDeserializer::~SILDeserializer() {
  // Drop our references to anything we've deserialized.
  for (auto &fnEntry : Funcs) {
    if (fnEntry.isDeserialized())
      fnEntry.get()->decrementRefCount();
  }
}

// Invalidate all cached SILFunctions.
void SILDeserializer::invalidateFunctionCache() {
  for (auto &fnEntry : Funcs)
    if (fnEntry.isDeserialized()) {
      fnEntry.get()->decrementRefCount();
      fnEntry.reset();
    }
}
