//===--- DeserializeSIL.cpp - Read SIL ------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "deserialize"
#include "DeserializeSIL.h"
#include "swift/Basic/Defer.h"
#include "swift/AST/ProtocolConformance.h"
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

#include <type_traits>

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
  case SIL_OBJC_SELECTOR: return StringLiteralInst::Encoding::ObjCSelector;
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
  if (SILCursor.AtEndOfStream() || SILIndexCursor.AtEndOfStream())
    return;

  // Load any abbrev records at the start of the block.
  SILCursor.advance();

  llvm::BitstreamCursor cursor = SILIndexCursor;
  // We expect SIL_FUNC_NAMES first, then SIL_VTABLE_NAMES, then
  // SIL_GLOBALVAR_NAMES, then SIL_WITNESS_TABLE_NAMES, and finally
  // SIL_DEFAULT_WITNESS_TABLE_NAMES. But each one can be
  // omitted if no entries exist in the module file.
  unsigned kind = 0;
  while (kind != sil_index_block::SIL_DEFAULT_WITNESS_TABLE_NAMES) {
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
             kind == sil_index_block::SIL_WITNESS_TABLE_NAMES ||
             kind == sil_index_block::SIL_DEFAULT_WITNESS_TABLE_NAMES)) &&
         "Expect SIL_FUNC_NAMES, SIL_VTABLE_NAMES, SIL_GLOBALVAR_NAMES, \
          SIL_WITNESS_TABLE_NAMES, or SIL_DEFAULT_WITNESS_TABLE_NAMES.");
    (void)prevKind;

    if (kind == sil_index_block::SIL_FUNC_NAMES)
      FuncTable = readFuncTable(scratch, blobData);
    else if (kind == sil_index_block::SIL_VTABLE_NAMES)
      VTableList = readFuncTable(scratch, blobData);
    else if (kind == sil_index_block::SIL_GLOBALVAR_NAMES)
      GlobalVarList = readFuncTable(scratch, blobData);
    else if (kind == sil_index_block::SIL_WITNESS_TABLE_NAMES)
      WitnessTableList = readFuncTable(scratch, blobData);
    else if (kind == sil_index_block::SIL_DEFAULT_WITNESS_TABLE_NAMES)
      DefaultWitnessTableList = readFuncTable(scratch, blobData);

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
    } else if (kind == sil_index_block::SIL_WITNESS_TABLE_NAMES) {
      assert((next.Kind == llvm::BitstreamEntry::Record &&
              offKind == sil_index_block::SIL_WITNESS_TABLE_OFFSETS) &&
             "Expect a SIL_WITNESS_TABLE_OFFSETS record.");
      WitnessTables.assign(scratch.begin(), scratch.end());
    } else if (kind == sil_index_block::SIL_DEFAULT_WITNESS_TABLE_NAMES) {
      assert((next.Kind == llvm::BitstreamEntry::Record &&
              offKind == sil_index_block::SIL_DEFAULT_WITNESS_TABLE_OFFSETS) &&
             "Expect a SIL_DEFAULT_WITNESS_TABLE_OFFSETS record.");
      DefaultWitnessTables.assign(scratch.begin(), scratch.end());
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
/// ForwardLocalValues for forward-referenced values (values that are
/// used but not yet defined). LocalValues are updated in setLocalValue where
/// the ID passed in assumes the same ordering as in serializer: in-order
/// for each basic block argument and each SILInstruction that has a value.
/// We update ForwardLocalValues in getLocalValue and when a value is defined
/// in setLocalValue, the corresponding entry in ForwardLocalValues will be
/// erased.
void SILDeserializer::setLocalValue(ValueBase *Value, ValueID Id) {
  ValueBase *&Entry = LocalValues[Id];
  assert(!Entry && "We should not redefine the same value.");

  auto It = ForwardLocalValues.find(Id);
  if (It != ForwardLocalValues.end()) {
    // Take the information about the forward ref out of the map.
    ValueBase *Placeholder = It->second;

    // Remove the entries from the map.
    ForwardLocalValues.erase(It);

    Placeholder->replaceAllUsesWith(Value);
  }

  // Store it in our map.
  Entry = Value;
}

SILValue SILDeserializer::getLocalValue(ValueID Id,
                                        SILType Type) {
  if (Id == 0)
    return SILUndef::get(Type, &SILMod);

  // Check to see if this is already defined.
  ValueBase *Entry = LocalValues.lookup(Id);
  if (Entry) {
    // If this value was already defined, check it to make sure types match.
    assert(Entry->getType() == Type && "Value Type mismatch?");
    return Entry;
  }

  // Otherwise, this is a forward reference.  Create a dummy node to represent
  // it until we see a real definition.
  ValueBase *&Placeholder = ForwardLocalValues[Id];
  if (!Placeholder)
    Placeholder = new (SILMod) GlobalAddrInst(SILDebugLocation(), Type);
  return Placeholder;
}

/// Return the SILBasicBlock of a given ID.
SILBasicBlock *SILDeserializer::getBBForDefinition(SILFunction *Fn,
                                                   SILBasicBlock *Prev,
                                                   unsigned ID) {
  SILBasicBlock *&BB = BlocksByID[ID];
  // If the block has never been named yet, just create it.
  if (BB == nullptr)
    return BB = Fn->createBasicBlock(Prev);

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
  BB = Fn->createBasicBlock();
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
  return M.createFunction(
      SILLinkage::Private, name, type.castTo<SILFunctionType>(), nullptr,
      RegularLocation(loc), IsNotBare, IsNotTransparent, IsNotFragile,
      IsNotThunk, SILFunction::NotRelevant);
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
/// in the module. If we cannot find it in the module, we attempt to
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

  DeclID clangNodeOwnerID;
  TypeID funcTyID;
  GenericEnvironmentID genericEnvID;
  unsigned rawLinkage, isTransparent, isFragile, isThunk, isGlobal,
      inlineStrategy, effect, numSpecAttrs, hasQualifiedOwnership;
  ArrayRef<uint64_t> SemanticsIDs;
  // TODO: read fragile
  SILFunctionLayout::readRecord(scratch, rawLinkage, isTransparent, isFragile,
                                isThunk, isGlobal, inlineStrategy, effect,
                                numSpecAttrs, hasQualifiedOwnership, funcTyID,
                                genericEnvID, clangNodeOwnerID, SemanticsIDs);

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

  ValueDecl *clangNodeOwner = nullptr;
  if (clangNodeOwnerID != 0) {
    clangNodeOwner = dyn_cast_or_null<ValueDecl>(MF->getDecl(clangNodeOwnerID));
    if (!clangNodeOwner) {
      DEBUG(llvm::dbgs() << "invalid clang node owner for SILFunction\n");
      MF->error();
      return nullptr;
    }
  }

  // If we weren't handed a function, check for an existing
  // declaration in the output module.
  if (!existingFn) existingFn = SILMod.lookUpFunction(name);
  auto fn = existingFn;

  // TODO: use the correct SILLocation from module.
  SILLocation loc = RegularLocation(SourceLoc());

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
    fn = SILMod.createFunction(
        linkage.getValue(), name, ty.castTo<SILFunctionType>(), nullptr, loc,
        IsNotBare, IsTransparent_t(isTransparent == 1),
        IsFragile_t(isFragile == 1), IsThunk_t(isThunk),
        SILFunction::NotRelevant, (Inline_t)inlineStrategy);
    fn->setGlobalInit(isGlobal == 1);
    fn->setEffectsKind((EffectsKind)effect);
    if (clangNodeOwner)
      fn->setClangNodeOwner(clangNodeOwner);
    for (auto ID : SemanticsIDs) {
      fn->addSemanticsAttr(MF->getIdentifier(ID).str());
    }

    if (Callback) Callback->didDeserialize(MF->getAssociatedModule(), fn);
  }

  assert(fn->empty() &&
         "SILFunction to be deserialized starts being empty.");

  fn->setBare(IsBare);
  const SILDebugScope *DS = fn->getDebugScope();
  if (!DS) {
    DS = new (SILMod) SILDebugScope(loc, fn);
    fn->setDebugScope(DS);
  }

  // Read and instantiate the specialize attributes.
  while (numSpecAttrs--) {
    auto next = SILCursor.advance(AF_DontPopBlockAtEnd);
    assert(next.Kind == llvm::BitstreamEntry::Record);

    scratch.clear();
    kind = SILCursor.readRecord(next.ID, scratch);
    assert(kind == SIL_SPECIALIZE_ATTR && "Missing specialization attribute");

    unsigned exported;
    unsigned specializationKindVal;
    SILSpecializeAttrLayout::readRecord(scratch, exported, specializationKindVal);
    SILSpecializeAttr::SpecializationKind specializationKind =
        specializationKindVal ? SILSpecializeAttr::SpecializationKind::Partial
                              : SILSpecializeAttr::SpecializationKind::Full;

    SmallVector<Requirement, 8> requirements;
    MF->readGenericRequirements(requirements, SILCursor);

    // Read the substitution list and construct a SILSpecializeAttr.
    fn->addSpecializeAttr(SILSpecializeAttr::create(
        SILMod, requirements, exported != 0, specializationKind));
  }

  GenericEnvironment *genericEnv = nullptr;
  if (!declarationOnly)
    genericEnv = MF->getGenericEnvironment(genericEnvID);

  // If the next entry is the end of the block, then this function has
  // no contents.
  entry = SILCursor.advance(AF_DontPopBlockAtEnd);
  bool isEmptyFunction = (entry.Kind == llvm::BitstreamEntry::EndBlock);
  assert((!isEmptyFunction || !genericEnv) &&
         "generic environment without body?!");

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

  if (!hasQualifiedOwnership)
    fn->setUnqualifiedOwnership();

  NumDeserializedFunc++;

  assert(!(fn->getGenericEnvironment() && !fn->empty())
         && "function already has context generic params?!");
  if (genericEnv)
    fn->setGenericEnvironment(genericEnv);

  scratch.clear();
  kind = SILCursor.readRecord(entry.ID, scratch);

  SILBasicBlock *CurrentBB = nullptr;

  // Clear up at the beginning of each SILFunction.
  BasicBlockID = 0;
  BlocksByID.clear();
  UndefinedBlocks.clear();
  LastValueID = 0;
  LocalValues.clear();
  ForwardLocalValues.clear();

  SILOpenedArchetypesTracker OpenedArchetypesTracker(*fn);
  SILBuilder Builder(*fn);
  // Track the archetypes just like SILGen. This
  // is required for adding typedef operands to instructions.
  Builder.setOpenedArchetypesTracker(&OpenedArchetypesTracker);

  // Define a callback to be invoked on the deserialized types.
  auto OldDeserializedTypeCallback = MF->DeserializedTypeCallback;
  SWIFT_DEFER {
    MF->DeserializedTypeCallback = OldDeserializedTypeCallback;
  };

  MF->DeserializedTypeCallback = [&OpenedArchetypesTracker] (Type ty) {
    OpenedArchetypesTracker.registerUsedOpenedArchetypes(ty);
  };

  // Another SIL_FUNCTION record means the end of this SILFunction.
  // SIL_VTABLE or SIL_GLOBALVAR or SIL_WITNESS_TABLE record also means the end
  // of this SILFunction.
  while (kind != SIL_FUNCTION && kind != SIL_VTABLE && kind != SIL_GLOBALVAR &&
         kind != SIL_WITNESS_TABLE) {
    if (kind == SIL_BASIC_BLOCK)
      // Handle a SILBasicBlock record.
      CurrentBB = readSILBasicBlock(fn, CurrentBB, scratch);
    else {
      // If CurrentBB is empty, just return fn. The code in readSILInstruction
      // assumes that such a situation means that fn is a declaration. Thus it
      // is using return false to mean two different things, error a failure
      // occurred and this is a declaration. Work around that for now.
      if (!CurrentBB)
        return fn;

      // Handle a SILInstruction record.
      if (readSILInstruction(fn, CurrentBB, Builder, kind, scratch)) {
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

  // Check that there are no unresolved forward definitions of opened
  // archetypes.
  if (OpenedArchetypesTracker.hasUnresolvedOpenedArchetypeDefinitions())
    llvm_unreachable(
        "All forward definitions of opened archetypes should be resolved");

  if (Callback)
    Callback->didDeserializeFunctionBody(MF->getAssociatedModule(), fn);

  return fn;
}

// We put these static asserts here to formalize our assumption that both
// SILValueCategory and ValueOwnershipKind have uint8_t as their underlying
// pointer values.
static_assert(
    std::is_same<std::underlying_type<SILValueCategory>::type, uint8_t>::value,
    "Expected an underlying uint8_t type");
// We put these static asserts here to formalize our assumption that both
// SILValueCategory and ValueOwnershipKind have uint8_t as their underlying
// pointer values.
static_assert(
    std::is_same<std::underlying_type<ValueOwnershipKind::innerty>::type,
                 uint8_t>::value,
    "Expected an underlying uint8_t type");
SILBasicBlock *SILDeserializer::readSILBasicBlock(SILFunction *Fn,
                                                  SILBasicBlock *Prev,
                                    SmallVectorImpl<uint64_t> &scratch) {
  ArrayRef<uint64_t> Args;
  SILBasicBlockLayout::readRecord(scratch, Args);

  // Args should be a list of triples of the following form:
  //
  // 1. A TypeID.
  // 2. A flag of metadata. This currently includes the SILValueCategory and
  //    ValueOwnershipKind. We enforce size constraints of these types above.
  // 3. A ValueID.
  SILBasicBlock *CurrentBB = getBBForDefinition(Fn, Prev, BasicBlockID++);
  bool IsEntry = CurrentBB->isEntry();
  for (unsigned I = 0, E = Args.size(); I < E; I += 3) {
    TypeID TyID = Args[I];
    if (!TyID) return nullptr;
    ValueID ValId = Args[I+2];
    if (!ValId) return nullptr;

    auto ArgTy = MF->getType(TyID);
    SILArgument *Arg;
    auto ValueCategory = SILValueCategory(Args[I + 1] & 0xF);
    SILType SILArgTy = getSILType(ArgTy, ValueCategory);
    if (IsEntry) {
      Arg = CurrentBB->createFunctionArgument(SILArgTy);
    } else {
      auto OwnershipKind = ValueOwnershipKind((Args[I + 1] >> 8) & 0xF);
      Arg = CurrentBB->createPHIArgument(SILArgTy, OwnershipKind);
    }
    LastValueID = LastValueID + 1;
    setLocalValue(Arg, LastValueID);
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
                                         SILBuilder &Builder,
                                         unsigned RecordKind,
                                         SmallVectorImpl<uint64_t> &scratch) {
  // Return error if Basic Block is null.
  if (!BB)
    return true;

  Builder.setInsertionPoint(BB);
  Builder.setCurrentDebugScope(Fn->getDebugScope());
  unsigned OpCode = 0, TyCategory = 0, TyCategory2 = 0, TyCategory3 = 0,
           Attr = 0, NumSubs = 0, NumConformances = 0, IsNonThrowingApply = 0;
  ValueID ValID, ValID2, ValID3;
  TypeID TyID, TyID2, TyID3;
  TypeID ConcreteTyID;
  ModuleID OwningModuleID;
  SourceLoc SLoc;
  ArrayRef<uint64_t> ListOfValues;
  SILLocation Loc = RegularLocation(SLoc);

  switch (RecordKind) {
  default:
    llvm_unreachable("Record kind for a SIL instruction is not supported.");
  case SIL_ONE_VALUE_ONE_OPERAND:
    SILOneValueOneOperandLayout::readRecord(scratch, OpCode, Attr,
                                            ValID, TyID, TyCategory,
                                            ValID2);
    break;
  case SIL_ONE_TYPE:
    SILOneTypeLayout::readRecord(scratch, OpCode, TyID, TyCategory);
    break;
  case SIL_ONE_OPERAND:
    SILOneOperandLayout::readRecord(scratch, OpCode, Attr,
                                    TyID, TyCategory, ValID);
    break;
  case SIL_ONE_TYPE_ONE_OPERAND:
    SILOneTypeOneOperandLayout::readRecord(scratch, OpCode, Attr,
                                           TyID, TyCategory,
                                           TyID2, TyCategory2,
                                           ValID);
    break;
  case SIL_INIT_EXISTENTIAL:
    SILInitExistentialLayout::readRecord(scratch, OpCode,
                                         TyID, TyCategory,
                                         TyID2, TyCategory2,
                                         ValID,
                                         ConcreteTyID,
                                         NumConformances);
    break;
  case SIL_INST_CAST:
    SILInstCastLayout::readRecord(scratch, OpCode, Attr,
                                  TyID, TyCategory,
                                  TyID2, TyCategory2,
                                  ValID);
    break;
  case SIL_ONE_TYPE_VALUES:
    SILOneTypeValuesLayout::readRecord(scratch, OpCode, TyID, TyCategory,
                                       ListOfValues);
    break;
  case SIL_TWO_OPERANDS:
    SILTwoOperandsLayout::readRecord(scratch, OpCode, Attr,
                                     TyID, TyCategory, ValID,
                                     TyID2, TyCategory2, ValID2);
    break;
  case SIL_TAIL_ADDR:
    SILTailAddrLayout::readRecord(scratch, OpCode,
                                  TyID, ValID,
                                  TyID2, ValID2,
                                  TyID3);
    break;
  case SIL_INST_APPLY: {
    unsigned IsPartial;
    SILInstApplyLayout::readRecord(scratch, IsPartial, NumSubs,
                                   TyID, TyID2, ValID, ListOfValues);
    switch (IsPartial) {
    case SIL_APPLY:
      OpCode = (unsigned)ValueKind::ApplyInst;
      break;
    case SIL_PARTIAL_APPLY:
      OpCode = (unsigned)ValueKind::PartialApplyInst;
      break;
    case SIL_BUILTIN:
      OpCode = (unsigned)ValueKind::BuiltinInst;
      break;
    case SIL_TRY_APPLY:
      OpCode = (unsigned)ValueKind::TryApplyInst;
      break;
    case SIL_NON_THROWING_APPLY:
      OpCode = (unsigned)ValueKind::ApplyInst;
      IsNonThrowingApply = true;
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

  SILInstruction *ResultVal;
  switch ((ValueKind)OpCode) {
  case ValueKind::SILPHIArgument:
  case ValueKind::SILFunctionArgument:
  case ValueKind::SILUndef:
    llvm_unreachable("not an instruction");

  case ValueKind::DebugValueInst:
  case ValueKind::DebugValueAddrInst:
    llvm_unreachable("not supported");

  case ValueKind::AllocBoxInst:
    assert(RecordKind == SIL_ONE_TYPE && "Layout should be OneType.");
    ResultVal = Builder.createAllocBox(Loc,
                       cast<SILBoxType>(MF->getType(TyID)->getCanonicalType()));
    break;
  
#define ONETYPE_INST(ID)                      \
  case ValueKind::ID##Inst:                   \
    assert(RecordKind == SIL_ONE_TYPE && "Layout should be OneType.");         \
    ResultVal = Builder.create##ID(Loc,                                        \
                  getSILType(MF->getType(TyID), (SILValueCategory)TyCategory));\
    break;
  ONETYPE_INST(AllocStack)
  ONETYPE_INST(Metatype)
#undef ONETYPE_INST
#define ONETYPE_ONEOPERAND_INST(ID)           \
  case ValueKind::ID##Inst:                   \
    assert(RecordKind == SIL_ONE_TYPE_ONE_OPERAND &&       \
           "Layout should be OneTypeOneOperand.");         \
    ResultVal = Builder.create##ID(Loc,                    \
                  getSILType(MF->getType(TyID), (SILValueCategory)TyCategory), \
                  getLocalValue(ValID,                              \
                    getSILType(MF->getType(TyID2),                             \
                               (SILValueCategory)TyCategory2)));               \
    break;
  ONETYPE_ONEOPERAND_INST(ValueMetatype)
  ONETYPE_ONEOPERAND_INST(ExistentialMetatype)
  ONETYPE_ONEOPERAND_INST(AllocValueBuffer)
  ONETYPE_ONEOPERAND_INST(ProjectValueBuffer)
  ONETYPE_ONEOPERAND_INST(ProjectExistentialBox)
  ONETYPE_ONEOPERAND_INST(DeallocValueBuffer)
#undef ONETYPE_ONEOPERAND_INST
  case ValueKind::DeallocBoxInst:
    assert(RecordKind == SIL_ONE_TYPE_ONE_OPERAND &&
           "Layout should be OneTypeOneOperand.");
    ResultVal = Builder.createDeallocBox(Loc,
                  getLocalValue(ValID,
                    getSILType(MF->getType(TyID2),
                               (SILValueCategory)TyCategory2)));
    break;

#define ONEOPERAND_ONETYPE_INST(ID)           \
  case ValueKind::ID##Inst:                   \
    assert(RecordKind == SIL_ONE_TYPE_ONE_OPERAND &&       \
           "Layout should be OneTypeOneOperand.");         \
    ResultVal = Builder.create##ID(Loc,                    \
                  getLocalValue(ValID,                              \
                    getSILType(MF->getType(TyID2),                             \
                               (SILValueCategory)TyCategory2)),                \
                  getSILType(MF->getType(TyID), (SILValueCategory)TyCategory));\
    break;
  ONEOPERAND_ONETYPE_INST(OpenExistentialAddr)
  ONEOPERAND_ONETYPE_INST(OpenExistentialRef)
  ONEOPERAND_ONETYPE_INST(OpenExistentialMetatype)
  ONEOPERAND_ONETYPE_INST(OpenExistentialBox)
  ONEOPERAND_ONETYPE_INST(OpenExistentialOpaque)
  // Conversion instructions.
  ONEOPERAND_ONETYPE_INST(UncheckedRefCast)
  ONEOPERAND_ONETYPE_INST(UncheckedAddrCast)
  ONEOPERAND_ONETYPE_INST(UncheckedTrivialBitCast)
  ONEOPERAND_ONETYPE_INST(UncheckedBitwiseCast)
  ONEOPERAND_ONETYPE_INST(BridgeObjectToRef)
  ONEOPERAND_ONETYPE_INST(BridgeObjectToWord)
  ONEOPERAND_ONETYPE_INST(Upcast)
  ONEOPERAND_ONETYPE_INST(AddressToPointer)
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

  case ValueKind::ProjectBoxInst: {
    assert(RecordKind == SIL_ONE_TYPE_ONE_OPERAND &&
           "Layout should be OneTypeOneOperand.");
    ResultVal = Builder.createProjectBox(Loc,
                  getLocalValue(ValID,
                    getSILType(MF->getType(TyID2),
                               (SILValueCategory)TyCategory2)),
                  TyID);
    break;
  }

  case ValueKind::PointerToAddressInst: {
    assert(RecordKind == SIL_ONE_TYPE_ONE_OPERAND &&
           "Layout should be OneTypeOneOperand.");
    bool isStrict = Attr & 0x01;
    ResultVal = Builder.createPointerToAddress(
      Loc,
      getLocalValue(ValID, getSILType(MF->getType(TyID2),
                                      (SILValueCategory)TyCategory2)),
      getSILType(MF->getType(TyID), (SILValueCategory)TyCategory),
      isStrict);
    break;
  }
  case ValueKind::DeallocExistentialBoxInst: {
    assert(RecordKind == SIL_ONE_TYPE_ONE_OPERAND &&
           "Layout should be OneTypeOneOperand.");
    ResultVal = Builder.createDeallocExistentialBox(Loc,
                  MF->getType(TyID)->getCanonicalType(),
                  getLocalValue(ValID,
                    getSILType(MF->getType(TyID2),
                               (SILValueCategory)TyCategory2)));
    break;

  }
  
  case ValueKind::RefToBridgeObjectInst: {
    auto RefTy = getSILType(MF->getType(TyID), (SILValueCategory)TyCategory);
    auto Ref = getLocalValue(ValID, RefTy);
    auto BitsTy = getSILType(MF->getType(TyID2), (SILValueCategory)TyCategory2);
    auto Bits = getLocalValue(ValID2, BitsTy);
    
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
      operand = getLocalValue(ValID,
                         getSILType(Ty2, (SILValueCategory)TyCategory2));

    SmallVector<ProtocolConformanceRef, 2> conformances;
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
                                  ctxConformances);
      break;
    }
    break;
  }
  case ValueKind::AllocRefInst:
  case ValueKind::AllocRefDynamicInst: {
    assert(RecordKind == SIL_ONE_TYPE_VALUES &&
           "Layout should be OneTypeValues.");
    unsigned NumVals = ListOfValues.size();
    assert(NumVals >= 1 && "Not enough values");
    unsigned Flags = ListOfValues[0];
    bool isObjC = (bool)(Flags & 1);
    bool canAllocOnStack = (bool)((Flags >> 1) & 1);
    SILType ClassTy = getSILType(MF->getType(TyID), (SILValueCategory)TyCategory);
    SmallVector<SILValue, 4> Counts;
    SmallVector<SILType, 4> TailTypes;
    unsigned i = 1;
    for (; i + 2 < NumVals; i += 3) {
      SILType TailType = getSILType(MF->getType(ListOfValues[i]),
                                    SILValueCategory::Object);
      TailTypes.push_back(TailType);
      SILType CountType = getSILType(MF->getType(ListOfValues[i+2]),
                                     SILValueCategory::Object);
      SILValue CountVal = getLocalValue(ListOfValues[i+1], CountType);
      Counts.push_back(CountVal);
    }
    if ((ValueKind)OpCode == ValueKind::AllocRefDynamicInst) {
      assert(i + 2 == NumVals);
      assert(!canAllocOnStack);
      SILType MetadataType = getSILType(MF->getType(ListOfValues[i+1]),
                                        SILValueCategory::Object);
      SILValue MetadataOp = getLocalValue(ListOfValues[i], MetadataType);
      ResultVal = Builder.createAllocRefDynamic(Loc, MetadataOp, ClassTy,
                                                isObjC, TailTypes, Counts);
    } else {
      assert(i == NumVals);
      ResultVal = Builder.createAllocRef(Loc, ClassTy, isObjC, canAllocOnStack,
                                         TailTypes, Counts);
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
    SILFunctionConventions substConventions(SubstFnTy.castTo<SILFunctionType>(),
                                            Builder.getModule());
    assert(substConventions.getNumSILArguments() == ListOfValues.size()
           && "Argument number mismatch in ApplyInst.");
    SmallVector<SILValue, 4> Args;
    for (unsigned I = 0, E = ListOfValues.size(); I < E; I++)
      Args.push_back(getLocalValue(ListOfValues[I],
                                   substConventions.getSILArgumentType(I)));
    unsigned NumSub = NumSubs;

    SmallVector<Substitution, 4> Substitutions;
    while (NumSub--) {
      auto sub = MF->maybeReadSubstitution(SILCursor);
      assert(sub.hasValue() && "missing substitution");
      Substitutions.push_back(*sub);
    }

    ResultVal =
        Builder.createApply(Loc, getLocalValue(ValID, FnTy), SubstFnTy,
                            substConventions.getSILResultType(), Substitutions,
                            Args, IsNonThrowingApply != 0);
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

    SILFunctionConventions substConventions(SubstFnTy.castTo<SILFunctionType>(),
                                            Builder.getModule());
    assert(substConventions.getNumSILArguments() == ListOfValues.size()
           && "Argument number mismatch in ApplyInst.");
    SmallVector<SILValue, 4> Args;
    for (unsigned I = 0, E = ListOfValues.size(); I < E; I++)
      Args.push_back(getLocalValue(ListOfValues[I],
                                   substConventions.getSILArgumentType(I)));
    unsigned NumSub = NumSubs;

    SmallVector<Substitution, 4> Substitutions;
    while (NumSub--) {
      auto sub = MF->maybeReadSubstitution(SILCursor);
      assert(sub.hasValue() && "missing substitution");
      Substitutions.push_back(*sub);
    }

    ResultVal = Builder.createTryApply(Loc,
                                       getLocalValue(ValID, FnTy),
                                       SubstFnTy, Substitutions, Args,
                                       normalBB, errorBB);
    break;
  }
  case ValueKind::PartialApplyInst: {
    auto Ty = MF->getType(TyID);
    auto Ty2 = MF->getType(TyID2);
    SILType FnTy = getSILType(Ty, SILValueCategory::Object);
    SILType closureTy = getSILType(Ty2, SILValueCategory::Object);

    unsigned NumSub = NumSubs;
    SmallVector<Substitution, 4> Substitutions;
    while (NumSub--) {
      auto sub = MF->maybeReadSubstitution(SILCursor);
      assert(sub.hasValue() && "missing substitution");
      Substitutions.push_back(*sub);
    }
    
    auto SubstFnTy = SILType::getPrimitiveObjectType(
      FnTy.castTo<SILFunctionType>()
        ->substGenericArgs(Builder.getModule(), Substitutions));
    SILFunctionConventions fnConv(SubstFnTy.castTo<SILFunctionType>(),
                                  Builder.getModule());

    unsigned numArgs = fnConv.getNumSILArguments();
    assert(numArgs >= ListOfValues.size()
           && "Argument number mismatch in PartialApplyInst.");

    SILValue FnVal = getLocalValue(ValID, FnTy);
    SmallVector<SILValue, 4> Args;
    unsigned unappliedArgs = numArgs - ListOfValues.size();
    for (unsigned I = 0, E = ListOfValues.size(); I < E; I++)
      Args.push_back(getLocalValue(
          ListOfValues[I], fnConv.getSILArgumentType(I + unappliedArgs)));

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
    for (unsigned i = 0, e = ListOfValues.size(); i < e; i += 3) {
      auto ArgASTTy = MF->getType(ListOfValues[i+1]);
      auto ArgTy = getSILType(ArgASTTy,
                              (SILValueCategory)(unsigned)ListOfValues[i+2]);
      Args.push_back(getLocalValue(ListOfValues[i], ArgTy));
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
  case ValueKind::AllocGlobalInst: {
    // Format: Name and type. Use SILOneOperandLayout.
    Identifier Name = MF->getIdentifier(ValID);

    // Find the global variable.
    SILGlobalVariable *g = getGlobalForReference(Name.str());
    assert(g && "Can't deserialize global variable");

    ResultVal = Builder.createAllocGlobal(Loc, g);
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
        getLocalValue(ValID,
                      getSILType(Ty, (SILValueCategory)TyCategory)));
    break;
  }
  case ValueKind::DeallocRefInst: {
    auto Ty = MF->getType(TyID);
    bool OnStack = (bool)Attr;
    ResultVal = Builder.createDeallocRef(Loc,
        getLocalValue(ValID,
                      getSILType(Ty, (SILValueCategory)TyCategory)), OnStack);
    break;
  }
  case ValueKind::DeallocPartialRefInst: {
    auto Ty = MF->getType(TyID);
    auto Ty2 = MF->getType(TyID2);
    ResultVal = Builder.createDeallocPartialRef(Loc,
        getLocalValue(ValID,
                      getSILType(Ty,  (SILValueCategory)TyCategory)),
        getLocalValue(ValID2,
                      getSILType(Ty2,  (SILValueCategory)TyCategory2)));
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
        getLocalValue(ValID,
                      getSILType(Ty,  (SILValueCategory)TyCategory)),
        getLocalValue(ValID2,
                      getSILType(Ty2,  (SILValueCategory)TyCategory2)));
    break;
  }
  case ValueKind::IndexAddrInst: {
    auto Ty = MF->getType(TyID);
    auto Ty2 = MF->getType(TyID2);
    ResultVal = Builder.createIndexAddr(Loc,
        getLocalValue(ValID,
                      getSILType(Ty,  (SILValueCategory)TyCategory)),
        getLocalValue(ValID2,
                      getSILType(Ty2,  (SILValueCategory)TyCategory2)));
    break;
  }
  case ValueKind::TailAddrInst: {
    auto Ty = MF->getType(TyID);
    auto Ty2 = MF->getType(TyID2);
    auto ResultTy = MF->getType(TyID3);
    ResultVal = Builder.createTailAddr(Loc,
        getLocalValue(ValID, getSILType(Ty, SILValueCategory::Address)),
        getLocalValue(ValID2, getSILType(Ty2, SILValueCategory::Object)),
        getSILType(ResultTy, SILValueCategory::Address));
    break;
  }
  case ValueKind::IndexRawPointerInst: {
    auto Ty = MF->getType(TyID);
    auto Ty2 = MF->getType(TyID2);
    ResultVal = Builder.createIndexRawPointer(Loc,
        getLocalValue(ValID,
                      getSILType(Ty,  (SILValueCategory)TyCategory)),
        getLocalValue(ValID2,
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
    for (unsigned I = 0, E = ListOfValues.size(); I < E; I += 3) {
      auto EltTy = MF->getType(ListOfValues[I]);
      OpList.push_back(
        getLocalValue(ListOfValues[I+2],
                      getSILType(EltTy, (SILValueCategory)ListOfValues[I+1])));
    }
    ResultVal = Builder.createMarkFunctionEscape(Loc, OpList);
    break;
  }
  // Checked Conversion instructions.
  case ValueKind::UnconditionalCheckedCastInst: {
    SILValue Val = getLocalValue(ValID,
                 getSILType(MF->getType(TyID2), (SILValueCategory)TyCategory2));
    SILType Ty = getSILType(MF->getType(TyID), (SILValueCategory)TyCategory);
    ResultVal = Builder.createUnconditionalCheckedCast(Loc, Val, Ty);
    break;
  }

#define UNARY_INSTRUCTION(ID) \
  case ValueKind::ID##Inst:                   \
    assert(RecordKind == SIL_ONE_OPERAND &&            \
           "Layout should be OneOperand.");            \
    ResultVal = Builder.create##ID(Loc, getLocalValue(ValID,  \
                    getSILType(MF->getType(TyID),                        \
                               (SILValueCategory)TyCategory)));          \
   break;

#define REFCOUNTING_INSTRUCTION(ID) \
  case ValueKind::ID##Inst:                   \
    assert(RecordKind == SIL_ONE_OPERAND &&            \
           "Layout should be OneOperand.");            \
    ResultVal = Builder.create##ID(Loc, getLocalValue(ValID,  \
                    getSILType(MF->getType(TyID),                        \
                               (SILValueCategory)TyCategory)),  \
                                   (Atomicity)Attr);          \
    break;

  UNARY_INSTRUCTION(CondFail)
  REFCOUNTING_INSTRUCTION(RetainValue)
  UNARY_INSTRUCTION(UnmanagedRetainValue)
  UNARY_INSTRUCTION(CopyValue)
  UNARY_INSTRUCTION(CopyUnownedValue)
  UNARY_INSTRUCTION(DestroyValue)
  REFCOUNTING_INSTRUCTION(ReleaseValue)
  UNARY_INSTRUCTION(UnmanagedReleaseValue)
  REFCOUNTING_INSTRUCTION(AutoreleaseValue)
  UNARY_INSTRUCTION(UnmanagedAutoreleaseValue)
  REFCOUNTING_INSTRUCTION(SetDeallocating)
  UNARY_INSTRUCTION(DeinitExistentialAddr)
  UNARY_INSTRUCTION(EndBorrowArgument)
  UNARY_INSTRUCTION(DestroyAddr)
  UNARY_INSTRUCTION(IsNonnull)
  UNARY_INSTRUCTION(Return)
  UNARY_INSTRUCTION(Throw)
  UNARY_INSTRUCTION(FixLifetime)
  UNARY_INSTRUCTION(CopyBlock)
  UNARY_INSTRUCTION(LoadBorrow)
  UNARY_INSTRUCTION(BeginBorrow)
  REFCOUNTING_INSTRUCTION(StrongPin)
  REFCOUNTING_INSTRUCTION(StrongUnpin)
  REFCOUNTING_INSTRUCTION(StrongRetain)
  REFCOUNTING_INSTRUCTION(StrongRelease)
  REFCOUNTING_INSTRUCTION(StrongRetainUnowned)
  REFCOUNTING_INSTRUCTION(UnownedRetain)
  REFCOUNTING_INSTRUCTION(UnownedRelease)
  UNARY_INSTRUCTION(IsUnique)
  UNARY_INSTRUCTION(IsUniqueOrPinned)
#undef UNARY_INSTRUCTION
#undef REFCOUNTING_INSTRUCTION

  case ValueKind::LoadInst: {
    auto Ty = MF->getType(TyID);
    auto Qualifier = LoadOwnershipQualifier(Attr);
    ResultVal = Builder.createLoad(
        Loc, getLocalValue(ValID, getSILType(Ty, (SILValueCategory)TyCategory)),
        Qualifier);
    break;
  }

  case ValueKind::LoadUnownedInst: {
    auto Ty = MF->getType(TyID);
    bool isTake = (Attr > 0);
    ResultVal = Builder.createLoadUnowned(Loc,
        getLocalValue(ValID,
                      getSILType(Ty, (SILValueCategory)TyCategory)),
        IsTake_t(isTake));
    break;
  }
  case ValueKind::LoadWeakInst: {
    auto Ty = MF->getType(TyID);
    bool isTake = (Attr > 0);
    ResultVal = Builder.createLoadWeak(Loc,
        getLocalValue(ValID,
                      getSILType(Ty, (SILValueCategory)TyCategory)),
        IsTake_t(isTake));
    break;
  }
  case ValueKind::MarkUninitializedInst: {
    auto Ty = getSILType(MF->getType(TyID), (SILValueCategory)TyCategory);
    auto Kind = (MarkUninitializedInst::Kind)Attr;
    auto Val = getLocalValue(ValID, Ty);
    ResultVal = Builder.createMarkUninitialized(Loc, Val, Kind);
    break;
  }
  case ValueKind::StoreInst: {
    auto Ty = MF->getType(TyID);
    SILType addrType = getSILType(Ty, (SILValueCategory)TyCategory);
    SILType ValType = addrType.getObjectType();
    auto Qualifier = StoreOwnershipQualifier(Attr);
    ResultVal = Builder.createStore(Loc, getLocalValue(ValID, ValType),
                                    getLocalValue(ValID2, addrType), Qualifier);
    break;
  }
  case ValueKind::StoreBorrowInst: {
    auto Ty = MF->getType(TyID);
    SILType addrType = getSILType(Ty, (SILValueCategory)TyCategory);
    SILType ValType = addrType.getObjectType();
    ResultVal = Builder.createStoreBorrow(Loc, getLocalValue(ValID, ValType),
                                          getLocalValue(ValID2, addrType));
    break;
  }
  case ValueKind::EndBorrowInst: {
    SILValue BorrowSource, BorrowDest;
    BorrowSource = getLocalValue(
        ValID, getSILType(MF->getType(TyID), (SILValueCategory)TyCategory));
    BorrowDest = getLocalValue(
        ValID2, getSILType(MF->getType(TyID2), (SILValueCategory)TyCategory2));
    ResultVal = Builder.createEndBorrow(Loc, BorrowSource, BorrowDest);
    break;
  }
  case ValueKind::StoreUnownedInst: {
    auto Ty = MF->getType(TyID);
    SILType addrType = getSILType(Ty, (SILValueCategory)TyCategory);
    auto refType = addrType.getAs<WeakStorageType>();
    auto ValType = SILType::getPrimitiveObjectType(refType.getReferentType());
    bool isInit = (Attr > 0);
    ResultVal = Builder.createStoreUnowned(Loc,
                    getLocalValue(ValID, ValType),
                    getLocalValue(ValID2, addrType),
                    IsInitialization_t(isInit));
    break;
  }
  case ValueKind::StoreWeakInst: {
    auto Ty = MF->getType(TyID);
    SILType addrType = getSILType(Ty, (SILValueCategory)TyCategory);
    auto refType = addrType.getAs<WeakStorageType>();
    auto ValType = SILType::getPrimitiveObjectType(refType.getReferentType());
    bool isInit = (Attr > 0);
    ResultVal = Builder.createStoreWeak(Loc,
                    getLocalValue(ValID, ValType),
                    getLocalValue(ValID2, addrType),
                    IsInitialization_t(isInit));
    break;
  }
  case ValueKind::CopyAddrInst: {
    auto Ty = MF->getType(TyID);
    SILType addrType = getSILType(Ty, (SILValueCategory)TyCategory);
    bool isInit = (Attr & 0x2) > 0;
    bool isTake = (Attr & 0x1) > 0;
    ResultVal = Builder.createCopyAddr(Loc,
                    getLocalValue(ValID, addrType),
                    getLocalValue(ValID2, addrType),
                    IsTake_t(isTake),
                    IsInitialization_t(isInit));
    break;
  }
  case ValueKind::AssignInst: {
    auto Ty = MF->getType(TyID);
    SILType addrType = getSILType(Ty, (SILValueCategory)TyCategory);
    SILType ValType = addrType.getObjectType();
    ResultVal = Builder.createAssign(Loc,
                    getLocalValue(ValID, ValType),
                    getLocalValue(ValID2, addrType));
    break;
  }
  case ValueKind::BindMemoryInst: {
    assert(RecordKind == SIL_ONE_TYPE_VALUES &&
           "Layout should be OneTypeValues.");
    auto Ty = MF->getType(TyID);   // BoundTy
    ResultVal = Builder.createBindMemory(
      Loc,
      getLocalValue(ListOfValues[2],
                    getSILType(MF->getType(ListOfValues[0]),
                               (SILValueCategory)ListOfValues[1])),
      getLocalValue(ListOfValues[5],
                    getSILType(MF->getType(ListOfValues[3]),
                               (SILValueCategory)ListOfValues[4])),
      getSILType(Ty, (SILValueCategory)TyCategory));
    break;
  }
  case ValueKind::StructElementAddrInst:
  case ValueKind::StructExtractInst: {
    // Use SILOneValueOneOperandLayout.
    VarDecl *Field = cast<VarDecl>(MF->getDecl(ValID));
    auto Ty = MF->getType(TyID);
    auto Val = getLocalValue(ValID2,
                             getSILType(Ty, (SILValueCategory)TyCategory));
    auto ResultTy = Val->getType().getFieldType(Field, SILMod);
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
    for (unsigned I = 0, E = ListOfValues.size(); I < E; I += 3) {
      auto EltTy = MF->getType(ListOfValues[I]);
      OpList.push_back(
        getLocalValue(ListOfValues[I+2],
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
                      getLocalValue(ValID, ST),
                      TyID, getSILType(ResultTy, SILValueCategory::Address));
      break;
    case ValueKind::TupleExtractInst:
      ResultVal = Builder.createTupleExtract(Loc,
                                             getLocalValue(ValID,ST),
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
    for (unsigned I = 0, E = ListOfValues.size(); I < E; I++) {
      Type EltTy = TT->getElement(I).getType();
      OpList.push_back(
        getLocalValue(ListOfValues[I],
                      getSILType(EltTy, SILValueCategory::Object)));
    }
    ResultVal = Builder.createTuple(Loc,
                    getSILType(Ty, (SILValueCategory)TyCategory),
                    OpList);
    break;
  }
  case ValueKind::BranchInst: {
    SmallVector<SILValue, 4> Args;
    for (unsigned I = 0, E = ListOfValues.size(); I < E; I += 3)
      Args.push_back(
        getLocalValue(ListOfValues[I+2],
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
    SILValue Cond = getLocalValue(ListOfValues[0],
                                  getSILType(MF->getType(TyID),
                                             (SILValueCategory)TyCategory));

    unsigned NumTrueArgs = ListOfValues[3];
    unsigned StartOfTrueArg = 4;
    unsigned StartOfFalseArg = StartOfTrueArg + 3*NumTrueArgs;
    SmallVector<SILValue, 4> TrueArgs;
    for (unsigned I = StartOfTrueArg, E = StartOfFalseArg; I < E; I += 3)
      TrueArgs.push_back(
        getLocalValue(ListOfValues[I+2],
                      getSILType(MF->getType(ListOfValues[I]),
                                 (SILValueCategory)ListOfValues[I+1])));

    SmallVector<SILValue, 4> FalseArgs;
    for (unsigned I = StartOfFalseArg, E = ListOfValues.size(); I < E; I += 3)
      FalseArgs.push_back(
        getLocalValue(ListOfValues[I+2],
                      getSILType(MF->getType(ListOfValues[I]),
                                 (SILValueCategory)ListOfValues[I+1])));

    ResultVal = Builder.createCondBranch(Loc, Cond,
                    getBBForReference(Fn, ListOfValues[1]), TrueArgs,
                    getBBForReference(Fn, ListOfValues[2]), FalseArgs);
    break;
  }
  case ValueKind::SwitchEnumInst:
  case ValueKind::SwitchEnumAddrInst: {
    // Format: condition, a list of cases (EnumElementDecl + Basic Block ID),
    // default basic block ID. Use SILOneTypeValuesLayout: the type is
    // for condition, the list has value for condition, hasDefault, default
    // basic block ID, a list of (DeclID, BasicBlock ID).
    SILValue Cond = getLocalValue(ListOfValues[0],
                                  getSILType(MF->getType(TyID),
                                             (SILValueCategory)TyCategory));

    SILBasicBlock *DefaultBB = nullptr;
    if (ListOfValues[1])
      DefaultBB = getBBForReference(Fn, ListOfValues[2]);

    SmallVector<std::pair<EnumElementDecl*, SILBasicBlock*>, 4> CaseBBs;
    for (unsigned I = 3, E = ListOfValues.size(); I < E; I += 2) {
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
    SILValue Cond = getLocalValue(ListOfValues[0],
                                  getSILType(MF->getType(TyID),
                                             (SILValueCategory)TyCategory));

    Type ResultLoweredTy = MF->getType(ListOfValues[1]);
    SILValueCategory ResultCategory = (SILValueCategory)ListOfValues[2];
    SILType ResultTy = getSILType(ResultLoweredTy, ResultCategory);
    
    SILValue DefaultVal = nullptr;
    if (ListOfValues[3])
      DefaultVal = getLocalValue(ListOfValues[4], ResultTy);

    SmallVector<std::pair<EnumElementDecl*, SILValue>, 4> CaseVals;
    for (unsigned I = 5, E = ListOfValues.size(); I < E; I += 2) {
      auto Value = getLocalValue(ListOfValues[I+1], ResultTy);
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
    SILValue Cond = getLocalValue(ListOfValues[0], getSILType(MF->getType(TyID),
                                             (SILValueCategory)TyCategory));

    SILBasicBlock *DefaultBB = nullptr;
    if (ListOfValues[1])
      DefaultBB = getBBForReference(Fn, ListOfValues[2]);

    SmallVector<std::pair<SILValue, SILBasicBlock*>, 4> CaseBBs;
    for (unsigned I = 3, E = ListOfValues.size(); I < E; I += 2) {
      auto value = getLocalValue(ListOfValues[I], ResultTy);
      CaseBBs.push_back( {value, getBBForReference(Fn, ListOfValues[I+1])} );
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
    SILValue Cond = getLocalValue(ListOfValues[0], getSILType(MF->getType(TyID),
                                             (SILValueCategory)TyCategory));

    Type ResultLoweredTy = MF->getType(ListOfValues[1]);
    SILValueCategory ResultCategory = (SILValueCategory)ListOfValues[2];
    SILType ResultTy = getSILType(ResultLoweredTy, ResultCategory);

    SILValue DefaultVal = nullptr;
    if (ListOfValues[3])
      DefaultVal = getLocalValue(ListOfValues[4], ResultTy);

    SmallVector<std::pair<SILValue, SILValue>, 4> CaseValuesAndResults;
    for (unsigned I = 5, E = ListOfValues.size(); I < E; I += 2) {
      auto CaseValue = getLocalValue(ListOfValues[I], Cond->getType());
      auto Result = getLocalValue(ListOfValues[I+1], ResultTy);
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
    if (Attr)
      Operand = getLocalValue(ValID2,
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
                    getLocalValue(ValID2, OperandTy),
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
                    getLocalValue(ValID2, OperandTy),
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
                    getLocalValue(ValID2, OperandTy),
                    Elt, ResultTy);
    break;
  }
  case ValueKind::InjectEnumAddrInst: {
    // Use SILOneValueOneOperandLayout.
    EnumElementDecl *Elt = cast<EnumElementDecl>(MF->getDecl(ValID));
    auto Ty = MF->getType(TyID);
    ResultVal = Builder.createInjectEnumAddr(Loc,
                    getLocalValue(ValID2,
                                  getSILType(Ty, (SILValueCategory)TyCategory)),
                    Elt);
    break;
  }
  case ValueKind::RefElementAddrInst: {
    // Use SILOneValueOneOperandLayout.
    VarDecl *Field = cast<VarDecl>(MF->getDecl(ValID));
    auto Ty = MF->getType(TyID);
    auto Val = getLocalValue(ValID2,
                             getSILType(Ty, (SILValueCategory)TyCategory));
    auto ResultTy = Val->getType().getFieldType(Field, SILMod);
    ResultVal = Builder.createRefElementAddr(Loc, Val, Field,
                                             ResultTy);
    break;
  }
  case ValueKind::RefTailAddrInst: {
    assert(RecordKind == SIL_ONE_TYPE_ONE_OPERAND &&
           "Layout should be OneTypeOneOperand.");
    assert(Attr == 0);
    assert((SILValueCategory)TyCategory == SILValueCategory::Address);
    ResultVal = Builder.createRefTailAddr(
      Loc,
      getLocalValue(ValID, getSILType(MF->getType(TyID2),
                                      (SILValueCategory)TyCategory2)),
      getSILType(MF->getType(TyID), SILValueCategory::Address));
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
                    getLocalValue(ListOfValues[NextValueIndex], operandTy),
                    DRef, Ty, IsVolatile);
      break;
    case ValueKind::SuperMethodInst:
      ResultVal = Builder.createSuperMethod(Loc,
                    getLocalValue(ListOfValues[NextValueIndex], operandTy),
                    DRef, Ty, IsVolatile);
      break;
    case ValueKind::DynamicMethodInst:
      ResultVal = Builder.createDynamicMethod(Loc,
                    getLocalValue(ListOfValues[NextValueIndex], operandTy),
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

    auto Conformance = MF->readConformance(SILCursor);
    // Read the optional opened existential.
    SILValue ExistentialOperand;
    if (TyID3) {
      SILType ExistentialOperandTy =
          getSILType(MF->getType(TyID3), (SILValueCategory)TyCategory3);
      if (ValID3)
        ExistentialOperand = getLocalValue(ValID3, ExistentialOperandTy);
    }
    ResultVal = Builder.createWitnessMethod(
        Loc, Ty, Conformance, DRef, OperandTy, Attr);
    break;
  }
  case ValueKind::DynamicMethodBranchInst: {
    // Format: a typed value, a SILDeclRef, a BasicBlock ID for method,
    // a BasicBlock ID for no method. Use SILOneTypeValuesLayout.
    unsigned NextValueIndex = 1;
    SILDeclRef DRef = getSILDeclRef(MF, ListOfValues, NextValueIndex);
    assert(ListOfValues.size() == NextValueIndex + 2 &&
           "Wrong number of entries for DynamicMethodBranchInst");
    ResultVal = Builder.createDynamicMethodBranch(Loc,
                    getLocalValue(ListOfValues[0], getSILType(MF->getType(TyID),
                                             (SILValueCategory)TyCategory)),
                    DRef, getBBForReference(Fn, ListOfValues[NextValueIndex]),
                    getBBForReference(Fn, ListOfValues[NextValueIndex+1]));
    break;
  }
  case ValueKind::CheckedCastBranchInst: {
    // Format: the cast kind, a typed value, a BasicBlock ID for success,
    // a BasicBlock ID for failure. Uses SILOneTypeValuesLayout.
    assert(ListOfValues.size() == 6 &&
           "expect 7 numbers for CheckedCastBranchInst");
    bool isExact = ListOfValues[0] != 0;
    SILType opTy = getSILType(MF->getType(ListOfValues[2]),
                              (SILValueCategory)ListOfValues[3]);
    SILValue op = getLocalValue(ListOfValues[1], opTy);
    SILType castTy = getSILType(MF->getType(TyID),
                                (SILValueCategory)TyCategory);
    auto *successBB = getBBForReference(Fn, ListOfValues[4]);
    auto *failureBB = getBBForReference(Fn, ListOfValues[5]);

    ResultVal = Builder.createCheckedCastBranch(Loc, isExact, op, castTy,
                                                successBB, failureBB);
    break;
  }
  case ValueKind::UnconditionalCheckedCastAddrInst:
  case ValueKind::CheckedCastAddrBranchInst: {
    CastConsumptionKind consumption = getCastConsumptionKind(ListOfValues[0]);

    CanType sourceType = MF->getType(ListOfValues[1])->getCanonicalType();
    SILType srcAddrTy = getSILType(MF->getType(ListOfValues[3]),
                                   (SILValueCategory)ListOfValues[4]);
    SILValue src = getLocalValue(ListOfValues[2], srcAddrTy);

    CanType targetType = MF->getType(ListOfValues[5])->getCanonicalType();
    SILType destAddrTy =
      getSILType(MF->getType(TyID), (SILValueCategory) TyCategory);
    SILValue dest = getLocalValue(ListOfValues[6], destAddrTy);

    if (OpCode == (unsigned) ValueKind::UnconditionalCheckedCastAddrInst) {
      ResultVal = Builder.createUnconditionalCheckedCastAddr(Loc, consumption,
                                                             src, sourceType,
                                                             dest, targetType);
      break;
    }

    auto *successBB = getBBForReference(Fn, ListOfValues[7]);
    auto *failureBB = getBBForReference(Fn, ListOfValues[8]);
    ResultVal = Builder.createCheckedCastAddrBranch(Loc, consumption,
                                                    src, sourceType,
                                                    dest, targetType,
                                                    successBB, failureBB);
    break;
  }
  case ValueKind::UncheckedRefCastAddrInst: {
    CanType sourceType = MF->getType(ListOfValues[0])->getCanonicalType();
    SILType srcAddrTy = getSILType(MF->getType(ListOfValues[2]),
                                   (SILValueCategory)ListOfValues[3]);
    SILValue src = getLocalValue(ListOfValues[1], srcAddrTy);

    CanType targetType = MF->getType(ListOfValues[4])->getCanonicalType();
    SILType destAddrTy =
      getSILType(MF->getType(TyID), (SILValueCategory) TyCategory);
    SILValue dest = getLocalValue(ListOfValues[5], destAddrTy);

    ResultVal = Builder.createUncheckedRefCastAddr(Loc, src, sourceType,
                                                   dest, targetType);
    break;
  }
  case ValueKind::InitBlockStorageHeaderInst: {
    assert(ListOfValues.size() == 5 &&
           "expected 5 values for InitBlockStorageHeader");
    SILType blockTy
      = getSILType(MF->getType(TyID), (SILValueCategory)TyCategory);

    SILType storageTy = getSILType(MF->getType(ListOfValues[1]),
                                   SILValueCategory::Address);
    SILValue storage
      = getLocalValue(ListOfValues[0], storageTy);

    SILType invokeTy = getSILType(MF->getType(ListOfValues[3]),
                                  SILValueCategory::Object);
    SILValue invoke
      = getLocalValue(ListOfValues[2], invokeTy);
    
    unsigned NumSub = ListOfValues[4];

    SmallVector<Substitution, 4> Substitutions;
    while (NumSub--) {
      auto sub = MF->maybeReadSubstitution(SILCursor);
      assert(sub.hasValue() && "missing substitution");
      Substitutions.push_back(*sub);
    }
    
    ResultVal = Builder.createInitBlockStorageHeader(Loc, storage, invoke,
                                                     blockTy, Substitutions);
    break;
  }
  case ValueKind::UnreachableInst: {
    ResultVal = Builder.createUnreachable(Loc);
    break;
  }
  case ValueKind::MarkUninitializedBehaviorInst:
    llvm_unreachable("todo");
  }

  if (ResultVal->hasValue()) {
    LastValueID = LastValueID + 1;
    setLocalValue(ResultVal, LastValueID);
  }

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

/// Check for existence of a function with a given name and required linkage.
/// This function is modeled after readSILFunction. But it does not
/// create a SILFunction object.
bool SILDeserializer::hasSILFunction(StringRef Name,
                                     SILLinkage Linkage) {
  if (!FuncTable)
    return false;
  auto iter = FuncTable->find(Name);
  if (iter == FuncTable->end())
    return false;

  // There is a function with the required name.
  // Find out which linkage it has.
  auto FID = *iter;
  auto &cacheEntry = Funcs[FID-1];
  if (cacheEntry.isFullyDeserialized() ||
      (cacheEntry.isDeserialized()))
    return cacheEntry.get()->getLinkage() == Linkage ||
           Linkage == SILLinkage::Private;

  BCOffsetRAII restoreOffset(SILCursor);
  SILCursor.JumpToBit(cacheEntry.getOffset());

  auto entry = SILCursor.advance(AF_DontPopBlockAtEnd);
  if (entry.Kind == llvm::BitstreamEntry::Error) {
    DEBUG(llvm::dbgs() << "Cursor advance error in hasSILFunction.\n");
    MF->error();
    return false;
  }

  SmallVector<uint64_t, 64> scratch;
  StringRef blobData;
  unsigned kind = SILCursor.readRecord(entry.ID, scratch, &blobData);
  assert(kind == SIL_FUNCTION && "expect a sil function");
  (void)kind;

  // Read function properties only, e.g. its linkage and other attributes.
  // TODO: If this results in any noticeable performance problems, Cache the
  // linkage to avoid re-reading it from the bitcode each time?
  DeclID clangOwnerID;
  TypeID funcTyID;
  GenericEnvironmentID genericEnvID;
  unsigned rawLinkage, isTransparent, isFragile, isThunk, isGlobal,
    inlineStrategy, effect, numSpecAttrs, hasQualifiedOwnership;
  ArrayRef<uint64_t> SemanticsIDs;
  SILFunctionLayout::readRecord(scratch, rawLinkage, isTransparent, isFragile,
                                isThunk, isGlobal, inlineStrategy, effect,
                                numSpecAttrs, hasQualifiedOwnership, funcTyID,
                                genericEnvID, clangOwnerID, SemanticsIDs);
  auto linkage = fromStableSILLinkage(rawLinkage);
  if (!linkage) {
    DEBUG(llvm::dbgs() << "invalid linkage code " << rawLinkage
                       << " for SIL function " << Name << "\n");
    return false;
  }

  // Bail if it is not a required linkage.
  if (linkage.getValue() != Linkage && Linkage != SILLinkage::Private)
    return false;

  DEBUG(llvm::dbgs() << "Found SIL Function: " << Name << "\n");
  return true;
}


SILFunction *SILDeserializer::lookupSILFunction(StringRef name,
                                                bool declarationOnly) {
  if (!FuncTable)
    return nullptr;
  auto iter = FuncTable->find(name);
  if (iter == FuncTable->end())
    return nullptr;

  auto Func = readSILFunction(*iter, nullptr, name, declarationOnly);
  if (Func)
    DEBUG(llvm::dbgs() << "Deserialize SIL:\n";
          Func->dump());
  return Func;
}

SILGlobalVariable *SILDeserializer::readGlobalVar(StringRef Name) {
  if (!GlobalVarList)
    return nullptr;

  // If we already deserialized this global variable, just return it.
  if (auto *GV = SILMod.lookUpGlobalVariable(Name))
    return GV;

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
  unsigned rawLinkage, isFragile, IsDeclaration, IsLet;
  SILGlobalVarLayout::readRecord(scratch, rawLinkage, isFragile,
                                 IsDeclaration, IsLet, TyID, dID);
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
  v->setLet(IsLet);
  globalVarOrOffset = v;
  v->setDeclaration(IsDeclaration);

  if (Callback) Callback->didDeserialize(MF->getAssociatedModule(), v);
  return v;
}

void SILDeserializer::getAllSILGlobalVariables() {
  if (!GlobalVarList)
    return;

  for (auto Key : GlobalVarList->keys()) {
    readGlobalVar(Key);
  }
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

  std::vector<SILVTable::Entry> vtableEntries;
  // Another SIL_VTABLE record means the end of this VTable.
  while (kind != SIL_VTABLE && kind != SIL_WITNESS_TABLE &&
         kind != SIL_DEFAULT_WITNESS_TABLE &&
         kind != SIL_FUNCTION) {
    assert(kind == SIL_VTABLE_ENTRY &&
           "Content of Vtable should be in SIL_VTABLE_ENTRY.");
    ArrayRef<uint64_t> ListOfValues;
    DeclID NameID;
    unsigned RawLinkage;
    VTableEntryLayout::readRecord(scratch, NameID, RawLinkage, ListOfValues);

    Optional<SILLinkage> Linkage = fromStableSILLinkage(RawLinkage);
    if (!Linkage) {
      DEBUG(llvm::dbgs() << "invalid linkage code " << RawLinkage
            << " for VTable Entry\n");
      MF->error();
      return nullptr;
    }

    SILFunction *Func = getFuncForReference(MF->getIdentifier(NameID).str());
    if (Func) {
      unsigned NextValueIndex = 0;
      vtableEntries.emplace_back(getSILDeclRef(MF, ListOfValues, NextValueIndex),
                                 Func, Linkage.getValue());
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
                                                   SILWitnessTable *existingWt) {
  if (WId == 0)
    return nullptr;
  assert(WId <= WitnessTables.size() && "invalid WitnessTable ID");

  auto &wTableOrOffset = WitnessTables[WId-1];

  if (wTableOrOffset.isFullyDeserialized())
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
  assert(kind == SIL_WITNESS_TABLE && "expect a sil witnesstable");
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
                          MF->readConformance(SILCursor).getConcrete());

  if (!existingWt)
    existingWt = SILMod.lookUpWitnessTable(theConformance, false);
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
  if (IsDeclaration) {
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
  // Another record means the end of this WitnessTable.
  while (kind != SIL_WITNESS_TABLE &&
         kind != SIL_DEFAULT_WITNESS_TABLE &&
         kind != SIL_FUNCTION) {
    if (kind == SIL_WITNESS_BASE_ENTRY) {
      DeclID protoId;
      WitnessBaseEntryLayout::readRecord(scratch, protoId);
      ProtocolDecl *proto = cast<ProtocolDecl>(MF->getDecl(protoId));
      auto conformance = MF->readConformance(SILCursor);
      witnessEntries.push_back(SILWitnessTable::BaseProtocolWitness{
        proto, conformance.getConcrete()
      });
    } else if (kind == SIL_WITNESS_ASSOC_PROTOCOL) {
      DeclID assocId, protoId;
      WitnessAssocProtocolLayout::readRecord(scratch, assocId, protoId);
      ProtocolDecl *proto = cast<ProtocolDecl>(MF->getDecl(protoId));
      auto conformance = MF->readConformance(SILCursor);
      witnessEntries.push_back(SILWitnessTable::AssociatedTypeProtocolWitness{
        cast<AssociatedTypeDecl>(MF->getDecl(assocId)), proto,
        conformance
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
    readWitnessTable(I + 1, nullptr);
}

SILWitnessTable *
SILDeserializer::lookupWitnessTable(SILWitnessTable *existingWt) {
  assert(existingWt && "Cannot deserialize a null witness table declaration.");
  assert(existingWt->isDeclaration() && "Cannot deserialize a witness table "
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
  auto Wt = readWitnessTable(*iter, existingWt);
  if (Wt)
    DEBUG(llvm::dbgs() << "Deserialize SIL:\n"; Wt->dump());

  return Wt;
}

SILDefaultWitnessTable *SILDeserializer::
readDefaultWitnessTable(DeclID WId, SILDefaultWitnessTable *existingWt) {
  if (WId == 0)
    return nullptr;
  assert(WId <= DefaultWitnessTables.size() &&
         "invalid DefaultWitnessTable ID");

  auto &wTableOrOffset = DefaultWitnessTables[WId-1];

  if (wTableOrOffset.isFullyDeserialized())
    return wTableOrOffset.get();

  BCOffsetRAII restoreOffset(SILCursor);
  SILCursor.JumpToBit(wTableOrOffset.getOffset());
  auto entry = SILCursor.advance(AF_DontPopBlockAtEnd);
  if (entry.Kind == llvm::BitstreamEntry::Error) {
    DEBUG(llvm::dbgs() << "Cursor advance error in "
          "readDefaultWitnessTable.\n");
    return nullptr;
  }

  SmallVector<uint64_t, 64> scratch;
  StringRef blobData;
  unsigned kind = SILCursor.readRecord(entry.ID, scratch, &blobData);
  assert(kind == SIL_DEFAULT_WITNESS_TABLE && "expect a sil default witness table");
  (void)kind;

  unsigned RawLinkage;
  DeclID protoId;
  DefaultWitnessTableLayout::readRecord(scratch, protoId, RawLinkage);

  auto Linkage = fromStableSILLinkage(RawLinkage);
  if (!Linkage) {
    DEBUG(llvm::dbgs() << "invalid linkage code " << RawLinkage
                       << " for SILFunction\n");
    MF->error();
    return nullptr;
  }

  ProtocolDecl *proto = cast<ProtocolDecl>(MF->getDecl(protoId));
  if (proto == nullptr) {
    DEBUG(llvm::dbgs() << "invalid protocol code " << protoId << "\n");
    MF->error();
    return nullptr;
  }

  if (!existingWt)
    existingWt = SILMod.lookUpDefaultWitnessTable(proto, /*deserializeLazily=*/ false);
  auto wT = existingWt;

  // If we have an existing default witness table, verify that the protocol
  // matches up.
  if (wT) {
    if (wT->getProtocol() != proto) {
      DEBUG(llvm::dbgs() << "Protocol mismatch.\n");
      MF->error();
      return nullptr;
    }

    // Don't override the linkage of a default witness table with an existing
    // declaration.

  } else {
    // Otherwise, create a new witness table declaration.
    wT = SILDefaultWitnessTable::create(SILMod, *Linkage, proto);
    if (Callback)
      Callback->didDeserialize(MF->getAssociatedModule(), wT);
  }

  // Fetch the next record.
  scratch.clear();
  entry = SILCursor.advance(AF_DontPopBlockAtEnd);
  if (entry.Kind == llvm::BitstreamEntry::EndBlock)
    return nullptr;
  kind = SILCursor.readRecord(entry.ID, scratch);

  std::vector<SILDefaultWitnessTable::Entry> witnessEntries;
  // Another SIL_DEFAULT_WITNESS_TABLE record means the end of this WitnessTable.
  while (kind != SIL_DEFAULT_WITNESS_TABLE && kind != SIL_FUNCTION) {
    if (kind == SIL_DEFAULT_WITNESS_TABLE_NO_ENTRY) {
      witnessEntries.push_back(SILDefaultWitnessTable::Entry());
    } else {
      assert(kind == SIL_DEFAULT_WITNESS_TABLE_ENTRY &&
             "Content of DefaultWitnessTable should be in "
             "SIL_DEFAULT_WITNESS_TABLE_ENTRY.");
      ArrayRef<uint64_t> ListOfValues;
      DeclID NameID;
      DefaultWitnessTableEntryLayout::readRecord(scratch, NameID, ListOfValues);
      SILFunction *Func = nullptr;
      if (NameID != 0) {
        Func = getFuncForReference(MF->getIdentifier(NameID).str());
      }
      if (Func || NameID == 0) {
        unsigned NextValueIndex = 0;
        witnessEntries.push_back(SILDefaultWitnessTable::Entry(
          getSILDeclRef(MF, ListOfValues, NextValueIndex), Func));
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

  wT->convertToDefinition(witnessEntries);
  wTableOrOffset.set(wT, /*fully deserialized*/ true);
  if (Callback)
    Callback->didDeserializeDefaultWitnessTableEntries(MF->getAssociatedModule(), wT);
  return wT;
}

/// Deserialize all DefaultWitnessTables inside the module and add them to SILMod.
void SILDeserializer::getAllDefaultWitnessTables() {
  if (!DefaultWitnessTableList)
    return;
  for (unsigned I = 0, E = DefaultWitnessTables.size(); I < E; I++)
    readDefaultWitnessTable(I + 1, nullptr);
}

SILDefaultWitnessTable *
SILDeserializer::lookupDefaultWitnessTable(SILDefaultWitnessTable *existingWt) {
  assert(existingWt && "Cannot deserialize a null default witness table declaration.");
  assert(existingWt->isDeclaration() && "Cannot deserialize a default witness table "
                                        "definition.");

  // If we don't have a default witness table list, we can't look anything up.
  if (!DefaultWitnessTableList)
    return nullptr;

  // Use the mangled name of the protocol to lookup the partially
  // deserialized value from the default witness table list.
  auto iter = DefaultWitnessTableList->find(existingWt->getIdentifier().str());
  if (iter == DefaultWitnessTableList->end())
    return nullptr;

  // Attempt to read the default witness table.
  auto Wt = readDefaultWitnessTable(*iter, existingWt);
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

bool SILDeserializer::invalidateFunction(SILFunction *F) {
  for (auto &fnEntry : Funcs) {
    if (fnEntry.isDeserialized() && fnEntry.get() == F) {
      fnEntry.get()->decrementRefCount();
      fnEntry.reset();
      return true;
    }
  }
  return false;
}
