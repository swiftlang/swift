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

#include "DeserializationErrors.h"
#include "SILFormat.h"

#include "SILSerializationFunctionBuilder.h"
#include "swift/AST/GenericSignature.h"
#include "swift/AST/PrettyStackTrace.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/Basic/Defer.h"
#include "swift/Basic/PrettyStackTrace.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILDebugScope.h"
#include "swift/SIL/SILModule.h"
#include "swift/SIL/SILUndef.h"
#include "swift/Serialization/BCReadingExtras.h"
#include "swift/Serialization/ModuleFile.h"

#include "llvm/ADT/Statistic.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/DJB.h"
#include "llvm/Support/OnDiskHashTable.h"

#include <type_traits>

using namespace swift;
using namespace swift::serialization;
using namespace swift::serialization::sil_block;
using namespace llvm::support;

const char SILEntityError::ID = '\0';
void SILEntityError::anchor() {}

STATISTIC(NumDeserializedFunc, "Number of deserialized SIL functions");

static Optional<StringLiteralInst::Encoding>
fromStableStringEncoding(unsigned value) {
  switch (value) {
  case SIL_BYTES: return StringLiteralInst::Encoding::Bytes;
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
  case SIL_LINKAGE_PUBLIC_NON_ABI: return SILLinkage::PublicNonABI;
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

static Optional<SILVTable::Entry::Kind>
fromStableVTableEntryKind(unsigned value) {
  switch (value) {
  case SIL_VTABLE_ENTRY_NORMAL: return SILVTable::Entry::Kind::Normal;
  case SIL_VTABLE_ENTRY_INHERITED: return SILVTable::Entry::Kind::Inherited;
  case SIL_VTABLE_ENTRY_OVERRIDE: return SILVTable::Entry::Kind::Override;
  default: return None;
  }
}

/// Used to deserialize entries in the on-disk func hash table.
class SILDeserializer::FuncTableInfo {
  ModuleFile &MF;

public:
  using internal_key_type = StringRef;
  using external_key_type = StringRef;
  using data_type = DeclID;
  using hash_value_type = uint32_t;
  using offset_type = unsigned;

  explicit FuncTableInfo(ModuleFile &MF) : MF(MF) {}

  internal_key_type GetInternalKey(external_key_type ID) { return ID; }

  external_key_type GetExternalKey(internal_key_type ID) { return ID; }

  hash_value_type ComputeHash(internal_key_type key) {
    // FIXME: DJB seed=0, audit whether the default seed could be used.
    return llvm::djbHash(key, 0);
  }

  static bool EqualKey(internal_key_type lhs, internal_key_type rhs) {
    return lhs == rhs;
  }

  static std::pair<unsigned, unsigned> ReadKeyDataLength(const uint8_t *&data) {
    return { sizeof(uint32_t), sizeof(uint32_t) };
  }

  internal_key_type ReadKey(const uint8_t *data, unsigned length) {
    assert(length == sizeof(uint32_t) && "Expect a single IdentifierID.");
    IdentifierID keyID = endian::readNext<uint32_t, little, unaligned>(data);
    return MF.getIdentifierText(keyID);
  }

  static data_type ReadData(internal_key_type key, const uint8_t *data,
                            unsigned length) {
    assert(length == sizeof(uint32_t) && "Expect a single DeclID.");
    data_type result = endian::readNext<uint32_t, little, unaligned>(data);
    return result;
  }
};

SILDeserializer::SILDeserializer(
    ModuleFile *MF, SILModule &M,
    DeserializationNotificationHandlerSet *callback)
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
  while (kind != sil_index_block::SIL_PROPERTY_OFFSETS) {
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
             kind == sil_index_block::SIL_DEFAULT_WITNESS_TABLE_NAMES ||
             kind == sil_index_block::SIL_PROPERTY_OFFSETS)) &&
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
    else if (kind == sil_index_block::SIL_PROPERTY_OFFSETS) {
      // No matching 'names' block for property descriptors needed yet.
      MF->allocateBuffer(Properties, scratch);
      return;
    }

    // Read SIL_FUNC|VTABLE|GLOBALVAR_OFFSETS record.
    next = cursor.advance();
    scratch.clear();
    unsigned offKind = cursor.readRecord(next.ID, scratch, &blobData);
    (void)offKind;
    if (kind == sil_index_block::SIL_FUNC_NAMES) {
      assert((next.Kind == llvm::BitstreamEntry::Record &&
              offKind == sil_index_block::SIL_FUNC_OFFSETS) &&
             "Expect a SIL_FUNC_OFFSETS record.");
      MF->allocateBuffer(Funcs, scratch);
    } else if (kind == sil_index_block::SIL_VTABLE_NAMES) {
      assert((next.Kind == llvm::BitstreamEntry::Record &&
              offKind == sil_index_block::SIL_VTABLE_OFFSETS) &&
             "Expect a SIL_VTABLE_OFFSETS record.");
      MF->allocateBuffer(VTables, scratch);
    } else if (kind == sil_index_block::SIL_GLOBALVAR_NAMES) {
      assert((next.Kind == llvm::BitstreamEntry::Record &&
              offKind == sil_index_block::SIL_GLOBALVAR_OFFSETS) &&
             "Expect a SIL_GLOBALVAR_OFFSETS record.");
      MF->allocateBuffer(GlobalVars, scratch);
    } else if (kind == sil_index_block::SIL_WITNESS_TABLE_NAMES) {
      assert((next.Kind == llvm::BitstreamEntry::Record &&
              offKind == sil_index_block::SIL_WITNESS_TABLE_OFFSETS) &&
             "Expect a SIL_WITNESS_TABLE_OFFSETS record.");
      MF->allocateBuffer(WitnessTables, scratch);
    } else if (kind == sil_index_block::SIL_DEFAULT_WITNESS_TABLE_NAMES) {
      assert((next.Kind == llvm::BitstreamEntry::Record &&
              offKind == sil_index_block::SIL_DEFAULT_WITNESS_TABLE_OFFSETS) &&
             "Expect a SIL_DEFAULT_WITNESS_TABLE_OFFSETS record.");
      MF->allocateBuffer(DefaultWitnessTables, scratch);
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
                                                base + sizeof(uint32_t), base,
                                                FuncTableInfo(*MF)));
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
  if (BB == nullptr) {
    if (Prev) {
      BB = Fn->createBasicBlockAfter(Prev);      
    } else {
      BB = Fn->createBasicBlock();
    }
    return BB;
  }

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

/// Helper function to find a SILFunction, given its name and type.
SILFunction *SILDeserializer::getFuncForReference(StringRef name,
                                                  SILType type) {
  // Check to see if we have a function by this name already.
  SILFunction *fn = SILMod.lookUpFunction(name);
  if (!fn) {
    // Otherwise, look for a function with this name in the module.
    auto iter = FuncTable->find(name);
    if (iter != FuncTable->end()) {
      auto maybeFn = readSILFunctionChecked(*iter, nullptr, name,
                                            /*declarationOnly*/ true);
      if (maybeFn) {
        fn = maybeFn.get();
      } else {
        // Ignore the failure; we'll synthesize a bogus function instead.
        llvm::consumeError(maybeFn.takeError());
      }
    }
  }

  // FIXME: check for matching types.

  // At this point, if fn is set, we know that we have a good function to use.
  if (fn)
    return fn;

  // Otherwise, create a function declaration with the right type and a bogus
  // source location. This ensures that we can at least parse the rest of the
  // SIL.
  SourceLoc sourceLoc;
  SILSerializationFunctionBuilder builder(SILMod);
  return builder.createDeclaration(name, type, RegularLocation(sourceLoc));
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

  auto maybeFn = readSILFunctionChecked(*iter, nullptr, name,
                                        /*declarationOnly*/ true);
  if (!maybeFn) {
    // Ignore the failure and just pretend the function doesn't exist
    llvm::consumeError(maybeFn.takeError());
    return nullptr;
  }

  return maybeFn.get();
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
  llvm::Expected<SILFunction *> deserialized =
      readSILFunctionChecked(FID, existingFn, name, declarationOnly,
                             errorIfEmptyBody);
  if (!deserialized) {
    MF->fatal(deserialized.takeError());
  }
  return deserialized.get();
}

llvm::Expected<SILFunction *>
SILDeserializer::readSILFunctionChecked(DeclID FID, SILFunction *existingFn,
                                        StringRef name, bool declarationOnly,
                                        bool errorIfEmptyBody) {
  // We can't deserialize function bodies after IRGen lowering passes have
  // happened since other definitions in the module will no longer be in
  // canonical SIL form.
  switch (SILMod.getStage()) {
  case SILStage::Raw:
  case SILStage::Canonical:
    break;
    
  case SILStage::Lowered:
    llvm_unreachable("cannot deserialize into a module that has entered "
                     "Lowered stage");
  }
  
  if (FID == 0)
    return nullptr;
  assert(FID <= Funcs.size() && "invalid SILFunction ID");

  PrettyStackTraceStringAction trace("deserializing SIL function", name);

  auto &cacheEntry = Funcs[FID-1];
  if (cacheEntry.isFullyDeserialized() ||
      (cacheEntry.isDeserialized() && declarationOnly))
    return cacheEntry.get();

  BCOffsetRAII restoreOffset(SILCursor);
  SILCursor.JumpToBit(cacheEntry.getOffset());

  auto entry = SILCursor.advance(AF_DontPopBlockAtEnd);
  if (entry.Kind == llvm::BitstreamEntry::Error) {
    LLVM_DEBUG(llvm::dbgs() << "Cursor advance error in readSILFunction.\n");
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
  unsigned rawLinkage, isTransparent, isSerialized, isThunk,
      isWithoutactuallyEscapingThunk, isGlobal, inlineStrategy,
      // SWIFT_ENABLE_TENSORFLOW
      optimizationMode, effect, numSpecAttrs, numReverseDifferentiableAttrs,
      hasQualifiedOwnership, isWeakLinked;
  ArrayRef<uint64_t> SemanticsIDs;
  SILFunctionLayout::readRecord(
      scratch, rawLinkage, isTransparent, isSerialized, isThunk,
      isWithoutactuallyEscapingThunk, isGlobal, inlineStrategy,
      // SWIFT_ENABLE_TENSORFLOW
      optimizationMode, effect, numSpecAttrs, numReverseDifferentiableAttrs,
      hasQualifiedOwnership, isWeakLinked, funcTyID, genericEnvID,
      clangNodeOwnerID, SemanticsIDs);

  if (funcTyID == 0) {
    LLVM_DEBUG(llvm::dbgs() << "SILFunction typeID is 0.\n");
    MF->error();
    return nullptr;
  }
  auto astType = MF->getTypeChecked(funcTyID);
  if (!astType) {
    if (!existingFn || errorIfEmptyBody) {
      return llvm::make_error<SILEntityError>(
          name, takeErrorInfo(astType.takeError()));
    }
    llvm::consumeError(astType.takeError());
    return existingFn;
  }
  auto ty = getSILType(astType.get(), SILValueCategory::Object);
  if (!ty.is<SILFunctionType>()) {
    LLVM_DEBUG(llvm::dbgs() << "not a function type for SILFunction\n");
    MF->error();
    return nullptr;
  }

  auto linkage = fromStableSILLinkage(rawLinkage);
  if (!linkage) {
    LLVM_DEBUG(llvm::dbgs() << "invalid linkage code " << rawLinkage
                            << " for SILFunction\n");
    MF->error();
    return nullptr;
  }

  ValueDecl *clangNodeOwner = nullptr;
  if (clangNodeOwnerID != 0) {
    clangNodeOwner = dyn_cast_or_null<ValueDecl>(MF->getDecl(clangNodeOwnerID));
    if (!clangNodeOwner) {
      LLVM_DEBUG(llvm::dbgs() << "invalid clang node owner for SILFunction\n");
      MF->error();
      return nullptr;
    }
  }

  // If we weren't handed a function, check for an existing
  // declaration in the output module.
  if (!existingFn) existingFn = SILMod.lookUpFunction(name);
  auto fn = existingFn;

  // TODO: use the correct SILLocation from module.
  SILLocation loc = RegularLocation::getAutoGeneratedLocation();

  // If we've already serialized the module, don't mark the function
  // as serialized, since we no longer need to enforce resilience
  // boundaries.
  if (SILMod.isSerialized())
    isSerialized = IsNotSerialized;

  // If we have an existing function, verify that the types match up.
  if (fn) {
    if (fn->getLoweredType() != ty) {
      LLVM_DEBUG(llvm::dbgs() << "SILFunction type mismatch.\n");
      MF->error();
      return nullptr;
    }

    fn->setSerialized(IsSerialized_t(isSerialized));

    if (SILMod.getOptions().MergePartialModules)
      fn->setLinkage(*linkage);

    // Don't override the transparency or linkage of a function with
    // an existing declaration, except if we deserialized a
    // PublicNonABI function, which has HiddenExternal when
    // referenced as a declaration, and SharedExternal when it has
    // a deserialized body.
    if (fn->getLinkage() == SILLinkage::HiddenExternal &&
        linkage == SILLinkage::PublicNonABI) {
      fn->setLinkage(SILLinkage::SharedExternal);
    }
  } else {
    // Otherwise, create a new function.
    SILSerializationFunctionBuilder builder(SILMod);
    fn = builder.createDeclaration(name, ty, loc);
    fn->setLinkage(linkage.getValue());
    fn->setTransparent(IsTransparent_t(isTransparent == 1));
    fn->setSerialized(IsSerialized_t(isSerialized));
    fn->setThunk(IsThunk_t(isThunk));
    fn->setWithoutActuallyEscapingThunk(bool(isWithoutactuallyEscapingThunk));
    fn->setInlineStrategy(Inline_t(inlineStrategy));
    fn->setGlobalInit(isGlobal == 1);
    fn->setEffectsKind(EffectsKind(effect));
    fn->setOptimizationMode(OptimizationMode(optimizationMode));
    fn->setWeakLinked(isWeakLinked);
    if (clangNodeOwner)
      fn->setClangNodeOwner(clangNodeOwner);
    for (auto ID : SemanticsIDs) {
      fn->addSemanticsAttr(MF->getIdentifierText(ID));
    }

    if (Callback) Callback->didDeserialize(MF->getAssociatedModule(), fn);
  }
  // Mark this function as deserialized. This avoids rerunning diagnostic
  // passes. Certain passes in the madatory pipeline may not work as expected
  // after arbitrary optimization and lowering.
  if (!MF->IsSIB)
    fn->setWasDeserializedCanonical();

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

  // SWIFT_ENABLE_TENSORFLOW
  // Read and instantiate the differentiable attributes.
  while (numReverseDifferentiableAttrs--) {
    auto next = SILCursor.advance(AF_DontPopBlockAtEnd);
    assert(next.Kind == llvm::BitstreamEntry::Record);

    scratch.clear();
    kind = SILCursor.readRecord(next.ID, scratch);
    assert(kind == SIL_REVERSE_DIFFERENTIABLE_ATTR &&
           "Missing reverse differentiable attribute");

    uint64_t primalNameId;
    uint64_t adjointNameId;
    bool adjointIsPrimitive;
    uint64_t source;
    ArrayRef<uint64_t> parameters;
    SILReverseDifferentiableAttrLayout::readRecord(scratch, primalNameId,
                                                   adjointNameId,
                                                   adjointIsPrimitive,
                                                   source, parameters);

    StringRef primalName = MF->getIdentifier(primalNameId).str();
    StringRef adjointName = MF->getIdentifier(adjointNameId).str();
    llvm::SmallBitVector parametersBitVector(parameters.size());
    for (unsigned i = 0; i < parameters.size(); i++)
      parametersBitVector[i] = parameters[i];
    SILReverseAutoDiffIndices indices(source, parametersBitVector);

    auto *attr = SILReverseDifferentiableAttr::create(SILMod, indices,
                                                      primalName, adjointName,
                                                      adjointIsPrimitive);
    fn->addReverseDifferentiableAttr(attr);
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

  SILOpenedArchetypesTracker OpenedArchetypesTracker(fn);
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
    // We can't call getCanonicalType() immediately on everything we
    // deserialize, but fortunately we only need to register opened
    // existentials.
    if (ty->isOpenedExistential())
      OpenedArchetypesTracker.registerUsedOpenedArchetypes(CanType(ty));
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
        LLVM_DEBUG(llvm::dbgs() << "readSILInstruction returns error.\n");
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
      Arg = CurrentBB->createPhiArgument(SILArgTy, OwnershipKind);
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
  case SIL_CAST_CONSUMPTION_BORROW_ALWAYS:
    return CastConsumptionKind::BorrowAlways;
  default:
    llvm_unreachable("not a valid CastConsumptionKind for SIL");
  }
}

/// Construct a SILDeclRef from ListOfValues.
static SILDeclRef getSILDeclRef(ModuleFile *MF,
                                ArrayRef<uint64_t> ListOfValues,
                                unsigned &NextIdx) {
  assert(ListOfValues.size() >= NextIdx+4 &&
         "Expect 4 numbers for SILDeclRef");
  SILDeclRef DRef(cast<ValueDecl>(MF->getDecl(ListOfValues[NextIdx])),
                  (SILDeclRef::Kind)ListOfValues[NextIdx+1],
                  /*isCurried=*/ListOfValues[NextIdx+2] > 0,
                  /*isForeign=*/ListOfValues[NextIdx+3] > 0);
  NextIdx += 4;
  return DRef;
}

Optional<KeyPathPatternComponent>
SILDeserializer::readKeyPathComponent(ArrayRef<uint64_t> ListOfValues,
                                      unsigned &nextValue) {
  auto kind =
    (KeyPathComponentKindEncoding)ListOfValues[nextValue++];
  
  if (kind == KeyPathComponentKindEncoding::Trivial)
    return None;
  
  auto type = MF->getType(ListOfValues[nextValue++])
    ->getCanonicalType();

  auto handleComputedId =
  [&]() -> KeyPathPatternComponent::ComputedPropertyId {
    auto kind =
      (KeyPathComputedComponentIdKindEncoding)ListOfValues[nextValue++];
    switch (kind) {
    case KeyPathComputedComponentIdKindEncoding::Property:
      return cast<VarDecl>(MF->getDecl(ListOfValues[nextValue++]));
    case KeyPathComputedComponentIdKindEncoding::Function: {
      auto name = MF->getIdentifierText(ListOfValues[nextValue++]);
      return getFuncForReference(name);
    }
    case KeyPathComputedComponentIdKindEncoding::DeclRef: {
      // read SILDeclRef
      return getSILDeclRef(MF, ListOfValues, nextValue);
    }
    }
    llvm_unreachable("unhandled kind");
  };

  ArrayRef<KeyPathPatternComponent::Index> indices;
  SILFunction *indicesEquals = nullptr;
  SILFunction *indicesHash = nullptr;
  AbstractStorageDecl *externalDecl = nullptr;
  SubstitutionMap externalSubs;

  auto handleComputedExternalReferenceAndIndices = [&] {
    auto externalDeclID = ListOfValues[nextValue++];
    externalDecl =
      cast_or_null<AbstractStorageDecl>(MF->getDecl(externalDeclID));
    externalSubs = MF->getSubstitutionMap(ListOfValues[nextValue++]);
    
    SmallVector<KeyPathPatternComponent::Index, 4> indicesBuf;
    auto numIndexes = ListOfValues[nextValue++];
    indicesBuf.reserve(numIndexes);
    while (numIndexes-- > 0) {
      unsigned operand = ListOfValues[nextValue++];
      auto formalType = MF->getType(ListOfValues[nextValue++]);
      auto loweredType = MF->getType(ListOfValues[nextValue++]);
      auto loweredCategory = (SILValueCategory)ListOfValues[nextValue++];
      auto conformance = MF->readConformance(SILCursor);
      indicesBuf.push_back({
        operand, formalType->getCanonicalType(),
        SILType::getPrimitiveType(loweredType->getCanonicalType(),
                                  loweredCategory),
        conformance});
    }
    
    indices = MF->getContext().AllocateCopy(indicesBuf);
    if (!indices.empty()) {
      auto indicesEqualsName = MF->getIdentifierText(ListOfValues[nextValue++]);
      auto indicesHashName = MF->getIdentifierText(ListOfValues[nextValue++]);
      indicesEquals = getFuncForReference(indicesEqualsName);
      indicesHash = getFuncForReference(indicesHashName);
    }
  };

  switch (kind) {
  case KeyPathComponentKindEncoding::StoredProperty: {
    auto decl = cast<VarDecl>(MF->getDecl(ListOfValues[nextValue++]));
    return KeyPathPatternComponent::forStoredProperty(decl, type);
  }
  case KeyPathComponentKindEncoding::GettableProperty: {
    auto id = handleComputedId();
    auto getterName = MF->getIdentifierText(ListOfValues[nextValue++]);
    auto getter = getFuncForReference(getterName);
    handleComputedExternalReferenceAndIndices();
    return KeyPathPatternComponent::forComputedGettableProperty(
        id, getter, indices, indicesEquals, indicesHash,
        externalDecl, externalSubs, type);
  }
  case KeyPathComponentKindEncoding::SettableProperty: {
    auto id = handleComputedId();
    auto getterName = MF->getIdentifierText(ListOfValues[nextValue++]);
    auto getter = getFuncForReference(getterName);
    auto setterName = MF->getIdentifierText(ListOfValues[nextValue++]);
    auto setter = getFuncForReference(setterName);
    handleComputedExternalReferenceAndIndices();
    return KeyPathPatternComponent::forComputedSettableProperty(
        id, getter, setter, indices, indicesEquals, indicesHash,
        externalDecl, externalSubs, type);
    break;
  }
  case KeyPathComponentKindEncoding::OptionalChain:
    return KeyPathPatternComponent::forOptional(
        KeyPathPatternComponent::Kind::OptionalChain, type);
  case KeyPathComponentKindEncoding::OptionalForce:
    return KeyPathPatternComponent::forOptional(
        KeyPathPatternComponent::Kind::OptionalForce, type);
  case KeyPathComponentKindEncoding::OptionalWrap:
    return KeyPathPatternComponent::forOptional(
        KeyPathPatternComponent::Kind::OptionalWrap, type);
  case KeyPathComponentKindEncoding::Trivial:
    llvm_unreachable("handled above");
  }
  
  llvm_unreachable("invalid key path component kind encoding");
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
  unsigned RawOpCode = 0, TyCategory = 0, TyCategory2 = 0, TyCategory3 = 0,
           Attr = 0, NumSubs = 0, NumConformances = 0, IsNonThrowingApply = 0;
  // SWIFT_ENABLE_TENSORFLOW
  unsigned NumArguments = 0;
  unsigned GradResultIndex = 0;
  ValueID ValID, ValID2, ValID3;
  TypeID TyID, TyID2, TyID3;
  TypeID ConcreteTyID;
  SourceLoc SLoc;
  ArrayRef<uint64_t> ListOfValues;
  SILLocation Loc = RegularLocation(SLoc);

  switch (RecordKind) {
  default:
    llvm_unreachable("Record kind for a SIL instruction is not supported.");
  case SIL_ONE_VALUE_ONE_OPERAND:
    SILOneValueOneOperandLayout::readRecord(scratch, RawOpCode, Attr,
                                            ValID, TyID, TyCategory,
                                            ValID2);
    break;
  case SIL_ONE_TYPE:
    SILOneTypeLayout::readRecord(scratch, RawOpCode, TyID, TyCategory);
    break;
  case SIL_ONE_OPERAND:
    SILOneOperandLayout::readRecord(scratch, RawOpCode, Attr,
                                    TyID, TyCategory, ValID);
    break;
  case SIL_ONE_OPERAND_EXTRA_ATTR:
    SILOneOperandExtraAttributeLayout::readRecord(scratch, RawOpCode, Attr,
                                                  TyID, TyCategory, ValID);
    break;
  case SIL_ONE_TYPE_ONE_OPERAND:
    SILOneTypeOneOperandLayout::readRecord(scratch, RawOpCode, Attr,
                                           TyID, TyCategory,
                                           TyID2, TyCategory2,
                                           ValID);
    break;
  case SIL_INIT_EXISTENTIAL:
    SILInitExistentialLayout::readRecord(scratch, RawOpCode,
                                         TyID, TyCategory,
                                         TyID2, TyCategory2,
                                         ValID,
                                         ConcreteTyID,
                                         NumConformances);
    break;
  case SIL_INST_CAST:
    SILInstCastLayout::readRecord(scratch, RawOpCode, Attr,
                                  TyID, TyCategory,
                                  TyID2, TyCategory2,
                                  ValID);
    break;
  case SIL_ONE_TYPE_VALUES:
    SILOneTypeValuesLayout::readRecord(scratch, RawOpCode, TyID, TyCategory,
                                       ListOfValues);
    break;
  case SIL_TWO_OPERANDS:
    SILTwoOperandsLayout::readRecord(scratch, RawOpCode, Attr,
                                     TyID, TyCategory, ValID,
                                     TyID2, TyCategory2, ValID2);
    break;
  case SIL_TWO_OPERANDS_EXTRA_ATTR:
    SILTwoOperandsExtraAttributeLayout::readRecord(scratch, RawOpCode, Attr,
                                                   TyID, TyCategory, ValID,
                                                   TyID2, TyCategory2, ValID2);
    break;
  case SIL_TAIL_ADDR:
    SILTailAddrLayout::readRecord(scratch, RawOpCode,
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
      RawOpCode = (unsigned)SILInstructionKind::ApplyInst;
      break;
    case SIL_PARTIAL_APPLY:
      RawOpCode = (unsigned)SILInstructionKind::PartialApplyInst;
      break;
    case SIL_BUILTIN:
      RawOpCode = (unsigned)SILInstructionKind::BuiltinInst;
      break;
    case SIL_TRY_APPLY:
      RawOpCode = (unsigned)SILInstructionKind::TryApplyInst;
      break;
    case SIL_NON_THROWING_APPLY:
      RawOpCode = (unsigned)SILInstructionKind::ApplyInst;
      IsNonThrowingApply = true;
      break;
    case SIL_BEGIN_APPLY:
      RawOpCode = (unsigned)SILInstructionKind::BeginApplyInst;
      break;
    case SIL_NON_THROWING_BEGIN_APPLY:
      RawOpCode = (unsigned)SILInstructionKind::BeginApplyInst;
      IsNonThrowingApply = true;
      break;
        
    default:
      llvm_unreachable("unexpected apply inst kind");
    }
    break;
  }
  // SWIFT_ENABLE_TENSORFLOW
  case SIL_INST_GRAPH_OPERATION:
    SILInstGraphOperationLayout::readRecord(scratch, ValID, NumArguments,
                                            ListOfValues);
    RawOpCode = (unsigned)SILInstructionKind::GraphOperationInst;
    break;
  case SIL_INST_GRADIENT:
    SILInstGradientLayout::readRecord(scratch, Attr, TyID, TyCategory, ValID,
                                      GradResultIndex, ListOfValues);
    RawOpCode = (unsigned)SILInstructionKind::GradientInst;
    break;
  case SIL_INST_NO_OPERAND:
    SILInstNoOperandLayout::readRecord(scratch, RawOpCode);
    break;
  case SIL_INST_WITNESS_METHOD:
    SILInstWitnessMethodLayout::readRecord(
        scratch, TyID, TyCategory, Attr, TyID2, TyCategory2, TyID3,
        TyCategory3, ValID3, ListOfValues);
    RawOpCode = (unsigned)SILInstructionKind::WitnessMethodInst;
    break;
  }

  // FIXME: validate
  SILInstructionKind OpCode = (SILInstructionKind) RawOpCode;

  SILInstruction *ResultVal;
  switch (OpCode) {
  case SILInstructionKind::DebugValueInst:
  case SILInstructionKind::DebugValueAddrInst:
    llvm_unreachable("not supported");

  case SILInstructionKind::AllocBoxInst:
    assert(RecordKind == SIL_ONE_TYPE && "Layout should be OneType.");
    ResultVal = Builder.createAllocBox(Loc,
                       cast<SILBoxType>(MF->getType(TyID)->getCanonicalType()));
    break;
  
#define ONETYPE_INST(ID)                      \
  case SILInstructionKind::ID##Inst:                   \
    assert(RecordKind == SIL_ONE_TYPE && "Layout should be OneType.");         \
    ResultVal = Builder.create##ID(Loc,                                        \
                  getSILType(MF->getType(TyID), (SILValueCategory)TyCategory));\
    break;
  ONETYPE_INST(AllocStack)
  ONETYPE_INST(Metatype)
#undef ONETYPE_INST
#define ONETYPE_ONEOPERAND_INST(ID)           \
  case SILInstructionKind::ID##Inst:                   \
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
  case SILInstructionKind::DeallocBoxInst:
    assert(RecordKind == SIL_ONE_TYPE_ONE_OPERAND &&
           "Layout should be OneTypeOneOperand.");
    ResultVal = Builder.createDeallocBox(Loc,
                  getLocalValue(ValID,
                    getSILType(MF->getType(TyID2),
                               (SILValueCategory)TyCategory2)));
    break;
  case SILInstructionKind::OpenExistentialAddrInst:
    assert(RecordKind == SIL_ONE_TYPE_ONE_OPERAND &&
           "Layout should be OneTypeOneOperand.");
    ResultVal = Builder.createOpenExistentialAddr(
        Loc, getLocalValue(ValID, getSILType(MF->getType(TyID2),
                                             (SILValueCategory)TyCategory2)),
        getSILType(MF->getType(TyID), (SILValueCategory)TyCategory),
        Attr == 0 ? OpenedExistentialAccess::Immutable
                  : OpenedExistentialAccess::Mutable);
    break;

#define ONEOPERAND_ONETYPE_INST(ID)           \
  case SILInstructionKind::ID##Inst:                   \
    assert(RecordKind == SIL_ONE_TYPE_ONE_OPERAND &&       \
           "Layout should be OneTypeOneOperand.");         \
    ResultVal = Builder.create##ID(Loc,                    \
                  getLocalValue(ValID,                              \
                    getSILType(MF->getType(TyID2),                             \
                               (SILValueCategory)TyCategory2)),                \
                  getSILType(MF->getType(TyID), (SILValueCategory)TyCategory));\
    break;
  ONEOPERAND_ONETYPE_INST(OpenExistentialRef)
  ONEOPERAND_ONETYPE_INST(OpenExistentialMetatype)
  ONEOPERAND_ONETYPE_INST(OpenExistentialBox)
  ONEOPERAND_ONETYPE_INST(OpenExistentialValue)
  ONEOPERAND_ONETYPE_INST(OpenExistentialBoxValue)
  // Conversion instructions.
#define LOADABLE_REF_STORAGE(Name, ...) \
  ONEOPERAND_ONETYPE_INST(RefTo##Name) \
  ONEOPERAND_ONETYPE_INST(Name##ToRef)
#include "swift/AST/ReferenceStorage.def"
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
  ONEOPERAND_ONETYPE_INST(ThinToThickFunction)
  ONEOPERAND_ONETYPE_INST(ThickToObjCMetatype)
  ONEOPERAND_ONETYPE_INST(ObjCToThickMetatype)
  ONEOPERAND_ONETYPE_INST(ObjCMetatypeToObject)
  ONEOPERAND_ONETYPE_INST(ObjCExistentialMetatypeToObject)
  ONEOPERAND_ONETYPE_INST(ThinFunctionToPointer)
  ONEOPERAND_ONETYPE_INST(PointerToThinFunction)
  ONEOPERAND_ONETYPE_INST(ProjectBlockStorage)
#undef ONEOPERAND_ONETYPE_INST

  case SILInstructionKind::ProjectBoxInst: {
    assert(RecordKind == SIL_ONE_TYPE_ONE_OPERAND &&
           "Layout should be OneTypeOneOperand.");
    ResultVal = Builder.createProjectBox(Loc,
                  getLocalValue(ValID,
                    getSILType(MF->getType(TyID2),
                               (SILValueCategory)TyCategory2)),
                  TyID);
    break;
  }
  case  SILInstructionKind::ConvertEscapeToNoEscapeInst: {
    assert(RecordKind == SIL_ONE_TYPE_ONE_OPERAND &&
           "Layout should be OneTypeOneOperand.");
    bool isLifetimeGuaranteed = Attr & 0x01;
    bool isEscaped = Attr & 0x02;
    ResultVal = Builder.createConvertEscapeToNoEscape(
        Loc,
        getLocalValue(ValID, getSILType(MF->getType(TyID2),
                                        (SILValueCategory)TyCategory2)),
        getSILType(MF->getType(TyID), (SILValueCategory)TyCategory), isEscaped,
        isLifetimeGuaranteed);
    break;
  }
  case SILInstructionKind::ConvertFunctionInst: {
    assert(RecordKind == SIL_ONE_TYPE_ONE_OPERAND
           && "Layout should be OneTypeOneOperand.");
    bool withoutActuallyEscaping = Attr & 0x01;
    ResultVal = Builder.createConvertFunction(
        Loc,
        getLocalValue(ValID, getSILType(MF->getType(TyID2),
                                        (SILValueCategory)TyCategory2)),
        getSILType(MF->getType(TyID), (SILValueCategory)TyCategory),
        withoutActuallyEscaping);
    break;
  }
  case SILInstructionKind::PointerToAddressInst: {
    assert(RecordKind == SIL_ONE_TYPE_ONE_OPERAND &&
           "Layout should be OneTypeOneOperand.");
    bool isStrict = Attr & 0x01;
    bool isInvariant = Attr & 0x02;
    ResultVal = Builder.createPointerToAddress(
      Loc,
      getLocalValue(ValID, getSILType(MF->getType(TyID2),
                                      (SILValueCategory)TyCategory2)),
      getSILType(MF->getType(TyID), (SILValueCategory)TyCategory),
      isStrict, isInvariant);
    break;
  }
  case SILInstructionKind::DeallocExistentialBoxInst: {
    assert(RecordKind == SIL_ONE_TYPE_ONE_OPERAND &&
           "Layout should be OneTypeOneOperand.");
    ResultVal = Builder.createDeallocExistentialBox(Loc,
                  MF->getType(TyID)->getCanonicalType(),
                  getLocalValue(ValID,
                    getSILType(MF->getType(TyID2),
                               (SILValueCategory)TyCategory2)));
    break;

  }
  
  case SILInstructionKind::RefToBridgeObjectInst: {
    auto RefTy = getSILType(MF->getType(TyID), (SILValueCategory)TyCategory);
    auto Ref = getLocalValue(ValID, RefTy);
    auto BitsTy = getSILType(MF->getType(TyID2), (SILValueCategory)TyCategory2);
    auto Bits = getLocalValue(ValID2, BitsTy);
    
    ResultVal = Builder.createRefToBridgeObject(Loc, Ref, Bits);
    break;
  }

  case SILInstructionKind::ObjCProtocolInst: {
    auto Ty = getSILType(MF->getType(TyID), (SILValueCategory)TyCategory);
    auto Proto = MF->getDecl(ValID);
    ResultVal = Builder.createObjCProtocol(Loc, cast<ProtocolDecl>(Proto), Ty);
    break;
  }

  case SILInstructionKind::InitExistentialAddrInst:
  case SILInstructionKind::InitExistentialValueInst:
  case SILInstructionKind::InitExistentialMetatypeInst:
  case SILInstructionKind::InitExistentialRefInst:
  case SILInstructionKind::AllocExistentialBoxInst: {

    auto Ty = getSILType(MF->getType(TyID), (SILValueCategory)TyCategory);
    auto Ty2 = MF->getType(TyID2);
    CanType ConcreteTy;
    if (OpCode != SILInstructionKind::InitExistentialMetatypeInst)
      ConcreteTy = MF->getType(ConcreteTyID)->getCanonicalType();
    SILValue operand;
    if (OpCode != SILInstructionKind::AllocExistentialBoxInst)
      operand = getLocalValue(ValID,
                         getSILType(Ty2, (SILValueCategory)TyCategory2));

    SmallVector<ProtocolConformanceRef, 2> conformances;
    while (NumConformances--) {
      auto conformance = MF->readConformance(SILCursor);
      conformances.push_back(conformance);
    }

    auto ctxConformances = MF->getContext().AllocateCopy(conformances);

    switch (OpCode) {
    default: llvm_unreachable("Out of sync with parent switch");
    case SILInstructionKind::InitExistentialAddrInst:
      ResultVal = Builder.createInitExistentialAddr(Loc, operand,
                                                ConcreteTy,
                                                Ty,
                                                ctxConformances);
      break;
    case SILInstructionKind::InitExistentialValueInst:
      ResultVal = Builder.createInitExistentialValue(Loc, Ty, ConcreteTy,
                                                      operand, ctxConformances);
      break;
    case SILInstructionKind::InitExistentialMetatypeInst:
      ResultVal = Builder.createInitExistentialMetatype(Loc, operand, Ty,
                                                        ctxConformances);
      break;
    case SILInstructionKind::InitExistentialRefInst:
      ResultVal = Builder.createInitExistentialRef(Loc, Ty,
                                         ConcreteTy,
                                         operand,
                                         ctxConformances);
      break;
    case SILInstructionKind::AllocExistentialBoxInst:
      ResultVal = Builder.createAllocExistentialBox(Loc, Ty, ConcreteTy,
                                  ctxConformances);
      break;
    }
    break;
  }
  case SILInstructionKind::AllocRefInst:
  case SILInstructionKind::AllocRefDynamicInst: {
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
    if (OpCode == SILInstructionKind::AllocRefDynamicInst) {
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
  case SILInstructionKind::ApplyInst:
  case SILInstructionKind::BeginApplyInst: {
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
    SubstitutionMap Substitutions = MF->getSubstitutionMap(NumSubs);

    if (OpCode == SILInstructionKind::ApplyInst) {
      ResultVal = Builder.createApply(Loc, getLocalValue(ValID, FnTy),
                                      Substitutions, Args,
                                      IsNonThrowingApply != 0);
    } else {
      ResultVal = Builder.createBeginApply(Loc, getLocalValue(ValID, FnTy),
                                           Substitutions, Args,
                                           IsNonThrowingApply != 0);
    }
    break;
  }
  case SILInstructionKind::TryApplyInst: {
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
    SubstitutionMap Substitutions = MF->getSubstitutionMap(NumSubs);

    ResultVal = Builder.createTryApply(Loc, getLocalValue(ValID, FnTy),
                                       Substitutions, Args, normalBB,
                                       errorBB);
    break;
  }
  case SILInstructionKind::PartialApplyInst: {
    auto Ty = MF->getType(TyID);
    auto Ty2 = MF->getType(TyID2);
    SILType FnTy = getSILType(Ty, SILValueCategory::Object);
    SILType closureTy = getSILType(Ty2, SILValueCategory::Object);

    SubstitutionMap Substitutions = MF->getSubstitutionMap(NumSubs);

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
    ResultVal = Builder.createPartialApply(
        Loc, FnVal, Substitutions, Args,
        closureTy.castTo<SILFunctionType>()->getCalleeConvention());
    break;
  }
  case SILInstructionKind::BuiltinInst: {
    auto ASTTy = MF->getType(TyID);
    auto ResultTy = getSILType(ASTTy, (SILValueCategory)(unsigned)TyID2);
    SmallVector<SILValue, 4> Args;
    for (unsigned i = 0, e = ListOfValues.size(); i < e; i += 3) {
      auto ArgASTTy = MF->getType(ListOfValues[i+1]);
      auto ArgTy = getSILType(ArgASTTy,
                              (SILValueCategory)(unsigned)ListOfValues[i+2]);
      Args.push_back(getLocalValue(ListOfValues[i], ArgTy));
    }
    SubstitutionMap Substitutions = MF->getSubstitutionMap(NumSubs);
    Identifier Name = MF->getIdentifier(ValID);
    
    ResultVal = Builder.createBuiltin(Loc, Name, ResultTy, Substitutions,
                                      Args);
    break;
  }
  // SWIFT_ENABLE_TENSORFLOW
  case SILInstructionKind::GradientInst: {
    auto ASTTy = MF->getType(TyID);
    auto SILTy = getSILType(ASTTy, SILValueCategory::Object);
    auto Val = getLocalValue(ValID, SILTy);
    auto GradOpts = (SILGradientOptions)Attr;
    llvm::SmallBitVector paramIndices(ListOfValues.size());
    for (auto i : indices(ListOfValues))
      paramIndices[i] = ListOfValues[i];
    SILReverseAutoDiffIndices indices(GradResultIndex, paramIndices);
    SILReverseAutoDiffConfig config(indices, GradOpts);
    ResultVal = Builder.createGradient(Loc, Val, config);
    break;
  }
  case SILInstructionKind::GraphOperationInst: {
    // TODO(SR-8848): Deserialize attributes.
    auto EndOfArgValues = 3 * NumArguments;
    Identifier MangledName = MF->getIdentifier(ValID);
    SmallVector<SILValue, 4> Args;
    for (unsigned i = 0, e = EndOfArgValues; i < e; i += 3) {
      auto ArgASTTy = MF->getType(ListOfValues[i+1]);
      auto ArgTy = getSILType(ArgASTTy,
                              (SILValueCategory)(unsigned)ListOfValues[i+2]);
      Args.push_back(getLocalValue(ListOfValues[i], ArgTy));
    }
    SmallVector<SILType, 4> ResultSILTypes;
    for (unsigned i = EndOfArgValues, e = ListOfValues.size(); i < e; i += 2) {
      auto ASTTy = MF->getType(ListOfValues[i]);
      auto Ty = getSILType(ASTTy,
                           (SILValueCategory)(unsigned)ListOfValues[i+1]);
      ResultSILTypes.push_back(Ty);
    }
    // TODO: deserialize `runOutOfGraph`.
    ResultVal =
        Builder.createGraphOperation(Loc, MangledName, Args, {},
                                     /*runOutOfGraph*/ false, ResultSILTypes);
    break;
  }
  case SILInstructionKind::AllocGlobalInst: {
    // Format: Name and type. Use SILOneOperandLayout.
    StringRef Name = MF->getIdentifierText(ValID);

    // Find the global variable.
    SILGlobalVariable *g = getGlobalForReference(Name);
    assert(g && "Can't deserialize global variable");

    ResultVal = Builder.createAllocGlobal(Loc, g);
    break;
  }
  case SILInstructionKind::GlobalAddrInst:
  case SILInstructionKind::GlobalValueInst: {
    // Format: Name and type. Use SILOneOperandLayout.
    auto Ty = MF->getType(TyID);
    StringRef Name = MF->getIdentifierText(ValID);

    // Find the global variable.
    SILGlobalVariable *g = getGlobalForReference(Name);
    assert(g && "Can't deserialize global variable");
    SILType expectedType = (OpCode == SILInstructionKind::GlobalAddrInst ?
                            g->getLoweredType().getAddressType() :
                            g->getLoweredType());
    assert(expectedType == getSILType(Ty, (SILValueCategory)TyCategory) &&
           "Type of a global variable does not match GlobalAddr.");
    (void)Ty;
    (void)expectedType;
    if (OpCode == SILInstructionKind::GlobalAddrInst) {
      ResultVal = Builder.createGlobalAddr(Loc, g);
    } else {
      ResultVal = Builder.createGlobalValue(Loc, g);
    }
    break;
  }
  case SILInstructionKind::DeallocStackInst: {
    auto Ty = MF->getType(TyID);
    ResultVal = Builder.createDeallocStack(Loc,
        getLocalValue(ValID,
                      getSILType(Ty, (SILValueCategory)TyCategory)));
    break;
  }
  case SILInstructionKind::DeallocRefInst: {
    auto Ty = MF->getType(TyID);
    bool OnStack = (bool)Attr;
    ResultVal = Builder.createDeallocRef(Loc,
        getLocalValue(ValID,
                      getSILType(Ty, (SILValueCategory)TyCategory)), OnStack);
    break;
  }

  case SILInstructionKind::DeallocPartialRefInst: {
    auto Ty = MF->getType(TyID);
    auto Ty2 = MF->getType(TyID2);
    ResultVal = Builder.createDeallocPartialRef(Loc,
        getLocalValue(ValID,
                      getSILType(Ty,  (SILValueCategory)TyCategory)),
        getLocalValue(ValID2,
                      getSILType(Ty2,  (SILValueCategory)TyCategory2)));
    break;
  }
  case SILInstructionKind::FunctionRefInst: {
    auto Ty = MF->getType(TyID);
    StringRef FuncName = MF->getIdentifierText(ValID);
    ResultVal = Builder.createFunctionRef(Loc,
        getFuncForReference(FuncName,
                            getSILType(Ty, (SILValueCategory)TyCategory)));
    break;
  }
  case SILInstructionKind::MarkDependenceInst: {
    auto Ty = MF->getType(TyID);
    auto Ty2 = MF->getType(TyID2);
    ResultVal = Builder.createMarkDependence(Loc,
        getLocalValue(ValID,
                      getSILType(Ty,  (SILValueCategory)TyCategory)),
        getLocalValue(ValID2,
                      getSILType(Ty2,  (SILValueCategory)TyCategory2)));
    break;
  }
  case SILInstructionKind::CopyBlockWithoutEscapingInst: {
    auto Ty = MF->getType(TyID);
    auto Ty2 = MF->getType(TyID2);
    ResultVal = Builder.createCopyBlockWithoutEscaping(
        Loc, getLocalValue(ValID, getSILType(Ty, (SILValueCategory)TyCategory)),
        getLocalValue(ValID2, getSILType(Ty2, (SILValueCategory)TyCategory2)));
    break;
  }
  case SILInstructionKind::IndexAddrInst: {
    auto Ty = MF->getType(TyID);
    auto Ty2 = MF->getType(TyID2);
    ResultVal = Builder.createIndexAddr(Loc,
        getLocalValue(ValID,
                      getSILType(Ty,  (SILValueCategory)TyCategory)),
        getLocalValue(ValID2,
                      getSILType(Ty2,  (SILValueCategory)TyCategory2)));
    break;
  }
  case SILInstructionKind::TailAddrInst: {
    auto Ty = MF->getType(TyID);
    auto Ty2 = MF->getType(TyID2);
    auto ResultTy = MF->getType(TyID3);
    ResultVal = Builder.createTailAddr(Loc,
        getLocalValue(ValID, getSILType(Ty, SILValueCategory::Address)),
        getLocalValue(ValID2, getSILType(Ty2, SILValueCategory::Object)),
        getSILType(ResultTy, SILValueCategory::Address));
    break;
  }
  case SILInstructionKind::IndexRawPointerInst: {
    auto Ty = MF->getType(TyID);
    auto Ty2 = MF->getType(TyID2);
    ResultVal = Builder.createIndexRawPointer(Loc,
        getLocalValue(ValID,
                      getSILType(Ty,  (SILValueCategory)TyCategory)),
        getLocalValue(ValID2,
                      getSILType(Ty2,  (SILValueCategory)TyCategory2)));
    break;
  }
  case SILInstructionKind::IntegerLiteralInst: {
    auto Ty = MF->getType(TyID);
    auto intTy = Ty->castTo<BuiltinIntegerType>();
    StringRef StringVal = MF->getIdentifierText(ValID);
    // Build APInt from string.
    APInt value(intTy->getGreatestWidth(), StringVal, 10);
    ResultVal = Builder.createIntegerLiteral(Loc,
        getSILType(Ty, (SILValueCategory)TyCategory),
        value);
    break;
  }
  case SILInstructionKind::FloatLiteralInst: {
    auto Ty = MF->getType(TyID);
    auto floatTy = Ty->castTo<BuiltinFloatType>();
    StringRef StringVal = MF->getIdentifierText(ValID);
    // Build APInt from string.
    APInt bits(floatTy->getBitWidth(), StringVal, 16);
    if (bits.getBitWidth() != floatTy->getBitWidth())
      bits = bits.zextOrTrunc(floatTy->getBitWidth());

    APFloat value(floatTy->getAPFloatSemantics(), bits);

    ResultVal = Builder.createFloatLiteral(Loc,
        getSILType(Ty, (SILValueCategory)TyCategory),
        value);
    break;
  }
  case SILInstructionKind::StringLiteralInst: {
    StringRef StringVal = MF->getIdentifierText(ValID);
    auto encoding = fromStableStringEncoding(Attr);
    if (!encoding) return true;
    ResultVal = Builder.createStringLiteral(Loc, StringVal,
                                            encoding.getValue());
    break;
  }
  case SILInstructionKind::MarkFunctionEscapeInst: {
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
  case SILInstructionKind::UnconditionalCheckedCastInst: {
    SILValue Val = getLocalValue(ValID,
                 getSILType(MF->getType(TyID2), (SILValueCategory)TyCategory2));
    SILType Ty = getSILType(MF->getType(TyID), (SILValueCategory)TyCategory);
    ResultVal = Builder.createUnconditionalCheckedCast(Loc, Val, Ty);
    break;
  }

#define UNARY_INSTRUCTION(ID) \
  case SILInstructionKind::ID##Inst:                   \
    assert(RecordKind == SIL_ONE_OPERAND &&            \
           "Layout should be OneOperand.");            \
    ResultVal = Builder.create##ID(Loc, getLocalValue(ValID,  \
                    getSILType(MF->getType(TyID),                        \
                               (SILValueCategory)TyCategory)));          \
   break;

#define REFCOUNTING_INSTRUCTION(ID) \
  case SILInstructionKind::ID##Inst:                   \
    assert(RecordKind == SIL_ONE_OPERAND &&            \
           "Layout should be OneOperand.");            \
    ResultVal = Builder.create##ID(Loc, getLocalValue(ValID,  \
                    getSILType(MF->getType(TyID),                        \
                               (SILValueCategory)TyCategory)),  \
                                   (Atomicity)Attr);          \
    break;

#define ALWAYS_OR_SOMETIMES_LOADABLE_CHECKED_REF_STORAGE(Name, ...) \
  REFCOUNTING_INSTRUCTION(Name##Retain) \
  REFCOUNTING_INSTRUCTION(Name##Release) \
  REFCOUNTING_INSTRUCTION(StrongRetain##Name) \
  UNARY_INSTRUCTION(Copy##Name##Value)
#include "swift/AST/ReferenceStorage.def"
  UNARY_INSTRUCTION(CondFail)
  REFCOUNTING_INSTRUCTION(RetainValue)
  REFCOUNTING_INSTRUCTION(RetainValueAddr)
  REFCOUNTING_INSTRUCTION(UnmanagedRetainValue)
  UNARY_INSTRUCTION(CopyValue)
  UNARY_INSTRUCTION(DestroyValue)
  REFCOUNTING_INSTRUCTION(ReleaseValue)
  REFCOUNTING_INSTRUCTION(ReleaseValueAddr)
  REFCOUNTING_INSTRUCTION(UnmanagedReleaseValue)
  REFCOUNTING_INSTRUCTION(AutoreleaseValue)
  REFCOUNTING_INSTRUCTION(UnmanagedAutoreleaseValue)
  REFCOUNTING_INSTRUCTION(SetDeallocating)
  UNARY_INSTRUCTION(DeinitExistentialAddr)
  UNARY_INSTRUCTION(DeinitExistentialValue)
  UNARY_INSTRUCTION(EndBorrow)
  UNARY_INSTRUCTION(DestroyAddr)
  UNARY_INSTRUCTION(Return)
  UNARY_INSTRUCTION(Throw)
  UNARY_INSTRUCTION(ClassifyBridgeObject)
  UNARY_INSTRUCTION(ValueToBridgeObject)
  UNARY_INSTRUCTION(FixLifetime)
  UNARY_INSTRUCTION(EndLifetime)
  UNARY_INSTRUCTION(CopyBlock)
  UNARY_INSTRUCTION(LoadBorrow)
  UNARY_INSTRUCTION(BeginBorrow)
  REFCOUNTING_INSTRUCTION(StrongRetain)
  REFCOUNTING_INSTRUCTION(StrongRelease)
  UNARY_INSTRUCTION(IsUnique)
  UNARY_INSTRUCTION(AbortApply)
  UNARY_INSTRUCTION(EndApply)
#undef UNARY_INSTRUCTION
#undef REFCOUNTING_INSTRUCTION

  case SILInstructionKind::IsEscapingClosureInst: {
    assert(RecordKind == SIL_ONE_OPERAND && "Layout should be OneOperand.");
    unsigned verificationType = Attr;
    ResultVal = Builder.createIsEscapingClosure(
        Loc,
        getLocalValue(
            ValID, getSILType(MF->getType(TyID), (SILValueCategory)TyCategory)),
        verificationType);
    break;
  }

  case SILInstructionKind::DestructureTupleInst: {
    assert(RecordKind == SIL_ONE_OPERAND && "Layout should be OneOperand.");
    SILValue Operand = getLocalValue(
        ValID, getSILType(MF->getType(TyID), (SILValueCategory)TyCategory));
    ResultVal = Builder.createDestructureTuple(Loc, Operand);
    break;
  }
  case SILInstructionKind::DestructureStructInst: {
    assert(RecordKind == SIL_ONE_OPERAND && "Layout should be OneOperand.");
    SILValue Operand = getLocalValue(
        ValID, getSILType(MF->getType(TyID), (SILValueCategory)TyCategory));
    ResultVal = Builder.createDestructureStruct(Loc, Operand);
    break;
  }
  case SILInstructionKind::UncheckedOwnershipConversionInst: {
    auto Ty = MF->getType(TyID);
    auto ResultKind = ValueOwnershipKind(Attr);
    ResultVal = Builder.createUncheckedOwnershipConversion(
        Loc, getLocalValue(ValID, getSILType(Ty, (SILValueCategory)TyCategory)),
        ResultKind);
    break;
  }

  case SILInstructionKind::LoadInst: {
    auto Ty = MF->getType(TyID);
    auto Qualifier = LoadOwnershipQualifier(Attr);
    ResultVal = Builder.createLoad(
        Loc, getLocalValue(ValID, getSILType(Ty, (SILValueCategory)TyCategory)),
        Qualifier);
    break;
  }

#define NEVER_OR_SOMETIMES_LOADABLE_CHECKED_REF_STORAGE(Name, ...) \
  case SILInstructionKind::Load##Name##Inst: { \
    auto Ty = MF->getType(TyID); \
    bool isTake = (Attr > 0); \
    auto Val = getLocalValue(ValID, getSILType(Ty, \
                                               SILValueCategory(TyCategory)));\
    ResultVal = Builder.createLoad##Name(Loc, Val, IsTake_t(isTake)); \
    break; \
  } \
  case SILInstructionKind::Store##Name##Inst: { \
    auto Ty = MF->getType(TyID); \
    SILType addrType = getSILType(Ty, (SILValueCategory)TyCategory); \
    auto refType = addrType.castTo<Name##StorageType>(); \
    auto ValType = SILType::getPrimitiveObjectType(refType.getReferentType()); \
    bool isInit = (Attr > 0); \
    ResultVal = Builder.createStore##Name(Loc, \
                                          getLocalValue(ValID, ValType), \
                                          getLocalValue(ValID2, addrType), \
                                          IsInitialization_t(isInit)); \
    break; \
  }
#include "swift/AST/ReferenceStorage.def"
  case SILInstructionKind::MarkUninitializedInst: {
    auto Ty = getSILType(MF->getType(TyID), (SILValueCategory)TyCategory);
    auto Kind = (MarkUninitializedInst::Kind)Attr;
    auto Val = getLocalValue(ValID, Ty);
    ResultVal = Builder.createMarkUninitialized(Loc, Val, Kind);
    break;
  }
  case SILInstructionKind::StoreInst: {
    auto Ty = MF->getType(TyID);
    SILType addrType = getSILType(Ty, (SILValueCategory)TyCategory);
    SILType ValType = addrType.getObjectType();
    auto Qualifier = StoreOwnershipQualifier(Attr);
    ResultVal = Builder.createStore(Loc, getLocalValue(ValID, ValType),
                                    getLocalValue(ValID2, addrType), Qualifier);
    break;
  }
  case SILInstructionKind::StoreBorrowInst: {
    auto Ty = MF->getType(TyID);
    SILType addrType = getSILType(Ty, (SILValueCategory)TyCategory);
    SILType ValType = addrType.getObjectType();
    ResultVal = Builder.createStoreBorrow(Loc, getLocalValue(ValID, ValType),
                                          getLocalValue(ValID2, addrType));
    break;
  }
  case SILInstructionKind::BeginAccessInst: {
    SILValue op = getLocalValue(
        ValID, getSILType(MF->getType(TyID), (SILValueCategory)TyCategory));
    auto accessKind = SILAccessKind(Attr & 0x3);
    auto enforcement = SILAccessEnforcement((Attr >> 2) & 0x3);
    bool noNestedConflict = (Attr >> 4) & 0x01;
    bool fromBuiltin = (Attr >> 5) & 0x01;
    ResultVal =
        Builder.createBeginAccess(Loc, op, accessKind, enforcement,
                                  noNestedConflict, fromBuiltin);
    break;
  }
  case SILInstructionKind::EndAccessInst: {
    SILValue op = getLocalValue(
        ValID, getSILType(MF->getType(TyID), (SILValueCategory)TyCategory));
    bool aborted = Attr & 0x1;
    ResultVal = Builder.createEndAccess(Loc, op, aborted);
    break;
  }
  case SILInstructionKind::BeginUnpairedAccessInst: {
    SILValue source = getLocalValue(
        ValID, getSILType(MF->getType(TyID), (SILValueCategory)TyCategory));
    SILValue buffer = getLocalValue(
        ValID2, getSILType(MF->getType(TyID2), (SILValueCategory)TyCategory2));
    auto accessKind = SILAccessKind(Attr & 0x3);
    auto enforcement = SILAccessEnforcement((Attr >> 2) & 0x03);
    bool noNestedConflict = (Attr >> 4) & 0x01;
    bool fromBuiltin = (Attr >> 5) & 0x01;
    ResultVal = Builder.createBeginUnpairedAccess(
      Loc, source, buffer, accessKind, enforcement, noNestedConflict,
      fromBuiltin);
    break;
  }
  case SILInstructionKind::EndUnpairedAccessInst: {
    SILValue op = getLocalValue(
        ValID, getSILType(MF->getType(TyID), (SILValueCategory)TyCategory));
    bool aborted = Attr & 0x1;
    auto enforcement = SILAccessEnforcement((Attr >> 1) & 0x03);
    bool fromBuiltin = (Attr >> 3) & 0x01;
    ResultVal = Builder.createEndUnpairedAccess(Loc, op, enforcement, aborted,
                                                fromBuiltin);
    break;
  }
  case SILInstructionKind::CopyAddrInst: {
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
  case SILInstructionKind::AssignInst: {
    auto Ty = MF->getType(TyID);
    SILType addrType = getSILType(Ty, (SILValueCategory)TyCategory);
    SILType ValType = addrType.getObjectType();
    ResultVal = Builder.createAssign(Loc,
                    getLocalValue(ValID, ValType),
                    getLocalValue(ValID2, addrType));
    break;
  }
  case SILInstructionKind::BindMemoryInst: {
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
  case SILInstructionKind::StructElementAddrInst:
  case SILInstructionKind::StructExtractInst: {
    // Use SILOneValueOneOperandLayout.
    VarDecl *Field = cast<VarDecl>(MF->getDecl(ValID));
    auto Ty = MF->getType(TyID);
    auto Val = getLocalValue(ValID2,
                             getSILType(Ty, (SILValueCategory)TyCategory));
    auto ResultTy = Val->getType().getFieldType(Field, SILMod);
    if (OpCode == SILInstructionKind::StructElementAddrInst)
      ResultVal = Builder.createStructElementAddr(Loc, Val, Field,
                                                  ResultTy.getAddressType());
    else
      ResultVal = Builder.createStructExtract(Loc, Val, Field,
                                              ResultTy.getObjectType());
    break;
  }
  case SILInstructionKind::StructInst: {
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
  case SILInstructionKind::TupleElementAddrInst:
  case SILInstructionKind::TupleExtractInst: {
    // Use OneTypeOneOperand layout where the field number is stored in TypeID.
    auto Ty2 = MF->getType(TyID2);
    SILType ST = getSILType(Ty2, (SILValueCategory)TyCategory2);
    TupleType *TT = ST.castTo<TupleType>();

    auto ResultTy = TT->getElement(TyID).getType();
    switch (OpCode) {
    default: llvm_unreachable("Out of sync with parent switch");
    case SILInstructionKind::TupleElementAddrInst:
      ResultVal = Builder.createTupleElementAddr(Loc,
                      getLocalValue(ValID, ST),
                      TyID, getSILType(ResultTy, SILValueCategory::Address));
      break;
    case SILInstructionKind::TupleExtractInst:
      ResultVal = Builder.createTupleExtract(Loc,
                                             getLocalValue(ValID,ST),
                                             TyID,
                                getSILType(ResultTy, SILValueCategory::Object));
      break;
    }
    break;
  }
  case SILInstructionKind::TupleInst: {
    // Format: a type followed by a list of values. A value is expressed by
    // 2 IDs: ValueID, ValueResultNumber.
    auto Ty = MF->getType(TyID);
    TupleType *TT = Ty->castTo<TupleType>();
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
  case SILInstructionKind::ObjectInst: {
    llvm_unreachable("Serialization of global initializers not supported");
  }
  case SILInstructionKind::BranchInst: {
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
  case SILInstructionKind::CondBranchInst: {
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
  case SILInstructionKind::SwitchEnumInst:
  case SILInstructionKind::SwitchEnumAddrInst: {
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
    if (OpCode == SILInstructionKind::SwitchEnumInst)
      ResultVal = Builder.createSwitchEnum(Loc, Cond, DefaultBB, CaseBBs);
    else
      ResultVal = Builder.createSwitchEnumAddr(Loc, Cond,
                      DefaultBB, CaseBBs);
    break;
  }
  case SILInstructionKind::SelectEnumInst:
  case SILInstructionKind::SelectEnumAddrInst: {
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
    if (OpCode == SILInstructionKind::SelectEnumInst)
      ResultVal = Builder.createSelectEnum(Loc, Cond, ResultTy,
                                           DefaultVal, CaseVals);
    else
      ResultVal = Builder.createSelectEnumAddr(Loc, Cond, ResultTy,
                                               DefaultVal, CaseVals);
    break;
  }
  case SILInstructionKind::SwitchValueInst: {
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
  case SILInstructionKind::SelectValueInst: {
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
  case SILInstructionKind::EnumInst: {
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
  case SILInstructionKind::InitEnumDataAddrInst: {
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
  case SILInstructionKind::UncheckedEnumDataInst: {
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
  case SILInstructionKind::UncheckedTakeEnumDataAddrInst: {
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
  case SILInstructionKind::InjectEnumAddrInst: {
    // Use SILOneValueOneOperandLayout.
    EnumElementDecl *Elt = cast<EnumElementDecl>(MF->getDecl(ValID));
    auto Ty = MF->getType(TyID);
    ResultVal = Builder.createInjectEnumAddr(Loc,
                    getLocalValue(ValID2,
                                  getSILType(Ty, (SILValueCategory)TyCategory)),
                    Elt);
    break;
  }
  case SILInstructionKind::RefElementAddrInst: {
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
  case SILInstructionKind::RefTailAddrInst: {
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
  case SILInstructionKind::ClassMethodInst:
  case SILInstructionKind::SuperMethodInst:
  case SILInstructionKind::ObjCMethodInst:
  case SILInstructionKind::ObjCSuperMethodInst: {
    // Format: a type, an operand and a SILDeclRef. Use SILOneTypeValuesLayout:
    // type, Attr, SILDeclRef (DeclID, Kind, uncurryLevel), and an operand.
    unsigned NextValueIndex = 0;
    SILDeclRef DRef = getSILDeclRef(MF, ListOfValues, NextValueIndex);
    SILType Ty = getSILType(MF->getType(TyID), (SILValueCategory)TyCategory);
    assert(ListOfValues.size() >= NextValueIndex + 2 &&
           "Out of entries for MethodInst");
    SILType operandTy = getSILType(MF->getType(ListOfValues[NextValueIndex]),
                                   (SILValueCategory)ListOfValues[NextValueIndex+1]);
    NextValueIndex += 2;

    switch (OpCode) {
    default: llvm_unreachable("Out of sync with parent switch");
    case SILInstructionKind::ClassMethodInst:
      ResultVal = Builder.createClassMethod(Loc,
                    getLocalValue(ListOfValues[NextValueIndex], operandTy),
                    DRef, Ty);
      break;
    case SILInstructionKind::SuperMethodInst:
      ResultVal = Builder.createSuperMethod(Loc,
                    getLocalValue(ListOfValues[NextValueIndex], operandTy),
                    DRef, Ty);
      break;
    case SILInstructionKind::ObjCMethodInst:
      ResultVal = Builder.createObjCMethod(Loc,
                    getLocalValue(ListOfValues[NextValueIndex], operandTy),
                    DRef, Ty);
      break;
    case SILInstructionKind::ObjCSuperMethodInst:
      ResultVal = Builder.createObjCSuperMethod(Loc,
                    getLocalValue(ListOfValues[NextValueIndex], operandTy),
                    DRef, Ty);
      break;
    }
    break;
  }
  case SILInstructionKind::WitnessMethodInst: {
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
        Loc, Ty, Conformance, DRef, OperandTy);
    break;
  }
  case SILInstructionKind::DynamicMethodBranchInst: {
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
  case SILInstructionKind::CheckedCastBranchInst: {
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
  case SILInstructionKind::CheckedCastValueBranchInst: {
    // Format: the cast kind, a typed value, a BasicBlock ID for success,
    // a BasicBlock ID for failure. Uses SILOneTypeValuesLayout.
    assert(ListOfValues.size() == 5 &&
           "expect 6 numbers for CheckedCastValueBranchInst");
    SILType opTy = getSILType(MF->getType(ListOfValues[1]),
                              (SILValueCategory)ListOfValues[2]);
    SILValue op = getLocalValue(ListOfValues[0], opTy);
    SILType castTy =
        getSILType(MF->getType(TyID), (SILValueCategory)TyCategory);
    auto *successBB = getBBForReference(Fn, ListOfValues[3]);
    auto *failureBB = getBBForReference(Fn, ListOfValues[4]);

    ResultVal = Builder.createCheckedCastValueBranch(Loc, op, castTy, successBB,
                                                     failureBB);
    break;
  }
  case SILInstructionKind::UnconditionalCheckedCastValueInst: {
    SILValue Val = getLocalValue(
        ValID, getSILType(MF->getType(TyID2), (SILValueCategory)TyCategory2));
    SILType Ty = getSILType(MF->getType(TyID), (SILValueCategory)TyCategory);
    ResultVal = Builder.createUnconditionalCheckedCastValue(Loc, Val, Ty);
    break;
  }
  case SILInstructionKind::UnconditionalCheckedCastAddrInst: {
    // ignore attr.
    CanType sourceType = MF->getType(ListOfValues[0])->getCanonicalType();
    SILType srcAddrTy = getSILType(MF->getType(ListOfValues[2]),
                                   (SILValueCategory)ListOfValues[3]);
    SILValue src = getLocalValue(ListOfValues[1], srcAddrTy);

    CanType targetType = MF->getType(ListOfValues[4])->getCanonicalType();
    SILType destAddrTy =
        getSILType(MF->getType(TyID), (SILValueCategory)TyCategory);
    SILValue dest = getLocalValue(ListOfValues[5], destAddrTy);

    ResultVal = Builder.createUnconditionalCheckedCastAddr(Loc, src, sourceType,
                                                           dest, targetType);
    break;
  }
  case SILInstructionKind::CheckedCastAddrBranchInst: {
    CastConsumptionKind consumption = getCastConsumptionKind(ListOfValues[0]);

    CanType sourceType = MF->getType(ListOfValues[1])->getCanonicalType();
    SILType srcAddrTy = getSILType(MF->getType(ListOfValues[3]),
                                   (SILValueCategory)ListOfValues[4]);
    SILValue src = getLocalValue(ListOfValues[2], srcAddrTy);

    CanType targetType = MF->getType(ListOfValues[5])->getCanonicalType();
    SILType destAddrTy =
      getSILType(MF->getType(TyID), (SILValueCategory) TyCategory);
    SILValue dest = getLocalValue(ListOfValues[6], destAddrTy);

    auto *successBB = getBBForReference(Fn, ListOfValues[7]);
    auto *failureBB = getBBForReference(Fn, ListOfValues[8]);
    ResultVal = Builder.createCheckedCastAddrBranch(Loc, consumption,
                                                    src, sourceType,
                                                    dest, targetType,
                                                    successBB, failureBB);
    break;
  }
  case SILInstructionKind::UncheckedRefCastAddrInst: {
    CanType sourceType = MF->getType(ListOfValues[0])->getCanonicalType();
    // ignore attr.
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
  case SILInstructionKind::InitBlockStorageHeaderInst: {
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
    
    auto SubMap = MF->getSubstitutionMap(ListOfValues[4]);

    ResultVal = Builder.createInitBlockStorageHeader(Loc, storage, invoke,
                                                     blockTy, SubMap);
    break;
  }
  case SILInstructionKind::UnreachableInst: {
    ResultVal = Builder.createUnreachable(Loc);
    break;
  }
  case SILInstructionKind::UnwindInst: {
    ResultVal = Builder.createUnwind(Loc);
    break;
  }
  case SILInstructionKind::YieldInst: {
    SILBasicBlock *unwindBB = getBBForReference(Fn, ListOfValues.back());
    ListOfValues = ListOfValues.drop_back();
    SILBasicBlock *resumeBB = getBBForReference(Fn, ListOfValues.back());
    ListOfValues = ListOfValues.drop_back();

    SmallVector<SILValue, 4> yieldedValues;
    for (unsigned I = 0, E = ListOfValues.size(); I < E; I += 3) {
      auto valueTy = MF->getType(ListOfValues[I]);
      auto valueCategory = (SILValueCategory) ListOfValues[I+1];
      yieldedValues.push_back(
        getLocalValue(ListOfValues[I+2], getSILType(valueTy, valueCategory)));
    }

    ResultVal = Builder.createYield(Loc, yieldedValues, resumeBB, unwindBB);
    break;
  }
  case SILInstructionKind::KeyPathInst: {
    unsigned nextValue = 0;
    SILType kpTy
      = getSILType(MF->getType(TyID), (SILValueCategory)TyCategory);

    auto rootTy = MF->getType(ListOfValues[nextValue++]);
    auto valueTy = MF->getType(ListOfValues[nextValue++]);
    auto numComponents = ListOfValues[nextValue++];
    auto numOperands = ListOfValues[nextValue++];
    auto subMap = MF->getSubstitutionMap(ListOfValues[nextValue++]);
    auto objcString = MF->getIdentifierText(ListOfValues[nextValue++]);
    auto numGenericParams = ListOfValues[nextValue++];
    
    SmallVector<GenericTypeParamType *, 4> genericParams;
    while (numGenericParams-- > 0) {
      genericParams.push_back(MF->getType(ListOfValues[nextValue++])
                                ->castTo<GenericTypeParamType>());
    }
    
    SmallVector<KeyPathPatternComponent, 4> components;
    components.reserve(numComponents);
    while (numComponents-- > 0) {
      components.push_back(*readKeyPathComponent(ListOfValues, nextValue));
    }
    
    SmallVector<Requirement, 4> requirements;
    MF->readGenericRequirements(requirements, SILCursor);
    
    CanGenericSignature sig = nullptr;
    if (!genericParams.empty() || !requirements.empty())
      sig = GenericSignature::get(genericParams, requirements)
         ->getCanonicalSignature();
    
    auto pattern = KeyPathPattern::get(SILMod, sig,
                                       rootTy->getCanonicalType(),
                                       valueTy->getCanonicalType(),
                                       components,
                                       objcString);
    
    SmallVector<SILValue, 4> operands;
    
    operands.reserve(numOperands);
    while (numOperands-- > 0) {
      auto opValue = ListOfValues[nextValue++];
      auto opTy = MF->getType(ListOfValues[nextValue++]);
      auto opCat = (SILValueCategory)ListOfValues[nextValue++];
      operands.push_back(getLocalValue(opValue, getSILType(opTy, opCat)));
    }
    
    ResultVal = Builder.createKeyPath(Loc, pattern, subMap, operands, kpTy);
    break;
  }
  case SILInstructionKind::MarkUninitializedBehaviorInst:
    llvm_unreachable("todo");
  }

  for (auto result : ResultVal->getResults()) {
    LastValueID = LastValueID + 1;
    setLocalValue(result, LastValueID);
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

  auto maybeFunc = readSILFunctionChecked(*iter, InFunc, name,
                                          /*declarationOnly*/ false);
  if (!maybeFunc) {
    // Ignore the error; treat it as if we didn't have a definition.
    llvm::consumeError(maybeFunc.takeError());
    return nullptr;
  }

  if (maybeFunc.get()) {
    LLVM_DEBUG(llvm::dbgs() << "Deserialize SIL:\n";
               maybeFunc.get()->dump());
    assert(InFunc->getName() == maybeFunc.get()->getName());
  }

  return maybeFunc.get();
}

/// Check for existence of a function with a given name and required linkage.
/// This function is modeled after readSILFunction. But it does not
/// create a SILFunction object.
bool SILDeserializer::hasSILFunction(StringRef Name,
                                     Optional<SILLinkage> Linkage) {
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
    return !Linkage || cacheEntry.get()->getLinkage() == *Linkage;

  BCOffsetRAII restoreOffset(SILCursor);
  SILCursor.JumpToBit(cacheEntry.getOffset());

  auto entry = SILCursor.advance(AF_DontPopBlockAtEnd);
  if (entry.Kind == llvm::BitstreamEntry::Error) {
    LLVM_DEBUG(llvm::dbgs() << "Cursor advance error in hasSILFunction.\n");
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
  unsigned rawLinkage, isTransparent, isSerialized, isThunk,
      isWithoutactuallyEscapingThunk, isGlobal, inlineStrategy,
      // SWIFT_ENABLE_TENSORFLOW
      optimizationMode, effect, numSpecAttrs, numReverseDifferentiableAttrs,
      hasQualifiedOwnership, isWeakLinked;
  ArrayRef<uint64_t> SemanticsIDs;
  SILFunctionLayout::readRecord(
      scratch, rawLinkage, isTransparent, isSerialized, isThunk,
      isWithoutactuallyEscapingThunk, isGlobal, inlineStrategy,
      optimizationMode, effect, numSpecAttrs,
      // SWIFT_ENABLE_TENSORFLOW
      numReverseDifferentiableAttrs, hasQualifiedOwnership, isWeakLinked,
      funcTyID, genericEnvID, clangOwnerID, SemanticsIDs);
  auto linkage = fromStableSILLinkage(rawLinkage);
  if (!linkage) {
    LLVM_DEBUG(llvm::dbgs() << "invalid linkage code " << rawLinkage
                            << " for SIL function " << Name << "\n");
    return false;
  }

  // Bail if it is not a required linkage.
  if (Linkage && linkage.getValue() != *Linkage)
    return false;

  LLVM_DEBUG(llvm::dbgs() << "Found SIL Function: " << Name << "\n");
  return true;
}


SILFunction *SILDeserializer::lookupSILFunction(StringRef name,
                                                bool declarationOnly) {
  if (!FuncTable)
    return nullptr;
  auto iter = FuncTable->find(name);
  if (iter == FuncTable->end())
    return nullptr;

  auto maybeFunc = readSILFunctionChecked(*iter, nullptr, name,
                                          declarationOnly);

  if (!maybeFunc) {
    // Ignore the error; treat it as if we didn't have a definition.
    llvm::consumeError(maybeFunc.takeError());
    return nullptr;
  }

  if (maybeFunc.get()) {
    LLVM_DEBUG(llvm::dbgs() << "Deserialize SIL:\n";
               maybeFunc.get()->dump());
  }
  return maybeFunc.get();
}

SILGlobalVariable *SILDeserializer::readGlobalVar(StringRef Name) {
  if (!GlobalVarList)
    return nullptr;

  PrettyStackTraceStringAction trace("deserializing SIL global", Name);

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
    LLVM_DEBUG(llvm::dbgs() << "Cursor advance error in readGlobalVar.\n");
    return nullptr;
  }

  SmallVector<uint64_t, 64> scratch;
  StringRef blobData;
  unsigned kind = SILCursor.readRecord(entry.ID, scratch, &blobData);
  assert(kind == SIL_GLOBALVAR && "expect a sil global var");
  (void)kind;

  TypeID TyID;
  DeclID dID;
  unsigned rawLinkage, isSerialized, IsDeclaration, IsLet;
  SILGlobalVarLayout::readRecord(scratch, rawLinkage, isSerialized,
                                 IsDeclaration, IsLet, TyID, dID);
  if (TyID == 0) {
    LLVM_DEBUG(llvm::dbgs() << "SILGlobalVariable typeID is 0.\n");
    return nullptr;
  }

  auto linkage = fromStableSILLinkage(rawLinkage);
  if (!linkage) {
    LLVM_DEBUG(llvm::dbgs() << "invalid linkage code " << rawLinkage
                            << " for SILGlobalVariable\n");
    return nullptr;
  }

  auto Ty = MF->getType(TyID);
  SILGlobalVariable *v = SILGlobalVariable::create(
      SILMod, linkage.getValue(),
      isSerialized ? IsSerialized : IsNotSerialized,
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

    auto maybeFunc = readSILFunctionChecked(*DI, nullptr, *KI, false,
                                            false/*errorIfEmptyBody*/);
    if (!maybeFunc) {
      // Ignore the error; treat it as if we didn't have a definition.
      llvm::consumeError(maybeFunc.takeError());
    }
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
    LLVM_DEBUG(llvm::dbgs() << "Cursor advance error in readVTable.\n");
    return nullptr;
  }

  SmallVector<uint64_t, 64> scratch;
  StringRef blobData;
  unsigned kind = SILCursor.readRecord(entry.ID, scratch, &blobData);
  assert(kind == SIL_VTABLE && "expect a sil vtable");
  (void)kind;

  DeclID ClassID;
  unsigned Serialized;
  VTableLayout::readRecord(scratch, ClassID, Serialized);
  if (ClassID == 0) {
    LLVM_DEBUG(llvm::dbgs() << "VTable classID is 0.\n");
    return nullptr;
  }

  ClassDecl *theClass = cast<ClassDecl>(MF->getDecl(ClassID));

  PrettyStackTraceDecl trace("deserializing SIL vtable for", theClass);

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
         kind != SIL_FUNCTION &&
         kind != SIL_PROPERTY) {
    assert(kind == SIL_VTABLE_ENTRY &&
           "Content of Vtable should be in SIL_VTABLE_ENTRY.");
    ArrayRef<uint64_t> ListOfValues;
    DeclID NameID;
    unsigned RawLinkage;
    unsigned RawEntryKind;
    VTableEntryLayout::readRecord(scratch, NameID, RawEntryKind, RawLinkage, ListOfValues);

    auto Linkage = fromStableSILLinkage(RawLinkage);
    if (!Linkage) {
      LLVM_DEBUG(llvm::dbgs() << "invalid linkage code " << RawLinkage
                              << " for VTable Entry\n");
      MF->error();
      return nullptr;
    }

    auto EntryKind = fromStableVTableEntryKind(RawEntryKind);

    SILFunction *Func = getFuncForReference(MF->getIdentifierText(NameID));
    if (Func) {
      unsigned NextValueIndex = 0;
      vtableEntries.emplace_back(getSILDeclRef(MF, ListOfValues, NextValueIndex),
                                 Func, EntryKind.getValue(), Linkage.getValue());
    }

    // Fetch the next record.
    scratch.clear();
    entry = SILCursor.advance(AF_DontPopBlockAtEnd);
    if (entry.Kind == llvm::BitstreamEntry::EndBlock)
      // EndBlock means the end of this VTable.
      break;
    kind = SILCursor.readRecord(entry.ID, scratch);
  }

  // If we've already serialized the module, don't mark the witness table
  // as serialized, since we no longer need to enforce resilience
  // boundaries.
  if (SILMod.isSerialized())
    Serialized = 0;

  SILVTable *vT = SILVTable::create(
      SILMod, theClass,
      Serialized ? IsSerialized : IsNotSerialized,
      vtableEntries);
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

SILProperty *SILDeserializer::readProperty(DeclID PId) {
  auto &propOrOffset = Properties[PId-1];
  
  if (propOrOffset.isFullyDeserialized())
    return propOrOffset.get();

  BCOffsetRAII restoreOffset(SILCursor);
  SILCursor.JumpToBit(propOrOffset.getOffset());
  auto entry = SILCursor.advance(AF_DontPopBlockAtEnd);
  if (entry.Kind == llvm::BitstreamEntry::Error) {
    LLVM_DEBUG(llvm::dbgs() << "Cursor advance error in readProperty.\n");
    return nullptr;
  }

  SmallVector<uint64_t, 64> scratch;
  StringRef blobData;
  unsigned kind = SILCursor.readRecord(entry.ID, scratch, &blobData);
  assert(kind == SIL_PROPERTY && "expect a sil_property");
  (void)kind;

  unsigned Serialized;
  DeclID StorageID;
  ArrayRef<uint64_t> ComponentValues;
  PropertyLayout::readRecord(scratch, StorageID, Serialized, ComponentValues);
  
  auto decl = cast<AbstractStorageDecl>(MF->getDecl(StorageID));
  unsigned ComponentValueIndex = 0;
  auto component = readKeyPathComponent(ComponentValues, ComponentValueIndex);
  
  auto prop = SILProperty::create(SILMod, Serialized, decl, component);
  propOrOffset.set(prop, /*fully deserialized*/ true);
  return prop;
}

void SILDeserializer::getAllProperties() {
  for (unsigned I = 0, E = Properties.size(); I < E; ++I) {
    readProperty(I+1);
  }
}

void SILDeserializer::readWitnessTableEntries(
    llvm::BitstreamEntry &entry,
    std::vector<SILWitnessTable::Entry> &witnessEntries,
    std::vector<SILWitnessTable::ConditionalConformance>
      &conditionalConformances) {
  SmallVector<uint64_t, 64> scratch;
  unsigned kind = SILCursor.readRecord(entry.ID, scratch);

  // Another record means the end of this WitnessTable.
  while (kind != SIL_WITNESS_TABLE &&
         kind != SIL_DEFAULT_WITNESS_TABLE &&
         kind != SIL_FUNCTION) {
    if (kind == SIL_DEFAULT_WITNESS_TABLE_NO_ENTRY) {
      witnessEntries.push_back(SILDefaultWitnessTable::Entry());
    } else if (kind == SIL_WITNESS_BASE_ENTRY) {
      DeclID protoId;
      WitnessBaseEntryLayout::readRecord(scratch, protoId);
      ProtocolDecl *proto = cast<ProtocolDecl>(MF->getDecl(protoId));
      auto conformance = MF->readConformance(SILCursor);
      witnessEntries.push_back(SILWitnessTable::BaseProtocolWitness{
        proto, conformance.getConcrete()
      });
    } else if (kind == SIL_WITNESS_ASSOC_PROTOCOL) {
      TypeID assocId;
      DeclID protoId;
      WitnessAssocProtocolLayout::readRecord(scratch, assocId, protoId);
      CanType type = MF->getType(assocId)->getCanonicalType();
      ProtocolDecl *proto = cast<ProtocolDecl>(MF->getDecl(protoId));
      auto conformance = MF->readConformance(SILCursor);
      witnessEntries.push_back(SILWitnessTable::AssociatedTypeProtocolWitness{
        type, proto, conformance
      });
    } else if (kind == SIL_WITNESS_ASSOC_ENTRY) {
      DeclID assocId;
      TypeID tyId;
      WitnessAssocEntryLayout::readRecord(scratch, assocId, tyId);
      AssociatedTypeDecl *assoc = cast<AssociatedTypeDecl>(MF->getDecl(assocId));
      witnessEntries.push_back(SILWitnessTable::AssociatedTypeWitness{
        assoc, MF->getType(tyId)->getCanonicalType()
      });
    } else if (kind == SIL_WITNESS_METHOD_ENTRY) {
      ArrayRef<uint64_t> ListOfValues;
      DeclID NameID;
      WitnessMethodEntryLayout::readRecord(scratch, NameID, ListOfValues);
      SILFunction *Func = nullptr;
      if (NameID != 0) {
        Func = getFuncForReference(MF->getIdentifierText(NameID));
      }
      if (Func || NameID == 0) {
        unsigned NextValueIndex = 0;
        witnessEntries.push_back(SILWitnessTable::MethodWitness{
          getSILDeclRef(MF, ListOfValues, NextValueIndex), Func
        });
      }
    } else {
      assert(kind == SIL_WITNESS_CONDITIONAL_CONFORMANCE &&
             "Content of WitnessTable should be in "
             "SIL_WITNESS_CONDITIONAL_CONFORMANCE.");
      TypeID assocId;
      WitnessConditionalConformanceLayout::readRecord(scratch, assocId);
      CanType type = MF->getType(assocId)->getCanonicalType();
      auto conformance = MF->readConformance(SILCursor);
      conditionalConformances.push_back(
          SILWitnessTable::ConditionalConformance{type, conformance});
    }

    // Fetch the next record.
    scratch.clear();
    entry = SILCursor.advance(AF_DontPopBlockAtEnd);
    if (entry.Kind == llvm::BitstreamEntry::EndBlock)
      // EndBlock means the end of this WitnessTable.
      break;
    kind = SILCursor.readRecord(entry.ID, scratch);
  }
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
    LLVM_DEBUG(llvm::dbgs() << "Cursor advance error in readWitnessTable.\n");
    return nullptr;
  }

  SmallVector<uint64_t, 64> scratch;
  StringRef blobData;
  unsigned kind = SILCursor.readRecord(entry.ID, scratch, &blobData);
  assert(kind == SIL_WITNESS_TABLE && "expect a sil witnesstable");
  (void)kind;

  unsigned RawLinkage;
  unsigned IsDeclaration;
  unsigned Serialized;
  WitnessTableLayout::readRecord(scratch, RawLinkage,
                                 IsDeclaration, Serialized);

  auto Linkage = fromStableSILLinkage(RawLinkage);
  if (!Linkage) {
    LLVM_DEBUG(llvm::dbgs() << "invalid linkage code " << RawLinkage
                            << " for SILFunction\n");
    MF->error();
    return nullptr;
  }

  // Deserialize Conformance.
  auto theConformance = cast<NormalProtocolConformance>(
                          MF->readConformance(SILCursor).getConcrete());

  PrettyStackTraceType trace(SILMod.getASTContext(),
                             "deserializing SIL witness table for",
                             theConformance->getType());
  PrettyStackTraceDecl trace2("... to", theConformance->getProtocol());

  if (!existingWt)
    existingWt = SILMod.lookUpWitnessTable(theConformance, false);
  auto wT = existingWt;

  // If we have an existing witness table, verify that the conformance matches
  // up.
  if (wT) {
    if (wT->getConformance() != theConformance) {
      LLVM_DEBUG(llvm::dbgs() << "Conformance mismatch.\n");
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
  
  // We may see multiple shared-linkage definitions of the same witness table
  // for the same conformance.
  if (wT->isDefinition() && hasSharedVisibility(*Linkage)
      && hasSharedVisibility(wT->getLinkage())) {
    wTableOrOffset.set(wT, /*fully deserialized*/ true);
    return wT;
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

  std::vector<SILWitnessTable::Entry> witnessEntries;
  std::vector<SILWitnessTable::ConditionalConformance> conditionalConformances;
  readWitnessTableEntries(entry, witnessEntries, conditionalConformances);

  // If we've already serialized the module, don't mark the witness table
  // as serialized, since we no longer need to enforce resilience
  // boundaries.
  if (SILMod.isSerialized())
    Serialized = 0;

  wT->convertToDefinition(witnessEntries, conditionalConformances,
                          Serialized ? IsSerialized : IsNotSerialized);
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
    LLVM_DEBUG(llvm::dbgs() << "Deserialize SIL:\n"; Wt->dump());

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
    LLVM_DEBUG(llvm::dbgs() << "Cursor advance error in "
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
    LLVM_DEBUG(llvm::dbgs() << "invalid linkage code " << RawLinkage
                            << " for SILFunction\n");
    MF->error();
    return nullptr;
  }

  ProtocolDecl *proto = cast<ProtocolDecl>(MF->getDecl(protoId));
  if (proto == nullptr) {
    LLVM_DEBUG(llvm::dbgs() << "invalid protocol code " << protoId << "\n");
    MF->error();
    return nullptr;
  }

  PrettyStackTraceDecl trace("deserializing default witness table for", proto);

  if (!existingWt)
    existingWt = SILMod.lookUpDefaultWitnessTable(proto, /*deserializeLazily=*/ false);
  auto wT = existingWt;

  // If we have an existing default witness table, verify that the protocol
  // matches up.
  if (wT) {
    if (wT->getProtocol() != proto) {
      LLVM_DEBUG(llvm::dbgs() << "Protocol mismatch.\n");
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

  std::vector<SILWitnessTable::Entry> witnessEntries;
  std::vector<SILWitnessTable::ConditionalConformance> conditionalConformances;
  readWitnessTableEntries(entry, witnessEntries, conditionalConformances);

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
  auto iter = DefaultWitnessTableList->find(existingWt->getUniqueName());
  if (iter == DefaultWitnessTableList->end())
    return nullptr;

  // Attempt to read the default witness table.
  auto Wt = readDefaultWitnessTable(*iter, existingWt);
  if (Wt)
    LLVM_DEBUG(llvm::dbgs() << "Deserialize SIL:\n"; Wt->dump());

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
