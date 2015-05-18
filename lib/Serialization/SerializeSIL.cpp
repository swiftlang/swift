//===--- SerializeSIL.cpp - Read and write SIL ----------------------------===//
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

#define DEBUG_TYPE "sil-serialize"
#include "SILFormat.h"
#include "Serialization.h"
#include "swift/AST/Module.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILModule.h"
#include "swift/SIL/SILUndef.h"

#include "llvm/ADT/MapVector.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/EndianStream.h"
#include "llvm/Support/OnDiskHashTable.h"

using namespace swift;
using namespace swift::serialization;
using namespace swift::serialization::sil_block;
using namespace llvm::support;
using llvm::BCBlockRAII;

static unsigned toStableStringEncoding(StringLiteralInst::Encoding encoding) {
  switch (encoding) {
  case StringLiteralInst::Encoding::UTF8: return SIL_UTF8;
  case StringLiteralInst::Encoding::UTF16: return SIL_UTF16;
  }
  llvm_unreachable("bad string encoding");
}

static unsigned toStableSILLinkage(SILLinkage linkage) {
  switch (linkage) {
  case SILLinkage::Public: return SIL_LINKAGE_PUBLIC;
  case SILLinkage::Hidden: return SIL_LINKAGE_HIDDEN;
  case SILLinkage::Shared: return SIL_LINKAGE_SHARED;
  case SILLinkage::Private: return SIL_LINKAGE_PRIVATE;
  case SILLinkage::PublicExternal: return SIL_LINKAGE_PUBLIC_EXTERNAL;
  case SILLinkage::HiddenExternal: return SIL_LINKAGE_HIDDEN_EXTERNAL;
  case SILLinkage::SharedExternal: return SIL_LINKAGE_SHARED_EXTERNAL;
  case SILLinkage::PrivateExternal: return SIL_LINKAGE_PRIVATE_EXTERNAL;
  }
  llvm_unreachable("bad linkage");
}

static unsigned toStableCastConsumptionKind(CastConsumptionKind kind) {
  switch (kind) {
  case CastConsumptionKind::TakeAlways:
    return SIL_CAST_CONSUMPTION_TAKE_ALWAYS;
  case CastConsumptionKind::TakeOnSuccess:
    return SIL_CAST_CONSUMPTION_TAKE_ON_SUCCESS;
  case CastConsumptionKind::CopyOnSuccess:
    return SIL_CAST_CONSUMPTION_COPY_ON_SUCCESS;
  }
  llvm_unreachable("bad cast consumption kind");
}

namespace {
    /// Used to serialize the on-disk func hash table.
  class FuncTableInfo {
  public:
    using key_type = Identifier;
    using key_type_ref = key_type;
    using data_type = DeclID;
    using data_type_ref = const data_type &;
    using hash_value_type = uint32_t;
    using offset_type = unsigned;

    hash_value_type ComputeHash(key_type_ref key) {
      assert(!key.empty());
      return llvm::HashString(key.str());
    }

    std::pair<unsigned, unsigned> EmitKeyDataLength(raw_ostream &out,
                                                    key_type_ref key,
                                                    data_type_ref data) {
      uint32_t keyLength = key.str().size();
      uint32_t dataLength = sizeof(DeclID);
      endian::Writer<little> writer(out);
      writer.write<uint16_t>(keyLength);
      writer.write<uint16_t>(dataLength);
      return { keyLength, dataLength };
    }

    void EmitKey(raw_ostream &out, key_type_ref key, unsigned len) {
      out << key.str();
    }

    void EmitData(raw_ostream &out, key_type_ref key, data_type_ref data,
                  unsigned len) {
      static_assert(sizeof(DeclID) <= 32, "DeclID too large");
      endian::Writer<little>(out).write<uint32_t>(data);
    }
  };

  class SILSerializer {
    Serializer &S;
    ASTContext &Ctx;

    llvm::BitstreamWriter &Out;

    /// A reusable buffer for emitting records.
    SmallVector<uint64_t, 64> ScratchRecord;

    /// In case we want to encode the relative of InstID vs ValueID.
    ValueID InstID = 0;

    llvm::DenseMap<const ValueBase*, ValueID> ValueIDs;
    ValueID addValueRef(SILValue SV) {
      return addValueRef(SV.getDef());
    }
    ValueID addValueRef(const ValueBase *Val);

  public:
    using TableData = FuncTableInfo::data_type;
    using Table = llvm::MapVector<FuncTableInfo::key_type, TableData>;
  private:
    /// FuncTable maps function name to an ID.
    Table FuncTable;
    std::vector<BitOffset> Funcs;
    /// The current function ID.
    DeclID FuncID = 1;

    /// Maps class name to a VTable ID.
    Table VTableList;
    /// Holds the list of VTables.
    std::vector<BitOffset> VTableOffset;
    DeclID VTableID = 1;

    /// Maps global variable name to an ID.
    Table GlobalVarList;
    /// Holds the list of SIL global variables.
    std::vector<BitOffset> GlobalVarOffset;
    DeclID GlobalVarID = 1;

    /// Maps witness table identifier to an ID.
    Table WitnessTableList;
    /// Holds the list of WitnessTables.
    std::vector<BitOffset> WitnessTableOffset;
    DeclID WitnessTableID = 1;

    /// Give each SILBasicBlock a unique ID.
    llvm::DenseMap<const SILBasicBlock*, unsigned> BasicBlockMap;

    /// Functions that we've emitted a reference to.
    llvm::SmallSet<const SILFunction *, 16> FuncsToDeclare;

    std::array<unsigned, 256> SILAbbrCodes;
    template <typename Layout>
    void registerSILAbbr() {
      using AbbrArrayTy = decltype(SILAbbrCodes);
      static_assert(Layout::Code <= std::tuple_size<AbbrArrayTy>::value,
                    "layout has invalid record code");
      SILAbbrCodes[Layout::Code] = Layout::emitAbbrev(Out);
      DEBUG(llvm::dbgs() << "SIL abbre code " << SILAbbrCodes[Layout::Code]
                         << " for layout " << Layout::Code << "\n");
    }

    // TODO: this is not required anymore. Remove it.
    bool ShouldSerializeAll;

    /// Helper function to update ListOfValues for MethodInst. Format:
    /// Attr, SILDeclRef (DeclID, Kind, uncurryLevel, IsObjC), and an operand.
    void handleMethodInst(const MethodInst *MI, SILValue operand,
                          SmallVectorImpl<ValueID> &ListOfValues);

    void writeSILFunction(const SILFunction &F, bool DeclOnly = false);
    void writeSILBasicBlock(const SILBasicBlock &BB);
    void writeSILInstruction(const SILInstruction &SI);
    void writeSILVTable(const SILVTable &vt);
    void writeSILGlobalVar(const SILGlobalVariable &g);
    void writeSILWitnessTable(const SILWitnessTable &wt);

    void writeSILBlock(const SILModule *SILMod);
    void writeIndexTables();

    void writeConversionLikeInstruction(const SILInstruction *I);
    void writeOneTypeLayout(ValueKind valueKind, SILType type);
    void writeOneTypeOneOperandLayout(ValueKind valueKind,
                                      unsigned attrs,
                                      SILType type,
                                      SILValue operand);
    void writeOneTypeOneOperandLayout(ValueKind valueKind,
                                      unsigned attrs,
                                      CanType type,
                                      SILValue operand);
    void writeOneOperandLayout(ValueKind valueKind,
                               unsigned attrs,
                               SILValue operand);

    /// Helper function to determine if given the current state of the
    /// deserialization if the function body for F should be deserialized.
    bool shouldEmitFunctionBody(const SILFunction &F);

  public:
    SILSerializer(Serializer &S, ASTContext &Ctx,
                  llvm::BitstreamWriter &Out, bool serializeAll)
      : S(S), Ctx(Ctx), Out(Out), ShouldSerializeAll(serializeAll) {}

    void writeSILModule(const SILModule *SILMod);
  };
} // end anonymous namespace

/// We enumerate all values in a SILFunction beforehand to correctly
/// handle forward references of values.
ValueID SILSerializer::addValueRef(const ValueBase *Val) {
  if (!Val || isa<SILUndef>(Val))
    return 0;

  ValueID id = ValueIDs[Val];
  assert(id != 0 && "We should have assigned a value ID to each value.");
  return id;
}

void SILSerializer::writeSILFunction(const SILFunction &F, bool DeclOnly) {
  ValueIDs.clear();
  InstID = 0;

  FuncTable[Ctx.getIdentifier(F.getName())] = FuncID++;
  Funcs.push_back(Out.GetCurrentBitNo());
  unsigned abbrCode = SILAbbrCodes[SILFunctionLayout::Code];
  TypeID FnID = S.addTypeRef(F.getLoweredType().getSwiftType());
  DEBUG(llvm::dbgs() << "SILFunction " << F.getName() << " @ BitNo "
                     << Out.GetCurrentBitNo() << " abbrCode " << abbrCode
                     << " FnID " << FnID << "\n");
  DEBUG(llvm::dbgs() << "Serialized SIL:\n"; F.dump());

  IdentifierID SemanticsID =
    F.getSemanticsAttr().empty() ? (IdentifierID)0 :
    S.addIdentifierRef(Ctx.getIdentifier(F.getSemanticsAttr()));

  SILLinkage Linkage = F.getLinkage();

  // We serialize shared_external linkage as shared since:
  //
  // 1. shared_external linkage is just a hack to tell the optimizer that a
  // shared function was deserialized.
  //
  // 2. We can not just serialize a declaration to a shared_external function
  // since shared_external functions still have linkonce_odr linkage at the LLVM
  // level. This means they must be defined not just declared.
  //
  // TODO: When serialization is reworked, this should be removed.
  if (hasSharedVisibility(Linkage))
    Linkage = SILLinkage::Shared;

  SILFunctionLayout::emitRecord(
      Out, ScratchRecord, abbrCode, toStableSILLinkage(Linkage),
      (unsigned)F.isTransparent(), (unsigned)F.isFragile(),
      (unsigned)F.isThunk(), (unsigned)F.isGlobalInit(),
      (unsigned)F.getInlineStrategy(), (unsigned)F.getEffectsKind(),
      FnID, SemanticsID);

  if (DeclOnly || isAvailableExternally(Linkage) || F.isExternalDeclaration())
    return;

  // Write the body's context archetypes, unless we don't actually have a body.
  if (!F.isExternalDeclaration()) {
    if (auto gp = F.getContextGenericParams()) {
      // To help deserializing the context generic params, we serialize the
      // outer-most list first. In most cases, we do not have decls associated
      // with these parameter lists, so serialize the lists directly.
      std::vector<GenericParamList *> paramLists;
      for (; gp; gp = gp->getOuterParameters())
        paramLists.push_back(gp);
      for (unsigned i = 0, e = paramLists.size(); i < e; i++)
        S.writeGenericParams(paramLists.rbegin()[i], SILAbbrCodes);
    }
  }

  // Assign a unique ID to each basic block of the SILFunction.
  unsigned BasicID = 0;
  BasicBlockMap.clear();
  // Assign a value ID to each SILInstruction that has value and to each basic
  // block argument.
  unsigned ValueID = 0;
  for (const SILBasicBlock &BB : F) {
    BasicBlockMap.insert(std::make_pair(&BB, BasicID++));

    for (auto I = BB.bbarg_begin(), E = BB.bbarg_end(); I != E; ++I)
      ValueIDs[static_cast<const ValueBase*>(*I)] = ++ValueID;

    for (const SILInstruction &SI : BB)
      if (SI.hasValue())
        ValueIDs[&SI] = ++ValueID;
  }

  for (const SILBasicBlock &BB : F)
    writeSILBasicBlock(BB);
}

void SILSerializer::writeSILBasicBlock(const SILBasicBlock &BB) {
  SmallVector<DeclID, 4> Args;
  for (auto I = BB.bbarg_begin(), E = BB.bbarg_end(); I != E; ++I) {
    SILArgument *SA = *I;
    DeclID tId = S.addTypeRef(SA->getType().getSwiftRValueType());
    DeclID vId = addValueRef(static_cast<const ValueBase*>(SA));
    Args.push_back(tId);
    Args.push_back((unsigned)SA->getType().getCategory());
    Args.push_back(vId);
  }

  unsigned abbrCode = SILAbbrCodes[SILBasicBlockLayout::Code];
  SILBasicBlockLayout::emitRecord(Out, ScratchRecord, abbrCode, Args);

  for (const SILInstruction &SI : BB)
    writeSILInstruction(SI);
}

/// Add SILDeclRef to ListOfValues, so we can reconstruct it at
/// deserialization.
static void handleSILDeclRef(Serializer &S, const SILDeclRef &Ref,
                             SmallVectorImpl<ValueID> &ListOfValues) {
  ListOfValues.push_back(S.addDeclRef(Ref.getDecl()));
  ListOfValues.push_back((unsigned)Ref.kind);
  ListOfValues.push_back((unsigned)Ref.getResilienceExpansion());
  ListOfValues.push_back(Ref.uncurryLevel);
  ListOfValues.push_back(Ref.isForeign);
}

/// Helper function to update ListOfValues for MethodInst. Format:
/// Attr, SILDeclRef (DeclID, Kind, uncurryLevel, IsObjC), and an operand.
void SILSerializer::handleMethodInst(const MethodInst *MI,
                                     SILValue operand,
                                     SmallVectorImpl<ValueID> &ListOfValues) {
  ListOfValues.push_back(MI->isVolatile());
  handleSILDeclRef(S, MI->getMember(), ListOfValues);
  ListOfValues.push_back(
      S.addTypeRef(operand.getType().getSwiftRValueType()));
  ListOfValues.push_back((unsigned)operand.getType().getCategory());
  ListOfValues.push_back(addValueRef(operand));
  ListOfValues.push_back(operand.getResultNumber());
}

void SILSerializer::writeOneTypeLayout(ValueKind valueKind,
                                       SILType type) {
  unsigned abbrCode = SILAbbrCodes[SILOneTypeLayout::Code];
  SILOneTypeLayout::emitRecord(Out, ScratchRecord, abbrCode,
        (unsigned) valueKind,
        S.addTypeRef(type.getSwiftRValueType()),
        (unsigned)type.getCategory());
}

void SILSerializer::writeOneOperandLayout(ValueKind valueKind,
                                          unsigned attrs,
                                          SILValue operand) {

  auto operandType = operand.getType();
  auto operandTypeRef = S.addTypeRef(operandType.getSwiftRValueType());
  auto operandRef = addValueRef(operand);

  SILOneOperandLayout::emitRecord(Out, ScratchRecord,
        SILAbbrCodes[SILOneOperandLayout::Code],
        unsigned(valueKind), attrs,
        operandTypeRef, unsigned(operandType.getCategory()),
        operandRef, operand.getResultNumber());
}

void SILSerializer::writeOneTypeOneOperandLayout(ValueKind valueKind,
                                                 unsigned attrs,
                                                 SILType type,
                                                 SILValue operand) {
  auto typeRef = S.addTypeRef(type.getSwiftRValueType());
  auto operandType = operand.getType();
  auto operandTypeRef = S.addTypeRef(operandType.getSwiftRValueType());
  auto operandRef = addValueRef(operand);

  SILOneTypeOneOperandLayout::emitRecord(Out, ScratchRecord,
        SILAbbrCodes[SILOneTypeOneOperandLayout::Code],
        unsigned(valueKind), attrs,
        typeRef, unsigned(type.getCategory()),
        operandTypeRef, unsigned(operandType.getCategory()),
        operandRef, operand.getResultNumber());
}
void SILSerializer::writeOneTypeOneOperandLayout(ValueKind valueKind,
                                                 unsigned attrs,
                                                 CanType type,
                                                 SILValue operand) {
  auto typeRef = S.addTypeRef(type);
  auto operandType = operand.getType();
  auto operandTypeRef = S.addTypeRef(operandType.getSwiftRValueType());
  auto operandRef = addValueRef(operand);

  SILOneTypeOneOperandLayout::emitRecord(Out, ScratchRecord,
        SILAbbrCodes[SILOneTypeOneOperandLayout::Code],
        unsigned(valueKind), attrs,
        typeRef, 0,
        operandTypeRef, unsigned(operandType.getCategory()),
        operandRef, operand.getResultNumber());
}

/// Write an instruction that looks exactly like a conversion: all
/// important information is encoded in the operand and the result type.
void SILSerializer::writeConversionLikeInstruction(const SILInstruction *I) {
  assert(I->getNumOperands() == 1);
  assert(I->getNumTypes() == 1);
  writeOneTypeOneOperandLayout(I->getKind(), 0, I->getType(0),
                               I->getOperand(0));
}

void SILSerializer::writeSILInstruction(const SILInstruction &SI) {
  switch (SI.getKind()) {
  case ValueKind::SILArgument:
  case ValueKind::SILUndef:
    llvm_unreachable("not an instruction");

  case ValueKind::UnreachableInst: {
    unsigned abbrCode = SILAbbrCodes[SILInstNoOperandLayout::Code];
    SILInstNoOperandLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                       (unsigned)SI.getKind());
    break;
  }
  case ValueKind::AllocExistentialBoxInst:
  case ValueKind::InitExistentialAddrInst:
  case ValueKind::InitExistentialMetatypeInst:
  case ValueKind::InitExistentialRefInst: {
    SILValue operand;
    SILType Ty;
    CanType FormalConcreteType;
    ArrayRef<ProtocolConformance*> conformances;

    switch (SI.getKind()) {
    default: llvm_unreachable("out of sync with parent");
    case ValueKind::InitExistentialAddrInst: {
      auto &IEI = cast<InitExistentialAddrInst>(SI);
      operand = IEI.getOperand();
      Ty = IEI.getLoweredConcreteType();
      FormalConcreteType = IEI.getFormalConcreteType();
      conformances = IEI.getConformances();
      break;
    }
    case ValueKind::InitExistentialRefInst: {
      auto &IERI = cast<InitExistentialRefInst>(SI);
      operand = IERI.getOperand();
      Ty = IERI.getType();
      FormalConcreteType = IERI.getFormalConcreteType();
      conformances = IERI.getConformances();
      break;
    }
    case ValueKind::InitExistentialMetatypeInst: {
      auto &IEMI = cast<InitExistentialMetatypeInst>(SI);
      operand = IEMI.getOperand();
      Ty = IEMI.getType();
      conformances = IEMI.getConformances();
      break;
    }
    case ValueKind::AllocExistentialBoxInst: {
      auto &AEBI = cast<AllocExistentialBoxInst>(SI);
      Ty = AEBI.getExistentialType();
      FormalConcreteType = AEBI.getFormalConcreteType();
      conformances = AEBI.getConformances();
      break;
    }
    }

    TypeID operandType = 0;
    SILValueCategory operandCategory = SILValueCategory::Object;
    ValueID operandID = 0;
    if (operand) {
      operandType = S.addTypeRef(operand.getType().getSwiftRValueType());
      operandCategory = operand.getType().getCategory();
      operandID = addValueRef(operand);
    }

    unsigned abbrCode = SILAbbrCodes[SILInitExistentialLayout::Code];
    SILInitExistentialLayout::emitRecord(Out, ScratchRecord, abbrCode,
       (unsigned)SI.getKind(),
       S.addTypeRef(Ty.getSwiftRValueType()),
       (unsigned)Ty.getCategory(),
       operandType,
       (unsigned)operandCategory,
       operandID,
       operand.getResultNumber(),
       S.addTypeRef(FormalConcreteType),
       conformances.size());

    for (auto conformance : conformances) {
      S.writeConformance(conformance, SILAbbrCodes);
    }
    break;
  }
  case ValueKind::DeallocValueBufferInst: {
    auto DVBI = cast<DeallocValueBufferInst>(&SI);
    writeOneTypeOneOperandLayout(DVBI->getKind(), 0,
                                 DVBI->getValueType(),
                                 DVBI->getOperand());
    break;
  }
  case ValueKind::DeallocBoxInst: {
    auto DBI = cast<DeallocBoxInst>(&SI);
    writeOneTypeOneOperandLayout(DBI->getKind(), 0,
                                 DBI->getElementType(),
                                 DBI->getOperand());
    break;
  }
  case ValueKind::DeallocExistentialBoxInst: {
    auto DBI = cast<DeallocExistentialBoxInst>(&SI);
    writeOneTypeOneOperandLayout(DBI->getKind(), 0,
                                 DBI->getConcreteType(),
                                 DBI->getOperand());
    break;
  }
  case ValueKind::ValueMetatypeInst: {
    auto VMI = cast<ValueMetatypeInst>(&SI);
    writeOneTypeOneOperandLayout(VMI->getKind(), 0,
                                 VMI->getType(),
                                 VMI->getOperand());
    break;
  }
  case ValueKind::ExistentialMetatypeInst: {
    auto EMI = cast<ExistentialMetatypeInst>(&SI);
    writeOneTypeOneOperandLayout(EMI->getKind(), 0,
                                 EMI->getType(),
                                 EMI->getOperand());
    break;
  }
  case ValueKind::AllocValueBufferInst: {
    auto AVBI = cast<AllocValueBufferInst>(&SI);
    writeOneTypeOneOperandLayout(AVBI->getKind(), 0,
                                 AVBI->getValueType(),
                                 AVBI->getOperand());
    break;
  }
  case ValueKind::AllocBoxInst: {
    const AllocBoxInst *ABI = cast<AllocBoxInst>(&SI);
    writeOneTypeLayout(ABI->getKind(), ABI->getElementType());
    break;
  }
  case ValueKind::AllocRefInst: {
    const AllocRefInst *ARI = cast<AllocRefInst>(&SI);
    unsigned abbrCode = SILAbbrCodes[SILOneTypeValuesLayout::Code];
    ValueID Args[1] = { ARI->isObjC() };
    SILOneTypeValuesLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                       (unsigned)SI.getKind(),
                                       S.addTypeRef(
                                         ARI->getType().getSwiftRValueType()),
                                       (unsigned)ARI->getType().getCategory(),
                                       llvm::makeArrayRef(Args));
    break;
  }
  case ValueKind::AllocRefDynamicInst: {
    const AllocRefDynamicInst* ARD = cast<AllocRefDynamicInst>(&SI);
    unsigned flags = 0;
    if (ARD->isObjC())
      flags = 1;
    writeOneTypeOneOperandLayout(SI.getKind(), flags,
                                 ARD->getType(), ARD->getOperand());
    break;
  }
  case ValueKind::AllocStackInst: {
    const AllocStackInst *ASI = cast<AllocStackInst>(&SI);
    writeOneTypeLayout(ASI->getKind(), ASI->getElementType());
    break;
  }
  case ValueKind::ProjectValueBufferInst: {
    auto PVBI = cast<ProjectValueBufferInst>(&SI);
    writeOneTypeOneOperandLayout(PVBI->getKind(), 0,
                                 PVBI->getValueType(),
                                 PVBI->getOperand());
    break;
  }
  case ValueKind::BuiltinInst: {
    // Format: number of substitutions, the builtin name, result type, and
    // a list of values for the arguments. Each value in the list
    // is represented with 4 IDs:
    //   ValueID, ValueResultNumber, TypeID, TypeCategory.
    // The record is followed by the substitution list.
    const BuiltinInst *BI = cast<BuiltinInst>(&SI);
    SmallVector<ValueID, 4> Args;
    for (auto Arg : BI->getArguments()) {
      Args.push_back(addValueRef(Arg));
      Args.push_back(Arg.getResultNumber());
      Args.push_back(S.addTypeRef(Arg.getType().getSwiftRValueType()));
      Args.push_back((unsigned)Arg.getType().getCategory());
    }
    SILInstApplyLayout::emitRecord(Out, ScratchRecord,
                             SILAbbrCodes[SILInstApplyLayout::Code],
                             2 /*Builtin*/,
                             BI->getSubstitutions().size(),
                             S.addTypeRef(BI->getType().getSwiftRValueType()),
                             (unsigned)BI->getType().getCategory(),
                             S.addIdentifierRef(BI->getName()),
                             0,
                             Args);
    S.writeSubstitutions(BI->getSubstitutions(), SILAbbrCodes);
    break;
  }
  case ValueKind::ApplyInst: {
    // Format: attributes such as transparent and number of substitutions,
    // the callee's substituted and unsubstituted types, a value for
    // the callee and a list of values for the arguments. Each value in the list
    // is represented with 2 IDs: ValueID and ValueResultNumber. The record
    // is followed by the substitution list.
    const ApplyInst *AI = cast<ApplyInst>(&SI);
    SmallVector<ValueID, 4> Args;
    for (auto Arg: AI->getArguments()) {
      Args.push_back(addValueRef(Arg));
      Args.push_back(Arg.getResultNumber());
    }
    SILInstApplyLayout::emitRecord(Out, ScratchRecord,
        SILAbbrCodes[SILInstApplyLayout::Code], 0/*Apply*/,
        AI->getSubstitutions().size(),
        S.addTypeRef(AI->getCallee().getType().getSwiftRValueType()),
        S.addTypeRef(AI->getSubstCalleeType()),
        addValueRef(AI->getCallee()), AI->getCallee().getResultNumber(),
        Args);
    S.writeSubstitutions(AI->getSubstitutions(), SILAbbrCodes);
    break;
  }
  case ValueKind::TryApplyInst: {
    // Format: attributes such as transparent and number of substitutions,
    // the callee's substituted and unsubstituted types, a value for
    // the callee and a list of values for the arguments. Each value in the list
    // is represented with 2 IDs: ValueID and ValueResultNumber. The final two
    // entries in the list are the basic block destinations. The record
    // is followed by the substitution list.
    const TryApplyInst *AI = cast<TryApplyInst>(&SI);
    SmallVector<ValueID, 4> Args;
    for (auto Arg: AI->getArguments()) {
      Args.push_back(addValueRef(Arg));
      Args.push_back(Arg.getResultNumber());
    }
    Args.push_back(BasicBlockMap[AI->getNormalBB()]);
    Args.push_back(BasicBlockMap[AI->getErrorBB()]);
    SILInstApplyLayout::emitRecord(Out, ScratchRecord,
        SILAbbrCodes[SILInstApplyLayout::Code], 3/*TryApply*/,
        AI->getSubstitutions().size(),
        S.addTypeRef(AI->getCallee().getType().getSwiftRValueType()),
        S.addTypeRef(AI->getSubstCalleeType()),
        addValueRef(AI->getCallee()), AI->getCallee().getResultNumber(),
        Args);
    S.writeSubstitutions(AI->getSubstitutions(), SILAbbrCodes);
    break;
  }
  case ValueKind::PartialApplyInst: {
    const PartialApplyInst *PAI = cast<PartialApplyInst>(&SI);
        SmallVector<ValueID, 4> Args;
    for (auto Arg: PAI->getArguments()) {
      Args.push_back(addValueRef(Arg));
      Args.push_back(Arg.getResultNumber());
    }
    SILInstApplyLayout::emitRecord(Out, ScratchRecord,
        SILAbbrCodes[SILInstApplyLayout::Code], 1/*PartialApply*/,
        PAI->getSubstitutions().size(),
        S.addTypeRef(PAI->getCallee().getType().getSwiftRValueType()),
        S.addTypeRef(PAI->getSubstCalleeType()),
        addValueRef(PAI->getCallee()), PAI->getCallee().getResultNumber(),
        Args);
    S.writeSubstitutions(PAI->getSubstitutions(), SILAbbrCodes);
    break;
  }
  case ValueKind::GlobalAddrInst: {
    // Format: Name and type. Use SILOneOperandLayout.
    const GlobalAddrInst *GAI = cast<GlobalAddrInst>(&SI);
    SILOneOperandLayout::emitRecord(Out, ScratchRecord,
        SILAbbrCodes[SILOneOperandLayout::Code],
        (unsigned)SI.getKind(), 0,
        S.addTypeRef(GAI->getType().getSwiftRValueType()),
        (unsigned)GAI->getType().getCategory(),
        S.addIdentifierRef(
            Ctx.getIdentifier(GAI->getReferencedGlobal()->getName())),
        0);
    break;
  }
  case ValueKind::BranchInst: {
    // Format: destination basic block ID, a list of arguments. Use
    // SILOneTypeValuesLayout.
    const BranchInst *BrI = cast<BranchInst>(&SI);
    SmallVector<ValueID, 4> ListOfValues;
    for (auto Elt : BrI->getArgs()) {
      ListOfValues.push_back(S.addTypeRef(Elt.getType().getSwiftRValueType()));
      ListOfValues.push_back((unsigned)Elt.getType().getCategory());
      ListOfValues.push_back(addValueRef(Elt));
      ListOfValues.push_back(Elt.getResultNumber());
    }

    SILOneTypeValuesLayout::emitRecord(Out, ScratchRecord,
        SILAbbrCodes[SILOneTypeValuesLayout::Code],
        (unsigned)SI.getKind(),
        BasicBlockMap[BrI->getDestBB()], 0, ListOfValues);
    break;
  }
  case ValueKind::CondBranchInst: {
    // Format: condition, true basic block ID, a list of arguments, false basic
    // block ID, a list of arguments. Use SILOneTypeValuesLayout: the type is
    // for condition, the list has value for condition, true basic block ID,
    // false basic block ID, number of true arguments, and a list of true|false
    // arguments.
    const CondBranchInst *CBI = cast<CondBranchInst>(&SI);
    SmallVector<ValueID, 4> ListOfValues;
    ListOfValues.push_back(addValueRef(CBI->getCondition()));
    ListOfValues.push_back(CBI->getCondition().getResultNumber());
    ListOfValues.push_back(BasicBlockMap[CBI->getTrueBB()]);
    ListOfValues.push_back(BasicBlockMap[CBI->getFalseBB()]);
    ListOfValues.push_back(CBI->getTrueArgs().size());
    for (auto Elt : CBI->getTrueArgs()) {
      ListOfValues.push_back(S.addTypeRef(Elt.getType().getSwiftRValueType()));
      ListOfValues.push_back((unsigned)Elt.getType().getCategory());
      ListOfValues.push_back(addValueRef(Elt));
      ListOfValues.push_back(Elt.getResultNumber());
    }
    for (auto Elt : CBI->getFalseArgs()) {
      ListOfValues.push_back(S.addTypeRef(Elt.getType().getSwiftRValueType()));
      ListOfValues.push_back((unsigned)Elt.getType().getCategory());
      ListOfValues.push_back(addValueRef(Elt));
      ListOfValues.push_back(Elt.getResultNumber());
    }

    SILOneTypeValuesLayout::emitRecord(Out, ScratchRecord,
        SILAbbrCodes[SILOneTypeValuesLayout::Code],
        (unsigned)SI.getKind(),
        S.addTypeRef(CBI->getCondition().getType().getSwiftRValueType()),
        (unsigned)CBI->getCondition().getType().getCategory(),
        ListOfValues);
    break;
  }
  case ValueKind::SwitchEnumInst:
  case ValueKind::SwitchEnumAddrInst: {
    // Format: condition, a list of cases (EnumElementDecl + Basic Block ID),
    // default basic block ID. Use SILOneTypeValuesLayout: the type is
    // for condition, the list has value for condition, hasDefault, default
    // basic block ID, a list of (DeclID, BasicBlock ID).
    const SwitchEnumInstBase *SOI = cast<SwitchEnumInstBase>(&SI);
    SmallVector<ValueID, 4> ListOfValues;
    ListOfValues.push_back(addValueRef(SOI->getOperand()));
    ListOfValues.push_back(SOI->getOperand().getResultNumber());
    ListOfValues.push_back((unsigned)SOI->hasDefault());
    if (SOI->hasDefault())
      ListOfValues.push_back(BasicBlockMap[SOI->getDefaultBB()]);
    else
      ListOfValues.push_back(0);

    for (unsigned i = 0, e = SOI->getNumCases(); i < e; ++i) {
      EnumElementDecl *elt;
      SILBasicBlock *dest;
      std::tie(elt, dest) = SOI->getCase(i);
      ListOfValues.push_back(S.addDeclRef(elt));
      ListOfValues.push_back(BasicBlockMap[dest]);
    }
    SILOneTypeValuesLayout::emitRecord(Out, ScratchRecord,
        SILAbbrCodes[SILOneTypeValuesLayout::Code],
        (unsigned)SI.getKind(),
        S.addTypeRef(SOI->getOperand().getType().getSwiftRValueType()),
        (unsigned)SOI->getOperand().getType().getCategory(),
        ListOfValues);
    break;
  }
  case ValueKind::SelectEnumInst:
  case ValueKind::SelectEnumAddrInst: {
    // Format: condition, a list of cases (EnumElementDecl + Value ID),
    // default value ID. Use SILOneTypeValuesLayout: the type is
    // for condition, the list has value for condition, result type,
    //   hasDefault, default
    // basic block ID, a list of (DeclID, BasicBlock ID).
    const SelectEnumInstBase *SOI = cast<SelectEnumInstBase>(&SI);
    SmallVector<ValueID, 4> ListOfValues;
    ListOfValues.push_back(addValueRef(SOI->getEnumOperand()));
    ListOfValues.push_back(SOI->getEnumOperand().getResultNumber());
    ListOfValues.push_back(S.addTypeRef(SOI->getType().getSwiftRValueType()));
    ListOfValues.push_back((unsigned)SOI->getType().getCategory());
    ListOfValues.push_back((unsigned)SOI->hasDefault());
    if (SOI->hasDefault()) {
      ListOfValues.push_back(addValueRef(SOI->getDefaultResult()));
      ListOfValues.push_back(SOI->getDefaultResult().getResultNumber());
    } else {
      ListOfValues.push_back(0);
      ListOfValues.push_back(0);
    }
    for (unsigned i = 0, e = SOI->getNumCases(); i < e; ++i) {
      EnumElementDecl *elt;
      SILValue result;
      std::tie(elt, result) = SOI->getCase(i);
      ListOfValues.push_back(S.addDeclRef(elt));
      ListOfValues.push_back(addValueRef(result));
      ListOfValues.push_back(result.getResultNumber());
    }
    SILOneTypeValuesLayout::emitRecord(Out, ScratchRecord,
        SILAbbrCodes[SILOneTypeValuesLayout::Code],
        (unsigned)SI.getKind(),
        S.addTypeRef(SOI->getEnumOperand().getType().getSwiftRValueType()),
        (unsigned)SOI->getEnumOperand().getType().getCategory(),
        ListOfValues);
    break;
  }
  case ValueKind::SwitchValueInst: {
    // Format: condition, a list of cases (Value ID + Basic Block ID),
    // default basic block ID. Use SILOneTypeValuesLayout: the type is
    // for condition, the list contains value for condition, hasDefault, default
    // basic block ID, a list of (Value ID, BasicBlock ID).
    const SwitchValueInst *SII = cast<SwitchValueInst>(&SI);
    SmallVector<ValueID, 4> ListOfValues;
    ListOfValues.push_back(addValueRef(SII->getOperand()));
    ListOfValues.push_back(SII->getOperand().getResultNumber());
    ListOfValues.push_back((unsigned)SII->hasDefault());
    if (SII->hasDefault())
      ListOfValues.push_back(BasicBlockMap[SII->getDefaultBB()]);
    else
      ListOfValues.push_back(0);

    for (unsigned i = 0, e = SII->getNumCases(); i < e; ++i) {
      SILValue value;
      SILBasicBlock *dest;
      std::tie(value, dest) = SII->getCase(i);
      ListOfValues.push_back(addValueRef(value));
      ListOfValues.push_back(value.getResultNumber());
      ListOfValues.push_back(BasicBlockMap[dest]);
    }
    SILOneTypeValuesLayout::emitRecord(Out, ScratchRecord,
        SILAbbrCodes[SILOneTypeValuesLayout::Code],
        (unsigned)SI.getKind(),
        S.addTypeRef(SII->getOperand().getType().getSwiftRValueType()),
        (unsigned)SII->getOperand().getType().getCategory(),
        ListOfValues);
    break;
  }
  case ValueKind::SelectValueInst: {
    // Format: condition, a list of cases (Value ID + Value ID),
    // default value ID. Use SILOneTypeValuesLayout: the type is
    // for condition, the list has value for condition, result type,
    // hasDefault, default
    // basic block ID, a list of (Value ID, Value ID).
    const SelectValueInst *SVI = cast<SelectValueInst>(&SI);
    SmallVector<ValueID, 4> ListOfValues;
    ListOfValues.push_back(addValueRef(SVI->getOperand()));
    ListOfValues.push_back(SVI->getOperand().getResultNumber());
    ListOfValues.push_back(S.addTypeRef(SVI->getType().getSwiftRValueType()));
    ListOfValues.push_back((unsigned)SVI->getType().getCategory());
    ListOfValues.push_back((unsigned)SVI->hasDefault());
    if (SVI->hasDefault()) {
      ListOfValues.push_back(addValueRef(SVI->getDefaultResult()));
      ListOfValues.push_back(SVI->getDefaultResult().getResultNumber());
    } else {
      ListOfValues.push_back(0);
      ListOfValues.push_back(0);
    }
    for (unsigned i = 0, e = SVI->getNumCases(); i < e; ++i) {
      SILValue casevalue;
      SILValue result;
      std::tie(casevalue, result) = SVI->getCase(i);
      ListOfValues.push_back(addValueRef(casevalue));
      ListOfValues.push_back(casevalue.getResultNumber());
      ListOfValues.push_back(addValueRef(result));
      ListOfValues.push_back(result.getResultNumber());
    }
    SILOneTypeValuesLayout::emitRecord(Out, ScratchRecord,
        SILAbbrCodes[SILOneTypeValuesLayout::Code],
        (unsigned)SI.getKind(),
        S.addTypeRef(SVI->getOperand().getType().getSwiftRValueType()),
        (unsigned)SVI->getOperand().getType().getCategory(),
        ListOfValues);
    break;
  }
  case ValueKind::CondFailInst:
  case ValueKind::RetainValueInst:
  case ValueKind::ReleaseValueInst:
  case ValueKind::AutoreleaseValueInst:
  case ValueKind::DeallocStackInst:
  case ValueKind::DeallocRefInst:
  case ValueKind::DeinitExistentialAddrInst:
  case ValueKind::DestroyAddrInst:
  case ValueKind::IsNonnullInst:
  case ValueKind::LoadInst:
  case ValueKind::LoadWeakInst:
  case ValueKind::MarkUninitializedInst:
  case ValueKind::FixLifetimeInst:
  case ValueKind::CopyBlockInst:
  case ValueKind::StrongPinInst:
  case ValueKind::StrongReleaseInst:
  case ValueKind::StrongRetainInst:
  case ValueKind::StrongRetainAutoreleasedInst:
  case ValueKind::StrongUnpinInst:
  case ValueKind::AutoreleaseReturnInst:
  case ValueKind::StrongRetainUnownedInst:
  case ValueKind::UnownedRetainInst:
  case ValueKind::UnownedReleaseInst:
  case ValueKind::IsUniqueInst:
  case ValueKind::IsUniqueOrPinnedInst:
  case ValueKind::ReturnInst:
  case ValueKind::ThrowInst:
  case ValueKind::DebugValueInst:
  case ValueKind::DebugValueAddrInst: {
    unsigned Attr = 0;
    if (auto *LWI = dyn_cast<LoadWeakInst>(&SI))
      Attr = LWI->isTake();
    else if (auto *MUI = dyn_cast<MarkUninitializedInst>(&SI))
      Attr = (unsigned)MUI->getKind();
    writeOneOperandLayout(SI.getKind(), Attr, SI.getOperand(0));
    break;
  }
  case ValueKind::FunctionRefInst: {
    // Use SILOneOperandLayout to specify the function type and the function
    // name (IdentifierID).
    const FunctionRefInst *FRI = cast<FunctionRefInst>(&SI);
    SILFunction *ReferencedFunction = FRI->getReferencedFunction();
    unsigned abbrCode = SILAbbrCodes[SILOneOperandLayout::Code];
    SILOneOperandLayout::emitRecord(Out, ScratchRecord, abbrCode,
        (unsigned)SI.getKind(), 0,
        S.addTypeRef(FRI->getType().getSwiftRValueType()),
        (unsigned)FRI->getType().getCategory(),
        S.addIdentifierRef(Ctx.getIdentifier(ReferencedFunction->getName())),
        0);

    // Make sure we declare the referenced function.
    FuncsToDeclare.insert(ReferencedFunction);
    break;
  }
  case ValueKind::MarkDependenceInst:
  case ValueKind::IndexAddrInst:
  case ValueKind::IndexRawPointerInst: {
    SILValue operand, operand2;
    unsigned Attr = 0;
    if (SI.getKind() == ValueKind::IndexRawPointerInst) {
      const IndexRawPointerInst *IRP = cast<IndexRawPointerInst>(&SI);
      operand = IRP->getBase();
      operand2 = IRP->getIndex();
    } else if (SI.getKind() == ValueKind::MarkDependenceInst) {
      const MarkDependenceInst *MDI = cast<MarkDependenceInst>(&SI);
      operand = MDI->getValue();
      operand2 = MDI->getBase();
    } else {
      const IndexAddrInst *IAI = cast<IndexAddrInst>(&SI);
      operand = IAI->getBase();
      operand2 = IAI->getIndex();
    }
    SILTwoOperandsLayout::emitRecord(Out, ScratchRecord,
        SILAbbrCodes[SILTwoOperandsLayout::Code],
        (unsigned)SI.getKind(), Attr,
        S.addTypeRef(operand.getType().getSwiftRValueType()),
        (unsigned)operand.getType().getCategory(),
        addValueRef(operand), operand.getResultNumber(),
        S.addTypeRef(operand2.getType().getSwiftRValueType()),
        (unsigned)operand2.getType().getCategory(),
        addValueRef(operand2), operand2.getResultNumber());
    break;
  }
  case ValueKind::StringLiteralInst: {
    auto SLI = cast<StringLiteralInst>(&SI);
    StringRef Str = SLI->getValue();
    unsigned abbrCode = SILAbbrCodes[SILOneOperandLayout::Code];
    unsigned encoding = toStableStringEncoding(SLI->getEncoding());
    SILOneOperandLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                    (unsigned)SI.getKind(), encoding, 0, 0,
                                    S.addIdentifierRef(Ctx.getIdentifier(Str)),
                                    0);
    break;
  }
  case ValueKind::FloatLiteralInst:
  case ValueKind::IntegerLiteralInst: {
    // Use SILOneOperandLayout to specify the type and the literal.
    std::string Str;
    SILType Ty;
    switch (SI.getKind()) {
    default: llvm_unreachable("Out of sync with parent switch");
    case ValueKind::IntegerLiteralInst:
      Str = cast<IntegerLiteralInst>(&SI)->getValue().toString(10, true);
      Ty = cast<IntegerLiteralInst>(&SI)->getType();
      break;
    case ValueKind::FloatLiteralInst:
      Str = cast<FloatLiteralInst>(&SI)->getBits().toString(16,
                                                            /*Signed*/false);
      Ty = cast<FloatLiteralInst>(&SI)->getType();
      break;
    }
    unsigned abbrCode = SILAbbrCodes[SILOneOperandLayout::Code];
    SILOneOperandLayout::emitRecord(Out, ScratchRecord, abbrCode,
        (unsigned)SI.getKind(), 0,
        S.addTypeRef(Ty.getSwiftRValueType()),
        (unsigned)Ty.getCategory(),
        S.addIdentifierRef(Ctx.getIdentifier(Str)),
        0);
    break;
  }
  case ValueKind::MarkFunctionEscapeInst: {
    // Format: a list of typed values. A typed value is expressed by 4 IDs:
    // TypeID, TypeCategory, ValueID, ValueResultNumber.
    const MarkFunctionEscapeInst *MFE = cast<MarkFunctionEscapeInst>(&SI);
    SmallVector<ValueID, 4> ListOfValues;
    for (auto Elt : MFE->getElements()) {
      ListOfValues.push_back(S.addTypeRef(Elt.getType().getSwiftRValueType()));
      ListOfValues.push_back((unsigned)Elt.getType().getCategory());
      ListOfValues.push_back(addValueRef(Elt));
      ListOfValues.push_back(Elt.getResultNumber());
    }

    SILOneTypeValuesLayout::emitRecord(Out, ScratchRecord,
        SILAbbrCodes[SILOneTypeValuesLayout::Code],
        (unsigned)SI.getKind(), 0, 0, ListOfValues);
    break;
  }
  case ValueKind::MetatypeInst:
  case ValueKind::NullClassInst:
    writeOneTypeLayout(SI.getKind(), SI.getType(0));
    break;
  case ValueKind::ObjCProtocolInst: {
    const ObjCProtocolInst *PI = cast<ObjCProtocolInst>(&SI);
    unsigned abbrCode = SILAbbrCodes[SILOneOperandLayout::Code];
    SILOneOperandLayout::emitRecord(Out, ScratchRecord, abbrCode,
                              (unsigned)SI.getKind(), 0,
                              S.addTypeRef(PI->getType().getSwiftRValueType()),
                              (unsigned)PI->getType().getCategory(),
                              S.addDeclRef(PI->getProtocol()), 0);
    break;
  }
  // Conversion instructions (and others of similar form).
  case ValueKind::OpenExistentialAddrInst:
  case ValueKind::OpenExistentialRefInst:
  case ValueKind::OpenExistentialMetatypeInst:
  case ValueKind::OpenExistentialBoxInst:
  case ValueKind::UncheckedRefCastInst:
  case ValueKind::UncheckedAddrCastInst:
  case ValueKind::UncheckedTrivialBitCastInst:
  case ValueKind::UncheckedRefBitCastInst:
  case ValueKind::BridgeObjectToRefInst:
  case ValueKind::BridgeObjectToWordInst:
  case ValueKind::UpcastInst:
  case ValueKind::AddressToPointerInst:
  case ValueKind::PointerToAddressInst:
  case ValueKind::RefToRawPointerInst:
  case ValueKind::RawPointerToRefInst:
  case ValueKind::RefToUnownedInst:
  case ValueKind::UnownedToRefInst:
  case ValueKind::RefToUnmanagedInst:
  case ValueKind::UnmanagedToRefInst:
  case ValueKind::ThinToThickFunctionInst:
  case ValueKind::ThickToObjCMetatypeInst:
  case ValueKind::ObjCToThickMetatypeInst:
  case ValueKind::ConvertFunctionInst:
  case ValueKind::ThinFunctionToPointerInst:
  case ValueKind::PointerToThinFunctionInst:
  case ValueKind::ObjCMetatypeToObjectInst:
  case ValueKind::ObjCExistentialMetatypeToObjectInst:
  case ValueKind::ProjectBlockStorageInst: {
    writeConversionLikeInstruction(&SI);
    break;
  }
  case ValueKind::RefToBridgeObjectInst: {
    auto RI = cast<RefToBridgeObjectInst>(&SI);
    SILTwoOperandsLayout::emitRecord(Out, ScratchRecord,
           SILAbbrCodes[SILTwoOperandsLayout::Code], (unsigned)SI.getKind(),
           /*attr*/ 0,
           S.addTypeRef(RI->getConverted().getType().getSwiftRValueType()),
           (unsigned)RI->getConverted().getType().getCategory(),
           addValueRef(RI->getConverted()),
           RI->getConverted().getResultNumber(),
           S.addTypeRef(RI->getBitsOperand().getType().getSwiftRValueType()),
           (unsigned)RI->getBitsOperand().getType().getCategory(),
           addValueRef(RI->getBitsOperand()),
           RI->getBitsOperand().getResultNumber());
    break;
  }
  // Checked Conversion instructions.
  case ValueKind::UnconditionalCheckedCastInst: {
    auto CI = cast<UnconditionalCheckedCastInst>(&SI);
    SILInstCastLayout::emitRecord(Out, ScratchRecord,
        SILAbbrCodes[SILInstCastLayout::Code],
        (unsigned)SI.getKind(), /*attr*/ 0,
        S.addTypeRef(CI->getType().getSwiftRValueType()),
        (unsigned)CI->getType().getCategory(),
        S.addTypeRef(CI->getOperand().getType().getSwiftRValueType()),
        (unsigned)CI->getOperand().getType().getCategory(),
        addValueRef(CI->getOperand()), CI->getOperand().getResultNumber());
    break;
  }
  case ValueKind::UnconditionalCheckedCastAddrInst: {
    auto CI = cast<UnconditionalCheckedCastAddrInst>(&SI);
    ValueID listOfValues[] = {
      toStableCastConsumptionKind(CI->getConsumptionKind()),
      S.addTypeRef(CI->getSourceType()),
      addValueRef(CI->getSrc()),
      CI->getSrc().getResultNumber(),
      S.addTypeRef(CI->getSrc().getType().getSwiftRValueType()),
      (unsigned)CI->getSrc().getType().getCategory(),
      S.addTypeRef(CI->getTargetType()),
      addValueRef(CI->getDest()),
      CI->getDest().getResultNumber()
    };
    SILOneTypeValuesLayout::emitRecord(Out, ScratchRecord,
             SILAbbrCodes[SILOneTypeValuesLayout::Code], (unsigned)SI.getKind(),
             S.addTypeRef(CI->getDest().getType().getSwiftRValueType()),
             (unsigned)CI->getDest().getType().getCategory(),
             llvm::makeArrayRef(listOfValues));
    break;
  }

  case ValueKind::AssignInst:
  case ValueKind::CopyAddrInst:
  case ValueKind::StoreInst:
  case ValueKind::StoreWeakInst: {
    SILValue operand, value;
    unsigned Attr = 0;
    if (SI.getKind() == ValueKind::StoreWeakInst) {
      Attr = cast<StoreWeakInst>(&SI)->isInitializationOfDest();
      operand = cast<StoreWeakInst>(&SI)->getDest();
      value = cast<StoreWeakInst>(&SI)->getSrc();
    } else if (SI.getKind() == ValueKind::StoreInst) {
      operand = cast<StoreInst>(&SI)->getDest();
      value = cast<StoreInst>(&SI)->getSrc();
    } else if (SI.getKind() == ValueKind::AssignInst) {
      operand = cast<AssignInst>(&SI)->getDest();
      value = cast<AssignInst>(&SI)->getSrc();
    } else if (SI.getKind() == ValueKind::CopyAddrInst) {
      const CopyAddrInst *CAI = cast<CopyAddrInst>(&SI);
      Attr = (CAI->isInitializationOfDest() << 1) | CAI->isTakeOfSrc();
      operand = cast<CopyAddrInst>(&SI)->getDest();
      value = cast<CopyAddrInst>(&SI)->getSrc();
    } else
      llvm_unreachable("switch out of sync");

    unsigned abbrCode = SILAbbrCodes[SILOneValueOneOperandLayout::Code];
    SILOneValueOneOperandLayout::emitRecord(Out, ScratchRecord, abbrCode,
                  (unsigned)SI.getKind(), Attr, addValueRef(value),
                  value.getResultNumber(),
                  S.addTypeRef(operand.getType().getSwiftRValueType()),
                  (unsigned)operand.getType().getCategory(),
                  addValueRef(operand),
                  operand.getResultNumber());
    break;
  }
  case ValueKind::RefElementAddrInst:
  case ValueKind::StructElementAddrInst:
  case ValueKind::StructExtractInst:
  case ValueKind::InitEnumDataAddrInst:
  case ValueKind::UncheckedEnumDataInst:
  case ValueKind::UncheckedTakeEnumDataAddrInst:
  case ValueKind::InjectEnumAddrInst: {
    // Has a typed valueref and a field decl. We use SILOneValueOneOperandLayout
    // where the field decl is streamed as a ValueID.
    SILValue operand;
    Decl *tDecl;
    switch (SI.getKind()) {
    default: llvm_unreachable("Out of sync with parent switch");
    case ValueKind::RefElementAddrInst:
      operand = cast<RefElementAddrInst>(&SI)->getOperand();
      tDecl = cast<RefElementAddrInst>(&SI)->getField();
      break;
    case ValueKind::StructElementAddrInst:
      operand = cast<StructElementAddrInst>(&SI)->getOperand();
      tDecl = cast<StructElementAddrInst>(&SI)->getField();
      break;
    case ValueKind::StructExtractInst:
      operand = cast<StructExtractInst>(&SI)->getOperand();
      tDecl = cast<StructExtractInst>(&SI)->getField();
      break;
    case ValueKind::InitEnumDataAddrInst:
      operand = cast<InitEnumDataAddrInst>(&SI)->getOperand();
      tDecl = cast<InitEnumDataAddrInst>(&SI)->getElement();
      break;
    case ValueKind::UncheckedEnumDataInst:
      operand = cast<UncheckedEnumDataInst>(&SI)->getOperand();
      tDecl = cast<UncheckedEnumDataInst>(&SI)->getElement();
      break;
    case ValueKind::UncheckedTakeEnumDataAddrInst:
      operand = cast<UncheckedTakeEnumDataAddrInst>(&SI)->getOperand();
      tDecl = cast<UncheckedTakeEnumDataAddrInst>(&SI)->getElement();
      break;
    case ValueKind::InjectEnumAddrInst:
      operand = cast<InjectEnumAddrInst>(&SI)->getOperand();
      tDecl = cast<InjectEnumAddrInst>(&SI)->getElement();
      break;
    }
    SILOneValueOneOperandLayout::emitRecord(Out, ScratchRecord,
        SILAbbrCodes[SILOneValueOneOperandLayout::Code],
        (unsigned)SI.getKind(), 0, S.addDeclRef(tDecl), 0,
        S.addTypeRef(operand.getType().getSwiftRValueType()),
        (unsigned)operand.getType().getCategory(),
        addValueRef(operand), operand.getResultNumber());
    break;
  }
  case ValueKind::StructInst: {
    // Format: a type followed by a list of typed values. A typed value is
    // expressed by 4 IDs: TypeID, TypeCategory, ValueID, ValueResultNumber.
    const StructInst *StrI = cast<StructInst>(&SI);
    SmallVector<ValueID, 4> ListOfValues;
    for (auto Elt : StrI->getElements()) {
      ListOfValues.push_back(S.addTypeRef(Elt.getType().getSwiftRValueType()));
      ListOfValues.push_back((unsigned)Elt.getType().getCategory());
      ListOfValues.push_back(addValueRef(Elt));
      ListOfValues.push_back(Elt.getResultNumber());
    }

    SILOneTypeValuesLayout::emitRecord(Out, ScratchRecord,
        SILAbbrCodes[SILOneTypeValuesLayout::Code],
        (unsigned)SI.getKind(),
        S.addTypeRef(StrI->getType().getSwiftRValueType()),
        (unsigned)StrI->getType().getCategory(), ListOfValues);
    break;
  }
  case ValueKind::TupleElementAddrInst:
  case ValueKind::TupleExtractInst: {
    SILValue operand;
    unsigned FieldNo;
    switch (SI.getKind()) {
    default: llvm_unreachable("Out of sync with parent switch");
    case ValueKind::TupleElementAddrInst:
      operand = cast<TupleElementAddrInst>(&SI)->getOperand();
      FieldNo = cast<TupleElementAddrInst>(&SI)->getFieldNo();
      break;
    case ValueKind::TupleExtractInst:
      operand = cast<TupleExtractInst>(&SI)->getOperand();
      FieldNo = cast<TupleExtractInst>(&SI)->getFieldNo();
      break;
    }

    // Use OneTypeOneOperand layout where the field number is stored in TypeID.
    SILOneTypeOneOperandLayout::emitRecord(Out, ScratchRecord,
        SILAbbrCodes[SILOneTypeOneOperandLayout::Code],
        (unsigned)SI.getKind(), 0,
        FieldNo, 0,
        S.addTypeRef(operand.getType().getSwiftRValueType()),
        (unsigned)operand.getType().getCategory(),
        addValueRef(operand), operand.getResultNumber());
    break;
  }
  case ValueKind::TupleInst: {
    // Format: a type followed by a list of values. A value is expressed by
    // 2 IDs: ValueID, ValueResultNumber.
    const TupleInst *TI = cast<TupleInst>(&SI);
    SmallVector<ValueID, 4> ListOfValues;
    for (auto Elt : TI->getElements()) {
      ListOfValues.push_back(addValueRef(Elt));
      ListOfValues.push_back(Elt.getResultNumber());
    }

    unsigned abbrCode = SILAbbrCodes[SILOneTypeValuesLayout::Code];
    SILOneTypeValuesLayout::emitRecord(Out, ScratchRecord, abbrCode,
        (unsigned)SI.getKind(),
        S.addTypeRef(TI->getType().getSwiftRValueType()),
        (unsigned)TI->getType().getCategory(),
        ListOfValues);
    break;
  }
  case ValueKind::EnumInst: {
    // Format: a type, an operand and a decl ID. Use SILTwoOperandsLayout: type,
    // (DeclID + hasOperand), and an operand.
    const EnumInst *UI = cast<EnumInst>(&SI);
    TypeID OperandTy = UI->hasOperand() ?
      S.addTypeRef(UI->getOperand().getType().getSwiftRValueType()) : (TypeID)0;
    unsigned OperandTyCategory = UI->hasOperand() ?
        (unsigned)UI->getOperand().getType().getCategory() : 0;
    SILTwoOperandsLayout::emitRecord(Out, ScratchRecord,
        SILAbbrCodes[SILTwoOperandsLayout::Code], (unsigned)SI.getKind(), 0,
        S.addTypeRef(UI->getType().getSwiftRValueType()),
        (unsigned)UI->getType().getCategory(),
        S.addDeclRef(UI->getElement()), UI->hasOperand(),
        OperandTy, OperandTyCategory,
        UI->hasOperand() ? addValueRef(UI->getOperand()) : (ValueID)0,
        UI->hasOperand() ? UI->getOperand().getResultNumber() : 0);
    break;
  }
  case ValueKind::WitnessMethodInst: {
    // Format: a type, an operand and a SILDeclRef. Use SILOneTypeValuesLayout:
    // type, Attr, SILDeclRef (DeclID, Kind, uncurryLevel, IsObjC), and a type.
    const WitnessMethodInst *AMI = cast<WitnessMethodInst>(&SI);
    CanType Ty = AMI->getLookupType();
    SILType Ty2 = AMI->getType(0);

    SmallVector<ValueID, 8> ListOfValues;
    handleSILDeclRef(S, AMI->getMember(), ListOfValues);

    // Add an optional operand.
    TypeID OperandTy =
        AMI->hasOperand()
            ? S.addTypeRef(AMI->getOperand().getType().getSwiftRValueType())
            : (TypeID)0;
    unsigned OperandTyCategory =
        AMI->hasOperand() ? (unsigned)AMI->getOperand().getType().getCategory()
                          : 0;
    SILValue OptionalOpenedExistential =
        AMI->hasOperand() ? AMI->getOperand() : SILValue();
    auto OperandValueId = addValueRef(OptionalOpenedExistential);

    SILInstWitnessMethodLayout::emitRecord(
        Out, ScratchRecord, SILAbbrCodes[SILInstWitnessMethodLayout::Code],
        S.addTypeRef(Ty), 0, AMI->isVolatile(),
        S.addTypeRef(Ty2.getSwiftRValueType()), (unsigned)Ty2.getCategory(),
        OperandTy, OperandTyCategory, OperandValueId, ListOfValues);

    S.writeConformance(AMI->getConformance(), SILAbbrCodes);

    break;
  }
  case ValueKind::ClassMethodInst: {
    // Format: a type, an operand and a SILDeclRef. Use SILOneTypeValuesLayout:
    // type, Attr, SILDeclRef (DeclID, Kind, uncurryLevel, IsObjC),
    // and an operand.
    const ClassMethodInst *CMI = cast<ClassMethodInst>(&SI);
    SILType Ty = CMI->getType();
    SmallVector<ValueID, 9> ListOfValues;
    handleMethodInst(CMI, CMI->getOperand(), ListOfValues);

    SILOneTypeValuesLayout::emitRecord(Out, ScratchRecord,
        SILAbbrCodes[SILOneTypeValuesLayout::Code], (unsigned)SI.getKind(),
        S.addTypeRef(Ty.getSwiftRValueType()),
        (unsigned)Ty.getCategory(), ListOfValues);
    break;
  }
  case ValueKind::SuperMethodInst: {
    // Format: a type, an operand and a SILDeclRef. Use SILOneTypeValuesLayout:
    // type, Attr, SILDeclRef (DeclID, Kind, uncurryLevel, IsObjC),
    // and an operand.
    const SuperMethodInst *SMI = cast<SuperMethodInst>(&SI);
    SILType Ty = SMI->getType();
    SmallVector<ValueID, 9> ListOfValues;
    handleMethodInst(SMI, SMI->getOperand(), ListOfValues);

    SILOneTypeValuesLayout::emitRecord(Out, ScratchRecord,
        SILAbbrCodes[SILOneTypeValuesLayout::Code], (unsigned)SI.getKind(),
        S.addTypeRef(Ty.getSwiftRValueType()),
        (unsigned)Ty.getCategory(), ListOfValues);
    break;
  }
  case ValueKind::DynamicMethodInst: {
    // Format: a type, an operand and a SILDeclRef. Use SILOneTypeValuesLayout:
    // type, Attr, SILDeclRef (DeclID, Kind, uncurryLevel, IsObjC),
    // and an operand.
    const DynamicMethodInst *DMI = cast<DynamicMethodInst>(&SI);
    SILType Ty = DMI->getType();
    SmallVector<ValueID, 9> ListOfValues;
    handleMethodInst(DMI, DMI->getOperand(), ListOfValues);

    SILOneTypeValuesLayout::emitRecord(Out, ScratchRecord,
        SILAbbrCodes[SILOneTypeValuesLayout::Code], (unsigned)SI.getKind(),
        S.addTypeRef(Ty.getSwiftRValueType()),
        (unsigned)Ty.getCategory(), ListOfValues);
    break;
  }
  case ValueKind::DynamicMethodBranchInst: {
    // Format: a typed value, a SILDeclRef, a BasicBlock ID for method,
    // a BasicBlock ID for no method. Use SILOneTypeValuesLayout.
    const DynamicMethodBranchInst *DMB = cast<DynamicMethodBranchInst>(&SI);
    SmallVector<ValueID, 8> ListOfValues;
    ListOfValues.push_back(addValueRef(DMB->getOperand()));
    ListOfValues.push_back(DMB->getOperand().getResultNumber());
    handleSILDeclRef(S, DMB->getMember(), ListOfValues);
    ListOfValues.push_back(BasicBlockMap[DMB->getHasMethodBB()]);
    ListOfValues.push_back(BasicBlockMap[DMB->getNoMethodBB()]);

    SILOneTypeValuesLayout::emitRecord(Out, ScratchRecord,
        SILAbbrCodes[SILOneTypeValuesLayout::Code], (unsigned)SI.getKind(),
        S.addTypeRef(DMB->getOperand().getType().getSwiftRValueType()),
        (unsigned)DMB->getOperand().getType().getCategory(), ListOfValues);
    break;
  }
  case ValueKind::CheckedCastBranchInst: {
    // Format: the cast kind, a typed value, a BasicBlock ID for success,
    // a BasicBlock ID for failure. Uses SILOneTypeValuesLayout.
    const CheckedCastBranchInst *CBI = cast<CheckedCastBranchInst>(&SI);
    SmallVector<ValueID, 8> ListOfValues;
    ListOfValues.push_back(CBI->isExact()),
    ListOfValues.push_back(addValueRef(CBI->getOperand()));
    ListOfValues.push_back(CBI->getOperand().getResultNumber());
    ListOfValues.push_back(
               S.addTypeRef(CBI->getOperand().getType().getSwiftRValueType()));
    ListOfValues.push_back((unsigned)CBI->getOperand().getType().getCategory());
    ListOfValues.push_back(BasicBlockMap[CBI->getSuccessBB()]);
    ListOfValues.push_back(BasicBlockMap[CBI->getFailureBB()]);

    SILOneTypeValuesLayout::emitRecord(Out, ScratchRecord,
             SILAbbrCodes[SILOneTypeValuesLayout::Code], (unsigned)SI.getKind(),
             S.addTypeRef(CBI->getCastType().getSwiftRValueType()),
             (unsigned)CBI->getCastType().getCategory(),
             ListOfValues);
    break;
  }
  case ValueKind::CheckedCastAddrBranchInst: {
    // Format: the cast kind, two typed values, a BasicBlock ID for
    // success, a BasicBlock ID for failure.  Uses SILOneTypeValuesLayout;
    // the type is the type of the second (dest) operand.
    auto CBI = cast<CheckedCastAddrBranchInst>(&SI);
    ValueID listOfValues[] = {
      toStableCastConsumptionKind(CBI->getConsumptionKind()),
      S.addTypeRef(CBI->getSourceType()),
      addValueRef(CBI->getSrc()),
      CBI->getSrc().getResultNumber(),
      S.addTypeRef(CBI->getSrc().getType().getSwiftRValueType()),
      (unsigned)CBI->getSrc().getType().getCategory(),
      S.addTypeRef(CBI->getTargetType()),
      addValueRef(CBI->getDest()),
      CBI->getDest().getResultNumber(),
      BasicBlockMap[CBI->getSuccessBB()],
      BasicBlockMap[CBI->getFailureBB()]
    };
    SILOneTypeValuesLayout::emitRecord(Out, ScratchRecord,
             SILAbbrCodes[SILOneTypeValuesLayout::Code], (unsigned)SI.getKind(),
             S.addTypeRef(CBI->getDest().getType().getSwiftRValueType()),
             (unsigned)CBI->getDest().getType().getCategory(),
             llvm::makeArrayRef(listOfValues));
    break;
  }
  case ValueKind::InitBlockStorageHeaderInst: {
    auto IBSHI = cast<InitBlockStorageHeaderInst>(&SI);
    SmallVector<ValueID, 6> ListOfValues;
    ListOfValues.push_back(addValueRef(IBSHI->getBlockStorage()));
    ListOfValues.push_back(IBSHI->getBlockStorage().getResultNumber());
    ListOfValues.push_back(
         S.addTypeRef(IBSHI->getBlockStorage().getType().getSwiftRValueType()));
    // Always an address, don't need to save category
    
    ListOfValues.push_back(addValueRef(IBSHI->getInvokeFunction()));
    ListOfValues.push_back(IBSHI->getInvokeFunction().getResultNumber());
    ListOfValues.push_back(
       S.addTypeRef(IBSHI->getInvokeFunction().getType().getSwiftRValueType()));
    // Always a value, don't need to save category
    
    SILOneTypeValuesLayout::emitRecord(Out, ScratchRecord,
             SILAbbrCodes[SILOneTypeValuesLayout::Code], (unsigned)SI.getKind(),
             S.addTypeRef(IBSHI->getType().getSwiftRValueType()),
             (unsigned)IBSHI->getType().getCategory(),
             ListOfValues);
  }
  }
  // Non-void values get registered in the value table.
  if (SI.hasValue()) {
    addValueRef(&SI);
    ++InstID;
  }
}

/// Depending on the RecordKind, we write the SILFunction table, the global
/// variable table, the table for SILVTable, or the table for SILWitnessTable.
static void writeIndexTable(const sil_index_block::ListLayout &List,
                            sil_index_block::RecordKind kind,
                            const SILSerializer::Table &table) {
  assert((kind == sil_index_block::SIL_FUNC_NAMES ||
          kind == sil_index_block::SIL_VTABLE_NAMES ||
          kind == sil_index_block::SIL_GLOBALVAR_NAMES ||
          kind == sil_index_block::SIL_WITNESSTABLE_NAMES) &&
         "SIL function table, global, vtable and witness table are supported");
  llvm::SmallString<4096> hashTableBlob;
  uint32_t tableOffset;
  {
    llvm::OnDiskChainedHashTableGenerator<FuncTableInfo> generator;
    for (auto &entry : table)
      generator.insert(entry.first, entry.second);

    llvm::raw_svector_ostream blobStream(hashTableBlob);
    // Make sure that no bucket is at offset 0.
    endian::Writer<little>(blobStream).write<uint32_t>(0);
    tableOffset = generator.Emit(blobStream);
  }
  SmallVector<uint64_t, 8> scratch;
  List.emit(scratch, kind, tableOffset, hashTableBlob);
}

void SILSerializer::writeIndexTables() {
  BCBlockRAII restoreBlock(Out, SIL_INDEX_BLOCK_ID, 4);

  sil_index_block::ListLayout List(Out);
  sil_index_block::OffsetLayout Offset(Out);
  if (!FuncTable.empty()) {
    writeIndexTable(List, sil_index_block::SIL_FUNC_NAMES, FuncTable);
    Offset.emit(ScratchRecord, sil_index_block::SIL_FUNC_OFFSETS, Funcs);
  }

  if (!VTableList.empty()) {
    writeIndexTable(List, sil_index_block::SIL_VTABLE_NAMES, VTableList);
    Offset.emit(ScratchRecord, sil_index_block::SIL_VTABLE_OFFSETS,
                VTableOffset);
  }

  if (!GlobalVarList.empty()) {
    writeIndexTable(List, sil_index_block::SIL_GLOBALVAR_NAMES, GlobalVarList);
    Offset.emit(ScratchRecord, sil_index_block::SIL_GLOBALVAR_OFFSETS,
                GlobalVarOffset);
  }

  if (!WitnessTableList.empty()) {
    writeIndexTable(List, sil_index_block::SIL_WITNESSTABLE_NAMES, WitnessTableList);
    Offset.emit(ScratchRecord, sil_index_block::SIL_WITNESSTABLE_OFFSETS,
                WitnessTableOffset);
  }
}

void SILSerializer::writeSILGlobalVar(const SILGlobalVariable &g) {
  GlobalVarList[Ctx.getIdentifier(g.getName())] = GlobalVarID++;
  GlobalVarOffset.push_back(Out.GetCurrentBitNo());
  TypeID TyID = S.addTypeRef(g.getLoweredType().getSwiftType());
  DeclID dID = S.addDeclRef(g.getDecl());
  GlobalVarLayout::emitRecord(Out, ScratchRecord,
                              SILAbbrCodes[GlobalVarLayout::Code],
                              toStableSILLinkage(g.getLinkage()),
                              (unsigned)g.isFragile(),
                              TyID, dID, unsigned(!g.isDefinition()));
}

void SILSerializer::writeSILVTable(const SILVTable &vt) {
  VTableList[vt.getClass()->getName()] = VTableID++;
  VTableOffset.push_back(Out.GetCurrentBitNo());
  VTableLayout::emitRecord(Out, ScratchRecord, SILAbbrCodes[VTableLayout::Code],
                           S.addDeclRef(vt.getClass()));

  for (auto &entry : vt.getEntries()) {
    SmallVector<ValueID, 4> ListOfValues;
    handleSILDeclRef(S, entry.first, ListOfValues);
    FuncsToDeclare.insert(entry.second);
    // Each entry is a pair of SILDeclRef and SILFunction.
    VTableEntryLayout::emitRecord(Out, ScratchRecord,
        SILAbbrCodes[VTableEntryLayout::Code],
        // SILFunction name
        S.addIdentifierRef(Ctx.getIdentifier(entry.second->getName())),
        ListOfValues);
  }
}

void SILSerializer::writeSILWitnessTable(const SILWitnessTable &wt) {
  WitnessTableList[wt.getIdentifier()] = WitnessTableID++;
  WitnessTableOffset.push_back(Out.GetCurrentBitNo());

  WitnessTableLayout::emitRecord(
    Out, ScratchRecord,
    SILAbbrCodes[WitnessTableLayout::Code],
    toStableSILLinkage(wt.getLinkage()),
    unsigned(wt.isDeclaration()),
    unsigned(wt.isFragile()));

  S.writeConformance(wt.getConformance(), SILAbbrCodes);

  // If we have a declaration, do not attempt to serialize entries.
  if (wt.isDeclaration())
    return;

  for (auto &entry : wt.getEntries()) {
    if (entry.getKind() == SILWitnessTable::BaseProtocol) {
      auto &baseWitness = entry.getBaseProtocolWitness();

      WitnessBaseEntryLayout::emitRecord(Out, ScratchRecord,
          SILAbbrCodes[WitnessBaseEntryLayout::Code],
          S.addDeclRef(baseWitness.Requirement));

      S.writeConformance(baseWitness.Witness, SILAbbrCodes);
      continue;
    }
    if (entry.getKind() == SILWitnessTable::AssociatedTypeProtocol) {
      auto &assoc = entry.getAssociatedTypeProtocolWitness();

      WitnessAssocProtocolLayout::emitRecord(
        Out, ScratchRecord,
        SILAbbrCodes[WitnessAssocProtocolLayout::Code],
        S.addDeclRef(assoc.Requirement),
        S.addDeclRef(assoc.Protocol));
          
      S.writeConformance(assoc.Witness, SILAbbrCodes);
      continue;
    }
    if (entry.getKind() == SILWitnessTable::AssociatedType) {
      auto &assoc = entry.getAssociatedTypeWitness();
      WitnessAssocEntryLayout::emitRecord(Out, ScratchRecord,
          SILAbbrCodes[WitnessAssocEntryLayout::Code],
          S.addDeclRef(assoc.Requirement),
          S.addTypeRef(assoc.Witness));
      continue;
    }
    auto &methodWitness = entry.getMethodWitness();
    SmallVector<ValueID, 4> ListOfValues;
    handleSILDeclRef(S, methodWitness.Requirement, ListOfValues);
    FuncsToDeclare.insert(methodWitness.Witness);
    IdentifierID witnessID = 0;
    if (SILFunction *witness = methodWitness.Witness) {
      witnessID = S.addIdentifierRef(Ctx.getIdentifier(witness->getName()));
    }
    WitnessMethodEntryLayout::emitRecord(Out, ScratchRecord,
        SILAbbrCodes[WitnessMethodEntryLayout::Code],
        // SILFunction name
        witnessID,
        ListOfValues);
  }
}

/// Helper function for whether to emit a function body.
bool SILSerializer::shouldEmitFunctionBody(const SILFunction &F) {
  // If F is a declaration, it has no body to emit...
  if (F.isExternalDeclaration())
    return false;

  // If F is transparent, we should always emit its body.
  if (F.isFragile())
    return true;

  // Otherwise serialize the body of the function only if we are asked to
  // serialize everything.
  return false;
}

void SILSerializer::writeSILBlock(const SILModule *SILMod) {
  BCBlockRAII subBlock(Out, SIL_BLOCK_ID, 6);

  registerSILAbbr<SILFunctionLayout>();
  registerSILAbbr<SILBasicBlockLayout>();
  registerSILAbbr<SILOneValueOneOperandLayout>();
  registerSILAbbr<SILOneTypeLayout>();
  registerSILAbbr<SILOneOperandLayout>();
  registerSILAbbr<SILOneTypeOneOperandLayout>();
  registerSILAbbr<SILInitExistentialLayout>();
  registerSILAbbr<SILOneTypeValuesLayout>();
  registerSILAbbr<SILTwoOperandsLayout>();
  registerSILAbbr<SILInstApplyLayout>();
  registerSILAbbr<SILInstNoOperandLayout>();

  registerSILAbbr<VTableLayout>();
  registerSILAbbr<VTableEntryLayout>();
  registerSILAbbr<GlobalVarLayout>();
  registerSILAbbr<WitnessTableLayout>();
  registerSILAbbr<WitnessMethodEntryLayout>();
  registerSILAbbr<WitnessBaseEntryLayout>();
  registerSILAbbr<WitnessAssocProtocolLayout>();
  registerSILAbbr<WitnessAssocEntryLayout>();
  registerSILAbbr<SILGenericOuterParamsLayout>();

  registerSILAbbr<SILInstCastLayout>();
  registerSILAbbr<SILInstWitnessMethodLayout>();

  // Register the abbreviation codes so these layouts can exist in both
  // decl blocks and sil blocks.
  // We have to make sure BOUND_GENERIC_SUBSTITUTION does not overlap with
  // SIL-specific records.
  registerSILAbbr<decls_block::BoundGenericSubstitutionLayout>();
  registerSILAbbr<decls_block::NoConformanceLayout>();
  registerSILAbbr<decls_block::NormalProtocolConformanceLayout>();
  registerSILAbbr<decls_block::SpecializedProtocolConformanceLayout>();
  registerSILAbbr<decls_block::InheritedProtocolConformanceLayout>();
  registerSILAbbr<decls_block::NormalProtocolConformanceIdLayout>();
  registerSILAbbr<decls_block::ProtocolConformanceXrefLayout>();
  registerSILAbbr<decls_block::GenericParamListLayout>();
  registerSILAbbr<decls_block::GenericParamLayout>();
  registerSILAbbr<decls_block::GenericRequirementLayout>();
  registerSILAbbr<decls_block::LastGenericRequirementLayout>();

  for (const SILGlobalVariable &g : SILMod->getSILGlobals())
    writeSILGlobalVar(g);

  // Write out VTables first because it may require serializations of
  // non-transparent SILFunctions (body is not needed).
  // Go through all SILVTables in SILMod and write them if we should
  // serialize everything.
  // FIXME: Resilience: could write out vtable for fragile classes.
  for (const SILVTable &vt : SILMod->getVTables()) {
    if (ShouldSerializeAll &&
        vt.getClass()->getModuleContext() == SILMod->getSwiftModule())
      writeSILVTable(vt);
  }

  // Write out WitnessTables. For now, write out only if EnableSerializeAll.
  for (const SILWitnessTable &wt : SILMod->getWitnessTables()) {
    if (ShouldSerializeAll)
      writeSILWitnessTable(wt);
  }

  // Go through all the SILFunctions in SILMod and write out any
  // mandatory function bodies.
  for (const SILFunction &F : *SILMod) {
    if (shouldEmitFunctionBody(F) || ShouldSerializeAll)
      writeSILFunction(F);
  }

  if (ShouldSerializeAll)
    return;

  // Now write function declarations for every function we've
  // emitted a reference to without emitting a function body for.
  for (const SILFunction &F : *SILMod) {
    if (!shouldEmitFunctionBody(F) && FuncsToDeclare.count(&F))
      writeSILFunction(F, true);
  }
}

void SILSerializer::writeSILModule(const SILModule *SILMod) {
  writeSILBlock(SILMod);
  writeIndexTables();
}

void Serializer::writeSIL(const SILModule *SILMod, bool serializeAllSIL) {
  if (!SILMod)
    return;

  SILSerializer SILSer(*this, M->getASTContext(), Out, serializeAllSIL);
  SILSer.writeSILModule(SILMod);
}
