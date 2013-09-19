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

#include "SILFormat.h"
#include "Serialization.h"
#include "swift/AST/Module.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILModule.h"

// This is a template-only header; eventually it should move to llvm/Support.
#include "clang/Basic/OnDiskHashTable.h"

#include "llvm/ADT/StringExtras.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"

// To help testing serialization, deserialization, we turn on sil-serialize-all.
static llvm::cl::opt<bool>
EnableSerializeAll("sil-serialize-all", llvm::cl::Hidden,
                   llvm::cl::init(false));
static llvm::cl::opt<bool>
EnableSerialize("enable-sil-serialization", llvm::cl::Hidden,
                llvm::cl::init(false));

using namespace swift;
using namespace swift::serialization;
using namespace swift::serialization::sil_block;

namespace {
    /// Used to serialize the on-disk func hash table.
  class FuncTableInfo {
  public:
    using key_type = Identifier;
    using key_type_ref = key_type;
    using data_type = DeclID;
    using data_type_ref = const data_type &;

    uint32_t ComputeHash(key_type_ref key) {
      assert(!key.empty());
      return llvm::HashString(key.str());
    }

    std::pair<unsigned, unsigned> EmitKeyDataLength(raw_ostream &out,
                                                    key_type_ref key,
                                                    data_type_ref data) {
      using namespace clang::io;
      uint32_t keyLength = key.str().size();
      uint32_t dataLength = sizeof(DeclID);
      Emit16(out, keyLength);
      Emit16(out, dataLength);
      return { keyLength, dataLength };
    }

    void EmitKey(raw_ostream &out, key_type_ref key, unsigned len) {
      out << key.str();
    }

    void EmitData(raw_ostream &out, key_type_ref key, data_type_ref data,
                  unsigned len) {
      static_assert(sizeof(DeclID) <= 32, "DeclID too large");
      using namespace clang::io;
      Emit32(out, data);
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
    ValueID LastValueID = 0;
    ValueID addValueRef(SILValue SV) {
      return addValueRef(SV.getDef());
    }
    ValueID addValueRef(const ValueBase *Val);

    /// FuncTable maps function name to an ID.
    using TableData = FuncTableInfo::data_type;
    using Table = llvm::DenseMap<FuncTableInfo::key_type, TableData>;
    Table FuncTable;
    std::vector<BitOffset> Funcs;
    /// The current function ID.
    DeclID FuncID;

    /// Give each SILBasicBlock a unique ID.
    llvm::DenseMap<const SILBasicBlock*, unsigned> BasicBlockMap;

    std::array<unsigned, 256> SILAbbrCodes;
    template <typename Layout>
    void registerSILAbbr() {
      using AbbrArrayTy = decltype(SILAbbrCodes);
      static_assert(Layout::Code <= std::tuple_size<AbbrArrayTy>::value,
                    "layout has invalid record code");
      SILAbbrCodes[Layout::Code] = Layout::emitAbbrev(Out);
      DEBUG(llvm::dbgs() << "SIL abbre code " << SILAbbrCodes[Layout::Code]
                         << "\n");
    }

    /// Helper function to update ListOfValues for MethodInst. Format:
    /// Attr, SILDeclRef (DeclID, Kind, uncurryLevel, IsObjC), and an operand.
    void handleMethodInst(const MethodInst *MI, SILValue operand,
                          SmallVectorImpl<ValueID> &ListOfValues);

    void writeSILFunction(const SILFunction &F);
    void writeSILBasicBlock(const SILBasicBlock &BB);
    void writeSILInstruction(const SILInstruction &SI);
    void writeFuncTable();

  public:
    SILSerializer(Serializer &S, ASTContext &Ctx,
                  llvm::BitstreamWriter &Out);

    void writeAllSILFunctions(const SILModule *M);
  };
} // end anonymous namespace

SILSerializer::SILSerializer(Serializer &S, ASTContext &Ctx,
                             llvm::BitstreamWriter &Out) :
                            S(S), Ctx(Ctx), Out(Out), FuncID(1) {
}

/// We enumerate all values to update ValueIDs in a separate pass
/// to correctly handle forward reference of a value.
ValueID SILSerializer::addValueRef(const ValueBase *Val) {
  if (!Val)
    return 0;

  ValueID &id = ValueIDs[Val];
  if (id != 0)
    return id;

  id = ++LastValueID;
  return id;
}

void SILSerializer::writeSILFunction(const SILFunction &F) {
  DEBUG(llvm::dbgs() << "Serialize SIL:\n";
        F.dump());
  LastValueID = 0;
  FuncTable[Ctx.getIdentifier(F.getName())] = FuncID++;
  Funcs.push_back(Out.GetCurrentBitNo());
  InstID = 0;
  unsigned abbrCode = SILAbbrCodes[SILFunctionLayout::Code];
  TypeID FnID = S.addTypeRef(F.getLoweredType().getSwiftType());
  DEBUG(llvm::dbgs() << "SILFunction @" << Out.GetCurrentBitNo() <<
        " abbrCode " << abbrCode << " FnID " << FnID << "\n");
  SILFunctionLayout::emitRecord(Out, ScratchRecord, abbrCode,
                       (unsigned)F.getLinkage(), FnID);
  
  // Assign a unique ID to each basic block of the SILFunction.
  unsigned BasicID = 0;
  BasicBlockMap.clear();
  for (const SILBasicBlock &BB : F)
    BasicBlockMap.insert(std::make_pair(&BB, BasicID++));

  for (const SILBasicBlock &BB : F)
    writeSILBasicBlock(BB);
}

void SILSerializer::writeSILBasicBlock(const SILBasicBlock &BB) {
  SmallVector<DeclID, 4> Args;
  for (auto I = BB.bbarg_begin(), E = BB.bbarg_end(); I != E; ++I) {
    SILArgument *SA = *I;
    DeclID tId = S.addTypeRef(SA->getType().getSwiftType());
    DeclID vId = addValueRef(static_cast<const ValueBase*>(SA));
    Args.push_back(tId);
    Args.push_back(vId);
  }

  unsigned abbrCode = SILAbbrCodes[SILBasicBlockLayout::Code];
  SILBasicBlockLayout::emitRecord(Out, ScratchRecord, abbrCode, Args);

  for (const SILInstruction &SI : BB)
    writeSILInstruction(SI);
}

/// Helper function to update ListOfValues for MethodInst. Format:
/// Attr, SILDeclRef (DeclID, Kind, uncurryLevel, IsObjC), and an operand.
void SILSerializer::handleMethodInst(const MethodInst *MI,
                                     SILValue operand,
                                     SmallVectorImpl<ValueID> &ListOfValues) {
  ListOfValues.push_back(MI->isVolatile());
  ListOfValues.push_back(S.addDeclRef(MI->getMember().getDecl()));
  ListOfValues.push_back((unsigned)MI->getMember().kind);
  ListOfValues.push_back(MI->getMember().uncurryLevel);
  ListOfValues.push_back(MI->getMember().isObjC);
  ListOfValues.push_back(
      S.addTypeRef(operand.getType().getSwiftRValueType()));
  ListOfValues.push_back((unsigned)operand.getType().getCategory());
  ListOfValues.push_back(addValueRef(operand));
  ListOfValues.push_back(operand.getResultNumber());
}

void SILSerializer::writeSILInstruction(const SILInstruction &SI) {
  switch (SI.getKind()) {
  default: {
    unsigned abbrCode = SILAbbrCodes[SILInstTodoLayout::Code];
    SILInstTodoLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                  (unsigned)SI.getKind());
    break;
  }
  case ValueKind::DeallocBoxInst:
  case ValueKind::InitExistentialInst:
  case ValueKind::InitExistentialRefInst:
  case ValueKind::ArchetypeMetatypeInst:
  case ValueKind::ClassMetatypeInst:
  case ValueKind::ProtocolMetatypeInst:
  case ValueKind::AllocArrayInst: {
    SILValue operand;
    SILType Ty;
    switch (SI.getKind()) {
    default: assert(0 && "Out of sync with parent switch");
    case ValueKind::ArchetypeMetatypeInst:
      operand = cast<ArchetypeMetatypeInst>(&SI)->getOperand();
      Ty = cast<ArchetypeMetatypeInst>(&SI)->getType();
      break;
    case ValueKind::ClassMetatypeInst:
      operand = cast<ClassMetatypeInst>(&SI)->getOperand();
      Ty = cast<ClassMetatypeInst>(&SI)->getType();
      break;
    case ValueKind::InitExistentialInst:
      operand = cast<InitExistentialInst>(&SI)->getOperand();
      Ty = cast<InitExistentialInst>(&SI)->getConcreteType();
      break;
    case ValueKind::InitExistentialRefInst:
      operand = cast<InitExistentialRefInst>(&SI)->getOperand();
      Ty = cast<InitExistentialRefInst>(&SI)->getType();
      break;
    case ValueKind::ProtocolMetatypeInst:
      operand = cast<ProtocolMetatypeInst>(&SI)->getOperand();
      Ty = cast<ProtocolMetatypeInst>(&SI)->getType();
      break;
    case ValueKind::DeallocBoxInst:
      operand = cast<DeallocBoxInst>(&SI)->getOperand();
      Ty = cast<DeallocBoxInst>(&SI)->getElementType();
      break;
    case ValueKind::AllocArrayInst:
      operand = cast<AllocArrayInst>(&SI)->getNumElements();
      Ty = cast<AllocArrayInst>(&SI)->getElementType();
      break;
    }
    unsigned abbrCode = SILAbbrCodes[SILOneTypeOneOperandLayout::Code];
    SILOneTypeOneOperandLayout::emitRecord(Out, ScratchRecord, abbrCode,
        (unsigned)SI.getKind(), 0,
        S.addTypeRef(Ty.getSwiftRValueType()),
        (unsigned)Ty.getCategory(),
        S.addTypeRef(operand.getType().getSwiftRValueType()),
        (unsigned)operand.getType().getCategory(),
        addValueRef(operand),
        operand.getResultNumber());
    break;
  }
  case ValueKind::AllocBoxInst: {
    const AllocBoxInst *ABI = cast<AllocBoxInst>(&SI);
    unsigned abbrCode = SILAbbrCodes[SILOneTypeLayout::Code];
    SILOneTypeLayout::emitRecord(Out, ScratchRecord, abbrCode,
                      (unsigned)SI.getKind(),
                      S.addTypeRef(ABI->getElementType().getSwiftRValueType()),
                      (unsigned)ABI->getElementType().getCategory());
    break;
  }
  case ValueKind::AllocRefInst: {
    const AllocRefInst *ARI = cast<AllocRefInst>(&SI);
    unsigned abbrCode = SILAbbrCodes[SILOneTypeLayout::Code];
    SILOneTypeLayout::emitRecord(Out, ScratchRecord, abbrCode,
                      (unsigned)SI.getKind(),
                      S.addTypeRef(ARI->getType().getSwiftRValueType()),
                      (unsigned)ARI->getType().getCategory());
    break;
  }
  case ValueKind::AllocStackInst: {
    const AllocStackInst *ASI = cast<AllocStackInst>(&SI);
    unsigned abbrCode = SILAbbrCodes[SILOneTypeLayout::Code];
    SILOneTypeLayout::emitRecord(Out, ScratchRecord, abbrCode,
                      (unsigned)SI.getKind(),
                      S.addTypeRef(ASI->getElementType().getSwiftRValueType()),
                      (unsigned)ASI->getElementType().getCategory());
    break;
  }
  case ValueKind::BuiltinZeroInst: {
    const BuiltinZeroInst *BZI = cast<BuiltinZeroInst>(&SI);
    unsigned abbrCode = SILAbbrCodes[SILOneTypeLayout::Code];
    SILOneTypeLayout::emitRecord(Out, ScratchRecord, abbrCode,
                      (unsigned)SI.getKind(),
                      S.addTypeRef(BZI->getType().getSwiftRValueType()),
                      (unsigned)BZI->getType().getCategory());
    break;
  }
  case ValueKind::ApplyInst: {
    // Format: attributes such as transparent, the callee's type, a value for
    // the callee and a list of values for the arguments. Each value in the list
    // is represented with 2 IDs: ValueID and ValueResultNumber.
    const ApplyInst *AI = cast<ApplyInst>(&SI);
    SmallVector<ValueID, 4> Args;
    for (auto Arg: AI->getArguments()) {
      Args.push_back(addValueRef(Arg));
      Args.push_back(Arg.getResultNumber());
    }
    SILInstApplyLayout::emitRecord(Out, ScratchRecord,
        SILAbbrCodes[SILInstApplyLayout::Code], 0/*Transparent*/,
        (unsigned)AI->isTransparent(),
        S.addTypeRef(AI->getCallee().getType().getSwiftRValueType()),
        (unsigned)AI->getCallee().getType().getCategory(),
        addValueRef(AI->getCallee()), AI->getCallee().getResultNumber(),
        Args);
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
        0/*Transparent*/,
        S.addTypeRef(PAI->getCallee().getType().getSwiftRValueType()),
        (unsigned)PAI->getCallee().getType().getCategory(),
        addValueRef(PAI->getCallee()), PAI->getCallee().getResultNumber(),
        Args);
    break;
  }
  case ValueKind::BuiltinFunctionRefInst: {
    // Format: FuncDecl and type. Use SILOneOperandLayout.
    const BuiltinFunctionRefInst *BFR = cast<BuiltinFunctionRefInst>(&SI);
    SILOneOperandLayout::emitRecord(Out, ScratchRecord,
        SILAbbrCodes[SILOneOperandLayout::Code],
        (unsigned)SI.getKind(), 0,
        S.addTypeRef(BFR->getType().getSwiftRValueType()),
        (unsigned)BFR->getType().getCategory(),
        S.addDeclRef(BFR->getFunction()), 0);
    break;
  }
  case ValueKind::GlobalAddrInst: {
    // Format: VarDecl and type. Use SILOneOperandLayout.
    const GlobalAddrInst *GAI = cast<GlobalAddrInst>(&SI);
    SILOneOperandLayout::emitRecord(Out, ScratchRecord,
        SILAbbrCodes[SILOneOperandLayout::Code],
        (unsigned)SI.getKind(), 0,
        S.addTypeRef(GAI->getType().getSwiftRValueType()),
        (unsigned)GAI->getType().getCategory(),
        S.addDeclRef(GAI->getGlobal()), 0);
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
  case ValueKind::SwitchUnionInst:
  case ValueKind::DestructiveSwitchUnionAddrInst: {
    // Format: condition, a list of cases (UnionElementDecl + Basic Block ID),
    // default basic block ID. Use SILOneTypeValuesLayout: the type is
    // for condition, the list has value for condition, hasDefault, default
    // basic block ID, a list of (DeclID, BasicBlock ID).
    const SwitchUnionInstBase *SOI = cast<SwitchUnionInstBase>(&SI);
    SmallVector<ValueID, 4> ListOfValues;
    ListOfValues.push_back(addValueRef(SOI->getOperand()));
    ListOfValues.push_back(SOI->getOperand().getResultNumber());
    ListOfValues.push_back((unsigned)SOI->hasDefault());
    if (SOI->hasDefault())
      ListOfValues.push_back(BasicBlockMap[SOI->getDefaultBB()]);
    else
      ListOfValues.push_back(0);

    for (unsigned i = 0, e = SOI->getNumCases(); i < e; ++i) {
      UnionElementDecl *elt;
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
  case ValueKind::DeallocStackInst:
  case ValueKind::DeallocRefInst:
  case ValueKind::DeinitExistentialInst:
  case ValueKind::DestroyAddrInst:
  case ValueKind::InitializeVarInst:
  case ValueKind::IsNonnullInst:
  case ValueKind::LoadInst:
  case ValueKind::LoadWeakInst:
  case ValueKind::MarkUninitializedInst:
  case ValueKind::StrongReleaseInst:
  case ValueKind::StrongRetainInst:
  case ValueKind::StrongRetainAutoreleasedInst:
  case ValueKind::AutoreleaseReturnInst:
  case ValueKind::StrongRetainUnownedInst:
  case ValueKind::UnownedRetainInst:
  case ValueKind::UnownedReleaseInst:
  case ValueKind::ReturnInst: {
    unsigned Attr = 0;
    if (SI.getKind() == ValueKind::LoadWeakInst)
      Attr = cast<LoadWeakInst>(&SI)->isTake();
    else if (SI.getKind() == ValueKind::InitializeVarInst)
      Attr = cast<InitializeVarInst>(&SI)->canDefaultConstruct();
    unsigned abbrCode = SILAbbrCodes[SILOneOperandLayout::Code];
    SILOneOperandLayout::emitRecord(Out, ScratchRecord, abbrCode,
                 (unsigned)SI.getKind(), Attr,
                 S.addTypeRef(SI.getOperand(0).getType().getSwiftRValueType()),
                 (unsigned)SI.getOperand(0).getType().getCategory(),
                 addValueRef(SI.getOperand(0)),
                 SI.getOperand(0).getResultNumber());
    break;
  }
  case ValueKind::FunctionRefInst: {
    // Use SILOneOperandLayout to specify the function type and the function
    // name (IdentifierID).
    const FunctionRefInst *FRI = cast<FunctionRefInst>(&SI);
    unsigned abbrCode = SILAbbrCodes[SILOneOperandLayout::Code];
    SILOneOperandLayout::emitRecord(Out, ScratchRecord, abbrCode,
        (unsigned)SI.getKind(), 0,
        S.addTypeRef(FRI->getType().getSwiftRValueType()),
        (unsigned)FRI->getType().getCategory(),
        S.addIdentifierRef(Ctx.getIdentifier(FRI->getFunction()->getName())),
        0);
    break;
  }
  case ValueKind::IndexAddrInst:
  case ValueKind::IndexRawPointerInst:
  case ValueKind::UpcastExistentialInst: {
    SILValue operand, operand2;
    unsigned Attr = 0;
    if (SI.getKind() == ValueKind::IndexRawPointerInst) {
      const IndexRawPointerInst *IRP = cast<IndexRawPointerInst>(&SI);
      operand = IRP->getBase();
      operand2 = IRP->getIndex();
    } else if (SI.getKind() == ValueKind::UpcastExistentialInst) {
      Attr = cast<UpcastExistentialInst>(&SI)->isTakeOfSrc();
      operand = cast<UpcastExistentialInst>(&SI)->getSrcExistential();
      operand2 = cast<UpcastExistentialInst>(&SI)->getDestExistential();
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
  case ValueKind::FloatLiteralInst:
  case ValueKind::IntegerLiteralInst:
  case ValueKind::StringLiteralInst: {
    // Use SILOneOperandLayout to specify the type and the literal.
    StringRef Str;
    SILType Ty;
    switch (SI.getKind()) {
    default: assert(0 && "Out of sync with parent switch");
    case ValueKind::IntegerLiteralInst:
      Str = cast<IntegerLiteralInst>(&SI)->getValue().toString(10, true);
      Ty = cast<IntegerLiteralInst>(&SI)->getType();
      break;
    case ValueKind::FloatLiteralInst:
      Str = cast<FloatLiteralInst>(&SI)->getBits().toString(16,
                                                            /*Signed*/false);
      Ty = cast<FloatLiteralInst>(&SI)->getType();
      break;
    case ValueKind::StringLiteralInst:
      Str = cast<StringLiteralInst>(&SI)->getValue();
      Ty = cast<StringLiteralInst>(&SI)->getType();
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
  case ValueKind::MetatypeInst: {
    const MetatypeInst *MI = cast<MetatypeInst>(&SI);
    unsigned abbrCode = SILAbbrCodes[SILOneTypeLayout::Code];
    SILOneTypeLayout::emitRecord(Out, ScratchRecord, abbrCode,
                      (unsigned)SI.getKind(),
                      S.addTypeRef(MI->getType().getSwiftRValueType()),
                      (unsigned)MI->getType().getCategory());
    break;
  }
  case ValueKind::ModuleInst: {
    // Has IdentifierID for the module reference. Use SILOneTypeLayout.
    const ModuleInst *MI = cast<ModuleInst>(&SI);
    ModuleType *MT = MI->getType().castTo<ModuleType>();
    SILOneTypeLayout::emitRecord(Out, ScratchRecord,
                      SILAbbrCodes[SILOneTypeLayout::Code],
                      (unsigned)SI.getKind(),
                      S.addModuleRef(MT->getModule()), 0);
    break;
  }
  case ValueKind::ProjectExistentialInst: {
    const ProjectExistentialInst *PEI = cast<ProjectExistentialInst>(&SI);
    SILOneTypeOneOperandLayout::emitRecord(Out, ScratchRecord,
        SILAbbrCodes[SILOneTypeOneOperandLayout::Code],
        (unsigned)SI.getKind(), 0,
        S.addTypeRef(PEI->getType().getSwiftRValueType()),
        (unsigned)PEI->getType().getCategory(),
        S.addTypeRef(PEI->getOperand().getType().getSwiftRValueType()),
        (unsigned)PEI->getOperand().getType().getCategory(),
        addValueRef(PEI->getOperand()),
        PEI->getOperand().getResultNumber());
    break;
  }
  case ValueKind::ProjectExistentialRefInst: {
    const ProjectExistentialRefInst *PEI = cast<ProjectExistentialRefInst>(&SI);
    SILOneOperandLayout::emitRecord(Out, ScratchRecord,
        SILAbbrCodes[SILOneOperandLayout::Code],
        (unsigned)SI.getKind(), 0,
        S.addTypeRef(PEI->getOperand().getType().getSwiftRValueType()),
        (unsigned)PEI->getOperand().getType().getCategory(),
        addValueRef(PEI->getOperand()),
        PEI->getOperand().getResultNumber());
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
    SILValue operand;
    SILType Ty;
    switch (SI.getKind()) {
    default: assert(0 && "Out of sync with parent switch");
    case ValueKind::RefToObjectPointerInst:
      operand = cast<RefToObjectPointerInst>(&SI)->getOperand();
      Ty = cast<RefToObjectPointerInst>(&SI)->getType();
      break;
    case ValueKind::UpcastInst:
      operand = cast<UpcastInst>(&SI)->getOperand();
      Ty = cast<UpcastInst>(&SI)->getType();
      break;
    case ValueKind::CoerceInst:
      operand = cast<CoerceInst>(&SI)->getOperand();
      Ty = cast<CoerceInst>(&SI)->getType();
      break;
    case ValueKind::AddressToPointerInst:
      operand = cast<AddressToPointerInst>(&SI)->getOperand();
      Ty = cast<AddressToPointerInst>(&SI)->getType();
      break;
    case ValueKind::PointerToAddressInst:
      operand = cast<PointerToAddressInst>(&SI)->getOperand();
      Ty = cast<PointerToAddressInst>(&SI)->getType();
      break;
    case ValueKind::ObjectPointerToRefInst:
      operand = cast<ObjectPointerToRefInst>(&SI)->getOperand();
      Ty = cast<ObjectPointerToRefInst>(&SI)->getType();
      break;
    case ValueKind::RefToRawPointerInst:
      operand = cast<RefToRawPointerInst>(&SI)->getOperand();
      Ty = cast<RefToRawPointerInst>(&SI)->getType();
      break;
    case ValueKind::RawPointerToRefInst:
      operand = cast<RawPointerToRefInst>(&SI)->getOperand();
      Ty = cast<RawPointerToRefInst>(&SI)->getType();
      break;
    case ValueKind::RefToUnownedInst:
      operand = cast<RefToUnownedInst>(&SI)->getOperand();
      Ty = cast<RefToUnownedInst>(&SI)->getType();
      break;
    case ValueKind::UnownedToRefInst:
      operand = cast<UnownedToRefInst>(&SI)->getOperand();
      Ty = cast<UnownedToRefInst>(&SI)->getType();
      break;
    case ValueKind::ConvertCCInst:
      operand = cast<ConvertCCInst>(&SI)->getOperand();
      Ty = cast<ConvertCCInst>(&SI)->getType();
      break;
    case ValueKind::ThinToThickFunctionInst:
      operand = cast<ThinToThickFunctionInst>(&SI)->getOperand();
      Ty = cast<ThinToThickFunctionInst>(&SI)->getType();
      break;
    case ValueKind::BridgeToBlockInst:
      operand = cast<BridgeToBlockInst>(&SI)->getOperand();
      Ty = cast<BridgeToBlockInst>(&SI)->getType();
      break;
    case ValueKind::ArchetypeRefToSuperInst:
      operand = cast<ArchetypeRefToSuperInst>(&SI)->getOperand();
      Ty = cast<ArchetypeRefToSuperInst>(&SI)->getType();
      break;
    case ValueKind::ConvertFunctionInst:
      operand = cast<ConvertFunctionInst>(&SI)->getOperand();
      Ty = cast<ConvertFunctionInst>(&SI)->getType();
      break;
    case ValueKind::UpcastExistentialRefInst:
      operand = cast<UpcastExistentialRefInst>(&SI)->getOperand();
      Ty = cast<UpcastExistentialRefInst>(&SI)->getType();
      break;
    }
    SILOneTypeOneOperandLayout::emitRecord(Out, ScratchRecord,
        SILAbbrCodes[SILOneTypeOneOperandLayout::Code],
        (unsigned)SI.getKind(), 0,
        S.addTypeRef(Ty.getSwiftRValueType()),
        (unsigned)Ty.getCategory(),
        S.addTypeRef(operand.getType().getSwiftRValueType()),
        (unsigned)operand.getType().getCategory(),
        addValueRef(operand), operand.getResultNumber());
    break;
  }
  // Checked Conversion instructions.
  case ValueKind::DowncastInst:
  case ValueKind::SuperToArchetypeRefInst:
  case ValueKind::DowncastArchetypeAddrInst:
  case ValueKind::DowncastArchetypeRefInst:
  case ValueKind::ProjectDowncastExistentialAddrInst:
  case ValueKind::DowncastExistentialRefInst: {
    SILValue operand;
    const CheckedConversionInst *CI;
    switch (SI.getKind()) {
    default: assert(0 && "Out of sync with parent switch");
    case ValueKind::DowncastInst:
      operand = cast<DowncastInst>(&SI)->getOperand();
      CI = cast<DowncastInst>(&SI);
      break;
    case ValueKind::SuperToArchetypeRefInst:
      operand = cast<SuperToArchetypeRefInst>(&SI)->getOperand();
      CI = cast<SuperToArchetypeRefInst>(&SI);
      break;
    case ValueKind::DowncastArchetypeAddrInst:
      operand = cast<DowncastArchetypeAddrInst>(&SI)->getOperand();
      CI = cast<DowncastArchetypeAddrInst>(&SI);
      break;
    case ValueKind::DowncastArchetypeRefInst:
      operand = cast<DowncastArchetypeRefInst>(&SI)->getOperand();
      CI = cast<DowncastArchetypeRefInst>(&SI);
      break;
    case ValueKind::ProjectDowncastExistentialAddrInst:
      operand = cast<ProjectDowncastExistentialAddrInst>(&SI)->getOperand();
      CI = cast<ProjectDowncastExistentialAddrInst>(&SI);
      break;
    case ValueKind::DowncastExistentialRefInst:
      operand = cast<DowncastExistentialRefInst>(&SI)->getOperand();
      CI = cast<DowncastExistentialRefInst>(&SI);
      break;
    }
    SILOneTypeOneOperandLayout::emitRecord(Out, ScratchRecord,
        SILAbbrCodes[SILOneTypeOneOperandLayout::Code],
        (unsigned)SI.getKind(), (unsigned)CI->getMode(),
        S.addTypeRef(CI->getType().getSwiftRValueType()),
        (unsigned)CI->getType().getCategory(),
        S.addTypeRef(operand.getType().getSwiftRValueType()),
        (unsigned)operand.getType().getCategory(),
        addValueRef(operand), operand.getResultNumber());
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
      Attr = (CAI->isInitializationOfDest() << 1) || CAI->isTakeOfSrc();
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
  case ValueKind::UnionDataAddrInst:
  case ValueKind::InjectUnionAddrInst: {
    // Has a typed valueref and a field decl. We use SILOneValueOneOperandLayout
    // where the field decl is streamed as a ValueID.
    SILValue operand;
    Decl *tDecl;
    switch (SI.getKind()) {
    default: assert(0 && "Out of sync with parent switch");
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
    case ValueKind::UnionDataAddrInst:
      operand = cast<UnionDataAddrInst>(&SI)->getOperand();
      tDecl = cast<UnionDataAddrInst>(&SI)->getElement();
      break;
    case ValueKind::InjectUnionAddrInst:
      operand = cast<InjectUnionAddrInst>(&SI)->getOperand();
      tDecl = cast<InjectUnionAddrInst>(&SI)->getElement();
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
    default: assert(0 && "Out of sync with parent switch");
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
  case ValueKind::UnionInst: {
    // Format: a type, an operand and a decl ID. Use SILTwoOperandsLayout: type,
    // (DeclID + hasOperand), and an operand.
    const UnionInst *UI = cast<UnionInst>(&SI);
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
  case ValueKind::ArchetypeMethodInst: {
    // Format: a type, an operand and a SILDeclRef. Use SILOneTypeValuesLayout:
    // type, Attr, SILDeclRef (DeclID, Kind, uncurryLevel, IsObjC), and a type.
    const ArchetypeMethodInst *AMI = cast<ArchetypeMethodInst>(&SI);
    SILType Ty = AMI->getLookupArchetype();
    SILType Ty2 = AMI->getType(0);

    SmallVector<ValueID, 7> ListOfValues;
    ListOfValues.push_back(AMI->isVolatile());
    ListOfValues.push_back(S.addDeclRef(AMI->getMember().getDecl()));
    ListOfValues.push_back((unsigned)AMI->getMember().kind);
    ListOfValues.push_back(AMI->getMember().uncurryLevel);
    ListOfValues.push_back(AMI->getMember().isObjC);
    ListOfValues.push_back(S.addTypeRef(Ty2.getSwiftRValueType()));
    ListOfValues.push_back((unsigned)Ty2.getCategory());

    SILOneTypeValuesLayout::emitRecord(Out, ScratchRecord,
        SILAbbrCodes[SILOneTypeValuesLayout::Code], (unsigned)SI.getKind(),
        S.addTypeRef(Ty.getSwiftRValueType()),
        (unsigned)Ty.getCategory(), ListOfValues);
    break;
  }
  case ValueKind::ProtocolMethodInst: {
    // Format: a type, an operand and a SILDeclRef. Use SILOneTypeValuesLayout:
    // type, Attr, SILDeclRef (DeclID, Kind, uncurryLevel, IsObjC),
    // and an operand.
    const ProtocolMethodInst *PMI = cast<ProtocolMethodInst>(&SI);
    SILType Ty = PMI->getType();
    SmallVector<ValueID, 9> ListOfValues;
    handleMethodInst(PMI, PMI->getOperand(), ListOfValues);

    SILOneTypeValuesLayout::emitRecord(Out, ScratchRecord,
        SILAbbrCodes[SILOneTypeValuesLayout::Code], (unsigned)SI.getKind(),
        S.addTypeRef(Ty.getSwiftRValueType()),
        (unsigned)Ty.getCategory(), ListOfValues);
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
    ListOfValues.push_back(S.addDeclRef(DMB->getMember().getDecl()));
    ListOfValues.push_back((unsigned)DMB->getMember().kind);
    ListOfValues.push_back(DMB->getMember().uncurryLevel);
    ListOfValues.push_back(DMB->getMember().isObjC);
    ListOfValues.push_back(BasicBlockMap[DMB->getHasMethodBB()]);
    ListOfValues.push_back(BasicBlockMap[DMB->getNoMethodBB()]);

    SILOneTypeValuesLayout::emitRecord(Out, ScratchRecord,
        SILAbbrCodes[SILOneTypeValuesLayout::Code], (unsigned)SI.getKind(),
        S.addTypeRef(DMB->getOperand().getType().getSwiftRValueType()),
        (unsigned)DMB->getOperand().getType().getCategory(), ListOfValues);
    break;
  }
  }
  // Non-void values get registered in the value table.
  if (SI.hasValue()) {
    addValueRef(&SI);
    ++InstID;
  }
}

void SILSerializer::writeFuncTable() {
  using clang::OnDiskChainedHashTableGenerator;

  if (FuncTable.empty())
    return;

  SmallVector<uint64_t, 8> scratch;
  llvm::SmallString<4096> hashTableBlob;
  uint32_t tableOffset;
  {
    OnDiskChainedHashTableGenerator<FuncTableInfo> generator;
    for (auto &entry : FuncTable)
      generator.insert(entry.first, entry.second);

    llvm::raw_svector_ostream blobStream(hashTableBlob);
    // Make sure that no bucket is at offset 0
    clang::io::Emit32(blobStream, 0);
    tableOffset = generator.Emit(blobStream);
  }

  unsigned abbrCode = SILAbbrCodes[FuncListLayout::Code];
  FuncListLayout::emitRecord(Out, ScratchRecord, abbrCode, tableOffset,
                             hashTableBlob);

  abbrCode = SILAbbrCodes[FuncOffsetLayout::Code];
  FuncOffsetLayout::emitRecord(Out, ScratchRecord, abbrCode, Funcs);
}

void SILSerializer::writeAllSILFunctions(const SILModule *M) {
  {
    BCBlockRAII subBlock(Out, SIL_BLOCK_ID, 4);
    registerSILAbbr<SILFunctionLayout>();
    registerSILAbbr<SILBasicBlockLayout>();
    registerSILAbbr<SILOneValueOneOperandLayout>();
    registerSILAbbr<SILOneTypeLayout>();
    registerSILAbbr<SILOneOperandLayout>();
    registerSILAbbr<SILOneTypeOneOperandLayout>();
    registerSILAbbr<SILOneTypeValuesLayout>();
    registerSILAbbr<SILTwoOperandsLayout>();
    registerSILAbbr<SILInstApplyLayout>();
    registerSILAbbr<SILInstTodoLayout>();

    // Go through all SILFunctions in M, and if it is transparent,
    // write out the SILFunction.
    for (const SILFunction &F : *M) {
      if ((EnableSerialize || EnableSerializeAll) &&
          (EnableSerializeAll || F.isTransparent())
          && !F.empty())
        writeSILFunction(F);
    }
  }
  {
    BCBlockRAII restoreBlock(Out, SIL_INDEX_BLOCK_ID, 4);
    registerSILAbbr<FuncListLayout>();
    registerSILAbbr<FuncOffsetLayout>();
    writeFuncTable();
  }
}

void Serializer::writeSILFunctions(const SILModule *M) {
  if (!M)
    return;

  SILSerializer SILSer(*this, TU->Ctx, Out);
  SILSer.writeAllSILFunctions(M);

}
