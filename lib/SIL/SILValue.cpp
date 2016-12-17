//===--- SILValue.cpp - Implementation for SILValue -----------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/SIL/SILValue.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILBuiltinVisitor.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILVisitor.h"

using namespace swift;

//===----------------------------------------------------------------------===//
//                       Check SILValue Type Properties
//===----------------------------------------------------------------------===//

/// These are just for performance and verification. If one needs to make
/// changes that cause the asserts the fire, please update them. The purpose is
/// to prevent these predicates from changing values by mistake.
static_assert(std::is_standard_layout<SILValue>::value,
              "Expected SILValue to be standard layout");
static_assert(sizeof(SILValue) == sizeof(uintptr_t),
              "SILValue should be pointer sized");

//===----------------------------------------------------------------------===//
//                              Utility Methods
//===----------------------------------------------------------------------===//

void ValueBase::replaceAllUsesWith(ValueBase *RHS) {
  assert(this != RHS && "Cannot RAUW a value with itself");
  while (!use_empty()) {
    Operand *Op = *use_begin();
    Op->set(RHS);
  }
}

SILBasicBlock *ValueBase::getParentBlock() const {
  auto *NonConstThis = const_cast<ValueBase *>(this);
  if (auto *Inst = dyn_cast<SILInstruction>(NonConstThis))
    return Inst->getParent();
  if (auto *Arg = dyn_cast<SILArgument>(NonConstThis))
    return Arg->getParent();
  return nullptr;
}

SILFunction *ValueBase::getFunction() const {
  auto *NonConstThis = const_cast<ValueBase *>(this);
  if (auto *Inst = dyn_cast<SILInstruction>(NonConstThis))
    return Inst->getFunction();
  if (auto *Arg = dyn_cast<SILArgument>(NonConstThis))
    return Arg->getFunction();
  return nullptr;
}

SILModule *ValueBase::getModule() const {
  auto *NonConstThis = const_cast<ValueBase *>(this);
  if (auto *Inst = dyn_cast<SILInstruction>(NonConstThis))
    return &Inst->getModule();
  if (auto *Arg = dyn_cast<SILArgument>(NonConstThis))
    return &Arg->getModule();
  return nullptr;
}

//===----------------------------------------------------------------------===//
//                             ValueOwnershipKind
//===----------------------------------------------------------------------===//

llvm::raw_ostream &swift::operator<<(llvm::raw_ostream &os,
                                     ValueOwnershipKind Kind) {
  switch (Kind) {
  case ValueOwnershipKind::Trivial:
    return os << "Trivial";
  case ValueOwnershipKind::Unowned:
    return os << "Unowned";
  case ValueOwnershipKind::Owned:
    return os << "Owned";
  case ValueOwnershipKind::Guaranteed:
    return os << "Guaranteed";
  case ValueOwnershipKind::Undef:
    return os << "Undef";
  }
}

Optional<ValueOwnershipKind>
swift::ValueOwnershipKindMerge(Optional<ValueOwnershipKind> LHS,
                               Optional<ValueOwnershipKind> RHS) {
  if (!LHS.hasValue() || !RHS.hasValue())
    return NoneType::None;
  auto LHSVal = LHS.getValue();
  auto RHSVal = RHS.getValue();

  // Undef merges with anything.
  if (LHSVal == ValueOwnershipKind::Undef) {
    return RHSVal;
  }
  // Undef merges with anything.
  if (RHSVal == ValueOwnershipKind::Undef) {
    return LHSVal;
  }

  return (LHSVal == RHSVal) ? LHS : None;
}

//===----------------------------------------------------------------------===//
//                 Instruction ValueOwnershipKind Computation
//===----------------------------------------------------------------------===//

namespace {

class ValueOwnershipKindVisitor
    : public SILVisitor<ValueOwnershipKindVisitor,
                        Optional<ValueOwnershipKind>> {

public:
  ValueOwnershipKindVisitor() = default;
  ~ValueOwnershipKindVisitor() = default;
  ValueOwnershipKindVisitor(const ValueOwnershipKindVisitor &) = delete;
  ValueOwnershipKindVisitor(ValueOwnershipKindVisitor &&) = delete;

  Optional<ValueOwnershipKind> visitForwardingInst(SILInstruction *I);
  Optional<ValueOwnershipKind> visitPHISILArgument(SILArgument *Arg);

  Optional<ValueOwnershipKind> visitValueBase(ValueBase *V) {
    llvm_unreachable("unimplemented method on ValueBaseOwnershipVisitor");
  }
#define VALUE(Id, Parent)                                                      \
  Optional<ValueOwnershipKind> visit##Id(Id *ID);
#include "swift/SIL/SILNodes.def"
};

} // end anonymous namespace

#define CONSTANT_OWNERSHIP_INST(OWNERSHIP, INST)                               \
  Optional<ValueOwnershipKind> ValueOwnershipKindVisitor::visit##INST##Inst(   \
      INST##Inst *Arg) {                                                       \
    assert(Arg->hasValue() && "Expected to have a result");                    \
    if (ValueOwnershipKind::OWNERSHIP == ValueOwnershipKind::Trivial) {        \
      assert((Arg->getType().isAddress() ||                                    \
              Arg->getType().isTrivial(Arg->getModule())) &&                   \
             "Trivial ownership requires a trivial type or an address");       \
    }                                                                          \
    return ValueOwnershipKind::OWNERSHIP;                                      \
  }
CONSTANT_OWNERSHIP_INST(Guaranteed, BeginBorrow)
CONSTANT_OWNERSHIP_INST(Guaranteed, LoadBorrow)
CONSTANT_OWNERSHIP_INST(Guaranteed, StructExtract)
CONSTANT_OWNERSHIP_INST(Guaranteed, TupleExtract)
CONSTANT_OWNERSHIP_INST(Guaranteed, UncheckedEnumData)
CONSTANT_OWNERSHIP_INST(Owned, AllocBox)
CONSTANT_OWNERSHIP_INST(Owned, AllocExistentialBox)
CONSTANT_OWNERSHIP_INST(Owned, AllocRef)
CONSTANT_OWNERSHIP_INST(Owned, AllocRefDynamic)
CONSTANT_OWNERSHIP_INST(Owned, AllocValueBuffer)
CONSTANT_OWNERSHIP_INST(Owned, CopyBlock)
CONSTANT_OWNERSHIP_INST(Owned, CopyValue)
CONSTANT_OWNERSHIP_INST(Owned, CopyUnownedValue)
CONSTANT_OWNERSHIP_INST(Owned, LoadUnowned)
CONSTANT_OWNERSHIP_INST(Owned, LoadWeak)
CONSTANT_OWNERSHIP_INST(Owned, PartialApply)
CONSTANT_OWNERSHIP_INST(Owned, StrongPin)
// All addresses have trivial ownership. The values stored at the address may
// not though.
CONSTANT_OWNERSHIP_INST(Trivial, AddressToPointer)
CONSTANT_OWNERSHIP_INST(Trivial, AllocStack)
CONSTANT_OWNERSHIP_INST(Trivial, BindMemory)
CONSTANT_OWNERSHIP_INST(Trivial, BridgeObjectToWord)
CONSTANT_OWNERSHIP_INST(Trivial, ClassMethod)
CONSTANT_OWNERSHIP_INST(Trivial, DynamicMethod)
CONSTANT_OWNERSHIP_INST(Trivial, ExistentialMetatype)
CONSTANT_OWNERSHIP_INST(Trivial, FloatLiteral)
CONSTANT_OWNERSHIP_INST(Trivial, FunctionRef)
CONSTANT_OWNERSHIP_INST(Trivial, GlobalAddr)
CONSTANT_OWNERSHIP_INST(Trivial, IndexAddr)
CONSTANT_OWNERSHIP_INST(Trivial, IndexRawPointer)
CONSTANT_OWNERSHIP_INST(Trivial, InitBlockStorageHeader)
CONSTANT_OWNERSHIP_INST(Trivial, InitEnumDataAddr)
CONSTANT_OWNERSHIP_INST(Trivial, InitExistentialAddr)
CONSTANT_OWNERSHIP_INST(Trivial, InitExistentialMetatype)
CONSTANT_OWNERSHIP_INST(Trivial, IntegerLiteral)
CONSTANT_OWNERSHIP_INST(Trivial, IsNonnull)
CONSTANT_OWNERSHIP_INST(Trivial, IsUnique)
CONSTANT_OWNERSHIP_INST(Trivial, IsUniqueOrPinned)
CONSTANT_OWNERSHIP_INST(Trivial, MarkFunctionEscape)
CONSTANT_OWNERSHIP_INST(Trivial, MarkUninitialized)
CONSTANT_OWNERSHIP_INST(Trivial, MarkUninitializedBehavior)
CONSTANT_OWNERSHIP_INST(Trivial, Metatype)
CONSTANT_OWNERSHIP_INST(Trivial, ObjCExistentialMetatypeToObject) // Is this right?
CONSTANT_OWNERSHIP_INST(Trivial, ObjCMetatypeToObject) // Is this right?
CONSTANT_OWNERSHIP_INST(Trivial, ObjCProtocol) // Is this right?
CONSTANT_OWNERSHIP_INST(Trivial, ObjCToThickMetatype)
CONSTANT_OWNERSHIP_INST(Trivial, OpenExistentialAddr)
CONSTANT_OWNERSHIP_INST(Trivial, OpenExistentialBox)
CONSTANT_OWNERSHIP_INST(Trivial, OpenExistentialMetatype)
CONSTANT_OWNERSHIP_INST(Trivial, PointerToAddress)
CONSTANT_OWNERSHIP_INST(Trivial, PointerToThinFunction)
CONSTANT_OWNERSHIP_INST(Trivial, ProjectBlockStorage)
CONSTANT_OWNERSHIP_INST(Trivial, ProjectBox)
CONSTANT_OWNERSHIP_INST(Trivial, ProjectExistentialBox)
CONSTANT_OWNERSHIP_INST(Trivial, ProjectValueBuffer)
CONSTANT_OWNERSHIP_INST(Trivial, RefElementAddr)
CONSTANT_OWNERSHIP_INST(Trivial, RefTailAddr)
CONSTANT_OWNERSHIP_INST(Trivial, RefToRawPointer)
CONSTANT_OWNERSHIP_INST(Trivial, RefToUnmanaged)
CONSTANT_OWNERSHIP_INST(Trivial, SelectEnumAddr)
CONSTANT_OWNERSHIP_INST(Trivial, StringLiteral)
CONSTANT_OWNERSHIP_INST(Trivial, StructElementAddr)
CONSTANT_OWNERSHIP_INST(Trivial, SuperMethod)
CONSTANT_OWNERSHIP_INST(Trivial, TailAddr)
CONSTANT_OWNERSHIP_INST(Trivial, ThickToObjCMetatype)
CONSTANT_OWNERSHIP_INST(Trivial, ThinFunctionToPointer)
CONSTANT_OWNERSHIP_INST(Trivial, ThinToThickFunction)
CONSTANT_OWNERSHIP_INST(Trivial, TupleElementAddr)
CONSTANT_OWNERSHIP_INST(Trivial, UncheckedAddrCast)
CONSTANT_OWNERSHIP_INST(Trivial, UncheckedBitwiseCast)
CONSTANT_OWNERSHIP_INST(Trivial, UncheckedRefCastAddr)
CONSTANT_OWNERSHIP_INST(Trivial, UncheckedTakeEnumDataAddr)
CONSTANT_OWNERSHIP_INST(Trivial, UncheckedTrivialBitCast)
CONSTANT_OWNERSHIP_INST(Trivial, UnconditionalCheckedCastAddr)
CONSTANT_OWNERSHIP_INST(Trivial, ValueMetatype)
CONSTANT_OWNERSHIP_INST(Trivial, WitnessMethod)
// TODO: It would be great to get rid of these.
CONSTANT_OWNERSHIP_INST(Unowned, RawPointerToRef)
CONSTANT_OWNERSHIP_INST(Unowned, RefToUnowned)
CONSTANT_OWNERSHIP_INST(Unowned, UnmanagedToRef)
CONSTANT_OWNERSHIP_INST(Unowned, UnownedToRef)
#undef CONSTANT_OWNERSHIP_INST

// These are instructions that do not have any result, so we should never reach
// this point in the code since we need a SILValue to compute
// ValueOwnershipKind. We define methods so that all instructions have a method
// on the visitor (causing the compiler to warn if a new instruction is added
// within a method being added).
#define NO_RESULT_OWNERSHIP_INST(INST)                                         \
  Optional<ValueOwnershipKind> ValueOwnershipKindVisitor::visit##INST##Inst(   \
      INST##Inst *I) {                                                         \
    assert(!I->hasValue() && "Expected an instruction without a result");      \
    llvm_unreachable("Instruction without a result can not have ownership");   \
  }
NO_RESULT_OWNERSHIP_INST(DeallocStack)
NO_RESULT_OWNERSHIP_INST(DeallocRef)
NO_RESULT_OWNERSHIP_INST(DeallocPartialRef)
NO_RESULT_OWNERSHIP_INST(DeallocValueBuffer)
NO_RESULT_OWNERSHIP_INST(DeallocBox)
NO_RESULT_OWNERSHIP_INST(DeallocExistentialBox)
NO_RESULT_OWNERSHIP_INST(EndBorrow)
NO_RESULT_OWNERSHIP_INST(Store)
NO_RESULT_OWNERSHIP_INST(StoreWeak)
NO_RESULT_OWNERSHIP_INST(StoreUnowned)
NO_RESULT_OWNERSHIP_INST(StoreBorrow)
NO_RESULT_OWNERSHIP_INST(Assign)
NO_RESULT_OWNERSHIP_INST(DebugValue)
NO_RESULT_OWNERSHIP_INST(DebugValueAddr)
NO_RESULT_OWNERSHIP_INST(CopyAddr)
NO_RESULT_OWNERSHIP_INST(DestroyAddr)
NO_RESULT_OWNERSHIP_INST(StrongRetain)
NO_RESULT_OWNERSHIP_INST(StrongRelease)
NO_RESULT_OWNERSHIP_INST(StrongRetainUnowned)
NO_RESULT_OWNERSHIP_INST(StrongUnpin)
NO_RESULT_OWNERSHIP_INST(UnownedRetain)
NO_RESULT_OWNERSHIP_INST(UnownedRelease)
NO_RESULT_OWNERSHIP_INST(RetainValue)
NO_RESULT_OWNERSHIP_INST(ReleaseValue)
NO_RESULT_OWNERSHIP_INST(SetDeallocating)
NO_RESULT_OWNERSHIP_INST(AutoreleaseValue)
NO_RESULT_OWNERSHIP_INST(FixLifetime)
NO_RESULT_OWNERSHIP_INST(DestroyValue)
NO_RESULT_OWNERSHIP_INST(AllocGlobal)
NO_RESULT_OWNERSHIP_INST(InjectEnumAddr)
NO_RESULT_OWNERSHIP_INST(DeinitExistentialAddr)
NO_RESULT_OWNERSHIP_INST(CondFail)

// Terminators. These do not produce SILValue, so they do not have a
// ValueOwnershipKind. They do have ownership implications in terms of the
// SILArguments that they feed into. But that code is in SILArgument.
NO_RESULT_OWNERSHIP_INST(Unreachable)
NO_RESULT_OWNERSHIP_INST(Return)
NO_RESULT_OWNERSHIP_INST(Throw)
NO_RESULT_OWNERSHIP_INST(TryApply)
NO_RESULT_OWNERSHIP_INST(Branch)
NO_RESULT_OWNERSHIP_INST(CondBranch)
NO_RESULT_OWNERSHIP_INST(SwitchValue)
NO_RESULT_OWNERSHIP_INST(SwitchEnum)
NO_RESULT_OWNERSHIP_INST(SwitchEnumAddr)
NO_RESULT_OWNERSHIP_INST(DynamicMethodBranch)
NO_RESULT_OWNERSHIP_INST(CheckedCastBranch)
NO_RESULT_OWNERSHIP_INST(CheckedCastAddrBranch)
#undef NO_RESULT_OWNERSHIP_INST

// For a forwarding instruction, we loop over all operands and make sure they
// are all the same.
Optional<ValueOwnershipKind>
ValueOwnershipKindVisitor::visitForwardingInst(SILInstruction *I) {
  ArrayRef<Operand> Ops = I->getAllOperands();
  if (Ops.empty())
    llvm_unreachable("All forwarding insts should have operands");
  Optional<ValueOwnershipKind> Base = Ops[0].get().getOwnershipKind();
  if (!Base)
    return None;

  for (const Operand &Op : Ops.slice(1)) {
    if (!(Base = ValueOwnershipKindMerge(Base, Op.get().getOwnershipKind()))) {
      return None;
    }
  }
  return Base;
}

#define FORWARDING_OWNERSHIP_INST(INST)                                        \
  Optional<ValueOwnershipKind> ValueOwnershipKindVisitor::visit##INST##Inst(   \
      INST##Inst *I) {                                                         \
    assert(I->hasValue() && "Expected to have a value");                       \
    return visitForwardingInst(I);                                             \
  }
FORWARDING_OWNERSHIP_INST(BridgeObjectToRef)
FORWARDING_OWNERSHIP_INST(ConvertFunction)
FORWARDING_OWNERSHIP_INST(Enum)
FORWARDING_OWNERSHIP_INST(InitExistentialRef)
FORWARDING_OWNERSHIP_INST(OpenExistentialRef)
FORWARDING_OWNERSHIP_INST(RefToBridgeObject)
FORWARDING_OWNERSHIP_INST(SelectEnum)
FORWARDING_OWNERSHIP_INST(SelectValue)
FORWARDING_OWNERSHIP_INST(Struct)
FORWARDING_OWNERSHIP_INST(Tuple)
FORWARDING_OWNERSHIP_INST(UncheckedRefCast)
FORWARDING_OWNERSHIP_INST(UnconditionalCheckedCast)
FORWARDING_OWNERSHIP_INST(Upcast)
#undef FORWARDING_OWNERSHIP_INST

Optional<ValueOwnershipKind>
ValueOwnershipKindVisitor::visitSILUndef(SILUndef *Arg) {
  return ValueOwnershipKind::Undef;
}

Optional<ValueOwnershipKind>
ValueOwnershipKindVisitor::visitPHISILArgument(SILArgument *Arg) {
  llvm_unreachable("unimplemented");
}

Optional<ValueOwnershipKind>
ValueOwnershipKindVisitor::visitSILArgument(SILArgument *Arg) {
  // If we have a PHI node, we need to look at our predecessors.
  if (!Arg->isFunctionArg()) {
    return visitPHISILArgument(Arg);
  }

  // Otherwise, we need to discover our ownership kind from our function
  // signature.
  switch (Arg->getArgumentConvention()) {
  // These involve addresses and from an ownership perspective, addresses are
  // trivial. This is distinct from the ownership properties of the values that
  // they may contain.
  case SILArgumentConvention::Indirect_In:
  case SILArgumentConvention::Indirect_In_Guaranteed:
  case SILArgumentConvention::Indirect_Inout:
  case SILArgumentConvention::Indirect_InoutAliasable:
  case SILArgumentConvention::Indirect_Out:
    return ValueOwnershipKind::Trivial;
  case SILArgumentConvention::Direct_Owned:
    return ValueOwnershipKind::Owned;
  case SILArgumentConvention::Direct_Unowned:
    if (Arg->getType().isTrivial(Arg->getModule()))
      return ValueOwnershipKind::Trivial;
    return ValueOwnershipKind::Unowned;
  case SILArgumentConvention::Direct_Guaranteed:
    return ValueOwnershipKind::Guaranteed;
  case SILArgumentConvention::Direct_Deallocating:
    llvm_unreachable("No ownership associated with deallocating");
  }
}

// This is a forwarding instruction through only one of its arguments.
Optional<ValueOwnershipKind>
ValueOwnershipKindVisitor::visitMarkDependenceInst(MarkDependenceInst *MDI) {
  return MDI->getValue().getOwnershipKind();
}

Optional<ValueOwnershipKind>
ValueOwnershipKindVisitor::visitApplyInst(ApplyInst *AI) {
  auto Result = AI->getSingleResult();
  assert(Result && "SIL for now only has single results");
  switch (Result->getConvention()) {
  case ResultConvention::Indirect:
    return None;
  case ResultConvention::Autoreleased:
  case ResultConvention::Owned:
    return ValueOwnershipKind::Owned;
  case ResultConvention::Unowned:
  case ResultConvention::UnownedInnerPointer:
    return ValueOwnershipKind::Unowned;
  }
}

Optional<ValueOwnershipKind>
ValueOwnershipKindVisitor::visitLoadInst(LoadInst *LI) {
  switch (LI->getOwnershipQualifier()) {
  case LoadOwnershipQualifier::Take:
  case LoadOwnershipQualifier::Copy:
    return ValueOwnershipKind::Owned;
  case LoadOwnershipQualifier::Unqualified:
    return None;
  case LoadOwnershipQualifier::Trivial:
    return ValueOwnershipKind::Trivial;
  }
}

Optional<ValueOwnershipKind> SILValue::getOwnershipKind() const {
  // Once we have multiple return values, this must be changed.
  return ValueOwnershipKindVisitor().visit(const_cast<ValueBase *>(Value));
}

//===----------------------------------------------------------------------===//
//                   Builtin OwnershipValueKind Computation
//===----------------------------------------------------------------------===//

namespace {

struct ValueOwnershipKindBuiltinVisitor
    : SILBuiltinVisitor<ValueOwnershipKindBuiltinVisitor, ValueOwnershipKind> {

  ValueOwnershipKind visitLLVMIntrinsic(BuiltinInst *BI,
                                        llvm::Intrinsic::ID ID) {
    // LLVM intrinsics do not traffic in ownership, so if we have a result, it
    // must be trivial.
    assert(BI->hasValue() && "Can only get here if we have a SILValue");
    assert(BI->getType().isTrivial(BI->getModule()) &&
           "LLVM intrinsics should always be trivial");
    return ValueOwnershipKind::Trivial;
  }

#define BUILTIN(ID, NAME, ATTRS)                                               \
  ValueOwnershipKind visit##ID(BuiltinInst *BI, StringRef Attr);
#include "swift/AST/Builtins.def"
};

} // end anonymous namespace

#define CONSTANT_OWNERSHIP_BUILTIN(OWNERSHIP, ID)                              \
  ValueOwnershipKind ValueOwnershipKindBuiltinVisitor::visit##ID(              \
      BuiltinInst *BI, StringRef Attr) {                                       \
    assert(BI->hasValue() && "Expected to have type");                         \
    if (ValueOwnershipKind::OWNERSHIP == ValueOwnershipKind::Trivial) {        \
      assert(BI->getType().isTrivial(BI->getModule()) &&                       \
             "Only trivial types can have trivial ownership");                 \
    } else {                                                                   \
      assert(!BI->getType().isTrivial(BI->getModule()) &&                      \
             "Only non trivial types can have non trivial ownership");         \
    }                                                                          \
    return ValueOwnershipKind::OWNERSHIP;                                      \
  }
CONSTANT_OWNERSHIP_BUILTIN(Owned, Take)
CONSTANT_OWNERSHIP_BUILTIN(Owned, TryPin)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, AShr)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, Add)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, And)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, AssumeNonNegative)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, BitCast)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, ExactSDiv)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, ExactUDiv)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, FAdd)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, FCMP_OEQ)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, FCMP_OGE)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, FCMP_OGT)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, FCMP_OLE)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, FCMP_OLT)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, FCMP_ONE)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, FCMP_UEQ)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, FCMP_UGE)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, FCMP_UGT)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, FCMP_ULE)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, FCMP_ULT)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, FCMP_UNE)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, FDiv)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, FMul)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, FNeg)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, FPExt)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, FPToSI)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, FPToUI)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, FPTrunc)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, FRem)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, FSub)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, ICMP_EQ)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, ICMP_NE)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, ICMP_SGE)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, ICMP_SGT)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, ICMP_SLE)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, ICMP_SLT)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, ICMP_UGE)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, ICMP_UGT)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, ICMP_ULE)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, ICMP_ULT)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, IntToPtr)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, LShr)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, Load)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, LoadRaw)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, Mul)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, Or)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, PtrToInt)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, SAddOver)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, SDiv)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, SExt)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, SExtOrBitCast)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, SIToFP)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, SMulOver)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, SRem)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, SSubOver)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, Shl)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, Sub)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, Trunc)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, TruncOrBitCast)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, UAddOver)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, UDiv)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, UIToFP)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, UMulOver)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, URem)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, USubOver)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, Xor)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, ZExt)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, ZExtOrBitCast)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, FCMP_ORD)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, FCMP_UNO)
CONSTANT_OWNERSHIP_BUILTIN(Unowned, CastToUnknownObject)
CONSTANT_OWNERSHIP_BUILTIN(Unowned, CastFromUnknownObject)
CONSTANT_OWNERSHIP_BUILTIN(Unowned, CastToNativeObject)
CONSTANT_OWNERSHIP_BUILTIN(Unowned, CastFromNativeObject)
CONSTANT_OWNERSHIP_BUILTIN(Unowned, CastToBridgeObject)
CONSTANT_OWNERSHIP_BUILTIN(Unowned, CastReferenceFromBridgeObject)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, CastBitPatternFromBridgeObject)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, BridgeToRawPointer)
CONSTANT_OWNERSHIP_BUILTIN(Unowned, BridgeFromRawPointer)
CONSTANT_OWNERSHIP_BUILTIN(Unowned, CastReference)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, AddressOf)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, GepRaw)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, Gep)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, GetTailAddr)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, IsUnique)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, IsUniqueOrPinned)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, IsUnique_native)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, IsUniqueOrPinned_native)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, BindMemory)
CONSTANT_OWNERSHIP_BUILTIN(Owned, AllocWithTailElems)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, ProjectTailElems)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, IsOptionalType)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, Sizeof)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, Strideof)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, IsPOD)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, Alignof)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, AllocRaw)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, AssertConf)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, UToSCheckedTrunc)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, SToSCheckedTrunc)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, SToUCheckedTrunc)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, UToUCheckedTrunc)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, SUCheckedConversion)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, USCheckedConversion)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, IntToFPWithOverflow)

/// AtomicRMW has type (Builtin.RawPointer, T) -> T. But it provides overloads
/// for integer or rawpointer, so it should be trivial.
CONSTANT_OWNERSHIP_BUILTIN(Trivial, AtomicRMW)

CONSTANT_OWNERSHIP_BUILTIN(Trivial, CondUnreachable)
CONSTANT_OWNERSHIP_BUILTIN(Guaranteed, UnsafeGuaranteed)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, UnsafeGuaranteedEnd)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, GetObjCTypeEncoding)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, CanBeObjCClass)
#undef CONSTANT_OWNERSHIP_BUILTIN

#define NO_OWNERSHIP_BUILTIN(ID)                                               \
  ValueOwnershipKind ValueOwnershipKindBuiltinVisitor::visit##ID(              \
      BuiltinInst *BI, StringRef Attr) {                                       \
    assert(!BI->hasValue() && "Expected to not have a value");                 \
    llvm_unreachable("Should never get a SILValue to a Builtin "               \
                     "without result");                                        \
  }
NO_OWNERSHIP_BUILTIN(Unreachable)
NO_OWNERSHIP_BUILTIN(DestroyArray)
NO_OWNERSHIP_BUILTIN(CopyArray)
NO_OWNERSHIP_BUILTIN(TakeArrayFrontToBack)
NO_OWNERSHIP_BUILTIN(TakeArrayBackToFront)
NO_OWNERSHIP_BUILTIN(WillThrow)
NO_OWNERSHIP_BUILTIN(UnexpectedError)
NO_OWNERSHIP_BUILTIN(ErrorInMain)
NO_OWNERSHIP_BUILTIN(DeallocRaw)
NO_OWNERSHIP_BUILTIN(Fence)
NO_OWNERSHIP_BUILTIN(OnFastPath)
NO_OWNERSHIP_BUILTIN(Retain)
NO_OWNERSHIP_BUILTIN(Release)
NO_OWNERSHIP_BUILTIN(CondFail)
NO_OWNERSHIP_BUILTIN(FixLifetime)
NO_OWNERSHIP_BUILTIN(Autorelease)
NO_OWNERSHIP_BUILTIN(Unpin)
NO_OWNERSHIP_BUILTIN(Destroy)
NO_OWNERSHIP_BUILTIN(Assign)
NO_OWNERSHIP_BUILTIN(Init)
NO_OWNERSHIP_BUILTIN(AtomicStore)
NO_OWNERSHIP_BUILTIN(StaticReport)
NO_OWNERSHIP_BUILTIN(Once)
#undef NO_OWNERSHIP_BUILTIN

// Check all of these...
#define UNOWNED_OR_TRIVIAL_DEPENDING_ON_RESULT(ID)                             \
  ValueOwnershipKind ValueOwnershipKindBuiltinVisitor::visit##ID(              \
      BuiltinInst *BI, StringRef Attr) {                                       \
    if (BI->getType().isTrivial(BI->getModule())) {                            \
      return ValueOwnershipKind::Trivial;                                      \
    }                                                                          \
    return ValueOwnershipKind::Unowned;                                        \
  }
UNOWNED_OR_TRIVIAL_DEPENDING_ON_RESULT(ReinterpretCast)
UNOWNED_OR_TRIVIAL_DEPENDING_ON_RESULT(CmpXChg)
UNOWNED_OR_TRIVIAL_DEPENDING_ON_RESULT(AtomicLoad)
UNOWNED_OR_TRIVIAL_DEPENDING_ON_RESULT(ExtractElement)
UNOWNED_OR_TRIVIAL_DEPENDING_ON_RESULT(InsertElement)
UNOWNED_OR_TRIVIAL_DEPENDING_ON_RESULT(ZeroInitializer)
#undef UNOWNED_OR_TRIVIAL_DEPENDING_ON_RESULT

Optional<ValueOwnershipKind>
ValueOwnershipKindVisitor::visitBuiltinInst(BuiltinInst *BI) {
  // For now, just conservatively say builtins are None. We need to use a
  // builtin in here to guarantee correctness.
  return ValueOwnershipKindBuiltinVisitor().visit(BI);
}
