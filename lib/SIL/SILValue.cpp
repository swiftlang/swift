//===--- SILValue.cpp - Implementation for SILValue -----------------------===//
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

#include "swift/SIL/SILValue.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILBuiltinVisitor.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILModule.h"
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

ValueOwnershipKind::ValueOwnershipKind(SILModule &M, SILType Type,
                                       SILArgumentConvention Convention)
    : Value() {
  switch (Convention) {
  case SILArgumentConvention::Indirect_In:
    Value = SILModuleConventions(M).useLoweredAddresses()
      ? ValueOwnershipKind::Trivial
      : ValueOwnershipKind::Owned;
    break;
  case SILArgumentConvention::Indirect_In_Guaranteed:
    Value = SILModuleConventions(M).useLoweredAddresses()
      ? ValueOwnershipKind::Trivial
      : ValueOwnershipKind::Guaranteed;
    break;
  case SILArgumentConvention::Indirect_Inout:
  case SILArgumentConvention::Indirect_InoutAliasable:
  case SILArgumentConvention::Indirect_Out:
    Value = ValueOwnershipKind::Trivial;
    return;
  case SILArgumentConvention::Direct_Owned:
    Value = ValueOwnershipKind::Owned;
    return;
  case SILArgumentConvention::Direct_Unowned:
    Value = Type.isTrivial(M) ? ValueOwnershipKind::Trivial
                              : ValueOwnershipKind::Unowned;
    return;
  case SILArgumentConvention::Direct_Guaranteed:
    Value = ValueOwnershipKind::Guaranteed;
    return;
  case SILArgumentConvention::Direct_Deallocating:
    llvm_unreachable("Not handled");
  }
}

llvm::raw_ostream &swift::operator<<(llvm::raw_ostream &os,
                                     ValueOwnershipKind Kind) {
  switch (Kind) {
  case ValueOwnershipKind::Trivial:
    return os << "trivial";
  case ValueOwnershipKind::Unowned:
    return os << "unowned";
  case ValueOwnershipKind::Owned:
    return os << "owned";
  case ValueOwnershipKind::Guaranteed:
    return os << "guaranteed";
  case ValueOwnershipKind::Any:
    return os << "any";
  }

  llvm_unreachable("Unhandled ValueOwnershipKind in switch.");
}

Optional<ValueOwnershipKind>
ValueOwnershipKind::merge(ValueOwnershipKind RHS) const {
  auto LHSVal = Value;
  auto RHSVal = RHS.Value;

  // Any merges with anything.
  if (LHSVal == ValueOwnershipKind::Any) {
    return ValueOwnershipKind(RHSVal);
  }
  // Any merges with anything.
  if (RHSVal == ValueOwnershipKind::Any) {
    return ValueOwnershipKind(LHSVal);
  }

  return (LHSVal == RHSVal) ? Optional<ValueOwnershipKind>(*this) : None;
}

//===----------------------------------------------------------------------===//
//                 Instruction ValueOwnershipKind Computation
//===----------------------------------------------------------------------===//

namespace {

class ValueOwnershipKindVisitor
    : public SILVisitor<ValueOwnershipKindVisitor,
                        ValueOwnershipKind> {

public:
  ValueOwnershipKindVisitor() = default;
  ~ValueOwnershipKindVisitor() = default;
  ValueOwnershipKindVisitor(const ValueOwnershipKindVisitor &) = delete;
  ValueOwnershipKindVisitor(ValueOwnershipKindVisitor &&) = delete;

  ValueOwnershipKind visitForwardingInst(SILInstruction *I,
                                         ArrayRef<Operand> Ops);
  ValueOwnershipKind visitForwardingInst(SILInstruction *I) {
    return visitForwardingInst(I, I->getAllOperands());
  }

  ValueOwnershipKind visitValueBase(ValueBase *V) {
    llvm_unreachable("unimplemented method on ValueBaseOwnershipVisitor");
  }
#define VALUE(Id, Parent) ValueOwnershipKind visit##Id(Id *ID);
#include "swift/SIL/SILNodes.def"
};

} // end anonymous namespace

#define CONSTANT_OWNERSHIP_INST(OWNERSHIP, INST)                               \
  ValueOwnershipKind ValueOwnershipKindVisitor::visit##INST##Inst(   \
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
CONSTANT_OWNERSHIP_INST(Owned, AllocBox)
CONSTANT_OWNERSHIP_INST(Owned, AllocExistentialBox)
CONSTANT_OWNERSHIP_INST(Owned, AllocRef)
CONSTANT_OWNERSHIP_INST(Owned, AllocRefDynamic)
CONSTANT_OWNERSHIP_INST(Trivial, AllocValueBuffer)
CONSTANT_OWNERSHIP_INST(Owned, CopyBlock)
CONSTANT_OWNERSHIP_INST(Owned, CopyValue)
CONSTANT_OWNERSHIP_INST(Owned, CopyUnownedValue)
CONSTANT_OWNERSHIP_INST(Owned, LoadUnowned)
CONSTANT_OWNERSHIP_INST(Owned, LoadWeak)
CONSTANT_OWNERSHIP_INST(Owned, PartialApply)
CONSTANT_OWNERSHIP_INST(Owned, StrongPin)
CONSTANT_OWNERSHIP_INST(Owned, ThinToThickFunction)

// One would think that these /should/ be unowned. In truth they are owned since
// objc metatypes do not go through the retain/release fast path. In their
// implementations of retain/release nothing happens, so this is safe.
//
// You could even have an optimization that just always removed retain/release
// operations on these objects.
CONSTANT_OWNERSHIP_INST(Owned, ObjCExistentialMetatypeToObject)
CONSTANT_OWNERSHIP_INST(Owned, ObjCMetatypeToObject)

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
CONSTANT_OWNERSHIP_INST(Trivial, MarkUninitializedBehavior)
CONSTANT_OWNERSHIP_INST(Trivial, Metatype)
CONSTANT_OWNERSHIP_INST(Trivial, ObjCProtocol)           // Is this right?
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
CONSTANT_OWNERSHIP_INST(Trivial, TupleElementAddr)
CONSTANT_OWNERSHIP_INST(Trivial, UncheckedAddrCast)
CONSTANT_OWNERSHIP_INST(Trivial, UncheckedRefCastAddr)
CONSTANT_OWNERSHIP_INST(Trivial, UncheckedTakeEnumDataAddr)
CONSTANT_OWNERSHIP_INST(Trivial, UncheckedTrivialBitCast)
CONSTANT_OWNERSHIP_INST(Trivial, UnconditionalCheckedCastAddr)
CONSTANT_OWNERSHIP_INST(Trivial, ValueMetatype)
CONSTANT_OWNERSHIP_INST(Trivial, WitnessMethod)
CONSTANT_OWNERSHIP_INST(Trivial, StoreBorrow)
// TODO: It would be great to get rid of these.
CONSTANT_OWNERSHIP_INST(Unowned, RawPointerToRef)
CONSTANT_OWNERSHIP_INST(Unowned, RefToUnowned)
CONSTANT_OWNERSHIP_INST(Unowned, UnmanagedToRef)
CONSTANT_OWNERSHIP_INST(Unowned, UnownedToRef)
#undef CONSTANT_OWNERSHIP_INST

#define CONSTANT_OR_TRIVIAL_OWNERSHIP_INST(OWNERSHIP, INST)                    \
  ValueOwnershipKind ValueOwnershipKindVisitor::visit##INST##Inst(             \
      INST##Inst *I) {                                                         \
    if (I->getType().isTrivial(I->getModule())) {                              \
      return ValueOwnershipKind::Trivial;                                      \
    }                                                                          \
    return ValueOwnershipKind::OWNERSHIP;                                      \
  }
CONSTANT_OR_TRIVIAL_OWNERSHIP_INST(Guaranteed, StructExtract)
CONSTANT_OR_TRIVIAL_OWNERSHIP_INST(Guaranteed, TupleExtract)
CONSTANT_OR_TRIVIAL_OWNERSHIP_INST(Guaranteed, UncheckedEnumData)
#undef CONSTANT_OR_TRIVIAL_OWNERSHIP_INST

// These are instructions that do not have any result, so we should never reach
// this point in the code since we need a SILValue to compute
// ValueOwnershipKind. We define methods so that all instructions have a method
// on the visitor (causing the compiler to warn if a new instruction is added
// within a method being added).
#define NO_RESULT_OWNERSHIP_INST(INST)                                         \
  ValueOwnershipKind ValueOwnershipKindVisitor::visit##INST##Inst(   \
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
NO_RESULT_OWNERSHIP_INST(EndBorrowArgument)
NO_RESULT_OWNERSHIP_INST(Store)
NO_RESULT_OWNERSHIP_INST(StoreWeak)
NO_RESULT_OWNERSHIP_INST(StoreUnowned)
NO_RESULT_OWNERSHIP_INST(Assign)
NO_RESULT_OWNERSHIP_INST(DebugValue)
NO_RESULT_OWNERSHIP_INST(DebugValueAddr)
NO_RESULT_OWNERSHIP_INST(CopyAddr)
NO_RESULT_OWNERSHIP_INST(DestroyAddr)
NO_RESULT_OWNERSHIP_INST(StrongRetain)
NO_RESULT_OWNERSHIP_INST(StrongRelease)
NO_RESULT_OWNERSHIP_INST(StrongRetainUnowned)
NO_RESULT_OWNERSHIP_INST(StrongUnpin)
NO_RESULT_OWNERSHIP_INST(UnmanagedRetainValue)
NO_RESULT_OWNERSHIP_INST(UnmanagedReleaseValue)
NO_RESULT_OWNERSHIP_INST(UnmanagedAutoreleaseValue)
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

// For a forwarding instruction, we loop over all operands and make sure that
// all non-trivial values have the same ownership.
ValueOwnershipKind
ValueOwnershipKindVisitor::visitForwardingInst(SILInstruction *I,
                                               ArrayRef<Operand> Ops) {
  // A forwarding inst without operands must be trivial.
  if (Ops.empty())
    return ValueOwnershipKind::Trivial;

  // Find the first index where we have a trivial value.
  auto Iter =
    find_if(Ops,
            [&I](const Operand &Op) -> bool {
              if (I->isTypeDependentOperand(Op))
                return false;
              return Op.get().getOwnershipKind() != ValueOwnershipKind::Trivial;
            });
  // All trivial.
  if (Iter == Ops.end()) {
    return ValueOwnershipKind::Trivial;
  }

  // See if we have any Any. If we do, just return that for now.
  if (any_of(Ops,
             [&I](const Operand &Op) -> bool {
               if (I->isTypeDependentOperand(Op))
                 return false;
               return Op.get().getOwnershipKind() == ValueOwnershipKind::Any;
             }))
    return ValueOwnershipKind::Any;
  unsigned Index = std::distance(Ops.begin(), Iter);

  ValueOwnershipKind Base = Ops[Index].get().getOwnershipKind();

  for (const Operand &Op : Ops.slice(Index+1)) {
    if (I->isTypeDependentOperand(Op))
      continue;
    auto OpKind = Op.get().getOwnershipKind();
    if (OpKind.merge(ValueOwnershipKind::Trivial))
      continue;

    auto MergedValue = Base.merge(OpKind.Value);
    if (!MergedValue.hasValue()) {
      llvm_unreachable("Forwarding inst with mismatching ownership kinds?!");
    }
  }

  return Base;
}

#define FORWARDING_OWNERSHIP_INST(INST)                                        \
  ValueOwnershipKind ValueOwnershipKindVisitor::visit##INST##Inst(   \
      INST##Inst *I) {                                                         \
    assert(I->hasValue() && "Expected to have a value");                       \
    return visitForwardingInst(I);                                             \
  }
FORWARDING_OWNERSHIP_INST(BridgeObjectToRef)
FORWARDING_OWNERSHIP_INST(ConvertFunction)
FORWARDING_OWNERSHIP_INST(InitExistentialRef)
FORWARDING_OWNERSHIP_INST(OpenExistentialRef)
FORWARDING_OWNERSHIP_INST(RefToBridgeObject)
FORWARDING_OWNERSHIP_INST(SelectValue)
FORWARDING_OWNERSHIP_INST(Struct)
FORWARDING_OWNERSHIP_INST(Tuple)
FORWARDING_OWNERSHIP_INST(UncheckedRefCast)
FORWARDING_OWNERSHIP_INST(UnconditionalCheckedCast)
FORWARDING_OWNERSHIP_INST(Upcast)
FORWARDING_OWNERSHIP_INST(MarkUninitialized)
#undef FORWARDING_OWNERSHIP_INST

ValueOwnershipKind
ValueOwnershipKindVisitor::visitSelectEnumInst(SelectEnumInst *SEI) {
  // We handle this specially, since a select enum forwards only its case
  // values. We drop the first element since that is the condition element.
  return visitForwardingInst(SEI, SEI->getAllOperands().drop_front());
}

ValueOwnershipKind ValueOwnershipKindVisitor::visitUncheckedBitwiseCastInst(
    UncheckedBitwiseCastInst *UBCI) {
  ValueOwnershipKind OpOwnership = UBCI->getOperand().getOwnershipKind();
  bool ResultTypeIsTrivial = UBCI->getType().isTrivial(UBCI->getModule());

  // First check if our operand has a trivial value ownership kind...
  if (OpOwnership == ValueOwnershipKind::Trivial) {
    // If we do have a trivial value ownership kind, see if our result type is
    // trivial or non-trivial. If it is trivial, then we have trivial
    // ownership. Otherwise, we have unowned ownership since from an ownership
    // perspective, the value has instantaneously come into existence and
    // nothing has taken ownership of it.
    if (ResultTypeIsTrivial) {
      return ValueOwnershipKind::Trivial;
    }
    return ValueOwnershipKind::Unowned;
  }

  // If our operand has non-trivial ownership, but our result does, then of
  // course the result has trivial ownership.
  if (ResultTypeIsTrivial) {
    return ValueOwnershipKind::Trivial;
  }

  // Otherwise, we forward our ownership.
  return visitForwardingInst(UBCI);
}

// An enum without payload is trivial. One with non-trivial payload is
// forwarding.
ValueOwnershipKind
ValueOwnershipKindVisitor::visitEnumInst(EnumInst *EI) {
  if (!EI->hasOperand())
    return ValueOwnershipKind::Trivial;
  return visitForwardingInst(EI);
}

ValueOwnershipKind
ValueOwnershipKindVisitor::visitSILUndef(SILUndef *Arg) {
  return ValueOwnershipKind::Any;
}

ValueOwnershipKind
ValueOwnershipKindVisitor::visitSILPHIArgument(SILPHIArgument *Arg) {
  return Arg->getOwnershipKind();
}

ValueOwnershipKind
ValueOwnershipKindVisitor::visitSILFunctionArgument(SILFunctionArgument *Arg) {
  return Arg->getOwnershipKind();
}

// This is a forwarding instruction through only one of its arguments.
ValueOwnershipKind
ValueOwnershipKindVisitor::visitMarkDependenceInst(MarkDependenceInst *MDI) {
  return MDI->getValue().getOwnershipKind();
}

ValueOwnershipKind
ValueOwnershipKindVisitor::visitApplyInst(ApplyInst *AI) {
  SILModule &M = AI->getModule();
  bool IsTrivial = AI->getType().isTrivial(M);
  SILFunctionConventions fnConv(AI->getSubstCalleeType(), M);
  auto Results = fnConv.getDirectSILResults();
  // No results => empty tuple result => Trivial.
  if (Results.empty() || IsTrivial)
    return ValueOwnershipKind::Trivial;

  CanGenericSignature Sig = AI->getSubstCalleeType()->getGenericSignature();
  // Find the first index where we have a trivial value.
  auto Iter = find_if(Results, [&M, &Sig](const SILResultInfo &Info) -> bool {
    return Info.getOwnershipKind(M, Sig) != ValueOwnershipKind::Trivial;
  });
  // If we have all trivial, then we must be trivial.
  if (Iter == Results.end())
    return ValueOwnershipKind::Trivial;

  ValueOwnershipKind Base = Iter->getOwnershipKind(M);

  for (const SILResultInfo &ResultInfo :
       SILFunctionConventions::DirectSILResultRange(next(Iter),
                                                    Results.end())) {
    auto RKind = ResultInfo.getOwnershipKind(M, Sig);
    if (RKind.merge(ValueOwnershipKind::Trivial))
      continue;

    auto MergedValue = Base.merge(RKind.Value);
    if (!MergedValue.hasValue()) {
      llvm_unreachable("Forwarding inst with mismatching ownership kinds?!");
    }
  }

  return Base;
}

ValueOwnershipKind
ValueOwnershipKindVisitor::visitLoadInst(LoadInst *LI) {
  switch (LI->getOwnershipQualifier()) {
  case LoadOwnershipQualifier::Take:
  case LoadOwnershipQualifier::Copy:
    return ValueOwnershipKind::Owned;
  case LoadOwnershipQualifier::Unqualified:
    return ValueOwnershipKind::Any;
  case LoadOwnershipQualifier::Trivial:
    return ValueOwnershipKind::Trivial;
  }

  llvm_unreachable("Unhandled LoadOwnershipQualifier in switch.");
}

ValueOwnershipKind SILValue::getOwnershipKind() const {
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
// This returns a value at +1 that is destroyed strictly /after/ the
// UnsafeGuaranteedEnd. This provides the guarantee that we want.
CONSTANT_OWNERSHIP_BUILTIN(Owned, UnsafeGuaranteed)
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
CONSTANT_OWNERSHIP_BUILTIN(Trivial, OnFastPath)
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

// This is surprising, Builtin.unreachable returns a "Never" value which is
// trivially typed.
CONSTANT_OWNERSHIP_BUILTIN(Trivial, Unreachable)

/// AtomicRMW has type (Builtin.RawPointer, T) -> T. But it provides overloads
/// for integer or rawpointer, so it should be trivial.
CONSTANT_OWNERSHIP_BUILTIN(Trivial, AtomicRMW)

CONSTANT_OWNERSHIP_BUILTIN(Trivial, CondUnreachable)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, UnsafeGuaranteedEnd)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, GetObjCTypeEncoding)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, CanBeObjCClass)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, WillThrow)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, StaticReport)

CONSTANT_OWNERSHIP_BUILTIN(Trivial, DestroyArray)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, CopyArray)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, TakeArrayFrontToBack)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, TakeArrayBackToFront)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, UnexpectedError)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, ErrorInMain)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, DeallocRaw)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, Fence)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, Retain)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, Release)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, CondFail)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, FixLifetime)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, Autorelease)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, Unpin)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, Destroy)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, Assign)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, Init)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, AtomicStore)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, Once)
#undef CONSTANT_OWNERSHIP_BUILTIN

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

ValueOwnershipKind
ValueOwnershipKindVisitor::visitBuiltinInst(BuiltinInst *BI) {
  // For now, just conservatively say builtins are None. We need to use a
  // builtin in here to guarantee correctness.
  return ValueOwnershipKindBuiltinVisitor().visit(BI);
}
