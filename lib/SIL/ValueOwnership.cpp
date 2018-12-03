//===--- ValueOwnership.cpp -----------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/SIL/SILVisitor.h"
#include "swift/SIL/SILBuiltinVisitor.h"
#include "swift/SIL/SILModule.h"

using namespace swift;

//===----------------------------------------------------------------------===//
//                                 Interface
//===----------------------------------------------------------------------===//

namespace {

class ValueOwnershipKindClassifier
    : public SILValueVisitor<ValueOwnershipKindClassifier, ValueOwnershipKind> {

public:
  ValueOwnershipKindClassifier() = default;
  ~ValueOwnershipKindClassifier() = default;
  ValueOwnershipKindClassifier(const ValueOwnershipKindClassifier &) = delete;
  ValueOwnershipKindClassifier(ValueOwnershipKindClassifier &&) = delete;

  ValueOwnershipKind visitForwardingInst(SILInstruction *I,
                                         ArrayRef<Operand> Ops);
  ValueOwnershipKind visitForwardingInst(SILInstruction *I) {
    return visitForwardingInst(I, I->getAllOperands());
  }

#define VALUE(Id, Parent) ValueOwnershipKind visit##Id(Id *ID);
#include "swift/SIL/SILNodes.def"
};

} // end anonymous namespace

#define CONSTANT_OWNERSHIP_INST(OWNERSHIP, INST)                               \
  ValueOwnershipKind ValueOwnershipKindClassifier::visit##INST##Inst(          \
      INST##Inst *Arg) {                                                       \
    return ValueOwnershipKind::OWNERSHIP;                                      \
  }

#define NEVER_LOADABLE_CHECKED_REF_STORAGE(Name, ...) \
  CONSTANT_OWNERSHIP_INST(Owned, Load##Name)
#define ALWAYS_LOADABLE_CHECKED_REF_STORAGE(Name, ...) \
  CONSTANT_OWNERSHIP_INST(Unowned, RefTo##Name) \
  CONSTANT_OWNERSHIP_INST(Unowned, Name##ToRef) \
  CONSTANT_OWNERSHIP_INST(Owned, Copy##Name##Value)
#define SOMETIMES_LOADABLE_CHECKED_REF_STORAGE(Name, ...) \
  NEVER_LOADABLE_CHECKED_REF_STORAGE(Name, "...") \
  ALWAYS_LOADABLE_CHECKED_REF_STORAGE(Name, "...")
#define UNCHECKED_REF_STORAGE(Name, ...) \
  CONSTANT_OWNERSHIP_INST(Trivial, RefTo##Name) \
  CONSTANT_OWNERSHIP_INST(Unowned, Name##ToRef)
#include "swift/AST/ReferenceStorage.def"

CONSTANT_OWNERSHIP_INST(Guaranteed, BeginBorrow)
CONSTANT_OWNERSHIP_INST(Guaranteed, LoadBorrow)
CONSTANT_OWNERSHIP_INST(Owned, AllocBox)
CONSTANT_OWNERSHIP_INST(Owned, AllocExistentialBox)
CONSTANT_OWNERSHIP_INST(Owned, AllocRef)
CONSTANT_OWNERSHIP_INST(Owned, AllocRefDynamic)
CONSTANT_OWNERSHIP_INST(Trivial, AllocValueBuffer)
CONSTANT_OWNERSHIP_INST(Owned, CopyBlock)
CONSTANT_OWNERSHIP_INST(Owned, CopyBlockWithoutEscaping)
CONSTANT_OWNERSHIP_INST(Owned, CopyValue)
CONSTANT_OWNERSHIP_INST(Owned, KeyPath)
CONSTANT_OWNERSHIP_INST(Owned, PartialApply)
CONSTANT_OWNERSHIP_INST(Owned, InitExistentialValue)
CONSTANT_OWNERSHIP_INST(Owned, GlobalValue) // TODO: is this correct?

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
CONSTANT_OWNERSHIP_INST(Trivial, BeginAccess)
CONSTANT_OWNERSHIP_INST(Trivial, BridgeObjectToWord)
CONSTANT_OWNERSHIP_INST(Trivial, ClassMethod)
CONSTANT_OWNERSHIP_INST(Trivial, ClassifyBridgeObject)
CONSTANT_OWNERSHIP_INST(Trivial, ObjCMethod)
CONSTANT_OWNERSHIP_INST(Trivial, ExistentialMetatype)
CONSTANT_OWNERSHIP_INST(Trivial, FloatLiteral)
CONSTANT_OWNERSHIP_INST(Trivial, FunctionRef)
CONSTANT_OWNERSHIP_INST(Trivial, DynamicFunctionRef)
CONSTANT_OWNERSHIP_INST(Trivial, PreviousDynamicFunctionRef)
CONSTANT_OWNERSHIP_INST(Trivial, GlobalAddr)
CONSTANT_OWNERSHIP_INST(Trivial, IndexAddr)
CONSTANT_OWNERSHIP_INST(Trivial, IndexRawPointer)
CONSTANT_OWNERSHIP_INST(Trivial, InitEnumDataAddr)
CONSTANT_OWNERSHIP_INST(Trivial, InitExistentialAddr)
CONSTANT_OWNERSHIP_INST(Trivial, InitExistentialMetatype)
CONSTANT_OWNERSHIP_INST(Trivial, IntegerLiteral)
CONSTANT_OWNERSHIP_INST(Trivial, IsUnique)
CONSTANT_OWNERSHIP_INST(Trivial, IsEscapingClosure)
CONSTANT_OWNERSHIP_INST(Trivial, MarkUninitializedBehavior)
CONSTANT_OWNERSHIP_INST(Trivial, Metatype)
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
CONSTANT_OWNERSHIP_INST(Trivial, SelectEnumAddr)
CONSTANT_OWNERSHIP_INST(Trivial, StringLiteral)
CONSTANT_OWNERSHIP_INST(Trivial, StructElementAddr)
CONSTANT_OWNERSHIP_INST(Trivial, SuperMethod)
CONSTANT_OWNERSHIP_INST(Trivial, ObjCSuperMethod)
CONSTANT_OWNERSHIP_INST(Trivial, TailAddr)
CONSTANT_OWNERSHIP_INST(Trivial, ThickToObjCMetatype)
CONSTANT_OWNERSHIP_INST(Trivial, ThinFunctionToPointer)
CONSTANT_OWNERSHIP_INST(Trivial, TupleElementAddr)
CONSTANT_OWNERSHIP_INST(Trivial, UncheckedAddrCast)
CONSTANT_OWNERSHIP_INST(Trivial, UncheckedTakeEnumDataAddr)
CONSTANT_OWNERSHIP_INST(Trivial, UncheckedTrivialBitCast)
CONSTANT_OWNERSHIP_INST(Trivial, ValueMetatype)
CONSTANT_OWNERSHIP_INST(Trivial, WitnessMethod)
CONSTANT_OWNERSHIP_INST(Trivial, StoreBorrow)
CONSTANT_OWNERSHIP_INST(Trivial, ConvertEscapeToNoEscape)
CONSTANT_OWNERSHIP_INST(Unowned, InitBlockStorageHeader)
// TODO: It would be great to get rid of these.
CONSTANT_OWNERSHIP_INST(Unowned, RawPointerToRef)
CONSTANT_OWNERSHIP_INST(Unowned, ObjCProtocol)
CONSTANT_OWNERSHIP_INST(Unowned, ValueToBridgeObject)
#undef CONSTANT_OWNERSHIP_INST

#define CONSTANT_OR_TRIVIAL_OWNERSHIP_INST(OWNERSHIP, INST)                    \
  ValueOwnershipKind ValueOwnershipKindClassifier::visit##INST##Inst(          \
      INST##Inst *I) {                                                         \
    if (I->getType().isTrivial(I->getModule())) {                              \
      return ValueOwnershipKind::Trivial;                                      \
    }                                                                          \
    return ValueOwnershipKind::OWNERSHIP;                                      \
  }
CONSTANT_OR_TRIVIAL_OWNERSHIP_INST(Guaranteed, StructExtract)
CONSTANT_OR_TRIVIAL_OWNERSHIP_INST(Guaranteed, TupleExtract)
// OpenExistentialValue opens the boxed value inside an existential
// CoW box. The semantics of an existential CoW box implies that we
// can only consume the projected value inside the box if the box is
// unique. Since we do not know in general if the box is unique
// without additional work, in SIL we require opened archetypes to
// be borrowed sub-objects of the parent CoW box.
CONSTANT_OR_TRIVIAL_OWNERSHIP_INST(Guaranteed, OpenExistentialValue)
CONSTANT_OR_TRIVIAL_OWNERSHIP_INST(Guaranteed, OpenExistentialBoxValue)
CONSTANT_OR_TRIVIAL_OWNERSHIP_INST(Owned, UnconditionalCheckedCastValue)

// unchecked_bitwise_cast is a bitwise copy. It produces a trivial or unowned
// result.
//
// If the operand is nontrivial and the result is trivial, then it is the
// programmer's responsibility to use Builtin.fixLifetime.
//
// If both the operand and the result are nontrivial, then either the types must
// be compatible so that TBAA doesn't allow the destroy to be hoisted above uses
// of the cast, or the programmer must use Builtin.fixLifetime.
//
// FIXME
// -----
//
// SR-7175: Since we model this as unowned, then we must copy the
// value before use. This directly contradicts the semantics mentioned
// above since we will copy the value upon any use lest we use an
// unowned value in an owned or guaranteed way. So really all we will
// do here is perhaps add a copy slightly earlier unless the unowned
// value immediately is cast to something trivial. In such a case, we
// should be able to simplify the cast to just a trivial value and
// then eliminate the copy. That being said, we should investigate
// this since this is used in reinterpret_cast which is important from
// a performance perspective.
CONSTANT_OR_TRIVIAL_OWNERSHIP_INST(Unowned, UncheckedBitwiseCast)

// A thin_to_thick instruction can return a trivial (@noescape) type.
CONSTANT_OR_TRIVIAL_OWNERSHIP_INST(Owned, ThinToThickFunction)

#undef CONSTANT_OR_TRIVIAL_OWNERSHIP_INST

// For a forwarding instruction, we loop over all operands and make sure that
// all non-trivial values have the same ownership.
ValueOwnershipKind
ValueOwnershipKindClassifier::visitForwardingInst(SILInstruction *I,
                                                  ArrayRef<Operand> Ops) {
  // A forwarding inst without operands must be trivial.
  if (Ops.empty())
    return ValueOwnershipKind::Trivial;

  // Find the first index where we have a trivial value.
  auto Iter = find_if(Ops, [&I](const Operand &Op) -> bool {
    if (I->isTypeDependentOperand(Op))
      return false;
    return Op.get().getOwnershipKind() != ValueOwnershipKind::Trivial;
  });
  // All trivial.
  if (Iter == Ops.end()) {
    return ValueOwnershipKind::Trivial;
  }

  // See if we have any Any. If we do, just return that for now.
  if (any_of(Ops, [&I](const Operand &Op) -> bool {
        if (I->isTypeDependentOperand(Op))
          return false;
        return Op.get().getOwnershipKind() == ValueOwnershipKind::Any;
      }))
    return ValueOwnershipKind::Any;
  unsigned Index = std::distance(Ops.begin(), Iter);

  ValueOwnershipKind Base = Ops[Index].get().getOwnershipKind();

  for (const Operand &Op : Ops.slice(Index + 1)) {
    if (I->isTypeDependentOperand(Op))
      continue;
    auto OpKind = Op.get().getOwnershipKind();
    if (OpKind.merge(ValueOwnershipKind::Trivial))
      continue;

    auto MergedValue = Base.merge(OpKind.Value);
    if (!MergedValue.hasValue()) {
      // If we have mismatched SILOwnership and sil ownership is not enabled,
      // just return Any for staging purposes. If SILOwnership is enabled, then
      // we must assert!
      if (!I->getModule().getOptions().EnableSILOwnership) {
        return ValueOwnershipKind::Any;
      }
      llvm_unreachable("Forwarding inst with mismatching ownership kinds?!");
    }
  }

  return Base;
}

#define FORWARDING_OWNERSHIP_INST(INST)                                        \
  ValueOwnershipKind ValueOwnershipKindClassifier::visit##INST##Inst(          \
      INST##Inst *I) {                                                         \
    return I->getOwnershipKind();                                              \
  }
FORWARDING_OWNERSHIP_INST(BridgeObjectToRef)
FORWARDING_OWNERSHIP_INST(ConvertFunction)
FORWARDING_OWNERSHIP_INST(InitExistentialRef)
FORWARDING_OWNERSHIP_INST(OpenExistentialRef)
FORWARDING_OWNERSHIP_INST(RefToBridgeObject)
FORWARDING_OWNERSHIP_INST(SelectValue)
FORWARDING_OWNERSHIP_INST(Object)
FORWARDING_OWNERSHIP_INST(Struct)
FORWARDING_OWNERSHIP_INST(Tuple)
FORWARDING_OWNERSHIP_INST(UncheckedRefCast)
FORWARDING_OWNERSHIP_INST(UnconditionalCheckedCast)
FORWARDING_OWNERSHIP_INST(Upcast)
FORWARDING_OWNERSHIP_INST(MarkUninitialized)
FORWARDING_OWNERSHIP_INST(UncheckedEnumData)
FORWARDING_OWNERSHIP_INST(SelectEnum)
FORWARDING_OWNERSHIP_INST(Enum)
#undef FORWARDING_OWNERSHIP_INST

ValueOwnershipKind
ValueOwnershipKindClassifier::visitUncheckedOwnershipConversionInst(
    UncheckedOwnershipConversionInst *I) {
  return I->getConversionOwnershipKind();
}

ValueOwnershipKind ValueOwnershipKindClassifier::visitSILUndef(SILUndef *arg) {
  return arg->getOwnershipKind();
}

ValueOwnershipKind
ValueOwnershipKindClassifier::visitSILPhiArgument(SILPhiArgument *Arg) {
  return Arg->getOwnershipKind();
}

ValueOwnershipKind ValueOwnershipKindClassifier::visitDestructureStructResult(
    DestructureStructResult *Result) {
  return Result->getOwnershipKind();
}

ValueOwnershipKind ValueOwnershipKindClassifier::visitDestructureTupleResult(
    DestructureTupleResult *Result) {
  return Result->getOwnershipKind();
}

ValueOwnershipKind ValueOwnershipKindClassifier::visitBeginApplyResult(
    BeginApplyResult *Result) {
  return Result->getOwnershipKind();
}

ValueOwnershipKind ValueOwnershipKindClassifier::visitSILFunctionArgument(
    SILFunctionArgument *Arg) {
  return Arg->getOwnershipKind();
}

// This is a forwarding instruction through only one of its arguments.
ValueOwnershipKind
ValueOwnershipKindClassifier::visitMarkDependenceInst(MarkDependenceInst *MDI) {
  return MDI->getValue().getOwnershipKind();
}

ValueOwnershipKind ValueOwnershipKindClassifier::visitApplyInst(ApplyInst *AI) {
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

  ValueOwnershipKind Base = Iter->getOwnershipKind(M, Sig);

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

ValueOwnershipKind ValueOwnershipKindClassifier::visitLoadInst(LoadInst *LI) {
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
    return ValueOwnershipKind::OWNERSHIP;                                      \
  }
// This returns a value at +1 that is destroyed strictly /after/ the
// UnsafeGuaranteedEnd. This provides the guarantee that we want.
CONSTANT_OWNERSHIP_BUILTIN(Owned, UnsafeGuaranteed)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, AShr)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, Add)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, And)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, AssumeNonNegative)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, AssumeTrue)
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
CONSTANT_OWNERSHIP_BUILTIN(Trivial, OnFastPath)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, IsOptionalType)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, Sizeof)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, Strideof)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, StringObjectOr)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, IsPOD)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, IsBitwiseTakable)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, IsSameMetatype)
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
CONSTANT_OWNERSHIP_BUILTIN(Trivial, TakeArrayNoAlias)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, TakeArrayFrontToBack)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, TakeArrayBackToFront)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, AssignCopyArrayNoAlias)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, AssignCopyArrayFrontToBack)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, AssignCopyArrayBackToFront)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, AssignTakeArray)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, UnexpectedError)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, ErrorInMain)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, DeallocRaw)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, Fence)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, AtomicStore)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, Once)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, OnceWithContext)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, TSanInoutAccess)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, Swift3ImplicitObjCEntrypoint)
CONSTANT_OWNERSHIP_BUILTIN(Trivial, PoundAssert)

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
UNOWNED_OR_TRIVIAL_DEPENDING_ON_RESULT(CmpXChg)
UNOWNED_OR_TRIVIAL_DEPENDING_ON_RESULT(AtomicLoad)
UNOWNED_OR_TRIVIAL_DEPENDING_ON_RESULT(ExtractElement)
UNOWNED_OR_TRIVIAL_DEPENDING_ON_RESULT(InsertElement)
UNOWNED_OR_TRIVIAL_DEPENDING_ON_RESULT(ZeroInitializer)
#undef UNOWNED_OR_TRIVIAL_DEPENDING_ON_RESULT

#define BUILTIN(X,Y,Z)
#define BUILTIN_SIL_OPERATION(ID, NAME, CATEGORY) \
  ValueOwnershipKind ValueOwnershipKindBuiltinVisitor::visit##ID( \
      BuiltinInst *BI, StringRef Attr) { \
    llvm_unreachable("builtin should have been lowered in SILGen"); \
  }

#include "swift/AST/Builtins.def"

ValueOwnershipKind
ValueOwnershipKindClassifier::visitBuiltinInst(BuiltinInst *BI) {
  // For now, just conservatively say builtins are None. We need to use a
  // builtin in here to guarantee correctness.
  return ValueOwnershipKindBuiltinVisitor().visit(BI);
}

//===----------------------------------------------------------------------===//
//                            Top Level Entrypoint
//===----------------------------------------------------------------------===//

ValueOwnershipKind SILValue::getOwnershipKind() const {
  // Once we have multiple return values, this must be changed.
  ValueOwnershipKindClassifier Classifier;
  return Classifier.visit(const_cast<ValueBase *>(Value));
}
