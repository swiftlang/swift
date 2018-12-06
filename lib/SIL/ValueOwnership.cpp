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
  CONSTANT_OWNERSHIP_INST(Any, RefTo##Name) \
  CONSTANT_OWNERSHIP_INST(Unowned, Name##ToRef)
#include "swift/AST/ReferenceStorage.def"

CONSTANT_OWNERSHIP_INST(Guaranteed, BeginBorrow)
CONSTANT_OWNERSHIP_INST(Guaranteed, LoadBorrow)
CONSTANT_OWNERSHIP_INST(Owned, AllocBox)
CONSTANT_OWNERSHIP_INST(Owned, AllocExistentialBox)
CONSTANT_OWNERSHIP_INST(Owned, AllocRef)
CONSTANT_OWNERSHIP_INST(Owned, AllocRefDynamic)
CONSTANT_OWNERSHIP_INST(Any, AllocValueBuffer)
CONSTANT_OWNERSHIP_INST(Owned, CopyBlock)
CONSTANT_OWNERSHIP_INST(Owned, CopyBlockWithoutEscaping)
CONSTANT_OWNERSHIP_INST(Owned, CopyValue)
CONSTANT_OWNERSHIP_INST(Owned, KeyPath)
CONSTANT_OWNERSHIP_INST(Owned, PartialApply)
CONSTANT_OWNERSHIP_INST(Owned, InitExistentialValue)
CONSTANT_OWNERSHIP_INST(Owned, GlobalValue) // TODO: is this correct?

// NOTE: Even though init_existential_ref from a reference counting perspective
// is not considered to be "owned" since it doesn't affect reference counts,
// conceptually we want to treat it as an owned value that produces owned
// things, rather than a forwarding thing since initialization is generally a
// consuming operation.
CONSTANT_OWNERSHIP_INST(Owned, InitExistentialRef)

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
CONSTANT_OWNERSHIP_INST(Any, AddressToPointer)
CONSTANT_OWNERSHIP_INST(Any, AllocStack)
CONSTANT_OWNERSHIP_INST(Any, BeginAccess)
CONSTANT_OWNERSHIP_INST(Any, BridgeObjectToWord)
CONSTANT_OWNERSHIP_INST(Any, ClassMethod)
CONSTANT_OWNERSHIP_INST(Any, ClassifyBridgeObject)
CONSTANT_OWNERSHIP_INST(Any, ObjCMethod)
CONSTANT_OWNERSHIP_INST(Any, ExistentialMetatype)
CONSTANT_OWNERSHIP_INST(Any, FloatLiteral)
CONSTANT_OWNERSHIP_INST(Any, FunctionRef)
CONSTANT_OWNERSHIP_INST(Any, DynamicFunctionRef)
CONSTANT_OWNERSHIP_INST(Any, PreviousDynamicFunctionRef)
CONSTANT_OWNERSHIP_INST(Any, GlobalAddr)
CONSTANT_OWNERSHIP_INST(Any, IndexAddr)
CONSTANT_OWNERSHIP_INST(Any, IndexRawPointer)
CONSTANT_OWNERSHIP_INST(Any, InitEnumDataAddr)
CONSTANT_OWNERSHIP_INST(Any, InitExistentialAddr)
CONSTANT_OWNERSHIP_INST(Any, InitExistentialMetatype)
CONSTANT_OWNERSHIP_INST(Any, IntegerLiteral)
CONSTANT_OWNERSHIP_INST(Any, IsUnique)
CONSTANT_OWNERSHIP_INST(Any, IsEscapingClosure)
CONSTANT_OWNERSHIP_INST(Any, MarkUninitializedBehavior)
CONSTANT_OWNERSHIP_INST(Any, Metatype)
CONSTANT_OWNERSHIP_INST(Any, ObjCToThickMetatype)
CONSTANT_OWNERSHIP_INST(Any, OpenExistentialAddr)
CONSTANT_OWNERSHIP_INST(Any, OpenExistentialBox)
CONSTANT_OWNERSHIP_INST(Any, OpenExistentialMetatype)
CONSTANT_OWNERSHIP_INST(Any, PointerToAddress)
CONSTANT_OWNERSHIP_INST(Any, PointerToThinFunction)
CONSTANT_OWNERSHIP_INST(Any, ProjectBlockStorage)
CONSTANT_OWNERSHIP_INST(Any, ProjectBox)
CONSTANT_OWNERSHIP_INST(Any, ProjectExistentialBox)
CONSTANT_OWNERSHIP_INST(Any, ProjectValueBuffer)
CONSTANT_OWNERSHIP_INST(Any, RefElementAddr)
CONSTANT_OWNERSHIP_INST(Any, RefTailAddr)
CONSTANT_OWNERSHIP_INST(Any, RefToRawPointer)
CONSTANT_OWNERSHIP_INST(Any, SelectEnumAddr)
CONSTANT_OWNERSHIP_INST(Any, StringLiteral)
CONSTANT_OWNERSHIP_INST(Any, StructElementAddr)
CONSTANT_OWNERSHIP_INST(Any, SuperMethod)
CONSTANT_OWNERSHIP_INST(Any, ObjCSuperMethod)
CONSTANT_OWNERSHIP_INST(Any, TailAddr)
CONSTANT_OWNERSHIP_INST(Any, ThickToObjCMetatype)
CONSTANT_OWNERSHIP_INST(Any, ThinFunctionToPointer)
CONSTANT_OWNERSHIP_INST(Any, TupleElementAddr)
CONSTANT_OWNERSHIP_INST(Any, UncheckedAddrCast)
CONSTANT_OWNERSHIP_INST(Any, UncheckedTakeEnumDataAddr)
CONSTANT_OWNERSHIP_INST(Any, UncheckedTrivialBitCast)
CONSTANT_OWNERSHIP_INST(Any, ValueMetatype)
CONSTANT_OWNERSHIP_INST(Any, WitnessMethod)
CONSTANT_OWNERSHIP_INST(Any, StoreBorrow)
CONSTANT_OWNERSHIP_INST(Any, ConvertEscapeToNoEscape)
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
      return ValueOwnershipKind::Any;                                          \
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
ValueOwnershipKindClassifier::visitForwardingInst(SILInstruction *i,
                                                  ArrayRef<Operand> ops) {
  // A forwarding inst without operands must be trivial.
  if (ops.empty())
    return ValueOwnershipKind::Any;

  auto mergedValue = ValueOwnershipKind::merge(makeOptionalTransformRange(
      ops, [&i](const Operand &op) -> Optional<ValueOwnershipKind> {
        if (i->isTypeDependentOperand(op))
          return None;
        return op.get().getOwnershipKind();
      }));

  if (!mergedValue.hasValue()) {
    // If we have mismatched SILOwnership and sil ownership is not enabled,
    // just return Any for staging purposes. If SILOwnership is enabled, then
    // we must assert!
    if (!i->getModule().getOptions().EnableSILOwnership) {
      return ValueOwnershipKind::Any;
    }
    llvm_unreachable("Forwarding inst with mismatching ownership kinds?!");
  }

  return mergedValue.getValue();
}

#define FORWARDING_OWNERSHIP_INST(INST)                                        \
  ValueOwnershipKind ValueOwnershipKindClassifier::visit##INST##Inst(          \
      INST##Inst *I) {                                                         \
    return I->getOwnershipKind();                                              \
  }
FORWARDING_OWNERSHIP_INST(BridgeObjectToRef)
FORWARDING_OWNERSHIP_INST(ConvertFunction)
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

ValueOwnershipKind ValueOwnershipKindClassifier::visitApplyInst(ApplyInst *ai) {
  SILModule &m = ai->getModule();
  bool isTrivial = ai->getType().isTrivial(m);
  // Quick is trivial check.
  if (isTrivial)
    return ValueOwnershipKind::Any;

  SILFunctionConventions fnConv(ai->getSubstCalleeType(), m);
  auto results = fnConv.getDirectSILResults();
  // No results => Any.
  if (results.empty())
    return ValueOwnershipKind::Any;

  // Otherwise, map our results to their ownership kinds and then merge them!
  CanGenericSignature sig = ai->getSubstCalleeType()->getGenericSignature();
  auto resultOwnershipKinds =
      makeTransformRange(results, [&](const SILResultInfo &info) {
        return info.getOwnershipKind(m, sig);
      });
  auto mergedOwnershipKind = ValueOwnershipKind::merge(resultOwnershipKinds);
  if (!mergedOwnershipKind) {
    llvm_unreachable("Forwarding inst with mismatching ownership kinds?!");
  }

  return *mergedOwnershipKind;
}

ValueOwnershipKind ValueOwnershipKindClassifier::visitLoadInst(LoadInst *LI) {
  switch (LI->getOwnershipQualifier()) {
  case LoadOwnershipQualifier::Take:
  case LoadOwnershipQualifier::Copy:
    return ValueOwnershipKind::Owned;
  case LoadOwnershipQualifier::Unqualified:
  case LoadOwnershipQualifier::Trivial:
    return ValueOwnershipKind::Any;
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
    // must be any.
    return ValueOwnershipKind::Any;
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
CONSTANT_OWNERSHIP_BUILTIN(Any, AShr)
CONSTANT_OWNERSHIP_BUILTIN(Any, Add)
CONSTANT_OWNERSHIP_BUILTIN(Any, And)
CONSTANT_OWNERSHIP_BUILTIN(Any, AssumeNonNegative)
CONSTANT_OWNERSHIP_BUILTIN(Any, AssumeTrue)
CONSTANT_OWNERSHIP_BUILTIN(Any, BitCast)
CONSTANT_OWNERSHIP_BUILTIN(Any, ExactSDiv)
CONSTANT_OWNERSHIP_BUILTIN(Any, ExactUDiv)
CONSTANT_OWNERSHIP_BUILTIN(Any, FAdd)
CONSTANT_OWNERSHIP_BUILTIN(Any, FCMP_OEQ)
CONSTANT_OWNERSHIP_BUILTIN(Any, FCMP_OGE)
CONSTANT_OWNERSHIP_BUILTIN(Any, FCMP_OGT)
CONSTANT_OWNERSHIP_BUILTIN(Any, FCMP_OLE)
CONSTANT_OWNERSHIP_BUILTIN(Any, FCMP_OLT)
CONSTANT_OWNERSHIP_BUILTIN(Any, FCMP_ONE)
CONSTANT_OWNERSHIP_BUILTIN(Any, FCMP_UEQ)
CONSTANT_OWNERSHIP_BUILTIN(Any, FCMP_UGE)
CONSTANT_OWNERSHIP_BUILTIN(Any, FCMP_UGT)
CONSTANT_OWNERSHIP_BUILTIN(Any, FCMP_ULE)
CONSTANT_OWNERSHIP_BUILTIN(Any, FCMP_ULT)
CONSTANT_OWNERSHIP_BUILTIN(Any, FCMP_UNE)
CONSTANT_OWNERSHIP_BUILTIN(Any, FDiv)
CONSTANT_OWNERSHIP_BUILTIN(Any, FMul)
CONSTANT_OWNERSHIP_BUILTIN(Any, FNeg)
CONSTANT_OWNERSHIP_BUILTIN(Any, FPExt)
CONSTANT_OWNERSHIP_BUILTIN(Any, FPToSI)
CONSTANT_OWNERSHIP_BUILTIN(Any, FPToUI)
CONSTANT_OWNERSHIP_BUILTIN(Any, FPTrunc)
CONSTANT_OWNERSHIP_BUILTIN(Any, FRem)
CONSTANT_OWNERSHIP_BUILTIN(Any, FSub)
CONSTANT_OWNERSHIP_BUILTIN(Any, ICMP_EQ)
CONSTANT_OWNERSHIP_BUILTIN(Any, ICMP_NE)
CONSTANT_OWNERSHIP_BUILTIN(Any, ICMP_SGE)
CONSTANT_OWNERSHIP_BUILTIN(Any, ICMP_SGT)
CONSTANT_OWNERSHIP_BUILTIN(Any, ICMP_SLE)
CONSTANT_OWNERSHIP_BUILTIN(Any, ICMP_SLT)
CONSTANT_OWNERSHIP_BUILTIN(Any, ICMP_UGE)
CONSTANT_OWNERSHIP_BUILTIN(Any, ICMP_UGT)
CONSTANT_OWNERSHIP_BUILTIN(Any, ICMP_ULE)
CONSTANT_OWNERSHIP_BUILTIN(Any, ICMP_ULT)
CONSTANT_OWNERSHIP_BUILTIN(Any, IntToPtr)
CONSTANT_OWNERSHIP_BUILTIN(Any, LShr)
CONSTANT_OWNERSHIP_BUILTIN(Any, Mul)
CONSTANT_OWNERSHIP_BUILTIN(Any, Or)
CONSTANT_OWNERSHIP_BUILTIN(Any, PtrToInt)
CONSTANT_OWNERSHIP_BUILTIN(Any, SAddOver)
CONSTANT_OWNERSHIP_BUILTIN(Any, SDiv)
CONSTANT_OWNERSHIP_BUILTIN(Any, SExt)
CONSTANT_OWNERSHIP_BUILTIN(Any, SExtOrBitCast)
CONSTANT_OWNERSHIP_BUILTIN(Any, SIToFP)
CONSTANT_OWNERSHIP_BUILTIN(Any, SMulOver)
CONSTANT_OWNERSHIP_BUILTIN(Any, SRem)
CONSTANT_OWNERSHIP_BUILTIN(Any, SSubOver)
CONSTANT_OWNERSHIP_BUILTIN(Any, Shl)
CONSTANT_OWNERSHIP_BUILTIN(Any, Sub)
CONSTANT_OWNERSHIP_BUILTIN(Any, Trunc)
CONSTANT_OWNERSHIP_BUILTIN(Any, TruncOrBitCast)
CONSTANT_OWNERSHIP_BUILTIN(Any, UAddOver)
CONSTANT_OWNERSHIP_BUILTIN(Any, UDiv)
CONSTANT_OWNERSHIP_BUILTIN(Any, UIToFP)
CONSTANT_OWNERSHIP_BUILTIN(Any, UMulOver)
CONSTANT_OWNERSHIP_BUILTIN(Any, URem)
CONSTANT_OWNERSHIP_BUILTIN(Any, USubOver)
CONSTANT_OWNERSHIP_BUILTIN(Any, Xor)
CONSTANT_OWNERSHIP_BUILTIN(Any, ZExt)
CONSTANT_OWNERSHIP_BUILTIN(Any, ZExtOrBitCast)
CONSTANT_OWNERSHIP_BUILTIN(Any, FCMP_ORD)
CONSTANT_OWNERSHIP_BUILTIN(Any, FCMP_UNO)
CONSTANT_OWNERSHIP_BUILTIN(Any, OnFastPath)
CONSTANT_OWNERSHIP_BUILTIN(Any, IsOptionalType)
CONSTANT_OWNERSHIP_BUILTIN(Any, Sizeof)
CONSTANT_OWNERSHIP_BUILTIN(Any, Strideof)
CONSTANT_OWNERSHIP_BUILTIN(Any, StringObjectOr)
CONSTANT_OWNERSHIP_BUILTIN(Any, IsPOD)
CONSTANT_OWNERSHIP_BUILTIN(Any, IsBitwiseTakable)
CONSTANT_OWNERSHIP_BUILTIN(Any, IsSameMetatype)
CONSTANT_OWNERSHIP_BUILTIN(Any, Alignof)
CONSTANT_OWNERSHIP_BUILTIN(Any, AllocRaw)
CONSTANT_OWNERSHIP_BUILTIN(Any, AssertConf)
CONSTANT_OWNERSHIP_BUILTIN(Any, UToSCheckedTrunc)
CONSTANT_OWNERSHIP_BUILTIN(Any, SToSCheckedTrunc)
CONSTANT_OWNERSHIP_BUILTIN(Any, SToUCheckedTrunc)
CONSTANT_OWNERSHIP_BUILTIN(Any, UToUCheckedTrunc)
CONSTANT_OWNERSHIP_BUILTIN(Any, IntToFPWithOverflow)

// This is surprising, Builtin.unreachable returns a "Never" value which is
// trivially typed.
CONSTANT_OWNERSHIP_BUILTIN(Any, Unreachable)

/// AtomicRMW has type (Builtin.RawPointer, T) -> T. But it provides overloads
/// for integer or rawpointer, so it should be trivial.
CONSTANT_OWNERSHIP_BUILTIN(Any, AtomicRMW)

CONSTANT_OWNERSHIP_BUILTIN(Any, CondUnreachable)
CONSTANT_OWNERSHIP_BUILTIN(Any, UnsafeGuaranteedEnd)
CONSTANT_OWNERSHIP_BUILTIN(Any, GetObjCTypeEncoding)
CONSTANT_OWNERSHIP_BUILTIN(Any, CanBeObjCClass)
CONSTANT_OWNERSHIP_BUILTIN(Any, WillThrow)
CONSTANT_OWNERSHIP_BUILTIN(Any, StaticReport)

CONSTANT_OWNERSHIP_BUILTIN(Any, DestroyArray)
CONSTANT_OWNERSHIP_BUILTIN(Any, CopyArray)
CONSTANT_OWNERSHIP_BUILTIN(Any, TakeArrayNoAlias)
CONSTANT_OWNERSHIP_BUILTIN(Any, TakeArrayFrontToBack)
CONSTANT_OWNERSHIP_BUILTIN(Any, TakeArrayBackToFront)
CONSTANT_OWNERSHIP_BUILTIN(Any, AssignCopyArrayNoAlias)
CONSTANT_OWNERSHIP_BUILTIN(Any, AssignCopyArrayFrontToBack)
CONSTANT_OWNERSHIP_BUILTIN(Any, AssignCopyArrayBackToFront)
CONSTANT_OWNERSHIP_BUILTIN(Any, AssignTakeArray)
CONSTANT_OWNERSHIP_BUILTIN(Any, UnexpectedError)
CONSTANT_OWNERSHIP_BUILTIN(Any, ErrorInMain)
CONSTANT_OWNERSHIP_BUILTIN(Any, DeallocRaw)
CONSTANT_OWNERSHIP_BUILTIN(Any, Fence)
CONSTANT_OWNERSHIP_BUILTIN(Any, AtomicStore)
CONSTANT_OWNERSHIP_BUILTIN(Any, Once)
CONSTANT_OWNERSHIP_BUILTIN(Any, OnceWithContext)
CONSTANT_OWNERSHIP_BUILTIN(Any, TSanInoutAccess)
CONSTANT_OWNERSHIP_BUILTIN(Any, Swift3ImplicitObjCEntrypoint)
CONSTANT_OWNERSHIP_BUILTIN(Any, PoundAssert)

#undef CONSTANT_OWNERSHIP_BUILTIN

// Check all of these...
#define UNOWNED_OR_TRIVIAL_DEPENDING_ON_RESULT(ID)                             \
  ValueOwnershipKind ValueOwnershipKindBuiltinVisitor::visit##ID(              \
      BuiltinInst *BI, StringRef Attr) {                                       \
    if (BI->getType().isTrivial(BI->getModule())) {                            \
      return ValueOwnershipKind::Any;                                          \
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
