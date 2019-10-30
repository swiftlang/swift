//===--- ArraySemantic.cpp - Wrapper around array semantic calls. ---------===//
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

#include "swift/SILOptimizer/Analysis/ArraySemantic.h"
#include "swift/SIL/DebugUtils.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SILOptimizer/Analysis/DominanceAnalysis.h"
#include "swift/SILOptimizer/Utils/InstOptUtils.h"
#include "llvm/ADT/StringSwitch.h"

using namespace swift;

static ParameterConvention
getSelfParameterConvention(ApplyInst *SemanticsCall) {
  FunctionRefInst *FRI = cast<FunctionRefInst>(SemanticsCall->getCallee());
  SILFunction *F = FRI->getInitiallyReferencedFunction();
  auto FnTy = F->getLoweredFunctionType();

  return FnTy->getSelfParameter().getConvention();
}

/// Make sure that all parameters are passed with a reference count
/// neutral parameter convention except for self.
bool swift::ArraySemanticsCall::isValidSignature() {
  assert(SemanticsCall && getKind() != ArrayCallKind::kNone &&
         "Need an array semantic call");
  FunctionRefInst *FRI = cast<FunctionRefInst>(SemanticsCall->getCallee());
  SILFunction *F = FRI->getInitiallyReferencedFunction();
  auto FnTy = F->getLoweredFunctionType();
  auto &Mod = F->getModule();

  // Check whether we have a valid signature for semantic calls that we hoist.
  switch (getKind()) {
  // All other calls can be consider valid.
  default: break;
  case ArrayCallKind::kArrayPropsIsNativeTypeChecked: {
    // @guaranteed/@owned Self
    if (SemanticsCall->getNumArguments() != 1)
      return false;
    auto SelfConvention = FnTy->getSelfParameter().getConvention();
    return SelfConvention == ParameterConvention::Direct_Guaranteed ||
           SelfConvention == ParameterConvention::Direct_Owned;
  }
  case ArrayCallKind::kCheckIndex: {
    // Int, @guaranteed/@owned Self
    if (SemanticsCall->getNumArguments() != 2 ||
        !SemanticsCall->getArgument(0)->getType().isTrivial(*F))
      return false;
    auto SelfConvention = FnTy->getSelfParameter().getConvention();
    return SelfConvention == ParameterConvention::Direct_Guaranteed ||
           SelfConvention == ParameterConvention::Direct_Owned;
  }
  case ArrayCallKind::kCheckSubscript: {
    // Int, Bool, Self
    if (SemanticsCall->getNumArguments() != 3 ||
        !SemanticsCall->getArgument(0)->getType().isTrivial(*F))
      return false;
    if (!SemanticsCall->getArgument(1)->getType().isTrivial(*F))
      return false;
    auto SelfConvention = FnTy->getSelfParameter().getConvention();
    return SelfConvention == ParameterConvention::Direct_Guaranteed ||
           SelfConvention == ParameterConvention::Direct_Owned;
  }
  case ArrayCallKind::kMakeMutable: {
    auto SelfConvention = FnTy->getSelfParameter().getConvention();
    return SelfConvention == ParameterConvention::Indirect_Inout;
  }
  case ArrayCallKind::kArrayUninitialized: {
    // Make sure that if we are a _adoptStorage call that our storage is
    // uniquely referenced by us.
    SILValue Arg0 = SemanticsCall->getArgument(0);
    if (Arg0->getType().isExistentialType()) {
      auto *AllocBufferAI = dyn_cast<ApplyInst>(Arg0);
      if (!AllocBufferAI)
        return false;
      auto *AllocFn = AllocBufferAI->getReferencedFunctionOrNull();
      if (!AllocFn || AllocFn->getName() != "swift_bufferAllocate" ||
          !hasOneNonDebugUse(AllocBufferAI))
        return false;
    }
    return true;
  }
  case ArrayCallKind::kWithUnsafeMutableBufferPointer: {
    SILFunctionConventions origConv(SemanticsCall->getOrigCalleeType(), Mod);
    if (origConv.getNumIndirectSILResults() != 1
        || SemanticsCall->getNumArguments() != 3)
      return false;
    auto SelfConvention = FnTy->getSelfParameter().getConvention();
    return SelfConvention == ParameterConvention::Indirect_Inout;
  }
  }

  return true;
}

/// Match array semantic calls.
swift::ArraySemanticsCall::ArraySemanticsCall(SILValue V,
                                              StringRef semanticName,
                                              bool matchPartialName)
    : SemanticsCall(nullptr) {
  if (auto AI = dyn_cast<ApplyInst>(V))
    initialize(AI, semanticName, matchPartialName);
}

/// Match array semantic calls.
swift::ArraySemanticsCall::ArraySemanticsCall(SILInstruction *I,
                                              StringRef semanticName,
                                              bool matchPartialName)
    : SemanticsCall(nullptr) {
  if (auto AI = dyn_cast<ApplyInst>(I))
    initialize(AI, semanticName, matchPartialName);
}

/// Match array semantic calls.
swift::ArraySemanticsCall::ArraySemanticsCall(ApplyInst *AI,
                                              StringRef semanticName,
                                              bool matchPartialName)
    : SemanticsCall(nullptr) {
  initialize(AI, semanticName, matchPartialName);
}

void ArraySemanticsCall::initialize(ApplyInst *AI, StringRef semanticName,
                                    bool matchPartialName) {
  auto *fn = AI->getReferencedFunctionOrNull();
  if (!fn)
    return;

  if (!(matchPartialName
          ? fn->hasSemanticsAttrThatStartsWith(semanticName)
          : fn->hasSemanticsAttr(semanticName)))
    return;

  SemanticsCall = AI;

  // Need a 'self' argument otherwise this is not a semantic call that
  // we recognize.
  if (getKind() < ArrayCallKind::kArrayInit && !hasSelf())
    SemanticsCall = nullptr;

  // A arguments must be passed reference count neutral except for self.
  if (SemanticsCall && !isValidSignature())
    SemanticsCall = nullptr;
}

/// Determine which kind of array semantics call this is.
ArrayCallKind swift::ArraySemanticsCall::getKind() const {
  if (!SemanticsCall)
    return ArrayCallKind::kNone;

  auto F = cast<FunctionRefInst>(SemanticsCall->getCallee())
               ->getInitiallyReferencedFunction();

  ArrayCallKind Kind = ArrayCallKind::kNone;

  for (auto &Attrs : F->getSemanticsAttrs()) {
    auto Tmp =
        llvm::StringSwitch<ArrayCallKind>(Attrs)
            .Case("array.props.isNativeTypeChecked",
                  ArrayCallKind::kArrayPropsIsNativeTypeChecked)
            .StartsWith("array.init", ArrayCallKind::kArrayInit)
            .Case("array.uninitialized", ArrayCallKind::kArrayUninitialized)
            .Case("array.check_subscript", ArrayCallKind::kCheckSubscript)
            .Case("array.check_index", ArrayCallKind::kCheckIndex)
            .Case("array.get_count", ArrayCallKind::kGetCount)
            .Case("array.get_capacity", ArrayCallKind::kGetCapacity)
            .Case("array.get_element", ArrayCallKind::kGetElement)
            .Case("array.make_mutable", ArrayCallKind::kMakeMutable)
            .Case("array.get_element_address",
                  ArrayCallKind::kGetElementAddress)
            .Case("array.mutate_unknown", ArrayCallKind::kMutateUnknown)
            .Case("array.reserve_capacity_for_append",
                  ArrayCallKind::kReserveCapacityForAppend)
            .Case("array.withUnsafeMutableBufferPointer",
                  ArrayCallKind::kWithUnsafeMutableBufferPointer)
            .Case("array.append_contentsOf", ArrayCallKind::kAppendContentsOf)
            .Case("array.append_element", ArrayCallKind::kAppendElement)
            .Default(ArrayCallKind::kNone);
    if (Tmp != ArrayCallKind::kNone) {
      assert(Kind == ArrayCallKind::kNone && "Multiple array semantic "
                                             "strings?!");
      Kind = Tmp;
    }
  }

  return Kind;
}

bool swift::ArraySemanticsCall::hasSelf() const {
  assert(SemanticsCall && "Must have a semantics call");
  // Array.init and Array.uninitialized return 'self' @owned.
  return SemanticsCall->getOrigCalleeType()->hasSelfParam();
}

SILValue swift::ArraySemanticsCall::getSelf() const {
  return SemanticsCall->getSelfArgument();
}

Operand &swift::ArraySemanticsCall::getSelfOperand() const {
  return SemanticsCall->getSelfArgumentOperand();
}

bool swift::ArraySemanticsCall::hasGuaranteedSelf() const {
  if (!hasSelf())
    return false;
  return getSelfParameterConvention(SemanticsCall) ==
    ParameterConvention::Direct_Guaranteed;
}

bool swift::ArraySemanticsCall::hasGetElementDirectResult() const {
  assert(getKind() == ArrayCallKind::kGetElement &&
         "must be an array.get_element call");
  bool DirectResult =
      (SemanticsCall->getOrigCalleeConv().getNumIndirectSILResults() == 0);
  assert((DirectResult && SemanticsCall->getNumArguments() == 4 ||
          !DirectResult && SemanticsCall->getNumArguments() == 5) &&
         "wrong number of array.get_element call arguments");
  return DirectResult;
}

SILValue swift::ArraySemanticsCall::getTypeCheckedArgument() const {
  return SemanticsCall->getArgument(hasGetElementDirectResult() ? 1 : 2);
}

SILValue swift::ArraySemanticsCall::getSubscriptCheckArgument() const {
  return SemanticsCall->getArgument(hasGetElementDirectResult() ? 2 : 3);
}

SILValue swift::ArraySemanticsCall::getIndex() const {
  assert(SemanticsCall && "Must have a semantics call");
  assert(SemanticsCall->getNumArguments() && "Must have arguments");
  assert(getKind() == ArrayCallKind::kCheckSubscript ||
         getKind() == ArrayCallKind::kCheckIndex ||
         getKind() == ArrayCallKind::kGetElement ||
         getKind() == ArrayCallKind::kGetElementAddress);

  if (getKind() == ArrayCallKind::kGetElement)
    return SemanticsCall->getArgument(hasGetElementDirectResult() ? 0 : 1);

  return SemanticsCall->getArgument(0);
}

Optional<int64_t> swift::ArraySemanticsCall::getConstantIndex() const {
  auto *IndexStruct = dyn_cast<StructInst>(getIndex());
  if (!IndexStruct)
    return None;
  auto StructOpds = IndexStruct->getElements();
  if (StructOpds.size() != 1)
    return None;
  auto *Literal = dyn_cast<IntegerLiteralInst>(StructOpds[0]);
  if (!Literal)
    return None;

  auto Val = Literal->getValue();
  if (Val.getNumWords()>1)
    return None;

  return Val.getSExtValue();
}

static bool canHoistArrayArgument(ApplyInst *SemanticsCall, SILValue Arr,
                                  SILInstruction *InsertBefore,
                                  DominanceInfo *DT) {

  // We only know how to hoist inout, owned or guaranteed parameters.
  auto Convention = getSelfParameterConvention(SemanticsCall);
  if (Convention != ParameterConvention::Indirect_Inout &&
      Convention != ParameterConvention::Direct_Owned &&
      Convention != ParameterConvention::Direct_Guaranteed)
    return false;

  ValueBase *SelfVal = Arr;
  auto *SelfBB = SelfVal->getParentBlock();
  if (DT->dominates(SelfBB, InsertBefore->getParent()))
    return true;

  if (auto LI = dyn_cast<LoadInst>(SelfVal)) {
    // Are we loading a value from an address in a struct defined at a point
    // dominating the hoist point.
    auto Val = LI->getOperand();
    bool DoesNotDominate;
    StructElementAddrInst *SEI;
    while ((DoesNotDominate = !DT->dominates(Val->getParentBlock(),
                                             InsertBefore->getParent())) &&
           (SEI = dyn_cast<StructElementAddrInst>(Val)))
      Val = SEI->getOperand();
    return !DoesNotDominate;
  }

  return false;
}

bool swift::ArraySemanticsCall::canHoist(SILInstruction *InsertBefore,
                                         DominanceInfo *DT) const {
  auto Kind = getKind();
  switch (Kind) {
  default:
    break;

  case ArrayCallKind::kCheckIndex:
  case ArrayCallKind::kArrayPropsIsNativeTypeChecked:
  case ArrayCallKind::kGetElementAddress:
  case ArrayCallKind::kGetCount:
  case ArrayCallKind::kGetCapacity:
    return canHoistArrayArgument(SemanticsCall, getSelf(), InsertBefore, DT);

  case ArrayCallKind::kGetElement:
    // Not implemented yet.
    return false;

  case ArrayCallKind::kCheckSubscript: {
    auto IsNativeArg = getArrayPropertyIsNativeTypeChecked();
    ArraySemanticsCall IsNative(IsNativeArg,
                                "array.props.isNativeTypeChecked", true);
    if (!IsNative) {
      // Do we have a constant parameter?
      auto *SI = dyn_cast<StructInst>(IsNativeArg);
      if (!SI)
        return false;
      if (!isa<IntegerLiteralInst>(SI->getOperand(0)))
        return false;
    } else if (!IsNative.canHoist(InsertBefore, DT))
      // Otherwise, we must be able to hoist the function call.
      return false;

    return canHoistArrayArgument(SemanticsCall, getSelf(), InsertBefore, DT);
  }

  case ArrayCallKind::kMakeMutable: {
    return canHoistArrayArgument(SemanticsCall, getSelf(), InsertBefore, DT);
  }
  } // End switch.

  return false;
}

/// Copy the array load to the insert point.
static SILValue copyArrayLoad(SILValue ArrayStructValue,
                               SILInstruction *InsertBefore,
                               DominanceInfo *DT) {
  if (DT->dominates(ArrayStructValue->getParentBlock(),
                    InsertBefore->getParent()))
    return ArrayStructValue;

  auto *LI = cast<LoadInst>(ArrayStructValue);

  // Recursively move struct_element_addr.
  ValueBase *Val = LI->getOperand();
  auto *InsertPt = InsertBefore;
  while (!DT->dominates(Val->getParentBlock(), InsertBefore->getParent())) {
    auto *Inst = cast<StructElementAddrInst>(Val);
    Inst->moveBefore(InsertPt);
    Val = Inst->getOperand();
    InsertPt = Inst;
  }

  return cast<LoadInst>(LI->clone(InsertBefore));
}

static ApplyInst *hoistOrCopyCall(ApplyInst *AI, SILInstruction *InsertBefore,
                                  bool LeaveOriginal, DominanceInfo *DT) {
  if (!LeaveOriginal) {
    AI->moveBefore(InsertBefore);
  } else {
    // Leave the original and 'hoist' a clone.
    AI = cast<ApplyInst>(AI->clone(InsertBefore));
  }
  placeFuncRef(AI, DT);
  return AI;
}


/// Hoist or copy the self argument of the semantics call.
/// Return the hoisted self argument.
static SILValue hoistOrCopySelf(ApplyInst *SemanticsCall,
                                SILInstruction *InsertBefore,
                                DominanceInfo *DT, bool LeaveOriginal) {

  auto SelfConvention = getSelfParameterConvention(SemanticsCall);

  assert((SelfConvention == ParameterConvention::Direct_Owned ||
          SelfConvention == ParameterConvention::Direct_Guaranteed) &&
         "Expect @owned or @guaranteed self");

  auto Self = SemanticsCall->getSelfArgument();
  bool IsOwnedSelf = SelfConvention == ParameterConvention::Direct_Owned;

  // Emit matching release for owned self if we are moving the original call.
  if (!LeaveOriginal && IsOwnedSelf) {
    SILBuilderWithScope Builder(SemanticsCall);
    Builder.createReleaseValue(SemanticsCall->getLoc(), Self, Builder.getDefaultAtomicity());
  }

  auto NewArrayStructValue = copyArrayLoad(Self, InsertBefore, DT);

  // Retain the array.
  if (IsOwnedSelf) {
    SILBuilderWithScope Builder(InsertBefore, SemanticsCall);
    Builder.createRetainValue(SemanticsCall->getLoc(), NewArrayStructValue,
                              Builder.getDefaultAtomicity());
  }

  return NewArrayStructValue;
}

ApplyInst *swift::ArraySemanticsCall::hoistOrCopy(SILInstruction *InsertBefore,
                                                  DominanceInfo *DT,
                                                  bool LeaveOriginal) {
  assert(canHoist(InsertBefore, DT) &&
         "Must be able to hoist the semantics call");

  auto Kind = getKind();
  switch (Kind) {
  case ArrayCallKind::kArrayPropsIsNativeTypeChecked:
  case ArrayCallKind::kGetCount:
  case ArrayCallKind::kGetCapacity: {
    assert(SemanticsCall->getNumArguments() == 1 &&
           "Expect 'self' parameter only");

    auto HoistedSelf =
        hoistOrCopySelf(SemanticsCall, InsertBefore, DT, LeaveOriginal);

    auto *Call =
        hoistOrCopyCall(SemanticsCall, InsertBefore, LeaveOriginal, DT);
    Call->setSelfArgument(HoistedSelf);
    return Call;
  }

  case ArrayCallKind::kCheckSubscript:
  case ArrayCallKind::kCheckIndex: {
    auto HoistedSelf =
        hoistOrCopySelf(SemanticsCall, InsertBefore, DT, LeaveOriginal);

    SILValue NewArrayProps;
    if (Kind == ArrayCallKind::kCheckSubscript) {
      // Copy the array.props argument call.
      auto IsNativeArg = getArrayPropertyIsNativeTypeChecked();
      ArraySemanticsCall IsNative(IsNativeArg,
                                  "array.props.isNativeTypeChecked", true);
      if (!IsNative) {
        // Do we have a constant parameter?
        auto *SI = dyn_cast<StructInst>(IsNativeArg);
        assert(SI && isa<IntegerLiteralInst>(SI->getOperand(0)) &&
               "Must have a constant parameter or an array.props.isNative call "
               "as argument");
        SI->moveBefore(&*DT->findNearestCommonDominator(
                               InsertBefore->getParent(), SI->getParent())
                             ->begin());
        auto *IL = cast<IntegerLiteralInst>(SI->getOperand(0));
        IL->moveBefore(&*DT->findNearestCommonDominator(
                               InsertBefore->getParent(), IL->getParent())
                             ->begin());
      } else {
        NewArrayProps = IsNative.copyTo(InsertBefore, DT);
      }

      // Replace all uses of the check subscript call by a use of the empty
      // dependence. The check subscript call is no longer associated with
      // another operation.
      auto EmptyDep = SILBuilderWithScope(SemanticsCall)
                          .createStruct(SemanticsCall->getLoc(),
                                        SemanticsCall->getType(), {});
      SemanticsCall->replaceAllUsesWith(EmptyDep);
    }

    // Hoist the call.
    auto Call = hoistOrCopyCall(SemanticsCall, InsertBefore, LeaveOriginal, DT);
    Call->setSelfArgument(HoistedSelf);

    if (NewArrayProps) {
      // Set the array.props argument.
      Call->setArgument(1, NewArrayProps);
    }


    return Call;
  }

  case ArrayCallKind::kMakeMutable: {
    assert(!LeaveOriginal && "Copying not yet implemented");
    // Hoist the call.
    auto Call = hoistOrCopyCall(SemanticsCall, InsertBefore, LeaveOriginal, DT);
    return Call;
  }

  default:
    llvm_unreachable("Don't know how to hoist this instruction");
    break;
  } // End switch.
}

void swift::ArraySemanticsCall::removeCall() {
  if (getSelfParameterConvention(SemanticsCall) ==
      ParameterConvention::Direct_Owned) {
    SILBuilderWithScope Builder(SemanticsCall);
    Builder.createReleaseValue(SemanticsCall->getLoc(), getSelf(),
                               Builder.getDefaultAtomicity());
  }

  switch (getKind()) {
  default: break;
  case ArrayCallKind::kCheckSubscript: {
    // Remove all uses with the empty tuple ().
    auto EmptyDep = SILBuilderWithScope(SemanticsCall)
                        .createStruct(SemanticsCall->getLoc(),
                                      SemanticsCall->getType(), {});
    SemanticsCall->replaceAllUsesWith(EmptyDep);
  }
  break;
  case ArrayCallKind::kGetElement: {
    // Remove the matching isNativeTypeChecked and check_subscript call.
    ArraySemanticsCall IsNative(getTypeCheckedArgument(),
                                "array.props.isNativeTypeChecked");
    ArraySemanticsCall SubscriptCheck(getSubscriptCheckArgument(),
                                      "array.check_subscript");
    if (SubscriptCheck)
      SubscriptCheck.removeCall();

    // array.isNativeTypeChecked might be shared among several get_element
    // calls. The last user should delete it.
    if (IsNative && getSingleNonDebugUser((ApplyInst *)IsNative) ==
                      SemanticsCall) {
      deleteAllDebugUses(IsNative);
      (*IsNative).replaceAllUsesWithUndef();
      IsNative.removeCall();
    }
  }
  break;
  }

  SemanticsCall->eraseFromParent();
  SemanticsCall = nullptr;
}

SILValue
swift::ArraySemanticsCall::getArrayPropertyIsNativeTypeChecked() const {
  switch (getKind()) {
    case ArrayCallKind::kCheckSubscript:
      return SemanticsCall->getArgument(1);
    case ArrayCallKind::kGetElement:
      return getTypeCheckedArgument();
    default:
      llvm_unreachable("Must have an array.props argument");
  }
}

bool swift::ArraySemanticsCall::doesNotChangeArray() const {
  switch (getKind()) {
    default: return false;
    case ArrayCallKind::kArrayPropsIsNativeTypeChecked:
    case ArrayCallKind::kCheckSubscript:
    case ArrayCallKind::kCheckIndex:
    case ArrayCallKind::kGetCount:
    case ArrayCallKind::kGetCapacity:
    case ArrayCallKind::kGetElement:
      return true;
  }
}

bool swift::ArraySemanticsCall::mayHaveBridgedObjectElementType() const {
  assert(hasSelf() && "Need self parameter");

  auto Ty = getSelf()->getType();
  if (auto BGT = Ty.getAs<BoundGenericStructType>()) {
    // Check the array element type parameter.
    bool isClass = true;
    for (auto EltTy : BGT->getGenericArgs()) {
      if (EltTy->isBridgeableObjectType())
        return true;
      isClass = false;
    }
    return isClass;
  }
  return true;
}

bool swift::ArraySemanticsCall::canInlineEarly() const {
  switch (getKind()) {
    default:
      return false;
    case ArrayCallKind::kAppendContentsOf:
    case ArrayCallKind::kReserveCapacityForAppend:
    case ArrayCallKind::kAppendElement:
      // append(Element) calls other semantics functions. Therefore it's
      // important that it's inlined by the early inliner (which is before all
      // the array optimizations). Also, this semantics is only used to lookup
      // Array.append(Element), so inlining it does not prevent any other
      // optimization.
      return true;
  }
}

SILValue swift::ArraySemanticsCall::getInitializationCount() const {
  if (getKind() == ArrayCallKind::kArrayUninitialized) {
    // Can be either a call to _adoptStorage or _allocateUninitialized.
    // A call to _adoptStorage has the buffer as AnyObject as the first
    // argument. The count is the second argument.
    // A call to _allocateUninitialized has the count as first argument.
    SILValue Arg0 = SemanticsCall->getArgument(0);
    if (Arg0->getType().isExistentialType() ||
        Arg0->getType().hasReferenceSemantics())
      return SemanticsCall->getArgument(1);
    else return SemanticsCall->getArgument(0);
  }

  if (getKind() == ArrayCallKind::kArrayInit &&
      SemanticsCall->getNumArguments() == 3)
    // Repeated-value array initializer. Arguments are the value to
    // repeat, the count, and the value's type.
    return SemanticsCall->getArgument(1);

  return SILValue();
}

SILValue swift::ArraySemanticsCall::getArrayValue() const {
  if (getKind() == ArrayCallKind::kArrayUninitialized) {
    TupleExtractInst *ArrayDef = nullptr;
    for (auto *Op : SemanticsCall->getUses()) {
      auto *TupleElt = dyn_cast<TupleExtractInst>(Op->getUser());
      if (!TupleElt)
        return SILValue();
      switch (TupleElt->getFieldNo()) {
      default:
        return SILValue();
      case 0: {
          // Should only have one tuple extract after CSE.
        if (ArrayDef)
          return SILValue();
        ArrayDef = TupleElt;
        break;
      }
      case 1: /*Ignore the storage address */ break;
      }
    }
    return SILValue(ArrayDef);
  }

  if (getKind() == ArrayCallKind::kArrayInit)
    return SILValue(SemanticsCall);

  return SILValue();
}

SILValue swift::ArraySemanticsCall::getArrayElementStoragePointer() const {
  if (getKind() == ArrayCallKind::kArrayUninitialized) {
    TupleExtractInst *ArrayElementStorage = nullptr;
    for (auto *Op : SemanticsCall->getUses()) {
      auto *TupleElt = dyn_cast<TupleExtractInst>(Op->getUser());
      if (!TupleElt)
        return SILValue();
      switch (TupleElt->getFieldNo()) {
      default:
        return SILValue();
      case 0: {
        // Ignore the array value.
        break;
      }
      case 1:
        // Should only have one tuple extract after CSE.
        if (ArrayElementStorage)
          return SILValue();
        ArrayElementStorage = TupleElt;
        break;
      }
    }
    return SILValue(ArrayElementStorage);
  }

  return SILValue();
}

bool swift::ArraySemanticsCall::replaceByValue(SILValue V) {
  assert(getKind() == ArrayCallKind::kGetElement &&
         "Must be a get_element call");
  // We only handle loadable types.
  if (!V->getType().isLoadable(*SemanticsCall->getFunction()))
   return false;

  // Expect a check_subscript call or the empty dependence.
  auto SubscriptCheck = getSubscriptCheckArgument();
  ArraySemanticsCall Check(SubscriptCheck, "array.check_subscript");
  auto *EmptyDep = dyn_cast<StructInst>(SubscriptCheck);
  if (!Check && (!EmptyDep || !EmptyDep->getElements().empty()))
    return false;

  SILBuilderWithScope Builder(SemanticsCall);
  auto &ValLowering = Builder.getTypeLowering(V->getType());
  if (hasGetElementDirectResult()) {
    ValLowering.emitCopyValue(Builder, SemanticsCall->getLoc(), V);
    SemanticsCall->replaceAllUsesWith(V);
  } else {
    auto Dest = SemanticsCall->getArgument(0);

    // Expect an alloc_stack initialization.
    auto *ASI = dyn_cast<AllocStackInst>(Dest);
    if (!ASI)
      return false;

    ValLowering.emitCopyValue(Builder, SemanticsCall->getLoc(), V);
    ValLowering.emitStoreOfCopy(Builder, SemanticsCall->getLoc(), V, Dest,
                                IsInitialization_t::IsInitialization);
  }
  removeCall();
  return true;
}

bool swift::ArraySemanticsCall::replaceByAppendingValues(
    SILFunction *AppendFn, SILFunction *ReserveFn,
    const SmallVectorImpl<SILValue> &Vals, SubstitutionMap Subs) {
  assert(getKind() == ArrayCallKind::kAppendContentsOf &&
         "Must be an append_contentsOf call");
  assert(AppendFn && "Must provide an append SILFunction");

  auto *F = SemanticsCall->getFunction();

  // We only handle loadable types.
  if (any_of(Vals, [F](SILValue V) -> bool {
        return !V->getType().isLoadable(*F);
      }))
    return false;
  
  CanSILFunctionType AppendFnTy = AppendFn->getLoweredFunctionType();
  SILValue ArrRef = SemanticsCall->getArgument(1);
  SILBuilderWithScope Builder(SemanticsCall);
  auto Loc = SemanticsCall->getLoc();
  auto *FnRef = Builder.createFunctionRefFor(Loc, AppendFn);

  if (Vals.size() > 1) {
    // Create a call to reserveCapacityForAppend() to reserve space for multiple
    // elements.
    FunctionRefBaseInst *ReserveFnRef =
        Builder.createFunctionRefFor(Loc, ReserveFn);
    SILFunctionType *ReserveFnTy =
      ReserveFnRef->getType().castTo<SILFunctionType>();
    assert(ReserveFnTy->getNumParameters() == 2);
    StructType *IntType =
      ReserveFnTy->getParameters()[0].getArgumentType(F->getModule(), ReserveFnTy)
                 ->castTo<StructType>();
    StructDecl *IntDecl = IntType->getDecl();
    VarDecl *field = IntDecl->getStoredProperties()[0];
    SILType BuiltinIntTy =SILType::getPrimitiveObjectType(
                               field->getInterfaceType()->getCanonicalType());
    IntegerLiteralInst *CapacityLiteral =
      Builder.createIntegerLiteral(Loc, BuiltinIntTy, Vals.size());
    StructInst *Capacity = Builder.createStruct(Loc,
        SILType::getPrimitiveObjectType(CanType(IntType)), {CapacityLiteral});
    Builder.createApply(Loc, ReserveFnRef, Subs, {Capacity, ArrRef});
  }

  for (SILValue V : Vals) {
    auto SubTy = V->getType();
    auto &ValLowering = Builder.getTypeLowering(SubTy);
    auto CopiedVal = ValLowering.emitCopyValue(Builder, Loc, V);
    auto *AllocStackInst = Builder.createAllocStack(Loc, SubTy);

    ValLowering.emitStoreOfCopy(Builder, Loc, CopiedVal, AllocStackInst,
                                IsInitialization_t::IsInitialization);

    SILValue Args[] = {AllocStackInst, ArrRef};
    Builder.createApply(Loc, FnRef, Subs, Args);
    Builder.createDeallocStack(Loc, AllocStackInst);
    if (!isConsumedParameter(AppendFnTy->getParameters()[0].getConvention())) {
      ValLowering.emitDestroyValue(Builder, Loc, CopiedVal);
    }
  }
  CanSILFunctionType AppendContentsOfFnTy =
      SemanticsCall->getReferencedFunctionOrNull()->getLoweredFunctionType();
  if (AppendContentsOfFnTy->getParameters()[0].getConvention() ==
        ParameterConvention::Direct_Owned) {
    SILValue SrcArray = SemanticsCall->getArgument(0);
    Builder.createReleaseValue(SemanticsCall->getLoc(), SrcArray,
                               Builder.getDefaultAtomicity());
  }

  removeCall();

  return true;
}
