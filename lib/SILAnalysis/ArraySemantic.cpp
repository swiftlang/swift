//===- ArraySemantic.cpp - Wrapper around array semantic calls. -*- C++ -*-===//
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

#include "llvm/ADT/StringSwitch.h"
#include "swift/SILAnalysis/ArraySemantic.h"
#include "swift/SILAnalysis/DominanceAnalysis.h"
#include "swift/SILAnalysis/CallGraphAnalysis.h"
#include "swift/SILPasses/Utils/Local.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILFunction.h"

using namespace swift;

static ParameterConvention
getSelfParameterConvention(ApplyInst *SemanticsCall) {
  FunctionRefInst *FRI = cast<FunctionRefInst>(SemanticsCall->getCallee());
  SILFunction *F = FRI->getReferencedFunction();
  auto FnTy = F->getLoweredFunctionType();

  return FnTy->getSelfParameter().getConvention();
}

/// \brief Make sure that all parameters are passed with a reference count
/// neutral parameter convention except for self.
bool swift::ArraySemanticsCall::isValidSignature() {
  assert(SemanticsCall && getKind() != ArrayCallKind::kNone &&
         "Need an array semantic call");
  FunctionRefInst *FRI = cast<FunctionRefInst>(SemanticsCall->getCallee());
  SILFunction *F = FRI->getReferencedFunction();
  auto FnTy = F->getLoweredFunctionType();
  auto &Mod = F->getModule();

  // Check whether we have a valid signature for semantic calls that we hoist.
  switch (getKind()) {
  // All other calls can be consider valid.
  default: break;
  case ArrayCallKind::kArrayPropsIsNative:
  case ArrayCallKind::kArrayPropsIsNativeNoDTC: {
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
        !SemanticsCall->getArgument(0).getType().isTrivial(Mod))
      return false;
    auto SelfConvention = FnTy->getSelfParameter().getConvention();
    return SelfConvention == ParameterConvention::Direct_Guaranteed ||
           SelfConvention == ParameterConvention::Direct_Owned;
  }
  case ArrayCallKind::kCheckSubscript: {
    // Int, Bool, Self
    if (SemanticsCall->getNumArguments() != 3 ||
        !SemanticsCall->getArgument(0).getType().isTrivial(Mod))
      return false;
    if (!SemanticsCall->getArgument(1).getType().isTrivial(Mod))
      return false;
    auto SelfConvention = FnTy->getSelfParameter().getConvention();
    return SelfConvention == ParameterConvention::Direct_Guaranteed ||
           SelfConvention == ParameterConvention::Direct_Owned;
  }
  case ArrayCallKind::kMakeMutable: {
    auto SelfConvention = FnTy->getSelfParameter().getConvention();
    return SelfConvention == ParameterConvention::Indirect_Inout;
  }
  }

  return true;
}

/// Match array semantic calls.
swift::ArraySemanticsCall::ArraySemanticsCall(ValueBase *V,
                                              StringRef SemanticStr,
                                              bool MatchPartialName) {
  if (auto AI = dyn_cast<ApplyInst>(V))
    if (auto FRI = dyn_cast<FunctionRefInst>(AI->getCallee()))
      if (auto FunRef = FRI->getReferencedFunction()) {
        if ((MatchPartialName &&
             (FunRef->hasDefinedSemantics() &&
              FunRef->getSemanticsString().startswith(SemanticStr))) ||
            (!MatchPartialName && FunRef->hasSemanticsString(SemanticStr))) {
          SemanticsCall = AI;
          // Need a 'self' argument otherwise this is not a semantic call that
          // we recognize.
          if (getKind() < ArrayCallKind::kArrayInit && !hasSelf())
            SemanticsCall = nullptr;

          // A arguments must be passed reference count neutral except for self.
          if (SemanticsCall && !isValidSignature())
            SemanticsCall = nullptr;
          return;
        }
      }
  // Otherwise, this is not the semantic call we are looking for.
  SemanticsCall = nullptr;
}

/// Determine which kind of array semantics call this is.
ArrayCallKind swift::ArraySemanticsCall::getKind() const {
  if (!SemanticsCall)
    return ArrayCallKind::kNone;

  auto F = cast<FunctionRefInst>(SemanticsCall->getCallee())
               ->getReferencedFunction();

  auto Kind =
      llvm::StringSwitch<ArrayCallKind>(F->getSemanticsString())
          .Case("array.props.isNative", ArrayCallKind::kArrayPropsIsNative)
          .Case("array.props.isNativeNoDTC",
                ArrayCallKind::kArrayPropsIsNativeNoDTC)
          .Case("array.init", ArrayCallKind::kArrayInit)
          .Case("array.uninitialized", ArrayCallKind::kArrayUninitialized)
          .Case("array.check_subscript", ArrayCallKind::kCheckSubscript)
          .Case("array.check_index", ArrayCallKind::kCheckIndex)
          .Case("array.get_count", ArrayCallKind::kGetCount)
          .Case("array.get_capacity", ArrayCallKind::kGetCapacity)
          .Case("array.get_element", ArrayCallKind::kGetElement)
          .Case("array.owner", ArrayCallKind::kGetArrayOwner)
          .Case("array.make_mutable", ArrayCallKind::kMakeMutable)
          .Case("array.get_element_address", ArrayCallKind::kGetElementAddress)
          .Case("array.mutate_unknown", ArrayCallKind::kMutateUnknown)
          .Default(ArrayCallKind::kNone);

  return Kind;
}

bool swift::ArraySemanticsCall::hasSelf() const {
  assert(SemanticsCall && "Must have a semantics call");
  // Array.init and Array.uninitialized return 'self' @owned.
  return SemanticsCall->getOrigCalleeType()->hasSelfArgument();
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

SILValue swift::ArraySemanticsCall::getIndex() const {
  assert(SemanticsCall && "Must have a semantics call");
  assert(SemanticsCall->getNumArguments() && "Must have arguments");
  assert(getKind() == ArrayCallKind::kCheckSubscript ||
         getKind() == ArrayCallKind::kCheckIndex ||
         getKind() == ArrayCallKind::kGetElement ||
         getKind() == ArrayCallKind::kGetElementAddress);

  return SemanticsCall->getArgument(0);
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

  auto *SelfVal = Arr.getDef();
  auto *SelfBB = SelfVal->getParentBB();
  if (DT->dominates(SelfBB, InsertBefore->getParent()))
    return true;

  if (auto LI = dyn_cast<LoadInst>(SelfVal)) {
    // Are we loading a value from an address in a struct defined at a point
    // dominating the hoist point.
    auto Val = LI->getOperand().getDef();
    bool DoesNotDominate;
    StructElementAddrInst *SEI;
    while ((DoesNotDominate = !DT->dominates(Val->getParentBB(),
                                             InsertBefore->getParent())) &&
           (SEI = dyn_cast<StructElementAddrInst>(Val)))
      Val = SEI->getOperand().getDef();
    return DoesNotDominate == false;
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
  case ArrayCallKind::kArrayPropsIsNative:
  case ArrayCallKind::kArrayPropsIsNativeNoDTC:
  case ArrayCallKind::kGetElementAddress:
    return canHoistArrayArgument(SemanticsCall, getSelf(), InsertBefore, DT);

  case ArrayCallKind::kCheckSubscript:
  case ArrayCallKind::kGetElement: {
    auto IsNativeArg = getArrayPropertyIsNative();
    ArraySemanticsCall IsNative(IsNativeArg.getDef(), "array.props.isNative",
                                true);
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

    if (Kind == ArrayCallKind::kCheckSubscript)
      return canHoistArrayArgument(SemanticsCall, getSelf(), InsertBefore, DT);

    // Can we hoist the needsElementTypeCheck argument.
    ArraySemanticsCall TypeCheck(getArrayPropertyNeedsTypeCheck().getDef(),
                                 "array.props.needsElementTypeCheck", true);
    if (!TypeCheck || !TypeCheck.canHoist(InsertBefore, DT))
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
  if (isa<SILArgument>(ArrayStructValue.getDef())) {
    // Assume that the argument dominates the insert point.
    assert(DT->dominates(ArrayStructValue.getDef()->getParentBB(),
                         InsertBefore->getParent()));
    return ArrayStructValue;
  }

  auto *LI = cast<LoadInst>(ArrayStructValue.getDef());
  if (DT->dominates(LI->getParent(), InsertBefore->getParent()))
    return ArrayStructValue;

  // Recursively move struct_element_addr.
  auto *Val = LI->getOperand().getDef();
  auto *InsertPt = InsertBefore;
  while (!DT->dominates(Val->getParentBB(), InsertBefore->getParent())) {
    auto *Inst = cast<StructElementAddrInst>(Val);
    Inst->moveBefore(InsertPt);
    Val = Inst->getOperand().getDef();
    InsertPt = Inst;
  }

  return SILValue(LI->clone(InsertBefore), 0);
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


/// \brief Hoist or copy the self argument of the semantics call.
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
  if (!LeaveOriginal && IsOwnedSelf)
    SILBuilder(SemanticsCall)
        .createReleaseValue(SemanticsCall->getLoc(), Self)
        ->setDebugScope(SemanticsCall->getDebugScope());

  auto NewArrayStructValue = copyArrayLoad(Self, InsertBefore, DT);

  // Retain the array.
  if (IsOwnedSelf)
    SILBuilder(InsertBefore)
        .createRetainValue(SemanticsCall->getLoc(), NewArrayStructValue)
        ->setDebugScope(SemanticsCall->getDebugScope());

  return NewArrayStructValue;
}

ApplyInst *swift::ArraySemanticsCall::hoistOrCopy(SILInstruction *InsertBefore,
                                                  DominanceInfo *DT,
                                                  bool LeaveOriginal) {
  assert(canHoist(InsertBefore, DT) &&
         "Must be able to hoist the semantics call");

  auto Kind = getKind();
  switch (Kind) {
  case ArrayCallKind::kArrayPropsIsNative:
  case ArrayCallKind::kArrayPropsIsNativeNoDTC: {
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
      auto IsNativeArg = getArrayPropertyIsNative();
      ArraySemanticsCall IsNative(IsNativeArg.getDef(), "array.props.isNative",
                                 true);
      if (!IsNative) {
        // Do we have a constant parameter?
        auto *SI = dyn_cast<StructInst>(IsNativeArg);
        assert(SI && isa<IntegerLiteralInst>(SI->getOperand(0)) &&
               "Must have a constant parameter or an array.props.isNative call "
               "as argument");
        SI->moveBefore(
            DT->findNearestCommonDominator(InsertBefore->getParent(),
                                           SI->getParent())->begin());
        auto *IL = cast<IntegerLiteralInst>(SI->getOperand(0));
        IL->moveBefore(
            DT->findNearestCommonDominator(InsertBefore->getParent(),
                                           IL->getParent())->begin());
      } else {
        NewArrayProps = IsNative.copyTo(InsertBefore, DT);
      }
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

void swift::ArraySemanticsCall::removeCall(CallGraph *CG) {
  if (getSelfParameterConvention(SemanticsCall) ==
      ParameterConvention::Direct_Owned)
    SILBuilderWithScope<1>(SemanticsCall)
        .createReleaseValue(SemanticsCall->getLoc(), getSelf());

  // Invalidate any information in the callgraph.
  if (CG)
    if (auto *Edge = CG->getCallGraphEdge(SemanticsCall))
      CG->removeEdge(Edge);

  SemanticsCall->eraseFromParent();
  SemanticsCall = nullptr;
}

static bool hasArrayPropertyNeedsTypeCheck(ArrayCallKind Kind,
                                           unsigned &ArgIdx) {
  switch (Kind) {
  default: break;
  case ArrayCallKind::kGetElement:
    ArgIdx = 2;
    return true;
  }
  return false;
}
static bool hasArrayPropertyIsNative(ArrayCallKind Kind, unsigned &ArgIdx) {
  switch (Kind) {
  default: break;

  case ArrayCallKind::kCheckSubscript:
  case ArrayCallKind::kGetElement:
    ArgIdx = 1;
    return true;
  }
  return false;
}

SILValue swift::ArraySemanticsCall::getArrayPropertyIsNative() const {
  unsigned ArgIdx = 0;
  bool HasArg = hasArrayPropertyIsNative(getKind(), ArgIdx);
  (void)HasArg;
  assert(HasArg &&
         "Must have an array.props argument");

  return SemanticsCall->getArgument(ArgIdx);
}

SILValue swift::ArraySemanticsCall::getArrayPropertyNeedsTypeCheck() const {
  unsigned ArgIdx = 0;
  bool HasArg = hasArrayPropertyNeedsTypeCheck(getKind(), ArgIdx);
  (void)HasArg;
  assert(HasArg &&
         "Must have an array.props argument");

  return SemanticsCall->getArgument(ArgIdx);
}
