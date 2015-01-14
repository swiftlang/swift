//===--- Local.cpp - Functions that perform local SIL transformations. ---===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===---------------------------------------------------------------------===//
#include "swift/SILPasses/Utils/Local.h"
#include "swift/SILAnalysis/Analysis.h"
#include "swift/SILAnalysis/DominanceAnalysis.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILModule.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/Support/CommandLine.h"
#include <deque>

using namespace swift;

// Do we use array.props?
static bool HaveArrayProperty = false;

bool
swift::isSideEffectFree(BuiltinInst *FR) {

  // First, check if we are dealing with a swift builtin.
  const BuiltinInfo &BInfo = FR->getBuiltinInfo();
  if (BInfo.ID != BuiltinValueKind::None) {
    return BInfo.isReadNone();
  }

  // Second, specialcase llvm intrinsic.
  const IntrinsicInfo & IInfo = FR->getIntrinsicInfo();
  if (IInfo.ID != llvm::Intrinsic::not_intrinsic) {
    return ( (IInfo.hasAttribute(llvm::Attribute::ReadNone) ||
              IInfo.hasAttribute(llvm::Attribute::ReadOnly)) &&
            IInfo.hasAttribute(llvm::Attribute::NoUnwind) );
  }

  llvm_unreachable("All cases are covered.");
}

bool swift::isReadNone(BuiltinInst *FR) {
  // First, check if we are dealing with a swift builtin.
  const BuiltinInfo &BInfo = FR->getBuiltinInfo();
  if (BInfo.ID != BuiltinValueKind::None)
    return BInfo.isReadNone();

  // Second, specialcase llvm intrinsic.
  const IntrinsicInfo & IInfo = FR->getIntrinsicInfo();
  if (IInfo.ID != llvm::Intrinsic::not_intrinsic)
    return IInfo.hasAttribute(llvm::Attribute::ReadNone) &&
      IInfo.hasAttribute(llvm::Attribute::NoUnwind);

  llvm_unreachable("All cases are covered.");
}

bool swift::isReadNone(FunctionRefInst *FR) {
  auto *F = FR->getReferencedFunction();
  if (!F)
    return false;
  return F->getEffectsInfo() == EffectsKind::ReadNone;
}

llvm::cl::opt<bool>
DebugValuesPropagateLiveness("debug-values-propagate-liveness",
                             llvm::cl::init(false));

bool swift::debugValuesPropagateLiveness() {
#ifndef NDEBUG
  return DebugValuesPropagateLiveness;
#else
  return false;
#endif
}

/// \brief Perform a fast local check to see if the instruction is dead.
///
/// This routine only examines the state of the instruction at hand.
bool
swift::isInstructionTriviallyDead(SILInstruction *I) {
  if (!I->use_empty() || isa<TermInst>(I))
    return false;

  // We know that some calls do not have side effects.
  if (const ApplyInst *AI = dyn_cast<ApplyInst>(I)) {
    if (auto *FRI = dyn_cast<FunctionRefInst>(AI->getCallee()))
      // If we call an apply inst to a global initializer, but the value is not
      // used it is safe to remove it.
      if (FRI->getReferencedFunction()->isGlobalInit())
        return true;
  }

  if (auto *BI = dyn_cast<BuiltinInst>(I)) {
    return isSideEffectFree(BI);
  }

  // condfail instructions that obviously can't fail are dead.
  if (auto *CFI = dyn_cast<CondFailInst>(I))
    if (auto *ILI = dyn_cast<IntegerLiteralInst>(CFI->getOperand()))
      if (!ILI->getValue())
        return true;

  // mark_uninitialized is never dead.
  if (isa<MarkUninitializedInst>(I))
    return false;

  if (debugValuesPropagateLiveness() &&
      (isa<DebugValueInst>(I) || isa<DebugValueAddrInst>(I)))
    return false;

  // These invalidate enums so "write" memory, but that is not an essential
  // operation so we can remove these if they are trivially dead.
  if (isa<UncheckedTakeEnumDataAddrInst>(I))
    return true;
  
  if (!I->mayHaveSideEffects())
    return true;

  return false;
}

namespace {
  using CallbackTy = std::function<void(SILInstruction *)>;
} // end anonymous namespace

bool swift::
recursivelyDeleteTriviallyDeadInstructions(ArrayRef<SILInstruction *> IA,
                                           bool Force, CallbackTy Callback) {
  // Delete these instruction and others that become dead after it's deleted.
  llvm::SmallPtrSet<SILInstruction *, 8> DeadInsts;
  for (auto I : IA) {
    // If the instruction is not dead and force is false, do nothing.
    if (Force || isInstructionTriviallyDead(I))
      DeadInsts.insert(I);
  }
  llvm::SmallPtrSet<SILInstruction *, 8> NextInsts;
  while (!DeadInsts.empty()) {
    for (auto I : DeadInsts) {
      // Call the callback before we mutate the to be deleted instruction in any
      // way.
      Callback(I);

      // Check if any of the operands will become dead as well.
      MutableArrayRef<Operand> Ops = I->getAllOperands();
      for (Operand &Op : Ops) {
        SILValue OpVal = Op.get();
        if (!OpVal)
          continue;

        // Remove the reference from the instruction being deleted to this
        // operand.
        Op.drop();

        // If the operand is an instruction that is only used by the instruction
        // being deleted, delete it.
        if (SILInstruction *OpValInst = dyn_cast<SILInstruction>(OpVal))
          if (!DeadInsts.count(OpValInst) &&
              isInstructionTriviallyDead(OpValInst))
            NextInsts.insert(OpValInst);
      }

      // If we have a function ref inst, we need to especially drop its function
      // argument so that it gets a proper ref decement.
      auto *FRI = dyn_cast<FunctionRefInst>(I);
      if (FRI && FRI->getReferencedFunction())
        FRI->dropReferencedFunction();
    }

    for (auto I : DeadInsts) {
      // This will remove this instruction and all its uses.
      I->eraseFromParent();
    }

    NextInsts.swap(DeadInsts);
    NextInsts.clear();
  }

  return true;
}

/// \brief If the given instruction is dead, delete it along with its dead
/// operands.
///
/// \param I The instruction to be deleted.
/// \param Force If Force is set, don't check if the top level instruction is
///        considered dead - delete it regardless.
/// \return Returns true if any instructions were deleted.
bool swift::recursivelyDeleteTriviallyDeadInstructions(SILInstruction *I,
                                                       bool Force,
                                                       CallbackTy Callback) {

  ArrayRef<SILInstruction *> AI = ArrayRef<SILInstruction *>(I);
  return recursivelyDeleteTriviallyDeadInstructions(AI, Force, Callback);
}

void swift::eraseUsesOfInstruction(SILInstruction *Inst) {
  for (auto UI : Inst->getUses()) {
    auto *User = UI->getUser();

    // If the instruction itself has any uses, recursively zap them so that
    // nothing uses this instruction.
    eraseUsesOfInstruction(User);

    // Walk through the operand list and delete any random instructions that
    // will become trivially dead when this instruction is removed.

    for (auto &Op : User->getAllOperands()) {
      if (auto *OpI = dyn_cast<SILInstruction>(Op.get())) {
        // Don't recursively delete the pointer we're getting in.
        if (OpI != Inst) {
          Op.drop();
          recursivelyDeleteTriviallyDeadInstructions(OpI);
        }
      }
    }

    User->eraseFromParent();
  }
}

void swift::replaceWithSpecializedFunction(ApplyInst *AI, SILFunction *NewF) {
  SILLocation Loc = AI->getLoc();
  ArrayRef<Substitution> Subst;

  SmallVector<SILValue, 4> Arguments;
  for (auto &Op : AI->getArgumentOperands()) {
    Arguments.push_back(Op.get());
  }

  SILBuilderWithScope<2> Builder(AI);
  FunctionRefInst *FRI = Builder.createFunctionRef(Loc, NewF);

  ApplyInst *NAI =
      Builder.createApply(Loc, FRI, Arguments, AI->isTransparent());
  AI->replaceAllUsesWith(NAI);
  recursivelyDeleteTriviallyDeadInstructions(AI, true);
}

bool swift::hasUnboundGenericTypes(TypeSubstitutionMap &SubsMap) {
  // Check whether any of the substitutions are dependent.
  for (auto &entry : SubsMap)
    if (entry.second->getCanonicalType()->hasArchetype())
      return true;

  return false;
}

bool swift::hasUnboundGenericTypes(ArrayRef<Substitution> Subs) {
  // Check whether any of the substitutions are dependent.
  for (auto &sub : Subs)
    if (sub.getReplacement()->getCanonicalType()->hasArchetype())
      return true;
  return false;
}

/// Find a new position for an ApplyInst's FuncRef so that it dominates its
/// use. Not that FuncionRefInsts may be shared by multiple ApplyInsts.
void swift::placeFuncRef(ApplyInst *AI, DominanceInfo *DT) {
  FunctionRefInst *FuncRef = cast<FunctionRefInst>(AI->getCallee());
  SILBasicBlock *DomBB =
    DT->findNearestCommonDominator(AI->getParent(), FuncRef->getParent());
  if (DomBB == AI->getParent() && DomBB != FuncRef->getParent())
    // Prefer to place the FuncRef immediately before the call. Since we're
    // moving FuncRef up, this must be the only call to it in the block.
    FuncRef->moveBefore(AI);
  else
    // Otherwise, conservatively stick it at the beginning of the block.
    FuncRef->moveBefore(DomBB->begin());
}

/// \brief Add an argument, \p val, to the branch-edge that is pointing into
/// block \p Dest. Return a new instruction and do not erase the old
/// instruction.
TermInst *swift::addArgumentToBranch(SILValue Val, SILBasicBlock *Dest,
                                     TermInst *Branch) {
  SILBuilderWithScope<2> Builder(Branch);

  if (CondBranchInst *CBI = dyn_cast<CondBranchInst>(Branch)) {
    SmallVector<SILValue, 8> TrueArgs;
    SmallVector<SILValue, 8> FalseArgs;

    for (auto A : CBI->getTrueArgs())
      TrueArgs.push_back(A);

    for (auto A : CBI->getFalseArgs())
      FalseArgs.push_back(A);

    if (Dest == CBI->getTrueBB()) {
      TrueArgs.push_back(Val);
      assert(TrueArgs.size() == Dest->getNumBBArg());
    } else {
      FalseArgs.push_back(Val);
      assert(FalseArgs.size() == Dest->getNumBBArg());
    }

    return Builder.createCondBranch(CBI->getLoc(), CBI->getCondition(),
                                    CBI->getTrueBB(), TrueArgs,
                                    CBI->getFalseBB(), FalseArgs);
  }

  if (BranchInst *BI = dyn_cast<BranchInst>(Branch)) {
    SmallVector<SILValue, 8> Args;

    for (auto A : BI->getArgs())
      Args.push_back(A);

    Args.push_back(Val);
    assert(Args.size() == Dest->getNumBBArg());
    return Builder.createBranch(BI->getLoc(), BI->getDestBB(), Args);
  }

  llvm_unreachable("unsupported terminator");
}

SILLinkage swift::getSpecializedLinkage(SILLinkage L) {
  switch (L) {
  case SILLinkage::Public:
  case SILLinkage::PublicExternal:
  case SILLinkage::Shared:
  case SILLinkage::SharedExternal:
  case SILLinkage::Hidden:
  case SILLinkage::HiddenExternal:
    // Specializations of public or hidden symbols can be shared by all TUs
    // that specialize the definition.
    return SILLinkage::Shared;

  case SILLinkage::Private:
  case SILLinkage::PrivateExternal:
    // Specializations of private symbols should remain so.
    // TODO: maybe PrivateExternals should get SharedExternal (these are private
    // functions from the stdlib which are specialized in another module).
    return SILLinkage::Private;
  }
}

/// Match array semantic calls.
swift::ArraySemanticsCall::ArraySemanticsCall(ValueBase *V,
                                              StringRef SemanticStr,
                                              bool MatchPartialName) {
  if (auto AI = dyn_cast<ApplyInst>(V))
    if (auto FRI = dyn_cast<FunctionRefInst>(AI->getCallee()))
      if (auto FunRef = FRI->getReferencedFunction()) {
        if (MatchPartialName) {
          if (FunRef->hasDefinedSemantics() &&
              FunRef->getSemanticsString().startswith(SemanticStr)) {
            SemanticsCall = AI;
            return;
          }
        } else {
          if (FunRef->hasSemanticsString(SemanticStr)) {
            SemanticsCall = AI;
            return;
          }
        }
      }
  // Otherwise, this is not the semantic call we are looking for.
  SemanticsCall = nullptr;
}

/// Determine which kind of array semantics call this is.
ArrayCallKind swift::ArraySemanticsCall::getKind() {
  if (!SemanticsCall)
    return ArrayCallKind::kNone;

  auto F = cast<FunctionRefInst>(SemanticsCall->getCallee())
               ->getReferencedFunction();

  auto Kind =
      llvm::StringSwitch<ArrayCallKind>(F->getSemanticsString())
          .Case("array.props.isNative", ArrayCallKind::kArrayPropsIsNative)
          .Case("array.props.needsElementTypeCheck",
                ArrayCallKind::kArrayPropsNeedsTypeCheck)
          .Case("array.init", ArrayCallKind::kArrayInit)
          .Case("array.uninitialized", ArrayCallKind::kArrayUninitialized)
          .Case("array.check_subscript", ArrayCallKind::kCheckSubscript)
          .Case("array.check_index", ArrayCallKind::kCheckIndex)
          .Case("array.get_count", ArrayCallKind::kGetCount)
          .Case("array.get_capacity", ArrayCallKind::kGetCapacity)
          .Case("array.get_element", ArrayCallKind::kGetElement)
          .Case("array.make_mutable", ArrayCallKind::kMakeMutable)
          .Case("array.get_element_address", ArrayCallKind::kGetElementAddress)
          .Case("array.mutate_unknown", ArrayCallKind::kMutateUnknown)
          .Default(ArrayCallKind::kNone);

  return Kind;
}

SILValue swift::ArraySemanticsCall::getSelf() {
  assert(SemanticsCall && "Must have a semantics call");
  assert(SemanticsCall->getNumArguments() && "Must have arguments");
  return SemanticsCall->getSelfArgument();
}

SILValue swift::ArraySemanticsCall::getIndex() {
  assert(SemanticsCall && "Must have a semantics call");
  assert(SemanticsCall->getNumArguments() && "Must have arguments");
  assert(getKind() == ArrayCallKind::kCheckSubscript ||
         getKind() == ArrayCallKind::kCheckIndex ||
         getKind() == ArrayCallKind::kGetElement ||
         getKind() == ArrayCallKind::kGetElementAddress);

  return SemanticsCall->getArgument(0);
}

static bool canHoistArrayArgument(SILValue Arr, SILInstruction *InsertBefore,
                                  DominanceInfo *DT) {
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
                                         DominanceInfo *DT) {
  auto Kind = getKind();
  switch (Kind) {
  default:
    break;

  case ArrayCallKind::kCheckIndex:
  case ArrayCallKind::kArrayPropsIsNative:
  case ArrayCallKind::kArrayPropsNeedsTypeCheck:
  case ArrayCallKind::kGetElementAddress:
    return canHoistArrayArgument(getSelf(), InsertBefore, DT);

  case ArrayCallKind::kCheckSubscript:
  case ArrayCallKind::kGetElement: {
    if (HaveArrayProperty) {
      auto IsNativeArg = getArrayPropertyIsNative();
      ArraySemanticsCall IsNative(IsNativeArg.getDef(),
                                 "array.props.isNative", true);
      if (!IsNative) {
        // Do we have a constant parameter?
        auto *SI = dyn_cast<StructInst>(IsNativeArg);
        if (!SI)
          return false;
        if (!isa<IntegerLiteralInst>(SI->getOperand(0)))
          return false;
      } else if(!IsNative.canHoist(InsertBefore, DT))
        // Otherwise, we must be able to hoist the function call.
        return false;

      if (Kind == ArrayCallKind::kCheckSubscript)
        return canHoistArrayArgument(getSelf(), InsertBefore, DT);

      // Can we hoist the needsElementTypeCheck argument.
      ArraySemanticsCall TypeCheck(getArrayPropertyNeedsTypeCheck().getDef(),
                                  "array.props.needsElementTypeCheck", true);
      if (!TypeCheck || !TypeCheck.canHoist(InsertBefore, DT))
        return false;
    }

    return canHoistArrayArgument(getSelf(), InsertBefore, DT);
  }

  case ArrayCallKind::kMakeMutable: {
    return canHoistArrayArgument(getSelf(), InsertBefore, DT);
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

ApplyInst *swift::ArraySemanticsCall::hoistOrCopy(SILInstruction *InsertBefore,
                                                  DominanceInfo *DT,
                                                  bool LeaveOriginal) {
  auto Kind = getKind();
  switch (Kind) {
  case ArrayCallKind::kArrayPropsIsNative:
  case ArrayCallKind::kArrayPropsNeedsTypeCheck: {
    auto Self = getSelf();
    // Emit matching release if we are removing the original call.
    if (!LeaveOriginal)
      SILBuilder(SemanticsCall)
          .createReleaseValue(SemanticsCall->getLoc(), Self);

    auto NewArrayStructValue = copyArrayLoad(Self, InsertBefore, DT);

    SILBuilder B(InsertBefore);

    // Retain the array.
    B.createRetainValue(SemanticsCall->getLoc(), NewArrayStructValue);

    auto *Call =
        hoistOrCopyCall(SemanticsCall, InsertBefore, LeaveOriginal, DT);
    Call->setSelfArgument(NewArrayStructValue);
    return Call;
  }

  case ArrayCallKind::kCheckSubscript:
  case ArrayCallKind::kCheckIndex: {
    auto Self = getSelf();
    // We are going to have a retain, emit a matching release.
    if (!LeaveOriginal)
      SILBuilderWithScope<1>(SemanticsCall)
          .createReleaseValue(SemanticsCall->getLoc(), Self);

    // Hoist the array load, if neccessary.
    SILBuilder B(InsertBefore);
    auto NewArrayStructValue = copyArrayLoad(Self, InsertBefore, DT);

    // Retain the array.
    B.createRetainValue(SemanticsCall->getLoc(), NewArrayStructValue)
        ->setDebugScope(SemanticsCall->getDebugScope());

    SILValue NewArrayProps;
    if (HaveArrayProperty && Kind == ArrayCallKind::kCheckSubscript) {
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
    Call->setSelfArgument(NewArrayStructValue);

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

void swift::ArraySemanticsCall::replaceByRetainValue() {
  assert(getKind() < ArrayCallKind::kMakeMutable &&
         "Must be a semantics call that passes the array by value");
  SILBuilderWithScope<1>(SemanticsCall)
      .createReleaseValue(SemanticsCall->getLoc(), getSelf());
  SemanticsCall->eraseFromParent();
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

SILValue swift::ArraySemanticsCall::getArrayPropertyIsNative() {
  unsigned ArgIdx = 0;
  bool HasArg = hasArrayPropertyIsNative(getKind(), ArgIdx);
  (void)HasArg;
  assert(HasArg &&
         "Must have an array.props argument");

  return SemanticsCall->getArgument(ArgIdx);
}

SILValue swift::ArraySemanticsCall::getArrayPropertyNeedsTypeCheck() {
  unsigned ArgIdx = 0;
  bool HasArg = hasArrayPropertyNeedsTypeCheck(getKind(), ArgIdx);
  (void)HasArg;
  assert(HasArg &&
         "Must have an array.props argument");

  return SemanticsCall->getArgument(ArgIdx);
}
/// Remove all instructions in the body of \p BB in safe manner by using
/// undef.
void swift::clearBlockBody(SILBasicBlock *BB) {
  // Instructions in the dead block may be used by other dead blocks.  Replace
  // any uses of them with undef values.
  while (!BB->empty()) {
    // Grab the last instruction in the BB.
    auto *Inst = &BB->getInstList().back();

    // Replace any non-dead results with SILUndef values.
    Inst->replaceAllUsesWithUndef();

    // Pop the instruction off of the back of the basic block.
    BB->getInstList().pop_back();
  }

}

// Handle the mechanical aspects of removing an unreachable block.
void swift::removeDeadBlock(SILBasicBlock *BB) {
  // Clear the body of BB.
  clearBlockBody(BB);

  // Now that the BB is empty, eliminate it.
  BB->eraseFromParent();
}

/// Checks operands of a string concatenation operation to see if
/// optimization is applicable.
///
/// Returns false if optimization is not possible.
/// Returns true and initializes internal fields if optimization is possible.
bool StringConcatenationOptimizer::extractStringConcatOperands() {
  auto *FRI = dyn_cast<FunctionRefInst>(AI->getCallee());
  if (!FRI)
    return false;

  auto *FRIFun = FRI->getReferencedFunction();

  if (AI->getNumOperands() != 3 ||
      !FRIFun->hasSemanticsString("string.concat"))
    return false;

  // Left and right operands of a string concatenation operation.
  AILeft = dyn_cast<ApplyInst>(AI->getOperand(1));
  AIRight = dyn_cast<ApplyInst>(AI->getOperand(2));

  if (!AILeft || !AIRight)
    return false;

  FRILeft = dyn_cast<FunctionRefInst>(AILeft->getCallee());
  FRIRight = dyn_cast<FunctionRefInst>(AIRight->getCallee());

  if (!FRILeft || !FRIRight)
    return false;

  auto *FRILeftFun = FRILeft->getReferencedFunction();
  auto *FRIRightFun = FRIRight->getReferencedFunction();

  if (FRILeftFun->getEffectsInfo() >= EffectsKind::ReadWrite ||
      FRIRightFun->getEffectsInfo() >= EffectsKind::ReadWrite)
    return false;

  if (!FRILeftFun->hasDefinedSemantics() ||
      !FRIRightFun->hasDefinedSemantics())
    return false;

  auto SemanticsLeft = FRILeftFun->getSemanticsString();
  auto SemanticsRight = FRIRightFun->getSemanticsString();
  auto AILeftOperandsNum = AILeft->getNumOperands();
  auto AIRightOperandsNum = AIRight->getNumOperands();

  // makeUTF16 should have following parameters:
  // (start: RawPointer, numberOfCodeUnits: Word)
  // makeUTF8 should have following parameters:
  // (start: RawPointer, byteSize: Word, isASCII: Int1)
  if (!((SemanticsLeft == "string.makeUTF16" && AILeftOperandsNum == 4) ||
        (SemanticsLeft == "string.makeUTF8" && AILeftOperandsNum == 5) ||
        (SemanticsRight == "string.makeUTF16" && AIRightOperandsNum == 4) ||
        (SemanticsRight == "string.makeUTF8" && AIRightOperandsNum == 5)))
    return false;

  SLILeft = dyn_cast<StringLiteralInst>(AILeft->getOperand(1));
  SLIRight = dyn_cast<StringLiteralInst>(AIRight->getOperand(1));

  if (!SLILeft || !SLIRight)
    return false;

  // Only UTF-8 and UTF-16 encoded string literals are supported by this
  // optimization.
  if (SLILeft->getEncoding() != StringLiteralInst::Encoding::UTF8 &&
      SLILeft->getEncoding() != StringLiteralInst::Encoding::UTF16)
    return false;

  if (SLIRight->getEncoding() != StringLiteralInst::Encoding::UTF8 &&
      SLIRight->getEncoding() != StringLiteralInst::Encoding::UTF16)
    return false;

  return true;
}

/// Ensures that both string literals to be concatenated use the same
/// UTF encoding. Converts UTF-8 into UTF-16 if required.
void StringConcatenationOptimizer::adjustEncodings() {
  if (SLILeft->getEncoding() == SLIRight->getEncoding()) {
    FRIConvertFromBuiltin = FRILeft;
    IsTransparent = AILeft->isTransparent();
    if (SLILeft->getEncoding() == StringLiteralInst::Encoding::UTF8) {
      FuncResultType = AILeft->getOperand(4);
    } else {
      FuncResultType = AILeft->getOperand(3);
    }
    return;
  }

  // If one of the string literals is UTF8 and another one is UTF16,
  // convert the UTF8-encoded string literal into UTF16-encoding first.
  if (SLILeft->getEncoding() == StringLiteralInst::Encoding::UTF8 &&
      SLIRight->getEncoding() == StringLiteralInst::Encoding::UTF16) {
    FuncResultType = AIRight->getOperand(3);
    FRIConvertFromBuiltin = FRIRight;
    IsTransparent = AIRight->isTransparent();
    // Convert UTF8 representation into UTF16.
    SLILeft = Builder->createStringLiteral(AI->getLoc(), SLILeft->getValue(),
                                           StringLiteralInst::Encoding::UTF16);
    SLILeft->setDebugScope(AI->getDebugScope());
  }

  if (SLIRight->getEncoding() == StringLiteralInst::Encoding::UTF8 &&
      SLILeft->getEncoding() == StringLiteralInst::Encoding::UTF16) {
    FuncResultType = AILeft->getOperand(3);
    FRIConvertFromBuiltin = FRILeft;
    IsTransparent = AILeft->isTransparent();
    // Convert UTF8 representation into UTF16.
    SLIRight = Builder->createStringLiteral(AI->getLoc(), SLIRight->getValue(),
                                            StringLiteralInst::Encoding::UTF16);
    SLIRight->setDebugScope(AI->getDebugScope());
  }

  // It should be impossible to have two operands with different
  // encodings at this point.
  assert(SLILeft->getEncoding() == SLIRight->getEncoding() &&
        "Both operands of string concatenation should have the same encoding");
}

/// Computes the length of a concatenated string literal.
APInt StringConcatenationOptimizer::getConcatenatedLength() {
  // Real length of string literals computed based on its contents.
  // Length is in code units.
  auto SLILenLeft = SLILeft->getCodeUnitCount();
  (void) SLILenLeft;
  auto SLILenRight = SLIRight->getCodeUnitCount();
  (void) SLILenRight;

  // Length of string literals as reported by string.make functions.
  auto *LenLeft = dyn_cast<IntegerLiteralInst>(AILeft->getOperand(2));
  auto *LenRight = dyn_cast<IntegerLiteralInst>(AIRight->getOperand(2));

  // Real and reported length should be the same.
  assert(SLILenLeft == LenLeft->getValue() &&
         "Size of string literal in @semantics(string.make) is wrong");

  assert(SLILenRight == LenRight->getValue() &&
         "Size of string literal in @semantics(string.make) is wrong");


  // Compute length of the concatenated literal.
  return LenLeft->getValue() + LenRight->getValue();
}

/// Computes the isAscii flag of a concatenated UTF8-encoded string literal.
bool StringConcatenationOptimizer::isAscii() const{
  // Add the isASCII argument in case of UTF8.
  // IsASCII is true only if IsASCII of both literals is true.
  auto *AsciiLeft = dyn_cast<IntegerLiteralInst>(AILeft->getOperand(3));
  auto *AsciiRight = dyn_cast<IntegerLiteralInst>(AIRight->getOperand(3));
  auto IsAsciiLeft = AsciiLeft->getValue() == 1;
  auto IsAsciiRight = AsciiRight->getValue() == 1;
  return IsAsciiLeft && IsAsciiRight;
}

SILInstruction *StringConcatenationOptimizer::optimize() {
  // Bail out if string literals concatenation optimization is
  // not possible.
  if (!extractStringConcatOperands())
    return nullptr;

  // Perform string literal encodings adjustments if needed.
  adjustEncodings();

  // Arguments of the new StringLiteralInst to be created.
  SmallVector<SILValue, 4> Arguments;

  // Encoding to be used for the concatenated string literal.
  auto Encoding = SLILeft->getEncoding();

  // Create a concatenated string literal.
  auto LV = SLILeft->getValue();
  auto RV = SLIRight->getValue();
  auto *NewSLI = Builder->createStringLiteral(AI->getLoc(),
                                              LV + Twine(RV),
                                              Encoding);
  NewSLI->setDebugScope(AI->getDebugScope());
  Arguments.push_back(NewSLI);

  // Length of the concatenated literal according to its encoding.
  auto *Len = Builder->createIntegerLiteral(AI->getLoc(),
                                            AILeft->getOperand(2).getType(),
                                            getConcatenatedLength());
  Len->setDebugScope(AI->getDebugScope());
  Arguments.push_back(Len);

  // isAscii flag for UTF8-encoded string literals.
  if (Encoding == StringLiteralInst::Encoding::UTF8) {
    bool IsAscii = isAscii();
    auto ILType = AILeft->getOperand(3).getType();
    auto *Ascii = Builder->createIntegerLiteral(AI->getLoc(),
                                                ILType,
                                                intmax_t(IsAscii));
    Ascii->setDebugScope(AI->getDebugScope());
    Arguments.push_back(Ascii);
  }

  // Type.
  Arguments.push_back(FuncResultType);

  auto FnTy = FRIConvertFromBuiltin->getType();
  auto STResultType = FnTy.castTo<SILFunctionType>()->getResult().getSILType();
  return ApplyInst::create(AI->getLoc(),
                           FRIConvertFromBuiltin,
                           FnTy,
                           STResultType,
                           ArrayRef<Substitution>(),
                           Arguments,
                           IsTransparent,
                           *FRIConvertFromBuiltin->getReferencedFunction());
}

