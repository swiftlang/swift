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
#include <deque>

using namespace swift;

bool
swift::isSideEffectFree(BuiltinFunctionRefInst *FR) {

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

bool swift::isReadNone(BuiltinFunctionRefInst *FR) {
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

/// \brief Perform a fast local check to see if the instruction is dead.
///
/// This routine only examines the state of the instruction at hand.
bool
swift::isInstructionTriviallyDead(SILInstruction *I) {
  if (!I->use_empty() || isa<TermInst>(I))
    return false;

  // We know that some calls do not have side effects.
  if (const ApplyInst *AI = dyn_cast<ApplyInst>(I)) {
    if (auto *BFRI = dyn_cast<BuiltinFunctionRefInst>(AI->getCallee())) {
      return isSideEffectFree(BFRI);
    }

    if (auto *FRI = dyn_cast<FunctionRefInst>(AI->getCallee()))
      // If we call an apply inst to a global initializer, but the value is not
      // used it is safe to remove it.
      if (FRI->getReferencedFunction()->isGlobalInit())
        return true;
  }

  // condfail instructions that obviously can't fail are dead.
  if (auto *CFI = dyn_cast<CondFailInst>(I))
    if (auto *ILI = dyn_cast<IntegerLiteralInst>(CFI->getOperand()))
      if (!ILI->getValue())
        return true;

  // mark_uninitialized is never dead.
  if (isa<MarkUninitializedInst>(I))
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

  SILBuilder Builder(AI);
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

bool swift::hasAnyExistentialTypes(ArrayRef<Substitution> Subs) {
  // Check whether any of the substitutions are dependent.
  for (auto &sub : Subs)
    if (sub.getReplacement()->getCanonicalType()->isAnyExistentialType())
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
  SILBuilder Builder(Branch);

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

  auto Kind = llvm::StringSwitch<ArrayCallKind>(F->getSemanticsString())
      .Case("array.init", ArrayCallKind::kArrayInit)
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
         getKind() == ArrayCallKind::kSetElement);

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
  switch (getKind()) {
  case ArrayCallKind::kCheckSubscript:
  case ArrayCallKind::kCheckIndex: {
    return canHoistArrayArgument(getSelf(), InsertBefore, DT);
  }

  default:
    break;
  }
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

  case ArrayCallKind::kCheckSubscript:
  case ArrayCallKind::kCheckIndex: {
    auto Self = getSelf();
    // We are going to have a retain, emit a matching release.
    if (!LeaveOriginal)
      SILBuilder(SemanticsCall)
          .createReleaseValue(SemanticsCall->getLoc(), Self);

    // Hoist the array load, if neccessary.
    SILBuilder B(InsertBefore);
    auto NewArrayStructValue = copyArrayLoad(Self, InsertBefore, DT);

    // Retain the array.
    B.createRetainValue(SemanticsCall->getLoc(), NewArrayStructValue);
    auto Call = hoistOrCopyCall(SemanticsCall, InsertBefore, LeaveOriginal, DT);
    Call->setSelfArgument(NewArrayStructValue);
    return Call;
  }

  case ArrayCallKind::kMakeMutable: {
    assert(!LeaveOriginal && "Copying not yet implemented");
    SemanticsCall->moveBefore(InsertBefore);
    placeFuncRef(SemanticsCall, DT);
    return SemanticsCall;
  }

  default:
    llvm_unreachable("Don't know how to hoist this instruction");
    break;
  } // End switch.
}

void swift::ArraySemanticsCall::replaceByRetainValue() {
  assert(getKind() < ArrayCallKind::kMakeMutable &&
         "Must be a semantics call that passes the array by value");
  SILBuilder(SemanticsCall)
      .createReleaseValue(SemanticsCall->getLoc(), getSelf());
  SemanticsCall->eraseFromParent();
}
