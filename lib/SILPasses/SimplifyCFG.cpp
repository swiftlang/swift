//===--- SimplifyCFG.cpp - Clean up the SIL CFG ---------------------------===//
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

#define DEBUG_TYPE "sil-simplify-cfg"
#include "swift/SILPasses/Passes.h"
#include "swift/SIL/Dominance.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILCloner.h"
#include "swift/SIL/SILModule.h"
#include "swift/SIL/SILUndef.h"
#include "swift/SILAnalysis/DominanceAnalysis.h"
#include "swift/SILAnalysis/SimplifyInstruction.h"
#include "swift/SILPasses/Transforms.h"
#include "swift/SILPasses/Utils/CFG.h"
#include "swift/SILPasses/Utils/Local.h"
#include "swift/SILPasses/Utils/SILInliner.h"
#include "swift/SILPasses/Utils/SILSSAUpdater.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/Debug.h"
using namespace swift;

STATISTIC(NumBlocksDeleted,  "Number of unreachable blocks removed");
STATISTIC(NumBlocksMerged,   "Number of blocks merged together");
STATISTIC(NumJumpThreads,    "Number of jumps threaded");
STATISTIC(NumConstantFolded, "Number of terminators constant folded");
STATISTIC(NumDeadArguments,  "Number of unused arguments removed");

//===----------------------------------------------------------------------===//
//                           alloc_box Promotion
//===----------------------------------------------------------------------===//

namespace {
  class SimplifyCFG {
    SILFunction &Fn;
    SILPassManager *PM;

    // WorklistList is the actual list that we iterate over (for determinism).
    // Slots may be null, which should be ignored.
    SmallVector<SILBasicBlock*, 32> WorklistList;
    // WorklistMap keeps track of which slot a BB is in, allowing efficient
    // containment query, and allows efficient removal.
    llvm::SmallDenseMap<SILBasicBlock*, unsigned, 32> WorklistMap;

  public:
    SimplifyCFG(SILFunction &Fn, SILPassManager *PM) :
      Fn(Fn), PM(PM) {}

    bool run();

  private:
    /// popWorklist - Return the next basic block to look at, or null if the
    /// worklist is empty.  This handles skipping over null entries in the
    /// worklist.
    SILBasicBlock *popWorklist() {
      while (!WorklistList.empty())
        if (auto *BB = WorklistList.pop_back_val()) {
          WorklistMap.erase(BB);
          return BB;
        }

      return nullptr;
    }

    /// addToWorklist - Add the specified block to the work list if it isn't
    /// already present.
    void addToWorklist(SILBasicBlock *BB) {
      unsigned &Entry = WorklistMap[BB];
      if (Entry != 0) return;
      WorklistList.push_back(BB);
      Entry = WorklistList.size();
    }

    /// removeFromWorklist - Remove the specified block from the worklist if
    /// present.
    void removeFromWorklist(SILBasicBlock *BB) {
      assert(BB && "Cannot add null pointer to the worklist");
      auto It = WorklistMap.find(BB);
      if (It == WorklistMap.end()) return;

      // If the BB is in the worklist, null out its entry.
      if (It->second) {
        assert(WorklistList[It->second-1] == BB && "Consistency error");
        WorklistList[It->second-1] = nullptr;
      }

      // Remove it from the map as well.
      WorklistMap.erase(It);
    }

    bool simplifyBlocks();
    void canonicalizeSwitchEnums();
    bool dominatorBasedSimplify(DominanceInfo *DT);

    /// \brief Remove the basic block if it has no predecessors. Returns true
    /// If the block was removed.
    bool removeIfDead(SILBasicBlock *BB);

    bool tryJumpThreading(BranchInst *BI);
    bool simplifyAfterDroppingPredecessor(SILBasicBlock *BB);

    bool simplifyBranchOperands(OperandValueArrayRef Operands);
    bool simplifyBranchBlock(BranchInst *BI);
    bool simplifyCondBrBlock(CondBranchInst *BI);
    bool simplifySwitchEnumUnreachableBlocks(SwitchEnumInst *SEI);
    bool simplifySwitchEnumBlock(SwitchEnumInst *SEI);
    bool simplifyUnreachableBlock(UnreachableInst *UI);
    bool simplifyArgument(SILBasicBlock *BB, unsigned i);
    bool simplifyArgs(SILBasicBlock *BB);
  };

  class RemoveUnreachable {
    SILFunction &Fn;
    llvm::SmallSet<SILBasicBlock *, 8> Visited;
  public:
    RemoveUnreachable(SILFunction &Fn) : Fn(Fn) { }
    void visit(SILBasicBlock *BB);
    bool run();
  };
} // end anonymous namespace



static bool isConditional(TermInst *I) {
  switch (I->getKind()) {
  case ValueKind::CondBranchInst:
  case ValueKind::SwitchValueInst:
  case ValueKind::SwitchEnumInst:
  case ValueKind::SwitchEnumAddrInst:
  case ValueKind::CheckedCastBranchInst:
    return true;
  default:
    return false;
  }
}

// Replace a SwitchEnumInst with an unconditional branch based on the
// assertion that it will select a particular element.
static void simplifySwitchEnumInst(SwitchEnumInst *SEI,
                                   EnumElementDecl *Element,
                                   SILBasicBlock *BB) {
  auto *Dest = SEI->getCaseDestination(Element);

  if (Dest->bbarg_empty()) {
    SILBuilderWithScope<1>(SEI).createBranch(SEI->getLoc(), Dest);
    SEI->eraseFromParent();
    return;
  }

  SILValue Arg;

  if (BB->bbarg_empty()) {
    auto &Mod = SEI->getModule();
    auto OpndTy = SEI->getOperand()->getType(0);
    auto Ty = OpndTy.getEnumElementType(Element, Mod);
    auto *UED = SILBuilderWithScope<1>(SEI)
      .createUncheckedEnumData(SEI->getLoc(), SEI->getOperand(), Element, Ty);
    Arg = SILValue(UED);
  } else {
    Arg = BB->getBBArg(0);
  }

  ArrayRef<SILValue> Args = { Arg };
  SILBuilderWithScope<1>(SEI).createBranch(SEI->getLoc(), Dest, Args);
  SEI->eraseFromParent();
}

static void simplifyCheckedCastBranchInst(CheckedCastBranchInst *CCBI,
                                          bool SuccessTaken,
                                          SILBasicBlock *DomBB) {
  if (SuccessTaken)
    SILBuilderWithScope<1>(CCBI).createBranch(CCBI->getLoc(),
                                              CCBI->getSuccessBB(),
                                              SILValue(DomBB->getBBArg(0)));
  else
    SILBuilderWithScope<1>(CCBI).createBranch(CCBI->getLoc(),
                                              CCBI->getFailureBB());

  CCBI->eraseFromParent();
}

/// Returns true if BB is the basic block jumped to when CondBr's condition
/// evaluates to true.
static bool getBranchTaken(CondBranchInst *CondBr, SILBasicBlock *BB) {
  if (CondBr->getTrueBB() == BB)
    return true;
  else
    return false;
}

static EnumElementDecl *
getOtherElementOfTwoElementEnum(EnumDecl *E, EnumElementDecl *Element) {
  assert(Element && "This should never be null");
  if (!Element)
    return nullptr;
  EnumElementDecl *OtherElt = nullptr;

  for (EnumElementDecl *Elt : E->getAllElements()) {
    // Skip the case where we find the select_enum element
    if (Elt == Element)
      continue;
    // If we find another element, then we must have more than 2, so bail.
    if (OtherElt)
      return nullptr;
    OtherElt = Elt;
  }

  return OtherElt;
}

/// If PredTerm is a (cond_br (select_enum)) or a (switch_enum), return the decl
/// that will yield DomBB.
static EnumElementDecl *
getEnumEltTaken(TermInst *PredTerm, SILBasicBlock *DomBB) {
  // First check if we have a (cond_br (select_enum)).
  if (auto *CBI = dyn_cast<CondBranchInst>(PredTerm)) {
    auto *SEI = dyn_cast<SelectEnumInst>(CBI->getCondition());
    if (!SEI)
      return nullptr;

    // Try to find a single literal "true" case.
    // TODO: More general conditions in which we can relate the BB to a single
    // case, such as when there's a single literal "false" case.
    EnumElementDecl *TrueElement = SEI->getSingleTrueElement();
    if (!TrueElement)
      return nullptr;

    // If DomBB is the taken branch, we know that the EnumElementDecl is the one
    // checked for by enum is tag. Return it.
    if (getBranchTaken(CBI, DomBB)) {
      return TrueElement;
    }

    // Ok, DomBB is not the taken branch. If we have an enum with only two
    // cases, we can still infer the other case for the current branch.
    EnumDecl *E = SEI->getEnumOperand().getType().getEnumOrBoundGenericEnum();

    // This will return nullptr if we have more than two cases in our decl.
    return getOtherElementOfTwoElementEnum(E, TrueElement);
  }

  auto *SWEI = dyn_cast<SwitchEnumInst>(PredTerm);
  if (!SWEI)
    return nullptr;

  return SWEI->getUniqueCaseForDestination(DomBB);
}

static void simplifyCondBranchInst(CondBranchInst *BI, bool BranchTaken) {
  auto LiveArgs =  BranchTaken ?  BI->getTrueArgs(): BI->getFalseArgs();
  auto *LiveBlock =  BranchTaken ? BI->getTrueBB() : BI->getFalseBB();

  SILBuilderWithScope<1>(BI).createBranch(BI->getLoc(), LiveBlock, LiveArgs);
  BI->dropAllReferences();
  BI->eraseFromParent();
}

/// Returns true if C1, C2 represent equivalent conditions in the
/// sense that each is eventually based on the same value.
static bool areEquivalentConditions(SILValue C1, SILValue C2) {
  if (auto *SEI = dyn_cast<SelectEnumInst>(C1))
    C1 = SEI->getEnumOperand().stripCasts();

  if (auto *SEI = dyn_cast<SelectEnumInst>(C2))
    C2 = SEI->getEnumOperand().stripCasts();

  return C1 == C2;
}

static bool trySimplifyConditional(TermInst *Term, DominanceInfo *DT) {
  assert(isConditional(Term) && "Expected conditional terminator!");

  auto *BB = Term->getParent();
  auto Condition = Term->getOperand(0);
  auto Kind = Term->getKind();

  for (auto *Node = DT->getNode(BB); Node; Node = Node->getIDom()) {
    auto *DomBB = Node->getBlock();
    auto *Pred = DomBB->getSinglePredecessor();
    if (!Pred)
      continue;

    // We assume that our predecessor terminator has operands like Term since
    // otherwise it can not be "conditional".
    auto *PredTerm = Pred->getTerminator();
    if (!PredTerm->getNumOperands() ||
        !areEquivalentConditions(PredTerm->getOperand(0), Condition))
      continue;

    // Okay, DomBB dominates Term, has a single predecessor, and that
    // predecessor conditionally branches on the same condition. So we
    // know that DomBB are control-dependent on the edge that takes us
    // from Pred to DomBB. Since the terminator kind and condition are
    // the same, we can use the knowledge of which edge gets us to
    // Inst to optimize Inst.
    switch (Kind) {
    case ValueKind::SwitchEnumInst: {
      if (EnumElementDecl *EltDecl = getEnumEltTaken(PredTerm, DomBB)) {
        simplifySwitchEnumInst(cast<SwitchEnumInst>(Term), EltDecl, DomBB);
        return true;
      }

      // FIXME: We could also simplify things in some cases when we
      //        reach this switch_enum_inst from another
      //        switch_enum_inst that is branching on the same value
      //        and taking the default path.
      continue;
    }
    case ValueKind::CondBranchInst: {
      auto *CBI = cast<CondBranchInst>(Term);

      // If this CBI has an 
      if (auto *SEI = dyn_cast<SelectEnumInst>(CBI->getCondition())) {
        if (auto TrueElement = SEI->getSingleTrueElement()) {
          if (EnumElementDecl *Element = getEnumEltTaken(PredTerm, DomBB)) {
            simplifyCondBranchInst(CBI, Element == TrueElement);
            return true;
          }
        }
      }

      // Ok, we failed to determine an enum element decl.
      auto *CondBrInst = dyn_cast<CondBranchInst>(PredTerm);
      if (!CondBrInst)
        continue;
      bool BranchTaken = getBranchTaken(CondBrInst, DomBB);
      simplifyCondBranchInst(CBI, BranchTaken);
      return true;
    }
    case ValueKind::SwitchValueInst:
    case ValueKind::SwitchEnumAddrInst:
      // FIXME: Handle these.
      return false;
    case ValueKind::CheckedCastBranchInst: {
      // We need to verify that the result type is the same in the
      // dominating checked_cast_br.
      auto *PredCCBI = dyn_cast<CheckedCastBranchInst>(PredTerm);
      if (!PredCCBI)
        continue;
      auto *CCBI = cast<CheckedCastBranchInst>(Term);
      if (PredCCBI->getCastType() != CCBI->getCastType())
        continue;

      assert((DomBB == PredCCBI->getSuccessBB() ||
              DomBB == PredCCBI->getFailureBB()) &&
          "Dominating block is not a successor of predecessor checked_cast_br");

      simplifyCheckedCastBranchInst(CCBI, DomBB == PredCCBI->getSuccessBB(),
                                    DomBB);
      return true;
    }
    default:
      llvm_unreachable("Should only see conditional terminators here!");
    }
  }
  return false;
}

// Simplifications that walk the dominator tree to prove redundancy in
// conditional branching.
bool SimplifyCFG::dominatorBasedSimplify(DominanceInfo *DT) {
  bool Changed = false;
  for (auto &BB : Fn)
    if (isConditional(BB.getTerminator()))
      Changed |= trySimplifyConditional(BB.getTerminator(), DT);

  return Changed;
}

// If BB is trivially unreachable, remove it from the worklist, add its
// successors to the worklist, and then remove the block.
bool SimplifyCFG::removeIfDead(SILBasicBlock *BB) {
  if (!BB->pred_empty() || BB == &*Fn.begin())
    return false;

  removeFromWorklist(BB);

  // Add successor blocks to the worklist since their predecessor list is about
  // to change.
  for (auto &S : BB->getSuccs())
    addToWorklist(S);

  removeDeadBlock(BB);
  ++NumBlocksDeleted;
  return true;
}

/// This is called when a predecessor of a block is dropped, to simplify the
/// block and add it to the worklist.
bool SimplifyCFG::simplifyAfterDroppingPredecessor(SILBasicBlock *BB) {
  // TODO: If BB has only one predecessor and has bb args, fold them away, then
  // use instsimplify on all the users of those values - even ones outside that
  // block.


  // Make sure that DestBB is in the worklist, as well as its remaining
  // predecessors, since they may not be able to be simplified.
  addToWorklist(BB);
  for (auto *P : BB->getPreds())
    addToWorklist(P);

  return false;
}



/// Return true if there are any users of V outside the specified block.
static bool isUsedOutsideOfBlock(SILValue V, SILBasicBlock *BB) {
  for (auto UI : V.getUses())
    if (UI->getUser()->getParent() != BB)
      return true;
  return false;
}

/// couldSimplifyUsers - Check to see if any simplifications are possible if
/// "Val" is substituted for BBArg.  If so, return true, if nothing obvious
/// is possible, return false.
static bool couldSimplifyUsers(SILArgument *BBArg, SILValue Val) {
  assert(!isa<IntegerLiteralInst>(Val) && !isa<FloatLiteralInst>(Val) &&
         "Obvious constants shouldn't reach here");

  // If the value being substituted is an enum, check to see if there are any
  // switches on it.
  auto *EI = dyn_cast<EnumInst>(Val);
  if (!EI)
    return false;

  for (auto UI : BBArg->getUses()) {
    auto *User = UI->getUser();
    if (isa<SwitchEnumInst>(User) || isa<SelectEnumInst>(User))
      return true;
  }
  return false;
}


namespace {
  class ThreadingCloner : public SILClonerWithScopes<ThreadingCloner> {
    friend class SILVisitor<ThreadingCloner>;
    friend class SILCloner<ThreadingCloner>;

    SILBasicBlock *FromBB, *DestBB;
  public:
    // A map of old to new available values.
    SmallVector<std::pair<ValueBase *, SILValue>, 16> AvailVals;

    ThreadingCloner(BranchInst *BI)
      : SILClonerWithScopes(*BI->getFunction()),
        FromBB(BI->getDestBB()), DestBB(BI->getParent()) {
      // Populate the value map so that uses of the BBArgs in the DestBB are
      // replaced with the branch's values.
      for (unsigned i = 0, e = BI->getArgs().size(); i != e; ++i) {
        ValueMap[FromBB->getBBArg(i)] = BI->getArg(i);
        AvailVals.push_back(std::make_pair(FromBB->getBBArg(i), BI->getArg(i)));
      }
    }

    void process(SILInstruction *I) { visit(I); }

    SILBasicBlock *remapBasicBlock(SILBasicBlock *BB) { return BB; }

    SILValue remapValue(SILValue Value) {
      // If this is a use of an instruction in another block, then just use it.
      if (auto SI = dyn_cast<SILInstruction>(Value)) {
        if (SI->getParent() != FromBB) return Value;
      } else if (auto BBArg = dyn_cast<SILArgument>(Value)) {
        if (BBArg->getParent() != FromBB) return Value;
      } else {
        assert(isa<SILUndef>(Value) && "Unexpected Value kind");
        return Value;
      }

      return SILCloner<ThreadingCloner>::remapValue(Value);
    }


    void postProcess(SILInstruction *Orig, SILInstruction *Cloned) {
      DestBB->getInstList().push_back(Cloned);
      SILClonerWithScopes<ThreadingCloner>::postProcess(Orig, Cloned);
      AvailVals.push_back(std::make_pair(Orig, SILValue(Cloned, 0)));
    }
  };
} // end anonymous namespace

static bool containsAllocStack(SILBasicBlock *BB) {
  for (auto &Inst : *BB)
    if (isa<AllocStackInst>(&Inst) || isa<DeallocStackInst>(&Inst))
      return true;
  return false;
}

/// Check whether we can 'thread' through the switch_enum instruction by
/// duplicating the switch_enum block into SrcBB.
static bool isThreadableSwitchEnumInst(SwitchEnumInst *SEI,
                                       SILBasicBlock *SrcBB, EnumInst *&E0,
                                       EnumInst *&E1) {
  auto SEIBB = SEI->getParent();
  auto PIt = SEIBB->pred_begin();
  auto PEnd = SEIBB->pred_end();

  // Recognize a switch_enum preceeded by two direct branch blocks that carry
  // the switch_enum operand's value as EnumInsts.
  if(std::distance(PIt, PEnd) != 2)
    return false;

  auto Arg = dyn_cast<SILArgument>(SEI->getOperand());
  if (!Arg)
    return false;

  if (Arg->getParent() != SEIBB)
    return false;

  // We must not duplicate alloc_stack, dealloc_stack.
  if (containsAllocStack(SEIBB))
      return false;

  auto Idx = Arg->getIndex();
  auto IncomingBr0 = dyn_cast<BranchInst>(((*PIt))->getTerminator());
  ++PIt;
  auto IncomingBr1 = dyn_cast<BranchInst>((*PIt)->getTerminator());

  // Make sure that we don't have an incoming critical edge.
  if (!IncomingBr0 || !IncomingBr1)
    return false;

  // We cannonicalize to IncomingBr0 to be from the basic block we clone
  // into.
  if (IncomingBr1->getParent() == SrcBB)
    std::swap(IncomingBr0, IncomingBr1);

  assert(IncomingBr0->getArgs().size() == SEIBB->getNumBBArg());
  assert(IncomingBr1->getArgs().size() == SEIBB->getNumBBArg());

  // Make sure that both predecessors arguments are an EnumInst so that we can
  // forward the branch.
  E0 = dyn_cast<EnumInst>(IncomingBr0->getArg(Idx));
  E1 = dyn_cast<EnumInst>(IncomingBr1->getArg(Idx));
  if (!E0 || !E1)
    return false;

  if (E0->getParent() != IncomingBr0->getParent() ||
      E1->getParent() != IncomingBr1->getParent())
    return false;

  // We also need to check for the absence of payload uses. we are not handling
  // them.
  auto SwitchDestBB0 = SEI->getCaseDestination(E0->getElement());
  auto SwitchDestBB1 = SEI->getCaseDestination(E1->getElement());
  return SwitchDestBB0->getNumBBArg() == 0 && SwitchDestBB1->getNumBBArg() == 0;
}

/// tryJumpThreading - Check to see if it looks profitable to duplicate the
/// destination of an unconditional jump into the bottom of this block.
bool SimplifyCFG::tryJumpThreading(BranchInst *BI) {
  auto *DestBB = BI->getDestBB();
  auto *SrcBB = BI->getParent();
  // If the destination block ends with a return, we don't want to duplicate it.
  // We want to maintain the canonical form of a single return where possible.
  if (isa<ReturnInst>(DestBB->getTerminator()))
    return false;

  bool isThreadableCondBr = isa<CondBranchInst>(DestBB->getTerminator()) &&
                  !containsAllocStack(DestBB);

  // We can jump thread switch enum instructions. But we need to 'thread' it by
  // hand - i.e. we need to replace the switch enum by branches - if we don't do
  // so the ssaupdater will fail because we can't form 'phi's with anything
  // other than branches and conditional branches because only they support
  // arguments :(.
  EnumInst *EnumInst0 = nullptr;
  EnumInst *EnumInst1 = nullptr;
  SwitchEnumInst *SEI = dyn_cast<SwitchEnumInst>(DestBB->getTerminator());
  bool isThreadableEnumInst =
      SEI && isThreadableSwitchEnumInst(SEI, SrcBB, EnumInst0, EnumInst1);

  // This code is intentionally simple, and cannot thread if the BBArgs of the
  // destination are used outside the DestBB.
  bool HasDestBBDefsUsedOutsideBlock = false;
  for (auto Arg : DestBB->getBBArgs())
    if ((HasDestBBDefsUsedOutsideBlock |= isUsedOutsideOfBlock(Arg, DestBB)))
      if (!isThreadableCondBr && !isThreadableEnumInst)
        return false;

  // We don't have a great cost model at the SIL level, so we don't want to
  // blissly duplicate tons of code with a goal of improved performance (we'll
  // leave that to LLVM).  However, doing limited code duplication can lead to
  // major second order simplifications.  Here we only do it if there are
  // "constant" arguments to the branch or if we know how to fold something
  // given the duplication.
  bool WantToThread = false;
  for (auto V : BI->getArgs()) {
    if (isa<IntegerLiteralInst>(V) || isa<FloatLiteralInst>(V)) {
      WantToThread = true;
      break;
    }
  }

  if (!WantToThread) {
    for (unsigned i = 0, e = BI->getArgs().size(); i != e; ++i)
      if (couldSimplifyUsers(DestBB->getBBArg(i), BI->getArg(i))) {
        WantToThread = true;
        break;
      }
  }

  // If we don't have anything that we can simplify, don't do it.
  if (!WantToThread) return false;

  // If it looks potentially interesting, decide whether we *can* do the
  // operation and whether the block is small enough to be worth duplicating.
  unsigned Cost = 0;

  for (auto &Inst : DestBB->getInstList()) {
    // This is a really trivial cost model, which is only intended as a starting
    // point.
    if (instructionInlineCost(Inst) != InlineCost::Free)
      if (++Cost == 4) return false;

    // If there is an instruction in the block that has used outside the block,
    // duplicating it would require constructing SSA, which we're not prepared
    // to do.
    if ((HasDestBBDefsUsedOutsideBlock |=
         isUsedOutsideOfBlock(&Inst, DestBB))) {
      if (!isThreadableCondBr && !isThreadableEnumInst)
        return false;

      // We can't build SSA for method values that lower to objc methods.
      if (auto *MI = dyn_cast<MethodInst>(&Inst))
        if (MI->getMember().isForeign)
          return false;
    }
  }


  // Okay, it looks like we want to do this and we can.  Duplicate the
  // destination block into this one, rewriting uses of the BBArgs to use the
  // branch arguments as we go.
  ThreadingCloner Cloner(BI);

  for (auto &I : *DestBB)
    Cloner.process(&I);

  // Once all the instructions are copied, we can nuke BI itself.  We also add
  // this block back to the worklist now that the terminator (likely) can be
  // simplified.
  addToWorklist(BI->getParent());
  BI->eraseFromParent();

  // Thread the switch enum instruction.
  if (isThreadableEnumInst && HasDestBBDefsUsedOutsideBlock) {
    assert(EnumInst0 && EnumInst1 && "Need to have two enum instructions");
    // We know that the switch enum is fed by enum instructions along all
    // incoming edges.
    auto SwitchDestBB0 = SEI->getCaseDestination(EnumInst0->getElement());
    auto SwitchDestBB1 = SEI->getCaseDestination(EnumInst1->getElement());
    assert(EnumInst0->getParent() == SrcBB);

    auto ClonedSEI = SrcBB->getTerminator();
    auto &InstList0 = EnumInst0->getParent()->getInstList();
    InstList0.insert(InstList0.end(),
                     BranchInst::create(SEI->getLoc(), SwitchDestBB0,
                                        *SEI->getParent()->getParent()));

    auto &InstList1 = SEI->getParent()->getInstList();
    InstList1.insert(InstList1.end(),
                     BranchInst::create(SEI->getLoc(), SwitchDestBB1,
                                        *SEI->getParent()->getParent()));
    ClonedSEI->eraseFromParent();
    SEI->eraseFromParent();
  }

  if (HasDestBBDefsUsedOutsideBlock) {
    // We are updating SSA form. This means we need to be able to insert phi
    // nodes. To make sure we can do this split all critical edges from
    // instructions that don't support block arguments.
    splitAllCriticalEdges(*DestBB->getParent(), true, nullptr, nullptr);

    SILSSAUpdater SSAUp;
    for (auto AvailValPair : Cloner.AvailVals) {
      ValueBase *Inst = AvailValPair.first;
      if (Inst->use_empty())
        continue;

      for (unsigned i = 0, e = Inst->getNumTypes(); i != e; ++i) {
        // Get the result index for the cloned instruction. This is going to be
        // the result index stored in the available value for arguments (we look
        // through the phi node) and the same index as the original value
        // otherwise.
        unsigned ResIdx = i;
        if (isa<SILArgument>(Inst))
          ResIdx = AvailValPair.second.getResultNumber();

        SILValue Res(Inst, i);
        SILValue NewRes(AvailValPair.second.getDef(), ResIdx);

        SmallVector<UseWrapper, 16> UseList;
        // Collect the uses of the value.
        for (auto Use : Res.getUses())
          UseList.push_back(UseWrapper(Use));

        SSAUp.Initialize(Res.getType());
        SSAUp.AddAvailableValue(DestBB, Res);
        SSAUp.AddAvailableValue(SrcBB, NewRes);

        if (UseList.empty())
          continue;

        // Update all the uses.
        for (auto U : UseList) {
          Operand *Use = U;
          SILInstruction *User = Use->getUser();
          assert(User && "Missing user");

          // Ignore uses in the same basic block.
          if (User->getParent() == DestBB)
            continue;

          SSAUp.RewriteUse(*Use);
        }
      }
    }
  }

  // We may be able to simplify DestBB now that it has one fewer predecessor.
  simplifyAfterDroppingPredecessor(DestBB);
  ++NumJumpThreads;
  return true;
}


/// simplifyBranchOperands - Simplify operands of branches, since it can
/// result in exposing opportunities for CFG simplification.
bool SimplifyCFG::simplifyBranchOperands(OperandValueArrayRef Operands) {
  bool Simplified = false;
  for (auto O = Operands.begin(), E = Operands.end(); O != E; ++O)
    if (auto *I = dyn_cast<SILInstruction>(*O))
      if (SILValue Result = simplifyInstruction(I)) {
        SILValue(I, 0).replaceAllUsesWith(Result.getDef());
        if (isInstructionTriviallyDead(I)) {
          I->eraseFromParent();
          Simplified = true;
        }
      }
  return Simplified;
}

/// \return True if this basic blocks has a single instruction that is the
/// terminator that jumps to another basic block passing all of the arguments
/// in the original order.
static bool isTrampolineBlock(SILBasicBlock *SBB) {
  // Ignore blocks with more than one instruction.
  if (SBB->getTerminator() != SBB->begin())
    return false;

  BranchInst *BI = dyn_cast<BranchInst>(SBB->getTerminator());
  if (!BI)
    return false;

  // Disallow infinite loops.
  if (BI->getDestBB() == SBB)
    return false;

  auto BrArgs = BI->getArgs();
  if (BrArgs.size() != SBB->getNumBBArg())
    return false;

  // Check that the arguments are the same and in the right order.
  for (int i = 0, e = SBB->getNumBBArg(); i < e; ++i)
    if (BrArgs[i] != SBB->getBBArg(i))
      return false;

  return true;
}

/// simplifyBranchBlock - Simplify a basic block that ends with an unconditional
/// branch.
bool SimplifyCFG::simplifyBranchBlock(BranchInst *BI) {
  // First simplify instructions generating branch operands since that
  // can expose CFG simplifications.
  bool Simplified = simplifyBranchOperands(BI->getArgs());

  auto *BB = BI->getParent(), *DestBB = BI->getDestBB();

  // If this block branches to a block with a single predecessor, then
  // merge the DestBB into this BB.
  if (BB != DestBB && DestBB->getSinglePredecessor()) {
    // If there are any BB arguments in the destination, replace them with the
    // branch operands, since they must dominate the dest block.
    for (unsigned i = 0, e = BI->getArgs().size(); i != e; ++i)
      SILValue(DestBB->getBBArg(i)).replaceAllUsesWith(BI->getArg(i));

    // Zap BI and move all of the instructions from DestBB into this one.
    BI->eraseFromParent();
    BB->getInstList().splice(BB->end(), DestBB->getInstList(),
                             DestBB->begin(), DestBB->end());

    // Revisit this block now that we've changed it and remove the DestBB.
    addToWorklist(BB);

    // This can also expose opportunities in the successors of
    // the merged block.
    for (auto &Succ : BB->getSuccs())
      addToWorklist(Succ);

    removeFromWorklist(DestBB);
    DestBB->eraseFromParent();
    ++NumBlocksMerged;
    return true;
  }

  // If the destination block is a simple trampoline (jump to another block)
  // then jump directly.
  if (isTrampolineBlock(DestBB)) {
    BranchInst* Br = dyn_cast<BranchInst>(DestBB->getTerminator());
    SILBuilderWithScope<1>(BI).createBranch(BI->getLoc(), Br->getDestBB(),
                                            BI->getArgs());
    // Eliminating the trampoline can expose opportuntities to improve the
    // new block we branch to.
    addToWorklist(Br->getDestBB());
    BI->eraseFromParent();
    removeIfDead(DestBB);
    addToWorklist(BB);
    return true;
  }

  // If this unconditional branch has BBArgs, check to see if duplicating the
  // destination would allow it to be simplified.  This is a simple form of jump
  // threading.
  if (!BI->getArgs().empty() &&
      tryJumpThreading(BI))
    return true;

  return Simplified;
}

/// simplifyCondBrBlock - Simplify a basic block that ends with a conditional
/// branch.
bool SimplifyCFG::simplifyCondBrBlock(CondBranchInst *BI) {
  // First simplify instructions generating branch operands since that
  // can expose CFG simplifications.
  simplifyBranchOperands(OperandValueArrayRef(BI->getAllOperands()));
  auto *ThisBB = BI->getParent();

  // If the condition is an integer literal, we can constant fold the branch.
  if (auto *IL = dyn_cast<IntegerLiteralInst>(BI->getCondition())) {
    bool isFalse = !IL->getValue();
    auto LiveArgs =  isFalse ? BI->getFalseArgs() : BI->getTrueArgs();
    auto *LiveBlock =  isFalse ? BI->getFalseBB() : BI->getTrueBB();
    auto *DeadBlock = !isFalse ? BI->getFalseBB() : BI->getTrueBB();
    auto *ThisBB = BI->getParent();

    SILBuilderWithScope<1>(BI).createBranch(BI->getLoc(), LiveBlock, LiveArgs);
    BI->eraseFromParent();
    if (IL->use_empty()) IL->eraseFromParent();

    addToWorklist(ThisBB);
    simplifyAfterDroppingPredecessor(DeadBlock);
    addToWorklist(LiveBlock);
    ++NumConstantFolded;
    return true;
  }

  // If the destination block is a simple trampoline (jump to another block)
  // then jump directly.
  SILBasicBlock *TrueSide = BI->getTrueBB();
  SILBasicBlock *FalseSide = BI->getFalseBB();

  if (isTrampolineBlock(TrueSide)) {
    BranchInst* Br = cast<BranchInst>(TrueSide->getTerminator());
    SILBuilderWithScope<1>(BI)
      .createCondBranch(BI->getLoc(), BI->getCondition(),
                        Br->getDestBB(), BI->getTrueArgs(),
                        BI->getFalseBB(), BI->getFalseArgs());
    BI->eraseFromParent();
    removeIfDead(TrueSide);
    addToWorklist(ThisBB);
    return true;
  }

  if (isTrampolineBlock(FalseSide)) {
    BranchInst* Br = cast<BranchInst>(FalseSide->getTerminator());
    SILBuilderWithScope<1>(BI)
      .createCondBranch(BI->getLoc(), BI->getCondition(),
                        BI->getTrueBB(), BI->getTrueArgs(),
                        Br->getDestBB(), BI->getFalseArgs());
    BI->eraseFromParent();
    removeIfDead(FalseSide);
    addToWorklist(ThisBB);
    return true;
  }

  // Simplify cond_br where both sides jump to the same blocks with the same
  // args.
  TrueSide = BI->getTrueBB();
  FalseSide = BI->getFalseBB();
  if (TrueSide == FalseSide) {
    auto TrueArgs = BI->getTrueArgs();
    auto FalseArgs = BI->getFalseArgs();
    assert(TrueArgs.size() == FalseArgs.size() && "Invalid args!");
    bool SameArgs = true;
    for (int i = 0, e = TrueArgs.size(); i < e; i++)
      if (TrueArgs[i] != FalseArgs[i]){
        SameArgs = false;
        break;
      }

    if (SameArgs) {
      SILBuilderWithScope<1>(BI).createBranch(BI->getLoc(), TrueSide, TrueArgs);
      BI->eraseFromParent();
      addToWorklist(ThisBB);
      addToWorklist(TrueSide);
      ++NumConstantFolded;
      return true;
    }
  }

  // If we have a (cond (select_enum)) on a two element enum, always have the
  // first case as our checked tag. If we have the second, create a new
  // select_enum with the first case and swap our operands. This simplifies
  // later dominance based processing.
  if (auto *SEI = dyn_cast<SelectEnumInst>(BI->getCondition())) {
    EnumDecl *E = SEI->getEnumOperand().getType().getEnumOrBoundGenericEnum();

    auto AllElts = E->getAllElements();
    auto Iter = AllElts.begin();
    EnumElementDecl *FirstElt = *Iter;

    if (SEI->getNumCases() >= 1
        && SEI->getCase(0).first != FirstElt) {
      ++Iter;

      if (Iter != AllElts.end() &&
          std::next(Iter) == AllElts.end() &&
          *Iter == SEI->getCase(0).first) {
        EnumElementDecl *SecondElt = *Iter;
        
        SILValue FirstValue;
        // SelectEnum must be exhaustive, so the second case must be handled
        // either by a case or the default.
        if (SEI->getNumCases() >= 2) {
          assert(FirstElt == SEI->getCase(1).first
                 && "select_enum missing a case?!");
          FirstValue = SEI->getCase(1).second;
        } else {
          FirstValue = SEI->getDefaultResult();
        }
        
        
        std::pair<EnumElementDecl*, SILValue> SwappedCases[2] = {
          {FirstElt, SEI->getCase(0).second},
          {SecondElt, FirstValue},
        };
        
        auto *NewSEI = SILBuilderWithScope<1>(SEI)
          .createSelectEnum(SEI->getLoc(),
                            SEI->getEnumOperand(),
                            SEI->getType(),
                            SILValue(),
                            SwappedCases);
        
        // We only change the condition to be NewEITI instead of all uses since
        // EITI may have other uses besides this one that need to be updated.
        BI->setCondition(NewSEI);
        BI->swapSuccessors();
        addToWorklist(BI->getParent());
        addToWorklist(TrueSide);
        addToWorklist(FalseSide);
        return true;
      }
    }
  }

  return false;
}

// Does this basic block consist of only an "unreachable" instruction?
static bool isOnlyUnreachable(SILBasicBlock *BB) {
  auto *Term = BB->getTerminator();
  if (!isa<UnreachableInst>(Term))
    return false;

  return (&*BB->begin() == BB->getTerminator());
}


/// simplifySwitchEnumUnreachableBlocks - Attempt to replace a
/// switch_enum_inst where all but one block consists of just an
/// "unreachable" with an unchecked_enum_data and branch.
bool SimplifyCFG::simplifySwitchEnumUnreachableBlocks(SwitchEnumInst *SEI) {
  auto Count = SEI->getNumCases();

  SILBasicBlock *Dest = nullptr;
  EnumElementDecl *Element = nullptr;

  if (SEI->hasDefault())
    if (!isOnlyUnreachable(SEI->getDefaultBB()))
      Dest = SEI->getDefaultBB();

  for (unsigned i = 0; i < Count; ++i) {
    auto EnumCase = SEI->getCase(i);

    if (isOnlyUnreachable(EnumCase.second))
      continue;

    if (Dest)
      return false;

    assert(!Element && "Did not expect to have an element without a block!");
    Element = EnumCase.first;
    Dest = EnumCase.second;
  }

  if (!Dest) {
    addToWorklist(SEI->getParent());
    SILBuilderWithScope<1>(SEI).createUnreachable(SEI->getLoc());
    SEI->eraseFromParent();
    return true;
  }

  if (!Element || !Element->hasArgumentType() || Dest->bbarg_empty()) {
    assert(Dest->bbarg_empty() && "Unexpected argument at destination!");

    SILBuilderWithScope<1>(SEI).createBranch(SEI->getLoc(), Dest);

    addToWorklist(SEI->getParent());
    addToWorklist(Dest);

    SEI->eraseFromParent();
    return true;
  }

  auto &Mod = SEI->getModule();
  auto OpndTy = SEI->getOperand()->getType(0);
  auto Ty = OpndTy.getEnumElementType(Element, Mod);
  auto *UED = SILBuilderWithScope<1>(SEI)
    .createUncheckedEnumData(SEI->getLoc(), SEI->getOperand(), Element, Ty);

  assert(Dest->bbarg_size() == 1 && "Expected only one argument!");
  ArrayRef<SILValue> Args = { UED };
  SILBuilderWithScope<1>(SEI).createBranch(SEI->getLoc(), Dest, Args);

  addToWorklist(SEI->getParent());
  addToWorklist(Dest);

  SEI->eraseFromParent();
  return true;
}

/// simplifySwitchEnumBlock - Simplify a basic block that ends with a
/// switch_enum instruction that gets its operand from a an enum
/// instruction.
bool SimplifyCFG::simplifySwitchEnumBlock(SwitchEnumInst *SEI) {
  auto *EI = dyn_cast<EnumInst>(SEI->getOperand());

  // If the operand is not from an enum, see if this is a case where
  // only one destination of the branch has code that does not end
  // with unreachable.
  if (!EI)
    return simplifySwitchEnumUnreachableBlocks(SEI);

  auto *LiveBlock = SEI->getCaseDestination(EI->getElement());
  auto *ThisBB = SEI->getParent();

  bool DroppedLiveBlock = false;
  // Copy the successors into a vector, dropping one entry for the liveblock.
  SmallVector<SILBasicBlock*, 4> Dests;
  for (auto &S : SEI->getSuccessors()) {
    if (S == LiveBlock && !DroppedLiveBlock) {
      DroppedLiveBlock = true;
      continue;
    }
    Dests.push_back(S);
  }

  if (EI->hasOperand() && !LiveBlock->bbarg_empty())
    SILBuilderWithScope<1>(SEI).createBranch(SEI->getLoc(), LiveBlock,
                                             EI->getOperand());
  else
    SILBuilderWithScope<1>(SEI).createBranch(SEI->getLoc(), LiveBlock);
  SEI->eraseFromParent();
  if (EI->use_empty()) EI->eraseFromParent();

  addToWorklist(ThisBB);

  for (auto B : Dests)
    simplifyAfterDroppingPredecessor(B);
  addToWorklist(LiveBlock);
  ++NumConstantFolded;
  return true;
}

/// simplifyUnreachableBlock - Simplify blocks ending with unreachable by
/// removing instructions that are safe to delete backwards until we
/// hit an instruction we cannot delete.
bool SimplifyCFG::simplifyUnreachableBlock(UnreachableInst *UI) {
  bool Changed = false;
  auto BB = UI->getParent();
  auto I = std::next(BB->rbegin());
  auto End = BB->rend();
  SmallVector<SILInstruction *, 8> DeadInstrs;

  // Walk backwards deleting instructions that should be safe to delete
  // in a block that ends with unreachable.
  while (I != End) {
    auto MaybeDead = I++;

    switch (MaybeDead->getKind()) {
      // These technically have side effects, but not ones that matter
      // in a block that we shouldn't really reach...
    case ValueKind::StrongRetainInst:
    case ValueKind::StrongReleaseInst:
    case ValueKind::RetainValueInst:
    case ValueKind::ReleaseValueInst:
      break;

    default:
      if (MaybeDead->mayHaveSideEffects()) {
        if (Changed)
          for (auto Dead : DeadInstrs)
            Dead->eraseFromParent();
        return Changed;
      }
    }

    for (unsigned i = 0, e = MaybeDead->getNumTypes(); i != e; ++i)
      if (!SILValue(&*MaybeDead, i).use_empty()) {
        auto Undef = SILUndef::get(MaybeDead->getType(i), BB->getModule());
        SILValue(&*MaybeDead, i).replaceAllUsesWith(Undef);
      }

    DeadInstrs.push_back(&*MaybeDead);
    Changed = true;
  }

  // If this block was changed and it now consists of only the unreachable,
  // make sure we process its predecessors.
  if (Changed) {
    for (auto Dead : DeadInstrs)
      Dead->eraseFromParent();

    if (isOnlyUnreachable(BB))
      for (auto *P : BB->getPreds())
        addToWorklist(P);
  }

  return Changed;
}

void RemoveUnreachable::visit(SILBasicBlock *BB) {
  if (!Visited.insert(BB))
    return;

  for (auto &Succ : BB->getSuccs())
    visit(Succ);
}

bool RemoveUnreachable::run() {
  bool Changed = false;

  // Clear each time we run so that we can run multiple times.
  Visited.clear();

  // Visit all blocks reachable from the entry block of the function.
  visit(Fn.begin());

  // Remove the blocks we never reached.
  for (auto It = Fn.begin(), End = Fn.end(); It != End; ) {
    auto *BB = &*It++;
    if (!Visited.count(BB)) {
      removeDeadBlock(BB);
      Changed = true;
    }
  }

  return Changed;
}

bool SimplifyCFG::simplifyBlocks() {
  bool Changed = false;

  // Add all of the blocks to the function.
  for (auto &BB : Fn)
    addToWorklist(&BB);

  // Iteratively simplify while there is still work to do.
  while (SILBasicBlock *BB = popWorklist()) {
    // If the block is dead, remove it.
    if (removeIfDead(BB)) {
      Changed = true;
      continue;
    }

    // Otherwise, try to simplify the terminator.
    TermInst *TI = BB->getTerminator();

    switch (TI->getKind()) {
    case ValueKind::BranchInst:
      Changed |= simplifyBranchBlock(cast<BranchInst>(TI));
      break;
    case ValueKind::CondBranchInst:
      Changed |= simplifyCondBrBlock(cast<CondBranchInst>(TI));
      break;
    case ValueKind::SwitchValueInst:
      // FIXME: Optimize for known switch values.
      break;
    case ValueKind::SwitchEnumInst:
      Changed |= simplifySwitchEnumBlock(cast<SwitchEnumInst>(TI));
      break;
    case ValueKind::UnreachableInst:
      Changed |= simplifyUnreachableBlock(cast<UnreachableInst>(TI));
      break;
    default:
      break;
    }

    // Simplify the block argument list.
    Changed |= simplifyArgs(BB);
  }

  return Changed;
}

/// Canonicalize all switch_enum and switch_enum_addr instructions.
/// If possible, replace the default with the corresponding unique case.
void SimplifyCFG::canonicalizeSwitchEnums() {
  for (auto &BB : Fn) {
    TermInst *TI = BB.getTerminator();
  
    SwitchEnumInstBase *SWI = dyn_cast<SwitchEnumInstBase>(TI);
    if (!SWI)
      continue;
    
    if (!SWI->hasDefault())
      continue;
    
    EnumElementDecl *elementDecl = SWI->getUniqueCaseForDefault();
    if (!elementDecl)
      continue;
    
    // Construct a new instruction by copying all the case entries.
    SmallVector<std::pair<EnumElementDecl*, SILBasicBlock*>, 4> CaseBBs;
    for (int idx = 0, numIdcs = SWI->getNumCases(); idx < numIdcs; idx++) {
      CaseBBs.push_back(SWI->getCase(idx));
    }
    // Add the default-entry of the original instruction as case-entry.
    CaseBBs.push_back(std::make_pair(elementDecl, SWI->getDefaultBB()));
    
    if (SWI->getKind() == ValueKind::SwitchEnumInst) {
      SILBuilderWithScope<1>(SWI).createSwitchEnum(SWI->getLoc(),
                                  SWI->getOperand(), nullptr, CaseBBs);
    } else {
      assert(SWI->getKind() == ValueKind::SwitchEnumAddrInst &&
             "unknown switch_enum instruction");
      SILBuilderWithScope<1>(SWI).createSwitchEnumAddr(SWI->getLoc(),
                                  SWI->getOperand(), nullptr, CaseBBs);
    }
    SWI->eraseFromParent();
  }
}

bool SimplifyCFG::run() {
  RemoveUnreachable RU(Fn);

  // First remove any block not reachable from the entry.
  bool Changed = RU.run();

  if (simplifyBlocks()) {
    // Simplifying other blocks might have resulted in unreachable
    // loops.
    RU.run();

    // Force dominator recomputation below.
    PM->invalidateAnalysis(&Fn, SILAnalysis::InvalidationKind::CFG);
    Changed = true;
  }

  // Do simplifications that require the dominator tree to be accurate.
  DominanceAnalysis* DA = PM->getAnalysis<DominanceAnalysis>();
  DominanceInfo *DT = DA->getDomInfo(&Fn);
  Changed |= dominatorBasedSimplify(DT);

  // Now attempt to simplify the remaining blocks.
  if (simplifyBlocks()) {
    // Simplifying other blocks might have resulted in unreachable
    // loops.
    RU.run();
    Changed = true;
  }

  // Split all critical edges from non cond_br terminators.
  Changed |= splitAllCriticalEdges(Fn, true, nullptr, nullptr);

  canonicalizeSwitchEnums();
  
  return Changed;
}

static void
removeArgumentFromTerminator(SILBasicBlock *BB, SILBasicBlock *Dest, int idx) {
  TermInst *Branch = BB->getTerminator();
  SILBuilderWithScope<2> Builder(Branch);

  if (CondBranchInst *CBI = dyn_cast<CondBranchInst>(Branch)) {
    DEBUG(llvm::dbgs() << "*** Fixing CondBranchInst.\n");

    SmallVector<SILValue, 8> TrueArgs;
    SmallVector<SILValue, 8> FalseArgs;

    for (auto A : CBI->getTrueArgs())
      TrueArgs.push_back(A);

    for (auto A : CBI->getFalseArgs())
      FalseArgs.push_back(A);

    if (Dest == CBI->getTrueBB())
      TrueArgs.erase(TrueArgs.begin() + idx);

    if (Dest == CBI->getFalseBB())
      FalseArgs.erase(FalseArgs.begin() + idx);

    Builder.createCondBranch(CBI->getLoc(), CBI->getCondition(),
                             CBI->getTrueBB(), TrueArgs,
                             CBI->getFalseBB(), FalseArgs);
    Branch->eraseFromParent();
    return;
  }

  if (BranchInst *BI = dyn_cast<BranchInst>(Branch)) {
    DEBUG(llvm::dbgs() << "*** Fixing BranchInst.\n");
    SmallVector<SILValue, 8> Args;

    for (auto A : BI->getArgs())
      Args.push_back(A);

    Args.erase(Args.begin() + idx);
    Builder.createBranch(BI->getLoc(), BI->getDestBB(), Args);
    Branch->eraseFromParent();
    return;
  }
  llvm_unreachable("unsupported terminator");
}

/// Is an argument from this terminator considered mandatory?
static bool hasMandatoryArgument(TermInst *term) {
  // It's more maintainable to just white-list the instructions that
  // *do* have mandatory arguments.
  return (!isa<BranchInst>(term) && !isa<CondBranchInst>(term));
}


// Get the element of Aggregate corresponding to the one extracted by
// Extract.
static SILValue getInsertedValue(SILInstruction *Aggregate,
                                 SILInstruction *Extract) {
  if (auto *Struct = dyn_cast<StructInst>(Aggregate)) {
    auto *SEI = cast<StructExtractInst>(Extract);
    return Struct->getFieldValue(SEI->getField());
  }
  auto *Tuple = cast<TupleInst>(Aggregate);
  auto *TEI = cast<TupleExtractInst>(Extract);
  return Tuple->getElementValue(TEI->getFieldNo());
}

/// Given a boolean argument, see if its it ultimately matching whether
/// a given enum is of a given tag.  If so, create a new select_enum instruction
/// to do the match.
/// TODO: Generalize this to simplify arbitrary simple switch_enum diamonds into
/// select_enums.
bool simplifySwitchEnumToSelectEnum(SILBasicBlock *BB,
                                    unsigned ArgNum,
                                    SILArgument* BoolArg) {
  auto IntTy = BoolArg->getType().getAs<BuiltinIntegerType>();
  if (!IntTy->isFixedWidth(1))
    return false;

  // Keep track of which predecessors map to true and which to false
  // If we have only a single predecessor as either true or false then we
  // can create a select_enum
  SmallVector<SILBasicBlock*, 4> TrueBBs;
  SmallVector<SILBasicBlock*, 4> FalseBBs;

  SwitchEnumInst *SWI = nullptr;

  for (auto P : BB->getPreds()) {
    // Only handle branch or conditional branch instructions.
    TermInst *TI = P->getTerminator();
    if (!isa<BranchInst>(TI) && !isa<CondBranchInst>(TI))
      return false;

    // Find the Nth argument passed to BB.
    SILValue Arg = TI->getOperand(ArgNum);
    SILInstruction *SI = dyn_cast<SILInstruction>(Arg);
    if (!SI)
      return false;
    IntegerLiteralInst *IntLit = dyn_cast<IntegerLiteralInst>(SI);
    if (!IntLit)
      return false;
    if (IntLit->getValue() == 0)
      FalseBBs.push_back(P);
    else
      TrueBBs.push_back(P);

    // Look for a single predecessor which terminates with a switch_enum
    SILBasicBlock *SinglePred = P->getSinglePredecessor();
    if (!SinglePred)
      return false;
    auto *PredSwi = dyn_cast<SwitchEnumInst>(SinglePred->getTerminator());
    if (!PredSwi)
      return false;
    if (SWI) {
      if (SWI != PredSwi)
        return false;
    } else {
      SWI = PredSwi;
      // TODO: Handle default
      if (SWI->hasDefault())
        return false;
      // switch_enum is required to be fully covered, If there is no default,
      // then we must have one enum case for each of our predecessors.
    }
  }
  // See if we are covering all enumerations.
  if (SWI->getNumCases() != (TrueBBs.size() + FalseBBs.size()))
    return false;

  SILBasicBlock *TrueBB;
  if (TrueBBs.size() == 1) {
    TrueBB = TrueBBs[0];
  } else {
    return false;
  }

  // Only a single BB has a true value.  We can create select_enum for this
  // single case.
  
  SILBuilderWithScope<4> B(SWI);
  
  auto TrueDef = B.createIntegerLiteral(SWI->getLoc(),
                                        BoolArg->getType(),
                                        1);
  auto FalseDef = B.createIntegerLiteral(SWI->getLoc(),
                                         BoolArg->getType(),
                                         0);
  
  EnumElementDecl* Elt = nullptr;
  for (unsigned i = 0, e = SWI->getNumCases(); i != e; ++i) {
    std::pair<EnumElementDecl*, SILBasicBlock*> Pair = SWI->getCase(i);
    if (Pair.second == TrueBB) {
      if (Elt) {
        // A case already jumped to this BB.  We need to bail out as multiple
        // cases are true.
        return false;
      }
      Elt = Pair.first;
    }
  }
  auto SelectCase = std::make_pair(Elt, SILValue(TrueDef));
  auto SelectInst = B.createSelectEnum(SWI->getLoc(),
                                       SWI->getOperand(),
                                       BoolArg->getType(),
                                       FalseDef,
                                       SelectCase);
  BoolArg->replaceAllUsesWith(SelectInst);
  return true;
}

// Attempt to simplify the ith argument of BB.  We simplify cases
// where there is a single use of the argument that is an extract from
// a struct or tuple and where the predecessors all build the struct
// or tuple and pass it directly.
bool SimplifyCFG::simplifyArgument(SILBasicBlock *BB, unsigned i) {
  auto *A = BB->getBBArg(i);

  // If we are reading an i1, then check to see if it comes from
  // a switch_enum.  If so, we may be able to lower this sequence to
  // a select_enum.
  if (A->getType().is<BuiltinIntegerType>())
    return simplifySwitchEnumToSelectEnum(BB, i, A);

  // For now, just focus on cases where there is a single use.
  if (!A->hasOneUse())
    return false;

  auto *Use = *A->use_begin();
  auto *User = cast<SILInstruction>(Use->getUser());
  if (!dyn_cast<StructExtractInst>(User) &&
      !dyn_cast<TupleExtractInst>(User))
    return false;

  // For now, just handle the case where all predecessors are
  // unconditional branches.
  for (auto *Pred : BB->getPreds()) {
    if (!isa<BranchInst>(Pred->getTerminator()))
      return false;
    auto *Branch = cast<BranchInst>(Pred->getTerminator());
    if (!isa<StructInst>(Branch->getArg(i)) &&
        !isa<TupleInst>(Branch->getArg(i)))
      return false;
  }

  // Okay, we'll replace the BB arg with one with the right type, replace
  // the uses in this block, and then rewrite the branch operands.
  A->replaceAllUsesWith(SILUndef::get(A->getType(), BB->getModule()));
  auto *NewArg = BB->replaceBBArg(i, User->getType(0));
  User->replaceAllUsesWith(NewArg);
  User->eraseFromParent();

  // Rewrite the branch operand for each incoming branch.
  for (auto *Pred : BB->getPreds()) {
    if (auto *Branch = cast<BranchInst>(Pred->getTerminator())) {
      auto V = getInsertedValue(cast<SILInstruction>(Branch->getArg(i)),
                                User);
      Branch->setOperand(i, V);
      addToWorklist(Pred);
    }
  }

  return true;
}

bool SimplifyCFG::simplifyArgs(SILBasicBlock *BB) {
  // Ignore blocks with no arguments.
  if (BB->bbarg_empty())
    return false;

  // Ignore the entry block.
  if (BB->pred_empty())
    return false;

  // Ignore blocks that are successors of terminators with mandatory args.
  for (SILBasicBlock *pred : BB->getPreds()) {
    if (hasMandatoryArgument(pred->getTerminator()))
      return false;
  }

  bool Changed = false;
  for (int i = BB->getNumBBArg() - 1; i >= 0; --i) {
    SILArgument *A = BB->getBBArg(i);

    // Try to simplify the argument
    if (!A->use_empty()) {
      if (simplifyArgument(BB, i))
        Changed = true;
      continue;
    }

    DEBUG(llvm::dbgs() << "*** Erasing " << i <<"th BB argument.\n");
    NumDeadArguments++;
    Changed = true;
    BB->eraseArgument(i);

    // Determine the set of predecessors in case any predecessor has
    // two edges to this block (e.g. a conditional branch where both
    // sides reach this block).
    llvm::SmallPtrSet<SILBasicBlock *, 4> PredBBs;
    for (auto *Pred : BB->getPreds())
      PredBBs.insert(Pred);

    for (auto *Pred : PredBBs)
      removeArgumentFromTerminator(Pred, BB, i);
  }

  return Changed;
}

namespace {
class SimplifyCFGPass : public SILFunctionTransform {

  /// The entry point to the transformation.
  void run() {
    if (SimplifyCFG(*getFunction(), PM).run())
      invalidateAnalysis(SILAnalysis::InvalidationKind::CFG);
  }

  StringRef getName() override { return "Simplify CFG"; }
};
} // end anonymous namespace


SILTransform *swift::createSimplifyCFG() {
  return new SimplifyCFGPass();
}

namespace {
class SplitCriticalEdges : public SILFunctionTransform {
  bool OnlyNonCondBrEdges;

public:
  SplitCriticalEdges(bool SplitOnlyNonCondBrEdges)
      : OnlyNonCondBrEdges(SplitOnlyNonCondBrEdges) {}

  void run() {
    auto &Fn = *getFunction();

    // Split all critical egdes from all or non only cond_br terminators.
    bool Changed =
        splitAllCriticalEdges(Fn, OnlyNonCondBrEdges, nullptr, nullptr);

    if (Changed)
      invalidateAnalysis(SILAnalysis::InvalidationKind::CFG);
  }

  StringRef getName() override { return "Split Critical Edges"; }
};
} // End anonymous namespace.

/// Splits all critical edges in a function.
SILTransform *swift::createSplitAllCriticalEdges() {
  return new SplitCriticalEdges(false);
}

/// Splits all critical edges from non cond_br terminators in a function.
SILTransform *swift::createSplitNonCondBrCriticalEdges() {
  return new SplitCriticalEdges(true);
}
