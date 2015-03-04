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
    // Keep track of loop headers - we don't want to jump-thread through them.
    SmallPtrSet<SILBasicBlock *, 32> LoopHeaders;

    // Dominance and post-dominance info for the current function
    DominanceInfo *DT;
    PostDominanceInfo *PDT;

  public:
    SimplifyCFG(SILFunction &Fn, SILPassManager *PM) :
      Fn(Fn), PM(PM) {}

    bool run();
    
    bool simplifyBlockArgs() {
      DominanceAnalysis *DA = PM->getAnalysis<DominanceAnalysis>();
      DT = DA->getDomInfo(&Fn);
      PDT = DA->getPostDomInfo(&Fn);
      bool Changed = false;
      for (SILBasicBlock &BB : Fn) {
        Changed |= simplifyArgs(&BB);
      }
      DT = nullptr;
      PDT = nullptr;
      return Changed;
    }

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

      if (LoopHeaders.count(BB))
        LoopHeaders.erase(BB);
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
    bool simplifyCheckedCastBranchBlock(CheckedCastBranchInst *CCBI);
    bool simplifySwitchEnumUnreachableBlocks(SwitchEnumInst *SEI);
    bool simplifySwitchEnumBlock(SwitchEnumInst *SEI);
    bool simplifyUnreachableBlock(UnreachableInst *UI);
    bool simplifyArgument(SILBasicBlock *BB, unsigned i);
    bool simplifyArgs(SILBasicBlock *BB);
    bool trySimplifyCheckedCastBr(TermInst *Term, DominanceInfo *DT);
    void findLoopHeaders();
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

/// Return true if there are any users of V outside the specified block.
static bool isUsedOutsideOfBlock(SILValue V, SILBasicBlock *BB) {
  for (auto UI : V.getUses())
    if (UI->getUser()->getParent() != BB)
      return true;
  return false;
}

/// We can not duplicate blocks with AllocStack instructions (they need to be
/// FIFO). Other instructions can be duplicated.
static bool canDuplicateBlock(SILBasicBlock *BB) {
  for (auto &I : *BB) {
    if (!I.isTriviallyDuplicatable())
      return false;
  }
  return true;
}

namespace {
  /// Base class for BB cloners.
  class BaseThreadingCloner : public SILClonerWithScopes<BaseThreadingCloner> {
    friend class SILVisitor<BaseThreadingCloner>;
    friend class SILCloner<BaseThreadingCloner>;

  protected:
    SILBasicBlock *FromBB, *DestBB;

  public:
    // A map of old to new available values.
    SmallVector<std::pair<ValueBase *, SILValue>, 16> AvailVals;

    BaseThreadingCloner(SILFunction &F)
        : SILClonerWithScopes(F), FromBB(nullptr), DestBB(nullptr) {}

    BaseThreadingCloner(SILFunction &F, SILBasicBlock *From, SILBasicBlock *Dest)
        : SILClonerWithScopes(F), FromBB(From), DestBB(Dest) {}

    void process(SILInstruction *I) { visit(I); }

    SILBasicBlock *remapBasicBlock(SILBasicBlock *BB) { return BB; }

    SILValue remapValue(SILValue Value) {
      // If this is a use of an instruction in another block, then just use it.
      if (auto SI = dyn_cast<SILInstruction>(Value)) {
        if (SI->getParent() != FromBB)
          return Value;
      } else if (auto BBArg = dyn_cast<SILArgument>(Value)) {
        if (BBArg->getParent() != FromBB)
          return Value;
      } else {
        assert(isa<SILUndef>(Value) && "Unexpected Value kind");
        return Value;
      }

      return SILCloner<BaseThreadingCloner>::remapValue(Value);
    }

    void postProcess(SILInstruction *Orig, SILInstruction *Cloned) {
      DestBB->getInstList().push_back(Cloned);
      SILClonerWithScopes<BaseThreadingCloner>::postProcess(Orig, Cloned);
      AvailVals.push_back(std::make_pair(Orig, SILValue(Cloned, 0)));
    }
  };

  // Cloner used by jump-threading.
  class ThreadingCloner : public BaseThreadingCloner {
  public:
    ThreadingCloner(BranchInst *BI)
        : BaseThreadingCloner(*BI->getFunction(), BI->getDestBB(),
                              BI->getParent()) {
      // Populate the value map so that uses of the BBArgs in the DestBB are
      // replaced with the branch's values.
      for (unsigned i = 0, e = BI->getArgs().size(); i != e; ++i) {
        ValueMap[FromBB->getBBArg(i)] = BI->getArg(i);
        AvailVals.push_back(std::make_pair(FromBB->getBBArg(i), BI->getArg(i)));
      }
    }
  };

  /// Helper class for cloning of basic blocks.
  class BasicBlockCloner : public BaseThreadingCloner {
  public:
    BasicBlockCloner(SILBasicBlock *From, SILBasicBlock *To = nullptr)
        : BaseThreadingCloner(*From->getParent()) {
      FromBB = From;
      if (To == nullptr) {
        // Create a new BB that is to be used as a target
        // for cloning.
        To = From->getParent()->createBasicBlock();
        for (auto *Arg : FromBB->getBBArgs()) {
          To->createBBArg(Arg->getType(), Arg->getDecl());
        }
      }
      DestBB = To;

      // Populate the value map so that uses of the BBArgs in the SrcBB are
      // replaced with the BBArgs of the DestBB.
      for (unsigned i = 0, e = FromBB->bbarg_size(); i != e; ++i) {
        ValueMap[FromBB->getBBArg(i)] = DestBB->getBBArg(i);
        AvailVals.push_back(
            std::make_pair(FromBB->getBBArg(i), DestBB->getBBArg(i)));
      }
    }

    // Clone all instructions of the FromBB into DestBB
    void clone() {
      for (auto &I : *FromBB) {
        process(&I);
      }
    }

    SILBasicBlock *getDestBB() { return DestBB; }
  };
} // end anonymous namespace

/// Helper function to perform SSA updates in case of jump threading.
static void updateSSAAfterCloning(BaseThreadingCloner &Cloner,
                                  SILBasicBlock *SrcBB,
                                  SILBasicBlock *DestBB) {
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
static NullablePtr<EnumElementDecl> getEnumEltTaken(TermInst *PredTerm,
                                                    SILBasicBlock *DomBB) {
  // First check if we have a (cond_br (select_enum)).
  if (auto *CBI = dyn_cast<CondBranchInst>(PredTerm)) {
    auto *SEI = dyn_cast<SelectEnumInst>(CBI->getCondition());
    if (!SEI)
      return nullptr;

    // Try to find a single literal "true" case.
    // TODO: More general conditions in which we can relate the BB to a single
    // case, such as when there's a single literal "false" case.
    NullablePtr<EnumElementDecl> TrueElement = SEI->getSingleTrueElement();
    if (TrueElement.isNull())
      return nullptr;

    // If DomBB is the taken branch, we know that the EnumElementDecl is the one
    // checked for by enum is tag. Return it.
    if (getBranchTaken(CBI, DomBB)) {
      return TrueElement.get();
    }

    // Ok, DomBB is not the taken branch. If we have an enum with only two
    // cases, we can still infer the other case for the current branch.
    EnumDecl *E = SEI->getEnumOperand().getType().getEnumOrBoundGenericEnum();

    // This will return nullptr if we have more than two cases in our decl.
    return getOtherElementOfTwoElementEnum(E, TrueElement.get());
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
      if (NullablePtr<EnumElementDecl> EltDecl =
              getEnumEltTaken(PredTerm, DomBB)) {
        simplifySwitchEnumInst(cast<SwitchEnumInst>(Term), EltDecl.get(),
                               DomBB);
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
          if (NullablePtr<EnumElementDecl> Element =
                  getEnumEltTaken(PredTerm, DomBB)) {
            simplifyCondBranchInst(CBI, Element.get() == TrueElement.get());
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

template <class SwitchEnumTy, class SwitchEnumCaseTy>
static SILBasicBlock *replaceSwitchDest(SwitchEnumTy *S,
                                     SmallVectorImpl<SwitchEnumCaseTy> &Cases,
                                     unsigned EdgeIdx,
                                     SILBasicBlock *NewDest) {
    auto *DefaultBB = S->hasDefault() ? S->getDefaultBB() : nullptr;
    for (unsigned i = 0, e = S->getNumCases(); i != e; ++i)
      if (EdgeIdx != i)
        Cases.push_back(S->getCase(i));
      else
        Cases.push_back(std::make_pair(S->getCase(i).first, NewDest));
    if (EdgeIdx == S->getNumCases())
      DefaultBB = NewDest;
    return DefaultBB;
}

/// Find a nearest common dominator for a given set of basic blocks.
static DominanceInfoNode *findCommonDominator(ArrayRef<SILBasicBlock *> BBs,
                                              DominanceInfo *DT) {
  DominanceInfoNode *CommonDom = nullptr;
  for (auto *BB : BBs) {
    if (!CommonDom) {
      CommonDom = DT->getNode(BB);
    } else {
      CommonDom = DT->getNode(
          DT->findNearestCommonDominator(CommonDom->getBlock(), BB));
    }
  }
  return CommonDom;
}

/// Find a nearest common dominator for all predecessors of
/// a given basic block.
static DominanceInfoNode *findCommonDominator(SILBasicBlock *BB,
                                              DominanceInfo *DT) {
  SmallVector<SILBasicBlock *, 8> Preds;
  for (auto *Pred: BB->getPreds())
    Preds.push_back(Pred);

  return findCommonDominator(Preds, DT);
}

/// Estimate the cost of inlining a given basic block.
static unsigned basicBlockInlineCost(SILBasicBlock *BB, unsigned Cutoff) {
  unsigned Cost = 0;
  for (auto &I : *BB) {
    auto ICost = instructionInlineCost(I);
    Cost += unsigned(ICost);
    if (Cost > Cutoff)
      return Cost;
  }
  return Cost;
}

namespace {
/// This is a class implementing a dominator-based jump-threading
/// for checked_cast_br [exact].
class CheckedCastBrJumpThreading {
  // The checked_cast_br instruction, which
  // we try to jump-thread
  CheckedCastBranchInst *CCBI;
  // Basic block of the current checked_cast_br instruction.
  SILBasicBlock *BB;
  // Condition used by the current checked_cast_br instruction.
  SILValue Condition;
  // Success branch of the current checked_cast_br instruction.
  SILBasicBlock *SuccessBB;
  // Failure branch of the current checked_cast_br instruction.
  SILBasicBlock *FailureBB;


  // Current dominating checked_cast_br instruction.
  CheckedCastBranchInst *DomCCBI;
  // Basic block of the dominating checked_cast_br instruction.
  SILBasicBlock *DomBB;
  // Condition used by the dominating checked_cast_br instruction.
  SILValue DomCondition;
  // Success branch of the dominating checked_cast_br instruction.
  SILBasicBlock *DomSuccessBB;
  // Failure branch of the dominating checked_cast_br instruction.
  SILBasicBlock *DomFailureBB;


  // Current dominator tree node where we look for a dominating
  // checked_cast_br instruction.
  llvm::DomTreeNodeBase<SILBasicBlock> *Node;
  SILBasicBlock *ArgBB;
  // Dominator information to be used.
  DominanceInfo *DT;
  // Basic block created as a landing BB for all failure predecessors.
  SILBasicBlock *TargetFailureBB;
  // Basic block created as a landing BB for all success predecessors.
  SILBasicBlock *TargetSuccessBB;

  // Cloner used to clone the BB to FailureSuccessBB.
  Optional<BasicBlockCloner> FailureBBCloner;

  // Cloner used to clone the BB to TargetSuccessBB.
  Optional<BasicBlockCloner> SuccessBBCloner;

  // Predecessors reached only via a path along the
  // success branch of the dominating checked_cast_br.
  SmallVector<SILBasicBlock *, 8> SuccessPreds;

  // Predecessors reached only via a path along the
  // failure branch of the dominating checked_cast_br.
  SmallVector<SILBasicBlock *, 8> FailurePreds;

  // All other predecessors, where the outcome of the
  // checked_cast_br along the path is not known.
  SmallVector<SILBasicBlock *, 8> UnknownPreds;

  // Basic blocks to be added to for reprocessing
  // after jump-threading is done.
  SmallVector<SILBasicBlock *, 16> BlocksForWorklist;

  bool areEquivalentConditionsAlongPaths();
  bool areEquivalentConditionsAlongSomePaths();
  bool handleArgBBIsEntryBlock(SILBasicBlock *ArgBB);
  bool checkCloningConstraints();
  void modifyCFGForUnknownPreds();
  void modifyCFGForFailurePreds();
  void modifyCFGForSuccessPreds();
  void updateDominatorTree();
  void updateSSA();
  void addBlockToSimplifyCFGWorklist(SILBasicBlock *BB);
  void addBlocksToWorklist();

  void classifyPredecessor(
      SILBasicBlock *Pred,
      SmallVectorImpl<SILBasicBlock *> &SuccessPreds,
      SmallVectorImpl<SILBasicBlock *> &FailurePreds,
      SmallVectorImpl<SILBasicBlock *> &UnknownPreds,
      bool SuccessDominates,
      bool FailureDominates);

  SILValue isArgValueEquivalentToCondition(SILValue Value,
                               SILBasicBlock *DomBB,
                               SILValue DomValue,
                               DominanceInfo *DT);

public:
  CheckedCastBrJumpThreading(DominanceInfo *DT) {
    this->DT = DT;
  }

  bool trySimplify(TermInst *Term);

  ArrayRef<SILBasicBlock*> getBlocksForWorklist() {
    return BlocksForWorklist;
  }
};
} // end anonymous namespace

void CheckedCastBrJumpThreading::addBlockToSimplifyCFGWorklist(SILBasicBlock *BB) {
  BlocksForWorklist.push_back(BB);
}

/// Add affected blocks for re-processing by simplifyCFG
void CheckedCastBrJumpThreading::addBlocksToWorklist() {
  if (TargetFailureBB) {
    if (!TargetFailureBB->pred_empty())
      addBlockToSimplifyCFGWorklist(TargetFailureBB);
  }

  if (TargetSuccessBB) {
    if (!TargetSuccessBB->pred_empty())
      addBlockToSimplifyCFGWorklist(TargetSuccessBB);
  }

  if (!BB->pred_empty())
    addBlockToSimplifyCFGWorklist(BB);
}

/// Classify a predecessor of a BB containing checked_cast_br as being
/// reachable via success or failure branches of a dominating checked_cast_br
/// or as unknown if it can be reached via success or failure branches
/// at the same time.
void CheckedCastBrJumpThreading::classifyPredecessor(
    SILBasicBlock *Pred,
    SmallVectorImpl<SILBasicBlock *> &SuccessPreds,
    SmallVectorImpl<SILBasicBlock *> &FailurePreds,
    SmallVectorImpl<SILBasicBlock *> &UnknownPreds,
    bool SuccessDominates,
    bool FailureDominates) {
  if (SuccessDominates && FailureDominates) {
    UnknownPreds.push_back(Pred);
    return;
  }

  if (SuccessDominates) {
    SuccessPreds.push_back(Pred);
    return;
  }

  if (FailureDominates) {
    FailurePreds.push_back(Pred);
    return;
  }

  UnknownPreds.push_back(Pred);
}

/// Check if the root value for Value that comes
/// along the path from DomBB is equivalent to the
/// DomCondition.
SILValue CheckedCastBrJumpThreading::isArgValueEquivalentToCondition(
    SILValue Value, SILBasicBlock *DomBB, SILValue DomValue,
    DominanceInfo *DT) {
  SmallPtrSet<ValueBase *, 16> SeenValues;
  DomValue = DomValue.stripClassCasts();

  while (true) {
    Value = Value.stripClassCasts();
    if (Value == DomValue)
      return Value;

    // We know how to propagate through BBArgs only.
    auto *V = dyn_cast<SILArgument>(Value);
    if (!V)
      return SILValue();

    // Have we visited this BB already?
    if (!SeenValues.insert(Value.getDef()).second)
      return SILValue();

    if (SeenValues.size() > 10)
      return SILValue();

    SmallVector<SILValue, 4> IncomingValues;
    if (!V->getIncomingValues(IncomingValues) || IncomingValues.empty())
      return SILValue();

    ValueBase *Def = nullptr;
    for (auto IncomingValue : IncomingValues) {
      // Each incoming value should be either from a block
      // dominated by DomBB or it should be the value used in
      // condition in DomBB
      Value = IncomingValue.stripClassCasts();
      if (Value == DomValue)
        continue;

      // Values should be the same
      if (!Def)
        Def = Value.getDef();

      if (Def != Value.getDef())
        return SILValue();

      if (!DT->dominates(DomBB, Value.getDef()->getParentBB()))
        return SILValue();
      // OK, this value is a potential candidate
    }

    Value = IncomingValues[0];
  }
}

/// Update the SSA form after all changes.
void CheckedCastBrJumpThreading::updateSSA() {
  assert(!(SuccessBBCloner.hasValue() && FailureBBCloner.hasValue()) &&
         "Both cloners cannot be used at the same time yet");

  // Now update the SSA form.
  if (!FailurePreds.empty() && FailureBBCloner.hasValue() &&
      !SuccessBBCloner.hasValue())
    updateSSAAfterCloning(*FailureBBCloner.getPointer(), TargetFailureBB, BB);

  if (SuccessBBCloner.hasValue() && !FailureBBCloner.hasValue()) {
    updateSSAAfterCloning(*SuccessBBCloner.getPointer(), TargetSuccessBB, BB);
  }
}

/// Update the SSA form after all changes.
void CheckedCastBrJumpThreading::updateDominatorTree() {
  // Update the dominator tree.

  // If BB was IDom of something, then PredCBBI becomes the IDOM
  // of this after jump-threading.
  auto *BBDomNode = DT->getNode(BB);
  auto &Children = BBDomNode->getChildren();
  if (Children.size() > 1) {
    SmallVector<DominanceInfoNode *, 16> ChildrenCopy;
    std::copy(Children.begin(), Children.end(),
              std::back_inserter(ChildrenCopy));
    for (auto *Child : ChildrenCopy) {
      DT->changeImmediateDominator(Child, Node);
    }
  }

  DominanceInfoNode *CommonDom;

  // Find a common dominator for all unknown preds.
  if (!UnknownPreds.empty()) {
    // Find a new IDom for FailureBB
    CommonDom = findCommonDominator(FailureBB, DT);
    if (CommonDom)
      DT->changeImmediateDominator(FailureBB, CommonDom->getBlock());

    CommonDom = findCommonDominator(UnknownPreds, DT);
    // This common dominator dominates the BB now.
    if (CommonDom) {
      DT->changeImmediateDominator(BB, CommonDom->getBlock());
    }
  }

  // Find a common dominator for all failure preds.
  CommonDom = findCommonDominator(FailurePreds, DT);
  // This common dominator dominates the TargetFailureBB now.
  if (CommonDom) {
    DT->addNewBlock(TargetFailureBB, CommonDom->getBlock());
    // Find a new IDom for FailureBB
    CommonDom = findCommonDominator(FailureBB, DT);
    if (CommonDom)
      DT->changeImmediateDominator(FailureBB, CommonDom->getBlock());
  }


  // Find a common dominator for all success preds.
  CommonDom = findCommonDominator(SuccessPreds, DT);
  // This common dominator of all success preds dominates the BB now.
  if (CommonDom) {
    if (TargetSuccessBB) {
      DT->addNewBlock(TargetSuccessBB, CommonDom->getBlock());
    } else {
      DT->changeImmediateDominator(BB, CommonDom->getBlock());
    }
    CommonDom = findCommonDominator(SuccessBB, DT);
    if (CommonDom)
      DT->changeImmediateDominator(SuccessBB, CommonDom->getBlock());
  }
  // End of dominator tree update.
}

void CheckedCastBrJumpThreading::modifyCFGForUnknownPreds() {
  if (UnknownPreds.empty())
    return;
  // Check the FailureBB if it is a BB that contains a class_method
  // referring to the same value as a condition. This pattern is typical
  // for method chaining code like obj.method1().method2().etc()
  SILInstruction *Inst = FailureBB->begin();
  if (ClassMethodInst *CMI = dyn_cast<ClassMethodInst>(Inst)) {
    if (CMI->getOperand() == Condition) {
      // Replace checked_cast_br by branch to FailureBB.
      auto &InsnList = BB->getInstList();
      auto *InsertedBI =
          BranchInst::create(CCBI->getLoc(), FailureBB, *BB->getParent());
      CCBI->eraseFromParent();
      InsnList.insert(InsnList.end(), InsertedBI);
    }
  }
}

/// Create a copy of the BB as a landing BB
/// for all FailurePreds.
void CheckedCastBrJumpThreading::modifyCFGForFailurePreds() {
  if (FailurePreds.empty())
    return;

  FailureBBCloner.emplace(BasicBlockCloner(BB));
  FailureBBCloner->clone();
  TargetFailureBB = FailureBBCloner->getDestBB();
  auto *TI = TargetFailureBB->getTerminator();
  SILBuilderWithScope<1> Builder(TI);
  // This BB copy branches to a FailureBB.
  Builder.createBranch(TI->getLoc(), FailureBB);
  TI->eraseFromParent();

  // Redirect all FailurePreds to the copy of BB.
  for (auto *Pred : FailurePreds) {
    TermInst *TI = Pred->getTerminator();
    // Replace branch to BB by branch to TargetFailureBB.
    // FIXME: Why is it valid to assume EdgeIdx=0 here?
    changeBranchTarget(TI, 0, TargetFailureBB, /*PreserveArgs=*/true);
    Pred = nullptr;
  }
}

/// Create a copy of the BB or reuse BB as
/// a landing basic block for all FailurePreds.
void CheckedCastBrJumpThreading::modifyCFGForSuccessPreds() {
  if (!UnknownPreds.empty()) {
    if (!SuccessPreds.empty()) {
      // Create a copy of the BB as a landing BB.
      // for all SuccessPreds.
      SuccessBBCloner.emplace(BasicBlockCloner(BB));
      SuccessBBCloner->clone();
      TargetSuccessBB = SuccessBBCloner->getDestBB();
      auto *TI = TargetSuccessBB->getTerminator();
      SILBuilderWithScope<1> Builder(TI);
      SmallVector<SILValue, 8> SuccessBBArgs;
      // Take argument value from the dominating BB.
      SuccessBBArgs.push_back(DomSuccessBB->getBBArg(0));
      // This BB copy branches to SuccessBB.
      Builder.createBranch(TI->getLoc(), SuccessBB, SuccessBBArgs);
      TI->eraseFromParent();

      // Redirect all SuccessPreds to the copy of BB.
      for (auto *Pred : SuccessPreds) {
        TermInst *TI = Pred->getTerminator();
        // Replace branch to BB by branch to TargetSuccessBB.
        // FIXME: Why is it valid to assume EdgeIdx=0 here?
        changeBranchTarget(TI, 0, TargetSuccessBB, /*PreserveArgs=*/true);
        SuccessBBArgs.push_back(DomSuccessBB->getBBArg(0));
        Pred = nullptr;
      }
    }
  } else {
    // There are no predecessors where it is not clear
    // if they are dominated by a success or failure branch
    // of DomBB. Therefore, there is no need to clone
    // the BB for SuccessPreds. Current BB can be re-used
    // instead as their target.

    // Add an unconditional jump at the end of the block.
    auto &InsnList = BB->getInstList();
    SmallVector<SILValue, 8> SuccessBBArgs;
    // Take argument value from the dominating BB
    SuccessBBArgs.push_back(DomSuccessBB->getBBArg(0));
    auto *InsertedBI = BranchInst::create(CCBI->getLoc(), SuccessBB,
                                          SuccessBBArgs, *BB->getParent());
    CCBI->eraseFromParent();
    InsnList.insert(InsnList.end(), InsertedBI);
  }
}

/// Handle a special case, where ArgBB is the entry block.
bool CheckedCastBrJumpThreading::handleArgBBIsEntryBlock(SILBasicBlock *ArgBB) {
  if (ArgBB->getPreds().empty()) {
    // It must be the entry block
    // See if it is reached over Success or Failure path.
    bool SuccessDominates = DomSuccessBB == BB;
    bool FailureDominates = DomFailureBB == BB;

    classifyPredecessor(ArgBB, SuccessPreds, FailurePreds, UnknownPreds,
                        SuccessDominates, FailureDominates);
    return true;
  }
  return false;
}

// Returns false if cloning required by jump threading cannot
// be performed, because some of the constraints are violated.
bool CheckedCastBrJumpThreading::checkCloningConstraints() {
  // Check some cloning related constraints.

  // If this argument from a different BB, then jump-threading
  // may require too much code duplication.
  if (ArgBB && ArgBB != BB)
    return false;

  // Bail out if current BB cannot be duplicated.
  if (!canDuplicateBlock(BB))
    return false;

  // Check if code-bloat would be too big when this BB
  // is jump-threaded.
  // TODO: Make InlineCostCutoff parameter configurable?
  // Dec 1, 2014:
  // We looked at the inline costs of BBs from our benchmark suite
  // and found that currently the highest inline cost for the
  // whole benchmark suite is 12. In 95% of all cases it is <=3.
  const unsigned InlineCostCutoff = 20;
  if (basicBlockInlineCost(BB, InlineCostCutoff) >= InlineCostCutoff)
    return false;

  return true;
}

/// If conditions are not equivalent along all paths, try harder
/// to check if they are actually equivalent along a subset of paths.
/// To do it, try to back-propagate the Condition
/// backwards and see if it is actually equivalent to DomCondition.
/// along some of the paths.
bool CheckedCastBrJumpThreading::areEquivalentConditionsAlongSomePaths() {
  auto *Arg = dyn_cast<SILArgument>(Condition);
  if (!Arg)
    return false;

  ArgBB = Arg->getParent();
  if (!DT->dominates(DomBB, ArgBB))
    return false;

  // Incoming values for the BBArg.
  SmallVector<SILValue, 4> IncomingValues;

  if (ArgBB != ArgBB->getParent()->begin() &&
      (!Arg->getIncomingValues(IncomingValues) || IncomingValues.empty()))
    return false;

  // Check for each predecessor, if the incoming value coming from it
  // is equivalent to the DomCondition. If this is the case, it is
  // possible to try jump-threading along this path.
  if (!handleArgBBIsEntryBlock(ArgBB)) {
    // ArgBB is not the entry block and has predecessors.
    unsigned idx = 0;
    for (auto *PredBB : ArgBB->getPreds()) {
      auto IncomingValue = IncomingValues[idx];
      SILValue ReachingValue = isArgValueEquivalentToCondition(
          IncomingValue, DomBB, DomCondition, DT);

      if (ReachingValue == SILValue()) {
        UnknownPreds.push_back(PredBB);
        idx++;
        continue;
      }

      // Condition is the same if BB is reached over a pass through Pred.
      DEBUG(llvm::dbgs() << "Condition is the same if reached over ");
      DEBUG(PredBB->print(llvm::dbgs()));

      // See if it is reached over Success or Failure path.
      bool SuccessDominates = DT->dominates(DomSuccessBB, PredBB) ||
                              DT->dominates(DomSuccessBB, BB) ||
                              DomSuccessBB == BB;
      bool FailureDominates = DT->dominates(DomFailureBB, PredBB) ||
                              DT->dominates(DomFailureBB, BB) ||
                              DomFailureBB == BB;

      classifyPredecessor(
          PredBB, SuccessPreds, FailurePreds, UnknownPreds,
          SuccessDominates, FailureDominates);
      idx++;
    }
  }


  // At this point we know for each predecessor of ArgBB if its reached
  // over the success, failure or unknown path from DomBB.

  // Now we can generate a new BB for preds reaching BB over the success
  // path and a new BB for preds reaching BB over the failure path.
  // Then we redirect those preds to those new basic blocks.
  return true;
}

/// Check if conditions of CCBI and DomCCBI are equivalent along
/// all or at least  some paths.
bool CheckedCastBrJumpThreading::areEquivalentConditionsAlongPaths() {
  // Are conditions equivalent along all paths?
  if (areEquivalentConditions(DomCondition, Condition)) {
    // Conditions are exactly the same, without any restrictions.
    // They are equivalent along all paths.

    // Figure out for each predecessor which branch of
    // the dominating checked_cast_br is used to reach it.
    for (auto *PredBB : BB->getPreds()) {
      // All predecessors should either unconditionally branch
      // to the current BB or be another checked_cast_br instruction.
      if (!dyn_cast<CheckedCastBranchInst>(PredBB->getTerminator()) &&
          !dyn_cast<BranchInst>(PredBB->getTerminator()))
        return false;

      bool SuccessDominates =
          DT->dominates(DomSuccessBB, PredBB) || DomSuccessBB == BB;
      bool FailureDominates =
          DT->dominates(DomFailureBB, PredBB) || DomFailureBB == BB;

      classifyPredecessor(PredBB, SuccessPreds, FailurePreds, UnknownPreds,
                          SuccessDominates, FailureDominates);
    }
    return true;
  }

  // Check if conditions are equivalent along a subset of reaching paths.
  return areEquivalentConditionsAlongSomePaths();
}

/// Try performing a dominator-based jump-threading for
/// checked_cast_br instructions.
bool CheckedCastBrJumpThreading::trySimplify(TermInst *Term) {
  CCBI = cast<CheckedCastBranchInst>(Term);
  if (!CCBI)
    return false;

  // Init information about the checked_cast_br we try to
  // jump-thread.
  BB = Term->getParent();
  Condition = Term->getOperand(0).stripClassCasts();
  SuccessBB = CCBI->getSuccessBB();
  FailureBB = CCBI->getFailureBB();

  // Find a dominating checked_cast_br, which performs the same check.
  for (Node = DT->getNode(BB)->getIDom(); Node; Node = Node->getIDom()) {
    // Get current dominating block.
    DomBB = Node->getBlock();
    auto *DomTerm = DomBB->getTerminator();

    if (!DomTerm->getNumOperands())
      continue;

    // Check that it is a dominating checked_cast_br.
    DomCCBI = dyn_cast<CheckedCastBranchInst>(DomTerm);
    if (!DomCCBI)
      continue;

    // We need to verify that the result type is the same in the
    // dominating checked_cast_br.
    if (DomCCBI->getCastType() != CCBI->getCastType())
      continue;


    // Conservatively check that both checked_cast_br instructions
    // are either exact or non-exact. This is very conservative,
    // but safe.
    //
    // TODO:
    // If the dominating checked_cast_br is non-exact, then
    // it is in general not safe to assume that current exact cast
    // would have the same outcome. But if the the dominating
    // non-exact checked_cast_br fails, then the current exact cast
    // would always fail as well.
    //
    // If the dominating checked_cast_br is exact then then
    // it is in general not safe to assume that the current non-exact
    // cast would have the same outcome. But if the the dominating
    // exact checked_cast_br succeeds, then the current non-exact
    // cast would always succeed as well.
    //
    // TODO: In some specific cases, it is possible to prove that
    // success or failure of the dominating cast is equivalent to
    // the success or failure of the current cast, even if one
    // of them is exact and the other not. This is the case
    // e.g. if the class has no subclasses.
    if (DomCCBI->isExact() != CCBI->isExact())
      continue;

    // Initialize state variables for the current round of checks
    // based on the found dominating checked_cast_br.
    DomSuccessBB = DomCCBI->getSuccessBB();
    DomFailureBB = DomCCBI->getFailureBB();
    DomCondition = DomTerm->getOperand(0).stripClassCasts();

    // Init state variables for paths analysis
    SuccessPreds.clear();
    FailurePreds.clear();
    UnknownPreds.clear();
    ArgBB = nullptr;

    // Init state variables for jump-threading transformation.
    TargetFailureBB = nullptr;
    TargetSuccessBB = nullptr;

    // Are conditions of CCBI and DomCCBI equivalent along (some) paths?
    // If this is the case, classify all incoming paths into SuccessPreds,
    // FailurePreds or UnknownPreds depending on how they reach CCBI.
    if (!areEquivalentConditionsAlongPaths())
      continue;

    // Check if any jump-threding is required and possible.
    if (SuccessPreds.empty() && FailurePreds.empty())
      return false;

    unsigned TotalPreds =
        SuccessPreds.size() + FailurePreds.size() + UnknownPreds.size();

    // We only need to clone the BB if not all of its
    // predecessors are in the same group.
    if (TotalPreds != SuccessPreds.size() &&
        TotalPreds != UnknownPreds.size()) {
      // Check some cloning related constraints.
      if (!checkCloningConstraints())
        return false;
    }

    // If we have predecessors, where it is not known if they are reached over
    // success or failure path, we cannot eliminate a checked_cast_br.
    // We have to generate new dedicated BBs as landing BBs for all
    // FailurePreds and all SuccessPreds.

    // Since we are going to change the BB,
    // add its successors and predecessors
    // for re-processing.
    for (auto *B : BB->getPreds()) {
      addBlockToSimplifyCFGWorklist(B);
    }

    for (auto &B : BB->getSuccs()) {
      addBlockToSimplifyCFGWorklist(B.getBB());
    }

    // Create a copy of the BB as a landing BB
    // for all FailurePreds.
    modifyCFGForFailurePreds();

    // Create a copy of the BB or reuse BB as
    // a landing basic block for all SuccessPreds.
    modifyCFGForSuccessPreds();

    // Handle unknown preds.
    modifyCFGForUnknownPreds();

    // Update the dominator tree after all changes.
    updateDominatorTree();

    // Update the SSA form after all changes.
    updateSSA();

    // Since a few BBs were changed now, add them for re-processing.
    addBlocksToWorklist();

    return true;
  }

  // Jump-threading was not possible.
  return false;
}


/// Perform a dominator-based jump-threading for checked_cast_br [exact]
/// instructions if they use the same condition (modulo upcasts and downcasts).
/// This is very beneficial for code that:
///  - references the same object multiple times (e.g. x.f1() + x.f2())
///  - and for method invocation chaining (e.g. x.f3().f4().f5())
bool
SimplifyCFG::trySimplifyCheckedCastBr(TermInst *Term, DominanceInfo *DT) {

  CheckedCastBrJumpThreading CCBJumpThreading(DT);

  bool Result = CCBJumpThreading.trySimplify(Term);

  if (Result) {
    auto BBs = CCBJumpThreading.getBlocksForWorklist();
    for (auto BB: BBs)
      addToWorklist(BB);
  }

  return Result;
}

// Simplifications that walk the dominator tree to prove redundancy in
// conditional branching.
bool SimplifyCFG::dominatorBasedSimplify(DominanceInfo *DT) {
  bool Changed = false;
  for (auto &BB : Fn) {
    // Any method called from this loop should update
    // the DT if it changes anything related to dominators.
    if (isConditional(BB.getTerminator())) {
      if (trySimplifyConditional(BB.getTerminator(), DT))
        Changed = true;
      else if (dyn_cast<CheckedCastBranchInst>(BB.getTerminator()))
        Changed = trySimplifyCheckedCastBr(BB.getTerminator(), DT);
    }
    // Simplify the block argument list.
    Changed |= simplifyArgs(&BB);
  }

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
    
    // Also allow enum of enum, which usually can be combined to a single
    // instruction. This helps to simplify the creation of an enum from an
    // integer raw value.
    if (isa<EnumInst>(User))
      return true;
  }
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
  if (!canDuplicateBlock(SEIBB))
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

  // We also need to check for the absence of payload uses. we are not handling
  // them.
  auto SwitchDestBB0 = SEI->getCaseDestination(E0->getElement());
  auto SwitchDestBB1 = SEI->getCaseDestination(E1->getElement());
  return SwitchDestBB0->getNumBBArg() == 0 && SwitchDestBB1->getNumBBArg() == 0;
}

void SimplifyCFG::findLoopHeaders() {
  /// Find back edges in the CFG. This performs a dfs search and identifies
  /// back edges as edges going to an ancestor in the dfs search. If a basic
  /// block is the target of such a back edge we will identify it as a header.
  LoopHeaders.clear();

  SmallPtrSet<SILBasicBlock *, 16> Visited;
  SmallPtrSet<SILBasicBlock *, 16> InDFSStack;
  SmallVector<std::pair<SILBasicBlock *, SILBasicBlock::succ_iterator>, 16>
      DFSStack;

  auto EntryBB = &Fn.front();
  DFSStack.push_back(std::make_pair(EntryBB, EntryBB->succ_begin()));
  Visited.insert(EntryBB);
  InDFSStack.insert(EntryBB);

  while (!DFSStack.empty()) {
    auto &D = DFSStack.back();
    // No successors.
    if (D.second == D.first->succ_end()) {
      // Retreat the dfs search.
      DFSStack.pop_back();
      InDFSStack.erase(D.first);
    } else {
      // Visit the next successor.
      SILBasicBlock *NextSucc = *(D.second);
      ++D.second;
      if (Visited.insert(NextSucc).second) {
        InDFSStack.insert(NextSucc);
        DFSStack.push_back(std::make_pair(NextSucc, NextSucc->succ_begin()));
      } else if (InDFSStack.count(NextSucc)) {
        // We have already visited this node and it is in our dfs search. This
        // is a back-edge.
        LoopHeaders.insert(NextSucc);
      }
    }
  }
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
                  canDuplicateBlock(DestBB);

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

  // Don't jump thread through a potential header - this can produce irreducible
  // control flow.
  if (!isThreadableEnumInst && LoopHeaders.count(DestBB))
    return false;

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

    auto ClonedSEI = SrcBB->getTerminator();
    auto &InstList0 = SrcBB->getInstList();
    InstList0.insert(InstList0.end(),
                     BranchInst::create(SEI->getLoc(), SwitchDestBB0,
                                        *SEI->getFunction()));

    auto &InstList1 = SEI->getParent()->getInstList();
    InstList1.insert(InstList1.end(),
                     BranchInst::create(SEI->getLoc(), SwitchDestBB1,
                                        *SEI->getFunction()));
    ClonedSEI->eraseFromParent();
    SEI->eraseFromParent();
  }

  if (HasDestBBDefsUsedOutsideBlock) {
    updateSSAAfterCloning(Cloner, SrcBB, DestBB);
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

/// \return If this basic blocks has a single br instruction passing all of the
/// arguments in the original order, then returns the destination of that br.
static SILBasicBlock *getTrampolineDest(SILBasicBlock *SBB) {
  // Ignore blocks with more than one instruction.
  if (SBB->getTerminator() != SBB->begin())
    return nullptr;

  BranchInst *BI = dyn_cast<BranchInst>(SBB->getTerminator());
  if (!BI)
    return nullptr;

  // Disallow infinite loops.
  if (BI->getDestBB() == SBB)
    return nullptr;

  auto BrArgs = BI->getArgs();
  if (BrArgs.size() != SBB->getNumBBArg())
    return nullptr;

  // Check that the arguments are the same and in the right order.
  for (int i = 0, e = SBB->getNumBBArg(); i < e; ++i)
    if (BrArgs[i] != SBB->getBBArg(i))
      return nullptr;

  return BI->getDestBB();
}

/// \return If this is a basic block without any arguments and it has
/// a single br instruction, return this br.
static BranchInst *getTrampolineWithoutBBArgsTerminator(SILBasicBlock *SBB) {
  if (!SBB->bbarg_empty())
    return nullptr;

  // Ignore blocks with more than one instruction.
  if (SBB->getTerminator() != SBB->begin())
    return nullptr;

  BranchInst *BI = dyn_cast<BranchInst>(SBB->getTerminator());
  if (!BI)
    return nullptr;

  // Disallow infinite loops.
  if (BI->getDestBB() == SBB)
    return nullptr;

  return BI;
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

    if (LoopHeaders.count(DestBB))
      LoopHeaders.insert(BB);

    removeFromWorklist(DestBB);
    DestBB->eraseFromParent();
    ++NumBlocksMerged;
    return true;
  }

  // If the destination block is a simple trampoline (jump to another block)
  // then jump directly.
  if (SILBasicBlock *TrampolineDest = getTrampolineDest(DestBB)) {
    SILBuilderWithScope<1>(BI).createBranch(BI->getLoc(), TrampolineDest,
                                            BI->getArgs());
    // Eliminating the trampoline can expose opportuntities to improve the
    // new block we branch to.
    if (LoopHeaders.count(DestBB))
      LoopHeaders.insert(BB);

    addToWorklist(TrampolineDest);
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

/// \brief Check if replacing an existing edge of the terminator by another
/// one which has a DestBB as its destination would create a critical edge.
static bool wouldIntroduceCriticalEdge(TermInst *T, SILBasicBlock *DestBB) {
  auto SrcSuccs = T->getSuccessors();
  if (SrcSuccs.size() <= 1)
    return false;

  assert(!DestBB->pred_empty() && "There should be a predecessor");
  if (DestBB->getSinglePredecessor())
    return false;

  return true;
}

/// \brief Is the first side-effect instruction in this block a cond_fail that
/// is guarantueed to fail.
static bool isCondFailBlock(SILBasicBlock *BB,
                            CondFailInst *&OrigCondFailInst) {
  auto It = BB->begin();
  CondFailInst *CondFail = nullptr;
  // Skip instructions that don't have side-effects.
  while (It != BB->end() && !(CondFail = dyn_cast<CondFailInst>(It))) {
    if (It->mayHaveSideEffects())
      return false;
    ++It;
  }
  if (!CondFail)
    return false;
  auto *IL = dyn_cast<IntegerLiteralInst>(CondFail->getOperand());
  if (!IL)
    return false;
  OrigCondFailInst = CondFail;
  return IL->getValue() != 0;
}

/// simplifyCondBrBlock - Simplify a basic block that ends with a conditional
/// branch.
bool SimplifyCFG::simplifyCondBrBlock(CondBranchInst *BI) {
  // First simplify instructions generating branch operands since that
  // can expose CFG simplifications.
  simplifyBranchOperands(OperandValueArrayRef(BI->getAllOperands()));
  auto *ThisBB = BI->getParent();
  SILBasicBlock *TrueSide = BI->getTrueBB();
  SILBasicBlock *FalseSide = BI->getFalseBB();
  auto TrueArgs = BI->getTrueArgs();
  auto FalseArgs = BI->getFalseArgs();

  // If the condition is an integer literal, we can constant fold the branch.
  if (auto *IL = dyn_cast<IntegerLiteralInst>(BI->getCondition())) {
    bool isFalse = !IL->getValue();
    auto LiveArgs =  isFalse ? FalseArgs : TrueArgs;
    auto *LiveBlock =  isFalse ? FalseSide : TrueSide;
    auto *DeadBlock = !isFalse ? FalseSide : TrueSide;
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
  SILBasicBlock *TrueTrampolineDest = getTrampolineDest(TrueSide);
  if (TrueTrampolineDest && TrueTrampolineDest != FalseSide) {
    SILBuilderWithScope<1>(BI)
      .createCondBranch(BI->getLoc(), BI->getCondition(),
                        TrueTrampolineDest, TrueArgs,
                        FalseSide, FalseArgs);
    BI->eraseFromParent();

    if (LoopHeaders.count(TrueSide))
      LoopHeaders.insert(ThisBB);
    removeIfDead(TrueSide);
    addToWorklist(ThisBB);
    return true;
  }

  SILBasicBlock *FalseTrampolineDest = getTrampolineDest(FalseSide);
  if (FalseTrampolineDest && FalseTrampolineDest != TrueSide) {
    SILBuilderWithScope<1>(BI)
      .createCondBranch(BI->getLoc(), BI->getCondition(),
                        TrueSide, TrueArgs,
                        FalseTrampolineDest, FalseArgs);
    BI->eraseFromParent();
    if (LoopHeaders.count(FalseSide))
      LoopHeaders.insert(ThisBB);
    removeIfDead(FalseSide);
    addToWorklist(ThisBB);
    return true;
  }

  // Simplify cond_br where both sides jump to the same blocks with the same
  // args.
  if (TrueArgs == FalseArgs && (TrueSide == FalseTrampolineDest ||
                                FalseSide == TrueTrampolineDest)) {
    SILBuilderWithScope<1>(BI).createBranch(BI->getLoc(),
                      TrueTrampolineDest ? FalseSide : TrueSide, TrueArgs);
    BI->eraseFromParent();
    addToWorklist(ThisBB);
    addToWorklist(TrueSide);
    ++NumConstantFolded;
    return true;
  }

  auto *TrueTrampolineBr = getTrampolineWithoutBBArgsTerminator(TrueSide);
  if (TrueTrampolineBr &&
      !wouldIntroduceCriticalEdge(BI, TrueTrampolineBr->getDestBB())) {
    SILBuilderWithScope<1>(BI).createCondBranch(
        BI->getLoc(), BI->getCondition(),
        TrueTrampolineBr->getDestBB(), TrueTrampolineBr->getArgs(),
        FalseSide, FalseArgs);
    BI->eraseFromParent();

    if (LoopHeaders.count(TrueSide))
      LoopHeaders.insert(ThisBB);
    removeIfDead(TrueSide);
    addToWorklist(ThisBB);
    return true;
  }

  auto *FalseTrampolineBr = getTrampolineWithoutBBArgsTerminator(FalseSide);
  if (FalseTrampolineBr &&
      !wouldIntroduceCriticalEdge(BI, FalseTrampolineBr->getDestBB())) {
    SILBuilderWithScope<1>(BI).createCondBranch(
        BI->getLoc(), BI->getCondition(),
        TrueSide, TrueArgs,
        FalseTrampolineBr->getDestBB(), FalseTrampolineBr->getArgs());
    BI->eraseFromParent();
    if (LoopHeaders.count(FalseSide))
      LoopHeaders.insert(ThisBB);
    removeIfDead(FalseSide);
    addToWorklist(ThisBB);
    return true;
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

  // Simplify a condition branch to a block starting with "cond_fail 1".
  //
  // cond_br %cond, TrueSide, FalseSide
  // TrueSide:
  //   cond_fail 1
  //
  bool IsTrueSideFailing;
  CondFailInst *OrigCFI = nullptr;
  if ((IsTrueSideFailing = isCondFailBlock(TrueSide, OrigCFI)) ||
      isCondFailBlock(FalseSide, OrigCFI)) {
    auto LiveArgs =  IsTrueSideFailing ? FalseArgs : TrueArgs;
    auto *LiveBlock =  IsTrueSideFailing ? FalseSide : TrueSide;
    auto *DeadBlock = !IsTrueSideFailing ? FalseSide : TrueSide;
    auto *ThisBB = BI->getParent();
    auto CFCondition = BI->getCondition();

    // If the false side is failing, negate the branch condition.
    if (!IsTrueSideFailing) {
      auto *True = SILBuilderWithScope<1>(BI).createIntegerLiteral(
          OrigCFI->getLoc(), OrigCFI->getOperand().getType(), 1);
      CFCondition = SILBuilderWithScope<1>(BI).createBuiltinBinaryFunction(
          OrigCFI->getLoc(), "xor", CFCondition.getType(),
          CFCondition.getType(), {CFCondition, True});
    }

    // Create the cond_fail and a branch.
    SILBuilderWithScope<1>(BI)
        .createCondFail(OrigCFI->getLoc(), CFCondition);
    SILBuilderWithScope<1>(BI).createBranch(BI->getLoc(), LiveBlock, LiveArgs);

    BI->eraseFromParent();

    addToWorklist(ThisBB);
    simplifyAfterDroppingPredecessor(DeadBlock);
    addToWorklist(LiveBlock);
    return true;
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

bool SimplifyCFG::simplifyCheckedCastBranchBlock(CheckedCastBranchInst *CCBI) {
  // [exact] does not perform any cast.
  // It checks that the types are exactly the same.
  if (CCBI->isExact())
    return false;

  bool isSourceTypeExact = isa<MetatypeInst>(CCBI->getOperand());

  // Check if we can statically predict the outcome of the cast.
  auto Feasibility = classifyDynamicCast(CCBI->getModule().getSwiftModule(),
                          CCBI->getOperand().getType().getSwiftRValueType(),
                          CCBI->getCastType().getSwiftRValueType(),
                          isSourceTypeExact);

  if (Feasibility == DynamicCastFeasibility::MaySucceed)
    return false;

  auto *FailureBB = CCBI->getFailureBB();
  auto *SuccessBB = CCBI->getSuccessBB();
  auto *ThisBB = CCBI->getParent();


  if (Feasibility == DynamicCastFeasibility::WillFail) {
    SILBuilderWithScope<1> Builder(CCBI);
    Builder.createBranch(CCBI->getLoc(), FailureBB);
    CCBI->eraseFromParent();
    removeIfDead(SuccessBB);
    addToWorklist(ThisBB);
    return true;
  }

  if (Feasibility == DynamicCastFeasibility::WillSucceed) {
    SILBuilderWithScope<1> Builder(CCBI);
    SmallVector<SILValue, 1> Args;
    bool ResultHasNoUse = SuccessBB->getBBArg(0)->use_empty();
    SILValue CastedValue ;
    if (CCBI->getOperand().getType() != CCBI->getCastType()) {
      if (!ResultHasNoUse) {
        // Replace by unconditional_cast, followed by a branch.
        CastedValue = Builder.createUnconditionalCheckedCast(
            CCBI->getLoc(), CCBI->getOperand(), CCBI->getCastType());
      } else {
        CastedValue = SILUndef::get(CCBI->getCastType(), CCBI->getModule());
      }
    } else {
      // No need to cast.
      CastedValue = CCBI->getOperand();
    }
    Args.push_back(CastedValue);
    Builder.createBranch(CCBI->getLoc(), SuccessBB, Args);
    CCBI->eraseFromParent();
    removeIfDead(FailureBB);
    addToWorklist(ThisBB);
    return true;
  }

  return false;
}


void RemoveUnreachable::visit(SILBasicBlock *BB) {
  if (!Visited.insert(BB).second)
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
    case ValueKind::CheckedCastBranchInst:
      Changed |= simplifyCheckedCastBranchBlock(cast<CheckedCastBranchInst>(TI));
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

    NullablePtr<EnumElementDecl> elementDecl = SWI->getUniqueCaseForDefault();
    if (!elementDecl)
      continue;
    
    // Construct a new instruction by copying all the case entries.
    SmallVector<std::pair<EnumElementDecl*, SILBasicBlock*>, 4> CaseBBs;
    for (int idx = 0, numIdcs = SWI->getNumCases(); idx < numIdcs; idx++) {
      CaseBBs.push_back(SWI->getCase(idx));
    }
    // Add the default-entry of the original instruction as case-entry.
    CaseBBs.push_back(std::make_pair(elementDecl.get(), SWI->getDefaultBB()));

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

  // Find the set of loop headers. We don't want to jump-thread through headers.
  findLoopHeaders();

  DT = nullptr;
  PDT = nullptr;
  if (simplifyBlocks()) {
    // Simplifying other blocks might have resulted in unreachable
    // loops.
    RU.run();

    // Force dominator recomputation below.
    PM->invalidateAnalysis(&Fn, SILAnalysis::InvalidationKind::CFG);
    Changed = true;
  }

  // Do simplifications that require the dominator tree to be accurate.
  DominanceAnalysis *DA = PM->getAnalysis<DominanceAnalysis>();
  DT = DA->getDomInfo(&Fn);
  PDT = DA->getPostDomInfo(&Fn);
  Changed |= dominatorBasedSimplify(DT);
  DT = nullptr;
  PDT = nullptr;

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

/// Check a diamond-form property of graphs generated by swith_enum
/// instructions, who only produce integer values by each of BBs handling its
/// case tags.
/// In such graphs, switch_enum dominates any blocks processing cases and all
/// those blocks processing different cases are post-dominated by a single
/// basic block consuming those values, where all the paths join.
static bool isDiamondForm(SILBasicBlock *BB, SILBasicBlock *PredBB,
                          SILBasicBlock *PostBB, DominanceInfo *DT,
                          PostDominanceInfo *PDT) {
  if (PredBB && !DT->dominates(PredBB, BB))
    return false;
  if (PostBB && !PDT->dominates(PostBB, BB))
    return false;
  return true;
}

/// Check if a basic blocks consists of a single branch instruction.
static bool isSingleBranchBlock(SILBasicBlock *BB) {
  TermInst *TI = BB->getTerminator();
  if (!isa<BranchInst>(TI))
    return false;
  return TI == BB->begin();
}

/// Find a parent SwitchEnumInst of a basic block. If SEI is set, then
/// only return non-nullptr if the found SwitchEnumInst is the same one as SEI.
/// We consider only those predecessor blocks which are post-dominated by
/// PostBB and dominated by SEI. This ensures that we have a diamond-like CFG
/// starting at SEI and ending at PostBB.
static SwitchEnumInst *
getSwitchEnumPred(SILBasicBlock *BB, SwitchEnumInst *SEI, SILBasicBlock *PostBB,
                  SmallPtrSet<SILBasicBlock *, 8> &Blocks,
                  SmallPtrSet<SILBasicBlock *, 8> Visited, DominanceInfo *DT,
                  PostDominanceInfo *PDT) {

  if (BB->pred_empty())
    return nullptr;

  // Any BB can be visited only once.
  if (Visited.count(BB))
    return nullptr;

  // Remember that this BB was seen already.
  Visited.insert(BB);

  // Only consider blocks which are post-dominated by PostBB and
  // dominated by SEI.
  if (!isDiamondForm(BB, (SEI) ? SEI->getParent() : nullptr, PostBB, DT, PDT))
    return nullptr;

  // Check that this block only produces the value, but does not
  // have any side effects.
  bool BBHasIntegerLiteral = false;
  auto First = BB->begin();
  auto *BI = dyn_cast<BranchInst>(BB->getTerminator());
  if (!BI)
    return nullptr;
  if (BI != First) {
    // There may be only one instruction before the branch.
    if (BI != next(First))
      return nullptr;

    // There are some instructions besides the branch.
    // It should be only an integer literal instruction.
    // Handle only integer values for now.
    auto *ILI = dyn_cast<IntegerLiteralInst>(First);
    if (!ILI)
      return nullptr;

    // Check that this literal is used by the terminator.
    for (auto U : ILI->getUses())
      if (U->getUser() != BI)
        return nullptr;

    // The branch can pass arguments only to the PostBB.
    if (BI->getDestBB() != PostBB)
      return nullptr;
    BBHasIntegerLiteral = true;
  }

  // Each BB on the path should have only a single branch instruction.
  // The only exception is a BB which has a BB ending with switch_enum
  // as its single predecessor. Such a block may have an integer_literal
  // instruction before the branch.

  for (auto PredBB : BB->getPreds()) {
    SwitchEnumInst *PredSEI;
    if (isSingleBranchBlock(PredBB)) {
      PredSEI = getSwitchEnumPred(PredBB, SEI, PostBB, Blocks, Visited,
                                  DT, PDT);
    } else {
      // Check if a predecessor BB terminates with a switch_enum instruction
      PredSEI = dyn_cast<SwitchEnumInst>(PredBB->getTerminator());
      if (!PredSEI)
        return nullptr;
      // Remember that this BB is immediately reachable from a switch_enum.
      Blocks.insert(BB);
    }

    if (!PredSEI)
      return nullptr;

    if (SEI && PredSEI != SEI)
      return nullptr;

    if (!SEI)
      SEI = PredSEI;
  }
  return SEI;
}

/// Helper function to produce a SILValue from a result value
/// produced by a basic block responsible for handling a
/// specific enum tag.
static SILValue
getSILValueFromCaseResult(SILBuilder &B, SILLocation Loc,
                          SILType Type, SILValue Val) {
  if (auto *IL = dyn_cast<IntegerLiteralInst>(Val)) {
    auto Value = IL->getValue();
    if (Value.getBitWidth() != 1)
      return B.createIntegerLiteral(Loc, Type, Value);
    else
      // This is a boolean value
      return B.createIntegerLiteral(Loc, Type, Value.getBoolValue());
  } else {
    llvm::errs() << "Non IntegerLiteralInst switch case result\n";
    Val.dump();
    return Val;
  }
}

/// Given an integer argument, see if it is ultimately matching whether
/// a given enum is of a given tag.  If so, create a new select_enum instruction
/// This is used to simplify arbitrary simple switch_enum diamonds into
/// select_enums.
bool simplifySwitchEnumToSelectEnum(SILBasicBlock *BB, unsigned ArgNum,
                                    SILArgument *IntArg, DominanceInfo *DT,
                                    PostDominanceInfo *PDT) {

  if (!DT || !PDT)
    return false;

  // Don't know which values should be passed if there is more
  // than one basic block argument.
  if (BB->bbarg_size() > 1)
    return false;

  // Mapping from case values to the results corresponding to this case value.
  SmallVector<std::pair<EnumElementDecl *, SILValue>, 8> CaseToValue;

  // Mapping from BB responsible for a specific case value to the result it
  // produces.
  llvm::DenseMap<SILBasicBlock *, SILValue> BBToValue;

  // switch_enum instruction to be replaced.
  SwitchEnumInst *SEI = nullptr;

  bool HasNonSwitchEnumPreds = false;

  // Iterate over all immediate predecessors of the target basic block.
  // - Check that each one stems directly or indirectly from the same
  //   switch_enum instruction.
  // - Remember for each case tag of the switch_enum instruction which
  //   integer value it produces.
  // - Check that each block handling a given case tag of a switch_enum
  //   only produces an integer value and does not have any side-effects.
  for (auto P : BB->getPreds()) {
    // Only handle branch instructions.
    auto *TI = P->getTerminator();
    if (!isa<BranchInst>(TI))
      return false;

    // Find the Nth argument passed to BB.
    auto Arg = TI->getOperand(ArgNum);
    auto *SI = dyn_cast<SILInstruction>(Arg);
    if (!SI) {
        return false;
    } else {
      // Handle integer values
      auto *IntLit = dyn_cast<IntegerLiteralInst>(SI);
      if (!IntLit) {
        // TODO: SI may be any instruction that dominates the switch_enum
        // instruction?
        //if (!DT->dominates(SI->getParent(), P))
        return false;
      }
    }

    // Set of blocks that branch to/reach this basic block P and are immediate
    // successors of a switch_enum instruction.
    SmallPtrSet<SILBasicBlock *, 8> Blocks;
    // Set of blocks visited during a search for a parent switch_enum instruction.
    SmallPtrSet<SILBasicBlock *, 8> Visited;

    // Try to find a parent SwitchEnumInst for the current predecessor of BB.
    auto *PredSEI = getSwitchEnumPred(P, SEI, BB, Blocks, Visited, DT, PDT);

    // Predecessor is not produced by a switch_enum instruction, bail.
    if (!PredSEI) {
      HasNonSwitchEnumPreds = true;
      continue;
    }

    // Check if all predecessors stem from the same switch_enum instruction.
    if (SEI) {
      if (SEI != PredSEI) {
        // It comes from a different switch_enum instruction, bail.
        HasNonSwitchEnumPreds = true;
        continue;
      }
    } else {
      SEI = PredSEI;
    }

    // Remember the result value used to branch to this instruction.
    for (auto B : Blocks)
      BBToValue[B] = Arg;
  }

  if (!SEI)
    return false;

  // Insert the new enum_select instruction right after enum_switch
  SILBuilder B(SEI);

  // Form a set of case_tag:result pairs for select_enum
  for (unsigned i = 0, e = SEI->getNumCases(); i != e; ++i) {
    std::pair<EnumElementDecl *, SILBasicBlock *> Pair = SEI->getCase(i);
    // If one of the branches is not covered, bail
    if (!BBToValue.count(Pair.second))
      return false;
    auto CaseValue = BBToValue[Pair.second];
    auto CaseSILValue = getSILValueFromCaseResult(B, SEI->getLoc(),
                                                  IntArg->getType(),
                                                  CaseValue);
    CaseToValue.push_back(std::make_pair(Pair.first, CaseSILValue));
  }

  // Default value for select_enum.
  SILValue DefaultSILValue = SILValue();

  if (SEI->hasDefault()) {
    // Try to define a default case for enum_select based
    // on the default case of enum_switch.

    // If default branch is not covered, bail
    if (!BBToValue.count(SEI->getDefaultBB()))
      return false;
    auto DefaultValue = BBToValue[SEI->getDefaultBB()];
    DefaultSILValue = getSILValueFromCaseResult(B, SEI->getLoc(),
                                                IntArg->getType(),
                                                DefaultValue);
  } else {
    // Try to see if enum_switch covers all possible cases.
    // If it does, then pick one of those cases as a default.

    // Count the number of possible case tags for a given enum type
    auto *Enum = SEI->getOperand().getType().getEnumOrBoundGenericEnum();
    unsigned ElemCount = 0;
    for (auto E : Enum->getAllElements()) {
      if (E)
        ElemCount++;
    }

    // Check if all possible cases are covered.
    if (ElemCount == SEI->getNumCases()) {
      // This enum_switch instruction is exhaustive.
      // Make the last case a default.
      auto Pair = CaseToValue.pop_back_val();
      DefaultSILValue = Pair.second;
    }
  }

  // We don't need to have explicit cases for any case tags which produce the
  // same result as the default branch.
  if (DefaultSILValue != SILValue()) {
    auto DefaultValue = DefaultSILValue;
    auto *DefaultSI = dyn_cast<IntegerLiteralInst>(DefaultValue);
    for (auto I = CaseToValue.begin(); I != CaseToValue.end();) {
      auto CaseValue = I->second;
      if (CaseValue == DefaultValue) {
        I = CaseToValue.erase(I);
        continue;
      }

      if (DefaultSI) {
        if (auto CaseSI = dyn_cast<IntegerLiteralInst>(CaseValue)) {
          if (DefaultSI->getValue() == CaseSI->getValue()) {
            I = CaseToValue.erase(I);
            continue;
          }
        }
      }
      ++I;
    }
  }

  // Create a new select_enum instruction
  auto SelectInst = B.createSelectEnum(SEI->getLoc(), SEI->getOperand(),
                                       IntArg->getType(),
                                       DefaultSILValue, CaseToValue);
  if (!HasNonSwitchEnumPreds) {
    // Check that all uses of IntArg are dominated by SelectInst
    bool SelectDominatesAllArgUses = true;

    for(auto U : IntArg->getUses()) {
      if (!DT->dominates(SelectInst->getParent(), U->getUser()->getParent())) {
        SelectDominatesAllArgUses = false;
        break;
      }
    }

    // If all uses of IntArg are dominated by SelectInst, it is safe
    // to replace IntArg by the result of SelectInst because
    // it is the only incoming value for the IntArg.
    if (SelectDominatesAllArgUses) {
      IntArg->replaceAllUsesWith(SelectInst);
    }
  }

  // Do not replace the bbarg
  SmallVector<SILValue, 4> Args;
  Args.push_back(SelectInst);
  B.setInsertionPoint(SelectInst->getNextNode());
  B.createBranch(SEI->getLoc(), BB, Args);
  // Remove switch_enum instruction
  SEI->getParent()->getTerminator()->eraseFromParent();

  return true;
}

/// Collected information for a select_value case or default case.
struct CaseInfo {
  /// The input value or null if it is the default case.
  IntegerLiteralInst *Literal = nullptr;
  
  /// The result value.
  SILInstruction *Result = nullptr;

  /// The block which conains the cond_br of the input value comparison
  /// or the block which assigns the default value.
  SILBasicBlock *CmpOrDefault = nullptr;
};

/// Get information about a potential select_value case (or default).
/// \p Input is set to the common input value.
/// \p Pred is the predecessor block of the last merge block of the CFG pattern.
/// \p ArgNum is the index of the argument passed to the merge block.
CaseInfo getCaseInfo(SILValue &Input, SILBasicBlock *Pred, unsigned ArgNum) {
  
  CaseInfo CaseInfo;
  
  auto *TI = Pred->getTerminator();
  if (!isa<BranchInst>(TI))
    return CaseInfo;
  
  // Find the Nth argument passed to BB.
  auto Arg = TI->getOperand(ArgNum);

  // Currently we only accept enums as result values.
  auto *EI2 = dyn_cast<EnumInst>(Arg);
  if (!EI2)
    return CaseInfo;
  
  if (EI2->hasOperand()) {
    // ... or enums with enum data. This is exactly the pattern for an enum
    // with integer raw value initialization.
    auto *EI1 = dyn_cast<EnumInst>(EI2->getOperand());
    if (!EI1)
      return CaseInfo;
    
    // But not enums with enums with data.
    if (EI1->hasOperand())
      return CaseInfo;
  }
  
  // Check if we come to the Pred block by comparing the input value to a
  // constant.
  SILBasicBlock *CmpBlock = Pred->getSinglePredecessor();
  if (!CmpBlock)
    return CaseInfo;
  
  auto *CmpInst = dyn_cast<CondBranchInst>(CmpBlock->getTerminator());
  if (!CmpInst)
    return CaseInfo;
  
  auto *CondInst = dyn_cast<BuiltinInst>(CmpInst->getCondition());
  if (!CondInst)
    return CaseInfo;
  
  if (!CondInst->getName().str().startswith("cmp_eq"))
    return CaseInfo;
  
  auto CondArgs = CondInst->getArguments();
  assert(CondArgs.size() == 2);
  
  SILValue Arg1 = CondArgs[0];
  SILValue Arg2 = CondArgs[1];
  
  if (isa<IntegerLiteralInst>(Arg1))
    std::swap(Arg1, Arg2);
  
  auto *CmpVal = dyn_cast<IntegerLiteralInst>(Arg2);
  if (!CmpVal)
    return CaseInfo;

  SILBasicBlock *FalseBB = CmpInst->getFalseBB();
  if (!FalseBB)
    return CaseInfo;

  // Check for a common input value.
  if (Input && Input != Arg1)
    return CaseInfo;
  
  Input = Arg1;
  CaseInfo.Result = EI2;
  if (CmpInst->getTrueBB() == Pred) {
    // This is a case for the select_value.
    CaseInfo.Literal = CmpVal;
    CaseInfo.CmpOrDefault = CmpBlock;
  } else {
    // This is the default for the select_value.
    CaseInfo.CmpOrDefault = Pred;
  }
  
  return CaseInfo;
}

/// Move an instruction which is an operand to the new SelectValueInst to its
/// correct place.
/// Either the instruction is somewhere inside the CFG pattern, then we move it
/// up, immediately before the SelectValueInst in the pattern's dominating
/// entry block. Or it is somewhere above the entry block, then we can leave the
/// instruction there.
void moveIfNotDominating(SILInstruction *I, SILInstruction *InsertPos,
                         DominanceInfo *DT) {
  SILBasicBlock *InstBlock = I->getParent();
  SILBasicBlock *InsertBlock = InsertPos->getParent();
  if (!DT->dominates(InstBlock, InsertBlock)) {
    assert(DT->dominates(InsertBlock, InstBlock));
    I->moveBefore(InsertPos);
  }
}

/// Simplify a pattern of integer compares to a select_value.
/// \code
///   if input == 1 {
///     result = Enum.A
///   } else if input == 2 {
///     result = Enum.B
///   ...
///   } else {
///     result = Enum.X
///   }
/// \endcode
/// Currently this only works if the input value is an integer and the result
/// value is an enum.
/// \p MergeBlock The "last" block which contains an argument in which all
///               result values are merged.
/// \p ArgNum The index of the block argument which is the result value.
/// \p DT The dominance info.
/// \return Returns true if a select_value is generated.
bool simplifyToSelectValue(SILBasicBlock *MergeBlock, unsigned ArgNum,
                           DominanceInfo *DT) {
  if (!DT)
    return false;
  
  // Collect all case infos from the merge block's predecessors.
  SmallPtrSet<SILBasicBlock *, 8> FoundCmpBlocks;
  SmallVector<CaseInfo, 8> CaseInfos;
  SILValue Input;
  for (auto *Pred : MergeBlock->getPreds()) {
    CaseInfo CaseInfo = getCaseInfo(Input, Pred, ArgNum);
    if (!CaseInfo.Result)
      return false;

    FoundCmpBlocks.insert(CaseInfo.CmpOrDefault);
    CaseInfos.push_back(CaseInfo);
  }
  
  SmallVector<std::pair<SILValue, SILValue>, 8> Cases;
  SILValue defaultResult;
  
  // The block of the first input value compare. It dominates all other blocks
  // in this CFG pattern.
  SILBasicBlock *dominatingBlock = nullptr;
  
  // Build the cases for the SelectValueInst and find the first dominatingBlock.
  for (auto &CaseInfo : CaseInfos) {
    if (CaseInfo.Literal) {
      auto *BrInst = cast<CondBranchInst>(CaseInfo.CmpOrDefault->getTerminator());
      if (FoundCmpBlocks.count(BrInst->getFalseBB()) != 1)
        return false;
      Cases.push_back({CaseInfo.Literal, CaseInfo.Result});
      SILBasicBlock *Pred = CaseInfo.CmpOrDefault->getSinglePredecessor();
      if (!Pred || FoundCmpBlocks.count(Pred) == 0) {
        // There may be only a single block whose predecessor we didn't see. And
        // this is the entry block to the CFG pattern.
        if (dominatingBlock)
          return false;
        dominatingBlock = CaseInfo.CmpOrDefault;
      }
    } else {
      if (defaultResult)
        return false;
      defaultResult = CaseInfo.Result;
    }
  }
  if (!defaultResult)
    return false;

  if (!dominatingBlock)
    return false;
  
  // Generate the select_value right before the first cond_br of the pattern.
  SILInstruction *insertPos = dominatingBlock->getTerminator();
  SILBuilder B(insertPos);
  
  // Move all needed operands to a place where they dominate the select_value.
  for (auto &CaseInfo : CaseInfos) {
    if (CaseInfo.Literal)
      moveIfNotDominating(CaseInfo.Literal, insertPos, DT);
    auto *EI2 = dyn_cast<EnumInst>(CaseInfo.Result);
    assert(EI2);
    
    if (EI2->hasOperand()) {
      auto *EI1 = dyn_cast<EnumInst>(EI2->getOperand());
      assert(EI1);
      assert(!EI1->hasOperand());

      moveIfNotDominating(EI1, insertPos, DT);
    }
    moveIfNotDominating(EI2, insertPos, DT);
  }
  
  SILArgument *bbArg = MergeBlock->getBBArg(ArgNum);
  auto SelectInst = B.createSelectValue(dominatingBlock->getTerminator()->getLoc(),
                                        Input, bbArg->getType(),
                                       defaultResult, Cases);

  bbArg->replaceAllUsesWith(SelectInst);
  
  return true;
}

// Attempt to simplify the ith argument of BB.  We simplify cases
// where there is a single use of the argument that is an extract from
// a struct or tuple and where the predecessors all build the struct
// or tuple and pass it directly.
bool SimplifyCFG::simplifyArgument(SILBasicBlock *BB, unsigned i) {
  auto *A = BB->getBBArg(i);

  // Try to create a select_value.
  if (simplifyToSelectValue(BB, i, DT))
    return true;
  
  // If we are reading an i1, then check to see if it comes from
  // a switch_enum.  If so, we may be able to lower this sequence to
  // a select_enum.
  if (A->getType().is<BuiltinIntegerType>())
    return simplifySwitchEnumToSelectEnum(BB, i, A, DT, PDT);

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
    BB->eraseBBArg(i);

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
  void run() override {
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

  void run() override {
    auto &Fn = *getFunction();

    // Split all critical egdes from all or non only cond_br terminators.
    bool Changed =
        splitAllCriticalEdges(Fn, OnlyNonCondBrEdges, nullptr, nullptr);

    if (Changed)
      invalidateAnalysis(SILAnalysis::InvalidationKind::CFG);
  }

  StringRef getName() override { return "Split Critical Edges"; }
};
  
  
class SimplifyBBArgs : public SILFunctionTransform {
public:
  SimplifyBBArgs() {}
  
  /// The entry point to the transformation.
  void run() override {
    if (SimplifyCFG(*getFunction(), PM).simplifyBlockArgs())
      invalidateAnalysis(SILAnalysis::InvalidationKind::CFG);
  }
  
  StringRef getName() override { return "Simplify Block Args"; }
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

// Simplifies basic block arguments.
SILTransform *swift::createSimplifyBBArgs() {
  return new SimplifyBBArgs();
}

