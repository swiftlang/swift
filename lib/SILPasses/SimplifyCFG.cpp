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
//                             CFG Simplification
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
      auto *DA = PM->getAnalysis<DominanceAnalysis>();
      auto *PDA = PM->getAnalysis<PostDominanceAnalysis>();

      DT = DA->get(&Fn);
      PDT = PDA->get(&Fn);
      bool Changed = false;
      for (SILBasicBlock &BB : Fn) {
        Changed |= simplifyArgs(&BB);
      }
      DT = nullptr;
      PDT = nullptr;
      return Changed;
    }

  private:
    void clearWorklist() {
      WorklistMap.clear();
      WorklistList.clear();
    }

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
    bool canonicalizeSwitchEnums();
    bool dominatorBasedSimplify(DominanceInfo *DT);

    /// \brief Remove the basic block if it has no predecessors. Returns true
    /// If the block was removed.
    bool removeIfDead(SILBasicBlock *BB);

    bool tryJumpThreading(BranchInst *BI);
    bool tailDuplicateObjCMethodCallSuccessorBlocks();
    bool simplifyAfterDroppingPredecessor(SILBasicBlock *BB);

    bool simplifyBranchOperands(OperandValueArrayRef Operands);
    bool simplifyBranchBlock(BranchInst *BI);
    bool simplifyCondBrBlock(CondBranchInst *BI);
    bool simplifyCheckedCastBranchBlock(CheckedCastBranchInst *CCBI);
    bool simplifyCheckedCastAddrBranchBlock(CheckedCastAddrBranchInst *CCABI);
    bool simplifyTermWithIdenticalDestBlocks(SILBasicBlock *BB);
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

/// Helper function to perform SSA updates in case of jump threading.
void swift::updateSSAAfterCloning(BaseThreadingCloner &Cloner,
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

static SILValue getUnderlyingCondition(SILValue V) {
  if (auto *SEI = dyn_cast<SelectEnumInst>(V))
    V = SEI->getEnumOperand().stripCasts();

  if (auto *BI = dyn_cast<BuiltinInst>(V)) {
    // Expect-intrinsics have no effect, so we can see through them.
    if (BI->getIntrinsicInfo().ID == llvm::Intrinsic::expect)
      return BI->getArguments()[0];
  }
  return V;
}

/// Returns true if C1, C2 represent equivalent conditions in the
/// sense that each is eventually based on the same value.
bool swift::areEquivalentConditions(SILValue C1, SILValue C2) {
  return getUnderlyingCondition(C1) == getUnderlyingCondition(C2);
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

/// Perform a dominator-based jump-threading for checked_cast_br [exact]
/// instructions if they use the same condition (modulo upcasts and downcasts).
/// This is very beneficial for code that:
///  - references the same object multiple times (e.g. x.f1() + x.f2())
///  - and for method invocation chaining (e.g. x.f3().f4().f5())
bool
SimplifyCFG::trySimplifyCheckedCastBr(TermInst *Term, DominanceInfo *DT) {

  SmallVector<SILBasicBlock *, 16> BBs;
  auto Result = tryCheckedCastBrJumpThreading(Term, DT, BBs);

  if (Result) {
    for (auto BB: BBs)
      addToWorklist(BB);
  }

  return Result;
}

/// Holds information about a use of a terminator condition.
struct CondUseInfo {
  CondUseInfo(Operand *Use, size_t SuccessorIdx, bool inverted) :
  Use(Use), SuccessorIdx(SuccessorIdx), inverted(inverted) {}
  
  /// The use.
  Operand *Use;
  
  /// Specifies the successor of the terminator which dominates the use.
  size_t SuccessorIdx;
  
  /// For boolean conditions: true if the use is an inverted condition of the
  /// original.
  bool inverted;
};

/// Returns the underlying condition of a value.
/// It strips expect-intrinsics and boolean xor-invertions.
static SILValue getUnderlying(SILValue V, bool &inverted) {
  if (auto *BI = dyn_cast<BuiltinInst>(V)) {
    // Expect-intrinsics have no effect, so we can see through them.
    if (BI->getIntrinsicInfo().ID == llvm::Intrinsic::expect)
      return BI->getArguments()[0];
    if (BI->getBuiltinInfo().ID == BuiltinValueKind::Xor) {
      // Check if it's a boolean invertion of the condition.
      OperandValueArrayRef Args = BI->getArguments();
      if (auto *IL = dyn_cast<IntegerLiteralInst>(Args[1])) {
        if (IL->getValue().isAllOnesValue()) {
          inverted = !inverted;
          return Args[0];
        }
      }
    }
  }
  return SILValue();
}

/// Collects uses down the dominator tree.
/// We collect only uses which are dominated by a successor of the original
/// terminator instruction.
static void collectUsesDown(SmallVectorImpl<CondUseInfo> &Uses, SILValue Cond,
                            bool inverted,
                            const TermInst::SuccessorListTy &Successors,
                            DominanceInfo *DT) {
  for (Operand *UI : Cond.getUses()) {
    SILInstruction *User = UI->getUser();
    SILBasicBlock *UserBlock = User->getParentBB();
    // Check which successor (if any) dominates the use.
    for (size_t SuccIdx = 0, Size = Successors.size(); SuccIdx < Size;
         ++SuccIdx) {
      SILBasicBlock *SuccBB = Successors[SuccIdx].getBB();
      if (DT->dominates(SuccBB, UserBlock) && SuccBB->getSinglePredecessor()) {
        
        // The successor dominates the use. So we collect the use.
        Uses.push_back({ UI, SuccIdx, inverted });
        bool newInverted = inverted;
        if (getUnderlying(User, newInverted) == Cond) {
          // Continue to collect uses of this User.
          collectUsesDown(Uses, User, newInverted, Successors, DT);
        }
        break;
      }
    }
  }
}

/// Collects uses up the dominator tree.
static void collectUsesUp(SmallVectorImpl<CondUseInfo> &Uses, SILValue Cond,
                          bool inverted,
                          const TermInst::SuccessorListTy &Successors,
                          DominanceInfo *DT) {
  do {
    collectUsesDown(Uses, Cond, inverted, Successors, DT);
    Cond = getUnderlying(Cond, inverted);
  } while (Cond);
}

/// Replaces uses of a cond_br condition in blocks which are dominated by the
/// cond_br's successors. Such uses can be replaced with literals.
static bool propagateCondBrCondition(CondBranchInst *CBR, DominanceInfo *DT) {
  IntegerLiteralInst *LiteralInsts[2] = { nullptr, nullptr };
  auto Successors = CBR->getSuccessors();
  assert(Successors[0].getBB() == CBR->getTrueBB());
  assert(Successors[1].getBB() == CBR->getFalseBB());
  SmallVector<CondUseInfo, 8> Uses;
  SILValue Cond = CBR->getCondition();
  
  // Collect all uses of the cond_br condition which are dominated by either the
  // true- or false-block.
  collectUsesUp(Uses, Cond, false, Successors, DT);

  bool Changed = false;
  for (const CondUseInfo &CUI : Uses) {
    size_t Idx = CUI.SuccessorIdx;
    int Literal = CUI.inverted ? Idx : 1 - Idx;
    assert(Literal < 2);
    if (!LiteralInsts[Literal]) {
      // Create a 0- or 1- literal in the cond_br's block.
      SILInstruction *User = CUI.Use->getUser();
      LiteralInsts[Literal] = SILBuilderWithScope<1>(CBR).
        createIntegerLiteral(User->getLoc(), Cond.getType(), Literal);
    }
    // Replace the use of the cond_br condition with a literal.
    CUI.Use->set(LiteralInsts[Literal]);
    Changed = true;
  }
  return Changed;
}

/// Optimizes uses of a switch_enum condition in blocks which are dominated by
/// the switch_enum's successors.
static bool propagateSwitchEnumCondition(SwitchEnumInst *SWI, DominanceInfo *DT) {
  auto Successors = SWI->getSuccessors();
  SmallVector<CondUseInfo, 8> Uses;
  SILValue EnumVal = SWI->getOperand();
  collectUsesUp(Uses, EnumVal, false, Successors, DT);
  
  bool Changed = false;
  for (const CondUseInfo &CUI : Uses) {
    size_t Idx = CUI.SuccessorIdx;
    SILBasicBlock *DstBlock = Successors[Idx].getBB();
    auto Case = SWI->getUniqueCaseForDestination(DstBlock);
    if (!Case)
      continue;
    EnumElementDecl *EnumElement = Case.get();
    
    SILInstruction *User = CUI.Use->getUser();
    if (SwitchEnumInst *DomSWI = dyn_cast<SwitchEnumInst>(User)) {
      
      // The SWI successor dominates another switch_enum which share the same
      // condition. We know the case so we can just instantantiate the
      // enum_inst and make the dominated switch_enum use it.
      SILBuilderWithScope<1> Builder(DomSWI);

      SILValue NewEnum;
      // Do we have a payload.
      auto EnumTy = EnumVal->getType(0);
      if (EnumElement->hasArgumentType()) {
        auto Ty = EnumTy.getEnumElementType(EnumElement, DomSWI->getModule());
        SILValue UED(Builder.createUncheckedEnumData(DomSWI->getLoc(), EnumVal,
                                                     EnumElement, Ty),
                     0);

        NewEnum = Builder.createEnum(DomSWI->getLoc(), UED, EnumElement, EnumTy);
      } else
        NewEnum = Builder.createEnum(DomSWI->getLoc(), SILValue(), EnumElement,
                                     EnumTy);

      DomSWI->setOperand(0, NewEnum);
      Changed = true;
      continue;
    }
    if (SelectEnumInst *DomSEI = dyn_cast<SelectEnumInst>(User)) {

      // The SWI successor dominates a select_enum which share the same
      // condition.
      SILValue selected;
      for (unsigned i = 0, e = DomSEI->getNumCases(); i < e; ++i) {
        auto casePair = DomSEI->getCase(i);
        if (casePair.first == EnumElement) {
          selected = casePair.second;
          break;
        }
      }
      if (!selected)
        selected = DomSEI->getDefaultResult();

      DomSEI->replaceAllUsesWith(selected.getDef());
      
      Changed = true;
      continue;
    }
  }
  return Changed;
}

// Simplifications that walk the dominator tree to prove redundancy in
// conditional branching.
bool SimplifyCFG::dominatorBasedSimplify(DominanceInfo *DT) {
  bool Changed = false;
  for (auto &BB : Fn) {
    // Any method called from this loop should update
    // the DT if it changes anything related to dominators.
    TermInst *Term = BB.getTerminator();
    switch (Term->getKind()) {
    case ValueKind::CondBranchInst:
      Changed |= propagateCondBrCondition(cast<CondBranchInst>(Term), DT);
      break;
    case ValueKind::SwitchEnumInst:
      Changed |= propagateSwitchEnumCondition(cast<SwitchEnumInst>(Term), DT);
      break;
    case ValueKind::SwitchValueInst:
      // TODO: handle switch_value
      break;
    case ValueKind::CheckedCastBranchInst:
      Changed |= trySimplifyCheckedCastBr(BB.getTerminator(), DT);
      break;
    default:
      break;
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
  for (auto &S : BB->getSuccessors())
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
  // If the value being substituted is an enum, check to see if there are any
  // switches on it.
  auto *EI = dyn_cast<EnumInst>(Val);
  if (!EI)
    return false;

  for (auto UI : BBArg->getUses()) {
    auto *User = UI->getUser();
    // We only know we can simplify if the switch_enum user is in the block we
    // are trying to jump thread.
    // The value must not be define in the same basic block as the switch enum
    // user. If this is the case we have a single block switch_enum loop.
    if (isa<SwitchEnumInst>(User) || isa<SelectEnumInst>(User))
      if (BBArg->getParent() == User->getParent() &&
          EI->getParent() != BBArg->getParent())
        return true;

    // Also allow enum of enum, which usually can be combined to a single
    // instruction. This helps to simplify the creation of an enum from an
    // integer raw value.
    if (isa<EnumInst>(User))
      if (BBArg->getParent() == User->getParent() &&
          EI->getParent() != BBArg->getParent())
        return true;
  }
  return false;
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

  // We need to update SSA if a value duplicated is used outside of the
  // duplicated block.
  bool NeedToUpdateSSA = false;

  // Are the arguments to this block used outside of the block.
  for (auto Arg : DestBB->getBBArgs())
    if ((NeedToUpdateSSA |= isUsedOutsideOfBlock(Arg, DestBB))) {
      break;
    }

  // We don't have a great cost model at the SIL level, so we don't want to
  // blissly duplicate tons of code with a goal of improved performance (we'll
  // leave that to LLVM).  However, doing limited code duplication can lead to
  // major second order simplifications.  Here we only do it if there are
  // "constant" arguments to the branch or if we know how to fold something
  // given the duplication.
  bool WantToThread = false;

  if (isa<CondBranchInst>(DestBB->getTerminator()))
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
    if (!Inst.isTriviallyDuplicatable())
      return false;

    // Don't jumpthread function calls.
    if (isa<ApplyInst>(Inst))
      return false;

    // This is a really trivial cost model, which is only intended as a starting
    // point.
    if (instructionInlineCost(Inst) != InlineCost::Free)
      if (++Cost == 4) return false;

    // We need to update ssa if a value is used outside the duplicated block.
    if (!NeedToUpdateSSA)
      NeedToUpdateSSA |= isUsedOutsideOfBlock(&Inst, DestBB);
  }

  // Don't jump thread through a potential header - this can produce irreducible
  // control flow.
  if (!isa<SwitchEnumInst>(DestBB->getTerminator()) &&
      LoopHeaders.count(DestBB))
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

  if (NeedToUpdateSSA)
    updateSSAAfterCloning(Cloner, SrcBB, DestBB);

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
    for (auto &Succ : BB->getSuccessors())
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

/// \brief Returns the first cond_fail if it is the first side-effect
/// instruction in this block.
static CondFailInst *getFistCondFail(SILBasicBlock *BB) {
  auto It = BB->begin();
  CondFailInst *CondFail = nullptr;
  // Skip instructions that don't have side-effects.
  while (It != BB->end() && !(CondFail = dyn_cast<CondFailInst>(It))) {
    if (It->mayHaveSideEffects())
      return nullptr;
    ++It;
  }
  return CondFail;
}

/// \brief Is the first side-effect instruction in this block a cond_fail that
/// is guarantueed to fail.
static bool isCondFailBlock(SILBasicBlock *BB,
                            CondFailInst *&OrigCondFailInst) {
  CondFailInst *CondFail = getFistCondFail(BB);
  if (!CondFail)
    return false;
  auto *IL = dyn_cast<IntegerLiteralInst>(CondFail->getOperand());
  if (!IL)
    return false;
  OrigCondFailInst = CondFail;
  return IL->getValue() != 0;
}

/// \brief Creates a new cond_fail instruction, optionally with an xor inverted
/// condition.
static void createCondFail(CondFailInst *Orig, SILValue Cond, bool inverted,
                           SILBuilder &Builder) {
  if (inverted) {
    auto *True = Builder.createIntegerLiteral(Orig->getLoc(), Cond.getType(), 1);
    Cond = Builder.createBuiltinBinaryFunction(Orig->getLoc(), "xor",
                                               Cond.getType(), Cond.getType(),
                                               {Cond, True});
  }
  Builder.createCondFail(Orig->getLoc(), Cond);
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
    SILBuilderWithScope<1> Builder(BI);
    createCondFail(OrigCFI, CFCondition, !IsTrueSideFailing, Builder);
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
  auto SuccessBB = CCBI->getSuccessBB();
  auto FailureBB = CCBI->getFailureBB();
  auto ThisBB = CCBI->getParent();

  CastOptimizer CastOpt(
      [](SILInstruction *I, ValueBase *V){} /* ReplaceInstUsesAction */,
      [](SILInstruction *I) { /* EraseInstAction */
        I->eraseFromParent();
      },
      [&]() { /* WillSucceedAction */
        removeIfDead(FailureBB);
        addToWorklist(ThisBB);
      },
      [&]() { /* WillFailAction */
        removeIfDead(SuccessBB);
        addToWorklist(ThisBB);
      });

  return CastOpt.simplifyCheckedCastBranchInst(CCBI) != nullptr;
}

bool
SimplifyCFG::
simplifyCheckedCastAddrBranchBlock(CheckedCastAddrBranchInst *CCABI) {
  auto SuccessBB = CCABI->getSuccessBB();
  auto FailureBB = CCABI->getFailureBB();
  auto ThisBB = CCABI->getParent();

  CastOptimizer CastOpt(
      [](SILInstruction *I, ValueBase *V){} /* ReplaceInstUsesAction */,
      [](SILInstruction *I) { /* EraseInstAction */
        I->eraseFromParent();
      },
      [&]() { /* WillSucceedAction */
        removeIfDead(FailureBB);
        addToWorklist(ThisBB);
      },
      [&]() { /* WillFailAction */
        removeIfDead(SuccessBB);
        addToWorklist(ThisBB);
      });

  return CastOpt.simplifyCheckedCastAddrBranchInst(CCABI) != nullptr;
}

// Replace the terminator of BB with a simple branch if all successors go
// to trampoline jumps to the same destination block. The successor blocks
// and the destination blocks may have no arguments.
bool SimplifyCFG::simplifyTermWithIdenticalDestBlocks(SILBasicBlock *BB) {
  SILBasicBlock *commonDest = nullptr;
  for (const SILSuccessor &Succ : BB->getSuccessors()) {
    SILBasicBlock *SuccBlock = Succ.getBB();
    if (SuccBlock->getNumBBArg() != 0)
      return false;
    SILBasicBlock *DestBlock = getTrampolineDest(SuccBlock);
    if (!DestBlock)
      return false;
    if (!commonDest) {
      commonDest = DestBlock;
    } else if (DestBlock != commonDest) {
      return false;
    }
  }
  if (!commonDest)
    return false;
  
  assert(commonDest->getNumBBArg() == 0 &&
         "getTrampolineDest should have checked that commonDest has no args");
  
  TermInst *Term = BB->getTerminator();
  SILBuilderWithScope<1>(Term).createBranch(Term->getLoc(), commonDest, {});
  Term->eraseFromParent();
  addToWorklist(BB);
  addToWorklist(commonDest);
  return true;
}

void RemoveUnreachable::visit(SILBasicBlock *BB) {
  if (!Visited.insert(BB).second)
    return;

  for (auto &Succ : BB->getSuccessors())
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

/// Checks if the block contains a cond_fail as first side-effect instruction
/// and trys to move it to the predecessors (if benefitial). A sequence
///
///     bb1:
///       br bb3(%c)
///     bb2:
///       %i = integer_literal
///       br bb3(%i)            // at least one input argument must be constant
///     bb3(%a) // = BB
///       cond_fail %a          // %a must not have other uses
///
/// is replaced with
///
///     bb1:
///       cond_fail %c
///       br bb3(%c)
///     bb2:
///       %i = integer_literal
///       cond_fail %i
///       br bb3(%i)
///     bb3(%a)                 // %a is dead
///
static bool tryMoveCondFailToPreds(SILBasicBlock *BB) {
  
  CondFailInst *CFI = getFistCondFail(BB);
  if (!CFI)
    return false;
  
  // Find the underlying condition value of the cond_fail.
  SILValue cond = CFI->getOperand();
  bool inverted = false;
  while (auto *BI = dyn_cast<BuiltinInst>(cond)) {
    
    // This is not a correctness check, but we only want to to the optimization
    // if the condition gets dead after moving the cond_fail.
    if (!BI->hasOneUse())
      return false;
    
    OperandValueArrayRef Args = BI->getArguments();
    
    if (BI->getBuiltinInfo().ID == BuiltinValueKind::Xor) {
      // Check if it's a boolean invertion of the condition.
      if (auto *IL = dyn_cast<IntegerLiteralInst>(Args[1])) {
        if (IL->getValue().isAllOnesValue()) {
          cond = Args[0];
          inverted = !inverted;
          continue;
        }
      } else if (auto *IL = dyn_cast<IntegerLiteralInst>(Args[0])) {
        if (IL->getValue().isAllOnesValue()) {
          cond = Args[1];
          inverted = !inverted;
          continue;
        }
      }
    }
    break;
  }
  // Check if the condition is a single-used argument in the current block.
  SILArgument *condArg = dyn_cast<SILArgument>(cond);
  if (!condArg || !condArg->hasOneUse())
    return false;
  
  if (condArg->getParent() != BB)
    return false;
  
  // Check if some of the predecessor blocks provide a constant for the
  // cond_fail condition. So that the optimization has a positive effect.
  bool somePredsAreConst = false;
  for (auto *Pred : BB->getPreds()) {
    
    // The cond_fail must post-dominate the predecessor block. We may not
    // execute the cond_fail speculatively.
    if (!Pred->getSingleSuccessor())
      return false;
    
    SILValue incoming = condArg->getIncomingValue(Pred);
    if (isa<IntegerLiteralInst>(incoming)) {
      somePredsAreConst = true;
      break;
    }
  }
  if (!somePredsAreConst)
    return false;
  
  DEBUG(llvm::dbgs() << "### move to predecessors: " << *CFI);
  
  // Move the cond_fail to the predecessor blocks.
  for (auto *Pred : BB->getPreds()) {
    SILValue incoming = condArg->getIncomingValue(Pred);
    SILBuilderWithScope<4> Builder(Pred->getTerminator());
    
    createCondFail(CFI, incoming, inverted, Builder);
  }
  CFI->eraseFromParent();
  return true;
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
      Changed |= simplifyTermWithIdenticalDestBlocks(BB);
      break;
    case ValueKind::SwitchEnumInst:
      Changed |= simplifySwitchEnumBlock(cast<SwitchEnumInst>(TI));
      Changed |= simplifyTermWithIdenticalDestBlocks(BB);
      break;
    case ValueKind::UnreachableInst:
      Changed |= simplifyUnreachableBlock(cast<UnreachableInst>(TI));
      break;
    case ValueKind::CheckedCastBranchInst:
      Changed |= simplifyCheckedCastBranchBlock(cast<CheckedCastBranchInst>(TI));
      break;
    case ValueKind::CheckedCastAddrBranchInst:
      Changed |= simplifyCheckedCastAddrBranchBlock(cast<CheckedCastAddrBranchInst>(TI));
      break;
    case ValueKind::SwitchEnumAddrInst:
      Changed |= simplifyTermWithIdenticalDestBlocks(BB);
      break;
    default:
      break;
    }
    // If the block has a cond_fail, try to move it to the predecessors.
    Changed |= tryMoveCondFailToPreds(BB);

    // Simplify the block argument list.
    Changed |= simplifyArgs(BB);
  }

  return Changed;
}

/// Canonicalize all switch_enum and switch_enum_addr instructions.
/// If possible, replace the default with the corresponding unique case.
bool SimplifyCFG::canonicalizeSwitchEnums() {
  bool Changed = false;
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
    Changed = true;
  }

  return Changed;
}

static SILBasicBlock *isObjCMethodCallBlock(SILBasicBlock &Block) {
  auto *Branch = dyn_cast<BranchInst>(Block.getTerminator());
  if (!Branch)
    return nullptr;

  for (auto &Inst : Block) {
    // Look for a objc method call.
    auto *Apply = dyn_cast<ApplyInst>(&Inst);
    if (!Apply)
      continue;
    auto *Callee = dyn_cast<WitnessMethodInst>(Apply->getCallee());
    if (!Callee || !Callee->getMember().isForeign)
      continue;

    return Branch->getDestBB();
  }
  return nullptr;
}

/// We want to duplicate small blocks that contain a least on release and have
/// multiple predecessor.
static bool shouldTailDuplicate(SILBasicBlock &Block) {
  unsigned Cost = 0;
  bool SawRelease = false;

  if (isa<ReturnInst>(Block.getTerminator()))
    return false;

  if (Block.getSinglePredecessor())
    return false;

  for (auto &Inst : Block) {
    if (!Inst.isTriviallyDuplicatable())
      return false;

    if (isa<ApplyInst>(&Inst))
      return false;

    if (isa<ReleaseValueInst>(&Inst) ||
        isa<StrongReleaseInst>(&Inst))
      SawRelease = true;

    if (instructionInlineCost(Inst) != InlineCost::Free)
      if (++Cost == 12)
        return false;
  }

  return SawRelease;
}


/// Tail duplicate successor blocks of blocks that perform an objc method call
/// and who contain releases. Cloning such blocks can allow ARC to sink retain
/// releases onto the ObjC path.
bool SimplifyCFG::tailDuplicateObjCMethodCallSuccessorBlocks() {
  SmallVector<SILBasicBlock *, 16> ObjCBlocks;

  // Collect blocks to tail duplicate.
  for (auto &BB : Fn) {
    SILBasicBlock *DestBB;
    if ((DestBB = isObjCMethodCallBlock(BB)) && !LoopHeaders.count(DestBB) &&
        shouldTailDuplicate(*DestBB))
      ObjCBlocks.push_back(&BB);
  }

  bool Changed = false;
  for (auto *BB : ObjCBlocks) {
    auto *Branch = cast<BranchInst>(BB->getTerminator());
    auto *DestBB = Branch->getDestBB();
    Changed = true;

    // Okay, it looks like we want to do this and we can.  Duplicate the
    // destination block into this one, rewriting uses of the BBArgs to use the
    // branch arguments as we go.
    ThreadingCloner Cloner(Branch);

    for (auto &I : *DestBB)
      Cloner.process(&I);

    Branch->eraseFromParent();

    updateSSAAfterCloning(Cloner, BB, DestBB);
  }

  return Changed;
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

    Changed = true;
  }

  // Do simplifications that require the dominator tree to be accurate.
  DominanceAnalysis *DA = PM->getAnalysis<DominanceAnalysis>();
  PostDominanceAnalysis *PDA = PM->getAnalysis<PostDominanceAnalysis>();


  if (Changed) {
    // Force dominator recomputation since we modifed the cfg.
    DA->invalidate(&Fn, SILAnalysis::PreserveKind::Nothing);
    PDA->invalidate(&Fn, SILAnalysis::PreserveKind::Nothing);
  }

  DT = DA->get(&Fn);
  PDT = PDA->get(&Fn);
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

  if (tailDuplicateObjCMethodCallSuccessorBlocks()) {
    Changed = true;
    if (simplifyBlocks())
      RU.run();
  }

  // Split all critical edges from non cond_br terminators.
  Changed |= splitAllCriticalEdges(Fn, true, nullptr, nullptr);

  // Canonicalize switch_enum instructions.
  Changed |= canonicalizeSwitchEnums();
  
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
  return Tuple->getElement(TEI->getFieldNo());
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
  auto &Fn = *BB->getParent();

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

  // We have modified the CFG recompute the (post)dominators.
  PDT->recalculate(Fn);
  DT->recalculate(Fn);

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

static void tryToReplaceArgWithIncomingValue(SILBasicBlock *BB, unsigned i,
                                             DominanceInfo *DT) {
  auto *A = BB->getBBArg(i);
  SmallVector<SILValue, 4> Incoming;
  if (!A->getIncomingValues(Incoming) || Incoming.empty())
    return;
  
  SILValue V = Incoming[0];
  for (size_t Idx = 1, Size = Incoming.size(); Idx < Size; ++Idx) {
    if (Incoming[Idx] != V)
      return;
  }
  
  // If the incoming values of all predecessors are equal usually this means
  // that the common incoming value dominates the BB. But: this might be not
  // the case if BB is unreachable. Therefore we still have to check it.
  if (!DT->dominates(V.getDef()->getParentBB(), BB))
    return;
  A->replaceAllUsesWith(V.getDef());
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

    // Replace a block argument if all incoming values are equal. If this
    // succeeds, argument A will have no uses afterwards.
    if (DT)
      tryToReplaceArgWithIncomingValue(BB, i, DT);
    
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
      invalidateAnalysis(SILAnalysis::PreserveKind::Nothing);
  }

  StringRef getName() override { return "Simplify CFG"; }
};
} // end anonymous namespace


SILTransform *swift::createSimplifyCFG() {
  return new SimplifyCFGPass();
}

//===----------------------------------------------------------------------===//
//                          Passes only for Testing
//===----------------------------------------------------------------------===//

namespace {

// Used to test critical edge splitting with sil-opt.
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
      invalidateAnalysis(SILAnalysis::PreserveKind::Calls);
  }

  StringRef getName() override { return "Split Critical Edges"; }
};

// Used to test SimplifyCFG::simplifyArgs with sil-opt.
class SimplifyBBArgs : public SILFunctionTransform {
public:
  SimplifyBBArgs() {}
  
  /// The entry point to the transformation.
  void run() override {
    if (SimplifyCFG(*getFunction(), PM).simplifyBlockArgs())
      invalidateAnalysis(SILAnalysis::PreserveKind::Calls);
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

