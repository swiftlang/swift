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
#include "swift/SILPasses/Transforms.h"
#include "swift/SILPasses/Utils/Local.h"
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
    bool dominatorBasedSimplify(DominanceInfo *DT);

    /// \brief Remove the basic block if it has no predecessors. Returns true
    /// If the block was removed.
    bool removeIfDead(SILBasicBlock *BB);
    
    bool tryJumpThreading(BranchInst *BI);
    bool simplifyAfterDroppingPredecessor(SILBasicBlock *BB);

    bool simplifyBranchOperands(OperandValueArrayRef Operands);
    bool simplifyBranchBlock(BranchInst *BI);
    bool simplifyCondBrBlock(CondBranchInst *BI);
    bool simplifySwitchEnumBlock(SwitchEnumInst *SEI);
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
  case ValueKind::SwitchIntInst:
  case ValueKind::SwitchEnumInst:
  case ValueKind::SwitchEnumAddrInst:
    return true;
  default:
    return false;
  }
}

// Get the unique enum element of a switch_enum_inst that transfers control
// to a given basic block. If multiple cases go to the block, or only
// the default case does, return nullptr;
static EnumElementDecl *getUniqueCaseElement(SwitchEnumInst *SEI,
                                             SILBasicBlock *BB) {
  EnumElementDecl* element = nullptr;
  for (unsigned i = 0, e = SEI->getNumCases(); i != e; ++i) {
    std::pair<EnumElementDecl *, SILBasicBlock *> enumCase;

    enumCase = SEI->getCase(i);
    if (enumCase.second != BB)
      continue;

    if (element)
      return nullptr;

    element = enumCase.first;
  }

  return element;
}

// Replace a SwitchEnumInst with an unconditional branch based on the
// assertion that it will select a particular element.
static void simplifySwitchEnumInst(SwitchEnumInst *SEI,
                                   EnumElementDecl *Element,
                                   SILBasicBlock *BB) {
  auto *Dest = SEI->getCaseDestination(Element);

  if (BB->bbarg_empty() || Dest->bbarg_empty()) {
    SILBuilder(SEI).createBranch(SEI->getLoc(), Dest);
  } else {
    assert(BB->bbarg_size() == 1 && "Expected only one argument!");
    ArrayRef<SILValue> Args = { BB->getBBArg(0) };
    SILBuilder(SEI).createBranch(SEI->getLoc(), Dest, Args);
  }
  SEI->eraseFromParent();
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

    auto *PredTerm = Pred->getTerminator();
    if (PredTerm->getKind() != Kind || PredTerm->getOperand(0) != Condition)
      continue;

    // Okay, DomBB dominates Term, has a single predecessor, and that
    // predecessor conditionally branches on the same condition. So we
    // know that DomBB are control-dependent on the edge that takes us
    // from Pred to DomBB. Since the terminator kind and condition are
    // the same, we can use the knowledge of which edge gets us to
    // Inst to optimize Inst.

    switch (Kind) {
    case ValueKind::SwitchEnumInst: {
      auto *SEI = cast<SwitchEnumInst>(PredTerm);
      auto *Element = getUniqueCaseElement(SEI, DomBB);
      if (Element) {
        simplifySwitchEnumInst(cast<SwitchEnumInst>(Term), Element, DomBB);
        return true;
      }

      // FIXME: We could also simplify things in some cases when we
      //        reach this switch_enum_inst from another
      //        switch_enum_inst that is branching on the same value
      //        and taking the default path.
      continue;
    }
    case ValueKind::CondBranchInst:
    case ValueKind::SwitchIntInst:
    case ValueKind::SwitchEnumAddrInst:
      // FIXME: Handle these.
      return false;
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

// Handle the mechanical aspects of removing an unreachable block.
static void removeDeadBlock(SILBasicBlock *BB) {
  // Instructions in the dead block may be used by other dead blocks.  Replace
  // any uses of them with undef values.
  while (!BB->empty()) {
    auto *Inst = &BB->getInstList().back();

    // Replace any non-dead results with SILUndef values.
    for (unsigned i = 0, e = Inst->getNumTypes(); i != e; ++i)
      if (!SILValue(Inst, i).use_empty())
        SILValue(Inst, i).replaceAllUsesWith(SILUndef::get(Inst->getType(i),
                                                           BB->getModule()));
    BB->getInstList().pop_back();
  }

  BB->eraseFromParent();
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
    if (isa<SwitchEnumInst>(User))
      return true;
  }
  return false;
}


namespace {
  class ThreadingCloner : public SILCloner<ThreadingCloner> {
    friend class SILVisitor<ThreadingCloner>;
    friend class SILCloner<ThreadingCloner>;

    SILBasicBlock *FromBB, *DestBB;
  public:

    ThreadingCloner(BranchInst *BI)
      : SILCloner(*BI->getFunction()),
        FromBB(BI->getDestBB()), DestBB(BI->getParent()) {
      // Populate the value map so that uses of the BBArgs in the DestBB are
      // replaced with the branch's values.
      for (unsigned i = 0, e = BI->getArgs().size(); i != e; ++i)
        ValueMap[FromBB->getBBArg(i)] = BI->getArg(i);
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
      SILCloner<ThreadingCloner>::postProcess(Orig, Cloned);
    }
  };
} // end anonymous namespace

/// tryJumpThreading - Check to see if it looks profitable to duplicate the
/// destination of an unconditional jump into the bottom of this block.
bool SimplifyCFG::tryJumpThreading(BranchInst *BI) {
  auto *DestBB = BI->getDestBB();

  // If the destination block ends with a return, we don't want to duplicate it.
  // We want to maintain the canonical form of a single return where possible.
  if (isa<ReturnInst>(DestBB->getTerminator()))
    return false;

  // This code is intentionally simple, and cannot thread if the BBArgs of the
  // destination are used outside the DestBB.
  for (auto Arg : DestBB->getBBArgs())
    if (isUsedOutsideOfBlock(Arg, DestBB))
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
    if (++Cost == 4) return false;

    // If there is an instruction in the block that has used outside the block,
    // duplicating it would require constructing SSA, which we're not prepared
    // to do.
    if (isUsedOutsideOfBlock(&Inst, DestBB)) return false;
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
    SILBuilder(BI).createBranch(BI->getLoc(), Br->getDestBB(), BI->getArgs());
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
  simplifyBranchOperands(BI->getTrueArgs());
  simplifyBranchOperands(BI->getFalseArgs());
  auto *ThisBB = BI->getParent();

  // If the condition is an integer literal, we can constant fold the branch.
  if (auto *IL = dyn_cast<IntegerLiteralInst>(BI->getCondition())) {
    bool isFalse = !IL->getValue();
    auto LiveArgs =  isFalse ? BI->getFalseArgs() : BI->getTrueArgs();
    auto *LiveBlock =  isFalse ? BI->getFalseBB() : BI->getTrueBB();
    auto *DeadBlock = !isFalse ? BI->getFalseBB() : BI->getTrueBB();
    auto *ThisBB = BI->getParent();

    SILBuilder(BI).createBranch(BI->getLoc(), LiveBlock, LiveArgs);
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
    SILBuilder(BI).createCondBranch(BI->getLoc(), BI->getCondition(),
                                    Br->getDestBB(), BI->getTrueArgs(),
                                    BI->getFalseBB(), BI->getFalseArgs());
    BI->eraseFromParent();
    removeIfDead(TrueSide);
    addToWorklist(ThisBB);
    return true;
  }

  if (isTrampolineBlock(FalseSide)) {
    BranchInst* Br = cast<BranchInst>(FalseSide->getTerminator());
    SILBuilder(BI).createCondBranch(BI->getLoc(), BI->getCondition(),
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
      SILBuilder(BI).createBranch(BI->getLoc(), TrueSide, TrueArgs);
      BI->eraseFromParent();
      addToWorklist(ThisBB);
      addToWorklist(TrueSide);
      ++NumConstantFolded;
      return true;
    }
  }
  return false;
}


/// simplifySwitchEnumBlock - Simplify a basic block that ends with a
/// switch_enum instruction that gets its operand from a an enum
/// instruction.
bool SimplifyCFG::simplifySwitchEnumBlock(SwitchEnumInst *SEI) {
  auto *EI = dyn_cast<EnumInst>(SEI->getOperand());
  if (!EI)
    return false;

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
    SILBuilder(SEI).createBranch(SEI->getLoc(), LiveBlock,
                                 EI->getOperand());
  else
    SILBuilder(SEI).createBranch(SEI->getLoc(), LiveBlock);
  SEI->eraseFromParent();
  if (EI->use_empty()) EI->eraseFromParent();
    
  addToWorklist(ThisBB);
    
  for (auto B : Dests)
    simplifyAfterDroppingPredecessor(B);
  addToWorklist(LiveBlock);
  ++NumConstantFolded;
  return true;
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
    if (auto *BI = dyn_cast<BranchInst>(TI))
      Changed |= simplifyBranchBlock(BI);
    else if (auto *CBI = dyn_cast<CondBranchInst>(TI))
      Changed |= simplifyCondBrBlock(CBI);
    else if (auto *SII = dyn_cast<SwitchIntInst>(TI))
      (void)SII;
    else if (auto *SEI = dyn_cast<SwitchEnumInst>(TI))
      Changed |= simplifySwitchEnumBlock(SEI);

    // Simplify the block argument list.
    Changed |= simplifyArgs(BB);
  }

  return Changed;
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
    PM->invalidateAnalysis(SILAnalysis::InvalidationKind::CFG);
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
    return true;
  }
  return Changed;
}

static void
removeArgumentFromTerminator(SILBasicBlock *BB, SILBasicBlock *Dest, int idx) {
  TermInst *Branch = BB->getTerminator();
  SILBuilder Builder(Branch);

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
    else
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

// Attempt to simplify the ith argument of BB.  We simplify cases
// where there is a single use of the argument that is an extract from
// a struct or tuple and where the predecessors all build the struct
// or tuple and pass it directly.
bool SimplifyCFG::simplifyArgument(SILBasicBlock *BB, unsigned i) {
  auto *A = BB->getBBArg(i);

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

    for (auto *Pred : BB->getPreds())
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
