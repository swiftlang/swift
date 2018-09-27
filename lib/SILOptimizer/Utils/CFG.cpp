//===--- CFG.cpp - Utilities for SIL CFG transformations ------------------===//
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

#include "swift/SIL/BasicBlockUtils.h"
#include "swift/SIL/Dominance.h"
#include "swift/SIL/LoopInfo.h"
#include "swift/SIL/BasicBlockUtils.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SILOptimizer/Utils/CFG.h"
#include "swift/SILOptimizer/Utils/Local.h"

using namespace swift;

/// \brief Adds a new argument to an edge between a branch and a destination
/// block.
///
/// \param Branch The terminator to add the argument to.
/// \param Dest The destination block of the edge.
/// \param Val The value to the arguments of the branch.
/// \return The created branch. The old branch is deleted.
/// The argument is appended at the end of the argument tuple.
TermInst *swift::addNewEdgeValueToBranch(TermInst *Branch, SILBasicBlock *Dest,
                                         SILValue Val) {
  SILBuilderWithScope Builder(Branch);
  TermInst *NewBr = nullptr;

  if (auto *CBI = dyn_cast<CondBranchInst>(Branch)) {
    SmallVector<SILValue, 8> TrueArgs;
    SmallVector<SILValue, 8> FalseArgs;

    for (auto A : CBI->getTrueArgs())
      TrueArgs.push_back(A);

    for (auto A : CBI->getFalseArgs())
      FalseArgs.push_back(A);

    if (Dest == CBI->getTrueBB()) {
      TrueArgs.push_back(Val);
      assert(TrueArgs.size() == Dest->getNumArguments());
    }
    if (Dest == CBI->getFalseBB()) {
      FalseArgs.push_back(Val);
      assert(FalseArgs.size() == Dest->getNumArguments());
    }

    NewBr = Builder.createCondBranch(
        CBI->getLoc(), CBI->getCondition(), CBI->getTrueBB(), TrueArgs,
        CBI->getFalseBB(), FalseArgs, CBI->getTrueBBCount(),
        CBI->getFalseBBCount());
  } else if (auto *BI = dyn_cast<BranchInst>(Branch)) {
    SmallVector<SILValue, 8> Args;

    for (auto A : BI->getArgs())
      Args.push_back(A);

    Args.push_back(Val);
    assert(Args.size() == Dest->getNumArguments());
    NewBr = Builder.createBranch(BI->getLoc(), BI->getDestBB(), Args);
  } else {
    // At the moment we can only add arguments to br and cond_br.
    llvm_unreachable("Can't add argument to terminator");
  }

  Branch->dropAllReferences();
  Branch->eraseFromParent();

  return NewBr;
}

/// \brief Changes the edge value between a branch and destination basic block
/// at the specified index. Changes all edges from \p Branch to \p Dest to carry
/// the value.
///
/// \param Branch The branch to modify.
/// \param Dest The destination of the edge.
/// \param Idx The index of the argument to modify.
/// \param Val The new value to use.
/// \return The new branch. Deletes the old one.
/// Changes the edge value between a branch and destination basic block at the
/// specified index.
TermInst *swift::changeEdgeValue(TermInst *Branch, SILBasicBlock *Dest,
                                 size_t Idx, SILValue Val) {
  SILBuilderWithScope Builder(Branch);

  if (auto *CBI = dyn_cast<CondBranchInst>(Branch)) {
    SmallVector<SILValue, 8> TrueArgs;
    SmallVector<SILValue, 8> FalseArgs;

    OperandValueArrayRef OldTrueArgs = CBI->getTrueArgs();
    bool BranchOnTrue = CBI->getTrueBB() == Dest;
    assert((!BranchOnTrue || Idx < OldTrueArgs.size()) && "Not enough edges");

    // Copy the edge values overwriting the edge at Idx.
    for (unsigned i = 0, e = OldTrueArgs.size(); i != e; ++i) {
      if (BranchOnTrue && Idx == i)
        TrueArgs.push_back(Val);
      else
        TrueArgs.push_back(OldTrueArgs[i]);
    }
    assert(TrueArgs.size() == CBI->getTrueBB()->getNumArguments() &&
           "Destination block's number of arguments must match");

    OperandValueArrayRef OldFalseArgs = CBI->getFalseArgs();
    bool BranchOnFalse = CBI->getFalseBB() == Dest;
    assert((!BranchOnFalse || Idx < OldFalseArgs.size()) && "Not enough edges");

    // Copy the edge values overwriting the edge at Idx.
    for (unsigned i = 0, e = OldFalseArgs.size(); i != e; ++i) {
      if (BranchOnFalse && Idx == i)
        FalseArgs.push_back(Val);
      else
        FalseArgs.push_back(OldFalseArgs[i]);
    }
    assert(FalseArgs.size() == CBI->getFalseBB()->getNumArguments() &&
           "Destination block's number of arguments must match");

    CBI = Builder.createCondBranch(
        CBI->getLoc(), CBI->getCondition(), CBI->getTrueBB(), TrueArgs,
        CBI->getFalseBB(), FalseArgs, CBI->getTrueBBCount(),
        CBI->getFalseBBCount());
    Branch->dropAllReferences();
    Branch->eraseFromParent();
    return CBI;
  }

  if (auto *BI = dyn_cast<BranchInst>(Branch)) {
    SmallVector<SILValue, 8> Args;

    assert(Idx < BI->getNumArgs() && "Not enough edges");
    OperandValueArrayRef OldArgs = BI->getArgs();

    // Copy the edge values overwriting the edge at Idx.
    for (unsigned i = 0, e = OldArgs.size(); i != e; ++i) {
      if (Idx == i)
        Args.push_back(Val);
      else
        Args.push_back(OldArgs[i]);
    }
    assert(Args.size() == Dest->getNumArguments());

    BI = Builder.createBranch(BI->getLoc(), BI->getDestBB(), Args);
    Branch->dropAllReferences();
    Branch->eraseFromParent();
    return BI;
  }

  llvm_unreachable("Unhandled terminator leading to merge block");
}

template <class SwitchEnumTy, class SwitchEnumCaseTy>
SILBasicBlock *replaceSwitchDest(SwitchEnumTy *S,
                                     SmallVectorImpl<SwitchEnumCaseTy> &Cases,
                                     unsigned EdgeIdx, SILBasicBlock *NewDest) {
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

template <class SwitchEnumTy, class SwitchEnumCaseTy>
SILBasicBlock *replaceSwitchDest(SwitchEnumTy *S,
                                     SmallVectorImpl<SwitchEnumCaseTy> &Cases,
                                     SILBasicBlock *OldDest, SILBasicBlock *NewDest) {
    auto *DefaultBB = S->hasDefault() ? S->getDefaultBB() : nullptr;
    for (unsigned i = 0, e = S->getNumCases(); i != e; ++i)
      if (S->getCase(i).second != OldDest)
        Cases.push_back(S->getCase(i));
      else
        Cases.push_back(std::make_pair(S->getCase(i).first, NewDest));
    if (OldDest == DefaultBB)
      DefaultBB = NewDest;
    return DefaultBB;
}

/// \brief Replace a branch target.
///
/// \param T The terminating instruction to modify.
/// \param OldDest The successor block that will be replaced.
/// \param NewDest The new target block.
/// \param PreserveArgs If set, preserve arguments on the replaced edge.
void swift::replaceBranchTarget(TermInst *T, SILBasicBlock *OldDest,
                               SILBasicBlock *NewDest, bool PreserveArgs) {
  SILBuilderWithScope B(T);

  switch (T->getTermKind()) {
  // Only Branch and CondBranch may have arguments.
  case TermKind::BranchInst: {
    auto Br = cast<BranchInst>(T);
    assert(OldDest == Br->getDestBB() && "wrong branch target");
    SmallVector<SILValue, 8> Args;
    if (PreserveArgs) {
      for (auto Arg : Br->getArgs())
        Args.push_back(Arg);
    }
    B.createBranch(T->getLoc(), NewDest, Args);
    Br->dropAllReferences();
    Br->eraseFromParent();
    return;
  }

  case TermKind::CondBranchInst: {
    auto CondBr = cast<CondBranchInst>(T);
    SmallVector<SILValue, 8> TrueArgs;
    if (OldDest == CondBr->getFalseBB() || PreserveArgs) {
      for (auto Arg : CondBr->getTrueArgs())
        TrueArgs.push_back(Arg);
    }
    SmallVector<SILValue, 8> FalseArgs;
    if (OldDest == CondBr->getTrueBB() || PreserveArgs) {
      for (auto Arg : CondBr->getFalseArgs())
        FalseArgs.push_back(Arg);
    }
    SILBasicBlock *TrueDest = CondBr->getTrueBB();
    SILBasicBlock *FalseDest = CondBr->getFalseBB();
    if (OldDest == CondBr->getTrueBB()) {
      TrueDest = NewDest;
    } else {
      assert(OldDest == CondBr->getFalseBB() && "wrong cond_br target");
      FalseDest = NewDest;
    }

    B.createCondBranch(CondBr->getLoc(), CondBr->getCondition(), TrueDest,
                       TrueArgs, FalseDest, FalseArgs, CondBr->getTrueBBCount(),
                       CondBr->getFalseBBCount());
    CondBr->dropAllReferences();
    CondBr->eraseFromParent();
    return;
  }

  case TermKind::SwitchValueInst: {
    auto SII = cast<SwitchValueInst>(T);
    SmallVector<std::pair<SILValue, SILBasicBlock *>, 8> Cases;
    auto *DefaultBB = replaceSwitchDest(SII, Cases, OldDest, NewDest);
    B.createSwitchValue(SII->getLoc(), SII->getOperand(), DefaultBB, Cases);
    SII->eraseFromParent();
    return;
  }

  case TermKind::SwitchEnumInst: {
    auto SEI = cast<SwitchEnumInst>(T);
    SmallVector<std::pair<EnumElementDecl*, SILBasicBlock*>, 8> Cases;
    auto *DefaultBB = replaceSwitchDest(SEI, Cases, OldDest, NewDest);
    B.createSwitchEnum(SEI->getLoc(), SEI->getOperand(), DefaultBB, Cases);
    SEI->eraseFromParent();
    return;
  }

  case TermKind::SwitchEnumAddrInst: {
    auto SEI = cast<SwitchEnumAddrInst>(T);
    SmallVector<std::pair<EnumElementDecl*, SILBasicBlock*>, 8> Cases;
    auto *DefaultBB = replaceSwitchDest(SEI, Cases, OldDest, NewDest);
    B.createSwitchEnumAddr(SEI->getLoc(), SEI->getOperand(), DefaultBB, Cases);
    SEI->eraseFromParent();
    return;
  }

  case TermKind::DynamicMethodBranchInst: {
    auto DMBI = cast<DynamicMethodBranchInst>(T);
    assert(OldDest == DMBI->getHasMethodBB() || OldDest == DMBI->getNoMethodBB() && "Invalid edge index");
    auto HasMethodBB = OldDest == DMBI->getHasMethodBB() ? NewDest : DMBI->getHasMethodBB();
    auto NoMethodBB = OldDest == DMBI->getNoMethodBB() ? NewDest : DMBI->getNoMethodBB();
    B.createDynamicMethodBranch(DMBI->getLoc(), DMBI->getOperand(),
        DMBI->getMember(), HasMethodBB, NoMethodBB);
    DMBI->eraseFromParent();
    return;
  }

  case TermKind::CheckedCastBranchInst: {
    auto CBI = cast<CheckedCastBranchInst>(T);
    assert(OldDest == CBI->getSuccessBB() || OldDest == CBI->getFailureBB() && "Invalid edge index");
    auto SuccessBB = OldDest == CBI->getSuccessBB() ? NewDest : CBI->getSuccessBB();
    auto FailureBB = OldDest == CBI->getFailureBB() ? NewDest : CBI->getFailureBB();
    B.createCheckedCastBranch(CBI->getLoc(), CBI->isExact(), CBI->getOperand(),
                              CBI->getCastType(), SuccessBB, FailureBB,
                              CBI->getTrueBBCount(), CBI->getFalseBBCount());
    CBI->eraseFromParent();
    return;
  }

  case TermKind::CheckedCastValueBranchInst: {
    auto CBI = cast<CheckedCastValueBranchInst>(T);
    assert(OldDest == CBI->getSuccessBB() ||
           OldDest == CBI->getFailureBB() && "Invalid edge index");
    auto SuccessBB =
        OldDest == CBI->getSuccessBB() ? NewDest : CBI->getSuccessBB();
    auto FailureBB =
        OldDest == CBI->getFailureBB() ? NewDest : CBI->getFailureBB();
    B.createCheckedCastValueBranch(CBI->getLoc(), CBI->getOperand(),
                                   CBI->getCastType(), SuccessBB, FailureBB);
    CBI->eraseFromParent();
    return;
  }

  case TermKind::CheckedCastAddrBranchInst: {
    auto CBI = cast<CheckedCastAddrBranchInst>(T);
    assert(OldDest == CBI->getSuccessBB() || OldDest == CBI->getFailureBB() && "Invalid edge index");
    auto SuccessBB = OldDest == CBI->getSuccessBB() ? NewDest : CBI->getSuccessBB();
    auto FailureBB = OldDest == CBI->getFailureBB() ? NewDest : CBI->getFailureBB();
    auto TrueCount = CBI->getTrueBBCount();
    auto FalseCount = CBI->getFalseBBCount();
    B.createCheckedCastAddrBranch(CBI->getLoc(), CBI->getConsumptionKind(),
                                  CBI->getSrc(), CBI->getSourceType(),
                                  CBI->getDest(), CBI->getTargetType(),
                                  SuccessBB, FailureBB, TrueCount, FalseCount);
    CBI->eraseFromParent();
    return;
  }

  case TermKind::ReturnInst:
  case TermKind::ThrowInst:
  case TermKind::TryApplyInst:
  case TermKind::UnreachableInst:
  case TermKind::UnwindInst:
  case TermKind::YieldInst:
    llvm_unreachable("Branch target cannot be replaced for this terminator instruction!");
  }
  llvm_unreachable("Not yet implemented!");
}

/// \brief Check if the edge from the terminator is critical.
bool swift::isCriticalEdge(TermInst *T, unsigned EdgeIdx) {
  assert(T->getSuccessors().size() > EdgeIdx && "Not enough successors");

  auto SrcSuccs = T->getSuccessors();
  
  if (SrcSuccs.size() <= 1 &&
      // Also consider non-branch instructions with a single successor for
      // critical edges, for example: a switch_enum of a single-case enum.
      (isa<BranchInst>(T) || isa<CondBranchInst>(T)))
    return false;

  SILBasicBlock *DestBB = SrcSuccs[EdgeIdx];
  assert(!DestBB->pred_empty() && "There should be a predecessor");
  if (DestBB->getSinglePredecessorBlock())
    return false;

  return true;
}

/// Splits the basic block at the iterator with an unconditional branch and
/// updates the dominator tree and loop info.
SILBasicBlock *swift::splitBasicBlockAndBranch(SILBuilder &B,
                                               SILInstruction *SplitBeforeInst,
                                               DominanceInfo *DT,
                                               SILLoopInfo *LI) {
  auto *OrigBB = SplitBeforeInst->getParent();
  auto *NewBB = OrigBB->split(SplitBeforeInst->getIterator());
  B.setInsertionPoint(OrigBB);
  B.createBranch(SplitBeforeInst->getLoc(), NewBB);

  // Update the dominator tree.
  if (DT) {
    auto OrigBBDTNode = DT->getNode(OrigBB);
    if (OrigBBDTNode) {
      // Change the immediate dominators of the children of the block we
      // splitted to the splitted block.
      SmallVector<DominanceInfoNode *, 16> Adoptees(OrigBBDTNode->begin(),
                                                    OrigBBDTNode->end());

      auto NewBBDTNode = DT->addNewBlock(NewBB, OrigBB);
      for (auto *Adoptee : Adoptees)
        DT->changeImmediateDominator(Adoptee, NewBBDTNode);
    }
  }

  // Update loop info.
  if (LI)
    if (auto *OrigBBLoop = LI->getLoopFor(OrigBB)) {
      OrigBBLoop->addBasicBlockToLoop(NewBB, LI->getBase());
    }

  return NewBB;
}

/// Split every edge between two basic blocks.
void swift::splitEdgesFromTo(SILBasicBlock *From, SILBasicBlock *To,
                             DominanceInfo *DT, SILLoopInfo *LI) {
  for (unsigned EdgeIndex = 0, E = From->getSuccessors().size(); EdgeIndex != E;
       ++EdgeIndex) {
    SILBasicBlock *SuccBB = From->getSuccessors()[EdgeIndex];
    if (SuccBB != To)
      continue;
    splitEdge(From->getTerminator(), EdgeIndex, DT, LI);
  }
}

/// Splits the n-th critical edge from the terminator and updates dominance and
/// loop info if set.
/// Returns the newly created basic block on success or nullptr otherwise (if
/// the edge was not critical.
SILBasicBlock *swift::splitCriticalEdge(TermInst *T, unsigned EdgeIdx,
                                        DominanceInfo *DT, SILLoopInfo *LI) {
  if (!isCriticalEdge(T, EdgeIdx))
    return nullptr;

  return splitEdge(T, EdgeIdx, DT, LI);
}

bool swift::hasCriticalEdges(SILFunction &F, bool OnlyNonCondBr) {
  for (SILBasicBlock &BB : F) {
    // Only consider critical edges for terminators that don't support block
    // arguments.
    if (OnlyNonCondBr && isa<CondBranchInst>(BB.getTerminator()))
      continue;

    if (isa<BranchInst>(BB.getTerminator()))
      continue;

    for (unsigned Idx = 0, e = BB.getSuccessors().size(); Idx != e; ++Idx)
      if (isCriticalEdge(BB.getTerminator(), Idx))
        return true;
  }
  return false;
}

/// Split all critical edges in the function updating the dominator tree and
/// loop information (if they are not set to null).
bool swift::splitAllCriticalEdges(SILFunction &F, DominanceInfo *DT,
                                  SILLoopInfo *LI) {
  bool Changed = false;

  for (SILBasicBlock &BB : F) {
    if (isa<BranchInst>(BB.getTerminator()))
      continue;

    for (unsigned Idx = 0, e = BB.getSuccessors().size(); Idx != e; ++Idx) {
      auto *NewBB = splitCriticalEdge(BB.getTerminator(), Idx, DT, LI);
      assert(!NewBB
             || isa<CondBranchInst>(BB.getTerminator())
                    && "Only cond_br may have a critical edge.");
      Changed |= (NewBB != nullptr);
    }
  }
  return Changed;
}

/// Merge the basic block with its successor if possible. If dominance
/// information or loop info is non null update it. Return true if block was
/// merged.
bool swift::mergeBasicBlockWithSuccessor(SILBasicBlock *BB, DominanceInfo *DT,
                                         SILLoopInfo *LI) {
  auto *Branch = dyn_cast<BranchInst>(BB->getTerminator());
  if (!Branch)
    return false;

  auto *SuccBB = Branch->getDestBB();
  if (BB == SuccBB || !SuccBB->getSinglePredecessorBlock())
    return false;

  if (DT)
    if (auto *SuccBBNode = DT->getNode(SuccBB)) {
      // Change the immediate dominator for children of the successor to be the
      // current block.
      auto *BBNode = DT->getNode(BB);
      SmallVector<DominanceInfoNode *, 8> Children(SuccBBNode->begin(),
                                                   SuccBBNode->end());
      for (auto *ChildNode : Children)
        DT->changeImmediateDominator(ChildNode, BBNode);

      DT->eraseNode(SuccBB);
    }

  if (LI)
    LI->removeBlock(SuccBB);

  mergeBasicBlockWithSingleSuccessor(BB, SuccBB);

  return true;
}

/// Splits the critical edges between from and to. This code assumes there is
/// only one edge between the two basic blocks.
SILBasicBlock *swift::splitIfCriticalEdge(SILBasicBlock *From,
                                          SILBasicBlock *To,
                                          DominanceInfo *DT,
                                          SILLoopInfo *LI) {
  auto *T = From->getTerminator();
  for (unsigned i = 0, e = T->getSuccessors().size(); i != e; ++i) {
    if (T->getSuccessors()[i] == To)
      return splitCriticalEdge(T, i, DT, LI);
  }
  llvm_unreachable("Destination block not found");
}

void swift::completeJointPostDominanceSet(
    ArrayRef<SILBasicBlock *> UserBlocks, ArrayRef<SILBasicBlock *> DefBlocks,
    llvm::SmallVectorImpl<SILBasicBlock *> &Result) {
  assert(!UserBlocks.empty() && "Must have at least 1 user block");
  assert(!DefBlocks.empty() && "Must have at least 1 def block");

  // If we have only one def block and one user block and they are the same
  // block, then just return.
  if (DefBlocks.size() == 1 && UserBlocks.size() == 1 &&
      UserBlocks[0] == DefBlocks[0]) {
    return;
  }

  // Some notes on the algorithm:
  //
  // 1. Our VisitedBlocks set just states that a value has been added to the
  // worklist and should not be added to the worklist.
  // 2. Our targets of the CFG block are DefBlockSet.
  // 3. We find the missing post-domination blocks by finding successors of
  // blocks on our walk that we have not visited by the end of the walk. For
  // joint post-dominance to be true, no such successors should exist.

  // Our set of target blocks where we stop walking.
  llvm::SmallPtrSet<SILBasicBlock *, 8> DefBlockSet(DefBlocks.begin(),
                                                    DefBlocks.end());

  // The set of successor blocks of blocks that we visit. Any blocks still in
  // this set at the end of the walk act as a post-dominating closure around our
  // UserBlock set.
  llvm::SmallSetVector<SILBasicBlock *, 16> MustVisitSuccessorBlocks;

  // Add our user and def blocks to the VisitedBlock set. We never want to find
  // these in our worklist.
  llvm::SmallPtrSet<SILBasicBlock *, 32> VisitedBlocks(UserBlocks.begin(),
                                                       UserBlocks.end());

  // Finally setup our worklist by adding our user block predecessors. We only
  // add the predecessors to the worklist once.
  llvm::SmallVector<SILBasicBlock *, 32> Worklist;
  for (auto *Block : UserBlocks) {
    copy_if(Block->getPredecessorBlocks(), std::back_inserter(Worklist),
            [&](SILBasicBlock *PredBlock) -> bool {
              return VisitedBlocks.insert(PredBlock).second;
            });
  }

  // Then until we reach a fix point.
  while (!Worklist.empty()) {
    // Grab the next block from the worklist.
    auto *Block = Worklist.pop_back_val();
    assert(VisitedBlocks.count(Block) && "All blocks from worklist should be "
                                         "in the visited blocks set.");

    // Since we are visiting this block now, we know that this block can not be
    // apart of a the post-dominance closure of our UseBlocks.
    MustVisitSuccessorBlocks.remove(Block);

    // Then add each successor block of Block that has not been visited yet to
    // the MustVisitSuccessorBlocks set.
    for (auto *SuccBlock : Block->getSuccessorBlocks()) {
      if (!VisitedBlocks.count(SuccBlock)) {
        MustVisitSuccessorBlocks.insert(SuccBlock);
      }
    }

    // If this is a def block, then do not add its predecessors to the
    // worklist.
    if (DefBlockSet.count(Block))
      continue;

    // Otherwise add all unvisited predecessors to the worklist.
    copy_if(Block->getPredecessorBlocks(), std::back_inserter(Worklist),
            [&](SILBasicBlock *Block) -> bool {
              return VisitedBlocks.insert(Block).second;
            });
  }

  // Now that we are done, add all remaining must visit blocks to our result
  // list. These are the remaining parts of our joint post-dominance closure.
  copy(MustVisitSuccessorBlocks, std::back_inserter(Result));
}

bool swift::splitAllCondBrCriticalEdgesWithNonTrivialArgs(SILFunction &Fn,
                                                          DominanceInfo *DT,
                                                          SILLoopInfo *LI) {
  // Find our targets.
  llvm::SmallVector<std::pair<SILBasicBlock *, unsigned>, 8> Targets;
  for (auto &Block : Fn) {
    auto *CBI = dyn_cast<CondBranchInst>(Block.getTerminator());
    if (!CBI)
      continue;

    // See if our true index is a critical edge. If so, add block to the list
    // and continue. If the false edge is also critical, we will handle it at
    // the same time.
    if (isCriticalEdge(CBI, CondBranchInst::TrueIdx)) {
      Targets.emplace_back(&Block, CondBranchInst::TrueIdx);
    }

    if (!isCriticalEdge(CBI, CondBranchInst::FalseIdx)) {
      continue;
    }

    Targets.emplace_back(&Block, CondBranchInst::FalseIdx);
  }

  if (Targets.empty())
    return false;

  for (auto P : Targets) {
    SILBasicBlock *Block = P.first;
    unsigned Index = P.second;
    auto *Result = splitCriticalEdge(Block->getTerminator(), Index, DT, LI);
    (void)Result;
    assert(Result);
  }

  return true;
}

namespace {
  class RemoveUnreachable {
    SILFunction &Fn;
    llvm::SmallSet<SILBasicBlock *, 8> Visited;
  public:
    RemoveUnreachable(SILFunction &Fn) : Fn(Fn) { }
    void visit(SILBasicBlock *BB);
    bool run();
  };
} // end anonymous namespace

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
  visit(&*Fn.begin());

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

bool swift::removeUnreachableBlocks(SILFunction &Fn) {
  return RemoveUnreachable(Fn).run();
}
