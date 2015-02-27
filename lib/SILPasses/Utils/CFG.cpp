//===--- CFG.cpp - Utilities for SIL CFG transformations --------*- C++ -*-===//
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

#include "swift/SIL/Dominance.h"
#include "swift/SIL/LoopInfo.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SILPasses/Utils/CFG.h"

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
  SILBuilderWithScope<2> Builder(Branch);
  TermInst *NewBr = nullptr;

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
    }
    if (Dest == CBI->getFalseBB()) {
      FalseArgs.push_back(Val);
      assert(FalseArgs.size() == Dest->getNumBBArg());
    }

    NewBr = Builder.createCondBranch(CBI->getLoc(), CBI->getCondition(),
                                    CBI->getTrueBB(), TrueArgs,
                                    CBI->getFalseBB(), FalseArgs);
  } else if (BranchInst *BI = dyn_cast<BranchInst>(Branch)) {
    SmallVector<SILValue, 8> Args;

    for (auto A : BI->getArgs())
      Args.push_back(A);

    Args.push_back(Val);
    assert(Args.size() == Dest->getNumBBArg());
    NewBr = Builder.createBranch(BI->getLoc(), BI->getDestBB(), Args);
  } else {
    NewBr->dump();
    // At the moment we can only add arguments to br and cond_br.
    llvm_unreachable("Can't add argument to terminator");
    return NewBr;
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
  SILBuilderWithScope<2> Builder(Branch);

  if (CondBranchInst *CBI = dyn_cast<CondBranchInst>(Branch)) {
    SmallVector<SILValue, 8> TrueArgs;
    SmallVector<SILValue, 8> FalseArgs;

    OperandValueArrayRef OldTrueArgs = CBI->getTrueArgs();
    bool BranchOnTrue = CBI->getTrueBB() == Dest;
    assert((!BranchOnTrue || Idx < OldTrueArgs.size()) && "Not enough edges");

    // Copy the edge values overwritting the edge at Idx.
    for (unsigned i = 0, e = OldTrueArgs.size(); i != e; ++i) {
      if (BranchOnTrue && Idx == i)
        TrueArgs.push_back(Val);
      else
        TrueArgs.push_back(OldTrueArgs[i]);
    }
    assert(TrueArgs.size() == CBI->getTrueBB()->getNumBBArg() &&
           "Destination block's number of arguments must match");

    OperandValueArrayRef OldFalseArgs = CBI->getFalseArgs();
    bool BranchOnFalse = CBI->getFalseBB() == Dest;
    assert((!BranchOnFalse || Idx < OldFalseArgs.size()) && "Not enough edges");

    // Copy the edge values overwritting the edge at Idx.
    for (unsigned i = 0, e = OldFalseArgs.size(); i != e; ++i) {
      if (BranchOnFalse && Idx == i)
        FalseArgs.push_back(Val);
      else
        FalseArgs.push_back(OldFalseArgs[i]);
    }
    assert(FalseArgs.size() == CBI->getFalseBB()->getNumBBArg() &&
           "Destination block's number of arguments must match");

    CBI = Builder.createCondBranch(CBI->getLoc(), CBI->getCondition(),
                                    CBI->getTrueBB(), TrueArgs,
                                    CBI->getFalseBB(), FalseArgs);
    Branch->dropAllReferences();
    Branch->eraseFromParent();
    return CBI;
  }

  if (BranchInst *BI = dyn_cast<BranchInst>(Branch)) {
    SmallVector<SILValue, 8> Args;

    assert(Idx < BI->getNumArgs() && "Not enough edges");
    OperandValueArrayRef OldArgs = BI->getArgs();

    // Copy the edge values overwritting the edge at Idx.
    for (unsigned i = 0, e = OldArgs.size(); i != e; ++i) {
      if (Idx == i)
        Args.push_back(Val);
      else
        Args.push_back(OldArgs[i]);
    }
    assert(Args.size() == Dest->getNumBBArg());

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

void swift::changeBranchTarget(TermInst *T, unsigned EdgeIdx,
                               SILBasicBlock *NewDest, bool PreserveArgs) {
  SILBuilderWithScope<8> B(T);

  // Only Branch and CondBranch may have arguments.
  if (auto Br = dyn_cast<BranchInst>(T)) {
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

  if (auto CondBr = dyn_cast<CondBranchInst>(T)) {
    SmallVector<SILValue, 8> TrueArgs;
    if (EdgeIdx == CondBranchInst::FalseIdx || PreserveArgs) {
      for (auto Arg : CondBr->getTrueArgs())
        TrueArgs.push_back(Arg);
    }
    SmallVector<SILValue, 8> FalseArgs;
    if (EdgeIdx == CondBranchInst::TrueIdx || PreserveArgs) {
      for (auto Arg : CondBr->getFalseArgs())
        FalseArgs.push_back(Arg);
    }
    SILBasicBlock *TrueDest = CondBr->getTrueBB();
    SILBasicBlock *FalseDest = CondBr->getFalseBB();
    if (EdgeIdx == CondBranchInst::TrueIdx)
      TrueDest = NewDest;
    else
      FalseDest = NewDest;

    B.createCondBranch(CondBr->getLoc(), CondBr->getCondition(),
                       TrueDest, TrueArgs, FalseDest, FalseArgs);
    CondBr->dropAllReferences();
    CondBr->eraseFromParent();
    return;
  }

  if (auto SII = dyn_cast<SwitchValueInst>(T)) {
    SmallVector<std::pair<SILValue, SILBasicBlock *>, 8> Cases;
    auto *DefaultBB = replaceSwitchDest(SII, Cases, EdgeIdx, NewDest);
    B.createSwitchValue(SII->getLoc(), SII->getOperand(), DefaultBB, Cases);
    SII->eraseFromParent();
    return;
  }

  if (auto SEI = dyn_cast<SwitchEnumInst>(T)) {
    SmallVector<std::pair<EnumElementDecl*, SILBasicBlock*>, 8> Cases;
    auto *DefaultBB = replaceSwitchDest(SEI, Cases, EdgeIdx, NewDest);
    B.createSwitchEnum(SEI->getLoc(), SEI->getOperand(), DefaultBB, Cases);
    SEI->eraseFromParent();
    return;
  }

  if (auto SEI = dyn_cast<SwitchEnumAddrInst>(T)) {
    SmallVector<std::pair<EnumElementDecl*, SILBasicBlock*>, 8> Cases;
    auto *DefaultBB = replaceSwitchDest(SEI, Cases, EdgeIdx, NewDest);
    B.createSwitchEnumAddr(SEI->getLoc(), SEI->getOperand(), DefaultBB, Cases);
    SEI->eraseFromParent();
    return;
  }

  if (auto DMBI = dyn_cast<DynamicMethodBranchInst>(T)) {
    assert(EdgeIdx == 0 || EdgeIdx == 1 && "Invalid edge index");
    auto HasMethodBB = !EdgeIdx ? NewDest : DMBI->getHasMethodBB();
    auto NoMethodBB = EdgeIdx ? NewDest : DMBI->getNoMethodBB();
    B.createDynamicMethodBranch(DMBI->getLoc(), DMBI->getOperand(),
                                DMBI->getMember(), HasMethodBB, NoMethodBB);
    DMBI->eraseFromParent();
    return;
  }

  if (auto CBI = dyn_cast<CheckedCastBranchInst>(T)) {
    assert(EdgeIdx == 0 || EdgeIdx == 1 && "Invalid edge index");
    auto SuccessBB = !EdgeIdx ? NewDest : CBI->getSuccessBB();
    auto FailureBB = EdgeIdx ? NewDest : CBI->getFailureBB();
    B.createCheckedCastBranch(CBI->getLoc(), CBI->isExact(), CBI->getOperand(),
                              CBI->getCastType(), SuccessBB, FailureBB);
    CBI->eraseFromParent();
    return;
  }

  if (auto CBI = dyn_cast<CheckedCastAddrBranchInst>(T)) {
    assert(EdgeIdx == 0 || EdgeIdx == 1 && "Invalid edge index");
    auto SuccessBB = !EdgeIdx ? NewDest : CBI->getSuccessBB();
    auto FailureBB = EdgeIdx ? NewDest : CBI->getFailureBB();
    B.createCheckedCastAddrBranch(CBI->getLoc(), CBI->getConsumptionKind(),
                                  CBI->getSrc(), CBI->getSourceType(),
                                  CBI->getDest(), CBI->getTargetType(),
                                  SuccessBB, FailureBB);
    CBI->eraseFromParent();
    return;
  }

  llvm_unreachable("Missing implementation");
}

/// \brief Check if the edge from the terminator is critical.
bool swift::isCriticalEdge(TermInst *T, unsigned EdgeIdx) {
  assert(T->getSuccessors().size() > EdgeIdx && "Not enough successors");

  auto SrcSuccs = T->getSuccessors();
  if (SrcSuccs.size() <= 1)
    return false;

  SILBasicBlock *DestBB = SrcSuccs[EdgeIdx];
  assert(!DestBB->pred_empty() && "There should be a predecessor");
  if (DestBB->getSinglePredecessor())
    return false;

  return true;
}

template<class SwitchInstTy>
SILBasicBlock *getNthEdgeBlock(SwitchInstTy *S, unsigned EdgeIdx) {
  if (S->getNumCases() == EdgeIdx)
    return S->getDefaultBB();
  return S->getCase(EdgeIdx).second;
}

static void getEdgeArgs(TermInst *T, unsigned EdgeIdx, SILBasicBlock *NewEdgeBB,
                        SmallVectorImpl<SILValue> &Args) {
  if (auto Br = dyn_cast<BranchInst>(T)) {
    for (auto V : Br->getArgs())
      Args.push_back(V);
    return;
  }

  if (auto CondBr = dyn_cast<CondBranchInst>(T)) {
    assert(EdgeIdx < 2);
    auto OpdArgs = EdgeIdx ? CondBr->getFalseArgs() : CondBr->getTrueArgs();
    for (auto V: OpdArgs)
      Args.push_back(V);
    return;
  }

  if (auto SEI = dyn_cast<SwitchValueInst>(T)) {
    auto *SuccBB = getNthEdgeBlock(SEI, EdgeIdx);
    assert(SuccBB->getNumBBArg() == 0 && "Can't take an argument");
    (void) SuccBB;
    return;
  }

  // A switch_enum can implicitly pass the enum payload. We need to look at the
  // destination block to figure this out.
  if (auto SEI = dyn_cast<SwitchEnumInstBase>(T)) {
    auto *SuccBB = getNthEdgeBlock(SEI, EdgeIdx);
    assert(SuccBB->getNumBBArg() < 2 && "Can take at most one argument");
    if (!SuccBB->getNumBBArg())
      return;
    Args.push_back(
        SILValue(NewEdgeBB->createBBArg(SuccBB->getBBArg(0)->getType()), 0));
    return;
  }

  // A dynamic_method_br passes the function to the first basic block.
  if (auto DMBI = dyn_cast<DynamicMethodBranchInst>(T)) {
    auto *SuccBB =
        (EdgeIdx == 0) ? DMBI->getHasMethodBB() : DMBI->getNoMethodBB();
    if (!SuccBB->getNumBBArg())
      return;
    Args.push_back(
        SILValue(NewEdgeBB->createBBArg(SuccBB->getBBArg(0)->getType()), 0));
    return;
  }

  /// A checked_cast_br passes the result of the cast to the first basic block.
  if (auto CBI = dyn_cast<CheckedCastBranchInst>(T)) {
    auto SuccBB = EdgeIdx == 0 ? CBI->getSuccessBB() : CBI->getFailureBB();
    if (!SuccBB->getNumBBArg())
      return;
    Args.push_back(
        SILValue(NewEdgeBB->createBBArg(SuccBB->getBBArg(0)->getType()), 0));
    return;
  }
  if (auto CBI = dyn_cast<CheckedCastAddrBranchInst>(T)) {
    auto SuccBB = EdgeIdx == 0 ? CBI->getSuccessBB() : CBI->getFailureBB();
    if (!SuccBB->getNumBBArg())
      return;
    Args.push_back(
        SILValue(NewEdgeBB->createBBArg(SuccBB->getBBArg(0)->getType()), 0));
    return;
  }

  // For now this utility is only used to split critical edges involving
  // cond_br.
  llvm_unreachable("Not yet implemented");
}

/// Splits the basic block at the iterator with an unconditional branch and
/// updates the dominator tree and loop info.
SILBasicBlock *swift::splitBasicBlockAndBranch(SILInstruction *SplitBeforeInst,
                                               DominanceInfo *DT,
                                               SILLoopInfo *LI) {
  auto *OrigBB = SplitBeforeInst->getParent();
  auto *NewBB = OrigBB->splitBasicBlockAndBranch(SplitBeforeInst,
                                                 SplitBeforeInst->getLoc());

  // Update the dominator tree.
  if (DT) {
    auto OrigBBDTNode = DT->getNode(OrigBB);
    if (OrigBBDTNode) {
      auto NewBBDTNode = DT->addNewBlock(NewBB, OrigBB);
      for (auto *Child : *OrigBBDTNode)
        if (Child != NewBBDTNode)
          DT->changeImmediateDominator(Child, NewBBDTNode);
    }
  }

  // Update loop info.
  if (LI)
    if (auto *OrigBBLoop = LI->getLoopFor(OrigBB)) {
      OrigBBLoop->addBasicBlockToLoop(NewBB, LI->getBase());
    }

  return NewBB;
}
/// Splits the n-th critical edge from the terminator and updates dominance and
/// loop info if set.
/// Returns the newly created basic block on success or nullptr otherwise (if
/// the edge was not critical.
SILBasicBlock *swift::splitCriticalEdge(TermInst *T, unsigned EdgeIdx,
                                        DominanceInfo *DT, SILLoopInfo *LI) {
  if (!isCriticalEdge(T, EdgeIdx))
    return nullptr;

  auto *SrcBB = T->getParent();
  auto *Fn = SrcBB->getParent();

  SILBasicBlock *DestBB = T->getSuccessors()[EdgeIdx];

  // Create a new basic block in the edge, and insert it after the SrcBB.
  auto *EdgeBB = new (Fn->getModule()) SILBasicBlock(Fn, SrcBB);

  SmallVector<SILValue, 16> Args;
  getEdgeArgs(T, EdgeIdx, EdgeBB, Args);

  // Connect it to the successor with the args of the old edge.
  auto &InstList = EdgeBB->getInstList();
  InstList.insert(InstList.end(),
                  BranchInst::create(T->getLoc(), DestBB, Args, *Fn));

  // Strip the arguments and rewire the branch in the source block.
  changeBranchTarget(T, EdgeIdx, EdgeBB, /*PreserveArgs=*/false);

  if (!DT && !LI)
    return EdgeBB;

  // Update the dominator tree.
  if (DT) {
    auto *DestBBNode = DT->getNode(DestBB);
    // Unreachable code could result in a null return here.
    if (DestBBNode) {
      // The new block is dominated by the SrcBB.
      auto *EdgeBBNode = DT->addNewBlock(EdgeBB, SrcBB);

      // Are all predecessors of DestBB dominated by SrcBB?
      bool OldSrcBBDominatesAllPreds = std::all_of(
          DestBB->pred_begin(), DestBB->pred_end(), [=](SILBasicBlock *B) {
            if (DT->dominates(SrcBB, B))
              return true;
            return false;
          });

      // If so, the new bb dominates DestBB now.
      if (OldSrcBBDominatesAllPreds)
        DT->changeImmediateDominator(DestBBNode, EdgeBBNode);
    }
  }

  if (!LI)
    return EdgeBB;

  // Update loop info. Both blocks must be in a loop otherwise the split block
  // is outside the loop.
  SILLoop *SrcBBLoop = LI->getLoopFor(SrcBB);
  if (!SrcBBLoop)
    return EdgeBB;
  SILLoop *DstBBLoop = LI->getLoopFor(DestBB);
  if (!DstBBLoop)
    return EdgeBB;

  // Same loop.
  if (DstBBLoop == SrcBBLoop) {
    DstBBLoop->addBasicBlockToLoop(EdgeBB, LI->getBase());
    return EdgeBB;
  }

  // Edge from inner to outer loop.
  if (DstBBLoop->contains(SrcBBLoop)) {
    DstBBLoop->addBasicBlockToLoop(EdgeBB, LI->getBase());
    return EdgeBB;
  }

  // Edge from outer to inner loop.
  if (SrcBBLoop->contains(DstBBLoop)) {
    SrcBBLoop->addBasicBlockToLoop(EdgeBB, LI->getBase());
    return EdgeBB;
  }

  // Neither loop contains the other. The destination must be the header of its
  // loop. Otherwise, we would be creating irreducable control flow.
  assert(DstBBLoop->getHeader() == DestBB &&
         "Creating irreducible control flow?");

  // Add to outer loop if there is one.
  if (auto *Parent = DstBBLoop->getParentLoop())
    Parent->addBasicBlockToLoop(EdgeBB, LI->getBase());

  return EdgeBB;
}

/// Split all critical edges in the function updating the dominator tree and
/// loop information (if they are not set to null).
bool swift::splitAllCriticalEdges(SILFunction &F, bool OnlyNonCondBr,
                                  DominanceInfo *DT, SILLoopInfo *LI) {
  bool Changed = false;

  std::vector<SILBasicBlock*> Blocks;
  Blocks.reserve(F.size());

  for (auto &It : F)
    Blocks.push_back(&It);

  for (auto It : Blocks) {
    // Only split critical edges for terminators that don't support block
    // arguments.
    if (OnlyNonCondBr && isa<CondBranchInst>(It->getTerminator()))
      continue;

    if (isa<BranchInst>(It->getTerminator()))
      continue;

    for (unsigned Idx = 0, e = It->getSuccs().size(); Idx != e; ++Idx)
      Changed |=
        (splitCriticalEdge(It->getTerminator(), Idx, DT, LI) != nullptr);
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
  if (BB == SuccBB || !SuccBB->getSinglePredecessor())
    return false;

  // If there are any BB arguments in the destination, replace them with the
  // branch operands, since they must dominate the dest block.
  for (unsigned i = 0, e = Branch->getArgs().size(); i != e; ++i)
    SILValue(SuccBB->getBBArg(i)).replaceAllUsesWith(Branch->getArg(i));

  Branch->eraseFromParent();

  // Move the instruction from the successor block to the current block.
  BB->getInstList().splice(BB->end(), SuccBB->getInstList());

  if (DT)
    if (auto *SuccBBNode = DT->getNode(SuccBB)) {
      // Change the immediate dominator for children of the successor to be the
      // current block.
      auto *BBNode = DT->getNode(BB);
      SmallVector<DominanceInfoNode *, 8> Children(SuccBBNode->begin(),
                                                   SuccBBNode->end());
      for (auto *ChildNode : *SuccBBNode)
        DT->changeImmediateDominator(ChildNode, BBNode);

      DT->eraseNode(SuccBB);
    }

  if (LI)
    LI->removeBlock(SuccBB);

  SuccBB->eraseFromParent();

  return true;
}
