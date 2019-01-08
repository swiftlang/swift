//===--- BasicBlockUtils.cpp - Utilities for SILBasicBlock ----------------===//
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
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILBasicBlock.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILFunction.h"

using namespace swift;

static bool hasBranchArguments(TermInst *T, unsigned edgeIdx) {
  if (auto *BI = dyn_cast<BranchInst>(T)) {
    assert(edgeIdx == 0);
    return BI->getNumArgs() != 0;
  }
  if (auto CBI = dyn_cast<CondBranchInst>(T)) {
    assert(edgeIdx <= 1);
    return edgeIdx == CondBranchInst::TrueIdx ? !CBI->getTrueArgs().empty()
                                              : !CBI->getFalseArgs().empty();
  }
  // No other terminator have branch arguments.
  return false;
}

void swift::changeBranchTarget(TermInst *T, unsigned edgeIdx,
                               SILBasicBlock *newDest, bool preserveArgs) {
  // In many cases, we can just rewrite the successor in place.
  if (preserveArgs || !hasBranchArguments(T, edgeIdx)) {
    T->getSuccessors()[edgeIdx] = newDest;
    return;
  }

  // Otherwise, we have to build a new branch instruction.
  SILBuilderWithScope B(T);

  switch (T->getTermKind()) {
  // Only Branch and CondBranch may have arguments.
  case TermKind::BranchInst: {
    auto *BI = cast<BranchInst>(T);
    SmallVector<SILValue, 8> args;
    for (auto arg : BI->getArgs())
      args.push_back(arg);
    B.createBranch(T->getLoc(), newDest, args);
    BI->dropAllReferences();
    BI->eraseFromParent();
    return;
  }

  case TermKind::CondBranchInst: {
    auto CBI = cast<CondBranchInst>(T);
    SILBasicBlock *trueDest = CBI->getTrueBB();
    SILBasicBlock *falseDest = CBI->getFalseBB();

    SmallVector<SILValue, 8> trueArgs;
    SmallVector<SILValue, 8> falseArgs;
    if (edgeIdx == CondBranchInst::FalseIdx) {
      falseDest = newDest;
      for (auto arg : CBI->getTrueArgs())
        trueArgs.push_back(arg);
    } else {
      trueDest = newDest;
      for (auto arg : CBI->getFalseArgs())
        falseArgs.push_back(arg);
    }

    B.createCondBranch(CBI->getLoc(), CBI->getCondition(), trueDest, trueArgs,
                       falseDest, falseArgs, CBI->getTrueBBCount(),
                       CBI->getFalseBBCount());
    CBI->dropAllReferences();
    CBI->eraseFromParent();
    return;
  }

  default:
    llvm_unreachable("only branch and cond_branch have branch arguments");
  }
}

template <class SwitchInstTy>
static SILBasicBlock *getNthEdgeBlock(SwitchInstTy *S, unsigned edgeIdx) {
  if (S->getNumCases() == edgeIdx)
    return S->getDefaultBB();
  return S->getCase(edgeIdx).second;
}

void swift::getEdgeArgs(TermInst *T, unsigned edgeIdx, SILBasicBlock *newEdgeBB,
                        llvm::SmallVectorImpl<SILValue> &args) {
  switch (T->getKind()) {
  case SILInstructionKind::BranchInst: {
    auto *B = cast<BranchInst>(T);
    for (auto V : B->getArgs())
      args.push_back(V);
    return;
  }

  case SILInstructionKind::CondBranchInst: {
    auto CBI = cast<CondBranchInst>(T);
    assert(edgeIdx < 2);
    auto OpdArgs = edgeIdx ? CBI->getFalseArgs() : CBI->getTrueArgs();
    for (auto V : OpdArgs)
      args.push_back(V);
    return;
  }

  case SILInstructionKind::SwitchValueInst: {
    auto SEI = cast<SwitchValueInst>(T);
    auto *succBB = getNthEdgeBlock(SEI, edgeIdx);
    assert(succBB->getNumArguments() == 0 && "Can't take an argument");
    (void)succBB;
    return;
  }

  // A switch_enum can implicitly pass the enum payload. We need to look at the
  // destination block to figure this out.
  case SILInstructionKind::SwitchEnumInst:
  case SILInstructionKind::SwitchEnumAddrInst: {
    auto SEI = cast<SwitchEnumInstBase>(T);
    auto *succBB = getNthEdgeBlock(SEI, edgeIdx);
    assert(succBB->getNumArguments() < 2 && "Can take at most one argument");
    if (!succBB->getNumArguments())
      return;
    args.push_back(newEdgeBB->createPhiArgument(
        succBB->getArgument(0)->getType(), ValueOwnershipKind::Owned));
    return;
  }

  // A dynamic_method_br passes the function to the first basic block.
  case SILInstructionKind::DynamicMethodBranchInst: {
    auto DMBI = cast<DynamicMethodBranchInst>(T);
    auto *succBB =
        (edgeIdx == 0) ? DMBI->getHasMethodBB() : DMBI->getNoMethodBB();
    if (!succBB->getNumArguments())
      return;
    args.push_back(newEdgeBB->createPhiArgument(
        succBB->getArgument(0)->getType(), ValueOwnershipKind::Owned));
    return;
  }

  /// A checked_cast_br passes the result of the cast to the first basic block.
  case SILInstructionKind::CheckedCastBranchInst: {
    auto CBI = cast<CheckedCastBranchInst>(T);
    auto succBB = edgeIdx == 0 ? CBI->getSuccessBB() : CBI->getFailureBB();
    if (!succBB->getNumArguments())
      return;
    args.push_back(newEdgeBB->createPhiArgument(
        succBB->getArgument(0)->getType(), ValueOwnershipKind::Owned));
    return;
  }
  case SILInstructionKind::CheckedCastAddrBranchInst: {
    auto CBI = cast<CheckedCastAddrBranchInst>(T);
    auto succBB = edgeIdx == 0 ? CBI->getSuccessBB() : CBI->getFailureBB();
    if (!succBB->getNumArguments())
      return;
    args.push_back(newEdgeBB->createPhiArgument(
        succBB->getArgument(0)->getType(), ValueOwnershipKind::Owned));
    return;
  }
  case SILInstructionKind::CheckedCastValueBranchInst: {
    auto CBI = cast<CheckedCastValueBranchInst>(T);
    auto succBB = edgeIdx == 0 ? CBI->getSuccessBB() : CBI->getFailureBB();
    if (!succBB->getNumArguments())
      return;
    args.push_back(newEdgeBB->createPhiArgument(
        succBB->getArgument(0)->getType(), ValueOwnershipKind::Owned));
    return;
  }

  case SILInstructionKind::TryApplyInst: {
    auto *TAI = cast<TryApplyInst>(T);
    auto *succBB = edgeIdx == 0 ? TAI->getNormalBB() : TAI->getErrorBB();
    if (!succBB->getNumArguments())
      return;
    args.push_back(newEdgeBB->createPhiArgument(
        succBB->getArgument(0)->getType(), ValueOwnershipKind::Owned));
    return;
  }

  case SILInstructionKind::YieldInst:
    // The edges from 'yield' never have branch arguments.
    return;

  case SILInstructionKind::ReturnInst:
  case SILInstructionKind::ThrowInst:
  case SILInstructionKind::UnwindInst:
  case SILInstructionKind::UnreachableInst:
    llvm_unreachable("terminator never has successors");

#define TERMINATOR(ID, ...)
#define INST(ID, BASE) case SILInstructionKind::ID:
#include "swift/SIL/SILNodes.def"
    llvm_unreachable("not a terminator");
  }
  llvm_unreachable("bad instruction kind");
}

SILBasicBlock *swift::splitEdge(TermInst *T, unsigned edgeIdx,
                                DominanceInfo *DT, SILLoopInfo *LI) {
  auto *srcBB = T->getParent();
  auto *F = srcBB->getParent();

  SILBasicBlock *destBB = T->getSuccessors()[edgeIdx];

  // Create a new basic block in the edge, and insert it after the srcBB.
  auto *edgeBB = F->createBasicBlockAfter(srcBB);

  SmallVector<SILValue, 16> args;
  getEdgeArgs(T, edgeIdx, edgeBB, args);

  SILBuilder(edgeBB).createBranch(T->getLoc(), destBB, args);

  // Strip the arguments and rewire the branch in the source block.
  changeBranchTarget(T, edgeIdx, edgeBB, /*PreserveArgs=*/false);

  if (!DT && !LI)
    return edgeBB;

  // Update the dominator tree.
  if (DT) {
    auto *srcBBNode = DT->getNode(srcBB);

    // Unreachable code could result in a null return here.
    if (srcBBNode) {
      // The new block is dominated by the srcBB.
      auto *edgeBBNode = DT->addNewBlock(edgeBB, srcBB);

      // Are all predecessors of destBB dominated by destBB?
      auto *destBBNode = DT->getNode(destBB);
      bool oldSrcBBDominatesAllPreds = std::all_of(
          destBB->pred_begin(), destBB->pred_end(), [=](SILBasicBlock *B) {
            if (B == edgeBB)
              return true;
            auto *PredNode = DT->getNode(B);
            if (!PredNode)
              return true;
            if (DT->dominates(destBBNode, PredNode))
              return true;
            return false;
          });

      // If so, the new bb dominates destBB now.
      if (oldSrcBBDominatesAllPreds)
        DT->changeImmediateDominator(destBBNode, edgeBBNode);
    }
  }

  if (!LI)
    return edgeBB;

  // Update loop info. Both blocks must be in a loop otherwise the split block
  // is outside the loop.
  SILLoop *srcBBLoop = LI->getLoopFor(srcBB);
  if (!srcBBLoop)
    return edgeBB;
  SILLoop *DstBBLoop = LI->getLoopFor(destBB);
  if (!DstBBLoop)
    return edgeBB;

  // Same loop.
  if (DstBBLoop == srcBBLoop) {
    DstBBLoop->addBasicBlockToLoop(edgeBB, LI->getBase());
    return edgeBB;
  }

  // Edge from inner to outer loop.
  if (DstBBLoop->contains(srcBBLoop)) {
    DstBBLoop->addBasicBlockToLoop(edgeBB, LI->getBase());
    return edgeBB;
  }

  // Edge from outer to inner loop.
  if (srcBBLoop->contains(DstBBLoop)) {
    srcBBLoop->addBasicBlockToLoop(edgeBB, LI->getBase());
    return edgeBB;
  }

  // Neither loop contains the other. The destination must be the header of its
  // loop. Otherwise, we would be creating irreducible control flow.
  assert(DstBBLoop->getHeader() == destBB
         && "Creating irreducible control flow?");

  // Add to outer loop if there is one.
  if (auto *parent = DstBBLoop->getParentLoop())
    parent->addBasicBlockToLoop(edgeBB, LI->getBase());

  return edgeBB;
}

/// Merge the basic block with its successor if possible.
void swift::mergeBasicBlockWithSingleSuccessor(SILBasicBlock *BB,
                                               SILBasicBlock *succBB) {
  auto *BI = cast<BranchInst>(BB->getTerminator());
  assert(succBB->getSinglePredecessorBlock());

  // If there are any BB arguments in the destination, replace them with the
  // branch operands, since they must dominate the dest block.
  for (unsigned i = 0, e = BI->getArgs().size(); i != e; ++i)
    succBB->getArgument(i)->replaceAllUsesWith(BI->getArg(i));

  BI->eraseFromParent();

  // Move the instruction from the successor block to the current block.
  BB->spliceAtEnd(succBB);

  succBB->eraseFromParent();
}

//===----------------------------------------------------------------------===//
//                              DeadEndBlocks
//===----------------------------------------------------------------------===//

void DeadEndBlocks::compute() {
  assert(ReachableBlocks.empty() && "Computed twice");

  // First step: find blocks which end up in a no-return block (terminated by
  // an unreachable instruction).
  // Search for function-exiting blocks, i.e. return and throw.
  for (const SILBasicBlock &BB : *F) {
    const TermInst *TI = BB.getTerminator();
    if (TI->isFunctionExiting())
      ReachableBlocks.insert(&BB);
  }
  // Propagate the reachability up the control flow graph.
  unsigned Idx = 0;
  while (Idx < ReachableBlocks.size()) {
    const SILBasicBlock *BB = ReachableBlocks[Idx++];
    for (SILBasicBlock *Pred : BB->getPredecessorBlocks())
      ReachableBlocks.insert(Pred);
  }
}
