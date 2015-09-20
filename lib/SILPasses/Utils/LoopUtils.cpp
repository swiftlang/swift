//===--- LoopUtils.cpp ----------------------------------------------------===//
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

#define DEBUG_TYPE "sil-loop-utils"
#include "swift/SILPasses/Utils/LoopUtils.h"
#include "swift/SIL/Dominance.h"
#include "swift/SIL/LoopInfo.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILBasicBlock.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILModule.h"
#include "swift/SILPasses/Utils/CFG.h"
#include "llvm/Support/Debug.h"

using namespace swift;

static SILBasicBlock *getSingleOutsideLoopPredecessor(SILLoop *L,
                                                      SILBasicBlock *BB) {
  SmallVector<SILBasicBlock *, 8> Preds;
  for (auto *Pred : BB->getPreds())
    if (!L->contains(Pred))
      Preds.push_back(Pred);
  if (Preds.size() != 1)
    return nullptr;
  return Preds[0];
}

/// \brief Try to create a unique loop preheader.
///
/// FIXME: We should handle merging multiple loop predecessors.
static SILBasicBlock* insertPreheader(SILLoop *L, DominanceInfo *DT,
                                      SILLoopInfo *LI) {
  assert(!L->getLoopPreheader() && "Expect multiple preheaders");

  SILBasicBlock *Header = L->getHeader();
  SILBasicBlock *Preheader = nullptr;
  if (auto LoopPred = getSingleOutsideLoopPredecessor(L, Header)) {
    if (isa<CondBranchInst>(LoopPred->getTerminator())) {
      Preheader = splitIfCriticalEdge(LoopPred, Header, DT, LI);
      assert(Preheader && "Must have a preheader now");
    }
  }
  return Preheader;
}

/// \brief Convert a loop with multiple backedges to a single backedge loop.
///
/// Create a new block as a common target for all the current loop backedges.
static SILBasicBlock *insertBackedgeBlock(SILLoop *L, DominanceInfo *DT,
                                          SILLoopInfo *LI) {
  assert(!L->getLoopLatch() && "Must have > 1 backedge.");

  // For simplicity, assume a single preheader
  SILBasicBlock *Preheader = L->getLoopPreheader();
  if (!Preheader)
    return nullptr;

  SILBasicBlock *Header = L->getHeader();
  SILFunction *F = Header->getParent();

  // Figure out which basic blocks contain back-edges to the loop header.
  SmallVector<SILBasicBlock*, 4> BackedgeBlocks;
  for (auto *Pred : Header->getPreds()) {
    if (Pred == Preheader)
      continue;
    // Branches can be handled trivially and CondBranch edges can be split.
    if (!isa<BranchInst>(Pred->getTerminator())
        && !isa<CondBranchInst>(Pred->getTerminator())) {
      return nullptr;
    }
    BackedgeBlocks.push_back(Pred);
  }

  // Create and insert the new backedge block...
  SILBasicBlock *BEBlock =
    new (F->getModule()) SILBasicBlock(F, BackedgeBlocks.back());

  DEBUG(llvm::dbgs() << "  Inserting unique backedge block " << *BEBlock
        << "\n");

  // Now that the block has been inserted into the function, create PHI nodes in
  // the backedge block which correspond to any PHI nodes in the header block.
  SmallVector<SILValue, 6> BBArgs;
  for (auto *BBArg : Header->getBBArgs()) {
    BBArgs.push_back(BEBlock->createBBArg(BBArg->getType(), /*Decl=*/nullptr));
  }

  // Arbitrarily pick one of the predecessor's branch locations.
  SILLocation BranchLoc = BackedgeBlocks.back()->getTerminator()->getLoc();

  // Create an unconditional branch that propagates the newly created BBArgs.
  BranchInst *Branch = SILBuilder(BEBlock).createBranch(BranchLoc,
                                                        Header, BBArgs);

  // Redirect the backedge blocks to BEBlock instead of Header.
  for (auto *Pred : BackedgeBlocks) {
    auto *Terminator = Pred->getTerminator();

    if (auto *Branch = dyn_cast<BranchInst>(Terminator))
      changeBranchTarget(Branch, 0, BEBlock, /*PreserveArgs=*/true);
    else if (auto *CondBranch = dyn_cast<CondBranchInst>(Terminator)) {
      unsigned EdgeIdx = (CondBranch->getTrueBB() == Header)
        ? CondBranchInst::TrueIdx : CondBranchInst::FalseIdx;
      changeBranchTarget(CondBranch, EdgeIdx, BEBlock, /*PreserveArgs=*/true);
    }
    else {
      llvm_unreachable("Expected a branch terminator.");
    }
  }

  // Update Loop Information - we know that this block is now in the current
  // loop and all parent loops.
  L->addBasicBlockToLoop(BEBlock, LI->getBase());

  // Update dominator information
  SILBasicBlock *DomBB = BackedgeBlocks.back();
  for (auto BBIter = BackedgeBlocks.begin(),
         BBEnd = std::prev(BackedgeBlocks.end());
       BBIter != BBEnd; ++BBIter) {
    DomBB = DT->findNearestCommonDominator(DomBB, *BBIter);
  }
  DT->addNewBlock(BEBlock, DomBB);

  return BEBlock;
}

/// Canonicalize the loop for rotation and downstream passes.
///
/// Create a single preheader and single latch block.
///
/// FIXME: We should identify nested loops with a common header and separate
/// them before merging the latch. See LLVM's separateNestedLoop.
bool swift::canonicalizeLoop(SILLoop *L, DominanceInfo *DT, SILLoopInfo *LI) {
  bool ChangedCFG = false;
  if (!L->getLoopPreheader()) {
    if (insertPreheader(L, DT, LI))
      ChangedCFG = true;
    else
      return ChangedCFG; // Skip further simplification with no preheader.
  }
  if (!L->getLoopLatch())
    ChangedCFG |= (insertBackedgeBlock(L, DT, LI) != nullptr);

  return ChangedCFG;
}
